// Phase: A6 (tooling, 2026-10-04) — the balance sweep: the same matches on other numbers, measured.
// The behaviour bench (BehaviourBenchTests) says how units behave; this says what a number does to the fight. It plays
// each VARIANT of a spec (unit numbers, the match's config, the script's knobs) over the same seeds on the
// deterministic sim and writes one report per variant in the bench's shape, so Tools/abtest.py's report and
// Tools/sweep.py read them: same seeds, same code, same numbers, and two variants are an A/B.
// A variant is data and needs no recompile:
//  - "unit": one field of a unit's definition (Roster, Infantry, Weapon, Machine, Drive: UnitDef), set or multiplied,
//    written through UnitDefinitions.Apply before the first tick, the door the sim keeps for a test or a bake;
//  - "config": one field of the match's SimConfig (FactionA/B by name, LoadoutA/B as a list, the economy);
//  - "script_a" / "script_b": one public field of the ScriptedEnemy that plays that side (Odds, AttackGarrison, ...);
//  - "harness": the match's own footing, not a number of the game (Harness). Sea = false: nobody lands by boat, so
//    seat 1's men walk up from their spawn point as seat 0's do (the sea lift is taken off the world before its first
//    tick; the ground, the beach and the stores boat stay). FieldSeed: another ground of the same kind (the scene's
//    is 1917). Bombardment: the ambient shells a minute (the scene's 8). WindZ: the wind along the field in m/s
//    (the scene's -1: gas and smoke drift toward seat 0). A match only.
// An unknown unit, field or value throws with its name: a sweep that silently changed nothing reads as "no effect".
// Two scenarios, both the gate's own harnesses, neither copied:
//  - match: MatchLoopTests.Play, the scene's ground and economy, by default the script on both seats; with swapSeats
//    every seed is played twice, the sides changing seats, so a seat's advantage is told apart from a side's. Side
//    "a" is whoever config gives FactionA / LoadoutA / script_a to, on either seat;
//  - ladder: AssaultLadderTests.Run, N attackers at a garrison, bare or behind support.
// What it reports (the names are Tools/sweep.py's too):
//  - attrition_ratio: match, men lost in the open over men lost in a trench; ladder, attackers lost over defenders lost;
//  - time_to_breach_s: from an assault's start to the trench it takes. In a match an assault starts when four of a
//    side's men stand in the open between the two front lines (counted from the world, the same for a script and a
//    policy) and ends when none do; only a seed where a trench fell has the number, and the mean is over those;
//  - trench_retention: the share of the match each trench a side held at the start stayed its own;
//  - win_a, win_b, stalemate, win_seat0, end_s, captures, men lost and deployed, deaths per man-minute, and the other
//    side's men killed per silver spent;
//  - where the dead stood (2026-10-07): lost_open and lost_trench a side, and dead_rear, dead_own_trench,
//    dead_between, dead_nomans, dead_far by the lines as the match began. A dead man's trench is cleared with him
//    (SimWorld.Despawn), so the sweep keeps where each man stood at the end of the tick before; reading it off the
//    death event's slot, as it did, counted every death as "in the open";
//  - the army: men_10, rifle_share_10, men_20, rifle_share_20 (the living at 10 and 20 minutes, a match that lasted),
//    bought_rifle_share, bought_classes, machines; orders (the script's own over-the-top orders, which "assaults",
//    four men in the open, is not); march_s and ashore_s (a man from his purchase to the front trench, and to the
//    field); front_men and odds2_share (the front garrison at the script's attack checks, and how often it had two to
//    one and eight men).
// Each match also writes a "detail" line of JSON by SEAT (every zone, the cause of death, the mix by class, the boats'
// loads), for a reader that wants more than the means.
// Heroes are off unless the spec says so (HeroSystem.TeamMask is 1: only seat 0 gets one, which is a seat's edge).
// Run by name (Explicit): TW_SWEEP names the compiled spec (Tools/sweep.py writes it: every value a string, grids
// already expanded), TW_SWEEP_OUT the folder for <variant>.json and <variant>.txt, TW_BENCH_SEEDS the seeds as the
// bench reads them. A variant is written as soon as it is done, so a run that dies keeps what it finished. With no
// TW_SWEEP_OUT but abtest.py's TW_BENCH_OUT, the spec's first variant goes to that stem: a source variant's A/B.
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Text;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class BalanceSweepTests
    {
        [Serializable] public class Patch { public string on, unit, field, op, value; }
        [Serializable] public class Rung { public int attackers, defenders, gunners, attackGuns; public string support; public bool gunsCover; }
        [Serializable] public class Variant { public string name; public Patch[] patches; }
        /// <summary>What a "harness" patch turns: the footing of the match, which is the sweep's and no number of the
        /// game. Sea: seat 1 lands its men by boat (the scene's field); false, both sides walk up from a spawn point.
        /// FieldSeed: the ground (the scene's ShelledForest 1917). Bombardment: ambient shells a minute (the scene's 8).
        /// WindZ: the map's wind along the field, m/s (the scene's -1, toward seat 0's rear).</summary>
        public class Harness { public bool Sea = true; public uint FieldSeed = 1917u; public float Bombardment = 8f, WindZ = -1f; }
        [Serializable]
        public class Spec
        {
            public string scenario = "match", policy = "Script";
            public int minutes = 8;
            public bool heroes, swapSeats = true;
            public Rung[] ladder;
            public Variant[] variants;
        }

        public static readonly uint[] Seeds = { 1, 2, 3, 4, 5, 6, 7, 8 };
        /// <summary>Men of one side in the open between the front lines that make an assault, and the margin (m) either
        /// side of a front trench's line that still counts as the trench.</summary>
        public const int AssaultMen = 4;
        public const float LineMargin = 3f;

        // ---- a variant's patches ------------------------------------------------------------------------------------

        static readonly CultureInfo Inv = CultureInfo.InvariantCulture;

        /// <summary>A unit's archetype by the name the code gives it (InfantryArchetype, VehicleArchetype) or its id.</summary>
        public static byte Archetype(string name)
        {
            if (byte.TryParse(name, NumberStyles.Integer, Inv, out byte id)) return id;
            foreach (var t in new[] { typeof(InfantryArchetype), typeof(VehicleArchetype) })
            {
                var f = t.GetField(name, BindingFlags.Public | BindingFlags.Static | BindingFlags.IgnoreCase);
                if (f != null && f.IsLiteral && f.FieldType == typeof(byte) && f.Name != "Max") return (byte)f.GetRawConstantValue();
            }
            throw new ArgumentException($"sweep: no unit is called '{name}' (InfantryArchetype, VehicleArchetype)");
        }

        static object Turned(Type type, object was, string op, string value, string where)
        {
            bool mul = op == "mul";
            if (!mul && op != "set") throw new ArgumentException($"sweep: {where}: op '{op}' is neither set nor mul");
            try
            {
                if (type == typeof(FixedList32Bytes<byte>))
                {
                    if (mul) throw new ArgumentException("a list cannot be multiplied");
                    var list = new FixedList32Bytes<byte>();
                    foreach (var part in value.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries)) list.Add(Archetype(part.Trim()));
                    return list;
                }
                if (type == typeof(bool))
                {
                    if (mul) throw new ArgumentException("a switch cannot be multiplied");
                    if (value == "1" || value.Equals("true", StringComparison.OrdinalIgnoreCase)) return true;
                    if (value == "0" || value.Equals("false", StringComparison.OrdinalIgnoreCase)) return false;
                    throw new ArgumentException("not true or false");
                }
                if (type.IsEnum)
                {
                    if (mul) throw new ArgumentException("a name cannot be multiplied");
                    return Enum.Parse(type, value, true);
                }
                if (!mul && type == typeof(byte) && !byte.TryParse(value, NumberStyles.Integer, Inv, out _))
                    return (byte)(FactionId)Enum.Parse(typeof(FactionId), value, true);   // FactionA: "Brass"
                double v = double.Parse(value, NumberStyles.Float, Inv);
                if (mul) v *= Convert.ToDouble(was, Inv);
                if (type == typeof(float)) return (float)v;
                if (type == typeof(double)) return v;
                // a whole number keeps its kind; a multiplied one rounds to the nearest
                return Convert.ChangeType(Math.Round(v), type, Inv);
            }
            catch (Exception e) when (!(e is ArgumentException a && a.Message.StartsWith("sweep:")))
            {
                throw new ArgumentException($"sweep: {where}: '{value}' does not {op} a {type.Name} ({e.Message})");
            }
        }

        /// <summary>The boxed <paramref name="root"/> with the field at <paramref name="path"/> (dotted through nested
        /// structs) set or multiplied. A struct is changed in its box, so the caller unboxes what comes back.</summary>
        public static object Patched(object root, string path, string op, string value, string where)
        {
            int dot = path.IndexOf('.');
            string head = dot < 0 ? path : path.Substring(0, dot);
            var f = root.GetType().GetField(head, BindingFlags.Public | BindingFlags.Instance);
            if (f == null || f.IsInitOnly || f.IsLiteral) throw new ArgumentException($"sweep: {where}: {root.GetType().Name} has no field '{head}' to write");
            if (dot >= 0) f.SetValue(root, Patched(f.GetValue(root), path.Substring(dot + 1), op, value, where));
            else f.SetValue(root, Turned(f.FieldType, f.GetValue(root), op, value, where));
            return root;
        }

        static IEnumerable<Patch> Of(Variant v, string on)
        {
            if (v.patches == null) yield break;
            foreach (var p in v.patches)
            {
                if (p.on != "unit" && p.on != "config" && p.on != "script_a" && p.on != "script_b" && p.on != "harness")
                    throw new ArgumentException($"sweep: {v.name}: a patch is on '{p.on}', not unit, config, script_a, script_b or harness");
                if (p.on == on) yield return p;
            }
        }

        /// <summary>The variant's unit patches written into a match before its first tick. Each patched unit starts from
        /// the numbers the match has for it, so a patch names only what it changes.</summary>
        public static void ApplyUnits(MatchSim m, Variant v)
        {
            var w = m.World;
            var combat = w.GetSystem<CombatCatalogueSystem>();
            var drive = w.GetSystem<VehicleKinematicsSystem>();
            var defs = new Dictionary<byte, object>();
            foreach (var p in Of(v, "unit"))
            {
                byte a = Archetype(p.unit);
                if (a >= Archetypes.Count) throw new ArgumentException($"sweep: {v.name}: unit '{p.unit}' ({a}) is past the table");
                if (!defs.TryGetValue(a, out object def))
                {
                    var d = new UnitDef { Archetype = a, Roster = w.Units.Roster[a], Infantry = w.Units.Infantry[a] };
                    if (combat != null) { d.Weapon = combat.Weapon[a]; d.Machine = combat.Tank[a]; }
                    if (drive != null && drive.Profiles.IsCreated) d.Drive = drive.Profiles[a];
                    def = d;
                }
                if (p.field == "Archetype" || p.field == "Roster.Archetype") throw new ArgumentException($"sweep: {v.name}: a unit's id is not a number to sweep");
                defs[a] = Patched(def, p.field, p.op, p.value, $"{v.name}: {p.unit}.{p.field}");
            }
            if (defs.Count == 0) return;
            var all = new UnitDef[defs.Count]; int n = 0;
            foreach (var d in defs.Values) all[n++] = (UnitDef)d;
            UnitDefinitions.Apply(w, all);
        }

        static SimConfig ApplyConfig(SimConfig cfg, Variant v, bool swapped)
        {
            object box = cfg;
            foreach (var p in Of(v, "config"))
            {
                if (p.field == "Seed") throw new ArgumentException($"sweep: {v.name}: the seed is the sweep's, not a variant's");
                Patched(box, p.field, p.op, p.value, $"{v.name}: config.{p.field}");
            }
            cfg = (SimConfig)box;
            if (swapped)
            {
                (cfg.FactionA, cfg.FactionB) = (cfg.FactionB, cfg.FactionA);
                (cfg.LoadoutA, cfg.LoadoutB) = (cfg.LoadoutB, cfg.LoadoutA);
                (cfg.HeroPity0, cfg.HeroPity1) = (cfg.HeroPity1, cfg.HeroPity0);
            }
            return cfg;
        }

        static ScriptedEnemy Script(Variant v, string side, byte seat)
        {
            object s = new ScriptedEnemy();
            foreach (var p in Of(v, side))
            {
                if (p.field == "Side" || p.field == "Said") throw new ArgumentException($"sweep: {v.name}: {side}.{p.field} is the sweep's to set");
                Patched(s, p.field, p.op, p.value, $"{v.name}: {side}.{p.field}");
            }
            var script = (ScriptedEnemy)s; script.Side = seat;
            return script;
        }

        static Harness HarnessOf(Variant v)
        {
            object h = new Harness();
            foreach (var p in Of(v, "harness")) Patched(h, p.field, p.op, p.value, $"{v.name}: harness.{p.field}");
            return (Harness)h;
        }

        // ---- one match ----------------------------------------------------------------------------------------------

        /// <summary>Where a dead man stood, by the lines as the match began and from his own side: behind his rear
        /// trench, in it, between his two lines, in his front trench, in no man's land, in the other side's front
        /// trench, in the open past it, in its rear trench.</summary>
        public static readonly string[] Zones = { "rear", "reserve", "between", "front", "nomans", "their_front", "past", "their_reserve" };
        /// <summary>The ticks of the two army counts: 10 and 20 minutes.</summary>
        public static readonly uint[] MixTicks = { 12000, 24000 };

        public sealed class Watch
        {
            public readonly int[] LostOpen = new int[2], LostTrench = new int[2], Assaults = new int[2], Captures = new int[2];
            public readonly long[] Held = new long[2], ManTicks = new long[2];
            public readonly int[] Owned = new int[2], Start = { -1, -1 }, Breach = { -1, -1 };
            public readonly bool[] Active = new bool[2];
            public byte[] First; public int Ticks; public uint Seen = uint.MaxValue;
            // each slot at the end of the tick before: Despawn clears a dead man's trench and flags before anyone reads them
            public short[] WasIn; public ushort[] WasGen; public bool[] WasMachine;
            public readonly short[] Front = { -1, -1 }; public float[] Line; public readonly float[] RearLine = new float[2];
            public readonly int[][] Where = { new int[Zones.Length], new int[Zones.Length] };
            public readonly int[][] Cause = { new int[4], new int[4] };   // fire, blast, gas, anything else
            // the army: what was bought (by class), the living at MixTicks, machines
            public readonly int[][] Bought = { new int[Archetypes.Count], new int[Archetypes.Count] };
            public readonly int[][][] Mix = { new int[2][], new int[2][] };
            public readonly int[] Machines = new int[2];
            // a man's way up: the tick he was bought and the tick he stood on the field, until he reaches the front trench
            public int[] BoughtAt, FieldAt;
            public readonly List<int>[] March = { new List<int>(), new List<int>() }, Walk = { new List<int>(), new List<int>() };
            public readonly long[] AshoreTicks = new long[2]; public readonly int[] Fielded = new int[2], DiedOnTheWay = new int[2];
            public readonly Stack<int>[] Hold = new Stack<int>[SeaLandingSystem.MaxCraft]; public readonly int[] Aboard = new int[SeaLandingSystem.MaxCraft];
            public readonly Queue<int> Landed = new Queue<int>();
            public readonly List<int>[] Loads = { new List<int>(), new List<int>() };
            // the front garrisons at the script's attack check
            public readonly long[] FrontSum = new long[2], FrontSq = new long[2];
            public readonly int[] FrontMax = new int[2], Checks = new int[2], Odds2 = new int[2], Odds3 = new int[2];
            // what the scripts said they did
            public readonly int[] Orders = new int[2], Barrages = new int[2], Sos = new int[2];
            // who killed the dead (by the killer's class; KilledByNoMan: a shell, gas, fire), from where and how far
            public readonly int[][] KilledBy = { new int[Archetypes.Count], new int[Archetypes.Count] }; public readonly int[] KilledByNoMan = new int[2];
            public readonly int[] ShotFromTrench = new int[2], Shot = new int[2]; public readonly double[] ShotRange = new double[2];
            // minute by minute: the living, the dead so far, the front garrison
            public readonly List<int>[] AliveAt = { new List<int>(), new List<int>() }, DeadAt = { new List<int>(), new List<int>() }, FrontAt = { new List<int>(), new List<int>() };
        }

        static void Heard(Watch x, int seat, string said)
        {
            if (said.Contains("over the top")) x.Orders[seat]++;
            if (said.Contains("barrage called")) x.Barrages[seat]++;
            if (said.Contains("SOS")) x.Sos[seat]++;
        }

        /// <summary>The zone (Zones) a man of <paramref name="team"/> died in: the trench he stood in the tick before,
        /// or how far up the field he was.</summary>
        static int Zone(Watch x, int team, short was, float z)
        {
            if (was >= 0 && was < x.First.Length && x.First[was] < 2)
            {
                bool own = x.First[was] == team, front = was == x.Front[x.First[was]];
                return own ? (front ? 3 : 1) : (front ? 5 : 7);
            }
            short mine = x.Front[team], theirs = x.Front[1 - team];
            if (mine < 0 || theirs < 0) return 4;
            float dir = x.Line[theirs] > x.Line[mine] ? 1f : -1f;
            float u = (z - x.Line[mine]) * dir;
            if (u >= (x.Line[theirs] - x.Line[mine]) * dir) return 6;
            if (u >= 0f) return 4;
            return u < (x.RearLine[team] - x.Line[mine]) * dir ? 0 : 2;
        }

        /// <summary>An archetype's name as the code gives it, for a report.</summary>
        public static string NameOf(int id)
        {
            foreach (var t in new[] { typeof(InfantryArchetype), typeof(VehicleArchetype) })
                foreach (var f in t.GetFields(BindingFlags.Public | BindingFlags.Static))
                    if (f.IsLiteral && f.FieldType == typeof(byte) && f.Name != "Max" && (byte)f.GetRawConstantValue() == id) return f.Name;
            return "unit" + id.ToString(Inv);
        }

        static float LineZ(MatchSim m, short t)
        {
            var def = m.Map.Trenches[t];
            return m.Map.NavCellCenter(m.Map.TrenchCells[def.CellStart + def.CellCount / 2]).z;
        }

        /// <summary>Once a tick, after its step: who holds what, who is out in the open between the lines, who died
        /// where, who was bought and how long he took to the front.</summary>
        public static void Sample(MatchSim m, Watch x)
        {
            var w = m.World;
            if (w.Tick == x.Seen) return;
            x.Seen = w.Tick; x.Ticks++;
            var trenches = m.Fields.Trenches;
            if (x.First == null)
            {
                x.First = new byte[trenches.Length];
                for (int k = 0; k < trenches.Length; k++) { x.First[k] = trenches[k].OwnerTeam; if (x.First[k] < 2) x.Owned[x.First[k]]++; }
                x.Front[0] = m.Fields.FrontTrench(0); x.Front[1] = m.Fields.FrontTrench(1);
                x.Line = new float[trenches.Length];
                for (short k = 0; k < trenches.Length; k++) x.Line[k] = m.Map.Trenches[k].CellCount > 0 ? LineZ(m, k) : 0f;
                for (int t = 0; t < 2; t++)
                {
                    // his rearmost line: the own trench furthest from the other side's front
                    float far = x.Front[1 - t] >= 0 ? x.Line[x.Front[1 - t]] : 0f, best = -1f;
                    for (int k = 0; k < trenches.Length; k++)
                        if (x.First[k] == t && math.abs(x.Line[k] - far) > best) { best = math.abs(x.Line[k] - far); x.RearLine[t] = x.Line[k]; }
                }
                int n = w.TrenchId.Length;
                x.WasIn = new short[n]; x.WasGen = new ushort[n]; x.WasMachine = new bool[n]; x.BoughtAt = new int[n]; x.FieldAt = new int[n];
                for (int i = 0; i < n; i++) { x.WasIn[i] = -1; x.BoughtAt[i] = -1; }
                for (int c = 0; c < x.Hold.Length; c++) x.Hold[c] = new Stack<int>();
            }
            for (int k = 0; k < trenches.Length; k++)
                if (x.First[k] < 2 && trenches[k].OwnerTeam == x.First[k]) x.Held[x.First[k]]++;

            short f0 = m.Fields.FrontTrench(0), f1 = m.Fields.FrontTrench(1);
            bool lines = f0 >= 0 && f1 >= 0;
            float lo = 0f, hi = 0f;
            if (lines) { float a = LineZ(m, f0), b = LineZ(m, f1); lo = math.min(a, b) + LineMargin; hi = math.max(a, b) - LineMargin; }
            // the boats: a man who went aboard this tick was bought this tick; one who came off is the last aboard
            // (the hold empties from the back). Craft by craft, the order their men's UnitDeployed events are in.
            var sea = m.Landing;
            x.Landed.Clear();
            if (sea != null && m.Map.HasSea)
                for (int c = 0; c < sea.Count && c < x.Hold.Length; c++)
                {
                    int now = sea.AboardOf(c), was = x.Aboard[c];
                    for (int k = was; k < now; k++) x.Hold[c].Push((int)w.Tick);
                    for (int k = now; k < was && x.Hold[c].Count > 0; k++) x.Landed.Enqueue(x.Hold[c].Pop());
                    if (sea.StateOf(c) == LandingState.Idle) x.Hold[c].Clear();
                    x.Aboard[c] = now;
                }
            // the tick's events first: they are read against where every man stood at the end of the tick before
            var ev = w.Events.Events;
            for (int k = 0; k < ev.Length; k++)
            {
                if (ev[k].Type == SimEventType.TrenchCaptured && ev[k].B >= 0 && ev[k].B < 2)
                {
                    int t = ev[k].B;
                    x.Captures[t]++;
                    if (x.Breach[t] < 0 && x.Start[t] >= 0) x.Breach[t] = (int)w.Tick - x.Start[t];
                }
                if (ev[k].Type == SimEventType.CraftBeached && ev[k].B >= 0 && ev[k].B < 2 && sea != null) x.Loads[ev[k].B].Add(sea.AboardOf(ev[k].A));
                if (ev[k].Type == SimEventType.UnitDeployed)
                {
                    int slot = ev[k].A, team = (int)ev[k].Dir.y & 1;
                    x.Bought[team][w.Archetype[slot]]++;
                    // off a boat he stands on the sand by the waterline; refused by a full lift he stands at the spawn point
                    bool landed = m.Map.HasSea && team == m.Map.SeaTeam && x.Landed.Count > 0 && m.Map.Offshore(ev[k].Pos.z) > -12f;
                    int bought = landed ? x.Landed.Dequeue() : (int)w.Tick;
                    if ((w.Flags[slot] & (uint)UnitFlags.Vehicle) != 0) { x.Machines[team]++; continue; }
                    x.AshoreTicks[team] += (int)w.Tick - bought; x.Fielded[team]++;
                    // his march is timed only while his side's front is the trench it began with
                    x.BoughtAt[slot] = m.Fields.FrontTrench((byte)team) == x.Front[team] ? bought : -1; x.FieldAt[slot] = (int)w.Tick;
                }
                if (ev[k].Type == SimEventType.Death)
                {
                    int slot = ev[k].A, team = w.Team[slot] & 1;
                    bool known = x.WasGen[slot] == w.Generation[slot];   // not a man who came and died within the tick
                    if (known && x.WasMachine[slot]) continue;
                    short was = known ? x.WasIn[slot] : (short)-1;
                    if (was >= 0) x.LostTrench[team]++; else x.LostOpen[team]++;
                    x.Where[team][Zone(x, team, was, ev[k].Pos.z)]++;
                    int why = ev[k].B;
                    x.Cause[team][why >= 0 ? 0 : why == (int)DeathCause.Blast ? 1 : why == (int)DeathCause.Gas ? 2 : 3]++;
                    if (why >= 0 && why < w.Archetype.Length) x.KilledBy[team][w.Archetype[why]]++; else x.KilledByNoMan[team]++;
                    if (why >= 0 && why < w.Position.Length)
                    {
                        x.Shot[team]++; x.ShotRange[team] += math.distance(w.Position[why].xz, ev[k].Pos.xz);
                        if (x.WasIn[why] >= 0 || w.TrenchId[why] >= 0) x.ShotFromTrench[team]++;
                    }
                    if (x.BoughtAt[slot] >= 0) { x.DiedOnTheWay[team]++; x.BoughtAt[slot] = -1; }
                }
            }
            int out0 = 0, out1 = 0;
            for (int i = 0; i < w.HighWater; i++)
            {
                bool alive = w.IsAlive(i), machine = alive && (w.Flags[i] & (uint)UnitFlags.Vehicle) != 0;
                x.WasIn[i] = alive ? w.TrenchId[i] : (short)-1; x.WasGen[i] = w.Generation[i]; x.WasMachine[i] = machine;
                if (!alive || machine) continue;
                int team = w.Team[i] & 1;
                x.ManTicks[team]++;
                float z = w.Position[i].z;
                if (x.BoughtAt[i] >= 0 && x.Front[team] >= 0 && x.Front[1 - team] >= 0)
                {
                    float mine = x.Line[x.Front[team]], dir = x.Line[x.Front[1 - team]] > mine ? 1f : -1f;
                    if (w.TrenchId[i] == x.Front[team] || (z - mine) * dir >= 0f)
                    {
                        x.March[team].Add((int)w.Tick - x.BoughtAt[i]); x.Walk[team].Add((int)w.Tick - x.FieldAt[i]);
                        x.BoughtAt[i] = -1;
                    }
                }
                if (!lines || w.TrenchId[i] >= 0) continue;
                if (z > lo && z < hi) { if (team == 0) out0++; else out1++; }
            }
            for (int t = 0; t < 2; t++)
            {
                int outThere = t == 0 ? out0 : out1;
                if (!x.Active[t] && outThere >= AssaultMen) { x.Active[t] = true; x.Start[t] = (int)w.Tick; x.Assaults[t]++; }
                else if (x.Active[t] && outThere == 0) x.Active[t] = false;
            }
            // what the script sees at its attack check (every 100 ticks at 50): its front garrison against the other's
            if (lines && w.Tick % 100 == 50)
                for (int t = 0; t < 2; t++)
                {
                    int mine = trenches[t == 0 ? f0 : f1].GarrisonCount, held = trenches[t == 0 ? f1 : f0].GarrisonCount;
                    x.Checks[t]++; x.FrontSum[t] += mine; x.FrontSq[t] += (long)mine * mine; x.FrontMax[t] = math.max(x.FrontMax[t], mine);
                    if (mine >= 8 && mine >= 2 * held) x.Odds2[t]++;
                    if (mine >= 8 && mine >= 3 * held) x.Odds3[t]++;
                }
            if (w.Tick % 1200 == 0)
            {
                var alive = new int[2];
                for (int i = 0; i < w.HighWater; i++)
                    if (w.IsAlive(i) && (w.Flags[i] & (uint)UnitFlags.Vehicle) == 0) alive[w.Team[i] & 1]++;
                for (int t = 0; t < 2; t++)
                {
                    short front = t == 0 ? f0 : f1;
                    x.AliveAt[t].Add(alive[t]); x.DeadAt[t].Add(x.LostOpen[t] + x.LostTrench[t]); x.FrontAt[t].Add(front >= 0 ? trenches[front].GarrisonCount : 0);
                }
            }
            for (int at = 0; at < MixTicks.Length; at++)
                if (w.Tick == MixTicks[at])
                {
                    x.Mix[at][0] = new int[Archetypes.Count]; x.Mix[at][1] = new int[Archetypes.Count];
                    for (int i = 0; i < w.HighWater; i++)
                        if (w.IsAlive(i)) x.Mix[at][w.Team[i] & 1][w.Archetype[i]]++;
                }
        }

        static int Percentile(List<int> values, float q)
        {
            if (values.Count == 0) return 0;
            var s = new List<int>(values); s.Sort();
            return s[math.clamp((int)math.round((s.Count - 1) * q), 0, s.Count - 1)];
        }

        static string Classes(int[] by)
        {
            var sb = new StringBuilder("{");
            if (by != null)
                for (int a = 0; a < by.Length; a++)
                    if (by[a] > 0) sb.Append($"{(sb.Length > 1 ? "," : "")}\"{NameOf(a)}\":{by[a]}");
            return sb.Append("}").ToString();
        }

        static string Ints(IEnumerable<int> values) => "[" + string.Join(",", values) + "]";

        /// <summary>Men alive in a count by class (machines are not men), and the riflemen among them.</summary>
        static void Men(int[] by, out int men, out int rifles)
        {
            men = 0; rifles = 0;
            if (by == null) return;
            for (int a = 0; a < by.Length; a++)
            {
                if (!InfantryArchetype.IsInfantry((byte)a)) continue;
                men += by[a];
                if (a == InfantryArchetype.Rifle) rifles += by[a];
            }
        }

        /// <summary>The match by SEAT as one line of JSON: everything the watch counted.</summary>
        static string Detail(Watch x, uint seed, bool swapped, int seatA, int winner, float seconds, float tick)
        {
            var sb = new StringBuilder();
            sb.Append($"{{\"seed\":{seed},\"swapped\":{(swapped ? 1 : 0)},\"seat_a\":{seatA},\"winner_seat\":{winner},\"end_s\":{seconds.ToString("F0", Inv)}");
            string Both(Func<int, string> of) => "[" + of(0) + "," + of(1) + "]";
            sb.Append(",\"lost_open\":" + Ints(x.LostOpen) + ",\"lost_trench\":" + Ints(x.LostTrench));
            sb.Append(",\"zones\":[\"" + string.Join("\",\"", Zones) + "\"]");
            sb.Append(",\"where\":" + Both(t => Ints(x.Where[t])));
            sb.Append(",\"cause_fire_blast_gas_other\":" + Both(t => Ints(x.Cause[t])));
            sb.Append(",\"bought\":" + Both(t => Classes(x.Bought[t])));
            sb.Append(",\"mix_10\":" + Both(t => Classes(x.Mix[0][t])) + ",\"mix_20\":" + Both(t => Classes(x.Mix[1][t])));
            sb.Append(",\"march\":" + Both(t => $"{{\"n\":{x.March[t].Count},\"median_s\":{(Percentile(x.March[t], 0.5f) * tick).ToString("F1", Inv)},\"p90_s\":{(Percentile(x.March[t], 0.9f) * tick).ToString("F1", Inv)}"
                + $",\"walk_median_s\":{(Percentile(x.Walk[t], 0.5f) * tick).ToString("F1", Inv)},\"ashore_mean_s\":{(x.Fielded[t] > 0 ? x.AshoreTicks[t] * tick / x.Fielded[t] : 0f).ToString("F1", Inv)},\"died_on_the_way\":{x.DiedOnTheWay[t]}}}"));
            sb.Append(",\"boat_loads\":" + Both(t => Ints(x.Loads[t])));
            sb.Append(",\"front\":" + Both(t =>
            {
                float n = math.max(1, x.Checks[t]), mean = x.FrontSum[t] / n, sd = math.sqrt(math.max(0f, x.FrontSq[t] / n - mean * mean));
                return $"{{\"checks\":{x.Checks[t]},\"mean\":{mean.ToString("F2", Inv)},\"sd\":{sd.ToString("F2", Inv)},\"max\":{x.FrontMax[t]},\"odds2\":{x.Odds2[t]},\"odds3\":{x.Odds3[t]}}}";
            }));
            sb.Append(",\"orders\":" + Ints(x.Orders) + ",\"barrages\":" + Ints(x.Barrages) + ",\"sos\":" + Ints(x.Sos));
            sb.Append(",\"killed_by\":" + Both(t => Classes(x.KilledBy[t])) + ",\"killed_by_no_man\":" + Ints(x.KilledByNoMan));
            sb.Append(",\"shot\":" + Ints(x.Shot) + ",\"shot_from_trench\":" + Ints(x.ShotFromTrench)
                + ",\"shot_range_m\":" + Both(t => (x.Shot[t] > 0 ? x.ShotRange[t] / x.Shot[t] : 0.0).ToString("F1", Inv)));
            sb.Append(",\"alive_by_minute\":" + Both(t => Ints(x.AliveAt[t])) + ",\"dead_by_minute\":" + Both(t => Ints(x.DeadAt[t])) + ",\"front_by_minute\":" + Both(t => Ints(x.FrontAt[t])));
            sb.Append(",\"assaults\":" + Ints(x.Assaults) + ",\"captures\":" + Ints(x.Captures) + ",\"machines\":" + Ints(x.Machines) + "}");
            return sb.ToString();
        }

        static void Put(Dictionary<string, float> d, string key, float v) => d[key] = v;

        /// <summary>One match of the variant on one seed. Swapped: side "a" sits on seat 1 (the enemy's), "b" on seat 0.</summary>
        public static Dictionary<string, float> Match(Spec spec, Variant v, uint seed, bool swapped, StringBuilder text)
        {
            var policy = (MatchLoopTests.Policy)Enum.Parse(typeof(MatchLoopTests.Policy), spec.policy ?? "Script", true);
            bool scripted = policy == MatchLoopTests.Policy.Script;
            if (swapped && !scripted) throw new ArgumentException($"sweep: seats swap only with the script on both (policy {policy})");
            byte seatA = (byte)(swapped ? 1 : 0), seatB = (byte)(1 - seatA);
            // seat 1 is always a script (the enemy); seat 0 is one only under Policy.Script
            var enemy = Script(v, seatA == 1 ? "script_a" : "script_b", 1);
            var player = scripted ? Script(v, seatA == 0 ? "script_a" : "script_b", 0) : null;
            var x = new Watch();
            var harness = HarnessOf(v);
            enemy.Said = said => Heard(x, 1, said);
            if (player != null) player.Said = said => Heard(x, 0, said);
            SimConfig used = default; var silverEnd = new int[2]; bool played = false;
            var report = MatchLoopTests.Play(policy, spec.minutes, seed, enemy, 8, m => { Sample(m, x); silverEnd[0] = m.World.Silver[0]; silverEnd[1] = m.World.Silver[1]; played = true; }, player,
                cfg => used = ApplyConfig(cfg, v, swapped),
                m =>
                {
                    if (!spec.heroes && m.Hero != null) m.Hero.TeamMask = 0;
                    // both sides walk: a deploy the lift does not take stands its man at the spawn point (SimWorld.Deploy)
                    if (!harness.Sea) m.World.SeaLift = null;
                    if (harness.WindZ != -1f) m.Map.Wind = new float2(m.Map.Wind.x, harness.WindZ);
                    ApplyUnits(m, v);
                },
                ground => { ground.Seed = harness.FieldSeed; ground.Bombardment = harness.Bombardment; return ground; });

            var r = new Dictionary<string, float>();
            int a = seatA, b = seatB;
            float seconds = report.EndTick * used.TickSeconds;
            int lostA = x.LostOpen[a] + x.LostTrench[a], lostB = x.LostOpen[b] + x.LostTrench[b];
            Put(r, "win_a", report.Winner == a ? 1f : 0f);
            Put(r, "win_b", report.Winner == b ? 1f : 0f);
            Put(r, "stalemate", report.Winner < 0 ? 1f : 0f);
            Put(r, "win_seat0", report.Winner == 0 ? 1f : 0f);
            Put(r, "win_seat1", report.Winner == 1 ? 1f : 0f);
            Put(r, "end_s", seconds);
            Put(r, "attrition_ratio", (x.LostOpen[0] + x.LostOpen[1]) / (float)math.max(1, x.LostTrench[0] + x.LostTrench[1]));
            Put(r, "loss_ratio_a_b", lostA / (float)math.max(1, lostB));
            Put(r, "lost_a", lostA); Put(r, "lost_b", lostB);
            Put(r, "deployed_a", report.Deployed[a]); Put(r, "deployed_b", report.Deployed[b]);
            Put(r, "captures_a", x.Captures[a]); Put(r, "captures_b", x.Captures[b]);
            Put(r, "assaults_a", x.Assaults[a]); Put(r, "assaults_b", x.Assaults[b]);
            if (x.Breach[a] >= 0) Put(r, "time_to_breach_s_a", x.Breach[a] * used.TickSeconds);
            if (x.Breach[b] >= 0) Put(r, "time_to_breach_s_b", x.Breach[b] * used.TickSeconds);
            Put(r, "trench_retention_a", x.Owned[a] == 0 || x.Ticks == 0 ? 0f : x.Held[a] / (float)((long)x.Owned[a] * x.Ticks));
            Put(r, "trench_retention_b", x.Owned[b] == 0 || x.Ticks == 0 ? 0f : x.Held[b] / (float)((long)x.Owned[b] * x.Ticks));
            float manMinutes = (x.ManTicks[0] + x.ManTicks[1]) * used.TickSeconds / 60f;
            Put(r, "deaths_per_man_min", manMinutes > 0f ? (lostA + lostB) / manMinutes : 0f);
            if (played)
            {
                float income = used.StartingSilver + used.SilverPerSecond * seconds;
                Put(r, "kills_per_100_silver_a", 100f * lostB / math.max(1f, income - silverEnd[a]));
                Put(r, "kills_per_100_silver_b", 100f * lostA / math.max(1f, income - silverEnd[b]));
            }
            foreach (var (side, t) in new[] { ("a", a), ("b", b) })
            {
                Put(r, "lost_open_" + side, x.LostOpen[t]); Put(r, "lost_trench_" + side, x.LostTrench[t]);
                Put(r, "dead_rear_" + side, x.Where[t][0]); Put(r, "dead_own_trench_" + side, x.Where[t][1] + x.Where[t][3]);
                Put(r, "dead_between_" + side, x.Where[t][2]); Put(r, "dead_nomans_" + side, x.Where[t][4]);
                Put(r, "dead_far_" + side, x.Where[t][5] + x.Where[t][6] + x.Where[t][7]);
                Put(r, "orders_" + side, x.Orders[t]);
                for (int at = 0; at < MixTicks.Length; at++)
                {
                    if (x.Mix[at][t] == null) continue;   // the match was over by then
                    Men(x.Mix[at][t], out int men, out int rifles);
                    string minute = (MixTicks[at] * used.TickSeconds / 60f).ToString("F0", Inv);
                    Put(r, $"men_{minute}_{side}", men); Put(r, $"rifle_share_{minute}_{side}", rifles / (float)math.max(1, men));
                }
                Men(x.Bought[t], out int boughtMen, out int boughtRifles);
                int classes = 0;
                for (int k = 0; k < Archetypes.Count; k++) if (x.Bought[t][k] > 0 && InfantryArchetype.IsInfantry((byte)k)) classes++;
                Put(r, "bought_rifle_share_" + side, boughtRifles / (float)math.max(1, boughtMen));
                Put(r, "bought_classes_" + side, classes); Put(r, "machines_" + side, x.Machines[t]);
                if (x.March[t].Count > 0) Put(r, "march_s_" + side, Percentile(x.March[t], 0.5f) * used.TickSeconds);
                if (x.Fielded[t] > 0) Put(r, "ashore_s_" + side, x.AshoreTicks[t] * used.TickSeconds / x.Fielded[t]);
                if (x.Checks[t] > 0) { Put(r, "front_men_" + side, x.FrontSum[t] / (float)x.Checks[t]); Put(r, "odds2_share_" + side, x.Odds2[t] / (float)x.Checks[t]); }
            }
            text.AppendLine($"  seed {seed}{(swapped ? " swapped" : "")}: winner {(report.Winner < 0 ? "none" : report.Winner == a ? "a" : "b")} at {seconds:F0} s  lost a {lostA} b {lostB}  open {x.LostOpen[0] + x.LostOpen[1]} trench {x.LostTrench[0] + x.LostTrench[1]}  captures a {x.Captures[a]} b {x.Captures[b]}  assaults a {x.Assaults[a]} b {x.Assaults[b]}");
            text.AppendLine("    detail " + Detail(x, seed, swapped, seatA, report.Winner, seconds, used.TickSeconds));
            return r;
        }

        /// <summary>The rungs of the spec's ladder on one seed, each rung's numbers under its own name.</summary>
        public static Dictionary<string, float> Ladder(Spec spec, Variant v, uint seed, StringBuilder text)
        {
            var r = new Dictionary<string, float>();
            foreach (var p in Of(v, "config")) throw new ArgumentException($"sweep: {v.name}: the ladder has its own config; config.{p.field} would change nothing");
            foreach (var side in new[] { "script_a", "script_b" })
                foreach (var p in Of(v, side)) throw new ArgumentException($"sweep: {v.name}: nobody scripts the ladder; {side}.{p.field} would change nothing");
            foreach (var p in Of(v, "harness")) throw new ArgumentException($"sweep: {v.name}: nobody is deployed on the ladder; harness.{p.field} would change nothing");
            if (spec.ladder == null || spec.ladder.Length == 0) throw new ArgumentException("sweep: the ladder scenario names no rung");
            foreach (var g in spec.ladder)
            {
                var support = (AssaultLadderTests.Support)Enum.Parse(typeof(AssaultLadderTests.Support), string.IsNullOrEmpty(g.support) ? "None" : g.support, true);
                var rung = AssaultLadderTests.Run(g.attackers, g.defenders, support, seed, 3600, g.gunners, g.attackGuns, g.gunsCover, m => ApplyUnits(m, v));
                string key = $"{g.attackers}v{g.defenders}_{support.ToString().ToLowerInvariant()}{(g.gunners > 0 || g.attackGuns > 0 ? $"_g{g.attackGuns}v{g.gunners}{(g.gunsCover ? "c" : "")}" : "")}";
                Put(r, key + "_taken", rung.Taken ? 1f : 0f);
                if (rung.Taken) Put(r, key + "_time_to_breach_s", rung.TakenTick / 20f);
                Put(r, key + "_attrition_ratio", rung.AttackersLost / (float)math.max(1, rung.DefendersLost));
                Put(r, key + "_attack_lost_share", rung.Attackers > 0 ? rung.AttackersLost / (float)rung.Attackers : 0f);
                Put(r, key + "_garrison_lost_share", rung.Defenders > 0 ? rung.DefendersLost / (float)rung.Defenders : 0f);
                text.AppendLine($"  seed {seed} {key}: {rung}");
            }
            return r;
        }

        // ---- the report ---------------------------------------------------------------------------------------------

        static string Num(float v) => v.ToString("R", Inv);

        /// <summary>One variant over the seeds, in the bench's shape: "seedN": {metric: value}, "mean": {...}. A number
        /// only some seeds have (a breach) is averaged over those.</summary>
        public static string Run(Spec spec, Variant v, uint[] seeds, StringBuilder text)
        {
            bool ladder = string.Equals(spec.scenario, "ladder", StringComparison.OrdinalIgnoreCase);
            if (!ladder && !string.Equals(spec.scenario, "match", StringComparison.OrdinalIgnoreCase))
                throw new ArgumentException($"sweep: scenario '{spec.scenario}' is neither match nor ladder");
            var json = new StringBuilder("{\n");
            var sum = new Dictionary<string, float>(); var count = new Dictionary<string, int>(); var order = new List<string>();
            text.AppendLine($"== {v.name}");
            foreach (uint seed in seeds)
            {
                var runs = new List<Dictionary<string, float>>();
                if (ladder) runs.Add(Ladder(spec, v, seed, text));
                else
                {
                    runs.Add(Match(spec, v, seed, false, text));
                    if (spec.swapSeats && string.Equals(spec.policy ?? "Script", "Script", StringComparison.OrdinalIgnoreCase)) runs.Add(Match(spec, v, seed, true, text));
                }
                // a seed's number is the mean of its seatings, over the seatings that have it
                var keys = new List<string>(); var s = new Dictionary<string, float>(); var c = new Dictionary<string, int>();
                foreach (var run in runs)
                    foreach (var kv in run)
                    {
                        if (!s.ContainsKey(kv.Key)) { keys.Add(kv.Key); s[kv.Key] = 0f; c[kv.Key] = 0; }
                        s[kv.Key] += kv.Value; c[kv.Key]++;
                    }
                json.Append($"  \"seed{seed}\": {{");
                for (int k = 0; k < keys.Count; k++)
                {
                    float value = s[keys[k]] / c[keys[k]];
                    json.Append($"{(k == 0 ? "" : ", ")}\"{keys[k]}\": {Num(value)}");
                    if (!sum.ContainsKey(keys[k])) { order.Add(keys[k]); sum[keys[k]] = 0f; count[keys[k]] = 0; }
                    sum[keys[k]] += value; count[keys[k]]++;
                }
                json.Append("},\n");
            }
            json.Append("  \"mean\": {");
            for (int k = 0; k < order.Count; k++) json.Append($"{(k == 0 ? "" : ", ")}\"{order[k]}\": {Num(sum[order[k]] / count[order[k]])}");
            json.Append("}\n}\n");
            return json.ToString();
        }

        public static uint[] SeedsFromEnvironment()
        {
            string pick = Environment.GetEnvironmentVariable("TW_BENCH_SEEDS");
            return string.IsNullOrEmpty(pick) ? Seeds : Array.ConvertAll(pick.Split(','), s => uint.Parse(s.Trim(), Inv));
        }

        // The test framework fails any test past 180 s, after it has finished: a ladder of six rungs on eight seeds is
        // three minutes a variant (2026-10-04: three variants 553 s, every report written, the run red). An hour and a
        // half is the launch's own limit in Tools/abtest.py; Tools/sweep.py keeps a launch to a few variants (--chunk).
        [Test, Timeout(5400000), Explicit("a report, not a claim: every variant of the spec TW_SWEEP names, over the seeds (Tools/sweep.py runs it)")]
        public void Report_TheSweep()
        {
            string path = Environment.GetEnvironmentVariable("TW_SWEEP"), outDir = Environment.GetEnvironmentVariable("TW_SWEEP_OUT");
            // under Tools/abtest.py bench (a SOURCE variant: the code changes, the spec does not) there is no folder, only
            // abtest's own stem: the spec's first variant is played and written where abtest reads it
            string stem = Environment.GetEnvironmentVariable("TW_BENCH_OUT");
            // a filter naming the class runs this too (an Explicit test is run by any filter that matches its name): with
            // nothing to play it is skipped, not red
            if (string.IsNullOrEmpty(path)) Assert.Ignore("TW_SWEEP names the compiled spec (python Tools/sweep.py run <spec.json>)");
            if (string.IsNullOrEmpty(outDir) && string.IsNullOrEmpty(stem)) Assert.Ignore("TW_SWEEP_OUT names the folder the reports go to");
            var spec = JsonUtility.FromJson<Spec>(File.ReadAllText(path));
            Assert.IsTrue(spec?.variants != null && spec.variants.Length > 0, "the spec names no variant");
            var seeds = SeedsFromEnvironment();
            if (string.IsNullOrEmpty(outDir))
            {
                var text = new StringBuilder();
                string json = Run(spec, spec.variants[0], seeds, text);
                File.WriteAllText(stem + ".txt", text.ToString());
                File.WriteAllText(stem + ".json", json);
                TestContext.WriteLine(text.ToString());
                return;
            }
            Directory.CreateDirectory(outDir);
            foreach (var v in spec.variants)
            {
                var text = new StringBuilder();
                string json = Run(spec, v, seeds, text);
                File.WriteAllText(Path.Combine(outDir, v.name + ".txt"), text.ToString());
                File.WriteAllText(Path.Combine(outDir, v.name + ".json"), json);
                TestContext.WriteLine(text.ToString());
            }
        }

        // ---- the sweep's own claims: it changes what it says it changes, and nothing else ----------------------------

        static Variant One(string name, params Patch[] patches) => new Variant { name = name, patches = patches };

        [Test]
        public void APatch_WritesTheFieldItNames_AndFailsOnAnyItCannot()
        {
            object weapon = new WeaponStats { Damage = 40f, BurstRounds = 3 };
            Assert.AreEqual(36f, ((WeaponStats)Patched(weapon, "Damage", "mul", "0.9", "t")).Damage, 1e-4f);
            Assert.AreEqual(5, ((WeaponStats)Patched(weapon, "BurstRounds", "set", "5", "t")).BurstRounds);
            object def = new UnitDef { Weapon = new WeaponStats { RangeMax = 100f } };
            Assert.AreEqual(80f, ((UnitDef)Patched(def, "Weapon.RangeMax", "mul", "0.8", "t")).Weapon.RangeMax, 1e-4f, "through a nested struct");
            object cfg = SimConfig.Default;
            Assert.AreEqual((byte)FactionId.Brass, ((SimConfig)Patched(cfg, "FactionA", "set", "Brass", "t")).FactionA, "a faction by name");
            Assert.AreEqual(2, ((SimConfig)Patched(cfg, "LoadoutA", "set", "Rifle, Machinegunner", "t")).LoadoutA.Length, "a loadout as a list");
            Assert.AreEqual(InfantryArchetype.Machinegunner, Archetype("machinegunner"));
            Assert.Throws<ArgumentException>(() => Patched(def, "Weapon.Damge", "set", "1", "t"), "a field that is not there");
            Assert.Throws<ArgumentException>(() => Patched(def, "Weapon.Damage", "add", "1", "t"), "an op that is not there");
            Assert.Throws<ArgumentException>(() => Patched(def, "Weapon.Damage", "set", "lots", "t"), "a value that is not a number");
            Assert.Throws<ArgumentException>(() => Archetype("Riffle"), "a unit that is not there");
        }

        [Test]
        public void AVariantThatChangesNothing_IsTheSameLadder_AndOneThatDisarmsTheGarrison_IsNot()
        {
            var spec = new Spec { scenario = "ladder", ladder = new[] { new Rung { attackers = 20, defenders = 10, support = "None" } } };
            var seeds = new uint[] { 0xA55A0000u };
            string now = Run(spec, One("now"), seeds, new StringBuilder());
            string same = Run(spec, One("same", new Patch { on = "unit", unit = "Rifle", field = "Weapon.Damage", op = "mul", value = "1" }), seeds, new StringBuilder());
            Assert.AreEqual(now, same, "a patch that multiplies by one is written and changes no number");
            // the garrison and three attackers in four are riflemen: without a rifle's damage nobody is shot
            string blunt = Run(spec, One("blunt", new Patch { on = "unit", unit = "Rifle", field = "Weapon.Damage", op = "set", value = "0" }), seeds, new StringBuilder());
            Assert.AreNotEqual(now, blunt, "a rifle that does no damage changes the rung");
        }

        [Test]
        public void AManKilledInATrench_IsCountedInIt_AndOneKilledInTheOpen_InTheOpen()
        {
            // The count read a dead man's trench off his slot after the tick, and the sim clears it as he dies: every
            // match line of every sweep said "trench 0" (2026-10-07). The watch keeps where he stood the tick before.
            var cfg = SimConfig.Default; cfg.Seed = 7;
            var field = BattlefieldParams.ShelledForest(1917u); field.Bombardment = 0f;
            using var m = MatchSim.CreateBattlefield(cfg, field);
            var w = m.World; var x = new Watch();
            short own = m.Fields.FrontTrench(0);
            float ownZ = LineZ(m, own), across = m.Map.SizeMeters.x * 0.5f;
            using var none = new NativeArray<SimCommand>(0, Allocator.Persistent);
            int inside = w.Spawn(0, InfantryArchetype.Rifle, new float3(across, 0f, ownZ - 8f), 100f, 3f, false);
            w.GoalId[inside] = m.Fields.GetGoal(GoalKey.Trench(own));
            for (int t = 0; t < 600 && w.TrenchId[inside] != own; t++) { m.Step(none); Sample(m, x); }
            Assert.AreEqual(own, w.TrenchId[inside], "the man walked into his front trench");
            int outside = w.Spawn(0, InfantryArchetype.Rifle, new float3(across, 0f, ownZ + 30f), 100f, 3f, false);
            m.Step(none); Sample(m, x);
            m.Blast.Queue(new Impact { Pos = w.Position[inside], Damage = 50000f, Radius = 3f, Player = -1 });
            m.Blast.Queue(new Impact { Pos = w.Position[outside], Damage = 50000f, Radius = 3f, Player = -1 });
            for (int t = 0; t < 40 && (w.IsAlive(inside) || w.IsAlive(outside)); t++) { m.Step(none); Sample(m, x); }
            Assert.IsFalse(w.IsAlive(inside) || w.IsAlive(outside), "each shell killed its man");
            Assert.AreEqual(-1, w.TrenchId[inside], "the sim clears a dead man's trench: what the count used to read");
            Assert.AreEqual(1, x.LostTrench[0], "the man in the trench is lost in a trench");
            Assert.AreEqual(1, x.LostOpen[0], "the man in the open is lost in the open");
            Assert.AreEqual(1, x.Where[0][Array.IndexOf(Zones, "front")], "in his own front trench");
            Assert.AreEqual(1, x.Where[0][Array.IndexOf(Zones, "nomans")], "in no man's land");
        }

        [Test]
        public void WithTheSeaOff_Seat1sMenStandOnTheFieldAsTheyAreBought_AndWithItOn_TheyRideABoatIn()
        {
            var spec = new Spec { scenario = "match", minutes = 2, swapSeats = false };
            var boats = Match(spec, One("now"), 1, false, new StringBuilder());
            var walk = Match(spec, One("walk", new Patch { on = "harness", field = "Sea", op = "set", value = "false" }), 1, false, new StringBuilder());
            Assert.Greater(boats["ashore_s_b"], 5f, "seat 1's men are bought, then ride a boat in");
            Assert.AreEqual(0f, boats["ashore_s_a"], "seat 0's stand on the field the tick they are bought");
            Assert.AreEqual(0f, walk["ashore_s_b"], "with the sea off seat 1's do too");
            Assert.Throws<ArgumentException>(() => HarnessOf(One("x", new Patch { on = "harness", field = "See", op = "set", value = "false" })), "a switch that is not there");
            var ladder = new Spec { scenario = "ladder", ladder = new[] { new Rung { attackers = 20, defenders = 10, support = "None" } } };
            Assert.Throws<ArgumentException>(() => Ladder(ladder, One("x", new Patch { on = "harness", field = "Sea", op = "set", value = "false" }), 1, new StringBuilder()),
                "nobody is deployed on the ladder");
        }
    }
}
