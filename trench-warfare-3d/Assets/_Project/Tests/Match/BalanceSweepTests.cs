// Phase: A6 (tooling, 2026-10-04) — the balance sweep: the same matches on other numbers, measured.
// The behaviour bench (BehaviourBenchTests) says how units behave; this says what a number does to the fight. It plays
// each VARIANT of a spec (unit numbers, the match's config, the script's knobs) over the same seeds on the
// deterministic sim and writes one report per variant in the bench's shape, so Tools/abtest.py's report and
// Tools/sweep.py read them: same seeds, same code, same numbers, and two variants are an A/B.
// A variant is data and needs no recompile:
//  - "unit": one field of a unit's definition (Roster, Infantry, Weapon, Machine, Drive: UnitDef), set or multiplied,
//    written through UnitDefinitions.Apply before the first tick, the door the sim keeps for a test or a bake;
//  - "config": one field of the match's SimConfig (FactionA/B by name, LoadoutA/B as a list, the economy);
//  - "script_a" / "script_b": one public field of the ScriptedEnemy that plays that side (Odds, AttackGarrison, ...).
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
//    side's men killed per silver spent.
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

namespace TW.Tests
{
    public class BalanceSweepTests
    {
        [Serializable] public class Patch { public string on, unit, field, op, value; }
        [Serializable] public class Rung { public int attackers, defenders, gunners, attackGuns; public string support; public bool gunsCover; }
        [Serializable] public class Variant { public string name; public Patch[] patches; }
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
                if (p.on != "unit" && p.on != "config" && p.on != "script_a" && p.on != "script_b")
                    throw new ArgumentException($"sweep: {v.name}: a patch is on '{p.on}', not unit, config, script_a or script_b");
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

        // ---- one match ----------------------------------------------------------------------------------------------

        sealed class Watch
        {
            public readonly int[] LostOpen = new int[2], LostTrench = new int[2], Assaults = new int[2], Captures = new int[2];
            public readonly long[] Held = new long[2], ManTicks = new long[2];
            public readonly int[] Owned = new int[2], Start = { -1, -1 }, Breach = { -1, -1 };
            public readonly bool[] Active = new bool[2];
            public byte[] First; public int Ticks; public uint Seen = uint.MaxValue;
        }

        /// <summary>The open track of one match: where every man stood each five seconds, one JSON line a sample, for
        /// Tools/sweepfilm.py's page. Off unless TW_SWEEP_TRACK names a folder. Hand-written strings like the report's
        /// JSON, invariant culture, plain newlines; no serializer.</summary>
        sealed class Track
        {
            StreamWriter _w;
            public int Every, Samples;
            // The slots of the last sample, so a man who has died since is marked once, where he last stood [SW.4].
            int[] _slot = System.Array.Empty<int>(); ushort[] _gen = System.Array.Empty<ushort>();
            float[] _x = System.Array.Empty<float>(), _z = System.Array.Empty<float>();
            byte[] _team = System.Array.Empty<byte>();
            int _was;

            public static Track Open(string dir, Spec spec, Variant v, uint seed, bool swapped, MatchSim m)
            {
                Directory.CreateDirectory(dir);
                float dt = m.World.Config.TickSeconds;
                var t = new Track { Every = math.max(1, (int)math.round(5f / dt)) };
                t._w = new StreamWriter(Path.Combine(dir, $"{v.name}-s{seed}{(swapped ? "-swapped" : "")}.jsonl"), false)
                { NewLine = "\n" };
                var head = new StringBuilder("{\"variant\": \"").Append(v.name).Append("\", \"seed\": ").Append(seed.ToString(Inv));
                head.Append(", \"swapped\": ").Append(swapped ? "true" : "false");
                head.Append(", \"scenario\": \"").Append(spec.scenario).Append("\", \"minutes\": ").Append(spec.minutes.ToString(Inv));
                head.Append(", \"tick_seconds\": ").Append(Num(dt)).Append(", \"every\": ").Append(t.Every.ToString(Inv));
                head.Append(", \"trenches\": [");
                var state = m.Fields.Trenches;
                for (int k = 0; k < state.Length; k++)
                {
                    var def = m.Map.Trenches[k];
                    head.Append(k == 0 ? "{" : ", {").Append("\"owner\": ").Append(((int)state[k].OwnerTeam).ToString(Inv)).Append(", \"cells\": [");
                    for (int j = 0; j < def.CellCount; j++)
                    {
                        var c = m.Map.NavCellCenter(m.Map.TrenchCells[def.CellStart + j]);
                        head.Append(j == 0 ? "[" : ", [").Append(One(c.x)).Append(", ").Append(One(c.z)).Append(']');
                    }
                    head.Append("]}");
                }
                head.Append("]}");
                t._w.Write(head.ToString()); t._w.Write('\n');
                return t;
            }

            /// <summary>x or z to one decimal, invariant.</summary>
            static string One(float v) => v.ToString("F1", Inv);

            /// <summary>One sample: every living man as [x, z, team, inTrench, died], then one entry per slot of the
            /// last sample whose man is gone, with his last known place and died 1. The dead are not counted in
            /// "alive". The team is written as the world holds it, unmasked, so a bad team shows on the page [SW.3].</summary>
            public void Write(MatchSim m)
            {
                var w = m.World;
                var b = new StringBuilder("{\"t\": ").Append(Num(w.Tick * w.Config.TickSeconds)).Append(", \"men\": [");
                int n = 0, alive0 = 0, alive1 = 0, live = 0;
                int cap = math.max(64, w.HighWater + 1);
                var slot = new int[cap]; var gen = new ushort[cap];
                var xs = new float[cap]; var zs = new float[cap]; var teams = new byte[cap];
                var seen = new bool[_was];
                for (int i = 0; i < w.HighWater; i++)
                {
                    if (!w.IsAlive(i) || (w.Flags[i] & (uint)UnitFlags.Vehicle) != 0) continue;
                    byte team = w.Team[i];
                    if ((team & 1) == 0) alive0++; else alive1++;
                    var p = w.Position[i];
                    b.Append(n++ == 0 ? "[" : ", [").Append(One(p.x)).Append(", ").Append(One(p.z)).Append(", ")
                     .Append(team.ToString(Inv)).Append(", ").Append(w.TrenchId[i] >= 0 ? '1' : '0').Append(", 0]");
                    for (int k = 0; k < _was; k++) if (_slot[k] == i && _gen[k] == w.Generation[i]) { seen[k] = true; break; }
                    slot[live] = i; gen[live] = w.Generation[i]; xs[live] = p.x; zs[live] = p.z; teams[live] = team; live++;
                }
                for (int k = 0; k < _was; k++)
                {
                    if (seen[k]) continue;
                    b.Append(n++ == 0 ? "[" : ", [").Append(One(_x[k])).Append(", ").Append(One(_z[k])).Append(", ")
                     .Append(_team[k].ToString(Inv)).Append(", 0, 1]");
                }
                _slot = slot; _gen = gen; _x = xs; _z = zs; _team = teams; _was = live;
                b.Append("], \"alive\": [").Append(alive0.ToString(Inv)).Append(", ").Append(alive1.ToString(Inv)).Append("], \"owners\": [");
                var state = m.Fields.Trenches;
                for (int k = 0; k < state.Length; k++) b.Append(k == 0 ? "" : ", ").Append(((int)state[k].OwnerTeam).ToString(Inv));
                b.Append("]}");
                _w.Write(b.ToString()); _w.Write('\n');
                Samples++;
            }

            public void Close() { _w?.Dispose(); _w = null; }
        }

        static float LineZ(MatchSim m, short t)
        {
            var def = m.Map.Trenches[t];
            return m.Map.NavCellCenter(m.Map.TrenchCells[def.CellStart + def.CellCount / 2]).z;
        }

        /// <summary>Once a tick, after its step: who holds what, who is out in the open between the lines, who died where.</summary>
        static void Sample(MatchSim m, Watch x)
        {
            var w = m.World;
            if (w.Tick == x.Seen) return;
            x.Seen = w.Tick; x.Ticks++;
            var trenches = m.Fields.Trenches;
            if (x.First == null)
            {
                x.First = new byte[trenches.Length];
                for (int k = 0; k < trenches.Length; k++) { x.First[k] = trenches[k].OwnerTeam; if (x.First[k] < 2) x.Owned[x.First[k]]++; }
            }
            for (int k = 0; k < trenches.Length; k++)
                if (x.First[k] < 2 && trenches[k].OwnerTeam == x.First[k]) x.Held[x.First[k]]++;

            short f0 = m.Fields.FrontTrench(0), f1 = m.Fields.FrontTrench(1);
            bool lines = f0 >= 0 && f1 >= 0;
            float lo = 0f, hi = 0f;
            if (lines) { float a = LineZ(m, f0), b = LineZ(m, f1); lo = math.min(a, b) + LineMargin; hi = math.max(a, b) - LineMargin; }
            int out0 = 0, out1 = 0;
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!w.IsAlive(i) || (w.Flags[i] & (uint)UnitFlags.Vehicle) != 0) continue;
                int team = w.Team[i] & 1;
                x.ManTicks[team]++;
                if (!lines || w.TrenchId[i] >= 0) continue;
                float z = w.Position[i].z;
                if (z > lo && z < hi) { if (team == 0) out0++; else out1++; }
            }
            for (int t = 0; t < 2; t++)
            {
                int outThere = t == 0 ? out0 : out1;
                if (!x.Active[t] && outThere >= AssaultMen) { x.Active[t] = true; x.Start[t] = (int)w.Tick; x.Assaults[t]++; }
                else if (x.Active[t] && outThere == 0) x.Active[t] = false;
            }
            var ev = w.Events.Events;
            for (int k = 0; k < ev.Length; k++)
            {
                if (ev[k].Type == SimEventType.TrenchCaptured && ev[k].B >= 0 && ev[k].B < 2)
                {
                    int t = ev[k].B;
                    x.Captures[t]++;
                    if (x.Breach[t] < 0 && x.Start[t] >= 0) x.Breach[t] = (int)w.Tick - x.Start[t];
                }
                // The dead slot is already cleared by Despawn: classify him from the pre-step snapshot [U039.1].
                if (ev[k].Type == SimEventType.Death && MatchLoopTests.Before.WasMan(ev[k].A))
                {
                    int team = MatchLoopTests.Before.TeamOf(ev[k].A);
                    if (MatchLoopTests.Before.WasInTrench(ev[k].A)) x.LostTrench[team]++; else x.LostOpen[team]++;
                }
            }
        }

        static void Put(Dictionary<string, float> d, string key, float v) => d[key] = v;

        /// <summary>A test may ask for the world's hash each 100 ticks, to show the track changes nothing [SW.3].
        /// Off by default, so a real sweep pays nothing for it.</summary>
        internal static bool WatchHash;
        internal static ulong LastHash;

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
            SimConfig used = default; var silverEnd = new int[2]; bool played = false;
            string trackDir = Environment.GetEnvironmentVariable("TW_SWEEP_TRACK");
            Track track = null;
            MatchLoopTests.Report report;
            try
            {
                report = MatchLoopTests.Play(policy, spec.minutes, seed, enemy, 8, m =>
                {
                    Sample(m, x); silverEnd[0] = m.World.Silver[0]; silverEnd[1] = m.World.Silver[1]; played = true;
                    if (WatchHash && m.World.Tick % 100 == 0) LastHash = m.World.Hash();   // [SW.3], off in a real sweep
                    if (track != null && m.World.Tick % (uint)track.Every == 0) track.Write(m);
                }, player,
                    cfg => used = ApplyConfig(cfg, v, swapped),
                    m =>
                    {
                        if (!spec.heroes && m.Hero != null) m.Hero.TeamMask = 0;
                        ApplyUnits(m, v);
                        // built runs on the fresh world before its first tick, so the first sample stands at t=0 [SW.4]
                        if (!string.IsNullOrEmpty(trackDir) && track == null)
                        {
                            track = Track.Open(trackDir, spec, v, seed, swapped, m);
                            track.Write(m);
                        }
                    });
            }
            finally { track?.Close(); }

            var r = new Dictionary<string, float>();
            int a = seatA, b = seatB;
            float seconds = report.EndTick * used.TickSeconds;
            int lostA = x.LostOpen[a] + x.LostTrench[a], lostB = x.LostOpen[b] + x.LostTrench[b];
            Put(r, "win_a", report.Winner == a ? 1f : 0f);
            Put(r, "win_b", report.Winner == b ? 1f : 0f);
            Put(r, "stalemate", report.Winner < 0 ? 1f : 0f);
            Put(r, "win_seat0", report.Winner == 0 ? 1f : 0f);
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
            text.AppendLine($"  seed {seed}{(swapped ? " swapped" : "")}: winner {(report.Winner < 0 ? "none" : report.Winner == a ? "a" : "b")} at {seconds:F0} s  lost a {lostA} b {lostB}  open {x.LostOpen[0] + x.LostOpen[1]} trench {x.LostTrench[0] + x.LostTrench[1]}  captures a {x.Captures[a]} b {x.Captures[b]}  assaults a {x.Assaults[a]} b {x.Assaults[b]}");
            return r;
        }

        /// <summary>The rungs of the spec's ladder on one seed, each rung's numbers under its own name.</summary>
        public static Dictionary<string, float> Ladder(Spec spec, Variant v, uint seed, StringBuilder text)
        {
            var r = new Dictionary<string, float>();
            foreach (var p in Of(v, "config")) throw new ArgumentException($"sweep: {v.name}: the ladder has its own config; config.{p.field} would change nothing");
            foreach (var side in new[] { "script_a", "script_b" })
                foreach (var p in Of(v, side)) throw new ArgumentException($"sweep: {v.name}: nobody scripts the ladder; {side}.{p.field} would change nothing");
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

        // The track the sweep writes with TW_SWEEP_TRACK: a header the page draws the trenches from, then one line
        // each five seconds with every man's place. Plain (not Explicit), so the edit gate runs it.
        [Test, Timeout(300000)]
        public void TheTrack_HoldsWhereEveryManStood_EachFiveSeconds()
        {
            string dir = Path.Combine(Path.GetTempPath(), "tw-sweep-track-" + System.Guid.NewGuid().ToString("N"));
            string was = Environment.GetEnvironmentVariable("TW_SWEEP_TRACK");
            var spec = new Spec { minutes = 1, swapSeats = false };
            Dictionary<string, float> r;
            try
            {
                Environment.SetEnvironmentVariable("TW_SWEEP_TRACK", dir);
                r = Match(spec, One("now"), 1u, false, new StringBuilder());
            }
            finally { Environment.SetEnvironmentVariable("TW_SWEEP_TRACK", was); }

            string file = Path.Combine(dir, "now-s1.jsonl");
            Assert.IsTrue(File.Exists(file), "the track is written where TW_SWEEP_TRACK says: " + file);
            var lines = File.ReadAllLines(file);
            // one minute at 20 ticks a second is 1200 ticks (Play returns EndTick; end_s is it times TickSeconds)
            Assert.AreEqual(60f, r["end_s"], 1e-3f, "the match ran the whole minute, so every sample is there");
            Assert.AreEqual(14, lines.Length, "[SW.4] a header, a sample at t=0 and one each five seconds of the minute");

            var head = lines[0];
            Assert.IsTrue(head.StartsWith("{\"variant\": \"now\""), "the header names the variant: " + head);
            Assert.GreaterOrEqual(Count(head, "\"owner\":"), 1, "the header lists the trenches and their cells: " + head);
            Assert.IsTrue(head.Contains("\"cells\": [["), "a trench has cell centres the page draws a line through");
            Assert.IsTrue(head.Contains("\"every\": 100"), "a sample each 100 ticks is each five seconds at 20 a second");

            float want = 0f;   // [SW.4] the first sample stands at t=0, before any step
            for (int i = 1; i < lines.Length; i++, want += 5f)
            {
                float t = float.Parse(Between(lines[i], "\"t\": ", ","), NumberStyles.Float, Inv);
                Assert.AreEqual(want, t, 1e-3f, $"sample {i} stands at {want} s");
                Assert.LessOrEqual(t, 60f, "no sample is past the minute");
                foreach (var man in Men(lines[i]))
                    Assert.IsTrue(man[2] == "0" || man[2] == "1", "a man is of team 0 or 1, not " + man[2]);
            }
            Assert.GreaterOrEqual(Men(lines[lines.Length - 1]).Count, 1, "the last sample still lists men");
        }

        // [SW.3] The writer's own numbers: two men put down by hand, so a swapped or zeroed axis and a masked team
        // turn this red. [SW.4] A man who dies is marked once, where he fell, and then gone.
        [Test]
        public void TheTrack_WritesEachMansOwnPlaceAndTeam_AndMarksHimDeadOnce()
        {
            string dir = Path.Combine(Path.GetTempPath(), "tw-sweep-track-" + System.Guid.NewGuid().ToString("N"));
            using var m = MatchSim.CreateGreybox(SimConfig.Default);
            var w = m.World;
            int a = w.Spawn(0, InfantryArchetype.Rifle, new float3(-20f, 0f, 5f), 100f, 0f, false);
            int b = w.Spawn(1, InfantryArchetype.Rifle, new float3(50f, 0f, 37f), 100f, 0f, false);

            var track = Track.Open(dir, new Spec { minutes = 1 }, One("hand"), 3u, false, m);
            track.Write(m);
            track.Write(m);                                   // both still alive: nobody is marked dead
            w.Despawn(b);
            track.Write(m);                                   // he is marked once, where he last stood
            track.Write(m);                                   // and is gone from the next
            track.Close();

            var lines = File.ReadAllLines(Path.Combine(dir, "hand-s3.jsonl"));
            Assert.AreEqual(5, lines.Length, "a header and the four samples");
            var men = Men(lines[1]);
            Assert.AreEqual(2, men.Count, "both men stand in the first sample");
            var one = men.Find(x => x[2] == "0");
            var two = men.Find(x => x[2] == "1");
            Assert.IsNotNull(one, "[SW.3] the man of team 0 is written with team 0: " + lines[1]);
            Assert.IsNotNull(two, "[SW.3] the man of team 1 is written with team 1: " + lines[1]);
            Assert.AreEqual("-20.0", one[0], "[SW.3] his x is his own x, not his z"); // the spawn above
            Assert.AreEqual("5.0", one[1], "[SW.3] his z is his own z");
            Assert.AreEqual("50.0", two[0], "[SW.3] the other man's x");
            Assert.AreEqual("37.0", two[1], "[SW.3] the other man's z");
            Assert.AreEqual(w.Position[a].x.ToString("F1", Inv), one[0], "[SW.3] x comes from World.Position at the sampled tick");
            Assert.AreEqual(w.Position[a].z.ToString("F1", Inv), one[1], "[SW.3] z comes from World.Position at the sampled tick");
            Assert.AreEqual(w.Team[a].ToString(Inv), one[2], "[SW.3] the team is the world's, unmasked");
            Assert.AreEqual(w.Team[b].ToString(Inv), two[2], "[SW.3] the other man's team is the world's");
            Assert.AreEqual("0", one[4], "a living man has died 0");
            Assert.AreEqual("0", two[4], "a living man has died 0");
            Assert.AreEqual(2, Men(lines[2]).Count, "nobody died between the first two samples");

            var third = Men(lines[3]);
            Assert.AreEqual(2, third.Count, "[SW.4] the living man and the one who fell");
            var dead = third.Find(x => x[4] == "1");
            Assert.IsNotNull(dead, "[SW.4] the man who died since the last sample is marked: " + lines[3]);
            Assert.AreEqual("50.0", dead[0], "[SW.4] he is marked where he last stood");
            Assert.AreEqual("37.0", dead[1], "[SW.4] he is marked where he last stood");
            Assert.AreEqual("1", dead[2], "[SW.4] with his own team");
            Assert.IsTrue(lines[3].Contains("\"alive\": [1, 0]"), "[SW.4] the dead are not counted alive: " + lines[3]);
            var fourth = Men(lines[4]);
            Assert.AreEqual(1, fourth.Count, "[SW.4] he is marked for one sample only");
            Assert.AreEqual("0", fourth[0][4], "[SW.4] only the living man is left");
        }

        // [SW.3] The track is off unless TW_SWEEP_TRACK names a folder.
        [Test, Timeout(300000)]
        public void AnUnsetTrackFolder_WritesNothing()
        {
            string dir = Path.Combine(Path.GetTempPath(), "tw-sweep-track-" + System.Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(dir);
            string was = Environment.GetEnvironmentVariable("TW_SWEEP_TRACK");
            try
            {
                Environment.SetEnvironmentVariable("TW_SWEEP_TRACK", null);
                Match(new Spec { minutes = 1, swapSeats = false }, One("now"), 1u, false, new StringBuilder());
            }
            finally { Environment.SetEnvironmentVariable("TW_SWEEP_TRACK", was); }
            Assert.AreEqual(0, Directory.GetFileSystemEntries(dir).Length, "[SW.3] with no folder named, no track is written");
        }

        // [SW.3] Writing a track reads the world and never touches it: the same match, with and without.
        [Test, Timeout(600000), Category("Long")]
        public void TheTrack_ChangesNeitherTheResultNorTheHash()
        {
            string dir = Path.Combine(Path.GetTempPath(), "tw-sweep-track-" + System.Guid.NewGuid().ToString("N"));
            string was = Environment.GetEnvironmentVariable("TW_SWEEP_TRACK");
            var spec = new Spec { minutes = 1, swapSeats = false };
            Dictionary<string, float> with, without;
            ulong hashWith, hashWithout;
            bool watched = WatchHash;
            try
            {
                WatchHash = true;
                Environment.SetEnvironmentVariable("TW_SWEEP_TRACK", dir);
                with = Match(spec, One("now"), 1u, false, new StringBuilder());
                hashWith = LastHash;
                Environment.SetEnvironmentVariable("TW_SWEEP_TRACK", null);
                without = Match(spec, One("now"), 1u, false, new StringBuilder());
                hashWithout = LastHash;
            }
            finally { WatchHash = watched; Environment.SetEnvironmentVariable("TW_SWEEP_TRACK", was); }

            Assert.AreEqual(hashWithout, hashWith, "[SW.3] the world hashes the same with the track on and off");
            Assert.AreEqual(without.Count, with.Count, "[SW.3] the same numbers are reported");
            foreach (var kv in without)
                Assert.AreEqual(kv.Value, with[kv.Key], 1e-4f, "[SW.3] " + kv.Key + " is the same with the track on");
        }

        static int Count(string s, string what)
        {
            int n = 0;
            for (int i = s.IndexOf(what, StringComparison.Ordinal); i >= 0; i = s.IndexOf(what, i + 1, StringComparison.Ordinal)) n++;
            return n;
        }

        static string Between(string s, string from, string to)
        {
            int a = s.IndexOf(from, StringComparison.Ordinal) + from.Length;
            return s.Substring(a, s.IndexOf(to, a, StringComparison.Ordinal) - a);
        }

        /// <summary>The men of one sample line, each as its five fields: x, z, team, in a trench, died.</summary>
        static List<string[]> Men(string line)
        {
            var men = new List<string[]>();
            string all = Between(line, "\"men\": [", "], \"alive\"");
            foreach (var part in all.Split('['))
            {
                string body = part.Trim().TrimEnd(',', ' ').TrimEnd(']');
                if (body.Length == 0) continue;
                var f = body.Split(',');
                Assert.AreEqual(5, f.Length, "a man is x, z, team, trench, died: " + body);
                men.Add(System.Array.ConvertAll(f, t => t.Trim()));
            }
            return men;
        }

        // [U039.1] SimWorld.Despawn clears Flags and TrenchId before the Death event is read, so Sample used to
        // classify a corpse by the slot's after-state: a garrisoned man counted as lost in the open, and a dead
        // machine counted as a man. Sample now reads MatchLoopTests.Before, the snapshot taken before the step.
        [Test]
        public void ADeathIsCounted_FromTheSlotAsItStoodAtTheStartOfTheTick()
        {
            using var m = MatchSim.CreateGreybox(SimConfig.Default);
            var w = m.World;
            int man = w.Spawn(1, InfantryArchetype.Rifle, new float3(50f, 0f, 50f), 100f, 0f, false);
            w.TrenchId[man] = 0;                                           // as if he garrisons the first trench
            int machine = w.Spawn(1, VehicleArchetype.Maw, new float3(60f, 0f, 60f), 1000f, 0f, true);
            MatchLoopTests.Before.Refresh(w);                              // what Play does just before the step
            w.Despawn(man); w.Despawn(machine);                            // both die inside the one step

            var x = new Watch();
            Sample(m, x);
            Assert.AreEqual(1, x.LostTrench[1], "[U039.1] the garrisoned man is lost in a trench, not in the open");
            Assert.AreEqual(0, x.LostOpen[1], "[U039.1] nobody of team 1 is lost in the open");
            Assert.AreEqual(0, x.LostTrench[0] + x.LostOpen[0], "[U039.1] team 0 lost nobody");
            Assert.AreEqual(1, x.LostTrench[1] + x.LostOpen[1], "[U039.1] the dead machine is no man lost");
        }
    }
}
