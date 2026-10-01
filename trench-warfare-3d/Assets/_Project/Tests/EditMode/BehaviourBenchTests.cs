// Phase: A6 (tooling, 2026-10-01) — how every unit behaves through a whole match, measured on the deterministic match
// the gate already plays (MatchLoopTests.Play: ShelledForest 1917, the scene's economy and shelling), with the scene's
// scripted enemy on BOTH seats, each fielding machines as well as men. Same seeds, same code: same numbers, so a
// report is a verdict and two reports are an A/B (MachineStudy, a match in Play at 4x, is not: it finds, this decides).
// Each unit is scored on what a player would see go wrong, and the worst unit is named, not only the mean (a match
// mean hid one Redoubt hunting 72 times per 100 m, 2026-09-29):
//  - machines: jammed (an order, not laying a gun, ditched, bogged or held, not arrived, not moving, over 2 s),
//    spinning (under 0.3 m/s, over 17 deg/s), hunting (turn reversals over 14 deg/s, per 100 m driven);
//  - men: stuck (an order to go, not pinned, not holding, not moving, over 5 s), idle in the open with an enemy
//    within 120 m, piled up (a friend inside 0.8 m while in the open), and deaths in clumps (4 or more of one side
//    within 6 m and 3 s: a crowd one shell could take).
// Context beside them: machines per type (a whole type hunting is its profile's fault, one unit its situation's), and
// men's share forced prone and pinned with the count of pins. A pin decays in about 3 s once the fire moves on
// (SuppressionRules.DecayPerSecond), so the share is small where pins are many: the scripts' match without the
// machines set down had 0.3-1 % and 20-33 pins, this one (over in about four minutes) 0-25 (2026-10-01; printed to
// whole percent it read 0 and looked like no man was ever pinned).
// Run by name (Explicit, under a minute a seed): it writes %TEMP%/tw-behaviour-bench.txt and .json, or the paths
// TW_BENCH_OUT names (a stem), for Tools/abtest.py to diff; TW_BENCH_SEEDS ("1,2,3,4,5,6") picks the seeds.
// Deterministic but chaotic: any change moves every later tick, so match-wide numbers (deaths, when it ends) scatter
// across seeds like noise, and with three seeds an unrelated number moves the same way on all of them one time in
// four. A number the change acts on directly (a machine's flips for a steering change) moves on every seed; for the
// rest, use six seeds or more (2026-10-01: TurnGain 3 raised flips on all three, the rest came out mixed).
using System.Collections.Generic;
using System.IO;
using System.Text;
using NUnit.Framework;
using Unity.Mathematics;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Nav;

namespace TW.Tests
{
    public class BehaviourBenchTests
    {
        public static readonly uint[] Seeds = { 1, 2, 3 };
        /// <summary>The machines each side sets down behind its spawn point at the start, sent at the enemy's line (the
        /// scripts spend 300 silver on men, so without these no machine takes part): MachineStudy's cast.</summary>
        public static readonly byte[] Ours = { 4, 5, 18, 6, 7, 8, 9, 10, 11, 19, 20, 21, 22, 23, 24 }, Theirs = { 4, 5, 18, 7, 19, 21 };
        public const int Minutes = 8;
        /// <summary>Metres a machine must drive before it can be the worst hunter: below 50 a few single reversals in a
        /// near-standing machine read as hunting (a Brute that drove 22 m with 5 turns was "the worst" at 23 per 100 m,
        /// 2026-10-01). Its flips still count in the totals.</summary>
        public const float WorstFloor = 50f;
        const float Dt = 1f / 20f;

        sealed class Unit
        {
            public byte Arch, Team; public bool Vehicle;
            public float Alive, Metres, Jam, JamRun, Spin, Flips, LastRate, Stuck, StuckRun, Idle, IdleRun, Piled, Held, Open, Moving, Pinned, Prone;
            public float3 Last; public float LastYaw;
        }

        public sealed class Result
        {
            public readonly Dictionary<string, float> Metrics = new Dictionary<string, float>();
            public string Text;
        }

        public static Result Run(uint seed, int minutes = Minutes)
        {
            var units = new Dictionary<long, Unit>();
            var deaths = new List<float4>();   // x, z, team, tick
            var pins = new int[1];               // Pinned events: a man going down under fire, however briefly
            var enemy = new ScriptedEnemy { DeploysTanks = true };
            var player = new ScriptedEnemy { Side = 0, DeploysTanks = true };
            uint seen = uint.MaxValue;
            var report = MatchLoopTests.Play(MatchLoopTests.Policy.Script, minutes, seed, enemy, 8, m => { if (m.World.Tick == seen) return; seen = m.World.Tick; if (seen == 1) SetDown(m); Sample(m, units, deaths, pins); }, player);

            var r = new Result();
            var sb = new StringBuilder($"seed {seed}: {minutes} min, winner {(report.Winner < 0 ? "none" : report.Winner.ToString())} at {report.EndTick / 20} s, captures {report.CapturedByPlayer}/{report.CapturedByEnemy}\n");
            // machines
            float metres = 0f, jam = 0f, spin = 0f, flips = 0f, worstFlip = 0f; string worstFlipOf = "-", worstJamOf = "-", worstSpinOf = "-"; float worstJam = 0f, worstSpin = 0f, worstLife = float.MaxValue; string worstLifeOf = "-";
            int machines = 0;
            var kinds = new SortedDictionary<byte, float4>();   // per machine type: count, metres, flips, jammed s
            // ...and how long that type was alive to do it in. Without this the metres cannot be read: a Pincer that
            // covered 12 m in an eight-minute match (seed 1, 2026-10-01, against 230-390 m for every other machine)
            // may have stood still for eight minutes or been destroyed after ten seconds, and the line said the same
            // either way. Only living units are sampled, so metres and lifetime have to be read together.
            var aliveOf = new SortedDictionary<byte, float>();
            // men
            float stuck = 0f, idle = 0f, piled = 0f, held = 0f, manSeconds = 0f, worstStuck = 0f, inOpen = 0f, moving = 0f, pinnedS = 0f, proneS = 0f;
            int men = 0;
            foreach (var u in units.Values)
            {
                if (u.Alive < 2f) continue;
                if (u.Vehicle)
                {
                    machines++; metres += u.Metres; jam += u.Jam; spin += u.Spin; flips += u.Flips;
                    kinds[u.Arch] = (kinds.TryGetValue(u.Arch, out var kd) ? kd : float4.zero) + new float4(1f, u.Metres, u.Flips, u.Jam);
                    aliveOf[u.Arch] = (aliveOf.TryGetValue(u.Arch, out float al) ? al : 0f) + u.Alive;
                    float per100 = u.Metres > WorstFloor ? 100f * u.Flips / u.Metres : 0f;
                    if (per100 > worstFlip) { worstFlip = per100; worstFlipOf = $"{Name(u.Arch)} (team {u.Team})"; }
                    if (u.Jam > worstJam) { worstJam = u.Jam; worstJamOf = $"{Name(u.Arch)} (team {u.Team})"; }
                    if (u.Spin > worstSpin) { worstSpin = u.Spin; worstSpinOf = $"{Name(u.Arch)} (team {u.Team})"; }
                    // Which machine went first, and whose it was. The lone Pincer lives 6-22 s where every other
                    // single machine lasts the whole ~220 s match, and until this line the report could not say which
                    // side it was on - and dying early is a different story on the losing team than on the winning one.
                    if (u.Alive < worstLife) { worstLife = u.Alive; worstLifeOf = $"{Name(u.Arch)} (team {u.Team})"; }
                }
                else
                {
                    men++; manSeconds += u.Alive; stuck += u.Stuck; idle += u.Idle; piled += u.Piled; held += u.Held; inOpen += u.Open; moving += u.Moving; pinnedS += u.Pinned; proneS += u.Prone;
                    worstStuck = math.max(worstStuck, u.Stuck);
                }
            }
            int clumped = 0, biggest = 0;
            var used = new bool[deaths.Count];
            for (int a = 0; a < deaths.Count; a++)
            {
                if (used[a]) continue;
                var group = new List<int> { a }; used[a] = true;
                for (int g = 0; g < group.Count; g++)
                    for (int b = 0; b < deaths.Count; b++)
                    {
                        if (used[b] || deaths[b].z != deaths[a].z) continue;
                        var p = deaths[group[g]]; var q = deaths[b];
                        if (math.abs(p.w - q.w) <= 60f && math.distance(p.xy, q.xy) <= 6f) { used[b] = true; group.Add(b); }
                    }
                biggest = math.max(biggest, group.Count);
                if (group.Count >= 4) clumped += group.Count;
            }
            var mx = r.Metrics;
            mx["machines"] = machines; mx["machine_metres"] = metres; mx["machine_jam_s"] = jam; mx["machine_spin_s"] = spin;
            mx["machine_flips_per100"] = metres > 1f ? 100f * flips / metres : 0f; mx["machine_worst_flips_per100"] = worstFlip;
            mx["machine_worst_jam_s"] = worstJam; mx["machine_worst_spin_s"] = worstSpin;
            mx["men"] = men; mx["man_minutes"] = manSeconds / 60f;
            mx["men_stuck_s_per_man_min"] = manSeconds > 0f ? stuck / (manSeconds / 60f) : 0f; mx["men_worst_stuck_s"] = worstStuck;
            mx["men_idle_open_share"] = manSeconds > 0f ? idle / manSeconds : 0f; mx["men_piled_share"] = manSeconds > 0f ? piled / manSeconds : 0f;
            mx["men_holding_share"] = manSeconds > 0f ? held / manSeconds : 0f;
            // context, so a zero above can be told from a measure that cannot fire
            mx["men_open_share"] = manSeconds > 0f ? inOpen / manSeconds : 0f; mx["men_moving_open_share"] = manSeconds > 0f ? moving / manSeconds : 0f;
            mx["men_pinned_share"] = manSeconds > 0f ? pinnedS / manSeconds : 0f; mx["men_prone_share"] = manSeconds > 0f ? proneS / manSeconds : 0f;
            mx["pins"] = pins[0];
            foreach (var kv in kinds) mx[$"flips_per100_{Name(kv.Key)}"] = kv.Value.y > 1f ? 100f * kv.Value.z / kv.Value.y : 0f;
            mx["deaths"] = deaths.Count; mx["deaths_in_clumps_share"] = deaths.Count > 0 ? (float)clumped / deaths.Count : 0f; mx["biggest_clump"] = biggest;
            mx["winner"] = report.Winner; mx["end_s"] = report.EndTick / 20f;
            sb.AppendLine($"  machines {machines}: {metres:F0} m, shortest life {worstLifeOf} {(worstLife < float.MaxValue ? worstLife : 0f):F0} s, jammed {jam:F0} s (worst {worstJamOf} {worstJam:F0} s), spinning {spin:F0} s (worst {worstSpinOf} {worstSpin:F0} s), "
                + $"{mx["machine_flips_per100"]:F1} flips/100 m (worst {worstFlipOf} {worstFlip:F0})");
            sb.AppendLine($"  men {men}: stuck {mx["men_stuck_s_per_man_min"]:F2} s per man-minute (worst {worstStuck:F0} s), idle in the open under fire {100f * mx["men_idle_open_share"]:F1} %, "
                + $"piled {100f * mx["men_piled_share"]:F1} %, holding {100f * mx["men_holding_share"]:F1} %; of their time {100f * mx["men_open_share"]:F0} % in the open "
                + $"({100f * mx["men_moving_open_share"]:F0} % moving), {100f * mx["men_prone_share"]:F1} % forced prone, {100f * mx["men_pinned_share"]:F2} % pinned ({pins[0]} pins)");
            // per machine type, the worst first: one bad unit stands out, a whole type that hunts is a profile's fault
            var byFlips = new List<KeyValuePair<byte, float4>>(kinds);
            byFlips.Sort((a, b) => (b.Value.y > 1f ? b.Value.z / b.Value.y : 0f).CompareTo(a.Value.y > 1f ? a.Value.z / a.Value.y : 0f));
            sb.Append("  by type (flips/100 m, metres, jammed s, alive s each):");
            foreach (var kv in byFlips)
                sb.Append($" {Name(kv.Key)}x{kv.Value.x:F0} {(kv.Value.y > 1f ? 100f * kv.Value.z / kv.Value.y : 0f):F0}/{kv.Value.y:F0}/{kv.Value.w:F0}/{(aliveOf.TryGetValue(kv.Key, out float al2) && kv.Value.x > 0f ? al2 / kv.Value.x : 0f):F0}");
            sb.AppendLine();
            sb.AppendLine($"  deaths {deaths.Count}: {100f * mx["deaths_in_clumps_share"]:F0} % in clumps of 4+, the biggest {biggest}");
            r.Text = sb.ToString();
            return r;
        }

        static void SetDown(MatchSim m)
        {
            var w = m.World; var init = w.Init;
            for (int side = 0; side < 2; side++)
            {
                var cast = side == 0 ? Ours : Theirs; float3 at = side == 0 ? init.SpawnA : init.SpawnB; float back = side == 0 ? -1f : 1f;
                for (int n = 0; n < cast.Length; n++)
                {
                    var e = w.Units.Roster[cast[n]];
                    int slot = w.Spawn((byte)side, cast[n], new float3(at.x + (n % 8 - 3.5f) * 12f, 0f, at.z + back * (n / 8) * 8f), e.Hp, e.Speed, true);
                    w.GoalId[slot] = m.Fields.DefaultGoal((byte)side, true);
                }
            }
        }

        static void Sample(MatchSim m, Dictionary<long, Unit> units, List<float4> deaths, int[] pins)
        {
            var w = m.World; var k = m.Vehicles; var f = m.Fields; var map = m.Map;
            var ev = w.Events.Events;
            for (int e = 0; e < ev.Length; e++)
            {
                if (ev[e].Type == SimEventType.Death && (w.Flags[ev[e].A] & (uint)UnitFlags.Vehicle) == 0)
                    deaths.Add(new float4(w.Position[ev[e].A].x, w.Position[ev[e].A].z, w.Team[ev[e].A], w.Tick));
                else if (ev[e].Type == SimEventType.Pinned) pins[0]++;
            }
            bool second = w.Tick % 20 == 0;   // the O(n^2) looks once a second
            const uint Held = (uint)(UnitFlags.Bogged | UnitFlags.Stalled | UnitFlags.Immobilised | UnitFlags.KnockedOut);
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!w.IsAlive(i)) continue;
                uint fl = w.Flags[i];
                if ((fl & (uint)UnitFlags.Emplacement) != 0) continue;
                long key = ((long)i << 16) | w.Generation[i];
                float3 p = w.Position[i]; float yaw = w.Yaw[i];
                bool vehicle = (fl & (uint)UnitFlags.Vehicle) != 0;
                if (!units.TryGetValue(key, out var u)) { units[key] = new Unit { Arch = w.Archetype[i], Team = w.Team[i], Vehicle = vehicle, Last = p, LastYaw = yaw }; continue; }
                u.Alive += Dt;
                float step = math.length((p - u.Last).xz), speed = step / Dt; u.Metres += step; u.Last = p;
                float rate = SimMath.WrapAngle(yaw - u.LastYaw) / Dt; u.LastYaw = yaw;
                int cell = map.NavIndex(map.NavCellOf(p).x, map.NavCellOf(p).y), goal = w.GoalId[i];
                bool going = goal >= 0 && f.Direction[goal * f.CellCount + cell] != FlowField.NoDirection;
                if (vehicle)
                {
                    bool excused = !going || k.HaltTicks[i] > 0 || k.DitchTicks[i] > 0 || k.BogTicks[i] > 0 || (fl & Held) != 0;
                    if (!excused && speed < 0.2f) { u.JamRun += Dt; if (u.JamRun > 2f) u.Jam += Dt; } else u.JamRun = 0f;
                    if (speed < 0.3f && math.abs(rate) > 0.3f) u.Spin += Dt;
                    if (math.abs(rate) > 0.25f) { if (math.abs(u.LastRate) > 0.25f && math.sign(rate) != math.sign(u.LastRate)) u.Flips++; u.LastRate = rate; }
                    continue;
                }
                bool open = w.TrenchId[i] < 0 && (fl & (uint)UnitFlags.InTrench) == 0;
                bool pinned = w.Suppression[i] >= StanceRules.PinnedSuppression;
                bool holding = m.Movement.Engage[i] == MovementSystem.EngageHold;
                if (open) { u.Open += Dt; if (speed >= 0.1f) u.Moving += Dt; }
                if (pinned) u.Pinned += Dt;
                if (w.Suppression[i] >= StanceRules.ProneSuppression) u.Prone += Dt;
                if (holding && open) u.Held += Dt;
                if (going && open && !pinned && !holding && speed < 0.1f) { u.StuckRun += Dt; if (u.StuckRun > 5f) u.Stuck += Dt; } else u.StuckRun = 0f;
                if (!second || !open) { if (!open) u.IdleRun = 0f; continue; }
                // once a second: idle in the open with an enemy near, and piled on a friend
                float foe = float.MaxValue, friend = float.MaxValue;
                for (int j = 0; j < w.HighWater; j++)
                {
                    if (j == i || !w.IsAlive(j)) continue;
                    float d = math.distance(w.Position[j].xz, p.xz);
                    if (w.Team[j] != w.Team[i]) foe = math.min(foe, d);
                    else if ((w.Flags[j] & (uint)UnitFlags.Vehicle) == 0 && w.TrenchId[j] < 0) friend = math.min(friend, d);
                }
                if (speed < 0.1f && !holding && !pinned && foe < 120f) u.Idle += 1f;
                if (friend < 0.8f) u.Piled += 1f;
            }
        }

        static string Name(byte archetype)
        {
            foreach (var fi in typeof(VehicleArchetype).GetFields())
                if (fi.IsLiteral && fi.FieldType == typeof(byte) && (byte)fi.GetRawConstantValue() == archetype) return fi.Name;
            return archetype.ToString();
        }

        [Test, Explicit("a report, not a claim: three eight-minute matches, both seats scripted and fielding machines")]
        public void Report_HowEveryUnitBehaves()
        {
            var text = new StringBuilder(); var json = new StringBuilder("{\n");
            var sum = new Dictionary<string, float>();
            var seeds = Seeds;
            string pick = System.Environment.GetEnvironmentVariable("TW_BENCH_SEEDS");
            if (!string.IsNullOrEmpty(pick)) seeds = System.Array.ConvertAll(pick.Split(','), x => uint.Parse(x.Trim()));
            for (int n = 0; n < seeds.Length; n++)
            {
                var r = Run(seeds[n]);
                text.Append(r.Text);
                json.Append($"  \"seed{seeds[n]}\": {{");
                bool first = true;
                foreach (var kv in r.Metrics)
                {
                    json.Append($"{(first ? "" : ", ")}\"{kv.Key}\": {kv.Value.ToString("R", System.Globalization.CultureInfo.InvariantCulture)}");
                    first = false;
                    sum[kv.Key] = (sum.TryGetValue(kv.Key, out var v) ? v : 0f) + kv.Value;
                }
                json.Append("},\n");
            }
            json.Append("  \"mean\": {");
            bool f0 = true;
            foreach (var kv in sum) { json.Append($"{(f0 ? "" : ", ")}\"{kv.Key}\": {(kv.Value / seeds.Length).ToString("R", System.Globalization.CultureInfo.InvariantCulture)}"); f0 = false; }
            json.Append("}\n}\n");
            string stem = System.Environment.GetEnvironmentVariable("TW_BENCH_OUT");
            if (string.IsNullOrEmpty(stem)) stem = Path.Combine(Path.GetTempPath(), "tw-behaviour-bench");
            File.WriteAllText(stem + ".txt", text.ToString());
            File.WriteAllText(stem + ".json", json.ToString());
            TestContext.WriteLine(text.ToString());
        }
    }
}
