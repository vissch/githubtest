// Phase: A3 — the owner, 2026-09-28, playing the game: "units are now walking in rows in seemingly defined paths, we
// need them to spread over the map instead", and "units should be attacking each other, go out of their way to attack
// each other."
// Both complaints are measured here, on what the sim does and not on how it is written (nothing in this file names
// Lane or EngageSystem: LaneAndEngageRulesTests holds the rules), so the same file runs on the code before the change:
//   - the rows: how many moving men walk in another man's footsteps (FileShare), how much of the field's width the
//     traffic uses (WidthUsed, TopStrips), and where a garrison climbs out of its trench;
//   - the fight: two sections that would have passed each other in the open turn on each other, stand and shoot.
// The numbers measured on the code before the change (8eea308) are written beside each bound.
using System.Collections.Generic;
using System.IO;
using System.Text;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;
using TW.Presentation;

namespace TW.Tests
{
    public class SpreadAndEngageTests
    {
        const int Rifleman = 0, Assault = 1;

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static bool OnFoot(SimWorld w, int i)
            => w.IsAlive(i) && (w.Flags[i] & (uint)UnitFlags.Vehicle) == 0;

        static bool InTheOpen(SimWorld w, int i)
            => OnFoot(w, i) && w.TrenchId[i] < 0 && (w.Flags[i] & (uint)(UnitFlags.InTrench | UnitFlags.Airborne)) == 0;

        static int Alive(SimWorld w, byte team)
        {
            int n = 0;
            for (int i = 0; i < w.HighWater; i++) if (OnFoot(w, i) && w.Team[i] == team) n++;
            return n;
        }

        /// <summary>What a battle looked like, summed over every sample of every man in the open.</summary>
        public struct Report
        {
            public int Samples;          // man-samples in the open and moving
            public float FileShare;      // share of those walking within 0.8 m of the line of a mate 0.5-6 m ahead, the same way
            public int WidthUsed;        // 3 m strips of the field's width holding at least 1% of the traffic
            public int Strips;           // strips there are
            public float TopStrips;      // share of the traffic in the five busiest strips
            public float TopCells;       // share of the traffic on the busiest 5% of the cells anyone stepped on
            public int Near;             // man-samples in the open, not pinned, with an enemy in the open within NearMetres
            public float Engaging;       // share of those closing on him or standing to shoot
            public float WalkingPast;    // share of those moving, and not towards him
            public int Shots, OpenShots, KillsA, KillsB, AliveA, AliveB;
            public float MeanShotRange;  // metres, shots at men in the open by men in the open
            public float StoodToShoot;   // share of those shots fired standing still
            public ulong Hash;
            public string Where;         // the file share by 20 m band along the field, for the report
            public string Deaths;        // who killed whom from where, for the report

            public override string ToString()
                => $"samples {Samples}  file {FileShare:F3}  width {WidthUsed}/{Strips}  top5strips {TopStrips:F3}  top5%cells {TopCells:F3}  "
                 + $"near {Near}  engaging {Engaging:F3}  walkingPast {WalkingPast:F3}  shots {Shots}  open {OpenShots} at {MeanShotRange:F1} m  "
                 + $"stood {StoodToShoot:F3}  kills {KillsA}/{KillsB}  alive {AliveA}/{AliveB}";
        }

        const float NearMetres = 45f;

        /// <summary>
        /// The battle the game plays, on the map it plays: each side deploys <paramref name="men"/> (three riflemen to
        /// one assault man), forms up in its rear trench, and from tick 400 on orders every trench it holds forward
        /// every 5 seconds, as ScriptedEnemy does for the enemy and an impatient player does for himself. With
        /// <paramref name="sides"/> 1 only team 0 takes the field: a march, with nobody to fight. No shells: they are
        /// nobody's decision and would only blur what is being measured.
        /// </summary>
        public static Report Battle(int men, int ticks, uint seed = 0xC0FFEE, uint field = 1917, int sides = 2, string tracks = null)
        {
            var trail = tracks != null ? new StringBuilder() : null;
            var cfg = SimConfig.Default; cfg.StartingSilver = 1000000; cfg.Seed = seed;
            var p = BattlefieldParams.ShelledForest(field); p.Bombardment = 0f;
            using var m = MatchSim.CreateBattlefield(cfg, p);
            var w = m.World;
            var map = m.Map;
            int strips = (int)math.ceil(map.SizeMeters.x / 3f);
            var strip = new long[strips];
            var cells = new Dictionary<int, int>();
            var r = new Report { Strips = strips };
            long filed = 0, engaging = 0, past = 0, stood = 0; double range = 0;
            int bands = (int)math.ceil(map.SizeMeters.y / 20f);
            var bandAll = new long[bands]; var bandFiled = new long[bands];
            var open = new List<int>();
            var wasOpen = new bool[cfg.MaxSlots];
            var how = new int[2, 2, 2]; var deathZ = new double[2]; var deathN = new int[2]; var when = new int[2, 6];
            int deployed = 0;
            for (int t = 0; t < ticks; t++)
            {
                var cmds = new List<SimCommand>();
                if (deployed < men && t % 4 == 0)
                {
                    int slot = deployed % 4 == 3 ? Assault : Rifleman;
                    cmds.Add(SimCommand.Deploy(w.Tick, 0, slot));
                    if (sides > 1) cmds.Add(SimCommand.Deploy(w.Tick, 1, slot));
                    deployed++;
                }
                if (t >= 400 && t % 100 == 0)
                    for (int tr = 0; tr < map.Trenches.Length; tr++)
                        if (m.Fields.Trenches[tr].GarrisonCount > 0)
                            cmds.Add(new SimCommand { Player = m.Fields.Trenches[tr].OwnerTeam, Type = CommandType.TrenchAdvance, A = tr });
                for (int i = 0; i < w.HighWater; i++) wasOpen[i] = InTheOpen(w, i);
                Step(m, cmds.ToArray());
                var dead = m.Fire.Killed;
                for (int k = 0; k < dead.Length; k++)
                {
                    int victim = dead[k].x, killer = dead[k].y;
                    int team = w.Team[killer] & 1;
                    how[team, wasOpen[killer] ? 0 : 1, wasOpen[victim] ? 0 : 1]++;
                    deathZ[team] += w.Position[victim].z; deathN[team]++;
                    if (t / 600 < 6) when[team, t / 600]++;
                }

                var ev = w.Events.Events;
                for (int e = 0; e < ev.Length; e++)
                {
                    if (ev[e].Type != SimEventType.Shot) continue;
                    r.Shots++;
                    int a = ev[e].A, b = ev[e].B;
                    if (a < 0 || b < 0 || !InTheOpen(w, a) || !InTheOpen(w, b)) continue;
                    r.OpenShots++;
                    range += math.distance(w.Position[a].xz, w.Position[b].xz);
                    if (math.length(w.Velocity[a].xz) < 0.05f) stood++;
                }
                if (t % 10 != 0) continue;
                if (trail != null)
                    for (int i = 0; i < w.HighWater; i++)
                        if (OnFoot(w, i)) trail.Append(t).Append(',').Append(i).Append(',').Append(w.Team[i]).Append(',').Append(InTheOpen(w, i) ? 1 : 0).Append(',')
                            .Append(w.Position[i].x.ToString("F2", System.Globalization.CultureInfo.InvariantCulture)).Append(',')
                            .Append(w.Position[i].z.ToString("F2", System.Globalization.CultureInfo.InvariantCulture)).Append(',')
                            .Append(math.length(w.Velocity[i].xz) < 0.05f ? 1 : 0).Append('\n');

                open.Clear();
                for (int i = 0; i < w.HighWater; i++) if (InTheOpen(w, i)) open.Add(i);
                foreach (int i in open)
                {
                    float2 pos = w.Position[i].xz, vel = w.Velocity[i].xz;
                    float speed = math.length(vel);
                    if (speed > 0.3f)
                    {
                        r.Samples++;
                        int band = math.clamp((int)(pos.y / 20f), 0, bands - 1);
                        bandAll[band]++;
                        strip[math.clamp((int)(pos.x / 3f), 0, strips - 1)]++;
                        int cell = map.NavIndex(map.NavCellOf(w.Position[i]).x, map.NavCellOf(w.Position[i]).y);
                        cells.TryGetValue(cell, out int n); cells[cell] = n + 1;
                        float2 fwd = vel / speed, side = new float2(-fwd.y, fwd.x);
                        foreach (int j in open)
                        {
                            if (j == i || w.Team[j] != w.Team[i]) continue;
                            float2 d = w.Position[j].xz - pos;
                            float along = math.dot(d, fwd), across = math.abs(math.dot(d, side));
                            if (along < 0.5f || along > 6f || across > 0.8f) continue;
                            float2 vj = w.Velocity[j].xz; float sj = math.length(vj);
                            if (sj < 0.3f || math.dot(vj / sj, fwd) < 0.9f) continue;
                            filed++; bandFiled[band]++; break;
                        }
                    }
                    if (w.Suppression[i] >= StanceRules.ProneSuppression) continue;
                    int foe = -1; float best = NearMetres * NearMetres;
                    foreach (int j in open)
                    {
                        if (w.Team[j] == w.Team[i]) continue;
                        float d2 = math.distancesq(w.Position[j].xz, pos);
                        if (d2 < best) { best = d2; foe = j; }
                    }
                    if (foe < 0) continue;
                    r.Near++;
                    float2 to = math.normalizesafe(w.Position[foe].xz - pos);
                    bool standing = speed < 0.05f, closing = speed >= 0.05f && math.dot(vel / speed, to) > 0.5f;
                    if ((standing && w.TargetSlot[i] >= 0) || closing) engaging++;
                    else if (!standing) past++;
                }
            }
            long total = 0; foreach (long s in strip) total += s;
            var sorted = new List<long>(strip); sorted.Sort(); sorted.Reverse();
            long top = 0; for (int k = 0; k < 5 && k < sorted.Count; k++) top += sorted[k];
            foreach (long s in strip) if (total > 0 && s * 100 >= total) r.WidthUsed++;
            var visits = new List<int>(cells.Values); visits.Sort(); visits.Reverse();
            long topCells = 0; int take = math.max(1, visits.Count / 20);
            for (int k = 0; k < take && k < visits.Count; k++) topCells += visits[k];
            r.TopStrips = total > 0 ? top / (float)total : 0f;
            r.TopCells = total > 0 ? topCells / (float)total : 0f;
            r.FileShare = r.Samples > 0 ? filed / (float)r.Samples : 0f;
            r.Engaging = r.Near > 0 ? engaging / (float)r.Near : 0f;
            r.WalkingPast = r.Near > 0 ? past / (float)r.Near : 0f;
            r.MeanShotRange = r.OpenShots > 0 ? (float)(range / r.OpenShots) : 0f;
            r.StoodToShoot = r.OpenShots > 0 ? stood / (float)r.OpenShots : 0f;
            r.KillsA = m.Fire.Kills[0]; r.KillsB = m.Fire.Kills[1];
            r.AliveA = Alive(w, 0); r.AliveB = Alive(w, 1);
            r.Hash = w.Hash();
            var sbw = new StringBuilder();
            for (int b = 0; b < bands; b++) if (bandAll[b] > 0) sbw.Append($" z{b * 20}:{bandFiled[b]}/{bandAll[b]}");
            r.Where = sbw.ToString();
            var sbd = new StringBuilder();
            for (int team = 0; team < 2; team++)
            {
                sbd.Append($" team{team} kills: open>open {how[team, 0, 0]} open>trench {how[team, 0, 1]} trench>open {how[team, 1, 0]} trench>trench {how[team, 1, 1]} meanZ {(deathN[team] > 0 ? deathZ[team] / deathN[team] : 0):F0} by30s");
                for (int k = 0; k < 6; k++) sbd.Append($" {when[team, k]}");
                sbd.Append(";");
            }
            r.Deaths = sbd.ToString();
            if (trail != null)
            {
                // the ground first (one line of nav layer bytes per row of cells), then every sample
                var ground = new StringBuilder();
                ground.Append(map.NavWidth).Append(',').Append(map.NavLength).Append('\n');
                for (int z = 0; z < map.NavLength; z++)
                {
                    for (int x = 0; x < map.NavWidth; x++) ground.Append(map.NavLayers[map.NavIndex(x, z)]).Append(x + 1 < map.NavWidth ? "," : "\n");
                }
                File.WriteAllText(tracks, ground.ToString() + "#\n" + trail);
            }
            return r;
        }

        /// <summary>Not a claim: the numbers, written where a session can read them (tw-spread-engage.txt in the temp
        /// folder), for three marches and three battles. Run it on both sides of a change to the movement.</summary>
        [Test, Explicit("a report, not a claim: 22 s of the gate's 600")]
        public void Report_TheNumbersOfThreeMarchesAndThreeBattles()
        {
            var sb = new StringBuilder();
            foreach (int sides in new[] { 1, 2 })
                foreach (var (seed, field) in new[] { (0xC0FFEEu, 1917u), (7u, 1917u), (0xC0FFEEu, 1918u) })
                {
                    // the first march and the first battle also leave their tracks, for Tools/tracks.py to draw
                    string tracks = seed == 0xC0FFEEu && field == 1917u ? Path.Combine(Path.GetTempPath(), sides == 1 ? "tw-tracks-march.csv" : "tw-tracks-battle.csv") : null;
                    var r = Battle(48, 3600, seed, field, sides, tracks);
                    sb.AppendLine($"{(sides == 1 ? "march " : "battle")} seed {seed:X} field {field}: {r}");
                    sb.AppendLine($"   files by band:{r.Where}");
                    if (sides > 1) sb.AppendLine($"   deaths:{r.Deaths}");
                }
            File.WriteAllText(Path.Combine(Path.GetTempPath(), "tw-spread-engage.txt"), sb.ToString());
            TestContext.WriteLine(sb.ToString());
        }

        // ---- the rows ------------------------------------------------------------------------------------------

        [Test]
        public void ACompanyOnTheMarch_UsesTheWidthOfTheField_AndDoesNotWalkInFile()
        {
            foreach (uint field in new[] { 1917u, 1918u })
            {
                var r = Battle(48, 3600, 0xC0FFEE, field, sides: 1);
                Assert.Greater(r.Samples, 3000, $"field {field}: the company marched ({r})");
                Assert.Less(r.FileShare, MarchFile, $"field {field}: men walking in another man's footsteps ({r})\n{r.Where}");
                Assert.GreaterOrEqual(r.WidthUsed, MarchWidth, $"field {field}: 3 m strips of the width that carry traffic ({r})");
                Assert.Less(r.TopStrips, MarchTopStrips, $"field {field}: the share of the traffic on the five busiest strips ({r})");
            }
        }
        // Measured by Report_TheNumbersOfThreeMarchesAndThreeBattles on fields 1917 and 1918. Before the change: in file
        // 0.44 and 0.47, 20 and 19 strips of 30 in use, 0.57 and 0.56 of the traffic on the five busiest. After: in file
        // 0.21 and 0.16, 26 and 27 strips, 0.37 and 0.28. What is left in file is the wire: a belt has two or three gaps.
        const float MarchFile = 0.30f, MarchTopStrips = 0.45f;
        const int MarchWidth = 24;

        /// <summary>
        /// Over the top, where they stand. The greybox trench has a ladder every 20 m and reinforcements came up on
        /// 60 m of front, so before the change a garrison of forty left its trench through a handful of ladder
        /// columns, in as many files. Where a man first stands on open ground beyond the trench is where he climbed out.
        /// </summary>
        [Test]
        public void AGarrisonOrderedForward_ClimbsOutAlongTheWholeTrench()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            using var m = MatchSim.CreateGreybox(cfg, combat: false);
            var w = m.World;
            for (int k = 0; k < 40; k++) Step(m, SimCommand.Deploy(w.Tick, 0, Rifleman));
            for (int t = 0; t < 1500 && m.Fields.Trenches[0].GarrisonCount < 40; t++) Step(m);
            Assert.AreEqual(40, m.Fields.Trenches[0].GarrisonCount, "setup: the garrison formed");
            float trenchZ = m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[0].CellStart]).z;

            Step(m, new SimCommand { Player = 0, Type = CommandType.TrenchAdvance, A = 0 });
            var outAt = new Dictionary<int, int>();   // slot -> the nav column he came out in
            for (int t = 0; t < 400; t++)
            {
                Step(m);
                for (int i = 0; i < w.HighWater; i++)
                {
                    if (!InTheOpen(w, i) || outAt.ContainsKey(i) || w.Position[i].z < trenchZ + 2f) continue;
                    outAt[i] = m.Map.NavCellOf(w.Position[i]).x;
                }
            }
            Assert.GreaterOrEqual(outAt.Count, 36, "the garrison went over the top");
            var columns = new HashSet<int>(outAt.Values);
            int byLadder = 0;
            foreach (int x in outAt.Values) if (x % 10 == 5) byLadder++;   // GreyboxMapGenerator.AddFireTrench: a link every tenth column
            Assert.GreaterOrEqual(columns.Count, 20, $"{outAt.Count} men climbed out in {columns.Count} columns");
            Assert.Less(byLadder, outAt.Count / 2, $"{byLadder} of {outAt.Count} men came up a ladder");
        }

        [Test]
        public void ReinforcementsComeUp_OnAFrontAsWideAsTheField()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            using var m = MatchSim.CreateGreybox(cfg, combat: false);
            var w = m.World;
            for (int k = 0; k < 40; k++) Step(m, SimCommand.Deploy(w.Tick, 0, Rifleman));
            float lo = float.MaxValue, hi = float.MinValue;
            for (int i = 0; i < w.HighWater; i++) if (OnFoot(w, i)) { lo = math.min(lo, w.Position[i].x); hi = math.max(hi, w.Position[i].x); }
            float width = m.Map.SizeMeters.x;
            Assert.Greater(hi - lo, width * 0.8f, $"forty men deployed between x {lo:F0} and {hi:F0} of a field {width:F0} m wide (before the change: 60 m of it)");
        }

        // ---- the fight -----------------------------------------------------------------------------------------

        /// <summary>Two sections of <paramref name="count"/> riflemen in the open on the playtest map's no man's land,
        /// each ordered to the other's front trench. Team 0 starts at z 200 round x0, team 1 at z 280 round x1.</summary>
        static MatchSim Meeting(uint seed, int count, float x0, float x1)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed;
            var m = MatchSim.CreatePlaytest(cfg);
            int goal0 = m.Fields.GetGoal(GoalKey.Trench(2)), goal1 = m.Fields.GetGoal(GoalKey.Trench(1));
            for (int k = 0; k < count; k++)
                for (byte team = 0; team < 2; team++)   // turn about, so neither side holds the lower slots
                {
                    float x = (team == 0 ? x0 : x1) + (k - count / 2) * 3f, z = team == 0 ? 200f : 280f;
                    int slot = m.World.Spawn(team, Rifleman, new float3(x, 0f, z), 100f, 3f, false);
                    m.World.GoalId[slot] = team == 0 ? goal0 : goal1;
                    m.World.Flags[slot] |= (uint)UnitFlags.Exposed;
                }
            return m;
        }

        /// <summary>
        /// 85 m apart across the field, 80 m along it: on the code before the change each section ran straight up
        /// the field to the trench it was sent to, passed the other beyond the 60 m a running man engages at, and
        /// hardly a shot was fired (measured: they came no nearer than 59 m). Now the men whose lanes bring them within
        /// reach of an enemy turn on him, close, stand and shoot; the rest of a section spread over a field 300 m wide
        /// go on to the trench they were sent to.
        /// </summary>
        [Test]
        public void TwoSectionsPassingInTheOpen_TurnOnEachOther_StandAndShoot()
        {
            using var m = Meeting(0xC0FFEE, 10, 107f, 193f);
            var w = m.World;
            int shots = 0, stood = 0; float nearest = float.MaxValue;
            for (int t = 0; t < 1200 && Alive(w, 0) > 0 && Alive(w, 1) > 0; t++)
            {
                Step(m);
                var ev = w.Events.Events;
                for (int e = 0; e < ev.Length; e++)
                {
                    if (ev[e].Type != SimEventType.Shot || !InTheOpen(w, ev[e].A)) continue;
                    shots++;
                    if (math.length(w.Velocity[ev[e].A].xz) < 0.05f) stood++;
                }
                for (int i = 0; i < w.HighWater; i++)
                    for (int j = i + 1; j < w.HighWater; j++)
                        if (InTheOpen(w, i) && InTheOpen(w, j) && w.Team[i] != w.Team[j])
                            nearest = math.min(nearest, math.distance(w.Position[i].xz, w.Position[j].xz));
            }
            int a = Alive(w, 0), b = Alive(w, 1);
            Assert.Less(nearest, 50f, $"the sections closed to {nearest:F0} m");
            Assert.Greater(shots, 40, $"{shots} shots in the open");
            Assert.Greater(stood, shots / 2, $"{stood} of {shots} shots were fired standing still");
            Assert.LessOrEqual(a + b, 14, $"men fell: {a} and {b} of 10 and 10 are left");
        }

        /// <summary>The same fight head on, over eight seeds: neither side may owe its wins to the order of its slots
        /// or the way it faces.</summary>
        [Test]
        public void AMeetingOfEqualSections_IsAnEvenFight()
        {
            int wins0 = 0, wins1 = 0, left0 = 0, left1 = 0;
            for (uint seed = 1; seed <= 8; seed++)
            {
                using var m = Meeting(seed, 12, 150f, 150f);
                var w = m.World;
                for (int t = 0; t < 1600 && Alive(w, 0) > 0 && Alive(w, 1) > 0; t++) Step(m);
                int a = Alive(w, 0), b = Alive(w, 1);
                left0 += a; left1 += b;
                if (a > b) wins0++; else if (b > a) wins1++;
            }
            Assert.GreaterOrEqual(wins0, 2, $"team 0 won {wins0} and team 1 {wins1} of 8 ({left0} and {left1} men left in all)");
            Assert.GreaterOrEqual(wins1, 2, $"team 0 won {wins0} and team 1 {wins1} of 8 ({left0} and {left1} men left in all)");
        }

        /// <summary>An assault does not stop in front of the trench it is storming to duel with the men beyond it.</summary>
        [Test]
        public void AnAssault_DoesNotStandInTheOpen_ToShootAcrossTheTrenchItIsStorming()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            using var m = MatchSim.CreateGreybox(cfg);
            var w = m.World;
            float trenchZ = m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[1].CellStart]).z;
            int goal = m.Fields.GetGoal(GoalKey.Trench(1));
            for (int k = 0; k < 10; k++)
            {
                int slot = w.Spawn(0, Rifleman, new float3(130f + k * 4f, 0f, trenchZ - 70f), 100f, 3f, false);
                w.GoalId[slot] = goal;
                w.Flags[slot] |= (uint)UnitFlags.Exposed;
            }
            // two of the enemy in the open 25 m beyond their trench, too slow to reach it while this lasts
            for (int k = 0; k < 2; k++) w.Spawn(1, Rifleman, new float3(140f + k * 20f, 0f, trenchZ + 25f), 100f, 0.02f, false);
            for (int t = 0; t < 700; t++) Step(m);
            int inTrench = 0;
            var lost = new StringBuilder();
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!OnFoot(w, i) || w.Team[i] != 0) continue;
                if (w.TrenchId[i] == 1) inTrench++;
                else lost.Append($" man {i} at {w.Position[i].x:F0},{w.Position[i].z:F0} (trench at z {trenchZ:F0}) stance {(Stance)w.StanceOf[i]} suppression {w.Suppression[i]:F0} goal {w.GoalId[i]} trench {w.TrenchId[i]};");
            }
            Assert.GreaterOrEqual(inTrench + (10 - Alive(w, 0)), 10, "every man of the assault reached the trench or fell on the way:" + lost);
            Assert.GreaterOrEqual(inTrench, 5, $"{inTrench} of 10 took the trench with two riflemen shooting at them");
        }

        /// <summary>A garrison fights from its trench. The enemy in the open in front of it is no reason to leave.</summary>
        [Test]
        public void AGarrison_StaysInItsTrench_WithTheEnemyInTheOpenBeforeIt()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            using var m = MatchSim.CreateGreybox(cfg);
            var w = m.World;
            for (int k = 0; k < 10; k++) Step(m, SimCommand.Deploy(w.Tick, 0, Rifleman));
            for (int t = 0; t < 1500 && m.Fields.Trenches[0].GarrisonCount < 10; t++) Step(m);
            Assert.AreEqual(10, m.Fields.Trenches[0].GarrisonCount, "setup: the garrison formed");
            float trenchZ = m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[0].CellStart]).z;
            for (int k = 0; k < 3; k++) w.Spawn(1, Rifleman, new float3(140f + k * 8f, 0f, trenchZ + 40f), 100f, 0.02f, false);
            int shots = 0;
            for (int t = 0; t < 300; t++)
            {
                Step(m);
                var ev = w.Events.Events;
                for (int e = 0; e < ev.Length; e++) if (ev[e].Type == SimEventType.Shot && w.Team[ev[e].A] == 0) shots++;
                for (int i = 0; i < w.HighWater; i++)
                    if (OnFoot(w, i) && w.Team[i] == 0) Assert.AreEqual(0, w.TrenchId[i], $"tick {w.Tick}: man {i} left the trench");
            }
            Assert.Greater(shots, 0, "the garrison fired on them");
        }

        [Test]
        public void ABattle_IsFoughtWhereTheSidesMeet()
        {
            var r = Battle(48, 3600);
            Assert.Greater(r.Near, 300, $"men met in the open ({r})");
            Assert.Greater(r.Engaging, 0.62f, $"of the men with an enemy in the open within {NearMetres} m, the share closing on him or standing to shoot ({r})");
            Assert.Greater(r.OpenShots, 100, $"shots in the open ({r})");
            Assert.Greater(r.StoodToShoot, 0.15f, $"the share of the shots in the open fired standing still ({r})");
        }

        [Test]
        public void TheBattle_IsTheSameOnEveryMachine()
        {
            Assert.AreEqual(Battle(24, 1500).Hash, Battle(24, 1500).Hash, "same seed, same lanes, same fight, same hash");
        }

        /// <summary>A man closing on an enemy does not zig-zag where he stands (2026-10-01). EngageSystem asks whether the
        /// way to him is open ground; sampled a metre apart from where he stood, the answer by the corner of a wire or
        /// trench cell changed with a tenth of a metre, so he closed on him one tick and followed his goal's field the
        /// next, back and forth (the scripts' match, seed 2: one man's steps reversed 34 times in one place, 1.3 reversals
        /// a man-minute in the open). Now the way is walked cell by cell, and a man already closing keeps on while the
        /// next CloseKeep metres are open. Counted over the match: a man in the open whose step reversed the one before.</summary>
        [Test]
        public void AManClosingOnTheEnemyDoesNotZigZagWhereHeStands()
        {
            int reversals = 0; float open = 0f;
            var last = new Dictionary<long, float2>(); var lastStep = new Dictionary<long, float2>();
            uint seen = uint.MaxValue;
            MatchLoopTests.Play(MatchLoopTests.Policy.Script, 8, 2u, new ScriptedEnemy(), 8, m =>
            {
                var w = m.World;
                if (w.Tick == seen) return; seen = w.Tick;
                for (int i = 0; i < w.HighWater; i++)
                {
                    if (!w.IsAlive(i) || (w.Flags[i] & (uint)(UnitFlags.Vehicle | UnitFlags.InTrench)) != 0 || w.TrenchId[i] >= 0) continue;
                    open += w.Config.TickSeconds;
                    long key = ((long)i << 16) | w.Generation[i];
                    float2 p = w.Position[i].xz;
                    if (last.TryGetValue(key, out var q))
                    {
                        float2 step = p - q;
                        if (lastStep.TryGetValue(key, out var ls))
                        {
                            float a = math.length(step), b = math.length(ls);
                            if (a > 0.015f && b > 0.015f && math.dot(step, ls) < -0.5f * a * b) reversals++;
                        }
                        lastStep[key] = step;
                    }
                    last[key] = p;
                }
            });
            float perMinute = reversals / math.max(1f, open / 60f);
            TestContext.WriteLine($"men in the open: {open / 60f:F0} man-minutes, {reversals} steps reversed ({perMinute:F2} a man-minute)");
            Assert.Less(perMinute, 0.8f, "a man closing on an enemy does not close and stop closing on alternate ticks (1.3 a man-minute when the way was sampled a metre apart)");
        }
    }
}
