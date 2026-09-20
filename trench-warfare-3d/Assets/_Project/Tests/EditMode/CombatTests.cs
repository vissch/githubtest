// Phase: A2 / A3 core (implemented) — a garrison shoots an assault coming over open ground and wins the exchange,
// hold-fire keeps a garrison silent and out of sight, fire suppresses and suppression decays, a garrison spreads
// along its trench, an undefended trench is captured and the HQ behind it ends the match, and all of it is
// deterministic.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;

namespace TW.Tests
{
    public class CombatTests
    {
        const int Rifleman = 0, MachineGunner = 2;

        static MatchSim NewMatch(uint seed = 0xC0FFEE)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed;
            return MatchSim.CreateGreybox(cfg);
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static int Alive(SimWorld w, byte team)
        {
            int n = 0;
            for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && w.Team[i] == team) n++;
            return n;
        }

        static float TrenchZ(MatchSim m, short trench)
            => m.Map.NavCellCenter(m.Map.TrenchCells[m.Map.Trenches[trench].CellStart]).z;

        /// <summary>Riflemen of <paramref name="team"/> placed in a row at world Z, already ordered to assault <paramref name="goalTrench"/>.</summary>
        static void SpawnAssault(MatchSim m, byte team, int count, float z, short goalTrench)
        {
            int goal = m.Fields.GetGoal(GoalKey.Trench(goalTrench));
            for (int k = 0; k < count; k++)
            {
                int slot = m.World.Spawn(team, Rifleman, new float3(120f + k * 3f, 0f, z), 100f, 3f, false);
                m.World.GoalId[slot] = goal;
                m.World.Flags[slot] |= (uint)UnitFlags.Exposed;
            }
        }

        /// <summary>Riflemen of <paramref name="team"/> garrisoned in <paramref name="trench"/>: deployed the normal way and walked in.</summary>
        static void Garrison(MatchSim m, byte team, short trench, int count, int rosterSlot = Rifleman)
        {
            int want = m.Fields.Trenches[trench].GarrisonCount + count;
            for (int k = 0; k < count; k++) Step(m, SimCommand.Deploy(m.World.Tick, team, rosterSlot));
            for (int t = 0; t < 2000 && m.Fields.Trenches[trench].GarrisonCount < want; t++) Step(m);
            Assert.AreEqual(want, m.Fields.Trenches[trench].GarrisonCount, "setup: garrison did not form");
        }

        [Test]
        public void Garrison_ShootsAnAssaultInTheOpen_AndWinsTheExchange()
        {
            using var m = NewMatch();
            Garrison(m, 1, 1, 8);
            Garrison(m, 1, 1, 2, MachineGunner);   // rifles alone kill but barely suppress: 0.5 rounds a second against 8 points of decay
            SpawnAssault(m, 0, 10, TrenchZ(m, 1) - 110f, 1);
            bool shot = false, suppressed = false;
            float maxSuppression = 0f; int mgShots = 0;
            for (int t = 0; t < 900; t++)
            {
                Step(m);
                for (int i = 0; i < m.World.HighWater; i++) if (m.World.IsAlive(i) && m.World.Team[i] == 0) maxSuppression = math.max(maxSuppression, m.World.Suppression[i]);
                var ev = m.World.Events.Events;
                for (int e = 0; e < ev.Length; e++)
                {
                    if (ev[e].Type == SimEventType.Shot) { shot = true; if (m.World.Archetype[ev[e].A] == MachineGunner) mgShots++; }
                    if (ev[e].Type == SimEventType.Suppressed || ev[e].Type == SimEventType.Pinned) suppressed = true;
                }
            }
            Assert.IsTrue(shot, "the garrison must open fire");
            Assert.IsTrue(suppressed, $"machine-gun fire must suppress men in the open (max {maxSuppression:F0}, MG shots {mgShots})");
            int attackers = Alive(m.World, 0), defenders = Alive(m.World, 1);
            Assert.Less(attackers, 10, "an assault over open ground takes losses");
            Assert.Greater(defenders, attackers, "men on the fire-step beat the same number in the open");
            Assert.Greater(m.Fire.Kills[1], 0);
        }

        [Test]
        public void HoldFire_KeepsTheGarrisonSilent_AndOutOfSight()
        {
            using var m = NewMatch();
            Garrison(m, 1, 1, 6);
            Step(m, new SimCommand { Player = 1, Type = CommandType.TrenchHoldFire, A = 1, B = 1 });
            SpawnAssault(m, 0, 6, TrenchZ(m, 1) - 100f, 1);
            // stop the assault 100 m out so nobody reaches the 8 m reveal range
            for (int i = 0; i < m.World.HighWater; i++) if (m.World.Team[i] == 0) { m.World.GoalId[i] = -1; m.World.TrenchId[i] = -1; m.World.Speed[i] = 0f; }
            for (int t = 0; t < 300; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int e = 0; e < ev.Length; e++) Assert.AreNotEqual(SimEventType.Shot, ev[e].Type, "nobody can engage: one side holds fire below the rim, the other cannot see it");
            }
            Assert.AreEqual(6, Alive(m.World, 0));
            Assert.AreEqual(6, Alive(m.World, 1));
        }

        [Test]
        public void Suppression_DecaysWhenTheFiringStops()
        {
            using var m = NewMatch();
            int slot = m.World.Spawn(0, Rifleman, new float3(150f, 0f, 300f), 100f, 3f, false);
            m.World.Suppression[slot] = 90f;
            Step(m);
            Assert.AreEqual((byte)Stance.Pinned, m.World.StanceOf[slot], "above 85 a man in the open is pinned");
            for (int t = 0; t < 100; t++) Step(m);   // 5 s at 8 per second
            Assert.Less(m.World.Suppression[slot], 60f);
            Assert.AreNotEqual((byte)Stance.Pinned, m.World.StanceOf[slot]);
            for (int t = 0; t < 200; t++) Step(m);
            Assert.AreEqual(0f, m.World.Suppression[slot]);
        }

        [Test]
        public void Garrison_SpreadsAlongItsTrench()
        {
            using var m = NewMatch();
            Garrison(m, 0, 0, 24);
            for (int t = 0; t < 600; t++) Step(m);
            var w = m.World;
            float minX = float.MaxValue, maxX = float.MinValue, closest = float.MaxValue;
            for (int a = 0; a < w.HighWater; a++)
            {
                if (!w.IsAlive(a) || w.TrenchId[a] != 0) continue;
                minX = math.min(minX, w.Position[a].x); maxX = math.max(maxX, w.Position[a].x);
                Assert.AreEqual(0, m.Map.NavLayers[m.Map.NavIndex(m.Map.NavCellOf(w.Position[a]).x, m.Map.NavCellOf(w.Position[a]).y)] & (byte)TW.Sim.Terrain.NavLayer.Link,
                    "nobody rests on a ladder");
                for (int b = a + 1; b < w.HighWater; b++)
                    if (w.IsAlive(b) && w.TrenchId[b] == 0) closest = math.min(closest, math.distance(w.Position[a], w.Position[b]));
            }
            Assert.Greater(maxX - minX, 14f, "24 men do not stay in one clump by the ladder");
            Assert.Greater(closest, 1.2f, "men stand apart along the trench");
        }

        [Test]
        public void UndefendedTrench_IsCaptured_AndTheHqBehindItEndsTheMatch()
        {
            using var m = NewMatch();
            SpawnAssault(m, 0, 5, TrenchZ(m, 1) - 30f, 1);
            bool trenchCaptured = false;
            for (int t = 0; t < 800 && !trenchCaptured; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int e = 0; e < ev.Length; e++) if (ev[e].Type == SimEventType.TrenchCaptured && ev[e].A == 1 && ev[e].B == 0) trenchCaptured = true;
            }
            Assert.IsTrue(trenchCaptured, "five men alone in the enemy trench capture it");
            Assert.AreEqual(0, m.Fields.Trenches[1].OwnerTeam);
            Assert.AreEqual(1, m.Fields.FrontTrench(0), "the captured trench is the new front");
            Assert.AreEqual(-1, m.World.WinnerTeam);

            Step(m, new SimCommand { Player = 0, Type = CommandType.TrenchAdvance, A = 1 });   // the new owner's order is accepted
            for (int t = 0; t < 1500 && m.World.WinnerTeam < 0; t++) Step(m);
            Assert.AreEqual(0, m.World.WinnerTeam, "holding the enemy HQ ends the match");
        }

        [Test]
        public void Hq_CannotBeCaptured_WhileTheMainLineHolds()
        {
            using var m = NewMatch();
            short hq = m.Fields.EnemyHq(0);
            int goal = m.Fields.GetGoal(GoalKey.Objective(hq));
            for (int k = 0; k < 5; k++)
            {
                int slot = m.World.Spawn(0, Rifleman, new float3(140f + k * 3f, 0f, m.Map.SizeMeters.y - 60f), 100f, 3f, false);
                m.World.GoalId[slot] = goal;
            }
            for (int t = 0; t < 900; t++) Step(m);
            Assert.AreEqual(-1, m.World.WinnerTeam, "objectives fall in order: main line first");
        }

        static ulong[] Battle(uint seed)
        {
            using var m = NewMatch(seed);
            var hashes = new ulong[1400];
            for (int t = 0; t < hashes.Length; t++)
            {
                if (t < 40 && t % 2 == 0) Step(m, SimCommand.Deploy(m.World.Tick, 0, t % 3), SimCommand.Deploy(m.World.Tick, 1, (t + 1) % 3));
                else if (t == 700) SpawnAndStep(m);
                else Step(m);
                hashes[t] = m.World.LastHash;
            }
            return hashes;
        }

        static void SpawnAndStep(MatchSim m)
        {
            SpawnAssault(m, 0, 12, TrenchZ(m, 1) - 100f, 1);
            SpawnAssault(m, 1, 12, TrenchZ(m, 0) + 100f, 0);
            Step(m);
        }

        [Test]
        public void Combat_IsDeterministic()
        {
            var a = Battle(7);
            var b = Battle(7);
            for (int i = 0; i < a.Length; i++) Assert.AreEqual(a[i], b[i], $"hash diverged at tick {i}");
            var c = Battle(8);
            bool differs = false;
            for (int i = 0; i < a.Length && !differs; i++) differs = a[i] != c[i];
            Assert.IsTrue(differs, "a different seed changes the dice");
        }
    }
}
