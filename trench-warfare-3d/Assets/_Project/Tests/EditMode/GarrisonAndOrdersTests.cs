// Phase: A1 (implemented) — deployed infantry walk to their front trench through links and garrison; trench orders
// (>> / ↩ / lock) move them on, back, or through; vehicles cross trenches without links; orders are validated per
// player; and all of it is deterministic.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class GarrisonAndOrdersTests
    {
        const int Rifleman = 0, Tank = 4;

        static MatchSim NewMatch(int silver = 100000, uint seed = 0xC0FFEE)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = silver; cfg.Seed = seed;
            return MatchSim.CreateGreybox(cfg);
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static void Run(MatchSim m, int ticks, LinkWatch watch = null)
        {
            for (int t = 0; t < ticks; t++) { Step(m); watch?.Check(m); }
        }

        static void Deploy(MatchSim m, byte player, int slot, int count)
        {
            for (int k = 0; k < count; k++) Step(m, SimCommand.Deploy(m.World.Tick, player, slot));
        }

        static SimCommand Order(byte player, CommandType type, int trench, int b = 0)
            => new SimCommand { Player = player, Type = type, A = trench, B = b };

        static int Count(SimWorld w, System.Func<int, bool> pred)
        {
            int n = 0;
            for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && pred(i)) n++;
            return n;
        }

        static bool HasEvent(SimWorld w, SimEventType type)
        {
            for (int i = 0; i < w.Events.Events.Length; i++) if (w.Events.Events[i].Type == type) return true;
            return false;
        }

        /// <summary>Asserts every trench entry by an infantry unit happens through a Link cell.</summary>
        sealed class LinkWatch
        {
            readonly Dictionary<int, int> lastCell = new Dictionary<int, int>();
            public int Entries;
            public void Check(MatchSim m)
            {
                var w = m.World; var map = m.Map;
                for (int i = 0; i < w.HighWater; i++)
                {
                    if (!w.IsAlive(i) || (w.Flags[i] & (uint)UnitFlags.Vehicle) != 0) continue;
                    var c = map.NavCellOf(w.Position[i]);
                    int cell = map.NavIndex(c.x, c.y);
                    if (lastCell.TryGetValue(i, out int prev) && prev != cell)
                    {
                        var from = (NavLayer)map.NavLayers[prev];
                        var to = (NavLayer)map.NavLayers[cell];
                        if ((to & NavLayer.Trench) != 0 && (from & NavLayer.Trench) == 0)
                        {
                            Entries++;
                            Assert.IsTrue((to & NavLayer.Link) != 0 || (from & NavLayer.Link) != 0, $"unit {i} entered a trench at cell {cell} without a link (tick {w.Tick})");
                        }
                    }
                    lastCell[i] = cell;
                }
            }
        }

        [Test]
        public void DeployedInfantry_WalkToFrontTrench_ThroughLinks_AndGarrison()
        {
            using var m = NewMatch();
            Deploy(m, 0, Rifleman, 20);
            var watch = new LinkWatch();
            Run(m, 1000, watch);   // 110 m at 3 m/s ≈ 740 ticks
            var w = m.World;
            Assert.AreEqual(20, w.AliveCount);
            Assert.AreEqual(20, Count(w, i => w.TrenchId[i] == 0), "every rifleman garrisons trench 0");
            Assert.AreEqual(20, Count(w, i => w.GoalId[i] < 0 && w.Layer[i] == (byte)NavLayer.Trench && w.StanceOf[i] == (byte)Stance.Crouch));
            Assert.AreEqual(20, Count(w, i => (w.Flags[i] & (uint)UnitFlags.InTrench) != 0 && (w.Flags[i] & (uint)UnitFlags.Exposed) == 0));
            Assert.GreaterOrEqual(watch.Entries, 20, "each unit entered the trench once");
            Assert.AreEqual(20, m.Fields.Trenches[0].GarrisonCount);
        }

        [Test]
        public void Advance_SendsGarrisonOverTheTop_ToTheEnemyTrench()
        {
            using var m = NewMatch();
            Deploy(m, 0, Rifleman, 20);
            Run(m, 1000);
            var w = m.World;
            Assert.AreEqual(20, Count(w, i => w.TrenchId[i] == 0));

            Step(m, Order(0, CommandType.TrenchAdvance, 0));
            Assert.IsTrue(HasEvent(w, SimEventType.UnitLeftTrench), "leaving the trench is reported");
            Assert.AreEqual(20, Count(w, i => w.TrenchId[i] < 0 && w.SourceTrench[i] == 0 && (w.Flags[i] & (uint)UnitFlags.Exposed) != 0));

            var watch = new LinkWatch();
            Run(m, 40, watch);
            Assert.Greater(Count(w, i => w.Layer[i] == (byte)NavLayer.Surface), 0, "units vault onto the surface");
            Assert.Greater(Count(w, i => w.StanceOf[i] == (byte)Stance.Sprint), 0, "exposed units sprint");

            Run(m, 3200, watch);   // 560 m at 4.5 m/s ≈ 2,500 ticks
            Assert.GreaterOrEqual(Count(w, i => w.TrenchId[i] == 1), 18, "the wave garrisons the enemy trench");
            Assert.AreEqual(0, Count(w, i => w.TrenchId[i] == 1 && (w.Flags[i] & (uint)UnitFlags.Exposed) != 0), "garrisoning clears Exposed");
            Assert.GreaterOrEqual(watch.Entries, 18);
        }

        [Test]
        public void Fallback_ReturnsUnitsInTheOpen_ToTheirTrench()
        {
            using var m = NewMatch();
            Deploy(m, 0, Rifleman, 10);
            Run(m, 1000);
            Step(m, Order(0, CommandType.TrenchAdvance, 0));
            Run(m, 400);   // ~90 m into no-man's-land
            var w = m.World;
            Assert.AreEqual(10, Count(w, i => w.TrenchId[i] < 0 && w.Position[i].z > 130f));

            Step(m, Order(0, CommandType.TrenchFallback, 0));
            Run(m, 900);
            Assert.AreEqual(10, Count(w, i => w.TrenchId[i] == 0), "everyone is back in trench 0");
        }

        [Test]
        public void LockedTrench_PassesArrivalsThrough()
        {
            using var m = NewMatch();
            Step(m, Order(0, CommandType.TrenchLock, 0, 1));
            Assert.AreEqual(1, m.Fields.Trenches[0].Locked);
            Deploy(m, 0, Rifleman, 5);
            Run(m, 1400);
            var w = m.World;
            Assert.AreEqual(0, Count(w, i => w.TrenchId[i] == 0), "nobody garrisons a locked trench");
            Assert.AreEqual(5, Count(w, i => w.Position[i].z > 130f), "arrivals continued toward the next goal");
            Assert.AreEqual(5, Count(w, i => w.GoalId[i] >= 0 && m.Fields.Goals[w.GoalId[i]].Kind == GoalKind.Trench && m.Fields.Goals[w.GoalId[i]].Ref == 1));

            Step(m, Order(0, CommandType.TrenchLock, 0, 0));
            Assert.AreEqual(0, m.Fields.Trenches[0].Locked);
        }

        [Test]
        public void SelectAdvance_MovesOnlyMaskedClasses()
        {
            using var m = NewMatch();
            Deploy(m, 0, Rifleman, 6);
            Deploy(m, 0, 1, 6);   // assault
            Run(m, 1100);
            var w = m.World;
            Assert.AreEqual(12, Count(w, i => w.TrenchId[i] == 0));
            Step(m, Order(0, CommandType.TrenchSelectAdvance, 0, 1 << 1));
            Assert.AreEqual(6, Count(w, i => w.TrenchId[i] == 0 && w.Archetype[i] == Rifleman), "riflemen stay");
            Assert.AreEqual(6, Count(w, i => w.TrenchId[i] < 0 && w.Archetype[i] == 1), "assault troops go");
        }

        [Test]
        public void Orders_FromThePlayerWhoDoesNotOwnTheTrench_AreRejected()
        {
            using var m = NewMatch();
            Step(m, Order(1, CommandType.TrenchAdvance, 0));
            Assert.IsTrue(HasEvent(m.World, SimEventType.CommandRejected));
            Step(m, Order(0, CommandType.TrenchAdvance, 99));
            Assert.IsTrue(HasEvent(m.World, SimEventType.CommandRejected));
            Step(m, Order(0, CommandType.TrenchAdvance, 0));
            Assert.IsFalse(HasEvent(m.World, SimEventType.CommandRejected), "own trench, valid id");
        }

        [Test]
        public void Vehicle_CrossesTheTrench_WithoutLinks_AndNeverGarrisons()
        {
            using var m = NewMatch();
            Step(m, SimCommand.Deploy(0, 0, Tank));
            var w = m.World;
            Assert.AreEqual(1, w.AliveCount);
            int tank = 0;
            Assert.IsTrue((w.Flags[tank] & (uint)UnitFlags.Vehicle) != 0);
            Run(m, 2600);   // 1.6 m/s: 110 m to the trench ≈ 1,375 ticks, then across
            Assert.Greater(w.Position[tank].z, 130f, "the tank drove through the trench line");
            Assert.AreEqual(-1, w.TrenchId[tank]);
            Assert.AreEqual((byte)NavLayer.Surface, w.Layer[tank]);
            Assert.AreEqual(GoalKind.Objective, m.Fields.Goals[w.GoalId[tank]].Kind);
            Assert.AreEqual(NavMode.Tracked, m.Fields.Goals[w.GoalId[tank]].Mode);
        }

        [Test]
        public void OrdersAndMovement_AreDeterministic()
        {
            ulong[] RunScript()
            {
                using var m = NewMatch(seed: 42);
                var hashes = new List<ulong>();
                for (uint t = 0; t < 1600; t++)
                {
                    if (t < 30) Step(m, SimCommand.Deploy(t, 0, Rifleman), SimCommand.Deploy(t, 1, Rifleman));
                    else if (t == 900) Step(m, Order(0, CommandType.TrenchAdvance, 0), Order(1, CommandType.TrenchAdvance, 1));
                    else if (t == 1300) Step(m, Order(0, CommandType.TrenchFallback, 0));
                    else if (t == 40) Step(m, SimCommand.Deploy(t, 0, Tank));
                    else Step(m);
                    hashes.Add(m.World.LastHash);
                }
                return hashes.ToArray();
            }
            var a = RunScript();
            var b = RunScript();
            for (int i = 0; i < a.Length; i++) Assert.AreEqual(a[i], b[i], $"diverged at tick {i}");
        }
    }
}
