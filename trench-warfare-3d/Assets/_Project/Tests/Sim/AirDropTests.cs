// Phase: A3 (implemented 2026-09-25) — paratroopers: eight men come down where the drop was called, a flight later,
// on open ground, and go for the nearest enemy trench; the silver is spent and the cooldown runs; a drop on water,
// in a trench, too near the enemy's rear line or by a faction without the ability is refused; and it is the same
// on every machine.
using System.Collections.Generic;
using System.Reflection;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class AirDropTests
    {
        static MatchSim NewMatch(uint seed = 0xC0FFEE, byte factionA = (byte)FactionId.Brass, byte factionB = (byte)FactionId.Iron)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = seed; cfg.FactionA = factionA; cfg.FactionB = factionB;
            return MatchSim.CreateGreybox(cfg);
        }

        static void Step(MatchSim m, params SimCommand[] cmds)
        {
            using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
            m.Step(arr);
        }

        static List<SimEvent> Run(MatchSim m, int ticks)
        {
            var log = new List<SimEvent>();
            for (int t = 0; t < ticks; t++)
            {
                Step(m);
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++) log.Add(ev[k]);
            }
            return log;
        }

        static int Count(List<SimEvent> log, SimEventType type, int b = int.MinValue)
        {
            int n = 0;
            foreach (var e in log) if (e.Type == type && (b == int.MinValue || e.B == b)) n++;
            return n;
        }

        static SimCommand Drop(MatchSim m, byte player, float3 at)
            => new SimCommand { Tick = m.World.Tick, Player = player, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.ParaDrop, Pos = at };

        /// <summary>Open ground on the greybox between the two lines, a safe way short of the enemy rear.</summary>
        static float3 OpenPoint(MatchSim m, byte player)
        {
            short rear = m.Fields.RearTrench((byte)(1 - player));
            var def = m.Map.Trenches[rear];
            float rearZ = m.Map.NavCellCenter(m.Map.TrenchCells[def.CellStart + def.CellCount / 2]).z;
            float z = player == 0 ? rearZ - OffMapAbilitySystem.ParaDropKeepOut - 20f : rearZ + OffMapAbilitySystem.ParaDropKeepOut + 20f;
            for (float x = 40f; x < m.Map.SizeMeters.x - 40f; x += 4f)
            {
                var at = new float3(x, 0f, z);
                bool open = true;
                for (int dz = -8; dz <= 8 && open; dz += 2) for (int dx = -8; dx <= 8; dx += 2)
                    if ((m.Map.LayerAt(at + new float3(dx, 0f, dz)) & (NavLayer.Trench | NavLayer.Blocked | NavLayer.Link)) != 0) { open = false; break; }
                if (open) return at;
            }
            Assert.Fail("setup: no open ground for a drop"); return default;
        }

        [Test]
        public void EightMenComeDownAFlightLaterOnOpenGroundAndGoForTheEnemyTrench()
        {
            using var m = NewMatch();
            var at = OpenPoint(m, 0);
            int silver = m.World.Silver[0];
            OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.ParaDrop, out var stats);
            Step(m, Drop(m, 0, at));
            Assert.AreEqual(silver - stats.Cost, m.World.Silver[0], "paid at once");
            Assert.AreEqual(1, m.World.Events.Events.Length > 0 ? CountType(m, SimEventType.DropInbound) : 0);
            var log = Run(m, stats.WarmupTicks - 2);
            Assert.AreEqual(0, Count(log, SimEventType.DropLanded), "nobody before the flight is over");
            log = Run(m, 3);
            Assert.AreEqual(stats.Men, Count(log, SimEventType.DropLanded, 0));
            Assert.AreEqual(stats.Men, Count(log, SimEventType.UnitSpawned, InfantryArchetype.Para));
            int goal = -1;
            foreach (var e in log)
            {
                if (e.Type != SimEventType.DropLanded) continue;
                int s = e.A;
                Assert.Less(math.distance(m.World.Position[s].xz, at.xz), stats.Radius + 2.5f, "inside the disc, or the open cell next to it");
                Assert.AreEqual(0, (int)(m.Map.LayerAt(m.World.Position[s]) & (NavLayer.Trench | NavLayer.Blocked)), "on open ground");
                Assert.AreNotEqual(0u, m.World.Flags[s] & (uint)UnitFlags.Exposed);
                Assert.GreaterOrEqual(m.World.GoalId[s], 0, "he has somewhere to go");
                goal = m.World.GoalId[s];
            }
            var key = m.Fields.Goals[goal];
            Assert.AreEqual(GoalKind.Trench, key.Kind);
            Assert.AreNotEqual(0, m.Fields.Trenches[key.Ref].OwnerTeam, "an enemy trench");
            Assert.Greater(m.Abilities.CooldownOf(0, OffMapAbilityId.ParaDrop), 0);
        }

        static int CountType(MatchSim m, SimEventType t)
        {
            int n = 0; var ev = m.World.Events.Events;
            for (int k = 0; k < ev.Length; k++) if (ev[k].Type == t) n++;
            return n;
        }

        // [U12] the drop read the compiled RosterEntry.Para default instead of the match's own table
        [Test]
        public void TheDroppedMenComeDownWithTheMatchTablesHitPoints()
        {
            using var m = NewMatch();
            var at = OpenPoint(m, 0);
            // what this match defines a Para to be, in place of the compiled default
            m.World.Units.Roster[InfantryArchetype.Para] = new RosterEntry { Archetype = InfantryArchetype.Para, Cost = 0, Hp = 222f, Speed = 3.4f };
            OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.ParaDrop, out var stats);
            Step(m, Drop(m, 0, at));
            var log = Run(m, stats.WarmupTicks + 1);
            int men = 0;
            foreach (var e in log)
            {
                if (e.Type != SimEventType.DropLanded) continue;
                men++;
                Assert.AreEqual(222f, m.World.MaxHp[e.A], 0.01f, "[U12] the dropped man came down with the compiled default, not the match table");
            }
            Assert.AreEqual(stats.Men, men, "[U12] the stick landed");
        }

        [Test]
        public void ADropIsRefusedWhereItCannotLandOrByAFactionWithoutIt()
        {
            using var m = NewMatch();
            int silver = m.World.Silver[0];
            // in a trench
            short rear = m.Fields.RearTrench(1);
            var def = m.Map.Trenches[rear];
            float3 inTrench = m.Map.NavCellCenter(m.Map.TrenchCells[def.CellStart + def.CellCount / 2]);
            Step(m, Drop(m, 0, inTrench));
            Assert.AreEqual(1, CountType(m, SimEventType.CommandRejected), "not into a trench");
            // too near the enemy rear
            Step(m, Drop(m, 0, inTrench + new float3(0f, 0f, -OffMapAbilitySystem.ParaDropKeepOut * 0.5f)));
            Assert.AreEqual(1, CountType(m, SimEventType.CommandRejected), "not on the enemy's doorstep");
            // off the map
            Step(m, Drop(m, 0, new float3(-5f, 0f, 50f)));
            Assert.AreEqual(1, CountType(m, SimEventType.CommandRejected));
            Assert.AreEqual(silver, m.World.Silver[0], "nothing was paid for");
            // Iron has no aircraft
            using var iron = NewMatch(0xC0FFEE, (byte)FactionId.Iron, (byte)FactionId.Brass);
            var at = OpenPoint(iron, 0);
            Step(iron, Drop(iron, 0, at));
            Assert.AreEqual(1, CountType(iron, SimEventType.CommandRejected), "Iron cannot call a drop");
            Step(iron, Drop(iron, 1, OpenPoint(iron, 1)));
            Assert.AreEqual(0, CountType(iron, SimEventType.CommandRejected), "Brass can");
        }

        [Test]
        public void TheBarrageAndTheGasStillWorkForBothFactions()
        {
            using var m = NewMatch();
            var at = OpenPoint(m, 0);
            Step(m, new SimCommand { Tick = m.World.Tick, Player = 0, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.HeBarrage, Pos = at },
                    new SimCommand { Tick = m.World.Tick, Player = 1, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.ChlorineGas, Pos = at });
            Assert.AreEqual(0, CountType(m, SimEventType.CommandRejected));
            Assert.AreEqual(2, CountType(m, SimEventType.AbilityFired));
        }

        /// <summary>[Z011.2] One landing question, askable from outside the sim: the rule lived only inside the command
        /// handler, so the HUD spent the aim on a drop the sim then refused. Reflection on purpose: the test must compile
        /// against the tree before the fix (CanLand private, no MatchSim read) so it can be seen red there.</summary>
        [Test]
        public void TheLandingQuestionIsReadableFromOutsideTheSim()
        {
            using var m = NewMatch();
            var inner = typeof(OffMapAbilitySystem).GetMethod("CanLand", BindingFlags.Public | BindingFlags.Instance);
            var exposed = typeof(MatchSim).GetMethod("CanDropLand", BindingFlags.Public | BindingFlags.Instance);
            Assert.IsNotNull(inner, "[Z011.2] the landing rule must be a public read on the ability system");
            Assert.IsNotNull(exposed, "[Z011.2] MatchSim must expose the landing question");
            short rear = m.Fields.RearTrench(1);
            var def = m.Map.Trenches[rear];
            float3 inTrench = m.Map.NavCellCenter(m.Map.TrenchCells[def.CellStart + def.CellCount / 2]);
            var open = OpenPoint(m, 0);
            Assert.IsFalse((bool)exposed.Invoke(m, new object[] { 0, inTrench }), "[Z011.2] a trench cell answers no, as the sim's Reject does");
            Assert.IsTrue((bool)exposed.Invoke(m, new object[] { 0, open }), "[Z011.2] open ground clear of the rear line answers yes");
            Assert.IsFalse((bool)inner.Invoke(m.Abilities, new object[] { 0, inTrench }), "[Z011.2] the system's own read agrees");
            Step(m, Drop(m, 0, inTrench));
            Assert.AreEqual(1, CountType(m, SimEventType.CommandRejected), "[Z011.2] the sim still refuses the trench");
            Step(m, Drop(m, 0, open));
            Assert.AreEqual(0, CountType(m, SimEventType.CommandRejected), "[Z011.2] and still accepts the open point");
        }

        /// <summary>A match with room for only a few men, and exact silver (no income), so the sums are arithmetic.</summary>
        static MatchSim SmallMatch(int maxSlots)
        {
            var cfg = SimConfig.Default;
            cfg.Seed = 0xC0FFEEu; cfg.StartingSilver = 100000; cfg.SilverPerSecond = 0f;
            cfg.FactionA = (byte)FactionId.Brass; cfg.FactionB = (byte)FactionId.Iron;
            cfg.MaxSlots = maxSlots;
            return MatchSim.CreateGreybox(cfg);
        }

        static void Deploy(MatchSim m, byte player, int slot, int times = 1)
        {
            var list = new SimCommand[times];
            for (int i = 0; i < times; i++) list[i] = SimCommand.Deploy(m.World.Tick, player, slot);
            using var cmds = new NativeArray<SimCommand>(list, Allocator.Temp);
            m.Step(cmds);
        }

        // the stick is paid for and the cooldown runs the moment it is called, and DropInbound promises stats.Men men:
        // when the field fills up partway through the landing the rest must be paid back, not silently dropped.
        [Test]
        public void AStickWithRoomForOnlyPartOfItIsPaidBackTheRest()
        {
            using var m = SmallMatch(3);
            var at = OpenPoint(m, 0);
            OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.ParaDrop, out var stats);
            int silver = m.World.Silver[0];
            Step(m, Drop(m, 0, at));
            var log = Run(m, stats.WarmupTicks);
            Assert.AreEqual(3, Count(log, SimEventType.DropLanded, 0), "three slots, three men");
            Assert.AreEqual(1, Count(log, SimEventType.DropLandedShort), "one word that the stick landed short");
            foreach (var e in log)
            {
                if (e.Type != SimEventType.DropLandedShort) continue;
                Assert.AreEqual(0, e.A, "the caller");
                Assert.AreEqual(stats.Men - 3, e.B, "five never came down");
                Assert.AreEqual(3f, e.Dir.x, "three did");
                Assert.AreEqual(260 * 5 / 8, e.Scalar, "260 * 5 / 8 = 162, integer: 162.5 cut down");
            }
            Assert.AreEqual(silver - stats.Cost + 162, m.World.Silver[0], "the five men's silver is back");
            // 1500 on the command tick (after that tick's decrement), then 100 flight ticks each take one: 1400 left
            // when Land runs, and 1400 * 3 / 8 = 525 of the wait is kept for the three who did land
            Assert.AreEqual(525, m.Abilities.CooldownOf(0, OffMapAbilityId.ParaDrop), "the wait goes back with the silver");
        }

        [Test]
        public void AStickWithNoRoomAtAllGetsTheWholeCostAndTheWholeWaitBack()
        {
            using var m = SmallMatch(2);
            var at = OpenPoint(m, 0);
            OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.ParaDrop, out var stats);
            Deploy(m, 0, 0, 2);   // the field is full before the drop is even called
            int silver = m.World.Silver[0];
            Step(m, Drop(m, 0, at));
            var log = Run(m, stats.WarmupTicks);
            Assert.AreEqual(0, Count(log, SimEventType.DropLanded), "nowhere to put a man");
            Assert.AreEqual(1, Count(log, SimEventType.DropLandedShort));
            foreach (var e in log)
            {
                if (e.Type != SimEventType.DropLandedShort) continue;
                Assert.AreEqual(stats.Men, e.B, "the whole stick");
                Assert.AreEqual(0f, e.Dir.x, "nobody landed");
                Assert.AreEqual(stats.Cost, e.Scalar, "the whole cost back");
            }
            Assert.AreEqual(silver, m.World.Silver[0], "the call cost nothing in the end");
            Assert.AreEqual(0, m.Abilities.CooldownOf(0, OffMapAbilityId.ParaDrop), "nobody landed: ready at once");
        }

        [Test]
        public void TheDropIsTheSameOnEveryMachine()
        {
            ulong Play()
            {
                using var m = NewMatch();
                Step(m, Drop(m, 0, OpenPoint(m, 0)));
                Run(m, 400);
                return m.World.Hash();
            }
            Assert.AreEqual(Play(), Play());
        }
    }
}
