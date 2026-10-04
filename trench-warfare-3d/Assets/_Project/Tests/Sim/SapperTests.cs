// Phase: A5 / docs/21 SIM-D (2026-09-28) — the sapper: ordered with a UnitAbility he walks to the point, places for
// three seconds and lays a mine there, spending a charge; a tripwire runs along the heading and length ordered; an order
// that cannot be carried out is rejected and costs nothing; a garrisoned sapper leaves his trench to do it and goes back;
// a man who dies placing lays nothing and his slot's next tenant starts fresh; an enemy who steps on what he laid is
// blown up by it; and it is the same on two worlds. MineTests holds the mines themselves.
using System;
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Units;

namespace TW.Tests
{
    public class SapperTests
    {
        static readonly float3 Open = new float3(150f, 0f, 240f);

        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public readonly List<SimEvent> Log = new List<SimEvent>();
            public SimWorld W => M.World;
            public SapperSystem Sapper => M.Sapper;
            public Rig()
            {
                var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
                M = MatchSim.CreatePlaytest(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
                M.World.HashInterval = 1;
            }
            public int Man(byte archetype, float3 at, byte team = 0, float hp = -1f)
            {
                var e = W.Units.Roster[archetype];
                return W.Spawn(team, archetype, at, hp < 0f ? e.Hp : hp, e.Speed, false);
            }
            public void Step(params SimCommand[] cmds)
            {
                using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
                M.Step(arr);
                var ev = W.Events.Events;
                for (int k = 0; k < ev.Length; k++) Log.Add(ev[k]);
            }
            public bool RunUntil(int maxTicks, Func<bool> done)
            {
                for (int t = 0; t < maxTicks && !done(); t++) Step();
                return done();
            }
            public int Count(SimEventType type, int a = int.MinValue, int b = int.MinValue)
            {
                int n = 0;
                foreach (var e in Log) if (e.Type == type && (a == int.MinValue || e.A == a) && (b == int.MinValue || e.B == b)) n++;
                return n;
            }
            public SimEvent First(SimEventType type)
            {
                foreach (var e in Log) if (e.Type == type) return e;
                Assert.Fail("no " + type + " event"); return default;
            }
            public void Dispose() => M.Dispose();
        }

        static SimCommand Lay(SimWorld w, byte player, int slot, UnitAbilityId id, float3 at, int heading = 0, int length = 0)
            => new SimCommand { Tick = w.Tick, Player = player, Type = CommandType.UnitAbility, A = slot, B = (int)id | (AbilityArgs.Pack(heading, 0, length) << 8), Pos = at };

        [Test]
        public void HeWalksToThePointAndLaysAMineThereSpendingACharge()
        {
            using var r = new Rig();
            Assert.IsTrue(r.M.Mines.Lies(Open), "the test spot is open ground");
            int s = r.Man(InfantryArchetype.Sapper, Open + new float3(0f, 0f, -20f));
            r.Step();
            Assert.AreEqual(2, r.Sapper.ChargesOf(r.W, s), "his spec's two charges");

            r.Step(Lay(r.W, 0, s, UnitAbilityId.LayMine, Open));
            Assert.AreEqual(0, r.Count(SimEventType.CommandRejected));
            Assert.AreEqual(1, r.Count(SimEventType.SapperOrdered, s, (int)UnitAbilityId.LayMine), "SapperOrdered (a = slot, b = the ability)");
            Assert.AreEqual(SapperSystem.Walking, r.Sapper.Phase[s]);

            Assert.IsTrue(r.RunUntil(400, () => r.Count(SimEventType.SapperLaying, s) > 0), "he reaches the spot and starts placing");
            uint started = r.W.Tick;
            Assert.AreEqual(0, r.Count(SimEventType.MinePlaced), "placing takes its three seconds");
            Assert.Less(math.distance(r.W.Position[s].xz, Open.xz), 3f, "he stands in the point's own nav cell");
            Assert.IsTrue(r.RunUntil(120, () => r.Count(SimEventType.MinePlaced) > 0), "and lays it");
            Assert.That((int)(r.W.Tick - started), Is.InRange(55, 65), "after LaySeconds");
            var placed = r.First(SimEventType.MinePlaced);
            Assert.AreEqual(0, placed.B, "the layer's side");
            Assert.AreEqual(Open.x, placed.Pos.x, 0.01f); Assert.AreEqual(Open.z, placed.Pos.z, 0.01f);
            Assert.AreEqual((float)MineKind.Mine, placed.Scalar);
            Assert.AreEqual(1, r.Sapper.ChargesOf(r.W, s), "a charge spent");
            Assert.AreEqual(SapperSystem.Idle, r.Sapper.Phase[s]);
            Assert.AreEqual(1, r.M.Mines.Live);

            // the second and last charge, then nothing
            r.Step(Lay(r.W, 0, s, UnitAbilityId.LayMine, Open + new float3(6f, 0f, 0f)));
            Assert.IsTrue(r.RunUntil(400, () => r.Count(SimEventType.MinePlaced) > 1), "the second mine");
            Assert.AreEqual(0, r.Sapper.ChargesOf(r.W, s));
            r.Step(Lay(r.W, 0, s, UnitAbilityId.LayMine, Open + new float3(12f, 0f, 0f)));
            Assert.AreEqual(1, r.Count(SimEventType.CommandRejected), "no charges left: rejected");
            Assert.AreEqual(2, r.Count(SimEventType.SapperOrdered));
        }

        [Test]
        public void AnEnemyWhoStepsOnWhatHeLaidIsBlownUpByItAndHisOwnSideIsNot()
        {
            using var r = new Rig();
            int s = r.Man(InfantryArchetype.Sapper, Open + new float3(0f, 0f, -8f));
            r.Step();
            r.Step(Lay(r.W, 0, s, UnitAbilityId.LayMine, Open));
            Assert.IsTrue(r.RunUntil(400, () => r.Count(SimEventType.MinePlaced) > 0));
            int mine = r.First(SimEventType.MinePlaced).A;
            r.RunUntil(MineSystem.ArmTicks + 2, () => false);
            Assert.AreEqual((int)MineState.Armed, r.M.Mines.Mines[mine].State, "armed, with its layer standing beside it");
            int enemy = r.W.Spawn(1, 0, Open + new float3(0.3f, 0f, 0f), 100f, 0f, false);
            r.Step(); r.Step();
            Assert.AreEqual(1, r.Count(SimEventType.MineTriggered, mine, enemy));
            Assert.IsFalse(r.W.IsAlive(enemy), "260 on a 100 hp man");
        }

        [Test]
        public void ATripwireIsLaidAlongTheHeadingAndLengthOrdered()
        {
            using var r = new Rig();
            int s = r.Man(InfantryArchetype.Sapper, Open + new float3(0f, 0f, -10f));
            r.Step();
            r.Step(Lay(r.W, 0, s, UnitAbilityId.LayTripwire, Open, heading: 90, length: 8));
            var ordered = r.First(SimEventType.SapperOrdered);
            Assert.AreEqual(8f, ordered.Dir.x, 0.01f, "dir = heading x length: 8 m along +X"); Assert.AreEqual(0f, ordered.Dir.z, 0.01f);
            Assert.IsTrue(r.RunUntil(500, () => r.Count(SimEventType.MinePlaced) > 0));
            var wire = r.M.Mines.Mines[r.First(SimEventType.MinePlaced).A];
            Assert.AreEqual((int)MineKind.Tripwire, wire.Kind);
            Assert.AreEqual(1f, wire.Dir.x, 0.001f); Assert.AreEqual(0f, wire.Dir.z, 0.001f);
            Assert.AreEqual(8f, wire.Length, 0.001f);

            // no length given: the default
            int s2 = r.Man(InfantryArchetype.Sapper, Open + new float3(30f, 0f, -10f));
            r.Step();
            r.Step(Lay(r.W, 0, s2, UnitAbilityId.LayTripwire, Open + new float3(30f, 0f, 0f), heading: 0));
            Assert.IsTrue(r.RunUntil(500, () => r.Count(SimEventType.MinePlaced) > 1));
            Assert.AreEqual((float)SapperSystem.DefaultTripwireMetres, r.M.Mines.Mines[1].Length, 0.001f);
        }

        [Test]
        public void AnOrderThatCannotBeCarriedOutIsRejectedAndCostsNothing()
        {
            using var r = new Rig();
            int s = r.Man(InfantryArchetype.Sapper, Open + new float3(0f, 0f, -10f));
            int rifle = r.Man(InfantryArchetype.Rifle, Open + new float3(4f, 0f, -10f));
            int theirs = r.Man(InfantryArchetype.Sapper, Open + new float3(8f, 0f, 30f), team: 1);
            int tank = r.W.Spawn(0, VehicleArchetype.Tusk, Open + new float3(-20f, 0f, -10f), 2000f, 0f, true);
            r.Step();
            var trenchCell = r.M.Map.NavCellCenter(r.M.Map.TrenchCells[r.M.Map.Trenches[1].CellStart]);
            Assert.IsFalse(r.M.Mines.Lies(trenchCell), "setup: a trench cell");

            var bad = new (string why, SimCommand cmd)[]
            {
                ("into a trench", Lay(r.W, 0, s, UnitAbilityId.LayMine, trenchCell)),
                ("a tripwire across a trench", Lay(r.W, 0, s, UnitAbilityId.LayTripwire, trenchCell + new float3(0f, 0f, -6f), heading: 0, length: 12)),
                ("the other side's man", Lay(r.W, 0, theirs, UnitAbilityId.LayMine, Open)),
                ("a rifleman", Lay(r.W, 0, rifle, UnitAbilityId.LayMine, Open)),
                ("a machine", Lay(r.W, 0, tank, UnitAbilityId.LayMine, Open)),
                ("a slot nobody holds", Lay(r.W, 0, 3000, UnitAbilityId.LayMine, Open)),
                ("a negative slot", Lay(r.W, 0, -1, UnitAbilityId.LayMine, Open)),
                ("an ability that is not laying", Lay(r.W, 0, s, UnitAbilityId.Bangalore, Open)),
            };
            int rejected = 0;
            foreach (var (why, cmd) in bad)
            {
                r.Step(cmd);
                rejected++;
                Assert.AreEqual(rejected, r.Count(SimEventType.CommandRejected), why + ": rejected");
                Assert.AreEqual(0, r.Count(SimEventType.SapperOrdered), why + ": nobody is ordered");
            }
            Assert.AreEqual(SapperSystem.Idle, r.Sapper.Phase[s]);
            Assert.AreEqual(2, r.Sapper.ChargesOf(r.W, s), "nothing was spent");

            // a second order while he is on an errand is refused too, and a pinned man refuses the first
            r.Step(Lay(r.W, 0, s, UnitAbilityId.LayMine, Open));
            Assert.AreEqual(1, r.Count(SimEventType.SapperOrdered));
            r.Step(Lay(r.W, 0, s, UnitAbilityId.LayMine, Open + new float3(6f, 0f, 0f)));
            Assert.AreEqual(rejected + 1, r.Count(SimEventType.CommandRejected), "already on an errand");
            int pinned = r.Man(InfantryArchetype.Sapper, Open + new float3(-8f, 0f, -10f));
            r.Step();
            r.W.Suppression[pinned] = 100f;
            r.Step(Lay(r.W, 0, pinned, UnitAbilityId.LayMine, Open + new float3(-8f, 0f, 0f)));
            Assert.AreEqual(rejected + 2, r.Count(SimEventType.CommandRejected), "pinned");
        }

        [Test]
        public void AGarrisonedSapperLeavesHisTrenchToLayAndGoesBack()
        {
            using var r = new Rig();
            var w = r.W;
            // deployed the ordinary way he walks up and garrisons his side's rear trench
            var cfg = w.Config;
            int s = r.Man(InfantryArchetype.Sapper, w.Rally[0]);
            Assert.IsTrue(r.RunUntil(1200, () => w.TrenchId[s] >= 0), "setup: he garrisons a trench");
            short trench = w.TrenchId[s];
            float3 spot = w.Position[s] + new float3(0f, 0f, 14f);
            Assert.IsTrue(r.M.Mines.Lies(spot), "setup: open ground ahead of the trench");

            r.Step(Lay(w, 0, s, UnitAbilityId.LayMine, spot));
            Assert.AreEqual(1, r.Count(SimEventType.SapperOrdered, s));
            Assert.AreEqual(1, r.Count(SimEventType.UnitLeftTrench, s, trench), "over the top, as on an advance");
            Assert.AreEqual(-1, w.TrenchId[s]);
            Assert.AreNotEqual(0u, w.Flags[s] & (uint)UnitFlags.Exposed);
            Assert.IsTrue(r.RunUntil(600, () => r.Count(SimEventType.MinePlaced) > 0), "he lays it");
            Assert.IsTrue(r.RunUntil(1200, () => w.TrenchId[s] == trench), "and goes back to the trench he left");
        }

        [Test]
        public void AnAdvanceThatTakesHimEndsTheErrandAndSpendsNothing()
        {
            using var r = new Rig();
            var w = r.W;
            int s = r.Man(InfantryArchetype.Sapper, Open + new float3(0f, 0f, -30f));
            r.Step();
            r.Step(Lay(w, 0, s, UnitAbilityId.LayMine, Open));
            Assert.AreEqual(SapperSystem.Walking, r.Sapper.Phase[s]);
            w.GoalId[s] = r.M.Fields.GetGoal(GoalKey.Trench(2));   // what an order from anyone else does to him
            r.Step();
            Assert.AreEqual(SapperSystem.Idle, r.Sapper.Phase[s], "the errand is over");
            r.RunUntil(300, () => false);
            Assert.AreEqual(0, r.Count(SimEventType.MinePlaced));
            Assert.AreEqual(2, r.Sapper.ChargesOf(w, s));
        }

        [Test]
        public void AManWhoDiesPlacingLaysNothingAndHisSlotStartsFresh()
        {
            using var r = new Rig();
            var w = r.W;
            int s = r.Man(InfantryArchetype.Sapper, Open + new float3(0f, 0f, -6f));
            r.Step();
            r.Step(Lay(w, 0, s, UnitAbilityId.LayMine, Open));
            Assert.IsTrue(r.RunUntil(300, () => r.Count(SimEventType.SapperLaying, s) > 0));
            r.RunUntil(10, () => false);
            w.Despawn(s);
            r.RunUntil(100, () => false);
            Assert.AreEqual(0, r.Count(SimEventType.MinePlaced), "a dead man lays nothing");

            int next = r.Man(InfantryArchetype.Sapper, Open + new float3(20f, 0f, -6f));
            Assert.AreEqual(s, next, "setup: the slot is re-used");
            r.Step();
            Assert.AreEqual(SapperSystem.Idle, r.Sapper.Phase[next]);
            Assert.AreEqual(2, r.Sapper.ChargesOf(w, next), "the next man has his own two");
            int rifle = r.W.Spawn(0, 0, Open + new float3(40f, 0f, -6f), 100f, 0f, false);
            r.Step();
            Assert.AreEqual(0, r.Sapper.ChargesOf(w, rifle), "and a rifleman none");
        }

        [Test, Category("Long")]
        public void ManyErrandsNeverRunTheGoalTableOut()
        {
            using var r = new Rig();
            var w = r.W;
            // more errands, one after another, than the table has goals: finished cell goals are made over
            for (int k = 0; k < FlowFieldManager.MaxGoals + 8; k++)
            {
                int s = r.Man(InfantryArchetype.Sapper, Open + new float3(-60f + k * 3f, 0f, -6f));
                r.Step();
                r.Step(Lay(w, 0, s, UnitAbilityId.LayMine, Open + new float3(-60f + k * 3f, 0f, 0f)));
                int want = k + 1;
                Assert.IsTrue(r.RunUntil(400, () => r.Count(SimEventType.MinePlaced) >= want), $"errand {k} laid its mine");
                w.Despawn(s);
            }
            Assert.AreEqual(0, r.Count(SimEventType.CommandRejected));
            Assert.LessOrEqual(r.M.Fields.GoalCount, FlowFieldManager.MaxGoals);
        }

        [Test]
        public void ItIsTheSameOnTwoWorlds()
        {
            ulong[] Play()
            {
                using var r = new Rig();
                var w = r.W;
                int a = r.Man(InfantryArchetype.Sapper, Open + new float3(0f, 0f, -12f));
                int b = r.Man(InfantryArchetype.Sapper, Open + new float3(20f, 0f, 40f), team: 1);
                for (int k = 0; k < 4; k++) r.Man(InfantryArchetype.Rifle, Open + new float3(-10f + k * 3f, 0f, 60f), team: 1);
                var hashes = new ulong[400];
                for (int t = 0; t < hashes.Length; t++)
                {
                    if (t == 2) r.Step(Lay(w, 0, a, UnitAbilityId.LayMine, Open), Lay(w, 1, b, UnitAbilityId.LayTripwire, Open + new float3(20f, 0f, 30f), heading: 90, length: 10));
                    else r.Step();
                    hashes[t] = w.LastHash;
                }
                Assert.AreEqual(2, r.Count(SimEventType.SapperOrdered), "both orders were taken");
                Assert.GreaterOrEqual(r.Count(SimEventType.MinePlaced), 1, "and something was laid inside the window");
                return hashes;
            }
            var one = Play(); var two = Play();
            for (int t = 0; t < one.Length; t++) Assert.AreEqual(one[t], two[t], $"the worlds part at tick {t}");
        }
    }
}
