// Phase: A5 (docs/21 phase 5) — the patterns: an HE barrage lands as a disc, a line or a box; a pattern the ability
// does not offer is refused; the creeping barrage lifts forward along its heading and spares the men following it
// but nobody else; creeping gas steps its sources up the field.
using System;
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class BarragePatternTests
    {
        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public SimWorld W => M.World;
            public Rig(int silver = 100000, uint seed = 0xC0FFEE)
            {
                var cfg = SimConfig.Default; cfg.StartingSilver = silver; cfg.Seed = seed;
                M = MatchSim.CreatePlaytest(cfg);
                M.Bombardment.ShellsPerMinute = 0f;
            }
            public int Man(float3 at, float hp, byte team = 1, byte archetype = 0) => W.Spawn(team, archetype, at, hp, 0f, false);
            public void Step(params SimCommand[] cmds)
            {
                using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
                M.Step(arr);
            }
            public static SimCommand Support(byte player, OffMapAbilityId id, float3 at, int heading = 0, int pattern = 0, int length = 0)
                => new SimCommand { Player = player, Type = CommandType.SupportFire, A = (int)id, B = AbilityArgs.Pack(heading, pattern, length), Pos = at };
            public int Count(SimEventType type)
            {
                int n = 0;
                var ev = W.Events.Events;
                for (int i = 0; i < ev.Length; i++) if (ev[i].Type == type) n++;
                return n;
            }
            public void Collect(SimEventType type, List<SimEvent> into)
            {
                var ev = W.Events.Events;
                for (int i = 0; i < ev.Length; i++) if (ev[i].Type == type) into.Add(ev[i]);
            }
            public List<SimEvent> RunCollecting(SimEventType type, int ticks)
            {
                var list = new List<SimEvent>();
                for (int t = 0; t < ticks; t++) { Step(); Collect(type, list); }
                return list;
            }
            public void Dispose() => M.Dispose();
        }

        static readonly float3 Start = new float3(60f, 0f, 200f);    // open ground, the line running +X
        static readonly float3 Centre = new float3(150f, 0f, 200f);  // open ground, the advance running +Z

        [Test]
        public void TheDiscPatternStaysInsideItsRadius()
        {
            using var r = new Rig();
            r.Step(Rig.Support(0, OffMapAbilityId.HeBarrage, Centre));
            var shells = r.RunCollecting(SimEventType.Explosion, 80 + 120 + 4);
            Assert.AreEqual(12, shells.Count);
            foreach (var e in shells) Assert.LessOrEqual(math.distance(e.Pos.xz, Centre.xz), 25.01f, "inside the 25 m disc");
        }

        [Test]
        public void TheLinePatternLaysTheShellsAlongTheHeading()
        {
            using var r = new Rig();
            r.Step(Rig.Support(0, OffMapAbilityId.HeBarrage, Start, heading: 90, pattern: AbilityPattern.Line, length: 60));
            var shells = r.RunCollecting(SimEventType.Explosion, 80 + 120 + 4);
            Assert.AreEqual(12, shells.Count);
            foreach (var e in shells)
            {
                Assert.LessOrEqual(math.abs(e.Pos.z - Start.z), 4.01f, "four metres either side of the line");
                Assert.That(e.Pos.x, Is.InRange(Start.x, Start.x + 60f), "along the drag");
                Assert.AreEqual(1f, e.Dir.z, 1e-3f, "the shells still arrive from the battery behind the line");
            }
        }

        [Test]
        public void TheBoxPatternFillsSixteenMetresAcross()
        {
            using var r = new Rig();
            r.Step(Rig.Support(0, OffMapAbilityId.HeBarrage, Start, heading: 90, pattern: AbilityPattern.Box, length: 60));
            var shells = r.RunCollecting(SimEventType.Explosion, 80 + 120 + 4);
            Assert.AreEqual(12, shells.Count);
            foreach (var e in shells)
            {
                Assert.LessOrEqual(math.abs(e.Pos.z - Start.z), OffMapAbilitySystem.BoxHalfWidth + 0.01f);
                Assert.That(e.Pos.x, Is.InRange(Start.x, Start.x + 60f));
            }
        }

        [Test]
        public void APatternTheAbilityDoesNotOfferIsRefused()
        {
            using var r = new Rig();
            int silver = r.W.Silver[0];
            r.Step(Rig.Support(0, OffMapAbilityId.HeBarrage, Start, heading: 90, pattern: 3, length: 60));
            Assert.AreEqual(1, r.Count(SimEventType.CommandRejected));
            Assert.AreEqual(0, r.M.Abilities.Scheduled.Length);
            Assert.GreaterOrEqual(r.W.Silver[0], silver, "nothing was paid");
            r.Step(Rig.Support(0, OffMapAbilityId.SmokeScreen, Start, heading: 90, pattern: 1, length: 40));
            Assert.AreEqual(1, r.Count(SimEventType.CommandRejected), "the smoke screen has only its plain form");
            Assert.AreEqual(0, r.M.Abilities.Scheduled.Length);
            Assert.IsTrue(OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.HeBarrage, out var he));
            Assert.IsTrue(he.Offers(0) && he.Offers(AbilityPattern.Line) && he.Offers(AbilityPattern.Box) && !he.Offers(3) && !he.Offers(-1) && !he.Offers(16));
        }

        [Test]
        public void TheCreepingBarrageLiftsForwardAlongItsHeading()
        {
            using var r = new Rig();
            Assert.IsTrue(OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.CreepingBarrage, out var stats));
            r.Step(Rig.Support(0, OffMapAbilityId.CreepingBarrage, Centre, heading: 0, length: 60));
            Assert.AreEqual(40, r.M.Abilities.Scheduled.Length, "ten lifts of four");
            var fired = new List<SimEvent>(); r.Collect(SimEventType.AbilityFired, fired);
            Assert.AreEqual(1, fired.Count);
            Assert.AreEqual(60f, fired[0].Dir.z, 0.05f); Assert.AreEqual(10f, fired[0].Scalar);

            var shells = r.RunCollecting(SimEventType.Explosion, stats.WarmupTicks + 9 * stats.StepTicks + 4);
            Assert.AreEqual(40, shells.Count);
            var byTick = new SortedDictionary<uint, List<SimEvent>>();
            foreach (var e in shells) { if (!byTick.TryGetValue(e.Tick, out var l)) byTick[e.Tick] = l = new List<SimEvent>(); l.Add(e); }
            Assert.AreEqual(10, byTick.Count, "one tick per lift");
            uint expect = (uint)stats.WarmupTicks;
            float firstZ = 0f, lastZ = 0f; int lift = 0;
            foreach (var kv in byTick)
            {
                Assert.AreEqual(expect, kv.Key, $"lift {lift} lands {stats.StepTicks} ticks after the one before");
                Assert.AreEqual(4, kv.Value.Count);
                float z = 0f;
                foreach (var e in kv.Value)
                {
                    z += e.Pos.z;
                    Assert.LessOrEqual(math.abs(e.Pos.x - Centre.x), 10.01f, "ten metres either side of the advance");
                    Assert.LessOrEqual(math.abs(e.Pos.z - (Centre.z + lift * stats.StepMetres)), OffMapAbilitySystem.LiftAlongScatter + 0.01f);
                    Assert.AreEqual(1f, e.Dir.z, 1e-3f, "the lift's shells run with the advance");
                }
                z /= 4f;
                if (lift == 0) firstZ = z;
                lastZ = z;
                expect += (uint)stats.StepTicks; lift++;
            }
            Assert.GreaterOrEqual(lastZ - firstZ, 50f, "the last lift is fifty metres and more up the field from the first");
        }

        [Test]
        public void TheLiftSparesTheMenFollowingItAndNobodyElse()
        {
            // the caller's own men five metres behind the first lift: untouched, not even shaken
            using (var own = new Rig())
            {
                var men = new int[5];
                for (int k = 0; k < 5; k++) men[k] = own.Man(new float3(Centre.x - 10f + k * 5f, 0f, Centre.z - 5f), 100f, team: 0);
                own.Step(Rig.Support(0, OffMapAbilityId.CreepingBarrage, Centre, heading: 0, length: 60));
                for (int t = 0; t < 900; t++) own.Step();
                foreach (int s in men)
                {
                    Assert.IsTrue(own.W.IsAlive(s));
                    Assert.AreEqual(100f, own.W.Hp[s], "the battery knows where its own men are");
                    Assert.AreEqual(0f, own.W.Suppression[s]);
                }
            }
            // the enemy's men in the same place: the first lift finds them
            using (var enemy = new Rig())
            {
                var men = new int[5];
                for (int k = 0; k < 5; k++) men[k] = enemy.Man(new float3(Centre.x - 10f + k * 5f, 0f, Centre.z - 5f), 100f, team: 1);
                enemy.Step(Rig.Support(0, OffMapAbilityId.CreepingBarrage, Centre, heading: 0, length: 60));
                for (int t = 0; t < 130; t++) enemy.Step();
                bool hurt = false;
                foreach (int s in men) if (!enemy.W.IsAlive(s) || enemy.W.Hp[s] < 100f) hurt = true;
                Assert.IsTrue(hurt, "behind the line means nothing to the other side");
            }
            // the caller's own men just AHEAD of the first lift (past its along-scatter): as dead as anyone
            using (var ahead = new Rig())
            {
                var men = new int[5];
                for (int k = 0; k < 5; k++) men[k] = ahead.Man(new float3(Centre.x - 10f + k * 5f, 0f, Centre.z + OffMapAbilitySystem.LiftAlongScatter + 2f), 100f, team: 0);
                ahead.Step(Rig.Support(0, OffMapAbilityId.CreepingBarrage, Centre, heading: 0, length: 60));
                for (int t = 0; t < 130; t++) ahead.Step();
                bool hurt = false;
                foreach (int s in men) if (!ahead.W.IsAlive(s) || ahead.W.Hp[s] < 100f) hurt = true;
                Assert.IsTrue(hurt, "a friend ahead of the burst is not spared");
            }
        }

        [Test]
        public void CreepingGasStepsItsSourcesUpTheField()
        {
            using var r = new Rig();
            Assert.IsTrue(OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.ChlorineGas, out var stats));
            r.Step(Rig.Support(0, OffMapAbilityId.ChlorineGas, Centre, heading: 0, pattern: AbilityPattern.Creeping, length: 64));
            Assert.AreEqual(8, r.M.Abilities.Scheduled.Length);
            var clouds = r.RunCollecting(SimEventType.GasCloudSpawned, stats.WarmupTicks + 7 * stats.StepTicks + 4);
            Assert.AreEqual(8, clouds.Count, "eight sources");
            for (int s = 0; s < clouds.Count; s++)
            {
                Assert.AreEqual((uint)(stats.WarmupTicks + s * stats.StepTicks), clouds[s].Tick, $"source {s} opens {stats.StepTicks} ticks after the one before");
                Assert.AreEqual(Centre.z + s * stats.StepMetres, clouds[s].Pos.z, 0.05f, "eight metres further each time");
                Assert.AreEqual(Centre.x, clouds[s].Pos.x, 0.05f);
                Assert.AreEqual(OffMapAbilitySystem.CreepingGasConcentration, clouds[s].Scalar);
                Assert.AreEqual(1f, clouds[s].Dir.z, 1e-3f, "the event says which way the cloud creeps");
            }
            Assert.IsTrue(r.M.Gas.Active);
            Assert.Greater(r.M.Gas.ConcentrationAt(Centre + new float3(0f, 0f, 56f)), 5f, "the last source is still fresh");
        }
    }
}
