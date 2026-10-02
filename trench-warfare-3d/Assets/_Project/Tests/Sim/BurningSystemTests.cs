// Phase: A5 (2026-09-26, docs/21 phase 4) — fire in the sim.
// An incendiary burst sets the men inside it and the ground under them alight; a man alight loses hit points every
// tick, is suppressed, leaves his trench and runs, catches from burning ground, and dies of Burning when it is more
// than he had. All of it is hashed and repeats exactly.
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;

namespace TW.Tests
{
    public class BurningSystemTests
    {
        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public SimWorld W => M.World;
            public Rig()
            {
                var cfg = SimConfig.Default;
                M = MatchSim.CreatePlaytest(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
            }
            public int Man(float3 at, float hp = 1000f, byte team = 0) => W.Spawn(team, 0, at, hp, 0f, false);
            public void Step(int ticks = 1)
            {
                for (int t = 0; t < ticks; t++)
                {
                    using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
                    M.Step(none);
                }
            }
            public int Count(SimEventType type, int slot = -1, int b = int.MinValue)
            {
                int n = 0;
                var events = W.Events.Events;
                for (int i = 0; i < events.Length; i++)
                {
                    var e = events[i];
                    if (e.Type != type) continue;
                    if (slot >= 0 && e.A != slot) continue;
                    if (b != int.MinValue && e.B != b) continue;
                    n++;
                }
                return n;
            }
            public bool Burning(int slot) => (W.Flags[slot] & (uint)UnitFlags.Burning) != 0;
            public void Dispose() => M.Dispose();
        }

        static Impact Incendiary(float3 at, float radius, float damage = 20f)
            => new Impact { Pos = at, Radius = radius, Damage = damage, Suppression = 30f, Player = 0, Shape = (int)BlastShape.Incendiary, Source = 1 };

        [Test]
        public void AnIncendiaryBurstSetsTheMenInsideItAlightAndTheGroundUnderThem()
        {
            using var r = new Rig();
            var at = new float3(150f, 0f, 240f);
            int inside = r.Man(at + new float3(2f, 0f, 0f));
            int outside = r.Man(at + new float3(30f, 0f, 0f));

            r.M.Blast.Queue(Incendiary(at, 6f));
            r.Step();

            Assert.IsTrue(r.Burning(inside), "the man inside the burst is alight");
            Assert.Greater(r.M.Burning.AlightUntil[inside], r.W.Tick);
            Assert.IsFalse(r.Burning(outside), "the man thirty metres off is not");
            Assert.AreEqual(1, r.Count(SimEventType.UnitAlight, inside, 1), "one UnitAlight (b = 1) for him");
            Assert.Greater(r.Count(SimEventType.CellBurning), 0, "and the ground under the burst is alight");
            Assert.Greater(r.M.Burning.Cells.Length, 0);
            Assert.LessOrEqual(r.M.Burning.Cells.Length, BurningSystem.MaxCellsPerBurst);
            Assert.IsTrue(r.M.Burning.Active);
        }

        [Test]
        public void AManAlightLosesTwelveHitPointsASecondAndThenTheFireGoesOut()
        {
            using var r = new Rig();
            int man = r.Man(new float3(150f, 0f, 240f));
            float dt = r.W.Config.TickSeconds;
            int ticksForTwoSeconds = (int)math.ceil(2f / dt);

            r.M.Burning.Ignite(r.W, man, 2f);
            Assert.IsTrue(r.Burning(man));
            Assert.AreEqual(1, r.Count(SimEventType.UnitAlight, man, 1));

            r.Step(ticksForTwoSeconds);
            float lost = 1000f - r.W.Hp[man];
            Assert.AreEqual(BurningSystem.BurnDps * 2f, lost, BurningSystem.BurnDps * dt * 1.01f, "twelve a second for two seconds");
            Assert.IsFalse(r.Burning(man), "and then the fire is out");
            Assert.AreEqual(0u, r.M.Burning.AlightUntil[man]);
            Assert.Greater(r.W.Suppression[man], 0f, "burning is suppressing");

            r.Step();
            Assert.AreEqual(lost, 1000f - r.W.Hp[man], 1e-3f, "out means out: no more damage");
        }

        [Test]
        public void ItSaysWhenTheFireGoesOut()
        {
            using var r = new Rig();
            int man = r.Man(new float3(150f, 0f, 240f));
            r.M.Burning.Ignite(r.W, man, 0.5f);
            int seen = 0;
            for (int t = 0; t < 40 && seen == 0; t++) { r.Step(); seen += r.Count(SimEventType.UnitAlight, man, 0); }
            Assert.AreEqual(1, seen, "one UnitAlight (b = 0) when it goes out");
            Assert.IsFalse(r.Burning(man));
        }

        [Test]
        public void AManWhoStandsOnBurningGroundCatches()
        {
            using var r = new Rig();
            var at = new float3(150f, 0f, 240f);
            r.M.Burning.IgniteCell(r.W, at, 5f, 0);
            int man = r.Man(at);
            Assert.IsFalse(r.Burning(man));

            r.Step();

            Assert.IsTrue(r.Burning(man), "burning ground lights the man standing on it");
            Assert.AreEqual(1, r.Count(SimEventType.UnitAlight, man, 1));
        }

        [Test]
        public void AManWhoBurnsToDeathDiesOfBurning()
        {
            using var r = new Rig();
            int man = r.Man(new float3(150f, 0f, 240f), 5f);
            r.M.Burning.Ignite(r.W, man, 5f);

            SimEvent death = default; bool died = false;
            for (int t = 0; t < 40 && !died; t++)
            {
                r.Step();
                var events = r.W.Events.Events;
                for (int i = 0; i < events.Length && !died; i++)
                    if (events[i].Type == SimEventType.Death && events[i].A == man) { death = events[i]; died = true; }
            }

            Assert.IsTrue(died, "five hit points at twelve a second is well under a second");
            Assert.AreEqual((int)DeathCause.Burning, death.B);
            Assert.IsFalse(r.M.World.IsAlive(man));
            Assert.AreEqual(0u, r.M.Burning.AlightUntil[man], "a dead man's fire is out");
        }

        [Test]
        public void AGarrisonedManAlightLeavesHisTrench()
        {
            using var r = new Rig();
            var map = r.M.Map;
            float3 inTrench = new float3(150f, 0f, 62f);   // the z = 60 trench of the playtest map
            int man = r.Man(inTrench);
            short trench = map.CellTrenchId[map.NavIndex(75, 31)];
            Assert.GreaterOrEqual(trench, 0, "the playtest map has a trench there");
            r.W.TrenchId[man] = trench;
            r.W.Flags[man] |= (uint)UnitFlags.InTrench;

            r.M.Burning.Ignite(r.W, man, 5f);
            r.Step();

            Assert.AreEqual(-1, r.W.TrenchId[man], "he is out of the trench");
            Assert.AreEqual(trench, r.W.SourceTrench[man], "and remembers which one, for a fall-back");
            Assert.AreEqual(1, r.Count(SimEventType.UnitLeftTrench, man));
        }

        [Test]
        public void AReusedSlotDoesNotInheritTheLastMansFire()
        {
            using var r = new Rig();
            int man = r.Man(new float3(150f, 0f, 240f), 5f);
            r.M.Burning.Ignite(r.W, man, 5f);
            r.Step((int)math.ceil(5f / (BurningSystem.BurnDps * r.W.Config.TickSeconds)) + 1);   // 0.6 hp a tick at 20 Hz: nine ticks for 5 hp, one more to be sure
            Assert.IsFalse(r.W.IsAlive(man));
            int next = r.Man(new float3(150f, 0f, 260f));
            Assert.AreEqual(man, next, "the slot is reused (last in, first out)");
            r.Step();
            Assert.IsFalse(r.Burning(next), "the new man is not alight");
            Assert.AreEqual(1000f - BurningSystem.BurnDps * r.W.Config.TickSeconds * 0f, r.W.Hp[next], 1e-3f);
        }

        [Test]
        public void FireIsInTheHash()
        {
            using var a = new Rig();
            using var b = new Rig();
            int ma = a.Man(new float3(150f, 0f, 240f));
            int mb = b.Man(new float3(150f, 0f, 240f));
            a.Step(); b.Step();
            Assert.AreEqual(a.W.Hash(), b.W.Hash(), "two quiet worlds agree");

            b.M.Burning.Ignite(b.W, mb, 5f);
            Assert.AreNotEqual(a.W.Hash(), b.W.Hash(), "a man alight is compared state");

            using var c = new Rig();
            c.M.Burning.IgniteCell(c.W, new float3(150f, 0f, 240f), 5f, 0);
            Assert.AreNotEqual(a.W.Hash(), c.W.Hash(), "and so is burning ground");
        }

        [Test]
        public void AnIncendiaryBurstIsDeterministic()
        {
            ulong[] Run()
            {
                using var r = new Rig();
                for (int i = 0; i < 12; i++) r.Man(new float3(140f + i * 2f, 0f, 240f), 60f);
                r.M.Blast.Queue(Incendiary(new float3(150f, 0f, 240f), 8f, 10f));
                var hashes = new ulong[60];
                for (int t = 0; t < 60; t++) { r.Step(); hashes[t] = r.W.LastHash; }
                Assert.Greater(r.M.Burning.MenIgnited, 0);
                return hashes;
            }
            var x = Run(); var y = Run();
            for (int t = 0; t < x.Length; t++) Assert.AreEqual(x[t], y[t], $"hash diverged at tick {t}");
        }

        [Test]
        public void AManAlightWhoDiesOfSomethingElseLeavesNoFireForTheNextManInHisSlot()
        {
            using var r = new Rig();
            var at = new float3(150f, 0f, 240f);
            int man = r.Man(at);
            r.M.Burning.Ignite(r.W, man, 8f);
            r.Step();
            Assert.IsTrue(r.Burning(man));
            // gassed after this system stepped: the slot is freed with the timer still running, and handed on
            r.W.Despawn(man, (int)DeathCause.Gas);
            int recruit = r.Man(at);
            Assert.AreEqual(man, recruit, "the freed slot is the one the recruit gets");
            r.Step();
            Assert.IsFalse(r.Burning(recruit), "the recruit is not alight");
            Assert.AreEqual(0u, r.M.Burning.AlightUntil[recruit], "the dead man's timer is gone");
            Assert.AreEqual(1000f, r.W.Hp[recruit], 1e-3f, "and he lost nothing to it");
            Assert.AreEqual(0, r.Count(SimEventType.UnitAlight, recruit, 1), "nobody said he caught");
            Assert.IsFalse(r.M.Burning.IsAlight(r.W, recruit));
        }

        [Test]
        public void AManAlightWhoIsKilledIsSaidToBeOut()
        {
            using var r = new Rig();
            int man = r.Man(new float3(150f, 0f, 240f));
            r.M.Burning.Ignite(r.W, man, 8f);
            r.Step();
            r.W.Despawn(man, 3);   // shot dead by slot 3 between this system's steps
            r.Step();
            Assert.AreEqual(1, r.Count(SimEventType.UnitAlight, man, 0), "one UnitAlight (b = 0) for the corpse: the picture douses his slot");
            Assert.AreEqual(0u, r.M.Burning.AlightUntil[man]);
        }

        [Test]
        public void ARecruitLitBeforeTheSystemStepsGetsHisOwnFireNotTheDeadMans()
        {
            using var r = new Rig();
            var at = new float3(150f, 0f, 240f);
            int man = r.Man(at);
            r.M.Burning.Ignite(r.W, man, 8f);
            r.Step();
            r.W.Despawn(man, (int)DeathCause.Gas);   // after this system stepped: the timer is still running on the slot
            int recruit = r.Man(at);
            Assert.AreEqual(man, recruit, "the freed slot is the one the recruit gets");
            uint before = r.W.Tick;
            r.M.Burning.Ignite(r.W, recruit, 2f);   // a beam or an incendiary lights him before the job runs
            Assert.AreEqual(1, r.Count(SimEventType.UnitAlight, recruit, 1), "his own UnitAlight (b = 1)");
            uint twoSeconds = (uint)math.ceil(2f / r.W.Config.TickSeconds);
            Assert.AreEqual(before + twoSeconds, r.M.Burning.AlightUntil[recruit], "his own two seconds, not the dead man's eight");
            r.Step();
            Assert.IsTrue(r.Burning(recruit));
        }
    }
}
