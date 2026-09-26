// Phase: A5 (2026-09-26, docs/21 phase 4) — the Death event says what killed a man and how hard he was hit.
// b is the killer's slot when one did it, or a DeathCause below zero when a blast, gas or fire did; a blast's dead
// carry the way they were thrown in dir.xz (dir.y = 1 marks it) and how hard in scalar, so the picture can launch a
// body the way the sim would have thrown a survivor, and pile up the men a shell killed together.
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;

namespace TW.Tests
{
    public class DeathEventContractTests
    {
        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public Rig()
            {
                var cfg = SimConfig.Default;
                M = MatchSim.CreatePlaytest(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
            }
            public int Man(float3 at, float hp, byte team = 0) => M.World.Spawn(team, 0, at, hp, 0f, false);
            public void Step()
            {
                using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
                M.Step(none);
            }
            public bool Death(int slot, out SimEvent e, out int index)
            {
                var events = M.World.Events.Events;
                for (int i = 0; i < events.Length; i++)
                    if (events[i].Type == SimEventType.Death && events[i].A == slot) { e = events[i]; index = i; return true; }
                e = default; index = -1; return false;
            }
            public int FirstIndex(SimEventType type)
            {
                var events = M.World.Events.Events;
                for (int i = 0; i < events.Length; i++) if (events[i].Type == type) return i;
                return -1;
            }
            public void Dispose() => M.Dispose();
        }

        static Impact Shell(float3 at, float radius, float damage, float3 dir = default)
            => new Impact { Pos = at, Radius = radius, Damage = damage, Suppression = 40f, Player = -1, Dir = dir, Source = 1 };

        [Test]
        public void ABlastDeathCarriesItsCauseAndTheWayHeWasThrown()
        {
            using var r = new Rig();
            var at = new float3(150f, 0f, 240f);
            int man = r.Man(at + new float3(0f, 0f, 3f), 10f);

            r.M.Blast.Queue(Shell(at, 8f, 200f, new float3(0f, 0f, 1f)));
            r.Step();

            Assert.IsFalse(r.M.World.IsAlive(man));
            Assert.IsTrue(r.Death(man, out var e, out int deathAt), "a Death event for him");
            Assert.AreEqual((int)DeathCause.Blast, e.B, "no slot killed him: b names the cause");
            Assert.AreEqual(1f, e.Dir.y, 1e-4f, "dir.y = 1 says a blast threw him");
            Assert.AreEqual(1f, math.length(new float2(e.Dir.x, e.Dir.z)), 1e-3f, "dir.xz is the unit way he went");
            Assert.Greater(e.Dir.z, 0.5f, "away from the burst, leaning the way the shell was going");
            Assert.Greater(e.Scalar, 0f, "and scalar says how hard");
            int explosionAt = r.FirstIndex(SimEventType.Explosion);
            Assert.IsTrue(explosionAt >= 0 && explosionAt < deathAt, "the Explosion that did it comes first in the same buffer");
        }

        [Test]
        public void TheFarSideOfTheBurstThrowsTheDeadHarder()
        {
            using var r = new Rig();
            var at = new float3(150f, 0f, 240f);
            float3 flight = new float3(0f, 0f, 1f);
            int behind = r.Man(at - new float3(0f, 0f, 4f), 10f);
            int ahead = r.Man(at + new float3(0f, 0f, 4f), 10f);

            r.M.Blast.Queue(Shell(at, 8f, 200f, flight));
            r.Step();

            bool behindDied = r.Death(behind, out var eb, out _);
            bool aheadDied = r.Death(ahead, out var ea, out _);
            Assert.IsTrue(behindDied && aheadDied, "both men die");
            Assert.Greater(ea.Scalar, eb.Scalar, "the fragments carry on: the far side is thrown harder");
            Assert.AreEqual((int)DeathCause.Blast, ea.B);
        }

        [Test]
        public void AManTheShellOnlyWoundedIsNotADeath()
        {
            using var r = new Rig();
            var at = new float3(150f, 0f, 240f);
            int man = r.Man(at + new float3(0f, 0f, 3f), 1000f);

            r.M.Blast.Queue(Shell(at, 8f, 200f, new float3(0f, 0f, 1f)));
            r.Step();

            Assert.IsTrue(r.M.World.IsAlive(man));
            Assert.IsFalse(r.Death(man, out _, out _));
            Assert.Greater(math.length(r.M.World.Knock[man]), 0f, "a survivor in the open is thrown by the sim itself");
        }

        [Test]
        public void AGasDeathSaysSo()
        {
            using var r = new Rig();
            var at = new float3(150f, 0f, 240f);
            int man = r.Man(at, 1f);
            r.M.Gas.AddSource(at, 40f, 240, 1);

            SimEvent death = default; bool died = false;
            for (int t = 0; t < 40 && !died; t++) { r.Step(); died = r.Death(man, out death, out _); }

            Assert.IsTrue(died, "a man with one hit point in chlorine dies within two seconds");
            Assert.AreEqual((int)DeathCause.Gas, death.B);
            Assert.AreEqual(0f, death.Scalar, "gas throws nobody");
        }

        [Test]
        public void AShotDeathKeepsTheKillersSlot()
        {
            using var r = new Rig();
            // a rifleman in the open twenty metres from an enemy who cannot fight back and has one hit point
            int shooter = r.M.World.Spawn(0, 0, new float3(150f, 0f, 200f), 100f, 0f, false);
            int victim = r.M.World.Spawn(1, 0, new float3(150f, 0f, 220f), 1f, 0f, false);
            r.M.World.Flags[victim] |= (uint)UnitFlags.HoldFire;

            SimEvent death = default; bool died = false;
            for (int t = 0; t < 600 && !died; t++) { r.Step(); died = r.Death(victim, out death, out _); }

            Assert.IsTrue(died, "he is shot within thirty seconds");
            Assert.AreEqual(shooter, death.B, "b is the slot that shot him");
            Assert.AreEqual(0f, death.Dir.y, 1e-4f, "a shot is not a blast: dir.y stays 0");
        }
    }
}
