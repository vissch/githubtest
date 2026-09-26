// Phase: A5 (docs/21 phase 5) — the strafe run: one low pass lays 32 bursts along the corridor the player dragged,
// kills the men standing on it, touches nobody twelve metres off it, digs nothing, costs silver, cools down, and
// honours the length asked for. Deterministic.
using System;
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class StrafeRunTests
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
            public SimEvent First(SimEventType type)
            {
                var ev = W.Events.Events;
                for (int i = 0; i < ev.Length; i++) if (ev[i].Type == type) return ev[i];
                throw new InvalidOperationException("no " + type + " this tick");
            }
            public void Dispose() => M.Dispose();
        }

        static readonly float3 Start = new float3(60f, 0f, 200f);   // open ground on the playtest map, the corridor running +X

        [Test]
        public void AStrafeLaysItsBurstsAlongTheLineAndDigsNoCrater()
        {
            using var r = new Rig();
            Assert.IsTrue(OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.StrafeRun, out var stats));
            r.Step(Rig.Support(0, OffMapAbilityId.StrafeRun, Start, heading: 90, length: 80));
            Assert.AreEqual(stats.Shells, r.M.Abilities.Scheduled.Length, "every burst is scheduled at once");
            for (int t = 1; t < stats.WarmupTicks; t++) { r.Step(); Assert.AreEqual(0, r.Count(SimEventType.Explosion), "nothing lands during the run-in"); }
            var bursts = new List<SimEvent>();
            for (int t = 0; t < stats.SpreadTicks + 4; t++) { r.Step(); r.Collect(SimEventType.Explosion, bursts); }
            Assert.AreEqual(stats.Shells, bursts.Count, "one burst per scheduled round");
            foreach (var e in bursts)
            {
                Assert.LessOrEqual(math.abs(e.Pos.z - Start.z), OffMapAbilitySystem.StrafeScatter + 0.01f, "a burst lies on the line");
                Assert.That(e.Pos.x, Is.InRange(Start.x, Start.x + 80f), "and inside the corridor");
                Assert.Greater(e.Dir.x, 0.9f, "the rounds travel the aircraft's way");
                Assert.AreEqual((int)OffMapAbilityId.StrafeRun, e.A);
            }
            Assert.AreEqual(0, r.M.Abilities.Scheduled.Length);
            Assert.AreEqual(0, r.M.Deformation.Applied, "machine-gun fire digs no craters");
        }

        [Test]
        public void MenOnTheLineDieAndMenTwelveMetresOffAreNotTouched()
        {
            using var r = new Rig();
            var onLine = new int[7]; var off = new int[7];
            for (int k = 0; k < 7; k++)
            {
                onLine[k] = r.Man(new float3(70f + k * 10f, 0f, Start.z), 100f);
                off[k] = r.Man(new float3(70f + k * 10f, 0f, Start.z + 12f), 100f);
            }
            r.Step(Rig.Support(0, OffMapAbilityId.StrafeRun, Start, heading: 90, length: 80));
            for (int t = 0; t < 160; t++) r.Step();
            int alive = 0;
            foreach (int s in onLine) if (r.W.IsAlive(s)) alive++;
            Assert.LessOrEqual(alive, 1, "a man standing in the open on the line does not live through the pass");
            foreach (int s in off)
            {
                Assert.IsTrue(r.W.IsAlive(s));
                Assert.AreEqual(100f, r.W.Hp[s], "twelve metres off the line nothing reaches him");
                Assert.AreEqual(0f, r.W.Suppression[s], "not even the noise");
            }
        }

        [Test]
        public void AStrafeCostsSilverCoolsDownAndIsRefusedMeanwhile()
        {
            using var r = new Rig(silver: 1000);
            int before = r.W.Silver[0];
            r.Step(Rig.Support(0, OffMapAbilityId.StrafeRun, Start, heading: 90, length: 80));
            Assert.LessOrEqual(r.W.Silver[0], before - 180 + 1, "paid up front");
            Assert.AreEqual(1800, r.M.Abilities.CooldownOf(0, OffMapAbilityId.StrafeRun));
            Assert.AreEqual(1, r.Count(SimEventType.AbilityFired));
            var fired = r.First(SimEventType.AbilityFired);
            Assert.AreEqual(80f, math.length(fired.Dir), 0.05f, "dir carries heading x length");
            Assert.Greater(fired.Dir.x, 79f);
            Assert.AreEqual(3f, fired.Scalar, "scalar is the corridor's half width");
            Assert.AreEqual(Start.x, fired.Pos.x); Assert.AreEqual(Start.z, fired.Pos.z);

            r.Step(Rig.Support(0, OffMapAbilityId.StrafeRun, Start + new float3(0f, 0f, 20f), heading: 90, length: 80));
            Assert.AreEqual(1, r.Count(SimEventType.CommandRejected), "still cooling down");
            Assert.AreEqual(32, r.M.Abilities.Scheduled.Length, "and nothing more was scheduled");
            r.Step(Rig.Support(1, OffMapAbilityId.StrafeRun, Start + new float3(0f, 0f, 60f), heading: 90, length: 80));
            Assert.AreEqual(0, r.Count(SimEventType.CommandRejected), "the other player has his own cooldown");
            Assert.AreEqual(64, r.M.Abilities.Scheduled.Length);
        }

        [Test]
        public void TheLengthAskedForIsHonouredWithinTheAbilitys()
        {
            using var r = new Rig();
            r.Step(Rig.Support(0, OffMapAbilityId.StrafeRun, Start, heading: 90, length: 20));
            float far = 0f;
            for (int i = 0; i < r.M.Abilities.Scheduled.Length; i++) far = math.max(far, r.M.Abilities.Scheduled[i].Pos.x - Start.x);
            Assert.LessOrEqual(far, 20f, "a short drag is a short pass");
            Assert.Greater(far, 15f, "but it still runs the whole drag");

            using var longer = new Rig();
            longer.Step(Rig.Support(0, OffMapAbilityId.StrafeRun, Start, heading: 90, length: 250));
            far = 0f;
            for (int i = 0; i < longer.M.Abilities.Scheduled.Length; i++) far = math.max(far, longer.M.Abilities.Scheduled[i].Pos.x - Start.x);
            Assert.LessOrEqual(far, 80f, "a drag past the ability's length is cut to it");
            Assert.Greater(far, 75f);
        }

        static ulong[] Run(uint seed)
        {
            using var r = new Rig(seed: seed);
            for (int k = 0; k < 7; k++) r.Man(new float3(70f + k * 10f, 0f, Start.z), 100f);
            var hashes = new ulong[160];
            r.Step(Rig.Support(0, OffMapAbilityId.StrafeRun, Start, heading: 90, length: 80));
            for (int t = 0; t < hashes.Length; t++) { r.Step(); hashes[t] = r.W.LastHash; }
            return hashes;
        }

        [Test]
        public void AStrafeIsDeterministic()
        {
            var a = Run(7); var b = Run(7);
            for (int i = 0; i < a.Length; i++) Assert.AreEqual(a[i], b[i], $"hash diverged at tick {i}");
            Assert.AreNotEqual(a[0], a[159], "the pass changes the world");
        }
    }
}
