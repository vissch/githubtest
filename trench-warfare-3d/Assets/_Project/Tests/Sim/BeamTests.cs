// Phase: A5 (docs/21 phase 5, SIM-C) — the sweeping beam: its head walks the corridor and the man it passes dies as
// it reaches him while a man six metres off is untouched; a trench keeps limited protection; a Maw on the line loses
// hull and throws sparks; a tree under the head wears; a man it passes without killing runs alight; two sweeps a
// player at most; the ability pays, schedules and fires; the beam is hashed and deterministic.
using System;
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class BeamTests
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
            public int Man(float3 at, float hp, byte team = 1) => W.Spawn(team, 0, at, hp, 0f, false);
            public int Tank(float3 at, byte team = 1) => W.Spawn(team, 4, at, 5000f, 0f, true);
            public void Step(params SimCommand[] cmds)
            {
                using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
                M.Step(arr);
            }
            public static SimCommand Beam(byte player, float3 at, int heading = 90, int length = 60)
                => new SimCommand { Player = player, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.Beam, B = AbilityArgs.Pack(heading, 0, length), Pos = at };
            public int Count(SimEventType type)
            {
                int n = 0;
                var ev = W.Events.Events;
                for (int i = 0; i < ev.Length; i++) if (ev[i].Type == type) n++;
                return n;
            }
            public bool Find(SimEventType type, Func<SimEvent, bool> where, out SimEvent found)
            {
                var ev = W.Events.Events;
                for (int i = 0; i < ev.Length; i++) if (ev[i].Type == type && where(ev[i])) { found = ev[i]; return true; }
                found = default; return false;
            }
            public void Dispose() => M.Dispose();
        }

        static readonly float3 Start = new float3(120f, 0f, 200f);   // the corridor runs +X for 60 m across open ground
        const int Warm = 80, Sweep = 120;

        [Test]
        public void TheHeadWalksTheCorridorAndTheManItPassesDies()
        {
            using var r = new Rig();
            int onLine = r.Man(Start + new float3(30f, 0f, 0f), 100f), off = r.Man(Start + new float3(30f, 0f, 6f), 100f);
            r.Step(Rig.Beam(0, Start));
            int deathTick = -1, scorches = 0;
            for (int t = 1; t <= Warm + Sweep + 10; t++)
            {
                r.Step();
                if (deathTick < 0 && r.Find(SimEventType.Death, e => e.A == onLine, out var d)) { deathTick = t; Assert.AreEqual((int)DeathCause.Beam, d.B); Assert.AreEqual(4f, d.Scalar); }
                if (r.Find(SimEventType.Explosion, e => e.Dir.y == (float)BlastShape.Beam, out _)) scorches++;
            }
            Assert.That(deathTick, Is.InRange(Warm + 60 - 2, Warm + 60 + 12), "thirty metres along a sixty metre sweep: the head reaches him at the half and he dies as it passes");
            Assert.IsTrue(r.W.IsAlive(off));
            Assert.AreEqual(100f, r.W.Hp[off], "six metres off the line: untouched");
            Assert.AreEqual(0f, r.W.Suppression[off], "not even by the scorch, which is not for men");
            Assert.GreaterOrEqual(scorches, 25, "the scorch cue under the head, every few ticks of the sweep");
            Assert.AreEqual(0, r.M.Beam.Beams.Length, "over: the beam is gone");
        }

        [Test]
        public void ATrenchGivesLimitedProtection()
        {
            // one tick of the beam on two men side by side, one flagged below the parapet: the flag is read as it stands
            // (the beam steps before the movement and garrison systems touch it)
            using var r = new Rig();
            int open = r.Man(Start, 1000f), dug = r.Man(Start + new float3(0f, 0f, 1f), 1000f);
            int torch = r.Man(Start + new float3(0f, 0f, 30f), 1000f);   // alight but out of the beam: what the fire alone takes this tick
            r.W.Flags[dug] |= (uint)UnitFlags.InTrench;
            r.M.Burning.Ignite(r.W, torch, BeamSystem.AlightSeconds);
            Assert.IsTrue(r.M.Beam.Start(r.W, Start, new float3(1f, 0f, 0f), 60f, 2f, Sweep, 0, (int)OffMapAbilityId.Beam));
            r.Step();
            float burn = 1000f - r.W.Hp[torch];
            float tookOpen = 1000f - r.W.Hp[open] - burn, tookDug = 1000f - r.W.Hp[dug] - burn;
            Assert.AreEqual(BeamSystem.BeamDps * r.W.Config.TickSeconds, tookOpen, 0.01f, "in the open: the beam's full toll");
            Assert.AreEqual(tookOpen * BlastRules.TrenchBayFactor, tookDug, 0.01f, "below the parapet: the trench's limited protection, never nothing");
        }

        [Test]
        public void AMawOnTheLineLosesHullAndThrowsSparks()
        {
            using var r = new Rig();
            int maw = r.Tank(Start + new float3(30f, 0f, 0.5f));
            float hp = r.W.Hp[maw];
            r.Step(Rig.Beam(0, Start));
            bool sparks = false;
            for (int t = 0; t < Warm + Sweep + 10; t++)
            {
                r.Step();
                if (r.Find(SimEventType.VehicleArmourHit, e => e.A == maw && e.B == -1 && e.Scalar > 0f, out _)) sparks = true;
            }
            Assert.Less(r.W.Hp[maw], hp - 300f, "the hull takes it straight, through any plate");
            Assert.IsTrue(sparks, "and sparks fly off it each tick it is in the beam");
        }

        [Test]
        public void ATreeUnderTheHeadWears()
        {
            using var r = new Rig();
            int tree = r.M.Map.AddProp(new PropDef { Kind = PropKind.Tree, Pos = Start + new float3(30f, 0f, 0f), Hp = 120f, Scale = 1f });
            Assert.GreaterOrEqual(tree, 0, "setup: a tree on open ground");
            r.Step(Rig.Beam(0, Start));
            for (int t = 0; t < Warm + Sweep + 10; t++) r.Step();
            var after = r.M.Map.Props[tree];
            Assert.IsTrue(after.Hp < 120f || after.Kind != PropKind.Tree, "the scorch under the head wears what stands there");
        }

        [Test]
        public void AManItPassesWithoutKillingRunsAlight()
        {
            using var r = new Rig();
            int man = r.Man(Start + new float3(30f, 0f, 0f), 1e6f);
            r.Step(Rig.Beam(0, Start));
            bool caught = false;
            for (int t = 0; t < Warm + Sweep + 10; t++)
            {
                r.Step();
                if (r.Find(SimEventType.UnitAlight, e => e.A == man && e.B == 1, out _)) caught = true;
            }
            Assert.IsTrue(caught, "the beam sets him alight");
            Assert.IsTrue(r.W.IsAlive(man));
            Assert.Less(r.W.Hp[man], 1e6f, "and hurt him");
        }

        [Test]
        public void NoMoreThanTwoSweepsAPlayer()
        {
            using var r = new Rig();
            var dir = new float3(1f, 0f, 0f);
            Assert.IsTrue(r.M.Beam.Start(r.W, Start, dir, 60f, 2f, Sweep, 0, (int)OffMapAbilityId.Beam));
            Assert.IsTrue(r.M.Beam.Start(r.W, Start + new float3(0f, 0f, 20f), dir, 60f, 2f, Sweep, 0, (int)OffMapAbilityId.Beam));
            Assert.IsFalse(r.M.Beam.Start(r.W, Start + new float3(0f, 0f, 40f), dir, 60f, 2f, Sweep, 0, (int)OffMapAbilityId.Beam), "a third for the same player is refused");
            Assert.AreEqual(2, r.M.Beam.ActiveFor(0));
            Assert.IsTrue(r.M.Beam.Start(r.W, Start + new float3(0f, 0f, 60f), dir, 60f, 2f, Sweep, 1, (int)OffMapAbilityId.Beam), "the other player has his own two");
            Assert.AreEqual(3, r.M.Beam.Beams.Length);
            for (int t = 0; t < Sweep + 2; t++) r.Step();
            Assert.AreEqual(0, r.M.Beam.Beams.Length, "all over");
            Assert.AreEqual(3, r.M.Beam.Sweeps);
        }

        [Test]
        public void TheAbilityPaysSchedulesAndFires()
        {
            using var r = new Rig(silver: 1000);
            int before = r.W.Silver[0];
            r.Step(Rig.Beam(0, Start), Rig.Beam(0, Start + new float3(0f, 0f, 30f)));
            Assert.LessOrEqual(r.W.Silver[0], before - 300 + 1, "paid once");
            Assert.AreEqual(1, r.Count(SimEventType.CommandRejected), "the second is on cooldown");
            Assert.AreEqual(1, r.M.Abilities.Scheduled.Length);
            var p = r.M.Abilities.Scheduled[0];
            Assert.AreEqual((int)PayloadKind.BeamStart, p.Kind);
            Assert.AreEqual(60f, math.length(p.Dir), 0.05f, "a beam's Dir carries heading x length");
            Assert.AreEqual(2f, p.Radius); Assert.AreEqual(Sweep, p.Ticks);
            Assert.IsTrue(r.Find(SimEventType.AbilityFired, e => e.A == (int)OffMapAbilityId.Beam, out var fired));
            Assert.AreEqual(60f, fired.Dir.x, 0.05f); Assert.AreEqual(2f, fired.Scalar);
            for (int t = 1; t < Warm; t++) { r.Step(); Assert.AreEqual(0, r.M.Beam.Beams.Length, "nothing during the warm-up"); }
            r.Step();
            Assert.AreEqual(1, r.M.Beam.Beams.Length, "the sweep starts when the warm-up ends");
            Assert.AreEqual(1, r.M.Beam.Sweeps);
            Assert.AreEqual(0, r.M.Abilities.Scheduled.Length);
        }

        static ulong[] Run(uint seed, bool beam)
        {
            using var r = new Rig(seed: seed);
            for (int k = 0; k < 6; k++) r.Man(Start + new float3(10f + k * 8f, 0f, 0f), 100f);
            var hashes = new ulong[Warm + Sweep + 5];
            if (beam) r.Step(Rig.Beam(0, Start)); else r.Step();
            for (int t = 0; t < hashes.Length; t++) { r.Step(); hashes[t] = r.W.LastHash; }
            return hashes;
        }

        [Test]
        public void TheBeamIsHashedAndDeterministic()
        {
            var a = Run(5, true); var b = Run(5, true); var none = Run(5, false);
            for (int i = 0; i < a.Length; i++) Assert.AreEqual(a[i], b[i], $"hash diverged at tick {i}");
            Assert.AreNotEqual(a[Warm + 5], none[Warm + 5], "a running sweep is in the hash");
        }
    }
}
