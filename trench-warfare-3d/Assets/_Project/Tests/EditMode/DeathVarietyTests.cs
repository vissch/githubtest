// Phase: B5 / docs/21 phase 4 (implemented) — how the controller chooses and records a death: a blast death is thrown
// the way and as hard as the sim says; a man who dies in a heap is thrown further than the same man alone; burning,
// gas and a crushing track have their own deaths; the death record outlives the slot; two men shot standing next to
// each other do not die the same way; VatPad carries the char bits. No sim is stepped: the tests place men, kill
// them through SimWorld.Despawn (which writes the Death event the way every system does) and tick the controller.
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class DeathVarietyTests
    {
        static readonly float3 Here = new float3(150f, 0f, 300f);

        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public readonly AnimationController A;
            public SimWorld W => M.World;
            NativeArray<ushort> row; NativeArray<float> phase, yaw;

            public Rig()
            {
                var cfg = SimConfig.Default;
                M = MatchSim.CreateGreybox(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
                M.World.Tick = 1000;
                row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
                phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                A = new AnimationController(cfg.MaxSlots, M.Map, M.Gas, row, phase, yaw);
            }

            public int Man(float3 at, byte team = 0) => W.Spawn(team, 0, at, 100f, 3f, false);
            public int Tank(float3 at, byte team = 1) => W.Spawn(team, 4, at, 5000f, 2f, true);

            /// <summary>One tick of the controller and a tick's worth of frames, then the tick's events are gone.</summary>
            public void Tick()
            {
                A.Tick(W);
                A.Advance(W.Config.TickSeconds);
                W.Events.Clear();
                W.Tick++;
            }

            public void Dispose()
            {
                A.Dispose(); M.Dispose();
                row.Dispose(); phase.Dispose(); yaw.Dispose();
            }
        }

        [Test]
        public void ABlastDeathIsThrownTheWayAndAsHardAsTheSimSays()
        {
            using var r = new Rig();
            int man = r.Man(Here);
            r.Tick();
            uint at = r.W.Tick;
            r.W.Despawn(man, (int)DeathCause.Blast, new float3(1f, 1f, 0f), 10f);   // the knock: along +x, hard
            r.Tick();
            var s = r.A.State[man];
            Assert.AreEqual(Clip.DeathThrown, s.Clip);
            Assert.IsTrue(s.Dead);
            Assert.Greater(s.ThrowX, 3f, "thrown along the sim's knock");
            Assert.AreEqual(0f, s.ThrowZ, 0.05f);
            Assert.Greater(s.ThrowUp, 1f, "and up");
            Assert.AreEqual(-math.PI / 2f, s.BodyYaw, 1e-3f, "he faces the burst as it throws him backwards");
            Assert.IsTrue(r.A.TryDeath(man, at, out var rec), "the death is on the ring, by slot and the event's tick");
            Assert.AreEqual(Clip.DeathThrown, rec.Clip);
            Assert.AreEqual(s.ThrowX, rec.ThrowX); Assert.AreEqual(s.ThrowUp, rec.ThrowUp);
            Assert.AreEqual((byte)DeathKind.Blast, rec.Cause);
            Assert.AreEqual(0, rec.Density, "nobody else went down beside him");
            Assert.IsFalse(r.A.TryDeath(man, at + 5, out _), "another tick is another death");
        }

        static float ThrowOf(bool heap)
        {
            using var r = new Rig();
            // four men in slots 0-3: beside him in the heap, or far away and alive; he is slot 4 either way (same seed)
            var others = new int[4];
            for (int k = 0; k < 4; k++) others[k] = r.Man(heap ? Here + new float3(0.8f * k, 0f, 0.6f) : Here + new float3(100f, 0f, 0f));
            int man = r.Man(Here);
            Assert.AreEqual(4, man);
            r.Tick();
            if (heap) foreach (int o in others) r.W.Despawn(o, (int)DeathCause.Blast, new float3(1f, 1f, 0f), 6f);
            r.W.Despawn(man, (int)DeathCause.Blast, new float3(1f, 1f, 0f), 6f);
            r.Tick();
            var s = r.A.State[man];
            Assert.AreEqual(Clip.DeathThrown, s.Clip);
            Assert.IsTrue(r.A.TryDeath(man, r.W.Tick - 1, out var rec));
            Assert.AreEqual(heap ? 4 : 0, rec.Density);
            return s.ThrowX;
        }

        [Test]
        public void TheFifthManDownInABayIsThrownFurtherThanTheSameManAlone()
        {
            float alone = ThrowOf(false), heap = ThrowOf(true);
            Assert.Greater(alone, 1f);
            float expect = 1f + AnimationController.DensityPerMan * 4;
            Assert.AreEqual(expect, heap / alone, 0.02f, "four men already down beside him: 2.4x the throw");
            Assert.LessOrEqual(heap, AnimationController.FarCap);
        }

        [Test]
        public void AManWhoDiesAlightDropsMidStrideAndLiesCharred()
        {
            using var r = new Rig();
            int man = r.Man(Here);
            r.Tick();
            r.A.SetAlight(man, 5f);
            r.Tick();
            Assert.AreEqual(Clip.Burning, r.A.State[man].Clip, "alight: he runs");
            Assert.GreaterOrEqual(r.A.Char[man], 1, "and is singed from now on");
            uint at = r.W.Tick;
            r.W.Despawn(man, (int)DeathCause.Burning, new float3(0f, 0f, 1f), 0f);
            r.Tick();
            var s = r.A.State[man];
            Assert.AreEqual(Clip.DeathRunning, s.Clip, "he drops out of his run (DeathBurning once the bake adds it)");
            Assert.AreEqual(0f, s.ThrowUp, "nothing throws him");
            Assert.AreEqual(3, r.A.Char[man], "charred, with the embers still in him");
            Assert.IsTrue(r.A.TryDeath(man, at, out var rec));
            Assert.AreEqual(3, rec.Char); Assert.AreEqual((byte)DeathKind.Burning, rec.Cause);
        }

        [Test]
        public void GasAndATrackHaveTheirOwnDeaths()
        {
            using var r = new Rig();
            int gassed = r.Man(Here), crushed = r.Man(Here + new float3(20f, 0f, 0f));
            int tank = r.Tank(Here + new float3(20f, 0f, 3f));
            r.Tick();
            uint at = r.W.Tick;
            r.W.Despawn(gassed, (int)DeathCause.Gas);
            r.W.Despawn(crushed, tank, new float3(0f, 0f, -1f));   // the killer is the vehicle: a track went over him
            r.Tick();
            Assert.AreEqual(Clip.DeathKneel, r.A.State[gassed].Clip, "choking: down on his knees");
            Assert.AreEqual(Clip.DeathBlast, r.A.State[crushed].Clip, "under a track: flat and hard");
            Assert.IsTrue(r.A.TryDeath(gassed, at, out var g)); Assert.AreEqual((byte)DeathKind.Gas, g.Cause);
            Assert.IsTrue(r.A.TryDeath(crushed, at, out var c)); Assert.AreEqual((byte)DeathKind.Crushed, c.Cause);
            Assert.AreEqual(0, g.Char); Assert.AreEqual(0f, g.ThrowUp);
        }

        [Test]
        public void TheRecordOutlivesTheSlot()
        {
            using var r = new Rig();
            int man = r.Man(Here);
            r.Tick();
            uint at = r.W.Tick;
            ushort was = r.W.Generation[man];
            r.W.Despawn(man, (int)DeathCause.Blast, new float3(0f, 1f, 1f), 8f);
            r.Tick();
            int again = r.Man(Here + new float3(5f, 0f, 0f));
            Assert.AreEqual(man, again, "the freed slot is the one a new man gets");
            r.Tick(); r.Tick();
            Assert.AreNotEqual(was, r.W.Generation[man]);
            Assert.IsFalse(r.A.State[man].Dead, "the slot's state is the living man's now");
            Assert.IsTrue(r.A.TryDeath(man, at, out var rec), "but the dead man's record is still there");
            Assert.AreEqual(was, rec.Generation);
            Assert.AreEqual(Clip.DeathThrown, rec.Clip);
        }

        [Test]
        public void TwoMenShotStandingBesideEachOtherDoNotDieTheSameWay()
        {
            using var r = new Rig();
            int killer = r.Man(Here + new float3(0f, 0f, -40f), team: 1);
            int a = r.Man(Here), b = r.Man(Here + new float3(2f, 0f, 0f));
            r.Tick();
            r.W.Despawn(a, killer, new float3(0f, 0f, 1f));
            r.Tick();
            r.W.Despawn(b, killer, new float3(0f, 0f, 1f));
            r.Tick();
            Clip first = r.A.State[a].Clip, second = r.A.State[b].Clip;
            Assert.IsTrue(first == Clip.DeathBack || first == Clip.DeathHeadshot, "shot from behind, standing: " + first);
            Assert.IsTrue(second == Clip.DeathBack || second == Clip.DeathHeadshot || second == Clip.DeathRight || second == Clip.DeathLeft, "a standing death: " + second);
            Assert.AreNotEqual(first, second, "the man beside him a tick later dies another way");
            Assert.IsTrue(r.A.TryDeath(b, r.W.Tick - 1, out var rec) && rec.Cause == (byte)DeathKind.Shot);
        }

        [Test]
        public void VatPadCarriesTheCharBitsBesideEverythingElse()
        {
            float pad = VatPad.Pack(0b101010, 0.5f, 200, 3);
            Assert.AreEqual(0b101010, VatPad.Lost(pad));
            Assert.AreEqual(0.5f, VatPad.Grime(pad), 1f / 255f);
            Assert.AreEqual(200, VatPad.Seed(pad));
            Assert.AreEqual(3, VatPad.Char(pad));
            float full = VatPad.Pack(63, 1f, 255, 3);
            Assert.AreEqual(63, VatPad.Lost(full)); Assert.AreEqual(1f, VatPad.Grime(full), 1e-6f); Assert.AreEqual(255, VatPad.Seed(full)); Assert.AreEqual(3, VatPad.Char(full));
            Assert.Less(full, 16777216f, "every value a float holds exactly");
            Assert.AreEqual(0, VatPad.Char(VatPad.Pack(63, 1f, 255)), "nothing set: not burned");
            Assert.AreEqual(2, VatPad.Char(VatPad.Pack(0, 0f, 0, 2)));
        }

        [Test]
        public void TheTumbleEndsUprightAndAHeapedManLiesTilted()
        {
            Assert.AreEqual(0, VATRenderer.PitchStepOf(0f, 1.2f, 1, 0));
            Assert.AreEqual(16, VATRenderer.PitchStepOf(0.6f, 1.2f, 1, 0), "half way through one turn: upside down");
            Assert.AreEqual(0, VATRenderer.PitchStepOf(1.2f, 1.2f, 1, 0), "landed: as his clip left him");
            Assert.AreEqual(0, VATRenderer.PitchStepOf(1.5f, 1.2f, 2, 0));
            Assert.AreEqual(16, VATRenderer.PitchStepOf(0.375f, 1.5f, 2, 0), "two turns: upside down at the quarter");
            Assert.AreEqual(0, VATRenderer.PitchStepOf(0.75f, 1.5f, 2, 0), "and round once at the half");
            Assert.AreEqual(31, VATRenderer.PitchStepOf(3f, 1.2f, 1, -1), "down on a heap: one step off level");
            Assert.AreEqual(1, VATRenderer.PitchStepOf(3f, 0f, 0, 1));
        }
    }
}
