// Phase: B5 (implemented) — what a shell does to the men who live through it, as the character controller shows it: a man
// close enough for the sim to throw him hard is blown off his feet (lifted, down on his face away from it, up again,
// dazed); the men further out react one after another as the burst runs out through them, as far as WaveReach past
// its radius; everyone near it wears some of the earth it threw (grime), which reaches the shader through VatPad; and
// the camera feels it only once the sound could have arrived. No sim is stepped: the tests place men, write the
// Explosion event and the sim's throw the way BlastSystem would, and tick the controller alone.
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;
using TW.Presentation.Units;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class BlastReactionTests
    {
        static readonly float3 Burst = new float3(150f, 0f, 300f);
        const float Radius = 8f;   // the barrage shell's

        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public readonly AnimationController A;
            NativeArray<ushort> row; NativeArray<float> phase, yaw;

            public Rig()
            {
                var cfg = SimConfig.Default;
                M = MatchSim.CreateGreybox(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;   // no stray shell whistling in over the men
                M.World.Tick = 1000;   // the reactions are rate-limited against tick 0
                row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
                phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                A = new AnimationController(cfg.MaxSlots, M.Map, M.Gas, row, phase, yaw);
            }

            /// <summary>A rifleman standing still this far along +x from the burst.</summary>
            public int Man(float metres) => M.World.Spawn(0, 0, Burst + new float3(metres, 0f, 0f), 100f, 3f, false);

            /// <summary>One tick of the controller and a tick's worth of frames, then the tick's events are gone.</summary>
            public void Tick()
            {
                A.Tick(M.World);
                A.Advance(M.World.Config.TickSeconds);
                M.World.Events.Clear();
                M.World.Tick++;
            }

            /// <summary>The shell lands this tick.</summary>
            public void Shell() => M.World.Events.Add(M.World.Tick, SimEventType.Explosion, 0, 0, Burst, default, Radius);

            public Clip ClipOf(int slot) => A.State[slot].Clip;

            public void Dispose()
            {
                A.Dispose(); M.Dispose();
                row.Dispose(); phase.Dispose(); yaw.Dispose();
            }
        }

        static bool Flinched(Clip c) => c == Clip.Duck || c == Clip.KneelFlinch || c == Clip.Shield;

        [Test]
        public void AManThrownHardIsBlownOffHisFeet_ThenGetsUpDazed()
        {
            using var r = new Rig();
            int man = r.Man(2f);
            r.M.World.Events.Clear();
            r.Tick();
            r.M.World.Knock[man] = new float3(8f, 0f, 0f);   // BlastSystem's throw 2 m from an 8 m shell: away from it, along +x
            r.Shell();
            r.Tick();
            r.M.World.Knock[man] = float3.zero;

            var s = r.A.State[man];
            Assert.AreEqual(Clip.Trip, s.Clip, "thrown at 8 m/s standing in the open: blown off his feet, not a dive");
            Assert.IsTrue(s.Down);
            Assert.AreEqual(math.PI / 2f, s.BodyYaw, 1e-3f, "turned to face the way it throws him, so he falls away from it");
            r.A.Advance(0.2f);
            Assert.Greater(r.A.Hop[man], 0.2f, "a quarter of a second after the burst he is in the air (life-size metres)");

            bool gotUp = false, dazed = false;
            for (int t = 0; t < 240; t++)
            {
                r.Tick();
                gotUp |= r.ClipOf(man) == Clip.GetUp;
                dazed |= gotUp && r.ClipOf(man) == Clip.FidgetRubEyes;
            }
            Assert.IsTrue(gotUp, "he picks himself up");
            Assert.IsTrue(dazed, "and kneels a moment rubbing his eyes");
            Assert.AreEqual(0f, r.A.Hop[man], "and is back on the ground");
        }

        [Test]
        public void TheReactionRunsOutThroughTheMen()
        {
            using var r = new Rig();
            int inner = r.Man(7.6f);    // past 0.85 of the radius, where the sim stops throwing men, still inside the burst
            int edge = r.Man(16f);      // past the sim's reach, inside the wave's
            int beyond = r.Man(21f);    // past WaveReach
            r.M.World.Events.Clear();
            r.Tick();
            r.Shell();
            r.Tick();
            Assert.IsFalse(Flinched(r.ClipOf(inner)) || Flinched(r.ClipOf(edge)), "nobody past the inner ring moves on the burst's own tick");
            r.Tick(); r.Tick();
            Assert.IsTrue(Flinched(r.ClipOf(inner)), "7.6 m out he flinches within two ticks: " + r.ClipOf(inner));
            Assert.IsFalse(Flinched(r.ClipOf(edge)), "16 m out he has not felt it yet");
            for (int t = 0; t < 4; t++) r.Tick();
            Assert.IsTrue(Flinched(r.ClipOf(edge)), "16 m out he flinches a few ticks after the man nearer it: " + r.ClipOf(edge));
            for (int t = 0; t < 6; t++) r.Tick();
            Assert.IsFalse(Flinched(r.ClipOf(beyond)), "21 m out nothing reaches him");
        }

        [Test]
        public void TheNearerAManIsTheMoreEarthHeWears()
        {
            using var r = new Rig();
            int close = r.Man(2f), mid = r.Man(9f), far = r.Man(16f);
            r.M.World.Events.Clear();
            r.Tick();
            r.Shell();
            r.Tick();
            Assert.Greater(r.A.Grime[close], r.A.Grime[mid]);
            Assert.Greater(r.A.Grime[mid], 0f);
            Assert.AreEqual(0f, r.A.Grime[far], "six metres past the edge he is clean");
            float once = r.A.Grime[close];
            r.Shell();
            r.Tick();
            Assert.Greater(r.A.Grime[close], once, "a second shell adds to it");
            Assert.LessOrEqual(r.A.Grime[close], 1f);
        }

        [Test]
        public void ThePadCarriesTheLimbsTheGrimeAndTheSeed()
        {
            float pad = VatPad.Pack(0b101010, 0.5f, 200);
            Assert.AreEqual(0b101010, VatPad.Lost(pad));
            Assert.AreEqual(0.5f, VatPad.Grime(pad), 1f / 255f);
            Assert.AreEqual(200, VatPad.Seed(pad));
            float full = VatPad.Pack(63, 1f, 255);
            Assert.AreEqual(63, VatPad.Lost(full));
            Assert.AreEqual(1f, VatPad.Grime(full), 1e-6f);
            Assert.AreEqual(255, VatPad.Seed(full));
            Assert.Less(full, 16777216f, "a float holds it exactly only below 2^24");
            Assert.AreEqual(0, VatPad.Lost(VatPad.Pack(0, 1f, 255)), "a living man never reads as cut, however dirty");
        }

        [Test]
        public void AHullOnItsSpringsSettlesHoweverLongTheFrame()
        {
            // TankRenderer's springs carry a blast's kick. An explicit step went unstable once omega * dt neared 2, which a
            // single long frame reaches (0.34 s at omega 10), and the hull was drawn kilometres up. The exact solution
            // only ever decays: kicked and stepped through twenty hitches, it comes to rest without once swinging wider.
            var t = typeof(TankRenderer).GetNestedType("Spring", System.Reflection.BindingFlags.NonPublic);
            Assert.IsNotNull(t, "TankRenderer.Spring moved or was renamed");
            object s = Activator.CreateInstance(t);
            t.GetField("Velocity").SetValue(s, 3f);   // a close shell's heave kick
            var step = t.GetMethod("Step");
            float widest = 0f, last = 0f;
            for (int k = 0; k < 20; k++)
            {
                step.Invoke(s, new object[] { 0f, 0.34f, 10f });
                last = Mathf.Abs((float)t.GetField("Value").GetValue(s));
                widest = Mathf.Max(widest, last);
            }
            Assert.Less(widest, 0.5f, "a 3 m/s kick never lifts it half a metre");
            Assert.Less(last, 1e-3f, "and it has settled");
        }

        [Test]
        public void TheCameraFeelsABurstWhenTheSoundArrives()
        {
            Assert.AreEqual(1f, CameraShake.Arrival(CameraShake.SoundSpeed), 1e-5f);
            Assert.AreEqual(0f, CameraShake.Arrival(-3f));
            int before = CameraShake.Pending;
            Assume.That(before, Is.LessThan(24), "the queue is full from earlier tests");
            CameraShake.Add(Vector3.zero, 8f);
            Assert.AreEqual(before + 1, CameraShake.Pending, "the kick waits for the sound rather than landing at once");
        }
    }
}
