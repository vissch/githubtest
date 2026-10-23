// Phase: act-rush-and-drop (2026-10-23) — the rush and drop under fire, as the character controller shows it. The sim
// moves a man whose SimWorld.Alarm is at StanceRules.CarefulAlarm in rushes: RushUpTicks bent double at RushSpeed, then
// flat in the nearest shell hole (StanceRules, MovementSystem). The controller never read Alarm, so the dash was drawn
// upright (Sprint -> Standing) and the drop as a 1.97 s lie-down (KneelToProne). These tests hold the new read: the
// rush up is a crouched run, the drop is a dive flat inside the owner's half second, an unalarmed man still lies down,
// and the sim's per-man beats stay staggered.
// No sim is stepped: the men are placed, walked and given their stance by hand (AnimationPinTests' rig).
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
    public class RushDropTests
    {
        static readonly float3 Where = new float3(150f, 0f, 300f);
        const float Alarmed = 20f;   // over StanceRules.CarefulAlarm (8), under AlarmCap

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
                if (stray != null) stray.ShellsPerMinute = 0f;   // no stray shell over the men: the blast dive would win
                M.World.Tick = 1000;   // the controller's rate limits are against tick 0
                row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
                phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                A = new AnimationController(cfg.MaxSlots, M.Map, M.Gas, row, phase, yaw);
            }

            public int Man(float dx = 0f, float dz = 0f) => W.Spawn(0, (byte)InfantryArchetype.Rifle, Where + new float3(dx, 0f, dz), 100f, 3f, false);

            /// <summary>His stance and his alarm, as MovementSystem would have left them this tick.</summary>
            public void Sim(int slot, Stance stance, float alarm) { W.StanceOf[slot] = (byte)stance; W.Alarm[slot] = alarm; }

            /// <summary>A step of his rush, written before the tick that reads it (the controller's speed is the delta).</summary>
            public void Step(int slot, float mps) { var p = W.Position[slot]; p.z += mps * W.Config.TickSeconds; W.Position[slot] = p; }

            public void Tick()
            {
                A.Tick(W);
                A.Advance(W.Config.TickSeconds);
                W.Events.Clear();
                W.Tick++;
            }

            public Clip ClipOf(int slot) => A.State[slot].Clip;

            public void Dispose() { A.Dispose(); M.Dispose(); row.Dispose(); phase.Dispose(); yaw.Dispose(); }
        }

        static bool Crouched(Clip c) => Clips.Table[(int)c].Stance == (byte)Stance.Crouch;
        static bool Flat(Clip c) => Clips.Table[(int)c].Stance == (byte)Stance.Prone;

        /// <summary>The dash under fire is bent double, not an upright jog: the sim's Sprint used to be drawn Standing
        /// whatever the alarm, which is the whole complaint.</summary>
        [Test]
        public void AnAlarmedSprinterIsDrawnCrouched()
        {
            using var r = new Rig();
            int man = r.Man();
            r.W.Events.Clear();
            // he starts from a standstill, so the first tick spends StandToKneel (0.93 s) going down into the dash
            for (int t = 0; t < 40; t++)
            {
                r.Sim(man, Stance.Sprint, Alarmed);
                r.Step(man, 4f);
                r.Tick();
            }
            Assert.AreEqual(Clip.CrouchRun, r.ClipOf(man), "an alarmed man dashing at 4 m/s is drawn " + r.ClipOf(man));
            Assert.AreNotEqual((byte)Stance.Standing, r.A.State[man].Stance, "and is not held in a standing row");
        }

        /// <summary>The drop at the end of a rush is a dive into the hole, flat well inside the owner's half second
        /// (10 ticks); KneelToProne is 1.97 s and used to be held off another 10 ticks on top.</summary>
        [Test]
        public void TheDropAtTheEndOfARushIsFlatWithinHalfASecond()
        {
            using var r = new Rig();
            int man = r.Man();
            r.W.Events.Clear();
            for (int t = 0; t < 60; t++) { r.Sim(man, Stance.Crouch, Alarmed); r.Step(man, 3.45f); r.Tick(); }
            Assert.IsTrue(Crouched(r.ClipOf(man)), "the control: the rush up is crouched, not " + r.ClipOf(man));

            bool dived = false; int flatAt = -1;
            for (int t = 0; t < 10; t++)
            {
                r.Sim(man, Stance.Prone, Alarmed);   // the sim has dropped him where he is and holds him still
                r.Tick();
                if (r.ClipOf(man) == Clip.DiveAway) dived = true;
                if (flatAt < 0 && Flat(r.ClipOf(man))) flatAt = t;
            }
            Assert.IsTrue(dived, "he dives into the hole on the way down");
            Assert.GreaterOrEqual(flatAt, 0, "he is in a prone row within 10 ticks, not " + r.ClipOf(man));
            Assert.AreEqual((byte)Stance.Prone, r.A.State[man].Stance);
        }

        /// <summary>A man nobody is shooting at still lies down the slow way: the dive is the rush's, not every drop's.</summary>
        [Test]
        public void AnUnalarmedManStillLiesDown()
        {
            using var r = new Rig();
            int man = r.Man();
            r.W.Events.Clear();
            for (int t = 0; t < 60; t++) { r.Sim(man, Stance.Crouch, 0f); r.Step(man, 3.45f); r.Tick(); }
            for (int t = 0; t < 10; t++) { r.Sim(man, Stance.Prone, 0f); r.Tick(); }
            Assert.AreEqual(Clip.KneelToProne, r.ClipOf(man), "with no alarm he goes down flat through the transition");
        }

        /// <summary>A section on the sim's own beats: most of the men running are drawn crouched, and they do not all
        /// come up on the same tick (the beats are per man, MovementSystem; the dive must not bunch them).</summary>
        [Test]
        public void ASectionOnItsOwnBeatsStaysStaggered()
        {
            using var r = new Rig();
            var men = new int[20];
            for (int k = 0; k < men.Length; k++) men[k] = r.Man(k * 1.6f);
            r.W.Events.Clear();
            var upAt = new System.Collections.Generic.HashSet<uint>();
            int up = 0, upCrouched = 0;
            var wasUp = new bool[men.Length];
            int spread = StanceRules.RushTicks / men.Length;
            for (int t = 0; t < StanceRules.RushTicks; t++)
            {
                for (int k = 0; k < men.Length; k++)
                {
                    // his own beat, spread over the rush: the sim's is a hash of slot and tick, here one offset a man
                    bool rushing = (t + k * spread) % StanceRules.RushTicks < StanceRules.RushUpTicks;
                    r.Sim(men[k], rushing ? Stance.Crouch : Stance.Prone, Alarmed);
                    if (rushing) r.Step(men[k], 3.45f);
                }
                r.Tick();
                for (int k = 0; k < men.Length; k++)
                {
                    bool rushing = (t + k * spread) % StanceRules.RushTicks < StanceRules.RushUpTicks;
                    if (rushing && t > 8)
                    {
                        up++;
                        if (Crouched(r.ClipOf(men[k]))) upCrouched++;
                        if (!wasUp[k]) upAt.Add(r.W.Tick);
                    }
                    wasUp[k] = rushing;
                }
            }
            Assert.Greater(up, 0, "the control: men were counted while running");
            Assert.GreaterOrEqual(upCrouched / (float)up, 0.7f, "only " + upCrouched + " of " + up + " man-ticks in the rush up are drawn crouched");
            Assert.Greater(upAt.Count, 1, "the men come up on their own ticks, not all on one");
        }
    }
}
