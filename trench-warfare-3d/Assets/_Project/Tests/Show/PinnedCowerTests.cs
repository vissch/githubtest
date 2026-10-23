// Phase: act-pinned-reads — a pinned man cowers (flat, never aimed, a twitch at a near miss) instead of drawing the
// same ProneIdle row as a calm prone man. No sim is stepped: the tests write the world's stance, suppression and
// Pinned event the way MovementSystem and DirectFire would, and tick the controller alone.
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Tests
{
    public class PinnedCowerTests
    {
        static readonly float3 Pos = new float3(150f, 0f, 300f);
        static readonly Clip[] Aimed = { Clip.FireProne, Clip.FireMG, Clip.AimedIdle, Clip.KneelAimedIdle, Clip.ProneIdle, Clip.AimUp };

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
                if (stray != null) stray.ShellsPerMinute = 0f;
                M.World.Tick = 1000;
                row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
                phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                A = new AnimationController(cfg.MaxSlots, M.Map, M.Gas, row, phase, yaw);
            }

            public int Man(float x) => M.World.Spawn(0, 0, Pos + new float3(x, 0f, 0f), 100f, 3f, false);

            public void Tick()
            {
                A.Tick(M.World);
                A.Advance(M.World.Config.TickSeconds);
                M.World.Events.Clear();
                M.World.Tick++;
            }

            public void Dispose()
            {
                A.Dispose(); M.Dispose();
                row.Dispose(); phase.Dispose(); yaw.Dispose();
            }
        }

        [Test]
        public void APinnedManAndAProneManDrawDifferently()
        {
            using var r = new Rig();
            int a = r.Man(0f), b = r.Man(5f);
            r.M.World.Suppression[a] = 90f; r.M.World.StanceOf[a] = (byte)Stance.Pinned;
            r.M.World.StanceOf[b] = (byte)Stance.Prone;
            r.M.World.Events.Clear();
            for (int t = 0; t < 40; t++) r.Tick();

            Assert.AreEqual(Clip.ProneFlinch, r.A.State[a].Clip, "pinned: cowers (the held Prone Flinch frame)");
            Assert.AreNotEqual(r.A.State[a].Clip, r.A.State[b].Clip, "a pinned man and a prone man must not draw the same clip");
        }

        [Test]
        public void APinnedEventStartsTheCowerWithinSixTicks()
        {
            using var r = new Rig();
            int man = r.Man(0f);
            r.M.World.Events.Clear();
            r.Tick();   // standing, settles

            uint eventTick = r.M.World.Tick;
            r.M.World.Events.Add(eventTick, SimEventType.Pinned, man, 0, Pos);
            r.M.World.StanceOf[man] = (byte)Stance.Pinned;
            r.M.World.Suppression[man] = 90f;

            bool seen = false;
            for (int t = 0; t < 6; t++)
            {
                r.Tick();
                var c = r.A.State[man].Clip;
                if (c == Clip.DiveAway || c == Clip.ProneFlinch) { seen = true; break; }
            }
            Assert.IsTrue(seen, "within 0.3 s (6 ticks at 20 Hz) the Pinned event must have started the dive or the cower");
            Assert.GreaterOrEqual(r.A.State[man].ClipStart, eventTick, "the clip that answers the event must not predate it");
        }

        [Test]
        public void APinnedManIsNeverDrawnInAnAimedRow()
        {
            using var r = new Rig();
            int man = r.Man(0f);
            r.M.World.Suppression[man] = 90f; r.M.World.StanceOf[man] = (byte)Stance.Pinned;
            r.M.World.Events.Clear();
            for (int t = 0; t < 400; t++)
            {
                if (t % 50 == 0) r.M.World.Events.Add(r.M.World.Tick, SimEventType.NearMiss, man, 0, Pos);
                r.Tick();
                Assert.IsFalse(r.A.State[man].Aimed, "a pinned man must never be marked Aimed");
                Assert.False(Array.IndexOf(Aimed, r.A.State[man].Clip) >= 0, "a pinned man must never draw an aimed clip, got " + r.A.State[man].Clip);
            }
        }
    }
}
