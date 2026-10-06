// Phase: tooling (the gym, 2026-09-28) — the gym's clip pin (AnimationController.Pin.cs): a pinned man is drawn in the
// pinned clip tick after tick whatever the ladder would choose, a pinned one-shot plays again after its hold, the pin
// lets go when another man takes the slot, a death still wins over a pin, and a match that never pins has no pins.
// No sim is stepped: men are placed and the controller is ticked, as in DeathVarietyTests.
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Tests
{
    public class AnimationPinTests
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
        public void APinnedManIsDrawnInThePinnedClipTickAfterTick()
        {
            using var r = new Rig();
            int man = r.Man(Here);
            r.Tick();
            Assert.AreNotEqual(Clip.Wade, r.A.State[man].Clip, "the control: on dry ground nothing but the pin makes him wade");
            Assert.IsTrue(r.A.Pin(man, Clip.Wade, r.W.Generation[man]));
            for (int t = 0; t < 60; t++)
            {
                r.Tick();
                Assert.AreEqual(Clip.Wade, r.A.State[man].Clip, "tick " + t);
                Assert.AreEqual((ushort)Clip.Wade, r.A.Row[man], "the renderer is handed the pinned clip, tick " + t);
            }
            Assert.AreEqual(1, r.A.PinnedCount);
        }

        /// <summary>The gym spawns a man and pins him in the same frame, before the controller has ever seen him
        /// (critic r2: a pin that took State's generation let go on the man's first tick).</summary>
        [Test]
        public void AManPinnedTheFrameHeIsSpawnedStaysPinned()
        {
            using var r = new Rig();
            r.Tick();
            int man = r.Man(Here);
            Assert.IsTrue(r.A.Pin(man, Clip.Wade, r.W.Generation[man]));
            r.Tick(); r.Tick();
            Assert.AreEqual(Clip.Wade, r.A.State[man].Clip);
            Assert.AreEqual(1, r.A.PinnedCount);
        }

        [Test]
        public void APinnedOneShotPlaysAgainAfterItsHold()
        {
            using var r = new Rig();
            int man = r.Man(Here);
            r.Tick();
            r.A.Pin(man, Clip.FireStand, r.W.Generation[man]);
            r.Tick();
            uint first = r.A.State[man].ClipStart;
            float seconds = Clips.Table[(int)Clip.FireStand].Seconds + AnimationController.PinHold;
            int ticks = (int)math.ceil(seconds / r.W.Config.TickSeconds) + 4;
            for (int t = 0; t < ticks; t++) r.Tick();
            Assert.AreEqual(Clip.FireStand, r.A.State[man].Clip);
            Assert.Greater(r.A.State[man].ClipStart, first, "the one-shot started again once it had played and held");
        }

        [Test]
        public void ThePinLetsGoWhenAnotherManTakesTheSlot()
        {
            using var r = new Rig();
            int man = r.Man(Here);
            r.Tick();
            r.A.Pin(man, Clip.Wade, r.W.Generation[man]);
            r.Tick();
            r.W.Despawn(man);
            r.Tick();
            int next = r.Man(Here);
            Assert.AreEqual(man, next, "free slots are reused last in, first out");
            for (int t = 0; t < 5; t++) r.Tick();
            Assert.AreEqual(Clip.None, r.A.PinnedClip(next), "the pin was the first man's");
            Assert.AreEqual(0, r.A.PinnedCount);
        }

        [Test]
        public void ADeathWinsOverAPin()
        {
            using var r = new Rig();
            int man = r.Man(Here);
            r.Tick();
            r.A.Pin(man, Clip.Wade, r.W.Generation[man]);
            r.Tick();
            r.W.Despawn(man, (int)DeathCause.Blast, new float3(0f, 1f, 0f), 0f);
            r.Tick();
            Assert.IsTrue(r.A.State[man].Dead, "he died");
            Assert.AreNotEqual(Clip.Wade, r.A.State[man].Clip, "and plays a death, not the pinned clip");
        }

        [Test]
        public void AMatchThatNeverPinsHasNoPins()
        {
            using var r = new Rig();
            int man = r.Man(Here);
            r.Tick();
            Assert.AreEqual(0, r.A.PinnedCount);
            Assert.AreEqual(Clip.None, r.A.PinnedClip(man));
            Assert.IsFalse(r.A.Pin(-1, Clip.Idle, 0), "no slot, no pin");
            Assert.IsFalse(r.A.Pin(man, Clip.Count, r.W.Generation[man]), "no such clip");
            Assert.IsTrue(r.A.Pin(man, Clip.None, r.W.Generation[man]), "Clip.None unpins");
            Assert.AreEqual(0, r.A.PinnedCount);
        }
    }
}
