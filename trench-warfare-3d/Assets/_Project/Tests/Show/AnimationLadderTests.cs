// Phase: B5 (critic fixes P2a, P3a, P4a, CODE_REVIEW_2026-10-04.md lines 389, 391, 392) — Lift keyed off the clip's
// own phase instead of time since the climb began; a clip started as a turn ends dropping that turn's yaw; a
// vehicle's drawn yaw frozen at its spawn yaw. No sim is stepped: the tests place units and tick the controller alone.
using System;
using NUnit.Framework;
using UnityEngine;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Tests
{
    public class AnimationLadderTests
    {
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
        public void AClimbingManRisesUpTheParapetWithoutSnappingBack()
        {
            // [P2a][W2.2] the old lift read saturate(Frame/0.8): ClimbOut (0.53s = ~11 ticks) hands off to ClimbHold,
            // a 1s LOOP whose own Frame wraps back toward 0 once a second while the climb keeps going. Running 50
            // ticks (ClimbOut's ~11 plus more than one full ClimbHold loop of 20) puts a wrap inside the window
            // whatever phase ClimbHold happens to start the loop hashed to, so the old bug cannot dodge it by luck.
            using var r = new Rig();
            int man = r.M.World.Spawn(0, 0, new float3(10f, 0f, 10f), 100f, 3f, false);
            r.M.World.StanceOf[man] = (byte)Stance.Vault;

            float prev = 0f;
            bool climbing = false;
            int sinceClimb = -1;
            for (int t = 0; t < 50; t++)
            {
                r.Tick();
                var clip = r.A.State[man].Clip;
                bool isClimb = clip == Clip.ClimbOut || clip == Clip.ClimbHold || clip == Clip.ClimbLadder;
                if (isClimb) sinceClimb = climbing ? sinceClimb + 1 : 1;   // ClimbTime already carries this tick's dt
                climbing = isClimb;

                float lift = r.A.Lift[man];
                Assert.GreaterOrEqual(lift, prev - 1e-5f, "[P2a][W2.2] a climbing man's Lift must never fall back down mid-climb (tick " + t + ")");
                prev = lift;

                if (!isClimb || sinceClimb < 0) continue;
                // 0.8s to full at 20 Hz = 16 ticks, so each tick ramps 0.0625 of the way there
                float want = Mathf.Min(1f, sinceClimb * 0.0625f);
                Assert.AreEqual(want, lift, 0.01f,
                    $"[P2a][W2.2] tick {sinceClimb} since the climb began should give Lift {want:F3}, not {lift:F3} "
                  + "(ClimbHold's own Frame wraps once a second; Lift must key off time since the climb started, not the clip's phase)");
                if (want >= 1f)
                    Assert.AreEqual(1f, lift, 1e-4f, $"[P2a][W2.2] tick {sinceClimb}: Lift reached 1 but must stay exactly 1, not fall back as ClimbHold loops");
            }
            Assert.GreaterOrEqual(sinceClimb, 16, "[P2a][W2.2] the premise: the climb ran long enough (16+ ticks) to prove Lift held at 1 through a ClimbHold wrap");
        }

        [Test]
        public void AClipStartedAsATurnEndsKeepsTheTurnsYaw()
        {
            using var r = new Rig();
            int man = r.M.World.Spawn(0, 0, new float3(0f, 0f, 0f), 100f, 3f, false);
            int enemy = r.M.World.Spawn(1, 0, new float3(0f, 0f, -10f), 100f, 3f, false);
            r.M.World.Speed[man] = 0f;
            r.M.World.TargetSlot[man] = enemy;

            int guard = 0;
            while (guard++ < 400 && r.A.State[man].Clip != Clip.Turn180)
                r.Tick();
            Assert.Less(guard, 400, "[P3a] expected the man to start Turn180 to face the enemy behind him");

            // run the turn to just before it finishes (1.9s clip, frame must still read < Seconds - 0.02 to be "playing")
            while (r.A.State[man].Clip == Clip.Turn180 && r.A.State[man].Frame < 1.88f)
                r.Tick();

            // the turn is about to finish this very tick: land a Hit on this same tick so the reaction rung (which
            // returns before rung 6's own apply) is the one that starts a new clip, not rung 6 itself
            r.M.World.Events.Add(r.M.World.Tick, SimEventType.Hit, enemy, man, new float3(0f, 0f, 0f), new float3(0f, 0f, 1f), 1f);
            r.Tick();

            var s = r.A.State[man];
            Assert.AreEqual(s.TurnTo, s.ShownYaw, 0.25f, "[P3a] a clip started as the turn ends must still carry the turn's yaw, not drop it");
            Assert.AreEqual(s.TurnTo, s.BodyYaw, 1e-3f, "[P3a] BodyYaw must also land on TurnTo, not the pre-turn facing");
        }

        [Test]
        public void AVehiclesDrawnYawFollowsTheSim()
        {
            using var r = new Rig();
            int v = r.M.World.Spawn(0, 0, new float3(5f, 0f, 5f), 100f, 3f, true);
            r.Tick();   // Fresh takes the spawn yaw (0 for team 0)

            r.M.World.Yaw[v] = math.PI / 2f;
            r.Tick();

            Assert.AreEqual(math.PI / 2f, r.A.Yaw[v], 1e-4f, "[P4a] a vehicle's drawn yaw must follow the sim's yaw every tick, not freeze at spawn");
        }

        [Test]
        public void AManWhoFillsADeadMansSlotPlaysNoHitReaction()
        {
            using var r = new Rig();
            int man = r.M.World.Spawn(0, 0, new float3(10f, 0f, 10f), 100f, 3f, false);
            int enemy = r.M.World.Spawn(1, 0, new float3(10f, 0f, 0f), 100f, 3f, false);
            r.Tick();

            r.M.World.Generation[man]++;   // a newcomer takes the slot this same tick
            r.M.World.Events.Add(r.M.World.Tick, SimEventType.Hit, enemy, man, default, new float3(0, 0, 1), 1f);
            r.Tick();

            var s = r.A.State[man];
            Assert.AreNotEqual(Rung.Reaction, s.Rung, "[P6a] a newcomer in a refilled slot must not play the dead man's hit");
            Clip clip = (Clip)r.A.Row[man];
            Assert.IsFalse(clip == Clip.HitStand || clip == Clip.HitWalk || clip == Clip.HitRun || clip == Clip.HitHeavy || clip == Clip.KneelFlinch,
                "[P6a] a newcomer in a refilled slot must not play the dead man's hit");
        }

        [Test]
        public void TheTraceStopsGrowingAtItsCap()
        {
            using var r = new Rig();
            int man = r.M.World.Spawn(0, 0, new float3(10f, 0f, 10f), 100f, 3f, false);
            int enemy = r.M.World.Spawn(1, 0, new float3(10f, 0f, 0f), 100f, 3f, false);
            r.A.FollowSlot = man;
            r.A.TraceCap = 8;
            uint startTick = r.M.World.Tick;

            // a hit every few ticks keeps forcing a new rung-change (Note), well past the cap
            for (int t = 0; t < 200; t++)
            {
                if (t % 5 == 0) r.M.World.Events.Add(r.M.World.Tick, SimEventType.Hit, enemy, man, default, new float3(0, 0, 1), 1f);
                r.Tick();
            }

            Assert.LessOrEqual(r.A.Trace.Count, 8, "[P9a] the trace must not grow without a bound");
            Assert.Greater(r.A.Trace.Count, 0, "the premise: at least one rung change was recorded");
            Assert.Greater(r.A.Trace[0].Tick, startTick, "[P9a] the newest entries, not the oldest, must survive the cap");
        }

        [Test]
        public void TheDeadAnimationStoresAreGone()
        {
            Assert.IsNull(typeof(AnimState).GetField("LastNearMiss"), "[P9a] AnimState.LastNearMiss was never read and must be gone");
            Assert.IsNull(typeof(AnimationController).GetField("leftTrench", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance),
                "[P9a] AnimationController.leftTrench was never read and must be gone");
        }
    }
}
