// Phase: B5 (critic fixes P2a, P3a, P4a, CODE_REVIEW_2026-10-04.md lines 389, 391, 392) — Lift keyed off the clip's
// own phase instead of time since the climb began; a clip started as a turn ends dropping that turn's yaw; a
// vehicle's drawn yaw frozen at its spawn yaw. No sim is stepped: the tests place units and tick the controller alone.
using System;
using NUnit.Framework;
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
            using var r = new Rig();
            int man = r.M.World.Spawn(0, 0, new float3(10f, 0f, 10f), 100f, 3f, false);
            r.M.World.StanceOf[man] = (byte)Stance.Vault;

            float prev = 0f;
            bool reachedFull = false;
            for (int t = 0; t < 40; t++)
            {
                r.Tick();
                float lift = r.A.Lift[man];
                Assert.GreaterOrEqual(lift, prev, "[P2a] a climbing man's Lift must never fall back down mid-climb (tick " + t + ")");
                prev = lift;
                if (t >= 17 && lift >= 1f) reachedFull = true;
            }
            Assert.IsTrue(reachedFull, "[P2a] after 0.8s+ of climbing, Lift should reach and hold at 1");
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
    }
}
