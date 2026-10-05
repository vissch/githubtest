// Phase: A5b (2026-10-05) — which way a man's body faces while he walks (AnimationController.Decide): the way he has
// been going, not this tick's step. A man shoved back one step (by the man ahead of him in a trench, by separation in a
// crowd) used to be turned about for it and turned again on his next step forward; a man who really turns about must
// still show it at once. No sim is stepped: a man is moved by hand and the controller is ticked, as in AnimationPinTests.
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Tests
{
    public class BodyFacingTests
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

            /// <summary>A man walking up the field (+z) at a rifleman's pace for a second, so his heading has settled.</summary>
            public int Walker()
            {
                int man = W.Spawn(0, 0, Here, 100f, 3f, false);
                Tick();
                for (int t = 0; t < 20; t++) Step(man, 1f);
                return man;
            }

            /// <summary>One tick's step along z, forward (1) or back (-1), at 3 m/s.</summary>
            public void Step(int man, float dir)
            {
                var p = W.Position[man]; p.z += dir * 3f * W.Config.TickSeconds; W.Position[man] = p;
                Tick();
            }

            void Tick()
            {
                A.Tick(W);
                A.Advance(W.Config.TickSeconds);
                W.Events.Clear();
                W.Tick++;
            }

            /// <summary>How far his feet point from `want`, radians, 0 to pi.</summary>
            public float Off(int man, float want)
            {
                float d = A.State[man].BodyYaw - want;
                return math.abs(math.atan2(math.sin(d), math.cos(d)));
            }

            public void Dispose()
            {
                A.Dispose(); M.Dispose();
                row.Dispose(); phase.Dispose(); yaw.Dispose();
            }
        }

        [Test]
        public void AStepBackDoesNotTurnAWalkingManAbout()
        {
            using var r = new Rig();
            int man = r.Walker();
            Assert.Less(r.Off(man, 0f), 0.2f, "the control: walking up the field he faces up the field");
            r.Step(man, -1f);
            Assert.Less(r.Off(man, 0f), 0.2f, "shoved back one step, he still faces the way he was going");
            r.Step(man, 1f);
            Assert.Less(r.Off(man, 0f), 0.2f, "and on his next step forward");
            for (int t = 0; t < 12; t++) { r.Step(man, t % 4 == 3 ? -1f : 1f); Assert.Less(r.Off(man, 0f), 0.2f, "forward, forward, forward, back: step " + t); }
        }

        [Test]
        public void AManWhoTurnsAboutFacesHisNewWayWithinFourTicks()
        {
            using var r = new Rig();
            int man = r.Walker();
            for (int t = 0; t < 4; t++) r.Step(man, -1f);
            Assert.Less(r.Off(man, math.PI), 0.2f, "four steps back down the field and his feet point down the field");
        }
    }
}
