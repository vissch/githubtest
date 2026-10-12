// Phase: B5 / docs/15 — the render-frame half of the character controller walks only the men it draws. No sim is
// stepped: the tests place men, kill half of them through SimWorld.Despawn (which writes the Death event the way
// every system does, the rig of Tests/Show/DeathVarietyTests.cs) and tick the controller.
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
    public class AnimAdvanceTests
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

        /// <summary>[Z004.1] A dead man's slot still held a death clip and took the whole render-frame path every frame,
        /// although the living pass never draws it (SimPresenter keeps only Alive slots). Advance now walks only the slots
        /// Tick decided a clip for, so a dead man's row and phase stand still while a living man's phase moves on.</summary>
        [Test]
        public void ADeadMansSlotIsNotAdvanced()
        {
            using var r = new Rig();
            var men = new int[20];
            for (int i = 0; i < men.Length; i++) men[i] = r.Man(Here + new float3(i * 2f, 0f, 0f));
            r.Tick();
            for (int i = 0; i < 10; i++) r.W.Despawn(men[i], men[19], new float3(0f, 0f, 1f), 0f);   // shot by the man at the end of the row
            r.Tick();

            int dead = men[0], living = men[19];
            Assert.IsTrue(r.A.State[dead].Dead, "[Z004.1] the rig did not kill the man it meant to");
            float deadPhase = r.A.Phase[dead]; ushort deadRow = r.A.Row[dead];
            float livingPhase = r.A.Phase[living];

            for (int f = 0; f < 3; f++) r.A.Advance(0.1f);

            Assert.AreEqual(deadPhase, r.A.Phase[dead], 0f, "[Z004.1] the dead man's clip still runs every frame");
            Assert.AreEqual(deadRow, r.A.Row[dead], "[Z004.1] the dead man's row is still rewritten every frame");
            Assert.AreNotEqual(livingPhase, r.A.Phase[living], "[Z004.1] the living man's clip stopped running");
            // no assert on AdvancedCount: this test must compile against the code before the fix, which has no such counter
        }

        /// <summary>The job's arithmetic, held against the clip table's authored lengths and not against a run of the
        /// code: a loop's phase is the seconds advanced over the clip's length, a one-shot's clamps at its end.</summary>
        [Test]
        public void PhaseIsTheSecondsAdvancedOverTheClipLength()
        {
            using var r = new Rig();
            int a = r.Man(Here);
            int b = r.Man(Here + new float3(4f, 0f, 0f));
            r.Tick();

            var sa = r.A.State[a]; sa.Clip = Clip.Idle; sa.Frame = 0f; sa.Rate = 1f; sa.Blend = 0f; r.A.State[a] = sa;
            var sb = r.A.State[b]; sb.Clip = Clip.FireSnap; sb.Frame = 0f; sb.Rate = 1f; sb.Blend = 0f; r.A.State[b] = sb;

            float idle = Clips.Table[(int)Clip.Idle].Seconds;         // 2.4 s as authored, and the loop flag is set
            float snap = Clips.Table[(int)Clip.FireSnap].Seconds;     // 0.27 s, a one-shot
            Assert.IsTrue(Clips.Table[(int)Clip.Idle].Loop, "Idle is a loop");
            Assert.IsFalse(Clips.Table[(int)Clip.FireSnap].Loop, "FireSnap is a one-shot");

            r.A.Advance(0.6f);

            Assert.AreEqual(0.6f / idle, r.A.Phase[a], 1e-5f, "a loop's phase is the seconds advanced over its length");
            Assert.AreEqual((ushort)Clip.Idle, r.A.Row[a], "the row is the clip");
            Assert.Greater(0.6f, snap, "the one-shot is shorter than the step, so it is over");
            Assert.AreEqual(1f, r.A.Phase[b], 1e-5f, "a one-shot past its end clamps at phase 1");
        }
    }
}
