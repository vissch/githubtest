// Phase: SHOW (act-officer-rally, 2026-10-23) — an officer pulling men off the floor is invisible (AuraSystem.Unpin
// caps a covered man's suppression every tick and says nothing). RallyWatch reads the cap's rising edge and
// AnimationController.Rally.cs draws it: the man it just held off the floor comes up to aim, and the officer waves.
// The sim is stepped for real (MatchSim.Step, as Tests/Sim/OfficerTests.cs does), so AuraSystem.Unpin is the real one.
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Tests
{
    public class RallyTests
    {
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
                row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
                phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                A = new AnimationController(cfg.MaxSlots, M.Map, M.Gas, row, phase, yaw);
            }

            public int Man(float3 at, byte archetype = InfantryArchetype.Rifle) => W.Spawn(0, archetype, at, 100f, 3f, false);

            void StepSim()
            {
                using var cmds = new NativeArray<SimCommand>(0, Allocator.Temp);
                M.Step(cmds);
            }

            public void Tick()
            {
                StepSim();
                A.Tick(W);
                A.Advance(W.Config.TickSeconds);
                W.Events.Clear();
            }

            public void Dispose()
            {
                A.Dispose(); M.Dispose();
                row.Dispose(); phase.Dispose(); yaw.Dispose();
            }
        }

        static readonly float3 Here = new float3(150f, 0f, 300f);

        static bool Cowering(Clip c) => c == Clip.ProneIdle || c == Clip.Shield || c == Clip.HitProne || c == Clip.ProneFlinch;

        [Test]
        public void TheMenTheAuraCapsPlayTheGetUp()
        {
            using var r = new Rig();
            var men = new int[4];
            for (int k = 0; k < 4; k++) men[k] = r.Man(Here + new float3(k * 1.5f, 0f, 0f));

            // pin them: force their suppression above the threshold until the ladder draws them flat
            bool AllFlat()
            {
                foreach (var m in men) if (!Cowering(r.A.State[m].Clip) && r.A.State[m].Stance != (byte)Stance.Prone) return false;
                return true;
            }
            for (int t = 0; t < 120 && !AllFlat(); t++)
            {
                foreach (var m in men) r.W.Suppression[m] = 100f;
                r.Tick();
            }
            Assert.IsTrue(AllFlat(), "setup: the aura must cap men who are actually pinned, not standing");

            int officer = r.Man(Here + new float3(0f, 0f, 6f), InfantryArchetype.Officer);
            int ticksFor1s = (int)math.ceil(1f / r.W.Config.TickSeconds);
            var rallyTick = new int[4]; for (int k = 0; k < 4; k++) rallyTick[k] = -1;
            var getUpTick = new int[4]; for (int k = 0; k < 4; k++) getUpTick[k] = -1;

            for (int t = 0; t < ticksFor1s + 30; t++)
            {
                r.Tick();
                for (int k = 0; k < 4; k++)
                {
                    if (rallyTick[k] < 0 && r.A.RallyWatch.RalliedTick(men[k])) rallyTick[k] = t;
                    if (getUpTick[k] < 0 && r.A.State[men[k]].Clip == Clip.GetUp) getUpTick[k] = t;
                }
            }

            Assert.AreEqual(4, r.A.RallyWatch.Rallies, "every man the aura capped was seen rallying, exactly once each");
            for (int k = 0; k < 4; k++)
            {
                Assert.GreaterOrEqual(rallyTick[k], 0, $"man {k}: the aura never capped him");
                Assert.GreaterOrEqual(getUpTick[k], 0, $"man {k}: no get-up");
                Assert.LessOrEqual(getUpTick[k] - rallyTick[k], ticksFor1s, $"man {k}: get-up more than 1 s after the cap");
            }
            Assert.GreaterOrEqual(r.A.RallyWatch.Gestures, 1, "the officer waves at least once per rally");
        }

        [Test]
        public void WithNoOfficerNobodyIsCappedAndNobodyGetsUp()
        {
            using var r = new Rig();
            var men = new int[4];
            for (int k = 0; k < 4; k++) men[k] = r.Man(Here + new float3(k * 1.5f, 0f, 0f));

            for (int t = 0; t < 120; t++)
            {
                foreach (var m in men) r.W.Suppression[m] = 100f;
                r.Tick();
                foreach (var m in men) Assert.AreNotEqual(Clip.GetUp, r.A.State[m].Clip, "no officer: nothing lifts him off the floor");
            }
            Assert.AreEqual(0, r.A.RallyWatch.Rallies);
            Assert.AreEqual(0, r.A.RallyWatch.Gestures);
        }

        [Test]
        public void TheOldCodeNeverDrawsTheRally()
        {
            AnimationController.DrawRally = false;
            try
            {
                using var r = new Rig();
                var men = new int[4];
                for (int k = 0; k < 4; k++) men[k] = r.Man(Here + new float3(k * 1.5f, 0f, 0f));
                bool AllFlat()
                {
                    foreach (var m in men) if (!Cowering(r.A.State[m].Clip) && r.A.State[m].Stance != (byte)Stance.Prone) return false;
                    return true;
                }
                for (int t = 0; t < 120 && !AllFlat(); t++)
                {
                    foreach (var m in men) r.W.Suppression[m] = 100f;
                    r.Tick();
                }
                Assert.IsTrue(AllFlat(), "setup: the aura must cap men who are actually pinned, not standing");

                r.Man(Here + new float3(0f, 0f, 6f), InfantryArchetype.Officer);
                for (int t = 0; t < 60; t++) r.Tick();

                foreach (var m in men) Assert.AreNotEqual(Clip.GetUp, r.A.State[m].Clip, "DrawRally off: the reader exists but nothing calls it");
            }
            finally { AnimationController.DrawRally = true; }
        }
    }
}
