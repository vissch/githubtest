// Phase: tooling (2026-09-23) — a sim tick must not allocate, measured with an army on the field. Attributes the
// bytes to one of the four things SimHost does on a tick: step the drivers, capture the presenter, tick the
// animation controller, collect the events.
//
// Why a test and not a profiler session. The ~400 KB periodic allocation went unexplained for a day. The editor
// profiler could say it was inside SimHost.Update's subtree, which is true and was not enough; its callstack
// resolution then pointed confidently at one line, and that pointer could not be corroborated — the first attempt
// merged every allocation into one sample and resolved a single representative stack, and the second returned
// exactly one distinct site out of 40,899 samples, which is the signature of the same artefact rather than an
// answer. A number that cannot be reproduced is not a measurement.
//
// Why an army. The first version of this file built a fresh greybox match and measured a tick on an EMPTY field,
// and passed, which proved only that a tick is cheap when there is nothing to simulate. The 403 KB was measured
// with 1,997 men out. A green test that does not recreate the condition is worse than no test, because it argues
// you are fine. So this deploys until the field is populated, reports the population it actually achieved in every
// message, and measures that.
//
// Measured with TW.Perf.AllocProbe: the COUNT of managed allocations on this thread (GC.Alloc samples). The first
// version measured bytes with GC.GetAllocatedBytesForCurrentThread, which under Unity's Boehm GC is a stub that reads
// 0 whatever the code does, so it cleared AnimationController.Tick while that built a string for every man on every
// tick: the ~400 KB lump itself (perf pass, 2026-09-23). NativeArray and Burst allocations are native and correctly
// invisible here.
using System;
using NUnit.Framework;
using TW.Sim;
using TW.Sim.Match;
using TW.Net;
using TW.Presentation;
using TW.Perf;

namespace TW.Tests
{
    public class TickAllocationTests
    {
        /// <summary>Men each side is asked for. Kept well under the 2,000-man stress preset so the test stays a
        /// second or two; the allocation we are hunting is per tick, so it does not need the full army to show.</summary>
        const int WantMen = 300;
        const int DeployTicks = 400;

        // Allocations per tick allowed on each path. Zero is the budget (docs/05); the driver pair's is the measured
        // cost of LockstepDriver's per-tick Dictionary and ToArray pair, which the one-world change removes.
        const double DriverBudget = 16, CaptureBudget = 0, AnimateBudget = 0, CollectBudget = 0;

        sealed class Rig : IDisposable
        {
            public MatchSim Local, Peer;
            public LockstepDriver A, B;
            public SimPresenter Presenter;
            public AnimationController Animation;
            public EventPump Events;

            public Rig()
            {
                var cfg = SimConfig.Default;
                cfg.StartingSilver = 400000;   // silver is the only brake on deployment; we want it off
                Local = MatchSim.CreateGreybox(cfg);
                Peer = MatchSim.CreateGreybox(cfg);
                var net = new LoopbackNetwork(2, 0, 0f, cfg.Seed);
                A = new LockstepDriver(Local.World, net.A);
                B = new LockstepDriver(Peer.World, net.B);
                Presenter = new SimPresenter(cfg.MaxSlots);
                Animation = new AnimationController(cfg.MaxSlots, Local.Map, Local.Gas,
                                                    Presenter.RowIn, Presenter.PhaseIn, Presenter.YawIn);
                Events = new EventPump();
            }

            /// <summary>Deploy riflemen from both sides until the field holds men, doing everything SimHost does on
            /// a tick so the buffers this test measures are grown by the same traffic that will exercise them.</summary>
            public void Populate()
            {
                for (int i = 0; i < DeployTicks && Local.World.AliveCount < WantMen; i++)
                {
                    A.Issue(SimCommand.Deploy(Local.World.Tick, 0, 0));
                    B.Issue(SimCommand.Deploy(Peer.World.Tick, 1, 0));
                    Step();
                }
            }

            public void Step()
            {
                bool a = A.TryStep();
                B.TryStep();
                if (!a) return;
                Presenter.Capture(Local.World);
                Animation.Tick(Local.World);
                Events.Collect(Local.World);
                Events.Frame.Clear();
            }

            public void Dispose() { Local?.Dispose(); Peer?.Dispose(); }
        }

        /// <summary>Allocations a block makes, averaged over `reps`, after `warm` repetitions have grown every buffer
        /// it will ever grow. Without the warm-up this counts one-time capacity growth and calls it a leak.</summary>
        static double PerCall(Action step, int warm, int reps) => AllocProbe.PerCall(step, warm, reps);

        [Test]
        public void ATickWithAnArmyOutDoesNotAllocateHundredsOfKilobytes()
        {
            using var rig = new Rig();
            rig.Populate();
            int men = rig.Local.World.AliveCount;

            double drivers = PerCall(() => { rig.A.TryStep(); rig.B.TryStep(); }, 60, 120);
            double capture = PerCall(() => rig.Presenter.Capture(rig.Local.World), 20, 100);
            double animate = PerCall(() => rig.Animation.Tick(rig.Local.World), 20, 100);
            double collect = PerCall(() => { rig.Events.Collect(rig.Local.World); rig.Events.Frame.Clear(); }, 20, 100);
            double total = drivers + capture + animate + collect;

            // Events per tick, reported because Events.Dispatch's cost is its SUBSCRIBERS' (MonoBehaviours this rig does
            // not build), and a subscriber that allocates per event bills events-per-tick times that. With a counter
            // that sees allocations, the ~400 KB lump turned out to be Animation.Tick (1,518 allocations a tick with
            // 300 men, a `why` string per man; 0 since 2026-09-23), not a subscriber.
            rig.Events.Frame.Clear();
            rig.Step();
            rig.Events.Collect(rig.Local.World);
            int eventsPerTick = rig.Events.Frame.Count;
            rig.Events.Frame.Clear();

            string breakdown = $"allocations a tick with {men} men alive — drivers {drivers:0.#}, Presenter.Capture {capture:0.#}, " +
                               $"Animation.Tick {animate:0.#}, Events.Collect {collect:0.#}, total {total:0.#}; " +
                               $"{eventsPerTick} events a tick" +
                               (eventsPerTick == 0
                                   ? ". NOTE: no events, so the men are deployed but not yet in contact and this run "
                                     + "does NOT cover the systems that only work in contact (acquisition, fire, "
                                     + "suppression) or Events.Dispatch's subscribers."
                                   : " reach Dispatch and its subscribers");
            TestContext.WriteLine(breakdown);

            // A population of nearly nothing would make this test pass for the wrong reason, which is exactly how
            // its first version passed. Fail loudly rather than quietly measuring an empty field.
            Assert.That(men, Is.GreaterThan(WantMen / 4),
                $"only {men} men deployed in {DeployTicks} ticks, so this measured an almost empty field and proves " +
                "nothing about the per-tick allocation. Fix the deployment, do not relax the budget.");

            // The budget is zero a frame (docs/05), asserted per path so a red names the path that allocates.
            Assert.That(animate, Is.LessThanOrEqualTo(AnimateBudget),
                "AnimationController.Tick allocates per tick: at 20 Hz with an army out this is the periodic GC spike " +
                $"the frame budget kept failing on (a string per man per tick was the ~400 KB lump). {breakdown}");
            Assert.That(capture, Is.LessThanOrEqualTo(CaptureBudget), $"SimPresenter.Capture allocates per tick. {breakdown}");
            Assert.That(collect, Is.LessThanOrEqualTo(CollectBudget), $"EventPump.Collect allocates per tick. {breakdown}");
            Assert.That(drivers, Is.LessThanOrEqualTo(DriverBudget), $"the lockstep drivers allocate more per tick than they did. {breakdown}");
        }

        /// <summary>
        /// The driver alone, with the tightest bound, because it was the suspect the callstack named. A lockstep
        /// tick handles a handful of commands: there is no honest reason for it to allocate kilobytes.
        /// </summary>
        [Test]
        public void TheLockstepDriverDoesNotAllocatePerTick()
        {
            using var rig = new Rig();
            rig.Populate();
            double perTick = PerCall(() => { rig.A.TryStep(); rig.B.TryStep(); }, 60, 120);
            TestContext.WriteLine($"LockstepDriver pair with {rig.Local.World.AliveCount} men: {perTick:0.#} allocations per tick");
            Assert.That(perTick, Is.LessThanOrEqualTo(DriverBudget),
                $"stepping both drivers makes {perTick:0.#} allocations a tick with {rig.Local.World.AliveCount} men out");
        }
    }
}
