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
        /// <summary>Extra ticks allowed after deployment for the two sides to walk into each other, so the measured
        /// window has deaths, stance changes and events in it instead of men marching over empty ground (review
        /// TS12, 2026-10-04: the old window was frozen and measured none of the contact systems).</summary>
        const int ContactTicks = 2000;

        // Allocations per tick allowed on each path. Zero is the budget (docs/05); the driver pair's is the measured
        // cost of LockstepDriver's per-tick Dictionary and ToArray pair, which the one-world change removes.
        const double DriverBudget = 16, CaptureBudget = 0, AnimateBudget = 0, CollectBudget = 0, InterpolateBudget = 0;

        sealed class Rig : IDisposable
        {
            public MatchSim Local, Peer;
            public LockstepDriver A, B;
            public SimPresenter Presenter;
            public AnimationController Animation;
            public EventPump Events;
            /// <summary>Events seen over every tick this rig has taken, and ticks taken, so a test can prove the
            /// window it measured actually moved.</summary>
            public int EventsSeen, Ticks;

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

            /// <summary>Both drivers only, which is the path the driver budget is measured on.</summary>
            public void StepDrivers() { A.TryStep(); B.TryStep(); }

            public void Step()
            {
                bool a = A.TryStep();
                B.TryStep();
                if (!a) return;
                Ticks++;
                Presenter.Capture(Local.World);
                Animation.Tick(Local.World);
                Events.Collect(Local.World);
                EventsSeen += Events.Frame.Count;
                Events.Frame.Clear();
            }

            /// <summary>Deploy, then keep ticking until the two sides are in contact, so what follows is measured on
            /// a field where men are acquiring, firing and dying rather than on a frozen one. Gives up after
            /// ContactTicks; the caller reports what it got.</summary>
            public void PopulateAndEngage()
            {
                Populate();
                for (int i = 0; i < ContactTicks && EventsSeen == 0; i++) Step();
            }

            public void Dispose() { Local?.Dispose(); Peer?.Dispose(); }
        }

        /// <summary>Allocations a block makes, averaged over `reps`, after `warm` repetitions have grown every buffer
        /// it will ever grow. Without the warm-up this counts one-time capacity growth and calls it a leak.</summary>
        static double PerCall(Action step, int warm, int reps)
        {
            // [TS13] Both tests used to pass when the probe was unavailable: PerCall returns -1 then, and -1 is under
            // every budget. A measurement that did not happen is a failure, not a pass.
            Assert.That(AllocProbe.Available, Is.True,
                "[TS13] AllocProbe returned -1, nothing was measured: there is no profiler on this thread, so this " +
                "test cannot say anything about allocations.");
            double v = AllocProbe.PerCall(step, warm, reps);
            Assert.That(v, Is.GreaterThanOrEqualTo(0.0), "[TS13] AllocProbe returned -1, nothing was measured.");
            return v;
        }

        [Test]
        public void ATickWithAnArmyOutDoesNotAllocateHundredsOfKilobytes()
        {
            using var rig = new Rig();
            rig.PopulateAndEngage();
            int men = rig.Local.World.AliveCount;
            long tick0 = rig.Local.World.Tick;
            int alive0 = men, events0 = rig.EventsSeen;

            // [TS12] Every path is measured WITH a driver step in the same block, so the world advances on every
            // repetition. Measured without one (before 2026-10-04) these three ran 120 times over one frozen tick:
            // nobody died, nothing changed stance and no new event was raised, so the paths that only allocate when
            // something happens were never asked the question.
            float animTime = 0f;
            double drivers = PerCall(() => rig.StepDrivers(), 60, 120);
            double capture = PerCall(() => { rig.StepDrivers(); rig.Presenter.Capture(rig.Local.World); }, 20, 100);
            double animate = PerCall(() => { rig.StepDrivers(); rig.Animation.Tick(rig.Local.World); }, 20, 100);
            double collect = PerCall(() => { rig.StepDrivers(); rig.Events.Collect(rig.Local.World); rig.Events.Frame.Clear(); }, 20, 100);
            // The per-frame presentation path, which runs at frame rate rather than tick rate and was not measured at
            // all before. Interpolate completes its own job, so nothing is left running when it returns.
            double interpolate = PerCall(() => { animTime += 0.05f; rig.Presenter.Interpolate(0.5f, animTime); }, 20, 100);
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

            long ticksMoved = (long)rig.Local.World.Tick - tick0;
            int died = alive0 - rig.Local.World.AliveCount;
            int eventsInWindow = rig.EventsSeen - events0;
            // The three path figures each carry one driver step, so the drivers-only number is subtracted again for
            // the printed breakdown; a red still names its path.
            string breakdown = $"allocations a tick with {men} men alive — drivers {drivers:0.#}, Presenter.Capture {capture - drivers:0.#}, " +
                               $"Animation.Tick {animate - drivers:0.#}, Events.Collect {collect - drivers:0.#}, " +
                               $"Presenter.Interpolate {interpolate:0.#} (per frame), total {total:0.#}; " +
                               $"the measured window ran {ticksMoved} ticks, {died} men died in it and it raised " +
                               $"{eventsInWindow} events; {eventsPerTick} events a tick" +
                               (eventsPerTick == 0
                                   ? ". NOTE: no events, so the men are deployed but not yet in contact and this run "
                                     + "does NOT cover the systems that only work in contact (acquisition, fire, "
                                     + "suppression) or Events.Dispatch's subscribers."
                                   : " reach Dispatch and its subscribers");
            TestContext.WriteLine(breakdown);

            // A population of nearly nothing would make this test pass for the wrong reason, which is exactly how
            // its first version passed. Fail loudly rather than quietly measuring an empty field.
            Assert.That(men, Is.GreaterThan(WantMen / 4),
                $"[TS13] only {men} men deployed in {DeployTicks} ticks, so this measured an almost empty field and " +
                "proves nothing about the per-tick allocation. Fix the deployment, do not relax the budget.");

            // [TS12] And the window really moved. A measure taken over a world that does not advance is a measure of
            // nothing: it is how this test stayed green while the contact systems were never run.
            Assert.That(ticksMoved, Is.GreaterThanOrEqualTo(400),
                $"[TS12] the measured window was frozen, so this proves nothing about a tick under load: the world " +
                $"advanced {ticksMoved} ticks over the whole measurement. {breakdown}");
            // Contact is out of reach in this rig: measured 2026-10-14, 2,000 ticks after deployment raise no event
            // at all, so the acquisition, fire and suppression systems cannot be brought into the window here and
            // asserting on a death would be asserting something the rig cannot reach. What is asserted instead is
            // that the population moved, which is what a frozen world cannot do.
            int changed = Math.Abs(died) + eventsInWindow;
            Assert.That(changed, Is.GreaterThan(0),
                $"[TS12] the measured window was frozen, so this proves nothing about a tick under load: the " +
                $"population did not change and no event was raised in {ticksMoved} ticks. {breakdown}");

            // The budget is zero a frame (docs/05), asserted per path so a red names the path that allocates.
            Assert.That(animate, Is.LessThanOrEqualTo(DriverBudget + AnimateBudget),
                "AnimationController.Tick allocates per tick: at 20 Hz with an army out this is the periodic GC spike " +
                $"the frame budget kept failing on (a string per man per tick was the ~400 KB lump). {breakdown}");
            Assert.That(capture, Is.LessThanOrEqualTo(DriverBudget + CaptureBudget), $"SimPresenter.Capture allocates per tick. {breakdown}");
            Assert.That(collect, Is.LessThanOrEqualTo(DriverBudget + CollectBudget), $"EventPump.Collect allocates per tick. {breakdown}");
            Assert.That(interpolate, Is.LessThanOrEqualTo(InterpolateBudget), $"SimPresenter.Interpolate allocates per frame. {breakdown}");
            Assert.That(drivers, Is.LessThanOrEqualTo(DriverBudget), $"the lockstep drivers allocate more per tick than they did. {breakdown}");
        }

        /// <summary>
        /// The driver alone, with the tightest bound, because it was the suspect the callstack named. A lockstep
        /// tick handles a handful of commands: there is no honest reason for it to allocate kilobytes.
        /// </summary>
        // [TS13]
        [Test]
        public void TheLockstepDriverDoesNotAllocatePerTick()
        {
            using var rig = new Rig();
            rig.Populate();
            int men = rig.Local.World.AliveCount;
            // [TS13] The same population assert the other test has: without it this one was green on an empty field.
            Assert.That(men, Is.GreaterThan(WantMen / 4),
                $"[TS13] only {men} men deployed in {DeployTicks} ticks, so this measured the drivers on an almost " +
                "empty field and proves nothing. Fix the deployment, do not relax the budget.");
            double perTick = PerCall(() => rig.StepDrivers(), 60, 120);
            TestContext.WriteLine($"LockstepDriver pair with {rig.Local.World.AliveCount} men: {perTick:0.#} allocations per tick");
            Assert.That(perTick, Is.LessThanOrEqualTo(DriverBudget),
                $"stepping both drivers makes {perTick:0.#} allocations a tick with {rig.Local.World.AliveCount} men out");
        }
    }
}
