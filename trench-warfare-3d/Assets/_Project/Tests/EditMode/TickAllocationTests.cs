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
// GC.GetAllocatedBytesForCurrentThread counts managed allocation on this thread only, cumulatively, so a
// collection running mid-measurement cannot hide it. NativeArray and Burst allocations are native and correctly
// invisible here.
using System;
using NUnit.Framework;
using TW.Sim;
using TW.Sim.Match;
using TW.Net;
using TW.Presentation;

namespace TW.Tests
{
    public class TickAllocationTests
    {
        /// <summary>Men each side is asked for. Kept well under the 2,000-man stress preset so the test stays a
        /// second or two; the allocation we are hunting is per tick, so it does not need the full army to show.</summary>
        const int WantMen = 300;
        const int DeployTicks = 400;

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

        /// <summary>Bytes a block allocates, averaged over `reps`, after `warm` repetitions have grown every buffer
        /// it will ever grow. Without the warm-up this measures one-time capacity growth and calls it a leak.</summary>
        static double PerCall(Action step, int warm, int reps)
        {
            for (int i = 0; i < warm; i++) step();
            long before = GC.GetAllocatedBytesForCurrentThread();
            for (int i = 0; i < reps; i++) step();
            return (GC.GetAllocatedBytesForCurrentThread() - before) / (double)reps;
        }

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

            // Events per tick, reported because the cost this file is hunting is NOT in any of the four calls above
            // (they measure zero) and the remaining candidate is Events.Dispatch, whose cost is the SUBSCRIBERS'.
            // Dispatch runs inside SimHost.Update and only has anything to do on a frame that stepped a tick, which
            // is exactly the per-tick signature the profiler saw. If a subscriber allocates per event, the bill is
            // events-per-tick times that, so this number is the multiplier and belongs in the record.
            rig.Events.Frame.Clear();
            rig.Step();
            rig.Events.Collect(rig.Local.World);
            int eventsPerTick = rig.Events.Frame.Count;
            rig.Events.Frame.Clear();

            string breakdown = $"with {men} men alive — drivers {drivers:0} B, Presenter.Capture {capture:0} B, " +
                               $"Animation.Tick {animate:0} B, Events.Collect {collect:0} B, total {total:0} B; " +
                               $"{eventsPerTick} events a tick" +
                               (eventsPerTick == 0
                                   ? ". NOTE: no events, so the men are deployed but not yet in contact and this run "
                                     + "does NOT cover Events.Dispatch or its subscribers — which is where the "
                                     + "remaining per-tick allocation is believed to be. Do not read this pass as "
                                     + "clearing that path."
                                   : " reach Dispatch and its subscribers");
            TestContext.WriteLine(breakdown);

            // A population of nearly nothing would make this test pass for the wrong reason, which is exactly how
            // its first version passed. Fail loudly rather than quietly measuring an empty field.
            Assert.That(men, Is.GreaterThan(WantMen / 4),
                $"only {men} men deployed in {DeployTicks} ticks, so this measured an almost empty field and proves " +
                "nothing about the per-tick allocation. Fix the deployment, do not relax the budget.");

            // The budget is zero bytes a frame (docs/05). This asserts something far weaker — that a tick is not
            // allocating on the order of the 400 KB lump — because a test that fails for a reason nobody will fix
            // is a test that gets disabled. Tighten it once the cause is found and gone.
            Assert.That(total, Is.LessThan(64 * 1024),
                $"a sim tick allocates {total:0} bytes, which at 20 Hz is {total * 20 / 1024:0} KB a second of pure " +
                $"garbage and is the periodic GC spike the frame budget keeps failing on. {breakdown}");
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
            TestContext.WriteLine($"LockstepDriver pair with {rig.Local.World.AliveCount} men: {perTick:0} B per tick");
            Assert.That(perTick, Is.LessThan(4096),
                $"stepping both drivers allocates {perTick:0} bytes a tick with {rig.Local.World.AliveCount} men out");
        }
    }
}
