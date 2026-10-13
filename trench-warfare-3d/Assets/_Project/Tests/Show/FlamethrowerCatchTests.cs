// Phase: code review fixes (2026-10) — [X1] all ignition in a real match starts from UnitAlight (OnAlight), and the
// sim's own jets/pools already skip Catch (their Sim flag). Torch carried no such flag, so a sim-ignited man's
// trailing fuel (StepTorch's Spill) was always catchable: torch -> pool -> Catch -> torch, re-igniting bystanders
// forever with nothing the sim asked for. The debug panel's Alight tool (TestPanel.cs) keeps the old behaviour on
// purpose: its trail still catches bystanders.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class FlamethrowerCatchTests
    {
        static bool DriveUntilPools(Flamethrower f, FlipbookFx books, float startNow, out float now)
        {
            now = startNow;
            for (int i = 0; i < 40 && f.Pools == 0; i++)
            {
                now += 0.1f;
                f.SimNow = now;
                f.Update(now, null, books, slot => Vector3.zero, (x, z) => 0f);
            }
            return f.Pools > 0;
        }

        /// <summary>[X1] A sim-ignited torch's own trail-fire never re-ignites anyone: its Spill is laid with Sim=true
        /// (Torch.Sim, set by Ignite's sim parameter) and StepPool's Catch already skips Sim pools. Fails on old code
        /// (no Torch.Sim, Spill always sim:false): the flag flips.</summary>
        [Test]
        public void ASimTorchsTrailNeverCatchesABystander()
        {
            var books = new FlipbookFx();
            Assume.That(books.Ready, "the flipbook shader and sheets load in the editor");
            try
            {
                var f = new Flamethrower();
                bool caught = false;
                f.Catch = (at, radius, seconds) => caught = true;

                f.Ignite(0, 10f, sim: true);
                Assert.IsTrue(DriveUntilPools(f, books, 0f, out float now), "the torch should have spilled fuel by now");

                for (int i = 0; i < 15; i++)
                {
                    now += 0.1f;
                    f.SimNow = now;
                    f.Update(now, null, books, slot => Vector3.zero, (x, z) => 0f);
                }
                Assert.IsFalse(caught, "[X1] a sim-lit torch's own trail must not re-ignite bystanders");
            }
            finally { books.Dispose(); }
        }

        /// <summary>Companion case: the debug panel's path (Ignite with sim:false, TestPanel.cs's Alight tool) is left
        /// untouched — its trail still catches, proving the fix is scoped to the sim path only.</summary>
        [Test]
        public void ADebugTorchsTrailStillCatchesABystander()
        {
            var books = new FlipbookFx();
            Assume.That(books.Ready, "the flipbook shader and sheets load in the editor");
            try
            {
                var f = new Flamethrower();
                bool caught = false;
                f.Catch = (at, radius, seconds) => caught = true;

                f.Ignite(1, 10f, sim: false);
                Assert.IsTrue(DriveUntilPools(f, books, 0f, out float now), "the torch should have spilled fuel by now");

                for (int i = 0; i < 15 && !caught; i++)
                {
                    now += 0.1f;
                    f.SimNow = now;
                    f.Update(now, null, books, slot => Vector3.zero, (x, z) => 0f);
                }
                Assert.IsTrue(caught, "the debug panel's trail is meant to keep catching bystanders");
            }
            finally { books.Dispose(); }
        }
    }
}
