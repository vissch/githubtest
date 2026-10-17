// Phase: tooling (2026-10-17) — Gym.CommandLine must exit non-zero when the run never starts, or a batch editor
// sits idle forever holding the checkout's project lock and the GPU.
using NUnit.Framework;
using TW.Editor;

namespace TW.Tests
{
    public class GymExitCodeTests
    {
        // [found-gym-commandline-must-exit-non-zero-when-the-20261017-0]
        [Test]
        public void ARunThatNeverStartsExitsNonZero()
        {
            Assert.AreEqual(1, Gym.ExitCodeFor("the open scene has unsaved changes: save or discard them first"),
                "[found-gym-commandline-must-exit-non-zero-when-the-20261017-0] a gym that never entered Play must exit 1, not hang");
            Assert.AreEqual(1, Gym.ExitCodeFor(null),
                "[found-gym-commandline-must-exit-non-zero-when-the-20261017-0] no answer at all is no run either");
            Assert.AreEqual(0, Gym.ExitCodeFor(Gym.Entering + "tabs=clips quit=1"),
                "[found-gym-commandline-must-exit-non-zero-when-the-20261017-0] a run that entered Play exits itself");
        }
    }
}
