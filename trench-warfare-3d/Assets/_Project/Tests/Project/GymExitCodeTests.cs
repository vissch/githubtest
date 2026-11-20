// Phase: tooling (2026-10-17) — Gym.CommandLine must exit non-zero when the run never starts, or a batch editor
// sits idle forever holding the checkout's project lock and the GPU.
using NUnit.Framework;
using TW.Editor;
using UnityEditor;
using UnityEngine;
using UnityEngine.TestTools;

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

        [TestCase(false)]
        [TestCase(true)]
        public void PlayOnlyStartsTheGymForAnExplicitRequest(bool emptyRequest)
        {
            if (Object.FindFirstObjectByType<TW.Presentation.SimHost>() != null)
                Assert.Ignore("This callback regression check requires an edit-mode scene without a SimHost.");

            const string key = "TW.Gym.Request";
            string absent = "test-missing-" + System.Guid.NewGuid().ToString("N");
            string previous = SessionState.GetString(key, absent);
            try
            {
                SessionState.EraseString(key);
                if (emptyRequest)
                {
                    // Gym.Run() deliberately stores empty options. It must still reach StartRunner once.
                    SessionState.SetString(key, "");
                    LogAssert.Expect(LogType.Error, "Gym: no SimHost in the open scene");
                }

                var callback = typeof(Gym).GetMethod("OnPlayMode",
                    System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.NonPublic);
                callback.Invoke(null, new object[] { PlayModeStateChange.EnteredPlayMode });
                Assert.AreEqual(absent, SessionState.GetString(key, absent), "a request is consumed exactly once");
                callback.Invoke(null, new object[] { PlayModeStateChange.EnteredPlayMode });
                LogAssert.NoUnexpectedReceived();
            }
            finally
            {
                SessionState.EraseString(key);
                if (previous != absent) SessionState.SetString(key, previous);
            }
        }
    }
}
