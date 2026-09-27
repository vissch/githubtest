// Phase: maintenance (2026-09-27) — the fresh-clone command must not overwrite committed work.
// BootstrapSceneBuilder.SetupAll used to recreate the URP asset (losing the InkLines renderer feature), regenerate
// GreyboxCorridor and reset Build Settings to two scenes (MainMenu dropped out), and CLAUDE.md told fresh clones to
// run it. It now runs only the steps whose output is missing; on this repo that is none that writes an asset.
using NUnit.Framework;
using TW.Editor;

namespace TW.Tests
{
    public class FreshCloneSetupTests
    {
        [Test]
        public void SetupAll_Would_Not_Replace_The_Committed_Settings_Or_Scenes()
        {
            var steps = BootstrapSceneBuilder.StepsSetupAllWouldRun();
            CollectionAssert.DoesNotContain(steps, nameof(BootstrapSceneBuilder.SetupProject), "TW-URP.asset is committed");
            CollectionAssert.DoesNotContain(steps, nameof(BootstrapSceneBuilder.BuildScenes), "the scenes are committed");
        }
    }
}
