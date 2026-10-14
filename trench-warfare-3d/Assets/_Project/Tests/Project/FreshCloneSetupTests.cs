// Phase: maintenance (2026-09-27) — the fresh-clone command must not overwrite committed work.
// BootstrapSceneBuilder.SetupAll used to recreate the URP asset (losing the InkLines renderer feature), regenerate
// GreyboxCorridor and reset Build Settings to two scenes (MainMenu dropped out), and CLAUDE.md told fresh clones to
// run it. It now runs only the steps whose output is missing; on this repo that is none that writes an asset.
using NUnit.Framework;
using UnityEditor;
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

        const string MainMenu = "Assets/_Project/Scenes/MainMenu.unity";
        const string Bootstrap = "Assets/_Project/Scenes/Bootstrap.unity";
        const string Greybox = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        [Test]
        public void OnlyTheMissingSceneIsRebuilt()   // [E6]
        {
            // a checkout where GreyboxCorridor is on disk (and hand-edited) and Bootstrap is not
            var todo = BootstrapSceneBuilder.ScenesToBuild(p => p.EndsWith("GreyboxCorridor.unity"));
            Assert.AreEqual(1, todo.Length, "[E6] with only Bootstrap missing, both scenes were rebuilt: the edited GreyboxCorridor is overwritten");
            Assert.AreEqual(Bootstrap, todo[0], "[E6] the scene to build is not Bootstrap");
        }

        [Test]
        public void BuildSettingsKeepsMainMenu()   // [E6]
        {
            var merged = BootstrapSceneBuilder.MergeBuildSettings(
                new[] { new EditorBuildSettingsScene(MainMenu, true), new EditorBuildSettingsScene(Bootstrap, true) },
                Bootstrap, Greybox);
            Assert.AreEqual(3, merged.Length, "[E6] Build Settings was replaced instead of appended to");
            CollectionAssert.Contains(System.Array.ConvertAll(merged, e => e.path), MainMenu,
                "[E6] MainMenu dropped out of Build Settings");
        }

        [Test]
        public void TheTunedRendererCountsAsASetting()   // [E6]
        {
            // TW-URP is there but the tuned TW-Renderer (InkLines) is not: the step has to run
            var steps = BootstrapSceneBuilder.StepsSetupAllWouldRun(p => !p.EndsWith("TW-Renderer.asset"));
            CollectionAssert.Contains(steps, nameof(BootstrapSceneBuilder.SetupProject),
                "[E6] only TW-URP.asset was checked, so a missing TW-Renderer.asset was never noticed");
        }
    }
}
