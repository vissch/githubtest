// Phase: maintenance (2026-10-15) — Build Main Menu Scene must not throw away unsaved edits, and must not
// register a scene that is not on disk.
using NUnit.Framework;
using UnityEditor;
using TW.Editor;

namespace TW.Tests
{
    public class ShellSceneBuildTests
    {
        [Test]
        public void DeclinedSave_BuildsNothing()
        {
            // [found-build-main-menu-scene-throws-away-unsaved-sc-20261015-0]
            var before = EditorBuildSettings.scenes;

            bool result = ShellAssetsBuilder.BuildMainMenuScene(() => false);

            Assert.IsFalse(result, "[found-build-main-menu-scene-throws-away-unsaved-sc-20261015-0] declining the save should cancel the build");
            var after = EditorBuildSettings.scenes;
            Assert.AreEqual(before.Length, after.Length, "[found-build-main-menu-scene-throws-away-unsaved-sc-20261015-0] declined build must not change the build settings count");
            for (int i = 0; i < before.Length; i++)
                Assert.AreEqual(before[i].path, after[i].path, "[found-build-main-menu-scene-throws-away-unsaved-sc-20261015-0] declined build must not change the build settings paths");
        }

        [Test]
        public void MissingGreybox_IsLeftOutOfTheBuildSettings()
        {
            // [found-build-main-menu-scene-throws-away-unsaved-sc-20261015-0]
            var scenes = ShellAssetsBuilder.ShellBuildScenes(p => !p.EndsWith("GreyboxCorridor.unity"));

            Assert.AreEqual(2, scenes.Length, "[found-build-main-menu-scene-throws-away-unsaved-sc-20261015-0] a missing greybox scene should leave two entries");
            Assert.IsTrue(scenes[0].path.EndsWith("Bootstrap.unity"), "[found-build-main-menu-scene-throws-away-unsaved-sc-20261015-0] first entry should be Bootstrap");
            Assert.IsTrue(scenes[1].path.EndsWith("MainMenu.unity"), "[found-build-main-menu-scene-throws-away-unsaved-sc-20261015-0] second entry should be MainMenu");
            foreach (var s in scenes)
            {
                Assert.IsTrue(s.enabled, "[found-build-main-menu-scene-throws-away-unsaved-sc-20261015-0] every registered scene should be enabled");
                Assert.IsFalse(s.path.Contains("Greybox"), "[found-build-main-menu-scene-throws-away-unsaved-sc-20261015-0] the missing greybox scene must not appear");
            }
        }
    }
}
