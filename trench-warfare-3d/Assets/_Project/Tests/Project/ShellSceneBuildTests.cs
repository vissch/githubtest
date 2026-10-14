// Phase: maintenance (2026-10-15) — Build Main Menu Scene must not throw away unsaved edits, and must not
// register a scene that is not on disk.
using NUnit.Framework;
using UnityEditor;
// Phase: tooling (code review 2026-10-04, F8). Build Main Menu Scene rewrote MainMenu.unity from scratch on every
// run and replaced the whole build scene list, so a designer's menu scene and any other registered scene were lost.
// The two decisions are pure now and tested here; the save prompt before leaving a modified scene needs a person.
using System.Linq;
using System.Reflection;
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

        const string Dir = "Assets/_Project/Scenes";

        [Test]
        public void TheBuildSceneListKeepsScenesItDidNotMake()   // [F8]
        {
            var merge = typeof(ShellAssetsBuilder).GetMethod("MergeBuildScenes",
                BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public);
            Assert.IsNotNull(merge, "[F8] ShellAssetsBuilder.MergeBuildScenes is missing: the build list is still replaced");
            var current = new[] { Dir + "/Playtest.unity", Dir + "/Bootstrap.unity" };
            var required = new[] { Dir + "/Bootstrap.unity", Dir + "/MainMenu.unity", Dir + "/GreyboxCorridor.unity" };
            var got = (string[])merge.Invoke(null, new object[] { current, required });

            Assert.AreEqual(4, got.Length, "[F8] the merged build list is " + string.Join(", ", got));
            Assert.AreEqual(required, got.Take(3).ToArray(), "[F8] the three required scenes are not first, in order");
            Assert.Contains(Dir + "/Playtest.unity", got, "[F8] Playtest.unity, which the builder did not make, was dropped from the build settings");
            Assert.AreEqual(got.Length, got.Distinct().Count(), "[F8] a scene is registered twice");
        }

        [Test]
        public void AnExistingMainMenuIsNotRebuiltFromScratch()   // [F8]
        {
            var mode = typeof(ShellAssetsBuilder).GetMethod("MenuSceneMode",
                BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public);
            Assert.IsNotNull(mode, "[F8] ShellAssetsBuilder.MenuSceneMode is missing: the scene is still rebuilt every run");
            Assert.AreEqual("open", mode.Invoke(null, new object[] { true, false }), "[F8] an existing MainMenu.unity is overwritten");
            Assert.AreEqual("create", mode.Invoke(null, new object[] { false, false }), "[F8] a missing MainMenu.unity is not built");
            Assert.AreEqual("create", mode.Invoke(null, new object[] { true, true }), "[F8] a forced rebuild did not start over");
        }
    }
}
