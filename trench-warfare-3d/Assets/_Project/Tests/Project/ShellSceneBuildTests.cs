// Phase: tooling (code review 2026-10-04, F8). Build Main Menu Scene rewrote MainMenu.unity from scratch on every
// run and replaced the whole build scene list, so a designer's menu scene and any other registered scene were lost.
// The two decisions are pure now and tested here; the save prompt before leaving a modified scene needs a person.
using System.Linq;
using System.Reflection;
using NUnit.Framework;
using TW.Editor;

namespace TW.Tests
{
    public class ShellSceneBuildTests
    {
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
