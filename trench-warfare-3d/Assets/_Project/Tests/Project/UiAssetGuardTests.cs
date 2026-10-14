// Phase: tooling (code review 2026-10-04, F11, F12). ShellAssetsBuilder.Build and UiAssetBuilder.CreatePanelSettings
// overwrote a committed field with whatever AssetDatabase.LoadAssetAtPath answered, including null when an asset
// failed to load or was deleted, so a bad import silently wiped a good reference the next time the builder ran.
// UiSkinGenerator.LoadSidecar caught any parse failure and answered a fresh empty record, so a sidecar left with
// unresolved rebase conflict markers was read as "nothing recorded" and every placeholder got silently re-promoted.
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Editor;

namespace TW.Tests
{
    public class UiAssetGuardTests
    {
        [Test]
        public void AFailedLoadKeepsTheGoodReference()   // [F11]
        {
            var keep = typeof(UiAssetBuilder).GetMethod("Keep", BindingFlags.Static | BindingFlags.Public);
            Assert.IsNotNull(keep, "[F11] UiAssetBuilder.Keep is missing: a failed load still overwrites a good reference");
            var generic = keep.MakeGenericMethod(typeof(PanelSettings));

            var old = ScriptableObject.CreateInstance<PanelSettings>();
            var fresh = ScriptableObject.CreateInstance<PanelSettings>();
            try
            {
                var keptOld = (PanelSettings)generic.Invoke(null, new object[] { null, old, "x" });
                Assert.AreSame(old, keptOld, "[F11] a failed load must keep the reference already in the asset, not save null over it");

                var keptFresh = (PanelSettings)generic.Invoke(null, new object[] { fresh, old, "x" });
                Assert.AreSame(fresh, keptFresh, "[F11] a successful load must still replace the old reference");

                var keptNull = (PanelSettings)generic.Invoke(null, new object[] { null, null, "x" });
                Assert.IsNull(keptNull, "[F11] with nothing to keep, Keep must answer null, not throw");
            }
            finally
            {
                Object.DestroyImmediate(old);
                Object.DestroyImmediate(fresh);
            }
        }

        static bool TryParseSidecar(string text, out object side, out string error)
        {
            var m = typeof(UiSkinGenerator).GetMethod("TryParseSidecar", BindingFlags.Static | BindingFlags.Public);
            Assert.IsNotNull(m, "[F12] UiSkinGenerator.TryParseSidecar is missing: an unreadable sidecar is read as empty");
            var args = new object[] { text, null, null };
            bool ok = (bool)m.Invoke(null, args);
            side = args[1];
            error = (string)args[2];
            return ok;
        }

        [Test]
        public void AConflictedPlaceholderRecordIsNotReadAsEmpty()   // [F12]
        {
            string text = "<<<<<<< HEAD\n{\"generator\":3,\"files\":[]}\n=======\n{}\n>>>>>>> theirs\n";
            bool ok = TryParseSidecar(text, out var side, out var error);
            Assert.IsFalse(ok, "[F12] a sidecar with unresolved conflict markers must not parse as a valid record");
            Assert.IsFalse(string.IsNullOrEmpty(error), "[F12] a refusal must carry a reason");
        }

        [Test]
        public void AGoodPlaceholderRecordStillParses()   // [F12]
        {
            string text = "{\"generator\":3,\"files\":[{\"file\":\"a.png\",\"sha1\":\"ff\"}]}";
            bool ok = TryParseSidecar(text, out var sideObj, out var error);
            Assert.IsTrue(ok, "[F12] a good sidecar record must still parse: " + error);
            var side = (UiSkinGenerator.Sidecar)sideObj;
            Assert.AreEqual(3, side.generator, "[F12] generator version must round-trip");
            Assert.AreEqual(1, side.files.Count, "[F12] the one recorded file must round-trip");
            Assert.AreEqual("a.png", side.files[0].file, "[F12] the file name must round-trip");
        }
    }
}
