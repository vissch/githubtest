// Phase: tooling (code review 2026-10-04, F11). ShellAssetsBuilder.Build and UiAssetBuilder.CreatePanelSettings
// overwrote a committed field with whatever AssetDatabase.LoadAssetAtPath answered, including null when an asset
// failed to load or was deleted, so a bad import silently wiped a good reference the next time the builder ran.
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
    }
}
