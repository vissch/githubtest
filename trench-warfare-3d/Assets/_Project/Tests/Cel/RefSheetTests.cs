// Phase: CelFx reference sheets (2026-11-19) - the rules RefSheet effects rely on: the frame clock matches the
// shader's, the replay recipe is not dropped by tier rounding, and a bad layout is refused instead of drawn wrong.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class RefSheetTests
    {
        static RefSheetLayout Layout() => JsonUtility.FromJson<RefSheetLayout>(
            "{\"frames\":191,\"elems\":1,\"tile\":16,\"stored\":18,\"grid\":[84,48],\"ind_cols\":14,\"ind_size\":[1176,672]," +
            "\"sides\":[4096,4096],\"first_frame\":9,\"last_frame\":139,\"fps\":24,\"start_frame\":12}");

        [Test]
        public void FrameAt_RoundsSoAFilmStepNeverLandsOnABoundary()
        {
            Assert.AreEqual(12, RefSheet.FrameAt(0f, 24f, 12f));
            Assert.AreEqual(13, RefSheet.FrameAt(1f / 24f, 24f, 12f));
            Assert.AreEqual(13, RefSheet.FrameAt(1f / 24f - 1e-5f, 24f, 12f));
            Assert.AreEqual(12, RefSheet.FrameAt(-0.5f, 24f, 12f), "a record before its birth shows its first frame");
        }

        [Test]
        public void Recipe_HasARecordAtEveryTier()
        {
            var r = RefSheet.MakeRecipe("TestSheet", Layout(), 3);
            Assert.AreEqual(3, r.Layers.Count);
            foreach (var layer in r.Layers)
            {
                // Close 1 / Mid 0 blends to no record at the close shot (fire_bolts s01 drew nothing)
                Assert.AreEqual(1, layer.Close);
                Assert.AreEqual(1, layer.Mid);
                Assert.AreEqual(1, layer.Far);
                Assert.AreEqual(12f, layer.Extra.y, "born on the clip frame the shot fires on");
            }
            Assert.AreEqual(2, (int)r.Layers[2].Extra.z);
            Assert.AreEqual((139 + 1 - 12) / 24f, r.Layers[0].Life, 1e-5f, "plays to the sheet's last frame");
        }

        [Test]
        public void Layout_ParsesTheExportersJson()
        {
            var l = Layout();
            Assert.AreEqual(191, l.frames);
            Assert.AreEqual(84, l.grid[0]);
            Assert.AreEqual(672, l.ind_size[1]);
            Assert.AreEqual(12f, l.start_frame);
        }
    }
}
