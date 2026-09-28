// Phase: VFX pass (owner, 2026-09-28; tw3d-board catalogue IN-5) - FlipbookFx.FarGrow, how much wider the burst recipes'
// far-reading parts (the plume, the ground ring) are drawn at a zoom: 1 up to zoom 80, then with the zoom, capped.
using NUnit.Framework;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class FarGrowTests
    {
        [Test]
        public void One_UpToTheStandardViews()
        {
            foreach (float zoom in new[] { 0f, 6f, 30f, FlipbookFx.FarGrowFrom })
                Assert.AreEqual(1f, FlipbookFx.FarGrow(zoom), 1e-6f, "zoom " + zoom);
        }

        [Test]
        public void Grows_WithTheZoom_ThenStops()
        {
            float last = 1f;
            for (float zoom = FlipbookFx.FarGrowFrom; zoom <= 600f; zoom += 10f)
            {
                float g = FlipbookFx.FarGrow(zoom);
                Assert.GreaterOrEqual(g, last, "never shrinks as the camera pulls back (zoom " + zoom + ")");
                Assert.LessOrEqual(g, FlipbookFx.FarGrowMax, "capped (zoom " + zoom + ")");
                last = g;
            }
            Assert.AreEqual(FlipbookFx.FarGrowMax, FlipbookFx.FarGrow(600f), 1e-6f, "the overview is at the cap");
            Assert.AreEqual(1.5f, FlipbookFx.FarGrow(120f), 1e-6f, "zoom 120: half again as wide");
            Assert.AreEqual(FlipbookFx.FarGrowMax, FlipbookFx.FarGrow(240f), 1e-6f, "zoom 240 is already at the cap");
        }
        [Test]
        public void FarFire_BlendsInPastTheStandardView_AndThinsStably()
        {
            Assert.AreEqual(0f, FlipbookFx.FarFireBlend(30f), "close: the drawing as it is");
            Assert.AreEqual(0f, FlipbookFx.FarFireBlend(FlipbookFx.FarFireFrom), "the blend starts at FarFireFrom");
            Assert.AreEqual(1f, FlipbookFx.FarFireBlend(FlipbookFx.FarFireFull), "and is whole at FarFireFull");
            Assert.AreEqual(1f, FlipbookFx.FarKeepShare(0f), "no blend: every card kept");
            Assert.AreEqual(FlipbookFx.FarKeepMin, FlipbookFx.FarKeepShare(1f), 1e-6f);
            int kept = 0, n = 4000;
            for (int i = 0; i < n; i++)
            {
                float born = 10f + i * 0.0137f, life = 0.4f + (i % 7) * 0.05f;
                bool k = FlipbookFx.FarKeep(born, life, 0.4f);
                Assert.AreEqual(k, FlipbookFx.FarKeep(born, life, 0.4f), "a card is kept or not for its whole life");
                if (k) kept++;
                Assert.IsTrue(FlipbookFx.FarKeep(born, life, 1f), "share 1 keeps all");
            }
            Assert.That(kept / (float)n, Is.InRange(0.35f, 0.45f), "about the share asked for");
        }
    }
}
