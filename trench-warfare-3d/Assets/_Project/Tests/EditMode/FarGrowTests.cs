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
    }
}
