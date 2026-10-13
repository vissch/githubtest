// Phase: V3 (code review 2026-10-04) — VATRenderer.cs:243: the frustum cull radius ignored `grow` (up to 4x when the camera is zoomed out), so a
// grown man popped in near the screen edge instead of being culled by his real, grown-up drawn size.
using NUnit.Framework;
using TW.Presentation.Units;

namespace TW.Tests
{
    public sealed class VatCullRadiusTests
    {
        [Test]
        public void The_Cull_Radius_Grows_With_The_Zoomed_Out_Man()
        {
            Assert.AreEqual(3f * 2f * 1f, VATRenderer.EffectiveCullRadius(3f, 2f, 1f), 1e-5f, "[V3] no grow: unchanged from before");
            Assert.AreEqual(3f * 2f * 4f, VATRenderer.EffectiveCullRadius(3f, 2f, 4f), 1e-5f, "[V3] at MaxGrow (x4) the margin must grow with the man, not stay at x1");
        }
    }
}
