// Phase: VFX pass (owner, 2026-10-19) - harsh critic r1 (41/100), fix 2 on flame-jet-core: "no dark smoke past the jet's tip (orange
// cards only) and no probe key for it; also the black outline strokes round every card make the jet read as a
// texture." These guard the two fixes: a dark plume past the stream's end, and the stream's own cards (Jet, Core,
// Head) losing the default fire contour's ink ring.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;
using Book = TW.Presentation.Tactical.FlipbookFx.Book;

namespace TW.Tests
{
    public class FlameJetTipSmokeTests
    {
        [Test]
        public void FlameJetEndsInDarkSmoke()
        {
            Vector3 far = new Vector3(10f, 0f, 0f);
            Vector3 aim = Vector3.right;

            for (int i = 0; i < Flamethrower.TipSmokeCards; i++)
            {
                Vector3 at = Flamethrower.TipSmokeAt(far, aim, i, 0f);
                float along = Vector3.Dot(at - far, aim);
                Assert.Greater(along, 0.5f, $"card {i} must sit past the stream's end, not behind it");
                Assert.Less(along, 3.2f, $"card {i} must stay within the probe's 200 px window past the tip");
                Assert.LessOrEqual(at.y - far.y, 2.0f, $"card {i} must stay low over the mud, not climb into the sky");
            }

            Assert.AreEqual(Book.MineSmoke, Flamethrower.TipSmokeBook, "the tip's plume is soot, not Book.Smoke (ApplyTints overwrites Smoke's tint every scene)");
            float smokeLuma = Luma(FlipbookFx.RowTint(Book.Smoke));
            float mineSmokeLuma = Luma(FlipbookFx.RowTint(Flamethrower.TipSmokeBook));
            Assert.Less(mineSmokeLuma, smokeLuma, "the tip's plume must read darker than the stream's warm-grey smoke");
        }

        [Test]
        public void JetCardsCarryNoInkContour()
        {
            Assert.IsTrue(FlipbookFx.NoInkOf(Book.Jet), "the stream's own card must drop the ink ring, or it reads as a texture");
            Assert.IsTrue(FlipbookFx.NoInkOf(Book.Core));
            Assert.IsTrue(FlipbookFx.NoInkOf(Book.Head));
            Assert.IsFalse(FlipbookFx.NoInkOf(Book.Smoke));
            Assert.IsFalse(FlipbookFx.NoInkOf(Book.Bloom));
        }

        static float Luma(Color c) => 0.2126f * c.r + 0.7152f * c.g + 0.0722f * c.b;
    }
}
