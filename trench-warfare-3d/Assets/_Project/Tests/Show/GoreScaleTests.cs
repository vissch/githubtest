// Phase: gore-by-zoom (2026-10-13, the owner: "the red gore mesh coming out of the shot units is sometimes a bit big,
// we want this to scale based off the zoom"). GoreScale is the one place every red piece's size lives; this pins the
// curve and, above all, the cap, at the three zooms part 1 measured (commit a6312e1b, gore.txt):
//   zoom 6:   man 2.00 m; 26 red pieces, mean 0.157 m (7.8%), largest 0.295 m (14.7%); card 0.79-1.35 m (39-67%)
//   zoom 30:  man 2.00 m; 25 red pieces, mean 0.163 m (8.1%), largest 0.286 m (14.3%); card 1.26-2.16 m (63-108%)
//   zoom 120: man 2.00 m; 25 red pieces, mean 0.162 m (8.1%), largest 0.291 m (14.6%); card 1.26-2.16 m (63-108%)
// The lumps were already one size; the card at the play zoom was taller than the man. These cases are red with the old
// numbers (HitZoomFrom 12, HitZoomOver 15, HitZoomMost 1.6, blob 0.22, card 0.7+0.5) and green with GoreScale's.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class GoreScaleTests
    {
        /// <summary>The zooms part 1 photographed: the closest the camera goes, the zoom the game is played at, and
        /// the farthest zoom blood is drawn at.</summary>
        const float Close = 6f, Play = 30f, Far = 120f;
        /// <summary>A man as drawn, 2.00 m, one size at every zoom (FigureMetrics, owner 2026-10-13).</summary>
        const float Man = 2.0f;
        /// <summary>The biggest the callers' own jitter makes a piece: the splash blobs roll x0.8-1.2 and a droplet
        /// up to x1.25 of its share (CombatFx.HitBlood).</summary>
        const float BlobRoll = 1.2f, DropRoll = 1.25f;

        [Test]
        public void TheManIsTwoMetresAtEveryZoom()
        {
            Assert.AreEqual(Man, FigureMetrics.HeightM, 0.01f, "the shares are shares of this");
            Assert.AreEqual(Man, FigureMetrics.DrawnHeightM(Far), 0.01f, "one size at every zoom");
        }

        [Test]
        public void CloseAndAtThePlayZoom_TheGoreIsTrueToTheMan()
        {
            Assert.AreEqual(1f, GoreScale.Size(Close), 1e-5f, "at the closest zoom the gore is true to the man");
            Assert.AreEqual(1f, GoreScale.Size(Play), 1e-5f, "and at the zoom the game is played at");
            Assert.AreEqual(Play, GoreScale.TrueUntilZoom, 1e-5f, "the play zoom is where the grow starts");
        }

        [Test]
        public void AtThePlayZoom_NoRedPieceIsBiggerThanItsNamedShare()
        {
            foreach (float zoom in new[] { Close, Play })
            {
                Assert.LessOrEqual(GoreScale.Sized(GoreScale.BlobShare, zoom) * BlobRoll, 0.30f * Man,
                    "the largest splash blob stays a sixth of the man at zoom " + zoom);
                Assert.LessOrEqual(GoreScale.Sized(GoreScale.DropShare, zoom) * DropRoll, 0.10f * Man,
                    "a droplet stays a tenth of the man at zoom " + zoom);
                Assert.LessOrEqual(GoreScale.Sized(GoreScale.LumpShare, zoom) * 1.35f, 0.08f * Man,
                    "a gore lump stays under a twelfth of the man at zoom " + zoom);
            }
            // the piece that was 'a bit big': 2.16 m at the play zoom, taller than the 2.00 m man
            float heaviest = GoreScale.Sized(GoreScale.CardShare + GoreScale.CardHeavyShare, Play);
            Assert.LessOrEqual(heaviest, 0.60f * Man, "the heaviest blood card is well under the man at the play zoom");
            Assert.Less(heaviest, 2.16f, "and smaller than what part 1 measured");
        }

        [Test]
        public void PulledOut_ItGrowsByTheNamedNumbers()
        {
            Assert.AreEqual(1f + (Far - GoreScale.TrueUntilZoom) / GoreScale.GrowOver, GoreScale.Size(Far), 1e-5f,
                "at the farthest zoom blood is drawn at, the grow is the named curve");
            Assert.Greater(GoreScale.Size(Far), 1f, "pulled out, the gore grows so it still reads");
            Assert.Less(GoreScale.Size(Far), GoreScale.MostGrow, "but at zoom 120 it is not yet at the cap");
        }

        [Test]
        public void TheCapHolds_HoweverFarOut()
        {
            foreach (float zoom in new[] { 600f, 10_000f })
            {
                Assert.AreEqual(GoreScale.MostGrow, GoreScale.Size(zoom), 1e-5f, "the cap at zoom " + zoom);
                foreach (float share in new[] { GoreScale.BlobShare, GoreScale.DropShare, GoreScale.LumpShare,
                                                GoreScale.CardShare, GoreScale.BigCardShare })
                    Assert.AreEqual(GoreScale.Cap(share), GoreScale.Sized(share, zoom), 1e-5f,
                        "Sized never goes past Cap, at zoom " + zoom);
            }
            // no red piece is ever taller than the man, at any zoom
            Assert.LessOrEqual(GoreScale.Cap(GoreScale.CardShare + GoreScale.CardHeavyShare), Man,
                "even capped, the heaviest blood card is no taller than the man");
            Assert.LessOrEqual(GoreScale.Cap(GoreScale.BigCardShare), Man, "nor the sniper's card");
        }

        [Test]
        public void TheCurveNeverFallsAsTheZoomRises()
        {
            float last = 0f;
            for (float zoom = 0f; zoom <= 1200f; zoom += 3f)
            {
                float now = GoreScale.Size(zoom);
                Assert.GreaterOrEqual(now, last, "the gore never shrinks as the camera pulls out, at zoom " + zoom);
                Assert.LessOrEqual(now, GoreScale.MostGrow + 1e-5f, "nor goes past the cap, at zoom " + zoom);
                last = now;
            }
        }

        [Test]
        public void HitZoomBoost_IsTheSameOneCurve()
        {
            foreach (float zoom in new[] { Close, Play, Far, 600f })
                Assert.AreEqual(GoreScale.Size(zoom), CombatFx.HitZoomBoost(zoom), 1e-5f,
                    "the old name forwards to the one place, at zoom " + zoom);
        }
    }
}
