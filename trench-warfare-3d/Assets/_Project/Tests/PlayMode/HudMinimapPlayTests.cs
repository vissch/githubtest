// Phase: B6 (implemented) — the minimap's unit dots must be seen. The dot colours are the trench colours (ours bone,
// theirs rust), and men nearly always stand in a trench, so a bare dot was painted in the line's own colour and
// disappeared into it: measured live on 2026-10-17 (GreyboxCorridor, 90 s in), 22 of 38 lit dot pixels were
// bit-identical to the ground pixel beneath them, and no HUD capture of a running match showed a single dot.
// These tests drive HudMinimap's dot painting over a trench-coloured ground and assert every dot has a ring darker
// than any ground colour, so it reads on top of the line. No SimHost, no panel: the painting is pure pixels.
using NUnit.Framework;
using UnityEngine;
using TW.UI;

namespace TW.Tests
{
    public class HudMinimapPlayTests
    {
        const int W = 16, H = 10;

        static Color32[] Fill(Color32 c)
        {
            var px = new Color32[W * H];
            for (int i = 0; i < px.Length; i++) px[i] = c;
            return px;
        }

        static float Luma(Color32 c) => 0.299f * c.r + 0.587f * c.g + 0.114f * c.b;

        [Test]
        public void ADotOnItsOwnTrenchLineIsRingedSoItStillReads()
        {
            foreach (bool mine in new[] { true, false })
            {
                var colour = HudMinimap.DotColour(mine);
                var ground = Fill(colour);        // the worst case: the trench line under the man is his own colour
                var dots = new Color32[W * H];
                int u = 7, v = 5;
                HudMinimap.DotRing(dots, W, H, u, v);
                HudMinimap.DotCore(dots, W, H, u, v, colour);

                int lit = 0, same = 0, ring = 0;
                for (int i = 0; i < dots.Length; i++)
                {
                    if (dots[i].a == 0) continue;
                    lit++;
                    if (dots[i].r == ground[i].r && dots[i].g == ground[i].g && dots[i].b == ground[i].b) same++;
                    if (Luma(dots[i]) < Luma(ground[i]) - 20f) ring++;
                }
                Assert.That(lit, Is.EqualTo(16), "a dot is a 2x2 core inside a 4x4 ringed box");
                Assert.That(same, Is.EqualTo(4), "only the 2x2 core may share the ground's colour");
                Assert.That(ring, Is.EqualTo(12), $"the core needs a full dark ring to read on a {(mine ? "bone" : "rust")} trench line");
                // the ring must surround the core: every one of the core's four cells has a dark neighbour
                for (int dv = -1; dv <= 0; dv++) for (int du = 0; du <= 1; du++)
                {
                    bool dark = false;
                    for (int ev = -1; ev <= 1; ev++) for (int eu = -1; eu <= 1; eu++)
                    {
                        int uu = u + du + eu, vv = v + dv + ev;
                        if (uu < 0 || uu >= W || vv < 0 || vv >= H) continue;
                        if (Luma(dots[vv * W + uu]) < Luma(ground[vv * W + uu]) - 20f) dark = true;
                    }
                    Assert.That(dark, Is.True, "every core cell touches the ring");
                }
            }
        }

        [Test]
        public void TwoMenSideBySideKeepBothCores()
        {
            var dots = new Color32[W * H];
            var mine = HudMinimap.DotColour(true);
            var theirs = HudMinimap.DotColour(false);
            HudMinimap.DotRing(dots, W, H, 5, 5); HudMinimap.DotRing(dots, W, H, 8, 5);
            HudMinimap.DotCore(dots, W, H, 5, 5, mine); HudMinimap.DotCore(dots, W, H, 8, 5, theirs);
            Assert.That(dots[5 * W + 5].r, Is.EqualTo(mine.r), "the near man's core survives his neighbour's ring");
            Assert.That(dots[5 * W + 8].r, Is.EqualTo(theirs.r), "and so does the far man's");
        }

        [Test]
        public void ADotAtTheEdgeStaysInsideTheTexture()
        {
            var dots = new Color32[W * H];
            HudMinimap.DotRing(dots, W, H, 0, 0);
            HudMinimap.DotCore(dots, W, H, 0, 0, HudMinimap.DotColour(true));
            HudMinimap.DotRing(dots, W, H, W - 1, H - 1);
            HudMinimap.DotCore(dots, W, H, W - 1, H - 1, HudMinimap.DotColour(false));
            Assert.Pass("no out-of-range write");
        }
    }
}
