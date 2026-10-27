// Phase: B6 (implemented) — the interface skin on disk matches SkinSpec, or the gate says which file does not.
// UiSkinVerifier does the looking; this holds its report empty. A second test keeps the spec sheet honest: every file
// the table names must appear in docs/17-ui-art-spec.md, so the artist's document cannot drift from the code the way
// a HUD comment once claimed numbers nobody had checked (HudTextTests).
using System.IO;
using NUnit.Framework;
using UnityEngine;
using TW.Editor;
using TW.UI;

namespace TW.Tests
{
    public class SkinAssetTests
    {
        static string DocPath => Path.GetFullPath(Path.Combine(Application.dataPath, "..", "..", "docs", "17-ui-art-spec.md"));

        [Test]
        public void EverySkinFileMatchesTheSpec()
        {
            var r = UiSkinVerifier.Run();
            Assert.That(r.Failures, Is.Empty, r.ToString());
        }

        [Test]
        public void TheSpecTableHasNoDuplicateFiles()
        {
            var seen = new System.Collections.Generic.HashSet<string>();
            foreach (var e in SkinSpec.All) Assert.That(seen.Add(e.File), $"{e.File} is listed twice in SkinSpec");
        }

        [Test]
        public void EveryPlateAndElementSizeIsSane()
        {
            foreach (var e in SkinSpec.All)
            {
                Assert.That(e.W, Is.GreaterThan(0).And.LessThanOrEqualTo(2048), e.File);
                Assert.That(e.H, Is.GreaterThan(0).And.LessThanOrEqualTo(2048), e.File);
                Assert.That(e.L + e.R, Is.LessThan(e.W), $"{e.File}: horizontal borders {e.L}+{e.R} leave no stretch centre in {e.W} px");
                Assert.That(e.T + e.B, Is.LessThan(e.H), $"{e.File}: vertical borders {e.T}+{e.B} leave no stretch centre in {e.H} px");
                if (e.Kind == SkinKind.Glyph) Assert.That(e.HasBorder, Is.False, $"{e.File}: a glyph is never 9-sliced");
            }
        }

        [Test]
        public void TheArtSpecDocumentNamesEveryFile()
        {
            Assert.That(File.Exists(DocPath), $"{DocPath} is missing");
            string doc = File.ReadAllText(DocPath);
            foreach (var e in SkinSpec.All) StringAssert.Contains(Path.GetFileName(e.File), doc, $"docs/17-ui-art-spec.md does not mention {e.File}");
            foreach (var n in SkinSpec.PortraitNames) StringAssert.Contains(n + ".png", doc, $"docs/17-ui-art-spec.md does not mention Portraits/{n}.png");
            foreach (var f in SkinSpec.Fonts) StringAssert.Contains(Path.GetFileName(f), doc, $"docs/17-ui-art-spec.md does not mention {f}");
        }

        [Test]
        public void TheTokenSheetAgreesWithHudLayout()
        {
            string uss = File.ReadAllText(Path.GetFullPath(Path.Combine(Application.dataPath, "..", SkinSpec.Root, "dustfront.tokens.uss")));
            void Token(string name, float px) => StringAssert.Contains($"{name}: {px:0}px;", uss, $"{name} should be {px:0}px to match HudLayout");
            Token("--tw-bar-height", HudLayout.BarHeightPx);
            Token("--tw-card-size", HudLayout.CardPx);
            Token("--tw-card-size-infantry", HudLayout.InfantryCardPx);
            Token("--tw-card-gap", HudLayout.GapPx);
            Token("--tw-bar-inset", HudLayout.InsetPx);
            Token("--tw-bar-divider", HudLayout.DividerPx);
            Token("--tw-order-btn", HudLayout.OrderBtnPx);
            Token("--tw-order-gap", HudLayout.OrderGapPx);
            Token("--tw-order-off", HudLayout.OrderOffPx);
            Token("--tw-minimap-bezel", HudLayout.MinimapBezelPx);
        }

        static Texture2D Png(string rel)
        {
            var t = new Texture2D(2, 2);
            Assert.That(ImageConversion.LoadImage(t, File.ReadAllBytes(Path.GetFullPath(Path.Combine(Application.dataPath, "..", SkinSpec.Root, rel)))), Is.True, rel);
            return t;
        }

        /// <summary>
        /// The generator draws top row first and a texture's row 0 is the bottom: until v6 every sprite came out upside
        /// down (plates lit from below, the helmet a dropdown arrow, the padlock on its shackle) and no test noticed.
        /// A plate's top bevel is lighter than its bottom one, and the down arrow is wide at the top (GetPixel's y is up).
        /// </summary>
        [Test]
        public void SpritesAreTheRightWayUp()
        {
            var plate = Png("Sprites/plate_normal.png");
            float Lum(Color c) => 0.2126f * c.r + 0.7152f * c.g + 0.0722f * c.b;
            int x = plate.width / 2;
            Assert.That(Lum(plate.GetPixel(x, plate.height - 3)), Is.GreaterThan(Lum(plate.GetPixel(x, 2))), "plate_normal is lit from below: the sprite is upside down");
            var arrow = Png("Icons/ico_arrow_down.png");
            int upper = 0, lower = 0;
            for (int y = 0; y < arrow.height; y++) for (int ax = 0; ax < arrow.width; ax++)
                if (arrow.GetPixel(ax, y).a > 0.5f) { if (y >= arrow.height / 2) upper++; else lower++; }
            Assert.That(upper, Is.GreaterThan(lower), "ico_arrow_down points up: the glyph is upside down");
            Object.DestroyImmediate(plate); Object.DestroyImmediate(arrow);
        }

        /// <summary>Round 10: a plate's bevel runs round its corner arcs: on the ring just inside the dark edge, at 45
        /// degrees, the top-left arc is lighter than the bottom-right one (GetPixel's y is up).</summary>
        [Test]
        public void PlateHighlightRunsRoundTheCorner()
        {
            var p = Png("Sprites/plate_normal.png");
            float Lum(Color c) => 0.2126f * c.r + 0.7152f * c.g + 0.0722f * c.b;
            const int k = 5;   // radius 12: the ring 3 px inside the edge crosses the diagonal about 5 px from the corner
            Color tl = p.GetPixel(k, p.height - 1 - k), br = p.GetPixel(p.width - 1 - k, k);
            Assert.That(Lum(tl), Is.GreaterThan(Lum(br) + 0.03f), $"top-left arc {tl} is no lighter than bottom-right {br}: the bevel stops short of the corner");
            Object.DestroyImmediate(p);
        }

        /// <summary>Round 9: a recessed window's lip (light bottom and right) runs round its corner arc, so the edge reads
        /// as one continuous lip and not as a "|" beside the digits. On the arc at 45 degrees, bottom-right is lighter than
        /// top-left (GetPixel's y is up).</summary>
        [Test]
        public void TheWindowLipRunsRoundTheCorner()
        {
            var w = Png("Sprites/gauge_window.png");
            float Lum(Color c) => 0.2126f * c.r + 0.7152f * c.g + 0.0722f * c.b;
            Color br = w.GetPixel(w.width - 2, 1), tl = w.GetPixel(1, w.height - 2);
            Assert.That(br.a, Is.GreaterThan(0.5f), "the bottom-right arc pixel is transparent: the probe missed the ring");
            Assert.That(Lum(br), Is.GreaterThan(Lum(tl) + 0.05f), $"bottom-right arc {br} is no lighter than top-left {tl}: the lip stops short of the corner");
            Object.DestroyImmediate(w);
        }

        // Mirrors docs/evidence/sapper-look-3-g2/probe.py: PACK/CHARGE are sapperPackMat/sapperChargeMat
        // (CombatFx.SapperKit.cs), DISC is the placeholder's background disc, thresholds read pixels, never typed.
        static readonly Color32 ProbePack = new Color32(0xA8, 0x99, 0x70, 255);
        static readonly Color32 ProbeCharge = new Color32(0x6E, 0x62, 0x45, 255);
        static readonly Color32 ProbeDisc = new Color32(0x23, 0x25, 0x2A, 255);
        const int KitThreshold = 24;
        const int LumaThreshold = 24;

        static float Luma(Color32 c) => 0.299f * c.r + 0.587f * c.g + 0.114f * c.b;

        static float KitShare(Color32[] px, int w, int h)
        {
            int hits = 0;
            foreach (var c in px)
            {
                int dp = Mathf.Max(Mathf.Abs(c.r - ProbePack.r), Mathf.Abs(c.g - ProbePack.g), Mathf.Abs(c.b - ProbePack.b));
                int dc = Mathf.Max(Mathf.Abs(c.r - ProbeCharge.r), Mathf.Abs(c.g - ProbeCharge.g), Mathf.Abs(c.b - ProbeCharge.b));
                if (dp <= KitThreshold || dc <= KitThreshold) hits++;
            }
            return hits / (float)(w * h);
        }

        static float FigureCover(Color32[] px, int w, int h)
        {
            float discLuma = Luma(ProbeDisc);
            int y0 = h / 2, hits = 0, total = w * (h - y0);
            for (int y = y0; y < h; y++)
                for (int x = 0; x < w; x++)
                    if (Luma(px[y * w + x]) - discLuma >= LumaThreshold) hits++;
            return total > 0 ? hits / (float)total : 0f;
        }

        [Test]
        public void TheSapperPortraitIsAManNotAMark()
        {
            int s = SkinSpec.PortraitSize;
            var painted = UiSkinGenerator.PaintSapperPortrait();
            var placeholder = UiSkinGenerator.PaintPortraitPlaceholder("Sapper");

            float kitPainted = KitShare(painted, s, s), kitPlaceholder = KitShare(placeholder, s, s);
            float coverPainted = FigureCover(painted, s, s), coverPlaceholder = FigureCover(placeholder, s, s);

            Assert.That(kitPainted, Is.GreaterThanOrEqualTo(kitPlaceholder * 3f),
                $"kit_share painted={kitPainted} placeholder={kitPlaceholder}: not a 3x clear");
            Assert.That(coverPainted, Is.GreaterThanOrEqualTo(coverPlaceholder * 3f),
                $"figure_cover painted={coverPainted} placeholder={coverPlaceholder}: not a 3x clear");
            Assert.That(coverPainted, Is.GreaterThanOrEqualTo(0.30f), $"figure_cover={coverPainted} misses the 0.30 floor");
        }

        // Mirrors docs/evidence/sapper-look-3-g2-g2/probe.py's pack_blob: the largest connected component of
        // pack-tone pixels inside a disc of radius 0.40*side centred on the square, shaped like a satchel
        // (area share 0.015-0.12, taller than wide, mostly filled, off the midline). 0f if none passes.
        const float ShapeThreshold = 24f;
        const float DiscRadiusFraction = 0.40f;
        static readonly (float Lo, float Hi) PackBlobArea = (0.015f, 0.12f);
        static readonly (float Lo, float Hi) PackBlobAspect = (1.3f, 3.5f);
        const float PackBlobFillMin = 0.6f;
        const float PackBlobOffMidMin = 0.10f;

        static bool[,] DiscMask(int w, int h)
        {
            float side = Mathf.Min(w, h), cx = (w - 1) / 2f, cy = (h - 1) / 2f, r = DiscRadiusFraction * side;
            var mask = new bool[w, h];
            for (int y = 0; y < h; y++) for (int x = 0; x < w; x++)
                mask[x, y] = (x - cx) * (x - cx) + (y - cy) * (y - cy) <= r * r;
            return mask;
        }

        static bool[,] ToneHits(Color32[] px, int w, int h, bool[,] mask, Color32 tone)
        {
            var hit = new bool[w, h];
            for (int y = 0; y < h; y++) for (int x = 0; x < w; x++)
            {
                if (!mask[x, y]) continue;
                var c = px[y * w + x];
                int d = Mathf.Max(Mathf.Abs(c.r - tone.r), Mathf.Abs(c.g - tone.g), Mathf.Abs(c.b - tone.b));
                if (d <= ShapeThreshold) hit[x, y] = true;
            }
            return hit;
        }

        static System.Collections.Generic.List<System.Collections.Generic.List<(int X, int Y)>> ConnectedComponents(bool[,] hit, int w, int h)
        {
            var seen = new bool[w, h];
            var components = new System.Collections.Generic.List<System.Collections.Generic.List<(int X, int Y)>>();
            for (int sy = 0; sy < h; sy++) for (int sx = 0; sx < w; sx++)
            {
                if (!hit[sx, sy] || seen[sx, sy]) continue;
                var cells = new System.Collections.Generic.List<(int X, int Y)>();
                var stack = new System.Collections.Generic.Stack<(int X, int Y)>();
                stack.Push((sx, sy)); seen[sx, sy] = true;
                while (stack.Count > 0)
                {
                    var (x, y) = stack.Pop();
                    cells.Add((x, y));
                    foreach (var (nx, ny) in new[] { (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1) })
                        if (nx >= 0 && nx < w && ny >= 0 && ny < h && hit[nx, ny] && !seen[nx, ny]) { seen[nx, ny] = true; stack.Push((nx, ny)); }
                }
                components.Add(cells);
            }
            return components;
        }

        static float PackBlob(Color32[] px, int w, int h)
        {
            float side = Mathf.Min(w, h), area = w * h;
            var mask = DiscMask(w, h);
            var hit = ToneHits(px, w, h, mask, ProbePack);
            float best = 0f;
            foreach (var cells in ConnectedComponents(hit, w, h))
            {
                float share = cells.Count / area;
                if (share < PackBlobArea.Lo || share > PackBlobArea.Hi) continue;
                int x0 = int.MaxValue, y0 = int.MaxValue, x1 = int.MinValue, y1 = int.MinValue;
                foreach (var (x, y) in cells) { x0 = Mathf.Min(x0, x); y0 = Mathf.Min(y0, y); x1 = Mathf.Max(x1, x); y1 = Mathf.Max(y1, y); }
                int bw = x1 - x0 + 1, bh = y1 - y0 + 1;
                if (bw <= 0) continue;
                float aspect = bh / (float)bw;
                if (aspect < PackBlobAspect.Lo || aspect > PackBlobAspect.Hi) continue;
                float fill = cells.Count / (float)(bw * bh);
                if (fill < PackBlobFillMin) continue;
                float cx = (x0 + x1) / 2f;
                if (Mathf.Abs(cx / side - 0.5f) < PackBlobOffMidMin) continue;
                if (share > best) best = share;
            }
            return best;
        }

        [Test]
        public void TheSapperPackReadsAsAPack()
        {
            int s = SkinSpec.PortraitSize;
            var painted = UiSkinGenerator.PaintSapperPortrait();
            var placeholder = UiSkinGenerator.PaintPortraitPlaceholder("Sapper");

            float blobPainted = PackBlob(painted, s, s), blobPlaceholder = PackBlob(placeholder, s, s);

            Assert.That(blobPainted, Is.GreaterThan(0f), $"pack_blob={blobPainted}: the satchel does not read as a pack-shaped blob");
            Assert.That(blobPlaceholder, Is.EqualTo(0f), $"pack_blob={blobPlaceholder} on the '?' placeholder, which draws no pack tone at all");
        }
    }
}
