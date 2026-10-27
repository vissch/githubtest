// Phase: B6 (implemented) — the interface skin on disk matches SkinSpec, or the gate says which file does not.
// UiSkinVerifier does the looking; this holds its report empty. A second test keeps the spec sheet honest: every file
// the table names must appear in docs/17-ui-art-spec.md, so the artist's document cannot drift from the code the way
// a HUD comment once claimed numbers nobody had checked (HudTextTests).
using System.Collections.Generic;
using System.IO;
using System.Linq;
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

        // ---- the g1 round: PARTS, not tone shares -----------------------------------------------------------
        // A flood of pack tone passes kit_share and figure_cover, which is exactly what the owner rejected ("a grey
        // helmet blob over a tan flood"). These mirror docs/evidence/sapper-look-3-g2-g1/probe.py line for line -
        // nearest of five tones by max-channel distance, cap 32, connected parts over a 0.004 floor - and the bars
        // are the ones probe.py computes from the committed cur-* captures in probe.json (commit 187200b7e1):
        // face_share max(2 * 0.0, 0.02) = 0.02 and pack_flood min(0.6 * 0.5348, 0.55) = 0.3209.
        static readonly (string Name, Color32 Tone)[] ProbeTones =
        {
            ("HELMET", new Color32(0x3E, 0x4A, 0x52, 255)),
            ("FACE", new Color32(0xD8, 0xA0, 0x6A, 255)),
            ("PACK", new Color32(0xA8, 0x99, 0x70, 255)),
            ("CHARGE", new Color32(0x6E, 0x62, 0x45, 255)),
            ("DISC", new Color32(0x23, 0x25, 0x2A, 255)),
        };
        const int ToneThreshold = 32;
        const float MinPart = 0.004f;

        static string[] Classify(Color32[] px, int w, int h)
        {
            var grid = new string[w * h];
            for (int i = 0; i < w * h; i++)
            {
                var c = px[i];
                string best = null;
                int bestD = ToneThreshold;
                foreach (var (name, t) in ProbeTones)
                {
                    int d = Mathf.Max(Mathf.Abs(c.r - t.r), Mathf.Max(Mathf.Abs(c.g - t.g), Mathf.Abs(c.b - t.b)));
                    if (d <= bestD) { best = name; bestD = d; }
                }
                grid[i] = best;
            }
            return grid;
        }

        /// <summary>4-neighbour connected runs of one label, biggest first.</summary>
        static List<int> Components(string[] grid, int w, int h, string label)
        {
            var seen = new bool[w * h];
            var out_ = new List<int>();
            var stack = new Stack<int>();
            for (int i0 = 0; i0 < w * h; i0++)
            {
                if (seen[i0] || grid[i0] != label) continue;
                stack.Push(i0); seen[i0] = true;
                int area = 0;
                while (stack.Count > 0)
                {
                    int i = stack.Pop(); area++;
                    int x = i % w, y = i / w;
                    if (x > 0) Push(i - 1);
                    if (x < w - 1) Push(i + 1);
                    if (y > 0) Push(i - w);
                    if (y < h - 1) Push(i + w);
                    void Push(int n) { if (!seen[n] && grid[n] == label) { seen[n] = true; stack.Push(n); } }
                }
                out_.Add(area);
            }
            out_.Sort((a, b) => b.CompareTo(a));
            return out_;
        }

        static float? CentroidRow(string[] grid, int w, int h, string label)
        {
            int total = 0; long rows = 0;
            for (int i = 0; i < w * h; i++) if (grid[i] == label) { total++; rows += i / w; }
            return total > 0 ? rows / (float)total : (float?)null;
        }

        [Test]
        public void TheSapperPortraitHasAFaceAndSeparateCharges()
        {
            int s = SkinSpec.PortraitSize;
            var grid = Classify(UiSkinGenerator.PaintSapperPortrait(), s, s);
            float area = s * (float)s, floor = MinPart * area, lower = s * (float)(s - s / 2);

            int faceCount = 0;
            for (int i = 0; i < grid.Length; i++) if (grid[i] == "FACE") faceCount++;
            float faceShare = faceCount / area;
            var pack = Components(grid, s, s, "PACK");
            var charge = Components(grid, s, s, "CHARGE");
            int packParts = pack.Count(a => a >= floor), chargeParts = charge.Count(a => a >= floor);
            float packFlood = pack.Count > 0 ? pack[0] / lower : 0f;
            float? helmetRow = CentroidRow(grid, s, s, "HELMET"), faceRow = CentroidRow(grid, s, s, "FACE");

            Assert.That(faceShare, Is.GreaterThanOrEqualTo(0.02f), $"face_share={faceShare}: there is no face under the helmet");
            Assert.That(packParts, Is.GreaterThanOrEqualTo(2), $"pack_parts={packParts}: the kit is one slab, not a satchel and a bedroll");
            Assert.That(chargeParts, Is.EqualTo(2), $"charge_parts={chargeParts}: not exactly two separate charges");
            Assert.That(packFlood, Is.LessThanOrEqualTo(0.3209f), $"pack_flood={packFlood}: the biggest pack part still floods the lower disc");
            Assert.That(helmetRow, Is.Not.Null, "no helmet tone at all");
            Assert.That(faceRow, Is.Not.Null, "no face tone at all");
            Assert.That(helmetRow.Value, Is.LessThan(faceRow.Value - 0.05f * s),
                $"helmet row {helmetRow} is not above the face row {faceRow}: the bust is not shoulders-up");
        }
    }
}
