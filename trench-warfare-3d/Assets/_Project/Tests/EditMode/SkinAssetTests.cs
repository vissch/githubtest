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
            Token("--tw-card-gap", HudLayout.GapPx);
            Token("--tw-bar-inset", HudLayout.InsetPx);
            Token("--tw-bar-divider", HudLayout.DividerPx);
            Token("--tw-order-btn", HudLayout.OrderBtnPx);
            Token("--tw-order-gap", HudLayout.OrderGapPx);
            Token("--tw-order-off", HudLayout.OrderOffPx);
            Token("--tw-minimap-bezel", HudLayout.MinimapBezelPx);
        }
    }
}
