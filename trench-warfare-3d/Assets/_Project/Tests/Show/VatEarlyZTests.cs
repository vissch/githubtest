// Phase: tooling (perf pass, 2026-09-24) — the living are drawn with no clip(), so the GPU depth-tests a man before it
// shades him; the fallen keep the limb cut. Only the fallen can have lost a limb (VatPad: a living man's record packs
// none), so only the fallen's material enables _TW_LIMBCUT, the keyword the lost-limb and wound clips sit behind. A clip
// anywhere in a pass, even one that never fires, makes the GPU run the whole shader before the depth test: in the
// benchmark's held frame that was 1.5 ms of a 3.5 ms opaque pass (docs/05). Nothing but this test would notice it
// coming back, because the picture is the same either way.
using System.IO;
using System.Text.RegularExpressions;
using NUnit.Framework;

namespace TW.Tests
{
    public class VatEarlyZTests
    {
        const string ShaderPath = "Assets/_Project/Shaders/VAT_URP.shader";
        const string RendererPath = "Assets/_Project/Presentation/Units/VATRenderer.cs";

        [Test]
        public void TheLivingVariantCannotDiscard()
        {
            string src = File.ReadAllText(ShaderPath);
            // the source the keyword-off variant compiles: each _TW_LIMBCUT block replaced by its #else branch, if any
            string living = Regex.Replace(src, @"#if defined\(_TW_LIMBCUT\)(.*?)(?:#else(.*?))?#endif", m => m.Groups[2].Value, RegexOptions.Singleline);
            living = Regex.Replace(living, @"//[^\n]*", "");   // what the comments say is not code
            Assert.That(Regex.IsMatch(living, @"\bclip\s*\(|\bdiscard\b"), Is.False,
                "a clip() or discard reaches the living men's passes: put it behind _TW_LIMBCUT (VAT_CLIP_LOST), or every man loses early-Z");
            Assert.That(Regex.Matches(src, @"#pragma multi_compile_local_fragment _ _TW_LIMBCUT").Count, Is.EqualTo(4),
                "each pass that clips the fallen (ForwardLit, Outline, ShadowCaster, DepthOnly) declares the keyword, as multi_compile so a build keeps both variants of these runtime-made materials");
        }

        [Test]
        public void TheFallenKeepTheLimbCut()
        {
            Assert.That(File.ReadAllText(RendererPath), Does.Contain("fallen.EnableKeyword(\"_TW_LIMBCUT\")"),
                "the fallen's material must enable _TW_LIMBCUT, or a man a shell took a limb off lies whole");
        }
    }
}
