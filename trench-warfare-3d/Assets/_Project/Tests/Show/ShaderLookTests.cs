// Phase: review fixes (rv-41-p2-u20) — what the shader source must say for the look to come out right. These are
// source contracts, not pixels: nothing here can run a fragment shader, so each test pins the one line the finding
// is about and the number on the C# side it has to agree with.
using System.Globalization;
using System.IO;
using System.Text.RegularExpressions;
using NUnit.Framework;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class ShaderLookTests
    {
        const string ToonPath = "Assets/_Project/Shaders/Toon_URP.shader";
        const string VatPath = "Assets/_Project/Shaders/VAT_URP.shader";
        const string DebrisPath = "Assets/_Project/Shaders/Debris_URP.shader";
        const string PigmentPath = "Assets/_Project/Presentation/Terrain/BattlefieldPigment.cs";

        // [Hs1] the painted sheet is R8, so it clamps at 1, but the paint peaks at 1.22 (Earth's seam lip): the
        // bake has to scale it down and the shader has to scale it back, or every highlight above 1.0 is lost.
        [Test]
        public void ThePaintedHighlightsSurviveTheR8Sheet()
        {
            float peak = 0f;
            for (int y = 0; y < 64; y++)
            for (int x = 0; x < 64; x++)
            {
                float u = (x + .5f) / 64f, v = (y + .5f) / 64f;
                for (int layer = 0; layer < BattlefieldPigment.Count; layer++)
                    peak = System.Math.Max(peak, BattlefieldPigment.Value((BattlefieldPigment.Surface)layer, u, v));
            }
            Assert.That(peak, Is.GreaterThan(1.0f), "[Hs1] the painted vocabulary has highlights above 1.0 to lose");

            string pigment = File.ReadAllText(PigmentPath);
            var range = Regex.Match(pigment, @"const float Range = ([0-9.]+)f;");
            Assert.That(range.Success, Is.True, "[Hs1] BattlefieldPigment must name the range it stores the sheet in");
            float declared = float.Parse(range.Groups[1].Value, CultureInfo.InvariantCulture);
            Assert.That(declared, Is.GreaterThanOrEqualTo(peak), "[Hs1] the stored range must cover the peak painted value (1.22)");
            Assert.That(Regex.IsMatch(pigment, @"float value = Stored\("), Is.True,
                "[Hs1] the bake must store the scaled value, not the raw one, or R8 clamps the highlight away");

            string toon = File.ReadAllText(ToonPath);
            var define = Regex.Match(toon, @"#define TW_PIGMENT_RANGE ([0-9.]+)");
            Assert.That(define.Success, Is.True, "[Hs1] Toon_URP must define the range it multiplies the sheet back by");
            Assert.That(float.Parse(define.Groups[1].Value, CultureInfo.InvariantCulture), Is.EqualTo(declared).Within(1e-4f),
                "[Hs1] the shader's range and BattlefieldPigment.Range must be the same number");
            Assert.That(Regex.IsMatch(toon, @"SAMPLE_TEXTURE2D_ARRAY\(_PigmentSheet[^;]*\.r \* TW_PIGMENT_RANGE"), Is.True,
                "[Hs1] the painted sample is used raw, so every highlight above 1.0 is lost");
        }

        // [Hs2] the snow on a man is painted in his own space so it does not swim as he walks; the per-instance
        // offset must then be his own seed (grime.y), not anything from the world he is standing in.
        [Test]
        public void AMansSnowIsKeyedToHisOwnSeed()
        {
            string vat = File.ReadAllText(VatPath);
            var calls = Regex.Matches(vat, @"TWSnowAmount\([^;]*\);");
            Assert.That(calls.Count, Is.EqualTo(1), "[Hs2] VAT_URP must ask for snow in exactly one place");
            string call = calls[0].Value;
            Assert.That(call, Does.Contain("grime.y"), "[Hs2] the snow offset must be the man's own seed, or every soldier wears identical snow");
            Assert.That(call, Does.Not.Contain("positionWS"), "[Hs2] a world-space term seams a man's snow and makes it jump as he walks");
        }

        // [Hs3] an outline is drawn over the body it rims, so it has to wear the same air: without the mist and
        // field-fog calls the body makes, men in fog show as dark ink rims floating in white.
        [Test]
        public void EveryOutlinePassWearsTheFogTheBodyWears()
        {
            foreach (string path in new[] { ToonPath, VatPath, "Assets/_Project/Shaders/Tank_URP.shader", "Assets/_Project/Shaders/Debris_URP.shader" })
            {
                string src = File.ReadAllText(path);
                int start = src.IndexOf("Name \"Outline\"");
                Assert.That(start, Is.GreaterThan(-1), "[Hs3] no Outline pass found in " + path);
                int end = src.IndexOf("ENDHLSL", start);
                Assert.That(end, Is.GreaterThan(start), "[Hs3] the Outline pass of " + path + " has no HLSL block");
                string pass = src.Substring(start, end - start);
                Assert.That(pass, Does.Contain("ApplyMist("), "[Hs3] the outline pass of " + path + " skips the ground mist the body applies");
                Assert.That(pass, Does.Contain("ApplyFieldFog("), "[Hs3] the outline pass of " + path + " skips the field fog the body applies");
            }
        }

        // [Hs6] a hidden house chunk and a sunk debris piece are collapsed onto a point so their triangles have no
        // area: the outline pass must not push the three vertices apart in screen space again, or an ink dot of a
        // couple of pixels is left behind at the origin.
        [Test]
        public void ACollapsedPieceGrowsNoOutline()
        {
            string toon = File.ReadAllText(ToonPath);
            Assert.That(Regex.IsMatch(toon, @"bool TWChunkHidden\(float chunk\)"), Is.True,
                "[Hs6] Toon_URP must be able to ask whether a chunk is hidden, not only collapse it");
            Assert.That(OutlinePass(toon), Does.Match(@"cs\.xy \+= *hidden \?"),
                "[Hs6] Toon's outline pass extrudes a hidden chunk: it leaves an ink dot at the house origin");
            Assert.That(OutlinePass(toon), Does.Contain("TWChunkHidden(v.chunk.x)"),
                "[Hs6] Toon's outline pass must read the chunk mask to know what is hidden");
            Assert.That(Regex.IsMatch(toon, @"v\.positionOS\.xyz = TWChunk\("), Is.True,
                "[Hs6] the body passes must still collapse a hidden chunk");

            string debris = File.ReadAllText(DebrisPath);
            Assert.That(Regex.IsMatch(debris, @"struct Placed \{[^}]*float scale;"), Is.True,
                "[Hs6] Place must hand its scale on, or the outline pass cannot tell a gone piece from a live one");
            Assert.That(OutlinePass(debris), Does.Match(@"cs\.xy \+= *a\.scale <= 0\.0 \?"),
                "[Hs6] Debris' outline pass extrudes a sunk piece: it leaves an ink dot where the piece went under");
        }

        // [Hs7] these six passes build their shadow coord with TransformWorldToShadowCoord(positionWS), a
        // light-space coord, but their multi_compile also asked for _MAIN_LIGHT_SHADOWS_SCREEN, a screen-space
        // variant that only a Screen Space Shadows renderer feature can turn on; TW-Renderer.asset has none.
        [Test]
        public void NoShaderAsksForScreenSpaceShadows()
        {
            string[] paths =
            {
                ToonPath, VatPath, DebrisPath,
                "Assets/_Project/Shaders/Tank_URP.shader",
                "Assets/_Project/Shaders/Sea_URP.shader",
                "Assets/_Project/Shaders/Water_URP.shader",
            };
            foreach (string path in paths)
            {
                string src = File.ReadAllText(path);
                Assert.That(src, Does.Not.Contain("_MAIN_LIGHT_SHADOWS_SCREEN"),
                    "[Hs7] " + path + " compiles the screen-space shadow variant but passes a light-space coord");
                Assert.That(src, Does.Contain("_MAIN_LIGHT_SHADOWS_CASCADE"),
                    "[Hs7] " + path + " must still ask for the cascade shadow variant");
                Assert.That(src, Does.Contain("TransformWorldToShadowCoord"),
                    "[Hs7] " + path + " must still build its shadow coord in light space");
            }

            string renderer = File.ReadAllText("Assets/_Project/Settings/TW-Renderer.asset");
            Assert.That(renderer, Does.Not.Contain("ScreenSpaceShadows"),
                "[Hs7] TW-Renderer.asset now has a Screen Space Shadows feature: the dropped keyword must come back " +
                "and the shadow coord in these six passes must be fixed to screen space first");
        }

        /// The HLSL of the pass named "Outline", up to its ENDHLSL.
        static string OutlinePass(string src)
        {
            int start = src.IndexOf("Name \"Outline\"");
            Assert.That(start, Is.GreaterThan(-1), "[Hs6] no Outline pass found");
            int end = src.IndexOf("ENDHLSL", start);
            Assert.That(end, Is.GreaterThan(start), "[Hs6] the Outline pass has no HLSL block");
            return src.Substring(start, end - start);
        }
    }
}
