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
    }
}
