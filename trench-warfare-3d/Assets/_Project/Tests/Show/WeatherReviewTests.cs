// Phase: review fixes (rv-77-p3-u19a) — the weather findings W4 and W5. Both are source contracts: Storm's meshes
// are made in Start and freed in OnDestroy, which only run in play (and Destroy throws in edit mode), and the rain's
// wrap is a deal between Rain.cs and Rain_URP.shader that nothing here can run. So each test pins the one line the
// finding is about and the number on the other side it has to agree with.
using System.Globalization;
using System.IO;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEngine;

namespace TW.Tests
{
    public class WeatherReviewTests
    {
        const string StormPath = "Assets/_Project/Presentation/Terrain/Storm.cs";
        const string RainPath = "Assets/_Project/Presentation/Terrain/Rain.cs";
        const string RainShaderPath = "Assets/_Project/Shaders/Rain_URP.shader";

        // [W4] every Mesh Storm builds has to be held in a field and destroyed in OnDestroy: the "Lightning glow"
        // card was a local, so each scene load left one mesh behind for good.
        [Test]
        public void TheLightningGlowCardIsDestroyedWithTheStorm()
        {
            string src = File.ReadAllText(StormPath);
            Assert.That(Regex.IsMatch(src, @"var\s+\w+\s*=\s*new Mesh"), Is.False,
                "[W4] a Mesh kept in a local can never be destroyed; hold it in a field");

            var made = Regex.Matches(src, @"(\w+)\s*=\s*new Mesh\b");
            Assert.That(made.Count, Is.GreaterThan(0), "[W4] Storm must still build its meshes here");

            var body = Regex.Match(src, @"void OnDestroy\(\)\s*\{([^}]*)\}");
            Assert.That(body.Success, Is.True, "[W4] Storm must have an OnDestroy to free its meshes in");

            foreach (Match m in made)
            {
                string field = m.Groups[1].Value;
                Assert.That(Regex.IsMatch(src, @"Mesh\s+[\w\s,]*\b" + field + @"\b[\w\s,]*;"), Is.True,
                    "[W4] " + field + " must be a Mesh field of Storm, not a local");
                Assert.That(body.Groups[1].Value, Does.Match(@"Destroy\(\s*" + field + @"\s*\)"),
                    "[W4] OnDestroy never destroys " + field + ": one mesh leaks per scene load");
            }
        }

        // [W5] the CPU wraps the fall at a whole number of boxes, but the shader scales the Y offset per streak, so
        // the wrap is only seamless when that scale times the wrap count is whole. The finding's expected value: the
        // jump a streak makes at a wrap is zero boxes, for every streak.
        [Test]
        public void TheFallWrapsAtAWholeNumberOfBoxesForEveryStreak()
        {
            string rain = File.ReadAllText(RainPath);
            var wrapConst = Regex.Match(rain, @"const float Wraps = ([0-9.]+)f;");
            Assert.That(wrapConst.Success, Is.True, "[W5] Rain must name the number of boxes it wraps the fall at");
            float wraps = float.Parse(wrapConst.Groups[1].Value, CultureInfo.InvariantCulture);

            Assert.That(rain, Does.Match(@"Mathf\.Repeat\(fallen\[k\], Box\[k\] \* Wraps\)"),
                "[W5] the wrap must use the same Wraps the shader is told about");
            Assert.That(rain, Does.Match(@"SetVector\(OffsetId, new Vector4\(fallen\.x, fallen\.y, fallen\.z, Wraps\)\)"),
                "[W5] _Offset's free w must carry the wrap count, or the shader cannot quantise against it");

            string shader = File.ReadAllText(RainShaderPath);
            Assert.That(shader, Does.Match(@"float w = max\(1\.0, _Offset\.w\);"),
                "[W5] the shader must read the wrap count off _Offset.w (guarded against 0)");
            var scale = Regex.Match(shader, @"float ys = round\(\(([0-9.]+) \+ ([0-9.]+) \* v\.random\.x\) \* w\) / w;");
            Assert.That(scale.Success, Is.True,
                "[W5] the per-streak Y scale must be rounded to steps of 1/w, or every flake jumps once per wrap");
            Assert.That(shader, Does.Match(@"velocity = float3\(_Fall\.x, -_Fall\.y \* ys, _Fall\.z\)"),
                "[W5] the fall speed must use the quantised ys, or the slant stops matching the motion");
            Assert.That(shader, Does.Match(@"_Offset\.xyz \* float3\(1\.0, ys, 1\.0\)"),
                "[W5] the Y offset must be scaled by the quantised ys, or every flake jumps once per wrap");

            float a = float.Parse(scale.Groups[1].Value, CultureInfo.InvariantCulture);
            float b = float.Parse(scale.Groups[2].Value, CultureInfo.InvariantCulture);
            for (int i = 0; i < 64; i++)
            {
                float r = i / 63f;
                float s = Mathf.Round((a + b * r) * wraps) / wraps;      // what the shader's text says it does
                float jump = Mathf.Abs(wraps * s - Mathf.Round(wraps * s));
                Assert.That(jump, Is.LessThan(1e-4f),
                    "[W5] streak " + i + " jumps " + jump + " boxes at a wrap; it must jump none");
            }
        }
    }
}
