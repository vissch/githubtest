// Phase: deaths (2026-09-28, implemented) — what a VatInstance's Tint carries (VatTint) and that VAT_URP.shader reads
// it back the same way: a living man's Tint is still only his team, the old team + 2 x pitch encoding is untouched,
// every new field (roll, squash, the feet pivot) survives the float at its ends, and the shader's literals are the C#
// constants. No clip or discard is added to the living path (VatEarlyZTests holds the rest of that).
using System.IO;
using System.Text.RegularExpressions;
using NUnit.Framework;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class VatTintTests
    {
        static string Shader() => File.ReadAllText(Path.Combine("Assets", "_Project", "Shaders", "VAT_URP.shader"));   // relative to the project, so Tools/otr.py runs it too

        [Test]
        public void ALivingMansTintIsStillOnlyHisTeam()
        {
            Assert.AreEqual(0f, VatTint.Pack(0, 0));
            Assert.AreEqual(1f, VatTint.Pack(1, 0));
            VatTint.Unpack(1f, out int team, out int pitch, out int roll, out int squash, out bool feet);
            Assert.AreEqual(1, team); Assert.AreEqual(0, pitch); Assert.AreEqual(0, roll); Assert.AreEqual(0, squash); Assert.IsFalse(feet);
        }

        [Test]
        public void TheOldEncodingIsASubsetOfTheNew()
        {
            for (int team = 0; team < 2; team++)
                for (int pitch = 0; pitch < VATRenderer.PitchSteps; pitch++)
                {
                    Assert.AreEqual(team + 2f * pitch, VatTint.Pack(team, pitch), "team + 2 x pitch step, exactly as before (" + team + ", " + pitch + ")");
                    VatTint.Unpack(team + 2f * pitch, out int t, out int p, out int r, out int q, out bool f);
                    Assert.AreEqual(team, t); Assert.AreEqual(pitch, p); Assert.AreEqual(0, r); Assert.AreEqual(0, q); Assert.IsFalse(f);
                }
        }

        [Test]
        public void EveryFieldRoundTripsAtItsEnds()
        {
            int[] pitches = { 0, 1, 16, 31 }, rolls = { 0, 1, 17, 31 }, squashes = { VatTint.SquashMin, -1, 0, 1, 13, VatTint.SquashMax };
            foreach (int team in new[] { 0, 1 })
                foreach (int pitch in pitches)
                    foreach (int roll in rolls)
                        foreach (int squash in squashes)
                            foreach (bool feet in new[] { false, true })
                            {
                                float tint = VatTint.Pack(team, pitch, roll, squash, feet);
                                Assert.Less(tint, 16777216f, "inside what a float holds exactly");
                                Assert.AreEqual(tint, (float)(int)tint, "a whole number");
                                VatTint.Unpack(tint, out int t, out int p, out int r, out int q, out bool f);
                                Assert.AreEqual(team, t, "team"); Assert.AreEqual(pitch, p, "pitch"); Assert.AreEqual(roll, r, "roll");
                                Assert.AreEqual(squash, q, "squash"); Assert.AreEqual(feet, f, "feet");
                            }
        }

        [Test]
        public void AWoundRidesAboveEveryOtherFieldAndLeavesThemAlone()
        {
            foreach (int wound in new[] { 0, 1, 31, VatTint.WoundMax })
                foreach (bool feet in new[] { false, true })
                {
                    float tint = VatTint.Pack(1, 31, 31, VatTint.SquashMin, feet, wound);
                    Assert.Less(tint, 16777216f, "inside what a float holds exactly");
                    Assert.AreEqual(wound, VatTint.Wound(tint), "the wound");
                    VatTint.Unpack(tint, out int t, out int p, out int r, out int q, out bool f);
                    Assert.AreEqual(1, t); Assert.AreEqual(31, p); Assert.AreEqual(31, r); Assert.AreEqual(VatTint.SquashMin, q); Assert.AreEqual(feet, f, "the feet bit is not read from the wound");
                }
            Assert.AreEqual(VatTint.WoundMax, VatTint.Wound(VatTint.Pack(0, 0, wound: 999)), "clamped, not wrapped");
            VatTint.Unpack(VatTint.Pack(1, 0, wound: 40), out int team, out int pitch, out _, out _, out _);
            Assert.AreEqual(1, team); Assert.AreEqual(0, pitch, "a wounded man standing is his team and no tumble");
        }

        [Test]
        public void ASquashPastItsRangeIsClampedNotWrapped()
        {
            VatTint.Unpack(VatTint.Pack(0, 0, 0, -40), out _, out _, out _, out int flat, out _);
            Assert.AreEqual(VatTint.SquashMin, flat, "flatter than flat is flat, not a stretch");
            VatTint.Unpack(VatTint.Pack(0, 0, 0, 90), out _, out _, out _, out int tall, out _);
            Assert.AreEqual(VatTint.SquashMax, tall);
            Assert.AreEqual(0.12f, VatTint.Height(VatTint.SquashMin), 1e-4f, "a man under a track is an eighth of his height");
            Assert.AreEqual(-32, VatTint.SquashOf(0.1f));
            Assert.AreEqual(0, VatTint.SquashOf(1f));
        }

        [Test]
        public void TheShaderDecodesWhatVatTintPacks()
        {
            string s = Shader();
            StringAssert.Contains("float gag = floor(inst.tint / " + VatTint.RollShift + ".0);", s, "everything above team and pitch");
            StringAssert.Contains("inst.tint -= gag * " + VatTint.RollShift + ".0;", s, "leaves team + 2 x pitch below");
            StringAssert.Contains("float roll = fmod(gag, 32.0)", s, "the roll's five bits");
            StringAssert.Contains("float sq = fmod(floor(gag / 32.0), 64.0);", s, "the squash's six bits");
            StringAssert.Contains("sq = sq > 31.5 ? sq - 64.0 : sq;", s, "two's complement");
            StringAssert.Contains("fmod(floor(gag / " + (VatTint.FeetShift / VatTint.RollShift) + ".0), 2.0) > 0.5 ? 0.0 : 0.9", s, "the feet bit, alone (the wound sits above it)");
            StringAssert.Contains("float wound = floor(gag / " + (VatTint.WoundShift / VatTint.RollShift) + ".0) / " + VatTint.WoundMax + ".0;", s, "the wound's six bits");
            var step = Regex.Match(s, @"float sy = 1\.0 \+ sq \* ([0-9.]+);");
            Assert.IsTrue(step.Success, "the squash scales the height");
            Assert.AreEqual(VatTint.SquashStep, float.Parse(step.Groups[1].Value, System.Globalization.CultureInfo.InvariantCulture), 1e-6f, "by the same step as VatTint.SquashStep");
            Assert.Less(s.IndexOf("float gag = floor"), s.IndexOf("float team = fmod(inst.tint, 2.0);"), "the gag is taken off before the team and pitch are read");
        }

        [Test]
        public void TheNewDecodeAddsNoClipToTheLivingPath()
        {
            string s = Shader();
            int from = s.IndexOf("Animated Animate("), to = s.IndexOf("return o;", from);
            Assert.Greater(from, 0); Assert.Greater(to, from);
            string body = s.Substring(from, to - from);
            StringAssert.DoesNotContain("clip(", body, "the vertex stage never discards");
            StringAssert.DoesNotContain("discard", body);
        }
    }
}
