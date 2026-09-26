// Phase: tooling (AOSA loop, 2026-09-25) - the knob registry parses, falls back, logs what was read, and with nothing
// set every knob-backed value is the constant it replaced (the old numbers are written out here on purpose).
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Presentation;
using TW.Presentation.Units;
using TW.Presentation.Tactical;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class KnobsTests
    {
        [SetUp] public void SetUp() => Knobs.Clear();
        [TearDown] public void TearDown() => Knobs.Clear();

        [Test]
        public void NothingSet_GetReturnsTheFallback_AndReadRecordsIt()
        {
            Assert.AreEqual(170f, Knobs.Get("t.float", 170f));
            Assert.AreEqual(42, Knobs.Get("t.int", 42));
            Assert.AreEqual(true, Knobs.Get("t.bool", true));
            Assert.AreEqual(0, Knobs.Overrides.Count);
            Assert.AreEqual("170", Knobs.Read["t.float"]);
            Assert.AreEqual("42", Knobs.Read["t.int"]);
            Assert.AreEqual("1", Knobs.Read["t.bool"]);
        }

        [Test]
        public void Parse_CommaSeparated_FloatAndInt()
        {
            Knobs.Parse("a=1.5,b=2");
            Assert.AreEqual(1.5f, Knobs.Get("a", 0f));
            Assert.AreEqual(2, Knobs.Get("b", 0));
            Assert.AreEqual(2f, Knobs.Get("b", 0f), "an integer reads as a float too");
            Assert.AreEqual("1.5", Knobs.Read["a"]);
            Assert.AreEqual("2", Knobs.Read["b"]);
        }

        [Test]
        public void Parse_PipeAndSemicolonSeparated()
        {
            Knobs.Parse("a=3|b=4; c = 5 ");
            Assert.AreEqual(3, Knobs.Get("a", 0));
            Assert.AreEqual(4, Knobs.Get("b", 0));
            Assert.AreEqual(5, Knobs.Get("c", 0));
            Assert.AreEqual(3, Knobs.Overrides.Count);
        }

        [Test]
        public void Bools()
        {
            Knobs.Parse("a=1,b=0,c=true,d=FALSE");
            Assert.IsTrue(Knobs.Get("a", false));
            Assert.IsFalse(Knobs.Get("b", true));
            Assert.IsTrue(Knobs.Get("c", false));
            Assert.IsFalse(Knobs.Get("d", true));
            Assert.AreEqual("0", Knobs.Read["d"]);
        }

        [Test]
        public void Unparseable_FallsBack_WithOneWarningPerName()
        {
            Knobs.Parse("f=abc,i=2.5,b=maybe,j=3.0");
            LogAssert.Expect(LogType.Warning, new Regex("f=abc"));
            Assert.AreEqual(7f, Knobs.Get("f", 7f));
            Assert.AreEqual(7f, Knobs.Get("f", 7f), "asked again: the same fallback, and no second warning");
            LogAssert.Expect(LogType.Warning, new Regex("i=2.5"));
            Assert.AreEqual(9, Knobs.Get("i", 9));
            LogAssert.Expect(LogType.Warning, new Regex("b=maybe"));
            Assert.IsTrue(Knobs.Get("b", true));
            Assert.AreEqual(3, Knobs.Get("j", 0), "a whole number written as a float is still an integer");
            Assert.AreEqual("7", Knobs.Read["f"]);
        }

        [Test]
        public void LaterParse_OverridesEarlier_AndSetOverridesBoth()
        {
            Knobs.Parse("a=1,b=2");
            Knobs.Parse("a=10");
            Assert.AreEqual(10, Knobs.Get("a", 0));
            Assert.AreEqual(2, Knobs.Get("b", 0));
            Knobs.Set("b", "20");
            Assert.AreEqual("20", Knobs.Overrides["b"]);
        }

        [Test]
        public void Read_KeepsTheFirstRead()
        {
            Assert.AreEqual(5, Knobs.Get("a", 5));
            Knobs.Set("a", "6");
            Assert.AreEqual(6, Knobs.Get("a", 5));
            Assert.AreEqual("5", Knobs.Read["a"]);
        }

        [Test]
        public void Clear_DropsEverything_AndEveryChangeBumpsGeneration()
        {
            int g = Knobs.Generation;
            Knobs.Parse("a=1");
            Assert.AreEqual(g + 1, Knobs.Generation);
            Knobs.Set("b", "2");
            Assert.AreEqual(g + 2, Knobs.Generation);
            Knobs.Get("a", 0);
            Knobs.Clear();
            Assert.AreEqual(g + 3, Knobs.Generation);
            Assert.AreEqual(0, Knobs.Overrides.Count);
            Assert.AreEqual(0, Knobs.Read.Count);
            Assert.AreEqual(4, Knobs.Get("a", 4));
        }

        [Test]
        public void ToJson_IsSortedAndEscaped()
        {
            Assert.AreEqual("{\"set\":{},\"read\":{}}", Knobs.ToJson());
            Knobs.Set("z.q", "a\"b\\c");
            Knobs.Parse("b=2");
            Knobs.Get("b", 0);
            Knobs.Get("a", 1.5f);
            Assert.AreEqual("{\"set\":{\"b\":\"2\",\"z.q\":\"a\\\"b\\\\c\"},\"read\":{\"a\":\"1.5\",\"b\":\"2\"}}", Knobs.ToJson());
        }

        // ---- with nothing set, each area's knob-backed values are the constants they replaced

        [Test]
        public void Units_NothingSet_IsTheOldConstants()
        {
            Assert.AreEqual(120f, LodTiers.BlendZoom);
            Assert.AreEqual(1500000, LodTiers.VertexBudget);
            Assert.AreEqual(3f, LodTiers.CullRadius);
            Assert.AreEqual(120f, LodTiers.ReadBlendZoom());
            Assert.AreEqual(1500000, LodTiers.ReadVertexBudget());
            Assert.AreEqual(3f, LodTiers.ReadCullRadius());
            Knobs.Set("vat.blendZoom", "90");
            Assert.AreEqual(90f, LodTiers.ReadBlendZoom(), "and a set knob is what is read");
        }

        [Test]
        public void Camera_NothingSet_IsTheOldConstants()
        {
            int[] old = { 1024, 512, 384, 512, 256, 256, 64, 256, 128, 128 };
            float scale = Knobs.Get("debris.capacityScale", 1f);
            Assert.AreEqual(1f, scale);
            Assert.AreEqual(old.Length, (int)DebrisRenderer.Piece.Count);
            for (int k = 0; k < old.Length; k++)
            {
                Assert.AreEqual(old[k], DebrisRenderer.CapacityOf((DebrisRenderer.Piece)k));
                Assert.AreEqual(old[k], DebrisRenderer.CapacityOf((DebrisRenderer.Piece)k, scale));
            }
            Assert.AreEqual(55f, Knobs.Get("debris.shareNear", DebrisMath.ShareNear));
            Assert.AreEqual(120f, Knobs.Get("debris.shareFar", DebrisMath.ShareFar));
            foreach (float d in new[] { 0f, 54.9f, 55f, 80f, 119.9f, 120f, 500f })
                Assert.AreEqual(DebrisMath.Share(d), DebrisMath.Share(d, DebrisMath.ShareNear, DebrisMath.ShareFar));
            Assert.AreEqual(1536, FlipbookFx.MaxCards);
            Assert.AreEqual(160, TankRenderer.MaxLoose);
            Assert.AreEqual(1536, Knobs.Get("flipbook.maxCards", FlipbookFx.MaxCards));
            Assert.AreEqual(160, Knobs.Get("tank.maxLoose", TankRenderer.MaxLoose));
        }

        [Test]
        public void SmokeKnobs_DefaultIsTheNewLook_AndTheOldValuesDrawTheOldSmoke()
        {
            // AOSA C52: the default thins a barrage's smoke at the standard view; fx.smokeSoft=0,fx.smokeAlpha=1 is the old look
            Assert.AreEqual("fx.smokeSoft", FlipbookFx.SoftKnob);
            Assert.AreEqual("fx.smokeAlpha", FlipbookFx.AlphaKnob);
            Assert.AreEqual(FlipbookFx.DefaultSoft, FlipbookFx.ReadSoft());
            Assert.AreEqual(FlipbookFx.DefaultAlpha, FlipbookFx.ReadAlpha());
            Assert.AreEqual("0.3", Knobs.Read["fx.smokeSoft"]);
            Assert.AreEqual("0.85", Knobs.Read["fx.smokeAlpha"]);
            Assert.Greater(FlipbookFx.DefaultSoft, FlipbookFx.OldSoft, "the default is the new look");
            Assert.Less(FlipbookFx.DefaultAlpha, FlipbookFx.OldAlpha, "the default is the new look");
            Assert.Less(FlipbookFx.SmokeOpacity(0.65f, FlipbookFx.ReadAlpha(), 0f), 0.65f, "thinner at the standard view");
            Assert.AreEqual(0.65f, FlipbookFx.SmokeOpacity(0.65f, FlipbookFx.ReadAlpha(), 1f), 1e-6f, "the recipe's own among the men");

            // the old values: the recipe's opacity exactly (the float the old code passed), at every zoom
            Knobs.Set(FlipbookFx.SoftKnob, "0");
            Knobs.Set(FlipbookFx.AlphaKnob, "1");
            Assert.AreEqual(0f, FlipbookFx.ReadSoft());
            Assert.AreEqual(1f, FlipbookFx.ReadAlpha());
            foreach (float close in new[] { 0f, 0.25f, 0.5f, 0.999f, 1f })
            {
                Assert.IsTrue(FlipbookFx.SmokeOpacity(0.65f, FlipbookFx.ReadAlpha(), close) == 0.65f, "shell smoke, closeUp " + close);
                Assert.IsTrue(FlipbookFx.SmokeOpacity(0.6f, FlipbookFx.ReadAlpha(), close) == 0.6f, "cook-off smoke, closeUp " + close);
            }

            // out of range: softness never below 0 (the shader skips it), opacity kept in [0, 1]
            Knobs.Set(FlipbookFx.SoftKnob, "-2");
            Knobs.Set(FlipbookFx.AlphaKnob, "3");
            Assert.AreEqual(0f, FlipbookFx.ReadSoft());
            Assert.AreEqual(1f, FlipbookFx.ReadAlpha());
            Knobs.Set(FlipbookFx.AlphaKnob, "-1");
            Assert.AreEqual(0f, FlipbookFx.ReadAlpha());
        }

        [Test]
        public void NightSmokeKnobs_DefaultIsTheNewLook_AndTheOldValuesDrawTheOldSmoke()
        {
            // AOSA C59: the default draws a moonlit field's burst cloud and smoke dark warm grey and 40% narrower at the
            // standard view; fx.smokeNight=0,fx.smokeNightSize=1 is the old look
            Assert.AreEqual("fx.smokeNight", FlipbookFx.NightKnob);
            Assert.AreEqual("fx.smokeNightSize", FlipbookFx.NightSizeKnob);
            Assert.AreEqual(FlipbookFx.DefaultNight, FlipbookFx.ReadNight());
            Assert.AreEqual(FlipbookFx.DefaultNightSize, FlipbookFx.ReadNightSize());
            Assert.AreEqual("0.15", Knobs.Read["fx.smokeNight"]);
            Assert.AreEqual("0.6", Knobs.Read["fx.smokeNightSize"]);
            Assert.Greater(FlipbookFx.DefaultNight, FlipbookFx.OldNight, "the default is the new look");
            Assert.Less(FlipbookFx.DefaultNightSize, FlipbookFx.OldNightSize, "the default is the new look");
            Assert.GreaterOrEqual(FlipbookFx.NightLit, 0.5f, "the shader draws _Lit < 0.5 as an additive book");

            // the new tint: warm (red over green over blue), a valid colour, and at the default darker than the ground
            // under the moon (night key 0.56, 0.70, 1.0 on mud; the old Burst cloud measured 0.33, 0.40, 0.58 on screen)
            var tint = FlipbookFx.NightTint(FlipbookFx.DefaultNight);
            Assert.Greater(tint.r, tint.g); Assert.Greater(tint.g, tint.b);
            Assert.LessOrEqual(tint.r, 1f);
            float mid = Mathf.Lerp(0.43f, FlipbookFx.NightShade, FlipbookFx.NightLit);   // the drawings' middle ink, mixed as the shader does
            Assert.AreEqual(FlipbookFx.DefaultNight, (0.299f * tint.r + 0.587f * tint.g + 0.114f * tint.b) * mid, 1e-4f, "the knob is the drawn value");
            Assert.Less(tint.r * mid, 0.2f, "#2E2A28-#3A342F before fog and grade");

            // the moon: only a dark field not lit from a molten floor (the lava field keeps its rose smoke, day is untouched)
            Assert.IsTrue(FlipbookFx.MoonLit(true, false));
            Assert.IsFalse(FlipbookFx.MoonLit(true, true));
            Assert.IsFalse(FlipbookFx.MoonLit(false, false));
            Assert.IsFalse(FlipbookFx.MoonLit(false, true));

            // the size: the knob at the standard view on a moonlit field, 1 among the men and on every other field
            Assert.AreEqual(FlipbookFx.DefaultNightSize, FlipbookFx.NightScale(FlipbookFx.ReadNightSize(), 0f, true));
            Assert.IsTrue(FlipbookFx.NightScale(FlipbookFx.ReadNightSize(), 1f, true) == 1f, "full size among the men");
            Assert.IsTrue(FlipbookFx.NightScale(FlipbookFx.ReadNightSize(), 0f, false) == 1f, "day and lava keep their size");

            // the old values: nothing painted, and a width factor of exactly 1 at every zoom (the float the old code passed)
            Knobs.Set(FlipbookFx.NightKnob, "0");
            Knobs.Set(FlipbookFx.NightSizeKnob, "1");
            Assert.AreEqual(0f, FlipbookFx.ReadNight());
            Assert.AreEqual(1f, FlipbookFx.ReadNightSize());
            foreach (float close in new[] { 0f, 0.25f, 0.5f, 0.999f, 1f })
            foreach (bool moon in new[] { false, true })
            {
                float n = FlipbookFx.NightScale(FlipbookFx.ReadNightSize(), close, moon);
                Assert.IsTrue(n == 1f, "closeUp " + close + ", moonlit " + moon);
                foreach (float r in new[] { 2f, 3.7f, 9f })
                    Assert.IsTrue(r * 2.6f * n == r * 2.6f, "burst width, r " + r);
            }

            // out of range: the value kept in [0, 1], the size in [0.05, 4]
            Knobs.Set(FlipbookFx.NightKnob, "-1");
            Knobs.Set(FlipbookFx.NightSizeKnob, "0");
            Assert.AreEqual(0f, FlipbookFx.ReadNight());
            Assert.AreEqual(0.05f, FlipbookFx.ReadNightSize());
            Knobs.Set(FlipbookFx.NightKnob, "3");
            Knobs.Set(FlipbookFx.NightSizeKnob, "9");
            Assert.AreEqual(1f, FlipbookFx.ReadNight());
            Assert.AreEqual(4f, FlipbookFx.ReadNightSize());
        }

        [Test]
        public void NightEarthKnobs_DefaultIsTheNewLook_AndTheOldValuesDrawTheOldColumn()
        {
            // AOSA C57: the default draws a moonlit field's earth column opaque dark brown and smaller at the standard view;
            // fx.columnEarth=0,fx.columnEarthSize=1 is the old look
            Assert.AreEqual("fx.columnEarth", FlipbookFx.EarthKnob);
            Assert.AreEqual("fx.columnEarthSize", FlipbookFx.EarthSizeKnob);
            Assert.AreEqual(FlipbookFx.DefaultEarth, FlipbookFx.ReadEarth());
            Assert.AreEqual(FlipbookFx.DefaultEarthSize, FlipbookFx.ReadEarthSize());
            Assert.AreEqual("0.22", Knobs.Read["fx.columnEarth"]);
            Assert.AreEqual("0.38", Knobs.Read["fx.columnEarthSize"]);
            Assert.Greater(FlipbookFx.DefaultEarth, FlipbookFx.OldEarth, "the default is the new look");
            Assert.Less(FlipbookFx.DefaultEarthSize, FlipbookFx.OldEarthSize, "the default is the new look");

            // the new tint: #3B2A1E's brown (red over green over blue, blue about half of red), a valid colour, and the knob
            // is the value drawn at the drawing's middle ink, mixed as the shader mixes it (NightLit of the shade grey)
            var tint = FlipbookFx.EarthTint(FlipbookFx.DefaultEarth);
            Assert.Greater(tint.r, tint.g); Assert.Greater(tint.g, tint.b);
            Assert.AreEqual(0.508f, tint.b / tint.r, 1e-3f, "#3B2A1E: blue 30 over red 59");
            Assert.AreEqual(0.712f, tint.g / tint.r, 1e-3f, "#3B2A1E: green 42 over red 59");
            Assert.LessOrEqual(tint.r, 1f);
            float mid = Mathf.Lerp(0.61f, FlipbookFx.NightShade, FlipbookFx.NightLit);
            Assert.AreEqual(FlipbookFx.DefaultEarth, (0.299f * tint.r + 0.587f * tint.g + 0.114f * tint.b) * mid, 1e-4f, "the knob is the drawn value");
            Assert.AreEqual(0f, FlipbookFx.EarthTint(0f).r, "value 0 is black (and NightEarth paints nothing at 0)");

            // the size: the knob at the standard view on a moonlit field, 1 among the men and on every other field
            Assert.AreEqual(FlipbookFx.DefaultEarthSize, FlipbookFx.NightScale(FlipbookFx.ReadEarthSize(), 0f, true));
            Assert.IsTrue(FlipbookFx.NightScale(FlipbookFx.ReadEarthSize(), 1f, true) == 1f, "full size among the men");
            Assert.IsTrue(FlipbookFx.NightScale(FlipbookFx.ReadEarthSize(), 0f, false) == 1f, "day and lava keep their size");
            // r 8 (a barrage shell) at the standard view: the drawing (62% x 79% of its card, aspect 512/754) is 3-5 m wide
            // and 7-10 m tall while it rises (swell 1 -> 1.175 at mid-life)
            float card = 8f * 2.1f * FlipbookFx.DefaultEarthSize, aspect = 512f / 754f;
            Assert.That(card * 0.62f * 1.1f, Is.InRange(3f, 5f));
            Assert.That(card / aspect * 0.79f * 1.1f, Is.InRange(7f, 10f));

            // the old values: nothing painted, and a width factor of exactly 1 at every zoom (the float the old code passed,
            // with fx.columnScale on top as before)
            Knobs.Set(FlipbookFx.EarthKnob, "0");
            Knobs.Set(FlipbookFx.EarthSizeKnob, "1");
            Assert.AreEqual(0f, FlipbookFx.ReadEarth());
            Assert.AreEqual(1f, FlipbookFx.ReadEarthSize());
            foreach (float close in new[] { 0f, 0.25f, 0.5f, 0.999f, 1f })
            foreach (bool moon in new[] { false, true })
            {
                float n = FlipbookFx.NightScale(FlipbookFx.ReadEarthSize(), close, moon);
                Assert.IsTrue(n == 1f, "closeUp " + close + ", moonlit " + moon);
                foreach (float r in new[] { 2f, 3.7f, 8f, 9f })
                foreach (float scale in new[] { 1f, 3f, 0.5f })
                    Assert.IsTrue(r * 2.1f * scale * n == r * 2.1f * scale, "column width, r " + r + ", fx.columnScale " + scale);
            }

            // out of range: the value kept in [0, 1], the size in [0.05, 4]
            Knobs.Set(FlipbookFx.EarthKnob, "-1");
            Knobs.Set(FlipbookFx.EarthSizeKnob, "0");
            Assert.AreEqual(0f, FlipbookFx.ReadEarth());
            Assert.AreEqual(0.05f, FlipbookFx.ReadEarthSize());
            Knobs.Set(FlipbookFx.EarthKnob, "3");
            Knobs.Set(FlipbookFx.EarthSizeKnob, "9");
            Assert.AreEqual(1f, FlipbookFx.ReadEarth());
            Assert.AreEqual(4f, FlipbookFx.ReadEarthSize());
        }

        [Test]
        public void NightSmokeWeightKnobs_DefaultIsTheNewLook_AndTheOldValuesDrawC59sSmoke()
        {
            // AOSA C61: the default draws the night smoke charcoal-umber rather than tan, lit less by the burst, with hard-cut
            // edges; fx.smokeNightWarm=1,fx.smokeNightFire=1,fx.smokeHard=0 is C59's look
            Assert.AreEqual("fx.smokeNightWarm", FlipbookFx.NightWarmKnob);
            Assert.AreEqual("fx.smokeNightFire", FlipbookFx.NightFireKnob);
            Assert.AreEqual("fx.smokeHard", FlipbookFx.HardKnob);
            Assert.AreEqual(FlipbookFx.DefaultNightWarm, FlipbookFx.ReadNightWarm());
            Assert.AreEqual(FlipbookFx.DefaultNightFire, FlipbookFx.ReadNightFire());
            Assert.AreEqual(FlipbookFx.DefaultHard, FlipbookFx.ReadHard());
            Assert.AreEqual("0.35", Knobs.Read["fx.smokeNightWarm"]);
            Assert.AreEqual("0.35", Knobs.Read["fx.smokeNightFire"]);
            Assert.AreEqual("1", Knobs.Read["fx.smokeHard"]);
            Assert.Less(FlipbookFx.DefaultNightWarm, FlipbookFx.OldNightWarm, "the default is the new look");
            Assert.Less(FlipbookFx.DefaultNightFire, FlipbookFx.OldNightFire, "the default is the new look");
            Assert.Greater(FlipbookFx.DefaultHard, FlipbookFx.OldHard, "the default is the new look");

            // the new tint: still a little warm (charcoal-umber, red over green over blue), less warm than C59's, and at the
            // same drawn value (fx.smokeNight is the value; the warmth only turns the hue)
            float mid = Mathf.Lerp(0.43f, FlipbookFx.NightShade, FlipbookFx.NightLit);
            var c59 = FlipbookFx.NightTint(FlipbookFx.DefaultNight);
            var now = FlipbookFx.NightTint(FlipbookFx.DefaultNight, FlipbookFx.ReadNightWarm());
            Assert.Greater(now.r, now.g); Assert.Greater(now.g, now.b);
            Assert.Greater(now.b / now.r, c59.b / c59.r, "less warm than C59");
            Assert.AreEqual(FlipbookFx.DefaultNight, (0.299f * now.r + 0.587f * now.g + 0.114f * now.b) * mid, 1e-4f, "the same value");
            var grey = FlipbookFx.NightTint(FlipbookFx.DefaultNight, 0f);
            Assert.AreEqual(grey.r, grey.g, 1e-6f); Assert.AreEqual(grey.g, grey.b, 1e-6f);

            // the old values: C59's hue and tint to the bit (the floats the C59 code computed), the burst's light whole and no cut
            Knobs.Set(FlipbookFx.NightWarmKnob, "1");
            Knobs.Set(FlipbookFx.NightFireKnob, "1");
            Knobs.Set(FlipbookFx.HardKnob, "0");
            Assert.AreEqual(1f, FlipbookFx.ReadNightWarm());
            Assert.AreEqual(1f, FlipbookFx.ReadNightFire());
            Assert.AreEqual(0f, FlipbookFx.ReadHard());
            var hue = FlipbookFx.NightHue;
            var hueAt = FlipbookFx.NightHueAt(FlipbookFx.ReadNightWarm());
            Assert.IsTrue(hueAt.r == hue.r && hueAt.g == hue.g && hueAt.b == hue.b, "C59's hue, to the bit");   // Color == is approximate
            foreach (float value in new[] { 0.05f, 0.15f, 0.3f, 1f })
            {
                // C59's NightTint, written out
                float luma = 0.299f * hue.r + 0.587f * hue.g + 0.114f * hue.b;
                float k = value / (luma * Mathf.Lerp(0.43f, FlipbookFx.NightShade, FlipbookFx.NightLit));
                var old = new Color(hue.r * k, hue.g * k, hue.b * k, 1f);
                var at = FlipbookFx.NightTint(value, FlipbookFx.ReadNightWarm());
                Assert.IsTrue(at.r == old.r && at.g == old.g && at.b == old.b && at.a == old.a, "value " + value);
                var one = FlipbookFx.NightTint(value);
                Assert.IsTrue(one.r == at.r && one.g == at.g && one.b == at.b, "the one-argument NightTint is C59's");
            }

            // out of range: all three kept in [0, 1]
            Knobs.Set(FlipbookFx.NightWarmKnob, "-1");
            Knobs.Set(FlipbookFx.NightFireKnob, "-1");
            Knobs.Set(FlipbookFx.HardKnob, "-1");
            Assert.AreEqual(0f, FlipbookFx.ReadNightWarm());
            Assert.AreEqual(0f, FlipbookFx.ReadNightFire());
            Assert.AreEqual(0f, FlipbookFx.ReadHard());
            Knobs.Set(FlipbookFx.NightWarmKnob, "3");
            Knobs.Set(FlipbookFx.NightFireKnob, "3");
            Knobs.Set(FlipbookFx.HardKnob, "3");
            Assert.AreEqual(1f, FlipbookFx.ReadNightWarm());
            Assert.AreEqual(1f, FlipbookFx.ReadNightFire());
            Assert.AreEqual(1f, FlipbookFx.ReadHard());
        }

        [Test]
        public void Terrain_NothingSet_IsTheOldConstants()
        {
            Assert.AreEqual(48, Knobs.Get("props.maxLoose", PropDestruction.MaxLoose));
            Assert.AreEqual(256, Knobs.Get("props.maxFalling", PropDestruction.MaxFalling));
            Assert.AreEqual(2800, Knobs.Get("rain.maxStreaks", Rain.MaxStreaks));
            Assert.AreEqual(6, Knobs.Get("life.maxRats", SmallLife.MaxRats));
            Assert.AreEqual(900, Knobs.Get("life.maxMotes", SmallLife.MaxMotes));
            Assert.AreEqual(12, Knobs.Get("lights.maxLanterns", NightLights.MaxLanterns));
            Assert.AreEqual(14, Knobs.Get("lights.maxTrenchLamps", NightLights.MaxTrenchLamps));
            Assert.AreEqual(5, Knobs.Get("lights.maxFires", NightLights.MaxFires));
            Assert.AreEqual(6, Knobs.Get("lights.maxTorches", NightLights.MaxTorches));
            Assert.AreEqual(12, Knobs.Get("lights.maxPropLamps", NightLights.MaxPropLamps));
            Assert.AreEqual(8, Knobs.Get("lights.poolSize", NightLights.PoolSize));
            Assert.AreEqual(2.0, (double)Knobs.Get("terrain.chunkBudgetMs", (float)GreyboxTerrainView.ChunkBudgetMs));
            Assert.AreEqual(2.0, (double)Knobs.Get("terrain.paintBudgetMs", (float)GreyboxTerrainView.PaintBudgetMs));
            Assert.AreEqual(100.0, (double)Knobs.Get("terrain.applyIntervalMs", (float)GreyboxTerrainView.ApplyIntervalMs));
            Assert.AreEqual(true, Knobs.Get("terrain.applyOnDrain", GreyboxTerrainView.ApplyOnDrain));
            Assert.AreEqual(0.0, (double)Knobs.Get("terrain.mipIntervalMs", (float)GreyboxTerrainView.MipIntervalMs));
        }

        [Test]
        public void ShadowDistanceKnob_NothingSet_IsTheAssetsValue_AndASetValueIsRead()
        {
            // AOSA C79: render.shadowDistance falls back to the pipeline asset's own value, so nothing set writes nothing.
            // The asset is only read here (SerializedObject), never written, so the test leaves TW-URP.asset clean.
            Assert.AreEqual("render.shadowDistance", Atmosphere.ShadowDistanceKnob);
            var asset = UnityEditor.AssetDatabase.LoadAssetAtPath<ScriptableObject>("Assets/_Project/Settings/TW-URP.asset");
            Assert.IsNotNull(asset, "TW-URP.asset");
            var prop = new UnityEditor.SerializedObject(asset).FindProperty("m_ShadowDistance");
            Assert.IsNotNull(prop, "m_ShadowDistance");
            Assert.AreEqual(Atmosphere.AssetShadowDistance, prop.floatValue, "the asset still holds the old 220 m");
            Assert.AreEqual(220f, Atmosphere.ReadShadowDistance(Atmosphere.AssetShadowDistance));
            Assert.AreEqual("220", Knobs.Read["render.shadowDistance"]);
            Assert.AreEqual(0, Knobs.Overrides.Count);
            Knobs.Set(Atmosphere.ShadowDistanceKnob, "160");
            Assert.AreEqual(160f, Atmosphere.ReadShadowDistance(Atmosphere.AssetShadowDistance));
            Knobs.Set(Atmosphere.ShadowDistanceKnob, "40");
            Assert.AreEqual(40f, Atmosphere.ReadShadowDistance(Atmosphere.AssetShadowDistance));
            Knobs.Set(Atmosphere.ShadowDistanceKnob, "-5");
            Assert.AreEqual(0f, Atmosphere.ReadShadowDistance(Atmosphere.AssetShadowDistance), "never below 0");
        }

        [Test]
        public void TerrainApply_OldPathAndHeldClock_UploadWithMipsOnEveryDirtyFrame()
        {
            // AOSA C13: with the interval at 0 (the old path; the default is 100 since cycle 3), and whenever the clock is
            // held (Unmetered), the colour texture is uploaded with its mips on every dirty frame, whatever the time since
            foreach (double since in new[] { 0.0, 1e-3, 16.7, 1e6, double.PositiveInfinity })
            foreach (bool drained in new[] { false, true })
            foreach (bool onDrain in new[] { false, true })
            {
                Assert.IsTrue(GreyboxTerrainView.ApplyDue(since, 0.0, drained, onDrain, false));
                Assert.IsTrue(GreyboxTerrainView.MipsDue(since, GreyboxTerrainView.MipIntervalMs, drained, false));
                Assert.IsTrue(GreyboxTerrainView.ApplyDue(since, 100.0, drained, onDrain, true), "Unmetered uploads every dirty frame");
                Assert.IsTrue(GreyboxTerrainView.MipsDue(since, 1000.0, drained, true), "Unmetered rebuilds the mips");
            }
            // with an interval: not before it, at it, and on a drained queue only when applyOnDrain
            Assert.IsFalse(GreyboxTerrainView.ApplyDue(99.9, 100.0, false, true, false));
            Assert.IsTrue(GreyboxTerrainView.ApplyDue(100.0, 100.0, false, true, false));
            Assert.IsTrue(GreyboxTerrainView.ApplyDue(0.0, 100.0, true, true, false));
            Assert.IsFalse(GreyboxTerrainView.ApplyDue(0.0, 100.0, true, false, false));
            Assert.IsTrue(GreyboxTerrainView.ApplyDue(double.PositiveInfinity, 100.0, false, false, false), "the first upload is never held back");
            // mips: skipped between rebuilds, always rebuilt once the queue has drained
            Assert.IsFalse(GreyboxTerrainView.MipsDue(10.0, 1000.0, false, false));
            Assert.IsTrue(GreyboxTerrainView.MipsDue(1000.0, 1000.0, false, false));
            Assert.IsTrue(GreyboxTerrainView.MipsDue(10.0, 1000.0, true, false));
        }
    }
}
