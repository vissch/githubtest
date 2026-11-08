// Phase: B6 (implemented) — the boot logo's rules: its animation timing and that Resources/Logo ships every name
// layout.json gives. Moved out of SelectionTests, which guards selection, not the logo.
using NUnit.Framework;
using UnityEngine;
using TW.UI;

namespace TW.Tests
{
    public class LogoTests
    {
        [Test]
        public void TheLogoSpinsItsCogInThenTurnsSlowlyAndItsWordsLand()
        {
            Assert.That(GameLogo.GearAngle(0f), Is.EqualTo(GameLogo.GearFromDeg).Within(1e-3f), "the cog starts wound back");
            Assert.That(GameLogo.GearAngle(GameLogo.GearSpinIn), Is.EqualTo(0f).Within(1e-3f), "and arrives where the art has it");
            Assert.That(GameLogo.GearAngle(GameLogo.GearSpinIn + 10f), Is.EqualTo(10f * GameLogo.GearIdleDegPerSecond).Within(1e-3f), "then turns on slowly");
            GameLogo.Pose(0f, out _, out float fa0, out _, out float ta0, out _, out float ba0, out _);
            Assert.That(fa0, Is.EqualTo(0f)); Assert.That(ta0, Is.EqualTo(0f)); Assert.That(ba0, Is.EqualTo(0f));
            GameLogo.Pose(3f, out float fy, out float fa, out float ts, out float ta, out float bs, out float ba, out _);
            Assert.That(fa, Is.EqualTo(1f)); Assert.That(ta, Is.EqualTo(1f)); Assert.That(ba, Is.EqualTo(1f));
            Assert.That(fy, Is.EqualTo(0f).Within(1e-4f)); Assert.That(ts, Is.EqualTo(1f).Within(1e-4f)); Assert.That(bs, Is.EqualTo(1f).Within(1e-4f));
            GameLogo.Pose(GameLogo.TopAt + GameLogo.WordIn * 0.5f, out _, out _, out _, out _, out float bsEarly, out float baEarly, out _);
            Assert.That(baEarly, Is.EqualTo(0f), "TOADSTEEL lands after DIESELFRONT");
            Assert.That(GameLogo.GlintFrame(GameLogo.GlintAt - 0.01f, GameLogo.GlintAt, 12), Is.EqualTo(-1), "no glint before its time");
            Assert.That(GameLogo.GlintFrame(GameLogo.GlintAt, GameLogo.GlintAt, 12), Is.EqualTo(0));
            Assert.That(GameLogo.GlintFrame(GameLogo.GlintAt + GameLogo.GlintSweep - 0.001f, GameLogo.GlintAt, 12), Is.EqualTo(11), "the sweep ends on the last frame");
            Assert.That(GameLogo.GlintFrame(GameLogo.GlintAt + 2f, GameLogo.GlintAt, 12), Is.EqualTo(-1), "and rests between sweeps");
            Assert.That(GameLogo.GlintFrame(GameLogo.GlintAt + GameLogo.GlintPeriod, GameLogo.GlintAt, 12), Is.EqualTo(0), "then comes round again");
            Assert.That(GameLogo.Punch(-0.1f), Is.EqualTo(0f)); Assert.That(GameLogo.Punch(GameLogo.PunchSeconds * 0.5f), Is.EqualTo(GameLogo.PunchScale).Within(1e-4f));
            Assert.That(GameLogo.Punch(GameLogo.PunchSeconds + 0.01f), Is.EqualTo(0f), "a landing punches once");
            Assert.That(GameLogo.Kick(3f), Is.LessThan(0.01f), "the cog's kick settles");
        }

        [Test]
        public void TheLogoShipsWithEveryLayerItsLayoutNames()
        {
            var json = Resources.Load<TextAsset>(GameLogo.Folder + "/layout");
            Assert.That(json, Is.Not.Null, "Resources/Logo/layout.json");
            var names = new System.Collections.Generic.List<string> { "shadow", "window", "cog_top", "gear", "plate", "side_left", "side_right", "cog_left", "cog_right", "word_top", "word_bottom", "logo_full", "wordmark" };
            for (int i = 0; i < 12; i++) { names.Add("glint_top_" + i); names.Add("glint_bottom_" + i); }
            foreach (var n in names)
            {
                Assert.That(json.text.Contains("\"" + n + "\""), Is.True, $"layout names {n}");
                Assert.That(Resources.Load<Texture2D>(GameLogo.Folder + "/" + n), Is.Not.Null, $"Resources/Logo/{n}.png");
            }
            Assert.That(Resources.Load<Texture2D>(GameLogo.Folder + "/glint_top_12"), Is.Null, "twelve frames, not thirteen");
        }
    }
}
