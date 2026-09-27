// Phase: AOSA C109 (juice J01, from C108's finding) - fx.columnPlay: the part of the Column book the old dry column
// plays on a moonlit field before it fades out, so it stops before the book's late arcs (frames 6-15). At its default
// (1) no card is cut and the old fade and removal lines run.
using System.IO;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class ColumnPlayTests
    {
        const int Frames = 16;        // the Column sheet (4 x 4, 16 frames, Play 0 = all)
        const float OldLife = 1.8f;   // the old dry column's life (CombatFx)

        [SetUp] public void SetUp() => Knobs.Clear();
        [TearDown] public void TearDown() => Knobs.Clear();

        static float OldFade(float k) => 1f - Mathf.SmoothStep(0f, 1f, (k - 0.65f) / 0.35f);

        [Test]
        public void Knob_DefaultIsToday_AndReadsAreClamped()
        {
            Assert.AreEqual("fx.columnPlay", FlipbookFx.ColumnPlayKnob);
            Assert.AreEqual(FlipbookFx.OldColumnPlay, FlipbookFx.DefaultColumnPlay, "the default is today's column");
            Assert.AreEqual(1f, FlipbookFx.ReadColumnPlay());
            Assert.AreEqual("1", Knobs.Read["fx.columnPlay"]);

            Knobs.Set(FlipbookFx.ColumnPlayKnob, "0.4");
            Assert.AreEqual(0.4f, FlipbookFx.ReadColumnPlay(), 1e-6f);
            Knobs.Set(FlipbookFx.ColumnPlayKnob, "0");
            Assert.AreEqual(FlipbookFx.MinColumnPlay, FlipbookFx.ReadColumnPlay(), 1e-6f, "never cut to nothing");
            Knobs.Set(FlipbookFx.ColumnPlayKnob, "3");
            Assert.AreEqual(1f, FlipbookFx.ReadColumnPlay());
        }

        [Test]
        public void Cut_IsZeroExactlyAtOne()
        {
            Assert.IsTrue(FlipbookFx.ColumnPlayCut(1f) == 0f, "knob 1: no cut, the old card bit for bit");
            Assert.IsTrue(FlipbookFx.ColumnPlayCut(FlipbookFx.DefaultColumnPlay) == 0f);
            Assert.IsTrue(FlipbookFx.ColumnPlayCut(2f) == 0f);
            Assert.AreEqual(0.4f, FlipbookFx.ColumnPlayCut(0.4f), 1e-6f);
            Assert.AreEqual(FlipbookFx.MinColumnPlay, FlipbookFx.ColumnPlayCut(0f), 1e-6f);
            Assert.IsTrue(FlipbookFx.CardEnd(OldLife, 0f) == OldLife, "no cut: gone at its life");
            Assert.AreEqual(0.72f, FlipbookFx.CardEnd(OldLife, 0.4f), 1e-6f, "0.4: gone at 0.72 s");
        }

        [Test]
        public void CutFade_EndsAtZeroAtTheCut_WithNoPop()
        {
            foreach (float cut in new[] { 0.3f, 0.4f, 0.5f })
            {
                Assert.AreEqual(1f, FlipbookFx.CutFade(0f, cut), 1e-6f);
                Assert.AreEqual(1f, FlipbookFx.CutFade(cut * (1f - FlipbookFx.PlayFade) - 1e-4f, cut), 1e-6f, "whole until its last PlayFade");
                Assert.AreEqual(0f, FlipbookFx.CutFade(cut, cut), 1e-6f, "0 at the cut: nothing pops off");
                Assert.AreEqual(0f, FlipbookFx.CutFade(1f, cut), 1e-6f);
                // continuous and falling: no step bigger than a smooth ramp's over 1000 samples of the cut life
                float last = 1f;
                for (int i = 1; i <= 1000; i++)
                {
                    float f = FlipbookFx.CutFade(cut * i / 1000f, cut);
                    Assert.LessOrEqual(f, last + 1e-6f);
                    Assert.Less(last - f, 0.01f, "a smooth fade, no pop");
                    last = f;
                }
            }
            // a cut of 1 would be today's fade (the curve is every card's own last 35%)
            for (int i = 0; i <= 20; i++) Assert.AreEqual(OldFade(i / 20f), FlipbookFx.CutFade(i / 20f, 1f), 1e-5f);
        }

        [Test]
        public void Cut04_StopsBeforeTheArcs_AndKeepsTheHeave()
        {
            // the frame runs on the old clock (k over the full life), so the heave's frames come at today's times
            float cut = FlipbookFx.ColumnPlayCut(0.4f);
            Assert.AreEqual(6.4f, cut * Frames, 1e-4f, "the last frame drawn: just past the arcs' first (6)");
            float kArc = 6f / Frames;   // the arcs' first frame
            Assert.Less(FlipbookFx.CutFade(kArc, cut), 0.1f, "the arcs' first frame is nearly gone");
            for (int f = 0; f <= 4; f++)
                Assert.AreEqual(1f, FlipbookFx.CutFade((float)f / Frames, cut), 1e-6f, "frame " + f + ": the heave is whole, as today");
            Assert.AreEqual(OldFade(4f / Frames), 1f, 1e-6f);
        }

        [Test]
        public void Add_StoresTheCut_AndNoCutByDefault()
        {
            var fx = new FlipbookFx();
            try
            {
                Assume.That(fx.Ready, "the flipbook shader and sheets load in the editor");
                fx.Add(FlipbookFx.Book.Column, Vector3.zero, 4f, OldLife);
                fx.Add(FlipbookFx.Book.Column, Vector3.zero, 4f, OldLife, cut: 0f);
                fx.Add(FlipbookFx.Book.Column, Vector3.zero, 4f, OldLife, cut: 1f);
                fx.Add(FlipbookFx.Book.Column, Vector3.zero, 4f, OldLife, cut: 0.4f);
                Assert.AreEqual(4, fx.Alive);
                Assert.IsTrue(fx.CardCut(0) == 0f, "the old Add: no cut");
                Assert.IsTrue(fx.CardCut(1) == 0f);
                Assert.IsTrue(fx.CardCut(2) == 0f, "a cut of 1 is no cut");
                Assert.AreEqual(0.4f, fx.CardCut(3), 1e-6f);
            }
            finally
            {
                // DestroyImmediate: Dispose's Object.Destroy is not allowed in edit mode
                var mats = (Material[])typeof(FlipbookFx).GetField("mats", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                foreach (var m in mats) if (m != null) Object.DestroyImmediate(m);
                var quad = (Mesh)typeof(FlipbookFx).GetField("quad", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                if (quad != null) Object.DestroyImmediate(quad);
            }
        }

        [Test]
        public void Source_OldLinesRunWithNoCut_AndOnlyTheNightDryColumnIsCut()
        {
            // the bit-identity guard: with Cut 0 the old removal and fade expressions run unchanged
            string fx = File.ReadAllText(Path.Combine(Application.dataPath, "_Project", "Presentation", "Camera", "FlipbookFx.cs"));
            StringAssert.Contains("if (now - c.Born > (c.Cut > 0f ? CardEnd(c.Life, c.Cut) : c.Life))", fx);
            StringAssert.Contains("float fade = c.Cut > 0f ? CutFade(k, c.Cut) : 1f - Mathf.SmoothStep(0f, 1f, (k - 0.65f) / 0.35f);", fx);
            StringAssert.Contains("Cut = cut > 0f && cut < 1f ? cut : 0f", fx);

            // CombatFx: a cut only for a dry column on a moonlit field (as C57), passed to the capped and old column Adds
            string combat = File.ReadAllText(Path.Combine(Application.dataPath, "_Project", "Presentation", "Camera", "CombatFx.cs"));
            StringAssert.Contains("float cut = !wet && FlipbookFx.MoonLit(SceneMood.Night, SceneTints.Now.MoltenLiquid) ? FlipbookFx.ColumnPlayCut(columnPlay) : 0f;", combat);
            StringAssert.Contains("columnPlay = FlipbookFx.ReadColumnPlay();", combat);
            Assert.AreEqual(2, Count(combat, "cut: cut"), "the capped and the old column; never the soil heave or another book");
        }

        static int Count(string s, string what)
        {
            int n = 0;
            for (int i = s.IndexOf(what); i >= 0; i = s.IndexOf(what, i + what.Length)) n++;
            return n;
        }
    }
}
