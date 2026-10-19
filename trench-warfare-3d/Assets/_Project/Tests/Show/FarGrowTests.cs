// Phase: VFX pass (owner, 2026-09-28; tw3d-board catalogue IN-5) - FlipbookFx.FarGrow, how much wider the burst recipes'
// far-reading parts (the plume, the ground ring) are drawn at a zoom: 1 up to zoom 80, then with the zoom, capped.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class FarGrowTests
    {
        [Test]
        public void One_UpToTheStandardViews()
        {
            foreach (float zoom in new[] { 0f, 6f, 30f, FlipbookFx.FarGrowFrom })
                Assert.AreEqual(1f, FlipbookFx.FarGrow(zoom), 1e-6f, "zoom " + zoom);
        }

        [Test]
        public void Grows_WithTheZoom_ThenStops()
        {
            float last = 1f;
            for (float zoom = FlipbookFx.FarGrowFrom; zoom <= 600f; zoom += 10f)
            {
                float g = FlipbookFx.FarGrow(zoom);
                Assert.GreaterOrEqual(g, last, "never shrinks as the camera pulls back (zoom " + zoom + ")");
                Assert.LessOrEqual(g, FlipbookFx.FarGrowMax, "capped (zoom " + zoom + ")");
                last = g;
            }
            Assert.AreEqual(FlipbookFx.FarGrowMax, FlipbookFx.FarGrow(600f), 1e-6f, "the overview is at the cap");
            Assert.AreEqual(1.5f, FlipbookFx.FarGrow(120f), 1e-6f, "zoom 120: half again as wide");
            Assert.AreEqual(FlipbookFx.FarGrowMax, FlipbookFx.FarGrow(240f), 1e-6f, "zoom 240 is already at the cap");
        }
        [Test]
        public void FarFire_BlendsInPastTheStandardView_AndThinsStably()
        {
            Assert.AreEqual(0f, FlipbookFx.FarFireBlend(30f), "close: the drawing as it is");
            Assert.AreEqual(0f, FlipbookFx.FarFireBlend(FlipbookFx.FarFireFrom), "the blend starts at FarFireFrom");
            Assert.AreEqual(1f, FlipbookFx.FarFireBlend(FlipbookFx.FarFireFull), "and is whole at FarFireFull");
            Assert.AreEqual(1f, FlipbookFx.FarKeepShare(0f), "no blend: every card kept");
            Assert.AreEqual(FlipbookFx.FarKeepMin, FlipbookFx.FarKeepShare(1f), 1e-6f);
            int kept = 0, n = 4000;
            for (int i = 0; i < n; i++)
            {
                float born = 10f + i * 0.0137f, life = 0.4f + (i % 7) * 0.05f;
                bool k = FlipbookFx.FarKeep(born, life, 0.4f);
                Assert.AreEqual(k, FlipbookFx.FarKeep(born, life, 0.4f), "a card is kept or not for its whole life");
                if (k) kept++;
                Assert.IsTrue(FlipbookFx.FarKeep(born, life, 1f), "share 1 keeps all");
            }
            Assert.That(kept / (float)n, Is.InRange(0.35f, 0.45f), "about the share asked for");
        }
        [Test]
        public void Smoke_WeightAndLeanOfOne_IsTheOldSmoke_TheDefaultsLighterAndLeaner()
        {
            float w = 3f, h = 0f, life = 5f, alpha = 0.8f;
            FlipbookFx.ShapeSmoke(1f, 1f, 1.25f, ref w, ref h, ref life, ref alpha);
            Assert.AreEqual(3f, w); Assert.AreEqual(3f / 1.25f, h, 1e-6f, "the drawing's own aspect"); Assert.AreEqual(5f, life); Assert.AreEqual(0.8f, alpha);
            w = 3f; h = 0f; life = 5f; alpha = 0.8f;
            FlipbookFx.ShapeSmoke(FlipbookFx.DefaultSmokeWeight, FlipbookFx.DefaultSmokeLean, 1.25f, ref w, ref h, ref life, ref alpha);
            Assert.AreEqual(3f / 1.25f, h, 1e-6f, "as tall as it was");
            Assert.Less(w, 3f, "leaner"); Assert.Less(alpha, 0.8f, "lighter"); Assert.Less(life, 5f, "clears sooner");
            Assert.GreaterOrEqual(life, 5f * 0.6f, "never popping out in under 60 % of its time");
            Assert.IsTrue(FlipbookFx.IsSmoke(FlipbookFx.Book.Smoke) && FlipbookFx.IsSmoke(FlipbookFx.Book.ShellPlume) && FlipbookFx.IsSmoke(FlipbookFx.Book.WreckSmoke) && FlipbookFx.IsSmoke(FlipbookFx.Book.Burst));
            Assert.IsFalse(FlipbookFx.IsSmoke(FlipbookFx.Book.GasBank) || FlipbookFx.IsSmoke(FlipbookFx.Book.SmokeBank) || FlipbookFx.IsSmoke(FlipbookFx.Book.DustPuff), "the field's gas and screen, and grit, are not a fire's smoke");
        }

        [Test]
        public void DropInboundMarksTheZone()
        {
            // the drop survives the zoom that culls a shell's streak (harsh critic: "men just appear" at the overview)
            Assert.GreaterOrEqual(240f, CombatFx.IncomingFarZoom);
            Assert.IsTrue(CombatFx.DropDrawnAt(240f));

            const float seconds = 5f;
            int waves = CombatFx.DropStreakWaves(seconds);
            float prev = -1f;
            for (int w = 0; w < waves; w++)
            {
                float d = CombatFx.DropStreakDelay(w, waves, seconds);
                Assert.That(d, Is.InRange(0f, seconds));
                Assert.Greater(d, prev, "wave " + w + " starts after the last");
                prev = d;
            }
            Assert.AreEqual(seconds - CombatFx.IncomingLead, CombatFx.DropStreakDelay(waves - 1, waves, seconds), 1e-6f, "the last wave ends as the shell's streak would");
        }

        [Test]
        public void DropStreaksOnePerMan()
        {
            // r2 critique fix 3: the falling streaks could not be seen in any still (men 0 in frame) because each
            // man got only ONE 1 s card staggered across the whole flight, so at any instant 1-2 of 8 were up.
            // Every man now streaks unbroken over the last DropStreakWindow seconds, so the goal's capture moment
            // (2.5 s after the call, on a 5 s flight) shows all 8 at once, and nothing falls at the call itself
            // (the flare does that job).
            const int men = 8;
            const float seconds = 5f;
            Assert.AreEqual(8, CombatFx.DropStreaksUp(2.5f, men, seconds), "one streak per man at the capture moment");
            Assert.AreEqual(0, CombatFx.DropStreaksUp(0.2f, men, seconds), "nothing falls at the call");

            int waves = CombatFx.DropStreakWaves(seconds);
            Assert.AreEqual(seconds - CombatFx.IncomingLead, CombatFx.DropStreakDelay(waves - 1, waves, seconds), 1e-6f, "the last wave ends as the shell's own streak would");

            // the 8 ring points (k/men*2pi, alternating DropStreakRingNear/Far, no jitter) are each separated enough
            // that a 4.5 m-wide card at zoom 120 (3 * FarGrow(120)) cannot blob into its neighbour.
            var pts = new Vector3[men];
            for (int k = 0; k < men; k++)
            {
                float a = (float)k / men * 6.2832f;
                float d = (k & 1) == 0 ? CombatFx.DropStreakRingNear : CombatFx.DropStreakRingFar;
                pts[k] = new Vector3(Mathf.Cos(a) * d, 0f, Mathf.Sin(a) * d);
            }
            float min = float.MaxValue;
            for (int i = 0; i < men; i++)
                for (int j = i + 1; j < men; j++)
                    min = Mathf.Min(min, Vector3.Distance(pts[i], pts[j]));
            Assert.GreaterOrEqual(min, 6f, "farther apart than a card is wide at zoom 120");
        }

        [Test]
        public void DropLandingDustIsSeenAt240()
        {
            // the r2 critique (score 16/100): r1's dust probe measured a camp lamp going dark 570 px from the
            // drop, under a hand-found window, rather than the puff. The fix measures from CaptureRig.Pose's own
            // arithmetic: distance = zoom * tan(30deg) / tan(fov/2), fov = TacticalCamera.Fov = 25deg, so the view
            // height at the focus is 2 * distance * tan(fov/2) = 2 * zoom * tan(30deg) = 1.1547 * zoom metres —
            // 277.1 m at zoom 240 over the default 900-px-high still, which is 3.25 px/m (not r1's 3.75, which
            // assumed the view height equals the zoom). docs/evidence/paradrop-inbound-marker-r4/streakprobe.py
            // measures the stills at this exact pose, anchoring the number to a picture, not the formula.
            float viewHeightM = 2f * 240f * Mathf.Tan(30f * Mathf.Deg2Rad);
            float pxPerM = 900f / viewHeightM;
            Assert.AreEqual(2f, CombatFx.DropDustWidth(30f), 1e-6f, "the standard view is untouched");
            Assert.GreaterOrEqual(CombatFx.DropDustWidth(240f) * pxPerM, 12f, "readable landing dust at zoom 240");
            Assert.IsTrue(CombatFx.DropDrawnAt(240f));
        }
    }
}
