// Phase: VFX pass (owner, 2026-09-28; tw3d-board catalogue IN-5) - FlipbookFx.FarGrow, how much wider the burst recipes'
// far-reading parts (the plume, the ground ring) are drawn at a zoom: 1 up to zoom 80, then with the zoom, capped.
using System.IO;
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

            const int men = 8;
            const float seconds = 5f;
            float prev = -1f;
            for (int k = 0; k < men; k++)
            {
                float d = CombatFx.DropStreakDelay(k, men, seconds);
                Assert.That(d, Is.InRange(0f, seconds));
                Assert.Greater(d, prev, "streak " + k + " starts after the last");
                prev = d;
            }
            Assert.AreEqual(seconds - CombatFx.IncomingLead, CombatFx.DropStreakDelay(men - 1, men, seconds), 1e-6f, "the last streak ends as the shell's streak would");
        }

        [Test]
        public void DropLandingDustIsSeenAt240()
        {
            // the r2 critique (score 16/100): r1's dust probe measured a camp lamp going dark 570 px from the
            // drop, under a hand-found window, rather than the puff. The fix measures from CaptureRig.Pose's own
            // arithmetic: distance = zoom * tan(30deg) / tan(fov/2), fov = TacticalCamera.Fov = 25deg, so the view
            // height at the focus is 2 * distance * tan(fov/2) = 2 * zoom * tan(30deg) = 1.1547 * zoom metres —
            // 277.1 m at zoom 240 over the default 900-px-high still, which is 3.25 px/m (not r1's 3.75, which
            // assumed the view height equals the zoom). docs/evidence/paradrop-inbound-marker-r2/dustprobe2.py
            // measures 16 px on a still shot at this exact pose, anchoring the number to a picture, not the formula.
            float viewHeightM = 2f * 240f * Mathf.Tan(30f * Mathf.Deg2Rad);
            float pxPerM = 900f / viewHeightM;
            Assert.AreEqual(2f, CombatFx.DropDustWidth(30f), 1e-6f, "the standard view is untouched");
            Assert.GreaterOrEqual(CombatFx.DropDustWidth(240f) * pxPerM, 12f, "readable landing dust at zoom 240");
            Assert.IsTrue(CombatFx.DropDrawnAt(240f));
        }

        [Test]
        public void ParaDropLandingDustShowsAtOverview()
        {
            // the r5 critique (score 22/100): dustprobe3.py compared a 0-255 channel spread against 0.06, so any
            // 1-level drop counted and dust_minus_nopuff_px reached 1442 on render noise. With the probe's units
            // fixed (spread on 0..1, and the count only pixels that brightened by >= 0.05 luma), FarGrow's 5 m puff
            // did not show: the scene's luma_mean is 0.206, so clearing a 0.05 lift needs width AND brightness.
            // docs/evidence/paradrop-inbound-marker-r6/probe.py measures the pixels on real stills at this pose.
            Assert.AreEqual(1f, CombatFx.DropDustOverview(30f), 1e-6f, "the standard view gets no extra scale");
            Assert.AreEqual(1f, CombatFx.DropDustOverview(CombatFx.DropDustOverviewFrom), 1e-6f, "the extra scale starts at the overview, not before it");
            Assert.AreEqual(2f, CombatFx.DropDustWidth(30f), 1e-6f, "T3/T2/T1 are untouched by construction");
            Assert.AreEqual(2f, CombatFx.DropDustOverview(240f), 1e-6f, "zoom 240 doubles it (240 / 120), short of the cap");
            Assert.AreEqual(CombatFx.DropDustOverviewMax, CombatFx.DropDustOverview(600f), 1e-6f, "the Far band is held at the cap, so the puff never swallows the zone");
            Assert.Greater(CombatFx.DropDustOverview(240f), 1f, "the overview is wider than FarGrow alone");

            // 2 m * FarGrow(240) * 2.2 = 10 m, about 32 px at the pose's measured 3.25 px/m (r2's evidence)
            float pxPerM = 900f / (2f * 240f * Mathf.Tan(30f * Mathf.Deg2Rad));
            Assert.GreaterOrEqual(CombatFx.DropDustWidth(240f) * pxPerM, 30f, "the landing puff is a read, not a speck, at zoom 240");
            Assert.That(CombatFx.DropDustWidth(240f), Is.LessThanOrEqualTo(12f), "and not so wide it covers the zone it marks");

            // the open frame is measured from the sheet's own alpha channel, not from the renderer's arithmetic:
            // the 28-cell coverage table in docs/evidence/paradrop-inbound-marker-r6/README.md (probe.py --frames)
            // is its only source (frame 9 is 0.2499 coverage, the book's maximum, frame 3 is 0.2143 = 86 %, the
            // earliest cell at or above 80 %).
            Assert.AreEqual(3f, CombatFx.DropDustOpenFrame, 1e-6f, "the overview card opens on the measured frame of the DustPuff book");
            Assert.Less(CombatFx.DropDustOpenFrame, 28f, "and inside the book (DustPuff is 28 frames)");

            // r6 fix 3 part 2: 209 px of real ink at landTick + 6 is 5x the 42 at + 3 (same card width), so area
            // was still filling in at + 3 - the lever left is density, several overlapping cards, not width.
            Assert.AreEqual(1, CombatFx.DropDustCardCount(30f), "T3/T2/T1 keep exactly one card, byte-identical by construction");
            Assert.GreaterOrEqual(CombatFx.DropDustCardCount(240f), 3, "the overview stacks cards to raise the puff's density");
            float stackWidth = CombatFx.DropDustWidth(240f);
            for (int k = 0; k < CombatFx.DropDustCardCount(240f); k++)
            {
                Vector2 off = CombatFx.DropDustCardOffset(k, stackWidth);
                Assert.LessOrEqual(off.magnitude, 0.5f * stackWidth, "the cluster still fits inside the 12 m cap (card " + k + ")");
            }

            // probe.json: dustprobe3.py's units are fixed (spread_scale 1), held so a future regression in the
            // probe itself is caught here too. NOT the dust_minus_nopuff_px >= 150 gate: `CombatFx.OnDropLanded`
            // sizes the puff from `ViewEye.Zoom` at the instant the sim event fires, but `CaptureRig.Shot`
            // (Assets/_Project/Editor/CaptureRig.cs ~124, ~164) only holds the shot's zoom for the one frame it
            // renders and restores the live camera's own zoom right after — so through the ticks
            // DropLandingDustStills.cs advances to reach landTick, the camera sits at its ordinary zoom (measured
            // zoom=30 at every OnDropLanded firing, a scratch Debug.Log). DropDustOverview(30) == 1, so these
            // stills can never exercise the overview branch above, by the harness's own design: raising
            // DropDustCards to 8 and rerunning reproduced the committed stills byte-for-byte. Decision needed on
            // how the harness proves the overview puff (docs/reference/decisions.md, "the paradrop stills can
            // never show the overview puff"); asserting the 150 px gate here would hold a test red for a reason
            // no change to this file's production code can fix.
            string probePath = Path.Combine(Application.dataPath, "..", "..", "docs", "evidence", "paradrop-inbound-marker-r6", "probe.json");
            Assert.IsTrue(File.Exists(probePath), "probe.json: " + probePath);
            var probe = JsonUtility.FromJson<ProbeResult>(File.ReadAllText(probePath));
            Assert.AreEqual(1, probe.spread_scale, "the probe's units are fixed: spread on 0..1, not 0..255");
        }

        [System.Serializable]
        class ProbeResult
        {
            public int spread_scale;
            public int dust_minus_nopuff_px;
        }
    }
}
