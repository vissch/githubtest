// Phase: gun-fire effects (owner, 2026-10-14, option A: muzzle flash, muzzle smoke, bullet impact puff - 166 of 226
// effects fired in four match minutes). These guard the measurable half of the in-game review's faults: the day flash
// must be drawn from the premultiplied gun book at the standard view and big enough to read there, twenty men firing
// in one frame must not be twenty copies of one cel, and the clouds' lives and sizes must stay in the range the review
// asked for. The look itself (does the flash read on snow) no test can prove: that is the films.
// Two asserts here are RED on the code before the pass: DayFlare at closeUp 0 (DayFlareCloseUp was 0.3) and the rifle
// cone's standard size (MuzzleScale returned 1 at closeUp 0).
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class GunFxTests
    {
        const int Shooters = 20;      // the films' firing line: twenty riflemen in one frame
        const float RifleFlareLife = 0.18f;   // CombatFx.ArmsLook.Rifle
        const float SheetFps = 12f;   // the MuzzleRifle row (FlipbookFx.Sheets)
        const int SheetFrames = 9;    // the same row: 4 x 4 cells, 9 live frames

        bool wasNight;
        [SetUp] public void SetUp() { wasNight = SceneMood.Night; SceneMood.Night = false; }
        [TearDown] public void TearDown() { SceneMood.Night = wasNight; }

        [Test]
        public void DayFlare_IsDrawnFromTheGunBook_AtTheStandardView()
        {
            Assert.AreEqual(0f, CombatFx.DayFlareCloseUp, "the day gun book is on at every zoom, not only among the men");
            Assert.IsTrue(CombatFx.DayFlare(false, 0f, true), "at zoom 30 the additive Muzzle card measured 0.28 % warm on snow");
            Assert.IsTrue(CombatFx.DayFlare(false, 1f, true), "and up close as before");
            Assert.IsFalse(CombatFx.DayFlare(true, 0f, true), "at night the additive card still carries the flash");
            Assert.IsFalse(CombatFx.DayFlare(false, 0f, false), "fx.classArms 0 keeps the old look whole");
        }

        [Test]
        public void RifleCone_ReadsAtTheStandardView_AndStillGrowsUpClose()
        {
            float standard = CombatFx.MuzzleScale(FlipbookFx.Book.MuzzleRifle, 0f);
            Assert.GreaterOrEqual(standard, 1.3f, "a plain-sized premultiplied cone is a 2-3 px speck at zoom 30");
            Assert.LessOrEqual(standard, 1.6f, "and over half a man a firing line becomes a wall of flame");
            Assert.Greater(CombatFx.MuzzleScale(FlipbookFx.Book.MuzzleRifle, 1f), standard, "it still grows among the men");
            Assert.AreEqual(1f, CombatFx.MuzzleScale(FlipbookFx.Book.MuzzleStream, 0f), 1e-6f, "no class book is resized at the standard view");
        }

        [Test]
        public void TwentyShootersInOneFrame_ShowDifferentFlashes()
        {
            foreach (uint tick in new uint[] { 0u, 1u, 7u, 100u, 12345u })
            {
                var seen = new HashSet<string>();
                var cels = new HashSet<int>();
                var all = new CombatFx.MuzzleVariantLook[Shooters];
                for (int i = 0; i < Shooters; i++)
                {
                    all[i] = CombatFx.MuzzleVariant(tick, i);
                    seen.Add(string.Format("{0:0.0000}|{1:0.0000}|{2:0.0000}|{3}", all[i].Start, all[i].Roll, all[i].Scale, all[i].Mirror));
                    cels.Add(Mathf.FloorToInt(all[i].Start));
                }
                Assert.GreaterOrEqual(seen.Count, 15, "tick " + tick + ": twenty men firing drew one copied quad before this pass");
                Assert.GreaterOrEqual(cels.Count, 3, "tick " + tick + ": at least three different cels of the sheet are on screen");
                for (int i = 0; i + 1 < Shooters; i++)
                    Assert.AreNotEqual(
                        string.Format("{0:0.0000}|{1:0.0000}", all[i].Start, all[i].Roll),
                        string.Format("{0:0.0000}|{1:0.0000}", all[i + 1].Start, all[i + 1].Roll),
                        "tick " + tick + ": neighbours " + i + " and " + (i + 1) + " are the same flash");
            }
        }

        [Test]
        public void EveryFlashStaysInsideTheLivePartOfItsSheet()
        {
            // the row's span: Frames * Play - 1.001 with Play 0 (= all), so 7.999. A flare of 0.18 s at Fps 12 spends
            // 2.16 cels, so the start may not pass 5.839.
            const float span = SheetFrames * 1f - 1.001f;
            float spent = RifleFlareLife * SheetFps;
            for (uint tick = 0u; tick < 64u; tick++)
                for (int i = 0; i < Shooters; i++)
                {
                    var v = CombatFx.MuzzleVariant(tick, i);
                    Assert.GreaterOrEqual(v.Start, 0f);
                    Assert.LessOrEqual(v.Start, CombatFx.MuzzleStartCels);
                    Assert.Less(v.Start + spent, span, "a flash that starts this late runs off the end of the sheet");
                    Assert.GreaterOrEqual(v.Scale, 0.85f); Assert.LessOrEqual(v.Scale, 1.25f);
                    Assert.LessOrEqual(Mathf.Abs(v.Roll), 0.25f, "the roll jitters about the barrel, never along it");
                }
            Assert.AreEqual(3f, CombatFx.MuzzleStartCels, 1e-6f);
        }

        [Test]
        public void MuzzleSmoke_IsShortAndThin_BothLooks()
        {
            foreach (bool night in new[] { false, true })
                foreach (float closeUp in new[] { 0f, 0.5f, 1f })
                {
                    var s = CombatFx.MuzzleSmoke(night, closeUp);
                    Assert.GreaterOrEqual(s.Life, 0.3f, "short, the review's word");
                    Assert.LessOrEqual(s.Life, 0.7f, "a muzzle cloud that hangs becomes a smoke bank in a firefight");
                    Assert.LessOrEqual(s.Alpha, 0.5f, "thin: grey-white, not a wall");
                    Assert.Greater(s.Alpha, 0f);
                    Assert.Greater(s.Wide, 0f); Assert.LessOrEqual(s.Wide, 1.5f);
                }
            Assert.Less(CombatFx.MuzzleSmoke(true, 0f).Alpha, CombatFx.MuzzleSmoke(false, 0f).Alpha,
                "the owner's rule: smoke after dark is thinner, never a black patch");
        }

        [Test]
        public void ImpactPuff_IsAQuarterSecondOfDust_AndEveryHitDiffers()
        {
            foreach (uint tick in new uint[] { 0u, 3u, 77u, 4242u })
            {
                var seen = new HashSet<string>();
                for (int i = 0; i < Shooters; i++)
                {
                    var d = CombatFx.ImpactPuff(tick, i);
                    Assert.GreaterOrEqual(d.Life, 0.2f); Assert.LessOrEqual(d.Life, 0.35f, "the review asked for about 0.25 s");
                    Assert.GreaterOrEqual(d.Wide, 0.4f); Assert.LessOrEqual(d.Wide, 1.2f, "under a man wide");
                    Assert.GreaterOrEqual(d.Roll, 0f); Assert.LessOrEqual(d.Roll, 6.2832f);
                    seen.Add(string.Format("{0:0.0000}|{1:0.0000}", d.Roll, d.Wide));
                }
                Assert.GreaterOrEqual(seen.Count, 15, "tick " + tick + ": twenty hits, twenty different clouds");
            }
        }

        [Test]
        public void TheLookIsAFactOfTheTickAndTheMan_NotADraw()
        {
            // it must not take from UnityEngine.Random: the Shot and Hit paths share that stream with the tracers,
            // cases and chunks, so a new draw here would move every unrelated effect downstream.
            for (uint tick = 0u; tick < 8u; tick++)
                for (int i = 0; i < 8; i++)
                {
                    var a = CombatFx.MuzzleVariant(tick, i);
                    float moved = UnityEngine.Random.value;   // the shared stream moves on
                    var b = CombatFx.MuzzleVariant(tick, i);
                    Assert.GreaterOrEqual(moved, 0f);
                    Assert.AreEqual(a.Start, b.Start, 1e-6f);
                    Assert.AreEqual(a.Roll, b.Roll, 1e-6f);
                    Assert.AreEqual(a.Scale, b.Scale, 1e-6f);
                    Assert.AreEqual(a.Mirror, b.Mirror);
                    var c = CombatFx.ImpactPuff(tick, i); var d = CombatFx.ImpactPuff(tick, i);
                    Assert.AreEqual(c.Roll, d.Roll, 1e-6f);
                    Assert.AreEqual(c.Wide, d.Wide, 1e-6f);
                    Assert.AreEqual(c.Life, d.Life, 1e-6f);
                }
        }
    }
}
