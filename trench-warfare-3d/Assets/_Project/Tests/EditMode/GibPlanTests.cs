// Phase: deaths (2026-09-28, implemented) — what a shell takes off a man at fx.deathAbsurd above 0 (GibPlan, thrown by
// CombatFx.OwnGibs): at 0 nothing is decided there and today's dice are thrown; what flies is exactly what the corpse
// lost; torn in two or blown apart at most half the time in a heap of four or more, far less alone, never at low GORE;
// at GORE 0 only his kit flies; the same death comes apart the same way every time.
using System.Collections.Generic;
using NUnit.Framework;
using TW.Presentation.Tactical;
using Piece = TW.Presentation.Tactical.DebrisRenderer.Piece;

namespace TW.Tests
{
    public class GibPlanTests
    {
        static uint Seed(int k) { unchecked { return (uint)k * 2654435761u + 12345u; } }

        [Test]
        public void AtIntensityZeroTheLegacyGibsThrow()
        {
            var pieces = new List<Piece>();
            for (int k = 0; k < 200; k++)
            {
                var plan = GibPlan.Decide(Seed(k), 0f, 1f, k % 5);
                Assert.IsTrue(plan.Legacy, "nothing decided here: CombatFx.Gibs throws today's dice");
                Assert.IsFalse(plan.Whole);
                GibPlan.Pieces(plan, pieces);
                Assert.AreEqual(0, pieces.Count);
            }
        }

        [Test]
        public void WhatFliesIsWhatTheCorpseLost()
        {
            var pieces = new List<Piece>();
            int torn = 0, apart = 0, heads = 0;
            for (int k = 0; k < 4000; k++)
            {
                var plan = GibPlan.Decide(Seed(k), 1f + (k % 3) * 0.5f, 1f, k % 5);
                GibPlan.Pieces(plan, pieces);
                int arms = 0, legs = 0, head = 0, halves = 0;
                foreach (var p in pieces)
                {
                    if (p == Piece.Arm) arms++;
                    else if (p == Piece.Leg) legs++;
                    else if (p == Piece.Head) head++;
                    else if (p == Piece.UpperHalf || p == Piece.LowerHalf) halves++;
                }
                int mask = plan.Mask;
                Assert.AreEqual(Bit(mask, 2) + Bit(mask, 3), arms, "an arm flies for each arm the corpse lost");
                Assert.AreEqual(Bit(mask, 4) + Bit(mask, 5), legs, "a leg for each leg");
                Assert.AreEqual(Bit(mask, 1), head, "the head if it lost its head");
                Assert.AreEqual(0, mask & ~GibPlan.AllLimbs, "the mask is the VAT shader's limbs, nothing else");
                if (plan.Torn)
                {
                    torn++;
                    Assert.AreEqual(2, halves, "torn in two: both halves fly");
                    Assert.AreEqual(0, mask, "and there is no corpse to lose limbs");
                    Assert.IsFalse(plan.Helmet, "his helmet is on his upper half");
                }
                else Assert.AreEqual(0, halves);
                if (plan.Apart) { apart++; Assert.AreEqual(GibPlan.AllLimbs, mask, "blown apart: every limb"); }
                if ((mask & GibPlan.Head) != 0) heads++;
                if (!plan.Whole && !plan.Torn) Assert.IsTrue(plan.Helmet, "a man the shell takes apart loses his helmet");
            }
            Assert.Greater(torn, 0, "some are torn in two"); Assert.Greater(apart, 0, "some blown apart"); Assert.Greater(heads, 0);
        }

        static int Bit(int mask, int bit) => (mask >> bit) & 1;

        [Test]
        public void TornOrBlownApartAtMostHalfTheTimeInAHeapAndRarelyAlone()
        {
            foreach (float a in new[] { 1f, 2f })
            {
                int heap = 0, alone = 0; const int n = 20000;
                for (int k = 0; k < n; k++)
                {
                    var h = GibPlan.Decide(Seed(k), a, 1f, 4);
                    if (h.Torn || h.Apart) heap++;
                    var o = GibPlan.Decide(Seed(k), a, 1f, 0);
                    if (o.Torn || o.Apart) alone++;
                }
                Assert.LessOrEqual(heap / (float)n, GibPlan.HeapBurstCap + 0.01f, $"at {a}: a heap of four or more");
                Assert.Greater(heap / (float)n, 0.15f, $"at {a}: but it happens in a heap");
                Assert.LessOrEqual(alone / (float)n, GibPlan.AloneBurstCap + 0.01f, $"at {a}: alone");
                Assert.Less(alone, heap, "rarer alone than in a heap");
            }
        }

        [Test]
        public void GoreTurnsHimDownToHisKit()
        {
            var pieces = new List<Piece>();
            int kit = 0;
            for (int k = 0; k < 2000; k++)
            {
                var none = GibPlan.Decide(Seed(k), 2f, 0f, 4);
                Assert.AreEqual(0, none.Mask, "GORE 0: no part of him");
                Assert.IsFalse(none.Torn || none.Apart);
                Assert.AreEqual(0, none.Lumps);
                GibPlan.Pieces(none, pieces);
                foreach (var p in pieces) Assert.IsTrue(p == Piece.Helm || p == Piece.Rifle || p == Piece.Pack, $"{p} is not kit");
                if (pieces.Count > 0) kit++;
                var low = GibPlan.Decide(Seed(k), 2f, 0.3f, 4);
                Assert.IsFalse(low.Torn || low.Apart, "low GORE: never torn or blown apart");
                Assert.AreEqual(0, low.Mask & GibPlan.Head, "nor his head");
                int limbs = 0; for (int b = 2; b <= 5; b++) limbs += Bit(low.Mask, b);
                Assert.LessOrEqual(limbs, 1, "one limb at most");
            }
            Assert.Greater(kit, 1000, "his helmet, rifle and pack still fly at GORE 0");
        }

        [Test]
        public void TheSameDeathComesApartTheSameWay()
        {
            for (int k = 0; k < 500; k++)
            {
                var a = GibPlan.Decide(Seed(k), 1f, 1f, k % 5);
                var b = GibPlan.Decide(Seed(k), 1f, 1f, k % 5);
                Assert.AreEqual(a.Mask, b.Mask); Assert.AreEqual(a.Torn, b.Torn); Assert.AreEqual(a.Apart, b.Apart);
                Assert.AreEqual(a.Helmet, b.Helmet); Assert.AreEqual(a.Rifle, b.Rifle); Assert.AreEqual(a.Pack, b.Pack); Assert.AreEqual(a.Lumps, b.Lumps);
            }
        }
    }
}
