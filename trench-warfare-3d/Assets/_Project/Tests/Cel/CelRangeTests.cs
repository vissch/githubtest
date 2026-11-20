// Phase: CelFx Range (2026-11-14) - pre-roll starts a lead-in without moving the main timeline or immediate LOD shots.
using System;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.VFX;

namespace TW.Tests
{
    public sealed class CelRangeTests
    {
        Func<float, float, int> tick;

        [SetUp]
        public void SetUp()
        {
            CelRange.Begin(Vector3.zero, Vector3.zero, Vector3.one, Vector3.zero);
            var method = typeof(CelRange).GetMethod("TickAt", BindingFlags.NonPublic | BindingFlags.Static);
            Assert.That(method, Is.Not.Null);
            tick = (Func<float, float, int>)Delegate.CreateDelegate(typeof(Func<float, float, int>), method);
        }

        [TearDown]
        public void TearDown() => CelRange.Clear();

        [Test]
        public void DefaultEffectFiresAtT0OnceAndUpdatesAfterwards()
        {
            int fires = 0, updates = 0;
            float elapsed = -1f;
            CelRange.Register("__range_test_default", s => fires++, (s, dt) => { updates++; elapsed = dt; });
            var shot = CelRange.Fire("__range_test_default", 0.8f);
            Assert.That(shot.Preroll, Is.Zero);
            Assert.That(tick(0.79f, 10.79f), Is.Zero);
            Assert.That(fires, Is.Zero);
            Assert.That(tick(0.8f, 10.8f), Is.EqualTo(1));
            Assert.That(fires, Is.EqualTo(1));
            Assert.That(updates, Is.Zero);
            tick(0.9f, 10.9f);
            Assert.That(fires, Is.EqualTo(1));
            Assert.That(updates, Is.EqualTo(1));
            Assert.That(elapsed, Is.EqualTo(0.1f).Within(1e-5f));
        }

        [CelRangeEffect("__range_test_attribute", 0.25f)]
        static void AttributeFire(RangeShot shot) => shot.State = "fired";

        [Test]
        public void AttributePrerollStartsLeadInAndKeepsTheMainTimeline()
        {
            var shot = CelRange.Fire("__range_test_attribute", 0.8f);
            Assert.That(shot, Is.Not.Null, "attribute registration is discovered");
            Assert.That(shot.Preroll, Is.EqualTo(0.25f));
            Assert.That(tick(0.54f, 10.54f), Is.Zero);
            Assert.That(shot.State, Is.Null);
            Assert.That(tick(0.55f, 10.55f), Is.EqualTo(1));
            Assert.That(shot.State, Is.EqualTo("fired"));
            Assert.That(shot.T0, Is.EqualTo(0.8f), "the capture's main timeline does not move");
            Assert.That(shot.FiredAt + shot.Preroll, Is.EqualTo(10.8f).Within(1e-5f));
        }

        [TestCase(0f, 0f)]
        [TestCase(0.1f, 0.1f)]
        [TestCase(0.8f, 0.25f)]
        public void PrerollUsesOnlyTimeAvailableBeforeT0(float t0, float expected)
        {
            CelRange.Register("__range_test_available", s => { }, preroll: 0.25f);
            var shot = CelRange.Fire("__range_test_available", t0);
            Assert.That(shot.Preroll, Is.EqualTo(expected));
            Assert.That(tick(t0 - expected, 20f + t0 - expected), Is.EqualTo(1));
            Assert.That(shot.FiredAt + shot.Preroll, Is.EqualTo(20f + t0).Within(1e-5f));
        }

        [Test]
        public void AddingUpdatesFromFirePreservesPrerollAndNeverRefires()
        {
            int fires = 0, updates = 0;
            float elapsed = -1f;
            Action<RangeShot> fire = null;
            Action<RangeShot, float> update = (s, dt) => { updates++; elapsed = dt; };
            fire = s => { fires++; CelRange.Register("__range_test_replace", fire, update); };
            CelRange.Register("__range_test_replace", fire, preroll: 0.25f);
            var first = CelRange.Fire("__range_test_replace", 0.8f);
            tick(0.55f, 10.55f);
            tick(0.8f, 10.8f);
            Assert.That(fires, Is.EqualTo(1));
            Assert.That(updates, Is.EqualTo(1));
            Assert.That(elapsed, Is.EqualTo(first.Preroll).Within(1e-5f));
            var next = CelRange.Fire("__range_test_replace", 1.8f);
            Assert.That(next.Preroll, Is.EqualTo(0.25f));
            tick(1.55f, 11.55f);
            Assert.That(fires, Is.EqualTo(2));
            tick(1.8f, 11.8f);
            Assert.That(fires, Is.EqualTo(2));
        }

        [Test]
        public void TickingAPrerollEffectAllocatesNothing()
        {
            CelRange.Register("__range_test_alloc", s => { }, (s, dt) => { }, 0.25f);
            CelRange.Fire("__range_test_alloc", 0.8f);
            tick(0.55f, 10.55f);
            tick(0.8f, 10.8f);
            long before = GC.GetAllocatedBytesForCurrentThread();
            for (int i = 0; i < 200; i++) tick(0.9f, 10.9f);
            long bytes = GC.GetAllocatedBytesForCurrentThread() - before;
            Assert.That(bytes, Is.Zero, "pre-roll adds no work allocations to a frame");
        }
    }
}
