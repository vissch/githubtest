// Phase: tracers-side-by-side (2026-10-15) - the owner's default for "night tracers: inside the smoke or on top" is
// today's beams until he has seen both side by side (decisions.md, 2026-10-08). fx.tracerInSmoke stays off by
// default: this only proves that off is off and that both paths still draw. No production code changes.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class TracerSideBySideTests
    {
        [SetUp] public void SetUp() => Knobs.Clear();
        [TearDown] public void TearDown() => Knobs.Clear();

        [Test]
        public void Unset_TheSwitchIsOff_AndTheGameLooksAsToday()
        {
            Assert.AreEqual("fx.tracerInSmoke", TracerLook.Knob);
            Assert.AreEqual(0f, TracerLook.Read());
            Assert.IsFalse(TracerLook.On(TracerLook.Read()));
            Assert.AreEqual("0", Knobs.Read["fx.tracerInSmoke"]);
            Assert.AreEqual(TracerLook.DefaultInSmoke, TracerLook.OldInSmoke);
        }

        [Test]
        public void BothPaths_DrawAShapeThatIsNotDegenerate_ForEverySide()
        {
            var from = new Vector3(10f, 1.2f, 20f); var to = new Vector3(60f, 1.0f, 80f);
            Vector3 d = to - from; float len = d.magnitude;
            foreach (bool inSmoke in new[] { false, true })
                for (int side = 0; side < 3; side++)
                {
                    var m = TracerLook.Matrix(from, d, len, 0.4f, true, side, 0f, inSmoke);
                    Vector3 scale = m.lossyScale;
                    Assert.Greater(scale.x, 0f, $"inSmoke {inSmoke} side {side} x");
                    Assert.Greater(scale.y, 0f, $"inSmoke {inSmoke} side {side} y");
                    Assert.Greater(scale.z, 0f, $"inSmoke {inSmoke} side {side} z");
                }
        }

        [Test]
        public void TheTwoOrders_DrawDifferentShapes_ForSides0And1()
        {
            var from = new Vector3(10f, 1.2f, 20f); var to = new Vector3(60f, 1.0f, 80f);
            Vector3 d = to - from; float len = d.magnitude;
            for (int side = 0; side < 2; side++)
            {
                var today = TracerLook.Matrix(from, d, len, 0.4f, true, side, 0f, false);
                var insmoke = TracerLook.Matrix(from, d, len, 0.4f, true, side, 0f, true);
                Assert.IsFalse(today.Equals(insmoke), "side " + side);
            }
        }

        [Test]
        public void TheQueuesAndDepthWrite_MatchTheHeader_AndStayInsideUnitysRange()
        {
            Assert.AreEqual(3100, TracerLook.HaloQueue(false));
            Assert.AreEqual(3005, TracerLook.HaloQueue(true));
            Assert.That(TracerLook.HaloQueue(false), Is.InRange(0, 5000));
            Assert.That(TracerLook.HaloQueue(true), Is.InRange(0, 5000));
            Assert.AreEqual(0f, TracerLook.HaloZWrite(false, 1f));
            Assert.AreEqual(1f, TracerLook.HaloZWrite(true, 1f));
        }

        [Test]
        public void SetOn_TheSecondPathIsReachable_WithC104sNightShapeNumbers()
        {
            Knobs.Set(TracerLook.Knob, "1");
            Assert.AreEqual(1f, TracerLook.Read());
            Assert.IsTrue(TracerLook.On(TracerLook.Read()));
            Assert.AreEqual(7f, TracerLook.NightStreak, "the header's streak: 7 m, not 10");
            Assert.AreEqual(0.15f, TracerLook.NightHalo, "the header's halo: 0.15 m wide, not 0.24");
        }
    }
}
