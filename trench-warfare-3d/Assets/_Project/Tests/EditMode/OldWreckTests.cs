// Phase: SHOW (2026-10-01) — the wrecks a map starts with are drawn as burnt hulls (TankRenderer.OldWrecks), not
// as the four dark cubes the field stood in for them. How each one lies is hashed off its prop: the same on every
// machine and every visit, a tilt a tank could come to rest at, and never so deep that the hull is gone.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class OldWreckTests
    {
        [Test]
        public void AnOldWreckLiesTheSameWayEveryTime_AtATiltATankCouldRestAt()
        {
            bool tilted = false; var kinds = new System.Collections.Generic.HashSet<int>();
            for (int prop = 0; prop < 200; prop++)
            {
                var lie = TankRenderer.OldWreckLie(prop);
                Assert.AreEqual(lie, TankRenderer.OldWreckLie(prop), "prop " + prop + " lies the same way twice");
                Assert.That(lie.x, Is.InRange(0f, 3f), "a machine there is a model of");
                Assert.AreEqual(Mathf.Floor(lie.x), lie.x, "a whole index");
                Assert.LessOrEqual(Mathf.Abs(lie.y) * Mathf.Rad2Deg, 5f, "pitch");
                Assert.LessOrEqual(Mathf.Abs(lie.z) * Mathf.Rad2Deg, 7f, "roll");
                Assert.That(lie.w, Is.InRange(0.25f, 0.55f), "settled into the mud, its tracks still showing");
                if (Mathf.Abs(lie.z) * Mathf.Rad2Deg > 2f) tilted = true;
                kinds.Add((int)lie.x);
            }
            Assert.IsTrue(tilted, "some of them heel over");
            Assert.GreaterOrEqual(kinds.Count, 3, "not one model for every hulk");
        }

        [Test]
        public void AnOldWrecksSlotIsNoSimSlot()
        {
            Assert.Less(TankRenderer.OldWreckSlot, 0);
            Assert.Less(TankRenderer.OldWreckScorch, 0.85f, "paler than a hull still burning");
        }
    }
}
