// Phase: C1 (unit look, look-05, 2026-10-06) — who threw the streak.
//
// look-03's close shot (unit-look/flame/after_close.jpg) has a thin pink streak running from the flamethrower man
// toward the trench, and the master read it as the flame man still throwing a round. He is not: CombatFx.OnSimEvent
// breaks out of the Shot case before any tracer is added when the shooter's weapon SetsBurning (FlameRouteTests
// proves the route). A tracer's colour at night is its SIDE's — tracerNightA green for team 0, tracerNightB dark red
// for team 1 — so the streak's TEAM names its shooter, and no amount of looking at the picture does.
//
// CombatFx.TracerReport is how a capture rig reads that off a live frame. This test is what makes the report
// trustworthy: on the code before it, Tools/flamefight had nothing to print and the question could only be answered
// by eye.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class FlameShapeTests
    {
        [Test]
        public void TracerReportNamesEachStreaksTeam()
        {
            var go = new GameObject("combatfx-report");
            try
            {
                var fx = go.AddComponent<CombatFx>();
                Assert.AreEqual("tracers=0", fx.TracerReport(), "an empty frame throws nothing");

                fx.AddTracer(new Vector3(60f, 1f, 60f), new Vector3(70f, 1f, 60f), team: 1);
                string one = fx.TracerReport();
                StringAssert.StartsWith("tracers=1", one);
                StringAssert.Contains("team=1", one, "the streak must say whose side it is");

                fx.AddTracer(new Vector3(60f, 1f, 60f), new Vector3(70f, 1f, 60f), team: 0);
                StringAssert.StartsWith("tracers=2", fx.TracerReport());
            }
            finally
            {
                Object.DestroyImmediate(go);
            }
        }
    }
}
