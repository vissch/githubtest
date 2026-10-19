// Phase: the battle (2026-10-19) — the Hopper's hop arc (TankRenderer.HopArc.cs)
// What would go wrong silently: the arc's shape is edited and the Hopper either never rises over a trench, or rises
// and stays up, which reads as the dead-level cruise the owner sent back. The values below are the worked example of
// 4p(1 - p): 0 at both ends, the full height at the middle.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class HopperHopArcTests
    {
        const float Dt = 1f / 30f;

        [Test]
        public void HopperHopArcRisesOverTrench()
        {
            Assert.IsTrue(TankRenderer.HopArcCrossing((byte)NavLayer.Trench), "a trench is worth clearing");
            Assert.IsTrue(TankRenderer.HopArcCrossing((byte)NavLayer.Wire), "so is a wire belt");
            Assert.IsFalse(TankRenderer.HopArcCrossing((byte)NavLayer.Surface), "open ground is not");

            float phase = 0f, top = 0f;
            int landings = 0, frames = 0;
            phase = TankRenderer.HopArcStep(phase, true, Dt, out bool landed);
            Assert.AreEqual(0f, TankRenderer.HopArcRise(0f), 1e-5f, "it starts at its cruise");
            Assert.IsFalse(landed, "it does not settle on the frame it leaves");
            while (frames++ < 120)
            {
                float rise = TankRenderer.HopArcRise(phase);
                if (rise > top) top = rise;
                phase = TankRenderer.HopArcStep(phase, false, Dt, out landed);
                if (landed) { landings++; break; }
            }
            Assert.AreEqual(1, landings, "one settle per arc");
            Assert.Less(frames * Dt, 2f, "the arc is over within its length");
            Assert.Greater(top, TankRenderer.HopArcRise(0.5f) * 0.9f, "it clears most of the full height");
            Assert.Less(TankRenderer.HopArcRise(phase), 0.2f, "and is back at its cruise by the settle");
            Assert.AreEqual(0f, phase, 1e-5f, "the phase is back at rest");
            Assert.AreEqual(0f, TankRenderer.HopArcStep(0f, false, Dt, out _), 1e-5f, "over open ground it stays level");
        }

        [Test]
        public void HopperHopArcNosesUpLeavingAndDownSettling()
        {
            Assert.Greater(TankRenderer.HopArcTilt(0.25f), 0f, "nose up on the way up");
            Assert.Less(TankRenderer.HopArcTilt(0.75f), 0f, "nose down settling back");
            Assert.AreEqual(0f, TankRenderer.HopArcTilt(0f), 1e-5f, "level at rest");
        }
    }
}
