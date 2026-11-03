// Phase: VFX pass (owner decision, Decide page: "the gas bank, as it looks now" -> "needs to be more visual.
// not clear enough"). Pins the three-level table CombatFx.Banks.cs's DrawGasBank reads a cell's card from.
using NUnit.Framework;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class GasBankLevelsTests
    {
        [Test]
        public void GasBankLevel_SplitsAtAThirdAndTwoThirds()
        {
            Assert.AreEqual(0, CombatFx.GasBankLevel(0.0f));
            Assert.AreEqual(0, CombatFx.GasBankLevel(0.33f));
            Assert.AreEqual(1, CombatFx.GasBankLevel(0.34f));
            Assert.AreEqual(1, CombatFx.GasBankLevel(0.66f));
            Assert.AreEqual(2, CombatFx.GasBankLevel(0.67f));
            Assert.AreEqual(2, CombatFx.GasBankLevel(1.0f));
        }

        [Test]
        public void GasBankAlpha_StepsByLevel_NotAFade()
        {
            Assert.AreEqual(0.70f, CombatFx.GasBankAlpha(0.1f), 0.0001f);
            Assert.AreEqual(0.85f, CombatFx.GasBankAlpha(0.5f), 0.0001f);
            Assert.AreEqual(1.00f, CombatFx.GasBankAlpha(0.9f), 0.0001f);
            Assert.AreEqual(1.00f, CombatFx.GasBankAlpha(0.1f, edge: true), 0.0001f);

            Assert.GreaterOrEqual(CombatFx.GasBankAlpha(0.5f) - CombatFx.GasBankAlpha(0.1f), 0.1f, "a visible step, not a fade");
            Assert.GreaterOrEqual(CombatFx.GasBankAlpha(0.9f) - CombatFx.GasBankAlpha(0.5f), 0.1f, "a visible step, not a fade");
        }

        [Test]
        public void GasBankLift_TallerByLevel_AnEdgeStandsUp()
        {
            Assert.AreEqual(0.70f, CombatFx.GasBankLift(0.1f, false), 0.0001f);
            Assert.AreEqual(1.00f, CombatFx.GasBankLift(0.5f, false), 0.0001f);
            Assert.AreEqual(1.35f, CombatFx.GasBankLift(0.9f, false), 0.0001f);
            Assert.AreEqual(1.00f, CombatFx.GasBankLift(0.1f, true), 0.0001f);
        }

        [Test]
        public void GasBankBright_BrighterByLevel_AnEdgeIsADarkerRim()
        {
            Assert.AreEqual(0.90f, CombatFx.GasBankBright(0.1f, false), 0.0001f);
            Assert.AreEqual(1.15f, CombatFx.GasBankBright(0.9f, false), 0.0001f);
            Assert.AreEqual(0.85f, CombatFx.GasBankBright(0.1f, true), 0.0001f);
        }

        [Test]
        public void GasBankEdge_TrueWhenANeighbourFallsBelowTheFloor()
        {
            Assert.IsFalse(CombatFx.GasBankEdge(1f, 1f, 1f, 1f, 0.8f), "inside: every neighbour holds");
            Assert.IsTrue(CombatFx.GasBankEdge(1f, 1f, 1f, 0.2f, 0.8f), "a thin neighbour");
            Assert.IsTrue(CombatFx.GasBankEdge(1f, 1f, 1f, 0f, 0.8f), "off the field");
        }
    }
}
