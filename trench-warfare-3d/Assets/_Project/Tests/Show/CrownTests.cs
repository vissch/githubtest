// Phase: ability-fx-captures-r3-2-2-g2-g2-g2 - the close splash was one tall blue shaft with two prongs (splash_T1.png).
// The owner asked for a crown of water: a squat centre mass with spray thrown outward and up around its rim. The shape
// is CombatFx.CrownJet and the Crown* constants (CombatFx.ShellFire.cs); these sums are by hand, not from the code's own
// arithmetic, so a retune has to be meant. Style: ColumnWidthTests.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class CrownTests
    {
        const float Tol = 1e-4f;
        const float Width = 18.4f;   // the wet width of an r=8 shell: 8 * 2.3 (ColumnWidthTests)
        const float Rim = 10.672f;   // 18.4 * 0.58 (CombatFx.CrownRim)

        // [ability-fx-captures-r3-2-2-g2-g2-g2]
        [Test]
        public void Jets_SitOnTheRim_ByHand()
        {
            Vector3 j0 = CombatFx.CrownJet(0, 6, Width, 0f);
            Assert.AreEqual(Rim, j0.x, Tol, "[ability-fx-captures-r3-2-2-g2-g2-g2] jet 0 is out along +x at the rim: 18.4 * 0.58");
            Assert.AreEqual(0f, j0.y, Tol, "[ability-fx-captures-r3-2-2-g2-g2-g2] the ring lies flat on the water");
            Assert.AreEqual(0f, j0.z, Tol, "[ability-fx-captures-r3-2-2-g2-g2-g2] jet 0 has no z");

            Vector3 j3 = CombatFx.CrownJet(3, 6, Width, 0f);
            Assert.AreEqual(-Rim, j3.x, Tol, "[ability-fx-captures-r3-2-2-g2-g2-g2] jet 3 is opposite jet 0");
            Assert.AreEqual(0f, j3.z, Tol, "[ability-fx-captures-r3-2-2-g2-g2-g2] jet 3 has no z");

            Vector3 j1 = CombatFx.CrownJet(1, 6, Width, 0f);
            Assert.AreEqual(5.336f, j1.x, Tol, "[ability-fx-captures-r3-2-2-g2-g2-g2] jet 1 is a sixth of a turn on: 10.672 * cos 60");
            Assert.AreEqual(9.2422f, j1.z, Tol, "[ability-fx-captures-r3-2-2-g2-g2-g2] jet 1: 10.672 * sin 60");
        }

        // [ability-fx-captures-r3-2-2-g2-g2-g2]
        [Test]
        public void Crown_IsARingAndIsNotLopsided()
        {
            Vector3 sum = Vector3.zero;
            for (int i = 0; i < CombatFx.CrownJets; i++)
            {
                Vector3 off = CombatFx.CrownJet(i, CombatFx.CrownJets, Width, 0.7f);
                Assert.AreEqual(Rim, off.magnitude, 1e-3f, "[ability-fx-captures-r3-2-2-g2-g2-g2] jet " + i + " is on the rim, 10.672 m out");
                sum += off;
            }
            Assert.AreEqual(0f, sum.magnitude, 1e-4f, "[ability-fx-captures-r3-2-2-g2-g2-g2] the six offsets cancel: the crown is not lopsided");
            Assert.AreEqual(6, CombatFx.CrownJets, "[ability-fx-captures-r3-2-2-g2-g2-g2] six fingers");
        }

        // [ability-fx-captures-r3-2-2-g2-g2-g2]
        [Test]
        public void CentreCard_IsSquatterThanWide()
        {
            float height = Width * CombatFx.CrownHeight;
            Assert.AreEqual(17.48f, height, Tol, "[ability-fx-captures-r3-2-2-g2-g2-g2] 18.4 * 0.95");
            Assert.Less(height, Width, "[ability-fx-captures-r3-2-2-g2-g2-g2] the crown's centre is a low mass, not a shaft");
            Assert.Less(CombatFx.CrownJetWidth, 1f, "[ability-fx-captures-r3-2-2-g2-g2-g2] a finger is narrower than the mass");
        }

        // [ability-fx-captures-r3-2-2-g2-g2-g2]
        [Test]
        public void Source_TheRingIsOneLoop()
        {
            string combat = System.IO.File.ReadAllText(System.IO.Path.Combine(Application.dataPath, "_Project", "Presentation", "Camera", "CombatFx.cs"));
            Assert.AreEqual(1, Count(combat, "CrownJet("), "[ability-fx-captures-r3-2-2-g2-g2-g2] CombatFx.cs calls CrownJet once: the ring is a loop, not six pasted lines");
        }

        static int Count(string s, string what)
        {
            int n = 0;
            for (int i = s.IndexOf(what); i >= 0; i = s.IndexOf(what, i + what.Length)) n++;
            return n;
        }
    }
}
