// Phase: fire-trench-miss-strikes. Before this fix CombatFx's Shot case put every miss on a ring round the target's
// feet (0.35-1.7 m, any direction), so a man InTrench / on the FireStep took dirt on his own trench floor and
// behind him; the sim already counts the cover that stopped the round (DirectFire.cs). TrenchMiss.Lip finds the
// parapet between him and the shooter instead.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class TrenchMissTests
    {
        // A simplified trench profile, after GreyboxMapGenerator.AddFireTrench (the 1.8 m carve, + FireStepRise 0.7 m
        // on the firing row): the whole bay sits at a fire-step's height, -1.1 m, between z 10 and 13; open ground,
        // 0, outside it. x does not move the floor (the trench runs along x).
        static float Ground(float x, float z) => z < 10f || z > 13f ? 0f : -1.1f;

        [Test]
        public void Lip_SitsOnTheParapetBetweenTargetAndShooter()
        {
            Vector3 target = new Vector3(0f, -1.1f, 11f), shooter = new Vector3(0f, 0f, 2f);
            bool found = TrenchMiss.Lip(target, shooter, Ground, out Vector3 lip);

            Assert.IsTrue(found, "[fire-trench-miss-strikes] a man on the fire step with a shooter outside the trench must have a lip");
            Assert.LessOrEqual(Mathf.Abs(lip.z - 10f), 1f, "[fire-trench-miss-strikes] the lip must sit within 1 m of the trench's own parapet line");
            Assert.AreEqual(0f, lip.y, 0.01f, "[fire-trench-miss-strikes] the lip is the surface the round struck, not the trench floor under it");
        }

        [Test]
        public void TheOldRing_OftenMissedTheParapetTheLipAlwaysHits()
        {
            Vector3 target = new Vector3(0f, -1.1f, 11f), shooter = new Vector3(0f, 0f, 2f);
            Assert.IsTrue(TrenchMiss.Lip(target, shooter, Ground, out Vector3 lip));

            int ringNear = 0, ringTotal = 0;
            for (int a = 0; a < 8; a++)
            {
                float angle = a * Mathf.PI * 2f / 8f;
                foreach (float off in new[] { 0.35f, 1.7f })
                {
                    ringTotal++;
                    Vector3 ring = new Vector3(target.x + Mathf.Cos(angle) * off, 0f, target.z + Mathf.Sin(angle) * off);
                    if (Mathf.Abs(ring.z - lip.z) <= 1f) ringNear++;
                }
            }
            Assert.Less(ringNear, ringTotal / 2, "[fire-trench-miss-strikes] the old ring round the man's feet must NOT reliably land near the parapet (it scatters into the trench and behind him)");

            for (int a = 0; a < 8; a++)
            {
                float along = -1f + a * 2f / 7f;
                Vector3 strike = TrenchMiss.Strike(lip, shooter - target, along, 0.2f);
                Assert.LessOrEqual(Vector3.Distance(strike, lip), TrenchMiss.AlongLip + 0.25f, "[fire-trench-miss-strikes] the lip strike must land on the parapet every time, unlike the old ring");
            }
        }

        [Test]
        public void FlatGround_HasNoLip_SoTheRingStaysForMenInTheOpen()
        {
            Vector3 target = new Vector3(0f, 0f, 5f), shooter = new Vector3(0f, 0f, -5f);
            bool found = TrenchMiss.Lip(target, shooter, Ground, out _);
            Assert.IsFalse(found, "[fire-trench-miss-strikes] a man in the open has no parapet; the ring must be left alone");
        }

        [Test]
        public void AShooterInTheSameTrench_HasNoLip()
        {
            // the sim already gives cover 0 for this case (DirectFire.cs); along the trench, not across it
            Vector3 target = new Vector3(0f, -1.1f, 11f), shooter = new Vector3(5f, -1.1f, 11f);
            bool found = TrenchMiss.Lip(target, shooter, Ground, out _);
            Assert.IsFalse(found, "[fire-trench-miss-strikes] a shooter in the same trench must not get a lip strike either");
        }

        [Test]
        public void Strike_NeverWalksFarOffTheLip()
        {
            Vector3 lip = new Vector3(0f, 0f, 9.6f), toShooter = new Vector3(0f, 0f, -1f);
            for (int a = 0; a <= 10; a++)
            {
                float along = -1f + a * 2f / 10f;
                Vector3 strike = TrenchMiss.Strike(lip, toShooter, along, 0.2f);
                Assert.LessOrEqual(Vector3.Distance(strike, lip), TrenchMiss.AlongLip + 0.25f, "[fire-trench-miss-strikes] a run of misses must stay on the parapet, not walk off it");
            }
        }
    }
}
