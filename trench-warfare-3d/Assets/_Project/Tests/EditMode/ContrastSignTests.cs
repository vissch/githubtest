// Phase: tooling (2026-10-01) — the half of the readability number that was being thrown away.
//
// CaptureRig measures every drawn man as |man - bg| / (bg + 0.02): a disc on his chest against a median ring of the
// ground behind him. A third of the walker stills come back warning "men barely separate from the ground: unreadable"
// (contrast_median < 0.12), and that number has sat at ~0.16 since 2026-09-27.
//
// Mathf.Abs makes it impossible to act on. A man can be unreadable because he is too dark for his ground or too bright
// for it, and the fix is opposite in the two cases; the json said only "0.11". It is not academic: a range lift of the
// unit shaders on 2026-09-27 moved the battle 0.160 -> 0.125 and was reverted. With a man DARKER than his ground,
// lifting him moves him toward it, so the difference shrinks and the number falls — the lift was pushing the number
// the way it actually went, and nothing recorded could have said so beforehand.
//
// These pin the sign convention the stills now report, so a later reader cannot mistake which way is which.
using NUnit.Framework;
using TW.Editor;

namespace TW.Tests
{
    public class ContrastSignTests
    {
        /// <summary>The night case: a dark man on lighter mud reads negative.</summary>
        [Test]
        public void AManDarkerThanHisGroundIsNegative()
        {
            Assert.Less(CaptureRig.SignedContrast(0.10f, 0.25f), 0f);
        }

        /// <summary>And a man catching the light against dark ground reads positive.</summary>
        [Test]
        public void AManLighterThanHisGroundIsPositive()
        {
            Assert.Greater(CaptureRig.SignedContrast(0.40f, 0.18f), 0f);
        }

        /// <summary>
        /// The magnitude is unchanged: contrast_median still means exactly what it meant, so the 0.12 threshold and
        /// every number recorded before today stay comparable.
        /// </summary>
        [Test]
        public void TheMagnitudeIsTheNumberThatWasAlwaysReported()
        {
            const float man = 0.10f, bg = 0.25f;
            float legacy = UnityEngine.Mathf.Abs(man - bg) / (bg + 0.02f);
            Assert.AreEqual(legacy, UnityEngine.Mathf.Abs(CaptureRig.SignedContrast(man, bg)), 1e-6f);
        }

        /// <summary>
        /// The reverted lift, in one assertion. A man darker than his ground who is lifted toward it gets HARDER to
        /// see by this measure, not easier — which is what 0.160 -> 0.125 was.
        /// </summary>
        [Test]
        public void LiftingADarkManTowardHisGroundLowersHisContrast()
        {
            const float bg = 0.25f;
            float before = UnityEngine.Mathf.Abs(CaptureRig.SignedContrast(0.10f, bg));
            float lifted = UnityEngine.Mathf.Abs(CaptureRig.SignedContrast(0.17f, bg));
            Assert.Less(lifted, before, "lifting a dark man toward his ground must read as less separation, not more");
        }

        /// <summary>
        /// And the direction that does work on a dark man: deepen him, or lift the ground away from him. Both grow the
        /// gap. This is the claim the next change to the night look rests on, so it is pinned here.
        /// </summary>
        [Test]
        public void DeepeningTheManOrLiftingHisGroundBothRaiseHisContrast()
        {
            const float man = 0.10f, bg = 0.25f;
            float before = UnityEngine.Mathf.Abs(CaptureRig.SignedContrast(man, bg));
            Assert.Greater(UnityEngine.Mathf.Abs(CaptureRig.SignedContrast(0.06f, bg)), before, "a deeper man");
            Assert.Greater(UnityEngine.Mathf.Abs(CaptureRig.SignedContrast(man, 0.32f)), before, "lighter ground");
        }

        /// <summary>The denominator keeps a black background from reading as infinite contrast.</summary>
        [Test]
        public void APitchBlackBackgroundDoesNotReadAsInfiniteSeparation()
        {
            float c = CaptureRig.SignedContrast(0.2f, 0f);
            Assert.That(c, Is.GreaterThan(0f).And.LessThan(11f), "0.02 in the denominator caps it");
        }
    }
}
