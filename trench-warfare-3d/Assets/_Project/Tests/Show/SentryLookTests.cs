// Phase: character sim (2026-10-19) — the Sentry wore the Machinegunner's look and his tooltip said stand-in. He is the
// baked soldier still (only Soldier and Sniper are baked) and he fires the MG man's weapon, so what tells the two apart
// on a T2 sheet is the piece drawn over him: the 8 mm plate he fires over, on its post (CombatFx.Plate.cs). This holds
// that plate to the shape the plan specified, holds his gun to being an MG's, and holds his tooltip to being his own
// line and not a stand-in's.
//
// r2: the plate is emplaced, not carried. Its post stands ON the ground (r1 left it 0.455 m up), and the barrel is read
// once, so a 60 degree traverse does not swing the plate round with the gun.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class SentryLookTests
    {
        [Test]
        public void SentryWearsItsOwnLook()
        {
            var pieces = new Matrix4x4[CombatFx.PlatePieceMax];
            Vector3 foot = Vector3.zero, fwd = Vector3.forward;
            const float chestY = 1.30f;   // the chest socket of a man at UnitScale: he must fire over the plate's top

            Assert.That(CombatFx.PlatePieces(InfantryArchetype.Machinegunner, foot, fwd, 1f, pieces), Is.EqualTo(0),
                "the MG man carries no plate: his look must not change");
            Assert.That(CombatFx.PlatePieces(InfantryArchetype.Sentry, foot, fwd, 1f, pieces), Is.EqualTo(2),
                "the Sentry has a plate and its post");

            // the plate: over the post, on its centre line, wide and thin, and he fires over its top
            Vector3 at = pieces[0].GetColumn(3);
            Assert.That(Mathf.Abs(at.x), Is.LessThanOrEqualTo(0.10f), $"the plate is off the post's centre line (x {at.x})");
            float across = ((Vector3)pieces[0].GetColumn(0)).magnitude;
            float thick = ((Vector3)pieces[0].GetColumn(2)).magnitude;
            float tall = ((Vector3)pieces[0].GetColumn(1)).magnitude;
            Assert.That(across, Is.GreaterThanOrEqualTo(0.80f), $"the plate is too narrow for a 60 degree arc ({across} m)");
            Assert.That(thick, Is.LessThanOrEqualTo(0.08f), $"the plate is a box, not a plate ({thick} m thick)");
            Assert.That(tall, Is.GreaterThanOrEqualTo(0.50f), $"the plate is too short to cover chest to thigh ({tall} m)");
            Assert.That(at.y + tall * 0.5f, Is.LessThanOrEqualTo(chestY + 0.15f),
                $"the plate's top is too high: he must fire over it (top {at.y + tall * 0.5f}, chest {chestY})");

            // the post holds it up, and stands on the ground: r1's post ended 0.455 m in the air
            Vector3 postAt = pieces[1].GetColumn(3);
            float postTall = ((Vector3)pieces[1].GetColumn(1)).magnitude;
            Assert.That(postAt.y, Is.LessThan(at.y), "the post is not under the plate");
            Assert.That(postAt.y - postTall * 0.5f, Is.EqualTo(foot.y).Within(1e-4f),
                $"the post floats: its bottom is {postAt.y - postTall * 0.5f} m, the ground is {foot.y} m");
            Assert.That(at.y - tall * 0.5f, Is.GreaterThan(postAt.y - postTall * 0.5f),
                "the plate's bottom is not above the post's bottom");

            // his gun is the MG man's gun, so only the plate tells them apart
            Assert.That(CombatFx.KindOf(InfantryArchetype.Sentry), Is.EqualTo(CombatFx.ArmsKind.Mg), "his muzzle flash is still a rifle's");
            Assert.That(CombatFx.KindOf(InfantryArchetype.Sentry), Is.EqualTo(CombatFx.KindOf(InfantryArchetype.Machinegunner)),
                "he carries the MG man's weapon in the sim, so he must flash like him");

            Assert.That(UnitLook.Tip(InfantryArchetype.Sentry), Does.Not.Contain("stand-in"),
                "his tooltip still calls him a stand-in");
        }

        [Test]
        public void ThePlateStaysWhereHeEmplacedIt()
        {
            Vector3 feet = new Vector3(3f, 0f, -2f);
            var first = CombatFx.Emplace(default, feet, Vector3.forward, 1f);
            Assert.That(first.Set, Is.True, "he never set the plate down");
            Assert.That(first.Foot.z - feet.z, Is.GreaterThanOrEqualTo(0.20f),
                $"the plate is not in front of him (ahead {first.Foot.z - feet.z} m)");

            // he traverses 60 degrees; the barrel moves, the plate does not
            Vector3 turned = Quaternion.Euler(0f, 60f, 0f) * Vector3.forward;
            var again = CombatFx.Emplace(first, feet, turned, 1f);
            Assert.That(again.Foot, Is.EqualTo(first.Foot), $"the plate traversed with the gun (foot {again.Foot} was {first.Foot})");
            Assert.That(again.Fwd, Is.EqualTo(first.Fwd), $"the plate turned with the gun (fwd {again.Fwd} was {first.Fwd})");
        }

        [Test]
        public void TheCalibrationBarStandsOnTheGroundAndIsWiderThanThePlate()
        {
            var pieces = new Matrix4x4[CombatFx.PlatePieceMax];
            CombatFx.PlatePieces(InfantryArchetype.Sentry, Vector3.zero, Vector3.forward, 1f, pieces);
            var bar = CombatFx.PlateBar(Vector3.zero, Vector3.forward, 1f);
            Vector3 at = bar.GetColumn(3);
            float tall = ((Vector3)bar.GetColumn(1)).magnitude, across = ((Vector3)bar.GetColumn(0)).magnitude;
            Assert.That(at.y - tall * 0.5f, Is.EqualTo(0f).Within(1e-4f), "the bar does not stand on the ground, so it cannot measure the foot gap");
            Assert.That(tall, Is.EqualTo(0.50f).Within(1e-4f), "the probe reads metres off a 0.50 m bar");
            Assert.That(across, Is.GreaterThan(((Vector3)pieces[0].GetColumn(0)).magnitude), "the bar must stick out past the plate at both ends");
        }
    }
}
