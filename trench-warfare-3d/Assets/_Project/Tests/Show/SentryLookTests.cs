// Phase: character sim (2026-10-19) — the Sentry wore the Machinegunner's look and his tooltip said stand-in. He is the
// baked soldier still (only Soldier and Sniper are baked) and he fires the MG man's weapon, so what tells the two apart
// on a T2 sheet is the piece drawn over him: the 8 mm plate he fires over, on its post (CombatFx.Plate.cs). This holds
// that plate to the shape the plan specified, holds his gun to being an MG's, and holds his tooltip to being his own
// line and not a stand-in's.
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
            Vector3 chest = new Vector3(0f, 1.30f, 0.05f), muzzle = new Vector3(0f, 1.35f, 0.60f), barrel = new Vector3(0f, 0f, 1f);

            Assert.That(CombatFx.PlatePieces(InfantryArchetype.Machinegunner, chest, muzzle, barrel, 1f, pieces), Is.EqualTo(0),
                "the MG man carries no plate: his look must not change");
            Assert.That(CombatFx.PlatePieces(InfantryArchetype.Sentry, chest, muzzle, barrel, 1f, pieces), Is.EqualTo(2),
                "the Sentry has a plate and its post");

            // the plate: in front of him, on his centre line, wide and thin, and he fires over its top
            Vector3 at = pieces[0].GetColumn(3);
            Assert.That(at.z, Is.GreaterThanOrEqualTo(0.20f), $"the plate is not in front of him (z {at.z})");
            Assert.That(Mathf.Abs(at.x), Is.LessThanOrEqualTo(0.10f), $"the plate is off his centre line (x {at.x})");
            float across = ((Vector3)pieces[0].GetColumn(0)).magnitude;
            float thick = ((Vector3)pieces[0].GetColumn(2)).magnitude;
            float tall = ((Vector3)pieces[0].GetColumn(1)).magnitude;
            Assert.That(across, Is.GreaterThanOrEqualTo(0.80f), $"the plate is too narrow for a 60 degree arc ({across} m)");
            Assert.That(thick, Is.LessThanOrEqualTo(0.08f), $"the plate is a box, not a plate ({thick} m thick)");
            Assert.That(tall, Is.GreaterThanOrEqualTo(0.50f), $"the plate is too short to cover chest to thigh ({tall} m)");
            Assert.That(at.y + tall * 0.5f, Is.LessThanOrEqualTo(chest.y + 0.15f),
                $"the plate's top is too high: he must fire over it (top {at.y + tall * 0.5f}, chest {chest.y})");

            // the post holds it up, under the plate
            Assert.That(((Vector3)pieces[1].GetColumn(3)).y, Is.LessThan(at.y), "the post is not under the plate");

            // his gun is the MG man's gun, so only the plate tells them apart
            Assert.That(CombatFx.KindOf(InfantryArchetype.Sentry), Is.EqualTo(CombatFx.ArmsKind.Mg), "his muzzle flash is still a rifle's");
            Assert.That(CombatFx.KindOf(InfantryArchetype.Sentry), Is.EqualTo(CombatFx.KindOf(InfantryArchetype.Machinegunner)),
                "he carries the MG man's weapon in the sim, so he must flash like him");

            Assert.That(UnitLook.Tip(InfantryArchetype.Sentry), Does.Not.Contain("stand-in"),
                "his tooltip still calls him a stand-in");
        }
    }
}
