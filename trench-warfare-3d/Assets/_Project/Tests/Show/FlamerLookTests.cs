// Phase: character sim (2026-10-19) — the Flamethrower wore the Assault's look and his tooltip said stand-in. He is the
// baked soldier still (only Soldier and Sniper are baked), so what tells him apart on a T2 sheet is the kit drawn over
// him: two fuel tanks on his back and the wand in his hands (CombatFx.Kit.cs). This holds that kit to the shape the
// plan specified, and holds the tooltip to being a unit's own line and not a stand-in's.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class FlamerLookTests
    {
        [Test]
        public void FlamerWearsItsOwnLook()
        {
            var pieces = new Matrix4x4[CombatFx.KitPieceMax];
            Vector3 chest = new Vector3(0f, 1.30f, 0.05f), muzzle = new Vector3(0f, 1.35f, 0.60f), barrel = new Vector3(0f, 0f, 1f);

            Assert.That(CombatFx.KitPieces(InfantryArchetype.Assault, chest, muzzle, barrel, 1f, pieces), Is.EqualTo(0),
                "the Assault wears no kit: his look must not change");
            Assert.That(CombatFx.KitPieces(InfantryArchetype.Flamethrower, chest, muzzle, barrel, 1f, pieces), Is.EqualTo(3),
                "the Flamer wears two tanks and a wand");

            // the tanks: behind him, one each side of the spine, each a tank's height
            for (int k = 0; k < 2; k++)
            {
                Vector3 at = pieces[k].GetColumn(3);
                Assert.That(at.z, Is.LessThanOrEqualTo(-0.10f), $"tank {k} is not on his back (z {at.z})");
                Assert.That(Mathf.Abs(at.x), Is.InRange(0.08f, 0.15f), $"tank {k} sits the wrong way off his spine (x {at.x})");
                Assert.That(((Vector3)pieces[k].GetColumn(1)).magnitude, Is.GreaterThanOrEqualTo(0.40f), $"tank {k} is too short to read");
            }
            Assert.That(((Vector3)pieces[0].GetColumn(3)).x * ((Vector3)pieces[1].GetColumn(3)).x, Is.LessThan(0f),
                "both tanks are on the same side of him");

            // the wand: it reaches well past his chest, and down his centre line
            Vector3 wandAt = pieces[2].GetColumn(3), along = pieces[2].GetColumn(2);
            Vector3 far = wandAt + along * 0.5f;
            Assert.That(far.z - chest.z, Is.GreaterThanOrEqualTo(1.0f), $"the wand does not reach out in front of him (far end z {far.z})");
            Assert.That(Mathf.Abs(far.x), Is.LessThanOrEqualTo(0.20f), $"the wand points off his centre line (x {far.x})");

            Assert.That(UnitLook.Tip(InfantryArchetype.Flamethrower), Does.Not.Contain("stand-in"),
                "his tooltip still calls him a stand-in");
        }
    }
}
