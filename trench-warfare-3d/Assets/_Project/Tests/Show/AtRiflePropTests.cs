// Phase: atrifle-look-r3 (2026-10-19) — the AT rifleman's own gun, drawn as a prop (CombatFx.AtRifle.cs). The harsh
// critique of the r2 evidence found his silhouette all but the Sniper's, because both men are drawn from the same baked
// Soldier figure. The thing that tells them apart is the gun: a real Boys / PTRD anti-tank rifle is 1.6 to 2.0 m long,
// against a rifle's 1.1 m, so it must be that long on screen, laid along his facing, and at the height his drawn stance
// holds it. No scene and no sim: the lay is arithmetic, and these tests measure it.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class AtRiflePropTests
    {
        static readonly Vector3 Foot = new Vector3(120f, 0f, 240f);

        [Test]
        public void TheGunIsAsLongAsARealAntiTankRifle()
        {
            var lay = CombatFx.Lay(Foot, Foot.y, 0.7f, Stance.Prone, 1f);
            Assert.GreaterOrEqual(lay.Length, 1.6f, "an anti-tank rifle is 1.6 to 2.0 m long; this one is " + lay.Length + " m");
            Assert.LessOrEqual(lay.Length, 2.0f, "longer than any anti-tank rifle was");
        }

        [Test]
        public void ItLiesAlongTheYawHeIsDrawnAt([Values(0f, 1.1f, 3.0f, -2.2f)] float yaw)
        {
            var lay = CombatFx.Lay(Foot, Foot.y, yaw, Stance.Prone, 1f);
            var facing = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw));
            var along = lay.Rotation * Vector3.forward;
            Assert.Greater(Vector3.Dot(along, facing), 0.999f, "the gun points " + along + ", he faces " + facing);
        }

        [Test]
        public void ProneItSitsAtHisChestHeightOnTheGround()
        {
            float ground = 12.5f;
            var lay = CombatFx.Lay(new Vector3(Foot.x, ground, Foot.z), ground, 0.4f, Stance.Prone, 1f);
            float chest = ground + CombatFx.ChestHeight(Stance.Prone, false);
            Assert.That(lay.Centre.y, Is.EqualTo(chest).Within(0.4f), "the laid gun is at " + lay.Centre.y + ", his prone chest at " + chest);
            // and it is in front of him, not inside him
            Assert.Greater(Vector3.Dot(lay.Centre - new Vector3(Foot.x, ground, Foot.z), new Vector3(Mathf.Sin(0.4f), 0f, Mathf.Cos(0.4f))), 0.1f);
        }

        [Test]
        public void StandingItRidesHisShoulderNotTheGround()
        {
            var prone = CombatFx.Lay(Foot, Foot.y, 0f, Stance.Prone, 1f);
            var standing = CombatFx.Lay(Foot, Foot.y, 0f, Stance.Standing, 1f);
            Assert.Greater(standing.Centre.y - prone.Centre.y, 0.5f, "a man up on his feet carries it as high as his chest");
        }

        [Test]
        public void HisGunGrowsWithHisFigure()
        {
            var small = CombatFx.Lay(Foot, Foot.y, 0f, Stance.Prone, 1f);
            var grown = CombatFx.Lay(Foot, Foot.y, 0f, Stance.Prone, 2f);
            Assert.That(grown.Length, Is.EqualTo(small.Length * 2f).Within(1e-4f));
        }

        [Test]
        public void GunEnds_SpanTheGunAlongHisFacing()
        {
            // a man lying on the parapet at (50, 25), ground 2.95 m up, facing 0.7 rad, drawn life-size
            const float yaw = 0.7f, scale = 1f, ground = 2.95f;
            var lay = CombatFx.Lay(new Vector3(50f, ground, 25f), ground, yaw, Stance.Prone, scale);
            CombatFx.GunEnds(lay, out var muzzle, out var butt);
            Assert.That(Vector3.Distance(muzzle, butt), Is.EqualTo(lay.Length).Within(1e-4f), "the two ends are the gun's length apart");
            Assert.That(Vector3.Distance((muzzle + butt) * 0.5f, lay.Centre), Is.EqualTo(0f).Within(1e-4f), "their midpoint is where the gun is drawn");
            var facing = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw));
            Assert.That(Vector3.Dot((muzzle - butt).normalized, facing), Is.EqualTo(1f).Within(1e-4f), "the muzzle is the end out along his facing");
            Assert.That(lay.Length, Is.EqualTo(1.65f * scale).Within(1e-4f), "1.65 m at his drawn scale (docs/reference/tasks.md)");
        }

        [Test]
        public void OnlyTheAtRiflemanCarriesOne([Values(0, 1, 2, 3)] int arche)
        {
            Assert.IsFalse(CombatFx.Carries((byte)arche), "archetype " + arche + " keeps the rifle of his baked figure");
            Assert.IsTrue(CombatFx.Carries(InfantryArchetype.AtRifle));
        }
    }
}
