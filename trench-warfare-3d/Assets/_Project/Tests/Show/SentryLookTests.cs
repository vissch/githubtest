// Phase: character sim (2026-10-19) — the Sentry wore the Machinegunner's look and his tooltip said stand-in. He is the
// baked soldier still (only Soldier and Sniper are baked) and he fires the MG man's weapon, so what tells the two apart
// on a T2 sheet is the piece drawn over him: the 8 mm plate he fires over, on its post (CombatFx.Plate.cs). This holds
// that plate to the shape the plan specified, holds his gun to being an MG's, and holds his tooltip to being his own
// line and not a stand-in's.
//
// r2: the plate is emplaced, not carried. Its post stands ON the ground (r1 left it 0.455 m up), and the barrel is read
// once, so a 60 degree traverse does not swing the plate round with the gun.
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation.Units;
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

        // r4 (the harsh critic, 38/100): r2's "he is dead and the plate still stands" still was shot with the clock stopped,
        // so no tick ever ran: no Death event was collected and no death clip played (plate_dead.json: fallen 0, drawn 1).
        // This kills him the way every sim system does - SimWorld.Despawn, which writes the Death event - and reads the
        // death back off the controller's own ring, never off State[slot].
        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public readonly AnimationController A;
            public SimWorld W => M.World;
            NativeArray<ushort> row; NativeArray<float> phase, yaw;

            public Rig()
            {
                var cfg = SimConfig.Default;
                M = MatchSim.CreateGreybox(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
                M.World.Tick = 1000;
                row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
                phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                A = new AnimationController(cfg.MaxSlots, M.Map, M.Gas, row, phase, yaw);
            }

            public void Dispose()
            {
                A.Dispose(); M.Dispose();
                row.Dispose(); phase.Dispose(); yaw.Dispose();
            }
        }

        [Test]
        public void SentryPlateOutlivesDeathEvent()
        {
            using var r = new Rig();
            float3 at = new float3(150f, 0f, 300f);
            int sentry = r.W.Spawn(0, InfantryArchetype.Sentry, at, 100f, 3f, false);
            int killer = r.W.Spawn(1, InfantryArchetype.Rifle, at + new float3(15f, 0f, 0f), 100f, 3f, false);
            r.A.Tick(r.W);
            r.A.Advance(r.W.Config.TickSeconds);
            r.W.Events.Clear();
            r.W.Tick++;

            // he sets the plate down while he is alive, and we keep its pieces as they stood then
            var spot = CombatFx.Emplace(default, new Vector3(at.x, at.y, at.z), Vector3.forward, 1f);
            var before = new Matrix4x4[CombatFx.PlatePieceMax];
            Assert.That(CombatFx.PlatePieces(InfantryArchetype.Sentry, spot.Foot, spot.Fwd, 1f, before), Is.EqualTo(CombatFx.PlatePieceMax),
                "he never emplaced a plate to outlive him");

            // the sim kills him the way every system does
            uint died = r.W.Tick;
            r.W.Despawn(sentry, killer, new float3(1f, 0f, 0f), 2f);

            bool death = false;
            for (int k = 0; k < r.W.Events.Events.Length; k++)
            {
                var e = r.W.Events.Events[k];
                if (e.Type == SimEventType.Death && e.A == sentry && e.B == killer) death = true;
            }
            Assert.IsTrue(death, "no Death event for him: a flag flip is not a death the picture can see");

            // and the controller played a death off that event
            r.A.Tick(r.W);
            Assert.IsTrue(r.A.TryDeath(sentry, died, out var rec), "the death is not on the controller's ring: nothing played");
            Assert.That(rec.Clip, Is.Not.EqualTo(Clip.None), "the controller chose no death clip for him");

            // the plate is still his, and matrix for matrix where it stood before he died
            Assert.IsTrue(CombatFx.KeepsPlate(r.W.Archetype[sentry], r.W.Flags[sentry], true),
                $"his plate vanished with him (flags {r.W.Flags[sentry]})");
            var after = new Matrix4x4[CombatFx.PlatePieceMax];
            CombatFx.PlatePieces(InfantryArchetype.Sentry, spot.Foot, spot.Fwd, 1f, after);
            for (int k = 0; k < CombatFx.PlatePieceMax; k++)
                Assert.That(after[k], Is.EqualTo(before[k]), $"plate piece {k} moved when he died ({after[k]} was {before[k]})");

            // but a slot let again to someone else keeps no dead man's plate
            Assert.IsFalse(CombatFx.KeepsPlate(InfantryArchetype.Rifle, 0u, true), "a re-let slot kept the dead Sentry's plate");
        }
    }
}
