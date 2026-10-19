// Phase: character sim (2026-10-19) — the Sapper wore the Engineer's look and his tooltip said stand-in. He is the
// baked soldier still, so what tells him apart on a T2 sheet is the kit drawn over him: a pack and two charges
// (CombatFx.SapperKit.cs), and his lay-charge pose while he places a mine or strings a tripwire (AnimationController).
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Units;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class SapperLookTests
    {
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
                row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
                phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                A = new AnimationController(cfg.MaxSlots, M.Map, M.Gas, row, phase, yaw);
            }

            public void Tick()
            {
                A.Tick(W);
                A.Advance(W.Config.TickSeconds);
                W.Events.Clear();
                W.Tick++;
            }

            public void Dispose()
            {
                A.Dispose(); M.Dispose();
                row.Dispose(); phase.Dispose(); yaw.Dispose();
            }
        }

        [Test]
        public void SapperWearsItsOwnLook()
        {
            var pieces = new Matrix4x4[CombatFx.SapperKitPieceMax];
            Vector3 chest = new Vector3(0f, 1.30f, 0.05f), muzzle = new Vector3(0f, 1.35f, 0.60f), barrel = new Vector3(0f, 0f, 1f);

            Assert.That(CombatFx.SapperKitPieces(InfantryArchetype.Repair, chest, muzzle, barrel, 1f, pieces), Is.EqualTo(0),
                "the Engineer wears no kit: his look must not change");
            Assert.That(CombatFx.SapperKitPieces(InfantryArchetype.Sapper, chest, muzzle, barrel, 1f, pieces), Is.EqualTo(3),
                "the Sapper wears a pack and two charges");

            Vector3 packAt = pieces[0].GetColumn(3);
            Assert.That(packAt.z, Is.LessThanOrEqualTo(-0.10f), $"the pack is not on his back (z {packAt.z})");
            Assert.That(Mathf.Abs(packAt.x), Is.LessThanOrEqualTo(0.05f), $"the pack is off his centre line (x {packAt.x})");

            for (int k = 1; k < 3; k++)
            {
                Vector3 at = pieces[k].GetColumn(3);
                Assert.That(at.z, Is.LessThanOrEqualTo(-0.10f), $"charge {k} is not on his back (z {at.z})");
                Assert.That(Mathf.Abs(at.x), Is.InRange(0.14f, 0.26f), $"charge {k} sits the wrong way off the pack (x {at.x})");
            }
            Assert.That(((Vector3)pieces[1].GetColumn(3)).x * ((Vector3)pieces[2].GetColumn(3)).x, Is.LessThan(0f),
                "both charges are on the same side of him");

            Assert.That(UnitLook.Tip(InfantryArchetype.Sapper), Does.Not.Contain("stand-in"),
                "his tooltip still calls him a stand-in");

            using var r = new Rig();
            int slot = r.W.Spawn(0, InfantryArchetype.Sapper, new float3(150f, 0f, 300f), 100f, 3f, false);
            r.Tick();
            r.M.Sapper.Phase[slot] = SapperSystem.Laying;
            r.Tick();
            r.Tick();
            var s = r.A.State[slot];
            Assert.AreEqual(Clip.KneelInspect, s.Clip, "he kneels to lay the charge");
            Assert.AreEqual(Rung.Action, s.Rung);

            r.M.Sapper.Phase[slot] = SapperSystem.Idle;
            r.Tick();
            Assert.AreNotEqual(Clip.KneelInspect, r.A.State[slot].Clip, "the pose ends once the lay is over");
        }
    }
}
