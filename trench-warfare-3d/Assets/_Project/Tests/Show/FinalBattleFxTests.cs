// Phase: final CelFx battle delivery (2026-11-14) - routing and per-event scheduling contracts.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class FinalBattleFxTests
    {
        [Test]
        public void Only_Matching_Dry_Bursts_Replace_Their_Legacy_Visuals()
        {
            Assert.That(FinalBattleFx.Route(BlastShape.Shell, SourceId.Ability(1), false), Is.EqualTo(FinalBurstRoute.Artillery));
            Assert.That(FinalBattleFx.Route(BlastShape.Shell, SourceId.Unit(30), false), Is.EqualTo(FinalBurstRoute.Explosion));
            Assert.That(FinalBattleFx.Route(BlastShape.Incendiary, SourceId.Ability(2), false), Is.EqualTo(FinalBurstRoute.Incendiary));
            Assert.That(FinalBattleFx.Route(BlastShape.Shell, SourceId.Ship, true), Is.EqualTo(FinalBurstRoute.Legacy));
            Assert.That(FinalBattleFx.Route(BlastShape.Masonry, SourceId.Unit(30), false), Is.EqualTo(FinalBurstRoute.Legacy));
            Assert.That(FinalBattleFx.Route(BlastShape.CookOff, SourceId.CookOff, false), Is.EqualTo(FinalBurstRoute.Legacy));
        }

        [Test]
        public void One_Shot_Plays_One_Recipe_And_Restores_The_Existing_Clock()
        {
            var holder = new GameObject("final battle test");
            var renderer = holder.AddComponent<CelFxRenderer>();
            try
            {
                CelFx.Adopt(renderer); renderer.Init();
                CelFx.Clock = 12f; CelFx.ZoomOverride = 7.5f;
                int before = CelFx.Plays;
                FinalBattleFx.Shot(Vector3.zero, Vector3.forward * 10f, 1917u, .15f);
                Assert.That(CelFx.Plays - before, Is.EqualTo(1), "a game Shot must not launch a reference volley");
                Assert.That(CelFx.Clock, Is.EqualTo(12f), "the stagger is local to this emission");
                Assert.That(renderer.SpawnedOf(CelKinds.Streak), Is.GreaterThan(0));
            }
            finally { renderer.Shutdown(); Object.DestroyImmediate(holder); SceneStatics.ResetSession(); }
        }
    }
}
