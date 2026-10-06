// Phase: C1 (unit look, look-03, 2026-10-06) — the route that was missing: the sim's Shot of a weapon that sets
// burning (the Flamethrower, archetype 36) is the one that draws the jet, and it is told apart by the match's own
// weapon table, never by archetype id. No sim is stepped and nothing is drawn: the test asks CombatFx.FlameShot
// about the events the sim writes.
using System;
using NUnit.Framework;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class FlameRouteTests
    {
        static readonly float3 Here = new float3(150f, 0f, 300f);

        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public SimWorld W => M.World;

            public Rig()
            {
                M = MatchSim.CreateGreybox(SimConfig.Default);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
            }

            public int Man(float3 at, byte archetype, byte team = 0) => W.Spawn(team, archetype, at, 100f, 3f, false);
            public void Dispose() => M.Dispose();
        }

        static SimEvent Shot(int a, int b) => new SimEvent { Type = SimEventType.Shot, A = a, B = b };

        [Test]
        public void AFlameMansShotIsTheOneThatDrawsTheJet()
        {
            using var r = new Rig();
            int flame = r.Man(Here, InfantryArchetype.Flamethrower);
            int rifle = r.Man(Here + new float3(0f, 0f, 9f), 0, 1);

            Assert.IsTrue(CombatFx.FlameShot(r.W, r.M.Catalogue, Shot(flame, rifle)),
                          "the flamethrower's Shot must route to the jet");
            Assert.IsFalse(CombatFx.FlameShot(r.W, r.M.Catalogue, Shot(rifle, flame)),
                           "a rifleman's Shot keeps its tracer");
        }

        [Test]
        public void OnlyAShotRoutesAndANullCatalogueRoutesNothing()
        {
            using var r = new Rig();
            int flame = r.Man(Here, InfantryArchetype.Flamethrower);
            int rifle = r.Man(Here + new float3(0f, 0f, 9f), 0, 1);

            var hit = new SimEvent { Type = SimEventType.Hit, A = flame, B = rifle };
            Assert.IsFalse(CombatFx.FlameShot(r.W, r.M.Catalogue, hit), "only a Shot opens the stream");
            Assert.IsFalse(CombatFx.FlameShot(r.W, null, Shot(flame, rifle)), "no table, no route");
            Assert.IsFalse(CombatFx.FlameShot(r.W, r.M.Catalogue, Shot(-1, rifle)), "no shooter, no route");
            Assert.IsFalse(CombatFx.FlameShot(r.W, r.M.Catalogue, Shot(r.W.HighWater + 5, rifle)), "off the end, no route");
        }

        [Test]
        public void TheJetDrawnForTheSimLightsNobodyItself()
        {
            // the sim decides who burns (UnitAlight): a stream the sim fired must not call Catch, or the picture lights
            // men the sim never lit
            var flames = new Flamethrower();
            int caught = 0;
            flames.Catch = (at, radius, seconds) => caught++;
            flames.Burst(0, new Vector3(0f, 1f, 0f), Vector3.forward, Vector3.zero, CombatFx.FlameShotSeconds, sim: true);
            Assert.AreEqual(1, flames.Jets, "the jet is drawn");
            Assert.AreEqual(1, flames.SimJets, "and it is marked the sim's");
            Assert.AreEqual(0, caught, "a sim jet lights nobody by itself");
        }
    }
}
