// Phase: deaths (2026-10-19, the owner's pick) — the rocketed man planted head first in the mud (DeathGag.Planted):
// a blast's high throw sometimes drives him in instead of bouncing, his arc ends upside down (Arc.HalfTurn), about
// 0.9 m of legs stand above the mud, they kick for three seconds, then fold to half a metre.
// Every number here is TYPED from the worked geometry, never recomputed the way production does, so a change to the
// pivot, the scale or the fold silently moving the look fails the test. Pure arithmetic: no scene, no GPU.
using NUnit.Framework;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Presentation;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class PlantedDeathTests
    {
        struct Flat : IGroundHeight { public float At(float x, float z) => 0f; }

        const float G = 14f, NoWater = -10000f;
        /// <summary>FigureMetrics.UnitScale; the drawn ground a landing is laid on is 2 cm under it (FallenFlight.OnGround).</summary>
        const float Scale = 1.125f, Plane = -0.02f;
        /// <summary>The worked geometry (VAT_URP.shader: pitch, then squash, both about object y 0.9, then x scale):
        /// half a turn puts his feet 1.125 x 1.8 = 2.025 m above his Pos, so a Pos sunk this far leaves 0.905 m of legs,
        /// and the fold's squash code -15 draws them at 0.487 m. His head (object y 1.78) ends up under the mud.</summary>
        const float Depth = 1.120f, LegsAbove = 0.905f, FoldTo = 0.487f, HeadObjectH = 1.78f;
        const int FoldCode = -15;
        static readonly Vector2 Map = new Vector2(600f, 1200f);
        static readonly Vector3 Here = new Vector3(150f, 0f, 300f);

        /// <summary>How high above the drawn ground the man's feet are drawn, from his Pos and his squash code.</summary>
        static float FeetAbove(float posY, int squash) => posY - Plane + VatTint.UpsideDownHeight(0f, squash, Scale);
        static float HeadAbove(float posY, int squash) => posY - Plane + VatTint.UpsideDownHeight(HeadObjectH, squash, Scale);

        static GagInput Blast(uint seed) => new GagInput
        {
            Cause = DeathKind.Blast, Clip = Clip.DeathBack, Travel = new float3(0f, 0f, 1f),
            Throw = new float3(5f, 9f, 2f), Density = 0, BodyYaw = 0.3f, KillerYaw = 1.1f, Seed = seed, Tick = 4000,
        };

        [TearDown] public void Unpin() => DeathGags.Pin(-1f);

        [Test]
        public void RocketDeathLeavesLegsUpright()
        {
            DeathGags.Pin(1f);
            GagPlan plan = default; float3 fly = default; bool found = false;
            for (uint seed = 1; seed <= 64 && !found; seed++)
            {
                var g = Blast(seed);
                fly = g.Throw; Clip clip = g.Clip; float yaw = g.BodyYaw;
                var p = DeathGags.Choose(g, 1f, ref fly, ref clip, ref yaw);
                if (p.Gag != DeathGag.Planted) continue;
                found = true; plan = p;
                Assert.AreEqual(Clip.DeathThrown, clip, "a planted man is drawn in the thrown death's pose");
            }
            Assert.IsTrue(found, "a high blast throw plants at least one of 64 men");
            Assert.AreEqual(0, plan.Bounces, "he sticks: no bounce");
            Assert.AreEqual(0f, plan.Skid, 1e-6f, "he sticks: no skid");
            Assert.AreEqual(GagFlags.Freeze, plan.Flags, "his clip is held straight: the kick is the roll");
            Assert.AreEqual(FoldCode, plan.RestQ, "the fold's squash code");

            // the renderer's wiring (VATRenderer.Gags): the arc ends upside down and drives on into the mud
            var ground = new Flat();
            var path = FallenFlight.Make(Here, (Vector3)fly, plan.Flips, plan.Rolls, plan.Bounces, plan.Skid, Vector3.zero,
                plan.Delay, plan.HoldLift, plan.Jig, G, Map, NoWater, ref ground);
            Assert.AreEqual(1, path.Arcs, "one arc: he does not bounce");
            path.RestQ = plan.RestQ;
            path.A0.HalfTurn = 1;
            FallenFlight.EndAt(ref path, path.Rest - Vector3.up * Depth);
            path.Kick = (byte)DeathGags.PlantKickSteps; path.KickHz = DeathGags.PlantKickHz;
            path.KickEnd = path.Settled + DeathGags.PlantKickSeconds;
            path.RestFrom = path.KickEnd; path.RestDur = DeathGags.PlantFoldSeconds;
            float land = path.Delay + path.Settled;

            // upside down as he arrives, and upside down for as long as he lies there
            Assert.AreEqual(16, FallenFlight.PitchStep(path, land - 1e-3f, 16), "half a turn by the end of the arc");
            Assert.AreEqual(16, FallenFlight.PitchStep(path, land + 5f, 16), "and he stays on his head");

            // 0.9 m of legs above the mud, his head under it
            float restY = FallenFlight.At(path, land + 0.5f).y;
            Assert.AreEqual(path.Rest.y, restY, 1e-4f, "he is at rest half a second after he lands");
            Assert.AreEqual(LegsAbove, FeetAbove(restY, 0), 0.01f, "legs above the mud");
            Assert.Less(HeadAbove(restY, 0), 0f, "his head is in the mud");

            // kicking for three seconds, still after them
            bool kicked = false;
            for (int k = 1; k <= 60; k++)
            {
                float age = land + 3f * k / 61f;
                if (FallenFlight.RollStep(path, age) != 0) kicked = true;
            }
            Assert.IsTrue(kicked, "his legs kick while the kick lasts");
            for (int k = 0; k <= 20; k++)
                Assert.AreEqual(0, FallenFlight.RollStep(path, land + 3.1f + 0.5f * k), "the kick is over by " + (3.1f + 0.5f * k) + " s");

            // then they fold over half a second, to half a metre
            Assert.AreEqual(FoldCode, FallenFlight.Squash(path, land + 3.6f, plan.Intensity), "folded by landing + 3.6 s");
            Assert.AreEqual(FoldTo, FeetAbove(restY, FoldCode), 0.02f, "the folded legs' height");
            Assert.AreEqual(0, FallenFlight.Squash(path, land + 1.5f, plan.Intensity), "and straight while he kicks");
        }
    }
}
