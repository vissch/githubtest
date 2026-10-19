// Phase: deaths (2026-10-19, implemented) — the frog's own death gag (DeathGags.FrogFlop, pick A: "flops belly up,
// legs twitch"), on top of the absurd-deaths ladder (DeathGagTests): a killed frog lands with half a turn's rest
// roll (belly up) and a decaying twitch in the legs; a non-frog death is unchanged.
using NUnit.Framework;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Presentation;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class FrogDeathTests
    {
        struct Flat : IGroundHeight { public float At(float x, float z) => 0f; }

        [TearDown] public void Unpin() => DeathGags.Pin(-1f);

        [Test]
        public void KilledFrogLandsBellyUp()
        {
            DeathGags.Pin(1f);
            var g = new GagInput { Frog = true, Cause = DeathKind.Shot, Seed = 7, Tick = 1000 };
            float3 fly = 0f; var clip = Clip.DeathBack; float yaw = 0f;
            var plan = DeathGags.Choose(g, 1f, ref fly, ref clip, ref yaw);
            Assert.AreEqual(DeathGag.FrogFlop, plan.Gag);
            Assert.AreEqual(16, plan.RestRoll);

            var ground = new Flat();
            var path = FallenFlight.Make(Vector3.zero, new Vector3(fly.x, fly.y, fly.z), plan.Flips, plan.Rolls, plan.Bounces,
                plan.Skid, Vector3.zero, plan.Delay, plan.HoldLift, plan.Jig, 14f, new Vector2(600f, 600f), -10000f, ref ground);
            path.RestRoll = plan.RestRoll; path.TwitchRoll = plan.TwitchRoll; path.TwitchHz = plan.TwitchHz; path.TwitchDur = plan.TwitchDur;

            Assert.AreEqual(16, FallenFlight.RollStep(path, path.Arrive + 2.0f), "settled belly up, half a turn");

            float tint = VatTint.Pack(0, 0, FallenFlight.RollStep(path, path.Arrive + 2.0f));
            VatTint.Unpack(tint, out _, out _, out int roll, out _, out _);
            Assert.AreEqual(16, roll, "the drawn tint round-trips the rest roll");

            float t0 = path.Arrive + 0.1f, t1 = t0 + 0.1f;
            Assert.Less(t1 - path.Arrive, plan.TwitchDur, "still inside the twitch window");
            Assert.AreNotEqual(FallenFlight.RollStep(path, t0), FallenFlight.RollStep(path, t1), "the legs move");
        }

        /// <summary>The twitch must not cancel at a 1 s sampling gap: 2 roll steps of 32 is 22.5 deg of a ~1.2 m
        /// body, about 40 px of silhouette at the 41 px/m of a 2560x1440 still at zoom 30 (goal's probe bar).</summary>
        [Test]
        public void TheTwitchReadsASecondApart()
        {
            DeathGags.Pin(1f);
            var g = new GagInput { Frog = true, Cause = DeathKind.Shot, Seed = 7, Tick = 1000 };
            float3 fly = 0f; var clip = Clip.DeathBack; float yaw = 0f;
            var plan = DeathGags.Choose(g, 1f, ref fly, ref clip, ref yaw);

            var ground = new Flat();
            var path = FallenFlight.Make(Vector3.zero, new Vector3(fly.x, fly.y, fly.z), plan.Flips, plan.Rolls, plan.Bounces,
                plan.Skid, Vector3.zero, plan.Delay, plan.HoldLift, plan.Jig, 14f, new Vector2(600f, 600f), -10000f, ref ground);
            path.RestRoll = plan.RestRoll; path.TwitchRoll = plan.TwitchRoll; path.TwitchHz = plan.TwitchHz; path.TwitchDur = plan.TwitchDur;

            for (float t1 = 0f; t1 <= 0.6f + 1e-4f; t1 += 0.05f)
            {
                int a = FallenFlight.RollStep(path, path.Arrive + t1);
                int b = FallenFlight.RollStep(path, path.Arrive + t1 + 1.0f);
                Assert.GreaterOrEqual(Mathf.Abs(a - b), 2, $"at t1={t1:F2}s, a second apart must differ by >= 2 roll steps");
            }
        }

        [Test]
        public void ANonFrogDeathIsUnchanged()
        {
            var g = new GagInput { Frog = false, Cause = DeathKind.Shot, Clip = Clip.DeathBack, Flat = true, Seed = 7, Tick = 1000 };
            float3 fly = 0f; var clip = g.Clip; float yaw = 0f;
            var plan = DeathGags.Choose(g, 1f, ref fly, ref clip, ref yaw);
            Assert.AreNotEqual(DeathGag.FrogFlop, plan.Gag);
            Assert.AreEqual(0, plan.RestRoll);
            Assert.AreEqual(0f, plan.TwitchRoll);
        }
    }
}
