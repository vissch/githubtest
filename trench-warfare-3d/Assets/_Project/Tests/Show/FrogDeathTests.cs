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
            // 0, not the old 16: the pixels fixed this, not the plan. docs/evidence/frog-death-flop-r3-g2 shows the
            // same death with the gag pinned off (ctrl_p15.png) already lying pale belly and green hands up, while
            // the half turn of 16 steps (flop_p15.png) showed his dark back. The death clip lays him on his back.
            Assert.AreEqual(0, plan.RestRoll);

            var ground = new Flat();
            var path = FallenFlight.Make(Vector3.zero, new Vector3(fly.x, fly.y, fly.z), plan.Flips, plan.Rolls, plan.Bounces,
                plan.Skid, Vector3.zero, plan.Delay, plan.HoldLift, plan.Jig, 14f, new Vector2(600f, 600f), -10000f, ref ground);
            path.RestRoll = plan.RestRoll; path.TwitchRoll = plan.TwitchRoll; path.TwitchHz = plan.TwitchHz; path.TwitchDur = plan.TwitchDur;

            // once the twitch has run out he lies as the clip left him, with no turn of the gag's own
            float settled = path.Arrive + plan.TwitchDur + 0.5f;
            Assert.AreEqual(0, FallenFlight.RollStep(path, settled), "settled belly up, as the death clip laid him");

            float tint = VatTint.Pack(0, 0, FallenFlight.RollStep(path, settled));
            VatTint.Unpack(tint, out _, out _, out int roll, out _, out _);
            Assert.AreEqual(0, roll, "the drawn tint round-trips the rest roll");

            float t0 = path.Arrive + 0.1f, t1 = t0 + 0.1f;
            Assert.Less(t1 - path.Arrive, plan.TwitchDur, "still inside the twitch window");
            Assert.AreNotEqual(FallenFlight.RollStep(path, t0), FallenFlight.RollStep(path, t1), "the legs move");
        }

        /// <summary>The twitch must not CANCEL at a 1 s sampling gap: two stills a second apart must never read the
        /// same roll. The sin this guards is the one 5 Hz committed, a whole number of cycles per second, which drew
        /// the very same pose at both.
        /// It asked for 2 roll steps until 2026-10-24, and was green only because the rest roll of 16 was still
        /// ramping in over RollSettle: the ramp, not the twitch, made the two samples differ. With the rest roll at 0
        /// (the death clip lays the frog on his back by itself, read off the pixels; see KilledFrogLandsBellyUp) the
        /// ramp is gone and the twitch alone gives 1 step at its worst phase, near t = 0 where both samples sit close
        /// to zero. No amplitude fixes that: scaling a waveform cannot pull two near-zero samples apart. So the bar is
        /// what the guard is really for -- never identical -- and how far the pose actually moves is now measured off
        /// the pixels instead, in docs/evidence/frog-death-flop-r3-g2/probe.py (twitch 0.68 of the body against a
        /// frozen-pair floor of 0.0006).</summary>
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
                // the roll wraps at 32, so the two are compared the short way round
                int d = Mathf.Abs(a - b);
                d = Mathf.Min(d, 32 - d);
                Assert.GreaterOrEqual(d, 1, $"at t1={t1:F2}s, a second apart must not draw the same roll");
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
