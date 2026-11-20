// Phase: unit motion (2026-09-30) — three machines doing what the sim says they do, drawn on top of the ride:
//   the Breaker (18, drawn on the Maw's model) through its cycle, read off BreakerSystem.Phase: the wind-up squats it on
//     its tail and shakes it harder and harder with black smoke pouring from its exhausts, the charge lifts its nose
//     behind a wall of dust, the strike lunges it into the trench with a jolt, the withdrawal backs it out nose-down
//     (UnitMotion.Breaker gives the pose);
//   the Kettle's mortar (a gun the sim marks Indirect) laid at a lobbing angle, 80 degrees close in to 45 at full range
//     (UnitMotion.MortarPitch), not the direct-fire angle clamped at 22;
//   the Skimmer knocked out: its cushion lets it down onto its pods, it slides on a little along its way and tips onto
//     one pod, its fan running down, and a wreck keeps that (UnitMotion.SkimmerSettle).
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Units;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        const float MortarLayRate = 30f;      // deg/s a mortar is laid at (it has 6.5 s between rounds and 80 degrees to go)
        const float MortarMaxPitch = 80f;     // degrees

        BreakerSystem breakers; TW.Sim.Match.MatchSim breakersOf;

        BreakerSystem Breakers(TW.Sim.Match.MatchSim match)
        {
            if (breakersOf != match) { breakersOf = match; breakers = match != null ? match.World.GetSystem<BreakerSystem>() : null; }
            return breakers;
        }

        /// <summary>A gun that lobs (TankGun.Indirect: the Kettle's mortar): laid at the high angle for the range, or up at
        /// its rest with nothing to shoot at. False for every other gun, which keeps its direct-fire angle.</summary>
        static bool Lobbed(in TankSpec spec, int k, bool hasTarget, float dist, out float want)
        {
            var g = spec.Gun(k);
            want = 0f;
            if (!g.Indirect) return false;
            want = hasTarget ? UnitMotion.MortarPitch(dist, g.RangeMax) : UnitMotion.MortarRest;
            return true;
        }

        /// <summary>The Breaker's cycle on its hull (see the file header). Adds to the ride's pitch and heave, sets the
        /// lunge, and works its exhausts and the dust.</summary>
        void BreakerMotion(TW.Sim.Match.MatchSim match, View v, float now, Vector3 fwd, Vector3 right, ref float pitch, ref float roll, ref float heave)
        {
            var b = Breakers(match);
            if (b == null || !b.Phase.IsCreated || v.Slot >= b.Phase.Length) return;
            var spec = Machine(match.World, v.Archetype);
            if (spec.BreakerRange <= 0f) return;
            int phase = v.State != 0 ? 0 : b.Phase[v.Slot];
            if (phase != v.BreakerPhase)
            {
                v.BreakerPhase = phase; v.BreakerSince = now;
                if (phase == (int)TW.Sim.Units.BreakerPhase.Strike)
                {
                    // the blow: the hull jolts as it hits the trench and the earth goes up in front of it
                    Vector3 nose = v.Pos + fwd * v.Model.HalfLength; nose.y = Ground(nose.x, nose.z);
                    v.Pitch.Velocity -= 0.9f;
                    CameraShake.Add(nose, 6f);
                    if (books != null && books.Ready)
                    {
                        books.Add(FlipbookFx.Book.Column, nose, 3.4f, 1.2f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, grow: 0.3f, alpha: 0.9f);
                        books.Add(FlipbookFx.Book.Puff, nose + Vector3.up * 0.6f, 3.6f, 1.8f, velocity: Vector3.up * 0.8f, grow: 1.4f, alpha: 0.7f);
                    }
                }
                else if (phase == (int)TW.Sim.Units.BreakerPhase.Charge)
                    CameraShake.Add(v.Pos, 2.5f);
            }
            float t = now - v.BreakerSince;
            float length = phase == 1 ? spec.WindupSeconds : phase == 2 ? spec.ChargeSeconds : phase == 3 ? spec.StrikeSeconds : BreakerSystem.WithdrawSeconds;
            var look = UnitMotion.Breaker(phase, t, length);
            pitch += look.Pitch;
            heave += look.Heave + Mathf.Sin(now * 37f + v.Slot) * look.Shake;
            roll += Mathf.Sin(now * 29f + v.Slot * 1.7f) * look.Shake * 0.6f;
            v.Offset = fwd * look.Lunge;
            if (books == null || !books.Ready) return;
            // the exhausts pour black as it works itself up, and while it charges
            if (look.Smoke > 0f && now >= v.NextBreakerSmoke)
            {
                v.NextBreakerSmoke = now + Mathf.Lerp(0.3f, 0.07f, look.Smoke);
                for (int k = 0; k < 2; k++)
                {
                    var at = SocketWorld(v, "Socket_Exhaust" + k, out bool ok);
                    if (!ok) { if (k > 0) break; at = v.Pos - fwd * (v.Model.HalfLength * 0.8f) + Vector3.up * (v.Heave.Value + v.Model.Height * 0.8f); }
                    books.Add(FlipbookFx.Book.Smoke, at + Vector3.up * 0.2f, 1.2f + 0.8f * look.Smoke, 2.4f, UnityEngine.Random.value < 0.5f ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                        velocity: Vector3.up * (1.6f + look.Smoke) - fwd * 0.8f + UnityEngine.Random.insideUnitSphere * 0.3f, grow: 1.6f, roll: UnityEngine.Random.Range(-1f, 1f), alpha: 0.9f);
                }
            }
            // a wall of dust thrown up round its nose and out behind its tracks as it charges
            if (look.Dust > 0.05f && now >= v.NextBreakerDust)
            {
                v.NextBreakerDust = now + 0.07f;
                for (int side = -1; side <= 1; side += 2)
                {
                    Vector3 at = v.Pos + fwd * (v.Model.HalfLength * 0.6f) + right * (side * (v.Model.HalfGauge + 0.6f));
                    at.y = Ground(at.x, at.z) + 0.3f;
                    books.Add(FlipbookFx.Book.Puff, at, 2.4f * look.Dust + 0.6f, 1.6f, side < 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                        velocity: right * (side * 1.4f) + Vector3.up * 1.1f - fwd * 0.6f, grow: 1.5f, alpha: 0.55f * look.Dust);
                }
            }
        }

        /// <summary>The Skimmer knocked out or wrecked: settled onto its pods from the moment its engine died (see the file
        /// header). Changes the hover pose's heave and roll, and the slide.</summary>
        void SettleOnPods(View v, float now, Vector3 fwd, ref float roll, ref float heave)
        {
            if (v.State == 0 && !v.Dead) { v.KnockedAt = -1f; v.Offset = Vector3.zero; return; }
            if (v.KnockedAt < 0f)
            {
                v.KnockedAt = now; v.KnockSpeed = v.Speed;
                v.KnockDir = v.Speed >= 0f ? fwd : -fwd;
                v.TiltSide = (v.Slot & 1) == 0 ? 1f : -1f;
            }
            UnitMotion.SkimmerSettle(now - v.KnockedAt, v.KnockSpeed, out float drop, out float bob, out float skid, out float tilt);
            heave -= drop + (1f - bob) * HoverBob * Mathf.Sin((now * HoverBobHz + v.Slot * 0.37f) * Mathf.PI * 2f);
            roll += tilt * v.TiltSide;
            v.Offset = v.KnockDir * skid;
        }

        /// <summary>A wreck that was hovering: it finishes settling (the pose is no longer sprung) and its fan runs down.</summary>
        void SettleWreck(View v, float now, float dt)
        {
            if (!v.Hover) return;
            v.FanRate = Mathf.MoveTowards(v.FanRate, 0f, dt * 8f);
            v.Fan = Mathf.Repeat(v.Fan + Mathf.Min(v.FanRate, FanMaxShown) * dt, Mathf.PI * 2f);
            if (v.KnockedAt >= 0f && now - v.KnockedAt > UnitMotion.SettleSeconds + 1.5f) return;   // settled: leave it (an unlinked hull sinks later)
            Vector3 fwd = new Vector3(Mathf.Sin(v.Yaw), 0f, Mathf.Cos(v.Yaw)), right = new Vector3(fwd.z, 0f, -fwd.x);
            HoverPose(v, fwd, right, now, dt, out float pitch, out float roll, out float heave);
            SettleOnPods(v, now, fwd, ref roll, ref heave);
            v.Pitch.Value = Mathf.Lerp(v.Pitch.Value, pitch, 1f - Mathf.Exp(-dt * HoverSettle));
            v.Roll.Value = Mathf.Lerp(v.Roll.Value, roll, 1f - Mathf.Exp(-dt * HoverSettle));
            v.Heave.Value = Mathf.Lerp(v.Heave.Value, heave, 1f - Mathf.Exp(-dt * 10f));
        }
    }
}
