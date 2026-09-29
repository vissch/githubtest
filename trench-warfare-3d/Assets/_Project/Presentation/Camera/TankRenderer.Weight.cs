// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — depends on: HullRide, Knobs, TankGunnerySystem.
// How heavy a machine looks, behind knobs that each draw today's machines at 0, so the owner can judge the captures
// before any default changes (the fx.deathAbsurd rule):
//  - tank.weight: each machine's own ride (TankRenderer.DriveStyle.cs) on an acceleration followed from the sim's own
//    speed; a walker's kicks and footfalls on layers of their own. Off, every machine rides PlainStyle, as before;
//  - tank.recoil: the barrel's travel and return by the weight of its shot (the Maw's six-pounder is 1);
//  - tank.shotRock: degrees a six-pounder rocks its hull, by the shot's weight (0 keeps today's kick of 0.35 rad/s);
//  - tank.gunHullFlash: the hull lights up (its hit-flash channel) when its gun fires;
//  - tank.traverseSettle: degrees a slow turret swings past its mark when it stops, never while a shot is due.
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        bool weightOn, calibreRecoil;
        float shotRockDeg, gunHullFlash, settleDeg;
        int weightKnobs = -1;

        /// <summary>Reads the weight knobs when any knob has changed, and re-rides the machines on the field.</summary>
        void WeightKnobs()
        {
            if (weightKnobs == Knobs.Generation) return;
            weightKnobs = Knobs.Generation;
            weightOn = Knobs.Get("tank.weight", false);
            calibreRecoil = Knobs.Get("tank.recoil", false);
            shotRockDeg = Mathf.Max(0f, Knobs.Get("tank.shotRock", 0f));
            gunHullFlash = Mathf.Clamp01(Knobs.Get("tank.gunHullFlash", 0f));
            settleDeg = Mathf.Clamp(Knobs.Get("tank.traverseSettle", 0f), 0f, 0.6f);
            foreach (var v in views.Values)
            {
                v.Style = RideFor(v.Archetype);
                if (v.Legs != null) { v.Legs.SwingScale = v.Style.Swing; v.Legs.ArcScale = v.Style.Arc; }
            }
        }

        /// <summary>The ride a machine is drawn with: its own with the weight layer on, the one every machine rode
        /// before it (PlainStyle) with it off.</summary>
        DriveStyle RideFor(byte archetype) => weightOn ? StyleFor(archetype) : PlainStyle;

        /// <summary>A new view's guns: how heavy each shot is, and how long its barrel takes to run back.</summary>
        static void WeighGuns(View v, in TankSpec spec)
        {
            for (int k = 0; k < 2; k++)
            {
                float w = k < spec.GunCount ? HullRide.ShotWeight(spec.Gun(k), spec.Rockets > 0 && k == 0) : 1f;
                v.ShotW[k] = w;
                v.ReturnT[k] = HullRide.ReturnSeconds(w);
            }
        }

        /// <summary>The felt acceleration: the sim's speed along the nose, sampled once a tick (with momentum it ramps,
        /// while the drawn position can freeze and jump when the presenter waits for a tick), followed at the ride's
        /// own pace in sim time, so a pause holds the pose and fast-forward does not exaggerate it.</summary>
        void FeelTheWay(SimWorld w, View v, float dt)
        {
            int s = v.Slot;
            if (w.Tick != v.SimTick)
            {
                v.SimTick = w.Tick;
                float3 vel = w.Velocity[s];
                float yaw = w.Yaw[s];
                v.SimSpeed = vel.x * math.sin(yaw) + vel.z * math.cos(yaw);
            }
            float h = dt * Mathf.Max(0f, Host.TimeScale);
            v.Accel = HullRide.Follow(ref v.Felt.Value, ref v.Felt.Velocity, v.SimSpeed, h, HullRide.FollowOmega(v.Style.Omega));
        }

        /// <summary>A walker's kicks. Its tilt is set straight off its feet each frame (a walker is not sprung twice), which
        /// wiped every kick a shot, a hit or a blast put on it: they go into a layer of their own first, which springs
        /// back and is drawn on top of the gait.</summary>
        void TakeWalkerKicks(View v, float dt)
        {
            if (!weightOn) return;
            v.KickP.Velocity += v.Pitch.Velocity * HullRide.WalkerKickShare;
            v.KickR.Velocity += v.Roll.Velocity * HullRide.WalkerKickShare;
            HullRide.StepKick(ref v.KickP.Value, ref v.KickP.Velocity, dt, HullRide.KickCap(v.Legs.TiltCapPitch));
            HullRide.StepKick(ref v.KickR.Value, ref v.KickR.Velocity, dt, HullRide.KickCap(v.Legs.TiltCapRoll));
        }

        /// <summary>The footfall's dip, on a spring of its own at the machine's heave rate rather than on the hull's heave,
        /// which the riders read to flinch (a Redoubt's two-footed stomp was 2.4 m/s against their 2 m/s).</summary>
        static void StepFootfall(View v, float dt) => HullRide.Solve(ref v.Foot.Value, ref v.Foot.Velocity, 0f, dt, v.Style.HeaveOmega, 1f);

        /// <summary>The kick a shot gives the hull: today's 0.35 rad/s, or with tank.shotRock the kick that tips this
        /// machine's springs that many degrees for a six-pounder, by the shot's weight (a walker's layer 0.6 of it).</summary>
        float ShotKick(View v, int gun)
        {
            if (shotRockDeg <= 0f) return 0.35f;
            float w = v.ShotW[gun];
            bool legged = v.Legs != null && v.Legs.Ready;
            return legged
                ? HullRide.KickFor(shotRockDeg * 0.6f * w, HullRide.KickOmega, HullRide.KickZeta) / HullRide.WalkerKickShare
                : HullRide.KickFor(shotRockDeg * w, v.Style.Omega, v.Style.Zeta);
        }

        /// <summary>A barrel's recoil now, as a share of its part's travel, and how fast Recoil runs out (today 2.6/s for
        /// every gun).</summary>
        float RecoilShape(View v, int k) => calibreRecoil ? HullRide.Kick(v.Recoil[k], v.ReturnT[k]) * HullRide.RecoilScale(v.ShotW[k]) : Kick(v.Recoil[k]);
        float RecoilDecay(View v, int k) => calibreRecoil ? 1f / v.ReturnT[k] : 2.6f;

        /// <summary>A slow turret coming to rest swings a little past its mark and settles back; never while a shot is due,
        /// so the barrel is on the mark the sim laid when it fires. Only ever an offset on top of the sim's aim.</summary>
        void SettleTurrets(TW.Sim.Match.MatchSim match, View v, in TankSpec spec, float dt)
        {
            var gun = match.Gunnery;
            for (int k = 0; k < 2; k++)
            {
                float yaw = v.GunYaw[k];
                float rate = dt > 1e-4f ? Mathf.DeltaAngle(v.LastGunYaw[k] * Mathf.Rad2Deg, yaw * Mathf.Rad2Deg) * Mathf.Deg2Rad / dt : 0f;
                v.LastGunYaw[k] = yaw;
                float was = v.LastGunRate[k];
                v.LastGunRate[k] = rate;
                if (settleDeg <= 0f || k >= spec.GunCount || Host.TimeScale <= 0f) { v.Settle[k] = default; continue; }
                var g = spec.Gun(k);
                float amp = HullRide.SettleDegrees(g.TraverseRate * Mathf.Rad2Deg, settleDeg);
                // it stopped: turning at more than half its rate last frame, hardly at all now
                if (amp > 0f && Mathf.Abs(was) > 0.5f * g.TraverseRate && Mathf.Abs(rate) < 0.1f * g.TraverseRate)
                    v.Settle[k].Velocity += Mathf.Sign(was) * HullRide.KickFor(amp, HullRide.SettleOmega, HullRide.SettleZeta);
                HullRide.Solve(ref v.Settle[k].Value, ref v.Settle[k].Velocity, 0f, dt, HullRide.SettleOmega, HullRide.SettleZeta);
                int at = v.Slot * TankGunnerySystem.Guns + k;
                if (gun != null && gun.GunTarget[at] >= 0 && gun.Reload[at] <= 2) v.Settle[k] = default;
                float cap = settleDeg * Mathf.Deg2Rad;
                v.Settle[k].Value = Mathf.Clamp(v.Settle[k].Value, -cap, cap);
            }
        }

        /// <summary>The rack sways as each rocket leaves (tank.shotRock): a small kick along the tube, and none while the
        /// hull is already moving faster than a 3-degree rock would.</summary>
        void RocketRock(View v)
        {
            if (shotRockDeg <= 0f) return;
            float most = HullRide.KickFor(3f, v.Style.Omega, v.Style.Zeta);
            if (Mathf.Abs(v.Pitch.Velocity) > most || Mathf.Abs(v.Roll.Velocity) > most) return;
            MuzzleWorld(v, 0, out Vector3 dir);
            Vector3 fwd = new Vector3(Mathf.Sin(v.Yaw), 0f, Mathf.Cos(v.Yaw)), right = new Vector3(fwd.z, 0f, -fwd.x);
            float kick = HullRide.KickFor(0.15f * shotRockDeg, v.Style.Omega, v.Style.Zeta);
            v.Pitch.Velocity += Vector3.Dot(dir, fwd) * kick;
            v.Roll.Velocity -= Vector3.Dot(dir, right) * kick;
        }

        /// <summary>A dead machine keeps none of this: its wreck lies as it fell, not frozen mid-rock.</summary>
        static void StillWeight(View v)
        {
            v.KickP = default; v.KickR = default; v.Foot = default;
            v.Settle[0] = default; v.Settle[1] = default;
        }
    }
}
