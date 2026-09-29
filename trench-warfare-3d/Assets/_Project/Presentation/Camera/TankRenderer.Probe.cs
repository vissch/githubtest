// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — a read-only look at one machine's ride as drawn this
// frame, for the Editor's WeightLab traces (the weight layer is judged on numbers before pictures). Reads, never writes.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    /// <summary>One machine's ride this frame: angles in degrees, heights in metres, speeds in metres a second.</summary>
    public struct WeightProbe
    {
        public float Speed, Accel, YawRate, Throttle;
        /// <summary>The ride springs alone (Pitch, Roll, Heave), and what the hull is drawn at (Hull*: with the kick layer,
        /// the flair and the footfall on top).</summary>
        public float Pitch, Roll, Heave, HullPitch, HullRoll, HullHeave;
        public float KickPitch, KickRoll, Foot;
        public float Recoil0, Recoil1, GunYaw0, Settle0, Settle1;
        public int FeetLanded;
        public bool Legged, Stalled, Weighted;
    }

    public sealed partial class TankRenderer
    {
        /// <summary>The ride of the machine in `slot` as drawn this frame; false when no live machine is drawn there.</summary>
        public bool Probe(int slot, out WeightProbe p)
        {
            p = default;
            if (!views.TryGetValue(slot, out var v)) return false;
            const float D = Mathf.Rad2Deg;
            p.Speed = v.Speed; p.Accel = v.Accel; p.YawRate = v.YawRate; p.Throttle = v.Throttle;
            p.Pitch = v.Pitch.Value * D; p.Roll = v.Roll.Value * D; p.Heave = v.Heave.Value;
            p.HullPitch = (v.Pitch.Value + v.PitchFx + v.KickP.Value) * D; p.HullRoll = (v.Roll.Value + v.KickR.Value) * D;
            p.HullHeave = v.Heave.Value + v.Bob + v.Foot.Value;
            p.KickPitch = v.KickP.Value * D; p.KickRoll = v.KickR.Value * D; p.Foot = v.Foot.Value;
            p.Recoil0 = v.Recoil[0]; p.Recoil1 = v.Recoil[1];
            p.GunYaw0 = (v.GunYaw[0] + v.Settle[0].Value) * D; p.Settle0 = v.Settle[0].Value * D; p.Settle1 = v.Settle[1].Value * D;
            p.Legged = v.Legs != null && v.Legs.Ready;
            p.FeetLanded = p.Legged ? v.Legs.Landed : 0;
            p.Stalled = v.Stalled; p.Weighted = weightOn;
            return true;
        }
    }
}
