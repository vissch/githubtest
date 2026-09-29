// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — pure; depends on nothing.
// How much dust each track throws. TankRenderer.Effects gates its track dust on the hull's speed alone (|Speed| over
// 0.25 m/s), so a machine pivoting on the spot, both tracks churning the ground in opposite directions, throws none:
// Dust Front's slow pivots churn soft dark dust. Here each track's own ground speed (the hull's plus or minus the turn at
// the track, as TankRenderer drives the treads), plus the scuff of a skid-steer turn, which drags both tracks sideways.
// The dust drawings are lane/show/pipe-vfx's: it calls this when it lands (docs/inbox/2026-09-29-show-pipe-vfx-track-dust.md);
// until then nothing does.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public static class TrackDust
    {
        /// <summary>Metres a second of a track over the ground below which it throws nothing (the old gate's 0.25), and at
        /// which it throws all it can.</summary>
        public const float StartSpeed = 0.25f, FullSpeed = 2f;
        /// <summary>How much a turn's sideways drag counts against the track's own run: the ends of a pivoting hull sweep
        /// the ground crosswise, which throws more dust than rolling over it.</summary>
        public const float Scuff = 0.6f;

        /// <summary>The dust of the left and right track, 0 to 1, from the hull's speed (m/s), its turn (rad/s) and half
        /// its track gauge (m). Signs as TankRenderer's treads: left = speed + turn x gauge, right = speed - turn x gauge.</summary>
        public static (float Left, float Right) Weigh(float speed, float yawRate, float halfGauge)
        {
            float turn = yawRate * halfGauge;
            float scuff = Scuff * Mathf.Abs(turn);
            return (Share(Mathf.Abs(speed + turn) + scuff), Share(Mathf.Abs(speed - turn) + scuff));
        }

        static float Share(float ground) => Mathf.Clamp01((ground - StartSpeed) / (FullSpeed - StartSpeed));
    }
}
