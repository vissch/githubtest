// Phase: the battle (2026-10-19) — the Hopper's hop arc over a trench or wire (presentation only; the sim path is
// untouched, and by the owner's #d-hopper the Hopper never lands: this is a rise above its 9 m cruise and a settle
// back, with downwash dust blown off the ground beneath it).
// What would go wrong silently: nothing in the renderer answered the ground the Hopper crosses, so it cruised dead
// level over every trench and wire belt. The statics below are pure so an EditMode test can hold the arc's shape.
using UnityEngine;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Terrain;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        /// <summary>Metres above its cruise the Hopper rises at the top of an arc, how long one arc takes, the nose
        /// tilt it gives, and how far ahead of the hull the ground is read.</summary>
        const float HopArcHeight = 3.2f, HopArcSeconds = 1.6f, HopArcTiltMax = 7f * Mathf.Deg2Rad, HopArcLook = 5f;

        /// <summary>Ground worth clearing: a trench, its link, or a wire belt.</summary>
        public static bool HopArcCrossing(byte layer)
            => (layer & (byte)(NavLayer.Trench | NavLayer.Link | NavLayer.Wire)) != 0;

        /// <summary>The rise above cruise at a point in the arc: 0 at both ends, HopArcHeight at the top.</summary>
        public static float HopArcRise(float phase) => HopArcHeight * 4f * phase * (1f - phase);

        /// <summary>Nose up leaving, down settling back (the same shape as the Bullfrog's HopTiltMax).</summary>
        public static float HopArcTilt(float phase) => phase > 0f ? HopArcTiltMax * Mathf.Cos(Mathf.PI * phase) : 0f;

        /// <summary>One step of the arc's phase. A phase of 0 only starts when `crossing`; once it has started it runs
        /// out whatever the ground below, wraps to 0 past the end and sets `landed` on that one frame.</summary>
        public static float HopArcStep(float phase, bool crossing, float dt, out bool landed)
        {
            landed = false;
            if (phase <= 0f && !crossing) return 0f;
            phase += dt / HopArcSeconds;
            if (phase >= 1f) { phase = 0f; landed = true; }
            return phase;
        }

        /// <summary>Where a Hopper is in its arc (0 at rest, or no such hull): what the stills test waits on, so it
        /// photographs the cruise and the top of one arc instead of guessing at frame numbers.</summary>
        public float HopArcPhaseOf(int slot) => views.TryGetValue(slot, out var v) ? v.HopArcPhase : 0f;

        /// <summary>The scene half: read the ground under and ahead of the hull, step the arc, and write the bob and
        /// nose tilt the root matrix reads. Runs after Flair, which leaves a Hopper's Bob at 0 (DriveStyle Chug = 0).</summary>
        void HopArcPose(View v, float dt)
        {
            if (HopDown(v.State, v.Dead)) { v.HopArcPhase = 0f; v.Bob = 0f; v.HopTilt = 0f; return; }
            var map = Host != null && Host.Local != null ? Host.Local.Map : null;
            bool crossing = false;
            if (map != null)
            {
                Vector3 fwd = new Vector3(Mathf.Sin(v.Yaw), 0f, Mathf.Cos(v.Yaw));
                crossing = HopArcCrossing((byte)map.LayerAt(new float3(v.Pos.x, 0f, v.Pos.z)))
                        || HopArcCrossing((byte)map.LayerAt(new float3(v.Pos.x + fwd.x * HopArcLook, 0f, v.Pos.z + fwd.z * HopArcLook)));
            }
            v.HopArcPhase = HopArcStep(v.HopArcPhase, crossing, dt, out bool landed);
            v.Bob = HopArcRise(v.HopArcPhase);
            v.HopTilt = HopArcTilt(v.HopArcPhase);
            if (landed && books != null && books.Ready)
            {
                // downwash from 9 m, not a landing: the same two puffs as the toad's, at half the weight
                Vector3 side = new Vector3(Mathf.Cos(v.Yaw), 0f, -Mathf.Sin(v.Yaw));
                float ground = Ground(v.Pos.x, v.Pos.z);
                for (int k = -1; k <= 1; k += 2)
                    books.Add(FlipbookFx.Book.Puff, new Vector3(v.Pos.x, ground + 0.3f, v.Pos.z) + side * (k * 1.2f), 2.2f, 0.9f,
                              velocity: side * (k * 1.8f) + Vector3.up * 0.6f, grow: 1.5f, alpha: 0.2f);
            }
        }
    }
}
