// Phase: maintenance (2026-09-27) — how far the camera looks before its view meets the ground plane (y = 0).
// The shake, the ambient kicks, the storm's bolts, the fog and the depth of field all need it; it was written out
// seven times with two different clamps. A view flatter than the clamp is capped at height / minDrop, so the distance
// stays finite when the camera looks at the horizon. Atmosphere passes 0.12 (the fog and focus it has always had);
// everything else uses MinDrop. The two differ only below 8.6 degrees of pitch (TacticalCamera allows 8).
using UnityEngine;

namespace TW.Presentation
{
    public static class ViewGround
    {
        /// <summary>The steepest-drop clamp most callers use: sin 8.63 degrees.</summary>
        public const float MinDrop = 0.15f;

        /// <summary>Distance along <paramref name="forward"/> from a point <paramref name="height"/> above y = 0 to that plane.</summary>
        public static float Along(float height, Vector3 forward, float minDrop = MinDrop) => height / Mathf.Max(minDrop, -forward.y);

        /// <summary>How far the lens looks before its view meets the ground plane.</summary>
        public static float Distance(Transform lens, float minDrop = MinDrop) => Along(lens.position.y, lens.forward, minDrop);

        /// <summary>Where the lens's view meets the ground plane.</summary>
        public static Vector3 Point(Transform lens, float minDrop = MinDrop) => lens.position + lens.forward * Distance(lens, minDrop);
    }
}
