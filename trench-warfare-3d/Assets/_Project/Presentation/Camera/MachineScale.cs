// Phase: units-one-size (implemented) — a machine's twin of FigureMetrics.Grow/DrawnHeightM: how big a tank or
// walker is drawn, at one zoom, in one place. One size at every zoom (owner, 2026-10-13); a grow put back here is
// the drawn size.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public static class MachineScale
    {
        /// <summary>A machine is one size at every zoom now (owner, 2026-10-13). A grow put back here is the drawn size.</summary>
        public static float Grow(float zoom) => 1f;

        /// <summary>The hull's root scale: wide/tall from the hop squash, times the zoom grow.</summary>
        public static Vector3 RootScale(float hopSquash, float zoom)
        {
            float wide = hopSquash == 1f ? 1f : 1f / Mathf.Sqrt(Mathf.Max(0.5f, hopSquash));
            float grow = Grow(zoom);
            return new Vector3(wide * grow, hopSquash * grow, wide * grow);
        }

        /// <summary>How tall the hull top is drawn above the ground (VehicleSize is baked into model.Height at load).</summary>
        public static float DrawnHeightM(TankModel model, float zoom) => model.Height * Grow(zoom);
    }
}
