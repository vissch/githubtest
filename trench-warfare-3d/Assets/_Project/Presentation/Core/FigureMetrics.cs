// Phase: B3 (implemented) — how big a man is drawn, in one place. VATRenderer draws him at this size and the unit
// picker (TW.UI, which cannot reference TW.Presentation.Units) finds him at it. Until 2026-09-25 the picker kept its
// own copy, 1.5, which went stale when the owner made the infantry 25% shorter (VATRenderer 1.125): every pick
// radius was a third too big and a click beside a man still selected him. SelectionTests pins the two together.
using UnityEngine;

namespace TW.Presentation
{
    public static class FigureMetrics
    {
        /// <summary>A man's drawn size as a multiple of his 1.78 m model: 1.125 draws him 2.0 m, the spacing
        /// SeparationJob keeps between two men in a trench (owner, 2026-09-23: infantry 25% shorter).</summary>
        public const float UnitScale = 1.125f;
        /// <summary>Men are drawn at x1 up to this zoom, then grow in proportion so they stay readable far out.</summary>
        public const float GrowFromZoom = 24f;
        /// <summary>The most a man grows with the zoom.</summary>
        public const float MaxGrow = 4f;

        /// <summary>How much a man is grown at this camera zoom, with the renderer's default settings.</summary>
        public static float Grow(float zoom) => Mathf.Clamp(zoom / Mathf.Max(1f, GrowFromZoom), 1f, MaxGrow);
    }
}
