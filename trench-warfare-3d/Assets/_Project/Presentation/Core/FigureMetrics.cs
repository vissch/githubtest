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
        /// <summary>The figure as baked (VATBaker normalises every figure to this).</summary>
        public const float BakeHeightM = 1.78f;
        /// <summary>The man's true drawn height at close zoom: one soldier unit for the scale audit (docs/21 phase 1).</summary>
        public const float HeightM = BakeHeightM * UnitScale;
        /// <summary>Historical: men used to be drawn at x1 up to this zoom, then grown in proportion. Kept only
        /// because CombatFx and SelectionTests still read it; it no longer feeds a grow.</summary>
        public const float GrowFromZoom = 24f;
        /// <summary>The owner, 2026-10-13: "the units... should allways be one size". A man no longer grows with
        /// the zoom (overturns the 2026-09-26 decisions.md row "the readability grow is a men-only exception").</summary>
        public const float MaxGrow = 1f;

        /// <summary>A man is one size at every zoom now (owner, 2026-10-13).</summary>
        public static float Grow(float zoom) => 1f;
        /// <summary>How tall he is drawn: always HeightM, at every zoom (owner, 2026-10-13: units are one size).</summary>
        public static float DrawnHeightM(float zoom) => HeightM;
    }
}
