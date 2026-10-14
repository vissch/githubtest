// Phase: gore-by-zoom (2026-10-13, the owner: "the red gore mesh coming out of the shot units is sometimes a bit big,
// we want this to scale based off the zoom"). The ONE place every red piece's size lives, as a share of the drawn man
// (FigureMetrics.HeightM, 2.0 m, one size at every zoom since 2026-10-13), and the one curve that takes it with the zoom.
//
// My reading of "scale based off the zoom", not his words: close in and at the zoom the game is played at, the gore is
// true to the man — never a larger share of his height than the shares named below. Pulled out past TrueUntilZoom it
// may grow, smoothly, just enough to stay readable, and never past MostGrow. Cap() is the hard ceiling: no caller can
// draw a red piece bigger than that, at any zoom.
//
// Part 1 measured what was there (commit a6312e1b, gore.txt): the lumps were already one size at every zoom (mean 8.1%
// of the man, largest 14.7%) — the one-size change had taken their grow away with the man's. What was 'a bit big' was
// the BloodSpurt card: 1.26-2.16 m at the play zoom, taller than the 2.00 m man himself. The shares here put the
// biggest card at 0.58 of him (1.16 m), and its cap at 0.87 of him, so no card is ever taller than the man.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public static class GoreScale
    {
        /// <summary>At or below this zoom the gore is true to the man: Size is exactly 1 (the play zoom is 30).</summary>
        public const float TrueUntilZoom = 30f;
        /// <summary>Zoom units over which the gore doubles once it is past TrueUntilZoom (so x1.30 at zoom 120).</summary>
        public const float GrowOver = 300f;
        /// <summary>The cap: the most the zoom may ever grow a red piece (reached at zoom 180 and held to 600).</summary>
        public const float MostGrow = 1.5f;

        /// <summary>The splash blobs where the round met him (part 1: 12.4% of the man, kept).</summary>
        public const float BlobShare = 0.12f;
        /// <summary>One droplet of the spray out of the far side of him (part 1: 5-8% of the man, kept).</summary>
        public const float DropShare = 0.06f;
        /// <summary>One gore lump off a death: a gib's trail, a popped head, a body under a track (part 1: 4-8%).</summary>
        public const float LumpShare = 0.0525f;
        /// <summary>The BloodSpurt card of a light hit, and what a heavy one adds: the piece that was too big.</summary>
        public const float CardShare = 0.40f, CardHeavyShare = 0.18f;
        /// <summary>The sniper's bigger card (CombatFx's BloodSnipe book), still under the man at its cap.</summary>
        public const float BigCardShare = 0.55f;

        /// <summary>How much larger the zoom draws a red piece: 1 up to TrueUntilZoom, then a smooth grow, MostGrow at most.</summary>
        public static float Size(float zoom) => Mathf.Clamp(1f + (zoom - TrueUntilZoom) / GrowOver, 1f, MostGrow);

        /// <summary>A red piece's size in metres: its share of the man's height, taken with the zoom.</summary>
        public static float Sized(float share, float zoom) => share * FigureMetrics.HeightM * Size(zoom);

        /// <summary>The most that share can ever be drawn at, in metres, at any zoom. Nothing may go past it.</summary>
        public static float Cap(float share) => share * FigureMetrics.HeightM * MostGrow;
    }
}
