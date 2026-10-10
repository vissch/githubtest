// Phase: B5 (the owner's idea of 2026-10-08, concept option A) — a trench's flag pole as arithmetic: where it
// stands, how big it is, which colour the cloth is, how long the old flag takes to come down and the new one to go
// up, and how the pole breaks. Plain numbers, no scene, no Unity object: TrenchFlags draws from this and
// TrenchFlagTests pins it down. Shaped on TrenchSectionRules, whose IsHeavy decides "heavy ordnance takes it
// outright" here too.
//
// The sizes are the concept sheet's SIZES panel for option A (docs/design/idea-a-frog-raises-a-flag-when-a-trench-
// is-ta-concept.md): a stout pole 3.1 m, a stiff pennant 3.3 x 2.0 m, the frog 1.0 m at its base hauling a rope.
// The two cloths are 2.66:1 apart in contrast (red 0.4021, blue 0.1202 in relative luminance), so the front still
// reads when the night grade pulls the colour out of both.
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    /// <summary>A pole's life: standing, snapped off at the butt (no cloth, a stump), blown away.</summary>
    public enum FlagState : byte { Intact = 0, Snapped = 1, Gone = 2 }

    public static class TrenchFlagRules
    {
        /// <summary>Option A's sizes, in metres (the concept's SIZES panel).</summary>
        public const float PoleHeight = 3.1f, FlagWide = 3.3f, FlagTall = 2.0f, FrogHeight = 1.0f;
        /// <summary>What is left of the pole once it is snapped.</summary>
        public const float StumpHeight = 0.55f;
        /// <summary>How far onto the parapet, along the trench's facing, the pole stands: far enough to be out of the
        /// bay the men walk, near enough to belong to the trench.</summary>
        public const float ParapetOffset = 1.1f;

        /// <summary>The taker's cloth. Team 0 is blue and team 1 red, the mapping TankRenderer.TeamA/TeamB and
        /// dustfront.tokens.uss already use; 255 (neutral) flies nothing.</summary>
        public static readonly Color ClothRed = new Color(1f, 138f / 255f, 92f / 255f);     // #ff8a5c
        public static readonly Color ClothBlue = new Color(52f / 255f, 99f / 255f, 155f / 255f); // #34639b

        /// <summary>Seconds the new flag takes to the top, and seconds the old one is in the air coming down.</summary>
        public const float RiseSeconds = 1.4f, FallSeconds = 2.2f;
        /// <summary>The strength of a pole. A grenade leaves it standing; two shake it down.</summary>
        public const float MaxHp = 1.0f;
        /// <summary>Turns a second of a tumbling fall is worth.</summary>
        public const float FallSpin = 1.35f;
        /// <summary>How near the parapet the cut flag's bottom edge ever comes. It is airborne the whole way: it
        /// lets go, tumbles clear and is still in the air when it stops being drawn, never sunk into the sandbags.</summary>
        public const float FallClearance = 0.45f;
        /// <summary>How far out from the pole the cut flag has swung by the time it is let go of.</summary>
        public const float FallSwing = 1.9f;
        /// <summary>Past this the pole is too thin to see: the flag is drawn Swell times bigger and the pole dropped.</summary>
        public const float PoleFadeMeters = 150f, Swell = 1.3f;

        public static Color Cloth(int team) => team == 1 ? ClothRed : ClothBlue;
        public static bool Flies(int team) => team == 0 || team == 1;

        /// <summary>Where a trench's pole stands: the middle cell of its chain, then ParapetOffset along its facing,
        /// so the pole is on the parapet and not in the bay. Y is the ground there.</summary>
        public static Vector3 Anchor(TrenchDef trench, MapData map)
        {
            int count = Mathf.Max(1, trench.CellCount);
            int cell = map.TrenchCells[trench.CellStart + count / 2];
            var mid = map.NavCellCenter(cell);
            float x = mid.x + Mathf.Sin(trench.FacingYaw) * ParapetOffset;
            float z = mid.z + Mathf.Cos(trench.FacingYaw) * ParapetOffset;
            return new Vector3(x, map.Height.Sample(x, z), z);
        }

        /// <summary>The new flag's climb, 0 at the butt to 1 at the top: hauled hard and eased into the stop, so the
        /// hoist is read as a pull and not as a slide.</summary>
        public static float Rise(float seconds)
        {
            float t = Mathf.Clamp01(seconds / RiseSeconds);
            return 1f - (1f - t) * (1f - t);
        }

        /// <summary>How high the old flag still is, 1 at the top to 0 on the ground: cut loose and falling, so it
        /// gathers speed instead of sinking.</summary>
        public static float Fall(float seconds)
        {
            float t = Mathf.Clamp01(seconds / FallSeconds);
            return 1f - t * t;
        }

        /// <summary>Where the cut flag's hoist edge is, in metres above the pole's butt, having let go at
        /// <paramref name="top"/>. It falls to a floor that keeps its bottom edge FallClearance over the parapet,
        /// so the whole drop is airborne — the flag is read as flying off, not as melting into the ground.</summary>
        public static float FallHeight(float seconds, float top)
        {
            float floor = Mathf.Min(FlagTall + FallClearance, top);
            return floor + (top - floor) * Fall(seconds);
        }

        /// <summary>The lowest point of the cut flag at that moment: its bottom edge, FlagTall under the hoist.</summary>
        public static float FallLowest(float seconds, float top) => FallHeight(seconds, top) - FlagTall;

        /// <summary>How far out from the pole the cut flag has swung: none at the moment it lets go, FallSwing by
        /// the end, so it is plainly clear of the pole rather than scraping down it.</summary>
        public static float FallOut(float seconds) => (1f - Fall(seconds)) * FallSwing;

        /// <summary>Radians the falling flag has turned by.</summary>
        public static float FallAngle(float seconds) => Mathf.Clamp(seconds, 0f, FallSeconds) * FallSpin * Mathf.PI * 2f;

        /// <summary>The cloth's relative luminance (WCAG: the sRGB channels linearised and weighted). The pair's
        /// contrast is what tells the sides apart once the grade has eaten the hue.</summary>
        public static float Luminance(Color c)
            => 0.2126f * Linear(c.r) + 0.7152f * Linear(c.g) + 0.0722f * Linear(c.b);

        static float Linear(float channel)
            => channel <= 0.04045f ? channel / 12.92f : Mathf.Pow((channel + 0.055f) / 1.055f, 2.4f);

        /// <summary>How far apart two cloths read (WCAG contrast, 1:1 the same, higher better).</summary>
        public static float Contrast(Color a, Color b)
        {
            float la = Luminance(a) + 0.05f, lb = Luminance(b) + 0.05f;
            return la > lb ? la / lb : lb / la;
        }

        /// <summary>A field stake: what a retaken trench raises its colour on while the pole is gone. Short, so the
        /// cloth is cut down with it (StakeFlagScale) instead of dragging in the mud.</summary>
        public const float StakeHeight = 1.4f, StakeFlagScale = 0.45f;
        /// <summary>How long the piece that snaps off is: everything over the stump.</summary>
        public static float FallenLength => PoleHeight - StumpHeight;
        /// <summary>How thick the rag on the mud lies, and how far from the butt it ends up.</summary>
        public const float RagLift = 0.07f, RagOut = 0.8f;
        /// <summary>What one shell throws off a pole as it goes: splinters of the shaft, and shreds of the cloth.</summary>
        public const int SplinterPieces = 7, ClothPieces = 3;
        /// <summary>How much of the cloth's colour one shell burns out of it, and the colour it burns toward. A rag
        /// on the mud starts here; standing in fire takes it the rest of the way to charcoal.</summary>
        public const float ScorchPerHit = 0.55f;
        public static readonly Color Char = new Color(0.26f, 0.21f, 0.17f);   // charred cloth, not a hole: a rag has to read on night mud

        /// <summary>How high whatever is left of the pole stands.</summary>
        public static float Standing(FlagState state) => state == FlagState.Intact ? PoleHeight : StumpHeight;

        /// <summary>Where a flag's hoist tops out on this pole: the pole's own head while it stands, the field
        /// stake's head once it is blown away, and nothing at all on a snapped one (the flag went down with it).</summary>
        public static float HoistTop(FlagState state)
            => state switch { FlagState.Intact => PoleHeight - 0.08f, FlagState.Gone => StakeHeight - 0.06f, _ => 0f };

        /// <summary>How big the cloth is drawn on that pole: full size on the pole, cut down on the field stake.</summary>
        public static float ClothScale(FlagState state) => state == FlagState.Gone ? StakeFlagScale : 1f;

        /// <summary>The harm a blast does a pole at that distance, falling off the way PropDestruction's does, so a
        /// pole and the lining beside it are shaken down by the same arithmetic.</summary>
        public static float Harm(float power, float distance, float reach)
            => distance > reach || reach <= 0f ? 0f : power * (1f - 0.75f * distance / reach);

        /// <summary>Which way the wreck is thrown: from the blast toward the pole. SimEvent.Explosion carries no
        /// direction today (Dir is zero), so the line between the two points is the only honest one.</summary>
        public static float ThrowYaw(Vector3 blast, Vector3 pole)
        {
            float dx = pole.x - blast.x, dz = pole.z - blast.z;
            return dx * dx + dz * dz < 1e-6f ? 0f : Mathf.Atan2(dx, dz);
        }

        /// <summary>The way the wreck is thrown, flat and a unit long.</summary>
        public static Vector3 ThrowDir(Vector3 blast, Vector3 pole)
        {
            float yaw = ThrowYaw(blast, pole);
            return new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw));
        }

        /// <summary>A cloth with the fire taken out of it: 0 the colour it flew, 1 charcoal.</summary>
        public static Color Scorched(Color cloth, float scorch) => Color.Lerp(cloth, Char, Mathf.Clamp01(scorch));

        /// <summary>One hit on a pole: heavy ordnance takes it outright, anything else snaps it when its strength
        /// runs out and blows the stump away on the next one. A gone pole stays gone.</summary>
        public static FlagState Apply(ref float hp, float harm, bool heavy, FlagState was)
        {
            if (was == FlagState.Gone) { hp = 0f; return FlagState.Gone; }
            if (heavy) { hp = 0f; return FlagState.Gone; }
            hp -= harm;
            if (hp > 0f) return was;
            hp = 0f;
            return was == FlagState.Snapped ? FlagState.Gone : FlagState.Snapped;
        }
    }
}
