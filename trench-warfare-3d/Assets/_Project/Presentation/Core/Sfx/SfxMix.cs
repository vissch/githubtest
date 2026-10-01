// Phase: B7 (2026-10-01, first pass) — how loud a sound is where the player is looking, pure (EventAudioRouter plays
// what this decides). The camera looks down from high: what you hear is centred where it looks (the focus), not where the
// lens is, and the higher it is the wider the field you hear. Each kind of sound has its own carry (a shell is heard
// across the map, a rifle across a few bays) and its own voice budget, so a machine gun on a line cannot take every
// voice from the shells.
namespace TW.Presentation.Audio
{
    public enum SfxGroup : byte { SmallArms, Blasts, Guns, Metal, Misc, Count }

    public static class SfxMix
    {
        /// <summary>The group a sound plays in (its voice budget).</summary>
        public static SfxGroup GroupOf(Sfx s)
        {
            switch (s)
            {
                case Sfx.Rifle: case Sfx.Mg: case Sfx.Snap: return SfxGroup.SmallArms;
                case Sfx.BoomSmall: case Sfx.BoomBig: case Sfx.CookOff: return SfxGroup.Blasts;
                case Sfx.TankGun: return SfxGroup.Guns;
                case Sfx.Clang: case Sfx.Ricochet: return SfxGroup.Metal;
                default: return SfxGroup.Misc;
            }
        }

        /// <summary>Most voices a group may hold at once (the pool is Voices).</summary>
        public static int Budget(SfxGroup g)
        {
            switch (g)
            {
                case SfxGroup.SmallArms: return 10;
                case SfxGroup.Blasts: return 8;
                case SfxGroup.Guns: return 4;
                case SfxGroup.Metal: return 4;
                default: return 6;
            }
        }
        public const int Voices = 32;
        /// <summary>Most new sounds started in one frame (a barrage's tick, a volley's).</summary>
        public const int StartsPerFrame = 10;

        /// <summary>How far a sound carries, as a share of what the view hears (Hearing): small arms the near field,
        /// blasts and guns well past the edge of the view.</summary>
        public static float Carry(Sfx s)
        {
            switch (GroupOf(s))
            {
                case SfxGroup.SmallArms: return s == Sfx.Snap ? 0.12f : 0.5f;
                case SfxGroup.Blasts: return 1.6f;
                case SfxGroup.Guns: return 1.3f;
                case SfxGroup.Metal: return 0.45f;
                default: return s == Sfx.Whistle ? 0.6f : 0.5f;
            }
        }

        /// <summary>The field the view hears (m) at a camera this high over its focus: wider as it pulls back.</summary>
        public static float Hearing(float height) => System.Math.Max(30f, height * 1.3f);

        /// <summary>The gain (0..1) of a sound `distance` m from the focus, with the camera `height` m up: 1 at the
        /// focus, half at its carry, falling as the square past it; under Floor it is not played at all.</summary>
        public static float Gain(Sfx s, float distance, float height)
        {
            float reach = Hearing(height) * Carry(s);
            float r = distance / reach;
            float g = 1f / (1f + r * r);
            // pulled far back, everything is a little quieter (the overview is not the trench)
            g *= 1f - 0.35f * Clamp01((height - 60f) / 180f);
            return g < Floor ? 0f : g;
        }
        public const float Floor = 0.02f;

        /// <summary>Whether a sound is heard as its far form (low-passed, smeared): past half its reach.</summary>
        public static bool Far(Sfx s, float distance, float height) => distance > Hearing(height) * Carry(s) * 0.5f;

        /// <summary>Seconds a sound takes to arrive: sound travels 343 m/s, and only blasts and guns are heard far enough
        /// off for it to show (a rifle's delay would only blur a volley).</summary>
        public static float Delay(Sfx s, float distance)
        {
            var g = GroupOf(s);
            return g == SfxGroup.Blasts || g == SfxGroup.Guns ? distance / 343f : 0f;
        }

        /// <summary>Stereo pan from where the sound is across the screen (viewport x, 0 left .. 1 right), never hard.</summary>
        public static float Pan(float viewportX) => Clamp((viewportX - 0.5f) * 1.6f, -0.85f, 0.85f);

        /// <summary>The least time between two sounds from one shooter (a machine gun's stream thinned to what reads).</summary>
        public static float Spacing(Sfx s) => s == Sfx.Mg ? 0.06f : s == Sfx.Rifle ? 0.12f : 0f;

        static float Clamp01(float x) => x < 0f ? 0f : x > 1f ? 1f : x;
        static float Clamp(float x, float a, float b) => x < a ? a : x > b ? b : x;
    }
}
