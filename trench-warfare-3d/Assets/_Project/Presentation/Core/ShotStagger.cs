// Phase: C22 (AOSA, juice J03) - when inside its sim tick each rifle shot is SHOWN.
// The sim fires a whole tick's shots at once, every 50 ms, and they reach the presentation in one frame: every tracer,
// flare and muzzle light of the tick was born on that frame, so a firing line strobed at 20 Hz. Each shot is now shown
// a little late, by a fraction of one tick that its shooter and its tick pick, so a line's fire runs along it as a
// flicker. Presentation only: nothing here is read by the sim, and the hash has no state, so a held-clock still repeats.
// CombatFx (the tracer, the flare, the spurt) and NightLights (the muzzle light) ask the same question and agree.
namespace TW.Presentation
{
    public static class ShotStagger
    {
        /// <summary>The knob: how much of one sim tick the shots of a tick are spread over. 1 = the whole tick,
        /// 0 = every shot at the start of its tick (the look before C22).</summary>
        public const string Knob = "fx.shotStagger";
        public const float DefaultSpread = 1f;

        /// <summary>The knob's value, read once by each reader (Awake/Start), never below 0.</summary>
        public static float ReadSpread()
        {
            float v = Knobs.Get(Knob, DefaultSpread);
            return v > 0f ? v : 0f;
        }

        /// <summary>Where in its tick this shooter's shot of this tick falls: [0, 1). A hash of the tick says where the
        /// tick's first slot falls, and each next slot is a golden-ratio step on from it (a Weyl sequence), so the
        /// neighbours of a firing line never share a moment and any run of slots covers the tick evenly: 40 neighbours
        /// put at most 14 on one held frame of three. A pure function: the same shooter and tick always agree.</summary>
        public static float Fraction(int shooter, uint tick)
        {
            uint h = unchecked(tick * 0x85EBCA77u + 0x632BE5ABu);
            h ^= h >> 16; h = unchecked(h * 0x7FEB352Du);
            h ^= h >> 15; h = unchecked(h * 0x846CA68Bu);
            h ^= h >> 16;
            uint phase = unchecked(h + (uint)shooter * 0x9E3779B9u);   // 2^32 / golden ratio: the step between slots
            return (phase >> 8) * (1f / 16777216f);   // the top 24 bits: exact in a float, and never 1
        }

        /// <summary>Seconds after the event arrives that this shot is shown: [0, spread x tickSeconds).</summary>
        public static float Delay(int shooter, uint tick, float tickSeconds, float spread)
        {
            if (spread <= 0f || tickSeconds <= 0f) return 0f;
            return Fraction(shooter, tick) * spread * tickSeconds;
        }
    }
}
