// Phase: M2 look (2026-09-23) — what a battlefield is made of, as data. Three of them: the night mud field the game
// has always had, a lava field and a winter mountain (docs/18).
//
// Why this exists. Before it, the look of the world was an enum with two values and a method that assigned nineteen
// fields by literal (Atmosphere.ApplyNight), plus a bool called SceneMood.Night read at twenty-odd call sites in
// CombatFx, TankRenderer and AnimationController. A third look added that way is a third hardcoded method and a
// growing switch, and — worse — every one of those twenty sites is a BINARY decision that a third biome breaks
// silently: a lava field is not "night", so it would quietly take the daylight glow values and the daylight smoke.
//
// So the bool stays, and it keeps the only meaning it ever really had: IS IT DARK. The night field is dark. The lava
// field is dark too — it is lit from below by its own floor, which is exactly the case those glow branches were
// tuned for. The winter field is an overcast day. Everything else that used to be implied by "night" is a field
// here, named, with a value per biome.
//
// The rule this file is written to keep: adding a fourth biome must be adding a fourth entry below and nothing else.
// If it ever needs a change outside this file, the design has failed.
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public enum Biome { NightMud, Lava, Winter }

    /// <summary>Everything that differs between one battlefield and another. Pure data: Atmosphere.Apply copies it in.</summary>
    public sealed class BiomeProfile
    {
        public Biome Id;
        public string Name;

        /// <summary>Dark enough that muzzle flashes, lamps and fires are the bright things on screen. Drives SceneMood.Night.</summary>
        public bool Dark;

        /// <summary>
        /// How bright fires, flashes and bursts are against this field, 0..1 over the night field's values.
        /// Dark was doing two jobs - "is it dark" and "how bright are the fires" - and winter is the case that
        /// separates them: an overcast snowfield is not dark, but its lamps are still the only warm thing in a
        /// world of one hue and must stay bright. Without this a shell burst on snow drops from 7 to 2.5 and
        /// nearly vanishes against a median of 0.55.
        /// </summary>
        public float GlowScale = 1f;

        // ---- the air, the key light and the grade: the fields Atmosphere used to assign by hand
        public Color Haze, Key, Ambient, ShadeTint, SkyMirror, Mist, Bank;
        public float KeyIntensity, ShadowStrength, Depth, MistDensity;
        public Vector3 KeyEuler;
        public float Wetness, WetGlint, Rain;
        public float Exposure, Contrast, Saturation, Vignette, Bloom;
        public bool Filmic;
        public Vector4 GradeShadows, GradeMids, GradeHighs;

        /// <summary>
        /// Light coming back UP off the ground into everything standing on it, against the sky's light falling down.
        /// ALPHA IS THE STRENGTH OF THE BOUNCE, not a flag: 0 is off, and the lava floor throws far more than mud does.
        /// A single flat ambient leaves an unlit plane with no form at all; night and mud hide that, a flat-lit
        /// snowfield does not, and on the lava field this is not a refinement but the entire key light.
        /// Alpha 0 = unset, and the shading is exactly what it was before this existed.
        /// </summary>
        public Color GroundLight = new Color(0f, 0f, 0f, 0f);

        // ---- snow. Coverage 0 means every term below is skipped by a uniform branch and costs nothing.
        public float SnowCoverage;
        /// <summary>The normal.y below which a face sheds its snow. 0.35 leaves snow on gentle slopes, 0.8 only on flat tops.</summary>
        public float SnowShedBelow = 0.35f;
        public float SnowSparkle, SnowBreakup = 0.35f;
        public Color SnowColor = new Color(0.90f, 0.93f, 0.97f, 0.25f);   // a = the gloss it adds

        // ---- molten ground. Strength 0 means the same.
        public float HeatStrength;
        /// <summary>Crust plates per metre. Low numbers give big plates and long cracks.</summary>
        public float HeatPlates = 0.11f;
        public float HeatCrackWidth = 0.055f;
        /// <summary>World Y of the molten level: the glow fades out above it, so a sandbag glows at its foot, not its top.</summary>
        public float MoltenLevel;
        public Color HeatColor = new Color(1.0f, 0.42f, 0.10f, 1f);

        /// <summary>
        /// Multiplies the ground and everything standing on it. The mud palette is baked on the CPU and is the
        /// same on every biome; this is what turns it to basalt or to old packed snow without re-baking it.
        /// Alpha 0 = unset and the world keeps its own colour exactly.
        /// </summary>
        public Color WorldTint = new Color(1f, 1f, 1f, 0f);

        /// <summary>
        /// Local lights (lanterns, flashes, flares) are multiplied by the albedo they land on. Snow's albedo is
        /// about three times mud's, so the lamps that read as small warm pools on the night field spread large
        /// salmon stains across a snowfield - and warm stains are exactly what winter's single-hue look cannot
        /// afford. 1 leaves them as they have always been.
        /// </summary>
        public float LampScale = 1f;

        /// <summary>Handed to DebrisRenderer.Biome: rgb tints every thrown piece, a is a floor under its ember glow.</summary>
        public Color DebrisTint = new Color(1f, 1f, 1f, 0f);

        /// <summary>
        /// What a lightning flash turns the sky, the shaded planes and the mirrored surfaces toward. These were three
        /// literals in Atmosphere, all of them night-blue, and the lava field is Dark with WantsStorm - so every
        /// strike would have lerped its magenta fog toward cold blue-grey and read as a colour glitch rather than as
        /// weather. The reference's lightning is near-white against pink, which is also why it reads at all.
        /// </summary>
        public Color FlashSky = new Color(0.40f, 0.48f, 0.66f);
        public Color FlashShade = new Color(0.60f, 0.66f, 0.85f);
        public Color FlashMirror = new Color(0.85f, 0.90f, 1.00f);

        /// <summary>How much of a shell hole stands full of water. The night field floods 78% of them; lava floods none.</summary>
        public float Flooding;

        // ---- which weather and light components the field wants standing in it
        public bool WantsLamps, WantsRain, WantsStorm;

        public static BiomeProfile For(Biome b)
        {
            switch (b)
            {
                case Biome.Lava: return Lava();
                case Biome.Winter: return Winter();
                default: return NightMud();
            }
        }

        /// <summary>The field the game has always had. These are exactly the values Atmosphere.ApplyNight assigned.</summary>
        public static BiomeProfile NightMud() => new BiomeProfile
        {
            Id = Biome.NightMud, Name = "night mud", Dark = true,
            Haze = new Color(0.075f, 0.105f, 0.17f),
            Key = new Color(0.56f, 0.70f, 1.0f), KeyIntensity = 1.0f, ShadowStrength = 0.66f,
            KeyEuler = new Vector3(30f, 122f, 0f),
            Ambient = new Color(0.10f, 0.13f, 0.20f),
            ShadeTint = new Color(0.20f, 0.29f, 0.56f),
            SkyMirror = new Color(0.44f, 0.54f, 0.74f),
            Wetness = 0.85f, WetGlint = 0.75f, Rain = 0.65f,
            Depth = 230f,
            Mist = new Color(0.17f, 0.23f, 0.35f), MistDensity = 0.55f,
            Bank = new Color(0.10f, 0.14f, 0.22f),
            Exposure = 0.62f, Filmic = true, Contrast = 20f, Saturation = 4f, Vignette = 0.34f, Bloom = 0.9f,
            GradeShadows = new Vector4(0.92f, 0.98f, 1.12f, 0f),
            GradeMids = new Vector4(0.98f, 1.0f, 1.04f, 0f),
            GradeHighs = new Vector4(1.08f, 1.0f, 0.90f, 0f),
            // the one change to the night look, and it is deliberate: the ground now gives a little cold light back
            // rather than none at all, so a plane facing away from the moon still has form. Kept very dark, because
            // the night field's whole character is that the lamps are the brightest things on it.
            GroundLight = new Color(0.055f, 0.065f, 0.085f, 0.25f),
            Flooding = 0.78f,
            WantsLamps = true, WantsRain = true, WantsStorm = true,
        };

        /// <summary>
        /// The lava field. The key light is UNDER THE FLOOR — that is the whole design, and everything else follows
        /// from it. The directional light is kept weak and high so it does almost nothing; the ground light does the
        /// work. Fog is magenta and, crucially, BRIGHT, because it is lit from within by the floor it lies on.
        /// </summary>
        public static BiomeProfile Lava() => new BiomeProfile
        {
            Id = Biome.Lava, Name = "lava", Dark = true,
            Haze = new Color(0.68f, 0.24f, 0.42f),
            // a thin, high, cold key so silhouettes still separate against the fog; the floor does the lighting
            // There is not one cast shadow in the lava reference: the spikes standing in the river have no dark
            // pool at their feet. A shadow lying across emissive ground is the most obviously wrong thing this
            // biome can do, and with 3,000 men there would be 3,000 of them.
            Key = new Color(0.85f, 0.55f, 0.80f), KeyIntensity = 0.18f, ShadowStrength = 0.10f,
            KeyEuler = new Vector3(62f, 200f, 0f),
            Ambient = new Color(0.34f, 0.12f, 0.24f),
            ShadeTint = new Color(0.82f, 0.30f, 0.46f),   // shaded planes see the magenta sky, not the orange floor
            SkyMirror = new Color(0.85f, 0.33f, 0.58f),
            Wetness = 0f, WetGlint = 0f, Rain = 0f,
            Depth = 190f,
            Mist = new Color(0.87f, 0.35f, 0.55f), MistDensity = 0.62f,
            Bank = new Color(0.80f, 0.28f, 0.50f),
            // No vignette: measured, the reference's corners are BRIGHTER than its centre. Fog lights the edges.
            Exposure = 0.70f, Filmic = true, Contrast = 20f, Saturation = 22f, Vignette = 0.12f, Bloom = 1.35f,
            GradeShadows = new Vector4(1.12f, 0.88f, 1.02f, 0f),
            GradeMids = new Vector4(1.06f, 0.96f, 0.98f, 0f),
            GradeHighs = new Vector4(1.06f, 1.00f, 0.90f, 0f),
            // Hue 350, not 15: what arrives on an underside in the reference is red, not orange, and its blue
            // channel is never 0.06 - no pure orange occurs anywhere in that image.
            GroundLight = new Color(0.80f, 0.22f, 0.16f, 1.2f),
            WorldTint = new Color(0.34f, 0.28f, 0.27f, 1f),   // mud, taken most of the way to cold basalt
            HeatStrength = 0.85f, HeatPlates = 0.075f, HeatCrackWidth = 0.032f, MoltenLevel = 0.6f,
            // An EMISSION colour, not the reference's final pixel colour. I had pasted the measured crack body
            // (0.95, 0.38, 0.42) straight in, but that number is what the reference shows AFTER its fog, its
            // grade and its bloom; used as emission and then pushed through Saturation +22 and Bloom 1.35 it
            // came out magenta, and the field read as neon piping rather than molten rock.
            HeatColor = new Color(1.0f, 0.42f, 0.14f, 1f),
            FlashSky = new Color(0.95f, 0.72f, 0.86f), FlashShade = new Color(1.00f, 0.82f, 0.92f),
            FlashMirror = new Color(1.00f, 0.92f, 0.97f),   // near-white against pink, never blue
            DebrisTint = new Color(0.18f, 0.15f, 0.14f, 0.45f),   // basalt that never quite cools
            Flooding = 0f,                                        // nothing stands in water on a lava field
            WantsLamps = true, WantsRain = false, WantsStorm = true,
        };

        /// <summary>
        /// The winter mountain. Overcast: the light is flat and everywhere, which is why the hemisphere term matters
        /// more here than anywhere else — it is the only thing giving an unlit plane any form at all. The danger in
        /// this biome is not darkness, it is mush.
        /// </summary>
        public static BiomeProfile Winter() => new BiomeProfile
        {
            Id = Biome.Winter, Name = "winter", Dark = false, GlowScale = 0.8f, LampScale = 0.34f,
            // Deliberately DARKER than the sky. At 0.72 the fog was brighter than what the mountains stand
            // against, so the horizon would blow out and W6 would be unbuildable.
            Haze = new Color(0.63f, 0.70f, 0.79f),
            Key = new Color(0.78f, 0.86f, 1.00f), KeyIntensity = 0.50f, ShadowStrength = 0.15f,
            KeyEuler = new Vector3(44f, 150f, 0f),
            Ambient = new Color(0.52f, 0.60f, 0.72f),
            ShadeTint = new Color(0.62f, 0.70f, 0.84f),
            SkyMirror = new Color(0.69f, 0.73f, 0.79f),   // measured: the sky is the LEAST blue thing in frame
            Wetness = 0.20f, WetGlint = 0.30f, Rain = 0f,
            Depth = 150f,                                          // the blizzard eats the distance
            Mist = new Color(0.64f, 0.75f, 0.90f), MistDensity = 0.72f,
            Bank = new Color(0.62f, 0.72f, 0.86f),
            // Contrast 12, not 26. I had set the HIGHEST contrast of the three biomes for the LOWEST-contrast
            // reference, to fight mush - and it is the wrong lever. 79% of the reference sits inside an 79-step
            // window, its darkest 1% is 0.14 and not 0. At 26 the ice glare clips and the trench crushes to
            // black. Mush is beaten locally, with occlusion and the hemisphere split, not with a global curve.
            Exposure = 0.92f, Filmic = true, Contrast = 12f, Saturation = -28f, Vignette = 0.05f, Bloom = 0.55f,
            GradeShadows = new Vector4(0.90f, 0.97f, 1.12f, 0f),
            GradeMids = new Vector4(0.99f, 1.0f, 1.03f, 0f),
            GradeHighs = new Vector4(1.02f, 1.01f, 1.0f, 0f),
            // 0.30, not 0.50. At 0.50 against a sky near 0.76 the hemisphere can only ever produce a 1.39:1
            // top-to-side range; the reference measures 1.7:1 on a sandbag and 2.26:1 on a dugout wall. The old
            // value was the direct cause of the mush I had raised Contrast to 26 to fight.
            GroundLight = new Color(0.30f, 0.34f, 0.41f, 0.9f),
            WorldTint = new Color(0.62f, 0.67f, 0.76f, 1f),   // the earth that shows through is cold, not brown
            SnowCoverage = 1.0f, SnowShedBelow = 0.22f, SnowSparkle = 0.15f, SnowBreakup = 0.45f,
            SnowColor = new Color(0.93f, 0.95f, 0.99f, 0.10f),   // fresh snow is matt; the glare belongs on the ice
            DebrisTint = new Color(0.80f, 0.84f, 0.90f, 0f),
            Flooding = 0.25f,                                      // what water there is has frozen; see docs/18 W2
            WantsLamps = true, WantsRain = false, WantsStorm = false,
        };
    }
}
