// Phase: B2 (presentation ground shared by terrain, unit placement and combat effects)
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Terrain;

namespace TW.Presentation
{
    public struct RenderGroundGrid
    {
        [ReadOnly] public NativeArray<float> Heights;
        public int Width, Length;
        public float Step;
        public float Sample(float x, float z, float fallback)
        {
            if (!Heights.IsCreated) return fallback;
            float fx = math.clamp(x / Step, 0f, Width - 1f), fz = math.clamp(z / Step, 0f, Length - 1f);
            int x0 = (int)math.floor(fx), z0 = (int)math.floor(fz), x1 = math.min(x0 + 1, Width - 1), z1 = math.min(z0 + 1, Length - 1);
            float tx = fx - x0, tz = fz - z0;
            float a = Heights[z0 * Width + x0], b = Heights[z0 * Width + x1], c = Heights[z1 * Width + x0], d = Heights[z1 * Width + x1];
            // Match the terrain's triangle diagonal exactly, rather than bilinear interpolation through the quad.
            return tx + tz <= 1f ? a + (b - a) * tx + (c - a) * tz : d + (c - d) * (1f - tx) + (b - d) * (1f - tz);
        }
    }
    /// <summary>The scene's mood, set by Atmosphere; effects in other assemblies (tracers) read it.</summary>
    public static class SceneMood
    {
        public static bool Night;
    }

    /// <summary>
    /// The colours a biome lends to the effects drawn in another assembly, in ONE place with a version stamp.
    ///
    /// It replaces a static per colour. The first biome colour was pushed as `CombatFx.SplashTint`, and the
    /// consumer then compared `waterMat.color` against it every frame to decide whether to write it — a managed
    /// to native read per tinted material per frame, and a new public static for every colour after it. A biome
    /// owns a dozen of these. Comparing an int instead costs nothing and the set arrives whole.
    ///
    /// Terrain writes it (Atmosphere) and Camera reads it (CombatFx), which is the same direction, and the same
    /// file, as SceneHooks.IsWater already crosses — so no assembly reference changes.
    /// </summary>
    public static class SceneTints
    {
        public struct Set
        {
            /// <summary>What a shell throws out of standing liquid: the chunks AND the column above them.</summary>
            public UnityEngine.Color Splash;
            /// <summary>Earth thrown straight up by a shell, and the wings that spread from its foot.</summary>
            public UnityEngine.Color Column;
            /// <summary>Dry dust: bullet spurts and the small puffs, the most frequent effect on the field.</summary>
            public UnityEngine.Color Dust;
            /// <summary>Shell smoke, which lingers longest and covers the most screen of any of them.</summary>
            public UnityEngine.Color Smoke;
            /// <summary>Multiplies every flash and ember. A dark field wants more; a bright one blows out.</summary>
            public float Glow;
        }

        /// <summary>No biome: the night field's own colours, which is what these effects were authored against.</summary>
        public static readonly Set Default = new Set
        {
            Splash = new UnityEngine.Color(0.62f, 0.70f, 0.82f),
            Column = new UnityEngine.Color(0.40f, 0.33f, 0.26f),
            Dust = new UnityEngine.Color(0.86f, 0.78f, 0.64f),
            Smoke = new UnityEngine.Color(0.34f, 0.32f, 0.29f),
            Glow = 1f,
        };

        public static Set Now = Default;
        /// <summary>Bumped on every push. A consumer keeps its own copy and reapplies when they differ.</summary>
        public static int Epoch;

        public static void Push(in Set s) { Now = s; Epoch++; }
        public static void Reset() => Push(Default);
    }

    /// <summary>
    /// Small services one presentation assembly offers another (combat effects live with the camera, water and lamps
    /// with the terrain). Every member may be null or empty: callers must cope with nobody being there.
    /// </summary>
    public static class SceneHooks
    {
        /// <summary>How close the camera is: 0 at the standard view and beyond, 1 when zoomed in among the men. Small
        /// things (footprints, brass, breath, litter) exist only while this is above 0. (TacticalCamera)</summary>
        public static float CloseUp;
        /// <summary>Is there standing water here? (WaterRings)</summary>
        public static System.Func<float, float, bool> IsWater;
        /// <summary>Open a ring on the water: x, z, size in metres. (WaterRings)</summary>
        public static System.Action<float, float, float> AddRing;
        /// <summary>Throw some sparks from a point. (CombatFx)</summary>
        public static System.Action<UnityEngine.Vector3, int> Sparks;
        /// <summary>Places that smoke or steam gently: dugout chimneys, fires in the rain. (NightLights fills it, CombatFx draws.)</summary>
        public static readonly System.Collections.Generic.List<UnityEngine.Vector3> SmokeSources = new System.Collections.Generic.List<UnityEngine.Vector3>();
        /// <summary>The tanks are drawn from their parts, with their own exhaust, sparks and wrecks: the box vehicle and the
        /// generic vehicle effects stand down. (TankRenderer)</summary>
        public static bool TanksDrawn;
        /// <summary>Half the track gauge (x) and half the track length (y) of the vehicle in a slot, metres: where its ruts
        /// run and where its tracks fling mud. (TankRenderer)</summary>
        public static System.Func<int, UnityEngine.Vector2> VehicleTracks;
        /// <summary>Where a vehicle's machine guns fire from (w = 1), or zero when unknown. (TankRenderer)</summary>
        public static System.Func<int, UnityEngine.Vector4> VehicleGunPort;
        /// <summary>Is a destroyed tank drawn as its own wreck at this x, z? The prop kit then leaves its stand-in out. (TankRenderer)</summary>
        public static System.Func<float, float, bool> DrawnWreck;
        /// <summary>Is this slot the tank TankRenderer draws (alive, or dying in the events being dispatched)? Answered
        /// from its views, which follow the events, so a slot freed by a tank and refilled by a man in the same tick
        /// reads right, where the slot's arrays would already hold the man. (TankRenderer)</summary>
        public static System.Func<int, bool> IsTankSlot;
    }

    public static class RenderGround
    {
        public static MapData Map;
        public static RenderGroundGrid Grid;
        public static float Sample(MapData map, float x, float z)
        {
            float original = map.Height.Sample(x, z);
            return ReferenceEquals(Map, map) ? Grid.Sample(x, z, original) : original;
        }
    }
}
