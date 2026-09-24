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

            /// <summary>
            /// The standing liquid on this field is MOLTEN, not water.
            ///
            /// SceneHooks.IsWater answers "is there liquid here" from the map alone - water level and painted
            /// puddles - and no biome touches it, so on the lava field it is true over the river and reads
            /// 11% of the battlefield. The effects then treat melt as a flooded crater and suppress the fire
            /// and the dust, which is right for water and exactly wrong for rock. Only the biome knows which
            /// liquid it is, so the biome says.
            /// </summary>
            public bool MoltenLiquid;
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
        /// <summary>A flash of light at a place: colour, peak intensity, reach (m), seconds. A tank's gun or a strike on its
        /// plate lights the ground round it the way a shell does. (NightLights lends one of its pooled lights)</summary>
        public static System.Action<UnityEngine.Vector3, UnityEngine.Color, float, float, float> Flash;
        /// <summary>A small burst where the sim has none: a dud shell cooking off in a fire (PropDestruction). Place, radius
        /// (m). Drawn only; the sim never hears of it. (CombatFx)</summary>
        public static System.Action<UnityEngine.Vector3, float> CookOff;
    }

    /// <summary>
    /// What the frame actually submitted. The scoreboard's one absolute condition is that the STANDARD view's cost
    /// never rises - detail for the close tiers is switched by SceneHooks.CloseUp and is supposed to be free at
    /// zoom 30 - and that condition is only as honest as the thing measuring it. BattlefieldProps counts its own
    /// draws, but the props are one submitter of nine, and every close-tier effect (prints, brass, breath, motes,
    /// rats, the flipbooks) lands in ones that counted nothing. A budget check that cannot see where the new work
    /// goes reports "unchanged" and rewards adding cost.
    ///
    /// So every instanced and single-mesh submission in Presentation goes through Draw. DebugOverlay is
    /// deliberately left out: its gizmos are not in the game and a debug key must not move the budget.
    ///
    /// The count is stamped with the frame it belongs to rather than cleared by a caller. There is no point in the
    /// frame where every submitter has finished - they draw from different components' Update and LateUpdate - so
    /// a Reset() from any one of them would split a frame across two readings on script execution order. The first
    /// touch of a new frame publishes the previous one, which makes the reading order-independent, and one frame
    /// behind.
    /// </summary>
    public static class FrameBudget
    {
        static int frame = -1, draws, lastDraws, indirect, lastIndirect;
        static long verts, lastVerts;

        static void Roll()
        {
            int f = UnityEngine.Time.frameCount;
            if (f == frame) return;
            lastDraws = draws; lastVerts = verts; lastIndirect = indirect;
            frame = f; draws = 0; verts = 0; indirect = 0;
        }

        /// <summary>Instanced submissions and single meshes in the last COMPLETE frame. Debug gizmos excluded.</summary>
        public static int DrawCalls { get { Roll(); return lastDraws; } }

        /// <summary>Vertices submitted in the last complete frame: mesh vertex count times instances, before any
        /// shadow pass, so it measures what was handed over rather than what the GPU then did with it.
        ///
        /// THIS EXCLUDES THE INDIRECT DRAWS, and that is not a rounding error - the infantry are indirect, so the
        /// men are missing from this number. An indirect draw's instance count lives in a GraphicsBuffer the GPU
        /// reads; the CPU never sees it, which is the whole point of the API. <see cref="IndirectDraws"/> counts
        /// how many such submissions were made, and VATRenderer.VerticesThisFrame is where the men's vertices are
        /// actually known. Read all three or the reading is wrong in the direction that flatters it.</summary>
        public static long Vertices { get { Roll(); return lastVerts; } }

        /// <summary>Indirect submissions in the last complete frame (the men, the debris). Included in
        /// <see cref="DrawCalls"/>, excluded from <see cref="Vertices"/>.</summary>
        public static int IndirectDraws { get { Roll(); return lastIndirect; } }

        public static void Draw<T>(UnityEngine.RenderParams rp, UnityEngine.Mesh mesh, int submesh, T[] instanceData, int count) where T : unmanaged
        {
            Roll();
            if (count > 0) { draws++; if (mesh != null) verts += (long)mesh.vertexCount * count; }
            UnityEngine.Graphics.RenderMeshInstanced(rp, mesh, submesh, instanceData, count);
        }

        public static void DrawIndirect(UnityEngine.RenderParams rp, UnityEngine.Mesh mesh, UnityEngine.GraphicsBuffer commandBuffer, int commandCount = 1, int startCommand = 0)
        {
            Roll();
            draws++; indirect++;
            UnityEngine.Graphics.RenderMeshIndirect(rp, mesh, commandBuffer, commandCount, startCommand);
        }

        public static void Draw(UnityEngine.RenderParams rp, UnityEngine.Mesh mesh, int submesh, UnityEngine.Matrix4x4 objectToWorld)
        {
            Roll();
            draws++; if (mesh != null) verts += mesh.vertexCount;
            UnityEngine.Graphics.RenderMesh(rp, mesh, submesh, objectToWorld);
        }
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
