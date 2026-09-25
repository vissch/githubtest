// Phase: B2 (implemented with code-made colours; textures and the GPU-displaced version come with the art pass)
// Look (owner's visual target, 2026-09-21): a painted cartoon mudfield. The colour texture (2 texels a metre) is flat
// tones of grey-brown mud cut by dark ink contours, pale puddles with an ink edge in the low and muddy ground, dark
// shell holes with an ink rim and a pale lip, plank floors in the trenches; a tiling stroke texture adds the brush
// marks up close. Shading is TW/Toon. Beyond the map edge the same mud runs on to the horizon (the skirt), and
// Atmosphere adds the overcast haze, so the flat standard view never shows a void.
// The ground. One mesh per 32 m chunk at the heightfield's 1 m resolution, normals taken from the heightfield so
// chunks meet without seams. A crater re-reads only the chunks it touches. Colour is one texel per height cell:
// churned earth with grass left on the higher ground, darker where it is mud or close to the water table, scorched
// in shell holes, duckboards in the trenches. Water is a single flat sheet at MapData.WaterLevel: the ground hides
// it wherever the ground is higher, so a new crater that goes under the water table shows water by itself.
// Adds BattlefieldProps (trees, wrecks, wire, trench kit) to the same object.
using System.Collections.Generic;
using UnityEngine;
using TW.Presentation;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed class GreyboxTerrainView : MonoBehaviour
    {
        public SimHost Host;
        [Tooltip("Legacy, kept so old scenes deserialize. Field decides the look now.")]
        public Atmosphere.Mood Look = Atmosphere.Mood.Night;
        [Tooltip("Which battlefield this is (docs/18). An Atmosphere already on this object wins, so a scene can override it.")]
        public Biome Field = Biome.NightMud;
        /// <summary>The profile this view built itself from. Resolved before anything reads it.</summary>
        public BiomeProfile Profile { get; private set; }
        public BattlefieldSurface Surface { get; private set; }
        public const int ChunkMeters = 32;
        const float GridStep = .5f; // Presentation mesh only: simulation height and traversal remain untouched.
        const int Tpm = 8;   // 720 x 1920 on the standard map: ~7 MiB including mipmaps; no per-pixel noise shader

        sealed class Chunk
        {
            public Mesh Mesh; public Vector3[] Verts; public Vector3[] Normals; public int X0, Z0, W, L; public bool Dirty;
            /// <summary>The first row still to re-read in a rebuild spread over frames; 0 = from the start.</summary>
            public int NextRow;
            /// <summary>The surface 0.25 m on from the last row read, which is the next row's sample 0.25 m back.</summary>
            public float[] RowAhead;
        }

        readonly List<Chunk> chunks = new List<Chunk>();
        readonly List<Object> owned = new List<Object>();
        int chunksX;
        Texture2D colorTex, depthTex;
        byte[] depthPx;
        bool depthDirty;
        RenderGroundGrid renderGrid;
        bool colorDirty, subscribed;
        readonly Queue<Vector2Int> paintTiles = new Queue<Vector2Int>();
        readonly HashSet<Vector2Int> queuedTiles = new HashSet<Vector2Int>();
        readonly List<TW.Sim.SimEvent> scorchMarks = new List<TW.Sim.SimEvent>();
        /// <summary>When each scorch mark landed, in Time.time. A crater is bare earth when it lands - the blast
        /// throws the snow off it - and fills in again over SnowFillSeconds. Wall clock, not ticks: this is a
        /// presentation fade and nothing in the sim may depend on it.</summary>
        readonly List<float> scorchBorn = new List<float>();
        /// <summary>How long a shell hole takes to fill back in. Guessed, wants a look.</summary>
        public const float SnowFillSeconds = 150f, ScorchHoldSeconds = 14f, RepaintEvery = 2.2f;
        int scorchSweep; float nextScorchRepaint;
        bool hollowsDirty;
        /// <summary>Terrain chunks re-read from the height field per frame after craters; the rest wait their turn.
        /// Superseded by ChunkBudgetMs (2026-09-23): kept for callers, no longer the limit.</summary>
        public const int MaxChunkRebuilds = 2;
        /// <summary>Milliseconds a frame spent re-reading crater-dirtied chunks, a row at a time (at least one row a frame).
        /// Two whole chunks a frame was the 27 ms worst frame measured on 2026-09-23; the finished mesh is the same.</summary>
        public const double ChunkBudgetMs = 2.0;
        double chunkBudgetMs = ChunkBudgetMs;   // or the knob terrain.chunkBudgetMs (Start)
        readonly System.Diagnostics.Stopwatch chunkWatch = new System.Diagnostics.Stopwatch();
        float[] rowBehindAhead = new float[0];   // the samples 0.25 m either side of a row's vertices along x, shared
        int chunkCursor;
        /// <summary>A crater's hollows are still to be rescanned: until they are, nothing that reads them (the chunks
        /// here, BattlefieldProps' composition) should rebuild, or it would keep the ground's old pools.</summary>
        public bool HollowsPending => hollowsDirty;
        readonly System.Diagnostics.Stopwatch paintWatch = new System.Diagnostics.Stopwatch();
        public int PendingPaintTiles => paintTiles.Count;
        public float LastPaintMilliseconds { get; private set; }

        void Start()
        {
            chunkBudgetMs = Knobs.Get("terrain.chunkBudgetMs", (float)ChunkBudgetMs);   // 2.0 is exact as a float
            if (Host == null || Host.Local == null) return;
            var map = Host.Local.Map;
            var hf = map.Height;
            // The battlefield's profile is resolved HERE, before anything reads it: flooding below changes how the
            // ground itself is interpreted (BattlefieldSurface), long before the Atmosphere component is added at
            // the foot of this method. An Atmosphere already in the scene wins, so a scene can override the view.
            var mood = GetComponent<Atmosphere>();
            // A LAUNCHED MISSION OUTRANKS BOTH. The component and the serialized Field are what this scene was
            // saved with; a mission is what the player just chose. A winter mission coming up in night mud
            // because GreyboxCorridor happens to carry an Atmosphere set to NightMud would be the most
            // confusing failure available. With no mission running - pressing Play in the scene - nothing
            // changes and the old rule stands, which is what MatchLaunch's header promises.
            var launched = MatchLaunch.Running;
            var chosen = launched != null ? BiomeProfile.ForGround(launched.Ground)
                                          : (mood != null ? mood.Field : Field);
            // written back to both so they cannot drift apart: that is the bug cycle 6 found in the snow colour
            if (launched != null) { Field = chosen; if (mood != null) mood.Field = chosen; }
            Profile = BiomeProfile.For(chosen);
            flooding = Profile.Flooding;   // the night field is a soaked one; nothing stands in water on lava
            Surface = new BattlefieldSurface(map, flooding);
            renderGrid = new RenderGroundGrid { Width = Mathf.RoundToInt(hf.Width / GridStep) + 1, Length = Mathf.RoundToInt(hf.Length / GridStep) + 1, Step = GridStep };
            renderGrid.Heights = new Unity.Collections.NativeArray<float>(renderGrid.Width * renderGrid.Length, Unity.Collections.Allocator.Persistent);
            var old = GetComponent<MeshRenderer>();   // scenes built for the single-mesh version
            if (old != null) old.enabled = false;

            var mat = Toon(Color.white);
            BuildChurn(map);
            colorTex = BuildColorTexture(map);
            mat.SetTexture("_BaseMap", colorTex);

            chunksX = (hf.Width + ChunkMeters - 1) / ChunkMeters;
            int chunksZ = (hf.Length + ChunkMeters - 1) / ChunkMeters;
            for (int cz = 0; cz < chunksZ; cz++)
            for (int cx = 0; cx < chunksX; cx++)
            {
                var c = new Chunk { X0 = cx * ChunkMeters, Z0 = cz * ChunkMeters };
                c.W = Mathf.RoundToInt(Mathf.Min(ChunkMeters, hf.Width - c.X0) / GridStep) + 1; c.L = Mathf.RoundToInt(Mathf.Min(ChunkMeters, hf.Length - c.Z0) / GridStep) + 1;
                c.Verts = new Vector3[c.W * c.L]; c.Normals = new Vector3[c.W * c.L];
                var uvs = new Vector2[c.W * c.L];
                var tris = new int[(c.W - 1) * (c.L - 1) * 6];
                int t = 0;
                for (int z = 0; z < c.L; z++)
                for (int x = 0; x < c.W; x++)
                {
                    uvs[z * c.W + x] = new Vector2((c.X0 + x * GridStep) / hf.Width, (c.Z0 + z * GridStep) / hf.Length);
                    if (x == c.W - 1 || z == c.L - 1) continue;
                    int i = z * c.W + x;
                    tris[t++] = i; tris[t++] = i + c.W; tris[t++] = i + 1;
                    tris[t++] = i + 1; tris[t++] = i + c.W; tris[t++] = i + c.W + 1;
                }
                Fill(c, hf);
                c.Mesh = new Mesh { name = $"Terrain {cx},{cz}", vertices = c.Verts, normals = c.Normals, uv = uvs, triangles = tris };
                c.Mesh.RecalculateBounds();
                owned.Add(c.Mesh);
                var go = new GameObject(c.Mesh.name) { hideFlags = HideFlags.DontSave };
                go.transform.SetParent(transform, false);
                go.AddComponent<MeshFilter>().sharedMesh = c.Mesh;
                go.AddComponent<MeshRenderer>().sharedMaterial = mat;
                chunks.Add(c);
            }

            RenderGround.Map = map; RenderGround.Grid = renderGrid;
            if (map.WaterLevel > MapData.NoWater) BuildWater(map);
            BuildSkirt(map);
            if (GetComponent<Atmosphere>() == null) gameObject.AddComponent<Atmosphere>().Field = Profile.Id;
            // which weather stands on this field is the profile's to say, not a test against one mood: the lava
            // field keeps its lamps and its lightning and loses the rain, and a fourth biome adds no line here.
            if (Profile.WantsLamps && GetComponent<NightLights>() == null) gameObject.AddComponent<NightLights>().Host = Host;
            if ((Profile.WantsRain || Profile.WantsSnowfall) && GetComponent<Rain>() == null) { var fall = gameObject.AddComponent<Rain>(); fall.AsSnow = Profile.WantsSnowfall; fall.Snowfall = Profile.Snowfall; }
            if (Profile.WantsStorm && GetComponent<Storm>() == null) gameObject.AddComponent<Storm>();
            if (GetComponent<SmallLife>() == null) gameObject.AddComponent<SmallLife>().Host = Host;
            if (GetComponent<QuietFog>() == null) gameObject.AddComponent<QuietFog>().Host = Host;
            if (GetComponent<FogWisps>() == null) gameObject.AddComponent<FogWisps>().Build(map);
            if (GetComponent<WaterRings>() == null)
            {
                var rings = gameObject.AddComponent<WaterRings>();
                rings.Host = Host;
                rings.IsWater = (x, z) =>
                {
                    if (x < 0f || z < 0f || x >= map.SizeMeters.x || z >= map.SizeMeters.y) return false;
                    if (map.WaterLevel > MapData.NoWater && RenderGround.Sample(map, x, z) < map.WaterLevel - .04f) return true;
                    return colorTex.GetPixel((int)(x * Tpm), (int)(z * Tpm)).a < .42f;   // a painted puddle
                };
            }
            if (map.HasSea)
            {
                if (GetComponent<Ocean>() == null) gameObject.AddComponent<Ocean>().Host = Host;
                if (GetComponent<LandingCraftView>() == null) gameObject.AddComponent<LandingCraftView>().Host = Host;
            }
            var props = GetComponent<BattlefieldProps>();
            if (props == null) props = gameObject.AddComponent<BattlefieldProps>();
            props.Host = Host;
            if (GetComponent<PropDestruction>() == null) gameObject.AddComponent<PropDestruction>();   // what a shell knocks down, breaks into debris and stays down
        }

        /// <summary>Vertices sit on the corners between height cells; normals come from the heightfield, not the chunk.</summary>
        void Fill(Chunk c, TW.Sim.Terrain.Heightfield hf)
        {
            for (int z = 0; z < c.L; z++) FillRow(c, z);
        }

        /// <summary>
        /// One row of a chunk. A vertex's normal takes the surface 0.25 m either side of it along x and z, and those points
        /// are its neighbours' too: x + .25 is the next vertex's x - .25, and z + .25 is the next row's z - .25. Each is
        /// sampled once and shared, which halves the ground samples a rebuild takes (the surface is expensive to sample:
        /// trench-lip blending, mounds, rills, pools). The values are the ones the per-vertex version computed: every
        /// position here is a multiple of 0.25 m, exact in float, so the shared point is the same float either way.
        /// Rows must be read in order from 0; row 0 samples its own z - .25.
        /// </summary>
        void FillRow(Chunk c, int z)
        {
            float wz = c.Z0 + z * GridStep;
            if (rowBehindAhead.Length < c.W + 1) rowBehindAhead = new float[c.W + 1];
            if (c.RowAhead == null || c.RowAhead.Length < c.W) c.RowAhead = new float[c.W];
            for (int x = 0; x <= c.W; x++) rowBehindAhead[x] = Surface.VisualHeight(c.X0 + x * GridStep - .25f, wz);
            for (int x = 0; x < c.W; x++)
            {
                float wx = c.X0 + x * GridStep;
                float bed = Surface.Bed(wx, wz), top = Surface.VisualHeight(wx, wz, bed);
                c.Verts[z * c.W + x] = new Vector3(wx, top, wz);
                // men and debris stand on the bed of a flooded hole, but never deeper than the knee
                renderGrid.Heights[Mathf.RoundToInt(wz / GridStep) * renderGrid.Width + Mathf.RoundToInt(wx / GridStep)] = Mathf.Max(bed, top - .42f);
                float behind = z == 0 ? Surface.VisualHeight(wx, wz - .25f) : c.RowAhead[x];
                float ahead = Surface.VisualHeight(wx, wz + .25f);
                c.RowAhead[x] = ahead;
                float dx = rowBehindAhead[x + 1] - rowBehindAhead[x], dz = ahead - behind;
                c.Normals[z * c.W + x] = new Vector3(-dx, .5f, -dz).normalized;
            }
        }

        static Texture2D mudDetail, ripples;

        /// <summary>A ground material: toon shading, the mud's grain and relief in world space, no hull outline.</summary>
        Material Toon(Color color)
        {
            var mat = new Material(Shader.Find("TW/Toon (URP)")) { hideFlags = HideFlags.HideAndDontSave };
            mat.SetColor("_BaseColor", color);
            if (mudDetail == null) mudDetail = BuildMudDetail();
            mat.SetTexture("_DetailMap", mudDetail);
            mat.SetFloat("_DetailScale", 1f / 12f);
            mat.SetFloat("_DetailStrength", 0.20f);
            mat.SetFloat("_DetailBump", 1f);
            mat.SetShaderPassEnabled("SRPDefaultUnlit", false);
            owned.Add(mat);
            return mat;
        }

        /// <summary>Value noise that repeats after periodX by periodZ cells, so a texture made from it tiles.</summary>
        static float TileNoise(float x, float z, int periodX, int periodZ, int salt)
        {
            int x0 = Mathf.FloorToInt(x), z0 = Mathf.FloorToInt(z);
            float tx = x - x0, tz = z - z0;
            tx = tx * tx * (3f - 2f * tx); tz = tz * tz * (3f - 2f * tz);
            int xa = ((x0 % periodX) + periodX) % periodX, xb = (xa + 1) % periodX, za = ((z0 % periodZ) + periodZ) % periodZ, zb = (za + 1) % periodZ;
            float a = Grain(xa + salt * 7919, za), b = Grain(xb + salt * 7919, za), c = Grain(xa + salt * 7919, zb), d = Grain(xb + salt * 7919, zb);
            return Mathf.Lerp(Mathf.Lerp(a, b, tx), Mathf.Lerp(c, d, tx), tz);
        }

        /// <summary>
        /// The mud's close-up surface, tiling over 12 m. A height field is built first: soft lumps everywhere, and in the
        /// churned patches a bed of clods (cellular noise: each cell a dome of its own height, a crevice where two
        /// meet), a scatter of stones. R is the tone (crevices dark, clod tops a little pale, brush dashes on top);
        /// G and B are the slope, which the shader lights as a hard-edged relief; A is the lumps' tone alone, which
        /// the shader reads a second time, larger and turned, for blotches broad enough to show from the overview.
        /// </summary>
        static Texture2D BuildMudDetail()
        {
            const int n = 512, cells = 17;
            var height = new float[n * n]; var tone = new float[n * n]; var broad = new float[n * n];
            for (int z = 0; z < n; z++)
            for (int x = 0; x < n; x++)
            {
                float u = x / (float)n, v = z / (float)n;
                float lumps = TileNoise(u * 6f, v * 6f, 6, 6, 1) * .5f + TileNoise(u * 13f, v * 13f, 13, 13, 2) * .3f + TileNoise(u * 31f, v * 31f, 31, 31, 3) * .2f;
                float smear = 1f - Mathf.Abs(TileNoise(u * 8f, v * 5f, 8, 5, 6) * 2f - 1f);   // folds where the mud was pushed about
                lumps = lumps * .7f + smear * smear * .3f;
                float churn = Mathf.SmoothStep(0f, 1f, (TileNoise(u * 4f, v * 4f, 4, 4, 4) * .7f + TileNoise(u * 9f, v * 9f, 9, 9, 5) * .3f - .53f) / .14f);
                float cu = u * cells, cv = v * cells; int cx = Mathf.FloorToInt(cu), cz = Mathf.FloorToInt(cv);
                float f1 = 9f, f2 = 9f, lift = 0f;
                for (int dz = -1; dz <= 1; dz++)
                for (int dx = -1; dx <= 1; dx++)
                {
                    int kx = ((cx + dx) % cells + cells) % cells, kz = ((cz + dz) % cells + cells) % cells;
                    float px2 = cx + dx + .15f + .7f * Grain(kx + 101, kz), pz2 = cz + dz + .15f + .7f * Grain(kx, kz + 211);
                    float d = (px2 - cu) * (px2 - cu) + (pz2 - cv) * (pz2 - cv);
                    if (d < f1) { f2 = f1; f1 = d; lift = Grain(kx + 307, kz + 17); }
                    else if (d < f2) f2 = d;
                }
                f1 = Mathf.Sqrt(f1); f2 = Mathf.Sqrt(f2);
                float seam = Mathf.SmoothStep(0f, 1f, (f2 - f1) / .30f);           // 0 in the crevice between two clods
                float clod = seam * (1f - .55f * f1 * f1) * (.35f + .65f * lift) * (lift < .22f ? .25f : 1f);   // some cells are trodden flat
                height[z * n + x] = lumps * .85f + churn * clod * .50f;
                broad[z * n + x] = (lumps - .5f) * .55f;
                tone[z * n + x] = (lumps - .5f) * .34f + churn * ((clod - .40f) * .26f - (1f - Mathf.SmoothStep(0f, 1f, (f2 - f1) / .10f)) * .12f);
            }
            for (int k = 0; k < 260; k++)   // stones: a small dome with a dark foot
            {
                int x = (int)(Grain(k, 61) * n), z = (int)(Grain(k, 67) * n); float r = 1.6f + Grain(k, 71) * 3.2f;
                for (int dz = -6; dz <= 6; dz++)
                for (int dx = -6; dx <= 6; dx++)
                {
                    float d = Mathf.Sqrt(dx * dx + dz * dz) / r; if (d > 1.35f) continue;
                    int i = ((z + dz + n) % n) * n + (x + dx + n) % n;
                    if (d <= 1f) { height[i] += (1f - d * d) * .30f; tone[i] += .10f; } else tone[i] -= .12f;
                }
            }
            for (int k = 0; k < 900; k++)   // brush dashes, mostly dark
            {
                int x = (int)(Grain(k, 11) * n), z = (int)(Grain(k, 23) * n), len = 7 + (int)(Grain(k, 37) * 16f);
                float lean = Grain(k, 41) * 0.8f - 0.4f, v = Grain(k, 53) < 0.78f ? -.20f : .16f;
                for (int t = 0; t < len; t++)
                {
                    int xx = (x + t) % n, zz = ((z + (int)(t * lean)) % n + n) % n;
                    tone[zz * n + xx] += v; if (v < 0f) tone[((zz + 1) % n) * n + xx] += v * .7f;
                }
            }
            var px = new Color32[n * n];
            for (int z = 0; z < n; z++)
            for (int x = 0; x < n; x++)
            {
                int xl = (x + n - 1) % n, xr = (x + 1) % n, zd = (z + n - 1) % n, zu = (z + 1) % n;
                float sx = (height[z * n + xl] - height[z * n + xr]) * 9f, sz = (height[zd * n + x] - height[zu * n + x]) * 9f;   // the normal's xz
                px[z * n + x] = new Color32((byte)(Mathf.Clamp01(.5f + tone[z * n + x]) * 255f), (byte)(Mathf.Clamp01(.5f + sx) * 255f), (byte)(Mathf.Clamp01(.5f + sz) * 255f), (byte)(Mathf.Clamp01(.5f + broad[z * n + x]) * 255f));
            }
            var tex = new Texture2D(n, n, TextureFormat.RGBA32, true) { name = "Mud detail", wrapMode = TextureWrapMode.Repeat, filterMode = FilterMode.Trilinear, anisoLevel = 4, hideFlags = HideFlags.HideAndDontSave };
            tex.SetPixels32(px); tex.Apply(true, true);
            return tex;
        }

        /// <summary>The water's moving surface, tiling: R and G the slope of a soft swell, B long streaks lying along x (the current).</summary>
        static Texture2D BuildRipples()
        {
            const int n = 256;
            var height = new float[n * n];
            for (int z = 0; z < n; z++)
            for (int x = 0; x < n; x++)
            {
                float u = x / (float)n, v = z / (float)n;
                height[z * n + x] = TileNoise(u * 5f, v * 7f, 5, 7, 11) * .5f + TileNoise(u * 11f, v * 15f, 11, 15, 12) * .3f + TileNoise(u * 23f, v * 29f, 23, 29, 13) * .2f;
            }
            var px = new Color32[n * n];
            for (int z = 0; z < n; z++)
            for (int x = 0; x < n; x++)
            {
                float u = x / (float)n, v = z / (float)n;
                int xl = (x + n - 1) % n, xr = (x + 1) % n, zd = (z + n - 1) % n, zu = (z + 1) % n;
                float sx = (height[z * n + xl] - height[z * n + xr]) * 14f, sz = (height[zd * n + x] - height[zu * n + x]) * 14f;
                float streak = TileNoise(u * 2f, v * 13f, 2, 13, 14) * .6f + TileNoise(u * 5f, v * 29f, 5, 29, 15) * .4f;
                px[z * n + x] = new Color32((byte)(Mathf.Clamp01(.5f + sx) * 255f), (byte)(Mathf.Clamp01(.5f + sz) * 255f), (byte)(Mathf.Clamp01(streak) * 255f), 255);
            }
            var tex = new Texture2D(n, n, TextureFormat.RGBA32, true) { name = "Water ripples", wrapMode = TextureWrapMode.Repeat, filterMode = FilterMode.Trilinear, anisoLevel = 2, hideFlags = HideFlags.HideAndDontSave };
            tex.SetPixels32(px); tex.Apply(true, true);
            return tex;
        }

        /// <summary>The land beyond the map: meets the edge heights, levels out over 30 m and runs to the horizon.</summary>
        public const float SkirtLevel = 1.6f, SkirtBlend = 30f;

        /// <summary>Height of the land outside the map at a point (BattlefieldProps stands trees on it).</summary>
        public static float SkirtHeight(MapData map, float x, float z)
        {
            float w = map.SizeMeters.x, l = map.SizeMeters.y;
            float outside = Mathf.Max(Mathf.Max(-x, x - w), Mathf.Max(-z, z - l));
            float edge = map.Height.Sample(Mathf.Clamp(x, 0f, w), Mathf.Clamp(z, 0f, l));
            // past the waterline the land does not level out at all: the bed takes over and goes on down (Shore)
            return Shore.Shape(map, x, z, Mathf.Lerp(edge, SkirtLevel, Mathf.SmoothStep(0f, 1f, Mathf.Clamp01(outside / SkirtBlend))));
        }

        void BuildSkirt(MapData map)
        {
            var hf = map.Height;
            float w = map.SizeMeters.x, l = map.SizeMeters.y;
            const float far = 1500f, level = SkirtLevel;
            float[] outs = { 0f, 2f, 4f, 7f, 10f, 14f, 18f, 24f, 30f, 60f, 140f, far };
            var verts = new List<Vector3>(); var cols = new List<Color>(); var tris = new List<int>();
            // side 0/1: along Z at x = 0 / w (these also cover the corners); side 2/3: along X at z = 0 / l
            for (int side = 0; side < 4; side++)
            {
                bool alongZ = side < 2; bool high = (side & 1) == 1;
                float from = alongZ ? -far : 0f, to = alongZ ? l + far : w, span = alongZ ? l : w;
                var stops = new List<float> { from };
                for (float t = 0f; t <= span; t += GridStep) stops.Add(t);   // the ground's own vertex spacing, so the two meet exactly
                if (alongZ) stops.Add(to); else stops.Add(span);
                int row0 = verts.Count, cols_ = outs.Length;
                foreach (float t in stops)
                {
                    // The land beyond the map continues what reaches the edge (the river's channel, a rise, a hollow), but not
                    // as a straight extrusion: each feature drifts sideways as it runs out and fades, the way the river would
                    // go on bending. A trench that runs out at the edge is closed by the bank beside it instead.
                    float tc = Mathf.Clamp(t, 0f, span);
                    float edge = alongZ ? Surface.VisualHeight(high ? w : 0f, tc) : Surface.VisualHeight(tc, high ? l : 0f);   // the very heights the ground mesh uses
                    float beyond = alongZ ? Mathf.Max(0f, Mathf.Max(-t, t - l)) : 0f;   // past the corner the ground is already level
                    for (int k = 0; k < cols_; k++)
                    {
                        float d = outs[k];
                        float y = k == 0 ? Mathf.Lerp(edge, level, Mathf.SmoothStep(0f, 1f, Mathf.Clamp01(beyond / SkirtBlend))) : SkirtY(map, side, t, d);
                        float off = high ? span2(alongZ, w, l) + d : -d;
                        var v = alongZ ? new Vector3(off, y, t) : new Vector3(t, y, off);
                        if (k == 0) v.y = Shore.Shape(map, v.x, v.z, v.y);   // the corner columns too: the coast runs on past both flanks
                        verts.Add(v); cols.Add(MudMid);   // one tone: the columns are too far apart to carry the painted patches
                    }
                }
                for (int r = 0; r < stops.Count - 1; r++)
                for (int k = 0; k < cols_ - 1; k++)
                {
                    int a = row0 + r * cols_ + k, b = a + 1, c = a + cols_, d2 = c + 1;
                    bool flip = alongZ == high;   // keep the faces up on all four sides
                    if (flip) { tris.Add(a); tris.Add(c); tris.Add(b); tris.Add(b); tris.Add(c); tris.Add(d2); }
                    else { tris.Add(a); tris.Add(b); tris.Add(c); tris.Add(b); tris.Add(d2); tris.Add(c); }
                }
            }
            var mesh = new Mesh { name = "Skirt", indexFormat = UnityEngine.Rendering.IndexFormat.UInt32 };
            var skirtUvs = new List<Vector2>(verts.Count);
            const float paintedBorder = 96f;
            foreach (var v in verts) skirtUvs.Add(new Vector2((v.x + paintedBorder) / (w + paintedBorder * 2f), (v.z + paintedBorder) / (l + paintedBorder * 2f)));
            mesh.SetVertices(verts); mesh.SetUVs(0, skirtUvs); mesh.SetTriangles(tris, 0);
            mesh.RecalculateNormals(); mesh.RecalculateBounds();
            owned.Add(mesh);
            var go = new GameObject("Skirt") { hideFlags = HideFlags.DontSave };
            go.transform.SetParent(transform, false);
            go.AddComponent<MeshFilter>().sharedMesh = mesh;
            var r2 = go.AddComponent<MeshRenderer>();
            var skirtMat = Toon(Color.white);
            // Texels a metre for a surface that is mostly drawn beyond the fog. Three was picked by eye and never
            // costed: at 12.4 MB this was the third largest texture in the game (docs/05). It stays at three because
            // the inner five metres of this texture blend into the real ground at the map edge, where a coarser grid
            // would show a seam; the saving comes from the format instead.
            const float texelsPerMetre = 3f;
            // Block compression works on 4x4 blocks and silently declines a texture whose sides are not a multiple of
            // four, so the grid is rounded up and the world mapping below is derived from the ROUNDED size rather
            // than assuming texelsPerMetre exactly. Getting that backwards would skew the paint against the mesh's UVs.
            float spanX = w + paintedBorder * 2f, spanZ = l + paintedBorder * 2f;
            int tw = Round4(Mathf.CeilToInt(spanX * texelsPerMetre)), th = Round4(Mathf.CeilToInt(spanZ * texelsPerMetre));
            var texture = new Texture2D(tw, th, TextureFormat.RGBA32, true) { name = "Painted horizon", wrapMode = TextureWrapMode.Clamp, filterMode = FilterMode.Bilinear, anisoLevel = 4 };
            var pixels = new Color32[tw * th];
            for (int z = 0; z < th; z++)
            for (int x = 0; x < tw; x++)
            {
                float wx = x * spanX / tw - paintedBorder, wz = z * spanZ / th - paintedBorder;
                float outside = Mathf.Max(Mathf.Max(-wx, wx - w), Mathf.Max(-wz, wz - l));
                Color c = Tone(wx, wz);
                if (outside < 5f) c = Color.Lerp(GroundColor(map, Mathf.Clamp(wx, 0f, w - 0.01f), Mathf.Clamp(wz, 0f, l - 0.01f)), c, Band(0f, 5f, outside));
                c = Color.Lerp(c, MudMid, Band(30f, 96f, outside));
                pixels[z * tw + x] = Coast(map, wx, wz, SkirtHeight(map, wx, wz), c);   // the beach does not stop at the map edge
            }
            // DXT5, not DXT1. The alpha here is not spare: Coast drives it down to 0.45 on wet sand, and TW/Toon reads
            // `gloss = max(_Gloss, 1.0 - base.a)` with `base.a < 0.45` meaning standing water. DXT1 carries one bit of
            // alpha and would turn the whole beach into a hard water/not-water edge. DXT5 keeps eight interpolated
            // bits, and Compress picks it automatically for a texture whose format has alpha.
            //
            // Mips are built while the pixels are still readable, then the compressed result is uploaded and the CPU
            // copy dropped. The cost is a one-time compression at scene load, not yet measured; the gain is four to
            // one on 4.5 MB of base level plus its mips.
            texture.SetPixels32(pixels);
            texture.Apply(true, false);
            texture.Compress(true);
            texture.Apply(false, true);
            owned.Add(texture);
            skirtMat.SetTexture("_BaseMap", texture);
            r2.sharedMaterial = skirtMat; r2.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
        }

        static float span2(bool alongZ, float w, float l) => alongZ ? w : l;

        /// <summary>Up to the next multiple of four, which is the block size every DXT format works in.</summary>
        static int Round4(int n) => (n + 3) & ~3;

        /// <summary>The edge height a skirt column continues: the ground's own, or the bank beside a trench that runs out there.</summary>
        float SkirtEdge(MapData map, int side, float tt)
        {
            bool alongZ = side < 2, high = (side & 1) == 1;
            float w = map.SizeMeters.x, l = map.SizeMeters.y, span = alongZ ? l : w, ex = high ? w - .5f : .5f;
            System.Func<float, bool> dug = zz => alongZ && ((NavLayer)map.NavLayers[map.NavIndex(Mathf.Clamp((int)(ex / MapData.NavCellSize), 0, map.NavWidth - 1), Mathf.Clamp((int)(zz / MapData.NavCellSize), 0, map.NavLength - 1))] & NavLayer.Trench) != 0;
            float c = Mathf.Clamp(tt, 0f, span);
            if (dug(c))
                for (float reach = 1f; reach <= 14f; reach += 1f)
                {
                    if (!dug(c - reach)) return Surface.VisualHeight(high ? w : 0f, Mathf.Max(0f, c - reach - 1.5f));
                    if (!dug(c + reach)) return Surface.VisualHeight(high ? w : 0f, Mathf.Min(span, c + reach + 1.5f));
                }
            return alongZ ? Surface.VisualHeight(high ? w : 0f, c) : Surface.VisualHeight(c, high ? l : 0f);
        }

        /// <summary>Skirt height d metres outside a side, t metres along it: the edge's features drift sideways and level out.</summary>
        float SkirtY(MapData map, int side, float t, float d)
        {
            float l = map.SizeMeters.y;
            float beyond = side < 2 ? Mathf.Max(0f, Mathf.Max(-t, t - l)) : 0f;   // past the corner the ground is already level
            float blend = Mathf.SmoothStep(0f, 1f, Mathf.Clamp01(Mathf.Max(d, beyond) / SkirtBlend));
            float drift = (12f * Mathf.Sin(d * .11f + side * 1.9f) + 5f * Mathf.Sin(d * .31f + side)) * Mathf.SmoothStep(0f, 1f, d / 8f);
            float x = side < 2 ? ((side & 1) == 1 ? map.SizeMeters.x + d : -d) : t;
            float z = side < 2 ? t : ((side & 1) == 1 ? map.SizeMeters.y + d : -d);
            return Shore.Shape(map, x, z, Mathf.Lerp(SkirtEdge(map, side, t + drift), SkirtLevel, blend));
        }

        /// <summary>The shared ripple map (RG slope, B streaks): the river's, and the sea's.</summary>
        public Texture2D Ripples()
        {
            if (ripples == null) ripples = BuildRipples();
            return ripples;
        }

        void BuildWater(MapData map)
        {
            // TW/Water: an opaque sheet that reads how deep it is from a map baked off the drawn ground, one texel a
            // ground vertex, so its shoreline and depth bands sit exactly where the mesh meets it.
            var m = new Material(Shader.Find("TW/Water (URP)")) { hideFlags = HideFlags.HideAndDontSave };
            owned.Add(m);
            if (ripples == null) ripples = BuildRipples();
            int dw = renderGrid.Width + 2 * DepthBorder;
            depthPx = new byte[dw * renderGrid.Length];
            depthTex = new Texture2D(dw, renderGrid.Length, TextureFormat.R8, false) { name = "Water depth", wrapMode = TextureWrapMode.Clamp, filterMode = FilterMode.Bilinear };
            owned.Add(depthTex);
            PaintDepth(0, 0, renderGrid.Width, renderGrid.Length);
            for (int z = 0; z < renderGrid.Length; z++)   // the land beyond the two long edges, as the skirt draws it
            for (int k = 1; k <= DepthBorder; k++)
            {
                depthPx[z * dw + DepthBorder - k] = DepthByte(map.WaterLevel - SkirtY(map, 0, z * GridStep, k * GridStep));
                depthPx[z * dw + DepthBorder + renderGrid.Width - 1 + k] = DepthByte(map.WaterLevel - SkirtY(map, 1, z * GridStep, k * GridStep));
            }
            depthTex.SetPixelData(depthPx, 0); depthTex.Apply(false, false);
            m.SetTexture("_DepthMap", depthTex);
            m.SetTexture("_RippleMap", ripples);
            m.SetVector("_DepthST", new Vector4(1f / (dw * GridStep), 1f / (renderGrid.Length * GridStep), (DepthBorder + .5f) / dw, .5f / renderGrid.Length));
            float w = map.SizeMeters.x, l = map.SizeMeters.y, y = map.WaterLevel;
            if (map.HasSea) { if (map.SeaSide == 1) l = map.SeaStartZ; }   // the sea's own sheet (Ocean) takes it from here: two sheets at one level would fight
            var mesh = new Mesh
            {
                name = "Water",
                vertices = new[] { new Vector3(-40f, y, map.HasSea && map.SeaSide == 0 ? map.SeaStartZ : 0f), new Vector3(-40f, y, l), new Vector3(w + 40f, y, l), new Vector3(w + 40f, y, map.HasSea && map.SeaSide == 0 ? map.SeaStartZ : 0f) },   // past the edge: the rising land beyond pinches the river out
                normals = new[] { Vector3.up, Vector3.up, Vector3.up, Vector3.up },
                triangles = new[] { 0, 1, 2, 0, 2, 3 },
            };
            owned.Add(mesh);
            var go = new GameObject("Water") { hideFlags = HideFlags.DontSave };
            go.transform.SetParent(transform, false);
            go.AddComponent<MeshFilter>().sharedMesh = mesh;
            var r = go.AddComponent<MeshRenderer>();
            r.sharedMaterial = m; r.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
        }

        /// <summary>Water depth over a block of ground vertices: (depth + 0.4 m) / 2 m, as TW/Water reads it.</summary>
        void PaintDepth(int x0, int z0, int w, int l)
        {
            float water = Host.Local.Map.WaterLevel; int dw = renderGrid.Width + 2 * DepthBorder;
            for (int z = z0; z < z0 + l && z < renderGrid.Length; z++)
            for (int x = x0; x < x0 + w && x < renderGrid.Width; x++)
                depthPx[z * dw + DepthBorder + x] = DepthByte(water - renderGrid.Heights[z * renderGrid.Width + x]);
        }

        const int DepthBorder = 80;   // vertices past each long edge: the 40 m the water sheet runs on
        static byte DepthByte(float depth) => (byte)(Mathf.Clamp01((depth + .4f) / 2f) * 255f);

        void Update()
        {
            using var perf = TW.Sim.PerfMarkers.TerrainUpdate.Auto();
            if (Host == null || Host.Local == null || colorTex == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            // the hollow rescan is whole-map work: one heavy job a frame (HeavyWork), and chunks mid-rebuild start over on
            // the new hollows, so no chunk is ever finished half on the old ones
            bool rescanned = false;
            if (hollowsDirty && HeavyWork.TryClaim())
            {
                TW.Sim.PerfMarkers.TerrainHollows.Begin(); Surface.RefreshHollows(); hollowsDirty = false; TW.Sim.PerfMarkers.TerrainHollows.End();
                foreach (var chunk in chunks) chunk.NextRow = 0;
                rescanned = true;
            }
            // Small tiles bound each work item. Terrain pigment catches up over frames after a barrage.
            TW.Sim.PerfMarkers.TerrainRepaint.Begin();
            paintWatch.Restart();
            // The fade has to reach the texture, and the texture is only repainted when something queues a tile.
            // So one crater's tiles are re-queued every RepaintEvery seconds, oldest first: sixty-four marks come
            // round in about the time one takes to fill, and it all goes through the budget below rather than a
            // second one. A crater that has finished filling stops being re-queued.
            if (scorchMarks.Count > 0 && Time.time >= nextScorchRepaint)
            {
                nextScorchRepaint = Time.time + RepaintEvery;
                scorchSweep = scorchSweep % scorchMarks.Count;
                int m = scorchSweep++;
                if (m < scorchBorn.Count && Time.time - scorchBorn[m] < SnowFillSeconds) QueueScorchTiles(scorchMarks[m]);
            }
            while (paintTiles.Count > 0 && paintWatch.Elapsed.TotalMilliseconds < 2.0)
            {
                var tile = paintTiles.Dequeue(); queuedTiles.Remove(tile);
                RepaintTile(tile); colorDirty = true;
            }
            LastPaintMilliseconds = (float)paintWatch.Elapsed.TotalMilliseconds;
            TW.Sim.PerfMarkers.TerrainRepaint.End();
            if (colorDirty) { TW.Sim.PerfMarkers.TerrainApply.Begin(); colorTex.Apply(true, false); colorDirty = false; TW.Sim.PerfMarkers.TerrainApply.End(); }
            // ChunkBudgetMs a frame, a row at a time, taken round the field from where the last frame stopped: a barrage
            // dirties most of the field in one tick. A chunk is uploaded when its last row is read, so a mesh is never
            // drawn half old and half new; not while the hollows wait to be rescanned (the rows would read old pools)
            TW.Sim.PerfMarkers.TerrainChunks.Begin();
            if (!hollowsDirty && !rescanned)
            {
                chunkWatch.Restart();
                int rows = 0;
                for (int n = 0; n < chunks.Count; n++)
                {
                    int i = (chunkCursor + n) % chunks.Count;
                    var c = chunks[i];
                    if (!c.Dirty) continue;
                    chunkCursor = i;   // stay on it until it is done
                    while (c.NextRow < c.L && (rows == 0 || chunkWatch.Elapsed.TotalMilliseconds < chunkBudgetMs)) { FillRow(c, c.NextRow++); rows++; }
                    if (c.NextRow < c.L) break;   // out of time: the rest of this chunk next frame
                    c.Dirty = false; c.NextRow = 0;
                    c.Mesh.vertices = c.Verts; c.Mesh.normals = c.Normals;
                    c.Mesh.RecalculateBounds();
                    if (depthTex != null) { PaintDepth(Mathf.RoundToInt(c.X0 / GridStep), Mathf.RoundToInt(c.Z0 / GridStep), c.W, c.L); depthDirty = true; }
                    chunkCursor = (i + 1) % chunks.Count;
                    if (chunkWatch.Elapsed.TotalMilliseconds >= chunkBudgetMs) break;
                }
            }
            TW.Sim.PerfMarkers.TerrainChunks.End();
            if (depthDirty) { depthTex.SetPixelData(depthPx, 0); depthTex.Apply(false, false); depthDirty = false; }
        }

        void OnDestroy()
        {
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
            if (renderGrid.Heights.IsCreated)
            {
                if (RenderGround.Grid.Heights.Equals(renderGrid.Heights)) { RenderGround.Grid = default; RenderGround.Map = null; }
                renderGrid.Heights.Dispose();
            }
            foreach (var resource in owned) if (resource != null) Destroy(resource);
        }

        /// <summary>A crater landed: repaint its texels and re-read the chunks it touches.</summary>
        void OnSimEvent(TW.Sim.SimEvent e)
        {
            if (e.Type != TW.Sim.SimEventType.CraterStamp && e.Type != TW.Sim.SimEventType.WireBreached) return;
            var map = Host.Local.Map;
            if (e.Type == TW.Sim.SimEventType.CraterStamp)
            {
                hollowsDirty = true;
                if (scorchMarks.Count == 64) { scorchMarks.RemoveAt(0); scorchBorn.RemoveAt(0); }
                scorchMarks.Add(e); scorchBorn.Add(Time.time);
            }
            float r = e.Scalar + 8f; // Include the inferred rim and its pale outer shoulder.
            int x0 = Mathf.Max(0, Mathf.FloorToInt(e.Pos.x - r)), x1 = Mathf.Min(map.Height.Width - 1, Mathf.CeilToInt(e.Pos.x + r));
            int z0 = Mathf.Max(0, Mathf.FloorToInt(e.Pos.z - r)), z1 = Mathf.Min(map.Height.Length - 1, Mathf.CeilToInt(e.Pos.z + r));
            for (int z = z0 / 2; z <= z1 / 2; z++) for (int x = x0 / 2; x <= x1 / 2; x++)
            { var tile = new Vector2Int(x, z); if (queuedTiles.Add(tile)) paintTiles.Enqueue(tile); }
            for (int cz = z0 / ChunkMeters; cz <= z1 / ChunkMeters; cz++)
            for (int cx = Mathf.Max(0, (x0 - 1) / ChunkMeters); cx <= x1 / ChunkMeters; cx++)
            {
                int i = cz * chunksX + cx;
                if (i >= 0 && i < chunks.Count) { chunks[i].Dirty = true; chunks[i].NextRow = 0; }   // heights changed: start it over
            }
        }

        /// <summary>Queue the tiles a crater covers, so its burn is repainted at whatever it has faded to.</summary>
        void QueueScorchTiles(TW.Sim.SimEvent mark)
        {
            var map = Host.Local.Map;
            float r = mark.Scalar * 1.25f + 1f;
            int x0 = Mathf.Max(0, Mathf.FloorToInt(mark.Pos.x - r)), x1 = Mathf.Min(map.Height.Width - 1, Mathf.CeilToInt(mark.Pos.x + r));
            int z0 = Mathf.Max(0, Mathf.FloorToInt(mark.Pos.z - r)), z1 = Mathf.Min(map.Height.Length - 1, Mathf.CeilToInt(mark.Pos.z + r));
            for (int z = z0 / 2; z <= z1 / 2; z++) for (int x = x0 / 2; x <= x1 / 2; x++)
            { var tile = new Vector2Int(x, z); if (queuedTiles.Add(tile)) paintTiles.Enqueue(tile); }
        }

        void RepaintTile(Vector2Int tile)
        {
            int x1 = Mathf.Min(colorTex.width, (tile.x + 1) * 2 * Tpm), z1 = Mathf.Min(colorTex.height, (tile.y + 1) * 2 * Tpm);
            for (int z = tile.y * 2 * Tpm; z < z1; z++) for (int x = tile.x * 2 * Tpm; x < x1; x++)
            {
                float wx = (x + .5f) / Tpm, wz = (z + .5f) / Tpm;
                Color c = GroundColor(Host.Local.Map, wx, wz);
                float burn = 0f;
                for (int m = 0; m < scorchMarks.Count; m++)
                {
                    var mark = scorchMarks[m];
                    float radius = mark.Scalar * 1.25f;
                    if (radius <= 0f || Mathf.Abs(wx - mark.Pos.x) > radius || Mathf.Abs(wz - mark.Pos.z) > radius) continue;
                    float distance = Vector2.Distance(new Vector2(wx, wz), new Vector2(mark.Pos.x, mark.Pos.z));
                    // The hole is black for ScorchHoldSeconds and then fills: the ground lightens back toward what
                    // it was, and because the snow is keyed off the burn (Toon_URP) the snow comes back with it.
                    float age = m < scorchBorn.Count ? Time.time - scorchBorn[m] : SnowFillSeconds;
                    float fresh = 1f - Mathf.Clamp01((age - ScorchHoldSeconds) / Mathf.Max(1f, SnowFillSeconds - ScorchHoldSeconds));
                    burn = Mathf.Max(burn, .45f * (1f - distance / radius) * fresh * fresh);
                }
                colorTex.SetPixel(x, z, Color.Lerp(c, new Color(.10f, .09f, .08f), burn));
            }
        }

        Texture2D BuildColorTexture(MapData map)
        {
            int w = map.Height.Width * Tpm, l = map.Height.Length * Tpm;
            var tex = new Texture2D(w, l, TextureFormat.RGBA32, true) { filterMode = FilterMode.Bilinear, wrapMode = TextureWrapMode.Clamp, anisoLevel = 4 };
            owned.Add(tex);
            var px = new Color[w * l];
            for (int z = 0; z < l; z++)
            for (int x = 0; x < w; x++) px[z * w + x] = GroundColor(map, (x + 0.5f) / Tpm, (z + 0.5f) / Tpm);
            Color[] teamTint = { new Color(0.55f, 0.45f, 0.25f), new Color(0.35f, 0.40f, 0.55f) };
            for (int o = 0; o < map.Objectives.Length; o++)
            {
                var def = map.Objectives[o];
                if (def.Kind != ObjectiveKind.HQ) continue;
                for (int k = 0; k < def.CellCount; k++)
                {
                    int cell = map.ObjectiveCells[def.CellStart + k];
                    int nx = cell % map.NavWidth, nz = cell / map.NavWidth;
                    int cellPx = (int)MapData.NavCellSize * Tpm;
                    for (int dz = 0; dz < cellPx; dz++) for (int dx = 0; dx < cellPx; dx++)
                    {
                        int i = (nz * cellPx + dz) * w + nx * cellPx + dx;
                        // HQ ownership is communicated by the HUD. Avoid a rectangular team-colour carpet on the mud.
                        if (i < px.Length) px[i] = Color.Lerp(px[i], teamTint[def.SideTeam & 1], 0.04f);
                    }
                }
            }
            tex.SetPixels(px);
            tex.Apply(true, false);   // stays readable: craters repaint texels at run time
            return tex;
        }

        float flooding;  // 0 dry day .. 1: share of shell holes and trench bays under water
        float[] churn;   // per nav cell: how trodden the ground is, 1 at a ladder or ramp, fading over 5 m

        /// <summary>Where men climb in and out of the trenches the ground is beaten to a dark, slick mess.</summary>
        void BuildChurn(MapData map)
        {
            churn = new float[map.NavWidth * map.NavLength];
            for (int z = 0; z < map.NavLength; z++)
            for (int x = 0; x < map.NavWidth; x++)
            {
                if (((NavLayer)map.NavLayers[map.NavIndex(x, z)] & NavLayer.Link) == 0) continue;
                for (int dz = -3; dz <= 3; dz++)
                for (int dx = -3; dx <= 3; dx++)
                {
                    int cx = x + dx, cz = z + dz;
                    if (cx < 0 || cz < 0 || cx >= map.NavWidth || cz >= map.NavLength) continue;
                    float v = 1f - Mathf.Sqrt(dx * dx + dz * dz) / 3.2f;
                    if (v > churn[cz * map.NavWidth + cx]) churn[cz * map.NavWidth + cx] = v;
                }
            }
        }

        float Churn(MapData map, float wx, float wz)
        {
            float fx = wx / MapData.NavCellSize - .5f, fz = wz / MapData.NavCellSize - .5f;
            int x0 = Mathf.Clamp(Mathf.FloorToInt(fx), 0, map.NavWidth - 2), z0 = Mathf.Clamp(Mathf.FloorToInt(fz), 0, map.NavLength - 2);
            float tx = Mathf.Clamp01(fx - x0), tz = Mathf.Clamp01(fz - z0);
            return Mathf.Lerp(Mathf.Lerp(churn[z0 * map.NavWidth + x0], churn[z0 * map.NavWidth + x0 + 1], tx), Mathf.Lerp(churn[(z0 + 1) * map.NavWidth + x0], churn[(z0 + 1) * map.NavWidth + x0 + 1], tx), tz);
        }

        /// <summary>Dried crust: distance to the nearest crack (cellular noise, plates about 0.6 m) and a tone for the plate.</summary>
        static float Crackle(float wx, float wz, out float plate)
        {
            float u = wx / .62f, v = wz / .62f; int cx = Mathf.FloorToInt(u), cz = Mathf.FloorToInt(v);
            float f1 = 9f, f2 = 9f; plate = 0f;
            for (int dz = -1; dz <= 1; dz++)
            for (int dx = -1; dx <= 1; dx++)
            {
                float px = cx + dx + .12f + .76f * Grain(cx + dx + 1013, cz + dz), pz = cz + dz + .12f + .76f * Grain(cx + dx, cz + dz + 2027);
                float d = (px - u) * (px - u) + (pz - v) * (pz - v);
                if (d < f1) { f2 = f1; f1 = d; plate = Grain(cx + dx + 31, cz + dz + 57); } else if (d < f2) f2 = d;
            }
            return Mathf.Sqrt(f2) - Mathf.Sqrt(f1);
        }

        static float Grain(int x, int z)
        {
            uint h = (uint)x * 0x9E3779B1u ^ (uint)z * 0x85EBCA77u;
            h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12;
            return (h & 0xFFFF) / 65535f;
        }

        /// <summary>How much of a layer lies around a point, 0..1: nav cells are 2 m blocks, this blends them.</summary>
        static float Amount(MapData map, float wx, float wz, NavLayer bit)
        {
            float fx = wx / MapData.NavCellSize - 0.5f, fz = wz / MapData.NavCellSize - 0.5f;
            int x0 = Mathf.FloorToInt(fx), z0 = Mathf.FloorToInt(fz);
            float tx = fx - x0, tz = fz - z0, sum = 0f;
            for (int dz = 0; dz < 2; dz++)
            for (int dx = 0; dx < 2; dx++)
            {
                int cx = Mathf.Clamp(x0 + dx, 0, map.NavWidth - 1), cz = Mathf.Clamp(z0 + dz, 0, map.NavLength - 1);
                if ((map.NavLayers[map.NavIndex(cx, cz)] & (byte)bit) != 0) sum += (dx == 0 ? 1f - tx : tx) * (dz == 0 ? 1f - tz : tz);
            }
            return sum;
        }

        // Palette (owner's breakdown, 2026-09-21): umber dirt, peat, olive drab; water is dark and takes its light from
        // the sky reflection in the shader (texture alpha 0.4 .. 0 = standing water by depth, 0.5 = liquid mud, below 1 = a
        // slick sheen), not from a pale albedo.
        static readonly Color Ink = new Color(0.13f, 0.105f, 0.085f);
        static readonly Color MudDark = new Color(0.235f, 0.205f, 0.175f);   // thick wet mud: mid ground, crater rims
        static readonly Color MudMid = new Color(0.325f, 0.285f, 0.24f);
        static readonly Color MudPale = new Color(0.42f, 0.37f, 0.31f);   // dry churned dirt: ridges and berms
        static readonly Color Puddle = new Color(0.20f, 0.215f, 0.19f);

        static readonly Color Occlusion = new Color(0.115f, 0.085f, 0.065f);
        float[] propShade; int shadeW, shadeL;

        /// <summary>0..0.7: how much a solid prop darkens the ground here. Stamped once at half-metre cells.</summary>
        float PropShade(MapData map, float wx, float wz)
        {
            if (propShade == null)
            {
                shadeW = map.Height.Width * 2; shadeL = map.Height.Length * 2; propShade = new float[shadeW * shadeL];
                for (int i = 0; i < map.Props.Length; i++)
                {
                    var prop = map.Props[i];
                    if (prop.Kind == PropKind.Bridge) continue;
                    float reach = prop.Kind == PropKind.Wreck ? 4.2f : prop.Kind == PropKind.Log ? 1.7f : 1.25f;
                    int x0 = Mathf.Max(0, (int)((prop.Pos.x - reach) * 2f)), x1 = Mathf.Min(shadeW - 1, (int)((prop.Pos.x + reach) * 2f));
                    int z0 = Mathf.Max(0, (int)((prop.Pos.z - reach) * 2f)), z1 = Mathf.Min(shadeL - 1, (int)((prop.Pos.z + reach) * 2f));
                    for (int z = z0; z <= z1; z++)
                    for (int x = x0; x <= x1; x++)
                    {
                        float d = Vector2.Distance(new Vector2((x + .5f) * .5f, (z + .5f) * .5f), new Vector2(prop.Pos.x, prop.Pos.z)) / reach;
                        float v = .7f * (1f - Mathf.SmoothStep(.25f, 1f, d));
                        if (v > propShade[z * shadeW + x]) propShade[z * shadeW + x] = v;
                    }
                }
            }
            float fx = Mathf.Clamp(wx * 2f - .5f, 0f, shadeW - 1.001f), fz = Mathf.Clamp(wz * 2f - .5f, 0f, shadeL - 1.001f);
            int ix = (int)fx, iz = (int)fz; float tx = fx - ix, tz = fz - iz;
            return Mathf.Lerp(Mathf.Lerp(propShade[iz * shadeW + ix], propShade[iz * shadeW + ix + 1], tx),
                              Mathf.Lerp(propShade[(iz + 1) * shadeW + ix], propShade[(iz + 1) * shadeW + ix + 1], tx), tz);
        }

        static float Band(float a, float b, float value) => Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(a, b, value));

        /// <summary>Bare painted mud: three flat tones from two octaves of noise, an ink line where two tones meet.</summary>
        static Color Tone(float wx, float wz)
        {
            float broad = Mathf.PerlinNoise(wx * .07f + 91f, wz * .07f);
            // Quiet pigment variation. Dark lines describe erosion and material edges, not noise thresholds.
            Color c = Color.Lerp(MudMid * .93f, MudMid * 1.07f, broad);
            // Soil follows the mounds: dry and pale on the rises, dark and wet in the dips between them.
            float mound = BattlefieldSurface.Mound(wx, wz);
            c = Color.Lerp(c, MudPale, Band(.12f, .30f, mound) * .75f);
            c = Color.Lerp(c, MudDark, Band(-.08f, -.30f, mound) * .50f);
            return c;
        }

        static Color Sediment(Color c, float x, float z, float activity)
        {
            const float sx = 2.3f, sz = 1.45f;
            int ix = Mathf.FloorToInt(x / sx), iz = Mathf.FloorToInt(z / sz);
            if (Grain(ix, iz + 81) > activity) return c;
            float cx = (ix + .3f + Grain(ix, iz + 82) * .4f) * sx;
            float cz = (iz + .3f + Grain(ix, iz + 83) * .4f) * sz;
            float dx = (x - cx) / (.40f + Grain(ix, iz + 84) * .45f), dz = (z - cz) / .29f;
            float arc = Mathf.Sqrt(dx * dx + dz * dz) + Mathf.Sin(x * 14f + z * 5f) * .11f;
            if (z > cz + .09f || Mathf.Abs(dx) > 1.25f) return c;
            if (arc > .86f && arc < 1.10f) return Color.Lerp(c, Ink, .67f);
            if (arc > 1.11f && arc < 1.40f) return Color.Lerp(c, MudPale, .75f);
            return c;
        }

        /// <summary>The colour of one point of ground from what the sim knows about it.</summary>
        Color GroundColor(MapData map, float wx, float wz)
        {
            var sample = Surface.At(wx, wz);
            float h = sample.Height;
            int nx = Mathf.Clamp((int)(wx / MapData.NavCellSize), 0, map.NavWidth - 1), nz = Mathf.Clamp((int)(wz / MapData.NavCellSize), 0, map.NavLength - 1);
            var layer = (NavLayer)map.NavLayers[map.NavIndex(nx, nz)];
            Color c = Tone(wx, wz);
            c = Sediment(c, wx, wz, sample.BankDistance < 4f ? .88f : sample.Concavity > .1f ? .60f : .23f);

            if ((layer & NavLayer.Trench) != 0)
            {
                // duckboards: planks across the trench, a dark gap between them
                // Actual duckboards are instanced in the kit; the banks remain earth rather than striped wood.
                c = Color.Lerp(Ink, Tone(wx, wz), 0.70f);
                c.a = 1f;
                // soaked field: long stretches of the trench floor stand in water, shallow, under and beside the duckboards
                if (flooding > 0f)
                {
                    float bay = Mathf.PerlinNoise(wx * .07f + 211f, wz * .07f + 97f) + .25f * Mathf.PerlinNoise(wx * .9f, wz * .9f + 40f);
                    float under = (bay - (1.02f - flooding * .62f)) / .16f;
                    if (under > 0f) { c = Puddle; c.a = .4f * (1f - Mathf.Clamp01(under) * .45f); }
                }
                return c;
            }

            float mud = Amount(map, wx, wz, NavLayer.Mud);
            c = Color.Lerp(c, MudDark, 0.28f * mud);
            if (sample.BankDistance < 3.7f)
            {
                float mass = Surface.BankRise(wx, wz);
                c = Color.Lerp(c, new Color(.45f, .395f, .325f), Mathf.Clamp01(mass * 1.5f));
                float crest = .75f + Mathf.Sin(wx * 3f + wz * 3f) * .13f;
                if (Mathf.Abs(sample.BankDistance - crest) < .09f) c = Color.Lerp(c, Ink, .64f);
                float erosion = Mathf.Sin(wx * 5.8f + wz * 5.3f + Mathf.Sin(wx + wz) * .8f);
                if (sample.BankDistance < 1.8f && erosion > .95f) c = Color.Lerp(c, Ink, .40f);
                float d = sample.BankDistance + (Mathf.PerlinNoise(wx * .8f, wz * .8f) - .5f) * .6f;
                float seam = Mathf.Abs(d - 2.3f);
                c = Color.Lerp(c, MudPale, .25f * (1f - Mathf.Clamp01(d / 3.7f)));
                if (seam < .10f) c = Color.Lerp(c, Ink, .72f);
                else if (seam < .24f) c = Color.Lerp(c, MudPale, .7f);
            }
            // Painted contact occlusion: a warm dark seat under the sandbag courses and around every trunk, stump, log
            // and wreck, so they sit in the mud instead of floating on it.
            float seat = PropShade(map, wx, wz);
            if (sample.BankDistance > .15f && sample.BankDistance < 1.5f) seat = Mathf.Max(seat, .55f * (1f - Mathf.Abs(sample.BankDistance - .8f) / .7f));
            if (seat > 0f) c = Color.Lerp(c, Occlusion, Mathf.Clamp01(seat));
            // Cart ruts and a trampled path run along every trench, just clear of the bank: two wheel lines a cart's
            // width apart with a pale ridge squeezed up beside each, and a darker beaten strip where men walk. Broken
            // up by noise so they fade in and out instead of ruling the field.
            if (sample.BankDistance > 3.9f && sample.BankDistance < 7.6f && sample.Hollow < 0)
            {
                float wander = (Mathf.PerlinNoise(wx * .11f + 3f, wz * .11f + 47f) - .5f) * 1.4f;
                float d = sample.BankDistance + wander;
                float worn = Mathf.PerlinNoise(wx * .06f + 19f, wz * .06f + 7f);
                if (worn > .34f)
                {
                    float wheel = Mathf.Min(Mathf.Abs(d - 5.3f), Mathf.Abs(d - 6.7f));
                    if (wheel < .11f) c = Color.Lerp(c, Ink, .62f);
                    else if (wheel < .26f) c = Color.Lerp(c, MudPale, .55f);
                    else if (d > 5.3f && d < 6.7f) c = Color.Lerp(c, MudDark, .30f);
                }
                if (Mathf.Abs(d - 4.4f) < .34f && worn < .62f) c = Color.Lerp(c, MudDark, .42f);
            }
            // shell holes, read from the ground itself so they are round: how far this point lies under the ground
            // 3 m around it. Dark inside, an ink rim, a pale lip of thrown earth (and a pale parapet along a trench).
            var hf = map.Height;
            float bowlX = (hf.Sample(wx - 3f, wz) + hf.Sample(wx + 3f, wz)) * 0.5f - h, bowlZ = (hf.Sample(wx, wz - 3f) + hf.Sample(wx, wz + 3f)) * 0.5f - h;
            float hollow = Mathf.Min(bowlX, bowlZ);   // a shell hole is hollow both ways; the foot of a slope only one way
            float bowl = hollow > 0.1f ? (bowlX + bowlZ) * 0.5f : Mathf.Min(0f, (bowlX + bowlZ) * 0.5f);
            bool inside = bowl > 0.34f, silt = false, water = false;
            if (sample.Hollow >= 0)
            {
                var depression = Surface.Hollows[sample.Hollow];
                Vector2 delta = new Vector2(wx, wz) - depression.Center;
                float a = Mathf.Atan2(delta.y, delta.x);
                float r = delta.magnitude / depression.Radius + Mathf.Sin(a * 7f + depression.Center.x) * .04f;
                if (r < .72f) { c = Color.Lerp(Ink, MudDark, .45f); silt = true; }
                else if (r < .92f) c = Color.Lerp(Ink, MudDark, .13f);
                else if (r < 1.02f) c = Ink;
                else if (r < 1.17f) c = MudPale;
            }
            if (inside && sample.Hollow < 0) c = Color.Lerp(MudDark, Ink, 0.35f);
            else if (sample.Hollow < 0 && bowl > 0.25f) c = Color.Lerp(c, Ink, .55f);
            else if (sample.Hollow < 0 && bowl < -0.12f) c = Color.Lerp(c, MudPale, 0.22f);

            // Mud by zone: a dried, cracked crust on the rises; a slick sheen in the dips between them (alpha below 1 is
            // gloss in TW/Toon); and round every ladder and ramp a dark beaten patch with boot prints pointing both ways.
            float sheen = 0f;
            if (sample.Hollow < 0 && !inside && sample.BankDistance > 1.5f)
            {
                float mound = BattlefieldSurface.Mound(wx, wz);
                float crust = Band(.15f, .30f, mound) * Band(.38f, .56f, Mathf.PerlinNoise(wx * .05f + 61f, wz * .05f + 17f));
                if (crust > .02f && sample.BankDistance > 7f)
                {
                    float seam = Crackle(wx + .25f * Mathf.PerlinNoise(wx * 1.3f, wz * 1.3f), wz, out float plate);
                    c = Color.Lerp(c, MudPale * (.93f + .14f * plate), crust * .50f);
                    if (seam < .05f) c = Color.Lerp(c, Ink, crust * .62f);
                }
                sheen = Band(-.10f, -.28f, mound) * .30f;
                c = Color.Lerp(c, MudDark, sheen * .6f);
                // Texturing by rule, as a terrain tool's splat rules do it. Flow: where water gathers (BattlefieldSurface.Rill)
                // the ground is a dark, slick channel with a pale margin of washed-out silt beside it. Slope: steep ground
                // has shed its topsoil and shows streaked subsoil.
                float rill = Surface.Rill(wx, wz);
                if (rill > .04f)
                {
                    if (rill < .30f) c = Color.Lerp(c, MudPale, Band(.04f, .16f, rill) * (1f - Band(.20f, .30f, rill)) * .34f);
                    c = Color.Lerp(c, Color.Lerp(MudDark, Ink, .40f), Band(.22f, .55f, rill) * .80f);
                    sheen = Mathf.Max(sheen, Band(.25f, .6f, rill) * .34f);
                }
                if (sample.Slope > .30f)
                {
                    float steep = Band(.30f, .65f, sample.Slope);
                    c = Color.Lerp(c, new Color(.285f, .235f, .185f), steep * .55f);
                    if (Mathf.PerlinNoise(wx * 3.1f + 7f, wz * 3.1f + 3f) > .60f) c = Color.Lerp(c, Ink, steep * .35f);
                }
                float trodden = churn != null ? Churn(map, wx, wz) : 0f;
                if (trodden > .02f)
                {
                    float blot = Mathf.PerlinNoise(wx * 1.9f + 5f, wz * 1.9f + 71f);
                    c = Color.Lerp(c, Color.Lerp(MudDark, Ink, .30f), trodden * (.30f + .50f * blot));
                    int bx = Mathf.FloorToInt(wx / .55f), bz = Mathf.FloorToInt(wz / .55f);
                    if (Grain(bx + 907, bz + 411) < .55f * trodden + .15f)
                    {
                        float ox = (bx + .25f + .5f * Grain(bx, bz + 77)) * .55f - wx, oz = (bz + .25f + .5f * Grain(bx + 55, bz)) * .55f - wz;
                        float lean = (Grain(bx + 3, bz + 9) - .5f) * .9f; float ax = ox + oz * lean;
                        if (ax * ax / (.075f * .075f) + oz * oz / (.15f * .15f) < 1f) c = Color.Lerp(c, Ink, .55f);
                    }
                    sheen = Mathf.Max(sheen, trodden * .26f);
                }
            }

            // puddles: standing water just above the water table, and in the low spots of muddy ground
            // Gradient noise and a warped domain avoid the square islands produced by thresholded lattice noise.
            float px = wx + 2f * Mathf.PerlinNoise(wx * 0.27f + 8f, wz * 0.27f);
            float pz = wz + 2f * Mathf.PerlinNoise(wx * 0.27f, wz * 0.27f + 31f);
            float pool = Mathf.PerlinNoise(px * 0.20f + 133f, pz * 0.20f);
            float wetness = pool + 0.08f * mud + (inside ? 0.10f : 0f) + flooding * .075f;
            if (map.WaterLevel > MapData.NoWater) wetness += Mathf.Clamp01(1f - (h - map.WaterLevel) / 0.5f) * 0.45f;
            if (wetness > 0.82f)
            {
                c = Puddle; water = true;
                float ripple = Mathf.PerlinNoise(wx * 1.8f, wz * 7f + 19f);
                if (ripple > 0.68f) c = Color.Lerp(c, new Color(0.36f, 0.39f, 0.38f), 0.5f);
            }
            else if (wetness > 0.808f) c = Color.Lerp(Puddle, new Color(0.52f, 0.54f, 0.52f), .8f);   // a thin pale shoreline where the sky catches the wet edge
            else if (wetness > 0.70f)
            {
                // soaked margin: darkest at the water, fading out into the mud; glossy close in, so it still mirrors a little
                float soak = Mathf.SmoothStep(0f, 1f, (wetness - .70f) / .108f);
                c = Color.Lerp(c, Color.Lerp(MudDark, Ink, .45f), soak * .85f);
                if (soak > .55f) silt = true;
            }

            // alpha, as TW/Toon reads it: 0.4 .. 0 standing water from its edge to its deepest, 0.5 liquid mud, below 1 a sheen
            c.a = water ? .4f * (1f - Mathf.Clamp01((wetness - .82f) / .09f)) : silt ? .5f : 1f - sheen;
            float poolDepth = flooding > 0f ? Surface.PoolDepth(wx, wz) : 0f;
            if (poolDepth > .01f) { c = Puddle; c.a = .4f * (1f - Mathf.Clamp01(poolDepth / .7f)); }   // a flooded shell hole: the river's bands by its own depth
            return Coast(map, wx, wz, h, c);
        }

        /// <summary>The coast takes over from the mud up the beach: pale dry sand at the top, darker and glossy where
        /// the tide wets it, ribbed by the runnels, with a line of wrack and weed along the high-water mark. The same
        /// rule paints the ground inside the map and the horizon beyond it, or the sand would stop at the map edge.</summary>
        static Color Coast(MapData map, float wx, float wz, float ground, Color c)
        {
            if (map == null || !map.HasSea) return c;
            float onto = (wz - map.SeaStartZ) * map.SeaAway;
            if (onto <= 0f) return c;
            float sand = Mathf.Clamp01(onto / 7f);
            float wet = Mathf.Clamp01((map.SeaLevel + .40f - ground) / .85f);
            var tone = Color.Lerp(new Color(.495f, .450f, .370f), new Color(.285f, .275f, .250f), wet);
            float rib = Mathf.PerlinNoise(wx * .33f + 5f, wz * .95f + 17f);            // ribs of sand, running with the shore
            float grit = Mathf.PerlinNoise(wx * 2.3f, wz * 2.3f + 63f);
            tone *= .93f + .11f * rib + .04f * grit;
            float wrack = 1f - Mathf.Clamp01(Mathf.Abs(ground - (map.SeaLevel + .62f)) / .13f);
            if (Mathf.PerlinNoise(wx * .6f + 91f, wz * .6f) > .42f) tone = Color.Lerp(tone, new Color(.215f, .205f, .165f), wrack * .60f);
            c = Color.Lerp(c, tone, sand);
            c.a = Mathf.Lerp(c.a, Mathf.Lerp(1f, .45f, wet), sand);                     // wet sand mirrors: below 1 is gloss in TW/Toon
            return c;
        }
    }
}
