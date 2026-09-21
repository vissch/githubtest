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
        public BattlefieldSurface Surface { get; private set; }
        public const int ChunkMeters = 32;
        const float GridStep = .5f; // Presentation mesh only: simulation height and traversal remain untouched.
        const int Tpm = 8;   // 720 x 1920 on the standard map: ~7 MiB including mipmaps; no per-pixel noise shader

        sealed class Chunk { public Mesh Mesh; public Vector3[] Verts; public Vector3[] Normals; public int X0, Z0, W, L; public bool Dirty; }

        readonly List<Chunk> chunks = new List<Chunk>();
        readonly List<Object> owned = new List<Object>();
        int chunksX;
        Texture2D colorTex;
        RenderGroundGrid renderGrid;
        bool colorDirty, subscribed;
        readonly Queue<Vector2Int> paintTiles = new Queue<Vector2Int>();
        readonly HashSet<Vector2Int> queuedTiles = new HashSet<Vector2Int>();
        readonly List<TW.Sim.SimEvent> scorchMarks = new List<TW.Sim.SimEvent>();
        bool hollowsDirty;
        readonly System.Diagnostics.Stopwatch paintWatch = new System.Diagnostics.Stopwatch();
        public int PendingPaintTiles => paintTiles.Count;
        public float LastPaintMilliseconds { get; private set; }

        void Start()
        {
            if (Host == null || Host.Local == null) return;
            var map = Host.Local.Map;
            var hf = map.Height;
            Surface = new BattlefieldSurface(map);
            renderGrid = new RenderGroundGrid { Width = Mathf.RoundToInt(hf.Width / GridStep) + 1, Length = Mathf.RoundToInt(hf.Length / GridStep) + 1, Step = GridStep };
            renderGrid.Heights = new Unity.Collections.NativeArray<float>(renderGrid.Width * renderGrid.Length, Unity.Collections.Allocator.Persistent);
            var old = GetComponent<MeshRenderer>();   // scenes built for the single-mesh version
            if (old != null) old.enabled = false;

            var mat = Toon(Color.white);
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
            if (GetComponent<Atmosphere>() == null) gameObject.AddComponent<Atmosphere>();
            var props = GetComponent<BattlefieldProps>();
            if (props == null) props = gameObject.AddComponent<BattlefieldProps>();
            props.Host = Host;
        }

        /// <summary>Vertices sit on the corners between height cells; normals come from the heightfield, not the chunk.</summary>
        void Fill(Chunk c, TW.Sim.Terrain.Heightfield hf)
        {
            for (int z = 0; z < c.L; z++)
            for (int x = 0; x < c.W; x++)
            {
                float wx = c.X0 + x * GridStep, wz = c.Z0 + z * GridStep;
                c.Verts[z * c.W + x] = new Vector3(wx, Surface.VisualHeight(wx, wz), wz);
                renderGrid.Heights[Mathf.RoundToInt(wz / GridStep) * renderGrid.Width + Mathf.RoundToInt(wx / GridStep)] = c.Verts[z * c.W + x].y;
                float dx = Surface.VisualHeight(wx + .25f, wz) - Surface.VisualHeight(wx - .25f, wz), dz = Surface.VisualHeight(wx, wz + .25f) - Surface.VisualHeight(wx, wz - .25f);
                c.Normals[z * c.W + x] = new Vector3(-dx, .5f, -dz).normalized;
            }
        }

        static Texture2D strokes;

        /// <summary>A ground material: toon shading, brush strokes in world space, no hull outline.</summary>
        Material Toon(Color color)
        {
            var mat = new Material(Shader.Find("TW/Toon (URP)")) { hideFlags = HideFlags.HideAndDontSave };
            mat.SetColor("_BaseColor", color);
            if (strokes == null) strokes = BuildStrokes();
            mat.SetTexture("_DetailMap", strokes);
            mat.SetFloat("_DetailScale", 1f / 12f);
            mat.SetFloat("_DetailStrength", 0.12f);
            mat.SetShaderPassEnabled("SRPDefaultUnlit", false);
            owned.Add(mat);
            return mat;
        }

        /// <summary>Tiling brush marks: short dark dashes and a few pale flecks on mid grey.</summary>
        static Texture2D BuildStrokes()
        {
            const int n = 256;
            var px = new Color32[n * n];
            for (int i = 0; i < px.Length; i++) px[i] = new Color32(128, 128, 128, 255);
            for (int k = 0; k < 420; k++)
            {
                int x = (int)(Grain(k, 11) * n), z = (int)(Grain(k, 23) * n), len = 4 + (int)(Grain(k, 37) * 9f);
                float slope = Grain(k, 41) * 0.8f - 0.4f;
                byte v = (byte)(Grain(k, 53) < 0.78f ? 70 : 176);
                for (int t = 0; t < len; t++)
                {
                    int xx = (x + t) % n, zz = ((z + (int)(t * slope)) % n + n) % n;
                    px[zz * n + xx] = new Color32(v, v, v, 255);
                    if (v < 128) px[((zz + 1) % n) * n + xx] = new Color32(v, v, v, 255);
                }
            }
            var tex = new Texture2D(n, n, TextureFormat.RGBA32, true) { wrapMode = TextureWrapMode.Repeat, filterMode = FilterMode.Bilinear, anisoLevel = 4, hideFlags = HideFlags.HideAndDontSave };
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
            return Mathf.Lerp(edge, SkirtLevel, Mathf.SmoothStep(0f, 1f, Mathf.Clamp01(outside / SkirtBlend)));
        }

        void BuildSkirt(MapData map)
        {
            var hf = map.Height;
            float w = map.SizeMeters.x, l = map.SizeMeters.y;
            const float far = 1500f, level = SkirtLevel;
            float[] outs = { 0f, 4f, 10f, 18f, 30f, 60f, 140f, far };
            var verts = new List<Vector3>(); var cols = new List<Color>(); var tris = new List<int>();
            // side 0/1: along Z at x = 0 / w (these also cover the corners); side 2/3: along X at z = 0 / l
            for (int side = 0; side < 4; side++)
            {
                bool alongZ = side < 2; bool high = (side & 1) == 1;
                float from = alongZ ? -far : 0f, to = alongZ ? l + far : w, span = alongZ ? l : w;
                var stops = new List<float> { from };
                for (float t = 0f; t <= span; t += 1f) stops.Add(t);   // the ground's own vertex spacing, so the two meet exactly
                if (alongZ) stops.Add(to); else stops.Add(span);
                int row0 = verts.Count, cols_ = outs.Length;
                foreach (float t in stops)
                {
                    float tc = Mathf.Clamp(t, 0f, span);
                    float edge = alongZ ? hf.Sample(high ? w : 0f, tc) : hf.Sample(tc, high ? l : 0f);
                    float beyond = alongZ ? Mathf.Max(0f, Mathf.Max(-t, t - l)) : 0f;   // past the corner the ground is already level
                    for (int k = 0; k < cols_; k++)
                    {
                        float d = outs[k], blend = Mathf.SmoothStep(0f, 1f, Mathf.Clamp01(Mathf.Max(d, beyond) / 30f));
                        float y = Mathf.Lerp(edge, level, blend);
                        float off = high ? span2(alongZ, w, l) + d : -d;
                        var v = alongZ ? new Vector3(off, y, t) : new Vector3(t, y, off);
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
            int tw = Mathf.CeilToInt((w + paintedBorder * 2f) * 3f), th = Mathf.CeilToInt((l + paintedBorder * 2f) * 3f);
            var texture = new Texture2D(tw, th, TextureFormat.RGBA32, true) { name = "Painted horizon", wrapMode = TextureWrapMode.Clamp, filterMode = FilterMode.Bilinear, anisoLevel = 4 };
            var pixels = new Color32[tw * th];
            for (int z = 0; z < th; z++)
            for (int x = 0; x < tw; x++)
            {
                float wx = x / 3f - paintedBorder, wz = z / 3f - paintedBorder;
                float outside = Mathf.Max(Mathf.Max(-wx, wx - w), Mathf.Max(-wz, wz - l));
                Color c = Tone(wx, wz);
                if (outside < 5f) c = Color.Lerp(GroundColor(map, Mathf.Clamp(wx, 0f, w - 0.01f), Mathf.Clamp(wz, 0f, l - 0.01f)), c, Band(0f, 5f, outside));
                pixels[z * tw + x] = Color.Lerp(c, MudMid, Band(30f, 96f, outside));
            }
            texture.SetPixels32(pixels); texture.Apply(true, true);
            owned.Add(texture);
            skirtMat.SetTexture("_BaseMap", texture);
            r2.sharedMaterial = skirtMat; r2.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
        }

        static float span2(bool alongZ, float w, float l) => alongZ ? w : l;

        void BuildWater(MapData map)
        {
            // Opaque painted water shares lighting/fog with the puddle pigment and writes depth before VFX.
            var m = Toon(Puddle);
            m.SetFloat("_DetailStrength", 0.05f);
            m.SetFloat("_Gloss", 1f);
            float w = map.SizeMeters.x, l = map.SizeMeters.y, y = map.WaterLevel;
            var mesh = new Mesh
            {
                name = "Water",
                vertices = new[] { new Vector3(0f, y, 0f), new Vector3(0f, y, l), new Vector3(w, y, l), new Vector3(w, y, 0f) },
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

        void Update()
        {
            if (Host == null || Host.Local == null || colorTex == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            if (hollowsDirty) { Surface.RefreshHollows(); hollowsDirty = false; }
            // Small tiles bound each work item. Terrain pigment catches up over frames after a barrage.
            paintWatch.Restart();
            while (paintTiles.Count > 0 && paintWatch.Elapsed.TotalMilliseconds < 2.0)
            {
                var tile = paintTiles.Dequeue(); queuedTiles.Remove(tile);
                RepaintTile(tile); colorDirty = true;
            }
            LastPaintMilliseconds = (float)paintWatch.Elapsed.TotalMilliseconds;
            if (colorDirty) { colorTex.Apply(true, false); colorDirty = false; }
            for (int i = 0; i < chunks.Count; i++)
            {
                var c = chunks[i];
                if (!c.Dirty) continue;
                c.Dirty = false;
                Fill(c, Host.Local.Map.Height);
                c.Mesh.vertices = c.Verts; c.Mesh.normals = c.Normals;
                c.Mesh.RecalculateBounds();
            }
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
                if (scorchMarks.Count == 64) scorchMarks.RemoveAt(0);
                scorchMarks.Add(e);
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
                if (i >= 0 && i < chunks.Count) chunks[i].Dirty = true;
            }
        }

        void RepaintTile(Vector2Int tile)
        {
            int x1 = Mathf.Min(colorTex.width, (tile.x + 1) * 2 * Tpm), z1 = Mathf.Min(colorTex.height, (tile.y + 1) * 2 * Tpm);
            for (int z = tile.y * 2 * Tpm; z < z1; z++) for (int x = tile.x * 2 * Tpm; x < x1; x++)
            {
                float wx = (x + .5f) / Tpm, wz = (z + .5f) / Tpm;
                Color c = GroundColor(Host.Local.Map, wx, wz);
                float burn = 0f;
                foreach (var mark in scorchMarks)
                {
                    float radius = mark.Scalar * 1.25f;
                    if (radius <= 0f || Mathf.Abs(wx - mark.Pos.x) > radius || Mathf.Abs(wz - mark.Pos.z) > radius) continue;
                    float distance = Vector2.Distance(new Vector2(wx, wz), new Vector2(mark.Pos.x, mark.Pos.z));
                    burn = Mathf.Max(burn, .45f * (1f - distance / radius));
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
        // the sky reflection in the shader (texture alpha 0 = standing water, 0.5 = liquid mud), not from a pale albedo.
        static readonly Color Ink = new Color(0.13f, 0.105f, 0.085f);
        static readonly Color MudDark = new Color(0.235f, 0.205f, 0.175f);   // thick wet mud: mid ground, crater rims
        static readonly Color MudMid = new Color(0.325f, 0.285f, 0.24f);
        static readonly Color MudPale = new Color(0.42f, 0.37f, 0.31f);   // dry churned dirt: ridges and berms
        static readonly Color Puddle = new Color(0.20f, 0.215f, 0.19f);

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

            // puddles: standing water just above the water table, and in the low spots of muddy ground
            // Gradient noise and a warped domain avoid the square islands produced by thresholded lattice noise.
            float px = wx + 2f * Mathf.PerlinNoise(wx * 0.27f + 8f, wz * 0.27f);
            float pz = wz + 2f * Mathf.PerlinNoise(wx * 0.27f, wz * 0.27f + 31f);
            float pool = Mathf.PerlinNoise(px * 0.20f + 133f, pz * 0.20f);
            float wetness = pool + 0.08f * mud + (inside ? 0.10f : 0f);
            if (map.WaterLevel > MapData.NoWater) wetness += Mathf.Clamp01(1f - (h - map.WaterLevel) / 0.5f) * 0.45f;
            if (wetness > 0.82f)
            {
                c = Puddle; water = true;
                float ripple = Mathf.PerlinNoise(wx * 1.8f, wz * 7f + 19f);
                if (ripple > 0.68f) c = Color.Lerp(c, new Color(0.36f, 0.39f, 0.38f), 0.5f);
            }
            else if (wetness > 0.79f) c = Ink;
            else if (wetness > 0.765f) c = Color.Lerp(c, MudDark, 0.65f);   // a soaked margin, not a pale one

            c.a = water ? 0f : silt ? .5f : 1f;
            return c;
        }
    }
}
