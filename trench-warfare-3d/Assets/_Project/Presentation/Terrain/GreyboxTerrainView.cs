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
        public const int ChunkMeters = 32;
        const int Tpm = 3;   // colour texels per metre

        sealed class Chunk { public Mesh Mesh; public Vector3[] Verts; public Vector3[] Normals; public int X0, Z0, W, L; public bool Dirty; }

        readonly List<Chunk> chunks = new List<Chunk>();
        int chunksX;
        Texture2D colorTex;
        bool colorDirty, subscribed;

        void Start()
        {
            if (Host == null || Host.Local == null) return;
            var map = Host.Local.Map;
            var hf = map.Height;
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
                c.W = Mathf.Min(ChunkMeters, hf.Width - c.X0) + 1; c.L = Mathf.Min(ChunkMeters, hf.Length - c.Z0) + 1;
                c.Verts = new Vector3[c.W * c.L]; c.Normals = new Vector3[c.W * c.L];
                var uvs = new Vector2[c.W * c.L];
                var tris = new int[(c.W - 1) * (c.L - 1) * 6];
                int t = 0;
                for (int z = 0; z < c.L; z++)
                for (int x = 0; x < c.W; x++)
                {
                    uvs[z * c.W + x] = new Vector2((c.X0 + x) / (float)hf.Width, (c.Z0 + z) / (float)hf.Length);
                    if (x == c.W - 1 || z == c.L - 1) continue;
                    int i = z * c.W + x;
                    tris[t++] = i; tris[t++] = i + c.W; tris[t++] = i + 1;
                    tris[t++] = i + 1; tris[t++] = i + c.W; tris[t++] = i + c.W + 1;
                }
                Fill(c, hf);
                c.Mesh = new Mesh { name = $"Terrain {cx},{cz}", vertices = c.Verts, normals = c.Normals, uv = uvs, triangles = tris };
                c.Mesh.RecalculateBounds();
                var go = new GameObject(c.Mesh.name) { hideFlags = HideFlags.DontSave };
                go.transform.SetParent(transform, false);
                go.AddComponent<MeshFilter>().sharedMesh = c.Mesh;
                go.AddComponent<MeshRenderer>().sharedMaterial = mat;
                chunks.Add(c);
            }

            if (map.WaterLevel > MapData.NoWater) BuildWater(map);
            BuildSkirt(map);
            if (GetComponent<Atmosphere>() == null) gameObject.AddComponent<Atmosphere>();
            var props = GetComponent<BattlefieldProps>();
            if (props == null) props = gameObject.AddComponent<BattlefieldProps>();
            props.Host = Host;
        }

        /// <summary>Vertices sit on the corners between height cells; normals come from the heightfield, not the chunk.</summary>
        static void Fill(Chunk c, TW.Sim.Terrain.Heightfield hf)
        {
            for (int z = 0; z < c.L; z++)
            for (int x = 0; x < c.W; x++)
            {
                float wx = c.X0 + x, wz = c.Z0 + z;
                c.Verts[z * c.W + x] = new Vector3(wx, hf.Sample(wx, wz), wz);
                float dx = hf.Sample(wx + 1f, wz) - hf.Sample(wx - 1f, wz), dz = hf.Sample(wx, wz + 1f) - hf.Sample(wx, wz - 1f);
                c.Normals[z * c.W + x] = new Vector3(-dx, 2f, -dz).normalized;
            }
        }

        static Texture2D strokes;

        /// <summary>A ground material: toon shading, brush strokes in world space, no hull outline.</summary>
        static Material Toon(Color color)
        {
            var mat = new Material(Shader.Find("TW/Toon (URP)")) { hideFlags = HideFlags.HideAndDontSave };
            mat.SetColor("_BaseColor", color);
            if (strokes == null) strokes = BuildStrokes();
            mat.SetTexture("_DetailMap", strokes);
            mat.SetFloat("_DetailScale", 1f / 12f);
            mat.SetFloat("_DetailStrength", 0.22f);
            mat.SetShaderPassEnabled("SRPDefaultUnlit", false);
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
            mesh.SetVertices(verts); mesh.SetColors(cols); mesh.SetTriangles(tris, 0);
            mesh.RecalculateNormals(); mesh.RecalculateBounds();
            var go = new GameObject("Skirt") { hideFlags = HideFlags.DontSave };
            go.transform.SetParent(transform, false);
            go.AddComponent<MeshFilter>().sharedMesh = mesh;
            var r2 = go.AddComponent<MeshRenderer>();
            r2.sharedMaterial = Toon(Color.white); r2.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
        }

        static float span2(bool alongZ, float w, float l) => alongZ ? w : l;

        void BuildWater(MapData map)
        {
            var unlit = Shader.Find("Universal Render Pipeline/Unlit");
            var m = new Material(unlit) { color = new Color(Puddle.r, Puddle.g, Puddle.b, 0.93f) };
            m.SetFloat("_Surface", 1f); m.SetFloat("_Blend", 0f); m.SetFloat("_ZWrite", 0f);
            m.SetInt("_SrcBlend", (int)UnityEngine.Rendering.BlendMode.SrcAlpha);
            m.SetInt("_DstBlend", (int)UnityEngine.Rendering.BlendMode.OneMinusSrcAlpha);
            m.EnableKeyword("_SURFACE_TYPE_TRANSPARENT");
            m.SetOverrideTag("RenderType", "Transparent");
            m.renderQueue = (int)UnityEngine.Rendering.RenderQueue.Transparent - 10;   // under tracers, gas and markers
            m.SetColor("_BaseColor", m.color);
            float w = map.SizeMeters.x, l = map.SizeMeters.y, y = map.WaterLevel;
            var mesh = new Mesh
            {
                name = "Water",
                vertices = new[] { new Vector3(0f, y, 0f), new Vector3(0f, y, l), new Vector3(w, y, l), new Vector3(w, y, 0f) },
                normals = new[] { Vector3.up, Vector3.up, Vector3.up, Vector3.up },
                triangles = new[] { 0, 1, 2, 0, 2, 3 },
            };
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
            if (colorDirty) { colorTex.Apply(false, false); colorDirty = false; }
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

        void OnDestroy() { if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent; }

        /// <summary>A crater landed: repaint its texels and re-read the chunks it touches.</summary>
        void OnSimEvent(TW.Sim.SimEvent e)
        {
            if (e.Type != TW.Sim.SimEventType.CraterStamp && e.Type != TW.Sim.SimEventType.WireBreached) return;
            var map = Host.Local.Map;
            float r = e.Scalar + 2f;
            int x0 = Mathf.Max(0, Mathf.FloorToInt(e.Pos.x - r)), x1 = Mathf.Min(map.Height.Width - 1, Mathf.CeilToInt(e.Pos.x + r));
            int z0 = Mathf.Max(0, Mathf.FloorToInt(e.Pos.z - r)), z1 = Mathf.Min(map.Height.Length - 1, Mathf.CeilToInt(e.Pos.z + r));
            for (int z = z0 * Tpm; z < (z1 + 1) * Tpm; z++)
            for (int x = x0 * Tpm; x < (x1 + 1) * Tpm; x++)
            {
                float wx = (x + 0.5f) / Tpm, wz = (z + 0.5f) / Tpm;
                Color c = GroundColor(map, wx, wz);
                float d = Vector2.Distance(new Vector2(wx, wz), new Vector2(e.Pos.x, e.Pos.z));
                if (e.Type == TW.Sim.SimEventType.CraterStamp && d < e.Scalar * 1.25f) c = Color.Lerp(c, new Color(0.10f, 0.09f, 0.08f), 0.45f * (1f - d / (e.Scalar * 1.25f)));   // fresh burn
                colorTex.SetPixel(x, z, c);
            }
            colorDirty = true;
            for (int cz = z0 / ChunkMeters; cz <= z1 / ChunkMeters; cz++)
            for (int cx = Mathf.Max(0, (x0 - 1) / ChunkMeters); cx <= x1 / ChunkMeters; cx++)
            {
                int i = cz * chunksX + cx;
                if (i >= 0 && i < chunks.Count) chunks[i].Dirty = true;
            }
        }

        Texture2D BuildColorTexture(MapData map)
        {
            int w = map.Height.Width * Tpm, l = map.Height.Length * Tpm;
            var tex = new Texture2D(w, l, TextureFormat.RGBA32, true) { filterMode = FilterMode.Bilinear, wrapMode = TextureWrapMode.Clamp, anisoLevel = 4 };
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
                        if (i < px.Length) px[i] = Color.Lerp(px[i], teamTint[def.SideTeam & 1], 0.30f);
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

        static readonly Color Ink = new Color(0.15f, 0.12f, 0.10f);
        static readonly Color MudDark = new Color(0.245f, 0.21f, 0.195f);
        static readonly Color MudMid = new Color(0.335f, 0.29f, 0.26f);
        static readonly Color MudPale = new Color(0.43f, 0.375f, 0.33f);
        static readonly Color Puddle = new Color(0.43f, 0.46f, 0.49f);

        /// <summary>Bare painted mud: three flat tones from two octaves of noise, an ink line where two tones meet.</summary>
        static Color Tone(float wx, float wz)
        {
            float n = BattlefieldGenerator.Noise(91u, wx, wz, 8f) * 0.55f + BattlefieldGenerator.Noise(47u, wx, wz, 2.9f) * 0.30f + BattlefieldGenerator.Noise(19u, wx, wz, 1.1f) * 0.15f;
            Color c = Color.Lerp(Color.Lerp(MudDark, MudMid, Mathf.SmoothStep(0.455f, 0.47f, n)), MudPale, Mathf.SmoothStep(0.535f, 0.55f, n));
            float edge = Mathf.Abs(n - 0.4625f);
            float broken = BattlefieldGenerator.Noise(5u, wx, wz, 4f);
            if (edge < 0.012f && broken > 0.45f) c = Color.Lerp(c, Ink, 0.55f);   // an ink contour under the darkest tone, broken up
            return c;
        }

        /// <summary>The colour of one point of ground from what the sim knows about it.</summary>
        static Color GroundColor(MapData map, float wx, float wz)
        {
            float h = map.Height.Sample(wx, wz);
            int nx = Mathf.Clamp((int)(wx / MapData.NavCellSize), 0, map.NavWidth - 1), nz = Mathf.Clamp((int)(wz / MapData.NavCellSize), 0, map.NavLength - 1);
            var layer = (NavLayer)map.NavLayers[map.NavIndex(nx, nz)];
            Color c = Tone(wx, wz);

            if ((layer & NavLayer.Trench) != 0)
            {
                // duckboards: planks across the trench, a dark gap between them
                float plank = wx * 2.2f - Mathf.Floor(wx * 2.2f);
                Color wood = (layer & NavLayer.Link) != 0 ? new Color(0.52f, 0.42f, 0.29f) : new Color(0.36f, 0.28f, 0.20f);
                wood *= 0.92f + 0.16f * Grain((int)Mathf.Floor(wx * 2.2f), nz);
                c = plank < 0.12f ? Color.Lerp(Ink, wood, 0.25f) : wood;
                c.a = 1f;
                return c;
            }

            float mud = Amount(map, wx, wz, NavLayer.Mud);
            c = Color.Lerp(c, MudDark, 0.55f * mud);
            // shell holes, read from the ground itself so they are round: how far this point lies under the ground
            // 3 m around it. Dark inside, an ink rim, a pale lip of thrown earth (and a pale parapet along a trench).
            var hf = map.Height;
            float bowlX = (hf.Sample(wx - 3f, wz) + hf.Sample(wx + 3f, wz)) * 0.5f - h, bowlZ = (hf.Sample(wx, wz - 3f) + hf.Sample(wx, wz + 3f)) * 0.5f - h;
            float hollow = Mathf.Min(bowlX, bowlZ);   // a shell hole is hollow both ways; the foot of a slope only one way
            float bowl = hollow > 0.1f ? (bowlX + bowlZ) * 0.5f : Mathf.Min(0f, (bowlX + bowlZ) * 0.5f);
            bool inside = bowl > 0.34f;
            if (inside) c = Color.Lerp(MudDark, Ink, 0.35f);
            else if (bowl > 0.25f) c = Ink;
            else if (bowl < -0.12f) c = Color.Lerp(c, MudPale, 0.75f);

            // puddles: standing water just above the water table, and in the low spots of muddy ground
            float pool = BattlefieldGenerator.Noise(133u, wx, wz, 6.5f);
            float wetness = pool + 0.08f * mud + (inside ? 0.10f : 0f);
            if (map.WaterLevel > MapData.NoWater) wetness += Mathf.Clamp01(1f - (h - map.WaterLevel) / 0.5f) * 0.45f;
            if (wetness > 0.86f) c = Puddle * (0.96f + 0.08f * BattlefieldGenerator.Noise(7u, wx, wz, 1.5f));
            else if (wetness > 0.835f) c = Color.Lerp(Ink, MudDark, 0.25f);

            c.a = 1f;
            return c;
        }
    }
}
