// Phase: B2 (implemented with code-made colours; textures and the GPU-displaced version come with the art pass)
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

            var shader = Shader.Find("Universal Render Pipeline/Lit");
            var mat = new Material(shader) { color = Color.white };
            mat.SetFloat("_Smoothness", 0.08f);
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

        void BuildWater(MapData map)
        {
            var unlit = Shader.Find("Universal Render Pipeline/Unlit");
            var m = new Material(unlit) { color = new Color(0.20f, 0.24f, 0.22f, 0.82f) };
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
            for (int z = z0; z <= z1; z++)
            for (int x = x0; x <= x1; x++)
            {
                Color c = GroundColor(map, x, z);
                float d = Vector2.Distance(new Vector2(x + 0.5f, z + 0.5f), new Vector2(e.Pos.x, e.Pos.z));
                if (e.Type == TW.Sim.SimEventType.CraterStamp && d < e.Scalar * 1.25f) c = Color.Lerp(c, new Color(0.10f, 0.09f, 0.08f), 0.55f * (1f - d / (e.Scalar * 1.25f)));   // fresh burn
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
            int w = map.Height.Width, l = map.Height.Length;
            var tex = new Texture2D(w, l, TextureFormat.RGBA32, true) { filterMode = FilterMode.Bilinear, wrapMode = TextureWrapMode.Clamp, anisoLevel = 4 };
            var px = new Color[w * l];
            for (int z = 0; z < l; z++)
            for (int x = 0; x < w; x++) px[z * w + x] = GroundColor(map, x, z);
            Color[] teamTint = { new Color(0.55f, 0.45f, 0.25f), new Color(0.35f, 0.40f, 0.55f) };
            for (int o = 0; o < map.Objectives.Length; o++)
            {
                var def = map.Objectives[o];
                if (def.Kind != ObjectiveKind.HQ) continue;
                for (int k = 0; k < def.CellCount; k++)
                {
                    int cell = map.ObjectiveCells[def.CellStart + k];
                    int nx = cell % map.NavWidth, nz = cell / map.NavWidth;
                    for (int dz = 0; dz < 2; dz++) for (int dx = 0; dx < 2; dx++)
                    {
                        int i = (nz * 2 + dz) * w + nx * 2 + dx;
                        if (i < px.Length) px[i] = Color.Lerp(px[i], teamTint[def.SideTeam & 1], 0.45f);
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

        /// <summary>How much of a layer lies around a height cell, 0..1: nav cells are 2 m blocks, this blends them.</summary>
        static float Amount(MapData map, int x, int z, NavLayer bit)
        {
            float fx = (x + 0.5f) / MapData.NavCellSize - 0.5f, fz = (z + 0.5f) / MapData.NavCellSize - 0.5f;
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

        /// <summary>The colour of one height cell from what the sim knows about it.</summary>
        static Color GroundColor(MapData map, int x, int z)
        {
            var earth = new Color(0.34f, 0.27f, 0.19f);
            var grass = new Color(0.33f, 0.38f, 0.22f);
            var mud = new Color(0.24f, 0.19f, 0.13f);
            var wet = new Color(0.17f, 0.15f, 0.11f);
            var crater = new Color(0.21f, 0.18f, 0.14f);
            var boards = new Color(0.30f, 0.22f, 0.15f);
            var ladder = new Color(0.58f, 0.47f, 0.30f);

            float h = map.Height.HeightAtCell(x, z);
            int nx = Mathf.Min(map.NavWidth - 1, x / 2), nz = Mathf.Min(map.NavLength - 1, z / 2);
            var layer = (NavLayer)map.NavLayers[map.NavIndex(nx, nz)];
            float patch = BattlefieldGenerator.Noise(91u, x, z, 9f);
            Color c = Color.Lerp(earth, grass, Mathf.Clamp01((h - 1.5f) * 0.9f + (patch - 0.62f) * 1.8f));   // grass survives on the higher ground
            c = Color.Lerp(c, mud, 0.8f * Amount(map, x, z, NavLayer.Mud));
            c = Color.Lerp(c, crater, 0.75f * Amount(map, x, z, NavLayer.Crater));
            if (map.WaterLevel > MapData.NoWater) c = Color.Lerp(c, wet, Mathf.Clamp01(1f - (h - map.WaterLevel) / 0.6f) * 0.8f);   // dark, soaked rim
            if ((layer & NavLayer.Trench) != 0) c = (layer & NavLayer.Link) != 0 ? ladder : boards;
            c *= 0.9f + 0.2f * Grain(x, z);
            c.a = 1f;
            return c;
        }
    }
}
