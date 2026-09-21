// Phase: B2 (implemented) — the drifting part of the fog: large soft cards along the fog bank, so something passes in
// front of the trees at the edge, and a few low ones over the middle of the field. One mesh, one material, one draw
// call; the shader (TW/Fog Wisp) moves and turns the cards, so nothing here runs after Start. The count is capped:
// each card is a screenful of blending at worst, and the minimum GPU pays for blending by the pixel.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed class FogWisps : MonoBehaviour
    {
        public const int MaxCards = 28;
        [Range(0f, 1f)] public float BankOpacity = 0.34f, FieldOpacity = 0.15f;
        Mesh mesh; Material material; Texture2D cloud;

        public void Build(MapData map)
        {
            float w = map.SizeMeters.x, l = map.SizeMeters.y;
            float ground = map.WaterLevel > MapData.NoWater ? map.WaterLevel : 0f;
            var pos = new List<Vector3>(); var corner = new List<Vector2>(); var drift = new List<Vector4>(); var shape = new List<Vector4>(); var tris = new List<int>();
            int cards = 0;
            void Card(Vector3 home, Vector2 dir, float run, float width, float height, float opacity, float speed)
            {
                if (cards >= MaxCards) return;
                float phase = Hash(cards, 7);
                int v0 = pos.Count;
                for (int k = 0; k < 4; k++)
                {
                    pos.Add(home); corner.Add(new Vector2(k == 0 || k == 3 ? -1f : 1f, k < 2 ? -1f : 1f));
                    drift.Add(new Vector4(dir.x, dir.y, run, phase)); shape.Add(new Vector4(width, height, opacity, speed));
                }
                tris.Add(v0); tris.Add(v0 + 2); tris.Add(v0 + 1); tris.Add(v0); tris.Add(v0 + 3); tris.Add(v0 + 2);
                cards++;
            }
            // along the two long edges, unevenly: drifting with the edge, a little outside it
            int perSide = Mathf.Clamp(Mathf.RoundToInt(l / 27f), 3, 9);
            for (int side = 0; side < 2; side++)
            for (int k = 0; k < perSide; k++)
            {
                float z = (k + .2f + .6f * Hash(k, 11 + side)) / perSide * l;
                float x = side == 0 ? -5f - 9f * Hash(k, 13 + side) : w + 5f + 9f * Hash(k, 13 + side);
                Card(new Vector3(x, ground, z), new Vector2(0f, side == 0 ? 1f : -1f), 70f, 36f + 20f * Hash(k, 17 + side), 9f + 6f * Hash(k, 19 + side), BankOpacity, .45f + .4f * Hash(k, 23 + side));
            }
            // across the two ends
            for (int end = 0; end < 2; end++)
            for (int k = 0; k < 2; k++)
                Card(new Vector3((k + .5f) / 2f * w, ground, end == 0 ? -10f : l + 10f), new Vector2(1f, 0f), w * .8f, 44f, 12f, BankOpacity, .5f);
            // low over the middle of the field, where the two sides look at each other
            for (int k = 0; cards < MaxCards && k < 6; k++)
                Card(new Vector3(w * (.15f + .7f * Hash(k, 31)), ground, l * (.36f + .28f * Hash(k, 37))), new Vector2(1f, .25f).normalized, w * .9f, 24f + 10f * Hash(k, 41), 3.6f + 1.6f * Hash(k, 43), FieldOpacity, .7f + .5f * Hash(k, 47));

            mesh = new Mesh { name = "Fog wisps", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(pos); mesh.SetUVs(0, corner); mesh.SetUVs(1, drift); mesh.SetUVs(2, shape); mesh.SetTriangles(tris, 0);
            mesh.bounds = new Bounds(new Vector3(w * .5f, 8f, l * .5f), new Vector3(w + 200f, 60f, l + 200f));   // the cards move in the shader
            cloud = BuildCloud();
            material = new Material(Shader.Find("TW/Fog Wisp (URP)")) { hideFlags = HideFlags.HideAndDontSave };
            material.SetTexture("_MainTex", cloud);
            var go = new GameObject("Fog wisps") { hideFlags = HideFlags.DontSave };
            go.transform.SetParent(transform, false);
            go.AddComponent<MeshFilter>().sharedMesh = mesh;
            var r = go.AddComponent<MeshRenderer>();
            r.sharedMaterial = material; r.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off; r.receiveShadows = false;
        }

        void OnDestroy()
        {
            if (mesh != null) Destroy(mesh);
            if (material != null) Destroy(material);
            if (cloud != null) Destroy(cloud);
        }

        static float Hash(int a, int b)
        {
            uint h = (uint)a * 0x9E3779B1u ^ (uint)b * 0x85EBCA77u;
            h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12;
            return (h & 0xFFFF) / 65535f;
        }

        /// <summary>Soft tiling cloud: three octaves of value noise that wrap.</summary>
        static Texture2D BuildCloud()
        {
            const int n = 128;
            var px = new Color32[n * n];
            for (int z = 0; z < n; z++)
            for (int x = 0; x < n; x++)
            {
                float u = x / (float)n, v = z / (float)n;
                float c = Wrap(u * 4f, v * 4f, 4) * .5f + Wrap(u * 9f, v * 9f, 9) * .3f + Wrap(u * 19f, v * 19f, 19) * .2f;
                byte b = (byte)(Mathf.Clamp01(c) * 255f);
                px[z * n + x] = new Color32(b, b, b, 255);
            }
            var tex = new Texture2D(n, n, TextureFormat.RGBA32, true) { name = "Fog cloud", wrapMode = TextureWrapMode.Repeat, filterMode = FilterMode.Bilinear, hideFlags = HideFlags.HideAndDontSave };
            tex.SetPixels32(px); tex.Apply(true, true);
            return tex;
        }

        static float Wrap(float x, float z, int period)
        {
            int x0 = Mathf.FloorToInt(x), z0 = Mathf.FloorToInt(z);
            float tx = x - x0, tz = z - z0;
            tx = tx * tx * (3f - 2f * tx); tz = tz * tz * (3f - 2f * tz);
            int xa = x0 % period, xb = (xa + 1) % period, za = z0 % period, zb = (za + 1) % period;
            return Mathf.Lerp(Mathf.Lerp(Hash(xa + period * 131, za), Hash(xb + period * 131, za), tx), Mathf.Lerp(Hash(xa + period * 131, zb), Hash(xb + period * 131, zb), tx), tz);
        }
    }
}
