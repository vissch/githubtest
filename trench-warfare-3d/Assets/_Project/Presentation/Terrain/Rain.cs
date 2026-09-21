// Phase: B2 (implemented) — the rain of the night look. One mesh of streaks and TW/Rain do everything on the GPU; this
// component only builds the mesh once and tells the material where the camera is looking. The rings the drops make on
// the river and the puddles are drawn by the water itself (TWWater.hlsl, _TWWet.z, set by Atmosphere).
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public sealed class Rain : MonoBehaviour
    {
        [Tooltip("Share of MaxStreaks built; how many of them fall at a moment follows Atmosphere.RainNow.")]
        [Range(0f, 1f)] public float Intensity = 1f;
        public const int MaxStreaks = 2800;
        public Vector3 Box = new Vector3(90f, 46f, 90f);
        public float FallSpeed = 17f, StreakLength = 0.75f;
        Mesh mesh; Material material; Camera cam;
        Vector3 fallen;   // how far the rain has fallen and blown so far: integrated here, so a change of wind bends the streaks without making them jump
        static readonly int OffsetId = Shader.PropertyToID("_Offset"), LevelId = Shader.PropertyToID("_Level");
        static readonly int CentreId = Shader.PropertyToID("_Centre"), SizeId = Shader.PropertyToID("_Size"), FallId = Shader.PropertyToID("_Fall"), ColorId = Shader.PropertyToID("_Color");

        void Start()
        {
            int count = Mathf.RoundToInt(MaxStreaks * Mathf.Clamp01(Intensity));
            if (count == 0) { enabled = false; return; }
            var pos = new List<Vector3>(count * 4); var quad = new List<Vector2>(count * 4); var random = new List<Vector2>(count * 4); var tris = new List<int>(count * 6);
            for (int i = 0; i < count; i++)
            {
                var p = new Vector3(Hash(i, 3), Hash(i, 5), Hash(i, 7));
                var r = new Vector2(Hash(i, 11), Hash(i, 13));   // x: speed and length, y: how hard it must rain before this streak falls
                int v0 = pos.Count;
                for (int k = 0; k < 4; k++) { pos.Add(p); quad.Add(new Vector2(k == 0 || k == 3 ? -1f : 1f, k < 2 ? 0f : 1f)); random.Add(r); }
                tris.Add(v0); tris.Add(v0 + 2); tris.Add(v0 + 1); tris.Add(v0); tris.Add(v0 + 3); tris.Add(v0 + 2);
            }
            mesh = new Mesh { name = "Rain", hideFlags = HideFlags.HideAndDontSave, indexFormat = UnityEngine.Rendering.IndexFormat.UInt32 };
            mesh.SetVertices(pos); mesh.SetUVs(0, quad); mesh.SetUVs(1, random); mesh.SetTriangles(tris, 0);
            mesh.bounds = new Bounds(Vector3.zero, Vector3.one * 5000f);   // the streaks are placed in the shader
            material = new Material(Shader.Find("TW/Rain (URP)")) { hideFlags = HideFlags.HideAndDontSave };
            var go = new GameObject("Rain") { hideFlags = HideFlags.DontSave };
            go.transform.SetParent(transform, false);
            go.AddComponent<MeshFilter>().sharedMesh = mesh;
            var r2 = go.AddComponent<MeshRenderer>();
            r2.sharedMaterial = material; r2.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off; r2.receiveShadows = false;
        }

        static float Hash(int a, int b)
        {
            uint h = (uint)a * 0x9E3779B1u ^ (uint)b * 0x85EBCA77u;
            h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12;
            return (h & 0xFFFF) / 65535f;
        }

        void OnDestroy()
        {
            if (mesh != null) Destroy(mesh);
            if (material != null) Destroy(material);
        }

        void LateUpdate()
        {
            if (material == null) return;
            if (cam == null) cam = Camera.main;
            if (cam == null) return;
            // the box sits between the lens and the ground it looks at, so the streaks cross the whole picture
            var t = cam.transform;
            float toGround = t.position.y / Mathf.Max(.15f, -t.forward.y);
            Vector3 centre = t.position + t.forward * Mathf.Min(toGround * .62f, 60f);
            centre.y = Mathf.Max(centre.y, Box.y * .5f - 2f);
            material.SetVector(CentreId, centre);
            material.SetVector(SizeId, Box);
            float level = Mathf.Clamp01(Atmosphere.RainNow);
            Vector2 wind = Atmosphere.WindNow;
            var velocity = new Vector3(wind.x, -(FallSpeed + 7f * level), wind.y);   // heavy drops fall faster
            fallen += velocity * Time.deltaTime;
            for (int k = 0; k < 3; k++) fallen[k] = Mathf.Repeat(fallen[k], Box[k] * 64f);   // keep the numbers small; 64 boxes is a whole number of wraps
            material.SetVector(OffsetId, fallen);
            material.SetVector(FallId, new Vector4(velocity.x, -velocity.y, velocity.z, StreakLength * (.7f + .9f * level)));
            material.SetFloat(LevelId, level);
        }
    }
}
