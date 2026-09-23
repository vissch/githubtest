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
        /// <summary>
        /// Fall as snow instead of rain. The same mesh and the same one draw call: snow is rain that falls at a tenth
        /// the speed, in short fat dashes rather than long thin ones, wandering as it goes. Building a second particle
        /// system for it would have bought nothing but another buffer and another draw.
        /// </summary>
        public bool AsSnow;
        /// <summary>
        /// How hard it falls, 0..1, when this is snow. Snow cannot ride on Atmosphere.RainNow the way rain does:
        /// that number is also _TWWet.z, which puts raindrop bursts on every upward face and a wet sheen on the mud.
        /// A snowfield wants the weather without the wet, so the two amounts are separate.
        /// </summary>
        [Range(0f, 1f)] public float Snowfall = 0.8f;
        public const int MaxStreaks = 2800;
        public Vector3 Box = new Vector3(90f, 46f, 90f);
        public float FallSpeed = 17f, StreakLength = 0.5f;
        Mesh mesh; Material material; Camera cam;
        Mesh curtainMesh; Material curtains; Texture2D curtainNoise; Vector2 drifted;
        const float CurtainCell = 70f, CurtainHeight = 55f;
        static readonly int DriftId = Shader.PropertyToID("_Drift"), WeatherId = Shader.PropertyToID("_Weather");
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
            if (AsSnow)
            {
                FallSpeed = 2.6f; StreakLength = 0.30f;   // a dash along its fall, not a dot: a square quad reads as confetti
                material.SetFloat("_Width", 0.042f);          // about a 7:1 dash, so it reads as falling and not as paper
                material.SetFloat("_Flutter", 0.42f);
                material.SetColor(ColorId, new Color(0.92f, 0.95f, 1.0f, 0.62f));
            }
            var go = new GameObject("Rain") { hideFlags = HideFlags.DontSave };
            go.transform.SetParent(transform, false);
            go.AddComponent<MeshFilter>().sharedMesh = mesh;
            var r2 = go.AddComponent<MeshRenderer>();
            r2.sharedMaterial = material; r2.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off; r2.receiveShadows = false;
            BuildCurtains();
        }

        /// <summary>Rain far off hangs in curtains (TW/Rain Curtain): a 7 x 7 block of cards, one a ground cell round the camera.</summary>
        void BuildCurtains()
        {
            var pos = new List<Vector3>(); var corner = new List<Vector2>(); var tris = new List<int>();
            for (int j = -3; j <= 3; j++)
            for (int i = -3; i <= 3; i++)
            {
                int v0 = pos.Count;
                for (int k = 0; k < 4; k++) { pos.Add(new Vector3(i, j, 0f)); corner.Add(new Vector2(k == 0 || k == 3 ? -1f : 1f, k < 2 ? -1f : 1f)); }
                tris.Add(v0); tris.Add(v0 + 2); tris.Add(v0 + 1); tris.Add(v0); tris.Add(v0 + 3); tris.Add(v0 + 2);
            }
            curtainMesh = new Mesh { name = "Rain curtains", hideFlags = HideFlags.HideAndDontSave };
            curtainMesh.SetVertices(pos); curtainMesh.SetUVs(0, corner); curtainMesh.SetTriangles(tris, 0);
            curtainMesh.bounds = new Bounds(Vector3.zero, Vector3.one * 5000f);
            const int n = 128;
            var px = new Color32[n * n];
            for (int z = 0; z < n; z++)
            for (int x = 0; x < n; x++)
            {
                // fine across, long down: falling streaks; the shader reads it a second time, stretched, for the veil
                float c = Tile(x / (float)n * 37f, z / (float)n * 3f, 37, 3) * .6f + Tile(x / (float)n * 11f, z / (float)n * 5f, 11, 5) * .4f;
                byte b = (byte)(Mathf.Clamp01(c) * 255f); px[z * n + x] = new Color32(b, b, b, 255);
            }
            curtainNoise = new Texture2D(n, n, TextureFormat.RGBA32, true) { name = "Curtain noise", wrapMode = TextureWrapMode.Repeat, filterMode = FilterMode.Bilinear, hideFlags = HideFlags.HideAndDontSave };
            curtainNoise.SetPixels32(px); curtainNoise.Apply(true, true);
            curtains = new Material(Shader.Find("TW/Rain Curtain (URP)")) { hideFlags = HideFlags.HideAndDontSave };
            curtains.SetTexture("_Noise", curtainNoise);
            var go = new GameObject("Rain curtains") { hideFlags = HideFlags.DontSave };
            go.transform.SetParent(transform, false);
            go.AddComponent<MeshFilter>().sharedMesh = curtainMesh;
            var r = go.AddComponent<MeshRenderer>();
            r.sharedMaterial = curtains; r.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off; r.receiveShadows = false;
        }

        static float Tile(float x, float z, int periodX, int periodZ)
        {
            int x0 = Mathf.FloorToInt(x), z0 = Mathf.FloorToInt(z);
            float tx = x - x0, tz = z - z0;
            tx = tx * tx * (3f - 2f * tx); tz = tz * tz * (3f - 2f * tz);
            int xa = x0 % periodX, xb = (xa + 1) % periodX, za = z0 % periodZ, zb = (za + 1) % periodZ;
            return Mathf.Lerp(Mathf.Lerp(Hash(xa + 977, za), Hash(xb + 977, za), tx), Mathf.Lerp(Hash(xa + 977, zb), Hash(xb + 977, zb), tx), tz);
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
            if (curtainMesh != null) Destroy(curtainMesh);
            if (curtains != null) Destroy(curtains);
            if (curtainNoise != null) Destroy(curtainNoise);
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
            float level = AsSnow ? Snowfall : Mathf.Clamp01(Atmosphere.RainNow);
            Vector2 wind = Atmosphere.WindNow;
            // Heavy rain falls faster; heavy SNOW does not - it just fills the air and goes sideways. With the
            // rain rule a flake fell at 8.5 m/s into a 1.6 m/s breeze, a slant of ten degrees, and read as pale
            // rain. A blizzard is the other way round: slow down, fast across.
            var velocity = AsSnow
                ? new Vector3(wind.x * 3.2f, -(FallSpeed + 1.2f * level), wind.y * 3.2f)
                : new Vector3(wind.x, -(FallSpeed + 7f * level), wind.y);   // heavy drops fall faster
            fallen += velocity * Time.deltaTime;
            for (int k = 0; k < 3; k++) fallen[k] = Mathf.Repeat(fallen[k], Box[k] * 64f);   // keep the numbers small; 64 boxes is a whole number of wraps
            material.SetVector(OffsetId, fallen);
            material.SetVector(FallId, new Vector4(velocity.x, -velocity.y, velocity.z, StreakLength * (.7f + .9f * level)));
            material.SetFloat(LevelId, level);
            if (curtains != null)
            {
                drifted += wind * (Time.deltaTime * 1.6f);   // the curtains cross the field a little faster than the wind at the ground
                float fall = Mathf.Max(6f, -velocity.y);
                float ground = RenderGround.Map != null && RenderGround.Map.WaterLevel > TW.Sim.Terrain.MapData.NoWater ? RenderGround.Map.WaterLevel : 0f;
                curtains.SetVector(DriftId, new Vector4(drifted.x, drifted.y, CurtainCell, CurtainHeight));
                curtains.SetVector(WeatherId, new Vector4(level, wind.x / fall, wind.y / fall, ground - 1f));
            }
        }
    }
}
