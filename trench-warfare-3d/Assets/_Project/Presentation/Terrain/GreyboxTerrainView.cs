// Phase: B1/B2 (minimal implementation for M1; replaced by TerrainChunkRenderer in B2)
// Builds one static mesh from the heightfield at 2 m resolution so trenches and the corridor are visible.
using UnityEngine;
using TW.Presentation;

namespace TW.Presentation.Terrain
{
    [RequireComponent(typeof(MeshFilter), typeof(MeshRenderer))]
    public sealed class GreyboxTerrainView : MonoBehaviour
    {
        public SimHost Host;
        public int Step = 2;
        Vector3[] verts;
        Mesh mesh;
        Texture2D layerTex;
        int vertsW, vertsL;
        bool meshDirty, subscribed;

        void Start()
        {
            if (Host == null || Host.Local == null) return;
            var hf = Host.Local.Map.Height;
            int w = hf.Width / Step + 1, l = hf.Length / Step + 1;
            verts = new Vector3[w * l];
            vertsW = w; vertsL = l;
            var uvs = new Vector2[w * l];
            for (int z = 0; z < l; z++)
            for (int x = 0; x < w; x++)
            {
                float wx = x * Step, wz = z * Step;
                verts[z * w + x] = new Vector3(wx, hf.Sample(wx, wz), wz);
                uvs[z * w + x] = new Vector2(wx / hf.Width, wz / hf.Length);
            }
            var tris = new int[(w - 1) * (l - 1) * 6];
            int t = 0;
            for (int z = 0; z < l - 1; z++)
            for (int x = 0; x < w - 1; x++)
            {
                int i = z * w + x;
                tris[t++] = i; tris[t++] = i + w; tris[t++] = i + 1;
                tris[t++] = i + 1; tris[t++] = i + w; tris[t++] = i + w + 1;
            }
            mesh = new Mesh { indexFormat = UnityEngine.Rendering.IndexFormat.UInt32, vertices = verts, uv = uvs, triangles = tris };
            mesh.RecalculateNormals();
            mesh.RecalculateBounds();
            GetComponent<MeshFilter>().sharedMesh = mesh;
            var shader = Shader.Find("Universal Render Pipeline/Lit");
            if (shader == null) shader = Shader.Find("Standard");
            var mat = new Material(shader) { color = Color.white };
            var tex = BuildLayerTexture();
            layerTex = tex;
            if (mat.HasProperty("_BaseMap")) mat.SetTexture("_BaseMap", tex); else mat.mainTexture = tex;
            GetComponent<MeshRenderer>().sharedMaterial = mat;
        }

        void Update()
        {
            if (Host == null || Host.Local == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            if (!meshDirty || mesh == null) return;
            meshDirty = false;
            mesh.vertices = verts;
            mesh.RecalculateNormals();
            layerTex.Apply(false, false);
        }

        void OnDestroy() { if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent; }

        /// <summary>A crater landed: re-read the heightfield and the nav layers around it.</summary>
        void OnSimEvent(TW.Sim.SimEvent e)
        {
            if (e.Type != TW.Sim.SimEventType.CraterStamp || verts == null) return;
            var map = Host.Local.Map;
            var hf = map.Height;
            float r = e.Scalar + Step;
            int x0 = Mathf.Max(0, Mathf.FloorToInt((e.Pos.x - r) / Step)), x1 = Mathf.Min(vertsW - 1, Mathf.CeilToInt((e.Pos.x + r) / Step));
            int z0 = Mathf.Max(0, Mathf.FloorToInt((e.Pos.z - r) / Step)), z1 = Mathf.Min(vertsL - 1, Mathf.CeilToInt((e.Pos.z + r) / Step));
            for (int z = z0; z <= z1; z++)
            for (int x = x0; x <= x1; x++)
                verts[z * vertsW + x].y = hf.Sample(x * Step, z * Step);
            float n = TW.Sim.Terrain.MapData.NavCellSize;
            var scorched = new Color(0.22f, 0.20f, 0.17f);
            int nx0 = Mathf.Max(0, Mathf.FloorToInt((e.Pos.x - r) / n)), nx1 = Mathf.Min(map.NavWidth - 1, Mathf.FloorToInt((e.Pos.x + r) / n));
            int nz0 = Mathf.Max(0, Mathf.FloorToInt((e.Pos.z - r) / n)), nz1 = Mathf.Min(map.NavLength - 1, Mathf.FloorToInt((e.Pos.z + r) / n));
            for (int z = nz0; z <= nz1; z++)
            for (int x = nx0; x <= nx1; x++)
                if ((map.NavLayers[map.NavIndex(x, z)] & (byte)TW.Sim.Terrain.NavLayer.Crater) != 0) layerTex.SetPixel(x, z, scorched);
            meshDirty = true;
        }

        /// <summary>One texel per nav cell: open ground, trench body (dark), ladders (sand), HQ lines (team tint),
        /// wire / mud / craters when A4 adds them, and a faint ruler line every 50 m so distances read on flat ground.</summary>
        Texture2D BuildLayerTexture()
        {
            var map = Host.Local.Map;
            int w = map.NavWidth, l = map.NavLength;
            var tex = new Texture2D(w, l, TextureFormat.RGBA32, false) { filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp };
            var px = new Color32[w * l];
            var ground = new Color(0.36f, 0.47f, 0.34f);      // the 2D game's muddy grass
            var groundAlt = new Color(0.37f, 0.485f, 0.35f);
            var trench = new Color(0.30f, 0.21f, 0.15f);      // duckboards and earth
            var link = new Color(0.60f, 0.48f, 0.30f);
            var wire = new Color(0.30f, 0.30f, 0.32f);
            var mud = new Color(0.30f, 0.24f, 0.16f);
            var crater = new Color(0.28f, 0.24f, 0.19f);
            var blocked = new Color(0.10f, 0.10f, 0.10f);
            Color[] teamTint = { new Color(0.55f, 0.45f, 0.25f), new Color(0.35f, 0.40f, 0.55f) };
            int rulerEvery = Mathf.RoundToInt(50f / TW.Sim.Terrain.MapData.NavCellSize);
            for (int z = 0; z < l; z++)
            for (int x = 0; x < w; x++)
            {
                int i = z * w + x;
                var layer = (TW.Sim.Terrain.NavLayer)map.NavLayers[i];
                Color c = ((x / 10 + z / 10) & 1) == 0 ? ground : groundAlt;   // 20 m checker, barely there
                if (z % rulerEvery == 0) c = Color.Lerp(c, Color.white, 0.12f);
                if ((layer & TW.Sim.Terrain.NavLayer.Mud) != 0) c = mud;
                if ((layer & TW.Sim.Terrain.NavLayer.Crater) != 0) c = crater;
                if ((layer & TW.Sim.Terrain.NavLayer.Wire) != 0) c = wire;
                if ((layer & TW.Sim.Terrain.NavLayer.Trench) != 0) c = (layer & TW.Sim.Terrain.NavLayer.Link) != 0 ? link : trench;
                if ((layer & TW.Sim.Terrain.NavLayer.Blocked) != 0) c = blocked;
                px[i] = c;
            }
            for (int o = 0; o < map.Objectives.Length; o++)
            {
                var def = map.Objectives[o];
                if (def.Kind != TW.Sim.Terrain.ObjectiveKind.HQ) continue;
                var tint = teamTint[def.SideTeam & 1];
                for (int k = 0; k < def.CellCount; k++)
                {
                    int i = map.ObjectiveCells[def.CellStart + k];
                    px[i] = Color.Lerp(px[i], tint, 0.6f);
                }
            }
            tex.SetPixels32(px);
            tex.Apply(false, false);   // stays readable: craters repaint texels at run time
            return tex;
        }
    }
}
