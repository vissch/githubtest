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

        void Start()
        {
            if (Host == null || Host.Local == null) return;
            var hf = Host.Local.Map.Height;
            int w = hf.Width / Step + 1, l = hf.Length / Step + 1;
            var verts = new Vector3[w * l];
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
            var mesh = new Mesh { indexFormat = UnityEngine.Rendering.IndexFormat.UInt32, vertices = verts, uv = uvs, triangles = tris };
            mesh.RecalculateNormals();
            mesh.RecalculateBounds();
            GetComponent<MeshFilter>().sharedMesh = mesh;
            var shader = Shader.Find("Universal Render Pipeline/Lit");
            if (shader == null) shader = Shader.Find("Standard");
            GetComponent<MeshRenderer>().sharedMaterial = new Material(shader) { color = new Color(0.36f, 0.32f, 0.26f) };
        }
    }
}
