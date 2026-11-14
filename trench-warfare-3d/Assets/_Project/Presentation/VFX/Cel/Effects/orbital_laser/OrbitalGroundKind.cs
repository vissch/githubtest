// Phase: CelFx orbital laser reference match (2026-11-14) - a ground grid for the shallow displaced crater rim.
// The existing orbital shader maps UVs into world space; this mesh is built once per renderer.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class OrbitalGroundKind : CelKind
    {
        public const string KindName = "OrbitalGround";
        public const int GridSegments = 40;

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/OrbitalLaser");
        public override int Capacity => 64;
        public override int Order => 17;
        public override void Configure(Material material) => material.renderQueue = 3015;

        public override Mesh BuildMesh()
        {
            const int stride = GridSegments + 1;
            var vertices = new Vector3[stride * stride];
            var normals = new Vector3[vertices.Length];
            var uv = new Vector2[vertices.Length];
            var triangles = new int[GridSegments * GridSegments * 6];
            for (int z = 0; z < stride; z++)
            {
                for (int x = 0; x < stride; x++)
                {
                    int index = z * stride + x;
                    float u = x / (float)GridSegments, v = z / (float)GridSegments;
                    vertices[index] = new Vector3(u - 0.5f, 0f, v - 0.5f);
                    normals[index] = Vector3.up;
                    uv[index] = new Vector2(u, v);
                }
            }
            int at = 0;
            for (int z = 0; z < GridSegments; z++)
            {
                for (int x = 0; x < GridSegments; x++)
                {
                    int a = z * stride + x;
                    triangles[at++] = a;
                    triangles[at++] = a + stride;
                    triangles[at++] = a + 1;
                    triangles[at++] = a + 1;
                    triangles[at++] = a + stride;
                    triangles[at++] = a + stride + 1;
                }
            }
            var mesh = new Mesh { name = "Cel Orbital Ground", hideFlags = HideFlags.HideAndDontSave };
            mesh.vertices = vertices;
            mesh.normals = normals;
            mesh.uv = uv;
            mesh.SetIndices(triangles, MeshTopology.Triangles, 0, false);
            mesh.bounds = new Bounds(Vector3.zero, Vector3.one * 2f);
            return mesh;
        }
    }
}
