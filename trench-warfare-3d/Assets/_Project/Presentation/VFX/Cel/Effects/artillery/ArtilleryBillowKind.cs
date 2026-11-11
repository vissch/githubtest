// Phase: CelFx artillery - a connected three-billow skin, built once and owned by the renderer.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class ArtilleryBillowKind : CelKind
    {
        public const string KindName = "ArtilleryBillow";
        const float Blend = 0.18f;
        const float RadiusBound = 1.16f;
        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/ArtilleryBillow");
        public override int Capacity => 768;
        public override int Order => 0;

        public override Mesh BuildMesh()
        {
            var mesh = CelMeshes.Icosphere(3, "Cel Artillery compound billow");
            var vertices = mesh.vertices;
            var normals = new Vector3[vertices.Length];
            var shape = new List<Vector2>(vertices.Length);
            for (int i = 0; i < vertices.Length; i++)
            {
                Vector3 direction = vertices[i];
                float lo = 0f, hi = RadiusBound;
                // Every constituent contains the origin: each radial ray has one outer surface crossing.
                for (int step = 0; step < 20; step++)
                {
                    float radius = (lo + hi) * 0.5f;
                    if (Field(direction * radius) < 0f) lo = radius;
                    else hi = radius;
                }
                Vector3 point = direction * ((lo + hi) * 0.5f);
                vertices[i] = point / RadiusBound;
                normals[i] = Normal(point);
                const float e = 0.025f;
                float divergence = (Normal(point + Vector3.right * e).x - Normal(point - Vector3.right * e).x
                    + Normal(point + Vector3.up * e).y - Normal(point - Vector3.up * e).y
                    + Normal(point + Vector3.forward * e).z - Normal(point - Vector3.forward * e).z) / (2f * e);
                shape.Add(new Vector2(Mathf.Clamp01(-divergence * 0.18f), Mathf.Clamp01(divergence * 0.18f)));
            }
            mesh.vertices = vertices;
            mesh.normals = normals;
            mesh.SetUVs(0, shape);
            mesh.bounds = new Bounds(Vector3.zero, Vector3.one * 2f);
            return mesh;
        }

        static float Field(Vector3 p)
        {
            float a = (p - new Vector3(-0.34f, 0.22f, -0.08f)).magnitude - 0.70f;
            float b = (p - new Vector3(0.34f, 0.10f, 0.08f)).magnitude - 0.65f;
            float c = (p - new Vector3(0.01f, -0.38f, 0.01f)).magnitude - 0.58f;
            return Join(Join(a, b), c);
        }

        static float Join(float a, float b)
        {
            float h = Mathf.Clamp01(0.5f + 0.5f * (b - a) / Blend);
            return Mathf.Lerp(b, a, h) - Blend * h * (1f - h);
        }

        static Vector3 Normal(Vector3 p)
        {
            const float e = 0.002f;
            return new Vector3(Field(p + Vector3.right * e) - Field(p - Vector3.right * e),
                Field(p + Vector3.up * e) - Field(p - Vector3.up * e),
                Field(p + Vector3.forward * e) - Field(p - Vector3.forward * e)).normalized;
        }
    }
}
