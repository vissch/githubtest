// Phase: B2 (implemented) — the sea (owner, 2026-09-22: "we're also adding an ocean with boats arriving").
// One mesh, one draw call, one material (TW/Sea). It runs from the top of the beach out to the haze and well past
// both flanks, so from the standard view the water closes the far end of the field and there is no edge to find.
//
// The grid is graded: two metres between rows where the breakers are and a hundred out in the open, three metres
// across the front of the beach and a hundred out on the flanks. About four thousand vertices cover a square
// kilometre and a half, because the only thing that needs resolution is the surf.
//
// Every vertex carries its own water: colour.r = depth in metres / 20 (read off the drawn ground, inside the map or
// on the bed beyond it), colour.g = how close it is to the waterline, colour.b = 1 where it is really under the
// sand, so the buried inshore rim of the sheet never heaves. Depth changes only when the ground does, which is why
// it is baked into the mesh instead of sampled per pixel.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed class Ocean : MonoBehaviour
    {
        public SimHost Host;
        /// <summary>Metres of water beyond the waterline and past each flank. The camera's far plane is 1,500 m.</summary>
        public const float Reach = 1150f, Flank = 780f;
        public const float SurfStep = 2f, OpenStep = 100f, FaceStep = 3f;
        /// <summary>The swell TW/Sea draws, held here because the boats have to ride the same water (LandingCraftView).</summary>
        public const float SwellHeight = .62f, SwellLength = 34f, SwellSpeed = 4.2f;

        Mesh mesh;
        Material material;
        GameObject sheet;
        MapData map;
        BattlefieldSurface surface;
        GreyboxTerrainView view;

        void Start() { TryBuild(); }
        void Update() { if (sheet == null) TryBuild(); }

        void TryBuild()
        {
            if (Host == null || Host.Local == null) return;
            view = GetComponent<GreyboxTerrainView>();
            if (view == null || view.Surface == null) return;
            map = Host.Local.Map; surface = view.Surface;
            if (!map.HasSea) { enabled = false; return; }
            Build();
        }

        /// <summary>The drawn ground at a point: the map's own where there is map, the bed beyond it.</summary>
        float Ground(float x, float z)
        {
            bool inside = x >= 0f && z >= 0f && x <= map.SizeMeters.x && z <= map.SizeMeters.y;
            return inside ? surface.VisualHeight(x, z) : GreyboxTerrainView.SkirtHeight(map, x, z);
        }

        /// <summary>Rows out to sea: close together through the surf, then stretching. The first is at the top of the
        /// beach, buried under the sand, so the sheet has no visible edge inshore.</summary>
        static List<float> Steps(float from, float to, float near, float far)
        {
            var stops = new List<float> { from };
            float at = from, step = near;
            while (at < to)
            {
                at += step;
                step = Mathf.Min(far, step * 1.11f);
                stops.Add(Mathf.Min(at, to));
            }
            return stops;
        }

        void Build()
        {
            float away = map.SeaAway, w = map.SizeMeters.x;
            // out to sea from the top of the beach (under the sand) to the haze
            var outs = Steps(map.Offshore(map.SeaStartZ), Reach, SurfStep, OpenStep);
            // across: the width of the field at three metres, then out past both flanks
            var lateral = new List<float>();
            foreach (float t in Steps(0f, Flank, FaceStep * 2f, OpenStep)) if (t > 0f) lateral.Add(-t);
            lateral.Reverse();
            for (float x = 0f; x < w; x += FaceStep) lateral.Add(x);
            foreach (float t in Steps(0f, Flank, FaceStep * 2f, OpenStep)) lateral.Add(w + t);

            var verts = new List<Vector3>(outs.Count * lateral.Count);
            var cols = new List<Color32>(verts.Capacity);
            var tris = new List<int>(verts.Capacity * 6);
            for (int r = 0; r < outs.Count; r++)
            {
                float z = map.ShoreZ + away * outs[r];
                for (int c = 0; c < lateral.Count; c++)
                {
                    float x = lateral[c];
                    float depth = map.SeaLevel - Ground(x, z);
                    verts.Add(new Vector3(x, map.SeaLevel, z));
                    cols.Add(new Color32(
                        (byte)(Mathf.Clamp01(depth / 20f) * 255f),
                        (byte)(Mathf.Clamp01(1f - outs[r] / 80f) * 255f),
                        (byte)(depth <= 0f ? 255 : 0),
                        255));
                }
            }
            int stride = lateral.Count;
            for (int r = 0; r < outs.Count - 1; r++)
            for (int c = 0; c < stride - 1; c++)
            {
                int a = r * stride + c, b = a + 1, d = a + stride, e = d + 1;
                // wound so the face is up whichever way the sea lies
                if (away > 0f) { tris.Add(a); tris.Add(d); tris.Add(b); tris.Add(b); tris.Add(d); tris.Add(e); }
                else { tris.Add(a); tris.Add(b); tris.Add(d); tris.Add(b); tris.Add(e); tris.Add(d); }
            }

            mesh = new Mesh { name = "Sea", indexFormat = UnityEngine.Rendering.IndexFormat.UInt32, hideFlags = HideFlags.DontSave };
            mesh.SetVertices(verts); mesh.SetColors(cols); mesh.SetTriangles(tris, 0);
            var up = new Vector3[verts.Count];
            for (int i = 0; i < up.Length; i++) up[i] = Vector3.up;   // the swell's slope is worked out in the shader
            mesh.normals = up;
            mesh.RecalculateBounds();
            var bounds = mesh.bounds; bounds.Expand(new Vector3(0f, 6f, 0f)); mesh.bounds = bounds;   // room for the swell, so it is not culled at the crest

            material = new Material(Shader.Find("TW/Sea (URP)")) { hideFlags = HideFlags.DontSave };
            material.SetTexture("_RippleMap", view.Ripples());
            material.SetVector("_Inbound", new Vector4(0f, -away, 0f, 0f));   // the swell runs at the beach
            material.SetFloat("_Swell", SwellHeight); material.SetFloat("_SwellLength", SwellLength); material.SetFloat("_SwellSpeed", SwellSpeed);

            sheet = new GameObject("Sea") { hideFlags = HideFlags.DontSave };
            sheet.transform.SetParent(transform, false);
            sheet.AddComponent<MeshFilter>().sharedMesh = mesh;
            var r2 = sheet.AddComponent<MeshRenderer>();
            r2.sharedMaterial = material;
            r2.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
            r2.receiveShadows = true;
        }

        void OnDestroy()
        {
            if (mesh != null) DestroyImmediate(mesh);
            if (material != null) DestroyImmediate(material);
            if (sheet != null) DestroyImmediate(sheet);
        }
    }
}
