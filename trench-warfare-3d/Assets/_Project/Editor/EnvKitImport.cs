// Phase: B2 (the imported environment sets)
// Import rules for Resources/Env: one FBX per prop, split from the owner's six Tripo sheets by Tools/envsplit.py
// (metre scale, ground pivot, front +Z, Y up baked in), and one shared base-colour texture per set. BattlefieldKit
// draws them with TW/Toon, so each mesh gets what the kit's own Combine gives a module: a smoothed normal per vertex
// in UV3 for the ink outline (a hard edge would split the pushed-out hull at its corners) and the painted form in
// the vertex colours (darker at the foot, full at the top). No materials, animation, cameras, lights or colliders.
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

namespace TW.Editor
{
    public sealed class EnvKitImport : AssetPostprocessor
    {
        public const string Folder = "Assets/_Project/Resources/Env/";

        public override uint GetVersion() => 2;

        static bool Ours(string path) => path.Replace('\\', '/').StartsWith(Folder);

        void OnPreprocessModel()
        {
            if (!Ours(assetPath)) return;
            var m = (ModelImporter)assetImporter;
            m.globalScale = 1f; m.useFileScale = true; m.bakeAxisConversion = true;
            m.importCameras = false; m.importLights = false; m.importVisibility = false; m.importBlendShapes = false;
            m.materialImportMode = ModelImporterMaterialImportMode.None;
            m.animationType = ModelImporterAnimationType.None; m.importAnimation = false;
            m.addCollider = false; m.generateSecondaryUV = false; m.isReadable = false;
            m.meshCompression = ModelImporterMeshCompression.Off;
            // the sheets' own normals do not survive the split; hard where the painted forms break, soft on curved stock
            m.importNormals = ModelImporterNormals.Calculate; m.normalSmoothingAngle = 60f;
            m.importTangents = ModelImporterTangents.None;
        }

        void OnPostprocessModel(GameObject root)
        {
            if (!Ours(assetPath)) return;
            foreach (var filter in root.GetComponentsInChildren<MeshFilter>())
            {
                var mesh = filter.sharedMesh;
                if (mesh == null) continue;
                var verts = mesh.vertices; var normals = mesh.normals;
                var sum = new Dictionary<Vector3Int, Vector3>();
                var keys = new Vector3Int[verts.Length];
                for (int i = 0; i < verts.Length; i++)
                {
                    keys[i] = new Vector3Int(Mathf.RoundToInt(verts[i].x * 500f), Mathf.RoundToInt(verts[i].y * 500f), Mathf.RoundToInt(verts[i].z * 500f));
                    sum.TryGetValue(keys[i], out var n); sum[keys[i]] = n + normals[i];
                }
                var smooth = new List<Vector3>(verts.Length); var colors = new List<Color>(verts.Length);
                var b = mesh.bounds;
                for (int i = 0; i < verts.Length; i++)
                {
                    smooth.Add(sum[keys[i]].normalized);
                    float form = Mathf.Lerp(.84f, 1.04f, Mathf.InverseLerp(b.min.y, b.max.y, verts[i].y));
                    colors.Add(new Color(form, form, form, 1f));
                }
                mesh.SetUVs(3, smooth); mesh.SetColors(colors);
            }
        }

        void OnPreprocessTexture()
        {
            if (!Ours(assetPath)) return;
            var t = (TextureImporter)assetImporter;
            t.textureType = TextureImporterType.Default; t.sRGBTexture = true; t.mipmapEnabled = true;
            // The packed sheet is 4096x2048 and must arrive at that size: a 2048 cap would halve it again, and the
            // sheet is a power of two precisely so the block compressor will take it. Measured 2026-09-22: cut at
            // 3072 wide instead, Unity either rounded it up to 4096 and resampled it, or — told to keep 3072 —
            // gave up on compressing it at all and returned 25 MB of RGB24, worse than the six sheets it replaced.
            bool atlas = assetPath.EndsWith("EnvAtlas.jpg", System.StringComparison.OrdinalIgnoreCase);
            t.maxTextureSize = atlas ? 4096 : 2048;
            t.anisoLevel = 4; t.wrapMode = TextureWrapMode.Clamp;
            t.textureCompression = TextureImporterCompression.Compressed; t.alphaSource = TextureImporterAlphaSource.None;
        }
    }
}
