// Phase: Playground (2026-09-26, lane/show/playground) — import rules for Playground/Art
// Import rules for Playground/Art (the playground's own copy of art that is not in the battle yet):
//  Tanks/<Name>/  Tools/tank3split.py output: as TankImport does for Resources/Vehicles - metres, hierarchy kept, no
//                 materials, READABLE meshes (tests read them; TankModel copies them), the painted form in the vertex
//                 colours (darker toward each part's foot), a smoothed normal in UV3 for the ink outline, masks in UV2.
//  Units/<Name>/  Tools/frogrig.py output: a skinned figure, Generic rig (the playground retargets the game's clips
//                 onto it itself), readable, no materials, no animation; the same painted form in the vertex colours.
//  Textures       sRGB, mipmapped, clamped, 1024 max.
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

namespace TW.Playground.Editor
{
    public sealed class PlaygroundImport : AssetPostprocessor
    {
        public const string Art = "Assets/_Project/Playground/Art/";
        public override uint GetVersion() => 4;
        static bool Tank(string p) => p.Replace('\\', '/').StartsWith(Art + "Tanks/");
        /// <summary>A machine whose tank3.json says "normals": "carried" (mechsplit.py TW_NORMALS=carry, the Bullfrog): its
        /// derived LODs carry LOD0's normals and must be imported as they are, not re-derived at 55 degrees per LOD.</summary>
        static bool CarriedNormals(string p)
        {
            if (!Tank(p)) return false;
            string json = System.IO.Path.Combine(System.IO.Path.GetDirectoryName(p), "tank3.json");
            return System.IO.File.Exists(json) && System.IO.File.ReadAllText(json).Contains("\"normals\": \"carried\"");
        }
        static bool Unit(string p) => p.Replace('\\', '/').StartsWith(Art + "Units/");

        void OnPreprocessModel()
        {
            if (!Tank(assetPath) && !Unit(assetPath)) return;
            var m = (ModelImporter)assetImporter;
            m.globalScale = 1f; m.useFileScale = true; m.bakeAxisConversion = Tank(assetPath); m.preserveHierarchy = true;
            m.importCameras = false; m.importLights = false; m.importVisibility = false; m.importBlendShapes = false;
            m.materialImportMode = ModelImporterMaterialImportMode.None;
            m.addCollider = false; m.generateSecondaryUV = false;
            m.isReadable = true;
            m.meshCompression = ModelImporterMeshCompression.Off;
            m.optimizeMeshVertices = true; m.optimizeMeshPolygons = true; m.weldVertices = true;
            // a figure's LODs carry LOD0's smooth normals from frogrig.py: import them, or a coarse LOD re-derives hard
            // edges at 55 degrees, reads as crumpled foil and triples its vertices; a vehicle keeps the hard 55 degree edges
            m.importNormals = Unit(assetPath) || CarriedNormals(assetPath) ? ModelImporterNormals.Import : ModelImporterNormals.Calculate; m.normalSmoothingAngle = 55f;
            m.importTangents = ModelImporterTangents.None;
            if (Tank(assetPath)) { m.animationType = ModelImporterAnimationType.None; m.importAnimation = false; }
            else
            {
                m.animationType = ModelImporterAnimationType.Generic; m.avatarSetup = ModelImporterAvatarSetup.NoAvatar;
                m.importAnimation = false; m.optimizeGameObjects = false; m.skinWeights = ModelImporterSkinWeights.Custom; m.maxBonesPerVertex = 4;
            }
        }

        void OnPostprocessModel(GameObject root)
        {
            bool tank = Tank(assetPath), unit = Unit(assetPath);
            if (!tank && !unit) return;
            var meshes = new List<Mesh>();
            foreach (var f in root.GetComponentsInChildren<MeshFilter>(true)) if (f.sharedMesh != null) meshes.Add(f.sharedMesh);
            foreach (var s in root.GetComponentsInChildren<SkinnedMeshRenderer>(true)) if (s.sharedMesh != null) meshes.Add(s.sharedMesh);
            foreach (var mesh in meshes)
            {
                var verts = mesh.vertices; var normals = mesh.normals;
                var b = mesh.bounds;
                var form = new List<Color>(verts.Length);
                // a mesh that brings its own colours (a figure's far LOD: frogrig.py bakes the atlas into them and drops
                // the UVs) keeps them, with the form laid over; everything else gets the form alone
                var own = unit && mesh.uv.Length == 0 ? mesh.colors : null;
                for (int i = 0; i < verts.Length; i++)
                {
                    float f = Mathf.Lerp(.86f, 1.03f, Mathf.InverseLerp(b.min.y, b.max.y, verts[i].y));
                    form.Add(own != null && own.Length == verts.Length ? new Color(own[i].r * f, own[i].g * f, own[i].b * f, 1f) : new Color(f, f, f, 1f));
                }
                mesh.SetColors(form);
                mesh.SetUVs(2, new List<Vector4>(new Vector4[verts.Length]));
                if (!tank) continue;   // a skinned figure's outline uses its own (skinned) normal: UV3 is not skinned
                var sum = new Dictionary<Vector3Int, Vector3>();
                var keys = new Vector3Int[verts.Length];
                for (int i = 0; i < verts.Length; i++)
                {
                    keys[i] = new Vector3Int(Mathf.RoundToInt(verts[i].x * 500f), Mathf.RoundToInt(verts[i].y * 500f), Mathf.RoundToInt(verts[i].z * 500f));
                    sum.TryGetValue(keys[i], out var n); sum[keys[i]] = n + normals[i];
                }
                var smooth = new List<Vector3>(verts.Length);
                for (int i = 0; i < verts.Length; i++) smooth.Add(sum[keys[i]].normalized);
                mesh.SetUVs(3, smooth);
            }
        }

        void OnPreprocessTexture()
        {
            if (!assetPath.Replace('\\', '/').StartsWith(Art)) return;
            var t = (TextureImporter)assetImporter;
            t.textureType = TextureImporterType.Default; t.sRGBTexture = true; t.mipmapEnabled = true;
            t.maxTextureSize = 1024; t.anisoLevel = 4; t.wrapMode = TextureWrapMode.Clamp;
            t.textureCompression = TextureImporterCompression.Compressed; t.alphaSource = TextureImporterAlphaSource.None;
            t.isReadable = true;   // LodTint reads each LOD's atlas to match its colour to LOD0's
        }
    }
}
