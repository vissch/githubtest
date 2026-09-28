// Phase: A5b / C4 (the owner's tanks)
// Import rules for Resources/Vehicles: one FBX per tank per LOD, split from the owner's two Tripo sheets by
// Tools/tanksplit.py (metre scale, ground pivot, front +Z, Y up baked in, one object per animated part with its origin on
// the part's pivot, Socket_* empties), and one shared atlas per LOD (TankAtlas_LOD0 2048, TankAtlas_LOD1 512).
// TankRenderer draws them with TW/Tank, so each mesh gets: the masks the split script painted in its vertex colours
// (R tread band, G furnace region, B exhaust outlet) moved to UV2, the painted form in the vertex colours (a little
// darker toward the foot of each part, as the kit does for props), and a smoothed normal per vertex in UV3 for the ink
// outline. The tread coordinate the script wrote as the second UV set arrives as UV1. No materials, animation, cameras,
// lights or colliders; the hierarchy is kept (TankRenderer reads the pivots and sockets from it).
// Blender's FBX export with bake_space_transform bakes the Z-up to Y-up turn into every mesh but gets the nested nodes
// wrong: a node directly under the hull (and the Tusk's muzzle empty) arrives with a 270 degree X turn and its offset
// still in Blender's axes. Every part was exported unturned, so such a node is put right here: its offset turned
// back (x, -z, y) and its rotation cleared. The Tusk's muzzle offset fits no turn at all, so it is put at the tip of
// the barrel (the gun mesh's front, which points +Z). Checked against tanksplit.py's tanks.json pivots and sockets.
// A node two levels down under a part off the root's origin is moved as well: the walkers' are put at crabs.json's
// places (CrabManifest).
//
// 2026-09-28, measured on the Croaker (a raw import beside this one, every node against mechsplit's manifest): that rule
// is exact only while the node ABOVE a turned node stands on its parent's origin. The exporter turns every other level
// of the hierarchy (the parts under the hull, then the parts under THEIR children), and writes a turned node T under
// a node U with offset u as turn(E + u) - u, and an unturned node under T as E - (u - unturn(u)), E being the offset
// meant. Under a hull on the origin both are the old rule, which is why the tanks and the hovercraft came out right;
// three levels down (a walker's foot under its shin under its thigh) the foot was displaced by its shin's offset, turned
// (the Croaker's feet 0.7 m up and 1.6 m forward of its ankles), and its toe socket with it. ParentAware names the models
// imported by the full rule. The crabs are not among them: their nodes are put at their manifest's places instead
// (CrabManifest, above; landed 2026-10-05), and no machine is under both rules.
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

namespace TW.Editor
{
    public sealed class TankImport : AssetPostprocessor
    {
        public const string Folder = "Assets/_Project/Resources/Vehicles/";

        public override uint GetVersion() => 4;   // 3 was taken twice: the crabs' manifest places, and ParentAware

        /// <summary>The models whose nested nodes are put right by the full rule (the header): written by the splitters'
        /// TW_BATTLE=1 on 2026-09-28.</summary>
        public static readonly string[] ParentAware = { "Brute", "Croaker", "Hopper", "Mercy" };

        static bool IsParentAware(string path)
        {
            string file = System.IO.Path.GetFileNameWithoutExtension(path);
            foreach (var n in ParentAware) if (file.StartsWith(n + "_LOD")) return true;
            return false;
        }

        /// <summary>The exporter's turn undone on an offset: Blender's axes to Unity's.</summary>
        public static Vector3 Unturn(Vector3 p) => new Vector3(p.x, -p.z, p.y);

        /// <summary>The offset meant for a turned node, from the one it arrived with and its parent's offset.</summary>
        public static Vector3 TurnedOffset(Vector3 arrived, Vector3 parent) => Unturn(arrived + parent) - parent;

        /// <summary>The offset meant for an unturned node under a turned one, from the one it arrived with and the offset
        /// of the node above its parent.</summary>
        public static Vector3 UnderTurnedOffset(Vector3 arrived, Vector3 grandparent) => arrived + grandparent - Unturn(grandparent);

        static bool Ours(string path) => path.Replace('\\', '/').StartsWith(Folder);

        void OnPreprocessModel()
        {
            if (!Ours(assetPath)) return;
            var m = (ModelImporter)assetImporter;
            m.globalScale = 1f; m.useFileScale = true; m.bakeAxisConversion = true; m.preserveHierarchy = true;
            m.importCameras = false; m.importLights = false; m.importVisibility = false; m.importBlendShapes = false;
            m.materialImportMode = ModelImporterMaterialImportMode.None;
            m.animationType = ModelImporterAnimationType.None; m.importAnimation = false;
            m.addCollider = false; m.generateSecondaryUV = false;
            // Readable, because TankModel builds these machines larger than the sculpt by copying each mesh
            // and moving its vertices, and a mesh without this has none to move. It costs a CPU copy of
            // 14,000 vertices across all seven machines, which is nothing, and without it the failure is
            // silent: .vertices returns an empty array, writing it back throws nothing, and what you get is
            // a full-sized body standing on legs spaced for a machine two and a half times bigger.
            m.isReadable = true;
            m.meshCompression = ModelImporterMeshCompression.Off;
            m.optimizeMeshVertices = true; m.optimizeMeshPolygons = true; m.weldVertices = true;
            m.importNormals = ModelImporterNormals.Calculate; m.normalSmoothingAngle = 55f;
            m.importTangents = ModelImporterTangents.None;
        }

        void OnPostprocessModel(GameObject root)
        {
            if (!Ours(assetPath)) return;
            var turned = Quaternion.Euler(270f, 0f, 0f);
            bool aware = IsParentAware(assetPath);
            var all = root.GetComponentsInChildren<Transform>(true);   // parents before their children
            var wasTurned = new HashSet<Transform>();
            foreach (var t in all) if (t != root.transform && Quaternion.Angle(t.localRotation, turned) <= 1f) wasTurned.Add(t);
            foreach (var t in all)
            {
                if (t == root.transform) continue;
                if (!wasTurned.Contains(t))
                {
                    // under a turned node: displaced by the offset of the node above that one (nothing, under a hull on the origin)
                    if (aware && t.parent != null && wasTurned.Contains(t.parent) && t.parent.parent != null && !t.name.StartsWith("Socket_Muzzle"))
                        t.localPosition = UnderTurnedOffset(t.localPosition, t.parent.parent.localPosition);
                    continue;
                }
                var p = t.localPosition;
                t.localRotation = Quaternion.identity;
                var gun = t.parent != null ? t.parent.GetComponent<MeshFilter>() : null;
                if (t.name.StartsWith("Socket_Muzzle") && gun != null && gun.sharedMesh != null)
                {
                    var b = gun.sharedMesh.bounds;
                    t.localPosition = new Vector3(b.center.x, b.center.y, b.max.z);
                }
                else t.localPosition = aware && t.parent != null ? TurnedOffset(p, t.parent.localPosition) : Unturn(p);
            }
            // The walkers and the Cutter (crabsplit.py) nest a level deeper under a body off the root's origin, and the
            // same export moves those nodes as well (CrabManifest): each goes to the place its manifest gives, parents
            // first, the muzzles kept at the barrel's tip as above. Read on every import of theirs, so the manifest is a
            // dependency.
            var places = CrabManifest.Places(CrabManifest.MachineOf(assetPath));
            if (places != null)
            {
                context.DependsOnSourceAsset(CrabManifest.Path);
                foreach (var t in root.GetComponentsInChildren<Transform>(true))
                    if (t != root.transform && !t.name.StartsWith("Socket_Muzzle") && places.TryGetValue(t.name, out var at))
                        t.localPosition = at - root.transform.InverseTransformPoint(t.parent.position);
            }
            foreach (var filter in root.GetComponentsInChildren<MeshFilter>())
            {
                var mesh = filter.sharedMesh;
                if (mesh == null) continue;
                var verts = mesh.vertices; var normals = mesh.normals; var colors = mesh.colors;
                var sum = new Dictionary<Vector3Int, Vector3>();
                var keys = new Vector3Int[verts.Length];
                for (int i = 0; i < verts.Length; i++)
                {
                    keys[i] = new Vector3Int(Mathf.RoundToInt(verts[i].x * 500f), Mathf.RoundToInt(verts[i].y * 500f), Mathf.RoundToInt(verts[i].z * 500f));
                    sum.TryGetValue(keys[i], out var n); sum[keys[i]] = n + normals[i];
                }
                var smooth = new List<Vector3>(verts.Length); var masks = new List<Vector4>(verts.Length); var form = new List<Color>(verts.Length);
                var b = mesh.bounds;
                bool hasMasks = colors != null && colors.Length == verts.Length;
                for (int i = 0; i < verts.Length; i++)
                {
                    smooth.Add(sum[keys[i]].normalized);
                    var c = hasMasks ? colors[i] : Color.black;
                    masks.Add(new Vector4(c.r, c.g, c.b, 0f));
                    float f = Mathf.Lerp(.86f, 1.03f, Mathf.InverseLerp(b.min.y, b.max.y, verts[i].y));
                    form.Add(new Color(f, f, f, 1f));
                }
                mesh.SetUVs(2, masks); mesh.SetUVs(3, smooth); mesh.SetColors(form);
            }
        }

        void OnPreprocessTexture()
        {
            if (!Ours(assetPath)) return;
            var t = (TextureImporter)assetImporter;
            t.textureType = TextureImporterType.Default; t.sRGBTexture = true; t.mipmapEnabled = true;
            t.maxTextureSize = assetPath.Contains("LOD1") ? 512 : 2048; t.anisoLevel = 4; t.wrapMode = TextureWrapMode.Clamp;
            t.textureCompression = TextureImporterCompression.Compressed; t.alphaSource = TextureImporterAlphaSource.None;
        }
    }
}
