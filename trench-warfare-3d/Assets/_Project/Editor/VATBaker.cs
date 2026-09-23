// Phase: B3 (implemented), C1 (the clip atlas), C2 (the figures)
// Bakes each infantry figure (Art/Characters/Soldier.fbx for the rifleman, assault and machine-gunner; Sniper.fbx, the
// hooded man; both Tripo models auto-rigged by Mixamo, under a thousand vertices) through every clip in
// InfantryClipTable into the VAT layout VATRenderer draws: U = vertex, V = frame, one row per controller Clip, written
// by VatCodec as Resources/Units/Figure<Name>Atlas.bytes next to the mesh and the VatAssetData that points at both.
// - Each clip is sampled at its table rate between its cuts; loops leave out the frame that repeats the first,
//   one-shots keep their last frame (the renderer holds it).
// - Root translation is stripped: the first frame's hips are put over the origin and any travel across the clip is
//   removed as a linear trend (KeepRoot leaves it: a death falls where it falls), so the sway stays and the sim moves
//   the man. Every clip is turned so its first frame faces +Z; StripYaw also removes a turn the file carries.
// - The clips were captured for another Mixamo character: the bone rotations carry over (same skeleton), the hips
//   translation is scaled by this rig's hip height over the clip character's (measured on Rifle Idle).
// - A Clip with no table entry (only Clip.None today) is posed from ProceduralSoldier.Sample of its fallback row.
// - A figure with a texture gets it sampled into the vertex colours; the vertices skinned to the cloth bones (spine,
//   arms, legs, hips) are stored as their brightness with the team mask set, so the shader recolours the uniform
//   per side and keeps the folds. A figure without one is coloured by its dominant bone. A rifle box is added in the
//   right hand (the models carry none); the old CrouchedRun figure also gets a helmet brim.
// The result is normalised to a 1.78 m man standing on y = 0, facing +Z.
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEditor;
using UnityEngine;
using TW.Sim;
using TW.Presentation;
using TW.Presentation.Units;

namespace TW.Editor
{
    public static class VATBaker
    {
        public const string OutputFolder = "Assets/_Project/Resources/Units";
        const int PosedFrames = 16;

        /// <summary>The figures, in VATRenderer.Figures order: name (the asset is Units/Figure&lt;Name&gt;), source file.</summary>
        public static readonly (string Name, string Path)[] Figures =
        {
            ("Soldier", "Assets/_Project/Art/Characters/Soldier.fbx"),
            ("Sniper", "Assets/_Project/Art/Characters/Sniper.fbx"),
        };

        sealed class Rig
        {
            public GameObject Root;
            public SkinnedMeshRenderer Skin;
            public Transform Hips, Spine, Neck, Head, ArmL, ArmR, ForeL, ForeR, HandL, HandR, ThighL, ShinL, ThighR, ShinR, FootL, FootR;
            public Transform[] All;
            public Vector3[] BindPos;
            public Quaternion[] BindRot;
            public float MinY, Scale, HipHeight, HipScale = 1f;
            public Matrix4x4 HeadBindInverse;
            public Quaternion HipsBindInverse;
            public Matrix4x4 GripR, GripL;   // the rifle's socket in each hand's space, from the bind pose
            public Texture2D Albedo;
            public bool HasHelmet;

            public void Reset()
            {
                for (int i = 0; i < All.Length; i++) All[i].SetLocalPositionAndRotation(BindPos[i], BindRot[i]);
            }

            /// <summary>The pelvis' facing (radians about Y, 0 = +Z) in the current pose.</summary>
            public float FacingYaw()
            {
                // the pelvis turned from its bind pose, where the man faces +Z (the thigh axis misreads a crossed-leg frame)
                Vector3 fwd = Hips.rotation * HipsBindInverse * Vector3.forward; fwd.y = 0f;
                if (fwd.sqrMagnitude < 1e-6f) return 0f;
                return Mathf.Atan2(fwd.x, fwd.z);
            }
        }

        public static string LastReport { get; private set; } = "";

        [MenuItem("TW/VAT/Bake Infantry")]
        public static void BakeInfantry()
        {
            LastReport = "";
            foreach (var fig in Figures)
            {
                var source = AssetDatabase.LoadAssetAtPath<GameObject>(fig.Path);
                if (source == null) { Debug.LogError("VATBaker: missing " + fig.Path); continue; }
                var rig = BuildRig(source, fig.Path);
                try { Bake(rig, fig.Name); }
                finally { Object.DestroyImmediate(rig.Root); }
            }
            foreach (var name in new[] { "InfantryVat", "InfantryVatMesh", "InfantryVatPositions", "InfantryVatNormals" })   // the single-figure bakes of B3 and C1
                AssetDatabase.DeleteAsset($"{OutputFolder}/{name}.asset");
            AssetDatabase.DeleteAsset($"{OutputFolder}/InfantryVatAtlas.bytes");
            AssetDatabase.SaveAssets();
            File.WriteAllText(Path.Combine(Application.dataPath, "../Library/vat-bake-report.txt"), LastReport);
        }

        static Rig BuildRig(GameObject source, string path)
        {
            var go = Object.Instantiate(source);
            go.hideFlags = HideFlags.HideAndDontSave;
            go.transform.SetPositionAndRotation(Vector3.zero, Quaternion.identity);
            var rig = new Rig { Root = go, Skin = go.GetComponentInChildren<SkinnedMeshRenderer>(), All = go.GetComponentsInChildren<Transform>() };
            rig.HasHelmet = !path.EndsWith("CrouchedRun.fbx");   // the Tripo figures model their own
            string fbm = path.Substring(0, path.Length - 4) + ".fbm";
            if (AssetDatabase.IsValidFolder(fbm))
                foreach (var guid in AssetDatabase.FindAssets("t:Texture2D", new[] { fbm }))
                {
                    string tp = AssetDatabase.GUIDToAssetPath(guid);
                    if (!tp.Contains("rgb")) continue;
                    var ti = AssetImporter.GetAtPath(tp) as TextureImporter;
                    if (ti != null && (!ti.isReadable || ti.textureCompression != TextureImporterCompression.Uncompressed)) { ti.isReadable = true; ti.textureCompression = TextureImporterCompression.Uncompressed; ti.SaveAndReimport(); }
                    rig.Albedo = AssetDatabase.LoadAssetAtPath<Texture2D>(tp);
                }
            Transform Bone(params string[] names)
            {
                foreach (var n in names)
                    foreach (var t in rig.All)
                        if (t.name == n || t.name.EndsWith(":" + n)) return t;
                throw new System.InvalidOperationException("VATBaker: bone not found: " + names[0]);
            }
            rig.Hips = Bone("Hips"); rig.Spine = Bone("Spine"); rig.Neck = Bone("Neck", "Head"); rig.Head = Bone("Head");
            rig.ArmL = Bone("LeftArm"); rig.ArmR = Bone("RightArm"); rig.ForeL = Bone("LeftForeArm"); rig.ForeR = Bone("RightForeArm");
            rig.HandL = Bone("LeftHand"); rig.HandR = Bone("RightHand");
            rig.ThighL = Bone("LeftUpLeg"); rig.ShinL = Bone("LeftLeg"); rig.ThighR = Bone("RightUpLeg"); rig.ShinR = Bone("RightLeg");
            rig.FootL = Bone("LeftFoot"); rig.FootR = Bone("RightFoot");

            // the instantiated FBX rests in its clip's first frame; the T-pose only survives in the mesh bind poses. Which
            // space they are relative to differs between exporters (the mesh node, or the file's root): the one that
            // stands the man up tallest is right
            var skinBones = rig.Skin.bones; var bind = rig.Skin.sharedMesh.bindposes;
            int Depth(Transform t) { int d = 0; while (t.parent != null) { t = t.parent; d++; } return d; }
            var order = Enumerable.Range(0, skinBones.Length).OrderBy(i => Depth(skinBones[i])).ToArray();
            var verts = new List<Vector3>(); var normals = new List<Vector3>();
            float best = -1f; Matrix4x4 bestRef = Matrix4x4.identity;
            foreach (var reference in new[] { rig.Skin.transform.localToWorldMatrix, go.transform.localToWorldMatrix })
            {
                foreach (int i in order) { var m = reference * bind[i].inverse; skinBones[i].SetPositionAndRotation(m.GetColumn(3), m.rotation); }
                Skinned(rig, verts, normals);
                float height = verts.Max(p => p.y) - verts.Min(p => p.y);
                if (height > best) { best = height; bestRef = reference; }
            }
            foreach (int i in order) { var m = bestRef * bind[i].inverse; skinBones[i].SetPositionAndRotation(m.GetColumn(3), m.rotation); }
            rig.BindPos = rig.All.Select(t => t.localPosition).ToArray();
            rig.BindRot = rig.All.Select(t => t.localRotation).ToArray();

            Skinned(rig, verts, normals);
            float minY = verts.Min(p => p.y), maxY = verts.Max(p => p.y);
            rig.MinY = minY;
            rig.Scale = ProceduralSoldier.Height / (maxY - minY);
            rig.HipHeight = rig.Hips.position.y - minY;
            rig.HeadBindInverse = rig.Head.worldToLocalMatrix;
            rig.HipsBindInverse = Quaternion.Inverse(rig.Hips.rotation);
            // the rifle's sockets: in the bind pose it lies in the right hand pointing forward and a little towards the left
            // hand; each hand keeps that grip in its own space, so the box turns with the hand instead of with the world
            Vector3 hand = rig.HandR.position, across = (rig.HandL.position - hand).normalized;
            Vector3 dir = (Vector3.forward * 0.62f + across * 0.38f + Vector3.up * 0.08f).normalized;
            var gripWorld = Matrix4x4.TRS(hand, Quaternion.LookRotation(dir, Vector3.up), Vector3.one);
            rig.GripR = rig.HandR.worldToLocalMatrix * gripWorld;
            rig.GripL = rig.HandL.worldToLocalMatrix * Matrix4x4.TRS(rig.HandL.position, Quaternion.LookRotation(dir, Vector3.up), Vector3.one) * Matrix4x4.Translate(new Vector3(0f, 0f, -0.35f / rig.Scale));
            return rig;
        }

        static readonly Mesh scratch = new Mesh();

        /// <summary>The skinned vertices of the current pose in the rig root's space (unscaled, not yet grounded).</summary>
        static void Skinned(Rig rig, List<Vector3> verts, List<Vector3> normals)
        {
            rig.Skin.BakeMesh(scratch, true);
            var t = rig.Skin.transform;
            var m = Matrix4x4.TRS(t.position, t.rotation, Vector3.one);
            scratch.GetVertices(verts); scratch.GetNormals(normals);
            for (int i = 0; i < verts.Count; i++) { verts[i] = m.MultiplyPoint3x4(verts[i]); normals[i] = m.MultiplyVector(normals[i]).normalized; }
        }

        /// <summary>The animation in a clip file, imported as Generic so it samples onto the figure's hierarchy.</summary>
        static AnimationClip LoadClip(string file)
        {
            string path = InfantryClipTable.Folder + "/" + file + ".fbx";
            var importer = AssetImporter.GetAtPath(path) as ModelImporter;
            if (importer == null) return null;
            if (importer.animationType != ModelImporterAnimationType.Generic || !importer.importAnimation || importer.materialImportMode != ModelImporterMaterialImportMode.None)
            {
                importer.animationType = ModelImporterAnimationType.Generic; importer.importAnimation = true; importer.materialImportMode = ModelImporterMaterialImportMode.None;
                importer.SaveAndReimport();
            }
            return AssetDatabase.LoadAllAssetsAtPath(path).OfType<AnimationClip>().FirstOrDefault(c => !c.name.StartsWith("__preview__"));
        }

        static void Bake(Rig rig, string figure)
        {
            var source = rig.Skin.sharedMesh;
            int skinCount = source.vertexCount;
            var verts = new List<Vector3>(); var normals = new List<Vector3>();

            // ---- extra rigid geometry, modelled in the bind pose in rig units -----------------------------------
            rig.Reset();
            Skinned(rig, verts, normals);
            float u = 1f / rig.Scale;   // one metre of the finished soldier, in rig units
            var skinWeights = source.boneWeights; var skinBones = rig.Skin.bones;
            float top = Enumerable.Range(0, skinCount).Where(i => skinBones[skinWeights[i].boneIndex0] == rig.Head).Select(i => verts[i].y).DefaultIfEmpty(verts.Max(p => p.y)).Max();
            var headPos = rig.Head.position;
            var helmet = rig.HasHelmet ? new BoxMesh { Pos = new Vector3[0], Nrm = new Vector3[0], Tris = new int[0] } : Box(new Vector3(headPos.x, top - 0.07f * u, headPos.z + 0.01f * u), new Vector3(0.31f, 0.03f, 0.34f) * u);   // the brim of a Brodie
            var rifle = Box(new Vector3(0f, 0f, 0.30f * u), new Vector3(0.06f, 0.10f, 1.15f) * u);   // fat enough to read at 30 m; butt 27 cm behind the grip
            const float RifleTip = 0.30f + 0.575f;   // the muzzle: the box's far end, metres ahead of the grip
            int vertexCount = skinCount + helmet.Pos.Length + rifle.Pos.Length;

            // the clips' hips translation is another character's: scale it to this rig's leg length
            var reference = LoadClip("Rifle Idle");
            if (reference != null)
            {
                rig.Reset(); reference.SampleAnimation(rig.Root, 0f);
                float clipHip = rig.Hips.position.y;
                rig.HipScale = clipHip > 1e-4f ? rig.HipHeight / clipHip : 1f;
                rig.Reset();
            }

            // ---- frames -----------------------------------------------------------------------------------------
            var frames = new List<Vector3[]>(); var frameNormals = new List<Vector3[]>(); var frameSockets = new List<Vector3[]>();
            bool aiming = false;   // an aimed clip: the forestock hand reaches well ahead of the grip, but both hands hold the rifle
            void Capture(Vector3 shift, float yawFix)
            {
                Skinned(rig, verts, normals);
                var p = new Vector3[vertexCount]; var n = new Vector3[vertexCount];
                for (int i = 0; i < skinCount; i++) { p[i] = verts[i]; n[i] = normals[i]; }
                var head = rig.Head.localToWorldMatrix * rig.HeadBindInverse;
                for (int i = 0; i < helmet.Pos.Length; i++) { p[skinCount + i] = head.MultiplyPoint3x4(helmet.Pos[i]); n[skinCount + i] = head.MultiplyVector(helmet.Nrm[i]).normalized; }
                // carried in the right hand's socket; when the right hand leaves the weapon (bolt, reload, a fidget, the
                // ladder, a throw) the left hand keeps it
                // both hands on it: from the right hand (the grip) through the left (the forestock), whatever the rig's hand axes are
                Vector3 span = rig.HandL.position - rig.HandR.position;
                bool rightOff = span.magnitude > 0.48f / rig.Scale && !aiming;
                Matrix4x4 grip;
                if (rightOff) grip = rig.HandL.localToWorldMatrix * rig.GripL;
                else if (span.magnitude > 0.08f / rig.Scale) grip = Matrix4x4.TRS(rig.HandR.position, Quaternion.LookRotation(span.normalized, Vector3.up), Vector3.one);
                else grip = rig.HandR.localToWorldMatrix * rig.GripR;
                int r0 = skinCount + helmet.Pos.Length;
                for (int i = 0; i < rifle.Pos.Length; i++) { p[r0 + i] = grip.MultiplyPoint3x4(rifle.Pos[i]); n[r0 + i] = grip.MultiplyVector(rifle.Nrm[i]); }
                var turn = Quaternion.Euler(0f, -yawFix * Mathf.Rad2Deg, 0f);
                for (int i = 0; i < vertexCount; i++) { p[i] = turn * ((p[i] - shift - new Vector3(0f, rig.MinY, 0f)) * rig.Scale); n[i] = turn * n[i]; }
                // sockets, in the same space as the vertices: the rifle's muzzle (the far end of the box), the way the barrel
                // points, and the chest (between the spine and the neck)
                Vector3 Place(Vector3 world) => turn * ((world - shift - new Vector3(0f, rig.MinY, 0f)) * rig.Scale);
                var sockets = new Vector3[3];
                sockets[VatAsset.Muzzle] = Place(grip.MultiplyPoint3x4(new Vector3(0f, 0f, RifleTip * u)));
                sockets[VatAsset.Barrel] = (turn * grip.MultiplyVector(Vector3.forward)).normalized;
                sockets[VatAsset.Chest] = Place(Vector3.Lerp(rig.Spine.position, rig.Neck.position, 0.6f));
                frames.Add(p); frameNormals.Add(n); frameSockets.Add(sockets);
            }
            var upperBody = rig.All.Where(t => t == rig.Spine || t.IsChildOf(rig.Spine)).ToArray();
            var upperRot = new Quaternion[upperBody.Length];
            void Pose(AnimationClip clip, float time, AnimationClip lower = null, float lowerTime = 0f)
            {
                rig.Reset(); clip.SampleAnimation(rig.Root, time);
                if (lower != null)
                {
                    // composed: this clip from the spine up on the other clip's hips and legs; the spine keeps this clip's
                    // world lean (on the kneel's tilted pelvis its own local bend doubled up and hunched him to his knee)
                    for (int k = 0; k < upperBody.Length; k++) upperRot[k] = upperBody[k].localRotation;
                    Quaternion spineWorld = rig.Spine.rotation;
                    rig.Reset(); lower.SampleAnimation(rig.Root, lowerTime);
                    for (int k = 0; k < upperBody.Length; k++) upperBody[k].localRotation = upperRot[k];
                    rig.Spine.rotation = spineWorld;
                }
                rig.Hips.position = rig.Hips.position * rig.HipScale + new Vector3(0f, rig.MinY, 0f);
            }

            int rows = (int)Clip.Count;
            var table = new Vector2[rows]; var seconds = new float[rows];
            var report = new System.Text.StringBuilder();
            var sources = InfantryClipTable.Entries.ToDictionary(e => e.Clip, e => e);
            int missing = 0, unbound = 0;
            for (int r = 0; r < rows; r++)
            {
                var clipId = (Clip)r;
                int start = frames.Count;
                if (sources.TryGetValue(clipId, out var src))
                {
                    var clip = LoadClip(src.File);
                    var lower = src.Lower != null ? LoadClip(src.Lower) : null;
                    if (src.Lower != null && lower == null) { Debug.LogError("VATBaker: no clip in " + src.Lower + ".fbx for the legs of " + clipId); missing++; }
                    if (clip == null) { Debug.LogError("VATBaker: no clip in " + src.File + ".fbx for " + clipId); missing++; }
                    else
                    {
                        float from = Mathf.Clamp(src.CutStart, 0f, clip.length), to = src.CutEnd > 0f ? Mathf.Min(src.CutEnd, clip.length) : clip.length;
                        float span = Mathf.Max(0.05f, to - from), rate = src.Rate > 0f ? src.Rate : 1f;
                        float played = span / rate;
                        int n = Mathf.Max(2, Mathf.RoundToInt(played * src.Fps) + (src.Loop ? 0 : 1));
                        // root: first frame's hips over the origin, travel removed as a trend; facing: first frame to +Z
                        Pose(clip, from, lower, src.LowerTime);
                        Vector3 first = Flat(rig.Hips.position); float yaw0 = rig.FacingYaw(), rifle0 = RifleYaw(rig);
                        Quaternion hips0 = rig.Hips.rotation; Vector3 knee0 = rig.ShinL.position - rig.Hips.position;
                        Pose(clip, from + span * 0.5f, lower, src.LowerTime);
                        bool bound = Quaternion.Angle(hips0, rig.Hips.rotation) > 0.5f || (rig.ShinL.position - rig.Hips.position - knee0).magnitude > 0.005f * u;
                        if (!bound) { Debug.LogWarning("VATBaker: " + src.File + " does not move between its first frame and its middle: its curves may not bind to this rig"); unbound++; }
                        Pose(clip, to, lower, src.LowerTime);
                        Vector3 last = Flat(rig.Hips.position); float yawN = rig.FacingYaw(), rifleN = RifleYaw(rig);
                        Vector3 travel = last - first;
                        Vector3 keep = src.KeepRoot ? travel * Mathf.Max(0f, 1f - 0.35f / rig.Scale / Mathf.Max(1e-4f, travel.magnitude)) : travel;   // a death keeps 35 cm of its fall
                        // aimed: face by the rifle (the pelvis stands 70 degrees side-on to it); a raise ends so, a lower starts so
                        if (src.Aim || src.LowerAim) yaw0 = rifle0;
                        if (src.RaiseAim) yawN = rifleN;
                        float turn = Mathf.DeltaAngle(yaw0 * Mathf.Rad2Deg, yawN * Mathf.Rad2Deg) * Mathf.Deg2Rad;
                        // aimed: the rifle lies from the right hand through the left (the forestock hand is 0.58 m out, past
                        // the one-hand test, and the left hand's own bind grip pointed it 35 to 41 degrees at the ground), and
                        // is turned so the clip's mean pitch is just under level where it strays, keeping its kick and sway
                        float lift = 0f;
                        aiming = src.Aim;
                        if (src.Aim)
                        {
                            float sum = 0f;
                            for (int f = 0; f < n; f++) { float frac = src.Loop ? f / (float)n : f / (float)(n - 1); Pose(clip, from + frac * span, lower, src.LowerTime); sum += RiflePitch(rig); }
                            float mean = sum / n;
                            if (Mathf.Abs(mean + 2f) > 6f) lift = -2f - mean;
                        }
                        for (int f = 0; f < n; f++)
                        {
                            float frac = src.Loop ? f / (float)n : f / (float)(n - 1);
                            Pose(clip, from + frac * span, lower, lower != null ? (src.LowerTime + frac * span) % Mathf.Max(0.1f, lower.length) : 0f);   // the legs breathe along the lower loop
                            if (src.Thrown) Throw(rig, frac * played, yaw0);
                            if (lift != 0f) Level(rig, lift);
                            Capture(first + keep * frac, yaw0 + (src.StripYaw ? turn * frac : 0f));
                        }
                        aiming = false;
                        table[r] = new Vector2(start, src.Loop ? n : -n); seconds[r] = played;
                        report.AppendLine($"{clipId,-18} {src.File,-34} {n,4} frames {played,5:0.00} s {(src.Loop ? "loop" : "once")} kept {(travel - keep).magnitude * rig.Scale * 100f,4:0} of {travel.magnitude * rig.Scale * 100f,4:0} cm travel{(src.Lower != null ? " on " + src.Lower : "")} turn {turn * Mathf.Rad2Deg,5:0} deg{(src.Aim ? $" rifle through both hands, turned {lift:+0;-0;0} deg" : "")}");
                        continue;
                    }
                }
                // no file: the procedural pose of the fallback row
                var row = Clips.Table[r].Fallback;
                for (int f = 0; f < PosedFrames; f++) { ApplyPose(rig, ProceduralSoldier.Sample(row, f / (float)PosedFrames)); Capture(Vector3.zero, 0f); }
                table[r] = new Vector2(start, PosedFrames); seconds[r] = 1f;
                report.AppendLine($"{clipId,-18} (posed from {row})");
            }

            // ---- mesh -------------------------------------------------------------------------------------------
            var colors = new Color[vertexCount];
            var weights = source.boneWeights; var bones = rig.Skin.bones;
            var uvs = source.uv;
            for (int i = 0; i < skinCount; i++)
            {
                string bone = bones[weights[i].boneIndex0].name;
                if (rig.Albedo != null && uvs != null && uvs.Length == skinCount)
                {
                    Color c = rig.Albedo.GetPixelBilinear(uvs[i].x, uvs[i].y);
                    if (Cloth(bone)) { float lum = Mathf.Clamp(c.grayscale / 0.55f, 0.25f, 1.6f); colors[i] = new Color(lum, lum, lum, 1f); }   // the uniform: brightness only, the team colours it
                    else colors[i] = new Color(c.r, c.g, c.b, 0f);
                }
                else colors[i] = BoneColor(bone);
            }
            for (int i = 0; i < helmet.Pos.Length; i++) colors[skinCount + i] = new Color(0.30f, 0.33f, 0.27f, 0f);
            for (int i = 0; i < rifle.Pos.Length; i++) colors[skinCount + helmet.Pos.Length + i] = new Color(0.27f, 0.18f, 0.11f, 0f);
            var tris = new List<int>(source.triangles);
            tris.AddRange(helmet.Tris.Select(t => t + skinCount));
            tris.AddRange(rifle.Tris.Select(t => t + skinCount + helmet.Pos.Length));
            int idle = (int)table[(int)Clip.Idle].x;
            var mesh = new Mesh { name = "Figure" + figure + "Mesh" };
            // UV1.x: which limb a vertex belongs to, 0 body, 1 head (and the helmet), 2/3 left/right arm, 4/5 left/right leg.
            // VAT_URP cuts the limbs VatInstance.Pad names; the rifle stays 0 (DebrisRenderer throws its own).
            var limbs = new Vector2[vertexCount];
            for (int i = 0; i < skinCount; i++) limbs[i] = new Vector2(LimbOf(bones[weights[i].boneIndex0].name), 0f);
            for (int i = 0; i < helmet.Pos.Length; i++) limbs[skinCount + i] = new Vector2(1f, 0f);
            mesh.SetVertices(frames[idle]); mesh.SetNormals(frameNormals[idle]); mesh.SetColors(colors); mesh.SetUVs(1, limbs); mesh.SetTriangles(tris, 0);
            mesh.bounds = new Bounds(new Vector3(0f, 0.9f, 0f), new Vector3(3f, 2.4f, 3f));

            // ---- atlas and assets -------------------------------------------------------------------------------
            int total = frames.Count;
            var bytes = VatCodec.Encode(frames.ToArray(), frameNormals.ToArray(), vertexCount, table, seconds, frameSockets.ToArray());
            if (!AssetDatabase.IsValidFolder("Assets/_Project/Resources")) AssetDatabase.CreateFolder("Assets/_Project", "Resources");
            if (!AssetDatabase.IsValidFolder(OutputFolder)) AssetDatabase.CreateFolder("Assets/_Project/Resources", "Units");
            AssetDatabase.DeleteAsset($"{OutputFolder}/Figure{figure}.asset"); AssetDatabase.DeleteAsset($"{OutputFolder}/Figure{figure}Mesh.asset");
            string atlasPath = $"{OutputFolder}/Figure{figure}Atlas.bytes";
            File.WriteAllBytes(atlasPath, bytes);
            AssetDatabase.ImportAsset(atlasPath, ImportAssetOptions.ForceSynchronousImport);
            var data = ScriptableObject.CreateInstance<VatAssetData>();
            data.Mesh = mesh; data.Rows = rows; data.TotalFrames = total; data.VertexCount = vertexCount;
            data.Atlas = AssetDatabase.LoadAssetAtPath<TextAsset>(atlasPath);
            AssetDatabase.CreateAsset(mesh, $"{OutputFolder}/Figure{figure}Mesh.asset");
            AssetDatabase.CreateAsset(data, $"{OutputFolder}/Figure{figure}.asset");
            AssetDatabase.SaveAssets();
            string summary = $"VATBaker {figure}: {vertexCount} vertices x {total} frames, {rows} clips, {bytes.Length / 1048576f:0.0} MB on disk, {(long)vertexCount * total * 12 / 1048576f:0.0} MB in memory, scale {rig.Scale:0.###}, hip scale {rig.HipScale:0.###}, albedo {(rig.Albedo != null ? rig.Albedo.name : "none")}; {missing} missing, {unbound} unbound";
            LastReport += summary + "\n" + report + "\n";
            Debug.Log(summary + "\n" + report);
        }

        static Vector3 Flat(Vector3 v) => new Vector3(v.x, 0f, v.z);

        /// <summary>The bones the uniform hangs on: what the team colour recolours.</summary>
        /// <summary>Which limb a bone belongs to, for the shell that takes it off: shoulders and hips stay with the body.</summary>
        static float LimbOf(string bone)
        {
            if (bone.Contains("Head")) return 1f;
            bool left = bone.Contains("Left"), right = bone.Contains("Right");
            if (bone.Contains("Arm") || bone.Contains("Hand")) return left ? 2f : right ? 3f : 0f;
            if (bone.Contains("Leg") || bone.Contains("Foot") || bone.Contains("Toe")) return left ? 4f : right ? 5f : 0f;
            return 0f;
        }

        static bool Cloth(string bone)
        {
            if (bone.Contains("Head") || bone.Contains("Neck") || bone.Contains("Hand") || bone.Contains("Foot") || bone.Contains("Toe")) return false;
            return true;
        }

        static Color BoneColor(string bone)
        {
            if (bone.Contains("Head") || bone.Contains("Neck") || bone.Contains("Hand")) return new Color(0.78f, 0.60f, 0.47f, 0f);
            if (bone.Contains("Foot") || bone.Contains("Toe")) return new Color(0.13f, 0.11f, 0.10f, 0f);
            if (bone.Contains("Leg") && !bone.Contains("UpLeg")) return new Color(0.72f, 0.72f, 0.66f, 1f);   // puttees
            return new Color(1f, 1f, 1f, 1f);
        }

        // ---- posing the rig from a SoldierPose ------------------------------------------------------------------
        /// <summary>
        /// Blown off his feet: the whole body tips back about the hips, flat a third of the way to the ground and a little
        /// past flat at the top of the arc, then hands back to the clip over a quarter second as his back lands (at
        /// AnimationController.ThrownLands). The clip's own limbs play on top.
        /// </summary>
        static void Throw(Rig rig, float t, float facing)
        {
            float land = AnimationController.ThrownLands;
            float weight = t <= land ? 1f : 1f - Mathf.Clamp01((t - land) / 0.25f);
            if (weight <= 0f) return;
            float pitch = t <= land ? 100f * Mathf.SmoothStep(0f, 1f, t / (0.35f * land)) - 10f * Mathf.SmoothStep(0f, 1f, (t - 0.6f * land) / (0.4f * land)) : 90f;
            Vector3 fwd = new Vector3(Mathf.Sin(facing), 0f, Mathf.Cos(facing)), right = Vector3.Cross(Vector3.up, fwd).normalized;
            Vector3 now = Vector3.ProjectOnPlane(rig.Neck.position - rig.Hips.position, right);
            if (now.sqrMagnitude < 1e-8f) return;
            float r = pitch * Mathf.Deg2Rad;
            Vector3 want = Vector3.up * Mathf.Cos(r) - fwd * Mathf.Sin(r);   // tipped backwards, away from the way he faced
            var turn = Quaternion.Slerp(Quaternion.identity, Quaternion.FromToRotation(now.normalized, want), weight);
            rig.Hips.rotation = turn * rig.Hips.rotation;   // everything hangs off the hips: the whole man turns about them
        }

        /// <summary>Where the rifle points when both hands hold it (radians about Y, 0 = +Z): the line from the right hand to the left.</summary>
        static float RifleYaw(Rig rig)
        {
            Vector3 s = rig.HandL.position - rig.HandR.position;
            return Mathf.Atan2(s.x, s.z);
        }

        /// <summary>The pitch of the rifle when both hands hold it (degrees, up positive): the line from the right hand to the left.</summary>
        static float RiflePitch(Rig rig)
        {
            Vector3 s = rig.HandL.position - rig.HandR.position;
            return Mathf.Atan2(s.y, new Vector2(s.x, s.z).magnitude) * Mathf.Rad2Deg;
        }

        /// <summary>
        /// Turns the rifle lift degrees up (down when negative) about the right hand (the butt stays in the shoulder) and
        /// brings the left hand back onto the forestock by two-bone IK on the left arm, the elbow kept on the side it was bent
        /// to. The rifle box and the muzzle socket follow, because Capture lays an aimed rifle from the right hand through the left.
        /// </summary>
        static void Level(Rig rig, float lift)
        {
            Vector3 grip = rig.HandR.position, span = rig.HandL.position - grip;
            Vector3 flat = new Vector3(span.x, 0f, span.z);
            if (flat.sqrMagnitude < 1e-8f) return;
            float pitch = Mathf.Atan2(span.y, flat.magnitude) + lift * Mathf.Deg2Rad;
            Vector3 target = grip + (flat.normalized * Mathf.Cos(pitch) + Vector3.up * Mathf.Sin(pitch)) * span.magnitude;
            Vector3 shoulder = rig.ArmL.position, elbow = rig.ForeL.position;
            float a = (elbow - shoulder).magnitude, b = (rig.HandL.position - elbow).magnitude;
            Vector3 reach = target - shoulder;
            if (a < 1e-5f || b < 1e-5f || reach.sqrMagnitude < 1e-10f) return;
            float d = Mathf.Clamp(reach.magnitude, Mathf.Abs(a - b) + 1e-4f, a + b - 1e-4f);
            Vector3 axis = reach.normalized, bend = Vector3.ProjectOnPlane(elbow - shoulder, axis);
            if (bend.sqrMagnitude < 1e-10f) bend = Vector3.ProjectOnPlane(Vector3.down, axis);
            bend.Normalize();
            float cos = Mathf.Clamp((a * a + d * d - b * b) / (2f * a * d), -1f, 1f);
            Vector3 elbowTo = shoulder + axis * (a * cos) + bend * (a * Mathf.Sqrt(1f - cos * cos));
            rig.ArmL.rotation = Quaternion.FromToRotation(elbow - shoulder, elbowTo - shoulder) * rig.ArmL.rotation;
            Vector3 wristTo = shoulder + axis * d;
            rig.ForeL.rotation = Quaternion.FromToRotation(rig.HandL.position - rig.ForeL.position, wristTo - rig.ForeL.position) * rig.ForeL.rotation;
        }

        static void ApplyPose(Rig rig, ProceduralSoldier.SoldierPose p)
        {
            rig.Reset();
            float u = 1f / rig.Scale;
            var root = Quaternion.Euler(p.RootEuler);
            rig.Hips.position = new Vector3(p.RootPos.x * u, p.RootPos.y / ProceduralSoldier.HipHeight * rig.HipHeight + rig.MinY, p.RootPos.z * u);
            rig.Hips.rotation = root * rig.Hips.rotation;
            var torso = root * Quaternion.Euler(p.Torso);
            Turn(rig.Spine, root, Quaternion.Euler(p.Torso));
            Turn(rig.Neck, torso, Quaternion.Euler(p.Head));
            Arm(rig.ArmL, rig.ForeL, torso, p.ArmL, p.ForeL, 1f);
            Arm(rig.ArmR, rig.ForeR, torso, p.ArmR, p.ForeR, -1f);
            Turn(rig.ThighL, root, Quaternion.Euler(p.ThighL, 0f, 0f));
            Turn(rig.ShinL, root * Quaternion.Euler(p.ThighL, 0f, 0f), Quaternion.Euler(p.ShinL, 0f, 0f));
            Turn(rig.ThighR, root, Quaternion.Euler(p.ThighR, 0f, 0f));
            Turn(rig.ShinR, root * Quaternion.Euler(p.ThighR, 0f, 0f), Quaternion.Euler(p.ShinR, 0f, 0f));
        }

        /// <summary>Rotate a bone by q, where q is expressed in the (already rotated) frame of its parent part.</summary>
        static void Turn(Transform bone, Quaternion frame, Quaternion q) => bone.rotation = frame * q * Quaternion.Inverse(frame) * bone.rotation;

        // side = +1 for the left arm (T-pose points to -X), -1 for the right. Down from the T-pose, swing forward by the
        // pose's X, then turn inwards towards the rifle; the forearm bends further inwards.
        static void Arm(Transform arm, Transform fore, Quaternion frame, Vector3 euler, float foreBend, float side)
        {
            var q = Quaternion.AngleAxis(side * Mathf.Abs(euler.z), Vector3.up) * Quaternion.AngleAxis(euler.x, Vector3.right) * Quaternion.AngleAxis(side * 80f, Vector3.forward);
            Turn(arm, frame, q);
            Turn(fore, frame, Quaternion.AngleAxis(side * foreBend, Vector3.up));
        }

        // ---- boxes ----------------------------------------------------------------------------------------------
        struct BoxMesh { public Vector3[] Pos, Nrm; public int[] Tris; }

        static BoxMesh Box(Vector3 center, Vector3 size)
        {
            var pos = new Vector3[24]; var nrm = new Vector3[24]; var tris = new int[36];
            Vector3[] axes = { Vector3.right, Vector3.left, Vector3.up, Vector3.down, Vector3.forward, Vector3.back };
            int v = 0, ti = 0;
            foreach (var n in axes)
            {
                Vector3 a = Mathf.Abs(n.y) > 0.5f ? Vector3.right : Vector3.up, b = Vector3.Cross(n, a);
                for (int k = 0; k < 4; k++)
                {
                    float sa = (k == 0 || k == 3) ? -1f : 1f, sb = k < 2 ? -1f : 1f;
                    pos[v + k] = center + Vector3.Scale(n + a * sa + b * sb, size * 0.5f);
                    nrm[v + k] = n;
                }
                int[] quad = { v, v + 1, v + 2, v, v + 2, v + 3 };
                if (Vector3.Dot(Vector3.Cross(pos[v + 1] - pos[v], pos[v + 2] - pos[v]), n) < 0f) { quad[1] = v + 2; quad[2] = v + 1; quad[4] = v + 3; quad[5] = v + 2; }
                foreach (int q in quad) tris[ti++] = q;
                v += 4;
            }
            return new BoxMesh { Pos = pos, Nrm = nrm, Tris = tris };
        }
    }
}
