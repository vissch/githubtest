// Phase: B3 (implemented for the one rig and clip we have; C2 adds clips, not code paths)
// Bakes Art/Characters/CrouchedRun.fbx (a Mixamo-rigged 881-vertex figure with a single crouched-run clip) into the
// VAT layout VATRenderer draws: U = vertex, V = frame, RGBAHalf positions and normals, row table in AnimRow order.
// - Walk, Sprint and CrouchWalk all point at the baked clip (men cross no man's land bent double anyway).
// - Standing rows (idle, fire, throw, flinch, vault) hold the clip's widest stride, which reads as a braced firing
//   crouch with the weapon up, plus breathing, recoil or a duck on the spine and hips.
// - Rows on the ground (prone, pinned, deaths) have no clip to borrow from: the skeleton is put back into its bind
//   T-pose (from the mesh bind poses; the FBX itself rests in frame 0 of the run) and posed in code from
//   ProceduralSoldier.Sample. Rotations are applied about the character's own axes, parent first, which keeps this
//   independent of Mixamo's bone axes.
// - The figure has no UVs: vertex colours come from the dominant bone (skin, boots, team-tinted cloth).
// - A helmet (rigid to the head) and a rifle (carried in the right hand, pointing forward) are added as boxes.
// The result is normalised to a 1.78 m man standing on y = 0, facing +Z.
using System.Collections.Generic;
using System.Linq;
using UnityEditor;
using UnityEngine;
using TW.Sim;
using TW.Presentation.Units;

namespace TW.Editor
{
    public static class VATBaker
    {
        public const int SampleHz = 30;
        public const string SourcePath = "Assets/_Project/Art/Characters/CrouchedRun.fbx";
        public const string OutputFolder = "Assets/_Project/Resources/Units";
        const int PosedFrames = 16;

        sealed class Rig
        {
            public GameObject Root;
            public SkinnedMeshRenderer Skin;
            public Transform Hips, Spine, Neck, Head, ArmL, ArmR, ForeL, ForeR, HandL, HandR, ThighL, ShinL, ThighR, ShinR, FootL, FootR;
            public Transform[] All;
            public Vector3[] BindPos;
            public Quaternion[] BindRot;
            public float MinY, Scale, HipHeight;
            public Matrix4x4 HeadBindInverse;

            public void Reset()
            {
                for (int i = 0; i < All.Length; i++) All[i].SetLocalPositionAndRotation(BindPos[i], BindRot[i]);
            }
        }

        [MenuItem("TW/VAT/Bake Infantry")]
        public static void BakeInfantry()
        {
            var source = AssetDatabase.LoadAssetAtPath<GameObject>(SourcePath);
            if (source == null) { Debug.LogError("VATBaker: missing " + SourcePath); return; }
            var clip = AssetDatabase.LoadAllAssetsAtPath(SourcePath).OfType<AnimationClip>().FirstOrDefault(c => !c.name.StartsWith("__preview__"));
            var rig = BuildRig(source);
            try { Bake(rig, clip); }
            finally { Object.DestroyImmediate(rig.Root); }
        }

        static Rig BuildRig(GameObject source)
        {
            var go = Object.Instantiate(source);
            go.hideFlags = HideFlags.HideAndDontSave;
            go.transform.SetPositionAndRotation(Vector3.zero, Quaternion.identity);
            var rig = new Rig { Root = go, Skin = go.GetComponentInChildren<SkinnedMeshRenderer>(), All = go.GetComponentsInChildren<Transform>() };
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

            // the instantiated FBX rests in the clip's first frame; the T-pose only survives in the mesh bind poses
            var skinBones = rig.Skin.bones; var bind = rig.Skin.sharedMesh.bindposes;
            var meshToWorld = rig.Skin.transform.localToWorldMatrix;
            int Depth(Transform t) { int d = 0; while (t.parent != null) { t = t.parent; d++; } return d; }
            foreach (int i in Enumerable.Range(0, skinBones.Length).OrderBy(i => Depth(skinBones[i])))
            {
                var m = meshToWorld * bind[i].inverse;
                skinBones[i].SetPositionAndRotation(m.GetColumn(3), m.rotation);
            }
            rig.BindPos = rig.All.Select(t => t.localPosition).ToArray();
            rig.BindRot = rig.All.Select(t => t.localRotation).ToArray();

            var verts = new List<Vector3>(); var normals = new List<Vector3>();
            Skinned(rig, verts, normals);
            float minY = verts.Min(p => p.y), maxY = verts.Max(p => p.y);
            rig.MinY = minY;
            rig.Scale = ProceduralSoldier.Height / (maxY - minY);
            rig.HipHeight = rig.Hips.position.y - minY;
            rig.HeadBindInverse = rig.Head.worldToLocalMatrix;
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

        static void Bake(Rig rig, AnimationClip clip)
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
            var helmet = Box(new Vector3(headPos.x, top - 0.07f * u, headPos.z + 0.01f * u), new Vector3(0.31f, 0.03f, 0.34f) * u);   // the brim of a Brodie
            var rifle = Box(new Vector3(0f, 0f, 0.20f * u), new Vector3(0.05f, 0.07f, 1.15f) * u);
            int vertexCount = skinCount + helmet.Pos.Length + rifle.Pos.Length;

            // ---- frames -----------------------------------------------------------------------------------------
            var frames = new List<Vector3[]>(); var frameNormals = new List<Vector3[]>();
            void Capture(Vector3 shift)
            {
                Skinned(rig, verts, normals);
                var p = new Vector3[vertexCount]; var n = new Vector3[vertexCount];
                for (int i = 0; i < skinCount; i++) { p[i] = verts[i]; n[i] = normals[i]; }
                var head = rig.Head.localToWorldMatrix * rig.HeadBindInverse;
                for (int i = 0; i < helmet.Pos.Length; i++) { p[skinCount + i] = head.MultiplyPoint3x4(helmet.Pos[i]); n[skinCount + i] = head.MultiplyVector(helmet.Nrm[i]).normalized; }
                // carried in the right hand, pointing forward and a little towards the left hand
                Vector3 hand = rig.HandR.position, across = (rig.HandL.position - hand).normalized;
                Vector3 dir = (Vector3.forward * 0.62f + across * 0.38f + Vector3.up * 0.08f).normalized;
                var grip = Matrix4x4.TRS(hand, Quaternion.LookRotation(dir, Vector3.up), Vector3.one);
                int r0 = skinCount + helmet.Pos.Length;
                for (int i = 0; i < rifle.Pos.Length; i++) { p[r0 + i] = grip.MultiplyPoint3x4(rifle.Pos[i]); n[r0 + i] = grip.MultiplyVector(rifle.Nrm[i]); }
                for (int i = 0; i < vertexCount; i++) p[i] = (p[i] - shift - new Vector3(0f, rig.MinY, 0f)) * rig.Scale;
                frames.Add(p); frameNormals.Add(n);
            }

            var table = new Vector2[(int)AnimRow.Count];
            Vector2 run = default;
            float contactTime = 0f; Vector3 contactShift = Vector3.zero;
            if (clip != null)
            {
                int n = Mathf.Max(2, Mathf.RoundToInt(clip.length * SampleHz));
                rig.Reset(); clip.SampleAnimation(rig.Root, 0f);
                Vector3 first = rig.Hips.position;
                clip.SampleAnimation(rig.Root, clip.length);
                bool travels = Vector3.Distance(Flat(rig.Hips.position), Flat(first)) > 0.2f * u;
                run = new Vector2(frames.Count, n);
                float widest = -1f;
                for (int f = 0; f < n; f++)
                {
                    rig.Reset();
                    clip.SampleAnimation(rig.Root, clip.length * f / n);
                    Capture(Flat(travels ? rig.Hips.position : first));   // baked in place: the sim moves the man
                    float stride = Mathf.Abs(rig.FootL.position.z - rig.FootR.position.z);
                    if (stride > widest) { widest = stride; contactTime = clip.length * f / n; contactShift = Flat(travels ? rig.Hips.position : first); }
                }
            }
            for (int r = 0; r < table.Length; r++)
            {
                var row = (AnimRow)r;
                if (clip != null && (row == AnimRow.Walk || row == AnimRow.Sprint || row == AnimRow.CrouchWalk)) { table[r] = run; continue; }
                table[r] = new Vector2(frames.Count, PosedFrames);
                for (int f = 0; f < PosedFrames; f++)
                {
                    float t = f / (float)PosedFrames;
                    if (clip != null && Standing(row))
                    {
                        rig.Reset();
                        clip.SampleAnimation(rig.Root, contactTime);
                        Adjust(rig, row, t);
                        Capture(contactShift);
                        continue;
                    }
                    ApplyPose(rig, ProceduralSoldier.Sample(row, t));
                    Capture(Vector3.zero);
                }
            }

            // ---- mesh -------------------------------------------------------------------------------------------
            var colors = new Color[vertexCount];
            var weights = source.boneWeights; var bones = rig.Skin.bones;
            for (int i = 0; i < skinCount; i++) colors[i] = BoneColor(bones[weights[i].boneIndex0].name);
            for (int i = 0; i < helmet.Pos.Length; i++) colors[skinCount + i] = new Color(0.30f, 0.33f, 0.27f, 0f);
            for (int i = 0; i < rifle.Pos.Length; i++) colors[skinCount + helmet.Pos.Length + i] = new Color(0.27f, 0.18f, 0.11f, 0f);
            var tris = new List<int>(source.triangles);
            tris.AddRange(helmet.Tris.Select(t => t + skinCount));
            tris.AddRange(rifle.Tris.Select(t => t + skinCount + helmet.Pos.Length));
            int idle = (int)table[(int)AnimRow.Idle].x;
            var mesh = new Mesh { name = "InfantryVatMesh" };
            mesh.SetVertices(frames[idle]); mesh.SetNormals(frameNormals[idle]); mesh.SetColors(colors); mesh.SetTriangles(tris, 0);
            mesh.bounds = new Bounds(new Vector3(0f, 0.9f, 0f), new Vector3(3f, 2.4f, 3f));

            // ---- atlases and assets -----------------------------------------------------------------------------
            int total = frames.Count;
            var pos = new Color[vertexCount * total]; var nrm = new Color[vertexCount * total];
            for (int f = 0; f < total; f++)
                for (int i = 0; i < vertexCount; i++)
                {
                    Vector3 p = frames[f][i], n = frameNormals[f][i];
                    pos[f * vertexCount + i] = new Color(p.x, p.y, p.z, 1f);
                    nrm[f * vertexCount + i] = new Color(n.x, n.y, n.z, 0f);
                }
            if (!AssetDatabase.IsValidFolder("Assets/_Project/Resources")) AssetDatabase.CreateFolder("Assets/_Project", "Resources");
            if (!AssetDatabase.IsValidFolder(OutputFolder)) AssetDatabase.CreateFolder("Assets/_Project/Resources", "Units");
            foreach (var name in new[] { "InfantryVat", "InfantryVatMesh", "InfantryVatPositions", "InfantryVatNormals" })
                AssetDatabase.DeleteAsset($"{OutputFolder}/{name}.asset");
            var data = ScriptableObject.CreateInstance<VatAssetData>();
            data.Mesh = mesh; data.RowTable = table; data.TotalFrames = total;
            data.Positions = Atlas("InfantryVatPositions", vertexCount, total, pos);
            data.Normals = Atlas("InfantryVatNormals", vertexCount, total, nrm);
            AssetDatabase.CreateAsset(mesh, $"{OutputFolder}/InfantryVatMesh.asset");
            AssetDatabase.CreateAsset(data.Positions, $"{OutputFolder}/InfantryVatPositions.asset");
            AssetDatabase.CreateAsset(data.Normals, $"{OutputFolder}/InfantryVatNormals.asset");
            AssetDatabase.CreateAsset(data, $"{OutputFolder}/InfantryVat.asset");
            AssetDatabase.SaveAssets();
            Debug.Log($"VATBaker: {vertexCount} vertices x {total} frames ({(clip != null ? clip.name + " " + run.y + " frames" : "no clip")}), scale {rig.Scale:0.###}");
        }

        static Vector3 Flat(Vector3 v) => new Vector3(v.x, 0f, v.z);

        static Texture2D Atlas(string name, int width, int height, Color[] pixels)
        {
            var t = new Texture2D(width, height, TextureFormat.RGBAHalf, false, true) { name = name, filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp };
            t.SetPixels(pixels);
            t.Apply(false, false);
            return t;
        }

        static Color BoneColor(string bone)
        {
            if (bone.Contains("Head") || bone.Contains("Neck") || bone.Contains("Hand")) return new Color(0.78f, 0.60f, 0.47f, 0f);
            if (bone.Contains("Foot") || bone.Contains("Toe")) return new Color(0.13f, 0.11f, 0.10f, 0f);
            if (bone.Contains("Leg") && !bone.Contains("UpLeg")) return new Color(0.72f, 0.72f, 0.66f, 1f);   // puttees
            return new Color(1f, 1f, 1f, 1f);
        }

        static bool Standing(AnimRow row) => row == AnimRow.Idle || row == AnimRow.FireStanding || row == AnimRow.FireFireStep || row == AnimRow.Throw
            || row == AnimRow.Flinch0 || row == AnimRow.Flinch1 || row == AnimRow.Flinch2 || row == AnimRow.Vault;

        /// <summary>Small motion on top of the held stride: breathing, recoil, a duck, a hop.</summary>
        static void Adjust(Rig rig, AnimRow row, float t)
        {
            float u = 1f / rig.Scale, kick = Mathf.Exp(-t * 9f), arc = Mathf.Sin(t * Mathf.PI);
            Vector3 spine = Vector3.zero, hips = Vector3.zero;
            switch (row)
            {
                case AnimRow.Idle: spine.x = 1.5f * Mathf.Sin(t * Mathf.PI * 2f); break;
                case AnimRow.FireStanding:
                case AnimRow.FireFireStep: spine.x = -5f * kick; hips.z = -0.025f * kick; break;
                case AnimRow.Throw: spine.y = -30f + 60f * t; break;
                case AnimRow.Vault: hips.y = 0.35f * arc; spine.x = 18f * arc; break;
                default: hips.y = -0.15f * arc; spine = new Vector3(24f * arc, (row - AnimRow.Flinch1) * 20f * arc, 0f); break;
            }
            rig.Hips.position += hips * u;
            Turn(rig.Spine, Quaternion.identity, Quaternion.Euler(spine));
        }

        // ---- posing the rig from a SoldierPose ------------------------------------------------------------------
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
