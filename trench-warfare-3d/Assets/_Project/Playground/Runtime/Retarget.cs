// Phase: Playground (2026-09-26, lane/show/playground) — the game's Mixamo clips carried onto other proportions at runtime
// Carries the game's Mixamo clips over to a figure with other proportions, at runtime, without copying or reimporting
// a single clip. The clip is sampled on the rig the game already samples it on (Art/Characters/Soldier.fbx, what
// VATBaker bakes), and each bone's CHANGE from a canonical T-pose is put onto the target's own canonical T-pose:
//     target(bone) = change(bone) * targetRest(bone),   change = source(bone) * inverse(sourceRest(bone))
// in each rig's own character frame (right, up, forward, measured off its skeleton, so a rig imported facing the other
// way still works). The canonical T-pose is the bind pose with each limb swung onto a fixed direction (spine up, arms
// out level, legs down, feet forward-down): the soldier was auto-rigged in an A-pose and the frog in a T-pose, and
// aligning both to one pose is what makes "change from rest" mean the same thing on both.
// The hips' height is scaled by the ratio of the two hip heights (as VATBaker scales its clips'), and a clip's travel is
// taken out as a straight-line trend so the figure walks on the spot and keeps its sway; a death keeps its travel.
using UnityEngine;

namespace TW.Playground
{
    public sealed class Retarget
    {
        public static readonly string[] Names =
        {
            "Hips", "Spine", "Spine1", "Spine2", "Neck", "Head",
            "LeftShoulder", "LeftArm", "LeftForeArm", "LeftHand",
            "RightShoulder", "RightArm", "RightForeArm", "RightHand",
            "LeftUpLeg", "LeftLeg", "LeftFoot", "LeftToeBase",
            "RightUpLeg", "RightLeg", "RightFoot", "RightToeBase",
        };
        // the bone each one is aimed along for the canonical pose (-1: it keeps its bind orientation relative to its parent)
        static readonly int[] Aim = { 1, 2, 3, 4, 5, -1, 7, 8, 9, -1, 11, 12, 13, -1, 15, 16, 17, -1, 19, 20, 21, -1 };
        // where it is aimed, in the character frame (x right, y up, z forward)
        static readonly Vector3[] Canon =
        {
            Vector3.up, Vector3.up, Vector3.up, Vector3.up, Vector3.up, Vector3.zero,
            Vector3.left, Vector3.left, Vector3.left, Vector3.zero,
            Vector3.right, Vector3.right, Vector3.right, Vector3.zero,
            Vector3.down, Vector3.down, new Vector3(0f, -0.45f, 1f), Vector3.zero,
            Vector3.down, Vector3.down, new Vector3(0f, -0.45f, 1f), Vector3.zero,
        };

        public sealed class Skeleton
        {
            public Transform Root;
            public readonly Transform[] Bones = new Transform[Names.Length];
            public readonly Quaternion[] Rest = new Quaternion[Names.Length];   // canonical pose, character frame
            public Quaternion[] BindLocal = new Quaternion[Names.Length];
            public Quaternion Frame = Quaternion.identity;                   // character frame in the root's frame
            public Vector3 HipsRest;                                         // character frame, root units
            public int Found;

            public static Skeleton Of(Transform root)
            {
                var s = new Skeleton { Root = root };
                var all = root.GetComponentsInChildren<Transform>(true);
                for (int i = 0; i < Names.Length; i++)
                    foreach (var t in all)
                        if (t.name == Names[i] || t.name.EndsWith(":" + Names[i]) || t.name.EndsWith("_" + Names[i])) { s.Bones[i] = t; s.Found++; break; }
                for (int i = 0; i < Names.Length; i++) if (s.Bones[i] != null) s.BindLocal[i] = s.Bones[i].localRotation;
                s.Canonicalise();
                return s;
            }

            public Vector3 Forward => Root.rotation * (Frame * Vector3.forward);

            /// <summary>Measure the character frame off the bind pose, swing each limb onto the canonical pose, record each
            /// bone's rotation there, and put the bind pose back.</summary>
            public void Canonicalise()
            {
                var hips = Bones[0]; var head = Bones[5] ?? Bones[4];
                var la = Bones[7]; var ra = Bones[11];
                if (hips == null || head == null || la == null || ra == null) { Debug.LogWarning("Retarget: skeleton incomplete under " + Root.name); return; }
                Vector3 up = Root.InverseTransformDirection(head.position - hips.position).normalized;
                Vector3 right = Root.InverseTransformDirection(ra.position - la.position); right -= up * Vector3.Dot(right, up); right.Normalize();
                Vector3 fwd = Vector3.Cross(right, up);
                Frame = Quaternion.LookRotation(fwd, up);
                var world = Root.rotation * Frame;
                for (int i = 0; i < Names.Length; i++)
                {
                    if (Bones[i] == null || Aim[i] < 0 || Bones[Aim[i]] == null) continue;
                    var dir = Bones[Aim[i]].position - Bones[i].position;
                    if (dir.sqrMagnitude < 1e-10f) continue;
                    var want = world * Canon[i].normalized;
                    Bones[i].rotation = Quaternion.FromToRotation(dir.normalized, want) * Bones[i].rotation;
                }
                var inv = Quaternion.Inverse(world);
                for (int i = 0; i < Names.Length; i++) if (Bones[i] != null) Rest[i] = inv * Bones[i].rotation;
                HipsRest = Quaternion.Inverse(Frame) * Root.InverseTransformPoint(hips.position);
                for (int i = 0; i < Names.Length; i++) if (Bones[i] != null) Bones[i].localRotation = BindLocal[i];
            }

            public Vector3 HipsNow => Quaternion.Inverse(Frame) * Root.InverseTransformPoint(Bones[0].position);
        }

        /// <summary>Put the source's current pose onto the target. hipScale: target hip height over the clip's.</summary>
        public static void Apply(Skeleton src, Skeleton dst, float hipScale, Vector3 hipsOffset, float weight = 1f, Quaternion[] from = null)
        {
            var sw = src.Root.rotation * src.Frame; var isw = Quaternion.Inverse(sw);
            var dw = dst.Root.rotation * dst.Frame;
            for (int i = 0; i < Names.Length; i++)
            {
                var s = src.Bones[i]; var d = dst.Bones[i];
                if (s == null || d == null) continue;
                var change = (isw * s.rotation) * Quaternion.Inverse(src.Rest[i]);
                d.rotation = dw * (change * dst.Rest[i]);
            }
            var hp = src.HipsNow - hipsOffset;
            dst.Bones[0].position = dst.Root.TransformPoint(dst.Frame * (hp * hipScale));
            if (from != null && weight < 1f)
                for (int i = 0; i < Names.Length; i++)
                    if (dst.Bones[i] != null) dst.Bones[i].localRotation = Quaternion.Slerp(from[i], dst.Bones[i].localRotation, weight);
        }
    }
}
