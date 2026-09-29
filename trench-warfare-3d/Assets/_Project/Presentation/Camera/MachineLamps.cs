// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — depends on: TankModel.
// Where a machine's running lamps sit, read off its own hull (no model carries lamp sockets): the four upper corners of
// the root part's upper half, front left and right, rear left and right, each nudged 6 cm out so the glow stands proud of
// the plate. Each pair is a vertex the mesh really has and its mirror across the hull. And the Maw's furnace, the middle of the hull's
// vertices its splitter painted as fire (UV2.y, Tools/tanksplit.py).
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public static class MachineLamps
    {
        /// <summary>Front left, front right, rear left, rear right, in the root part's mesh space (+z forward).</summary>
        public const int FrontLeft = 0, FrontRight = 1, RearLeft = 2, RearRight = 3;
        public const float Nudge = 0.06f;

        /// <summary>The four lamps of a model, in its root part's mesh space (what TankRenderer's v.World[0] takes), or null
        /// when it has no readable root mesh. Chosen in the hull's own frame (+y up, +z forward, +x right): the root part
        /// may carry a rotation of its own from its FBX, and its mesh space is then not the hull's.</summary>
        public static Vector3[] Place(TankModel model)
        {
            if (model == null || model.Lods == null || model.Lods.Length == 0 || model.Lods[0] == null || model.Lods[0].Parts.Count == 0) return null;
            var root = model.Lods[0].Parts[0];
            var mesh = root.Mesh;
            if (mesh == null || !mesh.isReadable) return null;
            var verts = mesh.vertices;
            if (verts.Length == 0) return null;
            var toHull = HullFrame(root);
            Vector3 lo = Vector3.positiveInfinity, hi = Vector3.negativeInfinity;
            var hull = new Vector3[verts.Length];
            for (int i = 0; i < verts.Length; i++) { hull[i] = toHull.MultiplyPoint3x4(verts[i]); lo = Vector3.Min(lo, hull[i]); hi = Vector3.Max(hi, hull[i]); }
            Vector3 c = (lo + hi) * 0.5f, e = Vector3.Max((hi - lo) * 0.5f, Vector3.one * 1e-3f);
            var best = new float[4] { float.MinValue, float.MinValue, float.MinValue, float.MinValue };
            var at = new Vector3[4];
            // only the upper half of the hull: scored over all of it, the Maw's rear lamp went to the tail of a track 2.3 m
            // up a 7.7 m hull, and its front pair to the top of the superstructure beside the centre line
            for (int i = 0; i < hull.Length; i++)
            {
                if (hull[i].y < c.y) continue;
                Vector3 n = new Vector3((hull[i].x - c.x) / e.x, (hull[i].y - c.y) / e.y, (hull[i].z - c.z) / e.z);
                for (int k = 0; k < 4; k++)
                {
                    float side = (k == FrontLeft || k == RearLeft) ? -1f : 1f, end = k < RearLeft ? 1f : -1f;
                    float score = side * n.x * 0.6f + end * n.z + n.y * 0.25f;
                    if (score > best[k]) { best[k] = score; at[k] = hull[i]; }
                }
            }
            // a pair is one lamp and its mirror across the hull (every hull is near symmetric): the better side's pick,
            // mirrored, so the Maw's two tail lamps do not stand 1.1 m and 2.1 m out from its centre line
            for (int pair = 0; pair < 4; pair += 2)
            {
                int keep = best[pair] >= best[pair + 1] ? pair : pair + 1, other = keep == pair ? pair + 1 : pair;
                at[other] = new Vector3(2f * c.x - at[keep].x, at[keep].y, at[keep].z);
            }
            var toMesh = toHull.inverse;
            for (int k = 0; k < 4; k++)
            {
                Vector3 out_ = at[k] - c; out_.y = 0f;
                if (out_.sqrMagnitude > 1e-6f) at[k] += out_.normalized * Nudge;
                at[k] = toMesh.MultiplyPoint3x4(at[k]);
            }
            return at;
        }

        /// <summary>From the root part's mesh space to the hull's frame (the model's root, which TankRenderer poses).</summary>
        public static Matrix4x4 HullFrame(TankModel.Part root) => Matrix4x4.TRS(root.Local, root.LocalRot, Vector3.one);

        /// <summary>The middle of the root part's fire-painted vertices (UV2.y above one half), in its mesh space: the
        /// Maw's furnace mouth. False when the model has none.</summary>
        public static bool Furnace(TankModel model, out Vector3 at)
        {
            at = Vector3.zero;
            if (model == null || model.Lods == null || model.Lods.Length == 0 || model.Lods[0] == null || model.Lods[0].Parts.Count == 0) return false;
            var mesh = model.Lods[0].Parts[0].Mesh;
            if (mesh == null || !mesh.isReadable) return false;
            var masks = new List<Vector4>();
            mesh.GetUVs(2, masks);
            var verts = mesh.vertices;
            if (masks.Count != verts.Length) return false;
            Vector3 sum = Vector3.zero; int n = 0;
            for (int i = 0; i < verts.Length; i++) if (masks[i].y > 0.5f) { sum += verts[i]; n++; }
            if (n == 0) return false;
            at = sum / n;
            return true;
        }
    }
}
