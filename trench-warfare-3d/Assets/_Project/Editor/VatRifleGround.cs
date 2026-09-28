// Phase: C2 (the troops' clip review, 2026-09-28) — keeps the baked rifle out of the ground, a pass over each clip's
// frames after VATBaker captures them. Every death and every prone, crawling and rolling clip stood a man's rifle 20-80
// cm deep in the mud (Tools/vatcheck.py, 57 clip rows on the two figures). Two treatments, both chosen by measuring the
// alternatives on the whole atlas (lift only floated the rifle up to 83 cm out of the hand; levelling it outright made
// the muzzle jump up to 1.4 m between frames where the source spins the rifle about the vertical):
//   a death drops the rifle: over 25 %-80 % of the clip it eases from the hand to lying level on the ground where it
//   ends, blended by its two ends (a blend of rotations flips round the far side when the source turns);
//   any other clip pitches the rifle about the grip towards level in proportion to how deep it went (5 cm or less only
//   lifts, 45 cm or more lies level), towards the way it leaned at the clip's start where the barrel is near vertical,
//   and lifts what is left. The muzzle and barrel sockets move with it.
using System.Collections.Generic;
using UnityEngine;
using TW.Presentation.Units;

namespace TW.Editor
{
    public static class VatRifleGround
    {
        /// <summary>The rifle box's own layout (VATBaker): 1.15 m long, 0.275 m of it behind the grip, 24 corners in face
        /// order +x, -x, +y, -y, +z, -z (4 each); x the 6 cm side, y the 10 cm side, z along the barrel.</summary>
        public const int Corners = 24;
        const float GripFromButt = 0.275f / 1.15f;

        struct Box { public Vector3 Centre, Long, Up, Side; public float Length; }

        static Vector3 Face(Vector3[] p, int r0, int face) => 0.25f * (p[r0 + 4 * face] + p[r0 + 4 * face + 1] + p[r0 + 4 * face + 2] + p[r0 + 4 * face + 3]);

        static Box Read(Vector3[] p, int r0)
        {
            Vector3 muzzleEnd = Face(p, r0, 4), buttEnd = Face(p, r0, 5), along = muzzleEnd - buttEnd;
            var b = new Box { Centre = 0.5f * (muzzleEnd + buttEnd), Length = along.magnitude, Long = along.normalized };
            b.Up = Vector3.ProjectOnPlane(Face(p, r0, 2) - Face(p, r0, 3), b.Long).normalized;
            b.Side = Vector3.Cross(b.Up, b.Long);
            return b;
        }

        static float Low(Vector3[] p, int r0)
        {
            float l = float.MaxValue;
            for (int i = 0; i < Corners; i++) l = Mathf.Min(l, p[r0 + i].y);
            return l;
        }

        /// <summary>Move the rifle rigidly from box `from` to box `to` (vertices, normals, muzzle, barrel), then lift it clear.</summary>
        static void Move(Vector3[] p, Vector3[] n, int r0, Vector3[] s, Box from, Box to)
        {
            var src = Quaternion.LookRotation(from.Long, from.Up); var dst = Quaternion.LookRotation(to.Long, to.Up);
            var q = dst * Quaternion.Inverse(src);
            for (int i = 0; i < Corners; i++) { p[r0 + i] = to.Centre + q * (p[r0 + i] - from.Centre); n[r0 + i] = q * n[r0 + i]; }
            s[VatAsset.Muzzle] = to.Centre + q * (s[VatAsset.Muzzle] - from.Centre);
            s[VatAsset.Barrel] = q * s[VatAsset.Barrel];
            float low = Low(p, r0);
            if (low < 0f) { for (int i = 0; i < Corners; i++) p[r0 + i].y -= low; s[VatAsset.Muzzle].y -= low; }
        }

        static float Smooth(float t) { t = Mathf.Clamp01(t); return t * t * (3f - 2f * t); }

        /// <summary>One clip's frames, [start, start + count), rifle corners from r0. A death drops the rifle.</summary>
        public static void Settle(IList<Vector3[]> frames, IList<Vector3[]> normals, IList<Vector3[]> sockets, int start, int count, int r0, bool death)
        {
            if (count <= 0) return;
            if (death) { Drop(frames, normals, sockets, start, count, r0); return; }
            var lean = Read(frames[start], r0).Long; lean.y = 0f;
            lean = lean.sqrMagnitude > 1e-6f ? lean.normalized : Vector3.forward;
            for (int f = start; f < start + count; f++) Pitch(frames[f], normals[f], r0, sockets[f], lean);
        }

        /// <summary>Pitch about the grip towards level as far as the depth asks, then lift the rest.</summary>
        public static void Pitch(Vector3[] p, Vector3[] n, int r0, Vector3[] s, Vector3 lean)
        {
            float depth = -Low(p, r0);
            if (depth <= 0f) return;
            var box = Read(p, r0);
            Vector3 grip = box.Centre - box.Long * box.Length * (0.5f - GripFromButt);
            Vector3 flat = new Vector3(box.Long.x, 0f, box.Long.z) + 0.6f * lean;
            var to = box;
            if (flat.sqrMagnitude > 1e-6f)
            {
                var level = Quaternion.FromToRotation(box.Long, flat.normalized);
                var q = Quaternion.Slerp(Quaternion.identity, level, Smooth((depth - 0.05f) / 0.40f));
                to.Long = q * box.Long; to.Up = q * box.Up; to.Centre = grip + q * (box.Centre - grip);
            }
            Move(p, n, r0, s, box, to);
        }

        /// <summary>A death: the rifle eases out of the hand to lie level on the ground where it ends.</summary>
        static void Drop(IList<Vector3[]> frames, IList<Vector3[]> normals, IList<Vector3[]> sockets, int start, int count, int r0)
        {
            var end = Read(frames[start + count - 1], r0);
            var along = new Vector3(end.Long.x, 0f, end.Long.z);
            along = along.magnitude > 0.2f ? along.normalized : Vector3.right;   // stood upright at the end: lay it along x
            var rest = new Box { Long = along, Up = Vector3.Cross(Vector3.up, along), Length = end.Length };
            // lying on its 10 cm side: its half height is half the 6 cm side
            float half = 0f;
            var endRot = Quaternion.LookRotation(end.Long, end.Up); var restRot = Quaternion.LookRotation(rest.Long, rest.Up);
            var p0 = frames[start + count - 1];
            for (int i = 0; i < Corners; i++) half = Mathf.Max(half, Mathf.Abs((restRot * Quaternion.Inverse(endRot) * (p0[r0 + i] - end.Centre)).y));
            rest.Centre = new Vector3(end.Centre.x, half + 0.005f, end.Centre.z);
            Vector3 restButt = rest.Centre - rest.Long * rest.Length * 0.5f, restMuzzle = rest.Centre + rest.Long * rest.Length * 0.5f;
            for (int k = 0; k < count; k++)
            {
                var p = frames[start + k];
                var box = Read(p, r0);
                float w = Smooth((k / (float)Mathf.Max(1, count - 1) - 0.25f) / 0.55f);
                Vector3 butt = Vector3.Lerp(box.Centre - box.Long * box.Length * 0.5f, restButt, w);
                Vector3 muzzle = Vector3.Lerp(box.Centre + box.Long * box.Length * 0.5f, restMuzzle, w);
                var to = box;
                if ((muzzle - butt).sqrMagnitude > 1e-8f)
                {
                    to.Long = (muzzle - butt).normalized;
                    to.Up = Vector3.ProjectOnPlane(Vector3.Lerp(box.Up, rest.Up, w), to.Long);
                    if (to.Up.sqrMagnitude < 1e-8f) to.Up = Vector3.ProjectOnPlane(Vector3.up, to.Long);
                    to.Up.Normalize();
                    to.Centre = 0.5f * (butt + muzzle);
                }
                Move(p, normals[start + k], r0, sockets[start + k], box, to);
            }
        }
    }
}
