// Phase: Playground (2026-09-26, lane/show/playground) — flight, landing and settling of a loose rigid piece
// One step of a loose rigid piece in its owner's LOCAL frame (ground at y = 0, up +Y): gravity, spin, a landing that
// thumps and scrapes, gravity tipping it off a corner onto a face, and rest. Everything is in the owner's units (a
// vehicle drawn at 1.7x has units of 1.7 m), so the same piece given the same start lands the same way wherever its
// owner stands in the world, bit for bit: no world position ever enters the arithmetic. That is what lets three copies
// of a vehicle at three LODs, side by side, fall apart identically.
using UnityEngine;

namespace TW.Playground
{
    public struct Tumble
    {
        public Vector3 Pos, Vel, Spin;     // local units, local units/s, rad/s about a local axis
        public Quaternion Rot;
        public int Still;
        public bool Resting;
        /// <summary>Which way it falls if it comes to a stop balanced on a corner or an edge (set when it is thrown, from
        /// the owner's seeded stream, so copies still agree).</summary>
        public Vector3 Nudge;

        /// <summary>Advance h seconds. box: the piece's bounds in its own frame; metres: how many metres one local unit
        /// is (gravity is 9.81 m/s^2 whatever the owner's scale). Returns the speed it hit the ground at (0: no landing).</summary>
        public float Step(Bounds box, float metres, float h)
        {
            if (Resting) return 0f;
            float g = 9.81f / Mathf.Max(0.01f, metres);
            Vel.y -= g * h;
            Pos += Vel * h;
            float w = Spin.magnitude;
            if (w > 1e-5f) Rot = Quaternion.AngleAxis(w * h * Mathf.Rad2Deg, Spin / w) * Rot;
            // the lowest corner of the box
            float minY = float.MaxValue; Vector3 low = default;
            System.Span<float> ys = stackalloc float[8];   // no garbage: this runs 120 times a second per piece
            for (int k = 0; k < 8; k++)
            {
                var c = box.center + Vector3.Scale(box.extents, new Vector3((k & 1) == 0 ? -1f : 1f, (k & 2) == 0 ? -1f : 1f, (k & 4) == 0 ? -1f : 1f));
                var p = Pos + Rot * c;
                ys[k] = p.y;
                if (p.y < minY) { minY = p.y; low = p; }
            }
            if (minY >= 0f) { Still = 0; return 0f; }
            Pos.y -= minY; low.y = 0f;
            var com = Pos + Rot * box.center;
            var lever = com - low;
            float r2 = Mathf.Max(0.02f, box.extents.sqrMagnitude);
            float impact = 0f;
            if (Vel.y < 0f)
            {
                float vn = -Vel.y; impact = vn * metres;
                Vel.y = impact > 1.5f ? vn * 0.28f : 0f;          // a thump, then it stays down
                Vel.x *= 0.72f; Vel.z *= 0.72f;
                Spin = Spin * 0.6f + Vector3.Cross(lever, Vector3.up) * (vn * 0.5f / r2);
            }
            // standing on a corner, gravity tips it over onto a face; scraping slows it
            Spin += Vector3.Cross(lever, new Vector3(0f, -g, 0f)) * (h / (0.45f * r2));
            Spin *= 1f - Mathf.Min(1f, 3.5f * h);
            Vel.x *= 1f - Mathf.Min(1f, 2.5f * h); Vel.z *= 1f - Mathf.Min(1f, 2.5f * h);
            // resting needs a face on the ground (three corners down); a piece stopped on a corner or an edge is nudged over
            float tol = 0.04f * box.size.magnitude; int down = 0;
            for (int k = 0; k < 8; k++) if (ys[k] - minY < tol) down++;
            bool slow = (Vel * metres).sqrMagnitude < 0.04f && Spin.sqrMagnitude < 0.09f;
            if (slow && down < 3) { Spin += Nudge * (2.5f * h); Still = 0; }
            else if (slow) { if (++Still > 45) { Resting = true; Vel = Spin = Vector3.zero; } }
            else Still = 0;
            return impact;
        }
    }
}
