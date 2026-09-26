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
        public int Still, Slow;
        public bool Resting;
        /// <summary>Which way it falls if it comes to a stop balanced on a corner or an edge (set when it is thrown, from
        /// the owner's seeded stream, so copies still agree).</summary>
        public Vector3 Nudge;
        /// <summary>Being pushed over off a small face (see Step).</summary>
        public bool Toppling; public Vector3 ToppleAxis; public float ToppleRate, ToppleAngle; public int Topples;

        /// <summary>Advance h seconds. box: the piece's bounds in its own frame; metres: how many metres one local unit
        /// is (gravity is 9.81 m/s^2 whatever the owner's scale). Returns the speed it hit the ground at (0: no landing).</summary>
        public float Step(Bounds box, float metres, float h) => Step(box, metres, h, 0f);

        /// <summary>floor: the height (in the owner's frame) of whatever lies under the piece: 0 on open ground, the top
        /// of the rubble it falls onto in a building.</summary>
        public float Step(Bounds box, float metres, float h, float floor)
        {
            if (Resting) return 0f;
            float g = 9.81f / Mathf.Max(0.01f, metres);
            if (Toppling)
            {
                // fall over about the corner it stands on, gathering speed, until a broad face is down
                Vector3 pivot = Pos; float lowY = float.MaxValue;
                for (int k = 0; k < 8; k++)
                {
                    var c = box.center + Vector3.Scale(box.extents, new Vector3((k & 1) == 0 ? -1f : 1f, (k & 2) == 0 ? -1f : 1f, (k & 4) == 0 ? -1f : 1f));
                    var q = Pos + Rot * c; if (q.y < lowY) { lowY = q.y; pivot = q; }
                }
                ToppleRate = Mathf.Min(5f, ToppleRate + 9f * h);
                var turn = Quaternion.AngleAxis(ToppleRate * h * Mathf.Rad2Deg, ToppleAxis);
                Rot = turn * Rot; Pos = pivot + turn * (Pos - pivot);
                Pos.y += floor - Mathf.Min(floor, LowestY(box));   // never into the ground
                // a push of at most 100 degrees: an axis that does not line up with the box's faces can roll it along
                // corner over corner for ever without a broad face ever coming down (a stack rolled 140 m, r7)
                ToppleAngle += ToppleRate * h * Mathf.Rad2Deg;
                if (Broad(box) || ToppleAngle > 100f) { Toppling = false; Vel = Vector3.zero; Spin = Vector3.zero; Still = Slow = 0; }
                return 0f;
            }
            Vel.y -= g * h;
            Pos += Vel * h;
            float w = Spin.magnitude;
            // turn about the centre of the box, not the part's pivot: turned about its pivot, a gun (pivot at the breech)
            // standing on its muzzle is a pendulum hung from the top, and every nudge that should topple it swings back
            // (critic r2: the antenna and gun came to rest standing 4 m tall on their tips)
            if (w > 1e-5f)
            {
                var centre0 = Pos + Rot * box.center;
                Rot = Quaternion.AngleAxis(w * h * Mathf.Rad2Deg, Spin / w) * Rot;
                Pos = centre0 - Rot * box.center;
            }
            // the lowest corner of the box
            float minY = float.MaxValue; Vector3 low = default;
            System.Span<float> ys = stackalloc float[8];   // no garbage: this runs 120 times a second per piece
            for (int k = 0; k < 8; k++)
            {
                var c = box.center + Vector3.Scale(box.extents, new Vector3((k & 1) == 0 ? -1f : 1f, (k & 2) == 0 ? -1f : 1f, (k & 4) == 0 ? -1f : 1f));
                var p = Pos + Rot * c;
                ys[k] = p.y - floor;
                if (p.y - floor < minY) { minY = p.y - floor; low = p; }
            }
            if (minY >= 0f) { Still = 0; return 0f; }
            Pos.y -= minY; low.y = floor;
            var com = Pos + Rot * box.center;
            var lever = com - low;
            float r2 = Mathf.Max(0.02f, box.extents.sqrMagnitude);
            float impact = 0f;
            if (Vel.y < 0f)
            {
                float vn = -Vel.y; impact = vn * metres;
                Vel.y = impact > 1.5f ? vn * 0.28f : 0f;          // a thump, then it stays down
                // a LANDING scrubs speed and spin; merely lying on the ground does not (gravity leaves the vertical speed a
                // hair below zero every step, and scrubbing spin 120 times a second froze every topple half-way over)
                if (impact > 0.4f)
                {
                    Vel.x *= 0.72f; Vel.z *= 0.72f;
                    Spin = Spin * 0.6f + Vector3.Cross(lever, Vector3.up) * (vn * 0.5f / r2);
                }
            }
            float tol = 0.04f * box.size.magnitude; int down = 0;
            for (int k = 0; k < 8; k++) if (ys[k] - minY < tol) down++;
            // A piece that has stopped on a SMALL face is pushed over, undamped, the way its seeded nudge points, until a
            // broad face is down. It is a box standing in for the part, and a box 1.5 x 2.1 x 4.6 m (the antenna with
            // its base) really is stable on its end within 18 degrees: gravity alone stood it back up (critic r2/r3).
            // on a corner or an edge, gravity tips it over onto a face; scraping slows it
            Spin += Vector3.Cross(lever, new Vector3(0f, -g, 0f)) * (h / (0.45f * r2));
            Spin *= 1f - Mathf.Min(1f, 3.5f * h);
            Vel.x *= 1f - Mathf.Min(1f, 2.5f * h); Vel.z *= 1f - Mathf.Min(1f, 2.5f * h);
            bool slow = (Vel * metres).sqrMagnitude < 0.04f && Spin.sqrMagnitude < 0.09f;
            if (slow && !Broad(box) && Topples < 3)
            {
                Topples++; ToppleAngle = 0f;
                var n = new Vector3(Nudge.x, 0f, Nudge.z); if (n.sqrMagnitude < 1e-6f) n = Vector3.right;
                ToppleAxis = Vector3.Cross(Vector3.up, n.normalized); Toppling = true; ToppleRate = 0f; Still = Slow = 0;
                return impact;
            }
            // resting needs a broad face on the ground (three corners down); on an edge gravity is still tipping it
            if (slow && down >= 3) { if (++Still > 45) { Resting = true; Vel = Spin = Vector3.zero; } }
            else Still = 0;
            // and a piece on a broad face that has crept along for two seconds without getting anywhere is at rest,
            // however its corners lie (a rounded lamp or a bent plate never shows three corners down: 7 parts a wreck were
            // still being pushed 13 s after the cook-off)
            if ((Vel * metres).sqrMagnitude < 0.09f && Spin.sqrMagnitude < 0.25f && Broad(box)) { if (++Slow > 240) { Resting = true; Vel = Spin = Vector3.zero; } }
            else Slow = 0;
            return impact;
        }

        float LowestY(Bounds box)
        {
            float lo = float.MaxValue;
            for (int k = 0; k < 8; k++)
                lo = Mathf.Min(lo, (Pos + Rot * (box.center + Vector3.Scale(box.extents, new Vector3((k & 1) == 0 ? -1f : 1f, (k & 2) == 0 ? -1f : 1f, (k & 4) == 0 ? -1f : 1f)))).y);
            return lo;
        }

        /// <summary>The box face nearest to facing down is at least 0.6 of its largest face.</summary>
        bool Broad(Bounds box)
        {
            var e = box.size;
            float ax = Mathf.Abs((Rot * Vector3.right).y), ay = Mathf.Abs((Rot * Vector3.up).y), az = Mathf.Abs((Rot * Vector3.forward).y);
            float face = ax >= ay && ax >= az ? e.y * e.z : ay >= az ? e.x * e.z : e.x * e.y;
            float largest = Mathf.Max(e.x * e.y, Mathf.Max(e.y * e.z, e.x * e.z));
            // 0.6: a gun barrel's box (1.8 x 1.9 x 3.1 m) stood on end is 0.58 of its largest face, and a track stood
            // upright on its rollers 0.45: both read as broken standing, both are pushed over
            return face >= 0.6f * largest;
        }
    }
}
