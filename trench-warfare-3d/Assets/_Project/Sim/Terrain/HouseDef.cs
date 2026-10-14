// Phase: A4 (implemented 2026-10-13) — a house the sim knows about: a rectangular footprint that gives a man on foot
// standing behind it cover from small arms (the owner, 2026-10-07: "A little: 35 % less, like a man in a shell hole").
// It blocks nothing and holds no hit points: only DirectFire reads it, and only on the small-arms open-ground branch.
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    /// <summary>A building's footprint: centre, heading and half extents in metres (y is ignored).</summary>
    public struct HouseDef
    {
        public float3 Pos;
        public float Yaw;      // radians
        public float2 Half;    // half extents along the house's own X and Z
    }

    public static class HouseRules
    {
        /// <summary>True when the segment from → to really crosses the footprint, so "behind" differs from "in
        /// front" and "beside" for any shooter bearing. A 2D slab test in the house's own frame.</summary>
        public static bool Blocks(in HouseDef h, float3 from, float3 to)
        {
            // the house's own frame, the sim's yaw convention (SimMath.DirFromYaw): forward (sin, cos), right (cos, -sin)
            float2 sc = SimMath.SinCos(h.Yaw);    // x = sin, y = cos
            float2 a = Local(from.xz - h.Pos.xz, sc), b = Local(to.xz - h.Pos.xz, sc);
            float2 d = b - a;
            float t0 = 0f, t1 = 1f;
            if (!Slab(a.x, d.x, h.Half.x, ref t0, ref t1)) return false;
            if (!Slab(a.y, d.y, h.Half.y, ref t0, ref t1)) return false;
            return true;
        }

        static float2 Local(float2 v, float2 sc) => new float2(v.x * sc.y - v.y * sc.x, v.x * sc.x + v.y * sc.y);

        static bool Slab(float a, float d, float half, ref float t0, ref float t1)
        {
            if (math.abs(d) < 1e-6f) return math.abs(a) <= half;   // parallel to this slab
            float lo = (-half - a) / d, hi = (half - a) / d;
            if (lo > hi) { float s = lo; lo = hi; hi = s; }
            t0 = math.max(t0, lo); t1 = math.min(t1, hi);
            return t0 <= t1;
        }
    }
}
