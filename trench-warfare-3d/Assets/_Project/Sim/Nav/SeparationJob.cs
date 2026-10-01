// Phase: A1 (implemented)
// Soft repulsion between infantry within 2r, plus avoidance of vehicles. Each slot reads its own 3x3 hash buckets
// and the (short) vehicle list and writes only its own velocity adjustment, so the job is parallel and deterministic.
// A garrison and a man outside the trench never push each other (the trench wall is between them).
// A garrison man on his way to his post squeezes past his mates (2026-10-01): between him and them the push is the soft
// one, at a body's width, not the 2 m spacing with its hard core under a metre. Held off at that core by the man at the
// post ahead of him, he was shoved back a step every few ticks (forward, forward, forward, back) for as long as it took,
// and drawn turning about on every back-step.
// A man down on his belly in the open is squeezed past the same way (2026-10-01, men under fire): between him and any
// man on the surface the push is the soft one at a body's width. With the 1.6 m surface spacing a man running up behind
// a mate who had dropped stepped from its soft push to the hard core at one metre and back, tick by tick (the scripts'
// match, seed 2: one man's steps reversed 19 times behind a lying mate), and men down in one shell hole shoved each
// other about; now a runner slips past a man who is down, and men lying together lie a body's width apart.
// Vehicles take no push: they are moved by VehicleKinematicsSystem and shove infantry, not the other way round.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;

namespace TW.Sim.Nav
{
    [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
    public struct SeparationJob : IJobParallelFor
    {
        public const float Radius = 0.5f;          // infantry collision radius (m)
        public const float VehicleRadius = 2.5f;   // hull half-width used for infantry avoidance (m)
        public const float Strength = 4f;          // m/s per metre of overlap
        public const float GarrisonSpacing = 2f;   // a garrison spreads along its trench until men stand this far apart
        public const float GarrisonStrength = 0.75f;   // soft: it must lose against the push that clears a ladder
        public const float SurfaceSpacing = 1.6f;      // men on open ground drift apart until they stand this far apart
        public const float SurfaceStrength = 0.55f;    // softly: the flow and their own drift still decide where they go
        public const float MaxPush = 6f;           // m/s cap, so a dense stack spreads out instead of being fired across the map
        public const float PostNear = 0.9f;        // MovementSystem's "at his post": nearer than this to its point, or in its cell

        [ReadOnly] public SpatialHash Hash;
        [ReadOnly] public NativeArray<float3> Position;
        [ReadOnly] public NativeArray<uint> Flags;
        [ReadOnly] public NativeArray<short> TrenchId;  // garrison trench per slot, -1 none
        [ReadOnly] public NativeArray<int> Vehicles;   // alive vehicle slots (slot order)
        public NativeArray<float3> Push;               // output: additive velocity for this tick
        [ReadOnly] public NativeArray<int> PostCell;    // his post in the trench (TrenchGarrisonSystem), -1 none
        [ReadOnly] public NativeArray<byte> StanceOf;   // last tick's stance: a man down on his belly (Prone, Pinned)
        public int NavWidth; public float NavCell;

        /// <summary>A garrison man walking to his post, not at it (MovementSystem's atPost, without its ladder case).</summary>
        bool OnTheWay(int k)
        {
            int post = PostCell[k];
            if (TrenchId[k] < 0 || post < 0) return false;
            float3 p = Position[k];
            int cell = (int)(p.z / NavCell) * NavWidth + (int)(p.x / NavCell);
            if (cell == post) return false;
            float3 at = new float3((post % NavWidth + 0.5f) * NavCell, 0f, (post / NavWidth + 0.5f) * NavCell) + TrenchPost.Offset(post, NavWidth);
            float3 d = at - p; d.y = 0f;
            return SimMath.Length(d) >= PostNear;
        }

        /// <summary>Two units on the same point: opposite directions for the pair (antisymmetric in i, j), and a
        /// different axis per pair so a stack of N fans out instead of moving as one.</summary>
        static float3 CoincidentNormal(int i, int j)
        {
            int lo = math.min(i, j), hi = math.max(i, j);
            int k = (lo * 7 + hi * 3) & 3;                       // 4 axes, 45 degrees apart
            float3 axis = k == 0 ? new float3(1f, 0f, 0f) : k == 1 ? new float3(0.70710677f, 0f, 0.70710677f)
                        : k == 2 ? new float3(0f, 0f, 1f) : new float3(-0.70710677f, 0f, 0.70710677f);
            return i < j ? axis : -axis;
        }

        public void Execute(int i)
        {
            Push[i] = float3.zero;
            uint f = Flags[i];
            if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) != 0) return;
            float3 p = Position[i];
            int cx = math.clamp((int)(p.x / Hash.CellSize), 0, Hash.Width - 1);
            int cz = math.clamp((int)(p.z / Hash.CellSize), 0, Hash.Length - 1);
            float3 sum = float3.zero;
            float diameter = Radius * 2f;
            short garrison = TrenchId[i];
            int cells = 2;   // the hash cell is 1 m; the garrison and surface spacings both reach past the next cell
            bool onSurface = garrison < 0 && (f & (uint)UnitFlags.InTrench) == 0;
            bool walking = OnTheWay(i);
            for (int dz = -cells; dz <= cells; dz++)
            for (int dx = -cells; dx <= cells; dx++)
            {
                int x = cx + dx, z = cz + dz;
                if (x < 0 || z < 0 || x >= Hash.Width || z >= Hash.Length) continue;
                if (Hash.Map.TryGetFirstValue(Hash.KeyXZ(x, z), out int j, out var it))
                {
                    do
                    {
                        if (j == i || (Flags[j] & (uint)UnitFlags.Vehicle) != 0) continue;
                        // a garrison and a man on the surface are on either side of the trench wall: no push through it,
                        // or the men lining the wall hold an arrival off the ladder for good
                        if ((garrison >= 0 && (Flags[j] & (uint)UnitFlags.InTrench) == 0) || (TrenchId[j] >= 0 && (f & (uint)UnitFlags.InTrench) == 0)) continue;
                        float3 d = p - Position[j];
                        d.y = 0f;
                        float dist = SimMath.Length(d);
                        bool mates = garrison >= 0 && TrenchId[j] == garrison;
                        bool open = onSurface && TrenchId[j] < 0 && (Flags[j] & (uint)UnitFlags.InTrench) == 0;
                        bool down = open && (StanceOf[i] == (byte)Stance.Prone || StanceOf[i] == (byte)Stance.Pinned || StanceOf[j] == (byte)Stance.Prone || StanceOf[j] == (byte)Stance.Pinned);
                        bool passing = down || (mates && (walking || OnTheWay(j)));   // one of them squeezing past the other: to his post, or past a man lying in the open
                        float want = passing ? diameter : mates ? GarrisonSpacing : open ? SurfaceSpacing : diameter;
                        if (dist < want)
                        {
                            float3 n = dist > 1e-4f ? d / dist : CoincidentNormal(i, j);
                            // two men on open ground: the soft spacing out to SurfaceSpacing, and inside a body's width the
                            // hard push for the overlap on top of it, so the push has no step at a metre (it went from 0.33
                            // to 2.4 m/s there, and a man running up on another at a wire belt was thrown back and came
                            // on again, tick by tick)
                            if (open && !passing)
                                sum += n * (dist >= diameter ? (want - dist) * SurfaceStrength : (want - diameter) * SurfaceStrength + (diameter - dist) * Strength);
                            else
                            {
                                float strength = passing ? GarrisonStrength : dist >= diameter ? (mates ? GarrisonStrength : SurfaceStrength) : Strength;
                                sum += n * (want - dist) * strength;
                            }
                        }
                    } while (Hash.Map.TryGetNextValue(out j, ref it));
                }
            }
            // vehicles: few of them, so every unit checks the whole list rather than widening the bucket search
            float reach = Radius + VehicleRadius;
            for (int k = 0; k < Vehicles.Length; k++)
            {
                int j = Vehicles[k];
                float3 d = p - Position[j];
                d.y = 0f;
                float dist = SimMath.Length(d);
                if (dist < reach)
                {
                    float3 n = dist > 1e-4f ? d / dist : new float3(((i & 1) == 0) ? 1f : -1f, 0f, 0f);
                    sum += n * (reach - dist) * Strength;
                }
            }
            float len = SimMath.Length(sum);
            Push[i] = len > MaxPush ? sum * (MaxPush / len) : sum;
        }
    }
}
