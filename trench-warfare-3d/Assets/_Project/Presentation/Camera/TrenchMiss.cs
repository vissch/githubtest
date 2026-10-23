// Phase: fire-trench-miss-strikes — a round that misses a man InTrench / on the FireStep lands on the parapet lip
// between him and the shooter, not in the ring round his feet (the ring reads right only in the open: GreyboxMapGenerator
// carves the trench 1.8 m into the height field, FireStepRise 0.7 m up from its floor, so the lip is a step the ground
// already holds). Plain arithmetic + a ground delegate, no scene, so TrenchMissTests can drive it without a GPU.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public static class TrenchMiss
    {
        /// <summary>How far toward the shooter the lip search marches (m), the rise above the target's own floor that
        /// counts as the parapet (m), the march's step (m), how far past the first rise the lip sits (toward the
        /// shooter, m), and how wide a run of misses spreads along the lip (m).</summary>
        public const float LipReach = 4f, LipRise = 0.6f, LipStep = 0.25f, LipBack = 0.15f, AlongLip = 0.35f;

        /// <summary>
        /// The parapet lip between target and shooter: marching from the target toward the shooter, the FIRST ground
        /// sample that stands LipRise or more above the target's own floor (not the highest — past the carve edge the
        /// surface is flat and every sample ties), moved LipBack further on. False when nothing rises within LipReach
        /// (flat ground: the ring stays; or a shooter in the same trench, where the sim already gives cover 0).
        /// </summary>
        public static bool Lip(Vector3 target, Vector3 shooter, System.Func<float, float, float> ground, out Vector3 lip)
        {
            lip = target;
            Vector3 flat = new Vector3(shooter.x - target.x, 0f, shooter.z - target.z);
            float dist = flat.magnitude;
            if (dist < 1e-4f) return false;
            Vector3 dir = flat / dist;
            float floor = ground(target.x, target.z);
            for (float d = LipStep; d <= LipReach; d += LipStep)
            {
                float x = target.x + dir.x * d, z = target.z + dir.z * d;
                float y = ground(x, z);
                if (y >= floor + LipRise)
                {
                    float bx = x + dir.x * LipBack, bz = z + dir.z * LipBack;
                    lip = new Vector3(bx, ground(bx, bz), bz);
                    return true;
                }
            }
            return false;
        }

        /// <summary>Spreads a miss along the lip rather than piling every strike on one point: along in -1..1 moves
        /// AlongLip across the shot (perpendicular to shooter-target), ahead the same distance further toward/away
        /// from the shooter, so the point never walks more than AlongLip + the ahead given off the lip itself.</summary>
        public static Vector3 Strike(Vector3 lip, Vector3 toShooter, float along, float ahead)
        {
            Vector3 flat = new Vector3(toShooter.x, 0f, toShooter.z);
            if (flat.sqrMagnitude < 1e-4f) flat = Vector3.forward; else flat.Normalize();
            Vector3 side = new Vector3(flat.z, 0f, -flat.x);
            return lip + side * (along * AlongLip) + flat * ahead;
        }
    }
}
