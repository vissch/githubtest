// Phase: A5b (implemented) — depends on: ArmorProfile, SimMath
// Which plate a round meets and whether it goes through. The facing comes from where the round comes from relative to
// the hull (or turret) heading: within 45 degrees of the nose it is the front plate, within 45 degrees of the tail the
// rear, otherwise a side. A round holes the plate when pen × cos(incidence) ≥ plate; beyond 70 degrees off the plate's
// normal it glances off whatever its penetration. Rounds that come down on the vehicle (shells, grenades on the deck)
// meet the top plate head on.
using Unity.Burst;
using Unity.Mathematics;

namespace TW.Sim.Combat
{
    public enum ArmourFacing : byte { Front = 0, Side = 1, Rear = 2, Top = 3 }

    [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
    public static class Armor
    {
        public const float GlanceCos = 0.342f;   // cos 70°
        const float Diagonal = 0.7071068f;       // cos 45°

        /// <summary>The plate a round travelling along <paramref name="shotDir"/> (XZ) strikes on a hull facing
        /// <paramref name="yaw"/>, the cosine of its incidence on that plate, and whether it struck the right-hand side.</summary>
        public static ArmourFacing FacingOf(float3 shotDir, float yaw, out float cosIncidence, out bool rightSide)
        {
            float len = SimMath.Sqrt(shotDir.x * shotDir.x + shotDir.z * shotDir.z);
            float2 incoming = len > 1e-5f ? new float2(-shotDir.x, -shotDir.z) / len : new float2(0f, 1f);
            float sy = SimMath.Sin(yaw), cy = SimMath.Cos(yaw);
            float front = incoming.x * sy + incoming.y * cy;     // toward the nose
            float right = incoming.x * cy - incoming.y * sy;     // toward the right-hand side (+X at yaw 0)
            rightSide = right > 0f;
            if (front >= Diagonal) { cosIncidence = front; return ArmourFacing.Front; }
            if (front <= -Diagonal) { cosIncidence = -front; return ArmourFacing.Rear; }
            cosIncidence = math.abs(right);
            return ArmourFacing.Side;
        }

        public static float PlateFor(in ArmorProfile armor, ArmourFacing facing)
            => facing == ArmourFacing.Front ? armor.FrontMm : facing == ArmourFacing.Side ? armor.SideMm : facing == ArmourFacing.Rear ? armor.RearMm : armor.TopMm;

        /// <summary>pen × cos(incidence) against the struck plate; a glancing round (over 70°) never goes through.</summary>
        public static bool Penetrates(in ArmorProfile armor, float penMm, float3 shotDir, float targetYaw, out float plateMm)
        {
            var facing = FacingOf(shotDir, targetYaw, out float c, out _);
            plateMm = PlateFor(armor, facing);
            return c >= GlanceCos && penMm * c >= plateMm;
        }

        /// <summary>A round or charge that comes down on the deck: the top plate, met square.</summary>
        public static bool PenetratesTop(in ArmorProfile armor, float penMm, out float plateMm)
        {
            plateMm = armor.TopMm;
            return penMm >= plateMm;
        }
    }
}
