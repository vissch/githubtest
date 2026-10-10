// Phase: deaths (2026-09-28, implemented) — part of AnimationController: the absurd death on top of the one the ladder
// chose (DeathGags). Die calls Gag once the ladder has its clip, cause and throw; Gag hands DeathGags.Choose what the
// latched events said (who killed him and with what, a claw or a track, how hard the hit, a trench under him, the
// heap round him) and writes back the throw, the clip and the yaw it changed. The plan rides on in the DeathRecord to
// CombatFx and VATRenderer. At fx.deathAbsurd 0 it returns no gag and changes nothing.
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Terrain;

namespace TW.Presentation
{
    public sealed partial class AnimationController
    {
        GagPlan Gag(int i, ref AnimState s, ref Clip clip, DeathKind cause, int b, float3 simDir, bool flat, bool crushed, int density, SimWorld w)
        {
            float a = DeathGags.Intensity;
            if (a <= 0f) return default;
            bool killer = b >= 0 && b < w.HighWater;
            bool vehicle = killer && (w.Flags[b] & (uint)UnitFlags.Vehicle) != 0;
            var input = new GagInput
            {
                Cause = cause, Clip = clip, Flat = flat, InTrench = s.PrevLayer == (byte)NavLayer.Trench,
                Crushed = crushed && clawed[i] == 0, Clawed = crushed && clawed[i] != 0,
                Heavy = hitKind[i] == 2,
                // a machine gun: the infantry's, or a machine's small arms (its tracks and claws are the two above)
                MachineGun = killer && !crushed && (vehicle || w.Archetype[b] == InfantryArchetype.Machinegunner),
                Density = density, Travel = new float3(simDir.x, 0f, simDir.z),
                Throw = new float3(s.ThrowX, s.ThrowUp, s.ThrowZ),
                BodyYaw = s.BodyYaw, KillerYaw = killer ? w.Yaw[b] : s.BodyYaw,
                // his own kind, from his latched state: w.Archetype[i] may already be a newcomer's (Tick, a same-tick refill)
                Archetype = s.Archetype,
                Seed = s.Seed, Tick = tick,
            };
            float3 fly = input.Throw; float yaw = s.ShownYaw;
            var plan = DeathGags.Choose(input, a, ref fly, ref clip, ref yaw);
            if (!plan.Any) return plan;
            s.ThrowX = fly.x; s.ThrowUp = fly.y; s.ThrowZ = fly.z;
            s.ShownYaw = s.BodyYaw = yaw;
            return plan;
        }
    }
}
