// Phase: A5 (stub) — depends on: ArmorProfile, WeaponStats.PenetrationMm, SimRandom.SystemId.Armor
// pen × cos(incidence) vs plate for the struck facing; fail → Hit event with negative damage (ricochet);
// success → damage + module roll (track 20 %, engine 10 %, crew 5 %). Track → Immobilised; engine → Stalled,
// repairable after 10 s without incoming fire.
using Unity.Mathematics;

namespace TW.Sim.Combat
{
    public static class Armor
    {
        public static bool Penetrates(in ArmorProfile armor, float penMm, float3 shotDir, float targetYaw, out float plateMm)
            => throw new System.NotImplementedException("Phase A5: Armor.Penetrates");
    }
}
