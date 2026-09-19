// Phase: A5 (stub) — unit-owned abilities from docs/07-abilities.md
// Officer aura (+15 % dmg, suppression ×0.5, un-pin, r 15 m); officer smoke marker → off-map mortar salvo;
// Bangalore (3 s place, 6 m breach); wire cutters (8 s uncontested, 2 m); sentry/shield frontal plate;
// flamethrower cone (Burning, bunker embrasure bypass, explodes on death); field gun AP/HE selection;
// mortar auto-fire on densest enemy cell; cavalry dismount; fascine drop (M3) turning ditch cells into Link.
namespace TW.Sim.Units
{
    public enum UnitAbilityId : short
    {
        None = 0, OfficerAura = 1, OfficerSmokeCall = 2, Bangalore = 3, WireCutters = 4, FrontalPlate = 5,
        FlamethrowerBurst = 6, FieldGunSelect = 7, MortarAuto = 8, Dismount = 9, Fascine = 10, TankCrush = 11, MobileCover = 12,
    }

    public sealed class SpecialAbilitiesSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Command + 20;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A5: SpecialAbilitiesSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
