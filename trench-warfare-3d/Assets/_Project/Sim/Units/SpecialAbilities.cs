// Phase: A5 (the ids; the systems came one by one) — unit-owned abilities from docs/07-abilities.md.
// Most of these ids are still names for planned work: the officer's aura is AuraSystem (650), the shield bearer's plate
// is InfantrySpec.ShieldPlateMm, the flamethrower's cone is a WeaponStats with SetsBurning, and the sapper's LayMine /
// LayTripwire (2026-09-28) are the first ids a UnitAbility command carries (SapperSystem, order SimSystemOrder.Sapper).
// The stub SpecialAbilitiesSystem that used to sit here (order 120, never registered, Step threw) is gone: that order is
// the sapper's.
namespace TW.Sim.Units
{
    public enum UnitAbilityId : short
    {
        None = 0, OfficerAura = 1, OfficerSmokeCall = 2, Bangalore = 3, WireCutters = 4, FrontalPlate = 5,
        FlamethrowerBurst = 6, FieldGunSelect = 7, MortarAuto = 8, Dismount = 9, Fascine = 10, TankCrush = 11, MobileCover = 12,
        // 2026-09-28 (replay v16): a sapper's orders. SimCommand.B carries the id in its low byte and AbilityArgs above it.
        LayMine = 13, LayTripwire = 14,
    }
}
