// Phase: A2 (stub) — depends on: SpatialHash (A1), HeightfieldRaycast, GasSmokeField (smoke attenuation), WeaponStats
// Staggered scan: one third of the slots per tick (slot % 3 == tick % 3). Picks the nearest visible enemy in
// range; snipers weight MG > Officer > Sentry > crew (SpecialAbilities). Writes SimWorld.TargetSlot.
namespace TW.Sim.Combat
{
    public sealed class TargetAcquisitionSystem : ISimSystem
    {
        public int Order => SimSystemOrder.TargetAcquisition;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A2: TargetAcquisitionSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
