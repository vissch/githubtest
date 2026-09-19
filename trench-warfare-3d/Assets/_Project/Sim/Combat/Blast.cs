// Phase: A5 (stub) — depends on: SpatialHash, CraterStamp (A4), Suppression, Armor (top plate for vehicles)
// Radius falloff damage over hash buckets, suppression pulse, CraterStamp when the calibre warrants, light
// vehicles (< 4,000 HP) destroyed on a direct hit, WireBreached inside the radius for ≥ 105 mm.
namespace TW.Sim.Combat
{
    public sealed class BlastSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Blast;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A5: BlastSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
