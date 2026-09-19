// Phase: A5 (stub) — depends on: IndirectFire (arc), Blast, TrenchGarrison (target cells)
// Assault / Shield grenadier / Raider throw automatically at ≤ 20 m onto Trench, Crater or bunker-rear cells
// (6 s cooldown). Incendiary variants mark cells Burning. Grenades ignore cover (open top).
namespace TW.Sim.Units
{
    public sealed class GrenadeSystem : ISimSystem
    {
        public int Order => SimSystemOrder.IndirectFire - 1;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A5: GrenadeSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
