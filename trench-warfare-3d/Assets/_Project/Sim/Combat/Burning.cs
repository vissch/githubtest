// Phase: A5 (stub) — incendiary grenades and flamethrowers mark nav cells Burning for N seconds (10 dmg/s, units flee).
namespace TW.Sim.Combat
{
    public sealed class BurningSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Blast + 5;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A5: BurningSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
