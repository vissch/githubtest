// Phase: A2 (stub) — depends on: TargetAcquisition, CoverVolume (directional arc test), Stance hit-box heights,
// SimRandom.SystemId.DirectFire, Suppression (near-miss), Armor (A5)
// hit = accuracy × rangeFalloff × stanceMod × (1 − coverBonus if attacker bearing inside the arc). A miss within
// 1.5 m of the target cell adds NearMiss suppression to every unit in that cell (spatial hash query).
namespace TW.Sim.Combat
{
    public sealed class DirectFireSystem : ISimSystem
    {
        public int Order => SimSystemOrder.DirectFire;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A2: DirectFireSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
