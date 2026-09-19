// Phase: A5 (stub) — depends on: WeaponStats.MuzzleVelocity, Heightfield, Blast
// Solves the launch angle for range with fixed v0 (high arc for mortars, low for howitzers), schedules the
// impact tick, and hands the terminal position to BlastSystem. Trench cells take full blast (open top);
// Bunker cells take none.
namespace TW.Sim.Combat
{
    public struct ScheduledImpact { public uint Tick; public Unity.Mathematics.float3 Pos; public short WeaponId; public int Shooter; public byte Team; }

    public sealed class IndirectFireSystem : ISimSystem
    {
        public int Order => SimSystemOrder.IndirectFire;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A5: IndirectFireSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
