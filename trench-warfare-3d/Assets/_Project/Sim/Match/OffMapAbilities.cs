// Phase: A5 (stub) — off-map support abilities from docs/07-abilities.md
// Consumes SupportFire commands: validates cost/cooldown/unlock, emits AbilityFired, then schedules payloads
// (ScheduledImpact rows for shells, GasSmoke sources, bomber line, creeping barrage line advancing 10 m / 4 s,
// recon reveal). Enemy WaveAI uses the same path.
namespace TW.Sim.Match
{
    public enum AbilityTargetMode : byte { Point = 0, Line = 1, Area = 2, Heading = 3, None = 4 }

    public enum OffMapAbilityId : short
    {
        None = 0, HeBarrage = 1, CreepingBarrage = 2, ChlorineGas = 3, MustardGas = 4, BomberRun = 5,
        SmokeScreen = 6, MortarSalvo = 7, ReconFlight = 8, ReinforcementSurge = 9,
    }

    public struct AbilityStats
    {
        public OffMapAbilityId Id;
        public int Cost;
        public int CooldownTicks;
        public int WarmupTicks;
        public AbilityTargetMode Target;
        public float Radius, Length;
        public int Shells; public float ShellDamage, ShellRadius; public int SpreadTicks;
        public float Concentration; public int PersistTicks;
        public bool BreachesWire, CollapsesTrench, DestroysBunker;
    }

    public sealed class OffMapAbilitySystem : ISimSystem
    {
        public int Order => SimSystemOrder.Command + 30;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A5: OffMapAbilitySystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
