// Phase: A6 (stub) — depends on: MissionScript wave tables, OffMapAbilitySystem, TrenchOrders, SpatialHash heat query
// Deploys from a budget with weighted composition, garrisons until strength ≥ X or a timer, then TrenchAdvance;
// fires off-map abilities on the densest player cell; counterattacks a lost objective after T seconds.
// Issues commands through SimWorld.TickCommands exactly like a human peer (the AI is player index 1 in lockstep).
namespace TW.Sim.Match
{
    public struct Difficulty
    {
        public float BudgetScale, WaveIntervalScale, AbilityCooldownScale;
        public static Difficulty Normal => new Difficulty { BudgetScale = 1f, WaveIntervalScale = 1f, AbilityCooldownScale = 1f };
        public static Difficulty Hard => new Difficulty { BudgetScale = 1.4f, WaveIntervalScale = 0.75f, AbilityCooldownScale = 0.7f };
        public static Difficulty Finale => new Difficulty { BudgetScale = 1.8f, WaveIntervalScale = 0.6f, AbilityCooldownScale = 0.6f };
    }

    public sealed class WaveAiSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Mission + 10;
        public Difficulty Difficulty = Difficulty.Normal;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A6: WaveAiSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
