// Phase: A2 (stub) — depends on: SimWorld.Suppression, Stance, Officer aura (A5), smoke field (A5)
// Meter 0..100, decays 8/s. > 60 forces Prone; > 85 Pinned (refuses Advance, auto-Fallback if a friendly trench
// is within 20 m). MG hits ×3 gain; officer aura and smoke ×0.5. Emits Suppressed / Pinned events on threshold crossings.
namespace TW.Sim.Combat
{
    public static class SuppressionRules
    {
        public const float DecayPerSecond = 8f;
        public const float ProneThreshold = 60f;
        public const float PinnedThreshold = 85f;
        public const float FallbackSearchRadius = 20f;
        public const float NearMissRadius = 1.5f;
    }

    public sealed class SuppressionSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Suppression;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A2: SuppressionSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
