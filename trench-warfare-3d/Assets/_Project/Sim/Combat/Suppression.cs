// Phase: A2 (implemented: decay; gain is applied by DirectFireSystem, the stance consequences by MovementSystem)
// — depends on: SimWorld.Suppression. Officer aura and smoke halving are A5.
// Meter 0..100, decays 8/s. > 60 forces Prone; > 85 Pinned (refuses Advance, auto-Fallback if a friendly trench
// is within 20 m). MG hits ×3 gain; officer aura and smoke ×0.5. Emits Suppressed / Pinned events on threshold crossings.
using Unity.Jobs;

namespace TW.Sim.Combat
{
    public static class SuppressionRules
    {
        public const float DecayPerSecond = 8f;
        public const float ProneThreshold = StanceRules.ProneSuppression;
        public const float PinnedThreshold = StanceRules.PinnedSuppression;
        public const float FallbackSearchRadius = 20f;
        public const float NearMissRadius = 1.5f;
    }

    public sealed class SuppressionSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Suppression;
        public void Initialize(SimWorld world) { }

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            if (n == 0) return;
            new DecayJob { Suppression = w.Suppression, Flags = w.Flags, Amount = SuppressionRules.DecayPerSecond * w.Config.TickSeconds }.Schedule(n, 256).Complete();
        }

        [Unity.Burst.BurstCompile(CompileSynchronously = true, FloatMode = Unity.Burst.FloatMode.Strict, FloatPrecision = Unity.Burst.FloatPrecision.Standard)]
        struct DecayJob : Unity.Jobs.IJobParallelFor
        {
            public Unity.Collections.NativeArray<float> Suppression;
            [Unity.Collections.ReadOnly] public Unity.Collections.NativeArray<uint> Flags;
            public float Amount;
            public void Execute(int i)
            {
                if ((Flags[i] & (uint)UnitFlags.Alive) == 0) { Suppression[i] = 0f; return; }
                Suppression[i] = Unity.Mathematics.math.max(0f, Suppression[i] - Amount);
            }
        }

        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
