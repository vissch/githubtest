// Phase: A2 (implemented: decay; gain is applied by DirectFireSystem, the stance consequences by MovementSystem)
// — depends on: SimWorld.Suppression. The officer's aura (AuraSystem, 2026-09-25) halves the gain in DirectFire and,
// here after the decay, keeps a covered man below Pinned; a man whose spec is NeverPinned (2026-09-28) is kept there by
// the decay itself. Smoke halving is A5.
// Meter 0..100, decays 8/s. > 60 forces Prone; > 85 Pinned (refuses Advance, auto-Fallback if a friendly trench
// is within 20 m). MG hits ×3 gain; officer aura and smoke ×0.5. Emits Suppressed / Pinned events on threshold crossings.
// SimWorld.Alarm (2026-10-01): after the decay it is raised to the man's suppression (held to StanceRules.AlarmCap)
// and otherwise comes down StanceRules.AlarmDecay a second, so a man stays careful after the fire stops.
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
        AuraSystem aura;
        public void Initialize(SimWorld world) { }

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            if (n == 0) return;
            new DecayJob
            {
                Suppression = w.Suppression, Flags = w.Flags, Archetype = w.Archetype, Specs = w.Units.Infantry,
                Amount = SuppressionRules.DecayPerSecond * w.Config.TickSeconds,
                Alarm = w.Alarm, AlarmAmount = StanceRules.AlarmDecay * w.Config.TickSeconds,
            }.Schedule(n, 256).Complete();
            if (aura == null) aura = w.GetSystem<AuraSystem>();
            aura?.Unpin(w);
        }

        [Unity.Burst.BurstCompile(CompileSynchronously = true, FloatMode = Unity.Burst.FloatMode.Strict, FloatPrecision = Unity.Burst.FloatPrecision.Standard)]
        struct DecayJob : Unity.Jobs.IJobParallelFor
        {
            public Unity.Collections.NativeArray<float> Suppression;
            public Unity.Collections.NativeArray<float> Alarm;
            public float AlarmAmount;
            [Unity.Collections.ReadOnly] public Unity.Collections.NativeArray<uint> Flags;
            [Unity.Collections.ReadOnly] public Unity.Collections.NativeArray<byte> Archetype;
            [Unity.Collections.ReadOnly] public Unity.Collections.NativeArray<InfantrySpec> Specs;   // the match table (SimWorld.Units)
            public float Amount;
            public void Execute(int i)
            {
                if ((Flags[i] & (uint)UnitFlags.Alive) == 0) { Suppression[i] = 0f; Alarm[i] = 0f; return; }
                float s = Unity.Mathematics.math.max(0f, Suppression[i] - Amount);
                // InfantrySpec.NeverPinned (2026-09-28, the Death Battalion): held one below Pinned, where the officer's
                // aura holds the men round him. Movement, the garrison and the next tick's fire read it after this.
                if (s > AuraSystem.UnpinTo && Specs[Archetype[i]].NeverPinned) s = AuraSystem.UnpinTo;
                Suppression[i] = s;
                Alarm[i] = Unity.Mathematics.math.max(Alarm[i] - AlarmAmount, Unity.Mathematics.math.min(s, StanceRules.AlarmCap));
            }
        }

        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
