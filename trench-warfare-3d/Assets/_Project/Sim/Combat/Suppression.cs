// Phase: A2 — this file only takes suppression away again. SuppressionSystem decays the meter
// SuppressionRules.DecayPerSecond (8/s) towards 0, holds a man whose InfantrySpec.NeverPinned is set one point below
// Pinned (AuraSystem.UnpinTo), raises SimWorld.Alarm (2026-10-01) to the man's suppression capped at
// StanceRules.AlarmCap and otherwise brings it down StanceRules.AlarmDecay a second, then calls AuraSystem.Unpin so an
// officer keeps the men round him off the floor. A dead slot's suppression and alarm are zeroed.
// Everything else lives elsewhere: the gain per shot and the Suppressed / Pinned events are DirectFire.cs:320-330
// (AddSuppression, scaled by AuraSystem.SuppressionMul, and no gain at all for an Airborne or a UnitFlags.Vehicle
// slot: small arms do not shake a crew, decisions.md 2026-10-18); the thresholds are StanceRules.ProneSuppression /
// PinnedSuppression, mirrored here as ProneThreshold / PinnedThreshold; what being Prone or Pinned costs a man is read
// in Engage.cs:174,199 (pinned stops engaging, prone holds where he stands) and MovementSystem.cs:310-322 (the stance
// itself). FallbackSearchRadius (auto-Fallback) has no reader today, and the smoke halving is not written yet (A5);
// NearMissRadius is read by DirectFire.cs:468.
// — depends on: SimWorld.Suppression, SimWorld.Alarm.
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
