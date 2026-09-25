// Phase: A3 (implemented 2026-09-25) — the officer's aura. Owner's brief: "an officer unit that inspires troops
// around him to fight 20% harder".
// Every tick, before DirectFire, one job in slot order writes three TRANSIENT per-slot arrays: DamageMul (what a
// man's rounds do), SuppressionMul (what fire does to his nerve) and Covered (is an officer near him). Both
// multipliers are 1 for everyone, then every living, unpinned officer sweeps his own side's infantry within
// InfantrySpec.AuraRadius: max for damage and min for suppression, so two officers do not stack. DirectFire reads
// the multipliers in the same tick; SuppressionSystem keeps a covered man below Pinned (AuraUnpin), which is the
// part a player can see — the men round the officer stand up and shoot. The arrays are fully rewritten every tick
// and never read across ticks, so they are not hashed (docs/03: transient, documented here).
// HeroSystem (805) folds its own hashed per-slot scale into DamageMul through HeroScale, so DirectFire has ONE
// multiplier to read whatever writes it. Until a HeroSystem hands one over, HeroScale is this system's own array
// of ones (a job may not be given the same container twice, so it is never aliased onto DamageMul).
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;

namespace TW.Sim.Combat
{
    public sealed class AuraSystem : ISimSystem, IAuraProvider
    {
        public int Order => SimSystemOrder.Aura;

        /// <summary>Per slot, transient: rewritten every tick before DirectFire reads it.</summary>
        public NativeArray<float> DamageMul, SuppressionMul;
        public NativeArray<byte> Covered;
        /// <summary>Per slot: a hashed scale folded into DamageMul each tick. HeroSystem hands its own array over
        /// (SetHeroScale); until then it is this system's array of ones.</summary>
        public NativeArray<float> HeroScale => heroScale;
        NativeArray<float> heroScale, ones;
        /// <summary>A covered man's suppression never stays above this: one below Pinned, so he keeps shooting.</summary>
        public const float UnpinTo = SuppressionRules.PinnedThreshold - 1f;

        public void Initialize(SimWorld world)
        {
            int n = world.Config.MaxSlots;
            DamageMul = new NativeArray<float>(n, Allocator.Persistent);
            SuppressionMul = new NativeArray<float>(n, Allocator.Persistent);
            Covered = new NativeArray<byte>(n, Allocator.Persistent);
            ones = new NativeArray<float>(n, Allocator.Persistent);
            for (int i = 0; i < n; i++) { DamageMul[i] = 1f; SuppressionMul[i] = 1f; ones[i] = 1f; }
            heroScale = ones;
        }

        /// <summary>HeroSystem's hashed per-slot scale (1 for most men, more for a hero or a veteran). The caller owns it.</summary>
        public void SetHeroScale(NativeArray<float> scale) => heroScale = scale.IsCreated ? scale : ones;

        bool IAuraProvider.Covered(int slot) => slot >= 0 && slot < Covered.Length && Covered[slot] != 0;

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            if (n == 0) return;
            new SweepJob
            {
                Count = n, Position = w.Position, Flags = w.Flags, Team = w.Team, Archetype = w.Archetype, Suppression = w.Suppression,
                HeroScale = heroScale, DamageMul = DamageMul, SuppressionMul = SuppressionMul, Covered = Covered,
            }.Run();
        }

        /// <summary>SuppressionSystem calls this after the decay: a man under an officer does not stay pinned.</summary>
        public void Unpin(SimWorld w)
        {
            int n = math.min(w.HighWater, Covered.Length);
            for (int i = 0; i < n; i++)
                if (Covered[i] != 0 && w.Suppression[i] > UnpinTo) w.Suppression[i] = UnpinTo;
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct SweepJob : IJob
        {
            public int Count;
            [ReadOnly] public NativeArray<float3> Position;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<byte> Team, Archetype;
            [ReadOnly] public NativeArray<float> Suppression;
            [ReadOnly] public NativeArray<float> HeroScale;
            public NativeArray<float> DamageMul, SuppressionMul;
            public NativeArray<byte> Covered;

            public void Execute()
            {
                for (int i = 0; i < Count; i++) { DamageMul[i] = 1f; SuppressionMul[i] = 1f; Covered[i] = 0; }
                for (int i = 0; i < Count; i++)
                {
                    uint f = Flags[i];
                    if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) != 0) continue;
                    var spec = InfantrySpec.For(Archetype[i]);
                    if (spec.AuraRadius <= 0f || Suppression[i] >= SuppressionRules.PinnedThreshold) continue;
                    float r2 = spec.AuraRadius * spec.AuraRadius;
                    byte team = Team[i]; float3 at = Position[i];
                    for (int j = 0; j < Count; j++)
                    {
                        uint fj = Flags[j];
                        if ((fj & (uint)UnitFlags.Alive) == 0 || (fj & (uint)UnitFlags.Vehicle) != 0 || Team[j] != team) continue;
                        float3 d = Position[j] - at; d.y = 0f;
                        if (math.lengthsq(d) > r2) continue;
                        DamageMul[j] = math.max(DamageMul[j], spec.AuraDamageMul);
                        SuppressionMul[j] = math.min(SuppressionMul[j], spec.AuraSuppressionMul);
                        if (spec.AuraUnpin) Covered[j] = 1;
                    }
                }
                for (int i = 0; i < Count; i++) DamageMul[i] *= HeroScale[i];
            }
        }

        public ulong Hash(ulong h) => h;   // transient: rewritten from hashed state every tick before anything reads it

        public void Dispose()
        {
            if (DamageMul.IsCreated) DamageMul.Dispose();
            if (SuppressionMul.IsCreated) SuppressionMul.Dispose();
            if (Covered.IsCreated) Covered.Dispose();
            if (ones.IsCreated) ones.Dispose();
        }
    }
}
