// Phase: A3 (stub) — depends on: TrenchGarrison, FlowFieldManager (A1: NextObjective / FallbackTrench goals)
// Consumes TrenchAdvance / TrenchSelectAdvance / TrenchLock / TrenchFallback / TrenchHoldFire from
// SimWorld.TickCommands. Advance: every garrisoned unit (or masked classes) gets GoalId = NextObjective,
// Sprint, and leaves via the nearest Link cell. Fallback: units in the open whose source trench matches get
// GoalId = FallbackTrench(id) with a 3 s suppression-gain grace. Lock: arrivals do not garrison.
using Unity.Collections;

namespace TW.Sim.Units
{
    public struct TrenchState { public byte OwnerTeam; public bool Locked; public bool HoldFire; public int GarrisonCount; }

    public sealed class TrenchOrdersSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Command + 10;
        public NativeArray<TrenchState> Trenches;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A3: TrenchOrdersSystem.Step");
        public ulong Hash(ulong h) => Trenches.IsCreated ? SimHash.Array(Trenches, h) : h;
        public void Dispose() { if (Trenches.IsCreated) Trenches.Dispose(); }
    }
}
