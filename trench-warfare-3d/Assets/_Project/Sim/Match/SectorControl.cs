// Phase: A3 (stub) — depends on: ObjectiveDef, SpatialHash, TrenchGarrison
// Objectives flip when ≥ RequiredUnits friendly infantry hold the cells for CaptureTicks with no enemy inside,
// in OrderIndex order (Outpost → Main → Reserve → HQ). Trench traverses flip segment by segment. Capturing the
// enemy HQ ends the match (MatchEnded). Mustard gas blocks capture while concentration > 5.
using Unity.Collections;

namespace TW.Sim.Match
{
    public struct ObjectiveState { public byte Owner; public int CaptureProgressTicks; public byte CapturingTeam; }

    public sealed class SectorControlSystem : ISimSystem
    {
        public int Order => SimSystemOrder.SectorControl;
        public NativeArray<ObjectiveState> States;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A3: SectorControlSystem.Step");
        public ulong Hash(ulong h) => States.IsCreated ? SimHash.Array(States, h) : h;
        public void Dispose() { if (States.IsCreated) States.Dispose(); }
    }
}
