// Phase: A1 (stub) — depends on: FlowField, MapData, ObjectiveDef, TrenchDef
// Owns one FlowField per (team, goal group): NextObjective, FallbackTrench(trenchId), Rally.
// Recomputes only fields whose goal or touched cost cells changed, time-sliced across ticks with double
// buffering so a rebuild never stalls a tick. Cap: 8 goal groups per team (docs/05-performance-budgets.md).
using Unity.Collections;

namespace TW.Sim.Nav
{
    public enum GoalKind : byte { NextObjective = 0, FallbackTrench = 1, Rally = 2, Custom = 3 }

    public struct GoalKey { public byte Team; public GoalKind Kind; public short Ref; }

    public sealed class FlowFieldManager
    {
        public const int MaxGoalsPerTeam = 8;

        public int GoalId(GoalKey key) => throw new System.NotImplementedException("Phase A1: FlowFieldManager.GoalId");
        public FlowField Field(int goalId) => throw new System.NotImplementedException("Phase A1: FlowFieldManager.Field");
        public void MarkCostDirty(int navCell) => throw new System.NotImplementedException("Phase A1: FlowFieldManager.MarkCostDirty");
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A1: FlowFieldManager.Step (time-sliced rebuilds)");
    }
}
