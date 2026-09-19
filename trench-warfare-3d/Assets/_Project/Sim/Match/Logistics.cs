// Phase: A3 (stub) — depends on: MapData.SupplyRoad, SpawnPoint.Kind, SimWorld.Rally
// Replaces instant spawning: deployed units appear at the map edge and walk the supply road (or emerge from the
// communication trench, slower but safe) to the player's rally point, then take the NextObjective goal.
namespace TW.Sim.Match
{
    public sealed class LogisticsSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Economy + 10;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A3: LogisticsSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
