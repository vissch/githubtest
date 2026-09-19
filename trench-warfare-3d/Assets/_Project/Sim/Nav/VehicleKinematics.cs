// Phase: A1 (stub) — depends on: MapData, NavLayer, UnitFlags.Vehicle
// Vehicles do not use separation. They follow the flow field with a turn-radius limit, may only enter trench
// cells narrower than their crossing width (Mark IV/A7V 3.5 m; cars never), crush Wire cells, and roll a
// deterministic bog check on Mud cells (SimRandom.SystemId.Bog).
using Unity.Mathematics;

namespace TW.Sim.Nav
{
    public struct VehicleProfile
    {
        public float TurnRateRad;      // rad/s
        public float TrenchCrossWidth; // metres; 0 = cannot cross
        public float SlopeLimitRad;    // max climbable slope
        public float BogChance;        // per-tick base probability on Mud
        public bool Wheeled;           // road-only effective speed
    }

    public sealed class VehicleKinematicsSystem : ISimSystem
    {
        public int Order => SimSystemOrder.VehicleKinematics;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A1: VehicleKinematicsSystem.Step");
        public ulong Hash(ulong h) => h;
        public void Dispose() { }
    }
}
