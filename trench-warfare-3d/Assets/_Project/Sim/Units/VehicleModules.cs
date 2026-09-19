// Phase: A5 (stub) — depends on: Armor, VehicleKinematics (A1)
// Tracks per-vehicle module state (track, engine, crew, gun), immobilise/stall timers, repair when unengaged
// for 10 s, VehicleTrackHit / VehicleStalled / VehicleDestroyed events, bog state from Mud checks.
using Unity.Collections;

namespace TW.Sim.Units
{
    public struct VehicleState { public int Slot; public int TrackTimer; public int StallTimer; public int BogTimer; public int LastHitTick; public byte CrewLost; }

    public sealed class VehicleModulesSystem : ISimSystem
    {
        public int Order => SimSystemOrder.VehicleKinematics + 1;
        public NativeList<VehicleState> Vehicles;
        public void Initialize(SimWorld world) { }
        public void Step(SimWorld world) => throw new System.NotImplementedException("Phase A5: VehicleModulesSystem.Step");
        public ulong Hash(ulong h) => Vehicles.IsCreated ? SimHash.Array(Vehicles.AsArray(), h) : h;
        public void Dispose() { if (Vehicles.IsCreated) Vehicles.Dispose(); }
    }
}
