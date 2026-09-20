// Phase: P0 (implemented composition; systems are registered as their phases land)
// Composes MapData + SimWorld + systems for one match. Presentation, Net and tests never build a SimWorld
// directly for a real match; they go through MatchSim so system order is identical everywhere.
using Unity.Collections;
using TW.Sim.Nav;
using TW.Sim.Terrain;
using TW.Sim.Units;

namespace TW.Sim.Match
{
    public sealed class MatchSim : System.IDisposable
    {
        public MapData Map;
        public SimWorld World;
        public FlowFieldManager Fields;
        public TrenchOrdersSystem Orders;
        public MovementSystem Movement;
        public VehicleKinematicsSystem Vehicles;

        public static MatchSim CreateGreybox(SimConfig config)
        {
            var map = GreyboxMapGenerator.Create(Allocator.Persistent);
            return new MatchSim(config, map);
        }

        public MatchSim(SimConfig config, MapData map)
        {
            Map = map;
            World = new SimWorld(config, map.ToWorldInit());
            // ---- system registration (docs/04-architecture.md, "Sim system order"). Step order follows ISimSystem.Order;
            // Initialize order follows AddSystem order, so providers are added before the systems that resolve them. ----
            Fields = new FlowFieldManager(map);
            World.AddSystem(Fields);                        // A1: goals, fields, trench state
            Orders = new TrenchOrdersSystem();
            World.AddSystem(Orders);                        // A1 initial / A3: >> ↑ lock ↩ hold-fire
            // A6: World.AddSystem(new MissionRunner(script));
            // A3: World.AddSystem(new LogisticsSystem(map));
            // A2: World.AddSystem(new TW.Sim.Combat.TargetAcquisitionSystem());
            // A2: World.AddSystem(new TW.Sim.Combat.DirectFireSystem());
            // A5: World.AddSystem(new TW.Sim.Units.GrenadeSystem());
            // A5: World.AddSystem(new TW.Sim.Combat.IndirectFireSystem());
            // A5: World.AddSystem(new TW.Sim.Combat.BlastSystem());
            // A5: World.AddSystem(new TW.Sim.Combat.BurningSystem());
            // A2: World.AddSystem(new TW.Sim.Combat.SuppressionSystem());
            // A3: World.AddSystem(new TW.Sim.Units.StanceSystem());
            // A3: World.AddSystem(new TW.Sim.Units.TrenchGarrisonSystem());
            // A5: World.AddSystem(new TW.Sim.Combat.GasSmokeSystem());
            // A4: World.AddSystem(new DeformationSystem(map));
            Movement = new MovementSystem(map);
            World.AddSystem(Movement);                      // A1: infantry
            Vehicles = new VehicleKinematicsSystem(map);
            World.AddSystem(Vehicles);                      // A1: vehicles
            // A5: World.AddSystem(new TW.Sim.Units.VehicleModulesSystem());
            // A3: World.AddSystem(new SectorControlSystem(map));
            // A5: World.AddSystem(new OffMapAbilitySystem());
            // A6: World.AddSystem(new WaveAiSystem());
        }

        public void Step(NativeArray<SimCommand> commands) => World.Step(commands);

        public void Dispose()
        {
            World?.Dispose();
            Map?.Dispose();
        }
    }
}
