// Phase: P0 (implemented composition; systems are registered as their phases land)
// Composes MapData + SimWorld + systems for one match. Presentation, Net and tests never build a SimWorld
// directly for a real match; they go through MatchSim so system order is identical everywhere.
using Unity.Collections;
using TW.Sim.Combat;
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
        public TargetAcquisitionSystem Acquisition;
        public DirectFireSystem Fire;
        public SuppressionSystem Suppression;
        public SectorControlSystem Sectors;
        public BlastSystem Blast;
        public GasSmokeSystem Gas;
        public DeformationSystem Deformation;
        public OffMapAbilitySystem Abilities;

        /// <param name="combat">false leaves out target acquisition and direct fire: movement-only tests and the M1 stress run.</param>
        public static MatchSim CreateGreybox(SimConfig config, bool combat = true)
        {
            var map = GreyboxMapGenerator.Create(Allocator.Persistent);
            return new MatchSim(config, map, combat);
        }

        public MatchSim(SimConfig config, MapData map, bool combat = true)
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
            if (combat)
            {
                Acquisition = new TargetAcquisitionSystem(map);
                World.AddSystem(Acquisition);               // A2: who shoots at whom
                Fire = new DirectFireSystem(map);
                World.AddSystem(Fire);                      // A2: shots, damage, near-miss suppression, deaths
            }
            // A5: World.AddSystem(new TW.Sim.Units.GrenadeSystem());
            // A5: World.AddSystem(new TW.Sim.Combat.IndirectFireSystem());
            Blast = new BlastSystem(map);
            World.AddSystem(Blast);                         // A5 core: radius damage, suppression, craters out
            // A5: World.AddSystem(new TW.Sim.Combat.BurningSystem());
            Suppression = new SuppressionSystem();
            World.AddSystem(Suppression);                   // A2: decay (stance consequences are applied by MovementSystem)
            // A3: World.AddSystem(new TW.Sim.Units.StanceSystem());
            // A3: World.AddSystem(new TW.Sim.Units.TrenchGarrisonSystem());
            Gas = new GasSmokeSystem(map);
            World.AddSystem(Gas);                           // A5 core: chlorine field, wind, trench sink, gassed garrisons run
            Deformation = new DeformationSystem(map);
            World.AddSystem(Deformation);                   // A4 core: crater stamps
            Movement = new MovementSystem(map);
            World.AddSystem(Movement);                      // A1: infantry
            Vehicles = new VehicleKinematicsSystem(map);
            World.AddSystem(Vehicles);                      // A1: vehicles
            // A5: World.AddSystem(new TW.Sim.Units.VehicleModulesSystem());
            Sectors = new SectorControlSystem(map);
            World.AddSystem(Sectors);                       // A3 core: objectives, trench ownership, HQ = match end
            Abilities = new OffMapAbilitySystem();
            World.AddSystem(Abilities);                     // A5 core: HE barrage, chlorine gas
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
