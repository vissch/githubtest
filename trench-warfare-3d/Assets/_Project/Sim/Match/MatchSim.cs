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
        public TankGunnerySystem Gunnery;        // null without combat
        public VehicleModulesSystem Modules;
        public TargetAcquisitionSystem Acquisition;
        public DirectFireSystem Fire;
        public SuppressionSystem Suppression;
        /// <summary>Null on an inland map.</summary>
        public SeaLandingSystem Landing;
        public TW.Sim.Units.TrenchGarrisonSystem Garrison;
        public SectorControlSystem Sectors;
        public BlastSystem Blast;
        public GasSmokeSystem Gas;
        public DeformationSystem Deformation;
        public OffMapAbilitySystem Abilities;
        public AmbientBombardmentSystem Bombardment;
        public BurningSystem Burning;
        public BeamSystem Beam;
        public TerrainHashSystem TerrainHash;

        /// <param name="combat">false leaves out target acquisition and direct fire: movement-only tests and the M1 stress run.</param>
        public static MatchSim CreateGreybox(SimConfig config, bool combat = true)
        {
            var map = GreyboxMapGenerator.Create(Allocator.Persistent);
            return new MatchSim(config, map, combat);
        }

        /// <summary>The two-line playtest map (GreyboxMapGenerator.CreatePlaytest).</summary>
        public static MatchSim CreatePlaytest(SimConfig config) => new MatchSim(config, GreyboxMapGenerator.CreatePlaytest(Allocator.Persistent));

        /// <summary>A generated battlefield (BattlefieldGenerator): the same params give the same map on every machine.</summary>
        public static MatchSim CreateBattlefield(SimConfig config, BattlefieldParams battlefield, bool combat = true)
        {
            var match = new MatchSim(config, BattlefieldGenerator.Create(battlefield, Allocator.Persistent), combat);
            match.Bombardment.ShellsPerMinute = battlefield.Bombardment;
            return match;
        }

        public MatchSim(SimConfig config, MapData map, bool combat = true)
        {
            Map = map;
            World = new SimWorld(config, map.ToWorldInit());
            // ---- system registration (docs/04-architecture.md, "Sim system order"). Step order follows ISimSystem.Order;
            // Initialize order follows AddSystem order, so providers are added before the systems that resolve them. ----
            TerrainHash = new TerrainHashSystem(map);
            World.AddSystem(TerrainHash);                   // A4: steps nothing; folds the map (ground, layers, holes) into the tick hash
            Fields = new FlowFieldManager(map);
            World.AddSystem(Fields);                        // A1: goals, fields, trench state
            Orders = new TrenchOrdersSystem();
            World.AddSystem(Orders);                        // A1 initial / A3: >> ↑ lock ↩ hold-fire
            // A6: World.AddSystem(new MissionRunner(script));
            // A3: World.AddSystem(new LogisticsSystem(map));     // the land half: units walk up the supply road
            if (map.HasSea) { Landing = new SeaLandingSystem(map); World.AddSystem(Landing); }   // A3: the sea half: they come ashore off a boat
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
            Bombardment = new AmbientBombardmentSystem(map);
            World.AddSystem(Bombardment);                   // A5: nobody's shells on no man's land (off unless the match asks for it)
            Burning = new BurningSystem(map);
            World.AddSystem(Burning);                       // A5: men and ground set alight by an incendiary burst (reads Blast.Resolved)
            Beam = new BeamSystem(map);
            World.AddSystem(Beam);                          // A5 / docs/21 SIM-C: the sweeping beam (steps before Burning, registered after it)
            Suppression = new SuppressionSystem();
            World.AddSystem(Suppression);                   // A2: decay (stance consequences are applied by MovementSystem)
            // A3: World.AddSystem(new TW.Sim.Units.StanceSystem());   // stance is decided in MoveJob, which has the movement context
            Garrison = new TW.Sim.Units.TrenchGarrisonSystem(map);
            World.AddSystem(Garrison);                      // A3: a post per man inside his trench (the fire step, or back from it)
            Gas = new GasSmokeSystem(map);
            World.AddSystem(Gas);                           // A5 core: chlorine field, wind, trench sink, gassed garrisons run
            Deformation = new DeformationSystem(map);
            World.AddSystem(Deformation);                   // A4 core: crater stamps
            Movement = new MovementSystem(map);
            World.AddSystem(Movement);                      // A1: infantry
            Vehicles = new VehicleKinematicsSystem(map);
            World.AddSystem(Vehicles);                      // A1 / A5b: vehicles: trenches, ditching, mud, slopes, crushing
            if (combat)
            {
                Gunnery = new TankGunnerySystem(map);
                World.AddSystem(Gunnery);                   // A5b: tank main guns (steps right after direct fire)
            }
            Modules = new VehicleModulesSystem(map);
            World.AddSystem(Modules);                       // A5b: armour, modules, crew, fire, bail-out, cook-off, wrecks
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
