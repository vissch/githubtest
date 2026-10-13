// Phase: A5b (implemented 2026-10-13) — the one machine that flies. Owner's brief: "Flies over everything, never
// lands; rifles and machine guns can shoot it down, tank guns and mines cannot."
// A machine whose VehicleProfile.Flies (FlyHeight above 0, the Hopper's 9 m) is marked UnitFlags.Flying and driven
// in a straight line at the enemy HQ, not along its flow field: it crosses trenches, wire, houses and wrecks as if
// they were not there (VehicleKinematicsSystem skips every ground rule for a flyer), and it holds off over the front
// rather than parking on the HQ (TankSpec.StandOffMetres, TankGunnerySystem.StandOff). Knocked out, the flag comes
// off and it falls back onto its field: it is a hulk on the ground like any other.
// Order 112, with the tick's commands: the flag is set the tick the machine is deployed, before every reader of it.
// No state of its own.
using Unity.Mathematics;
using TW.Sim.Terrain;

namespace TW.Sim.Nav
{
    public sealed class FlightSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Flight;

        readonly MapData map;
        FlowFieldManager fields;
        VehicleKinematicsSystem kinematics;

        public FlightSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("FlightSystem needs FlowFieldManager registered before it");
            kinematics = world.GetSystem<VehicleKinematicsSystem>() ?? throw new System.InvalidOperationException("FlightSystem needs VehicleKinematicsSystem registered before it");
        }

        public void Step(SimWorld w)
        {
            // the slots themselves, not MovementSystem's vehicle list: that is built at order 1110, so at 112 it still
            // holds last tick's machines and a flyer put down this tick would spend a tick on the ground
            for (int i = 0; i < w.HighWater; i++)
            {
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0 || (f & (uint)UnitFlags.Vehicle) == 0) continue;
                if (!kinematics.Profiles[w.Archetype[i]].Flies) continue;
                if ((f & (uint)UnitFlags.KnockedOut) != 0)
                {
                    // it falls: no longer flying, and back on its field, where the wreck rules take it
                    w.Flags[i] = f & ~(uint)UnitFlags.Flying;
                    kinematics.Drive[i] = VehicleKinematicsSystem.DriveFlow;
                    continue;
                }
                w.Flags[i] = f | (uint)UnitFlags.Flying;
                kinematics.Drive[i] = VehicleKinematicsSystem.DriveStraight;
                // GoalId is left as it was given (the enemy HQ objective, FlowFieldManager.DefaultGoal), so
                // TankGunnerySystem.StandOff's HoldGoal test still holds and an order sent to it still counts
                float3 to = HqPos(w.Team[i]);
                if (!math.any(math.isnan(to))) kinematics.DriveTarget[i] = to;
            }
        }

        /// <summary>The middle cell of the enemy HQ objective: where a flyer with no other orders goes. NaN when the map
        /// has no enemy HQ (a greybox with one side only): it then keeps the point it had.</summary>
        float3 HqPos(byte team)
        {
            short hq = fields.EnemyHq(team);
            if (hq < 0) return new float3(float.NaN);
            for (int o = 0; o < map.Objectives.Length; o++)
            {
                var def = map.Objectives[o];
                if (def.Id != hq || def.CellCount == 0) continue;
                return map.NavCellCenter(map.ObjectiveCells[def.CellStart + def.CellCount / 2]);
            }
            return new float3(float.NaN);
        }

        /// <summary>Nothing of its own: the Flying flag, Drive and DriveTarget it writes are hashed where they live
        /// (SimWorld.Flags, VehicleKinematicsSystem).</summary>
        public ulong Hash(ulong h) => h;

        public void Dispose() { }
    }
}
