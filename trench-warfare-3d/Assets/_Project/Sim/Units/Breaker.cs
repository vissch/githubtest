// Phase: A5b (implemented 2026-09-25) — the Breaker's cycle. Owner's brief: "A breaker vehicle that stops a while in
// front of an enemy trench. Charges up, then charges into enemy trench, shoots enemies with critical strike, then
// drives back and does it again."
// A machine with TankSpec.BreakerRange drives on its flow field (Approach) until an enemy-held fire trench lies
// within range ahead of it. Then: WIND-UP (it halts, HaltTicks, WindupSeconds: the glow the player sees), CHARGE
// (VehicleKinematicsSystem drives it straight at the trench cell at ChargeSpeedMul, Charging set: it sees below
// the rim, its guns crit, its deck is thicker), STRIKE (halted on the cell for StrikeSeconds; the claw pass in
// TankGunnerySystem takes what is in the trench), WITHDRAW (it reverses to where it wound up at WithdrawSpeedMul),
// a rest, and Approach again. A trench that turns friendly, or a hull knocked out, stalled or immobilised, drops it
// back to Approach. State per slot, all hashed: Phase, PhaseTicks, TargetTrench, TargetCell, HomePos, Still, gen.
// Order 1115: after Movement has listed the vehicles, before Kinematics drives them, so a phase decided here is
// driven the same tick.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Combat;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Units
{
    public enum BreakerPhase : byte { Approach = 0, Windup = 1, Charge = 2, Strike = 3, Withdraw = 4 }

    public sealed class BreakerSystem : ISimSystem
    {
        public const uint LookEvery = 4;          // ticks between looks for a trench while approaching
        public const float AheadCos = 0.3f;       // the trench must lie ahead of the nose, not beside it
        public const float StrikeReach = 2.5f;    // metres from the target cell that count as "in it"
        public const float WithdrawSeconds = 10f; // the most a withdrawal may take before it gives up and approaches again
        public const int StuckTicks = 20;         // a withdrawal that has not moved for this long is over
        public int Order => SimSystemOrder.Breaker;

        readonly MapData map;
        FlowFieldManager fields;
        MovementSystem movement;
        VehicleKinematicsSystem kinematics;
        public NativeArray<byte> Phase;
        public NativeArray<int> PhaseTicks, TargetCell, Still;
        public NativeArray<short> TargetTrench;
        public NativeArray<float3> HomePos;
        NativeArray<ushort> gen;
        public int Cycles;

        public BreakerSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("BreakerSystem needs FlowFieldManager");
            movement = world.GetSystem<MovementSystem>() ?? throw new System.InvalidOperationException("BreakerSystem needs MovementSystem registered before it");
            kinematics = world.GetSystem<VehicleKinematicsSystem>() ?? throw new System.InvalidOperationException("BreakerSystem needs VehicleKinematicsSystem registered before it");
            int n = world.Config.MaxSlots;
            Phase = new NativeArray<byte>(n, Allocator.Persistent);
            PhaseTicks = new NativeArray<int>(n, Allocator.Persistent);
            TargetCell = new NativeArray<int>(n, Allocator.Persistent);
            Still = new NativeArray<int>(n, Allocator.Persistent);
            TargetTrench = new NativeArray<short>(n, Allocator.Persistent);
            HomePos = new NativeArray<float3>(n, Allocator.Persistent);
            gen = new NativeArray<ushort>(n, Allocator.Persistent);
        }

        public void Step(SimWorld w)
        {
            var list = movement.Vehicles;
            float dt = w.Config.TickSeconds;
            for (int k = 0; k < list.Length; k++)
            {
                int i = list[k];
                var spec = TankSpec.For(w.Archetype[i]);
                if (spec.BreakerRange <= 0f) continue;
                if (gen[i] != w.Generation[i]) { gen[i] = w.Generation[i]; Phase[i] = 0; PhaseTicks[i] = 0; TargetCell[i] = -1; TargetTrench[i] = -1; Still[i] = 0; }
                uint f = w.Flags[i];
                if ((f & (uint)(UnitFlags.KnockedOut | UnitFlags.Stalled | UnitFlags.Immobilised)) != 0)
                {
                    if (Phase[i] != (byte)BreakerPhase.Approach) Approach(w, i, 0);
                    continue;
                }
                var phase = (BreakerPhase)Phase[i];
                short target = TargetTrench[i];
                bool lost = phase != BreakerPhase.Approach && (target < 0 || fields.Trenches[target].OwnerTeam == w.Team[i]);
                if (lost) { Approach(w, i, (int)(spec.RestSeconds / dt)); continue; }
                float3 p = w.Position[i];
                switch (phase)
                {
                    case BreakerPhase.Approach:
                        if (PhaseTicks[i] > 0) { PhaseTicks[i]--; break; }
                        if (w.Tick % LookEvery != (uint)i % LookEvery) break;
                        int cell = TrenchAhead(w, i, spec.BreakerRange);
                        if (cell < 0) break;
                        TargetCell[i] = cell; TargetTrench[i] = map.CellTrenchId[cell]; HomePos[i] = p;
                        Phase[i] = (byte)BreakerPhase.Windup;
                        PhaseTicks[i] = math.max(1, (int)(spec.WindupSeconds / dt));
                        kinematics.HaltTicks[i] = PhaseTicks[i];
                        kinematics.Drive[i] = VehicleKinematicsSystem.DriveFlow;
                        Emit(w, i, BreakerPhase.Windup);
                        break;
                    case BreakerPhase.Windup:
                        if (--PhaseTicks[i] > 0) break;
                        Phase[i] = (byte)BreakerPhase.Charge;
                        PhaseTicks[i] = math.max(1, (int)(spec.ChargeSeconds / dt));
                        kinematics.HaltTicks[i] = 0;
                        kinematics.Drive[i] = VehicleKinematicsSystem.DriveStraight;
                        kinematics.DriveTarget[i] = map.NavCellCenter(TargetCell[i]);
                        kinematics.DriveSpeedMul[i] = spec.ChargeSpeedMul;
                        w.Flags[i] = f | (uint)UnitFlags.Charging;
                        Emit(w, i, BreakerPhase.Charge);
                        break;
                    case BreakerPhase.Charge:
                    {
                        float3 goal = map.NavCellCenter(TargetCell[i]);
                        bool there = math.distancesq(p.xz, goal.xz) <= StrikeReach * StrikeReach;
                        if (!there && --PhaseTicks[i] > 0) break;
                        Phase[i] = (byte)BreakerPhase.Strike;
                        PhaseTicks[i] = math.max(1, (int)(spec.StrikeSeconds / dt));
                        kinematics.HaltTicks[i] = PhaseTicks[i];
                        Emit(w, i, BreakerPhase.Strike);
                        break;
                    }
                    case BreakerPhase.Strike:
                        if (--PhaseTicks[i] > 0) break;
                        Phase[i] = (byte)BreakerPhase.Withdraw;
                        PhaseTicks[i] = (int)(WithdrawSeconds / dt);
                        Still[i] = 0;
                        kinematics.HaltTicks[i] = 0;
                        kinematics.Drive[i] = VehicleKinematicsSystem.DriveReverse;
                        kinematics.DriveTarget[i] = HomePos[i];
                        kinematics.DriveSpeedMul[i] = spec.WithdrawSpeedMul;
                        w.Flags[i] = f & ~(uint)UnitFlags.Charging;
                        Emit(w, i, BreakerPhase.Withdraw);
                        break;
                    case BreakerPhase.Withdraw:
                    {
                        bool home = math.distancesq(p.xz, HomePos[i].xz) <= (VehicleKinematicsSystem.DriveArrive + 0.5f) * (VehicleKinematicsSystem.DriveArrive + 0.5f);
                        Still[i] = math.lengthsq(w.Velocity[i]) < 0.0025f ? Still[i] + 1 : 0;
                        if (!home && Still[i] < StuckTicks && --PhaseTicks[i] > 0) break;
                        Cycles++;
                        Approach(w, i, (int)(spec.RestSeconds / dt));
                        break;
                    }
                }
            }
        }

        void Approach(SimWorld w, int i, int restTicks)
        {
            Phase[i] = (byte)BreakerPhase.Approach; PhaseTicks[i] = restTicks; TargetCell[i] = -1; TargetTrench[i] = -1; Still[i] = 0;
            kinematics.Drive[i] = VehicleKinematicsSystem.DriveFlow; kinematics.DriveSpeedMul[i] = 1f;
            w.Flags[i] &= ~(uint)UnitFlags.Charging;
            Emit(w, i, BreakerPhase.Approach);
        }

        void Emit(SimWorld w, int i, BreakerPhase phase)
            => w.Events.Add(w.Tick, SimEventType.BreakerPhase, i, (int)phase, TargetCell[i] >= 0 ? map.NavCellCenter(TargetCell[i]) : w.Position[i]);

        /// <summary>The nearest body cell of an enemy-held fire trench within range and ahead of the nose, or -1.</summary>
        int TrenchAhead(SimWorld w, int i, float range)
        {
            float3 p = w.Position[i];
            float3 nose = SimMath.DirFromYaw(w.Yaw[i]);
            byte team = w.Team[i];
            int best = -1; float bestD = range * range;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                if (fields.Trenches[t].OwnerTeam == team) continue;
                var def = map.Trenches[t];
                if (def.Kind != 0) continue;
                for (int k = 0; k < def.CellCount; k++)
                {
                    int cell = map.TrenchCells[def.CellStart + k];
                    if ((map.NavLayers[cell] & (byte)NavLayer.Link) != 0) continue;
                    float3 d = map.NavCellCenter(cell) - p; d.y = 0f;
                    float dsq = math.lengthsq(d);
                    if (dsq >= bestD || dsq < 1f) continue;
                    if (math.dot(d / SimMath.Sqrt(dsq), nose) < AheadCos) continue;
                    bestD = dsq; best = cell;
                }
            }
            return best;
        }

        public ulong Hash(ulong h)
        {
            h = SimHash.Array(Phase, h);
            h = SimHash.Array(PhaseTicks, h);
            h = SimHash.Array(TargetCell, h);
            h = SimHash.Array(Still, h);
            h = SimHash.Array(TargetTrench, h);
            h = SimHash.Array(HomePos, h);
            h = SimHash.Array(gen, h);
            return SimHash.Value(Cycles, h);
        }

        public void Dispose()
        {
            if (Phase.IsCreated) Phase.Dispose();
            if (PhaseTicks.IsCreated) PhaseTicks.Dispose();
            if (TargetCell.IsCreated) TargetCell.Dispose();
            if (Still.IsCreated) Still.Dispose();
            if (TargetTrench.IsCreated) TargetTrench.Dispose();
            if (HomePos.IsCreated) HomePos.Dispose();
            if (gen.IsCreated) gen.Dispose();
        }
    }
}
