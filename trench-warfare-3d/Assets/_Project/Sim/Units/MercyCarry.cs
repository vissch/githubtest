// Phase: A5b (implemented 2026-10-07) — the Mercy carries the wounded. Owner's brief, 2026-10-07: "the behavior of the
// ambulance should be they take badly hurt units from the frontline, can get up to 4 units, once theyre in the ambulance
// they drive out of view, once out of view the units spawn again full health." Its healing on the spot is over.
// A machine with TankSpec.Seats drives to the badly hurt men of its own side (FETCH): it takes the nearest one within
// BoardReach of its hull aboard (SimWorld.Lift, no Death: he left the field), keeps his archetype in a seat, and goes on
// until it is full, finds nobody, or is itself under fire. Then LEAVE: it drives to its own side's spawn point and
// OutOfViewMetres past it, and there it puts every man it carries back at full health (SimWorld.Spawn), on his own lane
// the way a reinforcement comes up — GoalId -1, so MovementSystem gives him DefaultGoal and he walks up. It never drives
// past the foremost trench its side holds to fetch a man. Knocked out, stalled or bogged it keeps its cargo; destroyed,
// the men aboard are lost with it (the slot's generation moves and the seats are cleared, nothing returns).
// State per slot, all hashed: Phase, Aboard, CargoArch (Seats per slot), BoardTimer, HurtTicks, LastHp, gen.
// Order 1117: after Breaker (1115), before VehicleKinematics (1120), so the drive it picks runs the same tick.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Combat;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Units
{
    public enum MercyPhase : byte { Fetch = 0, Leave = 1 }

    public sealed class MercyCarrySystem : ISimSystem
    {
        // Three numbers the owner's words leave open; these are the defaults taken for him, and he can turn them.
        /// <summary>"Badly hurt": hit points under this share of his own maximum.</summary>
        public const float BadlyHurt = 0.35f;
        /// <summary>Men aboard at most (his word: "up to 4 units").</summary>
        public const byte Seats = 4;
        /// <summary>"Out of view": this many metres past its own side's spawn line (Init.SpawnA/SpawnB, where its men
        /// enter the field). The men it carried come back at that line.</summary>
        public const float OutOfViewMetres = 6f;

        /// <summary>Metres past its hull's half length within which a man is taken aboard.</summary>
        public const float BoardReach = 4f;
        /// <summary>The furthest it will go for a man.</summary>
        public const float SearchMetres = 140f;
        /// <summary>Ticks it counts as under fire after its hull last lost a hit point.</summary>
        public const int UnderFireTicks = 40;
        /// <summary>Ticks between two men going aboard: one every half second.</summary>
        public const int BoardEveryTicks = 10;

        public int Order => SimSystemOrder.MercyCarry;

        readonly MapData map;
        CombatCatalogueSystem catalogue;
        FlowFieldManager fields;
        MovementSystem movement;
        VehicleKinematicsSystem kinematics;

        public NativeArray<byte> Phase, Aboard, CargoArch;   // CargoArch: slot * Seats + seat
        public NativeArray<int> BoardTimer, HurtTicks;
        public NativeArray<float> LastHp;
        NativeArray<ushort> gen;
        public int Runs;                                     // loads put back, for the tests and the gym

        public MercyCarrySystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            catalogue = world.GetSystem<CombatCatalogueSystem>() ?? throw new System.InvalidOperationException("MercyCarrySystem needs CombatCatalogueSystem registered before it");
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("MercyCarrySystem needs FlowFieldManager");
            movement = world.GetSystem<MovementSystem>() ?? throw new System.InvalidOperationException("MercyCarrySystem needs MovementSystem registered before it");
            kinematics = world.GetSystem<VehicleKinematicsSystem>() ?? throw new System.InvalidOperationException("MercyCarrySystem needs VehicleKinematicsSystem registered before it");
            int n = world.Config.MaxSlots;
            Phase = new NativeArray<byte>(n, Allocator.Persistent);
            Aboard = new NativeArray<byte>(n, Allocator.Persistent);
            CargoArch = new NativeArray<byte>(n * Seats, Allocator.Persistent);
            BoardTimer = new NativeArray<int>(n, Allocator.Persistent);
            HurtTicks = new NativeArray<int>(n, Allocator.Persistent);
            LastHp = new NativeArray<float>(n, Allocator.Persistent);
            gen = new NativeArray<ushort>(n, Allocator.Persistent);
        }

        public void Step(SimWorld w)
        {
            var list = movement.Vehicles;
            for (int k = 0; k < list.Length; k++)
            {
                int i = list[k];
                var spec = catalogue.Tank[w.Archetype[i]];
                if (spec.Seats == 0) continue;
                if (gen[i] != w.Generation[i]) Reset(i, w.Hp[i], w.Generation[i]);

                // under fire: hull hit points lost since the last tick (suppression is written for men, not hulls)
                HurtTicks[i] = w.Hp[i] < LastHp[i] ? UnderFireTicks : math.max(0, HurtTicks[i] - 1);
                LastHp[i] = w.Hp[i];
                if (BoardTimer[i] > 0) BoardTimer[i]--;

                if ((w.Flags[i] & (uint)(UnitFlags.KnockedOut | UnitFlags.Stalled | UnitFlags.Immobilised)) != 0)
                {
                    kinematics.Drive[i] = VehicleKinematicsSystem.DriveFlow;   // driven by nobody; it keeps its cargo
                    continue;
                }

                byte team = w.Team[i];
                int seats = math.min((int)spec.Seats, (int)Seats);
                float3 spawn = team == 0 ? w.Init.SpawnA : w.Init.SpawnB;
                if ((MercyPhase)Phase[i] == MercyPhase.Fetch)
                {
                    float frontZ = FrontZ(w, team, spawn.z);
                    int man = Candidate(w, i, team, frontZ);
                    if (man >= 0 && Aboard[i] < seats)
                    {
                        float3 d = w.Position[man] - w.Position[i]; d.y = 0f;
                        float reach = BoardReach + kinematics.Profiles[w.Archetype[i]].HalfLength;
                        if (math.lengthsq(d) <= reach * reach && BoardTimer[i] == 0)
                        {
                            CargoArch[i * Seats + Aboard[i]] = w.Archetype[man];
                            Aboard[i]++;
                            BoardTimer[i] = BoardEveryTicks;
                            w.Events.Add(w.Tick, SimEventType.UnitBoarded, man, i, w.Position[man]);
                            w.Lift(man);
                        }
                        else
                        {
                            float3 at = w.Position[man];
                            at.z = ClampToFront(at.z, team, frontZ);
                            kinematics.Drive[i] = VehicleKinematicsSystem.DriveStraight;
                            kinematics.DriveTarget[i] = at;
                            kinematics.DriveSpeedMul[i] = 1f;
                        }
                    }
                    // full, or carrying somebody with nobody left to fetch, or carrying somebody while it is shot at
                    bool leave = Aboard[i] >= seats || (Aboard[i] > 0 && (man < 0 || HurtTicks[i] > 0));
                    if (leave) Phase[i] = (byte)MercyPhase.Leave;
                    else if (man < 0) { kinematics.Drive[i] = VehicleKinematicsSystem.DriveFlow; kinematics.DriveSpeedMul[i] = 1f; }
                }
                if ((MercyPhase)Phase[i] != MercyPhase.Leave) continue;

                float3 rear = spawn;
                rear.z += team == 0 ? -OutOfViewMetres : OutOfViewMetres;
                kinematics.Drive[i] = VehicleKinematicsSystem.DriveStraight;
                kinematics.DriveTarget[i] = rear;
                kinematics.DriveSpeedMul[i] = 1f;
                bool past = team == 0 ? w.Position[i].z <= rear.z : w.Position[i].z >= rear.z;
                if (!past) continue;
                Unload(w, i, team, spawn);
            }
        }

        void Unload(SimWorld w, int i, byte team, float3 spawn)
        {
            for (int s = 0; s < Aboard[i]; s++)
            {
                byte arch = CargoArch[i * Seats + s];
                var entry = w.Units.Roster[arch];
                var rng = SimRandom.For(w.Config.Seed, w.Tick, SimRandom.SystemId.Deployment, 0x5EF0u + (uint)i * 16u + (uint)s);
                float3 at = spawn;
                at.z += rng.NextFloat(-2f, 2f);
                int slot = w.Spawn(team, arch, w.ClampToMap(at), entry.Hp, entry.Speed, false);
                if (slot < 0) continue;                      // no slot free: nothing can come back for him
                float3 p = w.Position[slot];
                p.x = Lane.Of(slot, w.Generation[slot], w.Init.SizeMeters.x);   // his own lane, as a reinforcement comes up
                w.Position[slot] = w.ClampToMap(p);
                w.Events.Add(w.Tick, SimEventType.UnitReturned, slot, i, w.Position[slot], new float3(0f, team, 0f));
            }
            if (Aboard[i] > 0) Runs++;
            Aboard[i] = 0;
            Phase[i] = (byte)MercyPhase.Fetch;
            kinematics.Drive[i] = VehicleKinematicsSystem.DriveFlow;
        }

        void Reset(int i, float hp, ushort generation)
        {
            gen[i] = generation;
            Phase[i] = (byte)MercyPhase.Fetch; Aboard[i] = 0; BoardTimer[i] = 0; HurtTicks[i] = 0; LastHp[i] = hp;
            for (int s = 0; s < Seats; s++) CargoArch[i * Seats + s] = 0;
        }

        /// <summary>The z of the foremost trench its side holds: it goes no further forward than this for a man. No
        /// trench held: the rear trench, else its own spawn line.</summary>
        float FrontZ(SimWorld w, byte team, float spawnZ)
        {
            short t = fields.FrontTrench(team);
            if (t < 0) t = fields.RearTrench(team);
            if (t < 0) return spawnZ;
            var def = map.Trenches[t];
            if (def.CellCount == 0) return spawnZ;
            return map.NavCellCenter(map.TrenchCells[def.CellStart]).z;
        }

        static float ClampToFront(float z, byte team, float frontZ)
            => team == 0 ? math.min(z, frontZ) : math.max(z, frontZ);

        /// <summary>The nearest living man of its own side under BadlyHurt within SearchMetres who is not beyond the
        /// foremost trench its side holds (ties to the lower slot, as SupportSystem does), or -1.</summary>
        int Candidate(SimWorld w, int i, byte team, float frontZ)
        {
            int best = -1; float bestD = SearchMetres * SearchMetres;
            float3 at = w.Position[i];
            for (int j = 0; j < w.HighWater; j++)
            {
                if (j == i) continue;
                uint fj = w.Flags[j];
                if ((fj & (uint)UnitFlags.Alive) == 0 || (fj & (uint)UnitFlags.Vehicle) != 0 || w.Team[j] != team) continue;
                if (w.MaxHp[j] <= 0f || w.Hp[j] >= w.MaxHp[j] * BadlyHurt) continue;
                float3 p = w.Position[j];
                if (team == 0 ? p.z > frontZ : p.z < frontZ) continue;   // never past its own front trench
                float3 d = p - at; d.y = 0f;
                float d2 = math.lengthsq(d);
                if (d2 < bestD || (d2 == bestD && best >= 0 && j < best)) { bestD = d2; best = j; }
            }
            return best;
        }

        public ulong Hash(ulong h)
        {
            h = SimHash.Array(Phase, h);
            h = SimHash.Array(Aboard, h);
            h = SimHash.Array(CargoArch, h);
            h = SimHash.Array(BoardTimer, h);
            h = SimHash.Array(HurtTicks, h);
            h = SimHash.Array(LastHp, h);
            h = SimHash.Array(gen, h);
            return SimHash.Value(Runs, h);
        }

        public void Dispose()
        {
            if (Phase.IsCreated) Phase.Dispose();
            if (Aboard.IsCreated) Aboard.Dispose();
            if (CargoArch.IsCreated) CargoArch.Dispose();
            if (BoardTimer.IsCreated) BoardTimer.Dispose();
            if (HurtTicks.IsCreated) HurtTicks.Dispose();
            if (LastHp.IsCreated) LastHp.Dispose();
            if (gen.IsCreated) gen.Dispose();
        }
    }
}
