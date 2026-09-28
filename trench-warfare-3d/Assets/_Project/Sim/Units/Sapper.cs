// Phase: A5 / docs/21 SIM-D (implemented 2026-09-28: the sapper half; the mines themselves are MineSystem's) — the man
// who lays them. Owner's decision (decisions.md 2026-09-26): mines and tripwires are laid during the match by a sapper
// walking out on a UnitAbility command, not in a preparation phase.
// Depends on: MineSystem (Lies / LiesAlong: where a mine may lie; Place: the laying), FlowFieldManager (a cell goal he
// walks to), MovementSystem (walks him), InfantrySpec.MineCharges (who is a sapper and how many he carries).
//
// The order is CommandType.UnitAbility: a = the sapper's slot, pos = the point (a mine) or where the line starts (a
// tripwire), b = the UnitAbilityId (LayMine / LayTripwire) in its low byte and, above it, AbilityArgs.Pack(heading, 0,
// length) for a tripwire (length 0 = DefaultTripwireMetres). It is rejected (CommandRejected) unless the slot holds a
// living man of the player's own side whose spec carries charges, who has one left, is doing nothing else for this
// system and is not pinned, the id is one of the two, a mine may lie there (a tripwire: along its whole line), and a
// goal is to be had. Accepted: SapperOrdered, and he walks to the point's nav cell on a cell goal; a garrisoned man
// leaves his trench as on an advance (UnitLeftTrench, Exposed). In the cell he stops and places for LaySeconds
// (SapperLaying), the count waiting while he is pinned; then MineSystem.Place lays it (MinePlaced, arming as any mine),
// a charge is spent, and he goes back to the trench he left (no trench: wherever a fresh man would go). An order from
// anyone else that re-goals him on the way (an advance, a fallback) ends the errand and spends nothing.
//
// Steps with the commands (after TrenchOrders 110, before the flow fields 400 build his goal), in slot order on the main
// thread. Nothing here is random. State per slot, all hashed, all reset when the slot is re-used: Charges, Phase, Kind,
// Goal, Args, LayTicks, Back, Target.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Combat;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Sim.Units
{
    public sealed class SapperSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Sapper;

        /// <summary>Seconds a man takes to place a mine or string a wire once he stands at the spot (docs/21: 3 s).</summary>
        public const float LaySeconds = 3f;
        /// <summary>A tripwire ordered with no length.</summary>
        public const int DefaultTripwireMetres = 6;
        public const byte Idle = 0, Walking = 1, Laying = 2;

        readonly MapData map;
        MineSystem mines;
        FlowFieldManager fields;

        public NativeArray<byte> Charges, Phase, Kind;
        public NativeArray<int> Goal, Args, LayTicks;
        public NativeArray<short> Back;
        public NativeArray<float3> Target;
        NativeArray<ushort> gen;
        /// <summary>Stats, not hashed.</summary>
        public int Ordered { get; private set; }
        public int Laid { get; private set; }

        public SapperSystem(MapData map) { this.map = map; }

        public void Initialize(SimWorld world)
        {
            mines = world.GetSystem<MineSystem>() ?? throw new System.InvalidOperationException("SapperSystem needs MineSystem registered before it");
            fields = world.GetSystem<FlowFieldManager>() ?? throw new System.InvalidOperationException("SapperSystem needs FlowFieldManager registered before it");
            int n = world.Config.MaxSlots;
            Charges = new NativeArray<byte>(n, Allocator.Persistent);
            Phase = new NativeArray<byte>(n, Allocator.Persistent);
            Kind = new NativeArray<byte>(n, Allocator.Persistent);
            Goal = new NativeArray<int>(n, Allocator.Persistent);
            Args = new NativeArray<int>(n, Allocator.Persistent);
            LayTicks = new NativeArray<int>(n, Allocator.Persistent);
            Back = new NativeArray<short>(n, Allocator.Persistent);
            Target = new NativeArray<float3>(n, Allocator.Persistent);
            gen = new NativeArray<ushort>(n, Allocator.Persistent);
            for (int i = 0; i < n; i++) { Goal[i] = -1; Back[i] = -1; }
        }

        /// <summary>How many charges the man in a slot has left (0 for anyone who is not a sapper).</summary>
        public int ChargesOf(SimWorld w, int slot)
        {
            if (slot < 0 || slot >= w.HighWater || !w.IsAlive(slot)) return 0;
            return gen[slot] == w.Generation[slot] ? Charges[slot] : math.clamp(w.Units.Infantry[w.Archetype[slot]].MineCharges, 0, 255);
        }

        public void Step(SimWorld w)
        {
            int n = w.HighWater;
            // a slot's new tenant starts with his spec's charges and no errand
            for (int i = 0; i < n; i++)
            {
                if (gen[i] == w.Generation[i]) continue;
                gen[i] = w.Generation[i];
                Charges[i] = (byte)math.clamp(w.Units.Infantry[w.Archetype[i]].MineCharges, 0, 255);
                Clear(i);
            }

            for (int c = 0; c < w.TickCommands.Length; c++)
            {
                var cmd = w.TickCommands[c];
                if (cmd.Type != CommandType.UnitAbility) continue;
                if (!Accept(w, cmd)) w.Reject(cmd);
            }

            int layTicks = (int)math.round(LaySeconds * w.Config.TickRate);
            for (int i = 0; i < n; i++)
            {
                if (Phase[i] == Idle) continue;
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0) { Clear(i); continue; }
                if (Phase[i] == Walking)
                {
                    if (w.GoalId[i] != Goal[i]) { Clear(i); continue; }   // somebody else's order took him: the errand is over
                    var at = map.NavCellOf(w.Position[i]);
                    if (map.NavIndex(at.x, at.y) != fields.Goals[Goal[i]].Ref) continue;
                    Phase[i] = Laying; LayTicks[i] = layTicks;
                    w.Events.Add(w.Tick, SimEventType.SapperLaying, i, Kind[i], Target[i], default, LaySeconds);
                    continue;
                }
                // Laying: he keeps the cell goal, whose own cell has no direction, so he stands where he is
                if (w.GoalId[i] != Goal[i]) { Clear(i); continue; }
                if (w.Suppression[i] >= StanceRules.PinnedSuppression) continue;   // head down: the count waits
                if (--LayTicks[i] > 0) continue;
                Lay(w, i);
            }
        }

        bool Accept(SimWorld w, in SimCommand cmd)
        {
            if (cmd.Player >= SimConfig.MaxPlayers) return false;
            int id = cmd.B & 0xFF, args = cmd.B >> 8;
            if (id != (int)UnitAbilityId.LayMine && id != (int)UnitAbilityId.LayTripwire) return false;
            int i = cmd.A;
            if (i < 0 || i >= w.HighWater || !w.IsAlive(i)) return false;
            uint f = w.Flags[i];
            if ((f & (uint)UnitFlags.Vehicle) != 0 || w.Team[i] != cmd.Player) return false;
            if (w.Units.Infantry[w.Archetype[i]].MineCharges <= 0 || Charges[i] == 0 || Phase[i] != Idle) return false;
            if (w.Suppression[i] >= StanceRules.PinnedSuppression) return false;

            float3 pos = w.ClampToMap(cmd.Pos); pos.y = 0f;
            bool wire = id == (int)UnitAbilityId.LayTripwire;
            if (wire)
            {
                AbilityArgs.Unpack(args, out int heading, out _, out int length);
                if (length == 0) length = DefaultTripwireMetres;
                float metres = math.clamp(length, MineSystem.MinTripwireMetres, MineSystem.MaxTripwireMetres);
                if (!mines.LiesAlong(pos, AbilityArgs.Heading(heading), metres)) return false;
                args = AbilityArgs.Pack(heading, 0, (int)metres);
            }
            else { if (!mines.Lies(pos)) return false; args = 0; }

            var cell = map.NavCellOf(pos);
            int goal = GoalFor(w, map.NavIndex(cell.x, cell.y));
            if (goal < 0) return false;

            short trench = w.TrenchId[i];
            Phase[i] = Walking; Kind[i] = (byte)id; Goal[i] = goal; Args[i] = args; Target[i] = pos; LayTicks[i] = 0;
            Back[i] = trench >= 0 ? trench : w.SourceTrench[i];
            w.GoalId[i] = goal;
            if (trench >= 0)
            {
                // over the top, as on an advance (TrenchOrdersSystem.Advance)
                w.TrenchId[i] = -1;
                w.SourceTrench[i] = trench;
                w.Events.Add(w.Tick, SimEventType.UnitLeftTrench, i, trench, w.Position[i]);
            }
            w.Flags[i] = f | (uint)UnitFlags.Exposed;
            AbilityArgs.Unpack(args, out int h, out _, out int len);
            w.Events.Add(w.Tick, SimEventType.SapperOrdered, i, id, pos, wire ? AbilityArgs.Heading(h) * len : default, 0f);
            Ordered++;
            return true;
        }

        /// <summary>The goal of a nav cell: the one already made for it, else a cell goal nobody walks to any more made over
        /// for it, else a new one; -1 when the table is full of goals in use (the order is then rejected, never thrown).</summary>
        int GoalFor(SimWorld w, int navCell)
        {
            var key = GoalKey.Cell(navCell);
            for (int g = 0; g < fields.GoalCount; g++) if (fields.Goals[g].Equals(key)) return g;
            for (int g = 0; g < fields.GoalCount; g++)
            {
                if (fields.Goals[g].Kind != GoalKind.Cell || fields.Goals[g].Mode != NavMode.Infantry) continue;
                bool used = false;
                for (int i = 0; i < w.HighWater && !used; i++)
                    used = (w.Flags[i] & (uint)UnitFlags.Alive) != 0 && (w.GoalId[i] == g || (Phase[i] != Idle && Goal[i] == g));
                if (used) continue;
                fields.Retarget(g, key);
                return g;
            }
            return fields.TryGetGoal(key);
        }

        void Lay(SimWorld w, int i)
        {
            bool wire = Kind[i] == (int)UnitAbilityId.LayTripwire;
            AbilityArgs.Unpack(Args[i], out int heading, out _, out int length);
            int index = mines.Place(w, Target[i], wire ? AbilityArgs.Heading(heading) : float3.zero, length, w.Team[i], wire ? MineKind.Tripwire : MineKind.Mine);
            if (index >= 0) { Charges[i]--; Laid++; }   // a full field, or ground a shell has since closed: he keeps the charge
            short back = Back[i];
            Clear(i);
            // home: the trench he left if his side still holds it, else wherever a fresh man would go (MovementSystem)
            w.GoalId[i] = back >= 0 && back < fields.Trenches.Length && fields.Trenches[back].OwnerTeam == w.Team[i]
                ? fields.TryGetGoal(GoalKey.Trench(back)) : -1;
        }

        void Clear(int i) { Phase[i] = Idle; Kind[i] = 0; Goal[i] = -1; Args[i] = 0; LayTicks[i] = 0; Back[i] = -1; Target[i] = default; }

        public ulong Hash(ulong h)
        {
            h = SimHash.Array(Charges, h); h = SimHash.Array(Phase, h); h = SimHash.Array(Kind, h);
            h = SimHash.Array(Goal, h); h = SimHash.Array(Args, h); h = SimHash.Array(LayTicks, h);
            h = SimHash.Array(Back, h); h = SimHash.Array(Target, h);
            return SimHash.Array(gen, h);
        }

        public void Dispose()
        {
            if (Charges.IsCreated) Charges.Dispose();
            if (Phase.IsCreated) Phase.Dispose();
            if (Kind.IsCreated) Kind.Dispose();
            if (Goal.IsCreated) Goal.Dispose();
            if (Args.IsCreated) Args.Dispose();
            if (LayTicks.IsCreated) LayTicks.Dispose();
            if (Back.IsCreated) Back.Dispose();
            if (Target.IsCreated) Target.Dispose();
            if (gen.IsCreated) gen.Dispose();
        }
    }
}
