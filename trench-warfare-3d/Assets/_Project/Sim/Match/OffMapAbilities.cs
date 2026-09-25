// Phase: A5 (implemented core: HE barrage and chlorine gas; the rest of docs/07-abilities.md follows the same path)
// Consumes SupportFire commands (a = ability id, pos = target): validates ability, target, silver and the player's
// cooldown, spends the silver, emits AbilityFired (the spotting round / cylinder hiss the enemy can react to) and
// schedules the payload. HE: 12 shells over 6 s after a 4 s warm-up, scattered in a 25 m radius, each 150 damage
// in 8 m, +60 suppression and a 3 m crater. Chlorine: after 3 s a source of concentration 40 opens for 12 s and
// drifts with the map wind. The enemy script uses the same path.
// ParaDrop (2026-09-25, a Brass faction ability): after WarmupTicks of flight, Men paratroopers come down in a
// Radius disc round the point, each on open ground (VehicleModulesSystem.OpenGround), Exposed, and walk for the
// nearest enemy-held fire trench. A drop is REFUSED on water, in a trench, off the map, or within ParaDropKeepOut
// metres of the enemy's rear trench (the critic's ambush: a stick garrisoning the trench every fresh reinforcement
// walks into). Every SupportFire is also checked against the caller's faction (FactionRoster.MayCall).
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Combat;
using TW.Sim.Nav;
using TW.Sim.Terrain;
using TW.Sim.Units;

namespace TW.Sim.Match
{
    public enum AbilityTargetMode : byte { Point = 0, Line = 1, Area = 2, Heading = 3, None = 4 }

    public enum OffMapAbilityId : short
    {
        None = 0, HeBarrage = 1, CreepingBarrage = 2, ChlorineGas = 3, MustardGas = 4, BomberRun = 5,
        SmokeScreen = 6, MortarSalvo = 7, ReconFlight = 8, ReinforcementSurge = 9,
        /// <summary>Eight paratroopers on a point after a short flight (2026-09-25; a Brass faction ability).</summary>
        ParaDrop = 10,
    }

    public struct AbilityStats
    {
        public OffMapAbilityId Id;
        public int Cost;
        public int CooldownTicks;
        public int WarmupTicks;
        public AbilityTargetMode Target;
        public float Radius, Length;
        public int Shells; public float ShellDamage, ShellRadius; public int SpreadTicks;
        public float Concentration; public int PersistTicks;
        public bool BreachesWire, CollapsesTrench, DestroysBunker;
        /// <summary>Paratroopers that come down (ParaDrop); 0 for everything else.</summary>
        public int Men;
    }

    /// <summary>A payload waiting for its tick. All fields are 4 bytes wide, so the struct hashes without padding.</summary>
    public struct ScheduledPayload
    {
        public uint Tick;
        public int Ability;
        public int Player;
        public float3 Pos;
    }

    public sealed class OffMapAbilitySystem : ISimSystem
    {
        public const int AbilitySlots = 12;   // indexed by OffMapAbilityId (ParaDrop = 10 made it twelve)
        public int Order => SimSystemOrder.Command + 30;

        public NativeArray<int> Cooldown;               // player * AbilitySlots + ability id, ticks left
        public NativeList<ScheduledPayload> Scheduled;
        BlastSystem blast;
        GasSmokeSystem gas;
        FlowFieldManager fields;
        readonly MapData map;                           // null: no landing checks (a test without a map)
        /// <summary>A stick may not land closer than this to the enemy's rear fire trench.</summary>
        public const float ParaDropKeepOut = 30f;

        public OffMapAbilitySystem(MapData map = null) { this.map = map; }

        /// <summary>Placeholder stats until C2 bakes TW.Data.AbilityDefinition; the numbers are the ones in docs/07.</summary>
        public static bool TryGetStats(int ability, out AbilityStats s)
        {
            switch ((OffMapAbilityId)ability)
            {
                case OffMapAbilityId.HeBarrage:
                    s = new AbilityStats { Id = OffMapAbilityId.HeBarrage, Cost = 150, CooldownTicks = 1200, WarmupTicks = 80, Target = AbilityTargetMode.Area,
                                           Radius = 25f, Shells = 12, ShellDamage = 150f, ShellRadius = 8f, SpreadTicks = 120, BreachesWire = true };
                    return true;
                case OffMapAbilityId.ChlorineGas:
                    s = new AbilityStats { Id = OffMapAbilityId.ChlorineGas, Cost = 120, CooldownTicks = 1800, WarmupTicks = 60, Target = AbilityTargetMode.Point,
                                           Concentration = 40f, PersistTicks = 240 };
                    return true;
                case OffMapAbilityId.ParaDrop:
                    s = new AbilityStats { Id = OffMapAbilityId.ParaDrop, Cost = 260, CooldownTicks = 1500, WarmupTicks = 100, Target = AbilityTargetMode.Point,
                                           Radius = 12f, Men = 8 };
                    return true;
                default:
                    s = default;
                    return false;
            }
        }

        public void Initialize(SimWorld world)
        {
            blast = world.GetSystem<BlastSystem>() ?? throw new System.InvalidOperationException("OffMapAbilitySystem needs BlastSystem registered before it");
            gas = world.GetSystem<GasSmokeSystem>() ?? throw new System.InvalidOperationException("OffMapAbilitySystem needs GasSmokeSystem registered before it");
            fields = world.GetSystem<FlowFieldManager>();
            Cooldown = new NativeArray<int>(SimConfig.MaxPlayers * AbilitySlots, Allocator.Persistent);
            Scheduled = new NativeList<ScheduledPayload>(64, Allocator.Persistent);
        }

        public int CooldownOf(int player, OffMapAbilityId ability) => Cooldown[player * AbilitySlots + (int)ability];

        public void Step(SimWorld w)
        {
            for (int i = 0; i < Cooldown.Length; i++) if (Cooldown[i] > 0) Cooldown[i]--;

            for (int c = 0; c < w.TickCommands.Length; c++)
            {
                var cmd = w.TickCommands[c];
                if (cmd.Type != CommandType.SupportFire) continue;
                if (w.WinnerTeam >= 0 || cmd.A <= 0 || cmd.A >= AbilitySlots || !TryGetStats(cmd.A, out var stats)) { w.Reject(cmd); continue; }
                int slot = cmd.Player * AbilitySlots + cmd.A;
                var size = w.Init.SizeMeters;
                bool inside = cmd.Pos.x >= 0f && cmd.Pos.z >= 0f && cmd.Pos.x <= size.x && cmd.Pos.z <= size.y;
                if (!inside || Cooldown[slot] > 0 || w.Silver[cmd.Player] < stats.Cost) { w.Reject(cmd); continue; }
                if (!FactionRoster.MayCall(w.Config.FactionOf(cmd.Player), cmd.A)) { w.Reject(cmd); continue; }
                if (stats.Men > 0 && !CanLand(cmd.Player, cmd.Pos)) { w.Reject(cmd); continue; }

                w.Silver[cmd.Player] -= stats.Cost;
                Cooldown[slot] = stats.CooldownTicks;
                float3 target = new float3(cmd.Pos.x, 0f, cmd.Pos.z);
                w.Events.Add(w.Tick, SimEventType.AbilityFired, cmd.A, cmd.Player, target, default, stats.Radius);

                if (stats.Shells > 0)
                {
                    for (int k = 0; k < stats.Shells; k++)
                    {
                        var rng = SimRandom.For(w.Config.Seed, w.Tick, SimRandom.SystemId.IndirectFire, (uint)(cmd.Player * 64 + k));
                        float angle = rng.NextFloat(0f, 2f * math.PI);
                        float r = stats.Radius * SimMath.Sqrt(rng.NextFloat());   // uniform over the disc
                        float3 p = w.ClampToMap(target + new float3(SimMath.Cos(angle) * r, 0f, SimMath.Sin(angle) * r));
                        uint at = w.Tick + (uint)stats.WarmupTicks + (uint)(k * stats.SpreadTicks / stats.Shells);
                        Scheduled.Add(new ScheduledPayload { Tick = at, Ability = cmd.A, Player = cmd.Player, Pos = p });
                    }
                }
                else Scheduled.Add(new ScheduledPayload { Tick = w.Tick + (uint)stats.WarmupTicks, Ability = cmd.A, Player = cmd.Player, Pos = target });
                if (stats.Men > 0) w.Events.Add(w.Tick, SimEventType.DropInbound, cmd.Player, stats.Men, target, default, stats.WarmupTicks * w.Config.TickSeconds);
            }

            // deliver what is due, keeping the rest in order
            int keep = 0;
            for (int i = 0; i < Scheduled.Length; i++)
            {
                var p = Scheduled[i];
                if (p.Tick > w.Tick) { Scheduled[keep++] = p; continue; }
                TryGetStats(p.Ability, out var stats);
                if (stats.Shells > 0)
                {
                    // the battery is off the map behind its own line, so the shell arrives travelling up the field:
                    // player 0 fires towards +Z, player 1 towards -Z
                    float3 flight = new float3(0f, 0f, p.Player == 1 ? -1f : 1f);
                    blast.Queue(new Impact { Pos = p.Pos, Damage = stats.ShellDamage, Radius = stats.ShellRadius, Suppression = 60f, CraterRadius = 3f, CraterDepth = 1.2f, Source = p.Ability, Player = p.Player, Dir = flight });
                }
                else if (stats.Concentration > 0f)
                {
                    gas.AddSource(p.Pos, stats.Concentration, stats.PersistTicks, p.Player);
                    w.Events.Add(w.Tick, SimEventType.GasCloudSpawned, p.Ability, p.Player, p.Pos, default, stats.Concentration);
                }
                else if (stats.Men > 0) Land(w, p, stats);
            }
            Scheduled.Length = keep;
        }

        /// <summary>Open ground for a stick: the cell under the point and at least four of its eight neighbours open
        /// surface, and nowhere near the enemy's rear trench.</summary>
        bool CanLand(int player, float3 at)
        {
            if (map == null) return true;
            var c = map.NavCellOf(at);
            int open = 0;
            for (int dz = -1; dz <= 1; dz++)
            for (int dx = -1; dx <= 1; dx++)
            {
                int x = c.x + dx, z = c.y + dz;
                if (x < 0 || z < 0 || x >= map.NavWidth || z >= map.NavLength) continue;
                var layer = (NavLayer)map.NavLayers[map.NavIndex(x, z)];
                bool ok = (layer & NavLayer.Surface) != 0 && (layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Bunker | NavLayer.Blocked)) == 0;
                if (dx == 0 && dz == 0 && !ok) return false;
                if (ok) open++;
            }
            if (open < 5) return false;
            if (fields == null) return true;
            byte enemy = (byte)(1 - player);
            short rear = fields.RearTrench(enemy);
            if (rear < 0) return true;
            var def = map.Trenches[rear];
            float rearZ = map.NavCellCenter(map.TrenchCells[def.CellStart + def.CellCount / 2]).z;
            // team 0 attacks toward +Z: its drops stay short of the enemy's rear line by the keep-out, and the reverse
            return player == 0 ? at.z <= rearZ - ParaDropKeepOut : at.z >= rearZ + ParaDropKeepOut;
        }

        void Land(SimWorld w, ScheduledPayload p, in AbilityStats stats)
        {
            var entry = RosterEntry.Para;
            byte team = (byte)p.Player;
            int goal = -1;
            if (fields != null && map != null)
            {
                // the nearest fire trench the enemy holds: the stick goes for it from behind
                short best = -1; float bestD = float.MaxValue;
                for (int t = 0; t < map.Trenches.Length; t++)
                {
                    if (fields.Trenches[t].OwnerTeam == team) continue;
                    var def = map.Trenches[t];
                    if (def.Kind != 0 || def.CellCount == 0) continue;
                    float3 mid = map.NavCellCenter(map.TrenchCells[def.CellStart + def.CellCount / 2]);
                    float d = math.distancesq(mid.xz, p.Pos.xz);
                    if (d < bestD) { bestD = d; best = (short)t; }
                }
                if (best >= 0) goal = fields.GetGoal(GoalKey.Trench(best));
            }
            for (int k = 0; k < stats.Men; k++)
            {
                var rng = SimRandom.For(w.Config.Seed, w.Tick, SimRandom.SystemId.AirDrop, (uint)(p.Player * 64 + k));
                float angle = rng.NextFloat(0f, 2f * math.PI);
                float r = stats.Radius * SimMath.Sqrt(rng.NextFloat());
                float3 at = w.ClampToMap(p.Pos + new float3(SimMath.Cos(angle) * r, 0f, SimMath.Sin(angle) * r));
                if (map != null) at = VehicleModulesSystem.OpenGround(map, at);
                int slot = w.Spawn(team, entry.Archetype, at, entry.Hp, entry.Speed, false);
                if (slot < 0) return;   // the field is full
                w.Flags[slot] |= (uint)UnitFlags.Exposed;
                if (goal >= 0) w.GoalId[slot] = goal;
                w.Events.Add(w.Tick, SimEventType.DropLanded, slot, p.Player, at);
            }
        }

        public ulong Hash(ulong h)
        {
            h = SimHash.Array(Cooldown, h);
            for (int i = 0; i < Scheduled.Length; i++) h = SimHash.Value(Scheduled[i], h);
            return h;
        }

        public void Dispose()
        {
            if (Cooldown.IsCreated) Cooldown.Dispose();
            if (Scheduled.IsCreated) Scheduled.Dispose();
        }
    }
}
