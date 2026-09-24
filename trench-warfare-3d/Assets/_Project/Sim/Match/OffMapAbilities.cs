// Phase: A5 (implemented core: HE barrage and chlorine gas; the rest of docs/07-abilities.md follows the same path)
// Consumes SupportFire commands (a = ability id, pos = target): validates ability, target, silver and the player's
// cooldown, spends the silver, emits AbilityFired (the spotting round / cylinder hiss the enemy can react to) and
// schedules the payload. HE: 12 shells over 6 s after a 4 s warm-up, scattered in a 25 m radius, each 150 damage
// in 8 m, +60 suppression and a 3 m crater. Chlorine: after 3 s a source of concentration 40 opens for 12 s and
// drifts with the map wind. The enemy script uses the same path.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Combat;

namespace TW.Sim.Match
{
    public enum AbilityTargetMode : byte { Point = 0, Line = 1, Area = 2, Heading = 3, None = 4 }

    public enum OffMapAbilityId : short
    {
        None = 0, HeBarrage = 1, CreepingBarrage = 2, ChlorineGas = 3, MustardGas = 4, BomberRun = 5,
        SmokeScreen = 6, MortarSalvo = 7, ReconFlight = 8, ReinforcementSurge = 9,
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
        public const int AbilitySlots = 10;   // indexed by OffMapAbilityId
        public int Order => SimSystemOrder.Command + 30;

        public NativeArray<int> Cooldown;               // player * AbilitySlots + ability id, ticks left
        public NativeList<ScheduledPayload> Scheduled;
        BlastSystem blast;
        GasSmokeSystem gas;

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
                default:
                    s = default;
                    return false;
            }
        }

        public void Initialize(SimWorld world)
        {
            blast = world.GetSystem<BlastSystem>() ?? throw new System.InvalidOperationException("OffMapAbilitySystem needs BlastSystem registered before it");
            gas = world.GetSystem<GasSmokeSystem>() ?? throw new System.InvalidOperationException("OffMapAbilitySystem needs GasSmokeSystem registered before it");
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
            }
            Scheduled.Length = keep;
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
