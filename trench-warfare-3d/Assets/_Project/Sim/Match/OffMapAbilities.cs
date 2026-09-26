// Phase: A5 (implemented core: HE barrage in three patterns, creeping barrage, chlorine gas as a point or creeping,
// smoke screen, strafe run; the rest of docs/07-abilities.md follows the same path; Beam 11 is SIM-C in docs/21)
// Consumes SupportFire commands (a = ability id, pos = the target or where a line starts, b = AbilityArgs: heading,
// pattern, length): validates ability, pattern, target, silver and the player's cooldown, spends the silver, emits
// AbilityFired (the spotting round, the cylinder hiss, the engine note the enemy can react to) and schedules the
// payload. Every line starts at pos and runs along the heading for the length (the ability's own when b says 0).
//   HE barrage: 12 shells over 6 s after a 4 s warm-up, each 150 damage in 8 m, +60 suppression and a 3 m crater.
//     Pattern 0 scatters them over a 25 m disc, 1 along a line (up to 60 m, 4 m either side), 2 over a 16 m wide box.
//   Creeping barrage: 10 lifts of 4 shells (120 damage in 8 m), a lift every 4 s, each 6 m further along the
//     heading and 10 m either side of it; the caller's own men within 15 m behind a lift are spared (Impact.SafeBehind).
//   Chlorine: after 3 s a source of concentration 40 opens for 12 s and drifts with the map wind. Pattern 1 walks
//     eight sources of 30 up the heading, 8 m and 3 s apart, 10 s each: the cloud creeps.
//   Smoke screen: five sources of 30 along a 40 m line for 30 s, in GasSmokeSystem's smoke field; SmokeLos then
//     cuts sight and aim through it.
//   Strafe run: 32 bursts of 60 in 5 m along an 80 m corridor over 2 s, no crater (the aircraft is presentation).
//   Beam: after 4 s a sweep of 6 s up a 60 m corridor 2 m either side (BeamSystem: 300 dps on men, 1200 on hulls,
//     it sets men alight and scorches the ground); at most two running a player.
// The enemy script uses the same path.
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
        /// <summary>A line of machine-gun bursts from one low pass (docs/21 phase 5).</summary>
        StrafeRun = 10,
        /// <summary>The sweeping beam (docs/21 phase 5): a line of fire that walks its corridor (BeamSystem).</summary>
        Beam = 11,
    }

    /// <summary>The patterns an ability offers (AbilityArgs pattern). 0 is always the plain one.</summary>
    public static class AbilityPattern
    {
        public const int Disc = 0, Line = 1, Box = 2;   // HeBarrage
        public const int Point = 0, Creeping = 1;       // ChlorineGas
    }

    /// <summary>What a scheduled payload turns into when its tick comes.</summary>
    public enum PayloadKind : int { Shell = 0, GasSource = 1, SmokeSource = 2, BeamStart = 3 }

    public struct AbilityStats
    {
        public OffMapAbilityId Id;
        public int Cost;
        public int CooldownTicks;
        public int WarmupTicks;
        public AbilityTargetMode Target;
        public float Radius, Length;
        /// <summary>Half the width of a line's corridor in metres: how far either side of it the payload scatters.</summary>
        public float HalfWidth;
        public int Shells; public float ShellDamage, ShellRadius, ShellSuppression, CraterRadius; public int SpreadTicks;
        public float Concentration; public int PersistTicks;
        /// <summary>A stepping pattern: this many steps, this many ticks and metres apart.</summary>
        public int Steps, StepTicks; public float StepMetres;
        /// <summary>The caller's own men this far behind a burst (against its direction) take nothing from it.</summary>
        public float SafeBehind;
        /// <summary>Bit p set: pattern p is offered. Bit 0 is always on.</summary>
        public int Patterns;
        public bool BreachesWire, CollapsesTrench, DestroysBunker;

        public bool Offers(int pattern) => pattern >= 0 && pattern <= AbilityArgs.PatternMask && (((Patterns | 1) >> pattern) & 1) != 0;
    }

    /// <summary>A payload waiting for its tick. All fields are 4 bytes wide, so the struct hashes without padding.</summary>
    public struct ScheduledPayload
    {
        public uint Tick;
        public int Ability;
        public int Player;
        public float3 Pos;
        /// <summary>A shell's flight direction, a source's line heading (unit XZ); zero for a point; a beam's heading times its length.</summary>
        public float3 Dir;
        /// <summary>A shell's burst radius; a source's concentration; a beam's half width.</summary>
        public float Radius;
        /// <summary>PayloadKind.</summary>
        public int Kind;
        /// <summary>A source: how many ticks it holds its cell. A beam: the ticks of its sweep. A shell: 0.</summary>
        public int Ticks;
    }

    public sealed class OffMapAbilitySystem : ISimSystem
    {
        public const int AbilitySlots = 16;   // indexed by OffMapAbilityId
        /// <summary>The shortest line a player can ask for; a longer one than the ability's Length is cut to it.</summary>
        public const float MinLength = 10f;
        public const int SmokeSources = 5, LiftShells = 4;
        public const float BoxHalfWidth = 8f, LiftAlongScatter = 2f, StrafeScatter = 1f;
        public const float CreepingGasConcentration = 30f; public const int CreepingGasTicks = 200;
        public int Order => SimSystemOrder.Command + 30;

        public NativeArray<int> Cooldown;               // player * AbilitySlots + ability id, ticks left
        public NativeList<ScheduledPayload> Scheduled;
        BlastSystem blast;
        GasSmokeSystem gas;
        BeamSystem beam;   // registered after this system's providers; null in a world without one (the Beam is then refused)

        /// <summary>Placeholder stats until C2 bakes TW.Data.AbilityDefinition; the numbers are the ones in docs/07.</summary>
        public static bool TryGetStats(int ability, out AbilityStats s)
        {
            switch ((OffMapAbilityId)ability)
            {
                case OffMapAbilityId.HeBarrage:
                    s = new AbilityStats { Id = OffMapAbilityId.HeBarrage, Cost = 150, CooldownTicks = 1200, WarmupTicks = 80, Target = AbilityTargetMode.Area,
                                           Radius = 25f, Length = 60f, HalfWidth = 4f, Shells = 12, ShellDamage = 150f, ShellRadius = 8f, ShellSuppression = 60f, CraterRadius = 3f, SpreadTicks = 120,
                                           Patterns = 1 << AbilityPattern.Disc | 1 << AbilityPattern.Line | 1 << AbilityPattern.Box, BreachesWire = true };
                    return true;
                case OffMapAbilityId.CreepingBarrage:
                    s = new AbilityStats { Id = OffMapAbilityId.CreepingBarrage, Cost = 250, CooldownTicks = 2400, WarmupTicks = 120, Target = AbilityTargetMode.Heading,
                                           Length = 60f, HalfWidth = 10f, Shells = 40, ShellDamage = 120f, ShellRadius = 8f, ShellSuppression = 60f, CraterRadius = 3f, SpreadTicks = 720,
                                           Steps = 10, StepTicks = 80, StepMetres = 6f, SafeBehind = 15f, BreachesWire = true };
                    return true;
                case OffMapAbilityId.ChlorineGas:
                    s = new AbilityStats { Id = OffMapAbilityId.ChlorineGas, Cost = 120, CooldownTicks = 1800, WarmupTicks = 60, Target = AbilityTargetMode.Point,
                                           Length = 64f, HalfWidth = 4f, Concentration = 40f, PersistTicks = 240, Steps = 8, StepTicks = 60, StepMetres = 8f,
                                           Patterns = 1 << AbilityPattern.Point | 1 << AbilityPattern.Creeping };
                    return true;
                case OffMapAbilityId.SmokeScreen:
                    s = new AbilityStats { Id = OffMapAbilityId.SmokeScreen, Cost = 60, CooldownTicks = 600, WarmupTicks = 40, Target = AbilityTargetMode.Line,
                                           Length = 40f, HalfWidth = 2f, Concentration = 30f, PersistTicks = 600 };
                    return true;
                case OffMapAbilityId.StrafeRun:
                    s = new AbilityStats { Id = OffMapAbilityId.StrafeRun, Cost = 180, CooldownTicks = 1800, WarmupTicks = 100, Target = AbilityTargetMode.Line,
                                           Length = 80f, HalfWidth = 3f, Shells = 32, ShellDamage = 60f, ShellRadius = 5f, ShellSuppression = 35f, CraterRadius = 0f, SpreadTicks = 40 };
                    return true;
                case OffMapAbilityId.Beam:
                    // SpreadTicks is the sweep: the head walks Length in that many ticks (BeamSystem)
                    s = new AbilityStats { Id = OffMapAbilityId.Beam, Cost = 300, CooldownTicks = 3600, WarmupTicks = 80, Target = AbilityTargetMode.Heading,
                                           Length = 60f, HalfWidth = 2f, SpreadTicks = 120 };
                    return true;
                default:
                    s = default;
                    return false;
            }
        }

        /// <summary>A Line or Heading ability is a line; so is every pattern but the plain one of a point or area ability.</summary>
        public static bool IsLine(in AbilityStats stats, int pattern)
            => stats.Target == AbilityTargetMode.Line || stats.Target == AbilityTargetMode.Heading || pattern != 0;

        /// <summary>The corridor half width the picture and the validation use for this ability and pattern.</summary>
        public static float HalfWidthOf(in AbilityStats stats, int pattern)
            => stats.Id == OffMapAbilityId.HeBarrage && pattern == AbilityPattern.Box ? BoxHalfWidth : stats.HalfWidth;

        public void Initialize(SimWorld world)
        {
            blast = world.GetSystem<BlastSystem>() ?? throw new System.InvalidOperationException("OffMapAbilitySystem needs BlastSystem registered before it");
            gas = world.GetSystem<GasSmokeSystem>() ?? throw new System.InvalidOperationException("OffMapAbilitySystem needs GasSmokeSystem registered before it");
            beam = world.GetSystem<BeamSystem>();
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
                AbilityArgs.Unpack(cmd.B, out int headingDeg, out int pattern, out int lengthM);
                int slot = cmd.Player * AbilitySlots + cmd.A;
                var size = w.Init.SizeMeters;
                bool inside = cmd.Pos.x >= 0f && cmd.Pos.z >= 0f && cmd.Pos.x <= size.x && cmd.Pos.z <= size.y;
                if (!inside || !stats.Offers(pattern) || Cooldown[slot] > 0 || w.Silver[cmd.Player] < stats.Cost) { w.Reject(cmd); continue; }
                if (stats.Id == OffMapAbilityId.Beam && (beam == null || beam.ActiveFor(cmd.Player) >= BeamSystem.MaxPerPlayer)) { w.Reject(cmd); continue; }   // two sweeps a player

                w.Silver[cmd.Player] -= stats.Cost;
                Cooldown[slot] = stats.CooldownTicks;
                float3 start = new float3(cmd.Pos.x, 0f, cmd.Pos.z);
                bool line = IsLine(stats, pattern);
                float3 dir = line ? AbilityArgs.Heading(headingDeg) : float3.zero;
                float len = line ? (lengthM > 0 ? math.clamp(lengthM, MinLength, stats.Length) : stats.Length) : 0f;
                float halfWidth = HalfWidthOf(stats, pattern);
                // the event says what shape is coming: dir = heading x length for a line (zero for a point), scalar =
                // an area's radius or a line's half width, so the picture can draw the corridor without the stats
                w.Events.Add(w.Tick, SimEventType.AbilityFired, cmd.A, cmd.Player, start, dir * len, line ? halfWidth : stats.Radius);
                Schedule(w, stats, cmd.Player, start, dir, len, pattern, halfWidth);
            }
            Deliver(w);
        }

        static Unity.Mathematics.Random Dice(SimWorld w, int player, int k)
            => SimRandom.For(w.Config.Seed, w.Tick, SimRandom.SystemId.Abilities, (uint)(player * 256 + k));

        void Add(SimWorld w, in AbilityStats stats, int player, uint tick, float3 pos, float3 dir, float radius, PayloadKind kind = PayloadKind.Shell, int ticks = 0)
            => Scheduled.Add(new ScheduledPayload { Tick = tick, Ability = (int)stats.Id, Player = player, Pos = w.ClampToMap(pos), Dir = dir, Radius = radius, Kind = (int)kind, Ticks = ticks });

        void Schedule(SimWorld w, in AbilityStats stats, int player, float3 start, float3 dir, float len, int pattern, float halfWidth)
        {
            uint warm = w.Tick + (uint)stats.WarmupTicks;
            float3 right = new float3(dir.z, 0f, -dir.x);   // 90 degrees clockwise of the heading, seen from above
            // the battery is off the map behind its own line, so a barrage shell arrives travelling up the field:
            // player 0 fires towards +Z, player 1 towards -Z. A strafe's bursts run along the aircraft's heading and a
            // creeping barrage's along its advance (its own men follow behind it).
            float3 upField = new float3(0f, 0f, player == 1 ? -1f : 1f);
            switch (stats.Id)
            {
                case OffMapAbilityId.HeBarrage:
                    for (int k = 0; k < stats.Shells; k++)
                    {
                        var rng = Dice(w, player, k);
                        float3 p;
                        if (pattern == AbilityPattern.Disc)
                        {
                            float angle = rng.NextFloat(0f, SimMath.TwoPi);
                            float r = stats.Radius * SimMath.Sqrt(rng.NextFloat());   // uniform over the disc
                            p = start + new float3(SimMath.Cos(angle) * r, 0f, SimMath.Sin(angle) * r);
                        }
                        else
                        {
                            // a line or a box: evenly along it in shell order, scattered across it
                            float along = (k + rng.NextFloat()) / stats.Shells * len;
                            p = start + dir * along + right * rng.NextFloat(-halfWidth, halfWidth);
                        }
                        Add(w, stats, player, warm + (uint)(k * stats.SpreadTicks / stats.Shells), p, upField, stats.ShellRadius);
                    }
                    break;
                case OffMapAbilityId.CreepingBarrage:
                {
                    int lifts = math.clamp((int)(len / stats.StepMetres), 1, stats.Steps);
                    for (int s = 0; s < lifts; s++)
                    for (int j = 0; j < LiftShells; j++)
                    {
                        var rng = Dice(w, player, s * LiftShells + j);
                        float3 p = start + dir * (s * stats.StepMetres + rng.NextFloat(-LiftAlongScatter, LiftAlongScatter)) + right * rng.NextFloat(-halfWidth, halfWidth);
                        Add(w, stats, player, warm + (uint)(s * stats.StepTicks), p, dir, stats.ShellRadius);
                    }
                    break;
                }
                case OffMapAbilityId.ChlorineGas:
                    if (pattern == AbilityPattern.Point)
                        Add(w, stats, player, warm, start, float3.zero, stats.Concentration, PayloadKind.GasSource, stats.PersistTicks);
                    else
                    {
                        int steps = math.clamp((int)(len / stats.StepMetres), 1, stats.Steps);
                        for (int s = 0; s < steps; s++)
                            Add(w, stats, player, warm + (uint)(s * stats.StepTicks), start + dir * (s * stats.StepMetres), dir, CreepingGasConcentration, PayloadKind.GasSource, CreepingGasTicks);
                    }
                    break;
                case OffMapAbilityId.SmokeScreen:
                    for (int k = 0; k < SmokeSources; k++)
                        Add(w, stats, player, warm, start + dir * ((k + 0.5f) * len / SmokeSources), dir, stats.Concentration, PayloadKind.SmokeSource, stats.PersistTicks);
                    break;
                case OffMapAbilityId.StrafeRun:
                    for (int k = 0; k < stats.Shells; k++)
                    {
                        var rng = Dice(w, player, k);
                        float3 p = start + dir * ((k + 0.5f) * len / stats.Shells) + right * rng.NextFloat(-StrafeScatter, StrafeScatter);
                        Add(w, stats, player, warm + (uint)(k * stats.SpreadTicks / stats.Shells), p, dir, stats.ShellRadius);
                    }
                    break;
                case OffMapAbilityId.Beam:
                    Add(w, stats, player, warm, start, dir * len, stats.HalfWidth, PayloadKind.BeamStart, stats.SpreadTicks);
                    break;
            }
        }

        /// <summary>Deliver what is due, keeping the rest in order.</summary>
        void Deliver(SimWorld w)
        {
            int keep = 0;
            for (int i = 0; i < Scheduled.Length; i++)
            {
                var p = Scheduled[i];
                if (p.Tick > w.Tick) { Scheduled[keep++] = p; continue; }
                TryGetStats(p.Ability, out var stats);
                switch ((PayloadKind)p.Kind)
                {
                    case PayloadKind.Shell:
                        blast.Queue(new Impact
                        {
                            Pos = p.Pos, Dir = p.Dir, Damage = stats.ShellDamage, Radius = p.Radius, Suppression = stats.ShellSuppression,
                            CraterRadius = stats.CraterRadius, CraterDepth = stats.CraterRadius > 0f ? 1.2f : 0f,
                            Source = p.Ability, Player = p.Player, SafeBehind = stats.SafeBehind,
                        });
                        break;
                    case PayloadKind.GasSource:
                        gas.AddSource(p.Pos, p.Radius, p.Ticks, p.Player);
                        w.Events.Add(w.Tick, SimEventType.GasCloudSpawned, p.Ability, p.Player, p.Pos, p.Dir, p.Radius);
                        break;
                    case PayloadKind.SmokeSource:
                        gas.AddSmokeSource(p.Pos, p.Radius, p.Ticks, p.Player);
                        w.Events.Add(w.Tick, SimEventType.SmokeSpawned, p.Ability, p.Player, p.Pos, p.Dir, p.Radius);
                        break;
                    case PayloadKind.BeamStart:
                    {
                        // the picture times the sweep from the AbilityFired tick and the stats; the sim starts it here
                        float len = SimMath.Length(p.Dir);
                        if (beam != null && len > 1e-3f) beam.Start(w, p.Pos, p.Dir / len, len, p.Radius, p.Ticks, p.Player, p.Ability);
                        break;
                    }
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
