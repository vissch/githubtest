// Phase: P0 (implemented)
// Replay file v2: header (magic, format version, config, world init, map id, map hash, data hash) + per tick
// (command count, commands, state hash). Bump FormatVersion whenever a contract in docs/02 or this layout changes;
// MapHash/DataHash let a player reject a replay recorded against different map or unit data instead of desyncing.
// Used by DeterminismReplayTests, the platform gate report, desync dumps (N3) and the spectator/replay UI.
using System.Collections.Generic;
using System.IO;
using Unity.Collections;
using Unity.Mathematics;

namespace TW.Sim
{
    public sealed class ReplayRecorder
    {
        public const uint Magic = 0x31525754; // "TWR1"
        // v4 (2026-09-24): MapData is folded into the tick hash by TerrainHashSystem -- the heightfield, the nav
        // layers and the holes the shells have dug are compared state now, which docs/03 deferred "to the next
        // replay-format break". Blast impacts also carry a direction and a shape.
        // v5 (2026-09-26): BurningSystem is registered (its per-slot fire and burning cells join the hash), a blast's
        // dead carry their knock in the Death event, and Impact has an Incendiary shape. Header layout unchanged.
        // v6 (2026-09-26): SupportFire.b carries AbilityArgs (heading, pattern, length); ScheduledPayload gains Dir,
        // Radius, Kind and Ticks (hashed); GasSmokeSystem's smoke field and its sources join the hash while a screen
        // is up; the HE scatter draws from SimRandom.SystemId.Abilities; Impact gains SafeBehind. Layout unchanged.
        // v7 (2026-09-26): BeamSystem is registered (its running sweeps join the hash); OffMapAbilityId.Beam = 11 is a
        // real ability whose BeamStart payload carries heading x length in Dir; BlastShape.Beam = 4. Layout unchanged.
        // v8 (2026-09-26): MineSystem is registered (mines and tripwires join the hash, order 1130); BlastShape.Mine = 5.
        // Before v8 landed anywhere (2026-09-27): BlastShape.Strafe = 6 (a strafe's rounds throw nobody) and the ability dice
        // are salted by ability id as well as player and round, and BurningSystem hashes every slot's timer. Layout unchanged.
        // v9 (2026-09-27, the units-meta lane landed on the overhaul): SimConfig carries FactionA/FactionB, HeroPity0/1 and
        // the ten archetypes each side chose (Loadout*), written as a count and that many bytes per side, so the header
        // layout changed; its systems (aura, support, hero, leap, breaker, air drop, the combat catalogue) join the hash.
        // That lane had numbered these v5 and v6 on its own branch; those numbers were the overhaul's by the time it landed.
        // v10 (2026-09-28): UnitDefinitions.All holds its first two units (the Skimmer 19 and the Salvo 20, fielded by
        // no faction), so the match's unit table, whose Fingerprint UnitCatalogue and CombatCatalogue fold into every
        // tick's hash, is not the one v9 recorded against. Layout and hash chain unchanged.
        // v11 (2026-09-28): the Salvo's shot is a rack of rockets held in the air (TankGunnerySystem.Rockets, hashed at the
        // end of that system's part of the chain) and each bursts on its own later tick; SimEventType.RocketFired;
        // SimRandom.SystemId.Salvo = 22; TankSpec gains Rockets and RocketSpeed (the combat table's fingerprint).
        // v12 (2026-09-28): TankSpec gains StandOff (the Salvo holds while it has a target) and VehicleProfile Clearance
        // (added to Radius; the Skimmer and Salvo keep 1.2 m more round them); the Salvo's rack is 16 rockets, not 12.
        // v13 (2026-09-28, the balance critic's round): TankGunnerySystem hashes the stand-off hold (HoldTarget, HoldTicks,
        // Release, HoldHp) after the rockets; TankSpec.StandOff (bool) became StandOffMetres/StandOffPatience; InfantrySpec
        // gains HuntsArmour and LooksDownMetres; a machine's armour-hunting small arms fire armour-piercing at machines.
        // v14 (2026-09-28, critic round 3): TankGunnerySystem hashes HoldGoal after HoldHp; the hold is reset for a new unit
        // in a dead machine's slot, and holds only on the machine's first goal.
        // v15 (2026-09-28, critic round 4): PendingRocket gains LaunchTick, Shooter, ShooterGen (hashed with it): a rocket
        // still in its tube when its machine dies is dropped; a rack's landing points are clamped to the map.
        // v16 (2026-09-28, the Proving Ground): sixteen definitions (archetypes 21-36) in UnitDefinitions.All and two
        // InfantrySpec fields (MineCharges, NeverPinned) change the unit tables' fingerprints from tick 0; the header gains
        // SimConfig.Endless; UnitAbilityId 13-14, SapperOrdered/SapperLaying and SimSystemOrder.Sapper appended, unused yet.
        // v17 (2026-09-28, the sapper): SapperSystem joins the chain at order 120 with its per-slot state (Charges, Phase,
        // Kind, Goal, Args, LayTicks, Back, Target); CommandType.UnitAbility is consumed (b = UnitAbilityId in the low
        // byte, AbilityArgs above it); MineSystem's trigger reads the match's vehicle profiles.
        // v18 (2026-09-28, spread and the fight): infantry cross a trench wall anywhere (FlowField.CanStepInfantry,
        // ParapetCost), every man on foot keeps to a lane (Lane) and is deployed on it, and EngageSystem joins the chain
        // at order 1108 with its state (Hunt, HuntGen, the generation seen, MovementSystem.Engage): men in the open
        // close on the enemy and hold to shoot. Layout unchanged; every battle runs differently from the first deploy.
        // v19 (2026-09-28, the assault): a running man in the open is a harder mark the farther off he is
        // (CombatTables.RunningTarget), a miss at a man on the fire step suppresses him fully (the parapet), and a man
        // in the open bombs the trench man he fights from 5-22 m (DirectFireSystem, whose hash now folds its per-slot
        // bombs and their generation; SimEventType.GrenadeThrown appended, SourceId.Grenade 2002). Layout unchanged.
        public const ushort FormatVersion = 19;
        public SimConfig Config;
        public SimConfig.WorldInit Init;
        public int MapId;
        public ulong MapHash;   // MapData.Hash() of the map the replay was recorded on (0 = unknown)
        public ulong DataHash;  // hash of the baked unit/weapon/ability tables (0 until C2 wires it)
        public byte[] MapParams = System.Array.Empty<byte>();   // what a generated map is rebuilt from (BattlefieldParams.Serialize); empty for fixed maps
        public readonly List<SimCommand[]> Commands = new List<SimCommand[]>();
        public readonly List<ulong> Hashes = new List<ulong>();

        public ReplayRecorder(SimConfig config, SimConfig.WorldInit init, int mapId, ulong mapHash = 0, ulong dataHash = 0)
        { Config = config; Init = init; MapId = mapId; MapHash = mapHash; DataHash = dataHash; }

        public void Record(NativeArray<SimCommand> tickCommands, ulong hashAfterStep)
        {
            Commands.Add(tickCommands.ToArray());
            Hashes.Add(hashAfterStep);
        }

        public byte[] Serialize()
        {
            using var ms = new MemoryStream();
            using var w = new BinaryWriter(ms);
            w.Write(Magic);
            w.Write(FormatVersion);
            w.Write(Config.TickRate); w.Write(Config.InputDelayTicks); w.Write(Config.MaxSlots); w.Write(Config.Seed);
            w.Write(Config.SilverPerSecond); w.Write(Config.StartingSilver); w.Write(Config.EventCapacity);
            w.Write(Config.FactionA); w.Write(Config.FactionB); w.Write(Config.HeroPity0); w.Write(Config.HeroPity1);
            w.Write(Config.Endless);   // v16
            WriteLoadout(w, Config.LoadoutA); WriteLoadout(w, Config.LoadoutB);
            WriteF3(w, new float3(Init.SizeMeters, 0f)); WriteF3(w, Init.SpawnA); WriteF3(w, Init.SpawnB); w.Write(Init.GoalZA); w.Write(Init.GoalZB);
            w.Write(MapId);
            w.Write(MapHash); w.Write(DataHash);
            w.Write(MapParams.Length); w.Write(MapParams);
            w.Write(Commands.Count);
            for (int t = 0; t < Commands.Count; t++)
            {
                var cs = Commands[t];
                w.Write(cs.Length);
                foreach (var c in cs)
                {
                    w.Write(c.Tick); w.Write(c.Player); w.Write((byte)c.Type); w.Write(c.A); w.Write(c.B); WriteF3(w, c.Pos);
                }
                w.Write(Hashes[t]);
            }
            return ms.ToArray();
        }

        static void WriteF3(BinaryWriter w, float3 v) { w.Write(v.x); w.Write(v.y); w.Write(v.z); }

        static void WriteLoadout(BinaryWriter w, in Unity.Collections.FixedList32Bytes<byte> l)
        {
            w.Write((byte)l.Length);
            for (int i = 0; i < l.Length; i++) w.Write(l[i]);
        }
    }

    public sealed class ReplayPlayer
    {
        public SimConfig Config;
        public SimConfig.WorldInit Init;
        public int MapId;
        public ushort FormatVersion;
        public ulong MapHash, DataHash;
        public byte[] MapParams;
        public SimCommand[][] Commands;
        public ulong[] Hashes;
        public int TickCount => Commands.Length;

        public static ReplayPlayer Parse(byte[] bytes)
        {
            using var ms = new MemoryStream(bytes);
            using var r = new BinaryReader(ms);
            if (r.ReadUInt32() != ReplayRecorder.Magic) throw new InvalidDataException("Not a TWR1 replay");
            var p = new ReplayPlayer();
            p.FormatVersion = r.ReadUInt16();
            if (p.FormatVersion != ReplayRecorder.FormatVersion)
                throw new InvalidDataException($"Replay format v{p.FormatVersion}, this build reads v{ReplayRecorder.FormatVersion}");
            p.Config = new SimConfig
            {
                TickRate = r.ReadInt32(), InputDelayTicks = r.ReadInt32(), MaxSlots = r.ReadInt32(), Seed = r.ReadUInt32(),
                SilverPerSecond = r.ReadSingle(), StartingSilver = r.ReadInt32(), EventCapacity = r.ReadInt32(),
                FactionA = r.ReadByte(), FactionB = r.ReadByte(), HeroPity0 = r.ReadSingle(), HeroPity1 = r.ReadSingle(),
                Endless = r.ReadBoolean(),   // v16
            };
            p.Config.LoadoutA = ReadLoadout(r); p.Config.LoadoutB = ReadLoadout(r);
            float3 size = ReadF3(r);
            p.Init = new SimConfig.WorldInit { SizeMeters = size.xy, SpawnA = ReadF3(r), SpawnB = ReadF3(r), GoalZA = r.ReadSingle(), GoalZB = r.ReadSingle() };
            p.MapId = r.ReadInt32();
            p.MapHash = r.ReadUInt64(); p.DataHash = r.ReadUInt64();
            p.MapParams = r.ReadBytes(r.ReadInt32());
            int ticks = r.ReadInt32();
            p.Commands = new SimCommand[ticks][];
            p.Hashes = new ulong[ticks];
            for (int t = 0; t < ticks; t++)
            {
                int n = r.ReadInt32();
                var cs = new SimCommand[n];
                for (int i = 0; i < n; i++)
                    cs[i] = new SimCommand { Tick = r.ReadUInt32(), Player = r.ReadByte(), Type = (CommandType)r.ReadByte(), A = r.ReadInt32(), B = r.ReadInt32(), Pos = ReadF3(r) };
                p.Commands[t] = cs;
                p.Hashes[t] = r.ReadUInt64();
            }
            return p;
        }

        /// <summary>Re-simulate and return the first tick whose hash differs, or -1 if the replay verifies.</summary>
        public int Verify(SimWorld world)
        {
            for (int t = 0; t < TickCount; t++)
            {
                using var cmds = new NativeArray<SimCommand>(Commands[t], Allocator.Temp);
                world.Step(cmds);
                if (world.LastHash != Hashes[t]) return t;
            }
            return -1;
        }

        static Unity.Collections.FixedList32Bytes<byte> ReadLoadout(BinaryReader r)
        {
            var l = new Unity.Collections.FixedList32Bytes<byte>();
            int n = r.ReadByte();
            for (int i = 0; i < n; i++) l.Add(r.ReadByte());
            return l;
        }

        static float3 ReadF3(BinaryReader r) => new float3(r.ReadSingle(), r.ReadSingle(), r.ReadSingle());
    }
}
