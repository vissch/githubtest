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
        public const ushort FormatVersion = 7;
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
            };
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

        static float3 ReadF3(BinaryReader r) => new float3(r.ReadSingle(), r.ReadSingle(), r.ReadSingle());
    }
}
