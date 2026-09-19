// Phase: P0 (implemented)
// Replay file v1: header (config, world init, map id) + per tick (command count, commands, state hash).
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
        public SimConfig Config;
        public SimConfig.WorldInit Init;
        public int MapId;
        public readonly List<SimCommand[]> Commands = new List<SimCommand[]>();
        public readonly List<ulong> Hashes = new List<ulong>();

        public ReplayRecorder(SimConfig config, SimConfig.WorldInit init, int mapId)
        { Config = config; Init = init; MapId = mapId; }

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
            w.Write(Config.TickRate); w.Write(Config.InputDelayTicks); w.Write(Config.MaxSlots); w.Write(Config.Seed);
            w.Write(Config.SilverPerSecond); w.Write(Config.StartingSilver); w.Write(Config.EventCapacity);
            WriteF3(w, new float3(Init.SizeMeters, 0f)); WriteF3(w, Init.SpawnA); WriteF3(w, Init.SpawnB); w.Write(Init.GoalZA); w.Write(Init.GoalZB);
            w.Write(MapId);
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
        public SimCommand[][] Commands;
        public ulong[] Hashes;
        public int TickCount => Commands.Length;

        public static ReplayPlayer Parse(byte[] bytes)
        {
            using var ms = new MemoryStream(bytes);
            using var r = new BinaryReader(ms);
            if (r.ReadUInt32() != ReplayRecorder.Magic) throw new InvalidDataException("Not a TWR1 replay");
            var p = new ReplayPlayer();
            p.Config = new SimConfig
            {
                TickRate = r.ReadInt32(), InputDelayTicks = r.ReadInt32(), MaxSlots = r.ReadInt32(), Seed = r.ReadUInt32(),
                SilverPerSecond = r.ReadSingle(), StartingSilver = r.ReadInt32(), EventCapacity = r.ReadInt32(),
            };
            float3 size = ReadF3(r);
            p.Init = new SimConfig.WorldInit { SizeMeters = size.xy, SpawnA = ReadF3(r), SpawnB = ReadF3(r), GoalZA = r.ReadSingle(), GoalZB = r.ReadSingle() };
            p.MapId = r.ReadInt32();
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
