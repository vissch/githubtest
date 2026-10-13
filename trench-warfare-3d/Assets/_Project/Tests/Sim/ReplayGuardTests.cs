// Phase: review fixes (2026-10-12) — the replay guards of finding S5.
// A replay only proves anything when it was recorded on a world that hashes every tick and is verified against a
// fresh world at tick 0. These three check that each of the ways round that throws instead of passing quietly.
using System;
using System.IO;
using NUnit.Framework;
using Unity.Collections;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class ReplayGuardTests
    {
        static ReplayPlayer OneTickReplay(SimConfig cfg)
        {
            var recorder = new ReplayRecorder(cfg, default, TW.Sim.Terrain.GreyboxMapGenerator.MapId);
            using var match = MatchSim.CreateGreybox(cfg);
            using var cmds = new NativeArray<SimCommand>(0, Allocator.Temp);
            match.Step(cmds);
            recorder.Record(cmds, match.World.LastHash);
            return ReplayPlayer.Parse(recorder.Serialize());
        }

        // The byte offset of LoadoutA's length prefix in ReplayRecorder.Serialize()'s header, for SimConfig.Default
        // (whose LoadoutA/B are both empty): Magic(4) + FormatVersion(2) + TickRate(4) + InputDelayTicks(4) +
        // MaxSlots(4) + Seed(4) + SilverPerSecond(4) + StartingSilver(4) + EventCapacity(4) + FactionA(1) +
        // FactionB(1) + HeroPity0(4) + HeroPity1(4) + Endless(1) = 45.
        const int LoadoutALengthOffset = 45;

        [Test]   // [S8]
        public void ParseThrowsInvalidDataExceptionWhenALoadoutCountOverrunsFixedList32Bytes()
        {
            var cfg = SimConfig.Default;
            var recorder = new ReplayRecorder(cfg, default, TW.Sim.Terrain.GreyboxMapGenerator.MapId);
            using var cmds = new NativeArray<SimCommand>(0, Allocator.Temp);
            recorder.Record(cmds, 1UL);
            var bytes = recorder.Serialize();
            Assert.AreEqual(0, bytes[LoadoutALengthOffset], "test assumes SimConfig.Default's LoadoutA is empty");
            bytes[LoadoutALengthOffset] = 255;   // 255 > FixedList32Bytes<byte>.Capacity (30): the overrun in [S8]
            var ex = Assert.Throws<InvalidDataException>(() => ReplayPlayer.Parse(bytes),
                "[S8] a loadout count past FixedList32Bytes<byte>.Capacity must raise InvalidDataException, not whatever FixedList32Bytes itself throws on overrun");
            StringAssert.Contains("255", ex.Message);
        }

        [Test]   // [S8]
        public void ParseThrowsOnTheOverrunBeforeTouchingTheList()
        {
            var cfg = SimConfig.Default;
            var recorder = new ReplayRecorder(cfg, default, TW.Sim.Terrain.GreyboxMapGenerator.MapId);
            using var cmds = new NativeArray<SimCommand>(0, Allocator.Temp);
            recorder.Record(cmds, 1UL);
            var bytes = recorder.Serialize();
            bytes[LoadoutALengthOffset] = (byte)(new Unity.Collections.FixedList32Bytes<byte>().Capacity + 1);
            Assert.Throws<InvalidDataException>(() => ReplayPlayer.Parse(bytes),
                "[S8] one past capacity must still be rejected, not just the extreme 255 case");
        }

        [Test]
        public void ParseStillReadsAGoodLoadoutWithinCapacity()
        {
            var cfg = SimConfig.Default;
            cfg.LoadoutA = new Unity.Collections.FixedList32Bytes<byte>();
            cfg.LoadoutA.Add(0); cfg.LoadoutA.Add(1); cfg.LoadoutA.Add(2);
            var recorder = new ReplayRecorder(cfg, default, TW.Sim.Terrain.GreyboxMapGenerator.MapId);
            using var cmds = new NativeArray<SimCommand>(0, Allocator.Temp);
            recorder.Record(cmds, 1UL);
            var player = ReplayPlayer.Parse(recorder.Serialize());
            Assert.AreEqual(3, player.Config.LoadoutA.Length, "the bounds check must not reject a good loadout");
        }

        [Test]   // [S5]
        public void VerifyRefusesAWorldThatDoesNotHashEveryTick()
        {
            var cfg = SimConfig.Default;
            var player = OneTickReplay(cfg);
            using var match = MatchSim.CreateGreybox(cfg);
            match.World.HashInterval = 0;
            Assert.Throws<ArgumentException>(() => player.Verify(match.World),
                "[S5] a world that never hashes reports a good replay as a desync at tick 0");
        }

        [Test]   // [S5]
        public void VerifyRefusesAWorldThatHasAlreadyStepped()
        {
            var cfg = SimConfig.Default;
            var player = OneTickReplay(cfg);
            using var match = MatchSim.CreateGreybox(cfg);
            using var cmds = new NativeArray<SimCommand>(0, Allocator.Temp);
            match.Step(cmds);
            Assert.Throws<ArgumentException>(() => player.Verify(match.World),
                "[S5] a used world cannot re-simulate a recording from its start");
        }

        [Test]   // [S5]
        public void RecordRefusesTheZeroHash()
        {
            var cfg = SimConfig.Default;
            var recorder = new ReplayRecorder(cfg, default, TW.Sim.Terrain.GreyboxMapGenerator.MapId);
            using var cmds = new NativeArray<SimCommand>(0, Allocator.Temp);
            Assert.Throws<ArgumentException>(() => recorder.Record(cmds, 0),
                "[S5] an all-zero recording verifies against anything: a verify that cannot fail");
        }
    }
}
