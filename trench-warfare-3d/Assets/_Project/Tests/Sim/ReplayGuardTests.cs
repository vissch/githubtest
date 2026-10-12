// Phase: review fixes (2026-10-12) — the replay guards of finding S5.
// A replay only proves anything when it was recorded on a world that hashes every tick and is verified against a
// fresh world at tick 0. These three check that each of the ways round that throws instead of passing quietly.
using System;
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
