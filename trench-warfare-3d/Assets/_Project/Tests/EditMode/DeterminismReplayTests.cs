// Phase: P0 (implemented) — the core determinism guarantee: same seed + same commands → identical hash sequence.
// The script also fires every line ability (docs/21 phase 5: a strafe, a smoke screen, a line barrage, creeping
// gas) early enough for their payloads and the smoke field to be in the hash the replay verifies.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class DeterminismReplayTests
    {
        static SimCommand Support(uint t, byte player, OffMapAbilityId id, int heading, int pattern, int length, float x, float z)
            => new SimCommand { Tick = t, Player = player, Type = CommandType.SupportFire, A = (int)id, B = AbilityArgs.Pack(heading, pattern, length), Pos = new float3(x, 0f, z) };

        static SimCommand[] ScriptedCommands(uint t)
        {
            if (t == 41) return new[] { Support(t, 0, OffMapAbilityId.StrafeRun, 90, 0, 60, 60f, 300f) };
            if (t == 46) return new[] { Support(t, 1, OffMapAbilityId.SmokeScreen, 0, 0, 40, 150f, 400f) };
            if (t == 51) return new[] { Support(t, 0, OffMapAbilityId.HeBarrage, 0, AbilityPattern.Line, 60, 150f, 500f) };
            if (t == 56) return new[] { Support(t, 1, OffMapAbilityId.ChlorineGas, 180, AbilityPattern.Creeping, 64, 150f, 300f) };
            if (t % 5 == 0) return new[] { SimCommand.Deploy(t, 0, (int)(t / 5) % 5), SimCommand.Deploy(t, 1, (int)(t / 5 + 2) % 5) };
            if (t % 37 == 0) return new[] { SimCommand.Rally(t, 0, new float3(100f + t, 0f, 50f)) };
            return System.Array.Empty<SimCommand>();
        }

        static ulong[] Run(int ticks, ReplayRecorder recorder = null)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 4000;
            using var match = MatchSim.CreateGreybox(cfg);
            var hashes = new ulong[ticks];
            for (uint t = 0; t < ticks; t++)
            {
                using var cmds = new NativeArray<SimCommand>(ScriptedCommands(t), Allocator.Temp);
                match.Step(cmds);
                hashes[t] = match.World.LastHash;
                recorder?.Record(cmds, match.World.LastHash);
            }
            return hashes;
        }

        [Test]
        public void SameSeedAndCommands_ProduceIdenticalHashes()
        {
            var a = Run(300);
            var b = Run(300);
            for (int i = 0; i < a.Length; i++) Assert.AreEqual(a[i], b[i], $"hash diverged at tick {i}");
            Assert.AreNotEqual(a[0], a[299], "state should change over time");
        }

        [Test]
        public void Replay_SerializesAndVerifies()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 4000;
            var recorder = new ReplayRecorder(cfg, default, TW.Sim.Terrain.GreyboxMapGenerator.MapId);
            Run(200, recorder);
            var bytes = recorder.Serialize();
            var player = ReplayPlayer.Parse(bytes);
            Assert.AreEqual(200, player.TickCount);
            using var match = MatchSim.CreateGreybox(player.Config);
            Assert.AreEqual(-1, player.Verify(match.World), "replay must re-simulate to the recorded hashes");
        }

        [Test]
        public void DifferentSeeds_DivergeOnDeployment()
        {
            var cfg1 = SimConfig.Default; cfg1.StartingSilver = 4000; cfg1.Seed = 1;
            var cfg2 = cfg1; cfg2.Seed = 2;
            using var m1 = MatchSim.CreateGreybox(cfg1);
            using var m2 = MatchSim.CreateGreybox(cfg2);
            using var cmds = new NativeArray<SimCommand>(new[] { SimCommand.Deploy(0, 0, 0) }, Allocator.Temp);
            m1.Step(cmds); m2.Step(cmds);
            Assert.AreNotEqual(m1.World.LastHash, m2.World.LastHash, "spawn jitter derives from the seed");
        }
    }
}
