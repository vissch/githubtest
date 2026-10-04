// Phase: P0 (implemented) — the core determinism guarantee: same seed + same commands → identical hash sequence.
// The script also fires every line ability (docs/21 phase 5: a strafe, a smoke screen, a line barrage, creeping
// gas, and the beam, which lights what it crosses) early enough for their payloads, the smoke field, the sweep and
// the burning to be in the hash the replay verifies.
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
            if (t == 61) return new[] { Support(t, 0, OffMapAbilityId.Beam, 45, 0, 60, 120f, 350f) };
            if (t == 66) return new[] { Support(t, 1, OffMapAbilityId.CreepingBarrage, 180, 0, 60, 150f, 450f) };
            if (t == SapperDeployTick) return new[] { SimCommand.Deploy(t, 0, SapperSlot) };
            if (t % 5 == 0) return new[] { SimCommand.Deploy(t, 0, (int)(t / 5) % 5), SimCommand.Deploy(t, 1, (int)(t / 5 + 2) % 5) };
            if (t % 37 == 0) return new[] { SimCommand.Rally(t, 0, new float3(100f + t, 0f, 50f)) };
            return System.Array.Empty<SimCommand>();
        }

        static bool beamFired;
        static bool Fired(MatchSim m, int ability)
        {
            var ev = m.World.Events.Events;
            for (int i = 0; i < ev.Length; i++) if (ev[i].Type == SimEventType.AbilityFired && ev[i].A == ability) return true;
            return false;
        }

        static bool minesLaid;

        // ---- the sapper (2026-09-28): deployed and ordered by command, so the SERIALIZED replay verifies him too ----
        const int SapperSlot = 9;
        const uint SapperDeployTick = 26, SapperOrderTick = 33;
        static bool sapperLaid;

        /// <summary>Player 0's faction ten with the sapper in the last slot.</summary>
        static SimConfig Config()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 4000;
            var ten = new FixedList32Bytes<byte>();
            for (int s = 0; s < RosterEntry.SlotCount; s++) ten.Add(s == SapperSlot ? InfantryArchetype.Sapper : FactionRoster.Slot(cfg.FactionOf(0), s).Archetype);
            cfg.LoadoutA = ten;
            return cfg;
        }

        /// <summary>The order a player would give the sapper standing on the field: a mine a few metres from him.</summary>
        static SimCommand[] WithSapperOrder(MatchSim match, uint t, SimCommand[] scripted)
        {
            if (t != SapperOrderTick) return scripted;
            var w = match.World;
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!w.IsAlive(i) || w.Team[i] != 0 || w.Archetype[i] != InfantryArchetype.Sapper) continue;
                foreach (var off in new[] { new float3(0f, 0f, 6f), new float3(6f, 0f, 0f), new float3(-6f, 0f, 0f), new float3(0f, 0f, -6f) })
                {
                    if (!match.Mines.Lies(w.Position[i] + off)) continue;
                    var all = new SimCommand[scripted.Length + 1];
                    scripted.CopyTo(all, 0);
                    all[scripted.Length] = new SimCommand { Tick = t, Player = 0, Type = CommandType.UnitAbility, A = i, B = (int)TW.Sim.Units.UnitAbilityId.LayMine, Pos = w.Position[i] + off };
                    return all;
                }
            }
            return scripted;
        }

        static bool Laid(MatchSim m, int player)
        {
            var ev = m.World.Events.Events;
            for (int i = 0; i < ev.Length; i++) if (ev[i].Type == SimEventType.MinePlaced && ev[i].B == player) return true;
            return false;
        }

        /// <summary>The seams no command reaches (docs/21: one replay case per seam): a tripwire and a mine of player 1 and
        /// burning ground, by system call, the same in both runs. Only the two-run comparison does this; a serialized
        /// replay re-simulates commands alone, and the sapper's mine (player 0, below) is in it.</summary>
        static void Seams(MatchSim match, uint t)
        {
            var w = match.World;
            if (t == 30)
            {
                var mines = w.GetSystem<TW.Sim.Combat.MineSystem>();
                int a = mines.Place(w, new float3(100f, 0f, 300f), float3.zero, 0f, 1, TW.Sim.Combat.MineKind.Mine);
                int b = mines.Place(w, new float3(110f, 0f, 320f), new float3(1f, 0f, 0f), 6f, 1, TW.Sim.Combat.MineKind.Tripwire);
                minesLaid |= a >= 0 && b >= 0;
            }
            if (t == 35) w.GetSystem<TW.Sim.Combat.BurningSystem>().IgniteCell(w, new float3(90f, 0f, 280f), 8f, 0);
        }

        static ulong[] Run(int ticks, ReplayRecorder recorder = null)
        {
            var cfg = Config();
            using var match = MatchSim.CreateGreybox(cfg);
            var hashes = new ulong[ticks];
            for (uint t = 0; t < ticks; t++)
            {
                if (recorder == null) Seams(match, t);
                using var cmds = new NativeArray<SimCommand>(WithSapperOrder(match, t, ScriptedCommands(t)), Allocator.Temp);
                match.Step(cmds);
                sapperLaid |= Laid(match, 0);   // the seams' mines are player 1's: a mine of player 0 is the sapper's
                if (t == 61) beamFired |= Fired(match, (int)OffMapAbilityId.Beam);   // the script's beam was accepted, not silently rejected
                hashes[t] = match.World.LastHash;
                recorder?.Record(cmds, match.World.LastHash);
            }
            return hashes;
        }

        [Test, Category("Long")]
        public void SameSeedAndCommands_ProduceIdenticalHashes()
        {
            beamFired = false; minesLaid = false; sapperLaid = false;
            var a = Run(300);
            Assert.IsTrue(sapperLaid, "the sapper deployed at t = 26 and ordered at t = 33 laid his mine: SapperSystem is in the verified hash");
            var b = Run(300);
            Assert.IsTrue(beamFired, "the beam at t = 61 fired: the sweep and the burning are in the verified hash");
            Assert.IsTrue(minesLaid, "the mine and the tripwire were laid: the mine field is in the verified hash");
            for (int i = 0; i < a.Length; i++) Assert.AreEqual(a[i], b[i], $"hash diverged at tick {i}");
            Assert.AreNotEqual(a[0], a[299], "state should change over time");
        }

        [Test]
        public void Replay_SerializesAndVerifies()
        {
            var cfg = Config();
            var recorder = new ReplayRecorder(cfg, default, TW.Sim.Terrain.GreyboxMapGenerator.MapId);
            sapperLaid = false;
            Run(200, recorder);
            Assert.IsTrue(sapperLaid, "the recorded match has the sapper's mine in it, laid through commands alone");
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
