// Phase: P0 (implemented) — the core determinism guarantee: same seed + same commands → identical hash sequence.
// The script also fires every line ability (docs/21 phase 5: a strafe, a smoke screen, a line barrage, creeping
// gas, the beam, which lights what it crosses, and a creeping barrage) early enough for their payloads, the smoke
// field, the sweep and the burning to be in the hash the replay verifies. [T10] every one of the six is asserted
// to have been accepted (not silently rejected), and the line barrage is aimed at player 1's deployment point, so
// the recorded run wounds somebody: the damage is in the verified hash too.
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

        /// <summary>[T10] The middle of a player's living men, or his deployment point while he has none.</summary>
        static float3 MiddleOfTheirMen(MatchSim match, byte player)
        {
            var w = match.World;
            float3 sum = float3.zero; int n = 0;
            for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && w.Team[i] == player) { sum += w.Position[i]; n++; }
            return n > 0 ? sum / n : (player == 0 ? w.Init.SpawnA : w.Init.SpawnB);
        }

        static SimCommand[] ScriptedCommands(MatchSim match, uint t)
        {
            if (t == 41) return new[] { Support(t, 0, OffMapAbilityId.StrafeRun, 90, 0, 60, 60f, 300f) };
            if (t == 46) return new[] { Support(t, 1, OffMapAbilityId.SmokeScreen, 0, 0, 40, 150f, 400f) };
            // [T10] on player 1's men, not an empty corner of the map, so the barrage wounds somebody. Their middle
            // is read from the world, so both runs of the same script aim at the same metre, and a replay re-simulates
            // the command's recorded position.
            if (t == 51) { var c = MiddleOfTheirMen(match, 1); return new[] { Support(t, 0, OffMapAbilityId.HeBarrage, 0, AbilityPattern.Line, 60, c.x, c.z) }; }
            if (t == 56) return new[] { Support(t, 1, OffMapAbilityId.ChlorineGas, 180, AbilityPattern.Creeping, 64, 150f, 300f) };
            if (t == 61) return new[] { Support(t, 0, OffMapAbilityId.Beam, 45, 0, 60, 120f, 350f) };
            if (t == 66) return new[] { Support(t, 1, OffMapAbilityId.CreepingBarrage, 180, 0, 60, 150f, 450f) };
            if (t == SapperDeployTick) return new[] { SimCommand.Deploy(t, 0, SapperSlot) };
            if (t % 5 == 0) return new[] { SimCommand.Deploy(t, 0, (int)(t / 5) % 5), SimCommand.Deploy(t, 1, (int)(t / 5 + 2) % 5) };
            if (t % 37 == 0) return new[] { SimCommand.Rally(t, 0, new float3(100f + t, 0f, 50f)) };
            return System.Array.Empty<SimCommand>();
        }

        // [T10] every scripted ability, not the beam alone: one flag each, so a silently rejected command fails
        static readonly OffMapAbilityId[] ScriptedAbilities =
        {
            OffMapAbilityId.StrafeRun, OffMapAbilityId.SmokeScreen, OffMapAbilityId.HeBarrage,
            OffMapAbilityId.ChlorineGas, OffMapAbilityId.Beam, OffMapAbilityId.CreepingBarrage,
        };
        static readonly bool[] abilityFired = new bool[ScriptedAbilities.Length];
        static bool somebodyWasHurt;
        static readonly float[] lastHp = new float[4096];   // [T10] every man's hit points last tick, 0 when he was not there

        static bool Fired(MatchSim m, int ability)
        {
            var ev = m.World.Events.Events;
            for (int i = 0; i < ev.Length; i++) if (ev[i].Type == SimEventType.AbilityFired && ev[i].A == ability) return true;
            return false;
        }

        /// <summary>[T10] OR the per-ability flags of this tick's events into the statics, and watch every man for
        /// damage: fewer hit points than he had last tick, or alive last tick and gone now. Hit points, not a Hit or
        /// Death event: the event buffer of this busy script can overflow and drop them (SimEvents.cs:1-3).</summary>
        static void Watch(MatchSim m)
        {
            for (int k = 0; k < ScriptedAbilities.Length; k++) abilityFired[k] |= Fired(m, (int)ScriptedAbilities[k]);
            var w = m.World;
            for (int i = 0; i < w.HighWater && i < lastHp.Length; i++)
            {
                bool alive = w.IsAlive(i);
                if (alive && lastHp[i] > 0f && w.Hp[i] < lastHp[i]) somebodyWasHurt = true;
                if (!alive && lastHp[i] > 0f) somebodyWasHurt = true;
                lastHp[i] = alive ? w.Hp[i] : 0f;
            }
        }

        static void ResetWatch()
        {
            for (int k = 0; k < abilityFired.Length; k++) abilityFired[k] = false;
            for (int i = 0; i < lastHp.Length; i++) lastHp[i] = 0f;
            somebodyWasHurt = false;
        }

        static void AssertEveryAbilityFired()
        {
            for (int k = 0; k < ScriptedAbilities.Length; k++)
                Assert.IsTrue(abilityFired[k], $"[T10] the scripted {ScriptedAbilities[k]} was never accepted: its payload is in the verified hash");
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
            // a wreck in the line barrage's path (t == 51): its hit points and stages (2026-09-28) are hashed with the map
            if (t == 32) match.Map.AddProp(new TW.Sim.Terrain.PropDef { Pos = new float3(150f, 0f, 520f), Kind = TW.Sim.Terrain.PropKind.Wreck, Scale = 1f });
        }

        static ulong[] Run(int ticks, ReplayRecorder recorder = null)
        {
            var cfg = Config();
            using var match = MatchSim.CreateGreybox(cfg);
            for (int i = 0; i < lastHp.Length; i++) lastHp[i] = 0f;   // [T10] a fresh world: nobody was there last tick
            var hashes = new ulong[ticks];
            for (uint t = 0; t < ticks; t++)
            {
                if (recorder == null) Seams(match, t);
                using var cmds = new NativeArray<SimCommand>(WithSapperOrder(match, t, ScriptedCommands(match, t)), Allocator.Temp);
                match.Step(cmds);
                sapperLaid |= Laid(match, 0);   // the seams' mines are player 1's: a mine of player 0 is the sapper's
                Watch(match);
                hashes[t] = match.World.LastHash;
                recorder?.Record(cmds, match.World.LastHash);
            }
            return hashes;
        }

        [Test, Category("Long")]
        public void SameSeedAndCommands_ProduceIdenticalHashes()
        {
            ResetWatch(); minesLaid = false; sapperLaid = false;
            var a = Run(300);
            Assert.IsTrue(sapperLaid, "the sapper deployed at t = 26 and ordered at t = 33 laid his mine: SapperSystem is in the verified hash");
            var b = Run(300);
            AssertEveryAbilityFired();
            Assert.IsTrue(somebodyWasHurt, "[T10] the scripted run hits somebody: the damage is in the verified hash");
            Assert.IsTrue(minesLaid, "the mine and the tripwire were laid: the mine field is in the verified hash");
            for (int i = 0; i < a.Length; i++) Assert.AreEqual(a[i], b[i], $"hash diverged at tick {i}");
            Assert.AreNotEqual(a[0], a[299], "state should change over time");
        }

        [Test]
        public void Replay_SerializesAndVerifies()
        {
            var cfg = Config();
            using var recording = MatchSim.CreateGreybox(cfg);
            var recorder = recording.NewReplayRecorder();
            sapperLaid = false; ResetWatch();
            for (uint t = 0; t < 200; t++)
            {
                using var cmds = new NativeArray<SimCommand>(WithSapperOrder(recording, t, ScriptedCommands(recording, t)), Allocator.Temp);
                recording.Step(cmds);
                sapperLaid |= Laid(recording, 0);
                Watch(recording);
                recorder.Record(cmds, recording.World.LastHash);
            }
            Assert.IsTrue(sapperLaid, "the recorded match has the sapper's mine in it, laid through commands alone");
            AssertEveryAbilityFired();
            Assert.IsTrue(somebodyWasHurt, "[T10] the recorded run hits somebody: the damage is in the hashes the replay verifies");
            var bytes = recorder.Serialize();
            var player = ReplayPlayer.Parse(bytes);
            Assert.AreEqual(200, player.TickCount);
            // [T4] every recorded hash is real: an all-zero recording (a world that does not hash every tick)
            // verifies against any run at all, so the -1 below would prove nothing.
            for (int t = 0; t < player.TickCount; t++)
                Assert.AreNotEqual(0UL, player.Hashes[t], $"[T4] the recorded hash of tick {t} is 0: a verify that cannot fail");
            using var match = MatchSim.CreateGreybox(player.Config);
            Assert.AreEqual(-1, match.VerifyReplay(player), "replay must re-simulate to the recorded hashes");
            // [T4] the flipped-hash control: one bit off at tick 100 must be found, and found exactly there.
            var flipped = ReplayPlayer.Parse(bytes);
            flipped.Hashes[100] ^= 1;
            using var control = MatchSim.CreateGreybox(flipped.Config);
            Assert.AreEqual(100, control.VerifyReplay(flipped), "[T4] a replay with one hash bit flipped at tick 100 must report a desync at tick 100");
            // [T4] the world the verify above consumed is spent: verifying again must throw, not quietly re-run
            // from the middle (the S5 guard).
            Assert.Throws<System.ArgumentException>(() => match.VerifyReplay(player),
                "[T4] a verify on an already-simulated world must be refused");
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
