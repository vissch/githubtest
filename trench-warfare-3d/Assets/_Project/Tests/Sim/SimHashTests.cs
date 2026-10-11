// Phase: P0 (implemented)
using NUnit.Framework;
using Unity.Collections;
using TW.Sim;
using Unity.Mathematics;

namespace TW.Tests
{
    public class SimHashTests
    {
        [Test]
        public void EqualArrays_HashEqual_DifferentArrays_HashDiffer()
        {
            using var a = new NativeArray<float>(new[] { 1f, 2f, 3f }, Allocator.Temp);
            using var b = new NativeArray<float>(new[] { 1f, 2f, 3f }, Allocator.Temp);
            using var c = new NativeArray<float>(new[] { 1f, 2f, 3.0001f }, Allocator.Temp);
            Assert.AreEqual(SimHash.Array(a, SimHash.Offset), SimHash.Array(b, SimHash.Offset));
            Assert.AreNotEqual(SimHash.Array(a, SimHash.Offset), SimHash.Array(c, SimHash.Offset));
        }

        [Test]
        public void SimRandom_IsStableForSameInputs()
        {
            var r1 = SimRandom.For(42, 10, SimRandom.SystemId.Test, 3);
            var r2 = SimRandom.For(42, 10, SimRandom.SystemId.Test, 3);
            Assert.AreEqual(r1.NextUInt(), r2.NextUInt());
            var r3 = SimRandom.For(42, 11, SimRandom.SystemId.Test, 3);
            Assert.AreNotEqual(SimRandom.For(42, 10, SimRandom.SystemId.Test, 3).NextUInt(), r3.NextUInt());
        }

        [Test]
        public void SimMath_TrigMatchesReferenceWithinTolerance()
        {
            for (int i = -720; i <= 720; i++)
            {
                float x = i * 0.5f * Unity.Mathematics.math.PI / 180f * 4f;
                Assert.AreEqual(System.Math.Sin(x), SimMath.Sin(x), 2e-5, $"sin({x})");
                Assert.AreEqual(System.Math.Cos(x), SimMath.Cos(x), 2e-5, $"cos({x})");
            }
            Assert.AreEqual(System.Math.Atan2(1, 2), SimMath.Atan2(1f, 2f), 1e-4);
            Assert.AreEqual(System.Math.Atan2(-3, -0.5), SimMath.Atan2(-3f, -0.5f), 1e-4);
        }

        /// <summary>The systems whose state SimWorld.Hash() folds, in the order it folds them, as FormatVersion 37 has
        /// them in the greybox match. Two SIM lanes that each add a system merge without a conflict into a chain
        /// neither of them ran, and a determinism rerun then compares the merged build with itself and passes (the
        /// overhaul and units-meta, 2026-09-27). This fails instead: a change to the chain is a format change, so it
        /// bumps ReplayRecorder.FormatVersion and pins the new chain here in the same commit.</summary>
        // v9's chain held until v16 (v10-v16 changed what the tables hold and what TankGunnerySystem hashes); v17 put the
        // sapper in at 120, v18 the fight on foot at 1108, v19 DirectFireSystem's bombs (same order), v20 dead ground (no new state). Named for what it is, not for a version it no longer matches.
        const string SystemChain =
            "40 CombatCatalogueSystem;50 TerrainHashSystem;110 TrenchOrdersSystem;120 SapperSystem;130 OffMapAbilitySystem;" +
            "400 FlowFieldManager;600 TargetAcquisitionSystem;650 AuraSystem;700 DirectFireSystem;" +
            "705 TankGunnerySystem;715 AmbientBombardmentSystem;720 BlastSystem;723 BeamSystem;" +
            "725 BurningSystem;730 VehicleModulesSystem;735 SupportSystem;800 SuppressionSystem;" +
            "805 HeroSystem;820 TrenchGarrisonSystem;900 GasSmokeSystem;1000 DeformationSystem;" +
            "1105 LeapSystem;1108 EngageSystem;1109 MeleeSystem;1110 MovementSystem;1115 BreakerSystem;1120 VehicleKinematicsSystem;" +
            "1125 PounceSystem;1130 MineSystem;1200 SectorControlSystem;";

        [Test]
        public void TheHashChainIsTheOneItsFormatVersionNames()
        {
            using var match = TW.Sim.Match.MatchSim.CreateGreybox(SimConfig.Default);
            var chain = new System.Text.StringBuilder();
            foreach (var s in match.World.Systems) chain.Append(s.Order).Append(' ').Append(s.GetType().Name).Append(';');
            Assert.AreEqual(45, ReplayRecorder.FormatVersion, "a new FormatVersion pins its own chain here");
            Assert.AreEqual(SystemChain, chain.ToString(), "the hash chain changed: bump ReplayRecorder.FormatVersion and pin this chain: " + chain);
        }

        const int Seed = 1917;

        /// <summary>What the scripted match actually did, so a pin cannot pass on an empty world.</summary>
        struct Fight { public int Vehicles, Hits, Deaths, Wounded; }

        /// <summary>Both golden pins run this: six deploys at tick 0 (rifleman, machinegunner and the faction's
        /// machine for each side), two barrages a side at tick 20 aimed where a man will be when the shells land,
        /// and plain ticks for the rest. Returns what happened, which the pins assert on before the hash.</summary>
        static Fight Script(TW.Sim.Match.MatchSim m, int ticks = 300)
        {
            var w = m.World;
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            var fight = new Fight();
            for (int t = 0; t < ticks; t++)
            {
                if (t == 0 || t == 20)
                {
                    var cmds = t == 0 ? Deploys(w) : Barrages(w);
                    using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
                    m.Step(arr);
                }
                else m.Step(none);
                var ev = w.Events.Events;
                for (int e = 0; e < ev.Length; e++)        // the buffer is cleared every tick, so read it every tick
                {
                    if (ev[e].Type == SimEventType.Hit) fight.Hits++;
                    else if (ev[e].Type == SimEventType.Death) fight.Deaths++;
                }
            }
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!w.IsAlive(i)) continue;
                if (w.Hp[i] < w.MaxHp[i]) fight.Wounded++;    // a blast wounds without a Hit event
                if ((w.Flags[i] & (uint)UnitFlags.Vehicle) != 0) fight.Vehicles++;
            }
            return fight;
        }

        /// <summary>Roster slots 0 (rifleman) and 2 (machinegunner) are the same in every faction; 6 is the
        /// faction's first machine (Maw for Iron, Tusk for Brass).</summary>
        static SimCommand[] Deploys(SimWorld w)
        {
            var list = new System.Collections.Generic.List<SimCommand>();
            for (byte p = 0; p < 2; p++)
                foreach (int slot in new[] { 0, 2, 6 })
                    list.Add(SimCommand.Deploy(w.Tick, p, slot));
            return list.ToArray();
        }

        /// <summary>HE and gas from both sides, aimed 12 m along an enemy foot soldier's line of advance (about how
        /// far he walks during HeBarrage's 80-tick warmup). Both are open to every faction (FactionRoster.AbilityMask);
        /// ParaDrop is Brass's alone and is refused near the enemy rear, so it is left out. A player with no enemy on
        /// foot yet (the Landing's sea side is still aboard) shells his own lead man's path instead, so the shells
        /// always fall on somebody.</summary>
        static SimCommand[] Barrages(SimWorld w)
        {
            var list = new System.Collections.Generic.List<SimCommand>();
            for (byte p = 0; p < 2; p++)
            {
                int target = Lead(w, 1 - p);
                if (target < 0) target = Lead(w, p);
                if (target < 0) continue;
                var at = w.ClampToMap(w.Position[target] + new float3(0f, 0f, w.Team[target] == 0 ? 12f : -12f));
                list.Add(new SimCommand { Tick = w.Tick, Player = p, Type = CommandType.SupportFire, Pos = at,
                    A = (int)TW.Sim.Match.OffMapAbilityId.HeBarrage });
                list.Add(new SimCommand { Tick = w.Tick, Player = p, Type = CommandType.SupportFire, Pos = at,
                    A = (int)TW.Sim.Match.OffMapAbilityId.ChlorineGas });
            }
            return list.ToArray();
        }

        /// <summary>The lowest alive on-foot slot of a team, or -1.</summary>
        static int Lead(SimWorld w, int team)
        {
            for (int i = 0; i < w.HighWater; i++)
                if (w.IsAlive(i) && (w.Flags[i] & (uint)UnitFlags.Vehicle) == 0 && (w.Team[i] & 1) == (team & 1)) return i;
            return -1;
        }

        /// <summary>[T24c] The greybox the golden pin runs on. MatchSim.CreateGreybox takes GreyboxMapGenerator's
        /// default 300 x 800 m corridor, which spawns the two sides at z=10 and z=790 and digs their trenches at
        /// z=120 and z=680: 780 m apart, while the longest gun in CombatTables reaches 184 m and a rifleman covers
        /// at most 45 m in the 300 hashed ticks (15 s). Nobody ever fired a shot, so a change to DirectFire,
        /// TankGunnery, Engage, Melee, Pounce or TrenchGarrison left the pin green. A 400 m corridor (trenches at
        /// z=120 and z=280) with both sides deployed at z=180 and z=220 puts them inside gun range at tick 0. A
        /// rally point is not an option: GoalKey.Rally is built nowhere (FlowFieldManager.DefaultGoal), so SetRally
        /// moves nobody.</summary>
        static TW.Sim.Match.MatchSim ScriptedGreybox()
        {
            var map = TW.Sim.Terrain.GreyboxMapGenerator.Create(Allocator.Persistent, 300f, 400f);
            for (int i = 0; i < map.Spawns.Length; i++)
            {
                var sp = map.Spawns[i];
                sp.Pos = new float3(150f, 0f, sp.Team == 0 ? 180f : 220f);
                map.Spawns[i] = sp;
            }
            return new TW.Sim.Match.MatchSim(ScriptedConfig(), map);   // MatchSim.Dispose frees the map
        }

        /// <summary>The config both pins run on: enough silver for six deploys and two abilities a side (about 725
        /// for Iron), so no deploy and no barrage is ever refused for want of it.</summary>
        static SimConfig ScriptedConfig()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 2000; return cfg;
        }

        /// <summary>
        /// [T24] Golden-hash pin: TheHashChainIsTheOneItsFormatVersionNames guards the ORDER of systems, but says
        /// nothing about what any of them actually hash, so a system that silently stopped folding a field in would
        /// pass it and still desync a real match. This pins the value itself.
        ///
        /// [T24b] The match is SCRIPTED (Script above), not idle: with an empty command array nothing ever spawns,
        /// HighWater stays 0, and the 23 per-unit arrays SimWorld.Hash() folds at SimWorld.cs:417-439 fold zero
        /// bytes - a field deleted, inserted or reordered among them left both pins green through seven seam
        /// commits (replay v38 to v44). The asserts below say men exist, a machine is on the field and somebody was
        /// hit BEFORE the hash is compared, so the match cannot go idle again unnoticed.
        ///
        /// To re-pin after a deliberate change, bump ReplayRecorder.FormatVersion and paste the new value below.
        /// [T24e] NUnit's Assert.AreEqual prints a ulong in DECIMAL, not hex, so the failure never says
        /// "But was: 0x...". A real one from this change read
        ///   Expected: 4683619924441667745
        ///   But was:  6938716348497752424
        /// Take the decimal after "But was:" and convert it - python -c "print(hex(6938716348497752424))" - and
        /// paste that 0x... literal below.
        /// [T24e] Take it from a real editor run only, never from occ.py / otr.py: those run managed C# with no
        /// Burst codegen, and otr.py also reimplements Mathf.PerlinNoise, which the Landing map uses. Measured
        /// 2026-10-11: `python Tools/aosa/occ.py --changed` then `python Tools/otr.py SimHashTests` gives
        /// FAIL=2 PASS=4 - both pins fail, the greybox with "But was: 14849829938549376146" against the editor's
        /// 6938716348497752424, the Landing with 1387093808830256496 against 3379562990869556318. A no-Burst
        /// value is a different number, not a second opinion: paste one in and the gate goes red.
        /// </summary>
        [Test]
        public void TheGreyboxHashAfter300TicksIsTheGoldenOneForThisFormatVersion()
        {
            Assert.AreEqual(45, ReplayRecorder.FormatVersion, "a new FormatVersion re-pins the golden hash below");
            using var match = ScriptedGreybox();
            var fight = Script(match);
            Assert.Greater(match.World.HighWater, 0,
                "[T24b] nobody deployed: the 23 per-unit arrays SimWorld.Hash() folds at SimWorld.cs:417-439 fold " +
                "zero bytes and the pin below is blind to every one of them");
            Assert.Greater(fight.Vehicles, 0, "[T24b] no machine on the field, so the vehicle state is unhashed");
            Assert.Greater(fight.Hits + fight.Deaths + fight.Wounded, 0,
                "[T24b] the match went idle: nobody was hit, wounded or killed in the 300 ticks");
            Assert.AreEqual(0x604b45c18d677168UL, match.World.Hash(),
                "[T24b] the greybox hash after 300 scripted ticks moved from its FormatVersion 45 pin: a field was " +
                "added, moved or dropped inside SimWorld.Hash(), or a system in the chain changed what it hashes");
        }

        /// <summary>[T24] The same pin on the generated Landing battlefield, so a change that only shows up with
        /// terrain, deployment, or the coast systems (BattlefieldGenerator, SeaLandingSystem) has its own guard
        /// rather than riding on the greybox pin alone. [T24b] Scripted the same way; re-pin the same way.</summary>
        [Test]
        public void TheLandingBattlefieldHashAfter300TicksIsTheGoldenOneForThisFormatVersion()
        {
            Assert.AreEqual(45, ReplayRecorder.FormatVersion, "a new FormatVersion re-pins the golden hash below");
            using var match = TW.Sim.Match.MatchSim.CreateBattlefield(ScriptedConfig(), TW.Sim.Terrain.BattlefieldParams.Landing(Seed));
            var fight = Script(match);
            Assert.Greater(match.World.HighWater, 0,
                "[T24b] nobody deployed: the 23 per-unit arrays SimWorld.Hash() folds at SimWorld.cs:417-439 fold " +
                "zero bytes and the pin below is blind to every one of them");
            Assert.Greater(fight.Vehicles, 0, "[T24b] no machine on the field, so the vehicle state is unhashed");
            Assert.Greater(fight.Hits + fight.Deaths + fight.Wounded, 0,
                "[T24b] the match went idle: nobody was hit, wounded or killed in the 300 ticks");
            Assert.AreEqual(0x2ee69ea26ed62c5eUL, match.World.Hash(),
                "[T24b] the Landing battlefield hash after 300 scripted ticks moved from its FormatVersion 45 pin: a " +
                "field was added, moved or dropped inside SimWorld.Hash(), or a system in the chain changed what it hashes");
        }

        /// <summary>[T24c] Not a claim: what the two scripted matches do, tick by tick, so the tick counts and the
        /// mutant choice of this change come from a measurement and not from a guess. Explicit: it runs 600 ticks of
        /// both matches and asserts nothing.</summary>
        [Test, Explicit("a measurement, not a claim: what the scripted matches do tick by tick")]
        public void WhatTheScriptedMatchesDo_TickProfile()
        {
            var sb = new System.Text.StringBuilder();
            Profile(sb, "greybox 300x400, spawns z=180/220", ScriptedGreybox());
            Profile(sb, "Landing " + Seed, TW.Sim.Match.MatchSim.CreateBattlefield(ScriptedConfig(), TW.Sim.Terrain.BattlefieldParams.Landing(Seed)));
            // also on disk, because an Explicit test's console output is not in run_tests' JSON (tw-sim-hash-profile.txt)
            System.IO.File.WriteAllText(System.IO.Path.Combine(System.IO.Path.GetTempPath(), "tw-sim-hash-profile.txt"), sb.ToString());
            TestContext.WriteLine(sb.ToString());
        }

        static void Profile(System.Text.StringBuilder sb, string what, TW.Sim.Match.MatchSim m)
        {
            using (m)
            {
                var w = m.World;
                using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
                int firstShot = -1, firstHit = -1, firstBlow = -1, firstTrench = -1;
                int deployed = 0, fired = 0, rejected = 0, blows = 0, trenches = 0;
                sb.AppendLine("== " + what);
                for (int t = 0; t < 600; t++)
                {
                    if (t == 0 || t == 20)
                    {
                        var cmds = t == 0 ? Deploys(w) : Barrages(w);
                        using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
                        m.Step(arr);
                    }
                    else m.Step(none);
                    var ev = w.Events.Events;
                    for (int e = 0; e < ev.Length; e++)
                    {
                        switch (ev[e].Type)
                        {
                            case SimEventType.Shot: if (firstShot < 0) firstShot = t; break;
                            case SimEventType.Hit: if (firstHit < 0) firstHit = t; break;
                            case SimEventType.MeleeBlow: blows++; if (firstBlow < 0) firstBlow = t; break;
                            case SimEventType.UnitEnteredTrench: trenches++; if (firstTrench < 0) firstTrench = t; break;
                            case SimEventType.UnitDeployed: deployed++; break;
                            case SimEventType.AbilityFired: fired++; break;
                            case SimEventType.CommandRejected: rejected++; break;
                        }
                    }
                    if (t % 20 == 0 || t == 599)
                    {
                        int a0 = 0, a1 = 0;
                        for (int i = 0; i < w.HighWater; i++)
                        {
                            if (!w.IsAlive(i)) continue;
                            if ((w.Team[i] & 1) == 0) a0++; else a1++;
                        }
                        sb.AppendLine($"  t{t,3} highWater {w.HighWater,3} alive {a0}/{a1}");
                    }
                }
                sb.AppendLine($"  first: shot {firstShot} hit {firstHit} meleeBlow {firstBlow} enteredTrench {firstTrench}");
                sb.AppendLine($"  totals: deployed {deployed} abilityFired {fired} rejected {rejected} meleeBlows {blows} trenchEntries {trenches}");
            }
        }

    }
}
