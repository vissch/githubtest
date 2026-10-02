// Phase: A5 (docs/21 phase 5) — the smoke screen: SmokeLos counts only thick cells; a screen across a line of
// sight loses the target and a man three metres away keeps his; a man inside the smoke is hit less and kept down
// less; the field costs nothing and hashes nothing while it is down, and switches itself off when it has thinned.
using System;
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class SmokeScreenTests
    {
        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public SimWorld W => M.World;
            public Rig(int silver = 100000, uint seed = 0xC0FFEE)
            {
                var cfg = SimConfig.Default; cfg.StartingSilver = silver; cfg.Seed = seed;
                M = MatchSim.CreatePlaytest(cfg);
                M.Bombardment.ShellsPerMinute = 0f;
            }
            public int Man(float3 at, float hp, byte team = 1, byte archetype = 0) => W.Spawn(team, archetype, at, hp, 0f, false);
            public void Step(params SimCommand[] cmds)
            {
                using var arr = new NativeArray<SimCommand>(cmds, Allocator.Temp);
                M.Step(arr);
            }
            public static SimCommand Support(byte player, OffMapAbilityId id, float3 at, int heading = 0, int pattern = 0, int length = 0)
                => new SimCommand { Player = player, Type = CommandType.SupportFire, A = (int)id, B = AbilityArgs.Pack(heading, pattern, length), Pos = at };
            public int Count(SimEventType type, int a = -1)
            {
                int n = 0;
                var ev = W.Events.Events;
                for (int i = 0; i < ev.Length; i++) if (ev[i].Type == type && (a < 0 || ev[i].A == a)) n++;
                return n;
            }
            public bool First(SimEventType type, int a, out SimEvent e)
            {
                var ev = W.Events.Events;
                for (int i = 0; i < ev.Length; i++) if (ev[i].Type == type && ev[i].A == a) { e = ev[i]; return true; }
                e = default; return false;
            }
            public void Dispose() => M.Dispose();
        }

        [Test]
        public void MetresThroughCountsOnlyTheThickCellsOnTheLine()
        {
            const int w = 20, l = 20;
            var grid = new NativeArray<float>(w * l, Allocator.Temp);
            try
            {
                for (int z = 0; z < l; z++) for (int x = 5; x <= 7; x++) grid[z * w + x] = 30f;   // a 12 m band, x = 20..32
                Assert.AreEqual(12f, SmokeLos.MetresThrough(grid, w, l, new float3(0f, 0f, 10f), new float3(60f, 0f, 10f)), 0.01f, "the band, and only the band");
                Assert.AreEqual(12f, SmokeLos.MetresThrough(grid, w, l, new float3(60f, 0f, 10f), new float3(0f, 0f, 10f)), 0.01f, "either way along it");
                Assert.AreEqual(0f, SmokeLos.MetresThrough(grid, w, l, new float3(10f, 0f, 2f), new float3(10f, 0f, 70f)), "a line beside it");
                Assert.AreEqual(0f, SmokeLos.MetresThrough(grid, w, l, new float3(25f, 0f, 10f), new float3(25f, 0f, 10f)), "no length, no smoke");
                Assert.AreEqual(4f, SmokeLos.MetresThrough(grid, w, l, new float3(18f, 0f, 10f), new float3(24f, 0f, 10f)), 0.01f, "a segment ending inside it counts the part inside");
                for (int i = 0; i < grid.Length; i++) grid[i] = 5f;
                Assert.AreEqual(0f, SmokeLos.MetresThrough(grid, w, l, new float3(0f, 0f, 10f), new float3(60f, 0f, 10f)), "thin smoke does not count");
                for (int i = 0; i < grid.Length; i++) grid[i] = 30f;
                Assert.AreEqual(SmokeLos.MaxSteps * SmokeLos.Step, SmokeLos.MetresThrough(grid, w, l, new float3(0f, 0f, 10f), new float3(400f, 0f, 10f)), 0.01f, "past the cap a screen has done its work");
            }
            finally { grid.Dispose(); }
        }

        [Test]
        public void AScreenAcrossTheLineOfSightLosesTheTargetAndThreeMetresKeepsIt()
        {
            using (var far = new Rig())
            {
                int shooter = far.Man(new float3(100f, 0f, 200f), 1e6f, team: 0);
                int target = far.Man(new float3(140f, 0f, 200f), 1e6f, team: 1);
                for (int t = 0; t < 6; t++) far.Step();
                Assert.AreEqual(target, far.W.TargetSlot[shooter], "forty metres of open ground: seen");
                far.Step(Rig.Support(0, OffMapAbilityId.SmokeScreen, new float3(120f, 0f, 180f), heading: 0, length: 40));
                bool lost = false; int lostAt = -1;
                for (int t = 0; t < 500 && !lost; t++)
                {
                    far.Step();
                    if (far.W.TargetSlot[shooter] != target) { lost = true; lostAt = t; }
                }
                Assert.IsTrue(lost, "a screen across the line loses him");
                Assert.IsTrue(far.M.Gas.SmokeActive);
                Assert.GreaterOrEqual(far.M.Gas.MetresThrough(far.W.Position[shooter], far.W.Position[target], smoke: true), CombatTables.SmokeBlindMetres, $"lost at +{lostAt}: enough thick smoke lies between them");
                Assert.IsFalse(far.M.Gas.Active, "smoke is not gas");
            }
            using (var near = new Rig())
            {
                int shooter = near.Man(new float3(100f, 0f, 200f), 1e6f, team: 0);
                int target = near.Man(new float3(103f, 0f, 200f), 1e6f, team: 1);
                for (int t = 0; t < 6; t++) near.Step();
                Assert.AreEqual(target, near.W.TargetSlot[shooter]);
                near.Step(Rig.Support(0, OffMapAbilityId.SmokeScreen, new float3(101f, 0f, 180f), heading: 0, length: 40));
                for (int t = 0; t < 300; t++)
                {
                    near.Step();
                    Assert.AreEqual(target, near.W.TargetSlot[shooter], $"tick +{t}: three metres away he is seen through anything");
                }
                Assert.Greater(near.M.Gas.SmokeAt(near.W.Position[target]), SmokeLos.Thick, "and he is standing in it");
            }
        }

        [Test]
        public void SmokeCutsTheHitsOnAManInsideItAndHalvesWhatKeepsHimDown()
        {
            // the same seed, the same two men, the same dice per tick and slot: only the smoke differs. Both men's
            // suppression is wiped after every tick so neither goes prone in one run and not the other, and the one
            // thing left between the runs is the chance to hit.
            int Hits(bool smoke, out float nearMiss)
            {
                using var r = new Rig();
                int shooter = r.Man(new float3(100f, 0f, 200f), 1e6f, team: 0, archetype: 2);   // a machine gun: enough rounds to count
                int target = r.Man(new float3(108f, 0f, 200f), 1e6f, team: 1);
                if (smoke)
                    for (int dz = -8; dz <= 8; dz += 8) { r.M.Gas.AddSmokeSource(new float3(104f, 0f, 200f + dz), 30f, 2000, 1); r.M.Gas.AddSmokeSource(new float3(108f, 0f, 200f + dz), 30f, 2000, 1); }
                for (int t = 0; t < 100; t++) { r.Step(); r.W.Suppression[shooter] = 0f; r.W.Suppression[target] = 0f; }
                int hits = 0; nearMiss = -1f;
                for (int t = 0; t < 500; t++)
                {
                    r.Step();
                    hits += r.Count(SimEventType.Hit, shooter);
                    if (nearMiss < 0f && r.First(SimEventType.NearMiss, target, out var e)) nearMiss = e.Scalar;
                    r.W.Suppression[shooter] = 0f; r.W.Suppression[target] = 0f;
                }
                if (smoke) Assert.GreaterOrEqual(r.M.Gas.MetresThrough(r.W.Position[shooter], r.W.Position[target], smoke: true), 6f, "the smoke lies on the line of fire");
                return hits;
            }
            int clear = Hits(false, out float clearNear);
            int screened = Hits(true, out float screenedNear);
            Assert.Greater(clear, 15, "the gun finds him in the open");
            Assert.Less(screened, clear * 0.6f, $"through the smoke it finds him far less often ({screened} of {clear})");
            Assert.Greater(clearNear, 0f); Assert.Greater(screenedNear, 0f);
            Assert.AreEqual(clearNear * CombatTables.SmokeSuppression, screenedNear, 1e-3f, "inside the smoke a near miss keeps him down half as much");
        }

        [Test]
        public void TheScreenIsLaidByTheAbilityAndHashedOnlyWhileItIsUp()
        {
            using var r = new Rig();
            Assert.AreEqual(1UL, r.M.Gas.Hash(1UL), "no cloud, nothing to hash");
            Assert.IsTrue(OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.SmokeScreen, out var stats));
            int silver = r.W.Silver[0];
            r.Step(Rig.Support(0, OffMapAbilityId.SmokeScreen, new float3(120f, 0f, 180f), heading: 0, length: 40));
            Assert.LessOrEqual(r.W.Silver[0], silver - stats.Cost + 1);
            Assert.AreEqual(OffMapAbilitySystem.SmokeSources, r.M.Abilities.Scheduled.Length);
            Assert.IsFalse(r.M.Gas.SmokeActive, "not before the warm-up");
            int spawned = 0;
            for (int t = 1; t <= stats.WarmupTicks; t++) { r.Step(); spawned += r.Count(SimEventType.SmokeSpawned); }
            Assert.AreEqual(OffMapAbilitySystem.SmokeSources, spawned, "one event per pot");
            Assert.IsTrue(r.M.Gas.SmokeActive);
            Assert.AreNotEqual(1UL, r.M.Gas.Hash(1UL), "up: the field is in the hash");
            Assert.AreEqual(OffMapAbilitySystem.SmokeSources, r.M.Gas.SmokeSources.Length);
            for (int k = 0; k < OffMapAbilitySystem.SmokeSources; k++)
                Assert.Greater(r.M.Gas.SmokeAt(new float3(120f, 0f, 184f + k * 8f)), 15f, $"pot {k} sits on the line");
            Assert.IsFalse(r.M.Gas.Active, "the gas field is not touched");
        }

        [Test]
        public void AScreenThinsOutAndSwitchesItselfOff()
        {
            using var r = new Rig();
            r.M.Gas.AddSmokeSource(new float3(150f, 0f, 240f), 30f, 20, 0);
            int ticks = 0;
            while (r.M.Gas.SmokeActive && ticks < 3000) { r.Step(); ticks++; }
            Assert.IsFalse(r.M.Gas.SmokeActive, "a screen does not last for ever");
            Assert.Greater(ticks, 100, "but it lingers after the pot is spent");
            for (int i = 0; i < r.M.Gas.Smoke.Length; i++) Assert.AreEqual(0f, r.M.Gas.Smoke[i]);
            Assert.AreEqual(0, r.M.Gas.SmokeSources.Length);
            Assert.AreEqual(1UL, r.M.Gas.Hash(1UL), "down: nothing in the hash again");
        }
    }
}
