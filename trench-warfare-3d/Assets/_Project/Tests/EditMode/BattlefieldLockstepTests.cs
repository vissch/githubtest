// Phase: N1 — two lockstep worlds on the GENERATED battlefield stay hash-identical, and the ground under them
// stays identical too.
//
// CORRECTION, 2026-09-23. The first version of this file claimed nothing had ever run two lockstep worlds on
// the generated battlefield. That was false: BattlefieldTests.TwoSims_OnAGeneratedBattlefield_StayInSync_
// ThroughABarrage has done exactly that since 32cf419 - 900 ticks, per-tick hash compare, both sides
// deploying, two HE barrages, and it asserts the barrage cratered. It sits 26 lines below the test that was
// cited as proof the gap existed. The claim is corrected rather than the file deleted, because what these
// tests add is real once it is stated honestly:
//   - the ground is compared, which no other test does (see below);
//   - a second battlefield seed runs, where every other lockstep test uses 1917 alone;
//   - a failure names the array or the SYSTEM that diverged instead of only the tick.
//
// The compared value does not include the ground. MapData.Hash is never folded into SimWorld.Hash(), and
// DeformationSystem.Hash folds a checksum of crater INPUTS plus three counters, not the heightfield it wrote.
// So crater deformation - the headline reason for testing this map rather than the greybox - was outside the
// hash. These tests compare Map.Hash directly, which is the cheap way to cover it without changing what the
// shipped tick hash costs.
//
// SimHost runs TWO complete sims side by side and latches on the first hash divergence (compared at
// SimHost.cs:112, latched at :114, logged at :115) - but only on ticks where the two worlds are level, so a
// quiet SimHost is weaker evidence than it looks.
//
// It was written after a false alarm, and the false alarm is the useful part. On 2026-09-23 a live session logged
// "DESYNC at tick 33". That was self-inflicted - a tooling eval had called World.Spawn on Local only, which the
// peer world never sees - but it took an hour to establish, because tick 33 reads like one second of play and the
// probe had run fourteen seconds in. It is not one second: SimHost.Update takes at most max(8, TimeScale * 2)
// ticks per frame and entering Play here is expensive, so the opening seconds run at a few frames each. Tick
// number is not wall-clock time (docs/03).
//
// What the false alarm exposed was real: there was no way to answer "is the game desyncing?" except by arguing
// about a log line. A desync is the one class of bug this project cannot ship, so it gets a test at the level
// SimHost actually works at.
//
// On failure this does not just say "they differ" - it names the piece of state that differs first, because
// "Position diverged" and "the garrison's holder array diverged" send you to opposite ends of the codebase.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class BattlefieldLockstepTests
    {
        /// <summary>The seed the played scene uses, so a failure here is the failure the owner would see.</summary>
        const int SceneSeed = 1917;

        static string FirstDifference(SimWorld a, SimWorld b)
        {
            if (a.Tick != b.Tick) return $"Tick {a.Tick} vs {b.Tick}";
            if (a.HighWater != b.HighWater) return $"HighWater {a.HighWater} vs {b.HighWater}";
            if (a.AliveCount != b.AliveCount) return $"AliveCount {a.AliveCount} vs {b.AliveCount}";
            int n = a.HighWater;
            var named = new List<(string, ulong, ulong)>
            {
                ("Position", SimHash.Array(a.Position, n, SimHash.Offset), SimHash.Array(b.Position, n, SimHash.Offset)),
                ("Velocity", SimHash.Array(a.Velocity, n, SimHash.Offset), SimHash.Array(b.Velocity, n, SimHash.Offset)),
                ("Yaw", SimHash.Array(a.Yaw, n, SimHash.Offset), SimHash.Array(b.Yaw, n, SimHash.Offset)),
                ("Hp", SimHash.Array(a.Hp, n, SimHash.Offset), SimHash.Array(b.Hp, n, SimHash.Offset)),
                ("MaxHp", SimHash.Array(a.MaxHp, n, SimHash.Offset), SimHash.Array(b.MaxHp, n, SimHash.Offset)),
                ("Speed", SimHash.Array(a.Speed, n, SimHash.Offset), SimHash.Array(b.Speed, n, SimHash.Offset)),
                ("Team", SimHash.Array(a.Team, n, SimHash.Offset), SimHash.Array(b.Team, n, SimHash.Offset)),
                ("Archetype", SimHash.Array(a.Archetype, n, SimHash.Offset), SimHash.Array(b.Archetype, n, SimHash.Offset)),
                ("SourceTrench", SimHash.Array(a.SourceTrench, n, SimHash.Offset), SimHash.Array(b.SourceTrench, n, SimHash.Offset)),
                ("Generation", SimHash.Array(a.Generation, n, SimHash.Offset), SimHash.Array(b.Generation, n, SimHash.Offset)),
                ("Suppression", SimHash.Array(a.Suppression, n, SimHash.Offset), SimHash.Array(b.Suppression, n, SimHash.Offset)),
                ("StanceOf", SimHash.Array(a.StanceOf, n, SimHash.Offset), SimHash.Array(b.StanceOf, n, SimHash.Offset)),
                ("Layer", SimHash.Array(a.Layer, n, SimHash.Offset), SimHash.Array(b.Layer, n, SimHash.Offset)),
                ("TrenchId", SimHash.Array(a.TrenchId, n, SimHash.Offset), SimHash.Array(b.TrenchId, n, SimHash.Offset)),
                ("PostCell", SimHash.Array(a.PostCell, n, SimHash.Offset), SimHash.Array(b.PostCell, n, SimHash.Offset)),
                ("PostKind", SimHash.Array(a.PostKind, n, SimHash.Offset), SimHash.Array(b.PostKind, n, SimHash.Offset)),
                ("TargetSlot", SimHash.Array(a.TargetSlot, n, SimHash.Offset), SimHash.Array(b.TargetSlot, n, SimHash.Offset)),
                ("GoalId", SimHash.Array(a.GoalId, n, SimHash.Offset), SimHash.Array(b.GoalId, n, SimHash.Offset)),
                ("Flags", SimHash.Array(a.Flags, n, SimHash.Offset), SimHash.Array(b.Flags, n, SimHash.Offset)),
                ("Cooldown", SimHash.Array(a.Cooldown, n, SimHash.Offset), SimHash.Array(b.Cooldown, n, SimHash.Offset)),
                ("FireCooldown", SimHash.Array(a.FireCooldown, n, SimHash.Offset), SimHash.Array(b.FireCooldown, n, SimHash.Offset)),
                ("Knock", SimHash.Array(a.Knock, n, SimHash.Offset), SimHash.Array(b.Knock, n, SimHash.Offset)),
                ("Silver", SimHash.Array(a.Silver, SimHash.Offset), SimHash.Array(b.Silver, SimHash.Offset)),
                ("SilverFraction", SimHash.Array(a.SilverFraction, SimHash.Offset), SimHash.Array(b.SilverFraction, SimHash.Offset)),
                ("Rally", SimHash.Array(a.Rally, SimHash.Offset), SimHash.Array(b.Rally, SimHash.Offset)),
                ("SlotCooldown", SimHash.Array(a.SlotCooldown, SimHash.Offset), SimHash.Array(b.SlotCooldown, SimHash.Offset)),
                ("SlotUnlocked", SimHash.Array(a.SlotUnlocked, SimHash.Offset), SimHash.Array(b.SlotUnlocked, SimHash.Offset)),
            };
            foreach (var (name, ha, hb) in named) if (ha != hb) return name;
            if (a.WinnerTeam != b.WinnerTeam) return $"WinnerTeam {a.WinnerTeam} vs {b.WinnerTeam}";
            // not a unit array, then: name the system rather than hand back a list of eight to guess between.
            // The lists are sorted identically on both worlds, so index i is the same system in each.
            int systems = System.Math.Min(a.Systems.Count, b.Systems.Count);
            for (int i = 0; i < systems; i++)
                if (a.Systems[i].Hash(SimHash.Offset) != b.Systems[i].Hash(SimHash.Offset))
                    return a.Systems[i].GetType().Name;
            return "nothing we can name: the difference is in state that Hash() folds in but this check does not read";
        }

        /// <summary>The ground, which SimWorld.Hash() does not cover. Cheap, so it is checked often but not per tick.</summary>
        static void SameGround(MatchSim a, MatchSim b, string what)
            => Assert.AreEqual(a.Map.Hash(SimHash.Offset), b.Map.Hash(SimHash.Offset),
                $"{what}: the two worlds' TERRAIN diverged by tick {a.World.Tick} (heights or nav layers). " +
                "SimWorld.Hash() does not fold MapData.Hash in, and DeformationSystem hashes its crater inputs " +
                "rather than the heightfield it wrote, so a unit has to walk on it before the tick hash notices.");

        static void StayInSync(MatchSim a, MatchSim b, int ticks, string what)
        {
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            Assert.AreEqual(a.World.LastHash, b.World.LastHash, $"{what}: the two worlds do not even start equal");
            SameGround(a, b, what);
            for (int t = 0; t < ticks; t++)
            {
                a.Step(none);
                b.Step(none);
                if (t % 50 == 49) SameGround(a, b, what);
                if (a.World.LastHash == b.World.LastHash) continue;
                Assert.Fail($"{what}: the two lockstep worlds diverged at tick {a.World.Tick}. " +
                            $"First state that differs: {FirstDifference(a.World, b.World)}. " +
                            $"local {a.World.LastHash:X16} peer {b.World.LastHash:X16}");
            }
            SameGround(a, b, what);
        }

        /// <summary>
        /// The played configuration: the generated battlefield, no commands at all. Anything that diverges here
        /// diverges on its own - ambient bombardment, deformation, the garrison, the wave AI - with no input to
        /// blame it on. 400 ticks is comfortably past tick 33, where the live session latched.
        /// </summary>
        [Test]
        public void TwoWorldsOnTheGeneratedBattlefield_StayInSync_WithNoCommandsAtAll()
        {
            // the shipped numbers, not SimConfig.Default's: SimHost sets these on every real session, and both
            // Silver and SilverFraction are hashed state that gate deployment
            var cfg = SimConfig.Default; cfg.StartingSilver = 300; cfg.SilverPerSecond = 2f;
            var field = BattlefieldParams.ShelledForest(SceneSeed);
            using var a = MatchSim.CreateBattlefield(cfg, field);
            using var b = MatchSim.CreateBattlefield(cfg, field);
            StayInSync(a, b, 400, "generated battlefield, idle");
        }

        /// <summary>The same, with men in it: deployment, movement, the garrison taking its posts, and contact.</summary>
        [Test]
        public void TwoWorldsOnTheGeneratedBattlefield_StayInSync_WithBothSidesDeployed()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            var field = BattlefieldParams.ShelledForest(SceneSeed);
            using var a = MatchSim.CreateBattlefield(cfg, field);
            using var b = MatchSim.CreateBattlefield(cfg, field);
            Assert.AreEqual(a.World.LastHash, b.World.LastHash, "the two worlds do not even start equal");
            // NOT `using var`: a NativeArray declared that way is a readonly struct and cannot be indexed into
            // (CS1654). Allocated once outside the loop and disposed in the finally, which is cheaper anyway.
            var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            var forA = new NativeArray<SimCommand>(2, Allocator.Temp);
            var forB = new NativeArray<SimCommand>(2, Allocator.Temp);
            try
            {
                for (int t = 0; t < 900; t++)
                {
                    if (t % 20 == 0)
                    {
                        // each world gets its own copy, so a Step that sorted in place could not favour one of them
                        var c0 = SimCommand.Deploy(a.World.Tick, 0, t % 3);
                        var c1 = SimCommand.Deploy(a.World.Tick, 1, t % 3);
                        forA[0] = c0; forA[1] = c1;
                        forB[0] = c0; forB[1] = c1;
                        a.Step(forA); b.Step(forB);
                    }
                    else { a.Step(none); b.Step(none); }
                    if (a.World.LastHash == b.World.LastHash) continue;
                    Assert.Fail($"deployed: the two lockstep worlds diverged at tick {a.World.Tick}. " +
                                $"First state that differs: {FirstDifference(a.World, b.World)}.");
                }
            }
            finally { none.Dispose(); forA.Dispose(); forB.Dispose(); }
            SameGround(a, b, "deployed");
            Assert.Greater(a.World.AliveCount, 10, "men were actually deployed");
        }

        /// <summary>
        /// A SECOND battlefield seed. This replaced a greybox "control", which earned nothing: LockstepLoopbackTests
        /// already runs the greybox with two peers, commands and a lossy transport for 1000 ticks, which is strictly
        /// more. Seed 1917 was the only battlefield configuration any lockstep test had ever run, so a generator
        /// that produced one bad map in twenty would never have been caught.
        /// </summary>
        [Test]
        public void TwoWorldsOnASecondBattlefieldSeed_StayInSync()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 300; cfg.SilverPerSecond = 2f;
            using var a = MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(7));
            using var b = MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(7));
            StayInSync(a, b, 400, "generated battlefield, seed 7, idle");
        }

    }
}
