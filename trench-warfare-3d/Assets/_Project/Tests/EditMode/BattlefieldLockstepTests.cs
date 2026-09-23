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
        /// The negative control this whole suite rests on: does the tick hash NOTICE a change?
        ///
        /// Every lockstep test in the repo - the three here, BattlefieldTests, DeterminismReplayTests,
        /// LockstepLoopbackTests - compares two hashes and passes when they are equal. Not one of them asserts
        /// that a hash MOVES when the state does. A SimWorld.Hash() that quietly stopped folding in an array
        /// would leave every one of them green while that array diverged freely between peers.
        ///
        /// That is not a hypothetical failure, it is this project's own history: f3b7f80 reads "The hash did not
        /// cover the ground, MaxHp was never in it". A field sat outside the hash and the determinism suite was
        /// green throughout. SimHashTests does cover SimHash.Array - two different arrays hash differently - but
        /// that says nothing about whether SimWorld.Hash() ever READS the array in question.
        ///
        /// So: perturb one element of every array Hash() claims, one at a time, and require the hash to move.
        /// Each value is put back and the hash re-checked, so one probe cannot poison the next.
        ///
        /// Note the asymmetry, because it decides what this test is worth. A NEW field that is hashed but not
        /// listed here fails safe: it is simply not covered, and the suite is no worse than before. A field
        /// REMOVED from Hash() fails loudly. That is the direction the accident actually went.
        /// </summary>
        [Test]
        public void TheTickHashNoticesAChangeInEveryFieldItClaimsToCover()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            using var m = MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(SceneSeed));
            var deploy = new NativeArray<SimCommand>(2, Allocator.Temp);
            var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            try
            {
                deploy[0] = SimCommand.Deploy(m.World.Tick, 0, 0);
                deploy[1] = SimCommand.Deploy(m.World.Tick, 1, 0);
                m.Step(deploy);
                for (int t = 0; t < 40; t++) m.Step(none);
            }
            finally { deploy.Dispose(); none.Dispose(); }

            var w = m.World;
            Assert.Greater(w.HighWater, 0, "the unit-array probes below need at least one slot in use");

            var blind = new List<string>();
            void Check(string name, ulong before)
            {
                if (w.Hash() == before) blind.Add(name);
            }
            void Restored(string name, ulong before)
            {
                Assert.AreEqual(before, w.Hash(),
                    name + ": the probe did not put the world back, so every probe after it is unreliable");
            }
            void Pf3(string name, NativeArray<float3> a)
            { var o = a[0]; ulong b = w.Hash(); a[0] = o + new float3(1f, 0f, 0f); Check(name, b); a[0] = o; Restored(name, b); }
            void Pf(string name, NativeArray<float> a)
            { var o = a[0]; ulong b = w.Hash(); a[0] = o + 1f; Check(name, b); a[0] = o; Restored(name, b); }
            void Pb(string name, NativeArray<byte> a)
            { var o = a[0]; ulong b = w.Hash(); a[0] = (byte)(o + 1); Check(name, b); a[0] = o; Restored(name, b); }
            void Ps(string name, NativeArray<short> a)
            { var o = a[0]; ulong b = w.Hash(); a[0] = (short)(o + 1); Check(name, b); a[0] = o; Restored(name, b); }
            void Pus(string name, NativeArray<ushort> a)
            { var o = a[0]; ulong b = w.Hash(); a[0] = (ushort)(o + 1); Check(name, b); a[0] = o; Restored(name, b); }
            void Pi(string name, NativeArray<int> a)
            { var o = a[0]; ulong b = w.Hash(); a[0] = o + 1; Check(name, b); a[0] = o; Restored(name, b); }
            void Pu(string name, NativeArray<uint> a)
            { var o = a[0]; ulong b = w.Hash(); a[0] = o + 1u; Check(name, b); a[0] = o; Restored(name, b); }

            // every array SimWorld.Hash() folds in, in the order it folds them
            Pi("Silver", w.Silver); Pf("SilverFraction", w.SilverFraction); Pf3("Rally", w.Rally);
            Pi("SlotCooldown", w.SlotCooldown); Pb("SlotUnlocked", w.SlotUnlocked);
            Pf3("Position", w.Position); Pf3("Velocity", w.Velocity); Pf("Yaw", w.Yaw);
            Pf("Hp", w.Hp); Pf("MaxHp", w.MaxHp); Pf("Suppression", w.Suppression); Pf("Speed", w.Speed);
            Pb("StanceOf", w.StanceOf); Pb("Team", w.Team); Pb("Archetype", w.Archetype); Pb("Layer", w.Layer);
            Ps("TrenchId", w.TrenchId); Ps("SourceTrench", w.SourceTrench);
            Pi("PostCell", w.PostCell); Pb("PostKind", w.PostKind);
            Pi("TargetSlot", w.TargetSlot); Pi("GoalId", w.GoalId); Pu("Flags", w.Flags);
            Pus("Generation", w.Generation); Pi("Cooldown", w.Cooldown); Pi("FireCooldown", w.FireCooldown);
            Pf3("Knock", w.Knock);

            Assert.IsEmpty(blind,
                "SimWorld.Hash() does not notice a change in these, so two peers can differ in them for a whole "
                + "match and every lockstep test in this repo stays green: " + string.Join(", ", blind));
        }

        /// <summary>
        /// The idle sync tests are only worth what the idle battlefield actually does, and they never check.
        ///
        /// Both of them run 400 ticks with no commands and assert exactly one thing: that two identical
        /// computations agree. A world in which every system early-returned would pass them perfectly, and pass
        /// them faster. This project has written that failure down twice already - a number that rewards the
        /// thing going wrong - so the sync tests need a companion that says the 400 ticks were not empty.
        ///
        /// It matters most for the ground, because this file's header claims its distinctive contribution is
        /// that "the ground is compared, which no other test does". SameGround compares Map.Hash between the two
        /// worlds; if the ground never changes over those ticks then it compares two constants and has never
        /// been able to fail, whatever the header says.
        ///
        /// Ambient bombardment is the only thing that craters an idle battlefield. ShelledForest sets
        /// Bombardment = 8 shells a minute; the first falls at TickRate * 8 (tick 160 at TickRate 20) and the gap
        /// after it is TickRate * 60 / 8 * rand(0.35, 1.65), so 52 to 247 ticks. 400 ticks should carry two or
        /// three. The count is asserted rather than assumed, and printed on failure, because "should" is how the
        /// last two of these went wrong.
        /// </summary>
        [Test]
        public void TheIdleBattlefieldActuallyDoesSomething_OrTheSyncTestsAgreeAboutNothing()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 300; cfg.SilverPerSecond = 2f;
            using var m = MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(SceneSeed));
            ulong world0 = m.World.LastHash, ground0 = m.Map.Hash(SimHash.Offset);
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            for (int t = 0; t < 400; t++) m.Step(none);

            Assert.AreNotEqual(world0, m.World.LastHash,
                "400 idle ticks changed no hashed state at all, so the two idle sync tests agree about nothing");
            Assert.Greater(m.Bombardment.Fired, 0,
                $"no ambient shell fell in 400 ticks (Bombardment = {m.Bombardment.ShellsPerMinute}/min, first due "
                + $"at tick {cfg.TickRate * 8}), so nothing was there to crater the ground");
            Assert.AreNotEqual(ground0, m.Map.Hash(SimHash.Offset),
                $"{m.Bombardment.Fired} ambient shells fell and the GROUND hash never moved. SameGround therefore "
                + "compares two constants, and this file's claim to be the only test that checks the terrain is empty.");
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
