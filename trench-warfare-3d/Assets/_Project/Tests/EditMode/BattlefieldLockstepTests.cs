// Phase: N1 — two lockstep worlds on the GENERATED battlefield stay hash-identical.
//
// Why this file exists: LockstepLoopbackTests covers the greybox corridor, and BattlefieldTests covers the
// generated map being built the same twice. Nothing covered the two together - and the scene the owner actually
// plays (GreyboxCorridor.unity) sets GeneratedBattlefield = 1, so every real session runs the one combination
// that had no test. SimHost runs TWO complete sims side by side and latches on the first hash divergence
// (SimHost.cs:115).
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
            };
            foreach (var (name, ha, hb) in named) if (ha != hb) return name;
            return "none of the unit arrays: it is one of the systems (garrison posts, gas, deformation, sectors, waves)";
        }

        static void StayInSync(MatchSim a, MatchSim b, int ticks, string what)
        {
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            for (int t = 0; t < ticks; t++)
            {
                a.Step(none);
                b.Step(none);
                if (a.World.LastHash == b.World.LastHash) continue;
                Assert.Fail($"{what}: the two lockstep worlds diverged at tick {a.World.Tick}. " +
                            $"First state that differs: {FirstDifference(a.World, b.World)}. " +
                            $"local {a.World.LastHash:X16} peer {b.World.LastHash:X16}");
            }
        }

        /// <summary>
        /// The played configuration: the generated battlefield, no commands at all. Anything that diverges here
        /// diverges on its own - ambient bombardment, deformation, the garrison, the wave AI - with no input to
        /// blame it on. 400 ticks is comfortably past tick 33, where the live session latched.
        /// </summary>
        [Test]
        public void TwoWorldsOnTheGeneratedBattlefield_StayInSync_WithNoCommandsAtAll()
        {
            var cfg = SimConfig.Default;
            var field = BattlefieldParams.ShelledForest(SceneSeed);
            using var a = MatchSim.CreateBattlefield(cfg, field);
            using var b = MatchSim.CreateBattlefield(cfg, field);
            Assert.AreEqual(a.World.LastHash, b.World.LastHash, "the two worlds do not even start equal");
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
            Assert.Greater(a.World.AliveCount, 10, "men were actually deployed");
        }

        /// <summary>The greybox map the loopback tests use, as a control: if this passes and the battlefield one
        /// fails, the fault is in what the generated map adds (craters, mud, river, wire, bombardment).</summary>
        [Test]
        public void TwoWorldsOnTheGreyboxCorridor_StayInSync_AsAControl()
        {
            var cfg = SimConfig.Default;
            using var a = MatchSim.CreateGreybox(cfg);
            using var b = MatchSim.CreateGreybox(cfg);
            StayInSync(a, b, 400, "greybox corridor, idle");
        }
    }
}
