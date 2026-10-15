// Phase: infantry-firing (2026-10-15) — two of the three repairs the owner asked for in the infantry's firing (Rifle,
// Assault, Machinegunner, Sniper), as the character controller shows them. The third, the tracer cut to the gun, is
// TracerMuzzleTests.
//   * The shot fired while advancing has its own pose. The locomotion rung called a man "firing" only while
//     w.FireCooldown < 30 — but the cooldown is at its LONGEST right after a shot (a rifleman's is 41 ticks,
//     CombatTables; a sniper's slower), so the half second the fire pose was wanted was exactly the window the test was
//     false in, and a man walking into his own fire played WalkAimed (crouched: CrouchRun/StoopWalk).
//   * A squad does not fire in lockstep. Start phased loops only, so every man whose sim fired on the same tick stood at
//     frame 0 of the same one-shot: six identical men.
// No sim is stepped: the tests place men, walk them by hand, write the Shot event the way DirectFire would, and tick the
// controller alone (BlastReactionTests' rig).
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class InfantryFiringTests
    {
        static readonly float3 Where = new float3(150f, 0f, 300f);
        const float Pace = 1.4f;   // m/s, the rifleman's own walk: the pace a man advances and fires at

        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public readonly AnimationController A;
            NativeArray<ushort> row; NativeArray<float> phase, yaw;

            public Rig()
            {
                var cfg = SimConfig.Default;
                M = MatchSim.CreateGreybox(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;   // no stray shell over the men
                M.World.Tick = 1000;   // the controller's rate limits are against tick 0
                row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
                phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                A = new AnimationController(cfg.MaxSlots, M.Map, M.Gas, row, phase, yaw);
            }

            /// <summary>A man of an archetype, at an offset from the staging point.</summary>
            public int Man(byte arche, float dx, float dz, byte team = 0)
                => M.World.Spawn(team, arche, Where + new float3(dx, 0f, dz), 100f, 3f, false);

            /// <summary>He is shooting at `at`: the sim's target, and its cooldown just after a shot (a rifleman's 41 ticks).</summary>
            public void Aim(int slot, int at, int cooldown = 41)
            {
                M.World.TargetSlot[slot] = at;
                M.World.FireCooldown[slot] = cooldown;
            }

            /// <summary>The shot leaves his rifle this tick, as DirectFire writes it.</summary>
            public void Shot(int slot, int at) => M.World.Events.Add(M.World.Tick, SimEventType.Shot, slot, at, M.World.Position[slot], default, 0f);

            /// <summary>A step of his advance, before the tick that reads it (the controller's speed is the position delta).</summary>
            public void Step(int slot, float mps)
            {
                var p = M.World.Position[slot]; p.z += mps * M.World.Config.TickSeconds; M.World.Position[slot] = p;
            }

            public void Tick()
            {
                A.Tick(M.World);
                A.Advance(M.World.Config.TickSeconds);
                M.World.Events.Clear();
                M.World.Tick++;
            }

            /// <summary>The controller's tick without the frames after it: what Start put the men at, before Advance has
            /// played any of it (every man moves on by the same 0.05 s, which can shuffle two of them across a bucket).</summary>
            public void Decide()
            {
                A.Tick(M.World);
                M.World.Events.Clear();
                M.World.Tick++;
            }

            public Clip ClipOf(int slot) => A.State[slot].Clip;

            public void Dispose()
            {
                A.Dispose(); M.Dispose();
                row.Dispose(); phase.Dispose(); yaw.Dispose();
            }
        }

        static bool FiringGait(Clip c) => c == Clip.FireWalk || c == Clip.FireRun || c == Clip.FireSprint;

        [Test]
        public void AManWhoFiresWhileHeAdvancesIsInAFiringGait([Values(0, 1, 2, 3)] int arche)
        {
            using var r = new Rig();
            int man = r.Man((byte)arche, 0f, 0f);
            int enemy = r.Man((byte)InfantryArchetype.Rifle, 0f, 30f, 1);
            r.M.World.Events.Clear();
            r.Tick();
            // he walks at his own pace into his own fire, a shot every 41 ticks with the cooldown running down from it
            Clip seen = Clip.Idle;
            for (int t = 0; t < 24; t++)
            {
                r.Step(man, Pace);
                r.Aim(man, enemy);   // the cooldown stands at a rifleman's full 41 ticks: he has just fired
                if (t % 8 == 0) r.Shot(man, enemy);
                r.Tick();
                if (t > 4) seen = r.ClipOf(man);
            }
            Assert.IsTrue(FiringGait(seen), "archetype " + arche + " walks into his own fire in " + seen + ", not a firing gait");
        }

        [Test]
        public void CrouchedAndFiringOnTheMoveHeUsesTheCrouchedFiringGait([Values(0, 1, 2, 3)] int arche)
        {
            using var r = new Rig();
            int man = r.Man((byte)arche, 0f, 0f);
            int enemy = r.Man((byte)InfantryArchetype.Rifle, 0f, 30f, 1);
            r.M.World.StanceOf[man] = (byte)Stance.Crouch;
            r.M.World.Events.Clear();
            r.Tick();
            Clip seen = Clip.Idle;
            for (int t = 0; t < 24; t++)
            {
                r.M.World.StanceOf[man] = (byte)Stance.Crouch;
                r.Step(man, 1f);   // FireStoop is authored at 1 m/s
                r.Aim(man, enemy);   // the cooldown stands at a rifleman's full 41 ticks: he has just fired
                if (t % 8 == 0) r.Shot(man, enemy);
                r.Tick();
                if (t > 4) seen = r.ClipOf(man);
            }
            Assert.AreEqual(Clip.FireStoop, seen, "archetype " + arche + " fires crouched on the move in " + seen);
        }

        [Test]
        public void ASquadFiringOnOneTickIsNotSixOfTheSameMan([Values(0, 1, 2, 3)] int arche)
        {
            using var r = new Rig();
            var squad = new int[6];
            for (int k = 0; k < squad.Length; k++) squad[k] = r.Man((byte)arche, k * 1.6f, 0f);
            int enemy = r.Man((byte)InfantryArchetype.Rifle, 4f, 30f, 1);
            r.M.World.Events.Clear();
            r.Tick();
            foreach (int man in squad) { r.Aim(man, enemy); r.Shot(man, enemy); }
            r.Decide();

            // where each man stands in his clip, in buckets a film frame wide (1/24 of the clip: the review's films are
            // 24 fps, and closer than one frame reads as the same pose). The offset is 0.15 of the clip, so the squad is
            // spread over 3.6 of those frames: six men cannot fall thinner than four to a frame every time, and a wider
            // offset would start a bolt rifle's snap shot (FireSnap, 0.27 s) a third of the way through its rifle-raise.
            // The old code put all six in ONE frame of one clip, which is what this measures against.
            var clip = r.A.State[squad[0]].Clip;
            float seconds = Clips.Table[(int)clip].Seconds;
            var count = new System.Collections.Generic.Dictionary<int, int>();
            foreach (int man in squad)
            {
                var s = r.A.State[man];
                Assert.AreEqual(Rung.Fire, s.Rung, "archetype " + arche + ": slot " + man + " did not fire (" + s.Clip + ")");
                Assert.AreEqual(clip, s.Clip, "the squad fires the same clip");
                int bucket = (int)(s.Frame / (seconds / 24f));
                count[bucket] = count.TryGetValue(bucket, out int n) ? n + 1 : 1;
                if (AnimationController.OneShotFire(clip))
                    Assert.LessOrEqual(s.Frame, AnimationController.FirePhase * seconds + 1e-4f, "a man starts at most 0.15 into his shot");
            }
            int worst = 0;
            foreach (var b in count) worst = Math.Max(worst, b.Value);
            Assert.LessOrEqual(worst, 4, $"archetype {arche}: {worst} of 6 men of the squad stand in the same frame of {clip}");
            Assert.GreaterOrEqual(count.Count, 3, $"archetype {arche}: the squad stands in only {count.Count} frames of {clip}");
        }

        [Test]
        public void TwoMenOfASquadWorkThroughTheirShotAtTheirOwnPace()
        {
            using var r = new Rig();
            var squad = new int[6];
            for (int k = 0; k < squad.Length; k++) squad[k] = r.Man(0, k * 1.6f, 0f);
            int enemy = r.Man((byte)InfantryArchetype.Rifle, 4f, 30f, 1);
            r.M.World.Events.Clear();
            r.Tick();
            foreach (int man in squad) { r.Aim(man, enemy); r.Shot(man, enemy); }
            r.Tick();
            var rates = new System.Collections.Generic.HashSet<float>();
            foreach (int man in squad) { rates.Add(r.A.State[man].Rate); Assert.That(r.A.State[man].Rate, Is.InRange(0.9f, 1.1f)); }
            Assert.GreaterOrEqual(rates.Count, 4, "the men do not all work through the shot at one pace");
        }
    }
}
