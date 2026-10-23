// Phase: act-braced-weapons (2026-10-23) — a braced weapon is set down before it is fired. InfantrySpec.Braced (the
// machine gunner, the Sentry, the AT rifle) has always given these men the prone accuracy and the longer hold range in
// the sim, but nothing in Presentation read it: the AT rifleman fell to the rifle look and fired from the shoulder.
// The controller now takes a braced man down onto the bipod when he stops in the open (the stance ladder's own
// StandToKneel -> KneelToProne is the set-up seen between the stop and the first shot), rests the gun on the parapet in
// a trench (Unsling), and slings it again before he walks off. No sim rule changed; the knob fx.braced = 0 is the old
// look. No sim is stepped here: the men are placed, walked by hand, their Shot events written the way DirectFire would,
// and the controller ticked alone (InfantryFiringTests' rig).
using System;
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Tests
{
    public class BracedWeaponTests
    {
        static readonly float3 Where = new float3(150f, 0f, 300f);

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

            public int Man(byte arche, float dx, float dz, byte team = 0)
                => M.World.Spawn(team, arche, Where + new float3(dx, 0f, dz), 100f, 3f, false);

            /// <summary>He is shooting at `at`: the sim's target, and its cooldown just after a shot.</summary>
            public void Aim(int slot, int at, int cooldown = 41)
            {
                M.World.TargetSlot[slot] = at;
                M.World.FireCooldown[slot] = cooldown;
            }

            /// <summary>The shot leaves his gun this tick, as DirectFire writes it.</summary>
            public void Shot(int slot, int at) => M.World.Events.Add(M.World.Tick, SimEventType.Shot, slot, at, M.World.Position[slot], default, 0f);

            /// <summary>A step of his walk, before the tick that reads it (the controller's speed is the position delta).</summary>
            public void Step(int slot, float mps)
            {
                var p = M.World.Position[slot]; p.z += mps * M.World.Config.TickSeconds; M.World.Position[slot] = p;
            }

            /// <summary>He stands in a trench, on the firestep: the flag the controller reads, and a kneeling sim stance.</summary>
            public void InTrench(int slot)
            {
                M.World.Flags[slot] |= (uint)UnitFlags.InTrench;
                M.World.StanceOf[slot] = (byte)Stance.Crouch;
            }

            public void Tick()
            {
                A.Tick(M.World);
                A.Advance(M.World.Config.TickSeconds);
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

        /// <summary>The poses a braced weapon may be fired from: flat on the bipod, or rested on the parapet from the knee.</summary>
        static bool BracedPose(Clip c) => c == Clip.FireMG || c == Clip.FireProne || c == Clip.FireKneel;

        /// <summary>The man, settled where he is: aimed at his target but not yet firing. Returns every clip seen, in order.</summary>
        static List<Clip> Settle(Rig r, int man, int enemy, int ticks)
        {
            var seen = new List<Clip>();
            for (int t = 0; t < ticks; t++)
            {
                r.Aim(man, enemy);
                r.Tick();
                var c = r.ClipOf(man);
                if (seen.Count == 0 || seen[seen.Count - 1] != c) seen.Add(c);
            }
            return seen;
        }

        [Test]
        public void ABracedManFiresFromTheBipod([Values(2, 26, 27)] int arche)
        {
            using var r = new Rig();
            int man = r.Man((byte)arche, 0f, 0f);
            int enemy = r.Man((byte)InfantryArchetype.Rifle, 0f, 30f, 1);
            r.M.World.Events.Clear();
            Settle(r, man, enemy, 80);   // he sets up first: the stop, then the drop onto the bipod

            Clip want = arche == 27 ? Clip.FireProne : Clip.FireMG;   // the one-shot AT rifle, and the machine gun's loop
            int shots = 0, braced = 0;
            for (int t = 0; t < 200; t++)
            {
                r.Aim(man, enemy);
                bool shot = t % 25 == 0;
                if (shot) r.Shot(man, enemy);
                r.Tick();
                if (!shot) continue;
                shots++;
                var c = r.ClipOf(man);
                if (BracedPose(c)) braced++;
                Assert.AreEqual(want, c, "archetype " + arche + " fires from " + c + ", not the braced pose");
            }
            Assert.GreaterOrEqual(shots, 8, "the test fired too few shots to judge");
            Assert.GreaterOrEqual(braced / (float)shots, 0.9f, "archetype " + arche + ": only " + braced + " of " + shots + " shots are fired braced");
        }

        [Test]
        public void ABracedManInATrenchFiresRestedOnTheParapet([Values(2, 26, 27)] int arche)
        {
            using var r = new Rig();
            int man = r.Man((byte)arche, 0f, 0f);
            int enemy = r.Man((byte)InfantryArchetype.Rifle, 0f, 30f, 1);
            r.M.World.Events.Clear();
            r.InTrench(man);
            var seen = Settle(r, man, enemy, 80);
            Assert.Contains(Clip.Unsling, seen, "archetype " + arche + " in a trench never rests the gun on the parapet: " + string.Join(", ", seen));

            int shots = 0, braced = 0;
            for (int t = 0; t < 150; t++)
            {
                r.InTrench(man); r.Aim(man, enemy);
                bool shot = t % 25 == 0;
                if (shot) r.Shot(man, enemy);
                r.Tick();
                if (!shot) continue;
                shots++;
                if (BracedPose(r.ClipOf(man))) braced++;
            }
            Assert.GreaterOrEqual(braced / (float)shots, 0.9f, "archetype " + arche + " in a trench: only " + braced + " of " + shots + " shots are fired rested");
        }

        [Test]
        public void TheSetUpIsSeenBetweenTheStopAndTheFirstShot([Values(2, 26, 27)] int arche)
        {
            using var r = new Rig();
            int man = r.Man((byte)arche, 0f, 0f);
            int enemy = r.Man((byte)InfantryArchetype.Rifle, 0f, 30f, 1);
            r.M.World.Events.Clear();
            var seen = Settle(r, man, enemy, 80);
            Assert.IsTrue(seen.Contains(Clip.StandToKneel) || seen.Contains(Clip.KneelToProne),
                "archetype " + arche + " goes from the stop to the shot with no set-up: " + string.Join(", ", seen));
            Assert.AreEqual((byte)Stance.Prone, r.A.State[man].Stance, "archetype " + arche + " is not drawn flat once he has set up");
        }

        [Test]
        public void HePacksTheGunUpBeforeHeWalksOff()
        {
            using var r = new Rig();
            int man = r.Man((byte)InfantryArchetype.AtRifle, 0f, 0f);
            r.M.World.Events.Clear();
            for (int t = 0; t < 80; t++) r.Tick();   // stopped, no target: he is down on the bipod
            Assert.AreEqual((byte)Stance.Prone, r.A.State[man].Stance, "he never went flat");

            var seen = new List<Clip>();
            for (int t = 0; t < 120; t++)
            {
                r.Step(man, 0.8f);
                r.Tick();
                var c = r.ClipOf(man);
                if (seen.Count == 0 || seen[seen.Count - 1] != c) seen.Add(c);
            }
            int sling = seen.IndexOf(Clip.Sling);
            Assert.GreaterOrEqual(sling, 0, "he walks off without slinging the gun: " + string.Join(", ", seen));
            // the upright gaits only: a crawl before the sling is him still flat, which is right
            for (int k = 0; k < sling; k++)
                Assert.IsFalse(seen[k] == Clip.Walk || seen[k] == Clip.WalkAimed || seen[k] == Clip.WalkWary || seen[k] == Clip.StoopWalk,
                    "he is up and walking (" + seen[k] + ") before he has slung the gun: " + string.Join(", ", seen));
        }

        [Test]
        public void ARiflemanIsUnchanged()
        {
            using var r = new Rig();
            int man = r.Man((byte)InfantryArchetype.Rifle, 0f, 0f);
            int enemy = r.Man((byte)InfantryArchetype.Rifle, 0f, 30f, 1);
            r.M.World.Events.Clear();
            Settle(r, man, enemy, 80);
            Assert.AreEqual((byte)Stance.Standing, r.A.State[man].Stance, "the rifleman went flat");
            r.Aim(man, enemy); r.Shot(man, enemy); r.Tick();
            // the bolt rifle's own aimed shot (the ladder's `bolt ? FireSnap : FireStand`), not a braced pose
            Assert.AreEqual(Clip.FireSnap, r.ClipOf(man), "the rifleman fires from " + r.ClipOf(man));
        }
    }
}
