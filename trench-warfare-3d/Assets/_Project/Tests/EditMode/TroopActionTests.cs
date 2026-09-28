// Phase: C1 (troop animation pass, 2026-09-28) — what each troop does on screen for the sim's actions that used to
// play nothing or the wrong clip: a close assault throws the grenade bundle, the MG changes its belt when the burst
// ends, the SMG fires the looped burst at the shoulder and the pistol a snap shot, a rifleman at the parapet leans
// into it to fire, the officer whistles and points, a man stopped in a crater under fire goes down into
// the bowl, the medic kneels and the repair man hammers at their work facing it, a kneeling man eases round (no stoop
// turn), a jetpack leap arcs and comes down, and a para is drawn landing. No sim is stepped: the tests place men, write
// the events the sim would, and tick the controller alone (as BlastReactionTests does).
using System;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using TW.Presentation;

namespace TW.Tests
{
    public class TroopActionTests
    {
        static readonly float3 Here = new float3(150f, 0f, 300f);

        sealed class Rig : IDisposable
        {
            public readonly MatchSim M;
            public readonly AnimationController A;
            public SimWorld W => M.World;
            NativeArray<ushort> row; NativeArray<float> phase, yaw;

            public Rig()
            {
                var cfg = SimConfig.Default;
                M = MatchSim.CreateGreybox(cfg);
                var stray = M.World.GetSystem<AmbientBombardmentSystem>();
                if (stray != null) stray.ShellsPerMinute = 0f;
                M.World.Tick = 1000;
                row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
                phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                A = new AnimationController(cfg.MaxSlots, M.Map, M.Gas, row, phase, yaw);
            }

            public int Man(float3 at, byte archetype = 0, byte team = 0) => W.Spawn(team, archetype, at, 100f, 3f, false);

            public void Tick()
            {
                A.Tick(W);
                A.Advance(W.Config.TickSeconds);
                W.Events.Clear();
                W.Tick++;
            }

            /// <summary>Ticks until the man plays the clip (true), or n ticks pass.</summary>
            public bool Until(int man, Clip clip, int n)
            {
                for (int t = 0; t < n; t++) { if (A.State[man].Clip == clip) return true; Tick(); }
                return A.State[man].Clip == clip;
            }

            public void Dispose()
            {
                A.Dispose(); M.Dispose();
                row.Dispose(); phase.Dispose(); yaw.Dispose();
            }
        }

        [Test]
        public void ACloseAssaultThrowsTheGrenadeBundle()
        {
            using var r = new Rig();
            int man = r.Man(Here), tank = r.Man(Here + new float3(0f, 0f, 6f), 0, 1);
            r.Tick();
            r.W.Events.Add(r.W.Tick, SimEventType.Shot, man, tank, Here, new float3(0f, 0f, 1f), 1f);
            r.Tick();
            Assert.AreEqual(Clip.Throw, r.A.State[man].Clip, "Shot with Scalar 1 is a close assault: the throw");
            Assert.AreEqual(0, r.A.State[man].Shots, "and it is no round out of his magazine");
        }

        /// <summary>Critic r1: the throw stood a prone man up, and the stance rung laid him down again, every 3 s.</summary>
        [Test]
        public void AManLyingFlatDoesNotStandUpToThrow()
        {
            using var r = new Rig();
            int man = r.Man(Here), tank = r.Man(Here + new float3(0f, 0f, 6f), 0, 1);
            r.W.StanceOf[man] = (byte)Stance.Prone;
            r.Tick();
            r.W.Events.Add(r.W.Tick, SimEventType.Shot, man, tank, Here, new float3(0f, 0f, 1f), 1f);
            r.Tick();
            Assert.AreNotEqual(Clip.Throw, r.A.State[man].Clip);
            Assert.AreEqual((byte)Stance.Prone, r.A.State[man].Stance, "he stays down");
        }

        [Test]
        public void AnOrdinaryShotIsStillAShot()
        {
            using var r = new Rig();
            int man = r.Man(Here), foe = r.Man(Here + new float3(0f, 0f, 60f), 0, 1);
            r.Tick();
            r.W.Events.Add(r.W.Tick, SimEventType.Shot, man, foe, Here, new float3(0f, 0f, 1f), 0f);
            r.Tick();
            Assert.AreEqual(Clip.FireSnap, r.A.State[man].Clip, "the control: a rifleman's shot");
        }

        [TestCase((byte)1, Clip.FireBurst)]
        [TestCase((byte)17, Clip.FireBurst)]
        [TestCase((byte)2, Clip.FireStoop)]
        [TestCase((byte)13, Clip.FireSnap)]
        [TestCase((byte)12, Clip.FireStand)]
        public void EachWeaponFiresItsOwnClipStanding(byte archetype, Clip expected)
        {
            using var r = new Rig();
            int man = r.Man(Here, archetype), foe = r.Man(Here + new float3(0f, 0f, 30f), 0, 1);
            r.Tick();
            r.W.Events.Add(r.W.Tick, SimEventType.Shot, man, foe, Here, new float3(0f, 0f, 1f), 0f);
            r.W.FireCooldown[man] = 7;   // the SMG's 3 rps: 7 ticks to the next round
            r.Tick();
            Assert.AreEqual(expected, r.A.State[man].Clip, "archetype " + archetype);
            if (expected != Clip.FireStoop && expected != Clip.FireBurst) return;
            for (int t = 0; t < 6; t++)
            {
                r.W.FireCooldown[man] = 6 - t;
                r.Tick();
                Assert.AreEqual(expected, r.A.State[man].Clip, "critic r1: the burst holds through the gap to the next round, tick " + t);
            }
        }

        [Test]
        public void TheMGChangesItsBeltWhenTheBurstEnds()
        {
            using var r = new Rig();
            int mg = r.Man(Here, 2);
            r.Tick();
            var s = r.A.State[mg]; s.Shots = 50; r.A.State[mg] = s;   // a belt fired off; the target is gone
            Assert.IsTrue(r.Until(mg, Clip.ReloadStand, 10), "the MG gunner changes the belt (it never played before)");
        }

        [Test]
        public void TheMedicKneelsToTheWoundedManFacingHim()
        {
            using var r = new Rig();
            int medic = r.Man(Here, 14), patient = r.Man(Here + new float3(1.5f, 0f, 0f));
            for (int t = 0; t < 12; t++) r.Tick();   // stopped a moment beside him (a stop-go in a column does not kneel)
            r.W.Events.Add(r.W.Tick, SimEventType.UnitHealed, medic, patient, r.W.Position[patient], default, 5f);
            r.Tick();
            var s = r.A.State[medic];
            Assert.AreEqual(Clip.ReloadKneel, s.Clip, "busy hands on one knee");
            Assert.AreEqual((byte)Stance.Crouch, s.Stance, "on one knee");
            Assert.AreEqual(math.PI / 2f, s.BodyYaw, 1e-3f, "turned to the man on his right");

            // critic r1: the sim's sweep names him one tick in five; between sweeps he stood up and knelt again
            for (int t = 0; t < 120; t++)
            {
                if (t % 5 == 4) r.W.Events.Add(r.W.Tick, SimEventType.UnitHealed, medic, patient, r.W.Position[patient], default, 5f);
                r.Tick();
                Assert.AreEqual(Clip.ReloadKneel, r.A.State[medic].Clip, "still at work, tick " + t);
            }
            for (int t = 0; t < 100; t++) r.Tick();   // the last one plays out, then he gets up
            Assert.AreNotEqual(Clip.ReloadKneel, r.A.State[medic].Clip, "the sweeps stopped: he is done");
        }

        /// <summary>Critic r2: the heal reaches 8 m; kneeling to a man that far off read as idling.</summary>
        [Test]
        public void AMedicFarFromThePatientDoesNotKneel()
        {
            using var r = new Rig();
            int medic = r.Man(Here, 14), patient = r.Man(Here + new float3(6f, 0f, 0f));
            for (int t = 0; t < 12; t++) r.Tick();
            r.W.Events.Add(r.W.Tick, SimEventType.UnitHealed, medic, patient, r.W.Position[patient], default, 5f);
            r.Tick();
            Assert.AreNotEqual(Clip.ReloadKneel, r.A.State[medic].Clip);
        }

        [Test]
        public void TheRepairManHammersAtTheVehicle()
        {
            using var r = new Rig();
            int fitter = r.Man(Here, 15), hull = r.Man(Here + new float3(0f, 0f, -3f), 0);
            r.W.Flags[hull] |= (uint)UnitFlags.Vehicle;   // the controller reads the slot as a machine (no figure) and the tend as mending
            r.W.TargetSlot[fitter] = hull;   // critic r2: the armed repair man nearly always has a target at the front
            for (int t = 0; t < 20; t++) r.Tick();   // stopped a moment; the rifle up at it (and turning to it) on the way
            r.W.Events.Add(r.W.Tick, SimEventType.VehicleHullMended, hull, fitter, r.W.Position[hull], default, 5f);
            r.Tick();
            Assert.AreEqual(Clip.MeleeSmash, r.A.State[fitter].Clip, "Smash reads as hammering");
            Assert.AreEqual(math.PI, math.abs(r.A.State[fitter].BodyYaw), 1e-3f, "facing the hull behind him");
        }

        [Test]
        public void AManStoppedInACraterUnderFireGoesDownIntoTheBowl()
        {
            using var r = new Rig();
            int man = r.Man(Here), foe = r.Man(Here + new float3(0f, 0f, 80f), 0, 1);
            var map = r.M.Map;
            int cell = map.NavIndex((int)(Here.x / MapData.NavCellSize), (int)(Here.z / MapData.NavCellSize));
            byte was = map.NavLayers[cell];
            map.NavLayers[cell] = (byte)(was | (byte)NavLayer.Crater);
            r.W.TargetSlot[man] = foe;
            bool down = false;
            for (int t = 0; t < 200 && !down; t++) { r.Tick(); down = r.A.State[man].Stance == (byte)Stance.Prone; }
            Assert.IsTrue(down, "stopped in the bowl with a target he lies down in it");
            // critic r1: a target lost for a moment had him up out of the bowl and back down
            r.W.TargetSlot[man] = -1;
            for (int t = 0; t < 30; t++) { r.Tick(); Assert.AreEqual((byte)Stance.Prone, r.A.State[man].Stance, "the target out of sight a moment: he stays in the bowl, tick " + t); }
            // critic r2: the crowd nudges him at 0.3 m/s; he used to be set upright by the crawl, then lay down again
            r.W.TargetSlot[man] = foe;
            for (int t = 0; t < 8; t++) { r.W.Position[man] += new float3(0.015f, 0f, 0f); r.Tick(); }
            for (int t = 0; t < 40; t++)
            {
                r.Tick();
                Assert.AreEqual((byte)Stance.Prone, r.A.State[man].Stance, "nudged, he stays in the bowl, tick " + t);
                Assert.AreNotEqual(Clip.StandToKneel, r.A.State[man].Clip);
            }
            map.NavLayers[cell] = was;

            using var c = new Rig();
            int open = c.Man(Here), foe2 = c.Man(Here + new float3(0f, 0f, 80f), 0, 1);
            c.W.TargetSlot[open] = foe2;
            for (int t = 0; t < 200; t++) c.Tick();
            Assert.AreNotEqual((byte)Stance.Prone, c.A.State[open].Stance, "the control: on open ground he stays up");
        }

        /// <summary>Critic r2: the "kneel turn" files are stoops; on a knee they popped him up into a squat and back.</summary>
        [Test]
        public void AKneelingManEasesRoundWithoutTheStoopTurn()
        {
            using var r = new Rig();
            int man = r.Man(Here), foe = r.Man(Here + new float3(60f, 0f, 0f), 0, 1);
            r.W.StanceOf[man] = (byte)Stance.Crouch;
            r.W.Yaw[man] = 0f;
            r.Tick();
            r.W.TargetSlot[man] = foe;
            for (int t = 0; t < 120; t++)
            {
                r.Tick();
                var c = r.A.State[man].Clip;
                Assert.IsFalse(c == Clip.KneelTurn90L || c == Clip.KneelTurn90R || c == Clip.StoopTurn90L || c == Clip.StoopTurn90R || c == Clip.StoopTurn180, "tick " + t + ": " + c);
            }
        }

        [Test]
        public void AJetpackLeapArcsAndComesDownWhereHeLands()
        {
            using var r = new Rig();
            int man = r.Man(Here, 17);
            r.Tick();
            r.W.StanceOf[man] = (byte)Stance.Leap;
            r.W.Events.Add(r.W.Tick, SimEventType.LeapStarted, man, 0, Here + new float3(0f, 0f, 18f), Here, 1.5f);
            r.Tick();
            Assert.AreEqual(Clip.Airborne, r.A.State[man].Clip, "the spring and the tuck, not the sprint he used to play in the air");
            uint started = r.A.State[man].ClipStart;
            for (int t = 0; t < 14; t++) r.Tick();
            Assert.AreEqual(Clip.Airborne, r.A.State[man].Clip, "held in the air");
            Assert.AreEqual(started, r.A.State[man].ClipStart, "and not restarted while he is in the air");
            Assert.Greater(r.A.Hop[man], 1.5f, "critic r2: mid-flight he is drawn high in his arc, not gliding along the ground");
            Assert.IsTrue(r.Until(man, Clip.JumpDown, 10), "0.6 s before he lands: the drop");
            Assert.GreaterOrEqual(r.A.State[man].Frame, 0.4f, "entered past its top, so its landing comes as he touches down");
            Assert.AreEqual(1f, r.A.State[man].Rate, 1e-3f, "critic r2: at speed, not in slow motion");
        }

        /// <summary>The owner's foxhole ask (2026-09-28): men at the parapet fired the standing rifle shot upright; now they
        /// lean into the parapet and fire over it, and still work the bolt after.</summary>
        [Test]
        public void ARiflemanAtTheParapetLeansInToFire()
        {
            using var r = new Rig();
            int man = r.Man(Here), foe = r.Man(Here + new float3(0f, 0f, 80f), 0, 1);
            r.W.Flags[man] |= (uint)UnitFlags.InTrench;
            r.W.StanceOf[man] = (byte)Stance.FireStep;
            for (int t = 0; t < 40; t++) r.Tick();   // settled at the parapet, standing (the fire step)
            r.W.Events.Add(r.W.Tick, SimEventType.Shot, man, foe, Here, new float3(0f, 0f, 1f), 0f);
            r.Tick();
            Assert.AreEqual(Clip.FireParapet, r.A.State[man].Clip);
            Assert.IsTrue(r.Until(man, Clip.ReloadBolt, 30), "then the bolt");

            using var c = new Rig();
            int open = c.Man(Here), foe2 = c.Man(Here + new float3(0f, 0f, 80f), 0, 1);
            c.Tick();
            c.W.Events.Add(c.W.Tick, SimEventType.Shot, open, foe2, Here, new float3(0f, 0f, 1f), 0f);
            c.Tick();
            Assert.AreEqual(Clip.FireSnap, c.A.State[open].Clip, "the control: in the open, the standing shot");
        }

        [Test]
        public void TheOfficerWhistlesThenPointsHisMenAtTheTarget()
        {
            using var r = new Rig();
            int officer = r.Man(Here, 12), foe = r.Man(Here + new float3(0f, 0f, 80f), 0, 1);
            for (int t = 0; t < 130; t++) r.Tick();   // long enough since any order
            r.W.TargetSlot[officer] = foe;
            Assert.IsTrue(r.Until(officer, Clip.OfficerWhistle, 5), "a target: the whistle");
            Assert.IsTrue(r.Until(officer, Clip.OfficerPoint, 260), "then he points his men at it");

            using var c = new Rig();
            int rifleman = c.Man(Here, 0), foe2 = c.Man(Here + new float3(0f, 0f, 80f), 0, 1);
            for (int t = 0; t < 130; t++) c.Tick();
            c.W.TargetSlot[rifleman] = foe2;
            for (int t = 0; t < 260; t++) { c.Tick(); var k = c.A.State[rifleman].Clip; Assert.IsFalse(k == Clip.OfficerWhistle || k == Clip.OfficerPoint, "the control: a rifleman gives no orders"); }
        }

        [Test]
        public void AParaIsDrawnLanding()
        {
            using var r = new Rig();
            r.Tick();
            int man = r.Man(Here, 16);
            r.W.Events.Add(r.W.Tick, SimEventType.DropLanded, man, 0, Here);
            r.Tick();
            Assert.AreEqual(Clip.JumpDown, r.A.State[man].Clip);
            Assert.GreaterOrEqual(r.A.State[man].Frame, 1.05f, "from the landing, not the top of the fall");
        }
    }
}
