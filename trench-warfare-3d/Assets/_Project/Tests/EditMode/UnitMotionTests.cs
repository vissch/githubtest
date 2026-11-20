// Phase: unit motion (2026-09-30) — the troops and machines doing on screen what the sim says they do: the jetpack's leap
// drawn on an arc that tops out near 6 m and comes down where and when the sim lands him, in the air in an airborne
// pose and down in a crouch; the paratroopers' canopies falling to the points the sim will put each man on (the sim's
// own dice, read ahead); a round the shield plate stopped drawn as a brace, not a wound; the medic kneeling to the man
// he tends and the engineer looking over the machine he mends; the machine gunner changing his belt; the Kettle's
// mortar laid at a lobbing angle; the Skimmer knocked out settling onto its pods; the Breaker's cycle as a pose.
// The controller tests place men and write the events the way the sim would, then tick the controller alone
// (BlastReactionTests' rig); the drop's forecast runs the real sim.
using System;
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using TW.Presentation;

namespace TW.Tests
{
    public class UnitMotionTests
    {
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
                if (stray != null) stray.ShellsPerMinute = 0f;
                M.World.Tick = 1000;
                row = new NativeArray<ushort>(cfg.MaxSlots, Allocator.Persistent);
                phase = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                yaw = new NativeArray<float>(cfg.MaxSlots, Allocator.Persistent);
                A = new AnimationController(cfg.MaxSlots, M.Map, M.Gas, row, phase, yaw);
            }

            public SimWorld W => M.World;
            public float Dt => M.World.Config.TickSeconds;

            /// <summary>Open ground in the middle of the greybox, clear of its trenches.</summary>
            public float3 Open(float dx = 0f, float dz = 0f)
            {
                for (float z = 200f; z < M.Map.SizeMeters.y - 200f; z += 4f)
                {
                    var at = new float3(M.Map.SizeMeters.x * 0.5f, 0f, z);
                    bool open = true;
                    for (int oz = -12; oz <= 40 && open; oz += 2) for (int ox = -12; ox <= 12; ox += 2)
                        if ((M.Map.LayerAt(at + new float3(ox, 0f, oz)) & (NavLayer.Trench | NavLayer.Blocked | NavLayer.Link | NavLayer.Wire | NavLayer.Crater)) != 0) { open = false; break; }
                    if (open) return at + new float3(dx, 0f, dz);
                }
                Assert.Fail("setup: no open ground"); return default;
            }

            public int Man(byte archetype, float3 at) => W.Spawn(0, archetype, at, 100f, 3f, false);

            public void Tick()
            {
                A.Tick(W);
                A.Advance(Dt);
                W.Events.Clear();
                W.Tick++;
            }

            public void Dispose()
            {
                A.Dispose(); M.Dispose();
                row.Dispose(); phase.Dispose(); yaw.Dispose();
            }
        }

        // ------------------------------------------------------------------ the jetpack
        [Test]
        public void TheLeapsArcTopsOutNearSixMetresForTheFullJump_AndIsNothingAtEitherEnd()
        {
            float2 a = new float2(10f, 10f), b = new float2(10f, 38f);
            float peak = UnitMotion.LeapPeak(28f);
            Assert.AreEqual(5.9f, peak, 0.2f, "the owner's 6 m for a 28 m leap");
            Assert.AreEqual(0f, UnitMotion.ArcHeight(a, b, a, peak), 1e-4f, "on the ground at the take-off");
            Assert.AreEqual(0f, UnitMotion.ArcHeight(a, b, b, peak), 1e-4f, "on the ground at the landing");
            Assert.AreEqual(peak, UnitMotion.ArcHeight(a, b, (a + b) * 0.5f, peak), 1e-3f, "at the top half way");
            Assert.Greater(UnitMotion.ArcHeight(a, b, math.lerp(a, b, 0.25f), peak), 0.7f * peak, "a parabola, not a spike");
            Assert.AreEqual(2f, UnitMotion.LeapPeak(4f), 1e-4f, "a short hop still leaves the ground");
        }

        [Test]
        public void AJetpackManIsDrawnInTheAirOnHisLeap_AndLandsInACrouchOnTheTickTheSimPutsHimDown()
        {
            using var r = new Rig();
            float3 from = r.Open(), to = from + new float3(0f, 0f, 28f);
            int man = r.Man(InfantryArchetype.Jetpack, from);
            r.Tick();
            int ticks = (int)math.ceil(28f / (12f * r.Dt));   // LeapSystem.Jump: 12 m/s
            r.W.StanceOf[man] = (byte)Stance.Leap;
            r.W.Events.Add(r.W.Tick, SimEventType.LeapStarted, man, -1, to, from, ticks * r.Dt);
            r.Tick();
            Assert.AreEqual(Clip.ClimbOut, r.A.State[man].Clip, "the spring off the ground");
            Assert.AreEqual(UnitMotion.LeapPeak(28f), r.A.LeapPeak[man], 1e-4f, "the renderer is told how high to draw him");
            bool held = false, sprinted = false, down = false; Clip landed = Clip.None; float landFrame = 0f;
            for (int t = 1; t <= ticks; t++)
            {
                r.W.Position[man] = math.lerp(from, to, t / (float)ticks);   // MoveJob's straight line
                if (t == ticks) r.W.StanceOf[man] = (byte)Stance.Standing;
                r.Tick();
                var s = r.A.State[man];
                if (s.Clip == Clip.Sprint || s.Clip == Clip.Run) sprinted = true;
                if (t == ticks / 2) held = s.Clip == Clip.JumpDown && s.Rate < 0.1f && math.abs(s.Frame - AnimationController.JumpAir) < 0.05f;
                if (t == ticks - 1) down = s.Clip == Clip.JumpDown && s.Rate > 0.9f;
                if (t == ticks) { landed = s.Clip; landFrame = s.Frame; }
            }
            Assert.IsFalse(sprinted, "he flies: he does not sprint along the ground at 12 m/s");
            Assert.IsTrue(held, "held on the airborne frame half way over");
            Assert.IsTrue(down, "coming down in the last few ticks");
            Assert.AreEqual(Clip.JumpDown, landed);
            Assert.AreEqual(AnimationController.JumpTouch, landFrame, 0.08f, "his feet meet the ground on the landing tick");
            Assert.Greater(r.A.LeapPeak[man], 0f, "the arc is still drawn the tick he lands (the drawn position is a tick behind)");
            r.Tick();
            Assert.AreEqual(0f, r.A.LeapPeak[man], "and gone after it");
            bool rose = false;
            for (int t = 0; t < 30 && !rose; t++) { r.Tick(); rose = r.A.State[man].Clip == Clip.ClimbLand; }
            Assert.IsTrue(rose, "the landing crouch rises through Jump Down's recovery");
        }

        // ------------------------------------------------------------------ the paratroopers
        [Test]
        public void EveryCanopyComesDownOnThePointAndTheTickTheSimPutsItsManThere()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000; cfg.Seed = 0xBEEF; cfg.FactionA = (byte)FactionId.Brass; cfg.FactionB = (byte)FactionId.Iron;
            using var m = MatchSim.CreateGreybox(cfg);
            short rear = m.Fields.RearTrench(1);
            var def = m.Map.Trenches[rear];
            float rearZ = m.Map.NavCellCenter(m.Map.TrenchCells[def.CellStart + def.CellCount / 2]).z;
            float3 at = default; bool found = false;
            for (float x = 40f; x < m.Map.SizeMeters.x - 40f && !found; x += 4f)
            {
                at = new float3(x, 0f, rearZ - OffMapAbilitySystem.ParaDropKeepOut - 20f);
                found = true;
                for (int dz = -8; dz <= 8 && found; dz += 2) for (int dx = -8; dx <= 8; dx += 2)
                    if ((m.Map.LayerAt(at + new float3(dx, 0f, dz)) & (NavLayer.Trench | NavLayer.Blocked | NavLayer.Link)) != 0) { found = false; break; }
            }
            Assert.IsTrue(found, "setup: open ground for a drop");
            OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.ParaDrop, out var stats);
            var forecast = new float3[8]; int men = 0; uint landTick = 0;
            var landed = new List<SimEvent>();
            using (var cmd = new NativeArray<SimCommand>(new[] { new SimCommand { Tick = m.World.Tick, Player = 0, Type = CommandType.SupportFire, A = (int)OffMapAbilityId.ParaDrop, Pos = at } }, Allocator.Temp))
                m.Step(cmd);
            for (int t = 0; t < stats.WarmupTicks + 5; t++)
            {
                var ev = m.World.Events.Events;
                for (int k = 0; k < ev.Length; k++)
                {
                    var e = ev[k];
                    if (e.Type == SimEventType.DropInbound)
                    {
                        landTick = e.Tick + (uint)math.round(e.Scalar / m.World.Config.TickSeconds);
                        men = UnitMotion.ParaLandings(m.World, m.Map, e.A, e.Pos, landTick, e.B, stats.Radius, forecast);
                    }
                    if (e.Type == SimEventType.DropLanded) landed.Add(e);
                }
                using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
                m.Step(none);
            }
            Assert.AreEqual(8, men, "eight canopies forecast");
            Assert.AreEqual(8, landed.Count, "eight men put down");
            int matched = 0;
            var used = new bool[8];
            foreach (var e in landed)
            {
                Assert.AreEqual(landTick, e.Tick, "each on the tick the canopies were timed to");
                for (int k = 0; k < men; k++)
                    if (!used[k] && math.distance(forecast[k], e.Pos) < 0.01f) { used[k] = true; matched++; break; }
            }
            Assert.AreEqual(8, matched, "every man lands on a forecast point (not a canopy drifting to where nobody lands)");
        }

        [Test]
        public void AParatroopersStickLeavesTheAircraftInTheOrderItLands_AndHisCanopyBringsHimDownOnTime()
        {
            float3 drop = new float3(100f, 0f, 100f), head = UnitMotion.DropHeading(0);
            float early = UnitMotion.ParaExit(drop, head, drop + new float3(0f, 0f, -10f)), late = UnitMotion.ParaExit(drop, head, drop + new float3(0f, 0f, 10f));
            Assert.Less(early, late, "the man landing nearer the run's start goes out first");
            Assert.Greater(early, 0f, "nobody jumps before the aircraft is there");
            float land = 5f;
            UnitMotion.ParaFall(late + 0.1f, late, land, out float h0, out _, out float open0);
            UnitMotion.ParaFall(late + UnitMotion.DropOpenAfter + 0.3f, late, land, out float h1, out _, out float open1);
            UnitMotion.ParaFall(land, late, land, out float h2, out float along, out _);
            Assert.AreEqual(0f, open0, "falling, before the canopy opens");
            Assert.AreEqual(1f, open1, 1e-4f, "open a moment later");
            Assert.Greater(h0, h1);
            Assert.AreEqual(0f, h2, 1e-4f, "on the ground at the landing");
            Assert.AreEqual(1f, along, 1e-4f, "on his point");
        }

        [Test]
        public void AParatrooperLandsInTheCrouchAndRisesOutOfIt()
        {
            using var r = new Rig();
            int man = r.Man(InfantryArchetype.Para, r.Open());
            r.W.Events.Add(r.W.Tick, SimEventType.DropLanded, man, 0, r.W.Position[man]);
            r.Tick();
            Assert.AreEqual(Clip.JumpDown, r.A.State[man].Clip);
            Assert.AreEqual(AnimationController.JumpTouch, r.A.State[man].Frame, 0.06f, "his feet meet the ground");
            bool rose = false;
            for (int t = 0; t < 20 && !rose; t++) { r.Tick(); rose = r.A.State[man].Clip == Clip.ClimbLand; }
            Assert.IsTrue(rose);
        }

        // ------------------------------------------------------------------ the shield, the medic, the engineer, the MG
        [Test]
        public void ARoundTheShieldPlateStopsIsABrace_NotAWound()
        {
            using var r = new Rig();
            float3 at = r.Open();
            int man = r.Man(InfantryArchetype.Shield, at), shooter = r.W.Spawn(1, 0, at + new float3(0f, 0f, 60f), 100f, 3f, false);
            for (int t = 0; t < 20; t++) r.Tick();
            r.W.Events.Add(r.W.Tick, SimEventType.Hit, shooter, man, at, new float3(0f, 0f, -1f), -30f);
            r.W.Events.Add(r.W.Tick, SimEventType.ShieldBlocked, man, shooter, at, new float3(0f, 0f, -1f), 30f);
            r.Tick();
            var c = r.A.State[man].Clip;
            Assert.AreEqual(Clip.Duck, c, "he braces behind the plate");
            // and a real wound is still a wound
            for (int t = 0; t < 60; t++) r.Tick();
            r.W.Events.Add(r.W.Tick, SimEventType.Hit, shooter, man, at, new float3(0f, 0f, -1f), 30f);
            r.Tick();
            Assert.AreEqual(Clip.HitStand, r.A.State[man].Clip);
        }

        [Test]
        public void AMedicKneelsToTheManHeTends_FacingHim()
        {
            using var r = new Rig();
            float3 at = r.Open();
            int medic = r.Man(InfantryArchetype.Medic, at), patient = r.Man(InfantryArchetype.Rifle, at + new float3(2f, 0f, 0f));
            for (int t = 0; t < 20; t++) r.Tick();
            for (int t = 0; t < 30; t++)
            {
                if (t % 5 == 0) r.W.Events.Add(r.W.Tick, SimEventType.UnitHealed, medic, patient, r.W.Position[patient], default, 1.25f);
                r.Tick();
            }
            var s = r.A.State[medic];
            Assert.AreEqual(Clip.KneelInspect, s.Clip, "on one knee over him, hands busy");
            Assert.AreEqual(math.PI / 2f, s.BodyYaw, 1e-3f, "facing him (+x)");
            // the sweeps stop: he gets up again
            bool up = false;
            for (int t = 0; t < 120 && !up; t++) { r.Tick(); up = r.A.State[medic].Stance == (byte)Stance.Standing && r.A.State[medic].Clip != Clip.KneelInspect; }
            Assert.IsTrue(up, "back on his feet when the work is done");
        }

        [Test]
        public void AnEngineerMendingAMachineStandsLookingItOver()
        {
            using var r = new Rig();
            float3 at = r.Open();
            int man = r.Man(InfantryArchetype.Repair, at);
            int tank = r.W.Spawn(0, VehicleArchetype.Maw, at + new float3(0f, 0f, 5f), 3000f, 1.6f, true);
            for (int t = 0; t < 20; t++) r.Tick();
            for (int t = 0; t < 30; t++)
            {
                if (t % 5 == 0) r.W.Events.Add(r.W.Tick, SimEventType.VehicleHullMended, tank, man, r.W.Position[tank], default, 10f);
                r.Tick();
            }
            var s = r.A.State[man];
            Assert.AreEqual(Clip.FidgetInspect, s.Clip);
            Assert.AreEqual(0f, s.BodyYaw, 1e-3f, "facing the hull (+z)");
        }

        [Test]
        public void TheMachineGunnerChangesHisBeltWhenHisBurstEnds()
        {
            using var r = new Rig();
            float3 at = r.Open();
            int mg = r.Man(InfantryArchetype.Machinegunner, at), foe = r.W.Spawn(1, 0, at + new float3(0f, 0f, 80f), 100f, 3f, false);
            r.W.StanceOf[mg] = (byte)Stance.Prone;
            for (int t = 0; t < 60; t++) r.Tick();
            for (int t = 0; t < 60; t++)   // 60 rounds, a belt and then some
            {
                r.W.TargetSlot[mg] = foe;
                r.W.Events.Add(r.W.Tick, SimEventType.Shot, mg, foe, at, new float3(0f, 0f, 1f), 0f);
                r.Tick();
            }
            r.W.TargetSlot[mg] = -1;
            bool belt = false;
            for (int t = 0; t < 40 && !belt; t++) { r.Tick(); belt = r.A.State[mg].Clip == Clip.ReloadProne; }
            Assert.IsTrue(belt, "the belt change (Prone Reloading) once the burst is over");
        }

        // ------------------------------------------------------------------ the machines
        [Test]
        public void TheMortarLobs_EightyDegreesCloseInToFortyFiveAtFullRange()
        {
            Assert.AreEqual(80f, math.degrees(UnitMotion.MortarPitch(46f, 300f)), 0.5f, "the Kettle's minimum range");
            Assert.AreEqual(45f, math.degrees(UnitMotion.MortarPitch(300f, 300f)), 0.5f, "its longest throw");
            float mid = math.degrees(UnitMotion.MortarPitch(150f, 300f));
            Assert.Greater(mid, 60f); Assert.Less(mid, 80f);
            Assert.Greater(UnitMotion.MortarPitch(100f, 300f), UnitMotion.MortarPitch(200f, 300f), "steeper as the target comes in");
            Assert.Greater(math.degrees(UnitMotion.MortarRest), 22f, "never the direct-fire angle the old code clamped it to");
        }

        [Test]
        public void AKnockedOutSkimmerSettlesOntoItsPods_AndStaysDown()
        {
            UnitMotion.SkimmerSettle(0f, 6f, out float d0, out float b0, out float s0, out float t0);
            Assert.AreEqual(0f, d0); Assert.AreEqual(1f, b0); Assert.AreEqual(0f, s0); Assert.AreEqual(0f, t0);
            UnitMotion.SkimmerSettle(0.6f, 6f, out float d1, out float b1, out float s1, out _);
            Assert.AreEqual(UnitMotion.SettleDrop, d1, 1e-4f, "down the height of its cushion in 0.6 s");
            Assert.AreEqual(0f, b1, 1e-4f, "no more bob");
            UnitMotion.SkimmerSettle(30f, 6f, out float d2, out _, out float s2, out float t2);
            Assert.AreEqual(UnitMotion.SettleDrop, d2, 1e-4f, "a wreck stays down");
            Assert.Greater(s2, 2f, "a few metres of slide at 6 m/s");
            Assert.LessOrEqual(s2, UnitMotion.SkidMax);
            Assert.Greater(s2, s1);
            Assert.AreEqual(UnitMotion.SettleTilt, t2, 1e-4f, "tipped onto a pod");
            UnitMotion.SkimmerSettle(30f, 0f, out _, out _, out float still, out _);
            Assert.AreEqual(0f, still, "a machine that was standing does not slide");
        }

        [Test]
        public void TheBreakerSquatsAndShakesInItsWindUp_LiftsItsNoseToCharge_AndLungesToStrike()
        {
            var rest = UnitMotion.Breaker(0, 1f, 2f);
            Assert.AreEqual(0f, rest.Pitch); Assert.AreEqual(0f, rest.Shake); Assert.AreEqual(0f, rest.Lunge);
            var early = UnitMotion.Breaker(1, 0.3f, 3f); var late = UnitMotion.Breaker(1, 2.8f, 3f);
            Assert.Greater(late.Shake, 2f * early.Shake, "the shiver builds through the wind-up");
            Assert.Greater(late.Pitch, 0f, "squatting on its tail: nose up");
            Assert.Less(late.Heave, 0f);
            Assert.Greater(late.Smoke, early.Smoke);
            var charge = UnitMotion.Breaker(2, 0.5f, 8f);
            Assert.Greater(charge.Pitch, math.radians(3f), "the nose lifts to charge");
            Assert.AreEqual(1f, charge.Dust, "behind a wall of dust");
            float peak = 0f; float at = 0f;
            for (float t = 0f; t < 2f; t += 0.01f) { float l = UnitMotion.Breaker(3, t, 2f).Lunge; if (l > peak) { peak = l; at = t; } }
            Assert.AreEqual(0.9f, peak, 0.01f, "a short lunge");
            Assert.Less(at, 0.2f, "at once");
            Assert.AreEqual(0f, UnitMotion.Breaker(3, 1f, 2f).Lunge, 1e-4f, "and back");
            Assert.Less(UnitMotion.Breaker(4, 1f, 10f).Pitch, 0f, "backing out nose-down");
        }
    }
}
