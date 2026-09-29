// Phase: A5b (2026-09-28) — how a machine drives, not only where (owner: "improve the vehicles driving … each its own
// speed, rhythm, style"). Before this every machine reached full speed in one tick, stopped dead in one, stopped to
// pivot on any sharp turn, and zig-zagged along the flow field's 45-degree steps. These hold what replaced that
// (VehicleKinematicsSystem): momentum at the profile's Accel and Brake, a pivot share per machine, and a field read
// followed a hull length ahead (Steer), a hull on its line steered round (Avoid), and a charge that still ends dead
// in the trench it hits.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class DriveFeelTests
    {
        static MatchSim NewMatch()
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            return MatchSim.CreateGreybox(cfg);
        }

        static void Step(MatchSim m)
        {
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            m.Step(none);
        }

        /// <summary>One machine of team 0, sent at a nav cell `dx` columns across and near the far end of the map.</summary>
        static int Spawn(MatchSim m, byte archetype, float3 at, int dx = 0)
        {
            var e = m.World.Units.Roster[archetype];
            int slot = m.World.Spawn(0, archetype, at, e.Hp, e.Speed, true);
            var c = m.Map.NavCellOf(at);
            m.World.GoalId[slot] = m.Fields.GetGoal(GoalKey.Cell(m.Map.NavIndex(math.clamp(c.x + dx, 1, m.Map.NavWidth - 2), m.Map.NavLength - 6), NavMode.Tracked));
            return slot;
        }

        static float SpeedOf(MatchSim m, int slot) => math.length(m.World.Velocity[slot]);

        /// <summary>Ticks from standing to 90 % of its roster speed on open ground.</summary>
        static int TicksToPace(byte archetype)
        {
            using var m = NewMatch();
            int it = Spawn(m, archetype, new float3(120f, 0f, 40f));
            float pace = m.World.Units.Roster[archetype].Speed * 0.9f;
            for (int t = 1; t <= 400; t++)
            {
                Step(m);
                if (SpeedOf(m, it) >= pace) return t;
            }
            return int.MaxValue;
        }

        [Test]
        public void AMachineGathersWayAndRunsOnToAStop()
        {
            using var m = NewMatch();
            int it = Spawn(m, VehicleArchetype.Maw, new float3(120f, 0f, 40f));
            float speed = m.World.Units.Roster[VehicleArchetype.Maw].Speed;
            Step(m);
            Assert.Less(SpeedOf(m, it), speed * 0.1f, "a landship does not leap to full speed in a tick");
            for (int t = 0; t < 120; t++) Step(m);
            Assert.Greater(SpeedOf(m, it), speed * 0.85f, "but it gets there in a few seconds");

            var prof = m.Vehicles.Profiles[VehicleArchetype.Maw];
            float3 before = m.World.Position[it];
            m.Vehicles.HaltTicks[it] = 200;            // a gun being laid
            int stopped = -1;
            for (int t = 1; t <= 200 && stopped < 0; t++) { Step(m); if (SpeedOf(m, it) < 1e-3f) stopped = t; }
            Assert.Greater(stopped, 1, "it runs on: a halt does not stop it dead");
            Assert.LessOrEqual(stopped, (int)math.ceil(speed / prof.BrakeOr / SimConfig.Default.TickSeconds) + 2, "and stops within its braking");
            Assert.Greater(math.distance(before.xz, m.World.Position[it].xz), 0.3f, "covering ground as it brakes");
        }

        [Test]
        public void EachMachineHasItsOwnPace()
        {
            int maw = TicksToPace(VehicleArchetype.Maw), tusk = TicksToPace(VehicleArchetype.Tusk), censer = TicksToPace(VehicleArchetype.Censer);
            Assert.Less(tusk, maw, "the light tank is off the mark before the landship");
            Assert.Less(censer, maw, "and the quick crab too");
            Assert.Greater(maw, 40, "the landship takes more than two seconds");
            Assert.Less(maw, 200, "but it does get going");
        }

        /// <summary>The slowest it goes, as a share of its roster speed, while it turns about to a point behind it.</summary>
        static float SlowestThroughATurnAbout(byte archetype)
        {
            using var m = NewMatch();
            int it = Spawn(m, archetype, new float3(120f, 0f, 40f));
            for (int t = 0; t < 160; t++) Step(m);
            float3 p = m.World.Position[it];
            m.Vehicles.Drive[it] = VehicleKinematicsSystem.DriveStraight;
            m.Vehicles.DriveTarget[it] = p - SimMath.DirFromYaw(m.World.Yaw[it]) * 60f;
            float speed = m.World.Units.Roster[archetype].Speed, least = float.MaxValue;
            for (int t = 0; t < 200; t++) { Step(m); least = math.min(least, SpeedOf(m, it)); }
            return least / speed;
        }

        [Test]
        public void AWalkerStepsRoundASharpTurnWhereALandshipStopsToPivot()
        {
            float maw = SlowestThroughATurnAbout(VehicleArchetype.Maw), pincer = SlowestThroughATurnAbout(VehicleArchetype.Pincer);
            Assert.Less(maw, 0.1f, "the landship stops to pivot");
            Assert.Greater(pincer, 0.3f, "the walker keeps walking round");
        }

        [Test]
        public void ItDrivesACurveNotTheFieldsStepsAndGetsThere()
        {
            // a goal a third of a column across per row: the 8-way field alternates north and north-east cells
            using var m = NewMatch();
            var start = new float3(80f, 0f, 30f);
            int it = Spawn(m, VehicleArchetype.Tusk, start, 30);
            for (int t = 0; t < 60; t++) Step(m);
            int flips = 0; float lastTurn = 0f, lastYaw = m.World.Yaw[it];
            for (int t = 0; t < 400; t++)
            {
                Step(m);
                float turn = SimMath.WrapAngle(m.World.Yaw[it] - lastYaw); lastYaw = m.World.Yaw[it];
                if (math.abs(turn) < 1e-4f) continue;
                if (lastTurn != 0f && math.sign(turn) != math.sign(lastTurn)) flips++;
                lastTurn = turn;
            }
            Assert.LessOrEqual(flips, 6, "it holds a line instead of hunting between two field steps");
            Assert.Greater(math.distance(start.xz, m.World.Position[it].xz), 40f, "and it never stalls against an edge");
        }

        /// <summary>A machine that cannot move stands on another's line (2026-09-29, a study of a real match: a
        /// Croaker stood 12 s shoving a halted Banner, the two hulls' push apart undoing every step it took, and its
        /// nose hunted 24 degrees either way the whole time). The one behind goes round.</summary>
        [TestCase(VehicleArchetype.Maw, 0.5f)]
        [TestCase(VehicleArchetype.Croaker, 0f)]
        [TestCase(VehicleArchetype.Tusk, 0f)]
        public void AMachineGoesRoundOneStandingOnItsLine(byte mover, float offset)
        {
            using var m = NewMatch();
            var start = new float3(120f, 0f, 40f);
            int it = Spawn(m, mover, start);
            var e = m.World.Units.Roster[VehicleArchetype.Maw];
            int wall = m.World.Spawn(0, VehicleArchetype.Maw, new float3(start.x + offset, 0f, 62f), e.Hp, e.Speed, true);
            m.World.GoalId[wall] = -1;
            Step(m);   // (a machine's first tick resets its drive state)
            m.World.GoalId[wall] = -1; m.World.Speed[wall] = 0f; m.Vehicles.HaltTicks[wall] = 100000;
            m.Vehicles.DitchTicks[wall] = 100000;   // it stands, and gives way to nobody (a ditched machine is not shoved)
            float blockZ = m.World.Position[wall].z;
            int stalled = 0;
            for (int t = 0; t < 1000 && m.World.Position[it].z < blockZ + 8f; t++)
            {
                Step(m);
                if (t > 100 && SpeedOf(m, it) < 0.2f) stalled++;
            }
            Assert.AreEqual(62f, m.World.Position[wall].z, 0.5f, "the one in the way stood its ground (else this proves nothing)");
            Assert.Greater(m.World.Position[it].z, blockZ + 8f, "it got past the one in its way");
            Assert.Less(stalled, 60, "without standing shoving it (ticks nearly stopped)");
        }

        /// <summary>A column: every machine sent at one goal drives one line, so the ones ahead sit in the cone of
        /// every one behind. Swerving round hulls that are driving on only weaves the followers: in three 150 s studies
        /// of a real match (MachineStudy) our side spun in place 111 s, the Brute (which keeps 0.08 of its speed
        /// through a sharp turn) 49 s of it, most 9 m behind a machine going 0.6-1.9 m/s. The study's own start: our
        /// fifteen machines in two ranks behind the spawn point, all sent forward. Nor does any nose hunt either way:
        /// letting go of a hull ahead whenever it kept pace with the one behind (whose speed drops as it swerves) had
        /// the Breaker and the Redoubt flip 68 and 72 times per 100 m here; alone, every machine flips none.</summary>
        [Test]
        public void AColumnOfEveryMachineDrivesOnWithoutTurningOnTheSpot()
        {
            using var m = NewMatch();
            byte[] cast = { 4, 5, 18, 6, 7, 8, 9, 10, 11, 19, 20, 21, 22, 23, 24 };
            var spawn = m.World.Init.SpawnA;
            var slots = new int[cast.Length];
            for (int n = 0; n < cast.Length; n++)
            {
                var e = m.World.Units.Roster[cast[n]];
                slots[n] = m.World.Spawn(0, cast[n], new float3(spawn.x + (n % 8 - 3.5f) * 12f, 0f, spawn.z - (n / 8) * 8f), e.Hp, e.Speed, true);
                m.World.GoalId[slots[n]] = m.Fields.DefaultGoal(0, true);
            }
            var yaw = new float[cast.Length]; var spin = new int[cast.Length];
            var flips = new int[cast.Length]; var lastRate = new float[cast.Length]; var metres = new float[cast.Length]; var at = new float3[cast.Length];
            for (int n = 0; n < cast.Length; n++) { yaw[n] = m.World.Yaw[slots[n]]; at[n] = m.World.Position[slots[n]]; }
            for (int t = 0; t < 3000; t++)
            {
                Step(m);
                for (int n = 0; n < cast.Length; n++)
                {
                    int i = slots[n];
                    float rate = math.degrees(SimMath.WrapAngle(m.World.Yaw[i] - yaw[n])) / SimConfig.Default.TickSeconds; yaw[n] = m.World.Yaw[i];
                    if (t > 100 && (m.World.Flags[i] & (uint)UnitFlags.Alive) != 0 && SpeedOf(m, i) < 0.3f && math.abs(rate) > 17f) spin[n]++;
                    metres[n] += math.length((m.World.Position[i] - at[n]).xz); at[n] = m.World.Position[i];
                    if (math.abs(rate) > 14f) { if (math.abs(lastRate[n]) > 14f && math.sign(rate) != math.sign(lastRate[n])) flips[n]++; lastRate[n] = rate; }
                }
            }
            int total = 0; float worst = 0f; byte worstOf = 0; var line = new System.Text.StringBuilder();
            for (int n = 0; n < cast.Length; n++)
            {
                total += spin[n];
                float per100 = 100f * flips[n] / math.max(20f, metres[n]);
                if (per100 > worst) { worst = per100; worstOf = cast[n]; }
                line.Append($"{cast[n]}: spin {spin[n]}, {per100:F1} flips/100 m; ");
            }
            TestContext.WriteLine($"spun in place {total} ticks; {line}");
            Assert.Less(worst, 10f, $"no nose hunted either way (flips per 100 m, the worst: archetype {worstOf})");
            Assert.Less(total, 60, "the column spun in place under 3 s between fifteen machines (ticks; 116 when it swerved round every hull ahead)");
        }

        /// <summary>Alone on broken ground, a machine drives its course without snaking about it. On the Shelled Forest
        /// (seed 1917, the study's map) each drove a near-straight 230 m, yet its turn reversed 18-27 times per 100 m:
        /// every step of the field's line (a cell's width) snapped its heading 15-20 degrees at the full turn rate, and
        /// its lane was taken and dropped as the turn moved its probe. Now 6-15.</summary>
        [TestCase(VehicleArchetype.Tusk)]
        [TestCase(VehicleArchetype.Kettle)]
        [TestCase(VehicleArchetype.Croaker)]
        public void AloneOnBrokenGroundItDoesNotSnake(byte archetype)
        {
            var cfg = SimConfig.Default; cfg.StartingSilver = 100000;
            using var m = MatchSim.CreateBattlefield(cfg, BattlefieldParams.ShelledForest(1917), false);
            var e = m.World.Units.Roster[archetype];
            int it = m.World.Spawn(0, archetype, m.World.Init.SpawnA, e.Hp, e.Speed, true);
            m.World.GoalId[it] = m.Fields.DefaultGoal(0, true);
            float yaw = m.World.Yaw[it], last = 0f, metres = 0f; var at = m.World.Position[it]; int flips = 0;
            for (int t = 0; t < 2400; t++)
            {
                Step(m);
                float rate = math.degrees(SimMath.WrapAngle(m.World.Yaw[it] - yaw)) / cfg.TickSeconds; yaw = m.World.Yaw[it];
                metres += math.length((m.World.Position[it] - at).xz); at = m.World.Position[it];
                if (math.abs(rate) > 14f) { if (math.abs(last) > 14f && math.sign(rate) != math.sign(last)) flips++; last = rate; }
            }
            float per100 = 100f * flips / math.max(1f, metres);
            TestContext.WriteLine($"{archetype}: {metres:F0} m, {flips} reversals, {per100:F1} per 100 m");
            Assert.Greater(metres, 120f, "it drove on");
            Assert.Less(per100, 16f, "its turn reversed under 16 times per 100 m (26-28 when every step of the line was a snap)");
        }
    }
}
