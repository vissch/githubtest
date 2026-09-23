// Phase: A2 (implemented)
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Combat;
using TW.Sim;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class HeightfieldRaycastTests
    {
        [Test]
        public void FlatGround_HasLineOfSight()
        {
            var hf = new Heightfield(50, 50, 1f, Allocator.Temp);
            Assert.IsTrue(HeightfieldRaycast.HasLineOfSight(hf, new float3(2f, 1.6f, 2f), new float3(45f, 1.6f, 40f)));
            hf.Dispose();
        }

        [Test]
        public void Wall_BlocksLineOfSight_ButNotBehindTarget()
        {
            var hf = new Heightfield(50, 50, 1f, Allocator.Temp);
            for (int z = 0; z < 50; z++) hf.Set(25, z, 5f); // 5 m ridge along x = 25
            Assert.IsFalse(HeightfieldRaycast.HasLineOfSight(hf, new float3(5f, 1.6f, 10f), new float3(45f, 1.6f, 10f)));
            Assert.IsTrue(HeightfieldRaycast.HasLineOfSight(hf, new float3(5f, 1.6f, 10f), new float3(20f, 1.6f, 10f)));
            // shooting from above the ridge down to the target: clear
            Assert.IsTrue(HeightfieldRaycast.HasLineOfSight(hf, new float3(5f, 12f, 10f), new float3(45f, 1.6f, 10f)));
            hf.Dispose();
        }

        /// <summary>
        /// What this replaced, and why it had to be replaced rather than extended. The old version read:
        ///
        ///     target   = (150, Sample(150, 121) + 1.0, 121)
        ///     fireStep = (150, Sample(150, 121) + 1.8 + 1.5, 121)
        ///
        /// Both points stand at the SAME x and z. 121 is the FLOOR cell of the greybox trench - zBase 60, cells 60
        /// and 61, the step at 61, world z 122 to 124 - so the "fire step" point never touched a fire-step cell.
        /// It asserted that a head 3.3 m above the trench floor is visible and one 1.0 m above it is not: higher is
        /// easier to see, which the fire step has nothing to do with.
        ///
        /// MEASURED on that trench: floor -1.20, step top -0.49 (a rise of 0.71), lip in front 0.62. A real head on
        /// the step stands at 1.02 and clears the lip by 0.40 m. The old test's point stood at 2.81 and cleared it
        /// by 2.19 m. Remove FireStepRise from the generator altogether and that point only drops to 2.11, still
        /// 1.49 m clear - the test passed on a map with no fire steps in it. Neither FireStepRise nor EyeHeight was
        /// referenced: 1.8 and 1.5 were literals, and 1.8 is the CARVE DEPTH, not the rise.
        ///
        /// The comparison below is the one that bites. A man STANDING at full fire-step eye height on the trench
        /// FLOOR is hidden - measured in 78 of 78 sampled columns over both maps, head at 0.30 against a lip at
        /// 0.62 - so the 0.7 m rise is exactly and only what buys him the shot. Shrink the rise and this fails.
        /// </summary>
        [Test]
        public void OnlyTheRiseLetsAManShootOverHisOwnParapet_Greybox()
        {
            using var map = GreyboxMapGenerator.Create(Allocator.Persistent);
            var r = Look(map, 0, every: 7, outward: 40f);
            Assert.Greater(r.Tested, 5, "the trench has fire steps to walk");
            // flat ground by construction, so the claim is exact here and there is no fraction to choose
            Assert.AreEqual(r.Tested, r.StepSeen, $"a man on the fire step is exposed: seen in {r.StepSeen} of {r.Tested} columns");
            Assert.AreEqual(0, r.StandingOnFloorSeen,
                $"a man standing at fire-step height on the FLOOR was visible in {r.StandingOnFloorSeen} of {r.Tested} "
                + "columns, so the rise is not what is buying the shot and this test no longer measures the fire step");
            Assert.AreEqual(0, r.CrouchedSeen, "a crouched man below the rim is hidden");
            Assert.Greater(r.WorstMargin, 0.15f,
                $"the head clears the lip by only {r.WorstMargin:F2} m (measured 0.40 m when this was written); "
                + "any less and bilinear sampling decides who can shoot");
            Assert.Less(r.WorstMargin, 1.0f,
                $"the head clears the lip by {r.WorstMargin:F2} m, far more than the 0.40 m this is built on. Either "
                + "the trench changed or this test has drifted back to asserting that higher is easier to see.");
        }

        /// <summary>
        /// The same claim on the map the game plays. Asserted as a CONTRAST rather than as a percentage: craters,
        /// rolling ground and the river legitimately break some sightlines across no man's land, and a fraction
        /// fitted to one run would be a number chosen to pass rather than a claim about the trench.
        /// </summary>
        [Test]
        public void OnlyTheRiseLetsAManShootOverHisOwnParapet_OnTheMapWeActuallyPlay()
        {
            using var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(1917), Allocator.Persistent);
            var r = Look(map, 1, every: 3, outward: 40f);   // trench 1 is team 0's front line; 40 m out is no man's land
            Assert.Greater(r.Tested, 8, "the front trench has fire steps to walk");
            Assert.Greater(r.StepSeen, 0, $"not one of {r.Tested} men on the fire step could be seen from 40 m out");
            Assert.AreEqual(0, r.StandingOnFloorSeen,
                $"{r.StandingOnFloorSeen} of {r.Tested} men standing at fire-step height on the FLOOR were visible, "
                + "so on this map the rise is not what exposes them");
            Assert.AreEqual(0, r.CrouchedSeen, "a crouched man below the rim is hidden on ShelledForest too");
            Assert.Greater(r.WorstMargin, 0.15f, $"the tightest head-over-lip margin is {r.WorstMargin:F2} m (measured 0.35)");
        }

        struct Seen { public int Tested, StepSeen, StandingOnFloorSeen, CrouchedSeen; public float WorstMargin; }

        /// <summary>
        /// Walks the trench's own FireStepCells and the floor cell immediately behind each, and puts the same man
        /// at the same eye height in both places. Everything comes from the map and from
        /// HeightfieldRaycast.EyeHeight, so a change to the carve, the rise or the eye heights moves it.
        /// </summary>
        static Seen Look(MapData map, int trench, int every, float outward)
        {
            var def = map.Trenches[trench];
            var r = new Seen { WorstMargin = float.MaxValue };
            int back = def.OwnerTeam == 0 ? -1 : 1;          // away from the enemy
            float front = def.OwnerTeam == 0 ? outward : -outward;
            for (int k = 0; k < def.FireStepCount; k += every)
            {
                int fs = map.FireStepCells[def.FireStepStart + k];
                int floorCell = fs + back * map.NavWidth;
                if (floorCell < 0 || floorCell >= map.CellTrenchId.Length) continue;
                if (map.CellTrenchId[floorCell] != def.Id) continue;   // a column only one cell deep has no floor behind
                var c = map.NavCellCenter(fs);
                var f = map.NavCellCenter(floorCell);
                float sz = c.z + front;
                if (sz < 2f || sz > map.SizeMeters.y - 2f) continue;

                float stepTop = map.Height.Sample(c.x, c.z), floorTop = map.Height.Sample(f.x, f.z);
                float eye = HeightfieldRaycast.EyeHeight(Stance.FireStep);
                float3 shooter = new float3(c.x, map.Height.Sample(c.x, sz) + EyeHeightInTheOpen, sz);
                float3 onStep = new float3(c.x, stepTop + eye, c.z);
                float3 onFloor = new float3(f.x, floorTop + eye, f.z);
                float3 crouched = new float3(f.x, floorTop + HeightfieldRaycast.EyeHeight(Stance.Crouch), f.z);

                r.Tested++;
                if (HeightfieldRaycast.HasLineOfSight(map.Height, shooter, onStep)) r.StepSeen++;
                if (HeightfieldRaycast.HasLineOfSight(map.Height, shooter, onFloor)) r.StandingOnFloorSeen++;
                if (HeightfieldRaycast.HasLineOfSight(map.Height, shooter, crouched)) r.CrouchedSeen++;
                float lip = map.Height.Sample(c.x, c.z + (def.OwnerTeam == 0 ? 2f : -2f));
                r.WorstMargin = math.min(r.WorstMargin, onStep.y - lip);
            }
            return r;
        }

        /// <summary>A man in the open, upright: HeightfieldRaycast.EyeHeight's own default case.</summary>
        static readonly float EyeHeightInTheOpen = HeightfieldRaycast.EyeHeight(Stance.Standing);
    }
}
