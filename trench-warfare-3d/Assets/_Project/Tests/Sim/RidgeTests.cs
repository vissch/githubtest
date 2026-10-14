// Phase: the Ridge (board item idea-the-ridge-one-side-holds-the-high-ground) — the sim half: one front trench on
// a crest, the other in the hollow 9 m below.
//
// Every number here is read back from the finished map (map.Height, map.CellTrenchId), never from
// BattlefieldGenerator.RiseAt: the formula being right is not the claim, the GROUND being right is. Eight seeds,
// 1917 among them, because one bad seed is a real finding. Rise 0 is guarded by
// BattlefieldTests.Rise_Zero_LeavesTheThreePresets_Unchanged, not here.
//
// Fix A (owner, 2026-10-13): the profile's plateau ends at the crest trench's OWN line per nav column, not one
// straight crest line across the field. Two bars below are datumed against the trench's parapet rather than its
// carved floor (owner, 2026-10-15 00:10): the carve cuts 1.8 m down across every trench cell and gives the fire
// step 0.7 m back (GreyboxMapGenerator.AddFireTrench), so 2 m in front of the line used to still be inside the
// hole. The owner's numbers, 0.5 m and 4.0 m, are kept exactly; only the datum moved.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class RidgeTests
    {
        static readonly uint[] Seeds = { 1917u, 2201u, 2318u, 3001u, 42u, 7u, 99u, 12345u };

        /// <summary>The seed the Ridge is pitched on.</summary>
        const uint ShippedSeed = 1917u;

        /// <summary>Trench id 2 is the far front line (high Z) and sits on the crest; id 1 is the hollow's front.</summary>
        const short Crest = 2, Hollow = 1;

        static MapData[] maps;

        [OneTimeSetUp]
        public void BuildMaps()
        {
            maps = new MapData[Seeds.Length];
            for (int i = 0; i < Seeds.Length; i++)
                maps[i] = BattlefieldGenerator.Create(BattlefieldParams.Ridge(Seeds[i]), Allocator.Persistent);
        }

        [OneTimeTearDown]
        public void DisposeMaps()
        {
            for (int i = 0; i < maps.Length; i++) maps[i].Dispose();
            maps = null;
        }

        /// <summary>Mean ground height over every nav cell of one trench line, in metres.</summary>
        static float TrenchHeight(MapData map, short id)
        {
            float sum = 0f; int n = 0;
            for (int z = 0; z < map.NavLength; z++)
            for (int x = 0; x < map.NavWidth; x++)
                if (map.CellTrenchId[map.NavIndex(x, z)] == id)
                {
                    sum += map.Height.Sample((x + 0.5f) * MapData.NavCellSize, (z + 0.5f) * MapData.NavCellSize);
                    n++;
                }
            Assert.Greater(n, 0, "trench " + id + " is not in the map at all");
            return sum / n;
        }

        /// <summary>The trench's own middle in metres in one nav column, or -1 where the line is absent there.</summary>
        static float LineZ(MapData map, short id, int navX)
        {
            int first = -1, last = -1;
            for (int z = 0; z < map.NavLength; z++)
                if (map.CellTrenchId[map.NavIndex(navX, z)] == id) { if (first < 0) first = z; last = z; }
            if (first < 0) return -1f;
            return (first + last + 1) * 0.5f * MapData.NavCellSize;
        }

        /// <summary>The downhill boundary of the trench's nearest cell to the foot of the slope, in metres, or -1
        /// where the line is absent in this column.</summary>
        static float FrontEdgeZ(MapData map, short id, int navX)
        {
            int minZ = int.MaxValue;
            for (int z = 0; z < map.NavLength; z++)
                if (map.CellTrenchId[map.NavIndex(navX, z)] == id) minZ = math.min(minZ, z);
            return minZ == int.MaxValue ? -1f : minZ * MapData.NavCellSize;
        }

        /// <summary>Mean ground height across the whole width at one z, in metres.</summary>
        static float AcrossWidth(MapData map, float z)
        {
            float sum = 0f; int n = 0;
            for (int x = 0; x < map.Height.Width; x++) { sum += map.Height.Sample(x + 0.5f, z); n++; }
            return sum / n;
        }

        /// <summary>The same, averaged over a band of whole metres.</summary>
        static float AcrossWidth(MapData map, float z0, float z1)
        {
            float sum = 0f; int n = 0;
            for (float z = z0; z <= z1; z += 1f) { sum += AcrossWidth(map, z); n++; }
            return sum / n;
        }

        /// <summary>The nav column at the middle of the field.</summary>
        static int MidColumn(MapData map) => map.NavWidth / 2;

        /// <summary>The crest trench's raw platform in one column: 4 m uphill of its own line, which is on the
        /// level shelf step 1 of Create gives it (5 m uphill, pinned to the line) and clear of the carve, which
        /// reaches at most one nav cell (2 m) either side of the line's middle. Equal to the carved floor + 1.8 m
        /// (GreyboxMapGenerator.FireStepRise's carve depth), which is the parapet the owner's answer means.</summary>
        static float Parapet(MapData map, int navX, float lineZ)
        {
            float x = (navX + 0.5f) * MapData.NavCellSize, z = lineZ + 4f;   // uphill, since Rise > 0 on the Ridge preset
            int navZ = math.clamp((int)(z / MapData.NavCellSize), 0, map.NavLength - 1);
            Assert.AreNotEqual(Crest, map.CellTrenchId[map.NavIndex(navX, navZ)],
                "the parapet sample at +4 m landed back on the crest trench (a wide traverse) - needs the nearest clear z between 3 and 6 m uphill");
            return map.Height.Sample(x, z);
        }

        /// <summary>The idea in one number: the crest trench stands 9 m over the hollow's. The tolerance is the
        /// 0.4 m of dug-in noise each platform carries, on both of them.</summary>
        [Test]
        public void TheCrestStandsNineMetresOverTheHollow()
        {
            foreach (var map in maps)
            {
                float d = TrenchHeight(map, Crest) - TrenchHeight(map, Hollow);
                Assert.AreEqual(9f, d, 0.8f,
                    "the crest trench stands " + d + " m over the hollow's, not the 9 m the Ridge is for");
            }
        }

        /// <summary>The bank, measured against the crest trench's parapet rather than its carved floor (owner,
        /// 2026-10-15): the raw platform minus the open ground 15 m downhill of it (z 153..157). The lower bar is
        /// the owner's 4.0 m, kept as it is. The upper bar is raised from 7.0 to 7.5 m because the datum itself
        /// moved up by the 1.8 m carve depth: the floor-datum fall measured on this lane's committed ground
        /// (commit 73dd2b87e) was 3.41 to 4.98 m over the eight seeds, so the parapet-datum fall is 5.21 to 6.78 m;
        /// 7.5 m leaves headroom without weakening what the bar tests.</summary>
        [Test]
        public void TheBankIsSteep()
        {
            foreach (var map in maps)
            {
                float sum = 0f; int n = 0;
                for (int nx = 0; nx < map.NavWidth; nx++)
                {
                    float lineZ = LineZ(map, Crest, nx);
                    if (lineZ < 0f) continue;
                    sum += Parapet(map, nx, lineZ); n++;
                }
                Assert.Greater(n, 0, "no crest trench column found");
                float fall = sum / n - AcrossWidth(map, 153f, 157f);
                Assert.GreaterOrEqual(fall, 4.0f, "the bank falls only " + fall + " m over its 15 m run");
                Assert.LessOrEqual(fall, 7.5f, "the bank falls " + fall + " m over its 15 m run, a cliff");
            }
        }

        /// <summary>The long slope below the bank: 55 m of open ground at the design's 0.08 per metre. Both z are
        /// clear of every trench's blend zone.</summary>
        [Test]
        public void TheSlopeIsGentle()
        {
            foreach (var map in maps)
            {
                float g = (AcrossWidth(map, 155f) - AcrossWidth(map, 100f)) / 55f;
                Assert.GreaterOrEqual(g, 0.05f, "the slope is " + g + " per metre, nearly flat");
                Assert.LessOrEqual(g, 0.11f, "the slope is " + g + " per metre, a wall");
            }
        }

        /// <summary>Levelling the crest trench downhill as well would put a flat lip in front of it and blind it.
        /// Column by column, the ground 2 m downhill of the trench's own front edge must already have fallen away
        /// below its parapet. Measured against the floor this bar could never pass (the design's bank falls only
        /// 0.303 m per metre, far less than the 1.8 m carve plus 0.5 m); measured against the parapet, and from the
        /// front edge rather than the line, it tests what the owner meant: no flat lip blinds the crest.</summary>
        [Test]
        public void NoFlatLipBeforeTheCrest()
        {
            foreach (var map in maps)
            {
                for (int nx = 0; nx < map.NavWidth; nx++)
                {
                    float lineZ = LineZ(map, Crest, nx);
                    if (lineZ < 0f) continue;
                    float x = (nx + 0.5f) * MapData.NavCellSize;
                    float platform = Parapet(map, nx, lineZ);
                    float frontEdge = FrontEdgeZ(map, Crest, nx);
                    float below = map.Height.Sample(x, frontEdge - 2f);
                    Assert.LessOrEqual(below, platform - 0.5f,
                        "column " + nx + ": the ground 2 m in front of the crest is only "
                        + (platform - below) + " m below the parapet - a flat lip blinds the crest");
                }
            }
        }

        /// <summary>What the high ground is worth. From a fire step on the crest a man sees the foot of the slope
        /// 70 m below him (the sight line falls 0.146 per metre, the ground 0.30 then 0.08). From the hollow's
        /// front trench the crest's rear is dead ground: at 30 m behind the crest line on every seed, and at the
        /// design's 15 m on the shipped seed.</summary>
        [Test]
        public void TheCrestSeesTheFootAndTheHollowDoesNotSeeTheRear()
        {
            foreach (var map in maps)
            {
                int nx = MidColumn(map);
                float x = (nx + 0.5f) * MapData.NavCellSize;
                float crestZ = LineZ(map, Crest, nx), hollowZ = LineZ(map, Hollow, nx);
                Assert.Greater(crestZ, 0f, "no crest trench in the middle column");
                Assert.Greater(hollowZ, 0f, "no hollow trench in the middle column");

                Assert.IsTrue(HeightfieldRaycast.HasLineOfSight(map.Height,
                        Eye(map, x, crestZ, Stance.FireStep), Eye(map, x, crestZ - 70f, Stance.Standing)),
                    "the crest cannot see the foot of its own slope");

                Assert.IsFalse(HeightfieldRaycast.HasLineOfSight(map.Height,
                        Eye(map, x, hollowZ, Stance.FireStep), Eye(map, x, crestZ + 30f, Stance.Standing)),
                    "the hollow sees a standing man 30 m behind the crest - the rear is not dead ground");
            }

            // The design's own 15 m number, on the seed the Ridge is pitched on.
            for (int i = 0; i < Seeds.Length; i++)
            {
                if (Seeds[i] != ShippedSeed) continue;
                var map = maps[i];
                int nx = MidColumn(map);
                float x = (nx + 0.5f) * MapData.NavCellSize;
                float crestZ = LineZ(map, Crest, nx), hollowZ = LineZ(map, Hollow, nx);
                Assert.IsFalse(HeightfieldRaycast.HasLineOfSight(map.Height,
                        Eye(map, x, hollowZ, Stance.FireStep), Eye(map, x, crestZ + 15f, Stance.Standing)),
                    "the hollow sees a standing man 15 m behind the crest, the design's number");
            }
        }

        static float3 Eye(MapData map, float x, float z, Stance s) =>
            new float3(x, map.Height.Sample(x, z) + HeightfieldRaycast.EyeHeight(s), z);

        /// <summary>A slope is no good if it cuts the field in two: both sides must still reach each other.</summary>
        [Test]
        public void BothSidesStillReachEachOther()
        {
            foreach (var map in maps)
                Assert.IsTrue(BattlefieldGenerator.Connected(map), "the ridge cut the map in two");
        }
    }
}
