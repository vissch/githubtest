// Phase: A4 (2026-09-24) — the ground is dynamic for good, EXCEPT where a trench stands in it.
// Owner: the environment must be very malleable under high impacts and "the more explosions on 1 place on the
// battlefield the bigger the hole should become"; but this is a trench game, so "the ground near the trench wont be
// able to take alot of degradation. the trench allways need to stand and allways give limited protection".
//
// The test that matters most here is TheGroundDigsMoreTheFurtherItIsFromATrench: it shells a trench head-on and
// shows the ground moving MORE as it gets further away, against the bowl's own falloff, which is the only shape of
// evidence that separates the guard band from "the shell was just weaker out there".
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class DynamicGroundTests
    {
        // The playtest map: 300 x 480 m, trenches across the width at z 60, 140, 340, 420, each two nav cells deep,
        // so the z = 60 trench occupies world z 60..64. Ground is a ridge in z only, flat in x.
        const float TrenchZ = 60f, TrenchFarEdge = 64f, OpenZ = 240f, MidX = 150.5f;

        static MatchSim NewMatch(uint seed = 0xBEEF)
        {
            var cfg = SimConfig.Default; cfg.Seed = seed;
            return MatchSim.CreatePlaytest(cfg);
        }

        static void Step(MatchSim m)
        {
            using var none = new NativeArray<SimCommand>(0, Allocator.Temp);
            m.Step(none);
        }

        static float[] Snapshot(MapData map)
        {
            var h = new float[map.Height.Cm.Length];
            for (int i = 0; i < h.Length; i++) h[i] = map.Height.Cm[i] * 0.01f;
            return h;
        }

        /// <summary>How far the ground at this height cell has dropped since the snapshot (negative = it rose).</summary>
        static float Drop(MapData map, float[] before, int hx, int hz)
            => before[map.Height.Index(hx, hz)] - map.Height.HeightAtCell(hx, hz);

        static CraterStamp Shell(float x, float z, float radius, float depth)
            => new CraterStamp { Center = new float3(x, 0f, z), Radius = radius, Depth = depth };

        static int CraterCells(MapData map)
        {
            int n = 0;
            for (int i = 0; i < map.NavLayers.Length; i++) if (((NavLayer)map.NavLayers[i] & NavLayer.Crater) != 0) n++;
            return n;
        }

        [Test]
        public void EveryMapIsBornKnowingHowFarItsGroundIsFromATrench()
        {
            using var m = NewMatch();
            var map = m.Map;
            Assert.IsTrue(map.CellTrenchDist.IsCreated, "the band must be built by the generator");
            // a fresh map, before any trench is cut, must read FAR everywhere. If it read zero instead, every cell
            // would count as touching a trench and the generator's own shelling would be refused: the authored
            // battlefield came out flat the first time this landed.
            using (var bare = new MapData(99, new float2(40f, 40f), Allocator.Temp))
                for (int i = 0; i < bare.CellTrenchDist.Length; i++)
                    Assert.AreEqual(255, bare.CellTrenchDist[i], "an untrenched map is far from a trench, not on one");
            // a height cell inside the z = 60 trench reads zero, one well out in no man's land reads 'far'
            Assert.AreEqual(0, map.CellTrenchDist[map.Height.Index(150, 62)], "ground inside a trench is zero metres from it");
            Assert.AreEqual(255, map.CellTrenchDist[map.Height.Index(150, (int)OpenZ)], "no man's land is past the recorded range");
            Assert.Less(map.Bedrock, 0f, "the bedrock is below the lowest ground the map was generated with");
        }

        [Test]
        public void TheGroundDigsMoreTheFurtherItIsFromATrench()
        {
            using var m = NewMatch();
            var map = m.Map;
            var before = Snapshot(map);
            int hx = (int)MidX;

            // a heavy shell straight into the trench: wide enough that its bowl still reaches 10 m out
            Shell(MidX, 62.5f, 16f, 3f).ApplyDynamic(map);

            // beside the trench nothing moves at all
            Assert.AreEqual(0f, Drop(map, before, hx, 64), 1e-4f, "the metre behind the parapet does not move");
            // and from there outward the ground gives way progressively, THOUGH THE BOWL IS WEAKENING as it goes:
            // at 3, 4 and 6 m from the burst the bowl offers 0.92, 0.85 and 0.69 of its depth, so a rising series
            // here can only be the guard band opening up
            float d65 = Drop(map, before, hx, 65), d66 = Drop(map, before, hx, 66), d68 = Drop(map, before, hx, 68);
            Assert.Greater(d65, 0f, "five metres out the ground has started to give");
            Assert.Greater(d66, d65 * 1.5f, "further out it gives much more, against the bowl's own falloff");
            Assert.Greater(d68, d66 * 1.5f);
            Assert.Greater(d68, 1.4f, "well clear of the trench a 3 m shell digs most of its depth");
        }

        [Test]
        public void ATrenchStandsThroughABarrageWithItsFloorLayersAndPostsIntact()
        {
            using var m = NewMatch();
            var map = m.Map;
            var before = Snapshot(map);
            int trenchCells = map.TrenchCells.Length, fireSteps = map.FireStepCells.Length, links = map.LinkCells.Length;
            var layersBefore = new byte[map.NavLayers.Length];
            for (int i = 0; i < layersBefore.Length; i++) layersBefore[i] = map.NavLayers[i];

            for (int i = 0; i < 40; i++)
                Shell(MidX + (i % 5) - 2f, 62.5f + (i % 3) - 1f, 8f, 2.4f).ApplyDynamic(map);

            Assert.AreEqual(trenchCells, map.TrenchCells.Length, "forty shells took no cell out of the trench");
            Assert.AreEqual(fireSteps, map.FireStepCells.Length);
            Assert.AreEqual(links, map.LinkCells.Length, "the ladders are still there");
            for (int hz = 60; hz < 64; hz++)
                Assert.AreEqual(0f, Drop(map, before, (int)MidX, hz), 1e-4f, "the trench floor is where it was");
            Assert.AreEqual(0f, Drop(map, before, (int)MidX, 64), 1e-4f, "and so is the metre behind it");
            for (int i = 0; i < layersBefore.Length; i++)
                if (((NavLayer)layersBefore[i] & (NavLayer.Trench | NavLayer.Link)) != 0)
                    Assert.AreEqual(layersBefore[i], map.NavLayers[i], "a trench cell never becomes a shell hole: no cave-in");
            for (int i = 0; i < map.CellTrenchId.Length; i++)
                if (((NavLayer)layersBefore[i] & NavLayer.Trench) != 0)
                    Assert.AreNotEqual(-1, map.CellTrenchId[i], "and it still belongs to its trench, so its posts stand");
        }

        [Test]
        public void ASecondShellInTheSameHoleWidensItRatherThanOnlyDeepeningIt()
        {
            using var m = NewMatch();
            var map = m.Map;
            Assert.AreEqual(0, map.Holes.Length);

            Shell(MidX, OpenZ, 3f, 1.2f).ApplyDynamic(map);
            Assert.AreEqual(1, map.Holes.Length);
            Assert.AreEqual(1, map.Holes[0].Hits);
            Assert.AreEqual(3f, map.Holes[0].Radius, 1e-4f);
            int firstFootprint = CraterCells(map);
            float firstMiddle = map.Height.HeightAtCell((int)MidX, (int)OpenZ);

            Shell(MidX + 1f, OpenZ, 3f, 1.2f).ApplyDynamic(map);   // well inside MergeShare * 3 = 2.25 m
            Assert.AreEqual(1, map.Holes.Length, "one hole, not two");
            Assert.AreEqual(2, map.Holes[0].Hits);
            Assert.AreEqual(math.sqrt(9f + 0.6f * 9f), map.Holes[0].Radius, 1e-3f, "R' = sqrt(R^2 + 0.6 r^2)");
            Assert.Greater(CraterCells(map), firstFootprint, "the hole covers more ground than it did");
            Assert.Less(map.Height.HeightAtCell((int)MidX, (int)OpenZ), firstMiddle, "and is deeper in the middle");
        }

        [Test]
        public void AShelledSpotGrowsOneBigHoleThatStopsAtMaxRadius()
        {
            using var m = NewMatch();
            var map = m.Map;
            float last = 0f;
            for (int i = 0; i < 60; i++)
            {
                Shell(MidX, OpenZ, 3f, 1.2f).ApplyDynamic(map);
                Assert.AreEqual(1, map.Holes.Length, "sixty shells on one spot are one hole");
                float r = map.Holes[0].Radius;
                Assert.GreaterOrEqual(r, last, "a hole never shrinks");
                Assert.LessOrEqual(r, CraterStamp.MaxRadius + 1e-3f, "and never grows past MaxRadius");
                last = r;
            }
            Assert.Greater(last, 9f, "sixty shells make a crater far wider than the shell that made them");
        }

        [Test]
        public void SpoilIsThrownUpOutsideTheHoleAndABarrageNeverBuildsAMountain()
        {
            using var m = NewMatch();
            var map = m.Map;
            var before = Snapshot(map);

            Shell(MidX, OpenZ, 3f, 1.2f).ApplyDynamic(map);
            // the ring R..1.3R: 3 to 3.9 m out, peaking in the middle of it
            float rise = -Drop(map, before, (int)MidX, (int)OpenZ + 3);
            Assert.Greater(rise, 0.05f, "a shell throws its spoil up outside the lip");

            for (int i = 0; i < 80; i++) Shell(MidX, OpenZ, 3f, 1.2f).ApplyDynamic(map);

            float highest = 0f;
            for (int hz = (int)OpenZ - 20; hz <= (int)OpenZ + 20; hz++)
            for (int hx = (int)MidX - 20; hx <= (int)MidX + 20; hx++)
                highest = math.max(highest, -Drop(map, before, hx, hz));
            Assert.LessOrEqual(highest, CraterStamp.MaxRim + 0.02f,
                "eighty shells raise no more spoil than one hole is ever allowed: RimUp telescopes to MaxRim");
        }

        [Test]
        public void AMoundRaisesTheGroundAndCoversLikeAShellHole()
        {
            using var m = NewMatch();
            var map = m.Map;
            var before = Snapshot(map);
            float z = OpenZ + 40f;

            new CraterStamp { Center = new float3(MidX, 0f, z), Radius = 4f, Depth = 0.45f, Kind = (int)CraterKind.Mound }.ApplyDynamic(map);

            // the sampled cell sits half a metre off the middle, where the dome offers cos(pi/8) of its height,
            // so the heap is 0.43 m of its 0.45: assert the shape, not a hand-computed constant
            float rise = -Drop(map, before, (int)MidX, (int)z);
            Assert.Greater(rise, 0.40f, "rubble piles the ground UP");
            Assert.LessOrEqual(rise, 0.45f + 0.01f, "and never higher than the heap it was given");
            Assert.AreNotEqual(NavLayer.None, map.LayerAt(new float3(MidX, 0f, z)) & NavLayer.Crater,
                "and it is broken ground: cost and cover like a shell hole");
            Assert.AreEqual(0, map.Holes.Length, "a heap is not a hole, so nothing merges into it");
        }

        [Test]
        public void NoBarrageEverDigsThroughTheBedrockOrBlocksTheGround()
        {
            using var m = NewMatch();
            var map = m.Map;
            map.WaterLevel = 0.2f;
            for (int i = 0; i < 120; i++) Shell(MidX, OpenZ, 5f, 3f).ApplyDynamic(map);

            // only the shelled ground: the generator's own trench floors are cut far below the water table, and a
            // shell is not allowed to take the blame for them
            float lowest = float.MaxValue;
            for (int hz = (int)OpenZ - 20; hz <= (int)OpenZ + 20; hz++)
            for (int hx = (int)MidX - 20; hx <= (int)MidX + 20; hx++)
                lowest = math.min(lowest, map.Height.HeightAtCell(hx, hz));
            Assert.GreaterOrEqual(lowest, map.Bedrock - 0.01f, "a shell cannot dig through the bedrock");
            Assert.GreaterOrEqual(lowest, map.WaterLevel - CraterStamp.MaxUnderWater - 0.01f, "nor far under the water table");
            for (int i = 0; i < map.NavLayers.Length; i++)
                Assert.AreEqual(NavLayer.None, (NavLayer)map.NavLayers[i] & NavLayer.Blocked, "a shell never makes ground impassable");
        }

        [Test]
        public void TheGroundIsInTheTickHash()
        {
            using var m = NewMatch();
            ulong before = m.World.Hash();
            Shell(MidX, OpenZ, 4f, 1.6f).ApplyDynamic(m.Map);
            Assert.AreNotEqual(before, m.World.Hash(),
                "the heightfield, the layers and the holes are compared state now (TerrainHashSystem, replay v4)");
        }

        [Test]
        public void TwoMachinesShellingTheSameGroundAgreeOnIt()
        {
            ulong Run()
            {
                using var m = NewMatch();
                for (int i = 0; i < 6; i++)
                {
                    m.Blast.Queue(new Impact
                    {
                        Pos = new float3(MidX + i, 0f, OpenZ + i * 0.5f), Damage = 150f, Radius = 8f,
                        Suppression = 50f, CraterRadius = 3f, CraterDepth = 1.2f, Player = -1,
                    });
                    Step(m);
                }
                for (int i = 0; i < 10; i++) Step(m);   // let the deformation queue drain
                return m.World.Hash();
            }
            Assert.AreEqual(Run(), Run(), "the same shells on the same ground give the same ground");
        }
    }
}
