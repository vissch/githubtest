// Phase: A2 (implemented)
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Combat;
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

        [Test]
        public void ProneUnitInTrench_IsHiddenFromHorizontalFire()
        {
            using var map = GreyboxMapGenerator.Create(Allocator.Persistent);
            // shooter on the surface 40 m in front of the team 0 trench; target crouched inside it
            float3 shooter = new float3(150f, map.Height.Sample(150f, 170f) + 1.6f, 170f);
            float3 target = new float3(150f, map.Height.Sample(150f, 121f) + 1.0f, 121f);
            Assert.IsFalse(HeightfieldRaycast.HasLineOfSight(map.Height, shooter, target), "parapet hides a crouched defender");
            float3 fireStep = new float3(150f, map.Height.Sample(150f, 121f) + 1.8f + 1.5f, 121f);
            Assert.IsTrue(HeightfieldRaycast.HasLineOfSight(map.Height, shooter, fireStep), "a head on the fire-step is visible");
        }
    }
}
