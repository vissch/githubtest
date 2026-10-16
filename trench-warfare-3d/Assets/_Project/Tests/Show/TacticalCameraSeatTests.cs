// Phase: B1 (implemented 2026-10-16) — the standard view mirrors with the seat.
// The standard view read from the seat (LocalSeat), not from a hardcoded team 0: seated as player 1 "your men" are
// team 1, so the same focus that stood behind your men now stands behind theirs and the 21 degree turn mirrors.
using NUnit.Framework;
using Unity.Mathematics;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Match;

namespace TW.Tests
{
    public class TacticalCameraSeatTests
    {
        [TearDown] public void TearDown() => LocalSeat.Reset();

        static TacticalCamera NewCamera() => new GameObject("seat-cam").AddComponent<TacticalCamera>();

        [Test]
        public void TheViewMirrorsWithTheSeat()
        {
            using var m = MatchSim.CreateGreybox(SimConfig.Default);
            m.World.Spawn(0, 0, new float3(100f, 0f, 60f), 100f, 3f, false);
            m.World.Spawn(1, 0, new float3(100f, 0f, 260f), 100f, 3f, false);
            var cam = NewCamera();
            try
            {
                cam.Focus = new Vector2(100f, 60f);
                cam.ScanArmies(m.World, m.Map.SizeMeters.y);
                Assert.AreEqual(1f, cam.ToEnemy, 1e-4f, "seated as 0 the enemy lies along +Z");
                Assert.AreEqual(0f, cam.Along, 1e-3f, "that focus stands behind your own men");

                LocalSeat.Set(1);
                cam.ScanArmies(m.World, m.Map.SizeMeters.y);
                Assert.AreEqual(-1f, cam.ToEnemy, 1e-4f, "seated as 1 the enemy lies along -Z");
                Assert.AreEqual(1f, cam.Along, 1e-3f, "the same focus is now beyond the enemy");
            }
            finally { Object.DestroyImmediate(cam.gameObject); }
        }

        [Test]
        public void AMeleeKeepsYouBehindYourOwnMenSeatedAsOne()
        {
            using var m = MatchSim.CreateGreybox(SimConfig.Default);
            m.World.Spawn(0, 0, new float3(100f, 0f, 150f), 100f, 3f, false);
            m.World.Spawn(1, 0, new float3(100f, 0f, 160f), 100f, 3f, false);
            var cam = NewCamera();
            try
            {
                LocalSeat.Set(1);
                cam.Focus = new Vector2(100f, 160f);   // on your own man
                cam.ScanArmies(m.World, m.Map.SizeMeters.y);
                Assert.Less(cam.Along, 0.5f, "the collapsed span must not come out the wrong way round");
            }
            finally { Object.DestroyImmediate(cam.gameObject); }
        }
    }
}
