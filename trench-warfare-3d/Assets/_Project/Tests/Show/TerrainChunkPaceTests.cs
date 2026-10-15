// Phase: review fixes (CODE_REVIEW_2026-10-04, unit 18) - Gt3 the scorch mark pushed out of the 64-mark list,
// Gt4 the fully-filled repaint, Gt6 a wire breach, Gt8 a wreck's dark seat. Gt1 (a chunk under a barrage) is
// covered by integration's own fix and its tests. All drive pure helpers, so no scene and no editor are needed.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class TerrainChunkPaceTests
    {
        static TW.Sim.SimEvent Mark(float x, float z, float scalar)
        {
            var e = new TW.Sim.SimEvent();
            e.Type = TW.Sim.SimEventType.CraterStamp;
            e.Pos = new Unity.Mathematics.float3(x, 0f, z);
            e.Scalar = scalar;
            return e;
        }

        /// <summary>[Gt3] The 65th mark pushes the oldest out; its tiles must come back on the repaint queue, or its
        /// burn stays in the colour texture for good.</summary>
        [Test]
        public void TheMarkPushedOutIsRepaintedWithoutItsBurn()
        {
            const int mapW = 256, mapL = 256;
            var marks = new List<TW.Sim.SimEvent>();
            var born = new List<float>();
            for (int i = 0; i < GreyboxTerrainView.MaxScorchMarks; i++) { marks.Add(Mark(20f + i, 30f + i, 3f)); born.Add(i); }
            var oldest = marks[0];
            int sweep = 5;
            var repaint = new List<Vector2Int>();

            bool evicted = GreyboxTerrainView.MakeRoomForScorch(marks, born, ref sweep, mapW, mapL, repaint);

            Assert.IsTrue(evicted, "[Gt3] the 65th mark did not push the oldest out");
            Assert.AreEqual(GreyboxTerrainView.MaxScorchMarks - 1, marks.Count, "[Gt3] the mark list did not make room");
            Assert.AreEqual(GreyboxTerrainView.MaxScorchMarks - 1, born.Count, "[Gt3] the born list did not make room");
            Assert.AreEqual(4, sweep, "[Gt3] the sweep cursor did not move down with the list it indexes");

            var expected = new List<Vector2Int>();
            GreyboxTerrainView.ScorchTiles(oldest, mapW, mapL, expected);
            CollectionAssert.AreEquivalent(expected, repaint, "[Gt3] the tiles the evicted mark burned were not queued again");
            Assert.Contains(new Vector2Int((int)oldest.Pos.x / 2, (int)oldest.Pos.z / 2), repaint,
                "[Gt3] the evicted mark's own centre tile was not repainted");
        }

        /// <summary>[Gt3] With room to spare nothing is evicted and nothing is repainted.</summary>
        [Test]
        public void NothingIsRepaintedWhileThereIsRoom()
        {
            var marks = new List<TW.Sim.SimEvent> { Mark(10f, 10f, 3f) };
            var born = new List<float> { 0f };
            int sweep = 0;
            var repaint = new List<Vector2Int>();
            Assert.IsFalse(GreyboxTerrainView.MakeRoomForScorch(marks, born, ref sweep, 256, 256, repaint),
                "[Gt3] a mark was pushed out while the list had room");
            Assert.AreEqual(0, repaint.Count, "[Gt3] tiles were queued though no mark was pushed out");
            Assert.AreEqual(1, marks.Count, "[Gt3] the mark list changed while it had room");
        }

        /// <summary>[Gt4] A mark stops being re-queued exactly at SnowFillSeconds + one sweep cycle (RepaintEvery *
        /// marks): before that the visit that would paint it fully filled is not refused.</summary>
        [Test]
        public void AScorchMarkIsPaintedOnceItHasFilledIn()
        {
            const int marks = 64;
            var acceptedAges = new List<float>();
            float born = 0f;
            for (float t = 0f; t <= 400f; t += GreyboxTerrainView.RepaintEvery)
            {
                float age = t - born;
                if (GreyboxTerrainView.ScorchWorthRepaint(age, marks)) acceptedAges.Add(age);
            }

            Assert.IsTrue(acceptedAges.Exists(a => a >= GreyboxTerrainView.SnowFillSeconds),
                "[Gt4] no visit past SnowFillSeconds was accepted, so the mark never gets its fully-filled repaint");
            float cutoff = GreyboxTerrainView.SnowFillSeconds + 2 * GreyboxTerrainView.RepaintEvery * marks;
            Assert.IsFalse(acceptedAges.Exists(a => a > cutoff),
                "[Gt4] re-queuing never stopped: a visit well past the grace window was still accepted");
        }

        /// <summary>[Gt6] A wire breach never moves heights, so it must not be treated as ground-moving; a crater
        /// stamp does.</summary>
        [Test]
        public void AWireBreachDoesNotMoveTheGround()
        {
            Assert.IsTrue(GreyboxTerrainView.MovesGround(TW.Sim.SimEventType.CraterStamp),
                "[Gt6] a crater stamp was not treated as moving the ground");
            Assert.IsFalse(GreyboxTerrainView.MovesGround(TW.Sim.SimEventType.WireBreached),
                "[Gt6] a wire breach was treated as moving the ground, though it never raises or lowers heights");
        }

        /// <summary>[Gt8] A wreck that arrives in play gets the same dark seat a wreck placed at Start gets; a kind
        /// that casts no shade (Bridge, Cleared) reaches 0.</summary>
        [Test]
        public void AWreckThatArrivesInPlayGetsItsDarkSeat()
        {
            const int w = 80, l = 80;   // 40 x 40 m at half-metre cells
            var shade = new float[w * l];
            float reach = GreyboxTerrainView.ShadeReach(TW.Sim.Terrain.PropKind.Wreck);
            const float px = 10.25f, pz = 10.25f;   // a cell centre ((20+.5)*.5), so the wreck's own cell sits at distance 0

            GreyboxTerrainView.StampShade(shade, w, l, px, pz, reach);

            int underX = 20, underZ = 20;
            Assert.AreEqual(0.525f, shade[underZ * w + underX], 0.001f,
                "[Gt8] a wreck's own cell was not stamped dark (0.7 at the formula's edge, minus the SmoothStep bias)");
            int farX = underX + 2 * 10, farZ = underZ;   // 10 m away, past the 4.2 m wreck reach
            Assert.AreEqual(0f, shade[farZ * w + farX], 0.001f,
                "[Gt8] a cell well outside the wreck's reach was darkened");
            Assert.AreEqual(0f, GreyboxTerrainView.ShadeReach(TW.Sim.Terrain.PropKind.Cleared), "[Gt8] a cleared prop casts a shade");
            Assert.AreEqual(0f, GreyboxTerrainView.ShadeReach(TW.Sim.Terrain.PropKind.Bridge), "[Gt8] a bridge casts a shade");
        }
    }
}
