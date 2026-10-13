// Phase: review fixes (CODE_REVIEW_2026-10-04, unit 18) - Gt1 a chunk mid-rebuild under a barrage, Gt3 the scorch
// mark pushed out of the 64-mark list.
// Unit 18 review fixes: the chunk rebuild under a sustained barrage [Gt1] and the scorch mark pushed out of the
// 64-mark list [Gt3]. Both drive the pure helpers on GreyboxTerrainView, so no scene and no editor are needed.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class TerrainChunkPaceTests
    {
        /// <summary>[Gt1] The finding's own numbers: a 13-row chunk at a budget of 2 rows a frame (about 7 frames),
        /// with a crater arriving every 6th frame. The old rule put NextRow back to 0 on every crater, so the chunk
        /// reached at most 12 of its 13 rows and the mesh never uploaded.</summary>
        [Test]
        public void AChunkUnderABarrageStillFinishesItsRows()
        {
            const int rowsInChunk = 13, rowsAFrame = 2, craterEvery = 6;
            int nextRow = 0; bool dirty = true, again = false;
            int uploadedAtFrame = -1; bool dirtyAfterUpload = false;

            for (int frame = 1; frame <= 40 && uploadedAtFrame < 0; frame++)
            {
                if (frame % craterEvery == 0) GreyboxTerrainView.Restale(ref nextRow, ref dirty, ref again);
                if (!dirty) continue;
                for (int r = 0; r < rowsAFrame && nextRow < rowsInChunk; r++) nextRow++;
                if (nextRow < rowsInChunk) continue;
                uploadedAtFrame = frame;
                GreyboxTerrainView.Uploaded(ref nextRow, ref dirty, ref again);
                dirtyAfterUpload = dirty;
            }

            Assert.AreEqual(7, uploadedAtFrame, "[Gt1] a chunk under a barrage never finished its rows");
            Assert.IsTrue(dirtyAfterUpload, "[Gt1] the chunk read while the ground moved was not read over afterwards");
            Assert.AreEqual(0, nextRow, "[Gt1] the second pass did not start at row 0");
            Assert.IsFalse(again, "[Gt1] the read-it-over flag was not cleared on upload");
        }

        /// <summary>[Gt1] A chunk not yet started just starts: no extra pass.</summary>
        [Test]
        public void AChunkDirtiedBeforeItStartsIsReadOnlyOnce()
        {
            int nextRow = 0; bool dirty = false, again = false;
            GreyboxTerrainView.Restale(ref nextRow, ref dirty, ref again);
            GreyboxTerrainView.Restale(ref nextRow, ref dirty, ref again);
            Assert.IsFalse(again, "[Gt1] a chunk still at row 0 was queued for a second pass");
            nextRow = 5;
            GreyboxTerrainView.Uploaded(ref nextRow, ref dirty, ref again);
            Assert.IsFalse(dirty, "[Gt1] a chunk read in one go stayed dirty");
        }

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
    }
}
