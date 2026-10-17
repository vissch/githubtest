// Phase: B2 (presentation-only terrain chunk scheduling; found by look-code-presentation-terrain-20261017)
// A crater's whole-field chunk restart and whole-field
// chunk-work block (GreyboxTerrainView.Update) were both far wider than the staleness a rescan actually guards —
// RefreshHollows only changes ground within HollowReachMeters of the crater. These tests pin the two statics the
// fix reasons with (Overlaps, ChunkWaits) and run a frame-by-frame simulation of the chunk loop's scheduling rule,
// keeping the old whole-field rule as the oracle that proves the bug: under it, a chunk far from the shelling
// never finishes.
using NUnit.Framework;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class TerrainChunkScheduleTests
    {
        [Test]
        public void Overlaps_WorkedExamples()
        {
            // chunk at X0 = 64, 32 m wide: [64, 96)
            Assert.IsFalse(GreyboxTerrainView.Overlaps(64f, 0f, 96f, 1f, 0f, 0f, 20f, 1f), "crater region 0-20 does not reach the chunk");
            Assert.IsTrue(GreyboxTerrainView.Overlaps(64f, 0f, 96f, 1f, 70f, 0f, 90f, 1f), "64 < 90 and 70 < 96: they overlap");
        }

        [Test]
        public void ChunkWaits_OnlyWhenDirtyAndInTheRegion()
        {
            Assert.IsTrue(GreyboxTerrainView.ChunkWaits(true, true));
            Assert.IsFalse(GreyboxTerrainView.ChunkWaits(true, false));
            Assert.IsFalse(GreyboxTerrainView.ChunkWaits(false, true));
            Assert.IsFalse(GreyboxTerrainView.ChunkWaits(false, false));
        }

        const int ChunkCount = 16, RowsPerChunk = 65, RowsPerFrame = 3, ChunkMeters = 32;
        const int AwayChunk = 2; // X0 = 64, the first chunk outside chunk 0's crater region ([-10, 42)); the cursor reaches it first

        sealed class SimChunk { public bool Dirty = true; public int NextRow; public float X0; }

        static SimChunk[] NewChunks()
        {
            var chunks = new SimChunk[ChunkCount];
            for (int i = 0; i < ChunkCount; i++) chunks[i] = new SimChunk { X0 = i * ChunkMeters };
            return chunks;
        }

        /// <summary>Runs `frames` frames of the chunk-work loop. `wholeFieldRestart` and `blockOnHollowsDirty`
        /// reproduce the pre-fix rule (line 585's `foreach (var chunk in chunks) chunk.NextRow = 0;` and the
        /// `if (!hollowsDirty && !rescanned)` guard); false for both is the fixed rule. A crater lands on chunk 0
        /// every other frame (even frames); HeavyWork.TryClaim succeeds only on the following (odd) frame, so
        /// hollowsDirty spans exactly one frame each time, as the contended claim does in the real game.</summary>
        static (int rebuilds, int awayChunkDoneAtFrame) Run(int frames, bool wholeFieldRestart, bool blockOnHollowsDirty)
        {
            var chunks = NewChunks();
            bool hollowsDirty = false, pendingAny = false;
            float pendX0 = 0f, pendX1 = 0f;
            int cursor = 0, rebuilds = 0, awayDoneAt = -1;
            for (int frame = 0; frame < frames; frame++)
            {
                if (frame % 2 == 0)
                {
                    chunks[0].Dirty = true; chunks[0].NextRow = 0;
                    float reach = GreyboxTerrainView.HollowReachMeters;
                    pendX0 = chunks[0].X0 - reach; pendX1 = chunks[0].X0 + ChunkMeters + reach;
                    pendingAny = true;
                    hollowsDirty = true;
                }
                bool rescanned = false;
                if (hollowsDirty && frame % 2 == 1)   // the claim succeeds the frame after the crater lands
                {
                    hollowsDirty = false;
                    if (wholeFieldRestart)
                    {
                        foreach (var c in chunks) c.NextRow = 0;
                    }
                    else if (pendingAny)
                    {
                        foreach (var c in chunks)
                            if (GreyboxTerrainView.Overlaps(c.X0, 0f, c.X0 + ChunkMeters, 1f, pendX0, 0f, pendX1, 1f)) c.NextRow = 0;
                    }
                    pendingAny = false;
                    rescanned = true;
                }
                bool blocked = rescanned || (blockOnHollowsDirty && hollowsDirty);
                if (!blocked)
                {
                    int budget = RowsPerFrame;
                    for (int n = 0; n < ChunkCount && budget > 0; n++)
                    {
                        int i = (cursor + n) % ChunkCount;
                        var c = chunks[i];
                        if (!c.Dirty) continue;
                        bool inPending = !blockOnHollowsDirty && pendingAny &&
                            GreyboxTerrainView.Overlaps(c.X0, 0f, c.X0 + ChunkMeters, 1f, pendX0, 0f, pendX1, 1f);
                        if (GreyboxTerrainView.ChunkWaits(hollowsDirty, inPending)) continue;
                        cursor = i;
                        while (c.NextRow < RowsPerChunk && budget > 0) { c.NextRow++; budget--; }
                        if (c.NextRow < RowsPerChunk) break;
                        c.Dirty = false; c.NextRow = 0; rebuilds++;
                        if (i == AwayChunk && awayDoneAt < 0) awayDoneAt = frame;
                        cursor = (i + 1) % ChunkCount;
                    }
                }
            }
            return (rebuilds, awayDoneAt);
        }

        [Test]
        public void FixedRule_AwayChunkFinishesWithinBudget_AndChunksKeepRebuilding()
        {
            var (rebuilds, awayDoneAt) = Run(120, wholeFieldRestart: false, blockOnHollowsDirty: false);
            Assert.IsTrue(awayDoneAt >= 0 && awayDoneAt < 60, $"a chunk away from the shelling should finish well within 60 frames (was {awayDoneAt})");
            Assert.Greater(rebuilds, 0, "chunks away from the barrage should keep finishing");
        }

        [Test]
        public void OldRule_AwayChunkNeverFinishes()
        {
            // the bug, kept as the oracle: the whole-field restart undoes a chunk's rows before it can finish, and
            // the whole-field block on hollowsDirty stops it from working even on the frames in between.
            var (_, awayDoneAt) = Run(120, wholeFieldRestart: true, blockOnHollowsDirty: true);
            Assert.AreEqual(-1, awayDoneAt, "under the old rule a chunk far from the shelling is never finished");
        }
    }
}
