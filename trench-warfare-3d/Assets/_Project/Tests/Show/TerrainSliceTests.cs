// Phase: review 3 fix [Z004.2] — the ground repaint and the chunk re-read share one slice.
// Each amortised job had a wall-clock budget of its own (PaintBudgetMs 2.0, ChunkBudgetMs 2.0) and nothing added them
// up against the frame's 3 ms: one shell queues tiles and dirties chunks at once, so under a barrage both loops found
// work in the same frame and the frame paid both caps (TW.Terrain.Update p95 4.12-4.15 ms, runs/9/e0p). These tests
// hold one Update to one shared slice, and keep the stand-down in a frame another heavy job claimed.
using System.Diagnostics;
using System.Reflection;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Terrain;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class TerrainSliceTests
    {
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        static object Call(GreyboxTerrainView v, string name, params object[] args)
        {
            var m = typeof(GreyboxTerrainView).GetMethod(name, Any);
            Assert.IsNotNull(m, "GreyboxTerrainView." + name + " (renamed? this test calls it by name)");
            return m.Invoke(v, args);
        }

        static int DirtyChunks(GreyboxTerrainView v)
        {
            var f = typeof(GreyboxTerrainView).GetField("chunks", Any);
            Assert.IsNotNull(f, "GreyboxTerrainView.chunks (renamed? this test reads it by name)");
            int dirty = 0;
            foreach (var c in (System.Collections.IEnumerable)f.GetValue(v))
            {
                var d = c.GetType().GetField("Dirty", Any);
                if ((bool)d.GetValue(c)) dirty++;
            }
            return dirty;
        }

        /// <summary>A host whose match is nothing but this map, as RealLampSetTests.WalkTheView builds it.</summary>
        static SimHost HostOn(MapData map, GameObject hostGo)
        {
            var host = hostGo.AddComponent<SimHost>();
            var local = typeof(SimHost).GetProperty("Local");
            Assert.IsNotNull(local, "SimHost.Local (renamed? this test sets it by name)");
            var match = (TW.Sim.Match.MatchSim)System.Runtime.Serialization.FormatterServices.GetUninitializedObject(typeof(TW.Sim.Match.MatchSim));
            match.Map = map;
            local.GetSetMethod(true).Invoke(host, new object[] { match });
            return host;
        }

        [Test, Category("Long")]
        public void OneUpdate_PaysOneSharedSlice_NotBothBudgets()   // [Z004.2]
        {
            Knobs.Set("terrain.applyIntervalMs", "600000");   // the mip rebuild is 2.4-2.9 ms and is neither slice's
            // the editor's frame count need not move between two EditMode tests: this frame must be unclaimed
            typeof(HeavyWork).GetField("claimedFrame", BindingFlags.Static | BindingFlags.NonPublic).SetValue(null, -1);
            var hostGo = new GameObject("host under test") { hideFlags = HideFlags.DontSave }; hostGo.SetActive(false);
            var go = new GameObject("terrain under test") { hideFlags = HideFlags.DontSave }; go.SetActive(false);
            using var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(1917u), Allocator.Persistent);
            try
            {
                var view = go.AddComponent<GreyboxTerrainView>();
                view.Host = HostOn(map, hostGo);
                Call(view, "Start");
                Call(view, "Update");   // it subscribes on its first Update
                // The colour texture has never been uploaded, so its first upload happens whatever the interval knob
                // says (2.4-2.9 ms of mip rebuild, neither slice's work). Spend it here, queueing nothing, so the
                // measured frame below is the two amortised jobs and nothing else.
                typeof(GreyboxTerrainView).GetField("colorDirty", Any).SetValue(view, true);
                Call(view, "ApplyColor");
                // WireBreached queues tiles AND dirties chunks, and does not set hollowsDirty: no claim is in play,
                // so this is exactly the frame the finding is about, with work waiting for both jobs.
                Call(view, "OnSimEvent", new TW.Sim.SimEvent
                {
                    Type = TW.Sim.SimEventType.WireBreached,
                    Pos = new float3(45f, 0f, 120f),
                    Scalar = 30f,
                });
                Assert.Greater(view.PendingPaintTiles, 0, "[Z004.2] the shell queued no tiles: this test proves nothing");
                Assert.Greater(DirtyChunks(view), 1, "[Z004.2] the shell dirtied no chunks: this test proves nothing");
                // Warm up: the first row of each job pays for the JIT of GroundColor, VisualHeight and FillRow, which
                // is worth milliseconds in the editor and belongs to neither budget. Each of these frames is itself
                // capped, so the queue is still deep when the measured frame starts.
                for (int k = 0; k < 6; k++) Call(view, "Update");
                int queued = view.PendingPaintTiles;

                var watch = Stopwatch.StartNew();
                Call(view, "Update");
                watch.Stop();
                double ms = watch.Elapsed.TotalMilliseconds;
                // a part-painted tile stays counted as pending, so the clock is what says the slice did work
                Assert.Greater(view.LastPaintMilliseconds, 0f, "[Z004.2] the shared slice painted nothing at all");
                Assert.LessOrEqual(view.PendingPaintTiles, queued, "[Z004.2] the frame queued more than it painted");
                Assert.Less(ms, 3.0, $"[Z004.2] one frame paid both amortised terrain jobs: {ms:0.00} ms, against a " +
                    "shared slice of 2.0 ms (PaintBudgetMs) and the two budgets summing to 4.0 ms");
            }
            finally
            {
                Knobs.Clear();
                Object.DestroyImmediate(go); Object.DestroyImmediate(hostGo);
            }
        }
    }

    /// <summary>Claims the editor's own frame, so it may not share a class with the timing test above.</summary>
    public class TerrainSliceStandDownTests
    {
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        static object Call(GreyboxTerrainView v, string name, params object[] args)
        {
            var m = typeof(GreyboxTerrainView).GetMethod(name, Any);
            Assert.IsNotNull(m, "GreyboxTerrainView." + name + " (renamed? this test calls it by name)");
            return m.Invoke(v, args);
        }

        /// <summary>Whose turn it is to go first, read by name so this file still compiles against the old code.</summary>
        static object TurnOf(GreyboxTerrainView v)
        {
            var p = typeof(GreyboxTerrainView).GetProperty("ChunksFirstNextFrame", Any);
            Assert.IsNotNull(p, "GreyboxTerrainView.ChunksFirstNextFrame (renamed? this test reads it by name)");
            return p.GetValue(v);
        }

        static SimHost HostOn(MapData map, GameObject hostGo)
        {
            var host = hostGo.AddComponent<SimHost>();
            var match = (TW.Sim.Match.MatchSim)System.Runtime.Serialization.FormatterServices.GetUninitializedObject(typeof(TW.Sim.Match.MatchSim));
            match.Map = map;
            typeof(SimHost).GetProperty("Local").GetSetMethod(true).Invoke(host, new object[] { match });
            return host;
        }

        [Test, Category("Long")]
        public void AClaimedFrame_StandsBothSlicesDown_AndTheTurnAlternates()
        {
            var hostGo = new GameObject("host under test") { hideFlags = HideFlags.DontSave }; hostGo.SetActive(false);
            var go = new GameObject("terrain under test") { hideFlags = HideFlags.DontSave }; go.SetActive(false);
            using var map = BattlefieldGenerator.Create(BattlefieldParams.ShelledForest(1917u), Allocator.Persistent);
            try
            {
                var view = go.AddComponent<GreyboxTerrainView>();
                view.Host = HostOn(map, hostGo);
                Call(view, "Start");
                Call(view, "Update");
                Call(view, "OnSimEvent", new TW.Sim.SimEvent
                {
                    Type = TW.Sim.SimEventType.WireBreached,
                    Pos = new float3(45f, 0f, 120f),
                    Scalar = 30f,
                });
                int queued = view.PendingPaintTiles;
                object turn = TurnOf(view);
                Assert.IsTrue(HeavyWork.TryClaim(), "a heavy job claims this editor frame");
                Call(view, "Update");
                Assert.AreEqual(queued, view.PendingPaintTiles, "a frame a heavy job claimed paints no tile");
                Assert.AreEqual(turn, TurnOf(view), "a frame it stood down in does not use up its turn");
                Assert.AreEqual(0f, view.LastPaintMilliseconds, "a frame it stood down in reports no paint time");
                // and with the claim handed back, the two jobs take turns going first
                typeof(HeavyWork).GetField("claimedFrame", BindingFlags.Static | BindingFlags.NonPublic).SetValue(null, -1);
                Call(view, "Update");
                Assert.AreNotEqual(turn, TurnOf(view), "one unclaimed frame passes the turn to the other job");
                typeof(HeavyWork).GetField("claimedFrame", BindingFlags.Static | BindingFlags.NonPublic).SetValue(null, -1);
                Call(view, "Update");
                Assert.AreEqual(turn, TurnOf(view), "two unclaimed frames bring the turn back round");
            }
            finally
            {
                // the editor's frame count need not move between two EditMode tests, so hand the claim back
                typeof(HeavyWork).GetField("claimedFrame", BindingFlags.Static | BindingFlags.NonPublic).SetValue(null, -1);
                Object.DestroyImmediate(go); Object.DestroyImmediate(hostGo);
            }
        }
    }
}
