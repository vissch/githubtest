// Phase: review 2 (2026-10-14, code review unit 21, [TS5]) — the paths AllocProbe never watched.
// [TS5] P2 gap (report line 552): "the AllocProbe instrument is sound but nothing here calls DebrisRenderer.Throw/
// Burst, a TankRenderer frame, a BattlefieldProps recomposition or PropWear: X4, K4, B4, G10 cannot go red." Each
// test here opens the path the finding names with the AllocProbe instrument, so a future regression on any of them
// goes red here even where the finding's own fix (X4, K4, B4, G10) is out of this unit's scope.
using System.Collections;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Perf;
using TW.Sim;
using TW.Presentation.Terrain;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public sealed class ShowAllocationTests
    {
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        /// <summary>[TS5] Burst's own path (it is Throw, N times): the DebrisRenderer rig DebrisTests.cs uses for Throw,
        /// probed round Burst instead.</summary>
        [Test]
        public void Burst_DoesNotAllocate()
        {
            var go = new GameObject("DebrisRenderer (Burst probe)");
            Unity.Collections.NativeArray<DebrisRenderer.Record> records = default;
            try
            {
                var dr = go.AddComponent<DebrisRenderer>();
                var t = typeof(DebrisRenderer);
                t.GetField("groundFn", Any).SetValue(dr, (System.Func<float, float, float>)((x, z) => 0f));
                records = new Unity.Collections.NativeArray<DebrisRenderer.Record>(8, Unity.Collections.Allocator.Persistent);
                t.GetField("records", Any).SetValue(dr, records);
                var poolType = t.GetNestedType("Pool", BindingFlags.NonPublic);
                var pool = System.Activator.CreateInstance(poolType, nonPublic: true);
                poolType.GetField("Start").SetValue(pool, 0);
                poolType.GetField("Capacity").SetValue(pool, 8);
                var pools = (System.Array)t.GetField("pools", Any).GetValue(dr);
                pools.SetValue(pool, (int)DebrisRenderer.Piece.Clod);
                t.GetField("<Ready>k__BackingField", Any).SetValue(dr, true);

                Assume.That(AllocProbe.Available, "no profiler Recorder in the editor: the probe cannot run");
                double perCall = AllocProbe.PerCall(() => dr.Burst(DebrisRenderer.Piece.Clod, Vector3.zero, 8, 6f, 1f, Color.white), 2, 20);
                Assert.That(perCall, Is.EqualTo(0), "[TS5] Burst allocated: it is Throw N times, and Throw alone is clean");
            }
            finally
            {
                if (records.IsCreated) records.Dispose();
                Object.DestroyImmediate(go);
            }
        }

        /// <summary>[TS5] A burning hull's per-frame socket work (TankRenderer.Burning, K4's path): once warm, naming the
        /// two fire sockets and finding them on the model costs nothing per frame. The flames list is cleared inside the
        /// measured block so its own growth, already settled by the warm-up calls, is never what is counted.</summary>
        [Test]
        public void ABurningHullsFrameWorkDoesNotAllocate()
        {
            Assume.That(AllocProbe.Available, "no profiler Recorder in the editor: the probe cannot run");
            var go = new GameObject("TankRenderer (burning probe)");
            try
            {
                var r = go.AddComponent<TankRenderer>();
                var type = r.GetType();
                var viewType = type.GetNestedType("View", BindingFlags.NonPublic);
                Assert.NotNull(viewType, "TankRenderer.View");
                var model = TankModel.Load("Maw", VehicleArchetype.Maw, "Hull", 1.7f);
                Assert.NotNull(model, "the Maw hull did not load from Resources/Vehicles/Maw");
                int parts = model.Lods[0].Parts.Count;

                object v = System.Activator.CreateInstance(viewType, true);
                viewType.GetField("Model", Any).SetValue(v, model);
                var world = new Matrix4x4[parts];
                for (int i = 0; i < parts; i++) world[i] = Matrix4x4.identity;
                viewType.GetField("World", Any).SetValue(v, world);
                viewType.GetField("Off", Any).SetValue(v, new bool[parts]);

                var burning = type.GetMethod("Burning", Any);
                Assert.NotNull(burning, "TankRenderer.Burning");
                var flamesField = type.GetField("flames", Any);
                Assert.NotNull(flamesField, "TankRenderer.flames");
                var flames = (IList)flamesField.GetValue(r);

                // built once: MethodInfo.Invoke boxes every element of the args array it is given, so a fresh
                // `new object[] { ... }` every call would count reflection's own cost, not Burning's
                var args = new object[] { v, (object)100f, (object)0.8f, (object)1f };
                void Step()
                {
                    flames.Clear();
                    burning.Invoke(r, args);
                }

                double perCall = AllocProbe.PerCall(Step, 4, 20);
                Assert.That(perCall, Is.EqualTo(0), "[TS5] a burning hull's per-frame socket work allocated (K4: a built name every frame)");
            }
            finally
            {
                Object.DestroyImmediate(go);
            }
        }

        /// <summary>[TS5] PropWear's sweep (PropWear.SweepCell, G10's path): the one expensive call in the file is paid
        /// once per cell and then reused for as long as the fire lasts (PropWear.cs:195-196's own contract), so the
        /// second sweep of the same cell must cost nothing and a recomposition must make it pay again.</summary>
        [Test]
        public void PropWearsSweepIsPaidOnceAndReusedUntilARecomposition()
        {
            Assume.That(AllocProbe.Available, "no profiler Recorder in the editor: the probe cannot run");
            var kit = new BattlefieldKit();
            var go = new GameObject("PropWear sweep probe");
            try
            {
                var props = go.AddComponent<BattlefieldProps>();
                props.AttachKitForTests(kit);
                var module = kit.TrenchWalls[0];
                Assert.NotNull(module, "the premise: TrenchWalls[0] is a module");
                props.AddInstance(module, Matrix4x4.TRS(new Vector3(2f, 0f, 2f), Quaternion.identity, Vector3.one));

                var destruction = go.AddComponent<PropDestruction>();
                destruction.AttachForTests(props);
                var dType = typeof(PropDestruction);
                dType.GetField("cellsX", Any).SetValue(destruction, 1);
                dType.GetField("cellsZ", Any).SetValue(destruction, 1);

                var sweepCell = dType.GetMethod("SweepCell", Any);
                Assert.NotNull(sweepCell, "PropDestruction.SweepCell");
                // built once: MethodInfo.Invoke boxes every element of the args array it is given, so a fresh
                // `new object[] { 0 }` every call would count reflection's own cost, not SweepCell's
                var args = new object[] { (object)0 };

                int first = AllocProbe.Count(() => sweepCell.Invoke(destruction, args));
                Assert.Greater(first, 0, "[TS5] the first sweep of a cell must build its list of what stands there");

                int second = AllocProbe.Count(() => sweepCell.Invoke(destruction, args));
                Assert.AreEqual(0, second, "[TS5] a cell swept again while the fire still holds it must cost nothing (PropWear.cs:195-196)");

                props.RecomposeNowForTests();
                destruction.StepForTests(Time.time);

                int third = AllocProbe.Count(() => sweepCell.Invoke(destruction, args));
                Assert.Greater(third, 0, "[TS5] a recomposition renumbers every page and slot, so the sweep must be paid again");
            }
            finally
            {
                Object.DestroyImmediate(go);
                kit.Dispose();
            }
        }
    }
}
