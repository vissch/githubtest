// Phase: review 3 (2026-10-15) — K10's two cheap wins that can be read off the code: the walker gait's step order is
// sorted by a key measured once per leg, and an instanced batch starts small and grows, keeping what is queued in it.
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class TankBatchGrowthTests
    {
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        /// <summary>[K10] the step order falls by its key, and the keys travel with it: the worked example from the plan,
        /// keys {0.1, 0.9, 0.5} over order {0, 1, 2}, comes out {1, 2, 0}.</summary>
        [Test]
        public void TheStepOrderIsSortedByAKeyMeasuredOnce()
        {
            var order = new[] { 0, 1, 2 };
            var key = new[] { 0.1f, 0.9f, 0.5f };
            WalkerGait.SortDesc(order, 3, key);
            Assert.AreEqual(new[] { 1, 2, 0 }, order, "[K10] the legs must be ordered worst offender first");
            Assert.AreEqual(new[] { 0.9f, 0.5f, 0.1f }, key, "[K10] the keys must travel with the legs they belong to");
        }

        /// <summary>[K10] a batch starts at 64 instances, not 1023, and grows: every matrix queued past the first 64 is
        /// still there, in order, with the count to match.</summary>
        [Test]
        public void ABatchGrowsAndKeepsEveryQueuedInstance()
        {
            var go = new GameObject("batch rig");
            var mesh = new Mesh { name = "K10 batch mesh" };
            try
            {
                var r = go.AddComponent<TankRenderer>();
                var type = r.GetType();
                var batchType = type.GetNestedType("Batch", BindingFlags.NonPublic);
                Assert.NotNull(batchType, "[K10] TankRenderer.Batch");
                Assert.AreEqual(64, (int)batchType.GetField("Start", BindingFlags.Public | BindingFlags.Static).GetValue(null),
                    "[K10] a batch must start at 64 instances, not 1023");

                var queue = type.GetMethod("Queue", Any);
                Assert.NotNull(queue, "[K10] TankRenderer.Queue");
                const int n = 200;
                for (int i = 0; i < n; i++)
                    queue.Invoke(r, new object[] { mesh, null, Matrix4x4.Translate(new Vector3(i, 0f, 0f)), i * 0.5f,
                                                   Vector4.zero, Vector4.zero, Vector4.zero, 0u });

                var batches = (System.Collections.IDictionary)type.GetField("batches", Any).GetValue(r);
                object b = batches[mesh];
                Assert.NotNull(b, "[K10] the mesh must have a batch");
                int count = (int)b.GetType().GetField("Count", Any).GetValue(b);
                var m = (Matrix4x4[])b.GetType().GetField("M", Any).GetValue(b);
                var tread = (float[])b.GetType().GetField("Tread", Any).GetValue(b);
                Assert.AreEqual(n, count, "[K10] every queued instance must be kept");
                Assert.GreaterOrEqual(m.Length, n, "[K10] the batch must have grown to hold them");
                for (int i = 0; i < n; i++)
                {
                    Assert.AreEqual(i, m[i].GetColumn(3).x, 1e-4f, "[K10] instance " + i + " must keep its matrix across the grow");
                    Assert.AreEqual(i * 0.5f, tread[i], 1e-4f, "[K10] instance " + i + " must keep its tread across the grow");
                }
            }
            finally { Object.DestroyImmediate(mesh); Object.DestroyImmediate(go); }
        }
    }
}
