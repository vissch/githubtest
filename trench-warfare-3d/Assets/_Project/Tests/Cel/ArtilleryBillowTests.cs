// Phase: CelFx artillery - connected smoke surface invariants, recipe isolation and actual pool activity.
using System;
using System.Collections.Generic;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Tests
{
    public sealed class ArtilleryBillowTests
    {
        CelFxRenderer renderer;
        GameObject holder;
        FxTier previousQuality;

        [SetUp]
        public void SetUp()
        {
            previousQuality = FxQuality.Tier;
            FxQuality.Set(FxTier.High);
            SceneStatics.ResetSession();
            holder = new GameObject("ArtilleryBillowTests") { hideFlags = HideFlags.HideAndDontSave };
            renderer = holder.AddComponent<CelFxRenderer>();
            renderer.Init();
            CelFx.Adopt(renderer);
            CelFx.ZoomOverride = 10f;
            CelFx.Clock = 100f;
            SceneHooks.Flash = (at, colour, power, reach, life) => { };
        }

        [TearDown]
        public void TearDown()
        {
            try { renderer.Shutdown(); UnityEngine.Object.DestroyImmediate(holder); }
            finally { SceneStatics.ResetSession(); FxQuality.Set(previousQuality); }
        }

        static CelFxRecipe Recipe(string name, bool close)
        {
            return (CelFxRecipe)typeof(ArtilleryRecipe).GetField(name + (close ? "Close" : ""),
                BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic).GetValue(null);
        }

        [Test]
        public void Mesh_Is_Deterministic_Closed_Outward_And_Bounded_With_Real_Concavity()
        {
            var a = new ArtilleryBillowKind().BuildMesh();
            var b = new ArtilleryBillowKind().BuildMesh();
            try
            {
                var points = a.vertices;
                var normals = a.normals;
                var uv = a.uv;
                var triangles = a.triangles;
                Assert.That(points.Length, Is.EqualTo(642));
                Assert.That(triangles.Length, Is.EqualTo(3840));
                Assert.That(b.vertices, Is.EqualTo(points));
                Assert.That(b.normals, Is.EqualTo(normals));
                Assert.That(b.uv, Is.EqualTo(uv));
                Assert.That(b.triangles, Is.EqualTo(triangles));
                float minCavity = 1f, maxCavity = 0f;
                var edges = new Dictionary<long, int>();
                var neighbors = new List<int>[points.Length];
                for (int i = 0; i < points.Length; i++)
                {
                    neighbors[i] = new List<int>();
                    Assert.That(points[i].magnitude, Is.InRange(0.35f, 1.00001f), "finite bounded radius " + i);
                    Assert.That(normals[i].magnitude, Is.InRange(0.999f, 1.001f), "finite unit normal " + i);
                    Assert.That(Vector3.Dot(normals[i], points[i]), Is.GreaterThan(0f), "outward normal " + i);
                    Assert.That(uv[i].x, Is.InRange(0f, 1f));
                    Assert.That(uv[i].y, Is.InRange(0f, 1f));
                    minCavity = Mathf.Min(minCavity, uv[i].x); maxCavity = Mathf.Max(maxCavity, uv[i].x);
                }
                for (int i = 0; i < triangles.Length; i += 3)
                {
                    int x = triangles[i], y = triangles[i + 1], z = triangles[i + 2];
                    Vector3 face = Vector3.Cross(points[y] - points[x], points[z] - points[x]);
                    Assert.That(face.magnitude, Is.GreaterThan(1e-6f), "nondegenerate face " + i / 3);
                    Assert.That(Vector3.Dot(face, normals[x] + normals[y] + normals[z]), Is.GreaterThan(0f));
                    Edge(x, y); Edge(y, z); Edge(z, x);
                }
                foreach (int use in edges.Values) Assert.That(use, Is.EqualTo(2), "closed manifold edge");
                Assert.That(points.Length - edges.Count + triangles.Length / 3, Is.EqualTo(2), "sphere topology");
                var visited = new HashSet<int>();
                var queue = new Queue<int>(); queue.Enqueue(0);
                while (queue.Count > 0)
                {
                    int next = queue.Dequeue();
                    if (!visited.Add(next)) continue;
                    foreach (int neighbor in neighbors[next]) queue.Enqueue(neighbor);
                }
                Assert.That(visited.Count, Is.EqualTo(points.Length), "one connected skin");
                Assert.That(minCavity, Is.LessThan(0.01f));
                Assert.That(maxCavity, Is.GreaterThan(0.3f), "joined valleys must not regress into a sphere");
                TestContext.WriteLine("Compound mesh: 642 vertices/1280 faces; cavity range " + minCavity + ".." + maxCavity);

                void Edge(int x, int y)
                {
                    long key = ((long)Math.Min(x, y) << 32) | (uint)Math.Max(x, y);
                    edges.TryGetValue(key, out int count); edges[key] = count + 1;
                    neighbors[x].Add(y); neighbors[y].Add(x);
                }
            }
            finally { UnityEngine.Object.DestroyImmediate(a); UnityEngine.Object.DestroyImmediate(b); }
        }

        List<CelRecord> Records(int[] starts)
        {
            const BindingFlags flags = BindingFlags.Instance | BindingFlags.NonPublic;
            var pools = (Array)typeof(CelFxRenderer).GetField("pools", flags).GetValue(renderer);
            object records = typeof(CelFxRenderer).GetField("records", flags).GetValue(renderer);
            var item = records.GetType().GetProperty("Item");
            var result = new List<CelRecord>();
            for (int k = 0; k < renderer.KindCount; k++)
            {
                object pool = pools.GetValue(k);
                int start = (int)pool.GetType().GetField("Start").GetValue(pool);
                for (int i = starts[k]; i < renderer.SpawnedOf(k); i++)
                    result.Add((CelRecord)item.GetValue(records, new object[] { start + i }));
            }
            result.Sort((a, b) => a.Seed != b.Seed ? a.Seed.CompareTo(b.Seed) : a.Born.CompareTo(b.Born));
            return result;
        }

        [TestCase("Background")]
        [TestCase("Foreground")]
        [TestCase("Target")]
        public void Close_Changes_Only_The_Pool_And_Preserves_All_GPU_Records(string name)
        {
            var candidate = Recipe(name, true);
            var original = JsonUtility.FromJson<CelFxRecipe>(JsonUtility.ToJson(candidate));
            int changed = 0;
            foreach (var layer in original.Layers)
            {
                if (layer.Kind == ArtilleryBillowKind.KindName) { layer.Kind = CelKinds.LobeName; changed++; }
                typeof(CelLayer).GetField("kindId", BindingFlags.Instance | BindingFlags.NonPublic).SetValue(layer, int.MinValue);
                typeof(CelLayer).GetField("paletteId", BindingFlags.Instance | BindingFlags.NonPublic).SetValue(layer, -1);
                typeof(CelLayer).GetField("coolId", BindingFlags.Instance | BindingFlags.NonPublic).SetValue(layer, -1);
            }
            Assert.That(changed, Is.EqualTo(3));
            var start = new int[renderer.KindCount];
            CelFx.Play(original, Vector3.up, Vector3.forward, Vector3.up, 1917u, 1.25f);
            var expected = Records(start);
            Assert.That(expected.Count, Is.EqualTo(64), "actual High/Close GPU activity, excluding the queued light");
            for (int k = 0; k < start.Length; k++) start[k] = renderer.SpawnedOf(k);
            ArtilleryRecipe.PlayAt(Recipe(name, false), Vector3.up, Vector3.forward, 1917u, 1.25f);
            var actual = Records(start);
            Assert.That(actual.Count, Is.EqualTo(expected.Count));
            Assert.That(actual, Is.EqualTo(expected), "all 128-byte record fields, including births, motion, cooling and erosion");
            Assert.That(renderer.SpawnedOf(renderer.KindId(ArtilleryBillowKind.KindName)), Is.EqualTo(15));
        }

        [TestCase(30f)]
        [TestCase(120f)]
        public void Outer_Tiers_Never_Spawn_The_Close_Only_Kind(float zoom)
        {
            CelFx.ZoomOverride = zoom;
            foreach (string name in new[] { "Background", "Foreground", "Target" })
            {
                foreach (var layer in Recipe(name, false).Layers)
                    Assert.That(layer.Kind, Is.Not.EqualTo(ArtilleryBillowKind.KindName));
                ArtilleryRecipe.PlayAt(Recipe(name, false), Vector3.zero, Vector3.forward, 1917u, 1f);
            }
            Assert.That(renderer.SpawnedOf(renderer.KindId(ArtilleryBillowKind.KindName)), Is.EqualTo(0));
            Assert.That(renderer.SpawnedOf(renderer.KindId(ArtilleryFxKind.KindName)), Is.GreaterThan(0), "outer-tier control activity");
        }

        [Test]
        public void Warmed_Compound_Smoke_Play_And_Submit_Have_No_GCAlloc_Samples()
        {
            Assert.That(TW.Perf.AllocProbe.Available, Is.True);
            byte[] keep = null;
            Assert.That(TW.Perf.AllocProbe.PerCall(() => keep = new byte[4096], 4, 20), Is.EqualTo(1));
            GC.KeepAlive(keep);
            Assert.That(TW.Perf.AllocProbe.PerCall(() => { }, 4, 20), Is.EqualTo(0));
            var recipe = Recipe("Foreground", true);
            Action draw = () => { CelFx.Play(recipe, Vector3.zero, Vector3.forward, Vector3.zero, 1917u); CelFx.Pump(); renderer.Submit(100.2f); };
            Assert.That(TW.Perf.AllocProbe.PerCall(draw, 8, 30), Is.EqualTo(0));
            Assert.That(renderer.SpawnedOf(renderer.KindId(ArtilleryBillowKind.KindName)), Is.GreaterThan(0));
            Assert.That(renderer.DrawCalls, Is.GreaterThan(0));
            TestContext.WriteLine("GC.Alloc samples only; no whole-frame or GPU budget claim.");
        }
    }
}
