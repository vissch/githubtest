// Phase: B2 (presentation only; review calls B5, B7, B8)
// Review call B7: every (module, page) was given a 1,023-matrix array (65 KB) from its first instance, so a field of
// sparse pages cost tens of MB of mostly empty arrays. A page starts at 16 slots and doubles, capped at 1023.
using System;
using System.Collections.Generic;
using System.Reflection;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim.Terrain;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public sealed class BattlefieldReviewTests
    {
        BattlefieldKit kit;

        [OneTimeSetUp] public void BuildKit() => kit = new BattlefieldKit();
        [OneTimeTearDown] public void Tear() { kit?.Dispose(); kit = null; }

        /// <summary>The private nested BattlefieldProps.Batch, with a module set, and its Add bound.</summary>
        static object NewBatch(BattlefieldKit.Module module, out MethodInfo add, out IList<Matrix4x4[]> pages, out IList<int> counts)
        {
            var type = typeof(BattlefieldProps).GetNestedType("Batch", BindingFlags.NonPublic);
            Assert.IsNotNull(type, "[B7] BattlefieldProps.Batch is gone: the page growth this test guards moved");
            var batch = Activator.CreateInstance(type, true);
            type.GetField("Module").SetValue(batch, module);
            add = type.GetMethod("Add");
            pages = (IList<Matrix4x4[]>)type.GetField("Pages").GetValue(batch);
            counts = (IList<int>)type.GetField("Counts").GetValue(batch);
            return batch;
        }

        static int AddAt(MethodInfo add, object batch, Vector3 at)
        {
            var args = new object[] { Matrix4x4.TRS(at, Quaternion.identity, Vector3.one), 0 };
            int page = (int)add.Invoke(batch, args);
            return page;
        }

        /// <summary>[B7] One instance must not cost a whole 1,023-matrix page, and a page that does fill up grows to
        /// the instancing limit and keeps every matrix it held.</summary>
        [Test]
        public void APageStartsSmallAndGrowsWithoutLosingItsMatrices()
        {
            var batch = NewBatch(kit.sandbag, out var add, out var pages, out var counts);
            // one page: every position inside the same PageSize square (sandbag's is 64 m)
            float step = .5f;
            var first = new Vector3(.5f, 0f, .5f);
            AddAt(add, batch, first);
            Assert.AreEqual(1, counts[0], "[B7] the first instance did not land in page 0");
            Assert.Less(pages[0].Length, 1023, "[B7] one instance must not cost a 1,023-matrix page");
            for (int i = 1; i < 1023; i++) AddAt(add, batch, new Vector3(.5f + i % 32 * step, 0f, .5f + i / 32 * step));
            Assert.AreEqual(1, counts.Count, "[B7] the 1,023 instances spilled out of one page, so this says nothing about growth");
            Assert.AreEqual(1023, counts[0], "[B7] the page did not take all 1,023 instances");
            Assert.AreEqual(1023, pages[0].Length, "[B7] a full page must hold the instancing limit of 1,023 matrices");
            Assert.AreEqual(first, pages[0][0].GetPosition(), "[B7] growing a page dropped the matrices it held");
        }

        // ---- the composer's ground steps: the house's frame (B5) and the keep-out (B8) ----

        static readonly BindingFlags Hidden = BindingFlags.Instance | BindingFlags.NonPublic;

        /// <summary>A tilted map with no river, so PlaceHamlets places nothing and the test owns the hamlet list.</summary>
        static MapData TiltedMap()
        {
            var map = new MapData(99, new float2(60f, 80f), Allocator.Temp);
            for (int z = 0; z < map.Height.Length; z++)
            for (int x = 0; x < map.Height.Width; x++)
                map.Height.Cm[map.Height.Index(x, z)] = (short)(x * 4 + z * 3);
            map.RebuildCost();
            return map;
        }

        HouseKit.House AHouse()
        {
            var houses = Array.FindAll(kit.Houses ?? new HouseKit.House[0], h => h.Set == "Houses");
            Assert.Greater(houses.Length, 0, "[B5] the kit has no village houses, so this test says nothing");
            return houses[0];
        }

        /// <summary>The hamlets the composer holds, replaced by the test's own.</summary>
        static void PutHamlets(BattlefieldComposer composer, IEnumerable<BattlefieldComposer.Hamlet> list)
        {
            var field = (List<BattlefieldComposer.Hamlet>)typeof(BattlefieldComposer).GetField("hamlets", Hidden).GetValue(composer);
            field.Clear(); field.AddRange(list);
        }

        static List<Matrix4x4> RunStep(BattlefieldComposer composer, string step, MapData map, BattlefieldSurface surface, BattlefieldKit.Module only = null)
        {
            var got = new List<Matrix4x4>();
            Action<BattlefieldKit.Module, Matrix4x4> collect = (m, t) => { if (only == null || m == only) got.Add(t); };
            typeof(BattlefieldComposer).GetField("emit", Hidden).SetValue(composer, collect);
            var method = typeof(BattlefieldComposer).GetMethod(step, Hidden);
            Assert.IsNotNull(method, "[B8] BattlefieldComposer." + step + " is gone");
            method.Invoke(composer, method.GetParameters().Length == 2 ? new object[] { map, surface } : new object[] { map, surface, 0f, 0f });
            return got;
        }

        /// <summary>[B5] Winter ice belongs on the house's own eaves: on a wall of its real, rotated footprint and at
        /// its own roof line, not on a world-axis square the size of the footprint's diagonal.</summary>
        [Test]
        public void AnIcicleHangsOnTheHousesOwnRotatedEaves()
        {
            using var map = TiltedMap();
            var surface = new BattlefieldSurface(map);
            var composer = new BattlefieldComposer(kit, 1917);
            var house = AHouse();
            var ext = house.Bounds.extents;

            var turn = Quaternion.Euler(0f, 60f, 0f);   // the houses stand at 90 +- 30 degrees, never on the world axes
            var at = new Vector3(30f, 0f, 40f);
            var centre = at + turn * new Vector3(house.Bounds.center.x, 0f, house.Bounds.center.z);
            var hamlet = new BattlefieldComposer.Hamlet(house.Index, Matrix4x4.TRS(at, turn, Vector3.one), centre, new Vector2(ext.x, ext.z).magnitude);
            PutHamlets(composer, new[] { hamlet });

            var got = new List<Matrix4x4>();
            Action<BattlefieldKit.Module, Matrix4x4> collect = (m, t) => { if (m == kit.icicles) got.Add(t); };
            typeof(BattlefieldComposer).GetField("emit", Hidden).SetValue(composer, collect);
            var icicles = typeof(BattlefieldComposer).GetMethod("Icicles", Hidden);
            Assert.IsNotNull(icicles, "[B5] BattlefieldComposer.Icicles is gone");
            icicles.Invoke(composer, new object[] { hamlet });

            Assert.Greater(got.Count, 0, "[B5] a frozen house hung no ice at all");
            var inverse = Matrix4x4.TRS(at, turn, Vector3.one).inverse;
            foreach (var m in got)
            {
                var raw = inverse.MultiplyPoint3x4(m.GetPosition());   // the house's own frame: x and z off the footprint's middle, y off its base
                var local = raw - house.Bounds.center;
                Assert.LessOrEqual(Mathf.Abs(local.x), ext.x + .35f, "[B5] an icicle hangs off the house's footprint");
                Assert.LessOrEqual(Mathf.Abs(local.z), ext.z + .35f, "[B5] an icicle hangs off the house's footprint");
                bool onAWall = Mathf.Abs(Mathf.Abs(local.x) - ext.x) < .4f || Mathf.Abs(Mathf.Abs(local.z) - ext.z) < .4f;
                Assert.IsTrue(onAWall, "[B5] an icicle hangs off the house's footprint");
                Assert.Greater(raw.y, house.Bounds.max.y - .5f, "[B5] an icicle hangs off the house's footprint");
            }
        }

        /// <summary>[B8] Debris, litter and the clumps keep out of the buildings: they run after the layout and are
        /// checked against every house's footprint.</summary>
        [Test]
        public void TheLooseGroundKeepsOutOfTheBuildings()
        {
            using var map = TiltedMap();
            var surface = new BattlefieldSurface(map);
            var composer = new BattlefieldComposer(kit, 1917);
            var house = AHouse();
            var ext = house.Bounds.extents;

            var village = new List<BattlefieldComposer.Hamlet>();
            for (int i = 0; i < 6; i++)
            {
                float x = 12f + i % 3 * 18f, z = 22f + i / 3 * 30f;
                var turn = Quaternion.Euler(0f, 60f + i * 17f, 0f);
                var at = new Vector3(x, surface.VisualHeight(x, z), z);
                var centre = at + turn * new Vector3(house.Bounds.center.x, 0f, house.Bounds.center.z);
                village.Add(new BattlefieldComposer.Hamlet(house.Index, Matrix4x4.TRS(at, turn, Vector3.one), centre, new Vector2(ext.x, ext.z).magnitude));
            }
            PutHamlets(composer, village);

            var got = new List<Matrix4x4>();
            got.AddRange(RunStep(composer, "Debris", map, surface));
            got.AddRange(RunStep(composer, "Litter", map, surface));
            got.AddRange(RunStep(composer, "Clumps", map, surface));
            Assert.Greater(got.Count, 20, "[B8] the ground steps placed almost nothing, so this says nothing about buildings");

            int near = 0;
            foreach (var m in got)
            {
                var p = m.GetPosition();
                foreach (var h in village)
                {
                    var local = Quaternion.Inverse(h.Matrix.rotation) * new Vector3(p.x - h.Centre.x, 0f, p.z - h.Centre.z);
                    bool inside = Mathf.Abs(local.x) < ext.x && Mathf.Abs(local.z) < ext.z;
                    Assert.IsFalse(inside, "[B8] scrub inside a building");
                    if (new Vector2(p.x - h.Centre.x, p.z - h.Centre.z).magnitude < 8f) near++;
                }
            }
            Assert.Greater(near, 0, "[B8] nothing was placed anywhere near a house, so the keep-out was never tested");
        }
    }
}
