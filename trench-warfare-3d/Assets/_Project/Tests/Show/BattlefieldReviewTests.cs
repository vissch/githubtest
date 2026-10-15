// Phase: B2 (presentation only; review calls B5, B7, B8)
// Review call B7: every (module, page) was given a 1,023-matrix array (65 KB) from its first instance, so a field of
// sparse pages cost tens of MB of mostly empty arrays. A page starts at 16 slots and doubles, capped at 1023.
using System;
using System.Collections.Generic;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
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
    }
}
