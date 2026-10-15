// Phase: review 3 (2026-10-15, K11) — the renderer's "a new match" branch. It dropped the rockets, the views and the
// ribbons and left everything else of the last match in hand (wrecks, shards, flames, riders, pips, the batches' queued
// instances). The rig is the renderer itself: every per-match collection filled by reflection, ForgetMatch invoked, and
// then every collection the renderer holds walked and asserted empty bar a named allow-list of pools and caches.
using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class TankMatchResetTests
    {
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        /// <summary>Not of a match: the ribbon pool, the loaded models and their seat, lamp and furnace tables, the instanced batches
        /// (their arrays and property blocks are kept; only the queued count is reset, asserted below) and the cut
        /// carcasses, whose meshes the renderer owns and frees in OnDestroy.</summary>
        static readonly HashSet<string> Kept = new HashSet<string>
        {
            "ribbonPool", "models", "seatsOf", "lampsOf", "furnaceOf", "batches", "carcasses",
        };

        static IEnumerable<FieldInfo> Collections(Type t)
        {
            foreach (var f in t.GetFields(Any))
                if (!f.FieldType.IsArray && typeof(ICollection).IsAssignableFrom(f.FieldType))
                    yield return f;
        }

        /// <summary>One element of a collection: a struct by value, a class by its private constructor (a Rider must be
        /// real - UnboardAll hands every seated man back), and null when it has none.</summary>
        static object Element(Type t)
        {
            if (t.IsValueType) return Activator.CreateInstance(t);
            if (t.IsArray) return Array.CreateInstance(t.GetElementType(), 0);
            try { return Activator.CreateInstance(t, true); } catch { return null; }
        }

        static void Fill(object collection)
        {
            var t = collection.GetType();
            if (!t.IsGenericType) return;
            var args = t.GetGenericArguments();
            if (args.Length == 1) t.GetMethod("Add", new[] { args[0] }).Invoke(collection, new[] { Element(args[0]) });
            else if (args.Length == 2) t.GetMethod("Add", args).Invoke(collection, new[] { Element(args[0]), Element(args[1]) });
        }

        /// <summary>[K11] every collection the renderer fills for a match is empty again after ForgetMatch, and the
        /// batches it keeps have nothing queued in them.</summary>
        [Test]
        public void ANewMatchLeavesNothingOfTheLastOneInHand()
        {
            var go = new GameObject("match reset rig");
            try
            {
                var r = go.AddComponent<TankRenderer>();
                var type = r.GetType();
                var forget = type.GetMethod("ForgetMatch", Any);
                Assert.NotNull(forget, "[K11] TankRenderer has no ForgetMatch: the new-match branch still clears only three collections");

                int filled = 0;
                foreach (var f in Collections(type))
                {
                    var c = f.GetValue(r);
                    if (c == null) continue;
                    Fill(c);
                    if (((ICollection)c).Count > 0) filled++;
                }
                Assert.Greater(filled, 20, "[K11] the rig filled too few of the renderer's collections to prove anything");

                forget.Invoke(r, null);

                var left = new List<string>();
                foreach (var f in Collections(type))
                {
                    var c = f.GetValue(r) as ICollection;
                    if (c == null || Kept.Contains(f.Name)) continue;
                    if (c.Count > 0) left.Add(f.Name);
                }
                Assert.AreEqual(string.Empty, string.Join(", ", left), "[K11] these still hold the last match");

                var batches = (IDictionary)type.GetField("batches", Any).GetValue(r);
                foreach (var b in batches.Values)
                {
                    if (b == null) continue;
                    int count = (int)b.GetType().GetField("Count", Any).GetValue(b);
                    Assert.AreEqual(0, count, "[K11] a kept batch still has the last match's instances queued");
                }
            }
            finally { UnityEngine.Object.DestroyImmediate(go); }
        }
    }
}
