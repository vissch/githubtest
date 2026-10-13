// Phase: B2 (presentation only; review call B4)
// Review call B4: every recomposition (every crater, prop change, wire breach) built a fresh string key per named
// prop and fresh scratch collections - hundreds of KB of garbage per shell in play. The keys are cached per (kind,
// rounded spot) and the composer's dart-throwing scratch lists are cleared fields now.
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
    public sealed class CompositionGarbageTests
    {
        BattlefieldKit kit;

        [OneTimeSetUp] public void BuildKit() => kit = new BattlefieldKit();
        [OneTimeTearDown] public void Tear() { kit?.Dispose(); kit = null; }

        /// <summary>[B4] A prop's key is built once and handed out again: the same string instance, same text.</summary>
        [Test]
        public void TheSameKeyIsHandedOutAgain()
        {
            var cache = new Dictionary<(string, int, int), string>();
            var at = new Vector3(1.234f, 0f, 5.678f);
            string first = PropLayout.GeneratedKey("wirePost", at, cache);
            Assert.AreEqual("wirePost@123,568", first, "[B4] the cached key must read the same as the uncached one");
            string again = PropLayout.GeneratedKey("wirePost", at, cache);
            Assert.IsTrue(ReferenceEquals(first, again), "[B4] the second call built a fresh string instead of handing out the kept key");
        }

        static MapData ShelledMap()
        {
            var map = new MapData(99, new float2(60f, 80f), Allocator.Temp);
            // tilted ground, as WireBeltTests does it, so nothing can be read off a flat zero
            for (int z = 0; z < map.Height.Length; z++)
            for (int x = 0; x < map.Height.Width; x++)
                map.Height.Cm[map.Height.Index(x, z)] = (short)(x * 4 + z * 3);
            map.RebuildCost();
            var rng = new System.Random(7);
            float Next() => (float)rng.NextDouble();
            for (int k = 0; k < 20; k++)
            {
                float r = 2.4f + Next() * 2.2f;
                new CraterStamp { Center = new float3(6f + Next() * 48f, 0f, 24f + Next() * 50f), Radius = r, Depth = r * .40f }.Apply(map);
            }
            return map;
        }

        /// <summary>[B4] The composer's dart-throwing scratch collections are kept and cleared, not built afresh for
        /// every recomposition: the same instances carry from one composition to the next.</summary>
        [Test]
        public void TheScratchCollectionsAreKeptFromOneCompositionToTheNext()
        {
            using var map = ShelledMap();
            var surface = new BattlefieldSurface(map);
            var composer = new BattlefieldComposer(kit, 1917);
            int count = 0;
            Action<BattlefieldKit.Module, Matrix4x4> sink = (m, t) => count++;   // stores nothing, allocates nothing

            composer.Build(map, surface, sink);
            Assert.Greater(count, 100, "[B4] the shelled map should compose a good many props");

            var names = new[] { "patches", "stands", "floors" };
            var first = new object[names.Length];
            for (int i = 0; i < names.Length; i++)
            {
                var field = typeof(BattlefieldComposer).GetField(names[i], BindingFlags.Instance | BindingFlags.NonPublic);
                Assert.IsNotNull(field, $"[B4] BattlefieldComposer.{names[i]} is not a kept field: a fresh collection per recomposition is garbage per shell");
                first[i] = field.GetValue(composer);
                Assert.IsNotNull(first[i], $"[B4] BattlefieldComposer.{names[i]} was never built");
            }

            composer.Build(map, surface, sink);
            for (int i = 0; i < names.Length; i++)
            {
                var field = typeof(BattlefieldComposer).GetField(names[i], BindingFlags.Instance | BindingFlags.NonPublic);
                var again = field.GetValue(composer);
                Assert.IsTrue(ReferenceEquals(first[i], again), $"[B4] recomposing built a fresh BattlefieldComposer.{names[i]}");
            }
        }
    }
}
