// Phase: B2 (presentation-only crater hollows; review call B1)
// Review call B1: a crater-rim fringe and a dud shell are hashed on the hollow's list index, but RefreshHollows
// refills that list in scan order after every crater, so one new hole early in the scan renumbered every hollow
// after it and the duds moved all over the map. They are hashed on the hollow's own cell now.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim.Terrain;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public sealed class CompositionStabilityTests
    {
        BattlefieldKit kit;

        [OneTimeSetUp] public void BuildKit() => kit = new BattlefieldKit();
        [OneTimeTearDown] public void Tear() { kit?.Dispose(); kit = null; }

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
            for (int k = 0; k < 24; k++)
            {
                float r = 2.4f + Next() * 2.2f;
                Stamp(map, 6f + Next() * 48f, 24f + Next() * 50f, r, r * .40f);
            }
            return map;
        }

        static void Stamp(MapData map, float x, float z, float radius, float depth)
            => new CraterStamp { Center = new float3(x, 0f, z), Radius = radius, Depth = depth }.Apply(map);

        static List<Vector3> Duds(BattlefieldKit kit, BattlefieldComposer composer, MapData map, BattlefieldSurface surface)
        {
            var duds = new List<Vector3>();
            composer.Build(map, surface, (m, t) => { if (m == kit.dudShell) duds.Add(t.GetColumn(3)); });
            return duds;
        }

        /// <summary>[B1] A dud shell belongs to its own hole: a shell landing somewhere else must not move it.</summary>
        [Test]
        public void ADudStaysInItsOwnHoleWhenAnotherShellLands()
        {
            using var map = ShelledMap();
            var surface = new BattlefieldSurface(map);
            var composer = new BattlefieldComposer(kit, 1917);

            var before = Duds(kit, composer, map, surface);
            Assert.Greater(before.Count, 3, "[B1] the shelled map should hold several duds to begin with");

            // a new crater low in the scan (so it renumbers the hollow list), well clear of every dud there is
            var shell = new Vector2(30f, 12f);
            foreach (var d in before)
                Assert.Greater(Vector2.Distance(new Vector2(d.x, d.z), shell), 15f, "[B1] the new shell must land clear of the duds");
            Stamp(map, shell.x, shell.y, 3.4f, 1.4f);
            surface.RefreshHollows();

            var after = Duds(kit, composer, map, surface);
            foreach (var d in before)
            {
                if (Vector2.Distance(new Vector2(d.x, d.z), shell) < 8f) continue;
                bool kept = false;
                foreach (var e in after) if (Mathf.Abs(e.x - d.x) < .01f && Mathf.Abs(e.z - d.z) < .01f) { kept = true; break; }
                Assert.IsTrue(kept, $"[B1] the dud at {d.x:0.00},{d.z:0.00} moved or vanished because a shell landed {Vector2.Distance(new Vector2(d.x, d.z), shell):0.0} m away");
            }
        }

        static List<Vector3> Landmarks(BattlefieldKit kit, BattlefieldComposer composer, MapData map, BattlefieldSurface surface)
        {
            var at = new List<Vector3>();
            composer.Build(map, surface, (m, tr) =>
            { if (m == kit.fieldGun || m == kit.armouredStand || m == kit.well) at.Add(tr.GetColumn(3)); });
            return at;
        }

        /// <summary>[B2] A landmark stands where it was first put: shelling one never gives it an intact twin elsewhere.</summary>
        [Test]
        public void ALandmarkNeverMovesWhenTheNextShellLands()
        {
            using var map = ShelledMap();
            var surface = new BattlefieldSurface(map);
            var composer = new BattlefieldComposer(kit, 1917);

            var before = Landmarks(kit, composer, map, surface);
            Assert.Greater(before.Count, 2, "[B2] the map should hold several landmarks to begin with");

            // shell the ground out from under one of them, the way a barrage does
            var hit = before[0];
            for (int k = 0; k < 4; k++) Stamp(map, hit.x + (k - 1.5f) * 1.2f, hit.z, 3.2f, 1.3f);
            surface.RefreshHollows();

            var after = Landmarks(kit, composer, map, surface);
            foreach (var a in after)
            {
                bool known = false;
                foreach (var b in before) if (Mathf.Abs(a.x - b.x) < .01f && Mathf.Abs(a.z - b.z) < .01f) { known = true; break; }
                Assert.IsTrue(known, $"[B2] a landmark appeared at {a.x:0.00},{a.z:0.00}, where none stood before the shelling");
            }
        }
    }
}
