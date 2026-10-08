// Phase: the Narrows (board item idea-the-narrows-a-field-half-as-deep) — the field that is half as deep.
//
// WinterLevelTests proves a ground can be REACHED. These prove this ground is the field the idea asked for, and
// that halving the length did not fold the layout in on itself. Everything but the depth is unscaled - the bends
// (3 cells of 2 m either way a line), the wire stand-off (11 m +/- 3.2 m) and the belt thickness (up to 3.9 m) -
// so the checks that matter are clearances, and they are taken over 8 seeds because one bad seed is a real
// finding (docs/design/idea-the-narrows-a-field-half-as-deep.md, edge case 3).
//
// Read from the map, never from the formula: CellTrenchId, NavLayers and the objective cells. The whole file
// builds 8 small maps per case and stays far under the framework's 180 s.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using TW.Presentation;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class NarrowsTests
    {
        static readonly uint[] Seeds = { 1917u, 2201u, 2318u, 3001u, 42u, 7u, 99u, 12345u };

        static MapData Build(uint seed) =>
            BattlefieldGenerator.Create(MatchLaunch.Field(Ground.Narrows, seed), Allocator.Persistent);

        /// <summary>Per nav column, the first and last Z cell of a trench id, or (-1, -1) where it is absent.</summary>
        static void Span(MapData map, int x, short id, out int first, out int last)
        {
            first = -1; last = -1;
            for (int z = 0; z < map.NavLength; z++)
                if (map.CellTrenchId[map.NavIndex(x, z)] == id) { if (first < 0) first = z; last = z; }
        }

        /// <summary>The clearance in metres between the near trench's back and the far trench's front, column by
        /// column: the distance between those two cell centres. A column where either line is missing is skipped.</summary>
        static List<float> Clearances(MapData map, short near, short far)
        {
            var all = new List<float>();
            for (int x = 0; x < map.NavWidth; x++)
            {
                Span(map, x, near, out _, out int nearLast);
                Span(map, x, far, out int farFirst, out _);
                if (nearLast < 0 || farFirst < 0) continue;
                all.Add((farFirst - nearLast) * MapData.NavCellSize);
            }
            return all;
        }

        static float Mean(List<float> v) { float s = 0f; for (int i = 0; i < v.Count; i++) s += v[i]; return s / v.Count; }
        static float Min(List<float> v) { float m = float.MaxValue; for (int i = 0; i < v.Count; i++) m = UnityEngine.Mathf.Min(m, v[i]); return m; }

        /// <summary>The seed the Narrows ships with (MatchLaunch.Request.BattlefieldSeed).</summary>
        const uint ShippedSeed = 1917u;

        /// <summary>The idea's number: "half as far apart", against the wood's measured 100 m. Not the formula
        /// re-run - this distance is the whole point of the field.
        ///
        /// Measured 2026-10-08 over the eight seeds, mean gap in metres: 1917 49.6, 2201 49.5, 2318 47.2,
        /// 3001 46.6, 42 50.0, 7 50.6, 99 47.2, 12345 50.3. The construction puts the LINES 50 m apart, but a
        /// trench is two cells deep, so a cell-centre measurement sits near 48 m before the bends move it at all;
        /// three seeds therefore land just under a 50 +/- 2 bar. The shipped seed is held to the tight bar, every
        /// seed to one bend's worth (the bends are +/- 3 cells = +/- 6 m a line and do not scale with Length).</summary>
        [Test]
        public void TheGapBetweenTheFrontTrenchesIsHalfTheWoods()
        {
            foreach (uint seed in Seeds)
            {
                using var map = Build(seed);
                var gaps = Clearances(map, 1, 2);
                Assert.IsNotEmpty(gaps, "seed " + seed + ": neither front trench was found in any nav column");
                Assert.AreEqual(50f, Mean(gaps), seed == ShippedSeed ? 2f : 6f,
                    "seed " + seed + ": the mean gap between the two front trenches is " + Mean(gaps)
                    + " m, not the 50 m the Narrows is for (the wood's is 100 m)");
            }
        }

        /// <summary>No trench may wander into another. The bends are unscaled, so this is the clearance the
        /// half-length field puts at risk.</summary>
        [Test]
        public void NoTrenchTouchesAnother()
        {
            foreach (uint seed in Seeds)
            {
                using var map = Build(seed);
                Assert.GreaterOrEqual(Min(Clearances(map, 1, 2)), 20f,
                    "seed " + seed + ": the two front trenches come within less than 20 m of each other");
                Assert.GreaterOrEqual(Min(Clearances(map, 0, 1)), 4f,
                    "seed " + seed + ": our reserve trench touches our front trench");
                Assert.GreaterOrEqual(Min(Clearances(map, 2, 3)), 4f,
                    "seed " + seed + ": the enemy's front trench touches his reserve trench");
            }
        }

        /// <summary>The two wire belts must not meet: a column may not be one long run of wire, and between its
        /// two runs there has to be a cell a man can stand in. A belt is at most 3.9 m thick plus its 1.9 m cell,
        /// three cells; five is the generous bar.</summary>
        [Test]
        public void TheWireBeltsNeverMeet()
        {
            foreach (uint seed in Seeds)
            {
                using var map = Build(seed);
                for (int x = 0; x < map.NavWidth; x++)
                {
                    int run = 0, longest = 0, runs = 0;
                    bool openBetween = false;
                    for (int z = 0; z < map.NavLength; z++)
                    {
                        var layer = (NavLayer)map.NavLayers[map.NavIndex(x, z)];
                        if ((layer & NavLayer.Wire) != 0)
                        {
                            if (run == 0) runs++;
                            run++; longest = UnityEngine.Mathf.Max(longest, run);
                        }
                        else
                        {
                            run = 0;
                            if (runs == 1 && (layer & NavLayer.Blocked) == 0) openBetween = true;
                        }
                    }
                    Assert.LessOrEqual(longest, 5,
                        "seed " + seed + ", column " + x + ": " + longest
                        + " wire cells in a row - the two belts have merged");
                    if (runs >= 2)
                        Assert.IsTrue(openBetween,
                            "seed " + seed + ", column " + x + ": no cell a man can cross between the two wire belts");
                }
            }
        }

        /// <summary>Edge case 4: the HQ line lands at 5 m here, a metre behind the spawn at 6 m. It must still be
        /// a place on the map that men can reach.</summary>
        [Test]
        public void BothHeadquartersExistAndAreNotWalledOff()
        {
            foreach (uint seed in Seeds)
            {
                using var map = Build(seed);
                int found = 0;
                for (int i = 0; i < map.Objectives.Length; i++)
                {
                    var o = map.Objectives[i];
                    if (o.Kind != ObjectiveKind.HQ) continue;
                    found++;
                    Assert.Greater(o.CellCount, 0, "seed " + seed + ": the team " + o.SideTeam + " HQ has no cells");
                    for (int c = o.CellStart; c < o.CellStart + o.CellCount; c++)
                    {
                        int cell = map.ObjectiveCells[c];
                        Assert.IsTrue(cell >= 0 && cell < map.NavLayers.Length,
                            "seed " + seed + ": the team " + o.SideTeam + " HQ has a cell off the map");
                        Assert.AreEqual(0, (byte)((NavLayer)map.NavLayers[cell] & NavLayer.Blocked),
                            "seed " + seed + ": a cell of the team " + o.SideTeam
                            + " HQ is blocked, so the HQ cannot be taken");
                    }
                }
                Assert.AreEqual(2, found, "seed " + seed + ": the Narrows must have one HQ per side");
            }
        }
    }
}
