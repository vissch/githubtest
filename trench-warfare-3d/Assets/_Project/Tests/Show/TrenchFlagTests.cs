// Phase: B5 (the owner's idea of 2026-10-08) — the trench flag's arithmetic, pinned: the pole stands on the
// parapet of a cell the trench actually owns, option A's sizes are the concept sheet's numbers, the two cloths are
// far enough apart to tell the sides even when the hue is gone, and a shelled pole goes standing -> stump -> away.
//
// The two luminances are NOT recomputed the way TrenchFlagRules computes them: they are the numbers critic-r2.md
// derived on the board from the hex codes, so a change in the code's own arithmetic cannot quietly agree with
// itself.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using TW.Presentation.Terrain;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public sealed class TrenchFlagTests
    {
        /// <summary>From critic-r2.md on the board: the relative luminance of #ff8a5c and #34639b, and how far apart
        /// they read.</summary>
        const float RedLuminance = 0.4021f, BlueLuminance = 0.1202f, Apart = 2.66f;

        /// <summary>A 40 x 40 m field with one trench: five cells in a row across the middle, facing +Z.</summary>
        static MapData OneTrench(Allocator allocator, float yaw)
        {
            var map = new MapData(7, new float2(40f, 40f), allocator);
            int z = 10, start = map.TrenchCells.Length;
            for (int x = 6; x < 11; x++)
            {
                int cell = map.NavIndex(x, z);
                map.TrenchCells.Add(cell);
                map.CellTrenchId[cell] = 0;
            }
            map.Trenches.Add(new TrenchDef
            {
                Id = 0, OwnerTeam = 0, Kind = 0,
                CellStart = start, CellCount = map.TrenchCells.Length - start,
                NextTrenchForTeam0 = -1, NextTrenchForTeam1 = -1,
                FacingYaw = yaw, WidthMeters = 2.4f,
            });
            return map;
        }

        [Test]
        public void Pole_Stands_On_The_Parapet_Of_A_Cell_The_Trench_Owns()
        {
            using var map = OneTrench(Allocator.Temp, 0f);   // facing +Z
            var trench = map.Trenches[0];
            var anchor = TrenchFlagRules.Anchor(trench, map);

            // the middle cell of the chain, not an end
            int middle = map.TrenchCells[trench.CellStart + trench.CellCount / 2];
            var mid = (Vector3)map.NavCellCenter(middle);
            Assert.AreEqual(0, map.CellTrenchId[middle], "the anchor is taken off a cell this trench owns");

            Vector3 flat = new Vector3(anchor.x - mid.x, 0f, anchor.z - mid.z);
            Assert.AreEqual(TrenchFlagRules.ParapetOffset, flat.magnitude, 1e-3f, "it stands ParapetOffset off the cell centre");
            Assert.AreEqual(TrenchFlagRules.ParapetOffset, anchor.z - mid.z, 1e-3f, "facing +Z: the offset is the parapet side, not the bay");
            Assert.AreEqual(mid.x, anchor.x, 1e-3f);
            Assert.AreEqual(map.Height.Sample(anchor.x, anchor.z), anchor.y, 1e-3f, "the butt sits on the ground");
        }

        [Test]
        public void The_Parapet_Side_Follows_The_Trench_Facing()
        {
            using var east = OneTrench(Allocator.Temp, Mathf.PI * 0.5f);   // facing +X
            var anchor = TrenchFlagRules.Anchor(east.Trenches[0], east);
            var mid = (Vector3)east.NavCellCenter(east.TrenchCells[east.Trenches[0].CellStart + east.Trenches[0].CellCount / 2]);
            Assert.AreEqual(TrenchFlagRules.ParapetOffset, anchor.x - mid.x, 1e-3f, "facing +X: the pole steps east");
            Assert.AreEqual(mid.z, anchor.z, 1e-3f);
        }

        [Test]
        public void Sizes_Are_The_Concept_Sheets_Option_A()
        {
            Assert.AreEqual(3.1f, TrenchFlagRules.PoleHeight, 1e-4f, "a stout pole, 3.1 m");
            Assert.AreEqual(3.3f, TrenchFlagRules.FlagWide, 1e-4f, "a wide stiff pennant, 3.3 m across");
            Assert.AreEqual(2.0f, TrenchFlagRules.FlagTall, 1e-4f, "and 2.0 m deep");
            Assert.AreEqual(1.0f, TrenchFlagRules.FrogHeight, 1e-4f, "one frog at one size, 1.0 m");
            Assert.Less(TrenchFlagRules.FlagTall, TrenchFlagRules.PoleHeight, "the flag has to fit on the pole");
            Assert.Less(TrenchFlagRules.StumpHeight, TrenchFlagRules.FlagTall, "a stump is plainly shorter than the flag it lost");
        }

        [Test]
        public void The_Two_Cloths_Are_Far_Enough_Apart_To_Tell_The_Sides()
        {
            Assert.AreEqual(RedLuminance, TrenchFlagRules.Luminance(TrenchFlagRules.ClothRed), 5e-4f, "red #ff8a5c");
            Assert.AreEqual(BlueLuminance, TrenchFlagRules.Luminance(TrenchFlagRules.ClothBlue), 5e-4f, "blue #34639b");
            float apart = TrenchFlagRules.Contrast(TrenchFlagRules.ClothRed, TrenchFlagRules.ClothBlue);
            Assert.AreEqual(Apart, apart, 0.02f, "the board's 2.66:1");
            Assert.GreaterOrEqual(apart, 2.5f, "under 2.5:1 the sides stop being told apart in greyscale");
            Assert.AreEqual(TrenchFlagRules.ClothBlue, TrenchFlagRules.Cloth(0), "team 0 is blue, as TankRenderer.TeamA is");
            Assert.AreEqual(TrenchFlagRules.ClothRed, TrenchFlagRules.Cloth(1), "team 1 is red");
            Assert.IsFalse(TrenchFlagRules.Flies(255), "a neutral trench flies nothing");
            Assert.IsTrue(TrenchFlagRules.Flies(0) && TrenchFlagRules.Flies(1));
        }

        [Test]
        public void Standing_Then_A_Stump_Then_Away()
        {
            float hp = TrenchFlagRules.MaxHp;
            Assert.AreEqual(FlagState.Intact, TrenchFlagRules.Apply(ref hp, 0.3f, false, FlagState.Intact), "a grenade leaves it standing");
            Assert.AreEqual(0.7f, hp, 1e-5f);
            Assert.AreEqual(FlagState.Snapped, TrenchFlagRules.Apply(ref hp, 0.8f, false, FlagState.Intact), "its strength gone: snapped off");
            Assert.AreEqual(0f, hp);
            Assert.AreEqual(FlagState.Gone, TrenchFlagRules.Apply(ref hp, 0.2f, false, FlagState.Snapped), "the next shell takes the stump");
            Assert.AreEqual(FlagState.Gone, TrenchFlagRules.Apply(ref hp, 0f, false, FlagState.Gone), "gone is gone");
        }

        [Test]
        public void Heavy_Ordnance_Takes_The_Pole_Outright()
        {
            float hp = TrenchFlagRules.MaxHp;
            Assert.AreEqual(FlagState.Gone, TrenchFlagRules.Apply(ref hp, 0.1f, true, FlagState.Intact), "a barrage shell on the parapet");
            Assert.AreEqual(0f, hp);
            Assert.IsTrue(TrenchSectionRules.IsHeavy(8f, 1.3f, 2f, 9.2f), "and heavy is the lining's own word for it");
        }

        [Test]
        public void The_New_Flag_Climbs_And_Eases_Into_The_Stop()
        {
            Assert.AreEqual(1.4f, TrenchFlagRules.RiseSeconds, 1e-4f);
            Assert.AreEqual(2.2f, TrenchFlagRules.FallSeconds, 1e-4f);
            Assert.AreEqual(0f, TrenchFlagRules.Rise(0f), 1e-5f, "it starts at the butt");
            Assert.AreEqual(1f, TrenchFlagRules.Rise(TrenchFlagRules.RiseSeconds), 1e-5f, "and is at the top on time");
            Assert.AreEqual(1f, TrenchFlagRules.Rise(99f), 1e-5f, "and stays there");
            float half = TrenchFlagRules.Rise(TrenchFlagRules.RiseSeconds * 0.5f);
            Assert.Greater(half, 0.6f, "ease-out: most of the haul is over in the first half");
            float a = TrenchFlagRules.Rise(0.2f), b = TrenchFlagRules.Rise(0.4f), c = TrenchFlagRules.Rise(0.6f);
            Assert.Greater(b - a, c - b, "and it slows as it nears the stop");
        }

        [Test]
        public void The_Old_Flag_Falls_Away_Tumbling()
        {
            Assert.AreEqual(1f, TrenchFlagRules.Fall(0f), 1e-5f, "cut loose at the top");
            Assert.AreEqual(0f, TrenchFlagRules.Fall(TrenchFlagRules.FallSeconds), 1e-5f, "on the parapet at the end");
            Assert.AreEqual(0f, TrenchFlagRules.Fall(99f), 1e-5f);
            float early = TrenchFlagRules.Fall(0.3f) - TrenchFlagRules.Fall(0.6f), late = TrenchFlagRules.Fall(1.6f) - TrenchFlagRules.Fall(1.9f);
            Assert.Less(early, late, "it gathers speed instead of sinking");
            Assert.AreEqual(0f, TrenchFlagRules.FallAngle(0f), 1e-5f);
            Assert.Greater(TrenchFlagRules.FallAngle(TrenchFlagRules.FallSeconds), Mathf.PI * 2f, "it turns over at least once on the way down");
            Assert.AreEqual(TrenchFlagRules.FallAngle(TrenchFlagRules.FallSeconds), TrenchFlagRules.FallAngle(99f), 1e-4f, "and stops turning when it lands");
        }
    }
}
