// Phase: B5 (docs/21 phase 3) — the trench lining's two-step break: damaged at half, gone at nothing; heavy ordnance
// shatters a section outright; a section's pieces are gone in fifteen seconds; every lining panel has a damaged twin
// on the same footprint, lower than the intact piece; the lining is hit per panel, not per sack. The state machine is
// pure; the twins are measured on a kit built once for the fixture.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public sealed class TrenchSectionTests
    {
        BattlefieldKit kit;

        [OneTimeSetUp] public void Build() => kit = new BattlefieldKit();
        [OneTimeTearDown] public void Tear() { kit?.Dispose(); kit = null; }

        [Test]
        public void Damaged_At_Half_Gone_At_Nothing()
        {
            float hp = 1.2f;
            Assert.AreEqual(SectionState.Intact, TrenchSectionRules.Apply(ref hp, 1.2f, 0.5f, false, SectionState.Intact), "more than half left: still standing");
            Assert.AreEqual(0.7f, hp, 1e-5f);
            Assert.AreEqual(SectionState.Damaged, TrenchSectionRules.Apply(ref hp, 1.2f, 0.2f, false, SectionState.Intact), "half left: damaged");
            Assert.AreEqual(0.5f, hp, 1e-5f);
            Assert.AreEqual(SectionState.Damaged, TrenchSectionRules.Apply(ref hp, 1.2f, 0.1f, false, SectionState.Damaged), "and it stays damaged while anything is left");
            Assert.AreEqual(SectionState.Gone, TrenchSectionRules.Apply(ref hp, 1.2f, 0.6f, false, SectionState.Damaged), "nothing left: gone");
            Assert.AreEqual(0f, hp);
            hp = 0.3f;
            Assert.AreEqual(SectionState.Gone, TrenchSectionRules.Apply(ref hp, 1.2f, 0f, false, SectionState.Gone), "gone is gone");
            Assert.AreEqual(0f, hp);
        }

        [Test]
        public void Heavy_Ordnance_Shatters_A_Section_Outright()
        {
            Assert.IsTrue(TrenchSectionRules.IsHeavy(8f, 1.3f, 2f, 9.2f), "a barrage shell two metres off");
            Assert.IsTrue(TrenchSectionRules.IsHeavy(1.13f, 1.1f, 0.5f, 1.3f), "an AP round on the panel");
            Assert.IsFalse(TrenchSectionRules.IsHeavy(2.5f, 0.4f, 1f, 2.9f), "a grenade is not heavy");
            Assert.IsFalse(TrenchSectionRules.IsHeavy(8f, 1.3f, 6f, 9.2f), "nor is a barrage shell past the inner half of its reach");
            float hp = 1.2f;
            Assert.AreEqual(SectionState.Gone, TrenchSectionRules.Apply(ref hp, 1.2f, 0.1f, true, SectionState.Intact), "heavy: from intact to gone at once");
            hp = 1.2f;
            Assert.AreEqual(SectionState.Damaged, TrenchSectionRules.Apply(ref hp, 1.2f, 0.7f, false, SectionState.Intact), "light: only as far as the harm takes it");
        }

        [Test]
        public void A_Sections_Pieces_Are_Gone_In_Fifteen_Seconds()
        {
            Assert.LessOrEqual(TrenchSectionRules.LiningLife + DebrisMath.SinkSeconds, 15f, "a bay is clear of its fragments fifteen seconds after the shell");
            Assert.Greater(TrenchSectionRules.LiningLife, 5f, "but they do lie a while");
        }

        static void SameFootprint(BattlefieldKit.Module intact, BattlefieldKit.Module twin, string what, float zTolerance = 0.10f)
        {
            Assert.IsNotNull(twin, what + " has no damaged twin");
            var a = intact.Mesh.bounds.size; var b = twin.Mesh.bounds.size;
            Assert.AreEqual(a.x, b.x, a.x * 0.10f, what + ": the twin spans the same length");
            Assert.AreEqual(a.z, b.z, a.z * zTolerance, what + ": and the same depth");
            Assert.LessOrEqual(b.y, a.y + 0.02f, what + ": the broken piece is no taller than the whole one");
            Assert.GreaterOrEqual(b.y, a.y * 0.4f, what + ": but something of it still stands");
        }

        [Test]
        public void Every_Lining_Panel_Has_A_Damaged_Twin_On_The_Same_Footprint()
        {
            for (int k = 0; k < 3; k++)
            {
                SameFootprint(kit.TrenchWalls[k], kit.TrenchWallsDamaged[k], "revetment " + k);
                SameFootprint(kit.TrenchBags[k], kit.TrenchBagsDamaged[k], "parapet " + k);
                SameFootprint(kit.TrenchFloors[k], kit.TrenchFloorsDamaged[k], "duckboards " + k, 0.20f);
            }
        }

        [Test]
        public void The_Lining_Is_Hit_Per_Panel_Not_Per_Sack()
        {
            var all = new[] { kit.TrenchWalls, kit.TrenchBags, kit.TrenchFloors, kit.TrenchWallsDamaged, kit.TrenchBagsDamaged, kit.TrenchFloorsDamaged };
            int n = 0;
            foreach (var set in all)
                foreach (var m in set)
                {
                    n++;
                    Assert.GreaterOrEqual(m.Mesh.bounds.size.x, 1.8f, m.Mesh.name + " is a whole 2 m section, one key, one hp");
                }
            Assert.AreEqual(18, n, "three variants of three kinds, intact and broken");
        }
    }
}
