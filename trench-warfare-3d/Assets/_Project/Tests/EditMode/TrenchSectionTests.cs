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
            // what the lining throws with, jittered by Burst at its longest, is LiningLife: 12 s asked lay 15.6 s before
            Assert.LessOrEqual(DebrisMath.MaxLife(TrenchSectionRules.PieceLife), TrenchSectionRules.LiningLife + 1e-4f, "Burst's jitter does not stretch a lining piece past LiningLife");
            Assert.Greater(TrenchSectionRules.LiningLife, 5f, "but they do lie a while");
        }

        /// <summary>The wiring, not just the rules (critic r3 #8): one strike through the real PropDestruction on a fresh
        /// revetment leaves it Damaged with one broken twin drawn in its place; a recomposition keeps the twin (Replace);
        /// the next strike finishes the section, twin and all (Suppress keeps both out). Until 2026-09-27 the strike that
        /// drew the twin hit it again under the shared key and the section went straight to Gone: this test failed then.</summary>
        [Test]
        public void One_Strike_Leaves_A_Section_Damaged_And_The_Next_Finishes_It()
        {
            var go = new GameObject("lining test");
            try
            {
                var props = go.AddComponent<BattlefieldProps>();
                props.AttachKitForTests(kit);
                var destruction = go.AddComponent<PropDestruction>();
                destruction.AttachForTests(props);
                var wall = kit.TrenchWalls[0]; var twin = kit.TrenchWallsDamaged[0];
                var m = Matrix4x4.TRS(new Vector3(10f, 0f, 10f), Quaternion.identity, Vector3.one);
                props.AddInstance(wall, m);
                // harm 0.6 at 1 m from a 3 m reach, power 0.8: half the revetment's 1.2 hp, not heavy ordnance
                destruction.StrikeForTests(new Vector3(11f, 0f, 10f), 3f, 0.8f);
                Assert.AreEqual(1, destruction.SectionsDamaged, "the section is damaged, not gone");
                Assert.AreEqual(1, Drawn(props, twin), "one broken twin stands where the panel stood");
                Assert.AreEqual(0, Drawn(props, wall), "the whole panel is hidden");
                Assert.AreSame(twin, props.Replace(wall, m), "a recomposition draws the twin in the panel's place");
                Assert.IsFalse(props.Suppress(wall, m), "and does not drop it");
                destruction.StrikeForTests(new Vector3(11f, 0f, 10f), 3f, 0.8f);
                Assert.AreEqual(0, Drawn(props, twin), "the second strike brings the twin down");
                Assert.AreEqual(0, destruction.SectionsDamaged, "a gone section is not counted as damaged");
                Assert.IsTrue(props.Suppress(wall, m), "and the section stays out of every recomposition");
            }
            finally { Object.DestroyImmediate(go); }
        }

        static int Drawn(BattlefieldProps props, BattlefieldKit.Module module)
        {
            var found = new System.Collections.Generic.List<(int page, int slot, Matrix4x4 m)>();
            props.Within(module, new Vector2(10f, 10f), 5f, found);
            int shown = 0; foreach (var f in found) if (f.m.lossyScale.y > 1e-3f) shown++;
            return shown;
        }

        static void SameFootprint(BattlefieldKit.Module intact, BattlefieldKit.Module twin, string what, float zTolerance = 0.10f, float rolledOut = 0f)
        {
            Assert.IsNotNull(twin, what + " has no damaged twin");
            var a = intact.Mesh.bounds.size; var b = twin.Mesh.bounds.size;
            // the twin swaps in at the intact panel's matrix: it must not reach past the intact piece (a pop outward) and
            // must still span the whole 2 m section; an intact course may overhang its neighbour (the settled parapet's
            // top sack, 2.53 m) and its burst twin need not (first run under Tools/otr.py, 2026-09-27)
            Assert.LessOrEqual(b.x, a.x * 1.10f, what + ": the twin reaches no further along the edge than the whole one");
            Assert.GreaterOrEqual(b.x, 1.8f, what + ": the twin still spans the 2 m section");
            // the depth: the same, except that a burst parapet keeps the sack it rolls outward off the lip (rolledOut)
            Assert.GreaterOrEqual(b.z, a.z * (1f - zTolerance), what + ": and the same depth");
            Assert.LessOrEqual(b.z, a.z * (1f + zTolerance) + rolledOut, what + ": and no deeper than the whole one" + (rolledOut > 0f ? " and one rolled sack" : ""));
            Assert.LessOrEqual(b.y, a.y + 0.02f, what + ": the broken piece is no taller than the whole one");
            Assert.GreaterOrEqual(b.y, a.y * 0.4f, what + ": but something of it still stands");
        }

        [Test]
        public void Every_Lining_Panel_Has_A_Damaged_Twin_On_The_Same_Footprint()
        {
            for (int k = 0; k < 3; k++)
            {
                SameFootprint(kit.TrenchWalls[k], kit.TrenchWallsDamaged[k], "revetment " + k);
                SameFootprint(kit.TrenchBags[k], kit.TrenchBagsDamaged[k], "parapet " + k, 0.10f, 0.6f);   // 0.6: about one sack's length, turned 40 degrees
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

        [Test]
        public void The_Lining_Is_Sized_By_The_Composer_Not_The_Clamp()
        {
            // TrenchKit scales a panel per axis (x = the edge's length, y = the bay's height); one uniform clamp would
            // stretch a shallow bay's panel past its edge, so the lining rows report and never enforce
            foreach (var key in new[] { "kit/TrenchWalls", "kit/TrenchBags", "kit/TrenchFloors", "kit/TrenchWallsDamaged", "kit/TrenchBagsDamaged", "kit/TrenchFloorsDamaged" })
            {
                Assert.IsTrue(AssetScaleTable.TryGet(key, out var rule), key + " has a row");
                Assert.IsFalse(rule.Enforce, key + " is reported, not clamped");
            }
            kit.ResolveKeysAndRules();
            foreach (var h in new[] { 0.55f, 0.9f, 1.3f })
                for (int k = 0; k < 3; k++)
                {
                    var scale = new Vector3(1.0f, h, 1f);
                    foreach (var m in new[] { kit.TrenchWalls[k], kit.TrenchBags[k], kit.TrenchFloors[k] })
                        Assert.AreEqual(scale, AssetScaleTable.Clamp(m.Rule, m.Mesh.bounds.size, scale), kit.KeyOf[m] + " " + k + " at height " + h + " keeps the composer's scale");
                }
        }
    }
}
