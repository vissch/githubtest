// Phase: B6 / docs/21 phase 6 (implemented) — the Home Front's stage masks on a hand-built house: stage 0 shows the
// ground floor only, the ground floor never hides, the whole building shows at the top, a higher stage never hides
// more, and the boot installs the view factories. No Resources, no scene, no GameObject.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Meta;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class HomeFrontDioramaTests
    {
        static HouseKit.Chunk Chunk(int index, Vector3 min, Vector3 max)
        {
            var b = new Bounds(); b.SetMinMax(min, max);
            return new HouseKit.Chunk { Index = index, Local = b };
        }

        /// <summary>A tower of three floors and a lean-to: A the ground floor, B the first, C the top; D a low shed.</summary>
        static HouseKit.House Tower()
        {
            var house = new HouseKit.House
            {
                Name = "Tower",
                Chunks = new[]
                {
                    Chunk(0, new Vector3(-2f, 0f, -2f), new Vector3(2f, 1.5f, 2f)),
                    Chunk(1, new Vector3(-2f, 1.5f, -2f), new Vector3(2f, 3f, 2f)),
                    Chunk(2, new Vector3(-1f, 3f, -1f), new Vector3(1f, 4.5f, 1f)),
                    Chunk(3, new Vector3(2f, 0.2f, -1f), new Vector3(3f, 0.9f, 1f)),
                },
            };
            HouseKit.Solve(house);
            return house;
        }

        [Test]
        public void Stage_Zero_Shows_The_Ground_Floor_Only()
        {
            var house = Tower();
            Assert.That(house.Bounds.min.y, Is.EqualTo(0f).Within(1e-4f));
            Assert.That(house.Bounds.max.y, Is.EqualTo(4.5f).Within(1e-4f));
            var hidden = HomeFrontStages.HiddenAt(house, 0.3f);
            Assert.That(hidden.Has(0), Is.False, "the ground floor shows");
            Assert.That(hidden.Has(3), Is.False, "the shed on the ground shows");
            Assert.That(hidden.Has(1), Is.True, "the first floor is masked off");
            Assert.That(hidden.Has(2), Is.True, "the top is masked off");
            Assert.That(HomeFrontStages.ShownCount(house, 0.3f), Is.EqualTo(2));
        }

        [Test]
        public void The_Ground_Floor_Never_Hides()
        {
            var house = Tower();
            var hidden = HomeFrontStages.HiddenAt(house, 0f);
            Assert.That(hidden.Has(0), Is.False);
            Assert.That(hidden.Has(3), Is.False);
            Assert.That(HomeFrontStages.ShownCount(house, 0f), Is.EqualTo(2));
            Assert.That(HomeFrontStages.Limit(house, 0f), Is.EqualTo(HouseKit.GroundedBelow).Within(1e-4f));
        }

        [Test]
        public void The_Whole_Building_Shows_At_The_Top()
        {
            var house = Tower();
            Assert.That(HomeFrontStages.HiddenAt(house, 1f).Any, Is.False);
            Assert.That(HomeFrontStages.ShownCount(house, 1f), Is.EqualTo(4));
            Assert.That(HomeFrontStages.ShownCount(house, 1.7f), Is.EqualTo(4), "past the top clamps");
        }

        [Test]
        public void A_Higher_Stage_Never_Hides_More()
        {
            var house = Tower();
            float[] stages = { 0f, 0.3f, 0.55f, 0.8f, 1f };
            int last = -1;
            foreach (var s in stages)
            {
                int shown = HomeFrontStages.ShownCount(house, s);
                Assert.That(shown, Is.GreaterThanOrEqualTo(last), "stage " + s);
                last = shown;
            }
            // two levels (1.5 m, 3 m) for three paid stages: stage I shows the first floor, the last the whole tower, and the
            // repeat falls on the middle stage (HomeFrontStages.Reveal)
            Assert.That(HomeFrontStages.ShownCount(house, 0.3f), Is.EqualTo(2), "stage 0: the ground floor and the lean-to");
            Assert.That(HomeFrontStages.ShownCount(house, 0.55f), Is.EqualTo(3), "stage I shows the first floor");
            Assert.That(HomeFrontStages.HiddenAt(house, 0.55f).Has(2), Is.True, "the top is still off");
            Assert.That(HomeFrontStages.ShownCount(house, 0.8f), Is.EqualTo(3), "stage II repeats (two levels, three stages)");
            Assert.That(HomeFrontStages.ShownCount(house, 1f), Is.EqualTo(4), "the last stage is the whole tower");
        }

        [Test]
        public void An_Empty_House_Hides_Nothing()
        {
            Assert.That(HomeFrontStages.HiddenAt(null, 0.5f).Any, Is.False);
            Assert.That(HomeFrontStages.ShownCount(null, 0.5f), Is.Zero);
        }

        [Test]
        public void The_Boot_Installs_The_View_Factories()
        {
            var home = MetaServices.MakeHomeFront; var map = MetaServices.MakeMap;
            try
            {
                MetaServices.MakeHomeFront = null; MetaServices.MakeMap = null;
                MetaBoot.Install();
                Assert.That(MetaServices.MakeHomeFront, Is.Not.Null);
                Assert.That(MetaServices.MakeMap, Is.Not.Null);
            }
            finally { MetaServices.MakeHomeFront = home; MetaServices.MakeMap = map; }
        }
        /// <summary>Every real building the Home Front draws grows at every stage it is sold, as far as the model has levels
        /// to show (critic r5: on the real models stage III changed nothing on 8 of 10, the Blockhouse's stage I nothing).</summary>
        [Test]
        public void Every_Real_Building_Grows_At_Each_Stage_Its_Model_Can_Show()
        {
            CollectionAssert.AreEqual(TW.UI.FactionBuildings.StageHeights, HomeFrontStages.StageFractions, "the stage fractions agree on both sides");
            var sets = new System.Collections.Generic.Dictionary<string, HouseKit.House[]>();
            int checkedModels = 0;
            foreach (byte faction in new byte[] { 0, 1 })
                foreach (var b in TW.UI.FactionBuildings.Of(faction))
                {
                    if (!sets.TryGetValue(b.Set, out var houses)) sets[b.Set] = houses = HouseKit.Load(b.Set, chunk => new BattlefieldKit.Module(), 0);
                    var house = System.Array.Find(houses, h => h.Name == b.Model);
                    Assert.IsNotNull(house, b.Id + ": model " + b.Model + " in " + b.Set);
                    int levels = HomeFrontStages.Levels(house).Count, previous = -1, distinct = 0, first = -1, beforeLast = -1;
                    for (int s = 0; s < b.Stages.Length; s++)
                    {
                        int shown = HomeFrontStages.ShownCount(house, b.Stages[s].ShownHeight);
                        Assert.GreaterOrEqual(shown, previous, b.Id + " never loses chunks going up a stage");
                        if (shown > previous) distinct++;
                        if (s == 0) first = shown;
                        if (s == 1 && levels > 0) Assert.Greater(shown, first, b.Id + ": stage I shows something new");
                        if (s == b.Stages.Length - 2) beforeLast = shown;
                        previous = shown;
                    }
                    Assert.AreEqual(house.Chunks.Length, previous, b.Id + ": the top stage is the whole building");
                    if (levels > 1) Assert.Greater(previous, beforeLast, b.Id + ": the dearest stage shows something new");
                    Assert.AreEqual(Mathf.Min(b.Stages.Length, levels + 1), distinct, b.Id + " (" + b.Model + ", " + levels + " levels): every stage the model can show looks different");
                    checkedModels++;
                }
            Assert.GreaterOrEqual(checkedModels, 15, "every building of both factions");
        }
    }
}