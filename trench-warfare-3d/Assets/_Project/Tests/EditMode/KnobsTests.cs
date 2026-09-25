// Phase: tooling (AOSA loop, 2026-09-25) - the knob registry parses, falls back, logs what was read, and with nothing
// set every knob-backed value is the constant it replaced (the old numbers are written out here on purpose).
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Presentation;
using TW.Presentation.Units;
using TW.Presentation.Tactical;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public class KnobsTests
    {
        [SetUp] public void SetUp() => Knobs.Clear();
        [TearDown] public void TearDown() => Knobs.Clear();

        [Test]
        public void NothingSet_GetReturnsTheFallback_AndReadRecordsIt()
        {
            Assert.AreEqual(170f, Knobs.Get("t.float", 170f));
            Assert.AreEqual(42, Knobs.Get("t.int", 42));
            Assert.AreEqual(true, Knobs.Get("t.bool", true));
            Assert.AreEqual(0, Knobs.Overrides.Count);
            Assert.AreEqual("170", Knobs.Read["t.float"]);
            Assert.AreEqual("42", Knobs.Read["t.int"]);
            Assert.AreEqual("1", Knobs.Read["t.bool"]);
        }

        [Test]
        public void Parse_CommaSeparated_FloatAndInt()
        {
            Knobs.Parse("a=1.5,b=2");
            Assert.AreEqual(1.5f, Knobs.Get("a", 0f));
            Assert.AreEqual(2, Knobs.Get("b", 0));
            Assert.AreEqual(2f, Knobs.Get("b", 0f), "an integer reads as a float too");
            Assert.AreEqual("1.5", Knobs.Read["a"]);
            Assert.AreEqual("2", Knobs.Read["b"]);
        }

        [Test]
        public void Parse_PipeAndSemicolonSeparated()
        {
            Knobs.Parse("a=3|b=4; c = 5 ");
            Assert.AreEqual(3, Knobs.Get("a", 0));
            Assert.AreEqual(4, Knobs.Get("b", 0));
            Assert.AreEqual(5, Knobs.Get("c", 0));
            Assert.AreEqual(3, Knobs.Overrides.Count);
        }

        [Test]
        public void Bools()
        {
            Knobs.Parse("a=1,b=0,c=true,d=FALSE");
            Assert.IsTrue(Knobs.Get("a", false));
            Assert.IsFalse(Knobs.Get("b", true));
            Assert.IsTrue(Knobs.Get("c", false));
            Assert.IsFalse(Knobs.Get("d", true));
            Assert.AreEqual("0", Knobs.Read["d"]);
        }

        [Test]
        public void Unparseable_FallsBack_WithOneWarningPerName()
        {
            Knobs.Parse("f=abc,i=2.5,b=maybe,j=3.0");
            LogAssert.Expect(LogType.Warning, new Regex("f=abc"));
            Assert.AreEqual(7f, Knobs.Get("f", 7f));
            Assert.AreEqual(7f, Knobs.Get("f", 7f), "asked again: the same fallback, and no second warning");
            LogAssert.Expect(LogType.Warning, new Regex("i=2.5"));
            Assert.AreEqual(9, Knobs.Get("i", 9));
            LogAssert.Expect(LogType.Warning, new Regex("b=maybe"));
            Assert.IsTrue(Knobs.Get("b", true));
            Assert.AreEqual(3, Knobs.Get("j", 0), "a whole number written as a float is still an integer");
            Assert.AreEqual("7", Knobs.Read["f"]);
        }

        [Test]
        public void LaterParse_OverridesEarlier_AndSetOverridesBoth()
        {
            Knobs.Parse("a=1,b=2");
            Knobs.Parse("a=10");
            Assert.AreEqual(10, Knobs.Get("a", 0));
            Assert.AreEqual(2, Knobs.Get("b", 0));
            Knobs.Set("b", "20");
            Assert.AreEqual("20", Knobs.Overrides["b"]);
        }

        [Test]
        public void Read_KeepsTheFirstRead()
        {
            Assert.AreEqual(5, Knobs.Get("a", 5));
            Knobs.Set("a", "6");
            Assert.AreEqual(6, Knobs.Get("a", 5));
            Assert.AreEqual("5", Knobs.Read["a"]);
        }

        [Test]
        public void Clear_DropsEverything_AndEveryChangeBumpsGeneration()
        {
            int g = Knobs.Generation;
            Knobs.Parse("a=1");
            Assert.AreEqual(g + 1, Knobs.Generation);
            Knobs.Set("b", "2");
            Assert.AreEqual(g + 2, Knobs.Generation);
            Knobs.Get("a", 0);
            Knobs.Clear();
            Assert.AreEqual(g + 3, Knobs.Generation);
            Assert.AreEqual(0, Knobs.Overrides.Count);
            Assert.AreEqual(0, Knobs.Read.Count);
            Assert.AreEqual(4, Knobs.Get("a", 4));
        }

        [Test]
        public void ToJson_IsSortedAndEscaped()
        {
            Assert.AreEqual("{\"set\":{},\"read\":{}}", Knobs.ToJson());
            Knobs.Set("z.q", "a\"b\\c");
            Knobs.Parse("b=2");
            Knobs.Get("b", 0);
            Knobs.Get("a", 1.5f);
            Assert.AreEqual("{\"set\":{\"b\":\"2\",\"z.q\":\"a\\\"b\\\\c\"},\"read\":{\"a\":\"1.5\",\"b\":\"2\"}}", Knobs.ToJson());
        }

        // ---- with nothing set, each area's knob-backed values are the constants they replaced

        [Test]
        public void Units_NothingSet_IsTheOldConstants()
        {
            Assert.AreEqual(120f, LodTiers.BlendZoom);
            Assert.AreEqual(1500000, LodTiers.VertexBudget);
            Assert.AreEqual(3f, LodTiers.CullRadius);
            Assert.AreEqual(120f, LodTiers.ReadBlendZoom());
            Assert.AreEqual(1500000, LodTiers.ReadVertexBudget());
            Assert.AreEqual(3f, LodTiers.ReadCullRadius());
            Knobs.Set("vat.blendZoom", "90");
            Assert.AreEqual(90f, LodTiers.ReadBlendZoom(), "and a set knob is what is read");
        }

        [Test]
        public void Camera_NothingSet_IsTheOldConstants()
        {
            int[] old = { 1024, 512, 384, 512, 256, 256, 64, 256, 128, 128 };
            float scale = Knobs.Get("debris.capacityScale", 1f);
            Assert.AreEqual(1f, scale);
            Assert.AreEqual(old.Length, (int)DebrisRenderer.Piece.Count);
            for (int k = 0; k < old.Length; k++)
            {
                Assert.AreEqual(old[k], DebrisRenderer.CapacityOf((DebrisRenderer.Piece)k));
                Assert.AreEqual(old[k], DebrisRenderer.CapacityOf((DebrisRenderer.Piece)k, scale));
            }
            Assert.AreEqual(55f, Knobs.Get("debris.shareNear", DebrisMath.ShareNear));
            Assert.AreEqual(120f, Knobs.Get("debris.shareFar", DebrisMath.ShareFar));
            foreach (float d in new[] { 0f, 54.9f, 55f, 80f, 119.9f, 120f, 500f })
                Assert.AreEqual(DebrisMath.Share(d), DebrisMath.Share(d, DebrisMath.ShareNear, DebrisMath.ShareFar));
            Assert.AreEqual(1536, FlipbookFx.MaxCards);
            Assert.AreEqual(160, TankRenderer.MaxLoose);
            Assert.AreEqual(1536, Knobs.Get("flipbook.maxCards", FlipbookFx.MaxCards));
            Assert.AreEqual(160, Knobs.Get("tank.maxLoose", TankRenderer.MaxLoose));
        }

        [Test]
        public void Terrain_NothingSet_IsTheOldConstants()
        {
            Assert.AreEqual(48, Knobs.Get("props.maxLoose", PropDestruction.MaxLoose));
            Assert.AreEqual(256, Knobs.Get("props.maxFalling", PropDestruction.MaxFalling));
            Assert.AreEqual(2800, Knobs.Get("rain.maxStreaks", Rain.MaxStreaks));
            Assert.AreEqual(6, Knobs.Get("life.maxRats", SmallLife.MaxRats));
            Assert.AreEqual(900, Knobs.Get("life.maxMotes", SmallLife.MaxMotes));
            Assert.AreEqual(12, Knobs.Get("lights.maxLanterns", NightLights.MaxLanterns));
            Assert.AreEqual(14, Knobs.Get("lights.maxTrenchLamps", NightLights.MaxTrenchLamps));
            Assert.AreEqual(5, Knobs.Get("lights.maxFires", NightLights.MaxFires));
            Assert.AreEqual(6, Knobs.Get("lights.maxTorches", NightLights.MaxTorches));
            Assert.AreEqual(12, Knobs.Get("lights.maxPropLamps", NightLights.MaxPropLamps));
            Assert.AreEqual(8, Knobs.Get("lights.poolSize", NightLights.PoolSize));
            Assert.AreEqual(2.0, (double)Knobs.Get("terrain.chunkBudgetMs", (float)GreyboxTerrainView.ChunkBudgetMs));
        }
    }
}
