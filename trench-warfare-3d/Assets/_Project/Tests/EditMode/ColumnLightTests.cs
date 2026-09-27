// Phase: AOSA C108 (juice J01, split from C103) - the night column's share of the burst's light (fx.columnBurstLit) and
// its height cap over men (fx.columnCap), each on its own. At their defaults (1 and 0) nothing is set and the old Add runs.
using System.IO;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class ColumnLightTests
    {
        [SetUp] public void SetUp() => Knobs.Clear();
        [TearDown] public void TearDown() => Knobs.Clear();

        [Test]
        public void Knobs_DefaultIsToday_AndReadsAreClamped()
        {
            Assert.AreEqual("fx.columnBurstLit", FlipbookFx.ColumnBurstLitKnob);
            Assert.AreEqual("fx.columnCap", FlipbookFx.ColumnCapKnob);
            Assert.AreEqual(FlipbookFx.OldColumnBurstLit, FlipbookFx.DefaultColumnBurstLit, "the default is today's light");
            Assert.AreEqual(FlipbookFx.OldColumnCap, FlipbookFx.DefaultColumnCap, "the default is today's height");
            Assert.AreEqual(1f, FlipbookFx.ReadColumnBurstLit());
            Assert.AreEqual(0f, FlipbookFx.ReadColumnCap());
            Assert.AreEqual("1", Knobs.Read["fx.columnBurstLit"]);
            Assert.AreEqual("0", Knobs.Read["fx.columnCap"]);

            Knobs.Set(FlipbookFx.ColumnBurstLitKnob, "0.3");
            Knobs.Set(FlipbookFx.ColumnCapKnob, "0.5");
            Assert.AreEqual(0.3f, FlipbookFx.ReadColumnBurstLit(), 1e-6f);
            Assert.AreEqual(0.5f, FlipbookFx.ReadColumnCap(), 1e-6f);
            Knobs.Set(FlipbookFx.ColumnBurstLitKnob, "-1");
            Knobs.Set(FlipbookFx.ColumnCapKnob, "3");
            Assert.AreEqual(0f, FlipbookFx.ReadColumnBurstLit());
            Assert.AreEqual(1f, FlipbookFx.ReadColumnCap());
        }

        [Test]
        public void CapScale_IsOneExactlyAtZero_AndC103sCapAtOne()
        {
            foreach (float cap in new[] { FlipbookFx.SoilLow, 0.5f, 1f })
            {
                Assert.IsTrue(FlipbookFx.ColumnCapScale(0f, cap) == 1f, "knob 0: the old height, bit for bit");
                Assert.IsTrue(FlipbookFx.ColumnCapScale(-1f, cap) == 1f);
                Assert.AreEqual(cap, FlipbookFx.ColumnCapScale(1f, cap), 1e-6f, "knob 1: C103's cap");
                Assert.AreEqual(Mathf.Lerp(1f, cap, 0.5f), FlipbookFx.ColumnCapScale(0.5f, cap), 1e-6f);
            }
            // with no man behind it SoilCap is 1, so the capped column is today's column at any knob
            float reach = 20f * (1f + FlipbookFx.ColumnGrow);
            Assert.AreEqual(1f, FlipbookFx.ColumnCapScale(1f, FlipbookFx.SoilCap(float.MaxValue, 0.466f, reach)));
            // a man at its foot: never below SoilLow (a low heave, never gone)
            Assert.AreEqual(FlipbookFx.SoilLow, FlipbookFx.ColumnCapScale(1f, FlipbookFx.SoilCap(0f, 0.466f, reach)), 1e-6f);
            Assert.AreEqual(0.35f, FlipbookFx.ColumnGrow, "the old column's grow (CombatFx's Add)");
        }

        [Test]
        public void Shader_SkipsTheBurstLightLineAtOne()
        {
            // the bit-identity guard: _BurstLit defaults to 1 and the shader only scales the light below 1
            string src = File.ReadAllText(Path.Combine(Application.dataPath, "_Project", "Shaders", "Flipbook_URP.shader"));
            StringAssert.Contains("Range(0, 1)) = 1", src.Substring(src.IndexOf("_BurstLit (")));
            StringAssert.Contains("if (_BurstLit < 1.0) fire *= _BurstLit;", src);
        }

        [Test]
        public void ColumnBurstLit_SetsTheColumnBookOnly_AndOneSetsNothing()
        {
            var fx = new FlipbookFx();
            try
            {
                Assume.That(fx.Ready, "the flipbook shader and sheets load in the editor");
                var mats = (Material[])typeof(FlipbookFx).GetField("mats", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                var before = new float[mats.Length];
                for (int b = 0; b < mats.Length; b++) before[b] = mats[b].GetFloat("_BurstLit");

                // the cause: C57's paint leaves the column all of the burst's light
                fx.NightEarth(FlipbookFx.DefaultEarth);
                Assert.AreEqual(1f, fx.ColumnBurstLitNow, "NightEarth leaves _BurstLit 1 on the Column");

                // 1 (the default): nothing set
                fx.ColumnBurstLit(1f);
                for (int b = 0; b < mats.Length; b++) Assert.IsTrue(mats[b].GetFloat("_BurstLit") == before[b], (FlipbookFx.Book)b + " untouched at 1");

                // below 1: the Column book alone
                fx.ColumnBurstLit(0.3f);
                Assert.AreEqual(0.3f, fx.ColumnBurstLitNow, 1e-6f);
                for (int b = 0; b < mats.Length; b++)
                    if ((FlipbookFx.Book)b != FlipbookFx.Book.Column) Assert.IsTrue(mats[b].GetFloat("_BurstLit") == before[b], (FlipbookFx.Book)b + " untouched");

                // fx.columnSoil 0 sets nothing over it; above 0 the heave sets its own share
                fx.SoilEarth(0f, FlipbookFx.DefaultEarth);
                Assert.AreEqual(0.3f, fx.ColumnBurstLitNow, 1e-6f);
                fx.SoilEarth(1f, FlipbookFx.DefaultEarth);
                Assert.AreEqual(FlipbookFx.SoilFire, fx.ColumnBurstLitNow, 1e-6f);
            }
            finally
            {
                // DestroyImmediate: Dispose's Object.Destroy is not allowed in edit mode
                var mats = (Material[])typeof(FlipbookFx).GetField("mats", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                foreach (var m in mats) if (m != null) Object.DestroyImmediate(m);
                var quad = (Mesh)typeof(FlipbookFx).GetField("quad", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                if (quad != null) Object.DestroyImmediate(quad);
            }
        }
    }
}
