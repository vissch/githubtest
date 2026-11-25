// Phase: tooling (perf pass, 2026-11-25) — the fallen are drawn with two materials: the corpse that lost a limb keeps
// _TW_LIMBCUT (the one clip() in VAT_URP), every other corpse draws with a variant that has no clip() and so keeps
// early-Z. The picture must be the same: what makes a corpse look like a corpse is _TW_FALLEN, not _TW_LIMBCUT, and the
// split is decided by the record's own limb bits. Also: the expired bodies are dropped in one pass (DropExpired), with
// the result a Remove(i) per body gave.
using System.Collections;
using System.IO;
using System.Reflection;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class VatFallenSplitTests
    {
        const string ShaderPath = "Assets/_Project/Shaders/VAT_URP.shader";
        const string RendererPath = "Assets/_Project/Presentation/Units/VATRenderer.cs";
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        static string Code(string path) => Regex.Replace(File.ReadAllText(path), @"//[^\n]*", "");

        [Test]
        public void TheLimbCutKeywordGatesTheClipAndNothingElse()
        {
            string code = Code(ShaderPath);
            // every #if on _TW_LIMBCUT is the VAT_CLIP_LOST definition: no look branch hangs on the keyword that costs early-Z
            var blocks = Regex.Matches(code, @"#if defined\(_TW_LIMBCUT\)(.*?)#endif", RegexOptions.Singleline);
            Assert.That(blocks.Count, Is.EqualTo(1), "_TW_LIMBCUT may gate one block only: the clip");
            Assert.That(blocks[0].Value, Does.Contain("define VAT_CLIP_LOST(gone) clip("), "the one _TW_LIMBCUT block is the clip");
            Assert.That(Regex.Matches(code, @"\bclip\s*\(").Count, Is.EqualTo(1), "the only clip() is inside VAT_CLIP_LOST");
        }

        [Test]
        public void TheDeadsLookHangsOnTheFallenKeyword()
        {
            string code = Code(ShaderPath);
            Assert.That(Regex.Matches(code, @"#if defined\(_TW_FALLEN\)").Count, Is.EqualTo(2),
                "the snow dusting and the dead keeping their colour: both are look, both must reach every corpse, cut or not");
            Assert.That(Regex.Matches(code, @"#pragma multi_compile_local_fragment _ _TW_FALLEN").Count, Is.EqualTo(1),
                "_TW_FALLEN is read in the ForwardLit fragment only, and declared there");
        }

        [Test]
        public void EveryFallenMaterialIsFallenAndOnlyTheCuttingOneClips()
        {
            string code = Code(RendererPath);
            Assert.That(code, Does.Contain("fallen.EnableKeyword(\"_TW_FALLEN\")"));
            Assert.That(code, Does.Contain("fallen.EnableKeyword(\"_TW_LIMBCUT\")"));
            Assert.That(code, Does.Contain("fallenWhole.EnableKeyword(\"_TW_FALLEN\")"));
            Assert.That(code, Does.Not.Contain("fallenWhole.EnableKeyword(\"_TW_LIMBCUT\")"), "the whole-corpse material must have no clip()");
            Assert.That(Regex.Matches(code, @"fallenWhole.SetVector\(PosMinId").Count, Is.EqualTo(1),
                "new Material(m) drops _PosMin/_PosSize: the second fallen material sets them again");
        }

        [Test]
        public void ACorpseNeedsTheClippingMaterialExactlyWhenItsRecordPacksALostLimb()
        {
            for (int gib = 0; gib < 256; gib++)
            {
                float pad = VatPad.Pack(gib, 0.5f, 77, 2);   // what Fallen() writes into the record, whatever else it carries
                Assert.AreEqual(VatPad.Lost(pad) != 0, VATRenderer.FallenNeedsClip(gib), "gib " + gib);
            }
            Assert.IsFalse(VATRenderer.FallenNeedsClip(0));
            Assert.IsTrue(VATRenderer.FallenNeedsClip(1 << 5));
            Assert.IsFalse(VATRenderer.FallenNeedsClip(1 << 6), "bit 6 and up are not limbs: VatPad masks them, the shader never reads them as lost");
        }

        // the dead in one pass: the same bodies left, in the same order, with the same drawn positions as a Remove per body
        [Test]
        public void DroppingTheExpiredInOnePassIsWhatRemovingThemOneByOneWas()
        {
            bool[] expired = { true, false, true, true, false, false, true, false };
            var ga = new GameObject("fallen drop a"); var gb = new GameObject("fallen drop b");
            try
            {
                var a = Rig(ga, expired, out var drawA); var b = Rig(gb, expired, out var drawB);
                typeof(VATRenderer).GetMethod("DropExpired", Any).Invoke(a, new object[] { 100f });
                var remove = typeof(VATRenderer).GetMethod("Remove", Any);
                var menB = (IList)typeof(VATRenderer).GetField("fallenMen", Any).GetValue(b);
                for (int i = menB.Count - 1; i >= 0; i--)
                    if (100f - BornOf(menB[i]) > 10f) remove.Invoke(b, new object[] { i });
                var menA = (IList)typeof(VATRenderer).GetField("fallenMen", Any).GetValue(a);
                Assert.AreEqual(4, menA.Count, "the premise: four of the eight were still lying");
                Assert.AreEqual(menB.Count, menA.Count);
                for (int i = 0; i < menA.Count; i++) Assert.AreEqual(BornOf(menB[i]), BornOf(menA[i]), "survivor " + i + " out of order");
                Assert.AreEqual(drawB.Length, drawA.Length);
                for (int i = 0; i < drawA.Length; i++) Assert.AreEqual(drawB[i], drawA[i], "drawn position " + i);
            }
            finally { Object.DestroyImmediate(ga); Object.DestroyImmediate(gb); }
        }

        static float BornOf(object man) => (float)man.GetType().GetField("Born", Any).GetValue(man);

        static VATRenderer Rig(GameObject go, bool[] expired, out Vector3[] drawAt)
        {
            var r = go.AddComponent<VATRenderer>();
            for (int i = 0; i < expired.Length; i++) r.AddFallen(new Vector3(10f + 5f * i, 0f, 10f), 0f, 0, 0);
            var men = (IList)typeof(VATRenderer).GetField("fallenMen", Any).GetValue(r);
            Assert.AreEqual(expired.Length, men.Count, "the premise: every AddFallen recorded a man");
            drawAt = new Vector3[600];
            for (int i = 0; i < expired.Length; i++)
            {
                object m = men[i];
                var t = m.GetType();
                t.GetField("Born", Any).SetValue(m, (expired[i] ? 80f : 95f) + 0.1f * i);   // at now = 100 the first lay 20 s of a 10 s lie, the second 5 s
                t.GetField("Lies", Any).SetValue(m, 10f);
                men[i] = m;
                drawAt[i] = new Vector3(i + 1f, 0f, 0f);
            }
            typeof(VATRenderer).GetField("fallenDrawAt", Any).SetValue(r, drawAt);
            return r;
        }
    }
}
