// Phase: code review fixes (2026-10) — [X2] Book.Core/Head/Bloom were ordered to not match Sheets (decisions.md
// 2026-09-28, BUG-desktop-20260928-flipbook-ordinals): each drew the wrong sheet. [X3] fx.maxMarks/fx.maxBodies past
// their old, unclamped read could reach 0 (RemoveAt(0)/marks[0] on an empty list, both inside event handlers) or past
// 1023 (overflows the instancing batch).
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class FlipbookFxTests
    {
        [SetUp] public void SetUp() => Knobs.Clear();
        [TearDown] public void TearDown() => Knobs.Clear();

        /// <summary>[X2] Sheets runs ..., FireStand(Stand), FirePool(Pool), FireHead, FireBloom, FireCore, so Book's own
        /// order after Pool has to be Head, Bloom, Core for each entry to draw its own name. Fails on old code (Core,
        /// Head, Bloom come out swapped against the Sheets array), passes after the reorder.</summary>
        [Test]
        public void BookOrder_MatchesSheetsByPosition()
        {
            var sheets = (System.Array)typeof(FlipbookFx).GetField("Sheets", BindingFlags.NonPublic | BindingFlags.Static).GetValue(null);
            var sheetType = sheets.GetType().GetElementType();
            var nameField = sheetType.GetField("Name");
            foreach (var b in new[] { FlipbookFx.Book.Jet, FlipbookFx.Book.Blast, FlipbookFx.Book.Fan, FlipbookFx.Book.Stand,
                                       FlipbookFx.Book.Pool, FlipbookFx.Book.Head, FlipbookFx.Book.Bloom, FlipbookFx.Book.Core })
            {
                string name = (string)nameField.GetValue(sheets.GetValue((int)b));
                Assert.AreEqual("Fire" + b, name, "[X2] " + b + " is drawing the wrong sheet");
            }
        }

        /// <summary>[X3] CombatFx has no ExecuteAlways/ExecuteInEditMode, so AddComponent in an EditMode test never
        /// runs Awake: the clamp test passed on the unclamped code because the knobs were never read at all. Call it
        /// by hand, as DebrisTests does. Awake only reads knobs and pure Read* helpers, so it is safe here and twice
        /// over is harmless.</summary>
        static void Awake(CombatFx fx) =>
            typeof(CombatFx).GetMethod("Awake", BindingFlags.NonPublic | BindingFlags.Instance).Invoke(fx, null);

        /// <summary>[X3] fx.maxBodies=0 makes CombatFx.Deaths.cs's RemoveAt(0) run on an empty bodies list (OnDeath
        /// throws); fx.maxMarks=0 makes CombatFx.Ground.cs's AddMark fall into marks[0] on an empty list (throws). Past
        /// 1023 either overflows the instancing batch every frame it draws. Fails on old code (0 and 5000 pass through
        /// unclamped), passes once Awake clamps both to [1, 1023].</summary>
        [Test]
        public void Awake_ClampsMaxBodiesAndMaxMarks()
        {
            var go = new GameObject("CombatFx");
            try
            {
                Knobs.Set("fx.maxBodies", "0");
                Knobs.Set("fx.maxMarks", "0");
                var fx = go.AddComponent<CombatFx>();
                Awake(fx);
                Assert.GreaterOrEqual(fx.MaxBodies, 1, "[X3] fx.maxBodies=0 must not reach RemoveAt(0) on an empty list");
                int maxMarks = (int)typeof(CombatFx).GetField("MaxMarks", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                Assert.GreaterOrEqual(maxMarks, 1, "[X3] fx.maxMarks=0 must not reach marks[0] on an empty list");
                Object.DestroyImmediate(fx);

                Knobs.Set("fx.maxBodies", "5000");
                Knobs.Set("fx.maxMarks", "5000");
                var fx2 = go.AddComponent<CombatFx>();
                Awake(fx2);
                Assert.LessOrEqual(fx2.MaxBodies, 1023, "[X3] fx.maxBodies must not overflow the instancing batch");
                int maxMarks2 = (int)typeof(CombatFx).GetField("MaxMarks", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx2);
                Assert.LessOrEqual(maxMarks2, 1023, "[X3] fx.maxMarks must not overflow the instancing batch");
            }
            finally
            {
                Object.DestroyImmediate(go);
            }
        }

        /// <summary>[TS2] ColumnLightTests and ColumnPlayTests open on Assume.That(fx.Ready): a missing flipbook sheet
        /// or shader turns both Inconclusive, which the gate does not fail on, and no test anywhere asserted
        /// FlipbookFx.Ready. This is that assertion, so a sheet that goes missing is one red test rather than a silent
        /// pair of skips.</summary>
        [Test]
        public void TheFlipbookShaderAndSheetsLoadInTheEditor()
        {
            var fx = new FlipbookFx();
            try
            {
                Assert.IsTrue(fx.Ready, "[TS2] the flipbook shader and sheets load in the editor");
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

        /// <summary>[X6] Add at the cap used RemoveAt(0), which moved up to 1,535 Cards down the list on every Add and
        /// dropped the FIRST card added rather than the oldest one (a delayed card is born later than one added after
        /// it). The oldest card now gives up its slot in place. Fails on old code (the 0.3 s card is dropped and slot 0
        /// holds 0.2), passes once the slot is overwritten.</summary>
        [Test]
        public void At_The_Cap_The_Oldest_Card_Gives_Up_Its_Slot()
        {
            Knobs.Set("flipbook.maxCards", "4");
            var fx = new FlipbookFx();
            try
            {
                Assert.IsTrue(fx.Ready, "setup: the flipbook sheets load");
                float t0 = Time.time;
                foreach (float delay in new[] { 0.3f, 0.2f, 0.1f, 0f })
                    fx.Add(FlipbookFx.Book.Puff, Vector3.zero, 1f, 5f, delay: delay);
                fx.Add(FlipbookFx.Book.Puff, Vector3.zero, 1f, 5f, delay: 1f);   // at the cap: the 0 s card is the oldest

                var cards = (System.Collections.IList)typeof(FlipbookFx).GetField("cards", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                var born = cards[0].GetType().GetField("Born");
                var delays = new System.Collections.Generic.List<float>();
                for (int i = 0; i < cards.Count; i++) delays.Add((float)born.GetValue(cards[i]) - t0);
                Assert.AreEqual(4, cards.Count, "[X6] the cap holds");
                Assert.AreEqual(0.3f, delays[0], 0.01f, "[X6] the first card added kept its slot: nothing shifted");
                CollectionAssert.AreEquivalent(new[] { 0.3f, 0.2f, 0.1f, 1f }, delays.ConvertAll(d => Mathf.Round(d * 10f) / 10f), "[X6] the oldest card, not the first added, gave up its slot");
            }
            finally
            {
                var mats = (Material[])typeof(FlipbookFx).GetField("mats", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                foreach (var m in mats) if (m != null) Object.DestroyImmediate(m);
                var quad = (Mesh)typeof(FlipbookFx).GetField("quad", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                if (quad != null) Object.DestroyImmediate(quad);
            }
        }
    }
}
