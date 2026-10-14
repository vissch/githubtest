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
                var fx = go.AddComponent<CombatFx>();   // Awake runs on AddComponent
                Assert.GreaterOrEqual(fx.MaxBodies, 1, "[X3] fx.maxBodies=0 must not reach RemoveAt(0) on an empty list");
                int maxMarks = (int)typeof(CombatFx).GetField("MaxMarks", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                Assert.GreaterOrEqual(maxMarks, 1, "[X3] fx.maxMarks=0 must not reach marks[0] on an empty list");
                Object.DestroyImmediate(fx);

                Knobs.Set("fx.maxBodies", "5000");
                Knobs.Set("fx.maxMarks", "5000");
                var fx2 = go.AddComponent<CombatFx>();
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
    }
}
