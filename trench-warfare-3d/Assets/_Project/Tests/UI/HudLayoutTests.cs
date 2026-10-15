// Phase: tooling (2026-09-23) — the deploy bar's frame covers what is drawn into it, the bar fits the room it is
// given, it survives the roster growing again, it never produces a degenerate cell, and unit names still fit at the
// owner's target resolution. The first test that touches the HUD.
//
// Nothing in BattleHud had ever been executed by anything in this project. No test runs OnGUI, and until this file
// TW.Tests.EditMode did not reference TW.Presentation.Camera at all, so the whole of that class could be wrong in
// any way and still pass a green gate. It was: the wooden deploy frame was two pixels narrower than the buttons
// drawn into it, at every resolution, for longer than the roster has had seven slots, and when the roster grew the
// same fault became 174 px of buttons outside the frame and an IndexOutOfRangeException every frame.
//
// These are not tests of the fix. They are tests of the invariants the fix restored, written so that the next
// person to widen the bar, add a support button or grow the roster is told by the gate rather than by the owner.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.UI;
using UnityEngine.UIElements;
using TW.Sim;   // RosterEntry: the ASSEMBLY is TW.Sim.Core, the namespace is TW.Sim. They do not match.

namespace TW.Tests
{
    public class HudLayoutTests
    {
        /// <summary>Room across the bar for each window the game can plausibly be asked to draw itself into, with
        /// the debug panel shut and open. Derived the way BattleHud derives it, from the same thresholds.</summary>
        static float Room(int screenWidth, bool panelOpen)
        {
            float barLeft = panelOpen ? 320f : 0f;
            bool narrow = screenWidth - barLeft < 1000f;
            return screenWidth - barLeft - (narrow ? 8f : 118f) - (narrow ? 104f : 236f);
        }

        static readonly int[] Widths = { 3840, 2560, 1920, 1600, 1366, 1280, 1024, 800 };

        [Test]
        public void FrameCoversEverythingDrawnIntoIt([ValueSource(nameof(Widths))] int width)
        {
            foreach (bool panelOpen in new[] { false, true })
            {
                BattleHud.BarMetrics(Room(width, panelOpen), out float size, out float frameW);
                float consumed = BattleHud.BarConsumed(size);
                Assert.That(frameW, Is.GreaterThanOrEqualTo(consumed - 0.01f),
                    $"at {width} with the panel {(panelOpen ? "open" : "shut")} the frame is {frameW:0.##} px and " +
                    $"the bar draws {consumed:0.##} px into it: {consumed - frameW:0.##} px of buttons outside the wood");
            }
        }

        [Test]
        public void BarFitsTheRoomItIsGiven([ValueSource(nameof(Widths))] int width)
        {
            foreach (bool panelOpen in new[] { false, true })
            {
                float room = Room(width, panelOpen);
                BattleHud.BarMetrics(room, out _, out float frameW);
                Assert.That(frameW, Is.LessThanOrEqualTo(room + 0.01f),
                    $"at {width} with the panel {(panelOpen ? "open" : "shut")} the bar is {frameW:0.##} px wide in " +
                    $"{room:0.##} px of room, so its right-hand end — the support buttons — is off the screen");
            }
        }

        /// <summary>
        /// The invariant that actually failed. The frame was sized from a literal 7 while the loop drew
        /// RosterEntry.SlotCount + 2 cells, so the bar broke the moment the roster grew. It has grown twice since.
        /// </summary>
        [Test]
        public void SurvivesTheRosterGrowing()
        {
            Assert.That(RosterEntry.SlotCount, Is.GreaterThan(0), "a roster of no slots would divide by zero");
            BattleHud.BarMetrics(Room(1920, false), out float size, out float frameW);
            Assert.That(frameW, Is.GreaterThanOrEqualTo(BattleHud.BarConsumed(size) - 0.01f));
            Assert.That(size, Is.GreaterThan(24f),
                $"at 1920 with {RosterEntry.SlotCount} roster slots the buttons are down to {size:0.#} px; the bar " +
                "needs redesigning (a second row, or scrolling) rather than shrinking further");
        }

        /// <summary>A cell size of zero or less inverts every Rect on the bar, which GUI draws as garbage.</summary>
        [Test]
        public void NeverProducesADegenerateCell()
        {
            foreach (float room in new[] { -500f, 0f, 1f, 50f, 120f, 300f, 4000f })
            {
                BattleHud.BarMetrics(room, out float size, out float frameW);
                Assert.That(size, Is.GreaterThan(0f), $"room {room} gave a cell of {size}");
                Assert.That(frameW, Is.GreaterThan(0f), $"room {room} gave a frame of {frameW}");
            }
        }

        /// <summary>
        /// Unit names are drawn inside the button only above 70 px, so a window that falls below that loses them
        /// silently. Not a failure — it is the documented behaviour — but the owner's target must stay above it.
        /// </summary>
        [Test]
        public void UnitNamesStillFitAtTheTargetResolution()
        {
            BattleHud.BarMetrics(Room(1920, false), out float size, out _);
            Assert.That(size, Is.GreaterThanOrEqualTo(70f),
                $"at 1920x1080 the cells are {size:0.#} px, under the 70 px at which unit names stop being drawn");
        }

        [Test]
        public void TheWidthModelCountsEverySupportCardAndTheBarKeepsNineTenthsAtTheReferenceWidth()
        {
            // seven support cards and ten roster slots since units-meta landed on the overhaul: the model that drives
            // HudView.BarFit must count them. The bar is 1,732 wide, so at the 16:9 reference it draws at 0.91 (the
            // owner's choice, decisions.md 2026-09-27, over narrower infantry cards); it must not shrink further
            // unnoticed, and a narrower window shrinks it instead of overflowing
            // [TU4] Three of the four asserts here could not fail: SupportSlots IS SupportAbilities.Length,
            // BarWidth() is a sum that contains its own support row, and BarFit goes through Mathf.Clamp01 so it is
            // never above 1. The numbers are the owner's (decisions.md 2026-09-27): ten slots, seven support cards,
            // 1,732 px, 0.91. The 4:3 figure worked by hand: a 4:3 reference window is 900 * 4/3 = 1,200 px wide,
            // less the 2 * 12 px inset leaves 1,176 px of room, and 1,176 / 1,732 = 0.679.
            Assert.AreEqual(7, HudLayout.SupportSlots, "[TU4] seven support cards (decisions.md 2026-09-27)");
            Assert.AreEqual(10, RosterEntry.SlotCount, "[TU4] ten roster slots (decisions.md 2026-09-27)");
            Assert.That(HudLayout.BarWidth(), Is.EqualTo(1732f).Within(0.5f), "[TU4] the default bar is 1,732 px wide (decisions.md 2026-09-27)");
            Assert.GreaterOrEqual(HudView.BarFit(HudLayout.ReferenceWidthPx), 0.9f, "at the reference width the bar keeps nine tenths of its size");
            Assert.That(HudView.BarFit(HudLayout.ReferenceHeight * 4f / 3f), Is.EqualTo(0.679f).Within(0.002f), "[TU4] a 4:3 window shrinks the bar to 0.679, not to Clamp01's 1");
        }

        // no UXML: HudView.Build queries what it can and falls back to code-built cards (the HudBindTests idiom)
        static HudRefs BuildBare(RosterEntry[] roster, uint abilityMask = 0)
        {
            var root = new VisualElement { name = HudView.RootName };
            foreach (var n in HudView.RequiredNames) if (n != HudView.RootName) root.Add(n.EndsWith("-value") || n.StartsWith("tooltip") || n == "hint" || n == "banner-text" || n.StartsWith("caption") ? new Label { name = n } : n.StartsWith("speed-") && n != "speed-value" && n != "speed-bar" ? (VisualElement)new Button { name = n } : new VisualElement { name = n });
            return HudView.Build(root, null, roster, new[] { 150, 120 }, abilityMask);
        }

        /// <summary>[I4] The shrink factor is measured on the bar that was really drawn. The British Empire fields seven
        /// infantry cards and three armour: 24 inset + 7*180 + 6*8 infantry + 8 + 20 divider + 8 + max(3 armour 232,
        /// 7 support 552) = 1,920 px, where the default Iron roster is 1,732. Shrunk by Iron's factor the bar drew
        /// 1,747 px into the 1,576 px the reference width leaves, running off both screen edges.</summary>
        [Test]
        public void TheShrinkFactorIsMeasuredOnTheRosterTheBarReallyDrew()
        {
            var roster = new RosterEntry[RosterEntry.SlotCount];
            for (int s = 0; s < roster.Length; s++) roster[s] = FactionRoster.Slot(FactionId.British, s);
            var refs = BuildBare(roster);
            Assert.That(HudView.BarWidth(refs), Is.EqualTo(1920f).Within(0.5f), "[I4] the measured bar is not the roster the HUD drew");
            float room = HudLayout.ReferenceWidthPx - 2f * HudLayout.InsetPx;   // 1,576
            Assert.That(HudView.BarFit(HudLayout.ReferenceWidthPx, HudView.BarWidth(refs)) * 1920f, Is.LessThanOrEqualTo(room + 0.5f),
                "[I4] a seven-infantry faction's bar overflows both screen edges at 16:9");
        }

        /// <summary>[Q3] The debug panel is hidden in the shipped game; the editor, a Development build, or the
        /// -twpanel start option bring it back.</summary>
        [Test]
        public void ShippedGameOffersNoDebugPanelUnlessStarted()
        {
            Assert.IsFalse(TestPanel.OfferedIn(false, false, false), "[Q3] the shipped game still offers the debug panel");
            Assert.IsTrue(TestPanel.OfferedIn(true, false, false), "[Q3] the editor must still offer the debug panel");
            Assert.IsTrue(TestPanel.OfferedIn(false, true, false), "[Q3] a Development build must still offer the debug panel");
            Assert.IsTrue(TestPanel.OfferedIn(false, false, true), "[Q3] the -twpanel start option must offer the debug panel");
        }

        [Test]
        public void AShippedBuildCannotOpenTheDebugPanelThroughVisible()
        {
            var go = new GameObject("TestPanel");
            try
            {
                var panel = go.AddComponent<TestPanel>();
                panel.Offered = false;
                panel.Visible = true;
                Assert.IsFalse(panel.Visible, "[Q3] a shipped build can still open the debug drawer");
            }
            finally { Object.DestroyImmediate(go); }
        }

        /// <summary>[Q9] BattleHud generates ~35 HideAndDontSave textures (plus the GUIStyle backgrounds built from
        /// them) in Start and owned them alone: nothing destroyed them when the component did, so a restart (a new
        /// scene load adding a fresh BattleHud) leaked the lot. OnDestroy must free them.</summary>
        [Test]
        public void BattleHudFreesItsGeneratedTexturesOnDestroy()
        {
            var go = new GameObject("BattleHud");
            BattleHud hud = null;
            Texture2D[] icons;
            Texture2D side;
            try
            {
                hud = go.AddComponent<BattleHud>();
                typeof(BattleHud).GetMethod("Start", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic)
                    .Invoke(hud, null);
                icons = (Texture2D[])typeof(BattleHud).GetField("unitIcons", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic)
                    .GetValue(hud);
                side = (Texture2D)typeof(BattleHud).GetField("sideTex", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic)
                    .GetValue(hud);
                Assert.IsNotNull(side, "[Q9] setup: Start should have built sideTex");
                Assert.IsTrue(System.Array.Exists(icons, t => t != null), "[Q9] setup: Start should have built at least one icon");
                // invoked directly rather than through Object.Destroy(hud), which in edit mode only queues OnDestroy
                // for the next editor update rather than running it inside this test
                typeof(BattleHud).GetMethod("OnDestroy", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic)
                    .Invoke(hud, null);
            }
            finally
            {
                if (hud != null) Object.DestroyImmediate(hud);
                Object.DestroyImmediate(go);
            }
            Assert.IsTrue(side == null, "[Q9] BattleHud.OnDestroy did not free sideTex");
            Assert.IsTrue(System.Array.TrueForAll(icons, t => t == null), "[Q9] BattleHud.OnDestroy did not free every unit icon");
        }

        /// <summary>[Q9] DebugOverlay.Start always added a BattleHud alongside its own capsule/stats overlay, even
        /// when the Toolkit HUD (HudBridge.UseToolkitHud) is the one actually drawing the game's chrome: two HUDs
        /// drawing, and a BattleHud's textures the Toolkit HUD never needed.</summary>
        [Test]
        public void DebugOverlayAddsNoBattleHudUnderTheToolkitHud()
        {
            bool was = HudBridge.UseToolkitHud;
            var go = new GameObject("DebugOverlay");
            try
            {
                HudBridge.UseToolkitHud = true;
                var host = go.AddComponent<SimHost>();
                var overlay = go.AddComponent<DebugOverlay>();
                overlay.Host = host;
                typeof(DebugOverlay).GetMethod("Start", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic)
                    .Invoke(overlay, null);
                Assert.IsNull(go.GetComponent<BattleHud>(), "[Q9] DebugOverlay added a BattleHud while the Toolkit HUD is in charge");
            }
            finally { HudBridge.UseToolkitHud = was; Object.DestroyImmediate(go); }
        }
    }
}
