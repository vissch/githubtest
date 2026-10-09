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
using TW.Presentation.Tactical;
using TW.UI;
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
            Assert.AreEqual(HudView.SupportAbilities.Length, HudLayout.SupportSlots, "the model counts the cards the bar draws");
            float supportRow = HudLayout.Row(HudLayout.SupportSlots, HudLayout.CardPx);
            Assert.That(HudLayout.BarWidth(), Is.GreaterThanOrEqualTo(supportRow + HudLayout.InsetPx * 2f), "the bar is at least as wide as its support row");
            Assert.GreaterOrEqual(HudView.BarFit(HudLayout.ReferenceWidthPx), 0.9f, "at the reference width the bar keeps nine tenths of its size");
            Assert.Less(HudView.BarFit(HudLayout.ReferenceHeight * 4f / 3f), 1f, "a 4:3 window shrinks it rather than losing the support row");
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
    }
}
