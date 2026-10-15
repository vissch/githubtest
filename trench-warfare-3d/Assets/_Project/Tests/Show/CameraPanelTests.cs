// Phase: review-fix (rv-37-p2-u16, 2026-10-13) — code review 2026-10-04, unit 16: Q1 (TestPanel.cs:262), Q4
// (TacticalCamera.cs:156,171), Q5 (TestPanel.cs:59-67).
// rv-74-p3-u16, 2026-10-15: Q6 (TestPanel.cs:81), Q7 (TestPanel.cs:343).
using NUnit.Framework;
using Unity.Collections;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class CameraPanelTests
    {
        /// <summary>[Q1] TestPanel's deploy loop used to index a 5-long SlotNames table over RosterEntry.SlotCount
        /// (10) slots: every faction's 6th slot onward threw an IndexOutOfRangeException the moment it tried to
        /// draw its label.</summary>
        [Test]
        public void EveryDeploySlotHasAName()
        {
            foreach (var faction in new[] { FactionId.Iron, FactionId.Brass })
                for (int s = 0; s < RosterEntry.SlotCount; s++)
                {
                    var e = FactionRoster.Slot(faction, s);
                    string label = null;
                    Assert.DoesNotThrow(() => label = TestPanel.DeployLabel(s, e, 0, 1f),
                        $"[Q1] {faction} slot {s} threw building its deploy label");
                    StringAssert.Contains(UnitLook.Name(e.Archetype), label,
                        $"[Q1] {faction} slot {s} label did not name its own unit");
                }
        }

        /// <summary>[Q4] A middle-drag's pixels were scaled by PanSpeed * Time.unscaledDeltaTime same as the
        /// keys/edge-scroll per-second pan, so the same mouse movement panned less at a high frame rate and more
        /// at a low one. DragPanMeters is the pure fix: a drag's own pixels are the frame's actual movement
        /// already, never scaled by dt again.</summary>
        [Test]
        public void AMiddleDragPansTheSameAtAnyFrameRate()
        {
            const float zoom = 45f;
            Vector2 oneFrameOfTen = TacticalCamera.DragPanMeters(new Vector2(10f, 0f), zoom);
            Vector2 twoFramesOfFive = TacticalCamera.DragPanMeters(new Vector2(5f, 0f), zoom)
                                     + TacticalCamera.DragPanMeters(new Vector2(5f, 0f), zoom);
            Assert.AreEqual(oneFrameOfTen.x, twoFramesOfFive.x, 1e-4f, "[Q4] 10px in one frame != two frames of 5px");

            float expected = 10f * 0.12f * (zoom / 90f);
            Assert.AreEqual(expected, oneFrameOfTen.x, 1e-4f, "[Q4] a 10px drag did not move the worked-example distance");
        }

        /// <summary>[Q5] TestPanel.TryGroundPoint's chrome mask only ever covered this panel, the deploy bar and the
        /// minimap: a click on the silver box, the men count or any trench button (>>  Lock  Hold fire) fell
        /// through to the ground underneath it and could fire an armed ability or move a selection there instead.</summary>
        [Test]
        public void ATrenchButtonSwallowsTheGroundClick()
        {
            BattleHud.BeginChrome();
            BattleHud.AddChrome(new Rect(900, 400, 44, 44));   // a trench button, GUI coords (y from the top)

            const int screenW = 1920, screenH = 1080;
            Vector2 underButton = new Vector2(920f, screenH - 420f);   // same point, mouse coords (y from the bottom)
            Assert.IsTrue(TestPanel.OverImguiChrome(underButton, false, true, screenW, screenH),
                "[Q5] a click on a trench button reached the ground");

            Vector2 openField = new Vector2(1200f, 600f);
            Assert.IsFalse(TestPanel.OverImguiChrome(openField, false, true, screenW, screenH),
                "[Q5] the open field was wrongly swallowed");

            var corner = TestPanel.PanelButtonRect(screenW);
            Vector2 panelButton = new Vector2(corner.x + 5f, screenH - (corner.y + 5f));
            Assert.IsTrue(TestPanel.OverImguiChrome(panelButton, false, true, screenW, screenH),
                "[Q5] the Debug panel button itself did not swallow the click");
        }

        /// <summary>[Q6] A line-ability drag interrupted by a shell screen (gameplay lost mid-drag) stays alive with
        /// no button held, so a later release — neither held nor itself a release-this-frame — used to fire a
        /// corridor from the old press point. DragSurvives is false the moment gameplay is not focused or neither
        /// condition holds, true while the button is still down or on its own release frame.</summary>
        [Test]
        public void DragSurvivesOnlyWhileGameplayHasTheButtonOrItsRelease()
        {
            Assert.IsFalse(TestPanel.DragSurvives(false, false, false), "[Q6] no gameplay focus, nothing held: must not survive");
            Assert.IsFalse(TestPanel.DragSurvives(true, false, false), "[Q6] gameplay focus but no button and no release: must not survive");
            Assert.IsTrue(TestPanel.DragSurvives(true, true, false), "[Q6] the button is still held: must survive");
            Assert.IsTrue(TestPanel.DragSurvives(true, false, true), "[Q6] this is the release frame: must survive");
        }

        /// <summary>[Q6] DropDrag clears Dragging without touching Armed, so a following Release sees no drag and
        /// fires nothing — the stuck corridor is gone, the ability itself is still up.</summary>
        [Test]
        public void DropDragLosesTheLineButKeepsTheAbilityArmed()
        {
            var aim = new AbilityAim();
            aim.Arm(OffMapAbilityId.SmokeScreen);   // Target.Line: a press starts a drag rather than firing at once
            aim.Press(new Vector3(10f, 0f, 10f), 0, 0, out _);
            Assert.IsTrue(aim.Dragging, "[Q6] setup: the press should have started a drag");

            aim.DropDrag();

            Assert.IsFalse(aim.Dragging, "[Q6] DropDrag left the drag alive");
            Assert.AreEqual(OffMapAbilityId.SmokeScreen, aim.Armed, "[Q6] DropDrag disarmed the ability, not just the drag");
            Assert.IsFalse(aim.Release(new Vector3(20f, 0f, 10f), 1, 0, out var cmd), "[Q6] a release after DropDrag still fired a command");
        }

        /// <summary>[Q7] TankSlot is the first roster slot that is a tracked (tank) chassis. The FactionRoster table:
        /// slot 6 is the Maw for Iron and the Tusk for Brass (both tracked), but for the four historical armies slot 6
        /// is an engineer, an officer or a medic and their tank sits at slot 7.</summary>
        [Test]
        public void TankSlotFindsTheFirstTrackedSlotForEveryFaction()
        {
            var roster = new NativeArray<RosterEntry>(2 * RosterEntry.SlotCount, Allocator.Temp);
            try
            {
                void Check(FactionId faction, int expectedSlot)
                {
                    for (int s = 0; s < RosterEntry.SlotCount; s++) roster[RosterEntry.SlotCount + s] = FactionRoster.Slot(faction, s);
                    int ts = TestPanel.TankSlot(roster, RosterEntry.SlotCount);
                    Assert.AreEqual(expectedSlot, ts, $"[Q7] {faction}'s tank slot");
                    Assert.IsTrue(ChassisKind.IsTank(roster[RosterEntry.SlotCount + ts].Chassis), $"[Q7] {faction}'s found slot is not a tank");
                }
                Check(FactionId.Iron, 6);
                Check(FactionId.Brass, 6);
                Check(FactionId.British, 7);
                Check(FactionId.German, 7);
                Check(FactionId.French, 7);
                Check(FactionId.AustroHungarian, 7);
            }
            finally { roster.Dispose(); }
        }
    }
}
