// Phase: review-fix (rv-37-p2-u16, 2026-10-13) — code review 2026-10-04, unit 16: Q1 (TestPanel.cs:262), Q4
// (TacticalCamera.cs:156,171), Q5 (TestPanel.cs:59-67).
// rv-74-p3-u16, 2026-10-15: Q6 (TestPanel.cs:81), Q7 (TestPanel.cs:343), Q8 (DebugOverlay.cs:61,69-72),
// Q9 (BattleHud.cs:176-263,493-499; DebugOverlay.cs:50-51).
using System.Reflection;
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

        /// <summary>[Q8] ShowFlowField on a fresh DebugOverlay must be off: it used to default to true, walking
        /// 625 cells a frame for Debug.DrawLine calls a player build never shows.</summary>
        [Test]
        public void FlowFieldIsOffByDefault()
        {
            var go = new GameObject("DebugOverlayQ8");
            try
            {
                var overlay = go.AddComponent<DebugOverlay>();
                Assert.IsFalse(overlay.ShowFlowField, "[Q8] ShowFlowField defaulted to true");
            }
            finally { Object.DestroyImmediate(go); }
        }

        /// <summary>[Q8] DrawsFlowField only says yes when the toggle is on AND the build is the editor: a player
        /// build never shows the gizmo lines, so walking the cells for it there is pure waste.</summary>
        [Test]
        public void DrawsFlowFieldOnlyInTheEditorWithTheToggleOn()
        {
            Assert.IsFalse(DebugOverlay.DrawsFlowField(false, true), "[Q8] toggle off: must not draw");
            Assert.IsFalse(DebugOverlay.DrawsFlowField(true, false), "[Q8] a player build: must not draw");
            Assert.IsTrue(DebugOverlay.DrawsFlowField(true, true), "[Q8] editor with the toggle on: must draw");
        }

        /// <summary>[Q8] OffersCapsuleSwap mirrors TestPanel.OfferedIn: F4 used to swap the rendered men for
        /// capsules even in a plain shipped build, which has neither flag set.</summary>
        [Test]
        public void CapsuleSwapIsOfferedOnlyInEditorOrDevelopmentBuilds()
        {
            Assert.IsFalse(DebugOverlay.OffersCapsuleSwap(false, false), "[Q8] a shipped build must not offer it");
            Assert.IsTrue(DebugOverlay.OffersCapsuleSwap(true, false), "[Q8] the editor must offer it");
            Assert.IsTrue(DebugOverlay.OffersCapsuleSwap(false, true), "[Q8] a development build must offer it");
        }

        /// <summary>[Q8] The four debug keys used to read Keyboard.current directly, so they fired under a shell
        /// screen (InputFocus.Modal) and ignored any rebinding. Reading them through KeyMap.Down ties them to
        /// InputFocus.Gameplay like every other action.</summary>
        [Test]
        public void DebugKeysAreDeadUnderAModalScreen()
        {
            bool was = InputFocus.Modal;
            try
            {
                InputFocus.Modal = true;
                Assert.IsFalse(KeyMap.Down(GameAction.DebugFlowField), "[Q8] DebugFlowField read under a modal screen");
                Assert.IsFalse(KeyMap.Down(GameAction.DebugStats), "[Q8] DebugStats read under a modal screen");
                Assert.IsFalse(KeyMap.Down(GameAction.DebugNextGoal), "[Q8] DebugNextGoal read under a modal screen");
                Assert.IsFalse(KeyMap.Down(GameAction.DebugCapsules), "[Q8] DebugCapsules read under a modal screen");
            }
            finally { InputFocus.Modal = was; }
        }

        /// <summary>[Q9] BattleHud used to build ~35 HideAndDontSave textures in Start and never free them: a
        /// restart leaked the lot. Start now only looks up Panel; EnsureArt (first OnGUI) builds the art, and
        /// OnDestroy frees every texture it built.</summary>
        [Test]
        public void BattleHudBuildsItsArtLazilyAndFreesItOnDestroy()
        {
            var go = new GameObject("BattleHudQ9");
            try
            {
                var hud = go.AddComponent<BattleHud>();
                var t = typeof(BattleHud);
                var flags = BindingFlags.NonPublic | BindingFlags.Instance;
                string[] textureFields = { "sideTex", "stripTex", "coin", "lockClosed", "lockOpen", "lockGrey",
                    "advance", "fallback", "fire", "fireHeld", "pause", "barrage", "gas", "drop" };

                t.GetMethod("Start", flags).Invoke(hud, null);
                foreach (var name in textureFields)
                    Assert.IsNull(t.GetField(name, flags).GetValue(hud), $"[Q9] Start built {name} before EnsureArt ran");
                Assert.IsNull(t.GetField("unitIcons", flags).GetValue(hud), "[Q9] Start built unitIcons before EnsureArt ran");

                t.GetMethod("EnsureArt", flags).Invoke(hud, null);
                foreach (var name in textureFields)
                    Assert.IsNotNull(t.GetField(name, flags).GetValue(hud), $"[Q9] EnsureArt left {name} unbuilt");
                Assert.IsNotNull(t.GetField("unitIcons", flags).GetValue(hud), "[Q9] EnsureArt left unitIcons unbuilt");

                t.GetMethod("OnDestroy", flags).Invoke(hud, null);
                foreach (var name in textureFields)
                    Assert.IsNull(t.GetField(name, flags).GetValue(hud), $"[Q9] OnDestroy left {name} behind");
                Assert.IsNull(t.GetField("unitIcons", flags).GetValue(hud), "[Q9] OnDestroy left unitIcons behind");
            }
            finally { Object.DestroyImmediate(go); }
        }

        /// <summary>[Q9] DebugOverlay's two instanced materials were built in Start with nothing to free them;
        /// OnDestroy now discards both.</summary>
        [Test]
        public void DebugOverlayFreesItsMaterialsOnDestroy()
        {
            var go = new GameObject("DebugOverlayQ9");
            try
            {
                var overlay = go.AddComponent<DebugOverlay>();
                var t = typeof(DebugOverlay);
                var flags = BindingFlags.NonPublic | BindingFlags.Instance;
                var shader = Shader.Find("Universal Render Pipeline/Lit") ?? Shader.Find("Standard");
                var matA = new Material(shader);
                var matB = new Material(shader);
                t.GetField("matA", flags).SetValue(overlay, matA);
                t.GetField("matB", flags).SetValue(overlay, matB);

                t.GetMethod("OnDestroy", flags).Invoke(overlay, null);

                Assert.IsNull(t.GetField("matA", flags).GetValue(overlay), "[Q9] matA survived OnDestroy");
                Assert.IsNull(t.GetField("matB", flags).GetValue(overlay), "[Q9] matB survived OnDestroy");
            }
            finally { Object.DestroyImmediate(go); }
        }
    }
}
