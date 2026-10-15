// Phase: B6 (implemented) — the keyboard side of the battle HUD, through KeyMap.
// This handles the whole key map while the Toolkit HUD is the HUD: the ten deploy slots, arming the support cards,
// over the top on G, hold fire on F, the tactical pause, fall back, lock, speed and the HUD toggle. DebugOverlay's own
// poll of 1-5 / Space / Backspace / L is gated behind HudBridge.UseToolkitHud, so nothing here double-fires.
using UnityEngine;
using UnityEngine.InputSystem;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;

namespace TW.UI
{
    public sealed class HudHotkeys
    {
        readonly HudController hud;
        public HudHotkeys(HudController hud) { this.hud = hud; }

        public void Update()
        {
            if (Keyboard.current == null) return;
            // [I12] KeyMap.Down, not DownRaw: F9 must not toggle the HUD from a menu or while a rebind is capturing a key
            if (KeyMap.Down(GameAction.HudToggle)) { HudBridge.UseToolkitHud = !HudBridge.UseToolkitHud; hud.ApplyFlag(); }
            if (!InputFocus.Gameplay || !hud.Interactive) return;
            // with Ctrl or Shift held the digits are control-group keys (SelectionController), not deploys
            if (!SelectionController.GroupModifierHeld())
                for (int s = 0; s < RosterEntry.SlotCount; s++)
                    if (KeyMap.Down((GameAction)((int)GameAction.Deploy1 + s))) hud.Deploy(s);
            // support: F5-F7 arm the barrage, the gas and the paratroopers (the digit row is ten deploy slots), C M V B the
            // four line abilities (docs/21 phase 5); none of them is a group key, so no modifier applies
            if (KeyMap.Down(GameAction.ArmBarrage)) hud.ToggleArm(OffMapAbilityId.HeBarrage);
            if (KeyMap.Down(GameAction.ArmGas)) hud.ToggleArm(OffMapAbilityId.ChlorineGas);
            if (KeyMap.Down(GameAction.ArmDrop)) hud.ToggleArm(OffMapAbilityId.ParaDrop);
            if (KeyMap.Down(GameAction.ArmCreeping)) hud.ToggleArm(OffMapAbilityId.CreepingBarrage);
            if (KeyMap.Down(GameAction.ArmSmoke)) hud.ToggleArm(OffMapAbilityId.SmokeScreen);
            if (KeyMap.Down(GameAction.ArmStrafe)) hud.ToggleArm(OffMapAbilityId.StrafeRun);
            if (KeyMap.Down(GameAction.ArmBeam)) hud.ToggleArm(OffMapAbilityId.Beam);
            if (KeyMap.Down(GameAction.Advance)) hud.OrderFront(CommandType.TrenchAdvance);
            if (KeyMap.Down(GameAction.HoldFire)) hud.ToggleFront(CommandType.TrenchHoldFire);
            if (KeyMap.Down(GameAction.SpeedDown)) hud.Clock?.StepSpeed(-1);
            if (KeyMap.Down(GameAction.SpeedUp)) hud.Clock?.StepSpeed(+1);
            if (KeyMap.Down(GameAction.TacticalPause)) hud.Clock?.Toggle(MatchClock.Hold.Tactical);
            if (KeyMap.Down(GameAction.Fallback)) hud.OrderFront(CommandType.TrenchFallback);
            if (KeyMap.Down(GameAction.LockTrench)) hud.ToggleFront(CommandType.TrenchLock);
        }
    }
}
