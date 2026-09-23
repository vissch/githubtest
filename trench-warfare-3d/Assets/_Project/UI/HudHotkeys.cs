// Phase: B6 (implemented) — the keyboard side of the battle HUD, through KeyMap.
// While DebugOverlay still handles 1-5, Space, Backspace and L on its own, this only takes the keys nobody else reads
// (slots 6-8, arming the support cards, over the top on G, hold fire on F, speed, the HUD toggle); LegacyOverlayActive
// goes false when DebugOverlay is gated behind HudBridge (step A5 of the UI plan) and the whole map is handled here.
using UnityEngine;
using UnityEngine.InputSystem;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;

namespace TW.UI
{
    public sealed class HudHotkeys
    {
        /// <summary>DebugOverlay still polls 1-5, Space, Backspace and L itself; do not double-fire them.</summary>
        public static bool LegacyOverlayActive = true;

        readonly HudController hud;
        public HudHotkeys(HudController hud) { this.hud = hud; }

        public void Update()
        {
            if (Keyboard.current == null) return;
            if (KeyMap.DownRaw(GameAction.HudToggle)) { HudBridge.UseToolkitHud = !HudBridge.UseToolkitHud; hud.ApplyFlag(); }
            if (!InputFocus.Gameplay || !hud.Interactive) return;
            int first = LegacyOverlayActive ? 5 : 0;
            for (int s = first; s < RosterEntry.SlotCount; s++)
                if (KeyMap.Down((GameAction)((int)GameAction.Deploy1 + s))) hud.Deploy(s);
            if (KeyMap.Down(GameAction.ArmBarrage)) hud.ToggleArm(OffMapAbilityId.HeBarrage);
            if (KeyMap.Down(GameAction.ArmGas)) hud.ToggleArm(OffMapAbilityId.ChlorineGas);
            if (KeyMap.Down(GameAction.Advance)) hud.OrderFront(CommandType.TrenchAdvance);
            if (KeyMap.Down(GameAction.HoldFire)) hud.ToggleFront(CommandType.TrenchHoldFire);
            if (KeyMap.Down(GameAction.SpeedDown)) hud.Clock?.StepSpeed(-1);
            if (KeyMap.Down(GameAction.SpeedUp)) hud.Clock?.StepSpeed(+1);
            if (!LegacyOverlayActive)
            {
                if (KeyMap.Down(GameAction.TacticalPause)) hud.Clock?.Toggle(MatchClock.Hold.Tactical);
                if (KeyMap.Down(GameAction.Fallback)) hud.OrderFront(CommandType.TrenchFallback);
                if (KeyMap.Down(GameAction.LockTrench)) hud.ToggleFront(CommandType.TrenchLock);
            }
        }
    }
}
