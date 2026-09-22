// Phase: B6 (implemented) — who owns the keyboard and mouse this frame: the field, or a screen over it.
// Every gameplay input site polls Keyboard.current / Mouse.current directly (TacticalCamera, DebugOverlay,
// TestPanel, the Toolkit HUD's hotkeys), so there is no event system to swallow input when a menu is up. This
// static is the one switch they all consult. Escape is shared three ways (cancel an armed ability, close the
// menu, cancel a key capture), so whoever consumes it stamps the frame and the others stand down.
using UnityEngine;

namespace TW.Presentation
{
    public static class InputFocus
    {
        /// <summary>A shell screen (pause menu, settings, debrief) is up: gameplay polling ignores keys and clicks.</summary>
        public static bool Modal;
        /// <summary>A key-rebind capture is waiting for a key: even Space and Esc belong to it.</summary>
        public static bool Listening;
        static int escapeFrame = -1;

        /// <summary>Gameplay may read input this frame.</summary>
        public static bool Gameplay => !Modal && !Listening;

        /// <summary>Claim this frame's Escape press so no other Escape handler acts on it.</summary>
        public static void ConsumeEscape() => escapeFrame = Time.frameCount;
        public static bool EscapeConsumed => escapeFrame == Time.frameCount;

        /// <summary>Reset on scene load so a stale Modal from a destroyed screen cannot dead-lock the field.</summary>
        public static void Reset() { Modal = false; Listening = false; escapeFrame = -1; }
    }
}
