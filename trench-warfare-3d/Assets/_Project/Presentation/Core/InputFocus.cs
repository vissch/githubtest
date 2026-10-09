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
        /// <summary>He is typing words into a box (the feedback capture's): no key is a command, the debug overlays' and
        /// the HUD toggle's included, which read the keyboard raw and so ignore Modal.</summary>
        public static bool Typing;
        static int escapeFrame = -1, captureFrame = -1;

        /// <summary>Gameplay may read input this frame.</summary>
        public static bool Gameplay => !Modal && !Listening;

        /// <summary>Claim this frame's Escape press so no other Escape handler acts on it.</summary>
        public static void ConsumeEscape() => escapeFrame = Time.frameCount;
        public static bool EscapeConsumed => escapeFrame == Time.frameCount;

        /// <summary>Claim this frame's key press for the rebind capture it ended: it is the key to bind, so no handler that
        /// reads the keyboard raw (the feedback capture's F10) acts on it too. Listening is already false by then: the
        /// capture lets go of the keyboard in the input callback, before any Update runs.</summary>
        public static void ConsumeCapture() => captureFrame = Time.frameCount;
        public static bool CaptureConsumed => captureFrame == Time.frameCount;

        /// <summary>Reset on scene load so a stale Modal from a destroyed screen cannot dead-lock the field.</summary>
        public static void Reset() { Modal = false; Listening = false; Typing = false; escapeFrame = -1; captureFrame = -1; }
    }
}
