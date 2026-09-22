// Phase: B6 (implemented) — every key the player can press, by what it does, rebindable.
// The game polls Keyboard.current directly (TacticalCamera, DebugOverlay, TestPanel, the Toolkit HUD): three
// sites in one assembly, on one platform, so an InputActionAsset would be a migration for the same settings
// screen. This table is the one place a binding lives. Held/Down answer false while a shell screen has the input
// (InputFocus), so a menu never has to swallow keys it did not ask for. Dust Front's conventions are the
// defaults: Space is the tactical pause, control keys are single letters, Esc is the menu and is not rebindable.
using System;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.InputSystem.Controls;
using UnityEngine.InputSystem.Utilities;

namespace TW.Presentation
{
    public enum GameAction
    {
        PanUp, PanDown, PanLeft, PanRight, RotateLeft, RotateRight, ResetView, SuperZoom,
        TacticalPause, Menu,
        Deploy1, Deploy2, Deploy3, Deploy4, Deploy5, Deploy6, Deploy7, Deploy8,
        ArmBarrage, ArmGas,
        Advance, Fallback, LockTrench, HoldFire,
        SpeedDown, SpeedUp,
        DebugFlowField, DebugStats, DebugNextGoal, DebugCapsules, DebugPanel, HudToggle,
    }

    public static class KeyMap
    {
        public static readonly int ActionCount = Enum.GetValues(typeof(GameAction)).Length;

        [Serializable]
        public sealed class Bindings
        {
            public Key[] Primary = new Key[KeyMap.ActionCount];
            public Key[] Secondary = new Key[KeyMap.ActionCount];

            /// <summary>Arrays sized for this build's action list even if the JSON came from an older one.</summary>
            public void Normalise()
            {
                if (Primary == null || Primary.Length != KeyMap.ActionCount) Primary = Resize(Primary);
                if (Secondary == null || Secondary.Length != KeyMap.ActionCount) Secondary = Resize(Secondary);
            }

            static Key[] Resize(Key[] a)
            {
                var r = new Key[KeyMap.ActionCount];
                if (a != null) Array.Copy(a, r, Math.Min(a.Length, r.Length));
                return r;
            }

            public Bindings Clone()
            {
                var b = new Bindings();
                Array.Copy(Primary, b.Primary, KeyMap.ActionCount);
                Array.Copy(Secondary, b.Secondary, KeyMap.ActionCount);
                return b;
            }
        }

        public static Bindings Current = Defaults();

        public static Bindings Defaults()
        {
            var b = new Bindings();
            void D(GameAction a, Key p, Key s = Key.None) { b.Primary[(int)a] = p; b.Secondary[(int)a] = s; }
            D(GameAction.PanUp, Key.W, Key.UpArrow);
            D(GameAction.PanDown, Key.S, Key.DownArrow);
            D(GameAction.PanLeft, Key.A, Key.LeftArrow);
            D(GameAction.PanRight, Key.D, Key.RightArrow);
            D(GameAction.RotateLeft, Key.Q);
            D(GameAction.RotateRight, Key.E);
            D(GameAction.ResetView, Key.Home);
            D(GameAction.SuperZoom, Key.Z);
            D(GameAction.TacticalPause, Key.Space);
            D(GameAction.Menu, Key.Escape);
            D(GameAction.Deploy1, Key.Digit1); D(GameAction.Deploy2, Key.Digit2); D(GameAction.Deploy3, Key.Digit3);
            D(GameAction.Deploy4, Key.Digit4); D(GameAction.Deploy5, Key.Digit5); D(GameAction.Deploy6, Key.Digit6);
            D(GameAction.Deploy7, Key.Digit7); D(GameAction.Deploy8, Key.Digit8);
            D(GameAction.ArmBarrage, Key.Digit9);
            D(GameAction.ArmGas, Key.Digit0);
            D(GameAction.Advance, Key.G);
            D(GameAction.Fallback, Key.Backspace);
            D(GameAction.LockTrench, Key.L);
            D(GameAction.HoldFire, Key.F);
            D(GameAction.SpeedDown, Key.Minus);
            D(GameAction.SpeedUp, Key.Equals);
            D(GameAction.DebugFlowField, Key.F1);
            D(GameAction.DebugStats, Key.F2);
            D(GameAction.DebugNextGoal, Key.F3);
            D(GameAction.DebugCapsules, Key.F4);
            D(GameAction.DebugPanel, Key.F10);
            D(GameAction.HudToggle, Key.F9);
            return b;
        }

        /// <summary>Actions the settings screen lets the player change. Menu (Esc) is fixed.</summary>
        public static bool Rebindable(GameAction a) => a != GameAction.Menu;

        public static Key Primary(GameAction a) => Current.Primary[(int)a];
        public static Key Secondary(GameAction a) => Current.Secondary[(int)a];

        public static void Set(GameAction a, Key k, bool secondary = false)
        {
            if (secondary) Current.Secondary[(int)a] = k; else Current.Primary[(int)a] = k;
        }

        /// <summary>The action a key is bound to (primary or secondary), or null.</summary>
        public static GameAction? BoundTo(Key k, GameAction? except = null)
        {
            if (k == Key.None) return null;
            for (int i = 0; i < ActionCount; i++)
            {
                if (except.HasValue && (int)except.Value == i) continue;
                if (Current.Primary[i] == k || Current.Secondary[i] == k) return (GameAction)i;
            }
            return null;
        }

        // ---- polling -------------------------------------------------------------------------------------------

        /// <summary>The key is down and the field owns the input this frame.</summary>
        public static bool Held(GameAction a) => InputFocus.Gameplay && HeldRaw(a);
        /// <summary>The key went down this frame and the field owns the input.</summary>
        public static bool Down(GameAction a) => InputFocus.Gameplay && DownRaw(a);

        /// <summary>Ignores InputFocus: for the shell's own Esc and for tests.</summary>
        public static bool HeldRaw(GameAction a)
        {
            var kb = Keyboard.current;
            if (kb == null) return false;
            var p = Current.Primary[(int)a]; var s = Current.Secondary[(int)a];
            return (p != Key.None && kb[p].isPressed) || (s != Key.None && kb[s].isPressed);
        }

        public static bool DownRaw(GameAction a)
        {
            var kb = Keyboard.current;
            if (kb == null) return false;
            var p = Current.Primary[(int)a]; var s = Current.Secondary[(int)a];
            return (p != Key.None && kb[p].wasPressedThisFrame) || (s != Key.None && kb[s].wasPressedThisFrame);
        }

        // ---- text ----------------------------------------------------------------------------------------------

        /// <summary>The key's name for a key-cap: "W", "SPACE", "F1", "LEFT", "-". Uppercase: USS has no text-transform.</summary>
        public static string Display(Key k)
        {
            switch (k)
            {
                case Key.None: return "";
                case Key.Space: return "SPACE";
                case Key.Escape: return "ESC";
                case Key.Enter: return "ENTER";
                case Key.Backspace: return "BACKSPACE";
                case Key.Tab: return "TAB";
                case Key.Home: return "HOME";
                case Key.End: return "END";
                case Key.Delete: return "DEL";
                case Key.Insert: return "INS";
                case Key.PageUp: return "PG UP";
                case Key.PageDown: return "PG DN";
                case Key.UpArrow: return "UP";
                case Key.DownArrow: return "DOWN";
                case Key.LeftArrow: return "LEFT";
                case Key.RightArrow: return "RIGHT";
                case Key.LeftShift: return "L SHIFT";
                case Key.RightShift: return "R SHIFT";
                case Key.LeftCtrl: return "L CTRL";
                case Key.RightCtrl: return "R CTRL";
                case Key.LeftAlt: return "L ALT";
                case Key.RightAlt: return "R ALT";
                case Key.Minus: return "-";
                case Key.Equals: return "=";
                case Key.Comma: return ",";
                case Key.Period: return ".";
                case Key.Slash: return "/";
                case Key.Backslash: return "\\";
                case Key.Semicolon: return ";";
                case Key.Quote: return "'";
                case Key.LeftBracket: return "[";
                case Key.RightBracket: return "]";
                case Key.Backquote: return "`";
            }
            string n = k.ToString();
            if (n.StartsWith("Digit")) return n.Substring(5);
            if (n.StartsWith("Numpad")) return "NUM " + n.Substring(6).ToUpperInvariant();
            return n.ToUpperInvariant();
        }

        /// <summary>The action's label for a settings row.</summary>
        public static string Label(GameAction a)
        {
            switch (a)
            {
                case GameAction.PanUp: return "PAN UP";
                case GameAction.PanDown: return "PAN DOWN";
                case GameAction.PanLeft: return "PAN LEFT";
                case GameAction.PanRight: return "PAN RIGHT";
                case GameAction.RotateLeft: return "TURN LEFT";
                case GameAction.RotateRight: return "TURN RIGHT";
                case GameAction.ResetView: return "RESET VIEW";
                case GameAction.SuperZoom: return "SUPER ZOOM";
                case GameAction.TacticalPause: return "TACTICAL PAUSE";
                case GameAction.Menu: return "MENU";
                case GameAction.ArmBarrage: return "ARM HE BARRAGE";
                case GameAction.ArmGas: return "ARM CHLORINE";
                case GameAction.Advance: return "OVER THE TOP";
                case GameAction.Fallback: return "FALL BACK";
                case GameAction.LockTrench: return "LOCK TRENCH";
                case GameAction.HoldFire: return "HOLD FIRE";
                case GameAction.SpeedDown: return "SLOWER";
                case GameAction.SpeedUp: return "FASTER";
                case GameAction.DebugFlowField: return "DEBUG: FLOW FIELD";
                case GameAction.DebugStats: return "DEBUG: STATS";
                case GameAction.DebugNextGoal: return "DEBUG: NEXT GOAL";
                case GameAction.DebugCapsules: return "DEBUG: CAPSULES";
                case GameAction.DebugPanel: return "DEBUG PANEL";
                case GameAction.HudToggle: return "OLD / NEW HUD";
            }
            string n = a.ToString();
            return n.StartsWith("Deploy") ? "DEPLOY SLOT " + n.Substring(6) : n.ToUpperInvariant();
        }

        // ---- capture -------------------------------------------------------------------------------------------

        /// <summary>
        /// Wait for the next keyboard key: onKey gets it, Escape calls onCancel instead. Mouse buttons are
        /// ignored (a click on the key-cap started the capture). Dispose to stop early. InputFocus.Listening is
        /// held for the duration so Space cannot toggle the pause and Esc cannot open the menu meanwhile.
        /// </summary>
        public static IDisposable Listen(Action<Key> onKey, Action onCancel)
        {
            InputFocus.Listening = true;
            IDisposable sub = null; bool done = false;
            void Finish() { if (done) return; done = true; InputFocus.Listening = false; sub?.Dispose(); }
            sub = InputSystem.onAnyButtonPress.Call(ctrl =>
            {
                if (done || !(ctrl is KeyControl kc)) return;
                Finish();
                if (kc.keyCode == Key.Escape) { InputFocus.ConsumeEscape(); onCancel?.Invoke(); }
                else onKey?.Invoke(kc.keyCode);
            });
            return new Stopper(Finish);
        }

        sealed class Stopper : IDisposable
        {
            readonly Action stop;
            public Stopper(Action s) { stop = s; }
            public void Dispose() => stop();
        }
    }
}
