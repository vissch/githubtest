// Phase: tooling (2026-10-07) — the box for his words after F10: a small plate over the held match, a text field,
// SAVE and CLOSE. The capture is on disk before this comes up (FeedbackCapture; the router pushes this a frame after
// the picture), so whatever happens here it is kept. Modal: the router holds the sim and gameplay keys stand down, so
// typing moves no camera and deploys nobody. It does not dim the field or hide the HUD: he is writing about what he
// sees. Enter saves his words and closes, Shift+Enter is a new line, Esc closes without words, F10 again saves and
// closes (the router's). However it closes, the state file is written once more and the folder's "writing" mark is
// taken away, which is what lets the board take the capture in.
// His words are kept unless he says otherwise: only Esc and CLOSE leave them out. Every other way the box can go (the
// debrief coming up, a scene load, a screen under it popping the stack, the game quitting) writes what he had typed.
// While it is up the whole screen is the box's: a click beside the plate reaches no button of a menu under it.
using System;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.UIElements;
using TW.Presentation;

namespace TW.UI
{
    public sealed class FeedbackScreen : ShellScreen
    {
        public const string TreePath = "Shell/Feedback";
        public static readonly string[] RequiredNames = { "feedback-screen", "feedback-plate", "feedback-title", "feedback-said", "feedback-note", "btn-feedback-save", "btn-feedback-close", "feedback-hint" };
        public const string Said = "THE PICTURE AND THE STATE OF THE GAME ARE SAVED. SAY WHAT YOU SAW: IT GOES TO THE TASK BOARD WITH THEM.";
        public const string SavedWithWords = "SAVED TO THE TASK BOARD WITH YOUR WORDS", SavedWithout = "SAVED TO THE TASK BOARD WITHOUT WORDS", NotSaved = "YOUR WORDS COULD NOT BE SAVED: THE PICTURE AND THE STATE ARE KEPT";

        readonly FeedbackRecord record;
        readonly string folder;
        TextField note;
        bool finished, withoutWords;

        public FeedbackScreen(FeedbackRecord record, string folder) { this.record = record; this.folder = folder; }

        public string Folder => folder;
        public override bool HidesHud => false;
        public override VisualTreeAsset Tree(ShellAssets a) => Resources.Load<VisualTreeAsset>(TreePath);

        /// <summary>The capture's key closes the box only when it is no key he could be typing: bound to a letter, that
        /// letter belongs to his words while the box is up.</summary>
        public static bool ClosesTheBox(Key k) => k >= Key.F1 && k <= Key.F12;

        protected override void OnBind()
        {
            note = Root.Q<TextField>("feedback-note");
            InputFocus.Typing = true;
            Key again = KeyMap.Primary(GameAction.Feedback);
            SetText("feedback-title", "CAPTURED " + (record.when_local.Length >= 19 ? record.when_local.Substring(11) : record.when_local));
            SetText("feedback-said", Said);
            SetText("feedback-hint", $"ENTER  SAVE      SHIFT+ENTER  NEW LINE      {KeyMap.Display(KeyMap.Primary(GameAction.Menu))}  CLOSE WITHOUT WORDS" + (ClosesTheBox(again) ? $"      {KeyMap.Display(again)}  SAVE AND CLOSE" : ""));
            Btn("btn-feedback-save", () => Close(true));
            Btn("btn-feedback-close", () => Close(false));
            if (note == null) return;
            note.multiline = true;
            note.maxLength = FeedbackCapture.MaxNote;
            // before the field's own handling of the key: Enter is his "done", not a new line
            note.RegisterCallback<KeyDownEvent>(OnKey, TrickleDown.TrickleDown);
            note.schedule.Execute(() => note.Focus());   // once it is on the panel: he types at once
        }

        void OnKey(KeyDownEvent e) => OnKey(e.keyCode, e.character, e.shiftKey, e.StopImmediatePropagation);

        /// <summary>A key in the field. Enter comes as two events, the key and then its character: both stop here (stop is
        /// called for each). Left to itself the field puts a line break in on Enter and, on Shift+Enter, stops taking keys
        /// at all (seen in Play, 2026-10-07). Apart from the event, so a test presses the keys.</summary>
        public void OnKey(KeyCode code, char character, bool shift, Action stop)
        {
            bool key = code == KeyCode.Return || code == KeyCode.KeypadEnter;
            if (!key && character != '\n' && character != '\r') return;
            stop?.Invoke();
            if (!key) return;
            if (shift) NewLine(); else Close(true);
        }

        /// <summary>Shift+Enter: a line break where the cursor is (over what is selected, if anything is).</summary>
        public void NewLine()
        {
            if (note == null) return;
            string words = note.value ?? "";
            int a = Mathf.Clamp(Mathf.Min(note.cursorIndex, note.selectIndex), 0, words.Length), b = Mathf.Clamp(Mathf.Max(note.cursorIndex, note.selectIndex), 0, words.Length);
            if (words.Length - (b - a) + 1 > FeedbackCapture.MaxNote) return;
            note.value = words.Substring(0, a) + "\n" + words.Substring(b);
            note.SelectRange(a + 1, a + 1);
        }

        /// <summary>What he has typed so far (for the router's F10, and tests).</summary>
        public string Words { get => note != null ? note.value : ""; set { if (note != null) note.value = value; } }

        /// <summary>Close the box: with his words in the capture, or (Esc, CLOSE) on purpose without. Through the router
        /// when there is one, so the hold goes with the screen.</summary>
        public void Close(bool withWords)
        {
            if (finished) return;
            withoutWords = !withWords;
            if (Router != null) Router.Remove(this); else Unbind();
        }

        /// <summary>Esc: his "never mind". The one key that leaves his words out.</summary>
        public override void OnEscape() => Close(false);

        /// <summary>The game is going away under the box (it quits, or Play ends): what he has typed is kept and the
        /// capture is closed, or the board would wait half an hour for words that cannot come.</summary>
        public void Abandon()
        {
            if (finished) return;
            Unbind();
        }

        protected override void OnUnbind()
        {
            // however it went away (his key, a button, the debrief or a scene load clearing the stack, a screen under it
            // popping): the capture is closed once, and with what he had typed unless he closed it without on purpose
            if (finished) return;
            finished = true;
            InputFocus.Typing = false;
            if (!withoutWords) record.note = FeedbackCapture.Clip(Words);
            bool wrote = true;
            try { FeedbackCapture.Save(record, folder); }
            catch (Exception e) { wrote = false; Debug.LogWarning($"FeedbackScreen: could not write his words into {folder}: {e.Message}"); }
            FeedbackCapture.Done(folder);
            Router?.Notice(!wrote ? NotSaved : record.note.Length > 0 ? SavedWithWords : SavedWithout);
        }
    }
}
