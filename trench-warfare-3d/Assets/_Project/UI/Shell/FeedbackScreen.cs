// Phase: tooling (2026-10-07) — the box for his words after F10: a small plate over the held match, a text field,
// SAVE and CLOSE. The capture is on disk before this comes up (FeedbackCapture; the router pushes this a frame after
// the picture), so whatever happens here it is kept. Modal: the router holds the sim and gameplay keys stand down, so
// typing moves no camera and deploys nobody. It does not dim the field or hide the HUD: he is writing about what he
// sees. Enter saves his words and closes, Shift+Enter is a new line, Esc closes without words, F10 again saves and
// closes (the router's). However it closes, the state file is written once more and the folder's "writing" mark is
// taken away, which is what lets the board take the capture in.
using System;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;

namespace TW.UI
{
    public sealed class FeedbackScreen : ShellScreen
    {
        public const string TreePath = "Shell/Feedback";
        public static readonly string[] RequiredNames = { "feedback-screen", "feedback-plate", "feedback-title", "feedback-said", "feedback-note", "btn-feedback-save", "btn-feedback-close", "feedback-hint" };
        public const string Said = "THE PICTURE AND THE STATE OF THE GAME ARE SAVED. SAY WHAT YOU SAW: IT GOES TO THE TASK BOARD WITH THEM.";

        readonly FeedbackRecord record;
        readonly string folder;
        TextField note;
        bool finished;

        public FeedbackScreen(FeedbackRecord record, string folder) { this.record = record; this.folder = folder; }

        public string Folder => folder;
        public override bool HidesHud => false;
        public override VisualTreeAsset Tree(ShellAssets a) => Resources.Load<VisualTreeAsset>(TreePath);

        protected override void OnBind()
        {
            note = Root.Q<TextField>("feedback-note");
            SetText("feedback-title", "CAPTURED " + (record.when_local.Length >= 19 ? record.when_local.Substring(11) : record.when_local));
            SetText("feedback-said", Said);
            SetText("feedback-hint", $"ENTER  SAVE      SHIFT+ENTER  NEW LINE      {KeyMap.Display(KeyMap.Primary(GameAction.Menu))}  CLOSE WITHOUT WORDS      {KeyMap.Display(KeyMap.Primary(GameAction.Feedback))}  SAVE AND CLOSE");
            Btn("btn-feedback-save", () => Close(true));
            Btn("btn-feedback-close", () => Close(false));
            if (note == null) return;
            note.multiline = true;
            note.maxLength = FeedbackCapture.MaxNote;
            // before the field's own handling of the key: Enter is his "done", not a new line
            note.RegisterCallback<KeyDownEvent>(OnKey, TrickleDown.TrickleDown);
            note.schedule.Execute(() => note.Focus());   // once it is on the panel: he types at once
        }

        void OnKey(KeyDownEvent e)
        {
            // Enter comes as two events, the key and then its character: both stop here. Left to itself the field puts a
            // line break in on Enter and, on Shift+Enter, stops taking keys at all (seen in Play, 2026-10-07).
            bool key = e.keyCode == KeyCode.Return || e.keyCode == KeyCode.KeypadEnter;
            if (!key && e.character != '\n' && e.character != '\r') return;
            e.StopImmediatePropagation();
            if (!key) return;
            if (e.shiftKey) NewLine(); else Close(true);
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

        /// <summary>Close the box: with his words in the capture, or without. Through the router when there is one, so the
        /// hold goes with the screen.</summary>
        public void Close(bool withWords)
        {
            if (finished) return;
            if (withWords) record.note = FeedbackCapture.Clip(Words);
            if (Router != null) Router.Pop(); else Unbind();
        }

        /// <summary>The game is going away under the box (it quits, or Play ends): what he has typed is kept and the
        /// capture is closed, or the board would wait half an hour for words that cannot come.</summary>
        public void Abandon()
        {
            if (finished) return;
            record.note = FeedbackCapture.Clip(Words);
            Unbind();
        }

        protected override void OnUnbind()
        {
            // however it went away (his key, a button, a scene load that cleared the stack): the capture is closed once
            if (finished) return;
            finished = true;
            try { FeedbackCapture.Save(record, folder); }
            catch (Exception e) { Debug.LogWarning($"FeedbackScreen: could not write his words into {folder}: {e.Message}"); }
            FeedbackCapture.Done(folder);
        }
    }
}
