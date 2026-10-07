// Phase: tooling (2026-10-07) — the feedback capture (F10) writes what the asset board reads, where it looks, and his
// words reach it. The format is one file both sides test against (Tools/assetboard/feedback.example.json: the board's
// test_tasks.py reads it too): every key of the example must be a key the game writes, so a field renamed here is a
// red test here and not a task the board lists with nothing in it. Then the folder: a capture gets one of its own even
// in the same second, the state file is whole, the "writing" mark is there until his words are in. And the box: its
// UXML has what the screen asks for, and closing it, with words or without, writes the file and takes the mark away.
using System;
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;
using UnityEditor;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.UI;

namespace TW.Tests
{
    public class FeedbackCaptureTests
    {
        const string Folder = "Assets/_Project/UI/Resources/Shell/";
        string root;

        [SetUp] public void SetUp() { root = Path.Combine(Application.temporaryCachePath, "feedback-test-" + Guid.NewGuid().ToString("N")); }
        [TearDown] public void TearDown() { if (Directory.Exists(root)) Directory.Delete(root, true); }

        static FeedbackRecord Filled()
        {
            var r = FeedbackCapture.Gather(null, null, null, null, new DateTime(2026, 10, 7, 21, 14, 3));
            r.note = "The tank drove through the wire.";
            r.in_match = true;
            r.match.tick = 5400;
            r.match.units = new[] { new FeedbackRecord.Units { team = 0, archetype = 4, count = 2 } };
            return r;
        }

        // ---- the format ---------------------------------------------------------------------------------------------
        /// <summary>Every key of a JSON text as a path: "match.request.Title", and "match.units[].team" for what is in an
        /// array. Enough of a reader for what JsonUtility and the example hold: objects, arrays, strings, numbers, words.</summary>
        static HashSet<string> Keys(string json)
        {
            var found = new HashSet<string>();
            int at = 0;
            void Space() { while (at < json.Length && char.IsWhiteSpace(json[at])) at++; }
            string Str()
            {
                var sb = new System.Text.StringBuilder();
                at++;   // the opening quote
                while (json[at] != '"') { if (json[at] == '\\') at++; sb.Append(json[at]); at++; }
                at++;
                return sb.ToString();
            }
            void Value(string path)
            {
                Space();
                char c = json[at];
                if (c == '{')
                {
                    at++; Space();
                    while (json[at] != '}')
                    {
                        Space();
                        string key = Str(), child = path.Length == 0 ? key : path + "." + key;
                        found.Add(child);
                        Space(); at++;   // the colon
                        Value(child);
                        Space();
                        if (json[at] == ',') at++;
                        Space();
                    }
                    at++;
                }
                else if (c == '[')
                {
                    at++; Space();
                    while (json[at] != ']') { Value(path + "[]"); Space(); if (json[at] == ',') at++; Space(); }
                    at++;
                }
                else if (c == '"') Str();
                else while (at < json.Length && ",}] \r\n\t".IndexOf(json[at]) < 0) at++;
            }
            Value("");
            return found;
        }

        [Test]
        public void TheRecordHasEveryKeyOfTheExampleTheBoardReads()
        {
            string example = Path.Combine(Path.GetDirectoryName(Application.dataPath) ?? "", "Tools", "assetboard", "feedback.example.json");
            Assert.That(File.Exists(example), $"{example} is the format the board and the game share; it is gone");
            var asked = Keys(File.ReadAllText(example));
            var written = Keys(JsonUtility.ToJson(Filled(), true));
            Assert.That(asked.Count, Is.GreaterThan(40), "the example was not read: a test of nothing would pass");
            var missing = new List<string>();
            foreach (var k in asked) if (!written.Contains(k)) missing.Add(k);
            Assert.That(missing, Is.Empty, "the example has keys the game does not write: the board would read nothing there");
            Assert.That(written, Does.Contain("match.units[].count").And.Contain("match.request.BattlefieldSeed").And.Contain("view.position.x"));
        }

        [Test]
        public void TheSchemaAndTheNamesAreTheOnesTheBoardLooksFor()
        {
            Assert.That(FeedbackCapture.Schema, Is.EqualTo("tw-feedback/1"));
            Assert.That(FeedbackCapture.FileName, Is.EqualTo("capture.json"));
            Assert.That(FeedbackCapture.ShotName, Is.EqualTo("shot.png"));
            Assert.That(FeedbackCapture.OpenName, Is.EqualTo("writing"));
            Assert.That(new FeedbackRecord().schema, Is.EqualTo(FeedbackCapture.Schema));
        }

        [Test]
        public void OnAMenuTheRecordSaysNoMatchAndStillSaysWhichGame()
        {
            var r = FeedbackCapture.Gather(null, null, null, null, new DateTime(2026, 10, 7, 21, 14, 3));
            Assert.That(r.in_match, Is.False);
            Assert.That(r.id, Is.EqualTo("2026-10-07-211403"));
            Assert.That(r.when_local, Is.EqualTo("2026-10-07 21:14:03"));
            Assert.That(r.build.unity, Is.Not.Empty);
            Assert.That(r.build.editor, Is.True);
            Assert.That(Directory.Exists(Path.Combine(r.build.project, "Assets")), "the project's folder is what the board reads the branch from");
            Assert.That(r.settings, Is.Not.Null);
        }

        // ---- the folder ---------------------------------------------------------------------------------------------
        [Test]
        public void ACaptureIsAFolderOfItsOwnEvenInTheSameSecond()
        {
            var a = Filled(); var b = Filled();
            string first = FeedbackCapture.Write(a, root), second = FeedbackCapture.Write(b, root);
            Assert.That(second, Is.Not.EqualTo(first));
            Assert.That(Path.GetFileName(first), Is.EqualTo("2026-10-07-211403"));
            Assert.That(Path.GetFileName(second), Is.EqualTo("2026-10-07-211403-2"));
            Assert.That(b.id, Is.EqualTo("2026-10-07-211403-2"), "the record names the folder it is in");
            Assert.That(FeedbackCapture.Read(second).id, Is.EqualTo(b.id));
            Assert.That(FeedbackCapture.Read(first).note, Is.EqualTo("The tank drove through the wire."));
            Assert.That(Directory.GetFiles(first, "*.tmp"), Is.Empty, "the state file is swapped in whole");
        }

        [Test]
        public void TheFolderIsMarkedOpenUntilHisWordsAreIn()
        {
            string dir = FeedbackCapture.Write(Filled(), root);
            Assert.That(File.Exists(Path.Combine(dir, FeedbackCapture.OpenName)), "without the mark the board could take the capture away under his words");
            FeedbackCapture.Done(dir);
            Assert.That(File.Exists(Path.Combine(dir, FeedbackCapture.OpenName)), Is.False);
            Assert.DoesNotThrow(() => FeedbackCapture.Done(dir), "closing twice is closing once");
        }

        [Test]
        public void SavingHisWordsKeepsTheRest()
        {
            var r = Filled();
            string dir = FeedbackCapture.Write(r, root);
            r.note = FeedbackCapture.Clip("  The wire stayed up.  ");
            FeedbackCapture.Save(r, dir);
            var back = FeedbackCapture.Read(dir);
            Assert.That(back.note, Is.EqualTo("The wire stayed up."));
            Assert.That(back.match.tick, Is.EqualTo(5400u));
            Assert.That(back.match.units[0].count, Is.EqualTo(2));
            Assert.That(FeedbackCapture.Clip(new string('x', FeedbackCapture.MaxNote + 50)).Length, Is.EqualTo(FeedbackCapture.MaxNote));
        }

        [Test]
        public void TheFolderIsTheOneTheEnvironmentNames()
        {
            string was = Environment.GetEnvironmentVariable(FeedbackCapture.EnvVar);
            try
            {
                Environment.SetEnvironmentVariable(FeedbackCapture.EnvVar, root);
                Assert.That(FeedbackCapture.Root(), Is.EqualTo(root));
                Environment.SetEnvironmentVariable(FeedbackCapture.EnvVar, null);
                Assert.That(FeedbackCapture.Root().Replace('\\', '/'), Does.EndWith("TrenchWarfare/feedback"), "where Tools/assetboard/feedback.py looks when nothing is named");
            }
            finally { Environment.SetEnvironmentVariable(FeedbackCapture.EnvVar, was); }
        }

        // ---- the box ------------------------------------------------------------------------------------------------
        static VisualElement Box()
        {
            var tree = AssetDatabase.LoadAssetAtPath<VisualTreeAsset>(Folder + "Feedback.uxml");
            Assert.That(tree, Is.Not.Null, Folder + "Feedback.uxml did not load");
            return tree.Instantiate();
        }

        [Test]
        public void TheBoxHasWhatTheScreenAsksFor()
        {
            var box = Box();
            foreach (var n in FeedbackScreen.RequiredNames) Assert.That(box.Q(n), Is.Not.Null, $"Feedback.uxml has no element named '{n}'");
            Assert.That(box.Q<TextField>("feedback-note"), Is.Not.Null, "his words need a field");
            box.Query<Button>().ForEach(b => Assert.That(b.ClassListContains("tw-btn"), $"button '{b.name}' has no skin class"));
            Assert.That(box.Q("feedback-screen").ClassListContains("tw-screen"), Is.False, "tw-screen paints the whole screen: the match must stay visible under the box");
            Assert.That(Resources.Load<VisualTreeAsset>(FeedbackScreen.TreePath), Is.Not.Null, "the router loads the box by this name");
        }

        [Test]
        public void ClosingWithWordsWritesThemAndTakesTheMarkAway()
        {
            var r = Filled(); r.note = "";
            string dir = FeedbackCapture.Write(r, root);
            var screen = new FeedbackScreen(r, dir);
            screen.Bind(Box(), null);
            Assert.That(screen.Modal, "the sim is held and gameplay keys stand down while he types");
            Assert.That(screen.HidesHud, Is.False);
            screen.Words = "  The men look like white dots.  ";
            screen.Close(true);
            Assert.That(FeedbackCapture.Read(dir).note, Is.EqualTo("The men look like white dots."));
            Assert.That(File.Exists(Path.Combine(dir, FeedbackCapture.OpenName)), Is.False);
            Assert.DoesNotThrow(() => screen.Close(true), "a second close does nothing");
        }

        [Test]
        public void ShiftEnterPutsALineBreakInAndKeepsHisWords()
        {
            var r = Filled(); r.note = "";
            string dir = FeedbackCapture.Write(r, root);
            var screen = new FeedbackScreen(r, dir);
            screen.Bind(Box(), null);
            screen.Words = "ab";
            screen.NewLine();
            Assert.That(screen.Words.Length, Is.EqualTo(3));
            Assert.That(screen.Words.Replace("\n", ""), Is.EqualTo("ab"), "the line break went in and nothing went out");
            screen.Words = new string('x', FeedbackCapture.MaxNote);
            screen.NewLine();
            Assert.That(screen.Words.Length, Is.EqualTo(FeedbackCapture.MaxNote), "a full note takes no more");
            screen.Close(false);
        }

        [Test]
        public void TheGameGoingAwayUnderTheBoxKeepsWhatHeTyped()
        {
            var r = Filled(); r.note = "";
            string dir = FeedbackCapture.Write(r, root);
            var screen = new FeedbackScreen(r, dir);
            screen.Bind(Box(), null);
            screen.Words = "half a sentence";
            screen.Abandon();
            Assert.That(FeedbackCapture.Read(dir).note, Is.EqualTo("half a sentence"));
            Assert.That(File.Exists(Path.Combine(dir, FeedbackCapture.OpenName)), Is.False, "the board may take it");
        }

        [Test]
        public void ClosingWithoutWordsKeepsTheCaptureAndLeavesNoWords()
        {
            var r = Filled(); r.note = "";
            string dir = FeedbackCapture.Write(r, root);
            var screen = new FeedbackScreen(r, dir);
            screen.Bind(Box(), null);
            screen.Words = "typed and then thought better of";
            screen.Close(false);
            Assert.That(FeedbackCapture.Read(dir).note, Is.Empty);
            Assert.That(FeedbackCapture.Read(dir).match.tick, Is.EqualTo(5400u), "the capture is kept either way");
            Assert.That(File.Exists(Path.Combine(dir, FeedbackCapture.OpenName)), Is.False);
        }

        [Test]
        public void F10KeptItsPlaceInTheKeyTable()
        {
            // the action took over the slot of the debug panel's, which no code read: a player's saved bindings are an
            // array by slot, so the slot must not move
            Assert.That((int)GameAction.Feedback, Is.EqualTo((int)GameAction.DebugCapsules + 1));
            Assert.That((int)GameAction.HudToggle, Is.EqualTo((int)GameAction.Feedback + 1));
            Assert.That(KeyMap.Defaults().Primary[(int)GameAction.Feedback], Is.EqualTo(UnityEngine.InputSystem.Key.F10));
            Assert.That(KeyMap.Label(GameAction.Feedback), Is.EqualTo("FEEDBACK CAPTURE"));
        }
    }
}
