// Phase: tooling (2026-10-07) — F10 over a running match: the capture is written on the frame of the press, the box
// comes up on the next with the sim held and gameplay keys standing down, and closing it puts the match back as it
// was. Against a real SimHost and a real ShellRouter, so the hold lands where SimHost.Update reads it. The picture is
// stood in for (the gate has no screen): the router is asked for it once, in the capture's folder, before the box.
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.UI;

namespace TW.Tests
{
    public class FeedbackPlayTests
    {
        GameObject hostGo, shellGo;
        string root, envWas;
        bool bootWas;

        [UnitySetUp]
        public IEnumerator SetUp()
        {
            root = Path.Combine(Application.temporaryCachePath, "feedback-play-" + Guid.NewGuid().ToString("N"));
            envWas = Environment.GetEnvironmentVariable(FeedbackCapture.EnvVar);
            Environment.SetEnvironmentVariable(FeedbackCapture.EnvVar, root);
            bootWas = ShellBoot.Disabled; ShellBoot.Disabled = true;
            yield return null;
        }

        [UnityTearDown]
        public IEnumerator TearDown()
        {
            if (shellGo != null) UnityEngine.Object.Destroy(shellGo);
            if (hostGo != null) UnityEngine.Object.Destroy(hostGo);
            yield return null;
            InputFocus.Reset();
            ShellBoot.Disabled = bootWas;
            Environment.SetEnvironmentVariable(FeedbackCapture.EnvVar, envWas);
            if (Directory.Exists(root)) Directory.Delete(root, true);
        }

        /// <summary>A match with men on the field (the scripted enemy's stress waves, both sides) and a shell over it.</summary>
        IEnumerator Match(Action<SimHost, ShellRouter, List<string>> ready)
        {
            hostGo = new GameObject("feedback-test-host");
            hostGo.SetActive(false);
            var host = hostGo.AddComponent<SimHost>();
            host.GeneratedBattlefield = false;
            host.PlaytestMap = false;
            host.StressUnits = 40;
            hostGo.SetActive(true);

            var assets = ShellAssets.Load();
            Assert.That(assets != null && assets.Panel != null, "ShellAssets missing: run TW/UI/Build Shell Assets");
            shellGo = new GameObject("feedback-test-shell");
            var doc = shellGo.AddComponent<UIDocument>();
            doc.panelSettings = assets.Panel;
            var router = shellGo.AddComponent<ShellRouter>();
            router.Assets = assets;
            var shots = new List<string>();
            router.Shoot = shots.Add;
            yield return null;   // the router binds the scene's host
            yield return null;
            Assert.That(router.Host, Is.SameAs(host), "the test's rig: the router did not find the host");
            Assert.That(router.Clock, Is.Not.Null);
            for (int i = 0; i < 600 && host.Local.World.AliveCount == 0; i++) yield return null;
            Assert.That(host.Local.World.AliveCount, Is.GreaterThan(0), "the test's rig: nobody came onto the field, so a count of them would prove nothing");
            ready(host, router, shots);
        }

        [UnityTest]
        public IEnumerator ThePressWritesTheGameAsItIsAndTheBoxHoldsTheMatch()
        {
            SimHost host = null; ShellRouter router = null; List<string> shots = null;
            yield return Match((h, r, s) => { host = h; router = r; shots = s; });
            var w = host.Local.World;
            uint tick = w.Tick; int alive = w.AliveCount; ulong hash = w.Hash();

            router.Feedback();
            Assert.That(router.Top is FeedbackScreen, Is.False, "the box is not up on the frame of the press: it would be in the picture");
            var dirs = Directory.GetDirectories(root);
            Assert.That(dirs.Length, Is.EqualTo(1));
            var rec = FeedbackCapture.Read(dirs[0]);
            Assert.That(rec.in_match);
            Assert.That(rec.match.tick, Is.EqualTo(tick));
            Assert.That(rec.match.alive, Is.EqualTo(alive));
            int counted = 0; foreach (var u in rec.match.units) counted += u.count;
            Assert.That(counted, Is.EqualTo(alive), "the men counted by side and kind are the men alive");
            Assert.That(rec.match.hash, Is.EqualTo(hash.ToString("x16")));
            Assert.That(rec.match.holds_before, Is.EqualTo("None"));
            Assert.That(rec.match.request.BattlefieldSeed, Is.EqualTo(host.BattlefieldSeed));
            Assert.That(shots, Is.EqualTo(new[] { Path.Combine(dirs[0], FeedbackCapture.ShotName) }), "one picture, into the capture's folder");
            Assert.That(File.Exists(Path.Combine(dirs[0], FeedbackCapture.OpenName)), "open until his words are in");

            yield return null;
            yield return null;
            Assert.That(router.Top, Is.InstanceOf<FeedbackScreen>());
            Assert.That(router.Clock.Has(MatchClock.Hold.Menu), "the box holds the sim");
            Assert.That(InputFocus.Modal, "gameplay keys stand down while he types");
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(0f));
            uint held = host.Local.World.Tick;
            for (int i = 0; i < 5; i++) yield return null;
            Assert.That(host.Local.World.Tick, Is.EqualTo(held), "the match does not move under the box");

            var box = (FeedbackScreen)router.Top;
            box.Words = "The wire stayed up.";
            router.Feedback();   // F10 again: save and close
            Assert.That(router.Top is FeedbackScreen, Is.False);
            Assert.That(FeedbackCapture.Read(dirs[0]).note, Is.EqualTo("The wire stayed up."));
            Assert.That(File.Exists(Path.Combine(dirs[0], FeedbackCapture.OpenName)), Is.False, "the board may take it now");
            Assert.That(Directory.GetDirectories(root).Length, Is.EqualTo(1), "closing made no second capture");
            Assert.That(router.Clock.Paused, Is.False, "the match is as it was");
            Assert.That(InputFocus.Modal, Is.False);
            yield return null;
            Assert.That(host.TimeScale, Is.GreaterThan(0f));
        }

        [UnityTest]
        public IEnumerator HisOwnPauseOutlivesTheBox()
        {
            SimHost host = null; ShellRouter router = null; List<string> shots = null;
            yield return Match((h, r, s) => { host = h; router = r; shots = s; });
            router.Clock.Add(MatchClock.Hold.Tactical);
            yield return null;

            router.Feedback();
            yield return null;
            yield return null;
            Assert.That(router.Top, Is.InstanceOf<FeedbackScreen>());
            var dirs = Directory.GetDirectories(root);
            Assert.That(FeedbackCapture.Read(dirs[0]).match.holds_before, Is.EqualTo("Tactical"), "the file says the match was already held");
            router.Top.OnEscape();   // Esc: close without words
            Assert.That(router.Top is FeedbackScreen, Is.False);
            Assert.That(FeedbackCapture.Read(dirs[0]).note, Is.Empty);
            Assert.That(File.Exists(Path.Combine(dirs[0], FeedbackCapture.OpenName)), Is.False);
            Assert.That(router.Clock.Has(MatchClock.Hold.Tactical), "his pause was his: it stays on");
            Assert.That(router.Clock.Has(MatchClock.Hold.Menu), Is.False);
            yield return null;
            Assert.That(host.TimeScale, Is.EqualTo(0f));
        }

        [UnityTest]
        public IEnumerator ASceneThatGoesAwayUnderTheBoxStillClosesTheCapture()
        {
            SimHost host = null; ShellRouter router = null; List<string> shots = null;
            yield return Match((h, r, s) => { host = h; router = r; shots = s; });
            router.Feedback();
            yield return null;
            yield return null;
            Assert.That(router.Top, Is.InstanceOf<FeedbackScreen>());
            router.ClearStack();   // what a scene load does to the stack
            var dirs = Directory.GetDirectories(root);
            Assert.That(File.Exists(Path.Combine(dirs[0], FeedbackCapture.FileName)));
            Assert.That(File.Exists(Path.Combine(dirs[0], FeedbackCapture.OpenName)), Is.False, "a capture left marked open would wait half an hour for words that cannot come");
        }
    }
}
