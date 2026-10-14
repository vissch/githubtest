// Phase: B6 (implemented) — settings.json round-trips, tolerates an old or damaged file, and lands on disk atomically.
using System;
using System.IO;
using System.Linq;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.InputSystem;
using UnityEngine.UIElements;
using UnityEditor;
using TW.Presentation;

namespace TW.Tests
{
    public class GameSettingsTests
    {
        static FieldInfo Field(string path, out object owner, GameSettings s)
        {
            var parts = path.Split('.');
            owner = typeof(GameSettings).GetField(parts[0]).GetValue(s);
            return owner.GetType().GetField(parts[1]);
        }

        /// <summary>A slider's range lived in Settings.uxml, in Migrate's clamps and in [Range] attributes, and they
        /// had drifted (closest zoom 2-30 on screen, 2-60 on load; pan speed and shake never clamped). Now
        /// GameSettings.Sliders is the one table; this holds every other copy to it.</summary>
        [Test]
        public void EverySettingsSliderHasOneRange()
        {
            var uxml = File.ReadAllText(Path.Combine(Application.dataPath, "_Project/UI/Shell/Settings.uxml"));
            var names = System.Text.RegularExpressions.Regex.Matches(uxml, "<ui:Slider name=\"([\\w-]+)\"")
                .Cast<System.Text.RegularExpressions.Match>().Select(m => m.Groups[1].Value).ToList();
            CollectionAssert.AreEquivalent(names, GameSettings.Sliders.Select(x => x.Slider),
                "every slider in Settings.uxml has a row in GameSettings.Sliders, and no row is left over");
            foreach (var (slider, field, min, max) in GameSettings.Sliders)
            {
                var tag = System.Text.RegularExpressions.Regex.Match(uxml, $"<ui:Slider name=\"{slider}\"[^>]*>").Value;
                Assert.That(tag, Does.Contain($"low-value=\"{min.ToString(System.Globalization.CultureInfo.InvariantCulture)}\""), slider);
                Assert.That(tag, Does.Contain($"high-value=\"{max.ToString(System.Globalization.CultureInfo.InvariantCulture)}\""), slider);

                var s = GameSettings.Defaults();
                var f = Field(field, out var owner, s);
                Assert.IsNotNull(f, $"{field} is a field of GameSettings");
                var range = f.GetCustomAttribute<UnityEngine.RangeAttribute>();
                if (range != null)
                    Assert.That((range.min, range.max), Is.EqualTo((min, max)), $"[Range] on {field}");

                float def = (float)f.GetValue(owner);
                Assert.That(def, Is.InRange(min, max), $"the default {field} is on its slider");
                f.SetValue(owner, min - 1000f);
                s.Migrate();
                Assert.That((float)f.GetValue(owner), Is.InRange(min, max), $"a loaded {field} below the slider is clamped");
                f.SetValue(owner, max + 1000f);
                s.Migrate();
                Assert.That((float)f.GetValue(owner), Is.InRange(min, max), $"a loaded {field} above the slider is clamped");
            }
        }

        [Test]
        public void DefaultsRoundTripThroughJson()
        {
            var d = GameSettings.Defaults();
            var back = GameSettings.FromJson(d.ToJson());
            Assert.That(back.ToJson(), Is.EqualTo(d.ToJson()));
            Assert.That(back.Version, Is.EqualTo(GameSettings.CurrentVersion));
        }

        /// <summary>The owner asked for a game that does not start making noise. A default is easy to lose in a
        /// merge, and nobody notices sound that is ON when it should be off until it is in front of a player.</summary>
        [Test]
        public void TheGameStartsMuted()
        {
            var d = GameSettings.Defaults();
            Assert.That(d.Audio.Master, Is.EqualTo(0f), "a fresh install starts silent");
            Assert.That(d.Audio.Sfx, Is.GreaterThan(0f), "only the master is down: turning it up restores a mix");
            Assert.That(d.Audio.Ambience, Is.GreaterThan(0f), "only the master is down: turning it up restores a mix");
            Assert.That(GameSettings.FromJson(d.ToJson()).Audio.Master, Is.EqualTo(0f), "and it survives being saved");
        }

        /// <summary>
        /// The muting has to actually happen, not merely be configured. The previous version of this file asserted
        /// the default constant and nothing else, which is why it stayed green while Master was being applied TWICE
        /// - once on the listener and once again as a bus inside Storm, so thunder came out scaled by Master squared.
        /// A test that only reads the number it was told to expect cannot see a mechanism fault.
        /// </summary>
        [Test]
        public void ApplyingTheSettingsActuallySilencesTheGame_AndMasterIsAppliedExactlyOnce()
        {
            float restore = AudioListener.volume;
            try
            {
                TW.UI.SettingsApplier.ApplyAudio(GameSettings.Defaults());
                Assert.That(AudioListener.volume, Is.EqualTo(0f), "a fresh install is silent in fact, not just on paper");

                var up = GameSettings.Defaults();
                up.Audio.Master = 1f; up.Audio.Ambience = 0.5f;
                TW.UI.SettingsApplier.ApplyAudio(up);
                Assert.That(AudioListener.volume, Is.EqualTo(1f), "turning the master up turns the game on");

                // the mechanism: master is the listener and ONLY the listener, the buses carry their own value
                // unmultiplied. If master ever reappears as a bus, a source that reads both squares it.
                var half = GameSettings.Defaults();
                half.Audio.Master = 0.5f; half.Audio.Ambience = 1f;
                TW.UI.SettingsApplier.ApplyAudio(half);
                Assert.That(AudioListener.volume, Is.EqualTo(0.5f), "master goes to the listener unchanged");
                Assert.That(AudioLevels.Ambience, Is.EqualTo(1f), "the ambience bus is not scaled by master as well");
                Assert.That(AudioLevels.Sfx, Is.EqualTo(half.Audio.Sfx));
                Assert.That(AudioLevels.Music, Is.EqualTo(half.Audio.Music));
            }
            finally { AudioListener.volume = restore; TW.UI.SettingsApplier.ApplyAudio(SettingsStore.Current); }
        }

        /// <summary>
        /// SOMETHING has to apply the audio at launch, and that something is one attribute.
        ///
        /// AudioListener.volume's own default is 1 - Unity's value, not ours - so a fresh install is NOT silent by
        /// virtue of the default in GameSettings. It is silent because SettingsApplier.ApplyAudioOnLaunch runs
        /// before the first scene and puts the player's master on the listener. That method is private and nothing
        /// in the project calls it by name, so removing [RuntimeInitializeOnLoadMethod] breaks no compile, no
        /// reference and no other test here: TheGameStartsMuted would still pass on the constant, and
        /// ApplyingTheSettingsActuallySilencesTheGame would still pass because it calls ApplyAudio by hand. The
        /// game would ship at full volume with a green suite - which is the failure that method's own docstring
        /// says it exists to prevent.
        ///
        /// So this asserts the hook exists, runs BEFORE the first scene (after it, a scene's opening sounds have
        /// already played at Unity's default), and actually lands the player's master on the listener.
        /// </summary>
        [Test]
        public void SomethingPutsTheVolumeOnTheListenerAtLaunch()
        {
            var hooks = typeof(TW.UI.SettingsApplier)
                .GetMethods(BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic)
                .Where(m => m.GetCustomAttributes(typeof(RuntimeInitializeOnLoadMethodAttribute), false).Length > 0)
                .ToList();
            Assert.IsNotEmpty(hooks,
                "nothing on SettingsApplier runs at launch, so AudioListener.volume keeps Unity's default of 1 "
                + "and the game ships loud however muted the settings tree says it is");
            Assert.IsTrue(
                hooks.Any(m => ((RuntimeInitializeOnLoadMethodAttribute)m.GetCustomAttributes(
                    typeof(RuntimeInitializeOnLoadMethodAttribute), false)[0]).loadType == RuntimeInitializeLoadType.BeforeSceneLoad),
                "the launch hook must run BeforeSceneLoad: after it, a scene's first sounds have already played");

            // [TU7] The hook reads SettingsStore.Load(), which used to mean the developer's own settings.json and
            // left its contents in SettingsStore.Current for every later test in the session. The test now gives the
            // store a settings file of its own (SettingsStore.PathOverride) and puts Current back afterwards, and the
            // expected volume is the one IN THAT FILE, not a read of whatever the store happens to hold.
            float restore = AudioListener.volume;
            var before = SettingsStore.Current;
            string file = Path.Combine(Path.GetTempPath(), "tw-tu7-settings.json");
            try
            {
                var mine = GameSettings.Defaults();
                mine.Audio.Master = 0.4242f;      // a value nothing in the project would ever choose
                SettingsStore.SaveTo(mine, file);
                SettingsStore.PathOverride = file;
                AudioListener.volume = 0.7734f;
                foreach (var m in hooks) m.Invoke(null, null);
                Assert.That(AudioListener.volume, Is.EqualTo(0.4242f).Within(1e-5f),
                    "[TU7] the launch hook read the developer's own settings.json, not the path the test gave it");
            }
            finally
            {
                if (File.Exists(file)) File.Delete(file);
                SettingsStore.PathOverride = null;
                SettingsStore.SetCurrentForTests(before);
                AudioListener.volume = restore;
                TW.UI.SettingsApplier.ApplyAudio(SettingsStore.Current);
            }
        }

        /// <summary>
        /// The warning has to follow the value it describes. It was written once while the audio tab was built and
        /// guarded by `Master <= 0`, so it could only ever be turned ON: a player who dragged master up was still
        /// told SOUND IS OFF, and a player who dragged it down to zero was told nothing at all. That text is the
        /// only thing distinguishing a deliberately silent game from a broken one.
        ///
        /// [TU5] Driven through the real panel, not the source text: the old test only read SettingsScreen.cs for
        /// the words "SoundChanged()" on the master slider's builder line, so it passed on a SoundChanged that did
        /// nothing - the bug it was written for. This binds the real Settings.uxml and drives SoundChanged.
        /// </summary>
        [Test]
        public void TheSoundIsOffWarningIsRecomputedWhenTheMasterMoves()
        {
            var tree = AssetDatabase.LoadAssetAtPath<VisualTreeAsset>("Assets/_Project/UI/Shell/Settings.uxml");
            Assert.That(tree, Is.Not.Null, "Settings.uxml did not load");
            var root = tree.Instantiate();
            string plain = root.Q<Label>("audio-note").text;
            float restore = AudioListener.volume;
            var before = SettingsStore.Current;
            var screen = new TW.UI.SettingsScreen();
            try
            {
                // The panel opens on a master the player can hear, so the note starts plain whatever the machine's
                // own settings.json holds.
                var loud = GameSettings.Defaults(); loud.Audio.Master = 0.5f;
                SettingsStore.SetCurrentForTests(loud);
                screen.Bind(root, null);
                Assert.AreEqual(plain, root.Q<Label>("audio-note").text, "[TU5] the panel opens with the plain note");

                // The master slider's own callback cannot be fired here: a tree with no panel has no event
                // dispatcher, so slider.value = 0 changes the field and sends nothing. What the slider's callback
                // does IS SoundChanged (asserted on the builder line below), so the behaviour is driven through it.
                var draft = typeof(TW.UI.SettingsScreen).GetField("draft", BindingFlags.Instance | BindingFlags.NonPublic);
                var sound = typeof(TW.UI.SettingsScreen).GetMethod("SoundChanged", BindingFlags.Instance | BindingFlags.NonPublic);
                Assert.IsNotNull(draft, "SettingsScreen still edits a draft"); Assert.IsNotNull(sound, "[TU5] SettingsScreen.SoundChanged is gone");

                var d = (GameSettings)draft.GetValue(screen);
                d.Audio.Master = 0f; draft.SetValue(screen, d);
                sound.Invoke(screen, null);
                Assert.AreEqual("SOUND IS OFF. DRAG MASTER UP TO HEAR THE GAME.", root.Q<Label>("audio-note").text,
                    "[TU5] SoundChanged does not refresh the note");
                Assert.That(AudioListener.volume, Is.EqualTo(0f).Within(1e-5f), "[TU5] SoundChanged does not apply the draft");

                d.Audio.Master = 0.5f; draft.SetValue(screen, d);
                sound.Invoke(screen, null);
                Assert.AreEqual(plain, root.Q<Label>("audio-note").text,
                    "[TU5] SoundChanged does not refresh the note: the warning stays after the master is turned up");

                // and the master slider really reports its change, or nothing above is ever reached in the game
                var src = File.ReadAllText(Path.Combine(Application.dataPath, "_Project/UI/Shell/SettingsScreen.cs"));
                var master = src.Split('\n').FirstOrDefault(l => l.Contains("slider-master") && l.Contains("draft.Audio.Master"));
                Assert.IsNotNull(master, "the master slider is still built in SettingsScreen");
                StringAssert.Contains("SoundChanged()", master, "[TU5] the master slider does not report its change");
            }
            finally
            {
                screen.Unbind();
                SettingsStore.SetCurrentForTests(before);
                AudioListener.volume = restore;
                TW.UI.SettingsApplier.ApplyAudio(SettingsStore.Current);
            }
        }

        [Test]
        public void EditedValuesSurviveTheRoundTrip()
        {
            var s = GameSettings.Defaults();
            s.Audio.Master = 0.25f; s.Interface.UiScale = 1.25f; s.Camera.ZoomMax = 300f; s.Video.VSync = false;
            s.Bindings.Primary[(int)GameAction.Advance] = Key.Enter;
            var back = GameSettings.FromJson(s.ToJson());
            Assert.That(back.Audio.Master, Is.EqualTo(0.25f).Within(1e-5f));
            Assert.That(back.Interface.UiScale, Is.EqualTo(1.25f).Within(1e-5f));
            Assert.That(back.Camera.ZoomMax, Is.EqualTo(300f).Within(1e-5f));
            Assert.That(back.Video.VSync, Is.False);
            Assert.That(back.Bindings.Primary[(int)GameAction.Advance], Is.EqualTo(Key.Enter));
        }

        [Test]
        public void GarbageAndEmptyJsonFallBackToDefaults()
        {
            Assert.That(GameSettings.FromJson("").ToJson(), Is.EqualTo(GameSettings.Defaults().ToJson()));
            Assert.That(GameSettings.FromJson("not json {{{").ToJson(), Is.EqualTo(GameSettings.Defaults().ToJson()));
            Assert.That(GameSettings.FromJson(null).ToJson(), Is.EqualTo(GameSettings.Defaults().ToJson()));
        }

        [Test]
        public void AnOlderFileWithMissingSectionsIsMigrated()
        {
            // a version-0 file that only knew about audio, with a short bindings array
            string old = "{\"Version\":0,\"Audio\":{\"Master\":0.5,\"Ambience\":2.0,\"Sfx\":1,\"Music\":1},\"Bindings\":{\"Primary\":[22,23],\"Secondary\":[]}}";
            var s = GameSettings.FromJson(old);
            Assert.That(s.Version, Is.EqualTo(GameSettings.CurrentVersion));
            Assert.That(s.Audio.Master, Is.EqualTo(0.5f).Within(1e-5f));
            Assert.That(s.Audio.Ambience, Is.EqualTo(1f), "out-of-range values are clamped");
            Assert.That(s.Video, Is.Not.Null); Assert.That(s.Camera, Is.Not.Null); Assert.That(s.Interface, Is.Not.Null);
            Assert.That(s.Bindings.Primary.Length, Is.EqualTo(KeyMap.ActionCount));
            var d = KeyMap.Defaults();
            for (int i = 2; i < KeyMap.ActionCount; i++)
                Assert.That(s.Bindings.Primary[i], Is.EqualTo(d.Primary[i]), $"{(GameAction)i} should be filled from the defaults");
        }

        /// <summary>Rebinding survives a reload: give the beam's key to hold fire (the settings screen unbinds the beam), save,
        /// load. Migration used to refill every unbound action with its default on every load, so B did both again
        /// (critic r5, 2026-09-27).</summary>
        [Test]
        public void AnActionThePlayerUnboundStaysUnboundAfterAReload()
        {
            var s = GameSettings.Defaults();
            int beam = (int)GameAction.ArmBeam;
            var key = s.Bindings.Primary[beam];
            Assert.That(key, Is.Not.EqualTo(UnityEngine.InputSystem.Key.None), "the beam has a default key");
            s.Bindings.Primary[beam] = UnityEngine.InputSystem.Key.None; s.Bindings.Secondary[beam] = UnityEngine.InputSystem.Key.None;
            s.Bindings.Primary[0] = key;   // another action takes it
            var back = GameSettings.FromJson(s.ToJson());
            Assert.That(back.Bindings.Primary[beam], Is.EqualTo(UnityEngine.InputSystem.Key.None), "the beam stays unbound");
            Assert.That(back.Bindings.Secondary[beam], Is.EqualTo(UnityEngine.InputSystem.Key.None));
            Assert.That(back.Bindings.Primary[0], Is.EqualTo(key), "and its key stays where the player put it");
        }

        [Test]
        public void StoreWritesAndReadsAFile()
        {
            string path = Path.Combine(Application.temporaryCachePath, "tw-settings-test", "settings.json");
            try
            {
                var s = GameSettings.Defaults();
                s.Camera.PanSpeed = 42f;
                SettingsStore.SaveTo(s, path);
                Assert.That(File.Exists(path));
                Assert.That(File.Exists(path + ".tmp"), Is.False, "the temp file is swapped into place, not left behind");
                var back = SettingsStore.LoadFrom(path);
                Assert.That(back.Camera.PanSpeed, Is.EqualTo(42f).Within(1e-5f));
                // a second save replaces the first
                s.Camera.PanSpeed = 43f;
                SettingsStore.SaveTo(s, path);
                Assert.That(SettingsStore.LoadFrom(path).Camera.PanSpeed, Is.EqualTo(43f).Within(1e-5f));
            }
            finally
            {
                var dir = Path.GetDirectoryName(path);
                if (Directory.Exists(dir)) Directory.Delete(dir, true);
            }
        }

        [Test]
        public void LoadingAMissingFileGivesDefaultsWithoutThrowing()
        {
            var s = SettingsStore.LoadFrom(Path.Combine(Application.temporaryCachePath, "tw-settings-none", "settings.json"));
            Assert.That(s.ToJson(), Is.EqualTo(GameSettings.Defaults().ToJson()));
        }

        /// <summary>[P10a] A malformed settings.json used to be swallowed silently (GameSettings.FromJson ate the
        /// parse exception itself) and then overwritten on the next save; it must instead be kept aside, like
        /// ProfileStore keeps a broken profile.json, and defaults take over for this session.</summary>
        [Test]
        public void AMalformedSettingsFileIsKeptAsideAndDefaultsTakeOver()
        {
            string dir = Path.Combine(Path.GetTempPath(), "tw-settings-test-" + Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(dir);
            try
            {
                string path = Path.Combine(dir, "settings.json");
                File.WriteAllText(path, "{ not json");
                var s = SettingsStore.LoadFrom(path);
                Assert.That(s.ToJson(), Is.EqualTo(GameSettings.Defaults().ToJson()), "[P10a] defaults take over for a malformed file");
                var bad = Directory.GetFiles(dir, "settings.json.bad-*");
                Assert.That(bad.Length, Is.EqualTo(1), "[P10a] the broken file is kept aside, not overwritten silently");
                Assert.That(File.ReadAllText(bad[0]), Is.EqualTo("{ not json"));

                SettingsStore.SaveTo(GameSettings.Defaults(), path);
                SettingsStore.LoadFrom(path);
                Assert.That(Directory.GetFiles(dir, "settings.json.bad-*").Length, Is.EqualTo(1), "[P10a] a good file is never copied aside");
            }
            finally { try { Directory.Delete(dir, true); } catch (Exception) { } }
        }
    }
}
