// Phase: B6 (implemented) — settings.json round-trips, tolerates an old or damaged file, and lands on disk atomically.
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.InputSystem;
using TW.Presentation;

namespace TW.Tests
{
    public class GameSettingsTests
    {
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
    }
}
