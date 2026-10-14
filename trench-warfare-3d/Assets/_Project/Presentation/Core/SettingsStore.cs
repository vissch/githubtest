// Phase: B6 (implemented) — settings.json in and out of persistentDataPath.
// One human-readable file the player can attach to a bug report, written whole to a .tmp and swapped into place so a
// crash mid-write leaves the old file, not half of a new one. PlayerPrefs on Windows would be a scatter of registry
// values and cannot hold the key arrays.
using System;
using System.IO;
using UnityEngine;

namespace TW.Presentation
{
    public static class SettingsStore
    {
        public const string FileName = "settings.json";
        /// <summary>Tests: the file Load() and Save() use; null (the default) means persistentDataPath, so a fixture
        /// that forgets it cannot read or write the developer's own settings.json (ProfileStore.PersistOverride's idiom).</summary>
        public static string PathOverride;
        public static string DefaultPath => PathOverride ?? Path.Combine(Application.persistentDataPath, FileName);

        /// <summary>The settings in force. Load() fills it; SettingsApplier reads it.</summary>
        public static GameSettings Current { get; private set; } = GameSettings.Defaults();

        /// <summary>False once a malformed settings.json could not be kept aside as a .bad- copy (ProfileStore's idiom):
        /// the only copy of the damaged file must not then be lost to the next Save of defaults.</summary>
        public static bool Writable { get; private set; } = true;

        public static GameSettings Load() => Current = LoadFrom(DefaultPath);

        /// <summary>Tests: put back the Current a test found, so a launch hook it invoked does not leave its settings
        /// in force for the rest of the session.</summary>
        public static void SetCurrentForTests(GameSettings s) => Current = s;

        public static GameSettings LoadFrom(string path)
        {
            Writable = true;
            try
            {
                if (!File.Exists(path)) return GameSettings.Defaults();
                string text = File.ReadAllText(path);
                var s = GameSettings.FromJson(text, out bool ok);
                if (!ok)
                {
                    string kept = path + ".bad-" + DateTime.Now.ToString("yyyyMMdd-HHmmss-fff") + "-" + Guid.NewGuid().ToString("N").Substring(0, 6);
                    try { File.Copy(path, kept, false); Debug.LogWarning($"SettingsStore: could not parse {path}; using defaults, the old file kept as {kept}"); }
                    catch (Exception) { Writable = false; Debug.LogWarning($"SettingsStore: could not parse {path} and could not keep a copy; it will not be written over"); }
                    return GameSettings.Defaults();
                }
                return s;
            }
            catch (Exception e) { Debug.LogWarning($"SettingsStore: could not read {path}: {e.Message}; using defaults"); }
            return GameSettings.Defaults();
        }

        public static void Save(GameSettings s)
        {
            Current = s;
            if (!Writable) { Debug.LogWarning("SettingsStore: not saving over " + DefaultPath + " (a damaged copy could not be kept aside)"); return; }
            SaveTo(s, DefaultPath);
        }

        public static void SaveTo(GameSettings s, string path)
        {
            try
            {
                var dir = Path.GetDirectoryName(path);
                if (!string.IsNullOrEmpty(dir)) Directory.CreateDirectory(dir);
                string tmp = path + ".tmp";
                File.WriteAllText(tmp, s.ToJson());
                if (File.Exists(path)) File.Replace(tmp, path, null); else File.Move(tmp, path);
            }
            catch (Exception e) { Debug.LogWarning($"SettingsStore: could not write {path}: {e.Message}"); }
        }
    }
}
