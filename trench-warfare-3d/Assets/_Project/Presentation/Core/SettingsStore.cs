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
        public static string DefaultPath => Path.Combine(Application.persistentDataPath, FileName);

        /// <summary>The settings in force. Load() fills it; SettingsApplier reads it.</summary>
        public static GameSettings Current { get; private set; } = GameSettings.Defaults();

        public static GameSettings Load() => Current = LoadFrom(DefaultPath);

        public static GameSettings LoadFrom(string path)
        {
            try
            {
                if (File.Exists(path)) return GameSettings.FromJson(File.ReadAllText(path));
            }
            catch (Exception e) { Debug.LogWarning($"SettingsStore: could not read {path}: {e.Message}; using defaults"); }
            return GameSettings.Defaults();
        }

        public static void Save(GameSettings s) { Current = s; SaveTo(s, DefaultPath); }

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
