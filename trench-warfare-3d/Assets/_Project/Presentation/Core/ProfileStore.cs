// Phase: B6 / docs/21 phase 6 (implemented) — profile.json in and out of persistentDataPath, the way SettingsStore
// keeps settings.json: written whole to a .tmp and swapped into place, so a crash mid-write leaves the old file.
// Current persists across matches and scene loads by design (StaticLifecycleTests explains it).
using System;
using System.IO;
using UnityEngine;

namespace TW.Presentation
{
    public static class ProfileStore
    {
        public const string FileName = "profile.json";
        public static string DefaultPath => Path.Combine(Application.persistentDataPath, FileName);

        static CampaignProfile current;
        /// <summary>The profile in force, loaded on first use.</summary>
        public static CampaignProfile Current => current ??= LoadFrom(DefaultPath);
        /// <summary>Save writes the file. Off by default in the editor outside play (EditMode tests), so a fixture that forgets
        /// to substitute a profile cannot write the developer's; tests may set it either way.</summary>
        public static bool Persist = !(Application.isEditor && !Application.isPlaying);

        public static CampaignProfile Load() => current = LoadFrom(DefaultPath);

        public static CampaignProfile LoadFrom(string path)
        {
            try
            {
                if (File.Exists(path)) return CampaignProfile.FromJson(File.ReadAllText(path));
            }
            catch (Exception e) { Debug.LogWarning($"ProfileStore: could not read {path}: {e.Message}; starting a fresh campaign"); }
            return new CampaignProfile();
        }

        public static void Save(CampaignProfile p) { current = p; if (Persist) SaveTo(p, DefaultPath); }

        public static void SaveTo(CampaignProfile p, string path)
        {
            try
            {
                var dir = Path.GetDirectoryName(path);
                if (!string.IsNullOrEmpty(dir)) Directory.CreateDirectory(dir);
                string tmp = path + ".tmp";
                File.WriteAllText(tmp, p.ToJson());
                if (File.Exists(path)) File.Replace(tmp, path, null); else File.Move(tmp, path);
            }
            catch (Exception e) { Debug.LogWarning($"ProfileStore: could not write {path}: {e.Message}"); }
        }

        /// <summary>Tests: use this profile instead of the file.</summary>
        public static void Use(CampaignProfile p) => current = p;
    }
}
