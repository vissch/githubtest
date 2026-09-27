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
        /// <summary>Tests: force saving on or off; null (the default) leaves it to <see cref="Persist"/>'s rule. Save and
        /// restore the old value, never a read of Persist, so the rule comes back after the test.</summary>
        public static bool? PersistOverride;
        /// <summary>Does Save write the file: the override, else off in the editor outside play (EditMode tests, so a fixture
        /// that forgets to substitute a profile cannot write the developer's) and on everywhere else. Read each time, not
        /// fixed at type init: the project enters play without a domain reload (EditorSettings), so a value taken when
        /// the type first loaded would carry an EditMode "off" into play (a campaign that never saves) or a play "on" out
        /// of it (a test that writes the developer's profile).</summary>
        public static bool Persist => PersistOverride ?? !(Application.isEditor && !Application.isPlaying);

        public static CampaignProfile Load() => current = LoadFrom(DefaultPath);

        public static CampaignProfile LoadFrom(string path)
        {
            try
            {
                if (File.Exists(path)) return CampaignProfile.FromJson(File.ReadAllText(path));
            }
            catch (Exception e)
            {
                // keep the unreadable file beside the profile: the next Save replaces profile.json, and a hand-edit gone
                // wrong should cost the player a repair, not the campaign
                string kept = path + ".bad";
                try { File.Copy(path, kept, true); } catch (Exception) { kept = "(could not keep a copy)"; }
                Debug.LogWarning($"ProfileStore: could not read {path}: {e.Message}; starting a fresh campaign, the old file kept as {kept}");
            }
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
