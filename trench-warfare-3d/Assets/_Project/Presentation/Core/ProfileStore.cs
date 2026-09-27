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
        public static CampaignProfile Current => current ??= LoadDefault();

        /// <summary>False while profile.json could not be taken as it is: locked (an antivirus scan, a sync client) or written
        /// by a newer build. Save then writes nothing, so a passing lock or a newer save is never replaced by a fresh or
        /// downgraded profile (critic r5, 2026-09-27: a lock read as corruption, and the next Save wiped the campaign).</summary>
        public static bool Writable { get; private set; } = true;

        static CampaignProfile LoadDefault() { var p = LoadFrom(DefaultPath, out bool writable); Writable = writable; return p; }

        /// <summary>The campaign screens call this as they open: a profile.json that was locked when first read is read
        /// again, so a passing lock costs the player nothing past that moment instead of the whole session (critic r6).</summary>
        public static void RetryIfLocked()
        {
            if (Writable || current == null) return;
            var p = LoadFrom(DefaultPath, out bool writable);
            if (writable) { current = p; Writable = true; }
        }

        [Serializable] sealed class VersionOnly { public int Version; }

        /// <summary>What the campaign screens put beside the gold while nothing is saved (a locked or newer profile.json),
        /// so the player is told rather than finding the progress gone next time (critic r8).</summary>
        public const string NotSavingNote = "  ·  NOT SAVING: profile.json is locked or from a newer build";
        public static string GoldReadout(int gold) => Writable ? gold.ToString() : gold + NotSavingNote;
        /// <summary>Tests: force saving on or off; null (the default) leaves it to <see cref="Persist"/>'s rule. Save and
        /// restore the old value, never a read of Persist, so the rule comes back after the test.</summary>
        public static bool? PersistOverride;
        /// <summary>Does Save write the file: the override, else off in the editor outside play (EditMode tests, so a fixture
        /// that forgets to substitute a profile cannot write the developer's) and on everywhere else. Read each time, not
        /// fixed at type init, so it cannot go stale whatever the Enter Play Mode options (EditorSettings has them on with
        /// both reloads, m_EnterPlayModeOptions 0, today; turning off the domain reload would otherwise carry an EditMode
        /// "off" into play, a campaign that never saves, or a play "on" out of it).</summary>
        public static bool Persist => PersistOverride ?? !(Application.isEditor && !Application.isPlaying);

        public static CampaignProfile Load() => current = LoadDefault();

        public static CampaignProfile LoadFrom(string path) => LoadFrom(path, out _);

        /// <summary>The profile in a file, and whether that file may be written over. A file that cannot be opened (locked)
        /// is tried a few times, then left alone for the session (writable false); a file that opens but is not a profile
        /// is kept beside it under a timestamped .bad name and a fresh campaign starts; a file from a newer build loads but
        /// is not written back (its fields this build does not know would be lost).</summary>
        public static CampaignProfile LoadFrom(string path, out bool writable)
        {
            writable = true;
            if (!File.Exists(path)) return new CampaignProfile();
            string text = null;
            for (int attempt = 0; attempt < 4 && text == null; attempt++)
            {
                try { text = File.ReadAllText(path); }
                catch (IOException e) when (!(e is FileNotFoundException) && !(e is DirectoryNotFoundException))
                {
                    if (attempt == 3)
                    {
                        writable = false;
                        Debug.LogWarning($"ProfileStore: {path} is locked ({e.Message}); this session plays a fresh campaign and saves nothing over it");
                        return new CampaignProfile();
                    }
                    System.Threading.Thread.Sleep(50 * (attempt + 1));
                }
                catch (Exception e) { writable = false; Debug.LogWarning($"ProfileStore: cannot read {path}: {e.Message}; nothing will be saved over it"); return new CampaignProfile(); }
            }
            try
            {
                var version = JsonUtility.FromJson<VersionOnly>(text);
                var p = CampaignProfile.FromJson(text);
                if (version != null && version.Version > CampaignProfile.CurrentVersion)
                {
                    writable = false;
                    Debug.LogWarning($"ProfileStore: {path} was written by a newer build (version {version.Version}); it is read, not written back");
                }
                return p;
            }
            catch (Exception e)
            {
                // keep the unreadable file beside the profile, under a name of its own each time (a second bad load must not
                // overwrite the first copy): a hand-edit gone wrong should cost the player a repair, not the campaign
                string kept = path + ".bad-" + DateTime.Now.ToString("yyyyMMdd-HHmmss-fff") + "-" + Guid.NewGuid().ToString("N").Substring(0, 6);
                // no copy, no overwrite: if the broken file cannot be kept aside, the next Save must not replace the only copy
                try { File.Copy(path, kept, false); } catch (Exception) { kept = "(could not keep a copy: it will not be written over)"; writable = false; }
                Debug.LogWarning($"ProfileStore: could not read {path}: {e.Message}; starting a fresh campaign, the old file kept as {kept}");
                return new CampaignProfile();
            }
        }

        public static void Save(CampaignProfile p)
        {
            current = p;
            if (!Persist) return;
            if (!Writable) { Debug.LogWarning("ProfileStore: not saving over " + DefaultPath + " (it was locked or newer when read)"); return; }
            SaveTo(p, DefaultPath);
        }

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
