// Phase: Playground (2026-09-26, lane/show/playground) — TW/Playground/Build: the library asset and the scene
// TW/Playground/Build: (re)writes the playground's library asset from what is in Playground/Art and the game's clip
// folder, and the scene that holds it. Nothing is added to the build settings: the playground never ships.
//   Playground/Art/Tanks/<Name>/  <Name>_LOD<k>.fbx, <Name>_LOD<k>_Base.jpg, tank3.json   (Tools/tank3split.py)
//   Playground/Art/Units/<Name>/  <Name>.fbx, <Name>_LOD<k>_Base.jpg, frogrig.json         (Tools/frogrig.py)
//   Art/Characters/Clips/*.fbx    every clip the game has, played by retargeting (ClipSource = Art/Characters/Soldier.fbx)
using System.Collections.Generic;
using System.IO;
using System.Linq;
using UnityEditor;
using UnityEditor.SceneManagement;
using UnityEngine;

namespace TW.Playground.Editor
{
    public static class PlaygroundSetup
    {
        public const string Root = "Assets/_Project/Playground/";
        public const string LibraryPath = Root + "PlaygroundLibrary.asset";
        public const string ScenePath = Root + "Playground.unity";
        const string Clips = "Assets/_Project/Art/Characters/Clips";
        const string Source = "Assets/_Project/Art/Characters/Soldier.fbx";

        [MenuItem("TW/Playground/Build")]
        public static string Build()
        {
            var lib = AssetDatabase.LoadAssetAtPath<PlaygroundLibrary>(LibraryPath);
            if (lib == null) { lib = ScriptableObject.CreateInstance<PlaygroundLibrary>(); AssetDatabase.CreateAsset(lib, LibraryPath); }
            string summary = Fill(lib);
            EditorUtility.SetDirty(lib);
            AssetDatabase.SaveAssetIfDirty(AssetDatabase.GUIDFromAssetPath(LibraryPath));
            // the scene: one object, the host, holding the library
            if (!File.Exists(ScenePath))
            {
                if (!EditorSceneManager.SaveCurrentModifiedScenesIfUserWantsTo()) return "cancelled";
                var scene = EditorSceneManager.NewScene(NewSceneSetup.EmptyScene, NewSceneMode.Single);
                var go = new GameObject("Playground");
                go.AddComponent<PlaygroundHost>().Library = lib;
                EditorSceneManager.SaveScene(scene, ScenePath);
            }
            return summary;
        }

        /// <summary>Reads the playground's art and the game's clip table into <paramref name="lib"/> and returns the
        /// summary line. Writes nothing to disk, so a test may run it on a throwaway instance.</summary>
        public static string Fill(PlaygroundLibrary lib)
        {
            // vehicles
            var vehicles = new List<PlaygroundLibrary.VehicleEntry>();
            foreach (var dir in Dirs(PlaygroundImport.Art + "Tanks"))
            {
                string name = Path.GetFileName(dir);
                var e = new PlaygroundLibrary.VehicleEntry { Name = name, Manifest = AssetDatabase.LoadAssetAtPath<TextAsset>(dir + "/tank3.json") };
                var lods = new List<GameObject>(); var atlas = new List<Texture2D>();
                for (int k = 0; ; k++)
                {
                    var go = AssetDatabase.LoadAssetAtPath<GameObject>($"{dir}/{name}_LOD{k}.fbx");
                    if (go == null) break;
                    lods.Add(go); atlas.Add(AssetDatabase.LoadAssetAtPath<Texture2D>($"{dir}/{name}_LOD{k}_Base.jpg"));
                }
                e.Lods = lods.ToArray(); e.Atlas = atlas.ToArray();
                if (e.Manifest != null && e.Lods.Length > 0) vehicles.Add(e);
            }
            // units
            var units = new List<PlaygroundLibrary.UnitEntry>();
            foreach (var dir in Dirs(PlaygroundImport.Art + "Units"))
            {
                string name = Path.GetFileName(dir);
                var e = new PlaygroundLibrary.UnitEntry { Name = name, Model = AssetDatabase.LoadAssetAtPath<GameObject>($"{dir}/{name}.fbx"), Rig = AssetDatabase.LoadAssetAtPath<TextAsset>(dir + "/frogrig.json") };
                var atlas = new List<Texture2D>();
                for (int k = 0; ; k++) { var t = AssetDatabase.LoadAssetAtPath<Texture2D>($"{dir}/{name}_LOD{k}_Base.jpg"); if (t == null) break; atlas.Add(t); }
                e.Atlas = atlas.ToArray();
                if (e.Model != null) units.Add(e);
            }
            // the game's clips
            var clips = new List<PlaygroundLibrary.ClipEntry>();
            var untabled = new List<string>();
            foreach (var guid in AssetDatabase.FindAssets("t:Model", new[] { Clips }))
            {
                string path = AssetDatabase.GUIDToAssetPath(guid);
                var clip = AssetDatabase.LoadAllAssetsAtPath(path).OfType<AnimationClip>().FirstOrDefault(c => !c.name.StartsWith("__preview__"));
                if (clip == null) continue;
                string n = Path.GetFileNameWithoutExtension(path);
                bool death = n.Contains("Death") || n.Contains("Dying") || n == "Fall Over";
                var e = new PlaygroundLibrary.ClipEntry { Name = n, Clip = clip, Death = death, Rate = 1f };
                // how the game plays this file: its own table, not a guess from the name. A file several rows cut
                // different windows out of (Reloading: the whole reload at 1.5x, and the bolt 2.3-3.3 at 1.25x) is shown
                // as its widest window, which is what a playground capture is for; the narrower rows are not shown.
                var row = Widest(clip, n);
                if (row.File != null)
                {
                    e.Loop = row.Loop; e.CutStart = row.CutStart; e.CutEnd = row.CutEnd;
                    e.Rate = row.Rate > 0f ? row.Rate : 1f; e.KeepRoot = row.KeepRoot; e.Aim = row.Aim;
                }
                else
                {
                    untabled.Add(n);
                    e.Loop = !death && (clip.isLooping || n.Contains("Idle") || n.Contains("Walk") || n.Contains("Run") || n.Contains("Crawl") || n.Contains("Sprint") || n.StartsWith("Firing") || n.Contains("Wade"));
                    e.KeepRoot = !e.Loop;
                    e.Aim = n.StartsWith("Fir") || n.Contains("Aim");
                }
                clips.Add(e);
            }
            clips.Sort((a, b) => string.CompareOrdinal(a.Name, b.Name));
            lib.Vehicles = vehicles.ToArray(); lib.Units = units.ToArray(); lib.Clips = clips.ToArray();
            lib.ClipSource = AssetDatabase.LoadAssetAtPath<GameObject>(Source);
            untabled.Sort(string.CompareOrdinal);
            return $"playground library: {vehicles.Count} vehicles ({string.Join(",", vehicles.Select(v => v.Name + "x" + v.Lods.Length))}), {units.Count} units, {clips.Count} clips, source {(lib.ClipSource != null ? "ok" : "MISSING")}"
                 + (untabled.Count > 0 ? $"; {untabled.Count} clips the game's table has no row for, played whole and guessed from the name: {string.Join(", ", untabled)}" : "");
        }

        [MenuItem("TW/Playground/Open and Play")]
        public static string OpenAndPlay()
        {
            if (!File.Exists(ScenePath)) Build();
            if (!EditorSceneManager.SaveCurrentModifiedScenesIfUserWantsTo()) return "cancelled";
            EditorSceneManager.OpenScene(ScenePath, OpenSceneMode.Single);
            EditorApplication.isPlaying = true;
            return "playing " + ScenePath;
        }

        /// <summary>The row of the game's clip table for this file that cuts the widest window out of it (CutEnd 0 is the
        /// file's end; ties go to the first row). A file with no row: File is null.</summary>
        static TW.Editor.ClipSource Widest(AnimationClip clip, string file)
        {
            var best = default(TW.Editor.ClipSource); float widest = -1f;
            foreach (var row in TW.Editor.InfantryClipTable.Entries)
            {
                if (row.File != file) continue;
                float span = (row.CutEnd > 0f ? row.CutEnd : clip.length) - row.CutStart;
                if (span > widest) { widest = span; best = row; }
            }
            return best;
        }

        static IEnumerable<string> Dirs(string root)
        {
            if (!Directory.Exists(root)) yield break;
            foreach (var d in Directory.GetDirectories(root)) yield return d.Replace('\\', '/');
        }
    }
}
