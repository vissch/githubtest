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
            foreach (var guid in AssetDatabase.FindAssets("t:Model", new[] { Clips }))
            {
                string path = AssetDatabase.GUIDToAssetPath(guid);
                var clip = AssetDatabase.LoadAllAssetsAtPath(path).OfType<AnimationClip>().FirstOrDefault(c => !c.name.StartsWith("__preview__"));
                if (clip == null) continue;
                string n = Path.GetFileNameWithoutExtension(path);
                bool death = n.Contains("Death") || n.Contains("Dying") || n == "Fall Over";
                bool loop = !death && (clip.isLooping || n.Contains("Idle") || n.Contains("Walk") || n.Contains("Run") || n.Contains("Crawl") || n.Contains("Sprint") || n.StartsWith("Firing") || n.Contains("Wade"));
                clips.Add(new PlaygroundLibrary.ClipEntry { Name = n, Clip = clip, Loop = loop, Death = death });
            }
            clips.Sort((a, b) => string.CompareOrdinal(a.Name, b.Name));
            lib.Vehicles = vehicles.ToArray(); lib.Units = units.ToArray(); lib.Clips = clips.ToArray();
            lib.ClipSource = AssetDatabase.LoadAssetAtPath<GameObject>(Source);
            EditorUtility.SetDirty(lib);
            AssetDatabase.SaveAssets();
            // the scene: one object, the host, holding the library
            if (!File.Exists(ScenePath))
            {
                var scene = EditorSceneManager.NewScene(NewSceneSetup.EmptyScene, NewSceneMode.Single);
                var go = new GameObject("Playground");
                go.AddComponent<PlaygroundHost>().Library = lib;
                EditorSceneManager.SaveScene(scene, ScenePath);
            }
            return $"playground library: {vehicles.Count} vehicles ({string.Join(",", vehicles.Select(v => v.Name + "x" + v.Lods.Length))}), {units.Count} units, {clips.Count} clips, source {(lib.ClipSource != null ? "ok" : "MISSING")}";
        }

        [MenuItem("TW/Playground/Open and Play")]
        public static string OpenAndPlay()
        {
            if (!File.Exists(ScenePath)) Build();
            EditorSceneManager.OpenScene(ScenePath, OpenSceneMode.Single);
            EditorApplication.isPlaying = true;
            return "playing " + ScenePath;
        }

        static IEnumerable<string> Dirs(string root)
        {
            if (!Directory.Exists(root)) yield break;
            foreach (var d in Directory.GetDirectories(root)) yield return d.Replace('\\', '/');
        }
    }
}
