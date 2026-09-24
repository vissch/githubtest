// Phase: B6 (implemented) — builds what the shell loads: ShellAssets.asset, the mission catalog with its two cards,
// the main-menu backdrop plate, and the MainMenu scene registered with Bootstrap pointing at it.
// TW/UI/Build Shell Assets is idempotent: existing assets are updated in place, existing mission cards are left as
// the designer edited them. TW/UI/Build Main Menu Scene writes Scenes/MainMenu.unity (camera and light only; the
// menu is a UI panel), re-targets Bootstrap.unity at it, and registers [Bootstrap, MainMenu, GreyboxCorridor] in the
// build settings without regenerating GreyboxCorridor (which would reset SimHost's serialized fields).
using System.IO;
using UnityEditor;
using UnityEditor.SceneManagement;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.UI;

namespace TW.Editor
{
    public static class ShellAssetsBuilder
    {
        const string ShellFolder = "Assets/_Project/UI/Shell";
        const string MissionsFolder = "Assets/_Project/UI/Missions";
        const string ShellAssetPath = "Assets/_Project/UI/Resources/ShellAssets.asset";
        const string CatalogPath = MissionsFolder + "/MissionCatalog.asset";
        const string BackdropPath = ShellFolder + "/backdrop_menu.png";
        const string ScenesDir = "Assets/_Project/Scenes";

        [MenuItem("TW/UI/Build Shell Assets")]
        public static ShellAssets Build()
        {
            Folder("Assets/_Project/UI/Resources"); Folder(MissionsFolder);
            var backdrop = EnsureBackdrop();
            var catalog = EnsureCatalog();
            var assets = AssetDatabase.LoadAssetAtPath<ShellAssets>(ShellAssetPath);
            bool fresh = assets == null;
            if (fresh) assets = ScriptableObject.CreateInstance<ShellAssets>();
            assets.Panel = AssetDatabase.LoadAssetAtPath<PanelSettings>(UiAssetBuilder.PanelPath);
            assets.MainMenu = Tree("MainMenu"); assets.MissionSelect = Tree("MissionSelect"); assets.PauseMenu = Tree("PauseMenu");
            assets.Settings = Tree("Settings"); assets.Debrief = Tree("Debrief");
            assets.Backdrop = backdrop; assets.Catalog = catalog;
            if (assets.Panel == null) Debug.LogWarning("ShellAssetsBuilder: no PanelSettings yet (TW/UI/Create Panel Settings).");
            if (fresh) AssetDatabase.CreateAsset(assets, ShellAssetPath); else EditorUtility.SetDirty(assets);
            AssetDatabase.SaveAssets();
            Debug.Log($"ShellAssetsBuilder: {(fresh ? "created" : "updated")} {ShellAssetPath}");
            return assets;
        }

        static VisualTreeAsset Tree(string name)
        {
            var t = AssetDatabase.LoadAssetAtPath<VisualTreeAsset>($"{ShellFolder}/{name}.uxml");
            if (t == null) Debug.LogWarning($"ShellAssetsBuilder: {ShellFolder}/{name}.uxml missing or not imported.");
            return t;
        }

        static MissionCatalog EnsureCatalog()
        {
            var a = Card("ShelledWood1917", c =>
            {
                c.Id = "shelled-wood-1917"; c.Title = "SHELLED WOOD, 1917"; c.Subtitle = "THE COAST"; c.BattlefieldSeed = 1917;
                c.Description = "A wood shelled to stumps, a river with fords and one bridge, and beyond the enemy line the sea, where your reinforcements land by boat under the guns.";
            });
            var b = Card("ShelledWoodInland", c =>
            {
                c.Id = "shelled-wood-inland"; c.Title = "SHELLED WOOD, INLAND"; c.Subtitle = "THE RIVER"; c.BattlefieldSeed = 1916;
                c.Description = "The same shattered wood a mile inland: no coast, no boats, reinforcements walk up from the rear.";
            });
            var w = Card("WinterLine", c =>
            {
                c.Id = "winter-line"; c.Title = "THE WINTER LINE"; c.Subtitle = "THE HIGH GROUND"; c.BattlefieldSeed = 1917;
                c.Ground = TW.Presentation.Ground.WinterLine;
                c.FrontLine = "110 x 240 M";
                c.Description = "A long frozen position with no river and no flooded ground: what water there is has frozen hard enough to walk on, and the cold is the only thing either side agrees about.";
            });
            var catalog = AssetDatabase.LoadAssetAtPath<MissionCatalog>(CatalogPath);
            bool fresh = catalog == null;
            if (fresh) catalog = ScriptableObject.CreateInstance<MissionCatalog>();
            if (catalog.Cards == null || catalog.Cards.Length == 0) catalog.Cards = new[] { a, b, w };
            else if (System.Array.IndexOf(catalog.Cards, w) < 0)
            {
                // a designer's edited catalog is left alone EXCEPT that a new card is appended, or building the
                // assets would silently leave the winter level unreachable on every machine but a fresh one
                var grown = new MissionCard[catalog.Cards.Length + 1];
                System.Array.Copy(catalog.Cards, grown, catalog.Cards.Length);
                grown[catalog.Cards.Length] = w; catalog.Cards = grown; EditorUtility.SetDirty(catalog);
            }
            if (fresh) AssetDatabase.CreateAsset(catalog, CatalogPath); else EditorUtility.SetDirty(catalog);
            return catalog;
        }

        static MissionCard Card(string file, System.Action<MissionCard> init)
        {
            string path = $"{MissionsFolder}/{file}.asset";
            var c = AssetDatabase.LoadAssetAtPath<MissionCard>(path);
            if (c != null) return c;   // a designer may have edited it: leave it alone
            c = ScriptableObject.CreateInstance<MissionCard>();
            init(c);
            AssetDatabase.CreateAsset(c, path);
            return c;
        }

        /// <summary>A 960x540 dark plate: night sky into churned ground with an amber glow on the horizon, grained.</summary>
        static Texture2D EnsureBackdrop()
        {
            string full = UiSkinGenerator.FullPath(BackdropPath);
            if (!File.Exists(full))
            {
                const int W = 960, H = 540;
                var c = new UiSkinGenerator.Canvas(W, H);
                var sky = new Color32(0x0E, 0x0F, 0x12, 255); var haze = new Color32(0x3A, 0x22, 0x18, 255); var ground = new Color32(0x14, 0x14, 0x16, 255);
                int horizon = (int)(H * 0.58f);
                c.GradientV(0, 0, W, horizon, sky, haze);
                c.GradientV(0, horizon, W, H - horizon, new Color32(0x24, 0x1C, 0x18, 255), ground);
                for (int x = 0; x < W; x++)   // a broken skyline of stumps and ruins
                {
                    uint h = UiSkinGenerator.Canvas.Hash(x / 6, 0, 3);
                    int height = 4 + (int)(h % 26); if ((h >> 8) % 7 == 0) height += 30;
                    c.Fill(x, horizon - height, 1, height + 2, new Color32(0x0A, 0x0B, 0x0C, 255));
                }
                c.Grain(0, 0, W, H, 0, 0.12f, 11, true);
                var t = new Texture2D(W, H, TextureFormat.RGBA32, false); t.SetPixels32(c.Px); t.Apply();
                File.WriteAllBytes(full, t.EncodeToPNG()); Object.DestroyImmediate(t);
                AssetDatabase.ImportAsset(BackdropPath, ImportAssetOptions.ForceUpdate);
                var imp = AssetImporter.GetAtPath(BackdropPath) as TextureImporter;
                if (imp != null) { imp.textureType = TextureImporterType.Default; imp.mipmapEnabled = false; imp.textureCompression = TextureImporterCompression.Compressed; imp.maxTextureSize = 1024; imp.SaveAndReimport(); }
            }
            return AssetDatabase.LoadAssetAtPath<Texture2D>(BackdropPath);
        }

        static void Folder(string assetFolder)
        {
            string full = UiSkinGenerator.FullPath(assetFolder);
            if (!Directory.Exists(full)) { Directory.CreateDirectory(full); AssetDatabase.Refresh(); }
        }

        // ---- scenes ---------------------------------------------------------------------------------------------------
        [MenuItem("TW/UI/Build Main Menu Scene")]
        public static void BuildMainMenuScene()
        {
            var setup = EditorSceneManager.GetSceneManagerSetup();
            string menu = $"{ScenesDir}/MainMenu.unity";
            var scene = EditorSceneManager.NewScene(NewSceneSetup.DefaultGameObjects, NewSceneMode.Single);
            var cam = Camera.main;
            if (cam != null) { cam.clearFlags = CameraClearFlags.SolidColor; cam.backgroundColor = new Color(0.055f, 0.059f, 0.063f); cam.cullingMask = 0; }
            EditorSceneManager.SaveScene(scene, menu);

            string bootstrap = $"{ScenesDir}/Bootstrap.unity";
            if (File.Exists(UiSkinGenerator.FullPath(bootstrap)))
            {
                var bs = EditorSceneManager.OpenScene(bootstrap, OpenSceneMode.Single);
                var loader = Object.FindFirstObjectByType<BootstrapLoader>();
                if (loader != null && loader.SceneName != MatchLaunch.MenuScene) { loader.SceneName = MatchLaunch.MenuScene; EditorSceneManager.MarkSceneDirty(bs); EditorSceneManager.SaveScene(bs); }
            }
            string greybox = $"{ScenesDir}/GreyboxCorridor.unity";
            EditorBuildSettings.scenes = new[]
            {
                new EditorBuildSettingsScene(bootstrap, true),
                new EditorBuildSettingsScene(menu, true),
                new EditorBuildSettingsScene(greybox, true),
            };
            if (setup != null && setup.Length > 0) EditorSceneManager.RestoreSceneManagerSetup(setup);
            Debug.Log("ShellAssetsBuilder: MainMenu.unity built, Bootstrap targets MainMenu, build settings [Bootstrap, MainMenu, GreyboxCorridor].");
        }

        [MenuItem("TW/UI/Build All Shell (assets + scene)")]
        public static void BuildAll() { Build(); BuildMainMenuScene(); }
    }
}
