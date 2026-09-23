// Phase: B6 (implemented) — the assets the interface cannot run without and that no PNG can carry: the PanelSettings
// every screen shares, and the three TextCore font assets the skin's classes name.
// TW/UI/Create Panel Settings writes Assets/_Project/UI/Resources/UI/DustFrontPanel.asset (scale with screen size against
// 1920x1080, matched on height, themed by DustFront.tss) and updates it in place when it exists.
// TW/UI/Bake Skin Fonts copies the two OFL/Apache faces the Editor ships (Inter SemiBold, Roboto Mono Bold) with their
// licences into UI/Skin/Fonts and bakes DustFrontDisplay / DustFrontLabel / DustFrontMono .asset from them. Swapping the
// final stencil face is: drop the TTF over the placeholder name and run this again.
using System.IO;
using UnityEditor;
using UnityEngine;
using UnityEngine.TextCore.LowLevel;
using UnityEngine.TextCore.Text;
using UnityEngine.UIElements;
using TW.UI;

namespace TW.Editor
{
    public static class UiAssetBuilder
    {
        public const string ResourcesFolder = "Assets/_Project/UI/Resources/UI";
        public const string PanelPath = ResourcesFolder + "/DustFrontPanel.asset";
        public const string ThemePath = ResourcesFolder + "/DustFront.tss";
        public const string FontsFolder = SkinSpec.Root + "Fonts";

        [MenuItem("TW/UI/Create Panel Settings")]
        public static PanelSettings CreatePanelSettings()
        {
            EnsureFolder(ResourcesFolder);
            var theme = AssetDatabase.LoadAssetAtPath<ThemeStyleSheet>(ThemePath);
            if (theme == null) Debug.LogWarning($"UiAssetBuilder: {ThemePath} is missing or not yet imported; the panel has no theme until it is.");
            var ps = AssetDatabase.LoadAssetAtPath<PanelSettings>(PanelPath);
            bool fresh = ps == null;
            if (fresh) ps = ScriptableObject.CreateInstance<PanelSettings>();
            ps.themeStyleSheet = theme;
            ps.scaleMode = PanelScaleMode.ScaleWithScreenSize;
            ps.referenceResolution = new Vector2Int(1920, 1080);
            ps.screenMatchMode = PanelScreenMatchMode.MatchWidthOrHeight;
            ps.match = 1f;
            ps.clearColor = false;
            ps.sortingOrder = 0;
            ps.targetTexture = null;
            if (fresh) AssetDatabase.CreateAsset(ps, PanelPath); else EditorUtility.SetDirty(ps);
            AssetDatabase.SaveAssets();
            Debug.Log($"UiAssetBuilder: {(fresh ? "created" : "updated")} {PanelPath}");
            return ps;
        }

        // ---- fonts ------------------------------------------------------------------------------------------------
        static string EditorFonts => Path.Combine(EditorApplication.applicationContentsPath, "Resources", "Fonts");

        struct FontJob { public string Ttf, Licence, Asset; public int Size, Padding, Atlas; public GlyphRenderMode Mode; }

        static readonly FontJob[] Jobs =
        {
            new FontJob { Ttf = "Inter-SemiBold.ttf", Licence = "Inter-LICENSE.txt", Asset = "DustFrontDisplay.asset", Size = 96, Padding = 10, Atlas = 2048, Mode = GlyphRenderMode.SDFAA },
            new FontJob { Ttf = "Inter-SemiBold.ttf", Licence = "Inter-LICENSE.txt", Asset = "DustFrontLabel.asset", Size = 64, Padding = 8, Atlas = 1024, Mode = GlyphRenderMode.SDFAA },
            new FontJob { Ttf = "RobotoMono-Bold.ttf", Licence = "RobotoMono-LICENSE.txt", Asset = "DustFrontMono.asset", Size = 64, Padding = 8, Atlas = 1024, Mode = GlyphRenderMode.SDFAA },
        };

        /// <summary>ASCII plus the Latin-1 letters the unit and place names may need.</summary>
        static string Characters()
        {
            var sb = new System.Text.StringBuilder();
            for (int c = 32; c < 127; c++) sb.Append((char)c);
            for (int c = 0xA0; c <= 0xFF; c++) sb.Append((char)c);
            sb.Append("–—‘’“”•…×");   // dashes, quotes, bullet, ellipsis, multiply
            return sb.ToString();
        }

        [MenuItem("TW/UI/Bake Skin Fonts")]
        public static void BakeFonts()
        {
            EnsureFolder(FontsFolder);
            foreach (var job in Jobs)
            {
                string ttfAsset = FontsFolder + "/" + job.Ttf;
                if (!File.Exists(UiSkinGenerator.FullPath(ttfAsset)))
                {
                    string src = Path.Combine(EditorFonts, job.Ttf);
                    if (!File.Exists(src)) { Debug.LogError($"UiAssetBuilder: {src} not found; drop a TTF at {ttfAsset} and run again."); continue; }
                    File.Copy(src, UiSkinGenerator.FullPath(ttfAsset));
                    string lic = Path.Combine(EditorFonts, job.Licence);
                    if (File.Exists(lic) && !File.Exists(UiSkinGenerator.FullPath(FontsFolder + "/" + job.Licence))) File.Copy(lic, UiSkinGenerator.FullPath(FontsFolder + "/" + job.Licence));
                    AssetDatabase.ImportAsset(ttfAsset, ImportAssetOptions.ForceUpdate);
                }
                var font = AssetDatabase.LoadAssetAtPath<Font>(ttfAsset);
                if (font == null) { Debug.LogError($"UiAssetBuilder: {ttfAsset} did not import as a Font."); continue; }
                string assetPath = FontsFolder + "/" + job.Asset;
                var fa = FontAsset.CreateFontAsset(font, job.Size, job.Padding, job.Mode, job.Atlas, job.Atlas, AtlasPopulationMode.Static, false);
                if (fa == null) { Debug.LogError($"UiAssetBuilder: could not create a font asset from {ttfAsset}."); continue; }
                fa.TryAddCharacters(Characters(), out string missing);
                if (!string.IsNullOrEmpty(missing)) Debug.Log($"UiAssetBuilder: {job.Asset} lacks {missing.Length} glyph(s): {missing}");
                fa.name = Path.GetFileNameWithoutExtension(job.Asset);
                if (fa.material != null) fa.material.name = fa.name + " Material";
                if (File.Exists(UiSkinGenerator.FullPath(assetPath))) AssetDatabase.DeleteAsset(assetPath);
                AssetDatabase.CreateAsset(fa, assetPath);
                if (fa.material != null) AssetDatabase.AddObjectToAsset(fa.material, fa);
                if (fa.atlasTextures != null) for (int i = 0; i < fa.atlasTextures.Length; i++) if (fa.atlasTextures[i] != null) { fa.atlasTextures[i].name = fa.name + " Atlas " + i; AssetDatabase.AddObjectToAsset(fa.atlasTextures[i], fa); }
                EditorUtility.SetDirty(fa);
                Debug.Log($"UiAssetBuilder: baked {assetPath} from {job.Ttf} ({job.Size} pt, {job.Atlas} px)");
            }
            AssetDatabase.SaveAssets();
            AssetDatabase.Refresh();
        }

        [MenuItem("TW/UI/Build All UI Assets")]
        public static void BuildAll()
        {
            UiSkinGenerator.Generate(false);
            BakeFonts();
            ReimportSheets();
            CreatePanelSettings();
        }

        /// <summary>A style sheet resolves its url() references when it is imported, so a sheet imported before the
        /// sprites and fonts it names existed carries "invalid asset path" warnings until it is imported again.</summary>
        [MenuItem("TW/UI/Reimport Skin Sheets")]
        public static void ReimportSheets()
        {
            foreach (var sheet in UiSkinVerifier.Sheets) AssetDatabase.ImportAsset(SkinSpec.Root + sheet, ImportAssetOptions.ForceUpdate);
            AssetDatabase.ImportAsset(ThemePath, ImportAssetOptions.ForceUpdate);
            foreach (var guid in AssetDatabase.FindAssets("t:StyleSheet", new[] { "Assets/_Project/UI" }))
                AssetDatabase.ImportAsset(AssetDatabase.GUIDToAssetPath(guid), ImportAssetOptions.ForceUpdate);
            AssetDatabase.SaveAssets();
        }

        static void EnsureFolder(string assetFolder)
        {
            string full = UiSkinGenerator.FullPath(assetFolder);
            if (!Directory.Exists(full)) { Directory.CreateDirectory(full); AssetDatabase.Refresh(); }
        }
    }
}
