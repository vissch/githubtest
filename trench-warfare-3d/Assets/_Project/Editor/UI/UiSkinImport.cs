// Phase: B6 (implemented) — import rules for the interface skin, so an artist's PNG needs no Inspector visit.
// Every texture under Assets/_Project/UI/Skin/ becomes a UI sprite (Single, Full Rect, uncompressed, no mips, sRGB,
// alpha is transparency, clamp, bilinear) and, when SkinSpec knows the file, gets the 9-slice border the spec gives
// it. Drop a replacement over a placeholder with the same name and the borders, type and compression are right
// before anyone opens the Inspector; UiSkinVerifier checks the result against the same table. Pattern: TankImport.
using UnityEditor;
using UnityEngine;
using TW.UI;

namespace TW.Editor
{
    public sealed class UiSkinImport : AssetPostprocessor
    {
        public override uint GetVersion() => 2;

        public const string UnitArtRoot = "Assets/_Project/UI/Resources/UnitArt/";
        static bool Ours(string path) => path.Replace('\\', '/').StartsWith(SkinSpec.Root);
        static bool UnitArtFile(string path) => path.Replace('\\', '/').StartsWith(UnitArtRoot);

        void OnPreprocessTexture()
        {
            if (UnitArtFile(assetPath)) { ApplyUnitArt((TextureImporter)assetImporter); return; }
            if (!Ours(assetPath)) return;
            var t = (TextureImporter)assetImporter;
            Apply(t, assetPath);
        }

        /// <summary>Unit artwork (cutouts and state faces, loaded as Texture2D from Resources): straight alpha, no mips (the
        /// UI never minifies them far), high-quality compression; every file is 256 or 512 square, so BC7 applies.</summary>
        public static void ApplyUnitArt(TextureImporter t)
        {
            t.textureType = TextureImporterType.Default;
            t.sRGBTexture = true;
            t.alphaSource = TextureImporterAlphaSource.FromInput;
            t.alphaIsTransparency = true;
            t.mipmapEnabled = false;
            t.isReadable = false;
            t.textureCompression = TextureImporterCompression.CompressedHQ;
            t.maxTextureSize = 1024;
            t.wrapMode = TextureWrapMode.Clamp;
            t.filterMode = FilterMode.Bilinear;
        }

        /// <summary>The rules, callable from the verifier and from tests as well as from the import.</summary>
        public static void Apply(TextureImporter t, string assetPath)
        {
            t.textureType = TextureImporterType.Sprite;
            t.spriteImportMode = SpriteImportMode.Single;
            t.spritePixelsPerUnit = 100f;
            t.sRGBTexture = true;
            t.alphaSource = TextureImporterAlphaSource.FromInput;
            t.alphaIsTransparency = true;
            t.mipmapEnabled = false;
            t.isReadable = false;
            t.textureCompression = TextureImporterCompression.Uncompressed;
            t.maxTextureSize = 2048;
            t.wrapMode = TextureWrapMode.Clamp;
            t.filterMode = FilterMode.Bilinear;
            t.npotScale = TextureImporterNPOTScale.None;
            var s = new TextureImporterSettings();
            t.ReadTextureSettings(s);
            s.spriteMeshType = SpriteMeshType.FullRect;
            s.spriteGenerateFallbackPhysicsShape = false;
            s.spriteExtrude = 0;
            t.SetTextureSettings(s);
            string rel = assetPath.Replace('\\', '/').Substring(SkinSpec.Root.Length);
            if (SkinSpec.TryGet(rel, out var e) && e.HasBorder) t.spriteBorder = new Vector4(e.L, e.B, e.R, e.T);
            else t.spriteBorder = Vector4.zero;
        }
    }
}
