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

        /// <summary>Every importer setting Apply writes, in one place so the verifier reads the same numbers the
        /// import writes and the two cannot drift apart.</summary>
        public struct SpriteRules
        {
            public TextureImporterType TextureType;
            public SpriteImportMode SpriteImportMode;
            public float SpritePixelsPerUnit;
            public bool SRGBTexture;
            public TextureImporterAlphaSource AlphaSource;
            public bool AlphaIsTransparency;
            public bool MipmapEnabled;
            public bool IsReadable;
            public TextureImporterCompression TextureCompression;
            public int MaxTextureSize;
            public TextureWrapMode WrapMode;
            public FilterMode FilterMode;
            public TextureImporterNPOTScale NpotScale;
            public SpriteMeshType SpriteMeshType;
            public bool SpriteGenerateFallbackPhysicsShape;
            public uint SpriteExtrude;
        }

        public static readonly SpriteRules Rules = new SpriteRules
        {
            TextureType = TextureImporterType.Sprite,
            SpriteImportMode = SpriteImportMode.Single,
            SpritePixelsPerUnit = 100f,
            SRGBTexture = true,
            AlphaSource = TextureImporterAlphaSource.FromInput,
            AlphaIsTransparency = true,
            MipmapEnabled = false,
            IsReadable = false,
            TextureCompression = TextureImporterCompression.Uncompressed,
            MaxTextureSize = 2048,
            WrapMode = TextureWrapMode.Clamp,
            FilterMode = FilterMode.Bilinear,
            NpotScale = TextureImporterNPOTScale.None,
            SpriteMeshType = SpriteMeshType.FullRect,
            SpriteGenerateFallbackPhysicsShape = false,
            SpriteExtrude = 0,
        };

        /// <summary>The rules, callable from the verifier and from tests as well as from the import.</summary>
        public static void Apply(TextureImporter t, string assetPath)
        {
            var r = Rules;
            t.textureType = r.TextureType;
            t.spriteImportMode = r.SpriteImportMode;
            t.spritePixelsPerUnit = r.SpritePixelsPerUnit;
            t.sRGBTexture = r.SRGBTexture;
            t.alphaSource = r.AlphaSource;
            t.alphaIsTransparency = r.AlphaIsTransparency;
            t.mipmapEnabled = r.MipmapEnabled;
            t.isReadable = r.IsReadable;
            t.textureCompression = r.TextureCompression;
            t.maxTextureSize = r.MaxTextureSize;
            t.wrapMode = r.WrapMode;
            t.filterMode = r.FilterMode;
            t.npotScale = r.NpotScale;
            var s = new TextureImporterSettings();
            t.ReadTextureSettings(s);
            s.spriteMeshType = r.SpriteMeshType;
            s.spriteGenerateFallbackPhysicsShape = r.SpriteGenerateFallbackPhysicsShape;
            s.spriteExtrude = r.SpriteExtrude;
            t.SetTextureSettings(s);
            string rel = assetPath.Replace('\\', '/').Substring(SkinSpec.Root.Length);
            if (SkinSpec.TryGet(rel, out var e) && e.HasBorder) t.spriteBorder = new Vector4(e.L, e.B, e.R, e.T);
            else t.spriteBorder = Vector4.zero;
        }

        /// <summary>Reads a sprite importer's actual settings into the same shape <see cref="Rules"/> uses, so the
        /// verifier can compare field by field.</summary>
        public static SpriteRules Read(TextureImporter t)
        {
            var s = new TextureImporterSettings();
            t.ReadTextureSettings(s);
            return new SpriteRules
            {
                TextureType = t.textureType,
                SpriteImportMode = t.spriteImportMode,
                SpritePixelsPerUnit = t.spritePixelsPerUnit,
                SRGBTexture = t.sRGBTexture,
                AlphaSource = t.alphaSource,
                AlphaIsTransparency = t.alphaIsTransparency,
                MipmapEnabled = t.mipmapEnabled,
                IsReadable = t.isReadable,
                TextureCompression = t.textureCompression,
                MaxTextureSize = t.maxTextureSize,
                WrapMode = t.wrapMode,
                FilterMode = t.filterMode,
                NpotScale = t.npotScale,
                SpriteMeshType = s.spriteMeshType,
                SpriteGenerateFallbackPhysicsShape = s.spriteGenerateFallbackPhysicsShape,
                SpriteExtrude = s.spriteExtrude,
            };
        }
    }
}
