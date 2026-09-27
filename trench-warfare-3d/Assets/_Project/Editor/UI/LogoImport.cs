// Phase: B6 (implemented) — import rules for the game logo's layers (Resources/Logo, loaded as Texture2D by GameLogo):
// straight alpha, sRGB, no power-of-two rescale (layout.json places each layer by its pixel size), mip maps because the
// wordmark is shown far smaller than it is drawn, high-quality compression, clamped. A new cut of a layer dropped over
// the old one keeps these. Pattern: UiSkinImport.
using UnityEditor;
using UnityEngine;

namespace TW.Editor
{
    public sealed class LogoImport : AssetPostprocessor
    {
        public const string Root = "Assets/_Project/UI/Resources/Logo/";
        public override uint GetVersion() => 1;

        void OnPreprocessTexture()
        {
            if (!assetPath.Replace('\\', '/').StartsWith(Root)) return;
            var t = (TextureImporter)assetImporter;
            t.textureType = TextureImporterType.Default;
            t.sRGBTexture = true;
            t.alphaSource = TextureImporterAlphaSource.FromInput;
            t.alphaIsTransparency = true;
            t.npotScale = TextureImporterNPOTScale.None;
            t.mipmapEnabled = true;
            t.wrapMode = TextureWrapMode.Clamp;
            t.filterMode = FilterMode.Trilinear;
            t.maxTextureSize = 2048;
            t.textureCompression = TextureImporterCompression.CompressedHQ;
        }
    }
}
