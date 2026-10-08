// Phase: B6 (implemented) — the three DustFront font assets are really baked (glyphs or a source font to bake them
// from at runtime), a skin label resolves to the baked face and not TextMeshPro's fallback, and a bake keeps each
// asset's GUID instead of handing every skin label a new one (F13).
using System.IO;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEditor;
using UnityEngine;
using UnityEngine.TextCore.Text;
using TW.Editor;
using TW.UI;

namespace TW.Tests
{
    public class SkinFontTests
    {
        static string ProjectDataPath => Application.dataPath;

        [Test]
        public void Every_DustFront_Font_Holds_Glyphs_Or_A_Source_Font()
        {
            foreach (var f in SkinSpec.Fonts)
            {
                var fa = AssetDatabase.LoadAssetAtPath<FontAsset>(SkinSpec.Root + f);
                Assert.That(fa, Is.Not.Null, $"[F1] {SkinSpec.Root}{f} did not load as a FontAsset");
                bool hasGlyphs = fa.characterTable != null && fa.characterTable.Count > 0;
                bool hasSource = fa.sourceFontFile != null;
                Assert.That(hasGlyphs || hasSource, Is.True,
                    $"[F1] {f} has no glyphs and no source font: a skin label naming this asset falls back to TextMeshPro's default face instead of DustFront");
            }
        }

        static readonly Regex Url = new Regex("url\\(\\s*[\"']?([^\"')]+)[\"']?\\s*\\)", RegexOptions.Compiled);

        [Test]
        public void Every_Skin_Font_Url_Resolves_To_A_Baked_DustFront_Asset()
        {
            string full = Path.GetFullPath(Path.Combine(ProjectDataPath, "..", SkinSpec.Root, "dustfront.components.uss"));
            string uss = File.ReadAllText(full);
            int checkedUrls = 0;
            foreach (Match m in Url.Matches(uss))
            {
                string u = m.Groups[1].Value;
                if (!u.EndsWith(".asset")) continue;
                string rel = u.StartsWith("/") ? u.Substring(1).Replace("Assets/_Project/UI/Skin/", "") : u;
                string path = SkinSpec.Root + rel;
                Assert.That(System.Array.IndexOf(SkinSpec.Fonts, rel), Is.GreaterThanOrEqualTo(0),
                    $"[F1] {path} is named by dustfront.components.uss but not listed in SkinSpec.Fonts");
                var fa = AssetDatabase.LoadAssetAtPath<FontAsset>(path);
                Assert.That(fa, Is.Not.Null, $"[F1] {path} did not load as a FontAsset");
                bool resolvesToBakedFace = (fa.characterTable != null && fa.characterTable.Count > 0) || fa.sourceFontFile != null;
                Assert.That(resolvesToBakedFace, Is.True, $"[F1] a label using {u} resolves to {path}, which has no glyphs and no source font, so it falls back to TextMeshPro's default face");
                checkedUrls++;
            }
            Assert.That(checkedUrls, Is.GreaterThan(0), "no font url(...) found in dustfront.components.uss; the test did not check anything");
        }

        [Test]
        public void A_Bake_Keeps_The_Font_Asset_Guids()
        {
            var expected = new[]
            {
                ("Fonts/DustFrontDisplay.asset", "0c90c973bb5f96f4f98222cb1d4d4107"),
                ("Fonts/DustFrontLabel.asset", "e98e8e9360bd1d84f921715be32de5e7"),
                ("Fonts/DustFrontMono.asset", "05fed1701ff04b54b8491c997ca5e375"),
            };
            foreach (var (file, guid) in expected)
                Assert.That(AssetDatabase.AssetPathToGUID(SkinSpec.Root + file), Is.EqualTo(guid), $"[F13] {file}'s GUID changed: a bake must keep it, not delete+recreate the asset");

            string src = File.ReadAllText(Path.Combine(ProjectDataPath, "_Project", "Editor", "UI", "UiAssetBuilder.cs"));
            Assert.That(src, Does.Not.Contain("AssetDatabase.DeleteAsset(assetPath)"),
                "[F13] UiAssetBuilder.BakeFonts still deletes the asset before recreating it, which hands it a new GUID on every bake");
        }
    }
}
