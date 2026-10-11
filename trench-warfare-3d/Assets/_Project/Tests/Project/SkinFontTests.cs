// Phase: B6 (implemented) — the three DustFront font assets are really baked (glyphs or a source font to bake them
// from at runtime), a skin label resolves to the baked face and not TextMeshPro's fallback, and a bake keeps each
// asset's GUID instead of handing every skin label a new one (F13).
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEditor;
using UnityEngine;
using UnityEngine.TextCore.Text;
using UnityEngine.UIElements;
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
        }

        // [F13b] the test above only pinned the committed GUIDs and grepped UiAssetBuilder's source for one literal
        // delete call, so a delete written any other way stayed green. This one runs the real bake twice and checks
        // after each pass that the three font assets keep their GUID, their main object id and their .meta file.
        // Its reach, measured 2026-10-11: injecting AssetDatabase.DeleteAsset(assetPath) at the top of
        // WriteFontAsset (with and without a Refresh + SaveAssets after it) leaves this test green, because the
        // editor hands a path deleted and recreated inside one session its old GUID and main object id back. What
        // this pins is the whole bake twice over, not only the one delete call the grep it replaced looked for.
        [Test]
        [Category("Long")]
        public void A_Second_Bake_Keeps_The_Font_Asset_Identity()
        {
            var before = new List<(string File, string Guid, long Id)>();
            foreach (var f in SkinSpec.Fonts)
            {
                string path = SkinSpec.Root + f;
                var fa = AssetDatabase.LoadAssetAtPath<FontAsset>(path);
                Assert.That(fa, Is.Not.Null, $"[F13b] {path} did not load as a FontAsset");
                Assert.That(AssetDatabase.TryGetGUIDAndLocalFileIdentifier(fa, out string guid, out long id), Is.True,
                    $"[F13b] {path} has no GUID + local file id, so the test cannot tell whether a bake keeps them");
                before.Add((f, guid, id));
            }
            var metaBefore = MetaLinesOfSkinFonts();

            // a bake rewrites ~13 MB of .asset: keep the bytes so the leg never commits a re-baked asset.
            var saved = new Dictionary<string, byte[]>();
            foreach (var f in SkinSpec.Fonts)
                foreach (string rel in new[] { SkinSpec.Root + f, SkinSpec.Root + f + ".meta" })
                    saved[rel] = File.ReadAllBytes(Path.GetFullPath(Path.Combine(ProjectDataPath, "..", rel)));

            try
            {
                for (int pass = 1; pass <= 2; pass++)
                {
                    UiAssetBuilder.BakeFonts();
                    AssetDatabase.Refresh();

                    foreach (var (file, guid, id) in before)
                    {
                        string path = SkinSpec.Root + file;
                        Assert.That(AssetDatabase.AssetPathToGUID(path), Is.EqualTo(guid),
                            $"[F13b] bake pass {pass} changed {file}'s GUID: every skin label naming it now points at nothing");
                        var fa = AssetDatabase.LoadAssetAtPath<FontAsset>(path);
                        Assert.That(fa, Is.Not.Null, $"[F13b] bake pass {pass} left {path} unloadable as a FontAsset");
                        Assert.That(AssetDatabase.TryGetGUIDAndLocalFileIdentifier(fa, out string _, out long nowId), Is.True,
                            $"[F13b] bake pass {pass} left {path} without a GUID + local file id");
                        Assert.That(nowId, Is.EqualTo(id),
                            $"[F13b] bake pass {pass} changed {file}'s main object id ({id} -> {nowId}): a reference to the asset's object breaks even though the GUID held");
                    }
                    Assert.That(MetaLinesOfSkinFonts(), Is.EqualTo(metaBefore),
                        $"[F13b] bake pass {pass} changed a .meta file under UI/Skin/Fonts, so the bake does not keep the assets' identity");
                }
            }
            finally
            {
                foreach (var kv in saved)
                {
                    File.WriteAllBytes(Path.GetFullPath(Path.Combine(ProjectDataPath, "..", kv.Key)), kv.Value);
                    if (!kv.Key.EndsWith(".meta")) AssetDatabase.ImportAsset(kv.Key, ImportAssetOptions.ForceUpdate);
                }
                AssetDatabase.Refresh();
            }
        }

        /// <summary>The .meta lines of `git status` under UI/Skin/Fonts: a bake that keeps the assets' identity
        /// touches none of them, whether or not the tree already carries an uncommitted bake.</summary>
        static List<string> MetaLinesOfSkinFonts()
        {
            var lines = new List<string>();
            foreach (string line in GitStatusOfSkinFonts().Split('\n'))
            {
                string t = line.Trim();
                if (t.EndsWith(".meta")) lines.Add(t);
            }
            lines.Sort();
            return lines;
        }
        // [F14c] the skin's font assets are Dynamic (AtlasPopulationMode.Dynamic), so in the editor a face that is
        // asked for a character it does not hold writes that glyph into the tracked .asset and dirties the tree. A UI
        // Toolkit text field draws U+200B, the zero-width space, when it is empty, so the bake's character list must
        // already hold it; and the bake's list and the assets' character tables must be the same set of code points.
        [Test]
        public void Skin_Fonts_Hold_Every_Character_The_Skin_Asks_For()
        {
            var faces = new List<(string File, FontAsset Fa)>();
            foreach (var f in SkinSpec.Fonts)
            {
                var fa = AssetDatabase.LoadAssetAtPath<FontAsset>(SkinSpec.Root + f);
                Assert.That(fa, Is.Not.Null, $"[F14c] {SkinSpec.Root}{f} did not load as a FontAsset");
                faces.Add((f, fa));
            }

            // a. the character the F10 note box asks for.
            foreach (var (file, fa) in faces)
                Assert.That(fa.HasCharacter('​', false, false), Is.True,
                    $"[F14c] {file} lacks U+200B, the zero-width space a UI Toolkit text field draws when it is empty, so a Dynamic font asset writes it into the tracked .asset");

            // b. the ask itself: an empty, focused text field drawn with each skin face must leave the tracked files
            // under UI/Skin/Fonts exactly as they were. Before/after, not "clean": this test must also hold on a tree
            // that carries an uncommitted bake.
            string statusBefore = GitStatusOfSkinFonts();
            var bytesBefore = SkinFontBytes();
            foreach (var (_, fa) in faces)
            {
                var field = new TextField { value = string.Empty };
                field.style.unityFontDefinition = FontDefinition.FromSDFFont(fa);
                field.Focus();
                field.MeasureTextSize(string.Empty, 0f, VisualElement.MeasureMode.Undefined, 0f, VisualElement.MeasureMode.Undefined);
            }
            foreach (var (file, fa) in faces)
                Assert.That(EditorUtility.IsDirty(fa), Is.False,
                    $"[F14c] {file} is dirty after an empty text field was drawn with it: the editor wrote a glyph the bake's list does not hold into the tracked .asset");
            Assert.That(SkinFontBytes(), Is.EqualTo(bytesBefore),
                "[F14c] drawing an empty text field with the skin faces rewrote a tracked file under UI/Skin/Fonts, so no gate can record a tree");
            Assert.That(GitStatusOfSkinFonts(), Is.EqualTo(statusBefore),
                "[F14c] drawing an empty text field with the skin faces changed git's view of UI/Skin/Fonts, so no gate can record a tree");

            // c. the bake's list and each asset's character table are the same set of code points.
            var wanted = new HashSet<int>();
            foreach (char c in UiAssetBuilder.Characters()) wanted.Add(c);
            foreach (var (file, fa) in faces)
            {
                var have = new HashSet<int>();
                foreach (var e in fa.characterTable) have.Add((int)e.unicode);
                var missing = new List<int>(wanted); missing.RemoveAll(have.Contains);
                Assert.That(missing, Is.Empty,
                    $"[F14c] {file}'s character table lacks {missing.Count} code point(s) of the bake's list ({CodePoints(missing)}): a bake that drops one leaves the Dynamic asset to write them at draw time");
                var extra = new List<int>(have); extra.RemoveAll(wanted.Contains);
                Assert.That(extra, Is.Empty,
                    $"[F14c] {file}'s character table holds {extra.Count} code point(s) outside the bake's list ({CodePoints(extra)}): the editor wrote them in, and the next bake drops them again");
            }
        }

        static string CodePoints(List<int> cps)
        {
            var sb = new System.Text.StringBuilder();
            for (int i = 0; i < cps.Count && i < 12; i++) { if (i > 0) sb.Append(", "); sb.Append("U+").Append(cps[i].ToString("X4")); }
            if (cps.Count > 12) sb.Append(", ...");
            return sb.ToString();
        }

        static string SkinFontBytes()
        {
            var sb = new System.Text.StringBuilder();
            foreach (var f in SkinSpec.Fonts)
            {
                string full = Path.GetFullPath(Path.Combine(ProjectDataPath, "..", SkinSpec.Root + f));
                using (var md5 = System.Security.Cryptography.MD5.Create())
                    sb.Append(f).Append('=').Append(System.Convert.ToBase64String(md5.ComputeHash(File.ReadAllBytes(full)))).Append('\n');
            }
            return sb.ToString();
        }

        static string GitStatusOfSkinFonts()
        {
            string projectDir = Path.GetFullPath(Path.Combine(ProjectDataPath, ".."));   // git resolves the path below against this
            var psi = new ProcessStartInfo("git", "status --porcelain -- Assets/_Project/UI/Skin/Fonts")
            {
                WorkingDirectory = projectDir, RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false, CreateNoWindow = true,
            };
            using (var proc = Process.Start(psi))
            {
                string outp = proc.StandardOutput.ReadToEnd();
                proc.WaitForExit(30000);
                return outp.Trim();
            }
        }
    }
}
