// Phase: B6 (implemented) — every skin file on disk matches the table, every sheet resolves, every unit has a portrait.
// TW/UI/Verify Skin runs the same checks SkinAssetTests asserts on, so a missing or mis-sized artist file fails the
// gate rather than drawing a hole: per SkinSpec entry the PNG exists at the spec's size with the spec's border and
// import settings, glyphs are white-bodied and uncoloured, portraits have transparent corners and sane coverage; every
// url() in the skin sheets points at a file the table knows; the style sheets imported without errors; the fonts
// exist; and every archetype either side's default roster hands out maps to a portrait. The report also lists what
// is still a placeholder (UiSkinGenerator's record), which is the artist's to-do list.
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using Unity.Collections;
using UnityEditor;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Sim;
using TW.UI;

namespace TW.Editor
{
    public static class UiSkinVerifier
    {
        public static readonly string[] Sheets = { "dustfront.uss", "dustfront.tokens.uss", "dustfront.components.uss" };

        public sealed class Report
        {
            public readonly List<string> Failures = new List<string>();
            public readonly List<string> Placeholders = new List<string>();
            public int Checked;
            public bool Ok => Failures.Count == 0;
            public override string ToString()
            {
                var sb = new StringBuilder();
                sb.Append(Ok ? "skin OK" : $"skin: {Failures.Count} problem(s)").Append($", {Checked} file(s) checked, {Placeholders.Count} still placeholder\n");
                foreach (var f in Failures) sb.Append("  FAIL ").Append(f).Append('\n');
                if (Placeholders.Count > 0) { sb.Append("  placeholders: "); sb.Append(string.Join(", ", Placeholders)); sb.Append('\n'); }
                return sb.ToString();
            }
        }

        [MenuItem("TW/UI/Verify Skin")]
        public static void Menu()
        {
            var r = Run();
            if (r.Ok) Debug.Log(r.ToString()); else Debug.LogError(r.ToString());
        }

        public static Report Run()
        {
            var r = new Report();
            var known = new HashSet<string>();
            foreach (var e in SkinSpec.All) { known.Add(e.File); CheckSprite(e, r); }
            foreach (var n in SkinSpec.PortraitNames) { known.Add("Portraits/" + n + ".png"); CheckPortrait(n, r); }
            CheckSheets(r, known);
            foreach (var f in SkinSpec.Fonts)
                if (AssetDatabase.LoadAssetAtPath<UnityEngine.TextCore.Text.FontAsset>(SkinSpec.Root + f) == null) r.Failures.Add($"font asset missing: {SkinSpec.Root}{f} (TW/UI/Bake Skin Fonts)");
            CheckRosterPortraits(r);
            r.Placeholders.AddRange(UiSkinGenerator.StillPlaceholder());
            return r;
        }

        static void CheckSprite(in SkinEntry e, Report r)
        {
            r.Checked++;
            string full = UiSkinGenerator.FullPath(e.Path);
            if (!File.Exists(full)) { r.Failures.Add($"missing: {e.Path}"); return; }
            var imp = AssetImporter.GetAtPath(e.Path) as TextureImporter;
            if (imp == null) { r.Failures.Add($"not a texture import: {e.Path}"); return; }
            if (imp.textureType != TextureImporterType.Sprite) r.Failures.Add($"{e.File}: texture type is {imp.textureType}, expected Sprite");
            if (imp.spriteImportMode != SpriteImportMode.Single) r.Failures.Add($"{e.File}: sprite mode is {imp.spriteImportMode}, expected Single");
            if (imp.mipmapEnabled) r.Failures.Add($"{e.File}: mipmaps on");
            if (imp.textureCompression != TextureImporterCompression.Uncompressed) r.Failures.Add($"{e.File}: compressed");
            if (!imp.alphaIsTransparency) r.Failures.Add($"{e.File}: alpha is transparency off");
            var want = e.HasBorder ? new Vector4(e.L, e.B, e.R, e.T) : Vector4.zero;
            if (imp.spriteBorder != want) r.Failures.Add($"{e.File}: border {imp.spriteBorder} expected {want}");
            if (!Decode(full, out var tex)) { r.Failures.Add($"{e.File}: PNG did not decode"); return; }
            try
            {
                if (tex.width != e.W || tex.height != e.H) r.Failures.Add($"{e.File}: {tex.width}x{tex.height}, expected {e.W}x{e.H}");
                if (e.Kind == SkinKind.Glyph)
                {
                    // a white body with a near-black contour: the tint multiplies both, so only colour is an error; the
                    // contour is a large share of a thin glyph, so the body only has to be present, not dominant
                    var px = tex.GetPixels32(); int opaque = 0, white = 0, grey = 0;
                    foreach (var p in px) if (p.a > 127)
                    {
                        opaque++;
                        if (p.r >= 247 && p.g >= 247 && p.b >= 247) white++;
                        if (Mathf.Max(p.r, Mathf.Max(p.g, p.b)) - Mathf.Min(p.r, Mathf.Min(p.g, p.b)) <= 8) grey++;
                    }
                    if (opaque == 0) r.Failures.Add($"{e.File}: glyph is empty");
                    else if (grey < opaque * 0.98f) r.Failures.Add($"{e.File}: glyph is {100f * (opaque - grey) / opaque:0}% coloured where opaque; icons are white with a dark contour so USS can tint them");
                    else if (white < opaque * 0.20f) r.Failures.Add($"{e.File}: glyph is {100f * white / opaque:0}% white where opaque; the white body is missing or thinner than its contour");
                }
            }
            finally { Object.DestroyImmediate(tex); }
        }

        static void CheckPortrait(string name, Report r)
        {
            r.Checked++;
            string path = SkinSpec.PortraitPath(name);
            string full = UiSkinGenerator.FullPath(path);
            if (!File.Exists(full)) { r.Failures.Add($"portrait missing: {path}"); return; }
            if (!Decode(full, out var tex)) { r.Failures.Add($"{name}: PNG did not decode"); return; }
            try
            {
                int s = SkinSpec.PortraitSize;
                if (tex.width != s || tex.height != s) r.Failures.Add($"Portraits/{name}.png: {tex.width}x{tex.height}, expected {s}x{s}");
                var px = tex.GetPixels32();
                int w = tex.width, h = tex.height;
                int corners = px[0].a + px[w - 1].a + px[(h - 1) * w].a + px[(h - 1) * w + w - 1].a;
                if (corners > 0) r.Failures.Add($"Portraits/{name}.png: corners are not transparent");
                int opaque = 0; foreach (var p in px) if (p.a > 32) opaque++;
                float cover = opaque / (float)px.Length;
                if (cover < 0.12f || cover > 0.80f) r.Failures.Add($"Portraits/{name}.png: {100f * cover:0}% opaque, expected 12-80%");
            }
            finally { Object.DestroyImmediate(tex); }
        }

        static bool Decode(string full, out Texture2D tex)
        {
            tex = new Texture2D(2, 2, TextureFormat.RGBA32, false);
            if (ImageConversion.LoadImage(tex, File.ReadAllBytes(full), false)) return true;
            Object.DestroyImmediate(tex); tex = null; return false;
        }

        static readonly Regex Url = new Regex("url\\(\\s*[\"']?([^\"')]+)[\"']?\\s*\\)", RegexOptions.Compiled);

        static void CheckSheets(Report r, HashSet<string> known)
        {
            foreach (var sheet in Sheets)
            {
                string path = SkinSpec.Root + sheet;
                string full = UiSkinGenerator.FullPath(path);
                if (!File.Exists(full)) { r.Failures.Add($"sheet missing: {path}"); continue; }
                var ss = AssetDatabase.LoadAssetAtPath<StyleSheet>(path);
                if (ss == null) r.Failures.Add($"{sheet}: did not import as a StyleSheet");
                else if (ss.importedWithErrors) r.Failures.Add($"{sheet}: imported with errors (see the console)");
                foreach (Match m in Url.Matches(File.ReadAllText(full)))
                {
                    string u = m.Groups[1].Value;
                    if (u.StartsWith("unity-theme://") || u.StartsWith("project://") || u.EndsWith(".uss")) continue;
                    string rel = u.StartsWith("/") ? u.Substring(1).Replace("Assets/_Project/UI/Skin/", "") : u;
                    if (!File.Exists(UiSkinGenerator.FullPath(SkinSpec.Root + rel))) r.Failures.Add($"{sheet}: url(\"{u}\") does not exist");
                    else if (rel.EndsWith(".png") && !known.Contains(rel)) r.Failures.Add($"{sheet}: url(\"{u}\") is not in SkinSpec");
                }
            }
            string theme = UiSkinGenerator.FullPath(UiAssetBuilder.ThemePath);
            if (!File.Exists(theme)) r.Failures.Add($"theme missing: {UiAssetBuilder.ThemePath}");
            if (AssetDatabase.LoadAssetAtPath<PanelSettings>(UiAssetBuilder.PanelPath) == null) r.Failures.Add($"panel settings missing: {UiAssetBuilder.PanelPath} (TW/UI/Create Panel Settings)");
        }

        static void CheckRosterPortraits(Report r)
        {
            var roster = new NativeArray<RosterEntry>(RosterEntry.SlotCount * 2, Allocator.Temp);
            RosterEntry.FillDefault(roster, 0);
            RosterEntry.FillDefault(roster, RosterEntry.SlotCount);
            var seen = new HashSet<byte>();
            for (int s = 0; s < roster.Length; s++)
            {
                byte a = roster[s].Archetype;
                if (!seen.Add(a)) continue;
                string name = HudText.PortraitName(a);
                bool listed = System.Array.IndexOf(SkinSpec.PortraitNames, name) >= 0;
                if (!listed) r.Failures.Add($"archetype {a} ({name}) is deployable but has no portrait entry in SkinSpec.PortraitNames");
                if (a >= HudText.PortraitCount) r.Failures.Add($"archetype {a} is deployable but HudText.PortraitCount is {HudText.PortraitCount}");
            }
            roster.Dispose();
        }
    }
}
