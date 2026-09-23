// Phase: B6 (implemented) — paints a placeholder for every sprite in SkinSpec, and never over an artist's file.
// TW/UI/Generate Placeholder Skin writes each missing PNG: riveted gunmetal plates with rounded corners, recessed windows,
// white glyphs with a dark contour, the masks and straps, and a "?" portrait for every unit the baker has not drawn.
// placeholders.json beside the sprites records the hash of every file this tool wrote. A file whose hash still matches
// is a placeholder and is repainted when the generator's version rises (or on "(force)"); one whose hash differs was
// overwritten by an artist and is dropped from the record and never touched again; one the record never knew is an
// artist's from the start. UiSkinVerifier lists what is still recorded here: the artist's to-do list.
// C# rather than Python because the borders and import settings only exist in TextureImporter, the portrait baker
// beside this needs the renderer anyway, and AssetDatabase is what knows whether a file exists.
using System;
using System.Collections.Generic;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using UnityEditor;
using UnityEngine;
using TW.UI;

namespace TW.Editor
{
    public static class UiSkinGenerator
    {
        public const string SidecarPath = SkinSpec.Root + "placeholders.json";

        [Serializable] public sealed class Sidecar { public int generator; public List<Entry> files = new List<Entry>(); }
        [Serializable] public sealed class Entry { public string file; public string sha1; }

        [MenuItem("TW/UI/Generate Placeholder Skin")]
        public static void Generate() => Generate(false);

        [MenuItem("TW/UI/Generate Placeholder Skin (force)")]
        public static void GenerateForce() => Generate(true);

        /// <summary>Returns the number of files written.</summary>
        public static int Generate(bool force)
        {
            var side = LoadSidecar();
            var recorded = new Dictionary<string, Entry>();
            foreach (var e in side.files) recorded[e.file] = e;
            int written = 0, promoted = 0, kept = 0;
            EnsureFolders();
            AssetDatabase.StartAssetEditing();
            try
            {
                foreach (var spec in SkinSpec.All)
                {
                    var decision = Decide(spec.File, recorded, side.generator, force, out var entry);
                    if (decision == Decision.Artist) { if (entry != null) { side.files.Remove(entry); promoted++; } continue; }
                    if (decision == Decision.Keep) { kept++; continue; }
                    var px = Paint(spec);
                    WritePng(spec.Path, px, spec.W, spec.H);
                    Record(side, recorded, spec.File, spec.Path);
                    written++;
                }
                foreach (var name in SkinSpec.PortraitNames)
                {
                    string file = "Portraits/" + name + ".png";
                    var decision = Decide(file, recorded, side.generator, force, out var entry);
                    if (decision == Decision.Artist) { if (entry != null) { side.files.Remove(entry); promoted++; } continue; }
                    if (decision == Decision.Keep) { kept++; continue; }
                    var px = PaintPortraitPlaceholder(name);
                    WritePng(SkinSpec.PortraitPath(name), px, SkinSpec.PortraitSize, SkinSpec.PortraitSize);
                    Record(side, recorded, file, SkinSpec.PortraitPath(name));
                    written++;
                }
            }
            finally { AssetDatabase.StopAssetEditing(); }
            side.generator = SkinSpec.GeneratorVersion;
            SaveSidecar(side);
            AssetDatabase.Refresh();
            Debug.Log($"UiSkinGenerator: wrote {written} placeholder(s), kept {kept}, {promoted} promoted to artist files; {side.files.Count} still placeholder.");
            return written;
        }

        enum Decision { Paint, Keep, Artist }

        static Decision Decide(string file, Dictionary<string, Entry> recorded, int recordedVersion, bool force, out Entry entry)
        {
            recorded.TryGetValue(file, out entry);
            string full = FullPath(SkinSpec.Root + file);
            bool exists = File.Exists(full);
            if (!exists) return Decision.Paint;
            if (entry == null) return Decision.Artist;                       // present, never recorded: not ours
            if (Sha1(full) != entry.sha1) return Decision.Artist;             // overwritten in place: promoted
            return force || recordedVersion < SkinSpec.GeneratorVersion ? Decision.Paint : Decision.Keep;
        }

        static void Record(Sidecar side, Dictionary<string, Entry> recorded, string file, string assetPath)
        {
            string sha = Sha1(FullPath(assetPath));
            if (recorded.TryGetValue(file, out var e)) e.sha1 = sha;
            else { e = new Entry { file = file, sha1 = sha }; side.files.Add(e); recorded[file] = e; }
        }

        /// <summary>Files this tool wrote that nobody has replaced yet.</summary>
        public static List<string> StillPlaceholder()
        {
            var side = LoadSidecar();
            var list = new List<string>();
            foreach (var e in side.files)
            {
                string full = FullPath(SkinSpec.Root + e.file);
                if (File.Exists(full) && Sha1(full) == e.sha1) list.Add(e.file);
            }
            list.Sort();
            return list;
        }

        public static Sidecar LoadSidecar()
        {
            string full = FullPath(SidecarPath);
            if (!File.Exists(full)) return new Sidecar();
            try { return JsonUtility.FromJson<Sidecar>(File.ReadAllText(full)) ?? new Sidecar(); }
            catch (Exception) { return new Sidecar(); }
        }

        static void SaveSidecar(Sidecar s)
        {
            s.files.Sort((a, b) => string.CompareOrdinal(a.file, b.file));
            File.WriteAllText(FullPath(SidecarPath), JsonUtility.ToJson(s, true));
        }

        public static string FullPath(string assetPath) => Path.GetFullPath(Path.Combine(Application.dataPath, "..", assetPath));

        static string Sha1(string full)
        {
            using (var sha = SHA1.Create())
            {
                var hash = sha.ComputeHash(File.ReadAllBytes(full));
                var sb = new StringBuilder(40);
                foreach (byte b in hash) sb.Append(b.ToString("x2"));
                return sb.ToString();
            }
        }

        static void EnsureFolders()
        {
            foreach (var sub in new[] { "Sprites", "Icons", "Portraits", "Fonts" })
            {
                string dir = FullPath(SkinSpec.Root + sub);
                if (!Directory.Exists(dir)) Directory.CreateDirectory(dir);
            }
        }

        static void WritePng(string assetPath, Color32[] px, int w, int h)
        {
            // The canvas is drawn top row first (y down, as every rule above reads: "light top-left", a lock's shackle on
            // top); a Texture2D's row 0 is the BOTTOM. Until v6 the rows went in unflipped and every sprite and glyph came
            // out upside down: plates lit from below, the helmet read as a dropdown arrow, the padlock stood on its shackle.
            var rows = new Color32[px.Length];
            for (int y = 0; y < h; y++) System.Array.Copy(px, y * w, rows, (h - 1 - y) * w, w);
            var t = new Texture2D(w, h, TextureFormat.RGBA32, false);
            t.SetPixels32(rows); t.Apply(false, false);
            File.WriteAllBytes(FullPath(assetPath), t.EncodeToPNG());
            UnityEngine.Object.DestroyImmediate(t);
            AssetDatabase.ImportAsset(assetPath, ImportAssetOptions.ForceUpdate);
        }

        // ---- palette (mirrors dustfront.tokens.uss) ---------------------------------------------------------------
        static readonly Color32 Plate900 = Hex(0x0E0F10), Plate800 = Hex(0x141416), Plate700 = Hex(0x1B1D20), Plate600 = Hex(0x23252A),
            Plate500 = Hex(0x33363B), Plate400 = Hex(0x55595D), EdgeDark = Hex(0x0A0B0C), EdgeLight = Hex(0x6A6D70),
            WindowLip = Hex(0x3A3C3F), Keyline = Hex(0x464849),
            Rust500 = Hex(0x7A4324), Rust300 = Hex(0xA8683A), Brass = Hex(0xA8863A), Accent = Hex(0xE0762A), AccentDim = Hex(0x9C5E1E),
            Alarm = Hex(0xE02B2B), White = new Color32(255, 255, 255, 255), Clear = new Color32(0, 0, 0, 0);

        static Color32 Hex(int rgb) => new Color32((byte)(rgb >> 16), (byte)(rgb >> 8 & 255), (byte)(rgb & 255), 255);

        // ---- what each sprite looks like ------------------------------------------------------------------------
        public static Color32[] Paint(in SkinEntry e)
        {
            var c = new Canvas(e.W, e.H);
            string n = Path.GetFileNameWithoutExtension(e.File);
            switch (n)
            {
                case "plate_normal": PlateBody(c, e.L, Plate700, false, false); break;
                case "plate_hover": PlateBody(c, e.L, Plate600, false, false); c.InnerLine(e.L - 4, Accent, 100); break;
                case "plate_pressed": PlateBody(c, e.L, Plate800, true, false); break;
                case "plate_disabled": PlateBody(c, e.L, Plate800, false, true); break;
                // round 9: gunmetal like every other control (the brown plate, brass rivets and square inner line read as a
                // debug box); the USS gives the primary button its amber keyline along the real radius
                case "btn_accent_normal": PlateBody(c, e.L, Plate600, false, false); break;
                case "btn_accent_hover": PlateBody(c, e.L, Lerp(Plate600, Plate500, 0.6f), false, false); break;
                case "btn_accent_pressed": PlateBody(c, e.L, Plate800, true, false); break;
                case "panel_bg": PlateBody(c, e.L, Plate800, false, false, null, 3.5f); c.InnerLine(e.L - 6, Keyline, 140); break;
                // round 11: rivets belong on container plates, never on buttons; no square inner line on hover
                case "order_plate_normal": PlateBody(c, e.L, Plate700, false, false, null, 0f); break;
                case "order_plate_hover": PlateBody(c, e.L, Plate600, false, false, null, 0f); break;
                case "order_plate_pressed": PlateBody(c, e.L, Plate800, true, false, null, 0f); break;
                case "order_plate_disabled": PlateBody(c, e.L, Plate800, false, true, null, 0f); break;
                case "gauge_window": case "badge": Window(c, RadiusSm); break;
                case "card_nameplate": c.Fill(0, 0, c.W, c.H, Plate900); c.HLine(0, Plate500, 0, c.W); c.RoundCorners(RadiusSm, 1f); break;
                case "keycap": PlateBody(c, e.L, Plate500, false, false, null, 0f, RadiusMd); c.Fill(2, c.H - e.B, c.W - 4, 3, Plate900); break;
                case "row_plate": PlateBody(c, 8, Plate700, false, false, null, 0f, RadiusMd); break;
                case "tab_plate": PlateBody(c, 8, Plate700, false, false, null, 0f, RadiusMd); break;
                case "tab_plate_selected": PlateBody(c, 8, Plate700, false, false, null, 0f, RadiusMd); c.Fill(3, 2, c.W - 6, 2, Accent); break;
                case "tooltip_plate": PlateBody(c, e.L, Plate800, false, false, null, 0f); c.InnerLine(2, Rust500, 200); break;
                case "title_plate": PlateBody(c, 24, Plate700, false, false, null, 0f); c.Rivet(20, 20, 3.5f); c.Rivet(20, c.H - 21, 3.5f); c.Rivet(c.W - 21, 20, 3.5f); c.Rivet(c.W - 21, c.H - 21, 3.5f); c.Rivet(44, c.H / 2, 3.5f); c.Rivet(c.W - 45, c.H / 2, 3.5f); break;
                case "banner_ribbon": Banner(c); break;
                case "hazard_strip": Hazard(c); break;
                case "card_frame": Frame(c, e.L, RadiusLg, false); break;   // round 9: no rivets, they collided with the hotkey and cost windows
                case "card_rim": Rim(c, e.L); break;
                case "cooldown_mask": Hatch(c); break;
                case "divider_v": Strap(c, true); break;
                case "divider_h": Strap(c, false); break;
                case "minimap_frame": Frame(c, e.L, RadiusLg, true, 5f); break;
                case "bezel_round": BezelRound(c); break;
                case "order_round_normal": OrderRound(c, Plate700, false, false); break;
                case "order_round_hover": OrderRound(c, Plate600, false, false); break;
                case "order_round_pressed": OrderRound(c, Plate800, true, false); break;
                case "order_round_disabled": OrderRound(c, Plate800, false, true); break;
                case "slider_track": c.Fill(0, 0, c.W, c.H, Plate900); c.HLine(c.H - 1, Plate500, 0, c.W); c.HLine(0, EdgeDark, 0, c.W); c.RoundCorners(RadiusSm, 1f); break;
                case "slider_fill": c.GradientV(0, 0, c.W, c.H, Lerp(Accent, White, 0.1f), AccentDim); c.HLine(0, EdgeDark, 0, c.W); c.HLine(c.H - 1, EdgeDark, 0, c.W); c.RoundCorners(RadiusSm, 1f); break;
                case "slider_knob": Knob(c, false); break;
                case "slider_knob_hover": Knob(c, true); break;
                case "checkbox_off": PlateBody(c, 6, Plate700, false, false, null, 0f, RadiusSm); break;
                case "checkbox_on": PlateBody(c, 6, Plate700, false, false, null, 0f, RadiusSm); c.Glyph(Sdf.Union(Sdf.Segment(6, 6, c.W - 7, c.H - 7, 1.6f), Sdf.Segment(c.W - 7, 6, 6, c.H - 7, 1.6f)), Accent, EdgeDark); break;
                case "dim_vignette": Vignette(c); break;
                case "grain_overlay": Grain(c); break;
                case "scroller_track": c.Fill(0, 0, c.W, c.H, Plate900); c.VLine(0, Plate500, 0, c.H); break;
                case "scroller_knob": c.Fill(0, 0, c.W, c.H, Plate500); c.VLine(0, EdgeLight, 0, c.H); c.VLine(c.W - 1, EdgeDark, 0, c.H); c.RoundCorners(RadiusSm, 1f); break;
                case "stamp_victory": Stamp(c, true); break;
                case "stamp_defeat": Stamp(c, false); break;
                default:
                    if (e.Kind == SkinKind.Glyph) Icon(c, n);
                    else { PlateBody(c, Math.Max(2, e.L), Plate700, false, false); }
                    break;
            }
            return c.Px;
        }

        static Color32 Lerp(Color32 a, Color32 b, float t) => Color32.Lerp(a, b, t);

        // corner radii in sprite px, mirroring --tw-radius-lg / --tw-radius / --tw-radius-sm in dustfront.tokens.uss
        public const int RadiusLg = 12, RadiusMd = 8, RadiusSm = 5;

        /// <summary>The riveted plate every plate sprite starts from: rounded corners, a 2 px dark edge, a 1 px bevel
        /// (light top-left, dark bottom-right; inverted when pressed), a vertical gradient fill, grain only inside
        /// the border ring so the stretched centre stays flat, and a rivet in each corner inside the border.</summary>
        static void PlateBody(Canvas c, int border, Color32 fill, bool pressed, bool disabled, Color32? rivet = null, float rivetR = 2.5f, int radius = RadiusLg)
        {
            Color32 top = Lerp(fill, White, 0.06f), bottom = Lerp(fill, EdgeDark, 0.12f);
            c.GradientV(0, 0, c.W, c.H, top, bottom);
            if (border > 2) c.Grain(0, 0, c.W, c.H, border, 0.035f, 7);
            c.Fill(0, 0, c.W, 2, EdgeDark); c.Fill(0, c.H - 2, c.W, 2, EdgeDark); c.Fill(0, 0, 2, c.H, EdgeDark); c.Fill(c.W - 2, 0, 2, c.H, EdgeDark);
            Color32 hi = disabled ? Plate400 : EdgeLight, lo = Plate900;
            if (pressed) { var t = hi; hi = lo; lo = t; }
            c.HLine(2, hi, 2, c.W - 2); c.VLine(2, hi, 2, c.H - 2);
            c.HLine(c.H - 3, lo, 2, c.W - 2); c.VLine(c.W - 3, lo, 2, c.H - 2);
            if (rivetR > 0f && border >= 8)
            {
                Color32 rc = rivet ?? (disabled ? Plate400 : Plate500);
                float p = border * 0.5f;
                // mirrored exactly: pixel x covers [x, x+1], so the mirror of a centre at p is W - p (round 10: W - 1 - p sat
                // every right and bottom rivet a pixel further in than its twin)
                c.Rivet(p, p, rivetR, rc); c.Rivet(c.W - p, p, rivetR, rc); c.Rivet(p, c.H - p, rivetR, rc); c.Rivet(c.W - p, c.H - p, rivetR, rc);
            }
            int rr = border > 0 ? Math.Min(radius, border) : radius;
            c.RoundCorners(rr);   // the curve must stay inside the 9-slice border
            c.ArcBevel(rr, hi, lo);   // round 10: the 1 px bevel follows the arcs instead of stopping where they start
        }

        static void Window(Canvas c, int radius)
        {
            c.GradientV(0, 0, c.W, c.H, Plate900, Lerp(Plate900, Plate800, 0.5f));
            c.HLine(0, EdgeDark, 0, c.W); c.VLine(0, EdgeDark, 0, c.H);
            c.HLine(c.H - 1, WindowLip, 0, c.W); c.VLine(c.W - 1, WindowLip, 0, c.H);   // round 7: quiet, or the right edge reads as a "|"
            c.HLine(1, Plate900, 1, c.W - 1); c.VLine(1, Plate900, 1, c.H - 1);
            c.RoundCorners(radius, 1f);
            c.ArcLip(radius, WindowLip);   // round 9: carry the lip round the corners, or each edge reads as a separate "|"
        }

        static void Frame(Canvas c, int border, int radius, bool rivets, float rivetR = 2.5f)
        {
            PlateBody(c, border, Plate700, false, false, null, rivets ? rivetR : 0f, radius);
            int inner = border - 2;
            c.Fill(inner, inner, c.W - 2 * inner, c.H - 2 * inner, EdgeDark);
            c.Fill(border, border, c.W - 2 * border, c.H - 2 * border, Clear);
        }

        static void Rim(Canvas c, int border)
        {
            // a 3 px white ring on the frame's inner edge plus a fading glow, both following the card's rounded corner
            // (the ring is tinted in USS for hover / selected / armed)
            float inset = border - 3 + 1.5f, cx = c.W * 0.5f, cy = c.H * 0.5f;
            float hw = cx - inset, hh = cy - inset, r = Mathf.Max(1f, RadiusLg - inset);
            var box = Sdf.RoundBox(cx, cy, hw, hh, r);
            c.Glyph((x, y) => Mathf.Abs(box(x, y)) - 3.5f, new Color32(255, 255, 255, 50), null);
            c.Glyph((x, y) => Mathf.Abs(box(x, y)) - 2.5f, new Color32(255, 255, 255, 90), null);
            c.Glyph((x, y) => Mathf.Abs(box(x, y)) - 1.5f, White, null);
        }

        static void Hatch(Canvas c)
        {
            var baseC = new Color32(8, 9, 10, 158);
            var line = new Color32(8, 9, 10, 222);
            for (int y = 0; y < c.H; y++) for (int x = 0; x < c.W; x++) c.Px[y * c.W + x] = (x + y) % 6 == 0 ? line : baseC;
        }

        static void Strap(Canvas c, bool vertical)
        {
            c.Fill(0, 0, c.W, c.H, Plate700);
            if (vertical) { c.VLine(0, Rust500, 0, c.H); c.VLine(c.W - 1, Rust500, 0, c.H); c.VLine(1, Plate500, 0, c.H); for (int y = 8; y < c.H; y += 16) c.Rivet(c.W * 0.5f, y, 1.8f); }
            else { c.HLine(0, Rust500, 0, c.W); c.HLine(c.H - 1, Rust500, 0, c.W); c.HLine(1, Plate500, 0, c.W); for (int x = 8; x < c.W; x += 16) c.Rivet(x, c.H * 0.5f, 1.8f); }
        }

        static void BezelRound(Canvas c)
        {
            float cx = c.W * 0.5f - 0.5f, cy = c.H * 0.5f - 0.5f, ro = c.W * 0.5f - 1f, ri = ro - 12f;
            c.Glyph(Sdf.Ring(cx, cy, ro, 12f), Plate700, EdgeDark);
            c.Glyph(Sdf.Ring(cx, cy, ro - 1.5f, 1f), EdgeLight, null);
            c.Glyph(Sdf.Ring(cx, cy, ri + 1.5f, 1f), Plate900, null);
            for (int k = 0; k < 8; k++)
            {
                float a = (k + 0.5f) * Mathf.PI / 4f;
                c.Rivet(cx + Mathf.Cos(a) * (ro - 6f), cy + Mathf.Sin(a) * (ro - 6f), 2.2f);
            }
        }

        static void OrderRound(Canvas c, Color32 fill, bool pressed, bool disabled)
        {
            float cx = c.W * 0.5f - 0.5f, cy = c.H * 0.5f - 0.5f, r = c.W * 0.5f - 1.5f;
            c.Glyph(Sdf.Circle(cx, cy, r), fill, EdgeDark);
            c.Glyph(Sdf.Ring(cx, cy, r - 2f, 1f), pressed ? Plate900 : (disabled ? Plate400 : EdgeLight), null);
            for (int k = 0; k < 4; k++)
            {
                float a = (k + 0.5f) * Mathf.PI / 2f;
                c.Rivet(cx + Mathf.Cos(a) * (r - 6f), cy + Mathf.Sin(a) * (r - 6f), 2f, disabled ? Plate400 : Plate500);
            }
        }

        static void Knob(Canvas c, bool hover)
        {
            float cx = c.W * 0.5f - 0.5f, cy = c.H * 0.5f - 0.5f;
            c.Glyph(Sdf.Hexagon(cx, cy, c.W * 0.5f - 1.5f), Plate500, EdgeDark);
            c.Glyph(Sdf.Hexagon(cx, cy, c.W * 0.5f - 4f), Lerp(Plate500, White, 0.08f), null);
            c.Rivet(cx, cy, 2.5f, hover ? Brass : Plate400);
        }

        static void Banner(Canvas c)
        {
            int end = 96;
            c.GradientV(0, 0, c.W, c.H, Lerp(Plate800, White, 0.04f), Plate900);
            c.HLine(2, Rust500, end, c.W - end); c.HLine(c.H - 3, Rust500, end, c.W - end);
            c.HLine(0, EdgeDark, end, c.W - end); c.HLine(c.H - 1, EdgeDark, end, c.W - end);
            // torn ends: an angled cut with a three-step tear, mirrored
            for (int y = 0; y < c.H; y++)
            {
                float t = y / (float)(c.H - 1);
                int tear = (y / (c.H / 3)) % 2 == 0 ? 0 : 10;
                int cutL = (int)(t * 30f) + tear + 8, cutR = c.W - 1 - ((int)((1f - t) * 30f) + tear + 8);
                for (int x = 0; x < cutL; x++) c.Px[y * c.W + x] = Clear;
                for (int x = cutR + 1; x < c.W; x++) c.Px[y * c.W + x] = Clear;
                if (cutL < c.W) c.Px[y * c.W + cutL] = Rust300;
                if (cutR >= 0) c.Px[y * c.W + cutR] = Rust300;
            }
        }

        static void Hazard(Canvas c)
        {
            c.Fill(0, 0, c.W, c.H, Plate700);
            for (int y = 0; y < c.H; y++) for (int x = 0; x < c.W; x++)
                if (((x + y) / 4) % 2 == 0) c.Px[y * c.W + x] = Plate500;
            c.HLine(0, EdgeDark, 0, c.W); c.HLine(c.H - 1, EdgeDark, 0, c.W);
        }

        static void Vignette(Canvas c)
        {
            float cx = c.W * 0.5f, cy = c.H * 0.5f, rmax = Mathf.Sqrt(cx * cx + cy * cy);
            for (int y = 0; y < c.H; y++) for (int x = 0; x < c.W; x++)
            {
                float d = Mathf.Sqrt((x - cx) * (x - cx) + (y - cy) * (y - cy)) / rmax;
                byte a = (byte)Mathf.RoundToInt(Mathf.Lerp(140f, 217f, Mathf.SmoothStep(0f, 1f, d)));
                c.Px[y * c.W + x] = new Color32(8, 9, 10, a);
            }
        }

        static void Grain(Canvas c)
        {
            for (int y = 0; y < c.H; y++) for (int x = 0; x < c.W; x++)
            {
                uint h = Canvas.Hash(x, y, 31);
                byte v = (byte)(h & 255);
                byte a = (byte)((h >> 8) % 24);
                c.Px[y * c.W + x] = new Color32(v, v, v, a);
            }
        }

        static void Stamp(Canvas c, bool victory)
        {
            float cx = c.W * 0.5f - 0.5f, cy = c.H * 0.5f - 0.5f, r = c.W * 0.5f - 6f;
            var ring = Sdf.Union(Sdf.Ring(cx, cy, r, 5f), Sdf.Ring(cx, cy, r - 10f, 2f));
            Func<float, float, float> mark = victory
                ? Sdf.Union(Sdf.Segment(cx - 22, cy + 2, cx - 6, cy + 20, 6f), Sdf.Segment(cx - 6, cy + 20, cx + 26, cy - 20, 6f))
                : Sdf.Union(Sdf.Segment(cx - 20, cy - 20, cx + 20, cy + 20, 6f), Sdf.Segment(cx + 20, cy - 20, cx - 20, cy + 20, 6f));
            c.Glyph(Sdf.Union(ring, mark), White, EdgeDark);
            c.Grain(0, 0, c.W, c.H, 0, 0.35f, 5, true);
        }

        /// <summary>White glyphs with a 1 px dark contour, 4 px safe margin, 3 px minimum stroke; coloured by USS tint.</summary>
        static void Icon(Canvas c, string name)
        {
            float s = c.W, cx = s * 0.5f - 0.5f, cy = s * 0.5f - 0.5f, u = s / 64f;   // u: one 64-grid unit
            Func<float, float, float> g = null;
            switch (name)
            {
                case "ico_fallback": g = Chevrons(cx, cy, u, -1); break;
                case "ico_overthetop": g = Chevrons(cx, cy, u, +1); break;
                case "ico_lock_closed": g = Lock(cx, cy, u, false); break;
                case "ico_lock_open": g = Lock(cx, cy, u, true); break;
                case "ico_fireatwill": g = Crosshair(cx, cy, u); break;
                case "ico_holdfire": g = Sdf.Union(Crosshair(cx, cy, u), Sdf.Segment(cx - 22 * u, cy + 22 * u, cx + 22 * u, cy - 22 * u, 3 * u)); break;
                case "ico_barrage":
                    g = Sdf.Union(Sdf.Capsule(cx, cy - 20 * u, cx, cy + 2 * u, 6 * u),
                        Sdf.Union(Sdf.Box(cx, cy + 4 * u, 9 * u, 3 * u),
                        Sdf.Union(Sdf.Segment(cx - 18 * u, cy + 24 * u, cx - 10 * u, cy + 14 * u, 2.5f * u),
                        Sdf.Union(Sdf.Segment(cx, cy + 26 * u, cx, cy + 14 * u, 2.5f * u), Sdf.Segment(cx + 18 * u, cy + 24 * u, cx + 10 * u, cy + 14 * u, 2.5f * u)))));
                    break;
                case "ico_gas":
                    g = Sdf.Union(Sdf.Circle(cx - 10 * u, cy - 2 * u, 10 * u), Sdf.Union(Sdf.Circle(cx + 2 * u, cy - 8 * u, 12 * u),
                        Sdf.Union(Sdf.Circle(cx + 13 * u, cy - 1 * u, 9 * u), Sdf.Union(Sdf.Box(cx + 1 * u, cy + 2 * u, 22 * u, 6 * u),
                        Sdf.Union(Sdf.Capsule(cx - 12 * u, cy + 14 * u, cx - 12 * u, cy + 24 * u, 2.5f * u),
                        Sdf.Union(Sdf.Capsule(cx, cy + 14 * u, cx, cy + 26 * u, 2.5f * u), Sdf.Capsule(cx + 12 * u, cy + 14 * u, cx + 12 * u, cy + 24 * u, 2.5f * u)))))));
                    break;
                case "ico_pause": g = Sdf.Union(Sdf.Box(cx - 9 * u, cy, 5 * u, 18 * u), Sdf.Box(cx + 9 * u, cy, 5 * u, 18 * u)); break;
                case "ico_play": g = Sdf.Triangle(cx - 14 * u, cy - 18 * u, cx - 14 * u, cy + 18 * u, cx + 18 * u, cy); break;
                case "ico_speed": g = Sdf.Union(Sdf.Triangle(cx - 24 * u, cy - 14 * u, cx - 24 * u, cy + 14 * u, cx - 2 * u, cy), Sdf.Triangle(cx + 2 * u, cy - 14 * u, cx + 2 * u, cy + 14 * u, cx + 24 * u, cy)); break;
                case "ico_silver": g = Sdf.Union(Sdf.Hexagon(cx, cy, 22 * u), Sdf.Scale(-1f, Sdf.Hexagon(cx, cy, 15 * u))); g = Sdf.Union(Sdf.Sub(Sdf.Hexagon(cx, cy, 22 * u), Sdf.Hexagon(cx, cy, 16 * u)), Sdf.Hexagon(cx, cy, 9 * u)); break;
                case "ico_men":
                    // a Brodie helmet side-on: a shallow dome on a wide flat brim, the crown stud on top (R0.5: the old
                    // narrow brim read as a bowl)
                    g = Sdf.Union(Sdf.Sub(Sdf.Circle(cx, cy + 8 * u, 17 * u), Sdf.Box(cx, cy + 8 * u + 20 * u, 40 * u, 20 * u)),
                        Sdf.Union(Sdf.Capsule(cx - 27 * u, cy + 10 * u, cx + 27 * u, cy + 10 * u, 3.2f * u), Sdf.Circle(cx, cy - 10 * u, 3 * u)));
                    break;
                case "ico_clock": g = Sdf.Union(Sdf.Ring(cx, cy, 22 * u, 4 * u), Sdf.Union(Sdf.Segment(cx, cy, cx, cy - 12 * u, 2.5f * u), Sdf.Segment(cx, cy, cx + 9 * u, cy + 5 * u, 2.5f * u))); break;
                case "ico_settings": g = Gear(cx, cy, u); break;
                case "ico_close": g = Sdf.Union(Sdf.Segment(cx - 16 * u, cy - 16 * u, cx + 16 * u, cy + 16 * u, 4 * u), Sdf.Segment(cx + 16 * u, cy - 16 * u, cx - 16 * u, cy + 16 * u, 4 * u)); break;
                case "ico_check": g = Sdf.Union(Sdf.Segment(cx - 18 * u, cy + 2 * u, cx - 6 * u, cy + 16 * u, 4.5f * u), Sdf.Segment(cx - 6 * u, cy + 16 * u, cx + 20 * u, cy - 14 * u, 4.5f * u)); break;
                case "ico_key": g = Sdf.Union(Sdf.Ring(cx - 12 * u, cy, 10 * u, 4 * u), Sdf.Union(Sdf.Box(cx + 6 * u, cy, 16 * u, 2.5f * u), Sdf.Union(Sdf.Box(cx + 14 * u, cy + 5 * u, 2.5f * u, 5 * u), Sdf.Box(cx + 21 * u, cy + 4 * u, 2.5f * u, 4 * u)))); break;
                case "ico_bullet": g = Sdf.Union(Sdf.Circle(cx, cy, 9 * u), Sdf.Ring(cx, cy, 20 * u, 3 * u)); break;
                case "ico_missing": g = Question(cx, cy, u); break;
                case "ico_arrow_down": g = Sdf.Triangle(cx - 12 * u * 2, cy - 6 * u * 2, cx + 12 * u * 2, cy - 6 * u * 2, cx, cy + 8 * u * 2); break;
                case "ico_arrow_left": g = Sdf.Triangle(cx + 6 * u * 2, cy - 12 * u * 2, cx + 6 * u * 2, cy + 12 * u * 2, cx - 8 * u * 2, cy); break;
                case "ico_arrow_right": g = Sdf.Triangle(cx - 6 * u * 2, cy - 12 * u * 2, cx - 6 * u * 2, cy + 12 * u * 2, cx + 8 * u * 2, cy); break;
                case "ico_rifle": g = Sdf.Union(Sdf.Segment(cx - 24 * u, cy + 4 * u, cx + 26 * u, cy - 6 * u, 3 * u), Sdf.Union(Sdf.Box(cx - 14 * u, cy + 10 * u, 6 * u, 5 * u), Sdf.Segment(cx + 4 * u, cy - 2 * u, cx + 4 * u, cy + 9 * u, 2.5f * u))); break;
                case "ico_smg": g = Sdf.Union(Sdf.Box(cx, cy, 24 * u, 4 * u), Sdf.Union(Sdf.Box(cx - 6 * u, cy + 10 * u, 4 * u, 8 * u), Sdf.Union(Sdf.Box(cx + 8 * u, cy + 8 * u, 3 * u, 6 * u), Sdf.Box(cx - 22 * u, cy + 6 * u, 4 * u, 4 * u)))); break;
                case "ico_mg": g = Sdf.Union(Sdf.Box(cx + 2 * u, cy - 4 * u, 26 * u, 4 * u), Sdf.Union(Sdf.Segment(cx - 8 * u, cy, cx - 18 * u, cy + 18 * u, 2.5f * u), Sdf.Union(Sdf.Segment(cx + 4 * u, cy, cx + 14 * u, cy + 18 * u, 2.5f * u), Sdf.Box(cx - 22 * u, cy + 2 * u, 4 * u, 6 * u)))); break;
                default: g = Question(cx, cy, u); break;
            }
            c.Glyph(g, White, EdgeDark);
        }

        static Func<float, float, float> Chevrons(float cx, float cy, float u, int dir)
        {
            float w = 4f * u, h = 16f * u, d = 12f * u * dir;
            Func<float, float, float> One(float ox) => Sdf.Union(Sdf.Segment(ox - d, cy - h, ox, cy, w), Sdf.Segment(ox, cy, ox - d, cy + h, w));
            return Sdf.Union(One(cx - 6f * u * dir), One(cx + 12f * u * dir));
        }

        static Func<float, float, float> Lock(float cx, float cy, float u, bool open)
        {
            var body = Sdf.Box(cx, cy + 8 * u, 18 * u, 14 * u);
            var hole = Sdf.Union(Sdf.Circle(cx, cy + 6 * u, 3.5f * u), Sdf.Box(cx, cy + 12 * u, 2 * u, 5 * u));
            float sx = open ? cx - 8 * u : cx;
            var shackle = Sdf.Sub(Sdf.Sub(Sdf.Circle(sx, cy - 8 * u, 13 * u), Sdf.Circle(sx, cy - 8 * u, 7 * u)), Sdf.Box(sx, cy + 2 * u, 30 * u, 10 * u));
            if (open) shackle = Sdf.Sub(shackle, Sdf.Box(sx + 10 * u, cy - 4 * u, 8 * u, 8 * u));
            var legs = open
                ? Sdf.Union(Sdf.Box(sx - 10 * u, cy - 4 * u, 3 * u, 6 * u), Sdf.Box(sx + 10 * u, cy - 12 * u, 3 * u, 4 * u))
                : Sdf.Union(Sdf.Box(sx - 10 * u, cy - 4 * u, 3 * u, 6 * u), Sdf.Box(sx + 10 * u, cy - 4 * u, 3 * u, 6 * u));
            return Sdf.Union(Sdf.Sub(body, hole), Sdf.Union(shackle, legs));
        }

        static Func<float, float, float> Crosshair(float cx, float cy, float u) =>
            Sdf.Union(Sdf.Ring(cx, cy, 20 * u, 4 * u), Sdf.Union(Sdf.Circle(cx, cy, 4 * u),
            Sdf.Union(Sdf.Box(cx, cy - 22 * u, 2 * u, 7 * u), Sdf.Union(Sdf.Box(cx, cy + 22 * u, 2 * u, 7 * u),
            Sdf.Union(Sdf.Box(cx - 22 * u, cy, 7 * u, 2 * u), Sdf.Box(cx + 22 * u, cy, 7 * u, 2 * u))))));

        static Func<float, float, float> Gear(float cx, float cy, float u)
        {
            Func<float, float, float> g = Sdf.Sub(Sdf.Circle(cx, cy, 16 * u), Sdf.Circle(cx, cy, 7 * u));
            for (int k = 0; k < 8; k++)
            {
                float a = k * Mathf.PI / 4f;
                g = Sdf.Union(g, Sdf.RotBox(cx + Mathf.Cos(a) * 18 * u, cy + Mathf.Sin(a) * 18 * u, 4 * u, 5 * u, a));
            }
            return g;
        }

        static Func<float, float, float> Question(float cx, float cy, float u) =>
            Sdf.Union(Sdf.Sub(Sdf.Sub(Sdf.Circle(cx, cy - 8 * u, 13 * u), Sdf.Circle(cx, cy - 8 * u, 7 * u)), Sdf.Box(cx - 10 * u, cy + 2 * u, 12 * u, 10 * u)),
            Sdf.Union(Sdf.Box(cx, cy + 6 * u, 3 * u, 6 * u), Sdf.Circle(cx, cy + 20 * u, 4 * u)));

        /// <summary>A "?" in a faint disc: what a card shows for a unit the baker has not drawn yet.</summary>
        public static Color32[] PaintPortraitPlaceholder(string name)
        {
            int s = SkinSpec.PortraitSize;
            var c = new Canvas(s, s);
            float cx = s * 0.5f - 0.5f, cy = s * 0.5f - 0.5f;
            c.Glyph(Sdf.Circle(cx, cy, s * 0.42f), new Color32(0x23, 0x25, 0x2A, 200), EdgeDark);
            c.Glyph(Sdf.Ring(cx, cy, s * 0.42f - 4f, 2f), Plate500, null);
            c.Glyph(Question(cx, cy - 4f, s / 64f * 1.4f), Plate400, EdgeDark);
            // the name, as a bar of dots so each portrait differs (the verifier reads coverage, not text)
            int n = Mathf.Clamp(name.Length, 1, 12);
            for (int i = 0; i < n; i++) c.Rivet(cx - (n - 1) * 7f + i * 14f, cy + s * 0.34f, 3f, Plate500);
            return c.Px;
        }

        // ---- a small raster canvas ------------------------------------------------------------------------------
        public sealed class Canvas
        {
            public readonly int W, H;
            public readonly Color32[] Px;
            public Canvas(int w, int h) { W = w; H = h; Px = new Color32[w * h]; }

            public void Fill(int x, int y, int w, int h, Color32 c)
            {
                for (int j = Math.Max(0, y); j < Math.Min(H, y + h); j++)
                    for (int i = Math.Max(0, x); i < Math.Min(W, x + w); i++) Px[j * W + i] = c;
            }
            public void GradientV(int x, int y, int w, int h, Color32 top, Color32 bottom)
            {
                for (int j = Math.Max(0, y); j < Math.Min(H, y + h); j++)
                {
                    var c = Color32.Lerp(top, bottom, h <= 1 ? 0f : (j - y) / (float)(h - 1));
                    for (int i = Math.Max(0, x); i < Math.Min(W, x + w); i++) Px[j * W + i] = c;
                }
            }
            public void HLine(int y, Color32 c, int x0, int x1) { if (y < 0 || y >= H) return; for (int x = Math.Max(0, x0); x < Math.Min(W, x1); x++) Px[y * W + x] = c; }
            public void VLine(int x, Color32 c, int y0, int y1) { if (x < 0 || x >= W) return; for (int y = Math.Max(0, y0); y < Math.Min(H, y1); y++) Px[y * W + x] = c; }
            public void RectOutline(int x, int y, int w, int h, Color32 c) { HLine(y, c, x, x + w); HLine(y + h - 1, c, x, x + w); VLine(x, c, y, y + h); VLine(x + w - 1, c, y, y + h); }
            /// <summary>A 1 px line inset from the edge on all four sides, blended at the given alpha.</summary>
            public void InnerLine(int inset, Color32 c, byte alpha)
            {
                var cc = new Color32(c.r, c.g, c.b, alpha);
                for (int x = inset; x < W - inset; x++) { Blend(x, inset, cc); Blend(x, H - 1 - inset, cc); }
                for (int y = inset + 1; y < H - 1 - inset; y++) { Blend(inset, y, cc); Blend(W - 1 - inset, y, cc); }
            }
            public void Blend(int x, int y, Color32 c)
            {
                if (x < 0 || y < 0 || x >= W || y >= H) return;
                var d = Px[y * W + x];
                float a = c.a / 255f, da = d.a / 255f, oa = a + da * (1f - a);
                if (oa <= 0f) { Px[y * W + x] = new Color32(0, 0, 0, 0); return; }
                float r = (c.r * a + d.r * da * (1f - a)) / oa, g = (c.g * a + d.g * da * (1f - a)) / oa, b = (c.b * a + d.b * da * (1f - a)) / oa;
                Px[y * W + x] = new Color32((byte)Mathf.RoundToInt(r), (byte)Mathf.RoundToInt(g), (byte)Mathf.RoundToInt(b), (byte)Mathf.RoundToInt(oa * 255f));
            }
            /// <summary>The house corner: a quarter circle of radius r in each corner, anti-aliased, with a dark edge
            /// of edgePx following the arc so the plate's straight 2 px edge carries round the curve. The USS clips each
            /// component to the same radius (--tw-radius-*), so the painted and the clipped corner coincide.</summary>
            public void RoundCorners(int r, float edgePx = 2f)
            {
                if (r <= 0) return;
                r = Math.Min(r, Math.Min(W, H) / 2);
                for (int y = 0; y < H; y++) for (int x = 0; x < W; x++)
                {
                    int ex = Math.Min(x, W - 1 - x), ey = Math.Min(y, H - 1 - y);
                    if (ex >= r || ey >= r) continue;
                    float dx = r - (ex + 0.5f), dy = r - (ey + 0.5f);
                    float d = Mathf.Sqrt(dx * dx + dy * dy) - r;          // > 0 outside the arc
                    int i = y * W + x;
                    if (d >= 0.5f) { Px[i] = new Color32(0, 0, 0, 0); continue; }
                    var p = Px[i];
                    if (edgePx > 0f && d > -edgePx - 0.5f)
                    {
                        float k = Mathf.Clamp01(d + edgePx + 0.5f);          // 1 on the edge band, fading inward over 1 px
                        p = Color32.Lerp(p, new Color32(0x0A, 0x0B, 0x0C, p.a), k);
                    }
                    p.a = (byte)Mathf.RoundToInt(p.a * Mathf.Clamp01(0.5f - d));
                    Px[i] = p;
                }
            }
            /// <summary>
            /// Recolour the rounded corners' outer ring so a recessed window's lip (bottom and right) runs round the arcs:
            /// all lip in the bottom-right corner, blending into the dark rim towards 12 o'clock in the top-right and towards
            /// 9 o'clock in the bottom-left, rim in the top-left. Call after RoundCorners with the same radius.
            /// </summary>
            public void ArcLip(int r, Color32 lip)
            {
                if (r <= 0) return;
                r = Math.Min(r, Math.Min(W, H) / 2);
                var rim = new Color32(0x0A, 0x0B, 0x0C, 255);
                for (int y = 0; y < H; y++) for (int x = 0; x < W; x++)
                {
                    int ex = Math.Min(x, W - 1 - x), ey = Math.Min(y, H - 1 - y);
                    if (ex >= r || ey >= r) continue;
                    float dx = r - (ex + 0.5f), dy = r - (ey + 0.5f);
                    float d = Mathf.Sqrt(dx * dx + dy * dy) - r;
                    int i = y * W + x;
                    if (Px[i].a == 0 || d < -1.5f) continue;   // only the outer ring
                    bool right = x >= W / 2, bottom = y >= H / 2;
                    float w = right && bottom ? 1f : right ? dx / (dx + dy + 1e-4f) : bottom ? dy / (dx + dy + 1e-4f) : 0f;
                    var col = Color32.Lerp(rim, lip, w);
                    col.a = Px[i].a;
                    Px[i] = col;
                }
            }
            /// <summary>
            /// Carry a plate's 1 px bevel round its corner arcs, on the ring just inside RoundCorners' 2 px dark edge:
            /// the highlight all round the top-left arc, the shade all round the bottom-right one, and across the
            /// top-right and bottom-left arcs each fades into the face at 45 degrees (highlight towards 12 and 9 o'clock).
            /// </summary>
            public void ArcBevel(int r, Color32 hi, Color32 lo)
            {
                if (r <= 3) return;
                r = Math.Min(r, Math.Min(W, H) / 2);
                for (int y = 0; y < H; y++) for (int x = 0; x < W; x++)
                {
                    int ex = Math.Min(x, W - 1 - x), ey = Math.Min(y, H - 1 - y);
                    if (ex >= r || ey >= r) continue;
                    float dx = r - (ex + 0.5f), dy = r - (ey + 0.5f);
                    float d = Mathf.Sqrt(dx * dx + dy * dy) - r;
                    if (d < -3.5f || d >= -2.5f) continue;   // the ring one pixel inside the dark edge
                    int i = y * W + x;
                    if (Px[i].a == 0) continue;
                    bool right = x >= W / 2, bottom = y >= H / 2;
                    float up = dy / (dx + dy + 1e-4f);   // 1 at 12 or 6 o'clock, 0 at 3 or 9
                    Color32 target; float w;
                    if (!right && !bottom) { target = hi; w = 1f; }
                    else if (right && bottom) { target = lo; w = 1f; }
                    else if (right) { target = hi; w = Mathf.Clamp01((up - 0.5f) * 2f); }          // top-right: highlight near the top
                    else { target = hi; w = Mathf.Clamp01((0.5f - up) * 2f); }                      // bottom-left: highlight near the left
                    var col = Color32.Lerp(Px[i], target, w);
                    col.a = Px[i].a;
                    Px[i] = col;
                }
            }
            public void Rivet(float cx, float cy, float r, Color32? col = null)
            {
                var c = col ?? Hex(0x33363B);
                Glyph(Sdf.Circle(cx, cy, r), c, Hex(0x0A0B0C));
                Glyph(Sdf.Circle(cx - r * 0.35f, cy - r * 0.35f, r * 0.35f), Color32.Lerp(c, new Color32(255, 255, 255, 255), 0.35f), null);
            }
            public static uint Hash(int x, int y, int seed)
            {
                uint h = (uint)(x * 374761393) ^ (uint)(y * 668265263) ^ (uint)(seed * 2147483647);
                h = (h ^ (h >> 13)) * 1274126177u;
                return h ^ (h >> 16);
            }
            /// <summary>Value noise on the border ring only (inside `border` px of the edge) or everywhere when border is 0.</summary>
            public void Grain(int x, int y, int w, int h, int border, float amount, int seed, bool everywhere = false)
            {
                for (int j = y; j < y + h; j++) for (int i = x; i < x + w; i++)
                {
                    if (i < 0 || j < 0 || i >= W || j >= H) continue;
                    bool ring = everywhere || border <= 0 || i < x + border || i >= x + w - border || j < y + border || j >= y + h - border;
                    if (!ring) continue;
                    float n = (Hash(i, j, seed) & 1023) / 1023f - 0.5f;
                    var p = Px[j * W + i];
                    if (p.a == 0) continue;
                    float k = 1f + n * amount * 2f;
                    Px[j * W + i] = new Color32((byte)Mathf.Clamp(Mathf.RoundToInt(p.r * k), 0, 255), (byte)Mathf.Clamp(Mathf.RoundToInt(p.g * k), 0, 255), (byte)Mathf.Clamp(Mathf.RoundToInt(p.b * k), 0, 255), p.a);
                }
            }
            /// <summary>Fill where sdf &lt;= 0, 4x4 supersampled; an optional 1 px contour outside it first.</summary>
            public void Glyph(Func<float, float, float> sdf, Color32 fill, Color32? contour)
            {
                if (contour.HasValue) Coverage((x, y) => sdf(x, y) - 1.1f, contour.Value, 0.9f);
                Coverage(sdf, fill, 1f);
            }
            void Coverage(Func<float, float, float> sdf, Color32 col, float alphaScale)
            {
                const int S = 4;
                for (int y = 0; y < H; y++) for (int x = 0; x < W; x++)
                {
                    int inside = 0;
                    for (int sy = 0; sy < S; sy++) for (int sx = 0; sx < S; sx++)
                        if (sdf(x + (sx + 0.5f) / S, y + (sy + 0.5f) / S) <= 0f) inside++;
                    if (inside == 0) continue;
                    byte a = (byte)Mathf.RoundToInt(col.a * (inside / (float)(S * S)) * alphaScale);
                    Blend(x, y, new Color32(col.r, col.g, col.b, a));
                }
            }
        }

        /// <summary>Signed distance primitives (negative inside), composed with min/max.</summary>
        public static class Sdf
        {
            public static Func<float, float, float> Circle(float cx, float cy, float r) => (x, y) => Mathf.Sqrt((x - cx) * (x - cx) + (y - cy) * (y - cy)) - r;
            public static Func<float, float, float> Ring(float cx, float cy, float r, float thick) => (x, y) => Mathf.Abs(Mathf.Sqrt((x - cx) * (x - cx) + (y - cy) * (y - cy)) - (r - thick * 0.5f)) - thick * 0.5f;
            public static Func<float, float, float> Box(float cx, float cy, float hw, float hh) => (x, y) => { float dx = Mathf.Abs(x - cx) - hw, dy = Mathf.Abs(y - cy) - hh; return Mathf.Sqrt(Mathf.Max(dx, 0f) * Mathf.Max(dx, 0f) + Mathf.Max(dy, 0f) * Mathf.Max(dy, 0f)) + Mathf.Min(Mathf.Max(dx, dy), 0f); };
            public static Func<float, float, float> RoundBox(float cx, float cy, float hw, float hh, float r) => (x, y) =>
            {
                float qx = Mathf.Abs(x - cx) - hw + r, qy = Mathf.Abs(y - cy) - hh + r;
                return Mathf.Sqrt(Mathf.Max(qx, 0f) * Mathf.Max(qx, 0f) + Mathf.Max(qy, 0f) * Mathf.Max(qy, 0f)) + Mathf.Min(Mathf.Max(qx, qy), 0f) - r;
            };
            public static Func<float, float, float> RotBox(float cx, float cy, float hw, float hh, float angle)
            {
                float c = Mathf.Cos(-angle), s = Mathf.Sin(-angle);
                var b = Box(0f, 0f, hw, hh);
                return (x, y) => { float px = x - cx, py = y - cy; return b(px * c - py * s, px * s + py * c); };
            }
            public static Func<float, float, float> Segment(float x0, float y0, float x1, float y1, float thick) => (x, y) =>
            {
                float dx = x1 - x0, dy = y1 - y0, l2 = dx * dx + dy * dy;
                float t = l2 <= 0f ? 0f : Mathf.Clamp01(((x - x0) * dx + (y - y0) * dy) / l2);
                float qx = x0 + t * dx - x, qy = y0 + t * dy - y;
                return Mathf.Sqrt(qx * qx + qy * qy) - thick * 0.5f;
            };
            public static Func<float, float, float> Capsule(float x0, float y0, float x1, float y1, float r) => Segment(x0, y0, x1, y1, r * 2f);
            public static Func<float, float, float> Hexagon(float cx, float cy, float r) => (x, y) =>
            {
                float px = Mathf.Abs(x - cx), py = Mathf.Abs(y - cy);
                const float kx = -0.8660254f, ky = 0.5f, kz = 0.57735f;
                float d = 2f * Mathf.Min(kx * px + ky * py, 0f); px -= d * kx; py -= d * ky;
                px -= Mathf.Clamp(px, -kz * r, kz * r); py -= r;
                return Mathf.Sqrt(px * px + py * py) * Mathf.Sign(py);
            };
            public static Func<float, float, float> Triangle(float ax, float ay, float bx, float by, float cx, float cy)
            {
                // the winding is fixed once per triangle, so each edge keeps one outward normal (a per-point sign made
                // every edge distance non-negative and every triangle empty)
                float k = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax) >= 0f ? 1f : -1f;
                return (x, y) => Mathf.Max(Mathf.Max(Edge(ax, ay, bx, by, x, y, k), Edge(bx, by, cx, cy, x, y, k)), Edge(cx, cy, ax, ay, x, y, k));
            }
            static float Edge(float ax, float ay, float bx, float by, float x, float y, float k)
            {
                float ex = bx - ax, ey = by - ay, len = Mathf.Sqrt(ex * ex + ey * ey);
                if (len <= 0f) return 0f;
                return k * ((x - ax) * ey - (y - ay) * ex) / len;   // signed distance to the edge's line, positive outside
            }
            public static Func<float, float, float> Union(Func<float, float, float> a, Func<float, float, float> b) => (x, y) => Mathf.Min(a(x, y), b(x, y));
            public static Func<float, float, float> Sub(Func<float, float, float> a, Func<float, float, float> b) => (x, y) => Mathf.Max(a(x, y), -b(x, y));
            public static Func<float, float, float> Scale(float k, Func<float, float, float> a) => (x, y) => a(x, y) * k;
        }
    }
}
