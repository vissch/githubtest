// Phase: B6 (implemented) — the single table of every sprite the interface skin is made of.
// The importer (UiSkinImport) applies each entry's border and settings, the generator (UiSkinGenerator) paints a
// placeholder for it, the verifier (UiSkinVerifier / SkinAssetTests) checks that the file on disk matches it, and
// docs/16-ui-art-spec.md must mention every name in it. No file name is typed anywhere else, so an artist's
// replacement PNG can only be wrong in ways this table catches.
namespace TW.UI
{
    public enum SkinKind : byte
    {
        Plate,     // painted in final colour, 9-sliced, never tinted
        Glyph,     // white on transparent with a dark contour; coloured only through -unity-background-image-tint-color
        Element,   // painted, not a plate: knobs, masks, checkboxes, frames with a transparent centre
        Portrait,  // 256x256 RGBA unit / support portrait, transparent background
    }

    public readonly struct SkinEntry
    {
        public readonly string File;     // relative to SkinSpec.Root, forward slashes
        public readonly int W, H;        // pixel size
        public readonly int L, B, R, T;  // 9-slice border in px, TextureImporter.spriteBorder order (left, bottom, right, top)
        public readonly SkinKind Kind;
        public readonly bool Tiled;      // -unity-slice-type: tiled along the stretch axis (straps, hatch)
        public readonly string Note;     // one line for the spec sheet

        public SkinEntry(string file, int w, int h, int l, int b, int r, int t, SkinKind kind, string note, bool tiled = false)
        { File = file; W = w; H = h; L = l; B = b; R = r; T = t; Kind = kind; Note = note; Tiled = tiled; }

        public SkinEntry(string file, int w, int h, int border, SkinKind kind, string note, bool tiled = false)
            : this(file, w, h, border, border, border, border, kind, note, tiled) { }

        public string Path => SkinSpec.Root + File;
        public bool HasBorder => L + B + R + T > 0;
    }

    public static class SkinSpec
    {
        public const string Root = "Assets/_Project/UI/Skin/";
        /// <summary>Bump when the generator's drawing changes so recorded placeholders are repainted.</summary>
        public const int GeneratorVersion = 2;
        public const int PortraitSize = 256;

        static SkinEntry P(string f, int w, int h, int b, string n) => new SkinEntry("Sprites/" + f, w, h, b, SkinKind.Plate, n);
        static SkinEntry P(string f, int w, int h, int l, int bo, int r, int t, string n, bool tiled = false) => new SkinEntry("Sprites/" + f, w, h, l, bo, r, t, SkinKind.Plate, n, tiled);
        static SkinEntry E(string f, int w, int h, int b, string n, bool tiled = false) => new SkinEntry("Sprites/" + f, w, h, b, SkinKind.Element, n, tiled);
        static SkinEntry E(string f, int w, int h, int l, int bo, int r, int t, string n, bool tiled = false) => new SkinEntry("Sprites/" + f, w, h, l, bo, r, t, SkinKind.Element, n, tiled);
        static SkinEntry G(string f, int s, string n) => new SkinEntry("Icons/" + f, s, s, 0, SkinKind.Glyph, n);

        public static readonly SkinEntry[] All =
        {
            // ---- plates: the riveted gunmetal the whole interface stands on ------------------------------------
            P("plate_normal.png", 64, 64, 16, "base button/panel plate: cut corners, 2 px dark edge, 1 px light bevel top-left, corner rivets"),
            P("plate_hover.png", 64, 64, 16, "plate_normal +8% value, 1 px amber inner line at 40%"),
            P("plate_pressed.png", 64, 64, 16, "plate_normal -10% value, bevel inverted"),
            P("plate_disabled.png", 64, 64, 16, "plate-800 fill, plate-400 rivets, no light bevel"),
            P("btn_accent_normal.png", 64, 64, 16, "amber-lit plate, brass rivets, brass inner line (primary buttons)"),
            P("btn_accent_hover.png", 64, 64, 16, "btn_accent_normal, brighter"),
            P("btn_accent_pressed.png", 64, 64, 16, "btn_accent_normal, darker, bevel inverted"),
            P("panel_bg.png", 128, 128, 24, "deeper plate for menu panels, larger corner bolts, 1 px rust inner line"),
            P("order_plate_normal.png", 64, 64, 16, "square trench-order button plate, heavier bevel than plate_normal"),
            P("order_plate_hover.png", 64, 64, 16, "order_plate_normal, hover"),
            P("order_plate_pressed.png", 64, 64, 16, "order_plate_normal, pressed"),
            P("order_plate_disabled.png", 64, 64, 16, "order_plate_normal, disabled"),
            P("gauge_window.png", 32, 32, 6, "recessed dark window the amber digits sit in (silver, men, timer, hotkey badge)"),
            P("badge.png", 32, 32, 6, "small hard-cornered plate: counts and hotkeys on cards"),
            P("card_nameplate.png", 64, 20, 6, 0, 6, 0, "dark strap across a card's foot for the unit name"),
            P("keycap.png", 40, 40, 10, "key-rebind cap: plate-500 top face, 3 px darker lip in the bottom rows"),
            P("row_plate.png", 64, 40, 12, 8, 12, 8, "settings / debrief row"),
            P("tab_plate.png", 64, 40, 12, 8, 12, 8, "list / mission tab"),
            P("tab_plate_selected.png", 64, 40, 12, 8, 12, 8, "tab_plate with an amber top edge"),
            P("tooltip_plate.png", 48, 48, 12, "plate-800 with a 1 px rust edge"),
            P("title_plate.png", 256, 96, 64, 24, 64, 24, "screen heading strap, two bolts per end; stretches horizontally"),
            P("banner_ribbon.png", 512, 128, 96, 0, 96, 0, "centre banner: angled torn steel ends, rust lines top and bottom, flat centre"),
            P("hazard_strip.png", 64, 8, 8, 0, 8, 0, "CAUTION micro-text strap along plate edges", tiled: true),

            // ---- elements ----------------------------------------------------------------------------------------
            E("card_frame.png", 80, 80, 14, "card ring, transparent centre: 10 px plate ring, 2 px dark inner edge, 4 px cut corners"),
            E("card_rim.png", 80, 80, 14, "WHITE 3 px ring at the frame's inner edge + 2 px glow; tinted for selected / armed / hover"),
            E("cooldown_mask.png", 32, 32, 0, "rgba(8,9,10,.62) with a 1 px 45-degree hatch every 6 px; seamless", tiled: true),
            E("divider_v.png", 8, 64, 0, 8, 0, 8, "vertical rivet strap, rust edges", tiled: true),
            E("divider_h.png", 64, 8, 8, 0, 8, 0, "horizontal rivet strap", tiled: true),
            E("minimap_frame.png", 96, 96, 24, "heavy 24 px square bezel, corner bolts only, transparent centre"),
            E("bezel_round.png", 128, 128, 0, "round radar bezel, transparent centre (scale-to-fit; the Dust Front option)"),
            E("order_round_normal.png", 64, 64, 0, "round riveted order plate (scale-to-fit alternative to order_plate_*)"),
            E("order_round_hover.png", 64, 64, 0, "order_round_normal, hover"),
            E("order_round_pressed.png", 64, 64, 0, "order_round_normal, pressed"),
            E("order_round_disabled.png", 64, 64, 0, "order_round_normal, disabled"),
            E("slider_track.png", 32, 12, 6, 0, 6, 0, "dark groove, 1 px light bottom edge"),
            E("slider_fill.png", 32, 12, 6, 0, 6, 0, "amber fill with darker 1 px edges"),
            E("slider_knob.png", 24, 24, 0, "hexagonal knob with a centre rivet"),
            E("slider_knob_hover.png", 24, 24, 0, "slider_knob with a brass cap"),
            E("checkbox_off.png", 24, 24, 0, "24 px plate"),
            E("checkbox_on.png", 24, 24, 0, "checkbox_off with an amber X stamp (two 3 px diagonals)"),
            E("dim_vignette.png", 256, 256, 64, "#08090A, alpha .55 centre to .85 edge: the screen dim behind menus"),
            E("grain_overlay.png", 256, 256, 0, "film grain, alpha .06: the whole-screen overlay", tiled: true),
            E("scroller_track.png", 12, 32, 0, 6, 0, 6, "vertical scroll track"),
            E("scroller_knob.png", 12, 32, 0, 6, 0, 6, "vertical scroll knob"),
            E("stamp_victory.png", 128, 128, 0, "WHITE rubber-stamp glyph, tinted accent (debrief)"),
            E("stamp_defeat.png", 128, 128, 0, "WHITE rubber-stamp glyph, tinted alarm (debrief)"),

            // ---- glyphs: white on transparent, 1 px dark contour, 4 px safe margin, 3 px minimum stroke --------
            G("ico_fallback.png", 64, "double chevron left (BattleHud chevrons, mirrored)"),
            G("ico_overthetop.png", 64, "double chevron right"),
            G("ico_lock_closed.png", 64, "padlock, shackle down"),
            G("ico_lock_open.png", 64, "padlock, shackle up"),
            G("ico_fireatwill.png", 64, "crosshair: ring, four ticks, centre dot"),
            G("ico_holdfire.png", 64, "crosshair with a 5 px diagonal bar (the bar is part of the white glyph)"),
            G("ico_barrage.png", 64, "shell over three burst chevrons"),
            G("ico_gas.png", 64, "cloud over three drops"),
            G("ico_pause.png", 64, "two bars"),
            G("ico_play.png", 64, "triangle"),
            G("ico_speed.png", 64, "two triangles"),
            G("ico_silver.png", 64, "hexagonal ingot"),
            G("ico_men.png", 64, "helmet"),
            G("ico_clock.png", 64, "clock face"),
            G("ico_settings.png", 64, "gear"),
            G("ico_close.png", 64, "X"),
            G("ico_check.png", 64, "tick"),
            G("ico_key.png", 64, "key"),
            G("ico_bullet.png", 64, "objective bullet: disc with a ring"),
            G("ico_missing.png", 64, "a question mark: what a card shows when its portrait is absent"),
            G("ico_arrow_down.png", 32, "dropdown arrow"),
            G("ico_arrow_left.png", 32, "left arrow"),
            G("ico_arrow_right.png", 32, "right arrow"),
            G("ico_rifle.png", 32, "weapon badge for the Rifleman card"),
            G("ico_smg.png", 32, "weapon badge for the Assault card"),
            G("ico_mg.png", 32, "weapon badge for the MG card"),
        };

        /// <summary>
        /// Portrait file stems under Portraits/: one per archetype 0..11 in archetype order, then the two support
        /// emblems. HudText.PortraitName(archetype) yields the first twelve; UiSkinVerifier checks that every archetype
        /// RosterEntry.FillDefault hands out for EITHER side is here.
        /// </summary>
        public static readonly string[] PortraitNames =
        {
            "Rifleman", "Assault", "MG", "Sniper", "Maw", "Tusk", "Pincer", "Kettle", "Censer", "Pavise", "Banner", "Redoubt",
            "HeBarrage", "ChlorineGas",
        };

        /// <summary>Has a model under Resources/Vehicles but no archetype yet; baked when present, not required.</summary>
        public const string ReservedPortrait = "Cutter";

        public static string PortraitPath(string name) => Root + "Portraits/" + name + ".png";

        public static readonly string[] Fonts =
        {
            "Fonts/DustFrontDisplay.asset", "Fonts/DustFrontLabel.asset", "Fonts/DustFrontMono.asset",
        };

        public static bool TryGet(string file, out SkinEntry entry)
        {
            for (int i = 0; i < All.Length; i++)
                if (All[i].File == file) { entry = All[i]; return true; }
            entry = default;
            return false;
        }
    }
}
