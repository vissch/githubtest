// Phase: CelFx kit (2026-11-09) - the colours a cel primitive is banded in. A palette is a row of four colours from dark
// to light (the bands), a core colour that the record's HDR multiplier lifts above white (it blooms), the threshold of
// the band value above which the core shows, an ink colour (contours) and how many bands of the four are used (2..4,
// the lightest ones). One GPU buffer holds every row (_CelPalette); CelFxRenderer uploads it when it changes.
//
// Colours are given as they look (sRGB, as picked off a reference frame) and stored linear. Row 0 is plain white and
// means "none" where a recipe names no palette. Ids are handed out in the order palettes are added and never move.
using UnityEngine;
using TW.Presentation;

namespace TW.Presentation.VFX.Cel
{
    public static class CelPalette
    {
        public const int Max = 64, Stride = 6;
        static readonly Vector4[] rows = new Vector4[Max * Stride];
        static readonly string[] names = new string[Max];
        static int count;
        /// <summary>Bumped on every change; CelFxRenderer re-uploads when it differs from what it sent.</summary>
        public static int Version { get; private set; }
        public static int Count => count;
        internal static Vector4[] Rows => rows;

        // built-in rows every recipe may use; effect agents add their own with Add (their names, their files)
        public static readonly int White, Fire, FireHot, Smoke, SmokeDark, Dust, Spark, Ember, LaserGreen, LaserBlue, GlowWarm;

        static CelPalette()
        {
            White = Add("white", Hex(0x9A9A9A), Hex(0xC8C8C8), Hex(0xE8E8E8), Hex(0xFFFFFF), Hex(0xFFFFFF), 1.1f, 4, Hex(0x202020));
            // fire: soot red at the rim, orange body, yellow, a white heart (explosive_blasts, flamethrower)
            Fire = Add("fire", Hex(0xA8260C), Hex(0xF06A14), Hex(0xFFB22E), Hex(0xFFE58A), Hex(0xFFF6DC), 0.86f, 4, Hex(0x3A0E06));
            FireHot = Add("fire_hot", Hex(0xF06A14), Hex(0xFFB22E), Hex(0xFFE58A), Hex(0xFFF6DC), Hex(0xFFFFFF), 0.7f, 3, Hex(0x5A1A08));
            // smoke: the tan cartoon smoke wall, lit from above, dark brown ink
            Smoke = Add("smoke_tan", Hex(0x6E5A48), Hex(0x9C8670), Hex(0xC6B095), Hex(0xE0CFB4), Hex(0xE0CFB4), 1.1f, 3, Hex(0x2A1E16));
            SmokeDark = Add("smoke_black", Hex(0x0E0B0A), Hex(0x1E1816), Hex(0x332A25), Hex(0x4A3D35), Hex(0x4A3D35), 1.1f, 3, Hex(0x050404));
            Dust = Add("dust", Hex(0x7A6650), Hex(0xA08A6E), Hex(0xC2AD8E), Hex(0xD8C6A6), Hex(0xD8C6A6), 1.1f, 3, Hex(0x3A2E22));
            Spark = Add("spark", Hex(0xC0400A), Hex(0xFF8A1E), Hex(0xFFC860), Hex(0xFFF0C0), Hex(0xFFFFFF), 0.72f, 3, Hex(0x401004));
            Ember = Add("ember", Hex(0x801E06), Hex(0xD0480C), Hex(0xFF8A24), Hex(0xFFC860), Hex(0xFFE8B0), 0.8f, 3, Hex(0x200802));
            LaserGreen = Add("laser_green", Hex(0x1E6A10), Hex(0x46C81E), Hex(0x96FF4A), Hex(0xD8FFA8), Hex(0xF4FFE6), 0.72f, 3, Hex(0x0C2A06));
            LaserBlue = Add("laser_blue", Hex(0x103C8C), Hex(0x2A7AFF), Hex(0x78C0FF), Hex(0xC8ECFF), Hex(0xF2FAFF), 0.72f, 3, Hex(0x061436));
            GlowWarm = Add("glow_warm", Hex(0x40160A), Hex(0x8A3812), Hex(0xD0701E), Hex(0xFFB050), Hex(0xFFD890), 0.9f, 3, Hex(0x000000));
            // the rows survive a Play session (ids are cached in recipes); the GPU copy is sent again by the next renderer
            SceneStatics.Register(nameof(CelPalette), () => Version++);
        }

        /// <summary>A colour from 0xRRGGBB, as it looks.</summary>
        public static Color Hex(int rgb) => new Color(((rgb >> 16) & 255) / 255f, ((rgb >> 8) & 255) / 255f, (rgb & 255) / 255f, 1f);

        /// <summary>Add a palette (or replace one of the same name) and return its row. c0..c3 dark to light; core is
        /// lifted by the layer's HDR multiplier; coreAt is the band value (0..1) above which the core shows (1.1: never);
        /// bands 2..4 uses the lightest n of c0..c3; ink is the contour colour.</summary>
        public static int Add(string name, Color c0, Color c1, Color c2, Color c3, Color core, float coreAt, int bands, Color ink)
        {
            int row = Find(name);
            if (row < 0)
            {
                if (count >= Max) { Debug.LogWarning("CelPalette: full (" + Max + "); " + name + " is drawn white"); return 0; }
                row = count++;
                names[row] = name;
            }
            int o = row * Stride;
            rows[o] = c0.linear; rows[o + 1] = c1.linear; rows[o + 2] = c2.linear; rows[o + 3] = c3.linear;
            var core4 = (Vector4)core.linear; core4.w = coreAt; rows[o + 4] = core4;
            var ink4 = (Vector4)ink.linear; ink4.w = Mathf.Clamp(bands, 1, 4); rows[o + 5] = ink4;
            Version++;
            return row;
        }

        /// <summary>The row of a palette by name, -1 when there is none. No allocation.</summary>
        public static int Find(string name)
        {
            for (int i = 0; i < count; i++) if (string.Equals(names[i], name, System.StringComparison.Ordinal)) return i;
            return -1;
        }

        public static string NameOf(int row) => row >= 0 && row < count ? names[row] : null;
    }
}
