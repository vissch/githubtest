// Phase: VFX pass (owner, 2026-09-28: a reference still of the snow field with "explosions with more fidelity": a shell
// burst that is FIRE inside its smoke, not a flash in a grey cloud) — part of CombatFx. Bench snow0 (WinterLine, by day)
// drew no fire in a shell burst at all: the Flash card is additive, so on white snow it adds nothing, and what was left
// was a cold grey cloud. So every dry shell burst now also draws
//  - a FIREBALL: the pack's soot-flecked burst (Book.Fireball, the FireBurst sheet: fire with the smoke drawn into it)
//    over the hole, its eruption frames only (ShellFireLife), its drawn-in soot the burst cloud's own brown (FlipbookFx), so
//    the soot runs into the cloud behind and the flame reads as bursting out of it, and
//  - two FIRE POCKETS of the same book born a beat later up inside the rising cloud and riding up with it,
//  - and the cloud itself is lit warm from underneath while it is young (Flipbook_URP _Ember, EmberFor), denser than the
//    lingering smoke (FlipbookFx.BurstWeight), narrower by day (BurstWidth), with a sooty underside.
// History (bench s1-s3 and the harsh critique of s3): the flamethrower's Bloom/Head read as a cream slab; Blast/Fire, drawn
// over the cloud, as orange stickers pasted on a grey veil, with a ruler-cut base where an anchored card was sunk; c1 drew the
// fireball under the denser cloud, which hid it completely.
// Fire cards are premultiplied banded ink (Flipbook_URP), so unlike the flash they read on snow. fx.shellFire scales
// them (0: none, the burst as it was). No random draws: the pockets' places are a hash of the spot.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        public const string ShellFireKnob = "fx.shellFire", ShellGritKnob = "fx.shellGrit";
        public const float ShellFireLife = 1.35f;    // s: 16 of its 32 frames from frame 2, fading over the last third (at 0.9 the fire
                                                     // was dead at half the cloud's 1.7 s: critique c1b)
        public const float PocketLife = 1.25f;       // s
        public const int ShellPockets = 2;
        public const float PocketAlpha = 0.8f;       // a shade under the fireball's: fire further inside the cloud
        public const float DayColumnPlay = 0.5f;     // the thrown earth stops before its falling arcs by day as it does by night (fx.columnPlay)
        public const float DayColumnWidth = 0.7f;    // and is slimmer: the reference has chunks, no tall dark shafts (critique c1b)
        float shellFire = 1f, shellGrit = 1f;         // knobs fx.shellFire, fx.shellGrit (Awake; grit 0: none, 1: GritCount)

        /// <summary>The burst cloud's width over r: 2.6 as it was at night (AOSA-tuned) and with fx.shellFire 0; 2.2 by day,
        /// where the veil was the widest thing on the snow.</summary>
        public static float BurstWidth(bool night, float fire) => night || fire <= 0f ? 2.6f : 2.2f;

        /// <summary>The warm light on the young burst cloud's underside (0 with fx.shellFire 0): peach by day, an ember at night,
        /// where the moonlit grade was tuned against a darker cloud.</summary>
        public static Color EmberFor(bool night, float fire)
        {
            float k = Mathf.Clamp01(fire);
            return (night ? new Color(0.30f, 0.10f, 0.03f) : new Color(0.95f, 0.42f, 0.10f)) * k;   // by day past 0.55: the winter grade's -28 saturation ate half of it
        }

        /// <summary>Whether a burst is falling masonry (the sim's Dir.y shape 1): a wall coming down has dust, not fire.</summary>
        public static bool Masonry(float shape) => Mathf.RoundToInt(shape) == 1;

        public static float ReadShellFire() => Mathf.Clamp(Knobs.Get(ShellFireKnob, 1f), 0f, 2f);
        public static float ReadShellGrit() => Mathf.Clamp(Knobs.Get(ShellGritKnob, 1f), 0f, 2f);

        /// <summary>The fireball's width for a burst of radius r (2 to 9 m): about half the cloud (Burst is 2.2-2.6 r), and
        /// smaller up close, where the cloud is too (the flash comes down the same way).</summary>
        public static float ShellFireWidth(float r, float closeUp) => r * 1.9f * Mathf.Lerp(1f, 0.7f, Mathf.Clamp01(closeUp));   // the drawing fills 40 % of its cell

        /// <summary>Where fire pocket k sits over the hole, from a hash of the spot: low in the cloud, just over the fireball
        /// (0.45 to 0.95 r; bench s2 had them at up to 1.3 r, flames hanging in the air on their own) and off to one side,
        /// the two on opposite sides, never further out than half the cloud's width.</summary>
        public static Vector3 PocketOffset(Vector3 p, int k, float r)
        {
            uint h = (uint)Mathf.FloorToInt(p.x * 13f) * 73856093u ^ (uint)Mathf.FloorToInt(p.z * 13f) * 19349663u ^ (uint)(k + 1) * 83492791u;
            h ^= h >> 13; h *= 0x5bd1e995u; h ^= h >> 15;
            // one bearing for the spot (a hash without k), the odd pocket turned half a circle: the two on opposite sides
            uint g = (uint)Mathf.FloorToInt(p.x * 13f) * 2654435761u ^ (uint)Mathf.FloorToInt(p.z * 13f) * 40503u; g ^= g >> 15;
            float a = (g & 0x3FF) / 1024f * 6.2832f, side = 0.25f + ((h >> 10) & 0xFF) / 255f * 0.25f, up = 0.45f + ((h >> 18) & 0xFF) / 255f * 0.25f + k * 0.25f;
            if ((k & 1) == 1) a += 3.1416f;
            return new Vector3(Mathf.Cos(a) * side * r, up * r, Mathf.Sin(a) * side * r);
        }

        /// <summary>The shell's fire: the fireball on the hole and the pockets in the cloud. `rise` is the cloud's own velocity,
        /// so the pockets go up with it and stay inside it.</summary>
        void ShellFire(Vector3 p, float r, float closeUp, bool mirror, Vector3 rise)
        {
            if (shellFire <= 0f) return;
            bool night = SceneMood.Night;
            // the flamethrower's glow by day; at night less (critique s3: at 3.4 three near-white shapes outshone the tracers)
            float glow = (night ? 2.4f : 2.0f) * SceneTints.Now.Glow, alpha = Mathf.Min(1f, shellFire) * (night ? 0.85f : 1f);
            float far = ShellFar();   // grown with the zoom
            float w = ShellFireWidth(r, closeUp) * Mathf.Min(1f, shellFire) * far;
            // centred low over the hole, not anchored and sunk: a sunk card's bottom edge is a ruler line on the snow
            books.Add(FlipbookFx.Book.Fireball, p + Vector3.up * (0.5f * r * far), w, ShellFireLife, FlipbookFx.Kind.Upright | (mirror ? FlipbookFx.Kind.Mirror : 0),
                velocity: rise * 0.35f, grow: 0.3f, alpha: alpha, glow: glow, pop: 0.25f, startFrame: 2f);
            for (int k = 0; k < ShellPockets; k++)
                books.Add(FlipbookFx.Book.Fireball, p + PocketOffset(p, k, r) * far, w * 0.65f, PocketLife, (mirror ^ k == 1) ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                    velocity: rise, grow: 0.4f, alpha: alpha * PocketAlpha, glow: glow, pop: 0.3f, delay: 0.08f + 0.12f * k, startFrame: 6f + k * 3f);   // the fuller frames (4 and 6 read as glyphs)
        }

        /// <summary>How much the shell's fire grows with the zoom: from 40 (FlipbookFx.FarGrow starts at 80, so at z70 it was 1
        /// and the fire a pinprick in the blizzard: critique c1b), to 2.2 from 88. The cloud grows by its square root.</summary>
        public static float ShellFarAt(float zoom) => Mathf.Clamp(zoom / 40f, 1f, 2.2f);
        float ShellFar() { var eye = Camera.main; return ShellFarAt(eye != null && eye.TryGetComponent<IZoomSource>(out var zs) ? zs.CurrentZoom : 0f); }

        /// <summary>How many grains of grit a burst of radius r sprays (before DebrisRenderer's own share by distance and zoom).</summary>
        public static int GritCount(float r) => Mathf.RoundToInt(12f + 2.5f * Mathf.Clamp(r, 2f, 9f));

        /// <summary>The reference's grit: a spray of dark earth out of the burst, the fan of specks round the fireball that the
        /// clods (fist to head size, thrown up high) are not. Small clods in the Clod pool (no draw added), flung out and up
        /// along the shell's flight, gone in 1.4 s. At 0.05 m (s3) a grain was a pixel; at 0.12 m and 10 m/s (c1b) they sat
        /// on the fire as even dots. DebrisRenderer's own seeded stream: no random draws here.</summary>
        void ShellGrit(Vector3 p, float r, Vector3 lean, uint tick)
        {
            if (shellFire <= 0f || shellGrit <= 0f) return;
            // fast and short-lived, so it leaves the fire in the first frames instead of parking on it as polka dots, and two
            // sizes (critique c1b): seven in ten fine, three in ten coarse
            int n = Mathf.RoundToInt(GritCount(r) * Mathf.Min(1f, shellFire) * shellGrit), coarse = Mathf.RoundToInt(n * 0.3f);
            debris.Burst(DebrisRenderer.Piece.Clod, p + Vector3.up * 0.6f, n - coarse, 15f + r, 0.09f, Mud, 1.4f, 0f, 0.9f, lean, tick + 29u);
            debris.Burst(DebrisRenderer.Piece.Clod, p + Vector3.up * 0.6f, coarse, 12f + 0.8f * r, 0.2f, Mud, 1.4f, 0f, 0.9f, lean, tick + 31u);
        }
    }
}
