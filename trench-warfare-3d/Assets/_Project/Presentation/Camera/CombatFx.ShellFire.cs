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

        /// <summary>Whether the day slimming (DayColumnWidth) applies: dry, a live shell fire, and not moonlit. The soil
        /// heave (AOSA C103) and the capped card (AOSA C108) branches are moonlit-only, so this is always false there -
        /// an invariant this method, not a comment, now guarantees.</summary>
        public static bool DayEarth(bool wet, float fire, bool moonLit) => !wet && fire > 0f && !moonLit;

        /// <summary>The one width every column branch (soil, capped, ordinary) draws at, so a retune of 2.1, columnScale
        /// or DayColumnWidth moves all three together. The damp (water) column is the wider of the two: a shell in water
        /// throws a crown taller and broader than an earth column, where 1.25 drew it at 60 % of dry earth and the probe
        /// of ability-fx-captures-r3-2-2 measured only 0.0239 of the T1 frame against a 0.0709 floor.</summary>
        public static float ColumnWidth(float r, bool damp, float columnScale, float earth, float lookColumn, bool dayEarth) =>
            r * (damp ? 2.3f : 2.1f) * columnScale * earth * (dayEarth ? DayColumnWidth : 1f) * lookColumn;

        // ability-fx-captures-r3-2-2-g2-g2-g2 (the owner, in the goal: "make the close splash a crown of water, not the
        // blue column in splash_T1.png"): the wet column was ONE upright Splash card, which read as a tall blue shaft with
        // two prongs. A crown is a squat mass with spray thrown outward and up around its rim, so the wet branch now draws
        // the centre card wider than tall (CrownHeight) and a ring of CrownJets narrow cards of the SAME Splash book around
        // it - one instanced draw a book, so no new draw call, no new sheet and no new Book row. The concepts rule is not
        // followed here on purpose: the owner named the look himself.
        public const int CrownJets = 6;              // the fingers around the rim; every other one mirrored, so they are not six copies
        // The first crown shot covered only 165-221 m^2 of ground against the 400 m^2 the probe asks of a splash, so the
        // crown was grown until it covered 617-695 - and it was grown UPWARD: rim 0.58, fingers 0.95 of the column width
        // and a centre 0.95 as tall as it is wide left six near-full-width upright cards standing in a ring. The owner
        // (ability-fx-captures-r3-2-2-g2-g2-g2-g1, in the goal) called that what it is: "the thicket of tall blue columns
        // in splash_T1_r1, splash_T1_r2 and splash_T1_r3", and asked for a LOW crown, "a squat centre of water with spray
        // thrown outward". Measured on the pixels, the thicket stood at height/width 0.74-0.78 at T1 and 1.10-1.11 at
        // O240 (the g1 probe's crown_ratio).
        // So the spray now goes OUT, not up: a squat centre (CrownHeight), slim fingers (CrownJetWidth) on a wider rim
        // (CrownRim), and CrownUp well under CrownOut. The 400 m^2 footprint is carried by the two flat foam cards on the
        // water instead of by the crown's height (CombatFx.cs, the wet branch) - the bar itself is never lowered to fit
        // an effect (decisions.md, 2026-10-19).
        public const float CrownRim = 0.85f;         // the rim, as a share of columnWidth: the spray goes out, so the ground stays covered
        public const float CrownJetWidth = 0.45f;    // a finger's width, as a share of columnWidth: fingers, not columns
        public const float CrownHeight = 0.40f;      // the centre card's height over its width: a squat mass, well under half as tall as wide
        public const float CrownOut = 5.0f, CrownUp = 1.0f;   // m/s: thrown outward, barely up (CrownUp < CrownOut: spray, not a shaft)

        /// <summary>Where jet <paramref name="i"/> of a crown of <paramref name="count"/> sits, as an offset from the splash
        /// point: on a circle of CrownRim * columnWidth, flat on the water (y = 0), the ring turned by <paramref name="turn"/>
        /// (a hash of the spot, so two splashes a metre apart do not line up and nothing is random per frame).</summary>
        public static Vector3 CrownJet(int i, int count, float columnWidth, float turn)
        {
            float a = turn + 6.2831853f * i / Mathf.Max(1, count), rim = CrownRim * columnWidth;
            return new Vector3(Mathf.Cos(a) * rim, 0f, Mathf.Sin(a) * rim);
        }

        /// <summary>The warm light on the young burst cloud's underside (0 with fx.shellFire 0): peach by day, an ember at night,
        /// where the moonlit grade was tuned against a darker cloud.</summary>
        public static Color EmberFor(bool night, float fire)
        {
            float k = Mathf.Clamp01(fire);
            return (night ? new Color(0.30f, 0.10f, 0.03f) : new Color(0.55f, 0.24f, 0.06f)) * k;   // on the lit billows now (_EmberLit), where 0.95 by day was a peach wash
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
            // well under the flamethrower's glow: at 2.0 by day and 2.4 at night the body (tint 1.4 x glow) clipped to cream and a
            // yellow-white slab (critique c2); and less again as it grows with the zoom, so a far fire stays orange
            float far = ShellFar();   // grown with the zoom
            float glow = (night ? 1.8f : 1.15f) * SceneTints.Now.Glow / Mathf.Sqrt(far), alpha = Mathf.Min(1f, shellFire) * (night ? 0.85f : 1f);
            float w = ShellFireWidth(r, closeUp) * Mathf.Min(1f, shellFire) * far;
            // centred low over the hole, not anchored and sunk: a sunk card's bottom edge is a ruler line on the snow
            books.Add(FlipbookFx.Book.Fireball, p + Vector3.up * (0.5f * r * far), w, ShellFireLife, FlipbookFx.Kind.Upright | (mirror ? FlipbookFx.Kind.Mirror : 0),
                velocity: rise * 0.35f, grow: 0.3f, alpha: alpha, glow: glow, pop: 0.25f, startFrame: 5f);   // past frames 2-4, a four-point star cel
            for (int k = 0; k < ShellPockets; k++)
                books.Add(FlipbookFx.Book.Fireball, p + PocketOffset(p, k, r) * far, w * 0.65f, PocketLife, (mirror ^ k == 1) ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                    velocity: rise, grow: 0.4f, alpha: alpha * PocketAlpha, glow: glow, pop: 0.3f, delay: 0.08f + 0.12f * k, startFrame: 6f + k * 3f);   // the fuller frames (4 and 6 read as glyphs)
        }

        /// <summary>How much the shell's fire grows with the zoom: from 40 (FlipbookFx.FarGrow starts at 80, so at z70 it was 1
        /// and the fire a pinprick in the blizzard: critique c1b), to 2.2 from 88. The cloud grows by its square root.</summary>
        public static float ShellFarAt(float zoom) => Mathf.Clamp(zoom / 40f, 1f, 2.2f);
        float ShellFar() => ShellFarAt(ViewEye.Zoom);

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
            // specks, not dots (critique c2: one-value octagons parked on the fire): twice as many fine at 0.055 m, a fifth coarse at 0.14
            int n = Mathf.RoundToInt(GritCount(r) * Mathf.Min(1f, shellFire) * shellGrit * FxQuality.Now.Grit), coarse = Mathf.RoundToInt(n * 0.2f);   // x the effects' tier
            debris.Burst(DebrisRenderer.Piece.Clod, p + Vector3.up * 0.6f, 2 * (n - coarse), 15f + r, 0.055f, Mud, 1.4f, 0f, 0.9f, lean, tick + 29u);
            debris.Burst(DebrisRenderer.Piece.Clod, p + Vector3.up * 0.6f, coarse, 12f + 0.8f * r, 0.14f, Mud, 1.4f, 0f, 0.9f, lean, tick + 31u);
        }
    }
}
