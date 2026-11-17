// Phase: CelFx job, flamethrower (2026-11-09, lane/show/cel-flamethrower) - the cel flamethrower after the owner's
// reference clip flamethrower_nolora: a broad cone of flame from the lower left that widens onto the target and
// engulfs it (FlameJet, its edge a row of attached tongues licking up, banded white, yellow, orange, a thin red rim),
// round curling fire lobes rolling up and downstream off the target and growing to the cut, a black ink smoke
// column of round lobes that grows from above the target up and downstream and thins to grey, a big orange light
// pool on the ground, embers and sparks; when the jet is cut the fire at the target burns down to a small remnant.
// Times are seconds after the trigger. b05: the jet now grows out of the nozzle from the trigger (a nozzle flash, half
// length by 0.4 s, on the dummy by ~0.85 s), and the Range films it fired at 0.29 s (capture -StageOpts t0=0.29), the
// clip's first flame frame (f8); every target layer moved 0.5 s later to match. Clip, in seconds after our fire: jet front on the target 0.45, jet cut 3.15-3.45, burning
// smoulder to 4.2, smoke from 1.2 (small), column to 4.5, grey and gone by 5.9.
// The effect's target is the dummy's upper chest (Play aims 0.45 m above the chest), so the jet's axis runs over the
// board like the clip's; the target layers' offsets are from there (the foot is 1.4 m below it).
// The Range hook: FlamethrowerRange.cs. LOD: Close all of it; Mid fewer pieces; Far the jet, the pool, one smoke lobe.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class FlamethrowerRecipe
    {
        public const string Flame = "ft_flame", Burn = "ft_burn", SmokeInk = "ft_smoke", SmokeGrey = "ft_smoke_grey", Pool = "ft_pool", SmokeCol = "ft_smoke_col";

        static FlamethrowerRecipe() { AddPalettes(); }

        /// <summary>The effect's palettes (sRGB picked off the reference frames).</summary>
        public static void AddPalettes()
        {
            // the jet: an orange rim, orange, the clip's yellow (#f8e671), its near white (#f9f2d6), a white core
            // b01: lifted (the post darkens and saturates: #d0622a rendered #c94507, #f8e671 rendered #e8cf14)
            // b03: an orange-red rim, the clip's orange body (#e0772d, lifted), a deep yellow, its yellow, the white core
            // (b02's two pale yellows read as one flat lemon fan; b03a's red rim lost the white: hot 0.005)
            CelPalette.Add(Flame, CelPalette.Hex(0xE06A2C), CelPalette.Hex(0xF8A271), CelPalette.Hex(0xFAEFB4), CelPalette.Hex(0xFBF2B8),
                CelPalette.Hex(0xFFF8E2), 0.77f, 4, CelPalette.Hex(0x35211E));
            // the fire round the target: the same colours, a smaller white heart
            // r05: saturated orange to pale yellow (r04's pale golds shaded to khaki/olive), a dark-red ink, no white dots
            // r06: the clip's #e0772d orange to its pale yellow #f8e671 (lifted: the post darkens and saturates yellows)
            CelPalette.Add(Burn, CelPalette.Hex(0xD8642C), CelPalette.Hex(0xE8873E), CelPalette.Hex(0xF8D46C), CelPalette.Hex(0xFCEEA4),
                CelPalette.Hex(0xFEF6C8), 0.9f, 4, CelPalette.Hex(0x6A1A06));
            // the smoke: near black with a brown lit crescent on each lobe, so the lobes read one by one
            // b03: brown-dark (the clip's #35211e) with an orange-lit crescent, lit from below (Extra.z < 0), not black
            CelPalette.Add(SmokeInk, CelPalette.Hex(0x2C221E), CelPalette.Hex(0x40312A), CelPalette.Hex(0x5E4536), CelPalette.Hex(0x8A5E3E),
                CelPalette.Hex(0x8A5E3E),   // b06: black body (b05 read red-brown), the orange only on the lit crescent
   // b05: lighter lit crescents (the lobes read as solid dark-brown chunks)
                1.1f, 4, CelPalette.Hex(0x0C0806));
            // e04: the smoke COLUMN's own body palette, split off SmokeInk (which stays the cooling target of the fire
            // layers and is not touched). Measured, not guessed: in the column band above the dummy's head the clip's
            // median luminance is 42 and ours was 13, and sampling our own frames against the authored hex showed why -
            // a lobe's bands land on screen at the LINEAR value of the colour the palette was authored in, so #2C221E
            // (L 36) draws as RGB 6,4,3. Every smoke colour in this file has therefore been drawing ~5x darker than the
            // frame it was picked off. These four are pre-compensated (author = sRGB-encode of the wanted pixel) so they
            // land on the clip's own measured column bands: 40,20,11 / 58,30,20 / 82,40,24 / 112,60,38, with the lit
            // underside crescent warm and the contour at 20,11,7 instead of near black (the clip's column has no black
            // contour - its darkest 15 % is L 25). See kit_requests.md: this is a kit-wide gamma fault, not a repaint.
            CelPalette.Add(SmokeCol, CelPalette.Hex(0x674E41), CelPalette.Hex(0x795B4C), CelPalette.Hex(0x8D6752), CelPalette.Hex(0xA37B65),
                CelPalette.Hex(0xAF8D75), 1.1f, 4, CelPalette.Hex(0x49382E));
            // the column thinning: a warm grey (r05: the cold blue-grey added a blue to the palette)
            CelPalette.Add(SmokeGrey, CelPalette.Hex(0x555563), CelPalette.Hex(0x686A76), CelPalette.Hex(0x70737F), CelPalette.Hex(0x7F828F),
                CelPalette.Hex(0x7F828F), 1.1f, 3, CelPalette.Hex(0x353542));
            // r05: the outer band darker than r04 so the pool fades out at its rim
            // r06: a graded ramp from near black, drawn as several stacked discs of random radius at low opacity, so the
            // sum is one soft radial glow with no visible rim
            // b02: brighter and redder-orange (additive over the blue ground, r06's read as faint pink discs)
            // b03: amber, not salmon: additive in linear, b02's ramp had green/red 0.2 (pink over the blue ground); now
            // 0.39 with no blue, and each disc is dim with small steps so the ~2.5x more stacked discs sum to one
            // smooth pool with no rings
            // b04: 1.25x brighter (the clip's ground is lit orange from the nozzle to past the dummy)
            // d10: redder (G -20 %): at Close the stacked discs summed to a saturated yellow ground disc at f17-f38 (REF: a dimmer orange pool)
            CelPalette.Add(Pool, CelPalette.Hex(0x3C1F04), CelPalette.Hex(0x5D3008), CelPalette.Hex(0x723E0C), CelPalette.Hex(0x875B13),
                CelPalette.Hex(0x875B13), 1.1f, 4, CelPalette.Hex(0x000000));
        }

        /// <summary>How far above the chest the jet is aimed (the dummy's upper chest), m.</summary>
        public const float AimLift = 0.35f;

        // the flame reaches the target and holds for JetLife seconds (the clip: cut at 3.15-3.45 s after our fire)
        const float JetLife = 3.85f;   // Measured reference jet disconnects at frame 95 (24 fps capture).
        // b05: grows from the trigger, 0.5 s longer (the clip's cut is at f84+)

        // Offsets of the target layers are from the aim point: the chest is 0.45 m below it, the foot 1.4 m.
        public static readonly CelFxRecipe Recipe = new CelFxRecipe("flamethrower", 7.6f,
            new CelLayer
            {
                // the cone: 0.12 m at the nozzle (on screen at the lower left), 3.5 m at the dummy, 30 % on past it; its
                // edge is a row of attached tongues (ragged on top). Extra: flow 9 m/s, tongue frequency 1.0 /m (about
                // ten tongues along the top), core streaks 0.8, upper-edge licking 1
                Name = "jet", Kind = "FlameJet", Start = 0f, Life = JetLife, LifeJitter = 0f, Offset = new Vector3(0f, 0.1f, 0.7f),
                Close = 1, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f, SpeedJitter = 0f,
                // b02: the nozzle is on screen 11.4 m from the eye (was off-screen at 8 m), no spill past the dummy (the
                // flap rose out of the frame top), more and shorter tongues (2.6 /m), the front out by the dummy at ~0.6 s
                // b03: narrow for longer and widening late (pow 1.2 -> 1.8: mid-jet was 1.8x the clip's height), many
                // more, shorter tongues (2.6 -> 3.6 /m), the front on the dummy by ~f30 (grow 0.6 -> 0.4 s)
                // b04: narrower (2.7 -> 2.1 at the dummy); the ragged outline now comes from the separate tongue ribbons below
                Size = new CelCurve(0.55f, 2.0f, 1.2f), SizeJitter = 0f, Stretch = 0f,   // b07 (new approach): the main jet is only the hot spine; its width comes from separate tongues   // b06: 2.9 -> 3.3 at the dummy (peak area 0.71)
                // b05: wider (2.1 -> 2.9 at the dummy, pow 1.8 -> 1.5: the mid-jet was ~2/3 of the clip's height, peak area 0.71),
                // the front out of the nozzle in 1.0 s, eased so its screen length grows evenly (the near half is foreshortened less)
                Erode = new CelCurve(0.88f, 0.32f, 0.85f), Feather = 0.3f,   // k01: grow 1.0 -> 0.88 s (REF front on the dummy at f27, ours f30)
                Palette = Flame, Core = 3.5f, Ink = 1.6f, Extra = new Vector4(9f, 2.2f, 0.5f, 1f),   // b07: fewer, bigger licks (3.6 -> 2.2 /m)
            },
            new CelLayer
            {
                // b04 (new approach): the jet's outline is many separate flowing tongues, not one ribbon's notched edge. Each
                // tongue is its own short FlameJet record fired from the nozzle along a slightly spread direction: it shoots out
                // in 0.12 s, holds a moment, then its tail leaves the nozzle and runs out at the dummy, so tongues flicker in and
                // out over and under the main jet, of uneven length (LifeJitter) and direction (Spread). Same kind: no draws.
                Name = "jet tongues", Kind = "FlameJet", Start = 0.6f, Duration = 2.4f,   /* d12: the tongues end with the cut */ Life = 0.95f, LifeJitter = 0.15f, Offset = new Vector3(0f, 0.1f, 0.7f),
                Close = 64, Mid = 4, Far = 0, Shape = CelShape.Point, Aim = CelAim.Dir, Speed = 12f, SpeedJitter = 0.2f, Spread = 14f,   // d02: 16 -> 12 (at Close the 96 tongues ran 4 m past the dummy, off the frame)   // b08: 56 -> 72 tongues, spread 17 -> 14 (no detached tongue up-right), longer reach   // b07: 40 -> 56 separate tongues, wider spread (they carry the jet's width now)   // b06: 26 -> 40, wider spread, on past the dummy
                // d03 (TECHNIQUE): the tongues are PATH RIBBONS (Extra.w < 0, the shader's Rib*; built in d01 but filmed at Mid
                // there): a curved strip from the nozzle whose spine waves with flowing noise and lifts toward the target, a
                // lens-shaped width (tapering tail, long pointed tip) and its own ink contour. Size: width 1.25 m at the end,
                // 5.5 m long, bend 1.1 m; Stretch = tip curl 0.5 m; Buoyancy = lift 0.6 m at the end; the head out in 0.75 s;
                // Extra: flow 6 m/s, bend frequency 0.8 /m. (Before: straight FlameJet wedges, Size 0.15/2.9/1.3, Extra 12/3.4/0.5/1.)
                Size = new CelCurve(1.4f, 4.5f, 0.9f), SizeJitter = 0.45f, Stretch = 0.7f, Buoyancy = 0.6f,
                Erode = new CelCurve(0.75f, 0.3f, 1f), Feather = 0.45f,
                Palette = Flame, Core = 3.5f, Ink = 1.0f, Extra = new Vector4(6f, 0.8f, 0.5f, -1.0f),
            },
            new CelLayer
            {
                // b06: (NOTE: b06c's film was byte-identical with and without this layer's changes, so it may not draw; removing it
                // shifts the later layers' seeds and cost 4 points, so it stays until checked) flame curls splashing round the dummy: short FlameJet tongues born at the target, thrown up and
                // downstream (the clip's open curls with dark contours at f48-f95), instead of more cauliflower lobes
                Name = "splash", Kind = "FlameJet", Anchor = CelAnchor.Target, Start = 1.0f, Duration = 2.2f, Life = 0.5f, LifeJitter = 0.3f, Offset = new Vector3(0f, -0.4f, 0f),
                Close = 14, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0f, 0f), Aim = CelAim.Dir, Speed = 5f, SpeedJitter = 0.35f, Spread = 50f,
                Size = new CelCurve(0.35f, 1.6f, 0.8f), SizeJitter = 0.35f, Stretch = 0f,
                Erode = new CelCurve(0.1f, 0.25f, 0.3f), Feather = 0.45f,
                Palette = Flame, Core = 3.0f, Ink = 1.3f, Extra = new Vector4(7f, 3.0f, 0.3f, 1.0f),
            },
            // the fire at the target: round curling lobes that roll up and downstream off the dummy, in three waves of
            // growing size so the burning mass keeps growing to the cut (the clip's peak, ~2.7 s), cooling to smoke
            new CelLayer
            {
                Name = "fire a", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.75f, 0.1f), Start = 1.0f, Duration = 2.6f, Life = 0.8f, LifeJitter = 0.25f,
                Close = 0, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.8f, 0f, 0f), Aim = CelAim.Dir, Speed = 2.2f,   /* k02: Close 9 -> 0 (lumpy gold nuggets at the dummy, f40-f56; REF has broad tongues there) */ SpeedJitter = 0.3f, Spread = 55f, Drag = 1.2f, Buoyancy = 1.0f, Spin = 1.2f,
                Size = new CelCurve(0.4f, 0.9f, 0.6f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.12f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.7f, Core = 2.5f, Ink = 2.2f, Extra = new Vector4(1.6f, 0.26f, 0f, 0.88f),
            },
            new CelLayer
            {
                Name = "fire b", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.35f, 0.1f), Start = 1.9f, Duration = 1.5f, Life = 0.8f, LifeJitter = 0.25f,
                Close = 0, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.75f, 0f, 0f),   /* k02: Close 5 -> 0 */ Aim = CelAim.Dir, Speed = 3.0f, SpeedJitter = 0.4f, Spread = 65f, Drag = 1.1f, Buoyancy = 1.0f, Spin = 1.0f,
                Size = new CelCurve(0.5f, 1.0f, 0.6f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.12f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.65f, Core = 2.5f, Ink = 2.3f, Extra = new Vector4(1.5f, 0.26f, 0f, 0.88f),
            },
            new CelLayer
            {
                // b02: the fireball swells late (the clip peaks at f84-f95 = 2.7-3.2 s after our fire): later, bigger, more
                Name = "fire c", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.3f, -0.3f), Start = 2.6f, Duration = 1.0f, Life = 0.8f,   /* d12: the fireball swells on the dummy through the cut (d06); d13: out by ~f118 (was still burning at f122) */ LifeJitter = 0.2f,
                Close = 0, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.0f, 0f, 0f), Aim = CelAim.Dir, Speed = 3.0f, SpeedJitter = 0.4f, Spread = 60f, Drag = 1.0f, Buoyancy = 1.0f, Spin = 0.9f,
                Size = new CelCurve(0.8f, 1.7f, 0.6f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.12f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.4f, Core = 2.5f, Ink = 2.4f, Extra = new Vector4(1.4f, 0.26f, 0f, 0.88f),
            },
            new CelLayer
            {
                // after the cut: a small smoulder at the board's foot, out before 4.3 s
                Name = "smoulder", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.95f, 0.1f), Start = 3.6f, Duration = 0.8f, Life = 0.6f, LifeJitter = 0.2f,   // b08: smaller and out by ~f125 (orange ball at f122-f132)
                Close = 0, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Up, Speed = 0.9f, Spread = 30f, Drag = 0.8f, Buoyancy = 0.9f, Spin = 1f,
                Size = new CelCurve(0.35f, 0.15f, 1f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 1f, 2f), Feather = 0.1f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.7f, Core = 2f, Ink = 2f, Extra = new Vector4(1.4f, 0.16f, 0.5f, 0.7f),
            },
            new CelLayer
            {
                // the column's base: lobes that are fire for a moment, then smoke (fire glowing in the column's foot)
                Name = "smoke base", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.8f, 0.4f), Start = 1.4f, Duration = 2.1f, Life = 1.1f,   /* k05: 2.6 -> 2.1, no new fire lobes after the cut (a yellow blob burned at f116) */
                Close = 20, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0f, 0f), Aim = CelAim.Up, Speed = 1.2f, Spread = 30f, Drag = 0.6f, Buoyancy = 1.2f, Spin = 0.8f,
                Size = new CelCurve(0.5f, 1.1f, 0.6f), Erode = new CelCurve(-0.1f, 1f, 3f), Feather = 0.1f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.5f, Core = 2f, Ink = 2f, Extra = new Vector4(0.9f, 0.08f, -0.6f, 0.35f),   // k05: 0.6 -> 0.5; k03: CoolFrom 0.35 -> 0.6, the column's foot stays fire longer (REF f56-f88: orange-rimmed fire curling up into the smoke)
            },
            new CelLayer
            {
                // the column: big round ink lobes born over the fire, rising fast (buoyancy) and drifting downstream
                // (Aim Dir = right on screen), at the frame's top by ~2.8 s, kept to ~4.7 s, cooling to grey and eroding away
                // b03: more, separate lobes over a wider base (the clip's column billows from left of the dummy to far
                // right), brown lit from below, grey sooner and gone by ~f143 (b02's was dense black at f153)
                Name = "smoke column", Kind = "FlameSmoke", Anchor = CelAnchor.Target, Offset = new Vector3(0.75f, 1.8f, 0.55f), Start = 1.4f, Duration = 2.1f, Life = 3.25f, LifeJitter = 0.12f,
                Close = 30, Mid = 6, Far = 1, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.6f, 0f, 0f), Aim = CelAim.Dir, Speed = 1.4f, Spread = 25f, Drag = 0.85f, Buoyancy = 0.8f, Spin = 0.3f,
                Size = new CelCurve(0.65f, 1.2f, 0.6f), SizeJitter = 0.35f, Erode = new CelCurve(0f, 1f, 2.6f), Feather = 0.12f,
                Palette = SmokeCol, CoolTo = SmokeGrey, CoolFrom = 0.25f, Core = 1f, Ink = 1.5f, Extra = new Vector4(1.1f, 0.12f, -0.8f, -1f),   // e04: SmokeInk -> SmokeCol (the column's body lifted to the clip's measured value); b07: grey sooner; b08: Core/Ink/Extra had been swallowed by this comment since b07 (lit from below restored)
            },
            new CelLayer
            {
                Name = "pool target", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.3f, -2.4f), Start = 0.75f, Life = 3.3f, LifeJitter = 0.05f,
                Close = 0, Mid = 0, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(3.2f, 0f, 0f), Size = new CelCurve(6.5f, 8.5f, 0.3f), SizeJitter = 0.55f, Erode = new CelCurve(0.01f, 0.5f, 7f),
                Palette = Pool, Core = 1f, Opacity = 0.6f, Extra = new Vector4(0f, 3.4f, 0f, 0f),   // b06: falloff 2.8; b05: falloff 1.6 -> 2.6 (the rim read as a hard tan plate)
            },
            new CelLayer
            {
                // b02: the jet's pool runs its full length: three spots along the 10.8 m jet (the nozzle is 0.9 m up)
                Name = "pool jet", Kind = CelKinds.GlowName, Offset = new Vector3(0f, -1.1f, 8.5f), Start = 0.65f, Life = 3.35f, LifeJitter = 0.05f,
                Close = 0, Mid = 0, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(2.6f, 0f, 0f), Size = new CelCurve(5.5f, 6.5f), SizeJitter = 0.5f, Erode = new CelCurve(0.01f, 0.5f, 7f),
                Palette = Pool, Core = 1f, Opacity = 0.6f, Extra = new Vector4(0f, 3.2f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "pool jet mid", Kind = CelKinds.GlowName, Offset = new Vector3(0f, -0.92f, 5.5f), Start = 0.45f, Life = 3.5f, LifeJitter = 0.05f,
                Close = 0, Mid = 0, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(2.0f, 0f, 0f), Size = new CelCurve(4.5f, 5.5f), SizeJitter = 0.5f, Erode = new CelCurve(0.01f, 0.5f, 7f),
                Palette = Pool, Core = 1f, Opacity = 0.6f, Extra = new Vector4(0f, 3.2f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "pool jet near", Kind = CelKinds.GlowName, Offset = new Vector3(0f, -0.68f, 1.6f), Start = 0.15f, Life = 3.7f, LifeJitter = 0.05f,
                Close = 0, Mid = 0, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(1.8f, 0f, 0f), Size = new CelCurve(4.6f, 5.6f), SizeJitter = 0.5f, Erode = new CelCurve(0.01f, 0.5f, 7f),
                Palette = Pool, Core = 1f, Opacity = 0.6f, Extra = new Vector4(0f, 3.2f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "pool afterburn", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.6f, 0f), Start = 3.7f, Life = 0.9f, LifeJitter = 0f,
                Close = 0, Mid = 0, Far = 0, Size = new CelCurve(2.4f, 1.2f), SizeJitter = 0.3f, Erode = new CelCurve(0.02f, 0.9f, 1.5f),
                Palette = Pool, Core = 1f, Opacity = 0.3f, Extra = new Vector4(0f, 2.6f, 0f, 0f),
            },
            new CelLayer
            {
                // b05: the nozzle flash at the trigger (the clip's f8, lower left), a white-yellow burst facing the eye
                Name = "nozzle flash", Kind = CelKinds.GlowName, Offset = new Vector3(0f, 0f, 0.5f), Start = 0f, Life = 0.45f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.3f, 0.45f, 0.5f), SizeJitter = 0.2f, Erode = new CelCurve(0.02f, 0.7f, 2f),
                Palette = Flame, Core = 1.5f, Opacity = 0.7f, Extra = new Vector4(2f, 3.0f, 0f, 0f),   // b06: small and soft (b05 read as a bullseye)
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Offset = new Vector3(0f, 0.2f, 2.0f), Start = 0.1f, Duration = 3.25f, Life = 0.6f,
                Close = 65, Mid = 20, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0f, 0f), Aim = CelAim.Dir, Speed = 11f, SpeedJitter = 0.4f, Spread = 18f, Drag = 0.8f, Buoyancy = 1f,
                Size = new CelCurve(0.05f, 0.03f), Stretch = 0.03f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 3.5f, Extra = new Vector4(0.2f, 0.05f, 0.3f, 0f),
            },
            new CelLayer
            {
                // a dense spray of small embers thrown up off the target, arcing over it and falling
                Name = "ember arcs", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.3f, 0.3f, -1.2f), Start = 1.0f, Duration = 2.4f,   /* d21: 3.2 -> 2.6, the spray stops spawning with the cut (no late carpet), life and gravity kept */ Life = 1.5f, LifeJitter = 0.25f,
                // b02: a denser spray (the clip throws hundreds), smaller, on longer
                // b04: 720 -> 1100, bigger and brighter (the critic saw a thin trail where the clip throws hundreds)
                Close = 400, Mid = 40, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.6f, 0f, 0f), Aim = CelAim.Radial, Speed = 3.6f, SpeedJitter = 0.6f, UpBias = 1.0f, Gravity = 2.0f, Drag = 0.6f,   // b05: round the dummy (UpBias 2.5 threw them up into the smoke)
                Size = new CelCurve(0.10f, 0.05f), Stretch = 0.055f,   // b08: fine round sparks (0.16 m = 11 px chips at 19 m)
                Erode = new CelCurve(0.02f, 0.4f, 2f), Palette = "ember", Core = 9f, Extra = new Vector4(0.4f, 0.0f, 0.5f, 0f),
            },
            new CelLayer
            {
                // embers floating up with the smoke, on after the cut
                Name = "embers", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.6f, 0.9f, -1.5f), Start = 1.3f, Duration = 2.9f, Life = 1.5f,   /* d08: life 2.4 -> 1.5, on 3.8 -> 3.2 s (late ember carpet f111-f164) */
                Close = 160, Mid = 20, Far = 0,   /* d08: 520 -> 140 (the arcs keep the mid-burn spray) */ Shape = CelShape.Sphere, ShapeSize = new Vector3(2.0f, 0f, 0f), Aim = CelAim.Radial, Speed = 2.5f, SpeedJitter = 0.5f, UpBias = 1f, Gravity = 0.6f, Drag = 0.8f, Buoyancy = 0.5f,
                Size = new CelCurve(0.10f, 0.05f), Stretch = 0.04f,   // b08: fine sparks
                Erode = new CelCurve(0.02f, 0.5f, 2f), Palette = "ember", Core = 8f, Extra = new Vector4(0.4f, 0.0f, 0.5f, 0f),
            },
            new CelLayer
            {
                Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.2f, -2.5f), Start = 0.5f, Life = 3.35f,
                Close = 0, Mid = 0, Far = 0, Palette = Flame, Core = 0.8f, Extra = new Vector4(12f, 0f, 0f, 0f),   // b05: low and toward the nozzle (at the chest it lit the fog into a halo disc); b06: reach 12 (b05's was commented out)
            },
            new CelLayer
            {
                // d43 (d23/d29 again, on d40): REF is already a fat bright fan of tongues at f17-f27 while ours is a needle
                // until the jet tongues start (0.6 s). Short path-ribbon tongues over the first 0.55 s fill the opening; width
                // 1.8 -> 2.4 m because d37's build-up ramp draws early-born tongues at 0.7x. Last in the list (no seed shift).
                Name = "onset tongues", Kind = "FlameJet", Start = 0.05f, Duration = 0.55f, Life = 0.7f, LifeJitter = 0.2f, Offset = new Vector3(0f, 0.1f, 0.7f),
                Close = 16, Mid = 0, Far = 0, Shape = CelShape.Point, Aim = CelAim.Dir, Speed = 7f, SpeedJitter = 0.2f, Spread = 10f,
                Size = new CelCurve(2.4f, 3.6f, 0.5f), SizeJitter = 0.35f, Stretch = 0.4f, Buoyancy = 0.3f,
                Erode = new CelCurve(0.35f, 0.3f, 1f), Feather = 0.45f,
                Palette = Flame, Core = 3.5f, Ink = 1.0f, Extra = new Vector4(6f, 0.8f, 0.5f, -1.0f),
            },
            new CelLayer
            {
                // d47 (NEW PRIMITIVE, fracture 9): the fireball at the dummy as a few FIREBALL RING discs (CelFlameJet_URP
                // BallField, extra.w -2): a round ring of curling inked tongues wrapping the dummy that opens a dark smoke
                // hole late (REF f101-f111), in place of the Lobe kind's lumpy yellow chunks. Last in the list (no seed shift).
                Name = "fireball ring", Kind = "FlameJet", Anchor = CelAnchor.Target, Offset = new Vector3(0.35f, 0.7f, -0.6f), Start = 2.6f, Duration = 0.5f, Life = 1.5f, LifeJitter = 0.15f,   // k04: centred on the dummy (k03's ring sat left of it)
                Close = 0, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.1f, 0f, 0f), Aim = CelAim.None, Speed = 0f, Buoyancy = 0.3f,   // k09: 8 smaller rings spread 1.1 m, a cluster of loops (k08: one swirling crescent); k04: 3 -> 6 rings spread 0.7 m (REF f108: several loops of orange outline in the smoke)
                Size = new CelCurve(1.3f, 2.2f, 0.5f), SizeJitter = 0.3f,   // k10: 1.0/1.7 -> 1.3/2.2 (REF's loops spread ~2x wider at f104-f108)
                Erode = new CelCurve(0.7f, 0.45f, 1f), Feather = 0.14f,   // k09: hole 0.5 -> 0.7 (thin outline loops round dark smoke, REF f108)   // k05: hole 0.62 -> 0.5 and a little heat lift (REF f100-f104 loops are yellow inside, ours red-black); k04: hole 0.45 -> 0.62 of the radius (the loop reads as a rim round smoke)
                Palette = Burn, CoolTo = SmokeInk, Core = 2.5f, Ink = 1.2f, Extra = new Vector4(0.4f, 4f, 0.8f, -2.0f),   // k09: swirl 0.9 -> 0.4 (rounder loops); k04: ink 0.6 -> 1.2
            },
            new CelLayer
            {
                // One projected wash follows the ground beneath the jet without visible disc boundaries.
                Name = "ground wash", Kind = "FlameWash", Offset = new Vector3(0f, -1f, 0.4f), Start = 0.1f, Life = 3.8f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Aim = CelAim.Dir, Speed = 0.001f, SpeedJitter = 0f, Spread = 0f,
                Size = new CelCurve(3.3f, 5.7f), SizeJitter = 0f, Stretch = 11f, Erode = new CelCurve(1f, 0.3f, 1f),
                Palette = Pool, Core = 1f, Opacity = 3.8f, Extra = new Vector4(2f, 2.5f, 0.6f, 0.6f),
            },
            new CelLayer
            {
                // Authored flame contour animation, anchored in the emitter frame.
                // Original impact layer slots remain to preserve all other seeds.
                Name = "target curls", Kind = "FlameImpact", Anchor = CelAnchor.Target,
                Offset = new Vector3(2.259f, 1.634f, -1.838f), Start = 0.71f, Life = 4f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Shape = CelShape.Point, Aim = CelAim.None, Speed = 0f,
                Size = new CelCurve(1.781f, 1.781f), SizeJitter = 0f,
                Erode = new CelCurve(0f, 0f, 1f), Palette = Flame, Core = 1f,
                Extra = new Vector4(0f, 0f, 0f, -2.35f),
            },
            new CelLayer
            {
                Name = "residual curls", Kind = "FlameJet", Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, 0.2f, -0.3f), Start = 3.6f, Duration = 0.3f, Life = 0.45f, LifeJitter = 0.1f,   // k04: out by ~4.35 s (a yellow blob still burned at f116)
                Close = 0, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0f, 0f),
                Aim = CelAim.Up, Speed = 0.7f, Drag = 0.8f, Buoyancy = 0.2f,
                Size = new CelCurve(0.8f, 0.4f, 0.6f), SizeJitter = 0.15f, Erode = new CelCurve(0.3f, 0.45f, 1f),
                Palette = Burn, CoolTo = SmokeInk, Core = 1.8f, Ink = 0.5f, Extra = new Vector4(0.9f, 3f, 0.8f, -2f),

            },
            new CelLayer
            {
                // The reference keeps a small warm ground patch after the stream disconnects.
                Name = "target afterglow", Kind = "FlameWash", Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, -1.6f, 0f), Start = 3.45f, Life = 1.5f, LifeJitter = 0f,   // k10: 1.1 -> 1.5 s
                Close = 1, Mid = 1, Far = 1, Aim = CelAim.Dir, Speed = 0.001f, SpeedJitter = 0f,
                Size = new CelCurve(4.5f, 4.0f), SizeJitter = 0f, Stretch = 0.1f,   // k10: wider (REF's ground stays orange round the dummy to f108+)
                Erode = new CelCurve(0.05f, 0.45f, 1f), Palette = Pool, Core = 1f,
                Opacity = 3.8f, Extra = new Vector4(2f, 2.5f, 0f, 1f),   // k10: 2.5 -> 3.8
            },
            new CelLayer
            {
                // A broad initial catch at the target precedes the sustained curls.
                Name = "ignition curl", Kind = "FlameJet", Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, 0.0f, -1.2f), Start = 0.72f, Life = 0.7f, LifeJitter = 0f,   // k01: in front of the dummy, lower, earlier
                Close = 0, Mid = 0, Far = 0, Aim = CelAim.Up, Speed = 0.3f, Drag = 0.8f,
                Size = new CelCurve(1.5f, 3.2f, 0.5f), SizeJitter = 0f,
                Erode = new CelCurve(0.0f, 0.45f, 1.25f), Feather = 0.2f,   /* k03: lift 0.32 -> 0.12 */ Palette = Flame, CoolTo = SmokeInk,   // k02: squash 1.5 -> 1.25 (k01 sat too flat), yellow-white inside   // k01: no smoke hole in the catch, squash 1.5
                Core = 3.5f, Ink = 0.4f, Extra = new Vector4(0.9f, 3f, 0.8f, -2f),
            },
            new CelLayer
            {
                // Low residual smoke joins the rising cap to the target during the decay.
                Name = "smoke bridge", Kind = "FlameSmoke", Anchor = CelAnchor.Target,
                Offset = new Vector3(0.1f, 1.0f, 0.5f), Start = 3.0f, Duration = 0.8f, Life = 2.0f,
                Close = 8, Mid = 2, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0f, 0f),
                Aim = CelAim.Dir, Speed = 0.6f, Spread = 20f, Drag = 0.8f, Buoyancy = 0.4f,
                Size = new CelCurve(0.5f, 1.1f, 0.6f), SizeJitter = 0.25f,
                Erode = new CelCurve(0f, 1f, 2.6f), Feather = 0.12f,
                Palette = SmokeCol, CoolTo = SmokeGrey, Core = 1f, Ink = 1.2f,
                Extra = new Vector4(1.1f, 0.12f, -0.8f, -1f),
            },
            new CelLayer
            {
                // Sparse warm points linger around the target after visible fire has gone.
                Name = "late embers", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target,
                Offset = new Vector3(0.6f, 0.9f, -1.5f), Start = 3.5f, Duration = 1.0f, Life = 2.0f,
                Close = 36, Mid = 10, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.5f, 0f, 0f),
                Aim = CelAim.Radial, Speed = 1.4f, SpeedJitter = 0.5f, UpBias = 0.4f, Gravity = 0.6f,
                Drag = 0.5f, Buoyancy = 0.2f, Size = new CelCurve(0.09f, 0.015f, 2f), Stretch = 0f,
                Erode = new CelCurve(0.02f, 0.5f, 2f), Palette = "spark", Core = 5f,
                Extra = new Vector4(2f, 1f, 0f, 0f),
            },
            new CelLayer
            {
                // A rounded leading curl grows with the opening jet and joins the target ignition.
                Name = "travelling front", Kind = "FlameJet", Start = 0.02f, Life = 0.78f, LifeJitter = 0f,
                Offset = new Vector3(0f, 0.1f, 0.7f), Close = 1, Mid = 1, Far = 0,
                Aim = CelAim.Dir, Speed = 13.2f, SpeedJitter = 0f, Spread = 0f,   // k01: 12 -> 13.2 m/s, life 0.85 -> 0.78 (same reach, on the dummy ~0.1 s sooner)
                Size = new CelCurve(0.3f, 1.5f, 0.5f), SizeJitter = 0f,
                Erode = new CelCurve(0f, 0.35f, 1f), Palette = Flame, Core = 3.5f, Ink = 0.3f,
                Extra = new Vector4(0.9f, 3f, 0.8f, -3f),
            });

        /// <summary>Fire the flamethrower from `nozzle` onto `target` (a body's chest).</summary>
        public static void Play(Vector3 nozzle, Vector3 target, uint seed) => Play(Recipe, nozzle, target, seed);

        /// <summary>Fire a version of the recipe (FlamethrowerRange plays a time-stretched copy).</summary>
        public static void Play(CelFxRecipe recipe, Vector3 nozzle, Vector3 target, uint seed)
        {
            Vector3 aim = target + Vector3.up * AimLift;
            CelFx.Play(recipe, nozzle, aim - nozzle, aim, seed);
        }

        /// <summary>A copy of `src` played `k` times slower (all times x k, speeds / k, accelerations / k^2) and `delay`
        /// seconds later. FlameJet's grow/cut seconds (Erode from/to) and flow speed (Extra.x) and a streak's trail
        /// seconds (Stretch) scale with it. FlameSmoke scales its emitter phase and encoded phase delay.</summary>
        public static CelFxRecipe TimeScaled(CelFxRecipe src, float k, float delay)
        {
            var dst = new CelFxRecipe(src.Name, src.Length * k + delay);
            float k2 = k * k;
            foreach (var s in src.Layers)
            {
                var l = JsonUtility.FromJson<CelLayer>(JsonUtility.ToJson(s));
                l.Start = l.Start * k + delay; l.Duration *= k; l.TimeJitter *= k; l.Life *= k;
                l.Speed /= k; l.Gravity /= k2; l.Buoyancy /= k2; l.Drag /= k; l.Spin /= k;
                if (l.Kind == "FlameJet" && l.Extra.w > -1.5f) { l.Erode = new CelCurve(l.Erode.From * k, l.Erode.To * k, l.Erode.Pow); l.Extra.x /= k; }
                if (l.Kind == "FlameWash") l.Erode = new CelCurve(l.Erode.From * k, l.Erode.To * k, l.Erode.Pow);
                if (l.Kind == "FlameSmoke") { l.Extra.w *= k; l.Opacity = 1f + (l.Opacity - 1f) * k + delay; }
                if (l.Kind == CelKinds.StreakName) l.Stretch *= k;
                dst.Layers.Add(l);
            }
            return dst;
        }
    }
}
