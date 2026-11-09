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
        public const string Flame = "ft_flame", Burn = "ft_burn", SmokeInk = "ft_smoke", SmokeGrey = "ft_smoke_grey", Pool = "ft_pool";

        static FlamethrowerRecipe() { AddPalettes(); }

        /// <summary>The effect's palettes (sRGB picked off the reference frames).</summary>
        public static void AddPalettes()
        {
            // the jet: an orange rim, orange, the clip's yellow (#f8e671), its near white (#f9f2d6), a white core
            // b01: lifted (the post darkens and saturates: #d0622a rendered #c94507, #f8e671 rendered #e8cf14)
            // b03: an orange-red rim, the clip's orange body (#e0772d, lifted), a deep yellow, its yellow, the white core
            // (b02's two pale yellows read as one flat lemon fan; b03a's red rim lost the white: hot 0.005)
            CelPalette.Add(Flame, CelPalette.Hex(0xE06A2C), CelPalette.Hex(0xF0943C), CelPalette.Hex(0xF8D25C), CelPalette.Hex(0xFAEC92),
                CelPalette.Hex(0xFFF8E2), 0.76f, 4, CelPalette.Hex(0x35211E));   // b04: core from 0.82 (b03 hot 0.010 vs the clip's 0.101: the spine never reached 0.95)
            // the fire round the target: the same colours, a smaller white heart
            // r05: saturated orange to pale yellow (r04's pale golds shaded to khaki/olive), a dark-red ink, no white dots
            // r06: the clip's #e0772d orange to its pale yellow #f8e671 (lifted: the post darkens and saturates yellows)
            CelPalette.Add(Burn, CelPalette.Hex(0xD8642C), CelPalette.Hex(0xE8873E), CelPalette.Hex(0xF6BE62), CelPalette.Hex(0xFCEEA4),
                CelPalette.Hex(0xFEF4B0), 0.995f, 4, CelPalette.Hex(0x6A1A06));
            // the smoke: near black with a brown lit crescent on each lobe, so the lobes read one by one
            // b03: brown-dark (the clip's #35211e) with an orange-lit crescent, lit from below (Extra.z < 0), not black
            CelPalette.Add(SmokeInk, CelPalette.Hex(0x1C1614), CelPalette.Hex(0x2A221F), CelPalette.Hex(0x45332A), CelPalette.Hex(0xA65A2A),
                CelPalette.Hex(0xA65A2A),   // b06: black body (b05 read red-brown), the orange only on the lit crescent
   // b05: lighter lit crescents (the lobes read as solid dark-brown chunks)
                1.1f, 4, CelPalette.Hex(0x0C0806));
            // the column thinning: a warm grey (r05: the cold blue-grey added a blue to the palette)
            CelPalette.Add(SmokeGrey, CelPalette.Hex(0x3E3D40), CelPalette.Hex(0x525154), CelPalette.Hex(0x68676A), CelPalette.Hex(0x828084),
                CelPalette.Hex(0x828084),   // b06: neutral grey (b05's warm grey read brown)
 1.1f, 3, CelPalette.Hex(0x161212));
            // r05: the outer band darker than r04 so the pool fades out at its rim
            // r06: a graded ramp from near black, drawn as several stacked discs of random radius at low opacity, so the
            // sum is one soft radial glow with no visible rim
            // b02: brighter and redder-orange (additive over the blue ground, r06's read as faint pink discs)
            // b03: amber, not salmon: additive in linear, b02's ramp had green/red 0.2 (pink over the blue ground); now
            // 0.39 with no blue, and each disc is dim with small steps so the ~2.5x more stacked discs sum to one
            // smooth pool with no rings
            // b04: 1.25x brighter (the clip's ground is lit orange from the nozzle to past the dummy)
            CelPalette.Add(Pool, CelPalette.Hex(0x3C2304), CelPalette.Hex(0x5D380A), CelPalette.Hex(0x724610), CelPalette.Hex(0x875415),
                CelPalette.Hex(0x875415), 1.1f, 4, CelPalette.Hex(0x000000));
        }

        /// <summary>How far above the chest the jet is aimed (the dummy's upper chest), m.</summary>
        public const float AimLift = 0.35f;

        // the flame reaches the target and holds for JetLife seconds (the clip: cut at 3.15-3.45 s after our fire)
        const float JetLife = 4.25f;   // b07: 3.95 -> 4.25 (peak -8 f, end -7 f: the tail was cut short)
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
                Size = new CelCurve(0.3f, 2.3f, 1.4f), SizeJitter = 0f, Stretch = 0f,   // b07 (new approach): the main jet is only the hot spine; its width comes from separate tongues   // b06: 2.9 -> 3.3 at the dummy (peak area 0.71)
                // b05: wider (2.1 -> 2.9 at the dummy, pow 1.8 -> 1.5: the mid-jet was ~2/3 of the clip's height, peak area 0.71),
                // the front out of the nozzle in 1.0 s, eased so its screen length grows evenly (the near half is foreshortened less)
                Erode = new CelCurve(1.0f, 0.32f, 0.85f), Feather = 0.45f,   // b07: tongue amplitude 0.5 -> 0.85 (edge breaks into licks)
                Palette = Flame, Core = 3.5f, Ink = 1.6f, Extra = new Vector4(9f, 2.2f, 0.5f, 1f),   // b07: fewer, bigger licks (3.6 -> 2.2 /m)
            },
            new CelLayer
            {
                // b04 (new approach): the jet's outline is many separate flowing tongues, not one ribbon's notched edge. Each
                // tongue is its own short FlameJet record fired from the nozzle along a slightly spread direction: it shoots out
                // in 0.12 s, holds a moment, then its tail leaves the nozzle and runs out at the dummy, so tongues flicker in and
                // out over and under the main jet, of uneven length (LifeJitter) and direction (Spread). Same kind: no draws.
                Name = "jet tongues", Kind = "FlameJet", Start = 0.6f, Duration = 2.8f, Life = 0.95f, LifeJitter = 0.3f, Offset = new Vector3(0f, 0.1f, 0.7f),
                Close = 72, Mid = 4, Far = 0, Shape = CelShape.Point, Aim = CelAim.Dir, Speed = 16f, SpeedJitter = 0.25f, Spread = 14f,   // b08: 56 -> 72 tongues, spread 17 -> 14 (no detached tongue up-right), longer reach   // b07: 40 -> 56 separate tongues, wider spread (they carry the jet's width now)   // b06: 26 -> 40, wider spread, on past the dummy
                Size = new CelCurve(0.15f, 2.9f, 1.3f), SizeJitter = 0.4f, Stretch = 0f,
                Erode = new CelCurve(0.12f, 0.3f, 0.7f), Feather = 0.45f,   // b07: tongue amplitude 0.3 -> 0.7 (wavy, licking tips)
                Palette = Flame, Core = 3.5f, Ink = 1.4f, Extra = new Vector4(12f, 2.2f, 0.5f, 1.0f),   // b06: full upper-edge licking
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
                Close = 18, Mid = 7, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.8f, 0f, 0f), Aim = CelAim.Dir, Speed = 2.2f, SpeedJitter = 0.3f, Spread = 55f, Drag = 1.2f, Buoyancy = 1.0f, Spin = 1.2f,
                Size = new CelCurve(0.4f, 0.9f, 0.6f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.12f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.7f, Core = 2.5f, Ink = 2.2f, Extra = new Vector4(1.6f, 0.26f, 0f, 0.88f),
            },
            new CelLayer
            {
                Name = "fire b", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.35f, 0.1f), Start = 1.9f, Duration = 1.5f, Life = 0.8f, LifeJitter = 0.25f,
                Close = 10, Mid = 5, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.75f, 0f, 0f), Aim = CelAim.Dir, Speed = 3.0f, SpeedJitter = 0.4f, Spread = 65f, Drag = 1.1f, Buoyancy = 1.0f, Spin = 1.0f,
                Size = new CelCurve(0.5f, 1.0f, 0.6f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.12f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.65f, Core = 2.5f, Ink = 2.3f, Extra = new Vector4(1.5f, 0.26f, 0f, 0.88f),
            },
            new CelLayer
            {
                // b02: the fireball swells late (the clip peaks at f84-f95 = 2.7-3.2 s after our fire): later, bigger, more
                Name = "fire c", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.3f, -0.3f), Start = 2.45f, Duration = 0.9f, Life = 0.75f, LifeJitter = 0.2f,
                Close = 12, Mid = 3, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.0f, 0f, 0f), Aim = CelAim.Dir, Speed = 3.0f, SpeedJitter = 0.4f, Spread = 60f, Drag = 1.0f, Buoyancy = 1.0f, Spin = 0.9f,
                Size = new CelCurve(0.6f, 1.2f, 0.6f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.12f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.55f, Core = 2.5f, Ink = 2.4f, Extra = new Vector4(1.4f, 0.26f, 0f, 0.88f),
            },
            new CelLayer
            {
                // after the cut: a small smoulder at the board's foot, out before 4.3 s
                Name = "smoulder", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.95f, 0.1f), Start = 3.6f, Duration = 0.8f, Life = 0.6f, LifeJitter = 0.2f,   // b08: smaller and out by ~f125 (orange ball at f122-f132)
                Close = 10, Mid = 5, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Up, Speed = 0.9f, Spread = 30f, Drag = 0.8f, Buoyancy = 0.9f, Spin = 1f,
                Size = new CelCurve(0.35f, 0.15f, 1f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 1f, 2f), Feather = 0.1f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.7f, Core = 2f, Ink = 2f, Extra = new Vector4(1.4f, 0.16f, 0.5f, 0.7f),
            },
            new CelLayer
            {
                // the column's base: lobes that are fire for a moment, then smoke (fire glowing in the column's foot)
                Name = "smoke base", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.5f, 0.4f), Start = 1.4f, Duration = 2.6f, Life = 1.1f,
                Close = 12, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0f, 0f), Aim = CelAim.Up, Speed = 1.2f, Spread = 30f, Drag = 0.6f, Buoyancy = 1.2f, Spin = 0.8f,
                Size = new CelCurve(0.5f, 1.1f, 0.6f), Erode = new CelCurve(0f, 1f, 3f), Feather = 0.1f,
                Palette = Burn, CoolTo = SmokeInk, CoolFrom = 0.35f, Core = 2f, Ink = 3f, Extra = new Vector4(1.3f, 0.2f, 0f, 0.35f),
            },
            new CelLayer
            {
                // the column: big round ink lobes born over the fire, rising fast (buoyancy) and drifting downstream
                // (Aim Dir = right on screen), at the frame's top by ~2.8 s, kept to ~4.7 s, cooling to grey and eroding away
                // b03: more, separate lobes over a wider base (the clip's column billows from left of the dummy to far
                // right), brown lit from below, grey sooner and gone by ~f143 (b02's was dense black at f153)
                Name = "smoke column", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0.4f, 2.0f, 0.4f), Start = 1.4f, Duration = 2.4f, Life = 3.2f, LifeJitter = 0.2f,   // b07: 2.75 -> 3.2 (end -7 f: the tail was cut short)
                Close = 30, Mid = 6, Far = 1, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.8f, 0f, 0f), Aim = CelAim.Dir, Speed = 1.1f, Spread = 35f, Drag = 0.5f, Buoyancy = 1.1f, Spin = 0.3f,
                Size = new CelCurve(0.7f, 1.6f, 0.5f), SizeJitter = 0.35f, Erode = new CelCurve(0f, 1f, 1.5f), Feather = 0.12f,
                Palette = SmokeInk, CoolTo = SmokeGrey, CoolFrom = 0.12f, Core = 1f, Ink = 2.4f, Extra = new Vector4(1.4f, 0.2f, -0.7f, 0.4f),   // b07: grey sooner; b08: Core/Ink/Extra had been swallowed by this comment since b07 (lit from below restored)
            },
            new CelLayer
            {
                Name = "pool target", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.6f, -0.6f), Start = 0.75f, Life = 3.2f, LifeJitter = 0f,
                Close = 22, Mid = 4, Far = 1, Shape = CelShape.Disc, ShapeSize = new Vector3(3.2f, 0f, 0f), Size = new CelCurve(6.5f, 8.5f, 0.3f), SizeJitter = 0.55f, Erode = new CelCurve(0.01f, 0.2f, 8f),
                Palette = Pool, Core = 1f, Opacity = 1.0f, Extra = new Vector4(0f, 2.8f, 0f, 0f),   // b06: falloff 2.8; b05: falloff 1.6 -> 2.6 (the rim read as a hard tan plate)
            },
            new CelLayer
            {
                // b02: the jet's pool runs its full length: three spots along the 10.8 m jet (the nozzle is 0.9 m up)
                Name = "pool jet", Kind = CelKinds.GlowName, Offset = new Vector3(0f, -1.44f, 8.5f), Start = 0.65f, Life = 3.25f, LifeJitter = 0f,
                Close = 18, Mid = 2, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(2.6f, 0f, 0f), Size = new CelCurve(5.5f, 6.5f), SizeJitter = 0.5f, Erode = new CelCurve(0.01f, 0.2f, 8f),
                Palette = Pool, Core = 1f, Opacity = 1.0f, Extra = new Vector4(0f, 2.6f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "pool jet mid", Kind = CelKinds.GlowName, Offset = new Vector3(0f, -1.25f, 5.5f), Start = 0.45f, Life = 3.4f, LifeJitter = 0f,
                Close = 16, Mid = 2, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(2.0f, 0f, 0f), Size = new CelCurve(4.5f, 5.5f), SizeJitter = 0.5f, Erode = new CelCurve(0.01f, 0.2f, 8f),
                Palette = Pool, Core = 1f, Opacity = 1.0f, Extra = new Vector4(0f, 2.6f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "pool jet near", Kind = CelKinds.GlowName, Offset = new Vector3(0f, -0.98f, 1.6f), Start = 0.15f, Life = 3.6f, LifeJitter = 0f,
                Close = 20, Mid = 1, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(1.8f, 0f, 0f), Size = new CelCurve(4.6f, 5.6f), SizeJitter = 0.5f, Erode = new CelCurve(0.01f, 0.2f, 8f),
                Palette = Pool, Core = 1f, Opacity = 1.0f, Extra = new Vector4(0f, 2.6f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "pool afterburn", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.6f, 0f), Start = 3.7f, Life = 0.9f, LifeJitter = 0f,
                Close = 2, Mid = 1, Far = 0, Size = new CelCurve(2.4f, 1.2f), SizeJitter = 0.3f, Erode = new CelCurve(0.02f, 0.9f, 1.5f),
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
                Name = "sparks", Kind = CelKinds.StreakName, Offset = new Vector3(0f, 0.2f, 2.0f), Start = 0.1f, Duration = 3.7f, Life = 0.6f,
                Close = 80, Mid = 20, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0f, 0f), Aim = CelAim.Dir, Speed = 11f, SpeedJitter = 0.4f, Spread = 18f, Drag = 0.8f, Buoyancy = 1f,
                Size = new CelCurve(0.05f, 0.03f), Stretch = 0.03f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 3.5f, Extra = new Vector4(0.2f, 0.05f, 0.3f, 0f),
            },
            new CelLayer
            {
                // a dense spray of small embers thrown up off the target, arcing over it and falling
                Name = "ember arcs", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.3f, 0.3f, -1.2f), Start = 1.0f, Duration = 3.2f, Life = 1.8f, LifeJitter = 0.4f,
                // b02: a denser spray (the clip throws hundreds), smaller, on longer
                // b04: 720 -> 1100, bigger and brighter (the critic saw a thin trail where the clip throws hundreds)
                Close = 1600, Mid = 60, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.8f, 0f, 0f), Aim = CelAim.Radial, Speed = 4.5f, SpeedJitter = 0.6f, UpBias = 1.0f, Gravity = 2.0f, Drag = 0.6f,   // b05: round the dummy (UpBias 2.5 threw them up into the smoke)
                Size = new CelCurve(0.08f, 0.05f), Stretch = 0.012f,   // b08: fine round sparks (0.16 m = 11 px chips at 19 m)
                Erode = new CelCurve(0.02f, 0.4f, 2f), Palette = "ember", Core = 9f, Extra = new Vector4(1f, 0.0f, 0.5f, 0f),
            },
            new CelLayer
            {
                // embers floating up with the smoke, on after the cut
                Name = "embers", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.6f, 0.9f, -1.5f), Start = 1.3f, Duration = 3.8f, Life = 2.4f,
                Close = 600, Mid = 30, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(2.4f, 0f, 0f), Aim = CelAim.Radial, Speed = 2.5f, SpeedJitter = 0.5f, UpBias = 1f, Gravity = 0.6f, Drag = 0.8f, Buoyancy = 0.5f,
                Size = new CelCurve(0.07f, 0.045f), Stretch = 0.01f,   // b08: fine sparks
                Erode = new CelCurve(0.02f, 0.5f, 2f), Palette = "ember", Core = 8f, Extra = new Vector4(1f, 0.0f, 0.5f, 0f),
            },
            new CelLayer
            {
                Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1.2f, -2.5f), Start = 0.5f, Life = 3.6f,
                Close = 1, Mid = 1, Far = 1, Palette = Flame, Core = 2f, Extra = new Vector4(12f, 0f, 0f, 0f),   // b05: low and toward the nozzle (at the chest it lit the fog into a halo disc); b06: reach 12 (b05's was commented out)
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
        /// seconds (Stretch) scale with it.</summary>
        public static CelFxRecipe TimeScaled(CelFxRecipe src, float k, float delay)
        {
            var dst = new CelFxRecipe(src.Name, src.Length * k + delay);
            float k2 = k * k;
            foreach (var s in src.Layers)
            {
                var l = JsonUtility.FromJson<CelLayer>(JsonUtility.ToJson(s));
                l.Start = l.Start * k + delay; l.Duration *= k; l.TimeJitter *= k; l.Life *= k;
                l.Speed /= k; l.Gravity /= k2; l.Buoyancy /= k2; l.Drag /= k; l.Spin /= k;
                if (l.Kind == "FlameJet") { l.Erode = new CelCurve(l.Erode.From * k, l.Erode.To * k, l.Erode.Pow); l.Extra.x /= k; }
                if (l.Kind == CelKinds.StreakName) l.Stretch *= k;
                dst.Layers.Add(l);
            }
            return dst;
        }
    }
}
