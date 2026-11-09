// Phase: CelFx job (2026-11-09, lane/show/cel-bullet-impacts) - bullet impacts: a machine-gun burst into the target
// dummy and the ground round it, after the owner's reference clip bullet_impacts_cel. Each round is a shot and, when the
// tracer arrives, an impact:
//   the shot      two thin orange-white Streaks from the gun that end exactly at the hit (they arrive as they die); on
//                 one round in three a thick bolt past the camera, from the lower left corner, white head, orange bloom;
//   the impact    a small white core, a small star, five thin red-orange spark spikes, dark flecks, a crown of dirt
//                 spurts, and on ground hits a dust puff: a few small inked tan lobes that burst from the ground and rise;
//   the light     BiWash multiplies the scene by a warm light by distance (no disc, no ring, no haze: dark ground stays
//                 dark, the wooden dummy flares): a short pool at the dummy's feet, a short flare on its facing side, and
//                 on the bolt rounds the foreground along the bolt's path.
// LOD: Close = everything; Mid = one tracer, flash, two spikes, the pool, a few puff lobes; Far = the tracer and the
// flash (two primitives). Draws: Streak + Glow + Lobe (two passes) + BiWash (two passes) + BiThorn (b05: crowns and
// chips, Close only) = 7 at most, shared with every other cel effect drawn that frame.
// The Range fires the burst (CelRange id "bullet_impacts"); in the game each round is PlayShot then PlayImpact (or
// PlayRound for both at once).
using UnityEngine;
using TW.Presentation.VFX.Cel;

namespace TW.Presentation.VFX.Cel.Effects
{
    /// <summary>The warm light (TW/Cel/BiWash): see the shader's header for its record.</summary>
    public sealed class BiWashKind : CelKind
    {
        public override string Name => "BiWash";
        public override Shader FindShader() => Shader.Find("TW/Cel/BiWash");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel BiWash");
        public override int Capacity => 256;
        public override int Order => 15;
    }

    /// <summary>b05: the opaque inked thorn (TW/Cel/BiThorn) for the ground-hit crowns and the dirt chips: see the
    /// shader's header. Close only, so T1 and out add no draw.</summary>
    public sealed class BiThornKind : CelKind
    {
        public override string Name => "BiThorn";
        public override Shader FindShader() => Shader.Find("TW/Cel/BiThorn");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel BiThorn");
        public override int Capacity => 512;
        public override int Order => 16;
    }

    public static class BulletImpacts
    {
        // b01: the beach Range sits 19 m from a dummy scaled 1.31 (the corridor: 11.5 m, scale 1); the world sizes that
        // must keep their size against the dummy on screen are multiplied by K
        const float K = 1.25f;
        public const string WashName = "BiWash";
        public const string ThornName = "BiThorn";

        static BulletImpacts()
        {
            // the clip's dust: a mid-tan body (#89604c), the light tan (#dfb78f) only where the lobe faces up, a darker
            // brown shade, and a dark brown ink
            // r05: the field fog takes about 40 % off a lobe (r04: #E2BE96 read #8e6c57), so the colours are set lighter
            // than they should read; the light top is the core band (the lobe's up-facing side) so it shows on top only
            // b01: the beach has no corridor fog, so the clip's own tones (r05's lightened set read yellow-orange)
            // b02: grey-tan, not orange (b01 read #c18a5b under the pool light): grey-brown shade bands, the tan only on top,
            // and an ink dark enough to read as ink (L < 14; #2d2424 is L 15)
            // b03: soft grey-tan lobes: one grey-brown shade band, a grey-tan base, a light tan and a pale tan top (3 bands;
            // b02's 4 bands on many small lobes read as dark speckled grapes, ink_peak 3x the clip's)
            // b04: the critic's grey-tan: one grey-brown shade #60534f, a base near #89604c (greyed: the night grade warms
            // it), the light #dfb78f only on top, and an ink dark enough to count as ink (L < 14; b03's #2d2424 is L 15)
            // b04b: greyer again (b04a's #84705f/#b49c84 read #ae7f50 orange-brown after the night grade)
            // b04c: one step lighter (b04b's front faces sat in the dark band: the puffs read dark brown, the clip's are
            // light grey-tan with a dark ink)
            // b05: a touch warmer toward the clip's #89604c / #dfb78f (b04 read #775b44 / #bba17d)
            // b06: the critic's #89604c shadow / #dfb78f light, greyer mids (b05 read #be7a4c orange under the pool)
            // b07: measured on b06b f84-f107: the beach grade reads a lobe at about (0.78, 0.66, 0.59) of its set colour
            // (#B89E84 read #8f684e orange-brown), so the set is a cool grey that reads grey-tan (#a89484 light, #806757 mid)
            // b07b: half way back (b07a's cool grey read silver-white): the clip's are warm grey-tan
            // d03: two bands, the lightest (a light grey-tan body and a pale top), a slightly warmer body: b07's third,
            // dark band read #382723 and made the puffs dark brown boulders; the clip's are light tan, low contrast inside
            CelPalette.Add("bi_dust", CelPalette.Hex(0x544D4A), CelPalette.Hex(0x776B63), CelPalette.Hex(0x9C8A7A), CelPalette.Hex(0xC8B49F),
                CelPalette.Hex(0xE4CAAE), 0.9f, 2, CelPalette.Hex(0x141010));
            // the dirt crowns: darker than the puffs
            CelPalette.Add("bi_dirt", CelPalette.Hex(0x261C14), CelPalette.Hex(0x463628), CelPalette.Hex(0x5E4A3A), CelPalette.Hex(0x7A6450),
                CelPalette.Hex(0x7A6450), 1.1f, 3, CelPalette.Hex(0x0E0806));
            // the tracer: a warm white core, orange edges (no neutral white: it read blue in the fog)
            CelPalette.Add("bi_tracer", CelPalette.Hex(0xB0400C), CelPalette.Hex(0xF07A20), CelPalette.Hex(0xFF9C40), CelPalette.Hex(0xFFC070),
                CelPalette.Hex(0xFFE2B0), 0.45f, 3, CelPalette.Hex(0x401004));
            // the hit's spark spikes: red-orange with a white tip
            // d24: white core back to d20 size (hot_peak is its saturated area), the orange flash glow stays small
            // d22: flash and core about half (read as a disc), spikes longer and a little wider (invisible at 0.07 m)
            // d21: white-orange (the clip's spikes are pale orange to white; red read as an orb)
            CelPalette.Add("bi_spark", CelPalette.Hex(0xC05010), CelPalette.Hex(0xF08A30), CelPalette.Hex(0xFFB860), CelPalette.Hex(0xFFDCA0),
                CelPalette.Hex(0xFFF6E8), 0.85f, 3, CelPalette.Hex(0x300802));
            // the light: c3 is the light's colour (saturated orange; the scene is multiplied by 1 + it)
            // r05: cream-yellow (#e8c48a as it reads), not red: on the blue night ground a saturated orange light multiplied
            // into red, so the light colour is set near neutral warm and the fill brings the cream
            // b03: orange, not cream (the pool and the dummy should read orange-lit; blue low so a bright pool does not clip white)
            CelPalette.Add("bi_wash", CelPalette.Hex(0x40300A), CelPalette.Hex(0x806030), CelPalette.Hex(0xC09060), CelPalette.Hex(0xFFB060),
                CelPalette.Hex(0xFFB060), 1.1f, 2, CelPalette.Hex(0x000000));
            // b06: the pool: pale orange-cream, not saturated (b05's #FFB060 pool read as a hard orange disc)
            CelPalette.Add("bi_pool", CelPalette.Hex(0x403020), CelPalette.Hex(0x806848), CelPalette.Hex(0xC0A078), CelPalette.Hex(0xFFD2A0),
                CelPalette.Hex(0xFFD2A0), 1.1f, 2, CelPalette.Hex(0x000000));
            // the near field's light under the bolt: cream, lower gain than the pool (r05 try4: gain 3.2 clipped yellow on the near slope; try5 tan at 1.8 read red)
            // b05: tan-orange, not cream-yellow (b04's lit the crates yellow-white; the clip's near glow is orange)
            // b07b: yellower (#FFC080; b07a's half-gain #FFAA60 read deep red on the near ground)
            CelPalette.Add("bi_wash_near", CelPalette.Hex(0x40200A), CelPalette.Hex(0x804020), CelPalette.Hex(0xC07848), CelPalette.Hex(0xFFC080),
                CelPalette.Hex(0xFFC080), 1.1f, 2, CelPalette.Hex(0x000000));
            // the dummy's flare: yellow-white
            // b03: yellow-orange (the clip's dummy glows hot yellow-orange, not white)
            // b07: cream-gold (#FFDCA8), not yellow: b06's #FFC060 at gain 8 turned the board flat lemon yellow
            CelPalette.Add("bi_flare", CelPalette.Hex(0x403020), CelPalette.Hex(0x806848), CelPalette.Hex(0xC0A078), CelPalette.Hex(0xFFB878),
                CelPalette.Hex(0xFFB878), 1.1f, 2, CelPalette.Hex(0x000000));
            // the near bolt: a hot yellow head, orange body and rim (b03: less pure white, every shot now carries one)
            CelPalette.Add("bi_bolt", CelPalette.Hex(0xC04A10), CelPalette.Hex(0xFF8A30), CelPalette.Hex(0xFFC060), CelPalette.Hex(0xFFE0A0),
                CelPalette.Hex(0xFFF0C8), 0.45f, 3, CelPalette.Hex(0x401004));
            CelRange.Register("bullet_impacts", Init, (s, dt) => Step(s));
        }

        // ------------------------------------------------------------------------------------------ the shot
        /// <summary>Seconds from the shot to the impact: the tracers arrive at the hit on the frame they die.</summary>
        public const float Flight = 0.19f;   // b01: about 4.5 frames at the beach's 24 fps, so a whole line reaches the board

        // Tracers: Speed 0 + Arrive 1 makes each arrive at the hit on the frame it dies, so none flies past it.
        public static readonly CelFxRecipe Tracer = new CelFxRecipe("bi_tracer", 0.3f,
            new CelLayer
            {
                Name = "tracer", Kind = CelKinds.StreakName, Start = 0f, Duration = 0.03f, Life = Flight, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Shape = CelShape.Box, ShapeSize = new Vector3(0.5f, 0.3f, 0f), Aim = CelAim.Target,   // d14: one a shot Speed = 0f, Arrive = 1f, Spread = 0f,
                Size = new CelCurve(0.06f, 0.045f), SizeJitter = 0.15f, Stretch = 0.22f, Erode = new CelCurve(0f, 0.15f, 2f),   // d13: 2x wide (0.03 m is under a pixel at 19 m)
                Palette = "bi_tracer", Core = 4f, Extra = new Vector4(0.1f, 0.8f, 0.1f, 0f),
            });

        // the near bolt: thick (3x a tracer as it looks), a long trail back to the gun so it enters from the frame's
        // lower left corner, a white-clipped head and an orange bloom that rides on the head
        public static readonly CelFxRecipe NearBolt = new CelFxRecipe("bi_near_bolt", 0.3f,
            new CelLayer
            {
                Name = "bolt", Kind = CelKinds.StreakName, Start = 0f, Life = 0.13f, LifeJitter = 0f,
                Close = 1, Mid = 0, Far = 0, Aim = CelAim.Target, Speed = 0f, Arrive = 1f, Spread = 0f,
                // b02: thicker (0.15 m), a short trail (0.07 s), so it reads as a thick near streak with a white head
                // b03: thicker (0.2 m) and a longer trail so it reads as the clip's thick glowing near streak
                Size = new CelCurve(0.2f, 0.14f, 0.7f), SizeJitter = 0.1f, Stretch = 0.1f, Erode = new CelCurve(0f, 0.1f, 2f),
                Palette = "bi_bolt", Core = 3f, Extra = new Vector4(0.15f, 0.8f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "bolt bloom", Kind = CelKinds.GlowName, Start = 0f, Life = 0.13f, LifeJitter = 0f,
                Close = 1, Mid = 0, Far = 0, Aim = CelAim.Target, Speed = 0f, Arrive = 1f, Spread = 0f,
                Size = new CelCurve(0.3f, 0.22f), Erode = new CelCurve(0.05f, 0.3f), Palette = "fire", Core = 1.5f, Opacity = 0.35f,
                Extra = new Vector4(2f, 1.8f, 0f, 0f),
            });

        // ------------------------------------------------------------------------------------------ the impact
        public static readonly CelFxRecipe Hit = new CelFxRecipe("bi_hit", 0.8f,
            new CelLayer
            {
                // b02: a small white-orange star (6 points) facing the eye, 2-3 frames: the clip's f68 hit
                // b05: 3-4 frames (0.15 s), bigger: the clip's white-orange star at f29/f45/f53/f63/f68 (b04's 2-frame
                // star did not show in any sheet frame)
                // b07: 4 frames (0.17 s)
                Name = "flash", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.02f, -0.3f), Start = 0f, Life = 0.17f, LifeJitter = 0.05f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.17f * K, 0.11f * K, 0.5f), SizeJitter = 0.15f, Erode = new CelCurve(0.05f, 0.8f, 1.6f),
                Palette = "fire_hot", Core = 2.4f, Opacity = 0.95f, Spin = 2f, Extra = new Vector4(2f, 2.2f, 6f, 10f),
            },
            new CelLayer
            {
                // b02: a small white core (b01's 0.125 m at Core 10 washed the board white: hot_peak 28x the clip's)
                // d02: 1.6x bigger, Core 8 (b07 hot_peak 0.0018 vs the clip's 0.0045)
                // b05: a little bigger and hotter, 3 frames (hot_peak 0.001 vs the clip's 0.0045)
                Name = "core", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.03f, -0.3f), Start = 0f, Life = 0.16f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.24f * K, 0.13f * K), SizeJitter = 0.1f, Erode = new CelCurve(0f, 0.7f),
                Palette = "white", Core = 10f, Extra = new Vector4(2f, 0.5f, 0f, 0f),
            },
            new CelLayer
            {
                // b02: longer spikes (about the dummy's head long), 5 a hit, 2-3 frames
                // b05: wider, hotter and a frame longer, so they show at 19 m
                Name = "spikes", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.03f, -0.3f), Start = 0f, Duration = 0.005f, Life = 0.17f, LifeJitter = 0.05f,
                Close = 5, Mid = 2, Far = 0, Shape = CelShape.Shell, ShapeSize = new Vector3(0.03f, 0f, 0f), Aim = CelAim.Radial, UpBias = 0.25f,
                // b07: rays that stop about half the dummy's width out and hold for 4 frames (b06's 12 m/s spikes flew
                // off in a frame or two, so f29 showed a star with no spikes): 6 m/s with drag 8 tops out at 0.75 m
                // and the trail (stretch 0.17 s) reaches back to the core, so each reads as a ray from the hit
                Speed = 11f, SpeedJitter = 0.3f, Drag = 9f,
                Size = new CelCurve(0.10f, 0.05f), Stretch = 0.2f, Erode = new CelCurve(0f, 0.25f), Palette = "bi_spark", Core = 6f, Extra = new Vector4(0f, 0.3f, 0f, 0f),
            });

        // the crown: dark dirt spurts thrown up in a ring from the ground point, a few frames
        public static readonly CelFxRecipe Ground = new CelFxRecipe("bi_ground", 0.9f,
            new CelLayer
            {
                // b02: a spiky splash: more, thinner, faster spurts fanned wider (the clip's f68/f76 star of dirt)
                // b03: a thorny crown about half the dummy's height, 4-6 frames (b02's showed as a few dots at 19 m): more,
                // longer, faster spurts in a fan, gone in 0.22 s
                // b04: a fan, not a palisade: the spurts start on a wider ring and fly out radially with an up bias, so
                // the thorns splay out like the clip's crowns (f22/f60/f76), about half the dummy tall, 5-6 frames
                // b05: on the opaque BiThorn kind: each spurt is a wide-based thorn rooted on the ground that grows out
                // along its flight (the base stays put until it has flown 0.45 m; up bias 1.6: try a's 0.65 lay flat on the ground), so the crown splays like a fan of
                // thorns about half the dummy tall, dark brown with an ink edge, 5 frames; b04's Lobes squashed only
                // vertically and read as sticks
                Name = "crown", Kind = ThornName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.02f, 0f), Start = 0.0f, Duration = 0.02f, Life = 0.25f, LifeJitter = 0.15f,
                Close = 12, Mid = 0, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.1f * K, 0.03f, 0f), Aim = CelAim.Radial, UpBias = 2.2f, Speed = 6.0f * K, SpeedJitter = 0.35f,
                Gravity = 6f, Drag = 3.5f,
                Size = new CelCurve(0.26f * K, 0.12f * K), SizeJitter = 0.3f, Stretch = 0.36f, Erode = new CelCurve(0f, 0.6f, 2f), Feather = 0f,
                Palette = "bi_dirt", Core = 1f, Ink = 2.2f, Extra = new Vector4(0f, 0.45f, 0.3f, 0f),
            },
            new CelLayer
            {
                // b02: 9 dark flecks thrown out of every hit (b01's 4 at 2.5 cm did not show at 19 m)
                // b03: 10 bigger chips thrown up and out, falling back within about 15 frames
                // b06: thrown from the ground hit, not the chest (b05's board-hit flecks hung in the air as a splat)
                // b05: angular dark-brown chips (BiThorn diamonds with a notch), not b04's black round Lobe dots; they
                // turn with their arc, so they tumble as they fall
                Name = "flecks", Kind = ThornName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.08f, 0f), Start = 0.0f, Duration = 0.04f, Life = 0.6f, LifeJitter = 0.15f,
                Close = 10, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.1f, 0f, 0f), Aim = CelAim.Radial, Speed = 4.5f, SpeedJitter = 0.4f,
                UpBias = 1.6f, Gravity = 16f, Drag = 0.6f,
                Size = new CelCurve(0.075f, 0.06f), SizeJitter = 0.4f, Stretch = 0.022f, Erode = new CelCurve(0f, 0.3f, 3f),
                Palette = "bi_dirt", Core = 1f, Ink = 1.4f, Extra = new Vector4(1f, 0.1f, 0.8f, 0f),
            });

        // a dust puff: small round lobes that burst out of the ground at the hit and rise, together about the dummy's
        // torso wide, mid-tan with the light tan only on top, a thick dark ink on every lobe; they shrink and erode away
        // r05: the clip's puffs are cauliflower columns: few big lobes (each about the torso wide), born on the ground over
        // a few frames, rising and drifting off the hit, then shrinking apart (f84 -> f110); r04's 5-8 small balls read
        // as grapes. Size shrinks from r1 to a third with pow 2 (stays big, then shrinks late); ink 2.5 px.
        // b04: the puff is born small ON the ground at the hit (just after its crown, a little behind it so the crown
        // shows), grows fast to no wider than the dummy, rises as a column about a dummy height and spreads sideways,
        // then erodes away; a few small remnant lobes rise further and last to about f126 (the clip's f110-f126 balls).
        // b03's lobes were born at full size half a metre up, so the puffs hung at the dummy's waist from the first frame.
        static CelLayer PuffBody(int close, int mid, float width, float r0, float r1, float rise, float life) => new CelLayer
        {
            // b05: rooted on the ground (offset 0.5 r0) and a steady rise instead of a fast throw: drag 0.5 with buoyancy
            // 0.8 holds about 1.5 m/s, so a puff climbs 1-2 dummy heights over its 30-40 frames (b04's 1.4 m/s throw
            // with drag 1.4 parked it at the dummy's waist within 5 frames); spread 30 so it drifts off sideways
            // b07: cauliflower lobes (b07b: lump freq 1.15, amp 0.16; b07a 1.6 read as fine brain texture; b06's 0.9/0.09 read as smooth balls), stacked in a taller box
            // and a faster rise with more buoyancy, so each puff grows to a lumpy column up to about the dummy's height
            Name = "puff", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, r0 * 0.5f, 0.15f), Start = 0.06f, Duration = 0.2f, Life = life, LifeJitter = 0.15f,
            Close = close, Mid = mid, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(width, 0.25f, width * 0.4f), Aim = CelAim.Up, Spread = 40f,
            Speed = rise, SpeedJitter = 0.45f, Drag = 1.1f, Buoyancy = 0.35f, Spin = 0.2f,
            Size = new CelCurve(r0, r1, 0.45f), SizeJitter = 0.3f, Stretch = 1.1f, Erode = new CelCurve(0f, 0.8f, 3f), Feather = 0f,
            Palette = "bi_dust", Core = 1.0f, Ink = 2.4f, Extra = new Vector4(1.15f, 0.16f, 0.7f, 0f),
        };
        static CelLayer PuffRemnant(int close, int mid, float width, float r, float rise, float start, float life) => new CelLayer
        {
            Name = "puff remnant", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.3f, 0.15f), Start = start, Duration = 0.2f, Life = life, LifeJitter = 0.15f,
            Close = close, Mid = mid, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(width, 0.2f, width * 0.4f), Aim = CelAim.Up, Spread = 40f,
            Speed = rise, SpeedJitter = 0.35f, Drag = 1.2f, Buoyancy = 0.3f, Spin = 0.2f,
            Size = new CelCurve(r, r * 0.5f, 1f), SizeJitter = 0.3f, Stretch = 1f, Erode = new CelCurve(0f, 0.35f, 3f), Feather = 0f,
            Palette = "bi_dust", Core = 1.0f, Ink = 1.6f, Extra = new Vector4(1.1f, 0.14f, 0.7f, 0f),
        };
        // d20: the puff bodies live 1.4/1.2 s (were 1.75/1.5): the big masses clear by about f105 and only the small
        // remnants linger to f125, as the clip's (ours stayed big clusters at f99-f110)
        // b05: never wider than the dummy (b04's 0.69 m box of 0.85 m lobes made 1.5 m clumps): a 0.4 m box, lobes to 0.34 m
        public static readonly CelFxRecipe PuffBig = new CelFxRecipe("bi_puff_big", 2.6f,
            PuffBody(8, 3, 0.8f * K, 0.13f * K, 0.4f * K, 0.9f * K, 1.4f), PuffRemnant(5, 2, 0.75f * K, 0.12f * K, 0.8f * K, 0.75f, 1.1f));
        public static readonly CelFxRecipe PuffSmall = new CelFxRecipe("bi_puff_small", 2.2f,
            PuffBody(6, 2, 0.62f * K, 0.11f * K, 0.32f * K, 0.75f * K, 1.2f), PuffRemnant(4, 1, 0.55f * K, 0.1f * K, 0.7f * K, 0.65f, 1.0f));

        // ------------------------------------------------------------------------------------------ the light
        // power: the light's gain (the scene x (1 + power x colour)); fill: an added share of it so black ground warms too
        static CelLayer Wash(string name, string palette, float r0, float r1, float power, float life, int mid, float squash, float fill, float falloff = 1.3f) => new CelLayer
        {
            Name = name, Kind = WashName, Start = 0f, Life = life, LifeJitter = 0.1f,
            Close = 1, Mid = mid, Far = 0, Size = new CelCurve(r0, r1, 0.5f), SizeJitter = 0.08f,
            Palette = palette, Core = power, Opacity = 1f, Ink = fill, Stretch = 1.4f, Extra = new Vector4(0f, falloff, squash, 0.1f),
        };
        // the pool at the dummy's feet: about two dummy heights across, flat, 2-4 frames
        // b02: dimmer and wider than b01 (its gain 2.6-3.0 pool and gain-7 face flare turned the board white: hot_peak 0.13
        // vs the clip's 0.0045); the board should read yellow-lit, as the clip's f29/f45/f53
        // b03: brighter, orange, a flatter falloff and more fill (b02's showed only as a small glow at the dummy's base)
        // b04: a soft pool about 3 dummy widths across that lights the ground only (squash 3.5: it reaches 0.6 m up, so
        // the dust above it keeps its grey-tan; b03's round pool lit the lobes saturated orange and made f68's hot blob)
        public static readonly CelFxRecipe WashPool = new CelFxRecipe("bi_wash_pool", 0.3f, Wash("pool", "bi_pool", 3.2f * K, 3.5f * K, 1.1f, 0.18f, 1, 6.0f, 0.4f, 1.5f));
        // the same late in the burst, when the clip's light is at its brightest (f60-f80)
        public static readonly CelFxRecipe WashPoolBig = new CelFxRecipe("bi_wash_pool_big", 0.3f, Wash("pool", "bi_pool", 3.4f * K, 3.7f * K, 1.2f, 0.19f, 1, 6.0f, 0.4f, 1.5f));
        // b04: the dummy's own light: a column (sideways squash 2) round the dummy, as tall as it and only as wide, so the
        // board lights orange on every burst (the clip's f29/f45/f53/f76) and the puffs beside it do not
        // b05: gain 8 and more fill, 0.17 s (4 frames): the board reads gold on each board hit (the clip's f29/f45/f53/
        // f63/f68); b04's gain 5 left it a dull orange
        // d02: gain 2.0 and a warmer orange (#FFB878), half the fill: b07's gain 3.6 turned the board a flat white lantern (rings lost)
        public static readonly CelFxRecipe WashDummy = new CelFxRecipe("bi_wash_dummy", 0.3f, Wash("dummy", "bi_flare", 1.25f * K, 1.25f * K, 2.0f, 0.26f, 1, -2.0f, 0.06f));
        // b05: the warm pool round the dummy's foot on a board hit, about three dummy widths, with enough fill that the
        // dark ground shows it (b04's gain-2.2 pool with fill 0.1 did not show on the dark beach)
        public static readonly CelFxRecipe WashPoolHit = new CelFxRecipe("bi_wash_pool_hit", 0.3f, Wash("pool", "bi_pool", 3.8f * K, 4.2f * K, 1.3f, 0.21f, 1, 6.0f, 0.42f, 1.5f));
        // the same, softer, when the round strikes the ground near the dummy
        public static readonly CelFxRecipe WashDummySoft = new CelFxRecipe("bi_wash_dummy_soft", 0.3f, Wash("dummy", "bi_flare", 1.4f * K, 1.4f * K, 1.4f, 0.14f, 0, -2.0f, 0.03f));
        // b02: the gun's light on the near field on EVERY shot (the clip lights its lower-left quarter, crates and reeds,
        // on each of its 4.5-frame pulses: area 0.1-0.24 of the frame at f24/f33/f49/f62/f71/f76), 2-3 frames; a low fill
        // so the crates' shadows stay black
        // b05: tan-orange at about 3/4 of b04's gain (b04 lit the crates overbright yellow; half gain read dull red, peak area 0.49)
        public static readonly CelFxRecipe WashNear = new CelFxRecipe("bi_wash_near", 0.4f, Wash("near", "bi_wash_near", 4.6f, 5.0f, 1.0f, 0.1f, 0, 1.2f, 0.1f));
        // the bolt rounds: wider and brighter
        public static readonly CelFxRecipe WashNearBig = new CelFxRecipe("bi_wash_near_big", 0.4f, Wash("near", "bi_wash_near", 5.6f, 6.0f, 1.3f, 0.12f, 0, 1.2f, 0.14f));
        // b03: the burst's peak round (the clip's f76): the widest, brightest, longest near light
        // b04e: wider and longer (peak area was 0.48 of the clip's f76)
        public static readonly CelFxRecipe WashNearPeak = new CelFxRecipe("bi_wash_near_peak", 0.4f, Wash("near", "bi_wash_near", 11f, 11.8f, 1.7f, 0.2f, 0, 1.2f, 0.2f));

        // b07: the light is softer and wider with less gain: the pools reach about 3 dummy widths (falloff 1.5, b06 2.4)
        // and do not clip to a hard cream disc; the dummy column is at gain 3.6 (b06 8: flat lemon yellow); the near
        // light on the crates has half the multiply gain with the same fill, over a wider area

        // ------------------------------------------------------------------------------------------ the far beacon
        // b07: at T1 and out the close pieces are a few pixels or less (b06's T1 burst was one spark; O120/O240 showed
        // nothing), so each impact also plays a beacon sized for its band: a warm flash facing the eye, a pale ground pool
        // 3-4x the flash, and a tan dust smudge that lasts about a second. Glow + Lobe only (draws shared with the close
        // pieces). Mid (T1): all three; Far (O120): the flash and the smudge; Far2 (O240): the flash and a fainter smudge.
        static CelLayer BeaconFlash(float r, float life) => new CelLayer
        {
            Name = "beacon flash", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, r * 0.3f, 0f), Start = 0f, Life = life, LifeJitter = 0.05f,
            Close = 1, Mid = 1, Far = 1, Size = new CelCurve(r, r * 0.6f, 0.5f), SizeJitter = 0.1f, Erode = new CelCurve(0.05f, 0.8f, 1.6f),
            Palette = "fire_hot", Core = 2.6f, Opacity = 0.95f, Extra = new Vector4(2f, 2f, 6f, 6f),
        };
        static CelLayer BeaconPool(float r, float life) => new CelLayer
        {
            Name = "beacon pool", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.05f, 0f), Start = 0f, Life = life, LifeJitter = 0.05f,
            Close = 1, Mid = 1, Far = 1, Size = new CelCurve(r, r * 1.1f, 0.5f), SizeJitter = 0.1f, Erode = new CelCurve(0.1f, 0.85f, 1.2f),
            Palette = "bi_pool", Core = 1.4f, Opacity = 0.7f, Extra = new Vector4(0f, 1.4f, 0f, 0f),
        };
        static CelLayer BeaconSmudge(float r, float life, float ink) => new CelLayer
        {
            Name = "beacon smudge", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, r * 0.6f, 0f), Start = 0.04f, Life = life, LifeJitter = 0.1f,
            Close = 1, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.Up, Speed = r * 0.5f, Drag = 1.5f,
            Size = new CelCurve(r * 0.6f, r, 0.5f), SizeJitter = 0.15f, Stretch = 0.8f, Erode = new CelCurve(0f, 0.85f, 2.5f), Feather = 0f,
            Palette = "bi_dust", Core = 1f, Ink = ink, Extra = new Vector4(1.4f, 0.14f, 0.7f, 0f),
        };
        // b07b: bigger and longer (b07a's T1 flash missed both stills and O120 showed only a grey dot): the flash lives
        // longer than the shot gap, so a blink is always up while the gun fires; Far gets a pool too (the same Glow draw)
        public static readonly CelFxRecipe BeaconMid = new CelFxRecipe("bi_beacon_mid", 1.2f, BeaconFlash(1.4f, 0.24f), BeaconPool(3.0f, 0.3f), BeaconSmudge(0.8f, 1.05f, 1.6f));
        public static readonly CelFxRecipe BeaconFar = new CelFxRecipe("bi_beacon_far", 1.2f, BeaconFlash(5.0f, 0.28f), BeaconPool(8f, 0.3f), BeaconSmudge(3.0f, 1.0f, 1.2f));
        public static readonly CelFxRecipe BeaconFar2 = new CelFxRecipe("bi_beacon_far2", 1.2f, BeaconFlash(9.0f, 0.3f), BeaconPool(14f, 0.3f), BeaconSmudge(5.0f, 0.9f, 1f));

        /// <summary>The beacon for the current zoom (none at Close).</summary>
        static CelFxRecipe BeaconFor(float zoom) => float.IsNaN(zoom) || zoom < 22f ? null : zoom < 75f ? BeaconMid : zoom < 180f ? BeaconFar : BeaconFar2;

        // ------------------------------------------------------------------------------------------ one round
        /// <summary>The shot: tracers from `muzzle` to `hit` (they arrive after Flight s); `nearBolt` adds the thick bolt
        /// past the camera and the light it throws on the foreground. No allocation.</summary>
        public static void PlayShot(Vector3 muzzle, Vector3 hit, Vector3 ground, uint seed, bool nearBolt, bool bigLight = false, bool overshoot = false, bool peak = false)
        {
            Vector3 dir = hit - muzzle;
            // b02: the tracers start at the gun, so they enter from the frame's lower-left edge (b01's started 18 % down
            // the line and read as short lines in mid-frame); an overshoot round flies past the dummy to the upper right
            Vector3 aim = overshoot ? hit + dir * 0.55f + Vector3.up * 1.2f : hit;
            CelFx.Play(Tracer, muzzle, aim - muzzle, aim, seed);
            // d13/d14: about every other shot also sends one line on past and over the dummy (the clip's f63-f79 lines)
            if (!overshoot && (seed & 1u) == 0u)
            {
                Vector3 past = hit + dir * 0.6f + Vector3.up * (0.8f + 0.6f * ((seed * 2654435761u >> 8) & 255) / 255f);
                CelFx.Play(Tracer, muzzle + Vector3.up * 0.1f, past - muzzle, past, seed + 911u);
            }
            // the gun's light on the near field, every shot (on the near field's own surface; r05 try2: lerped to the
            // dummy's ground it floated off the near slope)
            Vector3 near = Vector3.Lerp(muzzle, hit, 0.36f);
            near.y = RenderGround.Map != null ? RenderGround.Sample(RenderGround.Map, near.x, near.z) + 0.4f : Mathf.Lerp(near.y, ground.y, 0.85f);
            CelFx.Play(peak ? WashNearPeak : nearBolt || bigLight ? WashNearBig : WashNear, near, dir, near, seed + 503u);
            if (!nearBolt) return;
            // the bolt crosses the near foreground only: its head dies under half way to the hit (the clip's f22/f37/f76
            // bolt head sits left of the frame's middle); its trail reaches back off the frame
            Vector3 end = Vector3.Lerp(muzzle, hit, 0.45f) + Vector3.up * 0.5f;
            CelFx.Play(NearBolt, muzzle, end - muzzle, end, seed + 401u);
        }

        /// <summary>The impact at `hit`, with `ground` where its dust goes and `washAt` the foot the light pools at.
        /// `onBody` hits flare the dummy harder; `puff` 0 none, 1 small, 2 big; `heat` 0..1 how long the gun has fired (the
        /// pool grows past half). No allocation.</summary>
        public static void PlayImpact(Vector3 muzzle, Vector3 hit, Vector3 ground, Vector3 washAt, uint seed, bool onBody, int puff, float heat = 0.5f)
        {
            Vector3 dir = hit - muzzle;
            CelFx.Play(Hit, muzzle, dir, hit, seed + 101u);
            CelFx.Play(Ground, ground - dir.normalized * 0.01f, dir, ground, seed + 211u);
            if (puff > 0) CelFx.Play(puff > 1 ? PuffBig : PuffSmall, ground - dir.normalized * 0.01f, dir, ground, seed + 601u);
            Vector3 under = washAt + Vector3.up * 0.05f;
            CelFx.Play(onBody ? WashPoolHit : heat > 0.5f ? WashPoolBig : WashPool, under, dir, under, seed + 307u);
            // the dummy's column light: on every round, strong when the round strikes the board (centre at its middle)
            Vector3 mid = washAt + Vector3.up * (1.15f * K);   // b06: higher, narrower column
            CelFx.Play(onBody ? WashDummy : WashDummySoft, mid, dir, mid, seed + 353u);
            // b07: the band's beacon at the ground spot (T1 and out)
            var beacon = BeaconFor(CelFx.Zoom);
            if (beacon != null) CelFx.Play(beacon, ground, dir, ground, seed + 701u);
        }

        /// <summary>Shot and impact at once (the game, where the flight is shorter than a frame or not wanted).</summary>
        public static void PlayRound(Vector3 muzzle, Vector3 hit, Vector3 ground, uint seed, bool nearBolt = false)
        {
            PlayShot(muzzle, hit, ground, seed, nearBolt);
            PlayImpact(muzzle, hit, ground, ground, seed, false, 1);
        }

        // ------------------------------------------------------------------------------------------ the Range burst
        // b02: the shots go off on the clip's own light pulses (its area peaks, measured on bullet_impacts_cel: f18 f24 f28
        // f33 f38 f42 f45 f49 f52 f57 f62 f66 f71 f76 f81; about 4.5 frames apart, not b01's even 4.15, which drifted to
        // lag 10). f18 is before the Range's fire (first drawn f20), so the first round goes at f20. Film frame f is
        // t = f/24 and the fire is at 0.8 s (f19.2), so a shot at frame F is due (F - 20.05)/24 s after the fire: it is
        // born on frame F whether the shot clock started at f19.2 or f20.
        // b07: board rounds 1 and 7 a frame earlier (23.3, 47.3) so their hits are born on f28/f52 and the board and pool
        // are lit at the clip's f29/f53 (b06's were born on those frames, at zero light)
        static readonly float[] ShotFrames = { 20f, 23.3f, 28f, 33f, 38f, 42f, 45f, 47.3f, 52f, 57f, 62f, 66f, 71f, 75f, 81f };
        static readonly int Rounds = ShotFrames.Length;

        sealed class Burst { public int NextShot, NextHit; }

        static float Hash01(int seed, int k, int salt)
        {
            uint h = (uint)seed * 747796405u + (uint)k * 2891336453u + (uint)salt * 1181783497u;
            h ^= h >> 16; h *= 0x7feb352du; h ^= h >> 15; h *= 0x846ca68bu; h ^= h >> 16;
            return (h & 0xFFFFFF) / 16777216f;
        }

        // where the rounds go, in the camera's frame at the dummy (x screen right, z away from the eye; m); uneven on
        // purpose: one near cluster left of the dummy, two behind it on the right, one far left, the board
        // b04: measured on the clip (about 65 px a metre at the dummy): 0 the board; 1 A left of the foot (f22 crown); 2 B
        // right, behind (f22/f29 puff); 3 C left, in front (f60/f76 crown, f84 left puff); 4 D right, in front (f84 crown,
        // the big right puff); 5 E far left (the small f84-f126 puff)
        static readonly Vector2[] Spots = { new Vector2(0f, 0f), new Vector2(-1.1f, 0f), new Vector2(1.6f, 0.4f), new Vector2(-1.7f, -0.8f), new Vector2(1.5f, -0.8f), new Vector2(-2.8f, 0.2f) };
        // the rounds that come as a thick bolt past the camera (the clip's f22, f37, f45, f53, f62, f76)
        // r05: which spot each round strikes, fixed (r04 picked by hash, so the dust gathered on the right). The hit of
        // round k lands at frame 19 + 4.15 k: the board on rounds 2/6/8/12 (f27/f44/f52/f69, the clip's flares); round 14 (f77) puts a crown and a big puff on the ground left of the dummy
        // b04: hits land at about f24.5 + the shot gaps: A, board (f29), B, A, board (f45), B, board, board (f53), C (f60
        // crown), A, board (f68), B, C (f76 crown), D (f84 crown), E
        // b05: round 9 (hit f62) strikes the board, so the board flashes at f29/f43/f50/f54/f62/f67 (the clip's f29/f45/
        // f53/f63/f68); A left of the foot keeps rounds 0 and 3, C left-front rounds 8 and 12
        static readonly int[] Order = { 1, 0, 2, 1, 0, 2, 0, 0, 3, 0, 0, 2, 3, 4, 5 };
        // b04: one puff per place, born with its first hit (2 big, 1 small, 0 none): A f24, B f32, C f56, D f79, E f85
        // (b03 puffed every ground hit and the re-struck spots grew two fixed clumps)
        // b05: only those first hits, no refeeds and no foot puffs on board hits: 4 big separate puffs and E small
        // (b04's refeeds merged into two fixed clumps beside the dummy)
        // b06: small puffs also at the dummy's foot on board hits r1 and r6, so the puffs make a low band either side of it
        // b07: one more foot puff on board hit r4, so the f37-f60 band reaches 3-4 dummy widths
        static readonly int[] Puffs = { 2, 1, 2, 0, 1, 0, 1, 0, 2, 0, 0, 0, 0, 2, 1 };
        // b02: the bolt rounds on the clip's f24/f38/f45/f52/f62/f76 shots; overshoots past the dummy on f33/f57/f66/f81
        static bool IsNear(int k) => !IsOver(k);   // b03: every round but the overshoots carries the thick near streak
        static bool IsOver(int k) => k == 3 || k == 11 || k == 14;

        static float ShotTime(RangeShot s, int k) => (ShotFrames[k] - 20.05f) / 24f;

        static void Places(RangeShot s, int k, Vector3 look, Vector3 right, out Vector3 from, out Vector3 hit, out Vector3 ground, out bool onBody)
        {
            float a = Hash01(s.Seed, k, 1), c = Hash01(s.Seed, k, 2), d = Hash01(s.Seed, k, 3);
            Vector2 spot = Spots[Order[k % Order.Length]];
            onBody = spot.sqrMagnitude < 1e-6f;
            if (onBody)
            {
                hit = s.Target + right * ((a - 0.5f) * 0.3f * K) + Vector3.up * ((c - 0.5f) * 0.45f * K);
                // b04d: the board hit kicks its dust at the dummy's foot, on the gun side (the clip's f37-f53 dust round its base)
                ground = s.Ground + right * K * (-0.35f + 0.5f * a) + look * K * (-0.45f + 0.3f * c);
            }
            else
            {
                ground = s.Ground + right * K * (spot.x + (a - 0.5f) * 0.3f) + look * K * (spot.y + (c - 0.5f) * 0.3f);
                hit = ground;
            }
            // on the field's own surface (the ground left of the dummy rises: a flat height buried the puffs there)
            if (RenderGround.Map != null) ground.y = RenderGround.Sample(RenderGround.Map, ground.x, ground.z);
            if (!onBody) hit = ground;
            from = s.Muzzle + Vector3.up * ((d - 0.5f) * 0.2f);
        }

        static void Init(RangeShot s) { EnsureRenderer(); s.State = new Burst(); Step(s); }

        /// <summary>The kit's CelFx.Renderer makes its renderer with AddComponent, whose Awake asks CelFx.Renderer again
        /// before it is set and recurses into a stack overflow (reported to the kit). Make it here first, on an
        /// inactive object, adopt it, then wake it: Awake then finds it adopted.</summary>
        public static void EnsureRenderer()
        {
            if (Object.FindFirstObjectByType<CelFxRenderer>() != null) return;
            var go = new GameObject("CelFx") { hideFlags = HideFlags.DontSave };
            go.SetActive(false);
            var r = go.AddComponent<CelFxRenderer>();
            CelFx.Adopt(r);
            go.SetActive(true);
        }

        static void Step(RangeShot s)
        {
            if (!(s.State is Burst b)) return;
            float since = Time.time - s.FiredAt;
            Vector3 look = s.Target - s.Eye; look.y = 0f;
            if (look.sqrMagnitude < 1e-4f) { look = s.Direction; look.y = 0f; }
            look = look.sqrMagnitude > 1e-6f ? look.normalized : Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, look).normalized;
            float keep = CelFx.ZoomOverride;
            CelFx.ZoomOverride = s.Zoom;   // the shot's tier, not whatever the camera reads while the stage poses it
            Vector3 from, hit, ground; bool onBody;
            while (b.NextShot < Rounds && ShotTime(s, b.NextShot) <= since + 1e-4f)
            {
                int k = b.NextShot++;
                Places(s, k, look, right, out from, out hit, out ground, out onBody);
                PlayShot(from, hit, ground, (uint)(s.Seed * 13 + k * 977), IsNear(k), k == 13, IsOver(k), k == 13);
            }
            while (b.NextHit < b.NextShot && ShotTime(s, b.NextHit) + Flight <= since + 1e-4f)
            {
                int k = b.NextHit++;
                Places(s, k, look, right, out from, out hit, out ground, out onBody);
                // dust on the ground hits up to round 14 (all gone by about f120); big puffs right and near left, small far left
                int puff = Puffs[k % Puffs.Length];
                PlayImpact(from, hit, ground, s.Ground, (uint)(s.Seed * 13 + k * 977), onBody, puff, k / (float)(Rounds - 1));
            }
            CelFx.ZoomOverride = keep;
        }

        // the scan finds this; its first call runs the static constructor, which registers the burst again with its
        // per-frame update (CelRange reads the entry afresh every frame), then starts it
        [CelRangeEffect("bullet_impacts")]
        static void RangeFire(RangeShot s) => Init(s);
    }
}
