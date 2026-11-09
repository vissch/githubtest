// Phase: CelFx job (2026-11-09, lane/show/cel-explosion) - the cel shell explosion (reference clip
// explosive_blasts_nolora): white-yellow inked fireball lobes with orange edges, white spark streaks, dark debris, a
// warm glow pool on the ground, secondary ground blasts (domes of white light), tan cel smoke clouds with ink and dark
// dirt in them, and orange embers drifting after. Built only from the kit's Streak, Lobe and Glow kinds (plus a light),
// plus its own ExpFx kind (ExplosionKind.cs: soft pools and flashes, jagged spark arcs) in place of the kit's Glow: at
// most four draws (Streak, Lobe x2, ExpFx) however many blasts play.
// r02: a white HDR heart and a soft bloom halo over orange-yellow lobes with thin red-orange ink; soft (unbanded) pools
// and domes; white jagged arcs; a thick orange late fan with orange spark dots; tracers lead the fire.
// r03: orange-yellow lobes with no white band (one white heart only), grey-tan puffs wrapping the fire from 0.7 s that
// take it over, side sites as a dome then dark debris and a spark plume (no fireball), a peak wash at 2.05-3.1 s,
// pale cream clouds with few specks and orange embers drifting to ~5.5 s.
// r04: a ragged white-yellow star (ExpFx mode 3) in place of the round heart over a smaller fireball, grey-tan rim
// puffs in front, the ground light cut to the fireball, side domes on the clip frames, narrower paler late clouds.
// r05: a fireball 1.5x as wide under a wider white-yellow star, no smoke round it before ~2.6 s (the wrap is born late
// and narrow), dark ink and dark chunks through the fire, a brown ground wash over the near field, flat-footed side
// domes each throwing 30+ big dark chunks, grey puffs and a spark fan, wider spark dots, embers kept to ~f155.
//
// Recipes (all anchored at their target, effect frame = the eye's flat forward, so x is screen right and z is depth):
//   Hold   the sustained blast at the hit: tracers stream in from `origin`, a boiling fireball, a spark stream, a
//          long glow pool, flung debris, a closing burst at 2.65 s, a light.
//   Site   one secondary ground blast (target = its ground point): a dome flash, a short pool, fire, sparks, debris.
//   Cloud  one smoke cloud (target = its middle): tan ink lobes, dark dirt lumps, embers. Lives ~2 s then cuts away.
//   Fan    the late spark fan thrown low across the ground (dir = where it goes).
// A single shell in the game is Site + Cloud (PlayShell). The Range (ExplosionRange.cs) composes the reference shot.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class ExplosionRecipe
    {
        static ExplosionRecipe()
        {
            // colours picked off the reference frames (f30 ground, f86 fireball, f110 smoke), as they look
            CelPalette.Add("exp_fire", CelPalette.Hex(0xF08A28), CelPalette.Hex(0xFFA030), CelPalette.Hex(0xFFC040), CelPalette.Hex(0xFFDC60), CelPalette.Hex(0xFFFFFF), 0.82f, 4, CelPalette.Hex(0xD05A18));
            CelPalette.Add("exp_dome", CelPalette.Hex(0xF0A050), CelPalette.Hex(0xC07030), CelPalette.Hex(0xFFD090), CelPalette.Hex(0xFFE8B0), CelPalette.Hex(0xFFFFFF), 0.5f, 4, CelPalette.Hex(0x000000));
            // the fireball's white heart and its bloom halo (soft ExpFx flashes: c1 edge, c3 middle, core above coreAt)
            // r03: the heart adds little at its edge (so it does not blow the lobes round it to white) and is white only
            // in its middle, one solid heart about 0.4x the fireball's width
            CelPalette.Add("exp_core", CelPalette.Hex(0x602008), CelPalette.Hex(0x602008), CelPalette.Hex(0x904010), CelPalette.Hex(0xC06020), CelPalette.Hex(0xFFFFFF), 0.3f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("exp_bloom", CelPalette.Hex(0x401808), CelPalette.Hex(0x602808), CelPalette.Hex(0x904010), CelPalette.Hex(0xC06424), CelPalette.Hex(0xFFB060), 0.85f, 4, CelPalette.Hex(0x000000));
            // r03: the fireball's lobes are orange to yellow with no white core band (the white is the heart's alone), and
            // an orange ink, not a dark rim
            CelPalette.Add("exp_ball", CelPalette.Hex(0xF08A28), CelPalette.Hex(0xFFA838), CelPalette.Hex(0xFFC848), CelPalette.Hex(0xFFE070), CelPalette.Hex(0xFFE478), 0.97f, 4, CelPalette.Hex(0xC85A1C));
            // r05: the same lobes with a dark brown ink (the ref's cel outlines read dark over the fire)
            CelPalette.Add("exp_ball_ink", CelPalette.Hex(0xF0882C), CelPalette.Hex(0xF8A848), CelPalette.Hex(0xF6CC88), CelPalette.Hex(0xF2DAA2), CelPalette.Hex(0xF2DAA2), 0.97f, 4, CelPalette.Hex(0x240E06));
            // the grey-tan puffs that wrap the fire from f40 on and take it over (warm-lit from the fire below)
            CelPalette.Add("exp_wrap", CelPalette.Hex(0x9A8C80), CelPalette.Hex(0xB4A694), CelPalette.Hex(0xC8BAA6), CelPalette.Hex(0xDACCB6), CelPalette.Hex(0xDACCB6), 1.1f, 4, CelPalette.Hex(0x8A7866));
            // r04: the ragged white-yellow star over the fireball (additive: orange rim, yellow, white heart)
            CelPalette.Add("exp_star", CelPalette.Hex(0x702808), CelPalette.Hex(0xA04812), CelPalette.Hex(0xD08A30), CelPalette.Hex(0xFFC860), CelPalette.Hex(0xFFFFFF), 0.68f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("exp_ember", CelPalette.Hex(0xC04010), CelPalette.Hex(0xF07020), CelPalette.Hex(0xFF9A30), CelPalette.Hex(0xFFB850), CelPalette.Hex(0xFFD080), 0.8f, 3, CelPalette.Hex(0x401004));
            CelPalette.Add("exp_arc", CelPalette.Hex(0xB8C8FF), CelPalette.Hex(0xD8E2FF), CelPalette.Hex(0xF0F4FF), CelPalette.Hex(0xFFFFFF), CelPalette.Hex(0xFFFFFF), 0.45f, 3, CelPalette.Hex(0x000000));
            CelPalette.Add("exp_spark", CelPalette.Hex(0xE08030), CelPalette.Hex(0xFFC070), CelPalette.Hex(0xFFE8B0), CelPalette.Hex(0xFFFFFF), CelPalette.Hex(0xFFFFFF), 0.6f, 3, CelPalette.Hex(0x401004));
            CelPalette.Add("exp_tracer", CelPalette.Hex(0xC04010), CelPalette.Hex(0xF07A20), CelPalette.Hex(0xFFB040), CelPalette.Hex(0xFFE090), CelPalette.Hex(0xFFF4D0), 0.75f, 3, CelPalette.Hex(0x401004));
            // the pool is additive: these are what it adds to the dark plate (#282c37 -> #b77650 at the middle)
            CelPalette.Add("exp_pool", CelPalette.Hex(0x2A1A08), CelPalette.Hex(0x6A3C14), CelPalette.Hex(0x9A6A30), CelPalette.Hex(0xD09048), CelPalette.Hex(0xFFD890), 0.7f, 4, CelPalette.Hex(0x000000));
            // the warm light the blasts throw over the whole field (the clips' ground turns brown from 0.9 s to 4 s)
            CelPalette.Add("exp_wash", CelPalette.Hex(0x181008), CelPalette.Hex(0x22180E), CelPalette.Hex(0x4A3420), CelPalette.Hex(0x6A4A30), CelPalette.Hex(0x6A4A30), 1.1f, 4, CelPalette.Hex(0x000000));
            // r03: pale cream smoke (#c8b8a0 in the middle band) with a soft brown ink, as the ref's f105-f135
            // r04: the faint wide warmth over the whole near field (brown, low chroma), under the brighter pool
            CelPalette.Add("exp_wide", CelPalette.Hex(0x100A06), CelPalette.Hex(0x1A120A), CelPalette.Hex(0x3A2816), CelPalette.Hex(0x543A22), CelPalette.Hex(0x543A22), 1.1f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("exp_smoke", CelPalette.Hex(0xB0A090), CelPalette.Hex(0xBEAE98), CelPalette.Hex(0xC8B8A0), CelPalette.Hex(0xD4C6AE), CelPalette.Hex(0xD4C6AE), 1.1f, 2, CelPalette.Hex(0x3A2A20));
            CelPalette.Add("exp_dirt", CelPalette.Hex(0x1E1714), CelPalette.Hex(0x2E2420), CelPalette.Hex(0x44362E), CelPalette.Hex(0x5A463A), CelPalette.Hex(0x5A463A), 1.1f, 3, CelPalette.Hex(0x120C0A));
            // r05: the thrown chunks, near black with a dark brown lit side (the ref's debris over the fire and the domes)
            CelPalette.Add("exp_chunk", CelPalette.Hex(0x120C0A), CelPalette.Hex(0x1A1210), CelPalette.Hex(0x261A14), CelPalette.Hex(0x3A2A20), CelPalette.Hex(0x3A2A20), 1.1f, 3, CelPalette.Hex(0x080504));
            // r05: the brown light over the near field (adds about #43200a at the middle: the plate's #282c37 to the ref's #6b4c41)
            CelPalette.Add("exp_brown", CelPalette.Hex(0x140A04), CelPalette.Hex(0x281408), CelPalette.Hex(0x3C2812), CelPalette.Hex(0x50381E), CelPalette.Hex(0x50381E), 1.1f, 4, CelPalette.Hex(0x000000));
        }

        /// <summary>Touch the class so the palettes exist before the first play.</summary>
        public static void Init() { }

        const string Fx = ExpFxKind.KindName;

        public static readonly CelFxRecipe Hold = new CelFxRecipe("explosion_hold", 3.4f,
            new CelLayer
            {
                Name = "tracers", Kind = CelKinds.StreakName, Anchor = CelAnchor.Origin, Shape = CelShape.Box, ShapeSize = new Vector3(0f, 0.7f, 1.2f),
                Start = 0f, Duration = 2.6f, TimeJitter = 0.04f, Life = 0.22f, LifeJitter = 0.1f, Close = 18, Mid = 8, Far = 0,
                Aim = CelAim.Target, Speed = 0f, Spread = 0f, Size = new CelCurve(0.16f, 0.12f), SizeJitter = 0.25f, Stretch = 0.18f,
                Erode = new CelCurve(0.05f, 0.25f), Palette = "exp_tracer", Core = 5f, Extra = new Vector4(0.1f, 1f, 0f, 0f),
            },
            new CelLayer
            {
                // the first dome of light: soft, a little wider than the dummy is tall, gone in 8 frames
                Name = "flash", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.3f, -2.2f), Start = 0f, Life = 0.34f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(2.0f, 2.2f, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 1.5f),
                Palette = "exp_dome", Core = 1.6f, Extra = new Vector4(ExpFxKind.Flash, 0.7f, 1.23f, 0f),
            },
            new CelLayer
            {
                // r04: the ragged white-yellow star over the fireball (10 uneven spikes that re-jump), re-born all hold
                Name = "star", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.55f, -1.6f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.15f, 0f, 0f),
                Start = 0.05f, Duration = 2.75f, Life = 0.3f, LifeJitter = 0.2f,
                Close = 12, Mid = 6, Far = 0, Size = new CelCurve(1.55f, 1.7f, 0.5f), SizeJitter = 0.12f, Erode = new CelCurve(0f, 0.7f, 2f),
                Palette = "exp_star", Core = 1.6f, Opacity = 0.85f, Extra = new Vector4(ExpFxKind.Star, 0f, 0f, 10f),
            },
            new CelLayer
            {
                // a soft warm bloom about 1.5x the fireball's radius (faint: it must not blow the lobes to white)
                Name = "bloom", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.55f, -1.8f), Start = 0f, Duration = 2.7f, Life = 0.6f, LifeJitter = 0.1f,
                Close = 6, Mid = 3, Far = 0, Size = new CelCurve(2.8f, 3.2f, 0.5f), SizeJitter = 0.08f, Erode = new CelCurve(0f, 1f, 2f),
                Palette = "exp_bloom", Core = 1f, Opacity = 0.3f, Extra = new Vector4(ExpFxKind.Flash, 2.0f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "pool", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.93f, 0f), Start = 0f, Life = 3.1f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(2.5f, 3.2f, 0.3f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 6f),
                Palette = "exp_pool", Core = 1f, Opacity = 0.8f, Extra = new Vector4(ExpFxKind.Pool, 1.2f, 0f, 0f),
            },
            new CelLayer
            {
                // the warm light over the field: full until ~2.9 s (the ref's ground is brown to f89), then gone by f95
                Name = "wash", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.93f, -2f), Start = 0f, Life = 3.15f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(5.5f, 7f, 0.7f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 9f),
                Palette = "exp_brown", Core = 1f, Opacity = 1.1f, Extra = new Vector4(ExpFxKind.Pool, 0.5f, 0f, 0f),
            },
            new CelLayer
            {
                // r04: a faint wide warmth over the near field (low chroma brown), the brighter pool stays at the fire
                Name = "wide wash", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(-0.5f, -0.45f, -2.5f), Start = 0f, Life = 3.15f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(12f, 16f, 0.7f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 9f),
                Palette = "exp_brown", Core = 1f, Opacity = 1.4f, Extra = new Vector4(ExpFxKind.Pool, 0.4f, 0f, 0f),
            },
            new CelLayer
            {
                // the peak (ref f72-f89): the side blasts and the closing burst light the whole near field
                Name = "peak wash", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(-5f, -0.93f, 2f), Start = 2.25f, Life = 0.95f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(6f, 9f, 0.6f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 8f),
                Palette = "exp_wash", Core = 1f, Opacity = 1.4f, Extra = new Vector4(ExpFxKind.Pool, 0.6f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "fireball", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.2f, 0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.55f, 0f, 0f),
                Start = 0f, Duration = 2.75f, Life = 0.5f, LifeJitter = 0.3f, Close = 84, Mid = 26, Far = 0,
                Aim = CelAim.Radial, Speed = 1.8f, Drag = 2.5f, Buoyancy = 0.3f, Spin = 2.0f,
                Size = new CelCurve(0.42f, 0.74f, 0.5f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 1f, 3.5f), Feather = 0.08f,
                Palette = "exp_ball_ink", CoolTo = "exp_wrap", CoolFrom = 0.8f, Core = 1f, Ink = 1.6f, Extra = new Vector4(1.2f, 0.12f, 0.3f, 0.7f),
            },
            new CelLayer
            {
                Name = "first burst", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.8f, 0f, 0f),
                Start = 0f, Duration = 0.06f, Life = 0.5f, Close = 10, Mid = 5, Far = 0, Aim = CelAim.Radial, Speed = 3f, Drag = 3f, Buoyancy = 0.5f, Spin = 2f,
                Size = new CelCurve(0.42f, 0.78f, 0.4f), Erode = new CelCurve(0f, 1f, 3.5f), Feather = 0.08f,
                Palette = "exp_ball_ink", CoolTo = "exp_wrap", CoolFrom = 0.6f, Core = 1f, Ink = 0.6f, Extra = new Vector4(1.2f, 0.12f, 0.3f, 0.6f),
            },
            new CelLayer
            {
                // grey-tan puffs in a slab just behind the fire: the fireball hides the middle ones, so they show round its
                // outline from ~f40 and grow until the fire sinks into them (f96-f105)
                Name = "wrap", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.05f, 0.75f), Shape = CelShape.Box, ShapeSize = new Vector3(1.5f, 1.2f, 0.4f),
                Start = 2.3f, Duration = 0.5f, Life = 1.4f, LifeJitter = 0.1f, Close = 16, Mid = 6, Far = 0, Aim = CelAim.Radial, Speed = 0.5f, Drag = 1.5f, Buoyancy = 0.12f, Spin = 0.5f,
                Size = new CelCurve(0.32f, 0.64f, 0.4f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 10f), Feather = 0.1f,
                Palette = "exp_wrap", Core = 1f, Ink = 1.0f, Extra = new Vector4(1.3f, 0.16f, 0.6f, 0f),
            },
            new CelLayer
            {
                Name = "wrap late", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.3f, 0.6f), Shape = CelShape.Box, ShapeSize = new Vector3(2.4f, 1.5f, 0.5f),
                Start = 2.75f, Duration = 0.4f, Life = 1.5f, LifeJitter = 0.1f, Close = 20, Mid = 8, Far = 0, Aim = CelAim.Radial, Speed = 0.6f, Drag = 1.5f, Buoyancy = 0.12f, Spin = 0.5f,
                Size = new CelCurve(0.4f, 0.8f, 0.4f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 10f), Feather = 0.1f,
                Palette = "exp_wrap", Core = 1f, Ink = 1.0f, Extra = new Vector4(1.3f, 0.16f, 0.6f, 0f),
            },
            new CelLayer
            {
                // r04: grey-tan puffs on a shell round the fire, the front ones over its rim (the star is drawn over them)
                Name = "rim puffs", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.15f, -0.15f), Shape = CelShape.Shell, ShapeSize = new Vector3(1.0f, 0f, 0f),
                Start = 1.0f, Duration = 1.8f, Life = 0.8f, LifeJitter = 0.2f, Close = 14, Mid = 5, Far = 0, Aim = CelAim.Radial, Speed = 0.7f, Drag = 1.6f, Buoyancy = 0.15f, Spin = 0.5f,
                Size = new CelCurve(0.16f, 0.32f, 0.4f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 6f), Feather = 0.1f,
                Palette = "exp_wrap", Core = 1f, Ink = 0.9f, Extra = new Vector4(1.3f, 0.16f, 0.6f, 0f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f),
                Start = 0f, Duration = 2.8f, Life = 0.4f, Close = 60, Mid = 20, Far = 0, Aim = CelAim.Radial, Speed = 20f, SpeedJitter = 0.5f, UpBias = 0.7f,
                Gravity = 4f, Drag = 1.5f, Size = new CelCurve(0.06f, 0.03f), Stretch = 0.09f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_spark", Core = 5f, Extra = new Vector4(0f, 0.3f, 0.5f, 0f),
            },
            new CelLayer
            {
                // white lightning-like arcs out of the fireball: 4 px wide, three kinks, up to ~1.5x its radius long
                Name = "arcs", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.35f, -1.0f), Shape = CelShape.Shell, ShapeSize = new Vector3(1.2f, 0f, 0f),
                Start = 0.05f, Duration = 2.9f, Life = 0.2f, LifeJitter = 0.3f, Close = 60, Mid = 0, Far = 0, Aim = CelAim.Radial, Speed = 0.6f, Drag = 4f, UpBias = 0.35f,
                Size = new CelCurve(0.1f, 0.06f), SizeJitter = 0.25f, Stretch = 2.0f, Erode = new CelCurve(0.03f, 0.3f),
                Palette = "exp_arc", Core = 4f, Extra = new Vector4(ExpFxKind.Arc, 0f, 0.22f, 0f),
            },
            new CelLayer
            {
                // orange spark dots on both sides through the late part of the hold
                Name = "dots", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0f, 0f),
                Start = 0.9f, Duration = 2.4f, Life = 0.9f, LifeJitter = 0.3f, Close = 130, Mid = 20, Far = 0, Aim = CelAim.Radial, Speed = 12f, SpeedJitter = 0.6f, UpBias = 0.45f,
                Gravity = 6f, Drag = 1.2f, Size = new CelCurve(0.075f, 0.04f), Stretch = 0.03f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 3f, Extra = new Vector4(1f, 0f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "debris", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.4f, 0f, 0f),
                Start = 0.05f, Duration = 2.7f, Life = 1.0f, Close = 48, Mid = 14, Far = 0, Aim = CelAim.Radial, Speed = 6f, SpeedJitter = 0.6f, UpBias = 0.35f,
                Gravity = 9.8f, Drag = 0.6f, Spin = 4f, Size = new CelCurve(0.1f, 0.07f), SizeJitter = 0.5f, Stretch = 0.7f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_chunk", Core = 1f, Ink = 1.5f, Extra = new Vector4(2.5f, 0.35f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "closing burst", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.9f, 0f, 0f),
                Start = 2.65f, Duration = 0.1f, Life = 0.6f, Close = 10, Mid = 4, Far = 0, Aim = CelAim.Radial, Speed = 2.4f, Drag = 2.5f, Buoyancy = 0.2f, Spin = 2f,
                Size = new CelCurve(0.45f, 0.8f, 0.4f), Erode = new CelCurve(0f, 1f, 3.5f), Feather = 0.08f,
                Palette = "exp_ball_ink", CoolTo = "exp_wrap", CoolFrom = 0.35f, Core = 1f, Ink = 0.6f, Extra = new Vector4(1.2f, 0.12f, 0.3f, 0.35f),
            },
            new CelLayer
            {
                Name = "closing star", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.5f, -1.8f), Start = 2.65f, Duration = 0.35f, Life = 0.3f,
                Close = 3, Mid = 1, Far = 0, Size = new CelCurve(1.55f, 1.75f, 0.5f), Erode = new CelCurve(0f, 0.7f, 2f),
                Palette = "exp_star", Core = 1.6f, Opacity = 0.85f, Extra = new Vector4(ExpFxKind.Star, 0f, 0f, 11f),
            },
            new CelLayer
            {
                Name = "closing sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.4f, 0f, 0f),
                Start = 2.65f, Duration = 0.12f, Life = 0.5f, Close = 45, Mid = 20, Far = 0, Aim = CelAim.Radial, Speed = 22f, SpeedJitter = 0.5f, UpBias = 0.4f,
                Gravity = 5f, Drag = 1.2f, Size = new CelCurve(0.07f, 0.035f), Stretch = 0.1f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_spark", Core = 5f, Extra = new Vector4(0f, 0.3f, 0.4f, 0f),
            },
            new CelLayer
            {
                Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = 0f, Life = 3.0f,
                Close = 1, Mid = 1, Far = 0, Palette = "exp_fire", Core = 3.2f, Extra = new Vector4(13f, 0f, 0f, 0f),
            });

        /// <summary>Two tracers that arrive before the fire (the clips' first round is in the air at 0.6 s).</summary>
        public static readonly CelFxRecipe Lead = new CelFxRecipe("explosion_lead", 0.4f,
            new CelLayer
            {
                Name = "lead tracers", Kind = CelKinds.StreakName, Anchor = CelAnchor.Origin, Shape = CelShape.Box, ShapeSize = new Vector3(0f, 0.5f, 0.8f),
                Start = 0f, Duration = 0.18f, Life = 0.22f, Close = 2, Mid = 1, Far = 0,
                Aim = CelAim.Target, Speed = 0f, Size = new CelCurve(0.16f, 0.12f), Stretch = 0.18f,
                Erode = new CelCurve(0.05f, 0.25f), Palette = "exp_tracer", Core = 5f, Extra = new Vector4(0.1f, 1f, 0f, 0f),
            });

        public static readonly CelFxRecipe Site = new CelFxRecipe("explosion_site", 1.2f,
            new CelLayer
            {
                // a soft dome of white-yellow light, no wider than about twice the dummy's height, gone in 8 frames
                Name = "dome", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.25f, -2f), Life = 0.34f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(1.8f, 2.0f, 0.4f), SizeJitter = 0.08f, Erode = new CelCurve(0f, 1f, 1.3f),
                Palette = "exp_dome", Core = 1.6f, Opacity = 2.4f, Extra = new Vector4(ExpFxKind.Flash, 0.6f, 0.25f, 0f),
            },
            new CelLayer
            {
                Name = "pool", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.02f, 0f), Life = 0.9f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(2.2f, 3f, 0.4f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 2f),
                Palette = "exp_pool", Core = 1f, Extra = new Vector4(ExpFxKind.Pool, 1.7f, 0f, 0f),
            },
            new CelLayer
            {
                // r03: no fireball at a side site; after the dome, an orange spark plume thrown up out of the ground
                Name = "plume", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.2f, 0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.4f, 0f, 0f),
                Start = 0.05f, Duration = 0.45f, Life = 0.8f, LifeJitter = 0.3f, Close = 44, Mid = 10, Far = 0, Aim = CelAim.Up, Spread = 50f, Speed = 12f, SpeedJitter = 0.5f,
                Gravity = 9f, Drag = 0.8f, Size = new CelCurve(0.12f, 0.06f), SizeJitter = 0.3f, Stretch = 0.08f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 3f, Extra = new Vector4(0f, 0.3f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.5f, 0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f),
                Duration = 0.2f, Life = 0.45f, Close = 40, Mid = 12, Far = 0, Aim = CelAim.Radial, Speed = 18f, SpeedJitter = 0.5f, UpBias = 0.9f,
                Gravity = 6f, Drag = 1.2f, Size = new CelCurve(0.05f, 0.03f), Stretch = 0.06f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 4f, Extra = new Vector4(0f, 0.3f, 0.4f, 0f),
            },
            new CelLayer
            {
                Name = "debris", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.4f, 0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.4f, 0f, 0f),
                Start = 0.04f, Duration = 0.3f, Life = 1.3f, LifeJitter = 0.2f, Close = 26, Mid = 8, Far = 0, Aim = CelAim.Radial, Speed = 7f, SpeedJitter = 0.6f, UpBias = 0.8f,
                Gravity = 9.8f, Drag = 0.5f, Spin = 4f, Size = new CelCurve(0.22f, 0.15f), SizeJitter = 0.4f, Stretch = 0.7f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_chunk", Core = 1f, Ink = 1.5f, Extra = new Vector4(2.5f, 0.35f, 0f, 0f),
            },
            new CelLayer
            {
                // r05: big chunks thrown high, a cluster against the fog (ref f59-f87)
                Name = "big chunks", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.6f, 0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.8f, 0f, 0f),
                Start = 0.06f, Duration = 0.5f, Life = 1.4f, LifeJitter = 0.25f, Close = 14, Mid = 4, Far = 0, Aim = CelAim.Radial, Speed = 8f, SpeedJitter = 0.5f, UpBias = 0.85f,
                Gravity = 9.8f, Drag = 0.6f, Spin = 3f, Size = new CelCurve(0.2f, 0.14f), SizeJitter = 0.4f, Stretch = 0.6f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_chunk", Core = 1f, Ink = 1.2f, Extra = new Vector4(2.5f, 0.35f, 0f, 0f),
            },
            new CelLayer
            {
                // r05: grey puffs standing round the site after its dome
                Name = "puffs", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.7f, 0f), Shape = CelShape.Box, ShapeSize = new Vector3(2.0f, 0.8f, 0.8f),
                Start = 0.15f, Duration = 0.4f, Life = 1.4f, LifeJitter = 0.2f, Close = 6, Mid = 2, Far = 0, Aim = CelAim.Radial, Speed = 0.8f, Drag = 1.2f, Buoyancy = 0.15f, Spin = 0.4f,
                Size = new CelCurve(0.28f, 0.5f, 0.4f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 8f), Feather = 0.1f,
                Palette = "exp_smoke", Core = 1f, Ink = 1.0f, Extra = new Vector4(1.3f, 0.16f, 0.6f, 0.0f),
            });

        // r04: a side blast far across the field (ref f77): the same pieces with a bigger, brighter dome
        public static readonly CelFxRecipe SiteFar = new CelFxRecipe("explosion_site_far", 1.2f,
            new CelLayer
            {
                // a soft dome of white-yellow light, no wider than about twice the dummy's height, gone in 8 frames
                Name = "dome", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.3f, -2f), Life = 0.34f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(3.0f, 3.4f, 0.4f), SizeJitter = 0.08f, Erode = new CelCurve(0f, 1f, 1.3f),
                Palette = "exp_dome", Core = 1.6f, Opacity = 3.0f, Extra = new Vector4(ExpFxKind.Flash, 0.55f, 0.3f, 0.3f),
            },
            new CelLayer
            {
                Name = "pool", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.02f, 0f), Life = 0.9f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(2.2f, 3f, 0.4f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 2f),
                Palette = "exp_pool", Core = 1f, Extra = new Vector4(ExpFxKind.Pool, 1.7f, 0f, 0f),
            },
            new CelLayer
            {
                // r03: no fireball at a side site; after the dome, an orange spark plume thrown up out of the ground
                Name = "plume", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.2f, 0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.4f, 0f, 0f),
                Start = 0.05f, Duration = 0.45f, Life = 0.8f, LifeJitter = 0.3f, Close = 44, Mid = 10, Far = 0, Aim = CelAim.Up, Spread = 50f, Speed = 12f, SpeedJitter = 0.5f,
                Gravity = 9f, Drag = 0.8f, Size = new CelCurve(0.12f, 0.06f), SizeJitter = 0.3f, Stretch = 0.08f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 3f, Extra = new Vector4(0f, 0.3f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.5f, 0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f),
                Duration = 0.2f, Life = 0.45f, Close = 40, Mid = 12, Far = 0, Aim = CelAim.Radial, Speed = 18f, SpeedJitter = 0.5f, UpBias = 0.9f,
                Gravity = 6f, Drag = 1.2f, Size = new CelCurve(0.05f, 0.03f), Stretch = 0.06f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 4f, Extra = new Vector4(0f, 0.3f, 0.4f, 0f),
            },
            new CelLayer
            {
                Name = "debris", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.4f, 0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.4f, 0f, 0f),
                Start = 0.04f, Duration = 0.3f, Life = 1.3f, LifeJitter = 0.2f, Close = 34, Mid = 8, Far = 0, Aim = CelAim.Radial, Speed = 7f, SpeedJitter = 0.6f, UpBias = 0.8f,
                Gravity = 9.8f, Drag = 0.5f, Spin = 4f, Size = new CelCurve(0.22f, 0.15f), SizeJitter = 0.4f, Stretch = 0.7f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_chunk", Core = 1f, Ink = 1.5f, Extra = new Vector4(2.5f, 0.35f, 0f, 0f),
            },
            new CelLayer
            {
                // r05: big chunks thrown high, so the cluster reads at ~40 m against the fog (ref f87 left and right)
                Name = "big chunks", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.6f, 0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(1.2f, 0f, 0f),
                Start = 0.06f, Duration = 0.5f, Life = 1.4f, LifeJitter = 0.25f, Close = 18, Mid = 5, Far = 0, Aim = CelAim.Radial, Speed = 9f, SpeedJitter = 0.5f, UpBias = 0.85f,
                Gravity = 9.8f, Drag = 0.6f, Spin = 3f, Size = new CelCurve(0.32f, 0.22f), SizeJitter = 0.4f, Stretch = 0.6f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_chunk", Core = 1f, Ink = 1.2f, Extra = new Vector4(2.5f, 0.35f, 0f, 0f),
            },
            new CelLayer
            {
                // r05: grey puffs standing round the site after its dome
                Name = "puffs", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 1.0f, 0f), Shape = CelShape.Box, ShapeSize = new Vector3(4.5f, 1.4f, 1.0f),
                Start = 0.15f, Duration = 0.4f, Life = 1.4f, LifeJitter = 0.2f, Close = 8, Mid = 3, Far = 0, Aim = CelAim.Radial, Speed = 0.8f, Drag = 1.2f, Buoyancy = 0.15f, Spin = 0.4f,
                Size = new CelCurve(0.8f, 1.4f, 0.4f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 8f), Feather = 0.1f,
                Palette = "exp_smoke", Core = 1f, Ink = 1.0f, Extra = new Vector4(1.3f, 0.16f, 0.6f, 0.0f),
            });

        public static readonly CelFxRecipe Cloud = new CelFxRecipe("explosion_cloud", 4.6f,
            new CelLayer
            {
                Name = "smoke", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Shape = CelShape.Box, ShapeSize = new Vector3(2.4f, 1.5f, 0.9f),
                Duration = 0.35f, Life = 1.55f, LifeJitter = 0.05f, Close = 36, Mid = 12, Far = 0, Aim = CelAim.Radial, Speed = 0.5f, Drag = 1.2f, Buoyancy = 0.1f, Spin = 0.4f,
                Size = new CelCurve(0.34f, 0.68f, 0.35f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 9f), Feather = 0.1f,
                Palette = "exp_smoke", Core = 1f, Ink = 1.1f, Extra = new Vector4(1.3f, 0.16f, 0.8f, 0f),
            },
            new CelLayer
            {
                // r03: a few small debris specks in the cloud, not black holes
                Name = "dirt", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.2f, -0.8f), Shape = CelShape.Box, ShapeSize = new Vector3(2.6f, 1.0f, 0.3f),
                Duration = 0.3f, Life = 1.6f, LifeJitter = 0.1f, Close = 4, Mid = 1, Far = 0, Aim = CelAim.Radial, Speed = 0.3f, Drag = 1.5f, Spin = 0.5f,
                Size = new CelCurve(0.07f, 0.11f, 0.4f), SizeJitter = 0.35f, Erode = new CelCurve(0f, 1f, 6f), Feather = 0.1f,
                Palette = "exp_dirt", Core = 1f, Ink = 0.8f, Extra = new Vector4(1.8f, 0.25f, 0f, 0f),
            },
            new CelLayer
            {
                // orange embers drifting up through and past the cloud, to the clip's f152 and after
                Name = "embers", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.5f, -0.8f), Shape = CelShape.Box, ShapeSize = new Vector3(5.5f, 1.8f, 2f),
                Start = 0.1f, Duration = 2.1f, Life = 0.85f, LifeJitter = 0.3f, Close = 34, Mid = 10, Far = 0, Aim = CelAim.Up, Spread = 45f, Speed = 0.7f, SpeedJitter = 0.4f, Buoyancy = 0.35f,
                Size = new CelCurve(0.1f, 0.07f), SizeJitter = 0.3f, Stretch = 0.05f, Erode = new CelCurve(0.05f, 0.5f, 2f),
                Palette = "exp_ember", Core = 3.5f, Extra = new Vector4(1f, 0.04f, 0.3f, 0f),
            });

        public static readonly CelFxRecipe Fan = new CelFxRecipe("explosion_fan", 1.6f,
            new CelLayer
            {
                // thick orange streaks thrown low to the lower left, kept coming through the clips' peak window
                Name = "ground fan", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.45f, 0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.35f, 0f, 0f),
                Duration = 0.8f, Life = 0.55f, LifeJitter = 0.2f, Close = 64, Mid = 18, Far = 0, Aim = CelAim.Dir, Spread = 20f, Speed = 26f, SpeedJitter = 0.4f,
                Gravity = 3f, Drag = 1f, Size = new CelCurve(0.15f, 0.07f), SizeJitter = 0.35f, Stretch = 0.16f, Erode = new CelCurve(0.05f, 0.45f),
                Palette = "exp_tracer", Core = 2.5f, Extra = new Vector4(0.2f, 0.6f, 0.2f, 0f),
            },
            new CelLayer
            {
                Name = "fan dots", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.3f, 0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0f, 0f),
                Duration = 0.9f, Life = 0.75f, LifeJitter = 0.3f, Close = 80, Mid = 20, Far = 0, Aim = CelAim.Dir, Spread = 55f, Speed = 11f, SpeedJitter = 0.7f,
                Gravity = 6f, Drag = 1.2f, Size = new CelCurve(0.06f, 0.035f), Stretch = 0.015f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 3f, Extra = new Vector4(1f, 0f, 0.3f, 0f),
            });

        /// <summary>Play a recipe `delay` seconds from now (its records are born on the pinned clock, then the clock is
        /// handed back). No allocation.</summary>
        public static void PlayAt(CelFxRecipe r, Vector3 origin, Vector3 dir, Vector3 target, uint seed, float delay)
        {
            float prev = CelFx.Clock;
            CelFx.Clock = CelFx.Now + delay;
            try { CelFx.Play(r, origin, dir, target, seed); }
            finally { CelFx.Clock = prev; }
        }

        /// <summary>One shell in the game: a ground blast at `ground` and its smoke cloud. `dir` = the eye's flat forward.</summary>
        public static void PlayShell(Vector3 ground, Vector3 dir, uint seed)
        {
            CelFx.Play(Site, ground, dir, ground, seed);
            PlayAt(Cloud, ground, dir, ground + Vector3.up * 1.1f, seed + 17u, 0.3f);
        }
    }
}
