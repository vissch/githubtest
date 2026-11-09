// Phase: CelFx job (2026-11-09, lane/show/cel-explosion) - the cel shell explosion (reference clip
// explosive_blasts_nolora): white-yellow inked fireball lobes with orange edges, white spark streaks, dark debris, a
// warm glow pool on the ground, secondary ground blasts (domes of white light), tan cel smoke clouds with ink and dark
// dirt in them, and orange embers drifting after. Built only from the kit's Streak, Lobe and Glow kinds (plus a light),
// plus its own ExpFx kind (ExplosionKind.cs: soft pools and flashes, jagged spark arcs) in place of the kit's Glow: at
// most four draws (Streak, Lobe x2, ExpFx) however many blasts play.
// r02: a white HDR heart and a soft bloom halo over orange-yellow lobes with thin red-orange ink; soft (unbanded) pools
// and domes; white jagged arcs; a thick orange late fan with orange spark dots; tracers lead the fire.
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
            CelPalette.Add("exp_core", CelPalette.Hex(0xFF9A30), CelPalette.Hex(0xFF9A30), CelPalette.Hex(0xFFB850), CelPalette.Hex(0xFFC860), CelPalette.Hex(0xFFFFFF), 0.55f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("exp_bloom", CelPalette.Hex(0x401808), CelPalette.Hex(0x602808), CelPalette.Hex(0x904010), CelPalette.Hex(0xC06424), CelPalette.Hex(0xFFB060), 0.85f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("exp_arc", CelPalette.Hex(0xB8C8FF), CelPalette.Hex(0xD8E2FF), CelPalette.Hex(0xF0F4FF), CelPalette.Hex(0xFFFFFF), CelPalette.Hex(0xFFFFFF), 0.45f, 3, CelPalette.Hex(0x000000));
            CelPalette.Add("exp_spark", CelPalette.Hex(0xE08030), CelPalette.Hex(0xFFC070), CelPalette.Hex(0xFFE8B0), CelPalette.Hex(0xFFFFFF), CelPalette.Hex(0xFFFFFF), 0.6f, 3, CelPalette.Hex(0x401004));
            CelPalette.Add("exp_tracer", CelPalette.Hex(0xC04010), CelPalette.Hex(0xF07A20), CelPalette.Hex(0xFFB040), CelPalette.Hex(0xFFE090), CelPalette.Hex(0xFFF4D0), 0.75f, 3, CelPalette.Hex(0x401004));
            // the pool is additive: these are what it adds to the dark plate (#282c37 -> #b77650 at the middle)
            CelPalette.Add("exp_pool", CelPalette.Hex(0x2A1A08), CelPalette.Hex(0x6A3C14), CelPalette.Hex(0x9A6A30), CelPalette.Hex(0xD09048), CelPalette.Hex(0xFFD890), 0.7f, 4, CelPalette.Hex(0x000000));
            // the warm light the blasts throw over the whole field (the clips' ground turns brown from 0.9 s to 4 s)
            CelPalette.Add("exp_wash", CelPalette.Hex(0x201008), CelPalette.Hex(0x301808), CelPalette.Hex(0x704020), CelPalette.Hex(0xA06030), CelPalette.Hex(0xA06030), 1.1f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("exp_smoke", CelPalette.Hex(0x6E5E52), CelPalette.Hex(0x9A8878), CelPalette.Hex(0xBAA894), CelPalette.Hex(0xDAC8B0), CelPalette.Hex(0xDAC8B0), 1.1f, 3, CelPalette.Hex(0x3A2C26));
            CelPalette.Add("exp_dirt", CelPalette.Hex(0x1E1714), CelPalette.Hex(0x2E2420), CelPalette.Hex(0x44362E), CelPalette.Hex(0x5A463A), CelPalette.Hex(0x5A463A), 1.1f, 3, CelPalette.Hex(0x120C0A));
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
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(1.5f, 1.7f, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 1.5f),
                Palette = "exp_dome", Core = 1.6f, Extra = new Vector4(ExpFxKind.Flash, 0.7f, 0f, 0f),
            },
            new CelLayer
            {
                // the blown white heart, in front of the lobes (toward the eye), re-born all through the hold
                Name = "heart", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.55f, -1.6f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.12f, 0f, 0f),
                Start = 0.05f, Duration = 2.75f, Life = 0.34f, LifeJitter = 0.2f,
                Close = 9, Mid = 5, Far = 0, Size = new CelCurve(0.7f, 0.85f, 0.5f), SizeJitter = 0.12f, Erode = new CelCurve(0f, 1f, 2f),
                Palette = "exp_core", Core = 2.2f, Opacity = 1f, Extra = new Vector4(ExpFxKind.Flash, 0.55f, 0f, 0f),
            },
            new CelLayer
            {
                // a soft warm bloom about 1.5x the fireball's radius
                Name = "bloom", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.55f, -1.8f), Start = 0f, Duration = 2.7f, Life = 0.6f, LifeJitter = 0.1f,
                Close = 6, Mid = 3, Far = 0, Size = new CelCurve(2.0f, 2.3f, 0.5f), SizeJitter = 0.08f, Erode = new CelCurve(0f, 1f, 2f),
                Palette = "exp_bloom", Core = 1f, Opacity = 0.7f, Extra = new Vector4(ExpFxKind.Flash, 2.0f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "pool", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.93f, 0f), Start = 0f, Life = 3.0f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(4f, 5f, 0.3f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 4f),
                Palette = "exp_pool", Core = 1f, Opacity = 1.2f, Extra = new Vector4(ExpFxKind.Pool, 1.2f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "wash", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.93f, -2f), Start = 0f, Life = 3.3f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(7f, 13f, 0.7f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 5f),
                Palette = "exp_wash", Core = 1f, Opacity = 1.4f, Extra = new Vector4(ExpFxKind.Pool, 0.7f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "fireball", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.1f, 0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.55f, 0f, 0f),
                Start = 0f, Duration = 2.8f, Life = 0.5f, LifeJitter = 0.3f, Close = 110, Mid = 34, Far = 0,
                Aim = CelAim.Radial, Speed = 1.8f, Drag = 2.5f, Buoyancy = 0.6f, Spin = 2.5f,
                Size = new CelCurve(0.34f, 0.64f, 0.5f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.08f,
                Palette = "exp_fire", CoolTo = "exp_smoke", CoolFrom = 0.6f, Core = 4f, Ink = 0.8f, Extra = new Vector4(1.4f, 0.18f, 0.3f, 0.6f),
            },
            new CelLayer
            {
                Name = "first burst", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.7f, 0f, 0f),
                Start = 0f, Duration = 0.06f, Life = 0.5f, Close = 10, Mid = 5, Far = 0, Aim = CelAim.Radial, Speed = 2.5f, Drag = 3f, Buoyancy = 0.5f, Spin = 2f,
                Size = new CelCurve(0.4f, 0.75f, 0.4f), Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.08f,
                Palette = "exp_fire", CoolTo = "exp_smoke", CoolFrom = 0.6f, Core = 5f, Ink = 0.8f, Extra = new Vector4(1.4f, 0.18f, 0.3f, 0.6f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f),
                Start = 0f, Duration = 2.8f, Life = 0.32f, Close = 50, Mid = 20, Far = 0, Aim = CelAim.Radial, Speed = 16f, SpeedJitter = 0.5f, UpBias = 0.7f,
                Gravity = 4f, Drag = 1.5f, Size = new CelCurve(0.06f, 0.03f), Stretch = 0.09f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_spark", Core = 5f, Extra = new Vector4(0f, 0.3f, 0.5f, 0f),
            },
            new CelLayer
            {
                // white lightning-like arcs out of the fireball: 4 px wide, three kinks, up to ~1.5x its radius long
                Name = "arcs", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.3f, -1.0f), Shape = CelShape.Shell, ShapeSize = new Vector3(0.9f, 0f, 0f),
                Start = 0.05f, Duration = 2.9f, Life = 0.2f, LifeJitter = 0.3f, Close = 60, Mid = 0, Far = 0, Aim = CelAim.Radial, Speed = 0.6f, Drag = 4f, UpBias = 0.35f,
                Size = new CelCurve(0.1f, 0.06f), SizeJitter = 0.25f, Stretch = 2.0f, Erode = new CelCurve(0.03f, 0.3f),
                Palette = "exp_arc", Core = 4f, Extra = new Vector4(ExpFxKind.Arc, 0f, 0.22f, 0f),
            },
            new CelLayer
            {
                // orange spark dots on both sides through the late part of the hold
                Name = "dots", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0f, 0f),
                Start = 1.4f, Duration = 1.9f, Life = 0.8f, LifeJitter = 0.3f, Close = 70, Mid = 20, Far = 0, Aim = CelAim.Radial, Speed = 8f, SpeedJitter = 0.6f, UpBias = 0.5f,
                Gravity = 6f, Drag = 1.2f, Size = new CelCurve(0.06f, 0.035f), Stretch = 0.015f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 3f, Extra = new Vector4(1f, 0f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "debris", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.4f, 0f, 0f),
                Start = 0.05f, Duration = 2.7f, Life = 1.0f, Close = 40, Mid = 12, Far = 0, Aim = CelAim.Radial, Speed = 8f, SpeedJitter = 0.5f, UpBias = 0.6f,
                Gravity = 9.8f, Drag = 0.4f, Spin = 4f, Size = new CelCurve(0.1f, 0.07f), Stretch = 0.7f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_dirt", Core = 1f, Ink = 1.5f, Extra = new Vector4(2.5f, 0.35f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "closing burst", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.0f, 0f, 0f),
                Start = 2.65f, Duration = 0.1f, Life = 0.6f, Close = 14, Mid = 6, Far = 0, Aim = CelAim.Radial, Speed = 3f, Drag = 2.5f, Buoyancy = 0.6f, Spin = 2f,
                Size = new CelCurve(0.45f, 0.85f, 0.4f), Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.08f,
                Palette = "exp_fire", CoolTo = "exp_smoke", CoolFrom = 0.5f, Core = 5f, Ink = 0.8f, Extra = new Vector4(1.4f, 0.18f, 0.3f, 0.5f),
            },
            new CelLayer
            {
                Name = "closing heart", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.55f, -1.8f), Start = 2.65f, Duration = 0.4f, Life = 0.3f,
                Close = 3, Mid = 2, Far = 0, Size = new CelCurve(0.9f, 1.05f, 0.5f), Erode = new CelCurve(0f, 1f, 2f),
                Palette = "exp_core", Core = 2.2f, Extra = new Vector4(ExpFxKind.Flash, 0.55f, 0f, 0f),
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
                Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = 0f, Life = 2.9f,
                Close = 1, Mid = 1, Far = 0, Palette = "exp_fire", Core = 6f, Extra = new Vector4(24f, 0f, 0f, 0f),
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
                Name = "dome", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.55f, -2f), Life = 0.34f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(1.4f, 1.65f, 0.4f), SizeJitter = 0.08f, Erode = new CelCurve(0f, 1f, 1.3f),
                Palette = "exp_dome", Core = 1.6f, Extra = new Vector4(ExpFxKind.Flash, 0.7f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "pool", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.02f, 0f), Life = 0.9f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(2.2f, 3f, 0.4f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 2f),
                Palette = "exp_pool", Core = 1f, Extra = new Vector4(ExpFxKind.Pool, 1.7f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "fire", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.7f, 0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.7f, 0f, 0f),
                Duration = 0.15f, Life = 0.6f, Close = 7, Mid = 3, Far = 0, Aim = CelAim.Radial, Speed = 2.5f, Drag = 2.5f, Buoyancy = 1f, Spin = 2f,
                Size = new CelCurve(0.32f, 0.6f, 0.4f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.08f,
                Palette = "exp_fire", CoolTo = "exp_smoke", CoolFrom = 0.5f, Core = 4f, Ink = 0.8f, Extra = new Vector4(1.4f, 0.18f, 0.3f, 0.5f),
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
                Duration = 0.1f, Life = 1.1f, Close = 12, Mid = 4, Far = 0, Aim = CelAim.Radial, Speed = 7f, SpeedJitter = 0.5f, UpBias = 0.8f,
                Gravity = 9.8f, Drag = 0.4f, Spin = 4f, Size = new CelCurve(0.12f, 0.08f), Stretch = 0.7f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_dirt", Core = 1f, Ink = 1.5f, Extra = new Vector4(2.5f, 0.35f, 0f, 0f),
            });

        public static readonly CelFxRecipe Cloud = new CelFxRecipe("explosion_cloud", 2.2f,
            new CelLayer
            {
                Name = "smoke", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Shape = CelShape.Box, ShapeSize = new Vector3(3.0f, 1.2f, 1.0f),
                Duration = 0.35f, Life = 2.1f, LifeJitter = 0.05f, Close = 34, Mid = 12, Far = 0, Aim = CelAim.Radial, Speed = 0.5f, Drag = 1.2f, Buoyancy = 0.1f, Spin = 0.4f,
                Size = new CelCurve(0.3f, 0.62f, 0.35f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 9f), Feather = 0.1f,
                Palette = "exp_smoke", Core = 1f, Ink = 2.5f, Extra = new Vector4(1.3f, 0.16f, 0.8f, 0f),
            },
            new CelLayer
            {
                Name = "dirt", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.7f), Shape = CelShape.Box, ShapeSize = new Vector3(3.4f, 1.1f, 0.4f),
                Duration = 0.3f, Life = 2.0f, LifeJitter = 0.05f, Close = 6, Mid = 2, Far = 0, Aim = CelAim.Radial, Speed = 0.3f, Drag = 1.5f, Spin = 0.5f,
                Size = new CelCurve(0.15f, 0.28f, 0.4f), SizeJitter = 0.35f, Erode = new CelCurve(0f, 1f, 6f), Feather = 0.1f,
                Palette = "exp_dirt", Core = 1f, Ink = 1f, Extra = new Vector4(1.8f, 0.25f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "embers", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Shape = CelShape.Box, ShapeSize = new Vector3(5f, 2.5f, 2f),
                Start = 0.1f, Duration = 1.6f, Life = 1.6f, Close = 26, Mid = 8, Far = 0, Aim = CelAim.Up, Spread = 40f, Speed = 0.6f, Buoyancy = 0.3f,
                Size = new CelCurve(0.04f, 0.025f), Stretch = 0.05f, Erode = new CelCurve(0.05f, 0.6f, 2f),
                Palette = "ember", Core = 3f, Extra = new Vector4(1f, 0.04f, 0.6f, 0f),
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
