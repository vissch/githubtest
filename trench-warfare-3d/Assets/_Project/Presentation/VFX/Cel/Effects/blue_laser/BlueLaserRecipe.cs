// Phase: CelFx job, blue_laser (2026-11-09, lane/show/cel-blue-laser) - the thin blue laser of the owner's clip
// laser_thin_blue_cel: a held beam (about 3.3 s) on one axis broken into 3-5 pointed cyan dashes with dark gaps,
// each with a white core, ink, fine zig-zag wisps and short branch arcs, re-drawn every 2-3 frames at new places; at
// the hit a white-hot round core in a soft pale-blue bloom with a spiky white star, white and orange sparks; a soft
// bright cyan pool on the ground (no Light layer: SceneHooks.Flash blooms a white ball on the board even at core 0.22);
// the energy builds to a last volley at about 3.4 s (the clip's peak, f101), when a trail of soft tan dust puffs and
// loose black flecks rises up-right along the beam's way past the target and fades by 4.8 s, blue-white sparkles
// left on the board.
// LAB ONLY (owner, 2026-11-09): no weapon fires it; VfxLab, the Playground and the Range call Play. Energy weapon, so
// blue light is allowed (exception to "every light warm").
// Three plays of the kit: the beam (shooter -> target), the ground (level, at the foot) and the dust (along the trail).
// r04: the bloom, pool, puffs and flecks are LightningBeam dots (soft modes in its shader), so the effect draws
// LightningBeam + Glow (star, sparkles) + Streak (sparks) and no Lobe.
// LOD: Close everything; Mid fewer dashes, flashes, sparks and puffs, no branch arcs (the shader drops them) and the
// steady thread; Far the thread and the impact dot only (one LightningBeam draw), both kept a few px wide and lit
// through the fog by the shader.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class BlueLaserRecipe
    {
        // ---------------------------------------------------------------- palettes (picked off the clip's frames)
        /// <summary>The spindles: cyan body (#9fdcec), a thin white core, dark ink; c0 is the halo's light.</summary>
        public static readonly int BeamRow = CelPalette.Add("blue_laser_beam",
            CelPalette.Hex(0x3C7E9E), CelPalette.Hex(0x8ED2E6), CelPalette.Hex(0x9FDCEC), CelPalette.Hex(0xB6E6F2),
            CelPalette.Hex(0xF2FCFF), 0.88f, 3, CelPalette.Hex(0x1A4258));
        /// <summary>The impact: white-blue (star, rings, sparks).</summary>
        public static readonly int HitRow = CelPalette.Add("blue_laser_hit",
            CelPalette.Hex(0x2C6488), CelPalette.Hex(0x5AAED0), CelPalette.Hex(0x9FDCEC), CelPalette.Hex(0xD8F4FF),
            CelPalette.Hex(0xFFFFFF), 0.8f, 3, CelPalette.Hex(0x06121E));
        /// <summary>The ground glow, as it adds onto the dark field (additive: these are the light it adds).</summary>
        public static readonly int GroundRow = CelPalette.Add("blue_laser_ground",
            CelPalette.Hex(0x16304A), CelPalette.Hex(0x224466), CelPalette.Hex(0x2E5A88), CelPalette.Hex(0x3C72AC),
            CelPalette.Hex(0x3C72AC), 1.1f, 4, CelPalette.Hex(0x000000));
        /// <summary>r04: the ground pool, a soft bright cyan glow (additive, unbanded: c2 + c3 x falloff^2 at its middle).</summary>
        public static readonly int PoolRow = CelPalette.Add("blue_laser_pool",
            CelPalette.Hex(0x1C3E62), CelPalette.Hex(0x2C5E92), CelPalette.Hex(0x3C98CC), CelPalette.Hex(0x62BCE4),
            CelPalette.Hex(0x8CC8F4), 0.92f, 3, CelPalette.Hex(0x000000));
        /// <summary>r04: the hit's soft bloom (additive light c2 + c3 x falloff^2) round a white-hot core.</summary>
        public static readonly int BloomRow = CelPalette.Add("blue_laser_bloom",
            CelPalette.Hex(0x1C3E62), CelPalette.Hex(0x2C5E92), CelPalette.Hex(0x5A9ED6), CelPalette.Hex(0x86C2EC),
            CelPalette.Hex(0xF4FCFF), 0.9f, 3, CelPalette.Hex(0x000000));
        /// <summary>The clip's dust is a soft grey-tan; r04 puffs cover (c2 at the edge, c3 inside), no ink, no bands.</summary>
        public static readonly int DustRow = CelPalette.Add("blue_laser_dust",
            // b01: pre-encoded (sRGB of the wanted colour): the puff's cover colour came out linearised twice (tan #B49A72
            // rendered as saturated #74522B-ish orange), so these hexes are the wanted #8A7A64..#B49A72 encoded once more
            CelPalette.Hex(0xC2B8A8), CelPalette.Hex(0xCBC0AB), CelPalette.Hex(0xD1C4AD), CelPalette.Hex(0xDBCCB2),
            CelPalette.Hex(0xDBCCB2), 1.1f, 2, CelPalette.Hex(0x141010));
        /// <summary>r04: the black flecks in the dust (darker than smoke_black, so they read as the clip's ink specks).</summary>
        public static readonly int FleckRow = CelPalette.Add("blue_laser_fleck",
            CelPalette.Hex(0x050404), CelPalette.Hex(0x080606), CelPalette.Hex(0x0A0807), CelPalette.Hex(0x15100D),
            CelPalette.Hex(0x15100D), 1.1f, 2, CelPalette.Hex(0x000000));

        const float Hold = 3.3f;           // seconds of dashes (the last dies about 3.45 s: the clip's beam ends at f105)
        const float Ramp = 0.6f;           // the thread fades in over half of this (the clip's beam builds up over its first 0.6 s)
        const float Volley = 3.05f;        // the last volley: longer dashes at once, the clip's peak (f96-f102)
        // NB: on a LightningBeam layer Ink is the beam's clip (tone.w: 1 stops the window at the target, 1.7 runs on
        // through it), not an ink width; the spindle's ink is fixed in its shader.
        const float DustAt = 2.9f;         // the dust trail (clip f88-f135)

        // r04: the beam is one axis broken into dashes: five lanes along it (heads at these shares of the beam,
        // window lengths picked so each dash is ~100-180 px on screen with a gap about as long), each lane re-drawn
        // every ~0.1 s (2-3 frames) at a jittered place, so 3-5 pointed dashes flicker along the beam at once.
        static CelLayer Dash(string name, float head, float window, float jitter, float start, int close, int mid, float width)
        {
            return new CelLayer
            {
                Name = name, Kind = LightningBeamKind.KindName, Start = start, Duration = Hold - start, TimeJitter = 0.03f, Life = 0.12f, LifeJitter = 0.3f,
                Close = close, Mid = mid, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.03f, 0f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(width, width * 0.75f), SizeJitter = 0.25f, Stretch = window, Erode = new CelCurve(0.06f, 0.3f, 2f), Feather = jitter,
                Palette = "blue_laser_beam", Core = 1.6f, Opacity = 0.95f, Ink = 1f, Extra = new Vector4(0.3f, 4f, head, head + 0.02f),
            };
        }

        static CelLayer Dim(CelLayer l, float opacity) { l.Opacity = opacity; return l; }

        /// <summary>r04: a LightningBeam dot (Stretch is minus its minimum px at Far; 0.001 = none).</summary>
        static CelLayer Dot(string name, string palette, int mode, float start, float duration, float life, int close, int mid, CelCurve size, float soft, float pointsOrLump, float sharpOrCore)
        {
            return new CelLayer
            {
                Name = name, Kind = LightningBeamKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.45f), Start = start, Duration = duration, TimeJitter = 0.02f, Life = life, LifeJitter = 0.25f,
                Close = close, Mid = mid, Far = 0, Shape = CelShape.Point, Aim = CelAim.None, Speed = 0f, Size = size, SizeJitter = 0.2f, Stretch = -0.001f,
                Erode = new CelCurve(0f, 0.5f, 1.5f), Feather = 0f, Palette = palette, Core = 2.0f, Opacity = 1f, Extra = new Vector4(mode, soft, pointsOrLump, sharpOrCore),
            };
        }

        public static readonly CelFxRecipe Beam = new CelFxRecipe("blue_laser_beam", 5f,
            // b01 (beach stage): the frame's lower-left edge is at ~0.27 of the beam (the muzzle is 9 m from the eye, the
            // target 19 m; perspective packs the far half), so the lanes are laid on SCREEN like the clip's 2-3 long dashes:
            // a from off-screen (full at the frame edge) to ~0.38 of the way, b ~0.52-0.78, c ~0.86-1.0, with dark gaps
            Dash("dash a", 0.56f, 0.5f, 0.05f, 0.00f, 32, 10, 0.19f),
            Dash("dash b", 0.88f, 0.19f, 0.025f, 0.05f, 32, 10, 0.16f),
            Dash("dash c", 1.00f, 0.075f, 0.006f, 0.1f, 30, 9, 0.13f),
            new CelLayer
            {
                // the last volley: three longer, wider dashes at the peak (clip f96-f102)
                Name = "volley", Kind = LightningBeamKind.KindName, Start = Volley, Duration = 0.38f, TimeJitter = 0.03f, Life = 0.14f, LifeJitter = 0.1f,
                Close = 4, Mid = 2, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.03f, 0f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(0.17f, 0.12f), SizeJitter = 0.2f, Stretch = 0.4f, Erode = new CelCurve(0.06f, 0.3f, 2f), Feather = 0.1f,
                Palette = "blue_laser_beam", Core = 1.7f, Opacity = 0.95f, Ink = 1f, Extra = new Vector4(0.32f, 4f, 0.74f, 0.8f),
            },
            new CelLayer
            {
                // a dash that carries on past the dummy, up-right along the beam (clip f89-f101)
                Name = "through", Kind = LightningBeamKind.KindName, Start = 1.0f, Duration = Hold - 0.9f, TimeJitter = 0.1f, Life = 0.16f, LifeJitter = 0.2f,
                Close = 14, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.03f, 0f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                Size = new CelCurve(0.08f, 0.06f), SizeJitter = 0.2f, Stretch = 0.2f, Erode = new CelCurve(0.06f, 0.3f), Feather = 0.08f,
                Palette = "blue_laser_beam", Core = 1.6f, Opacity = 0.95f, Ink = 1.7f, Extra = new Vector4(0.28f, 0f, 1.42f, 1.5f),
            },
            new CelLayer
            {
                // hidden at Close by the shader: the line that stands for the dashes at Mid and Far
                Name = "thread", Kind = LightningBeamKind.KindName, Start = Ramp * 0.5f, Life = Hold - Ramp * 0.5f + 0.25f, LifeJitter = 0f,
                // b01: none at Close (the beach shot's CloseUp is 0.91, so the shader's fade let it fill the dash gaps)
                Close = 0, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                Size = new CelCurve(0.06f, 0.05f), SizeJitter = 0f, Stretch = 1f, Erode = new CelCurve(0.05f, 0.1f), Feather = 0f,
                Palette = "blue_laser_beam", Core = 1.3f, Opacity = 0.7f, Ink = 1f, Extra = new Vector4(0.1f, 0f, 1f, 1f),
            },
            new CelLayer
            {
                // the impact dot: the star's white core up close; at Far kept 4 px in radius and lit through the fog
                Name = "dot", Kind = LightningBeamKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.45f), Start = 0.15f, Life = Hold + 0.15f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.None, Speed = 0f,
                Size = new CelCurve(0.04f, 0.05f), SizeJitter = 0f, Stretch = -4f, Erode = new CelCurve(0.05f, 0.12f), Feather = 0f,
                Palette = "blue_laser_hit", Core = 1f, Opacity = 0.85f, Extra = new Vector4(0f, 0f, 4f, 3f),
            },
            // r04: the hit is a white-hot round core about the torso's width in a soft pale-blue bloom (no bands, no
            // bullseye): a steady soft glow under 30 flickering blooms with cores
            // b01: smaller and dimmer (r04's 0.7 m glow + 0.5 m blooms with a 0.2 m core read as a moon over the dummy):
            // a core about half a torso wide; the spiky white burst is the "rays" layer (dot mode 4)
            Dim(Dot("glow", "blue_laser_bloom", 1, 0.15f, 0f, Hold + 0.1f, 1, 1, new CelCurve(0.55f, 0.6f), 3f, 0f, 0f), 0.7f),
            Dim(Dot("bloom", "blue_laser_bloom", 1, 0.15f, Hold, 0.13f, 30, 12, new CelCurve(0.26f, 0.3f, 0.6f), 3.5f, 0f, 0.5f), 0.8f),
            // b01: 6-10 thin white spike rays of uneven length out to 1-1.5 torso widths, re-drawn every 2-3 frames at a new turn
            Dot("rays", "blue_laser_hit", 4, 0.15f, Hold, 0.1f, 34, 10, new CelCurve(0.62f, 0.72f), 0.045f, 8f, 0.16f),
            Dot("afterglow", "blue_laser_bloom", 1, Hold + 0.1f, 0f, 1.2f, 1, 1, new CelCurve(0.22f, 0.16f), 6f, 0f, 0.3f),
            new CelLayer
            {
                Name = "star", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.45f), Start = 0.2f, Duration = Hold - 0.1f, TimeJitter = 0.03f, Life = 0.1f, LifeJitter = 0.2f,
                Close = 26, Mid = 10, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.05f, 0f, 0f), Size = new CelCurve(0.16f, 0.26f, 0.5f), SizeJitter = 0.25f,
                Erode = new CelCurve(0.1f, 0.7f, 1.5f), Spin = 3f, Palette = "blue_laser_hit", Core = 1.5f, Opacity = 0.7f, Extra = new Vector4(2f, 1.6f, 10f, 14f),
            },
            new CelLayer
            {
                // blue-white sparkles left on the board after the beam (clip f105-f125)
                Name = "sparkles", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.05f, -0.4f), Start = Hold + 0.1f, Duration = 1.05f, TimeJitter = 0.03f, Life = 0.16f, LifeJitter = 0.3f,
                Close = 34, Mid = 10, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.2f, 0f, 0f), Size = new CelCurve(0.06f, 0.035f), SizeJitter = 0.4f,
                Erode = new CelCurve(0.05f, 0.6f), Spin = 2f, Palette = "blue_laser_hit", Core = 2f, Opacity = 0.95f, Extra = new Vector4(2f, 1.4f, 4f, 8f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.4f), Start = 0.2f, Duration = Hold - 0.1f, Life = 0.25f,
                Close = 90, Mid = 22, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.08f, 0f, 0f), Aim = CelAim.Radial, Speed = 3.0f, SpeedJitter = 0.5f, UpBias = 0.3f, Gravity = 5f, Drag = 1.5f,
                Size = new CelCurve(0.022f, 0.012f), Stretch = 0.05f, Erode = new CelCurve(0.05f, 0.5f), Palette = "blue_laser_hit", Core = 1.8f, Extra = new Vector4(0.2f, 0.05f, 0.3f, 0f),
            },
            new CelLayer
            {
                // the peak's burst: a ring of long white spark lines (clip f101-f106)
                Name = "burst sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.4f), Start = Volley + 0.15f, Duration = 0.3f, Life = 0.32f, LifeJitter = 0.2f,
                Close = 40, Mid = 12, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.1f, 0f, 0f), Aim = CelAim.Radial, Speed = 6f, SpeedJitter = 0.5f, UpBias = 0.2f, Gravity = 4f, Drag = 1.5f,
                Size = new CelCurve(0.025f, 0.015f), Stretch = 0.07f, Erode = new CelCurve(0.05f, 0.5f), Palette = "blue_laser_hit", Core = 1.8f, Extra = new Vector4(0.2f, 0.05f, 0.3f, 0f),
            },
            new CelLayer
            {
                // r04: orange spark streaks thrown 0.5-1 torso widths out (more, longer and brighter than r03)
                Name = "warm sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.05f, -0.35f), Start = 0.3f, Duration = Hold - 0.2f, Life = 0.3f,
                Close = 100, Mid = 14, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.08f, 0f, 0f), Aim = CelAim.Radial, Speed = 3.2f, SpeedJitter = 0.5f, UpBias = 0.5f, Gravity = 5f, Drag = 1.8f,
                Size = new CelCurve(0.03f, 0.015f), Stretch = 0.12f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 2.2f, Extra = new Vector4(0.2f, 0.05f, 0.2f, 0f),
            });

        public static readonly CelFxRecipe Ground = new CelFxRecipe("blue_laser_ground", 4f,
            new CelLayer
            {
                // r04: a soft bright cyan pool ~1.5 torso widths across (no bands, no hard rim), a LightningBeam dot on the ground
                Name = "pool", Kind = LightningBeamKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0.05f, 0.02f, 0.05f), Start = 0.25f, Life = Hold + 0.1f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Shape = CelShape.Point, Aim = CelAim.None, Speed = 0f, Size = new CelCurve(1.3f, 1.4f), SizeJitter = 0f, Stretch = -0.001f,
                Erode = new CelCurve(0f, 0.1f), Feather = 0f, Palette = "blue_laser_pool", Core = 1f, Opacity = 1.2f, Extra = new Vector4(2f, 2.2f, 0f, 0f),
            });

        /// <summary>The dust trail, played along the trail (see Play): four stretches born near to far, then the flecks.</summary>
        public static readonly CelFxRecipe Dust = new CelFxRecipe("blue_laser_dust", 3f,
            // r04: 20 soft unlined tan puffs (LightningBeam puff dots that cover, each under a third of the dummy's height
            // on screen) rising up-right along the trail from mid-dummy to ~3 dummy heights past it, then thinning out
            // b01: four short gappy clumps (r04's long stretches merged into one column), each puff at most ~1/3 dummy height
            DustStretch("dust near", 0.45f, 0.8f, DustAt, 8, 2, new CelCurve(0.32f, 0.4f, 0.5f), 1.75f),
            DustStretch("dust mid", 2.4f, 0.9f, DustAt + 0.08f, 8, 2, new CelCurve(0.34f, 0.42f, 0.5f), 1.7f),
            DustStretch("dust mid2", 4.5f, 0.9f, DustAt + 0.16f, 6, 1, new CelCurve(0.36f, 0.44f, 0.5f), 1.6f),
            DustStretch("dust far", 6.6f, 0.8f, DustAt + 0.24f, 5, 0, new CelCurve(0.37f, 0.45f, 0.5f), 1.5f),
            new CelLayer
            {
                // loose separate black flecks: small hard-edged puff dots spread wider than the dust, drifting with it
                Name = "flecks", Kind = LightningBeamKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.25f, 3.8f), Start = DustAt - 0.05f, Duration = 0.6f, Life = 1.5f, LifeJitter = 0.35f,
                Close = 130, Mid = 36, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(1.6f, 1.1f, 8f), Aim = CelAim.Dir, Speed = 1.0f, SpeedJitter = 0.5f, Spread = 35f, Drag = 0.8f, Gravity = 0.4f,
                Size = new CelCurve(0.026f, 0.018f), SizeJitter = 0.5f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.6f, 3f), Feather = 0f,
                Palette = "blue_laser_fleck", Core = 1f, Opacity = 1f, Extra = new Vector4(3f, 0.04f, 0.3f, 0f),
            });

        static CelLayer DustStretch(string name, float along, float length, float start, int close, int mid, CelCurve size, float life)
        {
            return new CelLayer
            {
                Name = name, Kind = LightningBeamKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.3f, along), Start = start, Duration = 0.22f, TimeJitter = 0.05f, Life = life, LifeJitter = 0.2f,
                Close = close, Mid = mid, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(0.6f, 0.45f, length), Aim = CelAim.Dir, Speed = 0.5f, SpeedJitter = 0.4f, Spread = 20f, Drag = 1f, Buoyancy = 0.05f,
                Size = size, SizeJitter = 0.3f, Stretch = -0.001f, Erode = new CelCurve(0f, 0.95f, 2.2f), Feather = 0f,
                Palette = "blue_laser_dust", Core = 1f, Opacity = 0.55f, Extra = new Vector4(3f, 0.16f, 0.25f, 0f),
            };
        }

        /// <summary>Fire it: from the shooter to the target, the ground at the target's foot. LAB ONLY.</summary>
        public static void Play(Vector3 from, Vector3 to, Vector3 ground, uint seed)
        {
            Vector3 dir = to - from;
            CelFx.Play(Beam, from, dir, to, seed);
            Vector3 level = new Vector3(dir.x, 0f, dir.z);
            if (level.sqrMagnitude < 1e-6f) level = Vector3.forward;
            level.Normalize();
            CelFx.Play(Ground, ground, level, ground, seed + 1u);
            // the dust blows on past the target, along the beam's way
            Vector3 right = Vector3.Cross(Vector3.up, level);
            // r04: 20 degrees right and rising 0.2 (the clip's trail runs from mid-dummy up-right to ~3 dummy heights past it)
            // b01: rising 0.28 (beach stage: the clip's trail climbs to ~(1250, 250) px)
            Vector3 trail = (level * 0.94f + right * 0.342f + Vector3.up * 0.28f).normalized;
            CelFx.Play(Dust, to, trail, to, seed + 2u);
        }

        /// <summary>The Range shot: from the low muzzle (the clip's beam enters at the lower-left corner) to the chest.</summary>
        [CelRangeEffect("blue_laser")]
        static void Range(RangeShot s)
        {
            float keep = CelFx.ZoomOverride;
            CelFx.ZoomOverride = s.Zoom;
            try { Play(s.Muzzle, s.Target, s.Ground, (uint)s.Seed); }
            finally { CelFx.ZoomOverride = keep; }
        }
    }
}
