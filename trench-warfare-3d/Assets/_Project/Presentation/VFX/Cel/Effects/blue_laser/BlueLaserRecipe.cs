// Phase: CelFx job, blue_laser (2026-11-09, lane/show/cel-blue-laser) - the thin blue laser of the owner's clip
// laser_thin_blue_cel: a held beam (about 3.5 s) of separate cyan lightning spindles running from the shooter to the
// target, 2-4 at a time with gaps between, each pointed, inked, with a white core, a dim blue halo and short branch
// arcs; a small white star of spark lines on the target with blue rings on its board; a soft blue glow pool on the
// ground under it (no Light layer: SceneHooks.Flash blooms a white ball on the board even at core 0.22);
// the energy builds to a last volley at about 3.4 s (the clip's peak, f101), when a long sparse trail
// of dark tan dust with many black flecks blows up-right along the beam's way past the target and fades by 4.8 s.
// LAB ONLY (owner, 2026-11-09): no weapon fires it; VfxLab, the Playground and the Range call Play. Energy weapon, so
// blue light is allowed (exception to "every light warm").
// Three plays of the kit: the beam (shooter -> target), the ground (level, at the foot) and the dust (along the trail).
// LOD: Close everything; Mid fewer pulses, flashes, sparks and puffs, no branch arcs (the shader drops them) and the
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
        /// <summary>r03: the ground pool, brighter than the halo's light (the clip's pool is a bright blue glow ~1.5 torsos across).</summary>
        public static readonly int PoolRow = CelPalette.Add("blue_laser_pool",
            CelPalette.Hex(0x1C3E62), CelPalette.Hex(0x2C5E92), CelPalette.Hex(0x3E80C0), CelPalette.Hex(0x5AA2E0),
            CelPalette.Hex(0x8CC8F4), 0.92f, 3, CelPalette.Hex(0x000000));
        /// <summary>The clip's dust is a grey-tan about #22 lighter than its dark field; our field is lighter, so these are too.</summary>
        public static readonly int DustRow = CelPalette.Add("blue_laser_dust",
            CelPalette.Hex(0x5E544C), CelPalette.Hex(0x6E6256), CelPalette.Hex(0x8A7B6A), CelPalette.Hex(0x9A8A76),
            CelPalette.Hex(0x9A8A76), 1.1f, 2, CelPalette.Hex(0x141010));

        const float Hold = 3.3f;           // seconds of pulses (the last dies about 3.6 s: the clip's beam ends at f105)
        const float Ramp = 0.6f;           // the first pulses come sparse (the clip's beam builds up over its first 0.6 s)
        const float Volley = 3.05f;        // the last volley: more spindles at once, the clip's peak (f96-f102)
        const float PulseLife = 0.34f;
        // NB: on a LightningBeam layer Ink is the beam's clip (tone.w: 1 stops the window at the target, 1.6 runs on
        // through it), not an ink width; the spindle's ink is fixed in its shader.
        const float DustAt = 2.9f;         // the dust trail (clip f88-f135)

        public static readonly CelFxRecipe Beam = new CelFxRecipe("blue_laser_beam", 5f,
            new CelLayer
            {
                Name = "first pulses", Kind = LightningBeamKind.KindName, Start = 0f, Duration = Ramp, TimeJitter = 0.03f, Life = PulseLife, LifeJitter = 0.1f,
                Close = 8, Mid = 3, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(0.5f, 0.3f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(0.07f, 0.05f), SizeJitter = 0.2f, Stretch = 0.24f, Erode = new CelCurve(0.06f, 0.3f, 2f),
                Palette = "blue_laser_beam", Core = 1.5f, Opacity = 0.9f, Ink = 1f, Extra = new Vector4(0.2f, 1.2f, 0.1f, 1.3f),
            },
            new CelLayer
            {
                Name = "pulses", Kind = LightningBeamKind.KindName, Start = Ramp, Duration = Hold - Ramp, TimeJitter = 0.04f, Life = PulseLife, LifeJitter = 0.15f,
                Close = 48, Mid = 14, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(0.7f, 0.4f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(0.085f, 0.06f), SizeJitter = 0.25f, Stretch = 0.25f, Erode = new CelCurve(0.06f, 0.3f, 2f),
                Palette = "blue_laser_beam", Core = 1.5f, Opacity = 0.9f, Ink = 1f, Extra = new Vector4(0.2f, 1.2f, 0f, 1.3f),
            },
            new CelLayer
            {
                Name = "volley", Kind = LightningBeamKind.KindName, Start = Volley, Duration = 0.38f, TimeJitter = 0.03f, Life = PulseLife, LifeJitter = 0.1f,
                Close = 3, Mid = 2, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(0.9f, 0.5f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(0.09f, 0.06f), SizeJitter = 0.2f, Stretch = 0.27f, Erode = new CelCurve(0.06f, 0.3f, 2f),
                Palette = "blue_laser_beam", Core = 1.6f, Opacity = 0.9f, Ink = 1f, Extra = new Vector4(0.22f, 1.3f, 0f, 1.35f),
            },
            new CelLayer
            {
                Name = "through", Kind = LightningBeamKind.KindName, Start = 1.0f, Duration = Hold - 1.0f, TimeJitter = 0.1f, Life = 0.24f, LifeJitter = 0.2f,
                Close = 9, Mid = 3, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(0.3f, 0.2f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                Size = new CelCurve(0.06f, 0.04f), SizeJitter = 0.2f, Stretch = 0.24f, Erode = new CelCurve(0.06f, 0.35f),
                Palette = "blue_laser_beam", Core = 1.5f, Opacity = 0.9f, Ink = 1.6f, Extra = new Vector4(0.18f, 0f, 0.95f, 1.6f),
            },
            new CelLayer
            {
                // hidden at Close by the shader: the line that stands for the pulses at Mid and Far
                Name = "thread", Kind = LightningBeamKind.KindName, Start = Ramp * 0.5f, Life = Hold - Ramp * 0.5f + 0.25f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                Size = new CelCurve(0.06f, 0.05f), SizeJitter = 0f, Stretch = 1f, Erode = new CelCurve(0.05f, 0.1f),
                Palette = "blue_laser_beam", Core = 1.3f, Opacity = 0.7f, Ink = 1f, Extra = new Vector4(0.1f, 0f, 1f, 1f),
            },
            new CelLayer
            {
                // the impact dot: the star's white core up close; at Far kept 4 px in radius and lit through the fog
                Name = "dot", Kind = LightningBeamKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.45f), Start = 0.15f, Life = Hold + 0.15f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.None, Speed = 0f,
                Size = new CelCurve(0.04f, 0.05f), SizeJitter = 0f, Stretch = -4f, Erode = new CelCurve(0.05f, 0.12f),
                Palette = "blue_laser_hit", Core = 1f, Opacity = 0.85f, Extra = new Vector4(0f, 0f, 4f, 3f),
            },
            new CelLayer
            {
                Name = "halo", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.4f), Start = 0.15f, Life = Hold + 0.1f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.42f, 0.48f), SizeJitter = 0f, Erode = new CelCurve(0.03f, 0.2f, 3f),
                Palette = "blue_laser_hit", Core = 1f, Opacity = 0.75f, Extra = new Vector4(2f, 1.3f, 0f, 0f),
            },
            new CelLayer
            {
                // r03: the clip's hit is a white-blue bloom ball about the dummy's torso width, flickering f30-f101
                Name = "bloom", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.45f), Start = 0.15f, Duration = Hold, TimeJitter = 0.02f, Life = 0.14f, LifeJitter = 0.25f,
                Close = 32, Mid = 14, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.03f, 0f, 0f), Size = new CelCurve(0.14f, 0.19f, 0.6f), SizeJitter = 0.25f,
                Erode = new CelCurve(0.06f, 0.5f, 1.5f), Palette = "blue_laser_hit", Core = 1.7f, Opacity = 0.9f, Extra = new Vector4(2f, 1.4f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "star", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.45f), Start = 0.2f, Duration = Hold - 0.1f, TimeJitter = 0.03f, Life = 0.12f, LifeJitter = 0.2f,
                Close = 26, Mid = 10, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.05f, 0f, 0f), Size = new CelCurve(0.12f, 0.22f, 0.5f), SizeJitter = 0.25f,
                Erode = new CelCurve(0.05f, 0.6f, 1.5f), Spin = 3f, Palette = "blue_laser_hit", Core = 1.5f, Opacity = 0.85f, Extra = new Vector4(2f, 1.3f, 8f, 10f),
            },
            new CelLayer
            {
                Name = "board rings", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.35f), Start = 0.3f, Duration = Hold - 0.3f, Life = 0.32f, LifeJitter = 0.1f,
                Close = 4, Mid = 0, Far = 0, Size = new CelCurve(0.05f, 0.24f, 0.6f), SizeJitter = 0.15f, Erode = new CelCurve(0.05f, 0.6f),
                Palette = "blue_laser_hit", Core = 1f, Opacity = 0.3f, Extra = new Vector4(3f, 0.1f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "afterglow", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.08f, -0.4f), Start = Hold + 0.1f, Life = 1.3f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.13f, 0.1f), SizeJitter = 0f, Erode = new CelCurve(0.05f, 1f, 1.5f),
                Palette = "blue_laser_hit", Core = 1.2f, Opacity = 0.7f, Extra = new Vector4(2f, 1.5f, 6f, 6f),
            },
            new CelLayer
            {
                // r03: blue-white sparkles left on the board after the beam (clip f105-f125)
                Name = "sparkles", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.4f), Start = Hold + 0.15f, Duration = 0.85f, TimeJitter = 0.03f, Life = 0.16f, LifeJitter = 0.3f,
                Close = 18, Mid = 6, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.2f, 0f, 0f), Size = new CelCurve(0.05f, 0.03f), SizeJitter = 0.4f,
                Erode = new CelCurve(0.05f, 0.6f), Spin = 2f, Palette = "blue_laser_hit", Core = 2f, Opacity = 0.9f, Extra = new Vector4(2f, 1.4f, 4f, 8f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.4f), Start = 0.2f, Duration = Hold - 0.1f, Life = 0.3f,
                Close = 100, Mid = 24, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.08f, 0f, 0f), Aim = CelAim.Radial, Speed = 3.2f, SpeedJitter = 0.6f, UpBias = 0.3f, Gravity = 5f, Drag = 1.2f,
                Size = new CelCurve(0.025f, 0.015f), Stretch = 0.05f, Erode = new CelCurve(0.05f, 0.5f), Palette = "blue_laser_hit", Core = 1.6f, Extra = new Vector4(0.2f, 0.05f, 0.3f, 0f),
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
                Name = "warm sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.1f, -0.3f), Start = 0.4f, Duration = Hold - 0.4f, Life = 0.35f,
                Close = 40, Mid = 10, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.1f, 0f, 0f), Aim = CelAim.Radial, Speed = 2.6f, SpeedJitter = 0.5f, UpBias = 0.5f, Gravity = 5f, Drag = 1.5f,
                Size = new CelCurve(0.025f, 0.012f), Stretch = 0.06f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 2f, Extra = new Vector4(0.2f, 0.05f, 0f, 0f),
            });

        public static readonly CelFxRecipe Ground = new CelFxRecipe("blue_laser_ground", 4f,
            new CelLayer
            {
                // a soft pool: four close bands of dim light, brightest under the dummy (no ring)
                Name = "pool", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0.05f, 0.04f, 0.05f), Start = 0.25f, Life = Hold + 0.1f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.9f, 1.0f), SizeJitter = 0f, Erode = new CelCurve(0.01f, 0.05f),
                Palette = "blue_laser_pool", Core = 1.2f, Opacity = 0.85f, Extra = new Vector4(0f, 1.6f, 0f, 0f),
            });

        /// <summary>The dust trail, played along the trail (see Play): three stretches born near to far, then the flecks.</summary>
        public static readonly CelFxRecipe Dust = new CelFxRecipe("blue_laser_dust", 3f,
            // r03: 25 small soft puffs (each under a third of the dummy's height) in four stretches, not cumulus clumps
            DustStretch("dust near", 0.35f, 1.6f, DustAt, 7, 2, new CelCurve(0.25f, 0.32f, 0.5f), 2.1f),
            DustStretch("dust mid", 2.1f, 1.8f, DustAt + 0.07f, 7, 2, new CelCurve(0.26f, 0.33f, 0.5f), 2.05f),
            DustStretch("dust mid2", 4.0f, 1.8f, DustAt + 0.14f, 6, 1, new CelCurve(0.27f, 0.34f, 0.5f), 1.95f),
            DustStretch("dust far", 6.0f, 1.8f, DustAt + 0.2f, 5, 1, new CelCurve(0.28f, 0.35f, 0.5f), 1.85f),
            new CelLayer
            {
                Name = "flecks", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.55f, 3.6f), Start = DustAt + 0.05f, Duration = 0.45f, Life = 1.6f, LifeJitter = 0.3f,
                Close = 280, Mid = 60, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(1.0f, 0.6f, 7.6f), Aim = CelAim.Dir, Speed = 1.4f, Spread = 30f, Drag = 0.8f, Gravity = 0.6f,
                Size = new CelCurve(0.032f, 0.022f), SizeJitter = 0.5f, Stretch = 1f, Erode = new CelCurve(0f, 0.9f, 3f),
                Palette = "smoke_black", Core = 1f, Ink = 0f, Extra = new Vector4(1.3f, 0.1f, 0f, 0f),
            });

        static CelLayer DustStretch(string name, float along, float length, float start, int close, int mid, CelCurve size, float life)
        {
            return new CelLayer
            {
                Name = name, Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.62f, along), Start = start, Duration = 0.22f, TimeJitter = 0.05f, Life = life, LifeJitter = 0.25f,
                Close = close, Mid = mid, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(0.9f, 0.45f, length), Aim = CelAim.Dir, Speed = 0.7f, SpeedJitter = 0.4f, Spread = 20f, Drag = 1f, Buoyancy = 0.03f, Spin = 0.6f,
                Size = size, SizeJitter = 0.35f, Stretch = 0.85f, Erode = new CelCurve(0.2f, 1f, 2.2f), Feather = 0f,
                Palette = "blue_laser_dust", Core = 1f, Ink = 0f, Extra = new Vector4(1.6f, 0.1f, 0f, 0f),
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
            // the dust blows on past the target, turned 15 degrees to the shooter's right, level (the clip's trail
            // runs up-right from the dummy, about 3 dummy heights long)
            Vector3 right = Vector3.Cross(Vector3.up, level);
            Vector3 trail = level * 0.966f + right * 0.259f + Vector3.up * 0.0f;
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
