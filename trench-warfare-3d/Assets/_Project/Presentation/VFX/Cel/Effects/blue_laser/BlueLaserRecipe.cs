// Phase: CelFx job, blue_laser (2026-11-09, lane/show/cel-blue-laser) - the thin blue laser of the owner's clip
// laser_thin_blue_cel: a held beam (about 3.5 s) of blue-white lightning pulses running from the shooter to the target,
// each a pointed spindle with a zig-zag core, a ragged edge and branch arcs, over a thin steady thread; a white-blue
// star flaring on the target with every pulse, rings on its board and blue sparks off it; a blue glow pool and ring on
// the ground under it; at the end grey-tan dust with dark flecks blown on past the target. LAB ONLY (owner,
// 2026-11-09): no weapon fires it; VfxLab, the Playground and the Range call Play. Energy weapon, so blue light is
// allowed (exception to "every light warm").
// Three plays of the kit: the beam (shooter -> target), the ground (level, at the foot) and the dust (level, along
// the beam's way on past the target). LOD: Close everything; Mid fewer pulses, flashes, sparks and puffs and no branch
// arcs (the shader drops them); Far the steady thread and the target glow only.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class BlueLaserRecipe
    {
        // ---------------------------------------------------------------- palettes (picked off the clip's frames)
        public static readonly int BeamRow = CelPalette.Add("blue_laser_beam",
            CelPalette.Hex(0x2C6488), CelPalette.Hex(0x4A98C0), CelPalette.Hex(0x78C4DC), CelPalette.Hex(0xA8E4F0),
            CelPalette.Hex(0xEAF8FF), 0.88f, 4, CelPalette.Hex(0x0C2238));
        /// <summary>The ground glow, as it adds onto the dark field (additive: these are the light it adds).</summary>
        public static readonly int GroundRow = CelPalette.Add("blue_laser_ground",
            CelPalette.Hex(0x16304A), CelPalette.Hex(0x1F4262), CelPalette.Hex(0x2C5A82), CelPalette.Hex(0x3F78A6),
            CelPalette.Hex(0x3F78A6), 1.1f, 3, CelPalette.Hex(0x000000));
        /// <summary>The clip's dust is grey-tan, darker than the kit's smoke_tan.</summary>
        public static readonly int DustRow = CelPalette.Add("blue_laser_dust",
            CelPalette.Hex(0x4A3F3A), CelPalette.Hex(0x6E625A), CelPalette.Hex(0x958579), CelPalette.Hex(0xB3A290),
            CelPalette.Hex(0xB3A290), 1.1f, 3, CelPalette.Hex(0x261E1A));

        const float Hold = 3.35f;          // seconds of pulses
        const float Ramp = 0.6f;           // the first pulses come sparse (the clip's beam builds up over its first 0.6 s)
        const float PulseLife = 0.30f;

        public static readonly CelFxRecipe Beam = new CelFxRecipe("blue_laser_beam", 5f,
            new CelLayer
            {
                Name = "first pulses", Kind = LightningBeamKind.KindName, Start = 0f, Duration = Ramp, TimeJitter = 0.03f, Life = PulseLife, LifeJitter = 0.1f,
                Close = 3, Mid = 2, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(0.4f, 0.25f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(0.2f, 0.15f), SizeJitter = 0.2f, Stretch = 0.5f, Erode = new CelCurve(0.02f, 0.25f, 2f),
                Palette = "blue_laser_beam", Core = 1.7f, Opacity = 0.9f, Ink = 1f, Extra = new Vector4(0.28f, 1.3f, 0.15f, 1.5f),
            },
            new CelLayer
            {
                Name = "pulses", Kind = LightningBeamKind.KindName, Start = Ramp, Duration = Hold - Ramp, TimeJitter = 0.035f, Life = PulseLife, LifeJitter = 0.15f,
                Close = 26, Mid = 12, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(0.6f, 0.35f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f, Arrive = 1f,
                Size = new CelCurve(0.24f, 0.17f), SizeJitter = 0.25f, Stretch = 0.5f, Erode = new CelCurve(0.02f, 0.25f, 2f),
                Palette = "blue_laser_beam", Core = 1.7f, Opacity = 0.9f, Ink = 1f, Extra = new Vector4(0.28f, 1.3f, 0f, 1.5f),
            },
            new CelLayer
            {
                Name = "through", Kind = LightningBeamKind.KindName, Start = 1.0f, Duration = 2.3f, TimeJitter = 0.1f, Life = 0.22f, LifeJitter = 0.2f,
                Close = 6, Mid = 2, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(0.3f, 0.2f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                Size = new CelCurve(0.15f, 0.1f), SizeJitter = 0.2f, Stretch = 0.32f, Erode = new CelCurve(0.02f, 0.3f),
                Palette = "blue_laser_beam", Core = 1.7f, Ink = 1.6f, Extra = new Vector4(0.25f, 0f, 0.95f, 1.6f),
            },
            new CelLayer
            {
                Name = "thread", Kind = LightningBeamKind.KindName, Start = Ramp * 0.5f, Life = Hold - Ramp * 0.5f + 0.25f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                Size = new CelCurve(0.07f, 0.06f), SizeJitter = 0f, Stretch = 1f, Erode = new CelCurve(0.05f, 0.1f),
                Palette = "blue_laser_beam", Core = 1.2f, Opacity = 0.45f, Ink = 1f, Extra = new Vector4(0.4f, 0f, 1f, 1f),
            },
            new CelLayer
            {
                Name = "halo", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.4f), Start = 0.15f, Life = Hold + 0.1f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.3f, 0.34f), SizeJitter = 0f, Erode = new CelCurve(0.03f, 0.2f, 3f),
                Palette = "blue_laser_beam", Core = 1f, Opacity = 0.35f, Extra = new Vector4(2f, 2.6f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "star", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.45f), Start = 0.2f, Duration = Hold - 0.1f, TimeJitter = 0.03f, Life = 0.13f, LifeJitter = 0.2f,
                Close = 26, Mid = 10, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.06f, 0f, 0f), Size = new CelCurve(0.22f, 0.4f, 0.5f), SizeJitter = 0.25f,
                Erode = new CelCurve(0.03f, 0.6f, 1.5f), Spin = 3f, Palette = "blue_laser_beam", Core = 1.8f, Opacity = 0.8f, Extra = new Vector4(2f, 1.3f, 8f, 7f),
            },
            new CelLayer
            {
                Name = "board rings", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.35f), Start = 0.3f, Duration = Hold - 0.3f, Life = 0.3f, LifeJitter = 0.1f,
                Close = 10, Mid = 0, Far = 0, Size = new CelCurve(0.12f, 0.32f, 0.6f), SizeJitter = 0.1f, Erode = new CelCurve(0.05f, 0.7f),
                Palette = "blue_laser_beam", Core = 1f, Opacity = 0.5f, Extra = new Vector4(3f, 0.12f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "afterglow", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.1f, -0.4f), Start = Hold + 0.1f, Life = 1.4f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.22f, 0.14f), SizeJitter = 0f, Erode = new CelCurve(0.05f, 1f, 1.5f),
                Palette = "blue_laser_beam", Core = 2f, Opacity = 0.8f, Extra = new Vector4(2f, 1.5f, 6f, 6f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.4f), Start = 0.2f, Duration = Hold - 0.1f, Life = 0.4f,
                Close = 70, Mid = 20, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.15f, 0f, 0f), Aim = CelAim.Radial, Speed = 5f, SpeedJitter = 0.6f, UpBias = 0.3f, Gravity = 5f, Drag = 1.2f,
                Size = new CelCurve(0.035f, 0.02f), Stretch = 0.05f, Erode = new CelCurve(0.05f, 0.5f), Palette = "blue_laser_beam", Core = 2f, Extra = new Vector4(0.2f, 0.05f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "warm sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.1f, -0.3f), Start = 0.4f, Duration = Hold - 0.4f, Life = 0.35f,
                Close = 12, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.12f, 0f, 0f), Aim = CelAim.Radial, Speed = 4f, SpeedJitter = 0.5f, UpBias = 0.6f, Gravity = 6f, Drag = 1f,
                Size = new CelCurve(0.03f, 0.015f), Stretch = 0.06f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 3f, Extra = new Vector4(0.2f, 0.05f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.6f), Start = 0.15f, Life = Hold + 0.2f,
                Close = 1, Mid = 1, Far = 0, Palette = "blue_laser_beam", Core = 0.7f, Extra = new Vector4(6f, 0f, 0f, 0f),
            });

        public static readonly CelFxRecipe Ground = new CelFxRecipe("blue_laser_ground", 4f,
            new CelLayer
            {
                Name = "pool", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0.1f, 0.04f, 0.1f), Start = 0.25f, Life = Hold, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.75f, 0.85f), SizeJitter = 0f, Erode = new CelCurve(0.02f, 0.45f, 5f),
                Palette = "blue_laser_ground", Core = 1f, Opacity = 1f, Extra = new Vector4(0f, 0.35f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "ring", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0.1f, 0.04f, 0.1f), Start = 0.25f, Life = Hold, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.77f, 0.87f), SizeJitter = 0f, Erode = new CelCurve(0.05f, 0.5f, 5f),
                Palette = "blue_laser_beam", Core = 1f, Opacity = 0.5f, Extra = new Vector4(1f, 0.1f, 0f, 0f),
            });

        public static readonly CelFxRecipe Dust = new CelFxRecipe("blue_laser_dust", 3f,
            new CelLayer
            {
                Name = "dust", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.3f, 3.4f), Start = Hold - 0.55f, Duration = 1.2f, TimeJitter = 0.1f, Life = 1.6f, LifeJitter = 0.3f,
                Close = 54, Mid = 14, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(1.0f, 0.9f, 6.6f), Aim = CelAim.Dir, Speed = 1.4f, Spread = 25f, Drag = 1f, Buoyancy = 0.25f, Spin = 0.8f,
                Size = new CelCurve(0.26f, 0.5f, 0.5f), Stretch = 0.8f, Erode = new CelCurve(0f, 1f, 3f), Feather = 0.1f,
                Palette = "blue_laser_dust", Core = 1f, Ink = 1.6f, Extra = new Vector4(1.4f, 0.18f, 0.9f, 0f),
            },
            new CelLayer
            {
                Name = "flecks", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.2f, 2.6f), Start = Hold - 0.5f, Duration = 0.9f, Life = 1.1f, LifeJitter = 0.3f,
                Close = 30, Mid = 0, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(0.8f, 0.8f, 5.2f), Aim = CelAim.Dir, Speed = 2.5f, Spread = 30f, Drag = 0.8f, Gravity = 1f,
                Size = new CelCurve(0.045f, 0.03f), SizeJitter = 0.4f, Stretch = 1f, Erode = new CelCurve(0f, 0.9f, 2f),
                Palette = "smoke_black", Core = 1f, Ink = 0f, Extra = new Vector4(1.3f, 0.1f, 0f, 0f),
            });

        /// <summary>Fire it: from the shooter to the target, the ground at the target's foot. LAB ONLY.</summary>
        public static void Play(Vector3 from, Vector3 to, Vector3 ground, uint seed)
        {
            Vector3 dir = to - from;
            CelFx.Play(Beam, from, dir, to, seed);
            Vector3 level = new Vector3(dir.x, 0f, dir.z);
            if (level.sqrMagnitude < 1e-6f) level = Vector3.forward;
            CelFx.Play(Ground, ground, level, ground, seed + 1u);
            CelFx.Play(Dust, to, level, to, seed + 2u);
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
