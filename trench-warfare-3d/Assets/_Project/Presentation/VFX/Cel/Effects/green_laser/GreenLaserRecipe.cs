// Phase: CelFx job (2026-11-09, lane/show/cel-green-laser) - green_laser: an anime energy-bolt volley (lab only). Darts
// of green light come from the lower left in pairs for about four seconds and burst into spiky white-green stars on
// the target; a green pool lights the dirt under it, a second burst point lights the ground to its right, and green-grey
// cel smoke with ink puffs up and drifts right as the volley ends. Reference: vfx_h3 anime_laser_green_cel.mp4.
// Built only from the kit's Streak, Glow and Lobe kinds (3 kinds = 4 draws; none when other cel effects already draw).
// Two recipes: Bolts (muzzle -> chest: bolts, hit stars, spikes, light) and Ground (dummy foot -> burst point on the
// ground to the eye's right: pools, ground bursts, smoke, embers); GreenLaser.Play fires both.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class GreenLaser
    {
        // the volley: bolts born evenly over Volley seconds, each flying Travel seconds to the chest
        public const float Volley = 3.9f, Travel = 0.42f, Length = 5.0f;
        public const int Bolts = 30;

        static GreenLaser()
        {
            // colours picked off the reference (spec/green_laser.json palette, lightest four)
            CelPalette.Add("gl_bolt", CelPalette.Hex(0x3E8A2A), CelPalette.Hex(0x77C04E), CelPalette.Hex(0xB2EC74), CelPalette.Hex(0xDDF8A8), CelPalette.Hex(0xF6FFE4), 0.62f, 3, CelPalette.Hex(0x0C2A06));
            CelPalette.Add("gl_pool", CelPalette.Hex(0x14300F), CelPalette.Hex(0x23521A), CelPalette.Hex(0x3A7A2A), CelPalette.Hex(0x5C9E40), CelPalette.Hex(0x9AD86A), 1.1f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("gl_smoke", CelPalette.Hex(0x4E6440), CelPalette.Hex(0x7A9A5C), CelPalette.Hex(0xA6C27E), CelPalette.Hex(0xCADCA4), CelPalette.Hex(0xC8DCA0), 1.1f, 3, CelPalette.Hex(0x141E10));
        }

        static CelLayer Spikes(int k) => new CelLayer
        {
            Name = "hit spikes " + k, Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = Travel, Duration = Volley, Life = 0.2f, LifeJitter = 0.3f,
            Close = Bolts, Mid = k < 2 ? Bolts / 2 : 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.12f, 0f, 0f), Aim = CelAim.Radial, Speed = 9f, SpeedJitter = 0.5f, UpBias = 0.25f, Drag = 4f,
            Size = new CelCurve(0.07f, 0.02f), SizeJitter = 0.3f, Stretch = 0.06f, Erode = new CelCurve(0.05f, 0.5f), Palette = "gl_bolt", Core = 4f, Extra = new Vector4(0f, 0.25f, 0f, 0f),
        };

        public static readonly CelFxRecipe BoltsRecipe = new CelFxRecipe("green_laser_bolts", Length,
            new CelLayer
            {
                // the darts: pointed tail, a fat bright head, two lanes a little apart, all converging on the chest
                Name = "bolts", Kind = CelKinds.StreakName, Start = 0f, Duration = Volley, Life = Travel, LifeJitter = 0f,
                Close = Bolts, Mid = 18, Far = 8, Shape = CelShape.Box, ShapeSize = new Vector3(1.4f, 0.7f, 0.6f), Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                Size = new CelCurve(0.13f, 0.12f), SizeJitter = 0.15f, Stretch = 0.08f, Erode = new CelCurve(0.03f, 0.08f), Palette = "gl_bolt", Core = 5f, Extra = new Vector4(0f, 0.6f, 0.15f, 0f),
            },
            new CelLayer
            {
                Name = "bolts lane 2", Kind = CelKinds.StreakName, Start = 0.06f, Duration = Volley, Life = Travel, LifeJitter = 0f,
                Close = Bolts, Mid = 10, Far = 0, Offset = new Vector3(0.5f, -0.25f, 0f), Shape = CelShape.Box, ShapeSize = new Vector3(1.4f, 0.7f, 0.6f), Aim = CelAim.Target, Speed = 0f, Spread = 0f,
                Size = new CelCurve(0.11f, 0.1f), SizeJitter = 0.15f, Stretch = 0.075f, Erode = new CelCurve(0.03f, 0.08f), Palette = "gl_bolt", Core = 5f, Extra = new Vector4(0f, 0.5f, 0.15f, 0f),
            },
            new CelLayer
            {
                // the starburst on the target: a spiky flash facing the eye, white core, green spikes
                Name = "hit star", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = Travel, Duration = Volley, Life = 0.22f, LifeJitter = 0.2f,
                Close = Bolts, Mid = 18, Far = 8, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.25f, 0f, 0f), Size = new CelCurve(0.9f, 1.4f, 0.5f), SizeJitter = 0.25f, Spin = 2f,
                Erode = new CelCurve(0.02f, 0.85f, 1.5f), Palette = "gl_bolt", Core = 5f, Extra = new Vector4(2f, 1.3f, 11f, 3.5f),
            },
            new CelLayer
            {
                // a soft halo behind the stars: the bloom round the hit
                Name = "hit halo", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = Travel, Duration = Volley, Life = 0.45f, LifeJitter = 0.2f,
                Close = Bolts / 2, Mid = 6, Far = 0, Size = new CelCurve(1.3f, 1.9f, 0.5f), SizeJitter = 0.2f,
                Erode = new CelCurve(0.02f, 0.7f, 1.5f), Palette = "gl_pool", Core = 1f, Opacity = 0.8f, Extra = new Vector4(2f, 2.2f, 0f, 0f),
            },
            Spikes(0), Spikes(1), Spikes(2), Spikes(3),
            new CelLayer { Name = "light a", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = Travel, Life = 1.6f, Close = 1, Mid = 1, Far = 1, Palette = "gl_bolt", Core = 3f, Extra = new Vector4(9f, 0f, 0f, 0f) },
            new CelLayer { Name = "light b", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = Travel + 1.4f, Life = 1.6f, Close = 1, Mid = 1, Far = 0, Palette = "gl_bolt", Core = 3f, Extra = new Vector4(9f, 0f, 0f, 0f) },
            new CelLayer { Name = "light c", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = Travel + 2.8f, Life = 1.6f, Close = 1, Mid = 1, Far = 0, Palette = "gl_bolt", Core = 3f, Extra = new Vector4(9f, 0f, 0f, 0f) });

        // Ground: origin = the dummy's foot, target = the ground burst point, dir = the eye's right (level)
        public static readonly CelFxRecipe GroundRecipe = new CelFxRecipe("green_laser_ground", Length,
            new CelLayer
            {
                // the big pool that lights the dirt from the dummy to the right burst; it holds through the volley
                Name = "pool", Kind = CelKinds.GlowName, Offset = new Vector3(0f, 0f, 1.2f), Start = Travel, Life = Volley + 0.5f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(4.6f, 5.0f, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0.01f, 0.8f, 4f),
                Palette = "gl_pool", Core = 1f, Opacity = 1.3f, Extra = new Vector4(0f, 0.8f, 0f, 0f),
            },
            new CelLayer
            {
                // pulses of the pool under the dummy, one per few hits
                Name = "pool pulse", Kind = CelKinds.GlowName, Start = Travel, Duration = Volley, Life = 0.6f, LifeJitter = 0.2f,
                Close = 10, Mid = 4, Far = 0, Size = new CelCurve(2.4f, 3.4f, 0.5f), SizeJitter = 0.2f, Erode = new CelCurve(0.02f, 0.8f, 1.5f),
                Palette = "gl_pool", Core = 1f, Opacity = 1f, Extra = new Vector4(0f, 1.6f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "burst pool", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = Travel + 0.8f, Duration = Volley - 1.2f, Life = 0.9f, LifeJitter = 0.2f,
                Close = 6, Mid = 3, Far = 0, Size = new CelCurve(2.0f, 2.8f, 0.5f), SizeJitter = 0.2f, Erode = new CelCurve(0.02f, 0.85f, 1.5f),
                Palette = "gl_pool", Core = 1.6f, Opacity = 1f, Extra = new Vector4(0f, 1.2f, 0f, 0f),
            },
            new CelLayer
            {
                // secondary bursts on the ground right of the dummy: a star and a fountain of spikes
                Name = "burst star", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.25f, 0f), Start = Travel + 0.8f, Duration = Volley - 1.2f, Life = 0.25f, LifeJitter = 0.2f,
                Close = 10, Mid = 5, Far = 0, Size = new CelCurve(0.8f, 1.3f, 0.5f), SizeJitter = 0.25f, Spin = 1.5f,
                Erode = new CelCurve(0.02f, 0.85f, 1.5f), Palette = "gl_bolt", Core = 4f, Extra = new Vector4(2f, 1.3f, 9f, 3.5f),
            },
            new CelLayer
            {
                Name = "burst spikes", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = Travel + 0.8f, Duration = Volley - 1.2f, Life = 0.3f, LifeJitter = 0.3f,
                Close = 60, Mid = 20, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.25f, 0f, 0f), Aim = CelAim.Up, Speed = 7f, SpeedJitter = 0.5f, Spread = 35f, Drag = 3f, Gravity = 4f,
                Size = new CelCurve(0.06f, 0.02f), SizeJitter = 0.3f, Stretch = 0.07f, Erode = new CelCurve(0.05f, 0.5f), Palette = "gl_bolt", Core = 4f, Extra = new Vector4(0f, 0.3f, 0f, 0f),
            },
            new CelLayer
            {
                // green-grey cel smoke with ink: puffs off the dummy and the burst point, rising and drifting right
                Name = "smoke dummy", Kind = CelKinds.LobeName, Offset = new Vector3(0f, 1.3f, 0.2f), Start = 1.0f, Duration = Volley - 1.0f, Life = 1.1f, LifeJitter = 0.25f,
                Close = 10, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.35f, 0f, 0f), Aim = CelAim.Dir, Speed = 0.9f, Spread = 25f, Drag = 0.6f, Buoyancy = 0.5f, Spin = 0.8f,
                Size = new CelCurve(0.18f, 0.42f, 0.5f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 3f), Feather = 0.1f,
                Palette = "gl_smoke", Core = 1f, Ink = 2f, Extra = new Vector4(1.4f, 0.2f, 0.6f, 0f),
            },
            new CelLayer
            {
                Name = "smoke burst", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.5f, 0.3f), Start = 1.6f, Duration = Volley - 1.4f, Life = 1.2f, LifeJitter = 0.25f,
                Close = 12, Mid = 5, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.4f, 0f, 0f), Aim = CelAim.Dir, Speed = 1.0f, Spread = 25f, Drag = 0.6f, Buoyancy = 0.45f, Spin = 0.8f,
                Size = new CelCurve(0.22f, 0.5f, 0.5f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 3f), Feather = 0.1f,
                Palette = "gl_smoke", Core = 1f, Ink = 2f, Extra = new Vector4(1.4f, 0.2f, 0.6f, 0f),
            },
            new CelLayer
            {
                Name = "embers", Kind = CelKinds.StreakName, Offset = new Vector3(0f, 1f, 0.8f), Start = 1.2f, Duration = Volley - 0.4f, Life = 1.4f,
                Close = 24, Mid = 8, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.2f, 0f, 0f), Aim = CelAim.Radial, Speed = 1.2f, UpBias = 1f, Drag = 0.8f, Buoyancy = 0.8f,
                Size = new CelCurve(0.05f, 0.025f), Stretch = 0.03f, Erode = new CelCurve(0.05f, 0.6f, 2f), Palette = "gl_bolt", Core = 3f, Extra = new Vector4(1f, 0.06f, 0.5f, 0f),
            });

        /// <summary>Fire the volley: from `muzzle` at `chest`, standing on `foot`, seen from `eye` (the ground burst and the
        /// smoke's drift go to the eye's right).</summary>
        public static void Play(Vector3 muzzle, Vector3 chest, Vector3 foot, Vector3 eye, uint seed)
        {
            CelFx.Play(BoltsRecipe, muzzle, chest - muzzle, chest, seed);
            Vector3 look = foot - eye; look.y = 0f;
            if (look.sqrMagnitude < 1e-4f) look = chest - muzzle;
            look.y = 0f;
            look = look.sqrMagnitude > 1e-6f ? look.normalized : Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, look).normalized;
            Vector3 burst = foot + right * 2.3f + look * 1.0f;
            CelFx.Play(GroundRecipe, foot, right, burst, seed + 101u);
        }

        [CelRangeEffect("green_laser")]
        static void Fire(RangeShot s)
        {
            float keep = CelFx.ZoomOverride;
            CelFx.ZoomOverride = s.Zoom;
            Vector3 eye = s.Eye.sqrMagnitude > 0f ? s.Eye : (Camera.main != null ? Camera.main.transform.position : s.Muzzle);
            Play(s.Muzzle, s.Target, s.Ground, eye, (uint)s.Seed);
            CelFx.ZoomOverride = keep;
        }
    }
}
