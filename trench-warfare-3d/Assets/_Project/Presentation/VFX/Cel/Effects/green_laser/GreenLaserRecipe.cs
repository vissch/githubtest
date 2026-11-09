// Phase: CelFx job (2026-11-09, lane/show/cel-green-laser) - green_laser: an anime energy-bolt volley (lab only). Short
// green darts with white-hot heads come from the lower left in two lanes for about four seconds and burst into thin
// spiky green stars on the target; a soft green wash lights the dirt under it, a second burst point kicks dark dirt and
// light to its right, and pale yellow-green cel smoke with ink puffs up and drifts right, cooling to tan-grey, as the
// volley ends, with green embers left at the feet. Reference: vfx_h3 anime_laser_green_cel.mp4.
// Kinds: the kit's Streak, Glow and Lobe, plus this effect's GlBolt (darts and the smooth ground wash, one draw).
// Recipes: Bolts (lane 1 darts, hit stars, spikes, light), Lane2 (the second lane's darts) and Ground (dummy foot ->
// burst point on the ground to the eye's right: washes, ground bursts, dirt, smoke, embers); GreenLaser.Play fires all.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    /// <summary>The green laser's own primitive: darts with a bulbous head and the smooth ground wash (Shaders/Cel/Effects/green_laser).</summary>
    public sealed class GlBoltKind : CelKind
    {
        public override string Name => "GlBolt";
        public override Shader FindShader() => Shader.Find("TW/Cel/GlBolt");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel GlBolt");
        public override int Capacity => 256;
        public override int Order => 15;
    }

    public static class GreenLaser
    {
        // the volley: darts born evenly over Volley seconds, each flying Travel seconds to the chest
        public const float Volley = 3.9f, Travel = 0.42f, Length = 5.0f;
        public const int Bolts = 30;
        public const string BoltKind = "GlBolt";
        // where along muzzle (near, low) -> muzzle high (far, level) each lane starts: the reference's shallow climb
        public const float Lane1 = 0.24f, Lane2 = 0.1f;

        static GreenLaser()
        {
            // colours picked off the reference (spec/green_laser.json palette)
            CelPalette.Add("gl_bolt", CelPalette.Hex(0x3E9A22), CelPalette.Hex(0x5CC032), CelPalette.Hex(0x8EE04E), CelPalette.Hex(0x7DD848), CelPalette.Hex(0xF6FFE4), 0.62f, 3, CelPalette.Hex(0x0C2A06));
            CelPalette.Add("gl_star", CelPalette.Hex(0x3E9A22), CelPalette.Hex(0x5CC032), CelPalette.Hex(0x8EE04E), CelPalette.Hex(0xB8F070), CelPalette.Hex(0xF6FFE4), 0.8f, 3, CelPalette.Hex(0x0C2A06));
            CelPalette.Add("gl_wash", CelPalette.Hex(0x14300F), CelPalette.Hex(0x1E4416), CelPalette.Hex(0x2C5E1E), CelPalette.Hex(0x58A436), CelPalette.Hex(0x9AD86A), 1.1f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("gl_halo", CelPalette.Hex(0x14300F), CelPalette.Hex(0x23521A), CelPalette.Hex(0x3A7A2A), CelPalette.Hex(0x5C9E40), CelPalette.Hex(0x9AD86A), 1.1f, 2, CelPalette.Hex(0x000000));
            CelPalette.Add("gl_smoke", CelPalette.Hex(0xD7F39C), CelPalette.Hex(0xE4F8B0), CelPalette.Hex(0xEEFCC4), CelPalette.Hex(0xF6FFD8), CelPalette.Hex(0xFAFFE8), 1.1f, 4, CelPalette.Hex(0x0E160A));
            CelPalette.Add("gl_smoke_tan", CelPalette.Hex(0xC4BA98), CelPalette.Hex(0xD6CCAA), CelPalette.Hex(0xE4DCC0), CelPalette.Hex(0xEEE8D2), CelPalette.Hex(0xDDD6BC), 1.1f, 4, CelPalette.Hex(0x14120E));
            CelPalette.Add("gl_dirt", CelPalette.Hex(0x0E0E0A), CelPalette.Hex(0x16150F), CelPalette.Hex(0x201E16), CelPalette.Hex(0x2C291E), CelPalette.Hex(0x2C291E), 1.1f, 3, CelPalette.Hex(0x040403));
        }

        static CelLayer Darts(string name, float start, int close, int mid, int far, float width) => new CelLayer
        {
            // a dart: bulbous white-hot head, ragged green tail of a fixed length, soft bloom; every dart of a lane on one line
            Name = name, Kind = BoltKind, Start = start, Duration = Volley, Life = Travel, LifeJitter = 0f,
            Close = close, Mid = mid, Far = far, Shape = CelShape.Point, Aim = CelAim.Target, Speed = 0f, Spread = 0f,
            Size = new CelCurve(width, width * 0.9f), SizeJitter = 0.1f, Palette = "gl_bolt", Core = 2.6f, Opacity = 1f,
            Extra = new Vector4(0f, 1.1f, 0.22f, 1.5f),
        };

        static CelLayer Spikes(int k) => new CelLayer
        {
            Name = "hit spikes " + k, Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = Travel, Duration = Volley, Life = 0.2f, LifeJitter = 0.3f,
            Close = Bolts, Mid = k < 2 ? Bolts / 2 : 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.1f, 0f, 0f), Aim = CelAim.Radial, Speed = 8f, SpeedJitter = 0.5f, UpBias = 0.25f, Drag = 4f,
            Size = new CelCurve(0.05f, 0.015f), SizeJitter = 0.3f, Stretch = 0.06f, Erode = new CelCurve(0.05f, 0.5f), Palette = "gl_star", Core = 3f, Extra = new Vector4(0f, 0.2f, 0f, 0f),
        };

        static CelLayer Smoke(string name, CelAnchor anchor, Vector3 offset, float start, float duration, float life, int close, int mid, float r0, float r1) => new CelLayer
        {
            // pale yellow-green curly cel puffs with a dark ink line, rising and drifting right, cooling to tan-grey
            Name = name, Kind = CelKinds.LobeName, Anchor = anchor, Offset = offset, Start = start, Duration = duration, Life = life, LifeJitter = 0.2f,
            Close = close, Mid = mid, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.35f, 0f, 0f), Aim = CelAim.Dir, Speed = 1.0f, Spread = 30f, Drag = 0.6f, Buoyancy = 0.5f, Spin = 1.2f,
            Size = new CelCurve(r0, r1, 0.5f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 3f), Feather = 0.08f,
            Palette = "gl_smoke", CoolTo = "gl_smoke_tan", CoolFrom = 0.4f, Core = 1f, Ink = 4f, Extra = new Vector4(2.1f, 0.24f, 0f, 0.45f),
        };

        public static readonly CelFxRecipe BoltsRecipe = new CelFxRecipe("green_laser_bolts", Length,
            Darts("darts", 0f, Bolts, 16, 8, 0.085f),
            new CelLayer
            {
                // the starburst on the target: thin green spikes facing the eye round a small white core
                Name = "hit star", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = Travel, Duration = Volley, Life = 0.2f, LifeJitter = 0.2f,
                Close = Bolts * 2, Mid = 18, Far = 8, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.2f, 0f, 0f), Size = new CelCurve(0.7f, 1.05f, 0.5f), SizeJitter = 0.25f, Spin = 2f,
                Erode = new CelCurve(0.02f, 0.8f, 1.5f), Palette = "gl_star", Core = 3f, Extra = new Vector4(2f, 1.3f, 11f, 12f),
            },
            new CelLayer
            {
                // a soft halo behind the stars: the bloom round the hit
                Name = "hit halo", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = Travel, Duration = Volley, Life = 0.35f, LifeJitter = 0.2f,
                Close = Bolts / 2, Mid = 6, Far = 0, Size = new CelCurve(0.7f, 1.0f, 0.5f), SizeJitter = 0.2f,
                Erode = new CelCurve(0.02f, 0.7f, 1.5f), Palette = "gl_halo", Core = 1f, Opacity = 0.45f, Extra = new Vector4(2f, 2.2f, 0f, 0f),
            },
            Spikes(0), Spikes(1), Spikes(2),
            new CelLayer { Name = "light a", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = Travel, Life = 1.6f, Close = 1, Mid = 1, Far = 1, Palette = "gl_bolt", Core = 3f, Extra = new Vector4(7f, 0f, 0f, 0f) },
            new CelLayer { Name = "light b", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = Travel + 1.4f, Life = 1.6f, Close = 1, Mid = 1, Far = 0, Palette = "gl_bolt", Core = 3f, Extra = new Vector4(7f, 0f, 0f, 0f) },
            new CelLayer { Name = "light c", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = Travel + 2.8f, Life = 1.6f, Close = 1, Mid = 1, Far = 0, Palette = "gl_bolt", Core = 3f, Extra = new Vector4(7f, 0f, 0f, 0f) });

        public static readonly CelFxRecipe Lane2Recipe = new CelFxRecipe("green_laser_lane2", Length,
            Darts("darts lane 2", 0.065f, Bolts, 10, 0, 0.08f));

        // Ground: origin = the dummy's foot, target = the ground burst point, dir = the eye's right (level)
        public static readonly CelFxRecipe GroundRecipe = new CelFxRecipe("green_laser_ground", Length,
            new CelLayer
            {
                // one soft wash under the feet: smooth radial falloff, no rings; in with the first hit, out by f115
                Name = "wash", Kind = BoltKind, Start = Travel, Life = 5.0f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(2.0f, 2.2f, 0.5f), SizeJitter = 0f,
                Palette = "gl_wash", Opacity = 3.0f, Extra = new Vector4(1f, 0.03f, 0.78f, 3.0f),
            },
            new CelLayer
            {
                // a smaller brighter flicker of the wash with the hits
                Name = "wash pulse", Kind = BoltKind, Start = Travel, Duration = Volley - 0.6f, Life = 0.4f, LifeJitter = 0.2f,
                Close = 12, Mid = 4, Far = 0, Size = new CelCurve(0.9f, 1.2f, 0.5f), SizeJitter = 0.2f,
                Palette = "gl_wash", Opacity = 1.2f, Extra = new Vector4(1f, 0.15f, 0.4f, 3.5f),
            },
            new CelLayer
            {
                Name = "burst wash", Kind = BoltKind, Anchor = CelAnchor.Target, Start = Travel + 0.8f, Duration = Volley - 1.4f, Life = 0.8f, LifeJitter = 0.2f,
                Close = 6, Mid = 3, Far = 0, Size = new CelCurve(0.8f, 1.0f, 0.5f), SizeJitter = 0.2f,
                Palette = "gl_wash", Opacity = 2.0f, Extra = new Vector4(1f, 0.1f, 0.5f, 3f),
            },
            new CelLayer
            {
                // secondary bursts on the ground right of the dummy: a thin star and a fountain of spikes
                Name = "burst star", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.25f, 0f), Start = Travel + 0.8f, Duration = Volley - 1.4f, Life = 0.22f, LifeJitter = 0.2f,
                Close = 10, Mid = 5, Far = 0, Size = new CelCurve(0.5f, 0.8f, 0.5f), SizeJitter = 0.25f, Spin = 1.5f,
                Erode = new CelCurve(0.02f, 0.8f, 1.5f), Palette = "gl_star", Core = 3f, Extra = new Vector4(2f, 1.3f, 9f, 10f),
            },
            new CelLayer
            {
                Name = "burst spikes", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = Travel + 0.8f, Duration = Volley - 1.4f, Life = 0.3f, LifeJitter = 0.3f,
                Close = 60, Mid = 20, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.25f, 0f, 0f), Aim = CelAim.Up, Speed = 7f, SpeedJitter = 0.5f, Spread = 35f, Drag = 3f, Gravity = 4f,
                Size = new CelCurve(0.05f, 0.015f), SizeJitter = 0.3f, Stretch = 0.07f, Erode = new CelCurve(0.05f, 0.5f), Palette = "gl_star", Core = 3f, Extra = new Vector4(0f, 0.25f, 0f, 0f),
            },
            new CelLayer
            {
                // dark dirt clods kicked up at the burst point and the feet (the reference's dark specks)
                Name = "dirt burst", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Start = Travel + 0.8f, Duration = Volley - 1.2f, Life = 0.9f, LifeJitter = 0.3f,
                Close = 24, Mid = 8, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.5f, 0f, 0f), Aim = CelAim.Up, Speed = 3.5f, SpeedJitter = 0.5f, Spread = 40f, Gravity = 9f, Drag = 0.5f, Spin = 2f,
                Size = new CelCurve(0.1f, 0.06f), SizeJitter = 0.4f, Stretch = 0.6f, Erode = new CelCurve(0f, 0.7f, 2f), Feather = 0f,
                Palette = "gl_dirt", Core = 1f, Ink = 2f, Extra = new Vector4(2.2f, 0.3f, 0.4f, 0f),
            },
            new CelLayer
            {
                Name = "dirt feet", Kind = CelKinds.LobeName, Start = Travel + 0.3f, Duration = Volley - 0.6f, Life = 0.7f, LifeJitter = 0.3f,
                Close = 14, Mid = 4, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.35f, 0f, 0f), Aim = CelAim.Up, Speed = 2.5f, SpeedJitter = 0.5f, Spread = 45f, Gravity = 9f, Drag = 0.5f, Spin = 2f,
                Size = new CelCurve(0.08f, 0.05f), SizeJitter = 0.4f, Stretch = 0.6f, Erode = new CelCurve(0f, 0.7f, 2f), Feather = 0f,
                Palette = "gl_dirt", Core = 1f, Ink = 2f, Extra = new Vector4(2.2f, 0.3f, 0.4f, 0f),
            },
            Smoke("smoke dummy", CelAnchor.Origin, new Vector3(0f, 1.3f, 0.2f), 1.4f, 3.0f, 1.3f, 16, 4, 0.15f, 0.34f),
            Smoke("smoke burst", CelAnchor.Target, new Vector3(0f, 0.5f, 0.3f), 1.7f, 2.8f, 1.3f, 18, 5, 0.16f, 0.4f),
            new CelLayer
            {
                // small green embers left glinting at the feet after the volley
                Name = "embers", Kind = CelKinds.StreakName, Offset = new Vector3(0f, 0.05f, 0f), Start = 2.4f, Duration = 2.4f, Life = 1.1f, LifeJitter = 0.3f,
                Close = 30, Mid = 8, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.7f, 0.15f, 0f), Aim = CelAim.Up, Speed = 0.4f, Spread = 50f, Drag = 1f, Gravity = 0.6f,
                Size = new CelCurve(0.05f, 0.03f), SizeJitter = 0.4f, Stretch = 0.02f, Erode = new CelCurve(0.05f, 0.6f, 2f), Palette = "gl_bolt", Core = 2.5f, Extra = new Vector4(1f, 0.05f, 0.6f, 0f),
            });

        /// <summary>Fire the volley at `chest`, standing on `foot`, seen from `eye`. The lanes start between `muzzle` (near,
        /// low) and `muzzleHigh` (far, level) so they climb as shallow as the reference's; the ground burst and the smoke's
        /// drift go to the eye's right.</summary>
        public static void Play(Vector3 muzzle, Vector3 muzzleHigh, Vector3 chest, Vector3 foot, Vector3 eye, uint seed)
        {
            Vector3 a = Vector3.Lerp(muzzle, muzzleHigh, Lane1), b = Vector3.Lerp(muzzle, muzzleHigh, Lane2);
            CelFx.Play(BoltsRecipe, a, chest - a, chest, seed);
            Vector3 hit2 = chest + Vector3.down * 0.25f;
            CelFx.Play(Lane2Recipe, b, hit2 - b, hit2, seed + 57u);
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
            Vector3 high = s.MuzzleHigh.sqrMagnitude > 0f ? s.MuzzleHigh : s.Muzzle;
            Play(s.Muzzle, high, s.Target, s.Ground, eye, (uint)s.Seed);
            CelFx.ZoomOverride = keep;
        }
    }
}
