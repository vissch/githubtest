// Phase: CelFx job (2026-11-09, lane/show/cel-green-laser) - green_laser: an anime energy-bolt volley (lab only). Volleys
// of thin green darts (small pale pointed heads, long tapering tails broken into ragged pieces) climb ~22 degrees in
// from the lower left in two lanes; each volley lands as one pale yellow-green spiky burst on the target with pale inked
// S-curling flame wisps above it (seven separate hits, the reference's rhythm), a soft pale green wash pulses on the
// dirt under it, large spiky ground bursts kick up to its right late in the volley, and pale yellow-green inked cel
// smoke drifts right, turning tan-grey before it fades, with green embers left at the feet. Reference: vfx_h3
// anime_laser_green_cel.mp4. Kinds: the kit's Streak, Glow and Lobe, plus this effect's GlBolt (darts, the ground wash
// and the flame wisps: one kind). Recipes: Bolts (lane 1 darts and everything on the target), Lane2 (the second lane's
// darts) and Ground (dummy foot -> burst point on the ground to the eye's right); GreenLaser.Play fires all three.
// Times: seconds after the fire; the Range films 0.05 s a frame and fires at frame 16, so reference frame f = 0.05 f - 0.8.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    /// <summary>The green laser's own primitive: darts, the smooth ground wash and its shade ring (Shaders/Cel/Effects/green_laser).</summary>
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
        // each volley's first dart lands at Hits[i] (reference frames 25, 37, 51, 61, 73, 86, 98); a dart flies Travel s
        public const float Travel = 0.45f, TravelLate = 0.6f, Gap = 0.12f, Length = 6.0f;
        static readonly float[] Hits = { 0.45f, 1.05f, 1.75f, 2.25f, 2.85f, 3.5f, 4.1f };
        static readonly float[] HitScale = { 0.85f, 0.95f, 0.9f, 1.0f, 1.25f, 1.25f, 1.2f };
        // the ground bursts right of the target (reference frames 34-85, biggest at 70-77)
        static readonly float[] Ground = { 0.9f, 1.6f, 2.25f, 2.7f, 3.05f, 3.45f };
        static readonly float[] GroundScale = { 0.6f, 0.85f, 0.7f, 1.2f, 1.3f, 1.1f };
        public const string BoltKind = "GlBolt";
        // where along muzzle (near, low) -> muzzle high (far, level) each lane starts: the reference's ~22 degree climb
        public const float Lane1 = 0.06f, Lane2 = -0.06f;
        // the darts are born this far along their path, at the frame's edge, so the first one shows on the fire frame
        public const float Enter = 0.07f;

        static GreenLaser()
        {
            // colours picked off the reference (spec/green_laser.json palette: #49763c #77b556 #d7f39c)
            CelPalette.Add("gl_bolt", CelPalette.Hex(0x2E8A1A), CelPalette.Hex(0x4FB82A), CelPalette.Hex(0x77CF42), CelPalette.Hex(0xA8E870), CelPalette.Hex(0xE8F8B8), 0.8f, 3, CelPalette.Hex(0x0C2A06));
            CelPalette.Add("gl_star", CelPalette.Hex(0x49763C), CelPalette.Hex(0x77B556), CelPalette.Hex(0xA8DC78), CelPalette.Hex(0xD7F39C), CelPalette.Hex(0xF0FFD0), 0.78f, 3, CelPalette.Hex(0x0C2A06));
            CelPalette.Add("gl_wash", CelPalette.Hex(0x14300F), CelPalette.Hex(0x1E4416), CelPalette.Hex(0x2C5E1E), CelPalette.Hex(0x55863A), CelPalette.Hex(0x9AD86A), 1.1f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("gl_halo", CelPalette.Hex(0x14300F), CelPalette.Hex(0x23521A), CelPalette.Hex(0x3A7A2A), CelPalette.Hex(0x5C9E40), CelPalette.Hex(0x9AD86A), 1.1f, 2, CelPalette.Hex(0x000000));
            CelPalette.Add("gl_flame", CelPalette.Hex(0x2F6A22), CelPalette.Hex(0x5FA845), CelPalette.Hex(0x8CCB5E), CelPalette.Hex(0xB4E07E), CelPalette.Hex(0xD7F39C), 0.85f, 3, CelPalette.Hex(0x0A1A06));
            CelPalette.Add("gl_smoke", CelPalette.Hex(0x5F9A45), CelPalette.Hex(0x77B556), CelPalette.Hex(0xA8D47A), CelPalette.Hex(0xD7F39C), CelPalette.Hex(0xE4F8B0), 1.1f, 3, CelPalette.Hex(0x0E160A));
            CelPalette.Add("gl_smoke_tan", CelPalette.Hex(0x6E6550), CelPalette.Hex(0x8A7F62), CelPalette.Hex(0xA3977A), CelPalette.Hex(0xBDB094), CelPalette.Hex(0xC8BC9E), 1.1f, 3, CelPalette.Hex(0x16130E));
            CelPalette.Add("gl_wisp", CelPalette.Hex(0x9ACB6A), CelPalette.Hex(0xB8E184), CelPalette.Hex(0xCDEE98), CelPalette.Hex(0xE0F6AE), CelPalette.Hex(0xF0FFD0), 0.92f, 4, CelPalette.Hex(0x0A1A06));
            CelPalette.Add("gl_dirt", CelPalette.Hex(0x0E0E0A), CelPalette.Hex(0x16150F), CelPalette.Hex(0x201E16), CelPalette.Hex(0x2C291E), CelPalette.Hex(0x2C291E), 1.1f, 3, CelPalette.Hex(0x040403));
        }

        // ---- the darts -------------------------------------------------------------------------------------------
        static float TravelOf(int i) => i == 0 ? Travel : TravelLate;

        static CelLayer Darts(string name, float start, float travel, int count, int mid, int far, float width) => new CelLayer
        {
            // a thin dart: small pointed pale head, long tapering green tail broken into ragged pieces; every dart of a lane on one line
            Name = name, Kind = BoltKind, Start = start, Duration = Mathf.Max(0f, Gap * (count - 1)), Life = travel * (1f - Enter), LifeJitter = 0f,
            Close = count, Mid = mid, Far = far, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f,
            Size = new CelCurve(width, width * 0.92f), SizeJitter = 0.08f, Palette = "gl_bolt", Core = 1.5f, Opacity = 1.3f,
            Extra = new Vector4(0f, 1.5f, 0.2f, 2.2f),
        };

        static CelLayer[] Lane1Layers()
        {
            var l = new List<CelLayer>();
            for (int i = 0; i < Hits.Length; i++)
                l.Add(Darts("darts " + i, Hits[i] - TravelOf(i) * (1f - Enter), TravelOf(i), 3, 1, (i & 1) == 0 ? 1 : 0, 0.085f));
            return l.ToArray();
        }

        static CelLayer[] Lane2Layers()
        {
            var l = new List<CelLayer>();
            for (int i = 0; i < Hits.Length; i++)
                l.Add(Darts("darts lane 2 " + i, Hits[i] - TravelOf(i) * (1f - Enter) + Gap * 0.5f, TravelOf(i), i == 2 || i == 6 ? 2 : 3, (i & 1) == 1 ? 1 : 0, 0, 0.08f));
            return l.ToArray();
        }

        // ---- the hit on the target: one burst per volley ----------------------------------------------------------
        static void HitLayers(List<CelLayer> l, int i)
        {
            float h = Hits[i], s = HitScale[i];
            l.Add(new CelLayer
            {
                // a pale yellow-green spiky burst facing the eye, about the dummy's height
                Name = "hit star " + i, Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = h, Duration = Gap * 2f, Life = 0.34f, LifeJitter = 0.15f,
                Close = 3, Mid = 2, Far = 1, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.12f, 0f, 0f), Size = new CelCurve(0.8f * s, 1.1f * s, 0.5f), SizeJitter = 0.2f, Spin = 1.2f,
                Erode = new CelCurve(0.04f, 0.75f, 1.6f), Palette = "gl_star", Core = 2.2f, Extra = new Vector4(2f, 0.75f, 9f, 3f),
            });
            l.Add(new CelLayer
            {
                Name = "hit halo " + i, Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = h, Life = 0.45f, LifeJitter = 0.1f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.8f * s, 1.1f * s, 0.5f), SizeJitter = 0.1f,
                Erode = new CelCurve(0.02f, 0.6f, 1.5f), Palette = "gl_halo", Core = 1f, Opacity = 0.4f, Extra = new Vector4(2f, 1.8f, 0f, 0f),
            });
            l.Add(new CelLayer
            {
                Name = "hit spikes " + i, Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = h, Duration = Gap * 2f, Life = 0.22f, LifeJitter = 0.3f,
                Close = 16, Mid = 6, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.15f, 0f, 0f), Aim = CelAim.Radial, Speed = 9f * s, SpeedJitter = 0.5f, UpBias = 0.3f, Drag = 4f,
                Size = new CelCurve(0.06f, 0.02f), SizeJitter = 0.3f, Stretch = 0.06f, Erode = new CelCurve(0.05f, 0.5f), Palette = "gl_star", Core = 2.5f, Extra = new Vector4(0f, 0.25f, 0f, 0f),
            });
            l.Add(new CelLayer
            {
                // two or three pale S-curling flame wisps rising off the hit, inked (GlBolt mode 3), about the dummy's height
                Name = "hit wisps " + i, Kind = BoltKind, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.1f, 0f), Start = h + 0.03f, Duration = 0.1f, Life = 0.55f, LifeJitter = 0.2f,
                Close = (i & 1) == 0 ? 3 : 2, Mid = 1, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.22f, 0f, 0f), Aim = CelAim.Up, Speed = 0.6f, SpeedJitter = 0.4f, Spread = 35f, Drag = 1f, Buoyancy = 0.3f,
                Size = new CelCurve(0.6f * s, 1.0f * s, 0.5f), SizeJitter = 0.25f, Palette = "gl_wisp", Core = 1f, Opacity = 1f, Extra = new Vector4(3f, 0.12f, 0.55f, 0.3f),
            });
        }

        static CelLayer[] BoltLayers()
        {
            var l = new List<CelLayer>(Lane1Layers());
            for (int i = 0; i < Hits.Length; i++) HitLayers(l, i);
            l.Add(new CelLayer { Name = "light a", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = Hits[0], Life = 1.6f, Close = 1, Mid = 1, Far = 1, Palette = "gl_bolt", Core = 3f, Extra = new Vector4(7f, 0f, 0f, 0f) });
            l.Add(new CelLayer { Name = "light b", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = Hits[3], Life = 1.6f, Close = 1, Mid = 1, Far = 0, Palette = "gl_bolt", Core = 3f, Extra = new Vector4(7f, 0f, 0f, 0f) });
            l.Add(new CelLayer { Name = "light c", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = Hits[5], Life = 1.0f, Close = 1, Mid = 1, Far = 0, Palette = "gl_bolt", Core = 3f, Extra = new Vector4(7f, 0f, 0f, 0f) });
            return l.ToArray();
        }

        // ---- the ground: origin = the dummy's foot, target = the burst point, dir = the eye's right --------------------
        static CelLayer Wash(string name, CelAnchor anchor, float start, float duration, int close, int mid, int far, float life, float r0, float r1, float opacity, Vector4 extra) => new CelLayer
        {
            Name = name, Kind = BoltKind, Anchor = anchor, Start = start, Duration = duration, Life = life, LifeJitter = 0.1f,
            Close = close, Mid = mid, Far = far, Size = new CelCurve(r0, r1, 0.5f), SizeJitter = 0.05f, Palette = "gl_wash", Opacity = opacity, Extra = extra,
        };

        static CelLayer Smoke(string name, CelAnchor anchor, Vector3 offset, float start, float duration, float life, int close, int mid, float r0, float r1, bool tan) => new CelLayer
        {
            // curly cel puffs in three flat bands with a dark ink line, rising and drifting right; the late ones are tan-grey
            Name = name, Kind = CelKinds.LobeName, Anchor = anchor, Offset = offset, Start = start, Duration = duration, Life = life, LifeJitter = 0.15f,
            Close = close, Mid = mid, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.35f, 0f, 0f), Aim = CelAim.Dir, Speed = 1.1f, Spread = 30f, Drag = 0.6f, Buoyancy = 0.45f, Spin = 1.2f,
            Size = new CelCurve(r0, r1, 0.5f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 4f), Feather = 0.03f,
            Palette = tan ? "gl_smoke_tan" : "gl_smoke", Core = 1f, Ink = 3f, Extra = new Vector4(1.2f, 0.09f, 0.4f, 0f),
        };

        static CelLayer[] GroundLayers()
        {
            var l = new List<CelLayer>
            {
                // one soft pale wash under the feet (the floor of the glow between the hits), no rim
                Wash("wash", CelAnchor.Origin, Hits[0], 0f, 1, 1, 1, 4.4f, 1.7f, 1.8f, 1.7f, new Vector4(1f, 0.05f, 0.75f, 2.2f)),
            };
            for (int i = 0; i < Hits.Length; i++)
            {
                float h = Hits[i], s = HitScale[i];
                // the wash flares a little brighter with every hit
                l.Add(Wash("wash pulse " + i, CelAnchor.Origin, h, 0f, 1, 1, 0, 0.4f, 1.6f * s, 1.75f * s, 1.1f, new Vector4(1f, 0.08f, 0.3f, 2.2f)));
            }
            for (int i = 0; i < Ground.Length; i++)
            {
                float g = Ground[i], s = GroundScale[i];
                l.Add(new CelLayer
                {
                    // a large spiky green burst on the ground right of the dummy
                    Name = "burst star " + i, Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.35f * s, 0f), Start = g, Duration = 0.08f, Life = 0.32f, LifeJitter = 0.15f,
                    Close = 2, Mid = 1, Far = 0, Size = new CelCurve(0.8f * s, 1.15f * s, 0.5f), SizeJitter = 0.2f, Spin = 1.5f,
                    Erode = new CelCurve(0.04f, 0.75f, 1.5f), Palette = "gl_star", Core = 2.2f, Extra = new Vector4(2f, 0.8f, 11f, 4f),
                });
                l.Add(new CelLayer
                {
                    Name = "burst spikes " + i, Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = g, Duration = 0.1f, Life = 0.32f, LifeJitter = 0.3f,
                    Close = s > 0.8f ? 26 : 10, Mid = s > 0.8f ? 8 : 0, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.25f, 0f, 0f), Aim = CelAim.Up, Speed = 8f * s, SpeedJitter = 0.5f, Spread = 40f, Drag = 3f, Gravity = 4f,
                    Size = new CelCurve(0.06f, 0.02f), SizeJitter = 0.3f, Stretch = 0.07f, Erode = new CelCurve(0.05f, 0.5f), Palette = "gl_star", Core = 2.5f, Extra = new Vector4(0f, 0.25f, 0f, 0f),
                });
                l.Add(Wash("burst wash " + i, CelAnchor.Target, g, 0f, 1, 1, 0, 0.5f, 1.2f * s, 1.35f * s, 0.9f, new Vector4(1f, 0.1f, 0.45f, 3.0f)));
                // a dark scorch splat left on the dirt where a big burst lands (the reference's black craters), gone by ~f130
                if (s > 0.8f) l.Add(Wash("burst scorch " + i, CelAnchor.Target, g, 0f, 1, 0, 0, 5.7f - g, 0.75f * s, 0.85f * s, 0.8f, new Vector4(2f, 0.05f, 0.7f, 0.05f)));
                l.Add(new CelLayer
                {
                    // dark dirt clods kicked up by the burst (the reference's dark specks)
                    Name = "dirt burst " + i, Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Start = g, Duration = 0.1f, Life = 0.8f, LifeJitter = 0.3f,
                    Close = s > 0.8f ? 16 : 5, Mid = s > 0.8f ? 3 : 0, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.4f, 0f, 0f), Aim = CelAim.Up, Speed = 4f * s, SpeedJitter = 0.5f, Spread = 40f, Gravity = 9f, Drag = 0.5f, Spin = 2f,
                    Size = new CelCurve(0.1f, 0.06f), SizeJitter = 0.4f, Stretch = 0.6f, Erode = new CelCurve(0f, 0.7f, 2f), Feather = 0f,
                    Palette = "gl_dirt", Core = 1f, Ink = 2f, Extra = new Vector4(2.2f, 0.3f, 0.4f, 0f),
                });
            }
            l.Add(new CelLayer
            {
                Name = "dirt feet", Kind = CelKinds.LobeName, Start = Hits[0], Duration = Hits[Hits.Length - 1] - Hits[0], Life = 0.7f, LifeJitter = 0.3f,
                Close = 14, Mid = 4, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.35f, 0f, 0f), Aim = CelAim.Up, Speed = 2.5f, SpeedJitter = 0.5f, Spread = 45f, Gravity = 9f, Drag = 0.5f, Spin = 2f,
                Size = new CelCurve(0.08f, 0.05f), SizeJitter = 0.4f, Stretch = 0.6f, Erode = new CelCurve(0f, 0.7f, 2f), Feather = 0f,
                Palette = "gl_dirt", Core = 1f, Ink = 2f, Extra = new Vector4(2.2f, 0.3f, 0.4f, 0f),
            });
            // smoke: green while the volley lasts (gone by ~f106), then tan-grey puffs from ~f90 that drift right, gone by ~f130
            l.Add(Smoke("smoke dummy", CelAnchor.Origin, new Vector3(0f, 1.3f, 0.2f), 1.5f, 2.0f, 0.95f, 12, 2, 0.2f, 0.42f, false));
            l.Add(Smoke("smoke dummy late", CelAnchor.Origin, new Vector3(0f, 1.2f, 0.2f), 3.6f, 1.1f, 1.0f, 12, 3, 0.18f, 0.38f, true));
            l.Add(Smoke("smoke burst", CelAnchor.Target, new Vector3(0f, 0.5f, 0.3f), 1.9f, 1.6f, 1.0f, 12, 2, 0.2f, 0.45f, false));
            l.Add(Smoke("smoke burst late", CelAnchor.Target, new Vector3(0f, 0.55f, 0.3f), 3.6f, 1.1f, 1.05f, 10, 3, 0.16f, 0.36f, true));
            l.Add(new CelLayer
            {
                // small green embers glinting at the feet after the volley (reference frames 112-125)
                Name = "embers", Kind = CelKinds.StreakName, Offset = new Vector3(0f, 0.05f, 0f), Start = 4.55f, Duration = 0.55f, Life = 0.6f, LifeJitter = 0.3f,
                Close = 26, Mid = 8, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.6f, 0.1f, 0f), Aim = CelAim.Up, Speed = 0.4f, Spread = 50f, Drag = 1f, Gravity = 0.6f,
                Size = new CelCurve(0.05f, 0.03f), SizeJitter = 0.4f, Stretch = 0.02f, Erode = new CelCurve(0.05f, 0.6f, 2f), Palette = "gl_bolt", Core = 2.2f, Extra = new Vector4(1f, 0.05f, 0.6f, 0f),
            });
            return l.ToArray();
        }

        public static readonly CelFxRecipe BoltsRecipe = new CelFxRecipe("green_laser_bolts", Length, BoltLayers());
        public static readonly CelFxRecipe Lane2Recipe = new CelFxRecipe("green_laser_lane2", Length, Lane2Layers());
        public static readonly CelFxRecipe GroundRecipe = new CelFxRecipe("green_laser_ground", Length, GroundLayers());

        /// <summary>Fire the volley at `chest`, standing on `foot`, seen from `eye`. The lanes start between `muzzle` (near,
        /// low) and `muzzleHigh` (far, level) so they climb as steeply as the reference's, already at the frame's edge;
        /// the ground burst and the smoke's drift go to the eye's right.</summary>
        public static void Play(Vector3 muzzle, Vector3 muzzleHigh, Vector3 chest, Vector3 foot, Vector3 eye, uint seed)
        {
            Vector3 hit2 = chest + Vector3.down * 0.25f;
            Vector3 a = Vector3.Lerp(Vector3.LerpUnclamped(muzzle, muzzleHigh, Lane1), chest, Enter);
            Vector3 b = Vector3.Lerp(Vector3.LerpUnclamped(muzzle, muzzleHigh, Lane2), hit2, Enter);
            CelFx.Play(BoltsRecipe, a, chest - a, chest, seed);
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
