// Phase: CelFx job (2026-11-09, lane/show/cel-green-laser) - green_laser: an anime energy-bolt volley (lab only). Volleys
// of solid green spears (white-hot pointed heads, smooth unbroken tails tapering to nothing, soft bloom) climb in
// from the lower left in two lanes; each volley lands as one pale yellow-green spiky burst on the target with pale inked
// S-curling flame wisps above it (seven separate hits, the reference's rhythm), a soft pale green wash pulses on the
// dirt under it, large spiky ground bursts kick up to its right late in the volley, and pale yellow-green inked cel
// smoke drifts right, turning tan-grey before it fades, with green embers left at the feet. Reference: vfx_h3
// anime_laser_green_cel.mp4. Kinds: the kit's Streak and Glow, plus this effect's GlBolt (darts, the ground wash and
// shade, the inked stars, flame wisps and translucent smoke puffs: one kind). Recipes: Bolts (lane 1 darts and everything on the target), Lane2 (the second lane's
// darts) and Ground (dummy foot -> burst point on the ground to the eye's right); GreenLaser.Play fires all three.
// Times: seconds after the fire; the beach Range films 24 fps and fires at 0.8 s, so reference frame f = f / 24 - 0.8.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    /// <summary>The green laser's own primitive: darts, ground wash and shade, inked stars, wisps and puffs (Shaders/Cel/Effects/green_laser).</summary>
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
        // each volley's first dart lands at Hits[i] (reference frames 25, 37, 51, 61, 73, 86, 98, 106); a dart flies Travel s
        public const float Travel = 0.28f, TravelLate = 0.4f, Gap = 0.18f, Length = 5.0f;
        static readonly float[] Hits = { 0.26f, 0.74f, 1.33f, 1.74f, 2.24f, 2.78f, 3.28f, 3.62f };
        static readonly float[] HitScale = { 0.8f, 0.9f, 0.9f, 1.0f, 1.35f, 1.1f, 0.95f, 0.85f };
        // the ground bursts right of the target (reference frames 34-85, biggest at 70-77)
        static readonly float[] Ground = { 0.66f, 1.70f, 2.40f, 2.74f, 3.07f };
        static readonly float[] GroundScale = { 0.7f, 0.85f, 1.25f, 1.1f, 1.0f };
        public const string BoltKind = "GlBolt";
        // where along muzzle (near, low) -> muzzle high (far, level) each lane starts: the reference's ~22 degree climb
        // (measured on the reference: both lanes enter the frame's left edge ~5/6 down and climb ~15 degrees on screen)
        public const float Lane1 = 0.16f, Lane2 = 0.10f;
        // lane 1 lands a little under the chest, lane 2 lower on the board (the reference's lines meet the dummy's belly)
        public const float Drop1 = 0.3f, Drop2 = 0.6f;
        // the darts are born this far along their path, at the frame's edge, so the first one shows on the fire frame
        public const float Enter = 0.07f;

        static GreenLaser()
        {
            // colours picked off the reference (spec/green_laser.json palette: #49763c #77b556 #d7f39c)
            CelPalette.Add("gl_bolt", CelPalette.Hex(0x2E8A1A), CelPalette.Hex(0x4FB82A), CelPalette.Hex(0x77CF42), CelPalette.Hex(0xD7F39C), CelPalette.Hex(0xE6FFB0), 0.95f, 3, CelPalette.Hex(0x0C2A06));
            CelPalette.Add("gl_star", CelPalette.Hex(0x49763C), CelPalette.Hex(0x77B556), CelPalette.Hex(0xA8DC78), CelPalette.Hex(0xD7F39C), CelPalette.Hex(0xF0FFD0), 0.93f, 3, CelPalette.Hex(0x0A1A06));
            CelPalette.Add("gl_wash", CelPalette.Hex(0x14260F), CelPalette.Hex(0x1A3414), CelPalette.Hex(0x3E6A32), CelPalette.Hex(0x77B556), CelPalette.Hex(0xA4D088), 1.1f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("gl_haze", CelPalette.Hex(0x1C2C18), CelPalette.Hex(0x3E5A36), CelPalette.Hex(0x5E8050), CelPalette.Hex(0x9CC88A), CelPalette.Hex(0xB4D8A0), 1.1f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("gl_pulse", CelPalette.Hex(0x14300F), CelPalette.Hex(0x163A10), CelPalette.Hex(0x2C5E1E), CelPalette.Hex(0x77B556), CelPalette.Hex(0x86BE66), 1.1f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("gl_halo", CelPalette.Hex(0x14300F), CelPalette.Hex(0x2C5E20), CelPalette.Hex(0x4E8E3A), CelPalette.Hex(0x77B556), CelPalette.Hex(0xB5E07A), 1.1f, 6, CelPalette.Hex(0x000000));
            CelPalette.Add("gl_flame", CelPalette.Hex(0x2F6A22), CelPalette.Hex(0x5FA845), CelPalette.Hex(0x8CCB5E), CelPalette.Hex(0xB4E07E), CelPalette.Hex(0xD7F39C), 0.85f, 3, CelPalette.Hex(0x0A1A06));
            // d01: smoke colours measured on the reference's smoke cluster (f101 green-lit: dark #2c5430, mid #559c47,
            // light #b1e986; f118 cooled: #373b35 / #5a5d48 / #71825e), no hot core
            CelPalette.Add("gl_smoke", CelPalette.Hex(0x4F8A42), CelPalette.Hex(0x6FAE55), CelPalette.Hex(0x95CF72), CelPalette.Hex(0xB4E48A), CelPalette.Hex(0xB4E48A), 1.1f, 3, CelPalette.Hex(0x0E160A));
            CelPalette.Add("gl_smoke_tan", CelPalette.Hex(0x6E6550), CelPalette.Hex(0x8A7F62), CelPalette.Hex(0xA3977A), CelPalette.Hex(0xBDB094), CelPalette.Hex(0xC8BC9E), 1.1f, 3, CelPalette.Hex(0x16130E));
            CelPalette.Add("gl_wisp", CelPalette.Hex(0xBEDD96), CelPalette.Hex(0xCDE8A6), CelPalette.Hex(0xD7F39C), CelPalette.Hex(0xE4F6BA), CelPalette.Hex(0xF0FFD0), 0.98f, 4, CelPalette.Hex(0x0A1A06));
            CelPalette.Add("gl_burst", CelPalette.Hex(0x3F8A2A), CelPalette.Hex(0x6CC24A), CelPalette.Hex(0x9FE07A), CelPalette.Hex(0xB8EE98), CelPalette.Hex(0xE2FFD0), 0.93f, 3, CelPalette.Hex(0x0A1A06));
            CelPalette.Add("gl_smoke_grey", CelPalette.Hex(0x5A5E4C), CelPalette.Hex(0x70755C), CelPalette.Hex(0x888D70), CelPalette.Hex(0xA0A682), CelPalette.Hex(0xA0A682), 1.1f, 3, CelPalette.Hex(0x0E140A));
            CelPalette.Add("gl_dirt", CelPalette.Hex(0x0E0E0A), CelPalette.Hex(0x16150F), CelPalette.Hex(0x201E16), CelPalette.Hex(0x2C291E), CelPalette.Hex(0x2C291E), 1.1f, 3, CelPalette.Hex(0x040403));
                    }

        // ---- the darts -------------------------------------------------------------------------------------------
        static float TravelOf(int i) => i == 0 ? Travel : TravelLate;

        static CelLayer Darts(string name, float start, float travel, int count, int mid, int far, float width) => new CelLayer
        {
            // one solid spear: a pointed white-hot head, a smooth unbroken tail ~5 head lengths long tapering to nothing,
            // a soft green bloom round it; the darts of a lane follow each other on one line, a spear's length apart
            Name = name, Kind = BoltKind, Start = start, Duration = Mathf.Max(0f, Gap * (count - 1)), Life = travel * (1f - Enter), LifeJitter = 0f,
            Close = count, Mid = mid, Far = far, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.2f, 0f, 0f), Aim = CelAim.Target, Speed = 0f, Spread = 0f,
            Size = new CelCurve(width, width * 0.95f), SizeJitter = 0.06f, Palette = "gl_bolt", Core = 1.0f, Opacity = 1.0f,
            Extra = new Vector4(0f, 2.5f, 0.45f, 4.0f),
        };

        // the first volley is already in the air when the shot fires (the reference's first spear shows ~6 frames
        // before our fire frame): darts born part-way along the path, `along` metres past the frame's edge
        static CelLayer InFlight(string name, float along, float life, float width)
        {
            var d = Darts(name, 0f, life / (1f - Enter), 1, 1, 0, width);
            d.Offset = new Vector3(0f, 0f, along);
            return d;
        }

        static CelLayer[] Lane1Layers()
        {
            var l = new List<CelLayer>();
            l.Add(InFlight("darts in flight a", 6.0f, 0.13f, 0.26f));
            l.Add(InFlight("darts in flight b", 2.6f, 0.22f, 0.26f));
            for (int i = 0; i < Hits.Length; i++)
                l.Add(Darts("darts " + i, Hits[i] - TravelOf(i) * (1f - Enter), TravelOf(i), 3, 1, (i & 1) == 0 ? 1 : 0, 0.26f));
            return l.ToArray();
        }

        static CelLayer[] Lane2Layers()
        {
            var l = new List<CelLayer>();
            l.Add(InFlight("darts lane 2 in flight", 4.2f, 0.2f, 0.24f));
            for (int i = 0; i < Hits.Length; i++)
                l.Add(Darts("darts lane 2 " + i, Hits[i] - TravelOf(i) * (1f - Enter) + Gap * 0.5f, TravelOf(i), i == 2 || i == 6 ? 2 : 3, (i & 1) == 1 ? 1 : 0, 0, 0.24f));
            return l.ToArray();
        }

        // ---- the hit on the target: one burst per volley ----------------------------------------------------------
        static void HitLayers(List<CelLayer> l, int i)
        {
            float h = Hits[i], s = HitScale[i];
            l.Add(new CelLayer
            {
                // an inked spiky starburst facing the eye (GlBolt mode 5): a small white core about 1/4 of the burst,
                // pale hub, green spikes of uneven length, dark ink round every spike; about the dummy's height
                Name = "hit star " + i, Kind = BoltKind, Anchor = CelAnchor.Target, Start = h, Duration = 0.13f, Life = 0.12f, LifeJitter = 0.3f,
                Close = 3, Mid = 1, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.25f, 0f, 0f), Size = new CelCurve(1.2f * s, 1.7f * s, 0.4f), SizeJitter = 0.35f,
                Palette = "gl_burst", Core = 0.7f, Opacity = 1f, Extra = new Vector4(5f, 0.15f, 0.4f, 13f),
            });
            l.Add(new CelLayer
            {
                Name = "hit halo " + i, Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = h, Life = 0.24f, LifeJitter = 0.1f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(2.0f * s, 2.5f * s, 0.4f), SizeJitter = 0.1f,
                Palette = "gl_halo", Core = 1f, Opacity = 0.6f, Extra = new Vector4(2f, 3.0f, 0f, 0f),
            });
            l.Add(new CelLayer
            {
                Name = "hit spikes " + i, Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = h, Duration = Gap * 2f, Life = 0.2f, LifeJitter = 0.3f,
                Close = 10, Mid = 4, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.2f, 0f, 0f), Aim = CelAim.Radial, Speed = 12f * s, SpeedJitter = 0.5f, UpBias = 0.3f, Drag = 4f,
                Size = new CelCurve(0.08f, 0.025f), SizeJitter = 0.3f, Stretch = 0.06f, Erode = new CelCurve(0.05f, 0.5f), Palette = "gl_star", Core = 1.0f, Extra = new Vector4(0f, 0.25f, 0f, 0f),
            });
            l.Add(new CelLayer
            {
                // two or three pale S-curling flame wisps rising off the hit, inked (GlBolt mode 3), about the dummy's height
                Name = "hit wisps " + i, Kind = BoltKind, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.15f, 0f), Start = h + 0.03f, Duration = 0.1f, Life = 0.42f, LifeJitter = 0.2f,
                Close = (i & 1) == 0 ? 3 : 2, Mid = 1, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Up, Speed = 0.75f, SpeedJitter = 0.4f, Spread = 35f, Drag = 1f, Buoyancy = 0.3f,
                Size = new CelCurve(0.9f * s, 1.4f * s, 0.5f), SizeJitter = 0.25f, Palette = "gl_flame", Core = 1f, Opacity = 0.85f, Extra = new Vector4(3f, 0.12f, 0.55f, 0.3f),
            });
        }

        static CelLayer[] BoltLayers()
        {
            var l = new List<CelLayer>(Lane1Layers());
            for (int i = 0; i < Hits.Length; i++) HitLayers(l, i);
            l.Add(new CelLayer { Name = "light a", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = Hits[0], Life = 1.35f, Close = 1, Mid = 1, Far = 1, Palette = "gl_bolt", Core = 1.6f, Extra = new Vector4(9f, 0f, 0f, 0f) });
            l.Add(new CelLayer { Name = "light b", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = Hits[3], Life = 1.35f, Close = 1, Mid = 1, Far = 0, Palette = "gl_bolt", Core = 1.6f, Extra = new Vector4(9f, 0f, 0f, 0f) });
            l.Add(new CelLayer { Name = "light c", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = Hits[5], Life = 0.85f, Close = 1, Mid = 1, Far = 0, Palette = "gl_bolt", Core = 1.6f, Extra = new Vector4(9f, 0f, 0f, 0f) });
            return l.ToArray();
        }

        // ---- the ground: origin = the dummy's foot, target = the burst point, dir = the eye's right --------------------
        static CelLayer Wash(string name, CelAnchor anchor, float start, float duration, int close, int mid, int far, float life, float r0, float r1, float opacity, Vector4 extra) => new CelLayer
        {
            Name = name, Kind = BoltKind, Anchor = anchor, Start = start, Duration = duration, Life = life, LifeJitter = 0.1f,
            Close = close, Mid = mid, Far = far, Size = new CelCurve(r0, r1, 0.5f), SizeJitter = 0.05f, Palette = "gl_wash", Opacity = opacity, Extra = extra,
        };

        // lifted `up` and moved `right` metres to the eye's right (the Ground recipe's dir is the eye's right)
        static CelLayer Shifted(CelLayer l, float up, float right) { l.Offset = new Vector3(0f, up, right); return l; }

        static CelLayer Smoke(string name, CelAnchor anchor, Vector3 offset, float start, float duration, float life, int close, int mid, float r0, float r1, bool tan, float spread = 0.7f) => new CelLayer
        {
            // d01: a CLUSTER of many small translucent lumpy cel puffs (GlBolt mode 4; the reference's cloud is a heap of
            // cauliflower lumps each ~1/6 of the cloud, not one or two big blobs), in flat unlit bands with a dark ink
            // line, rising and drifting right; the late ones olive-grey, dissolving into holes
            Name = name, Kind = BoltKind, Anchor = anchor, Offset = offset, Start = start, Duration = duration, Life = life, LifeJitter = 0.25f,
            Close = close, Mid = mid, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(spread, 0f, 0f), Aim = CelAim.Dir, Speed = tan ? 0.9f : 0.8f, SpeedJitter = 0.5f, Spread = 50f, Drag = 0.8f, Buoyancy = tan ? 0.9f : 0.5f, Spin = 1.2f,
            Size = new CelCurve(r0, r1, 0.5f), SizeJitter = 0.35f,
            Palette = tan ? "gl_smoke_grey" : "gl_smoke", Core = 1f, Opacity = tan ? 0.8f : 0.85f, Extra = new Vector4(4f, 0.12f, 0.45f, 0.13f),
        };

        static CelLayer[] GroundLayers()
        {
            var l = new List<CelLayer>
            {
                // one soft pale wash under the feet, about 2.5x the dummy's height wide, held through ~f115: a pale
                // core plus a wide dim deep-green spill on the darkened dirt, no rim
                Shifted(Wash("wash", CelAnchor.Origin, Hits[0], 0f, 1, 1, 1, 3.9f, 1.4f, 1.5f, 0.9f, new Vector4(1f, 0.05f, 0.9f, 2.0f)), 0.45f, 0.3f),
                // d03: the ref's "pool" is mostly a wide soft pale-green MIST standing round the dummy (f54-f87), not a ground slab: a big flat flash facing the eye centred on the torso; the same light seen as a soft pale haze standing over the feet (the reference's wash rises well above the
                // dirt): a flat flash facing the eye, about 2.5x the dummy's height wide
                new CelLayer
                {
                    Name = "wash haze", Kind = BoltKind, Anchor = CelAnchor.Origin, Offset = new Vector3(0f, 1.1f, 0.1f), Start = Hits[0], Life = 3.9f, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(4.2f, 4.4f, 0.5f), SizeJitter = 0f, Palette = "gl_haze", Core = 1f, Opacity = 0.95f, Extra = new Vector4(6f, 0.05f, 0.85f, 0.6f),
                },
                // the dirt round the glow goes dark (the reference's night ground round the light), a smooth disc, no ring
                new CelLayer
                {
                    Name = "wash shade", Kind = BoltKind, Anchor = CelAnchor.Origin, Offset = new Vector3(0f, 0.45f, 0.3f), Start = Hits[0], Life = 3.9f, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(2.4f, 2.5f, 0.5f), SizeJitter = 0f, Palette = "gl_wash", Opacity = 0.6f, Extra = new Vector4(2f, 0.08f, 0.88f, 0.01f),
                },
            };
            for (int i = 0; i < Hits.Length; i++)
            {
                float h = Hits[i], s = HitScale[i];
                // the wash flares with every hit: a short wide pulse (one area spike per hit)
                var pulse = Shifted(Wash("wash pulse " + i, CelAnchor.Origin, h, 0f, 1, 1, 0, 0.17f, 3.4f * s, 3.8f * s, 1.6f, new Vector4(1f, 0.05f, 0.35f, 2.2f)), 0.45f, 0.6f);
                pulse.Palette = "gl_pulse";
                l.Add(pulse);
                // d03: the mist flares with every hit too (an upright soft flash round the dummy, desaturated)
                l.Add(new CelLayer
                {
                    Name = "mist pulse " + i, Kind = BoltKind, Anchor = CelAnchor.Origin, Offset = new Vector3(0f, 1.1f, 0.1f), Start = h, Life = 0.22f, LifeJitter = 0.1f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(4.0f * s, 4.6f * s, 0.5f), SizeJitter = 0.05f, Palette = "gl_haze", Core = 1f, Opacity = 0.6f, Extra = new Vector4(6f, 0.0f, 0.4f, 0.6f),
                });
            }
            for (int i = 0; i < Ground.Length; i++)
            {
                float g = Ground[i], s = GroundScale[i];
                l.Add(new CelLayer
                {
                    // d02: a SMALL ragged inked green star at the burst foot (was 1.6-2.2 m: 2-3x the ref f79), three overlapping
                    Name = "burst star " + i, Kind = BoltKind, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.3f * s, 0f), Start = g, Duration = 0.1f, Life = 0.16f, LifeJitter = 0.3f,
                    Close = 3, Mid = 1, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.2f, 0f, 0f), Size = new CelCurve(0.5f * s, 0.75f * s, 0.4f), SizeJitter = 0.4f,
                    Palette = "gl_burst", Core = 0.5f, Opacity = 1f, Extra = new Vector4(5f, 0.15f, 0.4f, 12f),
                });
                l.Add(new CelLayer
                {
                    Name = "burst spikes " + i, Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = g, Duration = 0.1f, Life = 0.27f, LifeJitter = 0.3f,
                    Close = s > 0.8f ? 30 : 14, Mid = s > 0.8f ? 6 : 0, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.2f, 0f, 0f), Aim = CelAim.Up, Speed = 11f * s, SpeedJitter = 0.6f, Spread = 28f, Drag = 3f, Gravity = 6f,
                    Size = new CelCurve(0.05f, 0.015f), SizeJitter = 0.4f, Stretch = 0.1f, Erode = new CelCurve(0.05f, 0.5f), Palette = "gl_burst", Core = 1.0f, Extra = new Vector4(0f, 0.25f, 0f, 0f),
                });
                // a white-hot flash at the burst's heart (the reference's bright ground starburst core)
                l.Add(new CelLayer
                {
                    Name = "burst flash " + i, Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.25f * s, 0f), Start = g, Life = 0.16f, LifeJitter = 0.1f,
                    Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.45f * s, 0.6f * s, 0.4f), SizeJitter = 0.1f,
                    Palette = "gl_burst", Core = 1.0f, Opacity = 0.6f, Extra = new Vector4(2f, 3.0f, 0f, 0f),
                });
                l.Add(Wash("burst wash " + i, CelAnchor.Target, g, 0f, 1, 1, 0, 0.3f, 1.5f * s, 1.7f * s, 1.0f, new Vector4(1f, 0.05f, 0.4f, 2.4f)));
                // a dark scorch splat left on the dirt where a big burst lands (the reference's black craters), gone by ~f130
                if (s > 0.8f) l.Add(Wash("burst scorch " + i, CelAnchor.Target, g, 0f, 1, 0, 0, 4.1f - g, 1.0f * s, 1.1f * s, 0.7f, new Vector4(2f, 0.05f, 0.7f, 0.01f)));
            }
            // smoke: translucent pale green puffs while the volley lasts (gone by ~f106), then translucent tan-grey puffs
            // from ~f90 that drift right, gone by ~f130
            l.Add(Smoke("smoke dummy", CelAnchor.Origin, new Vector3(0f, 1.9f, 0.4f), 1.4f, 1.4f, 0.7f, 8, 1, 0.3f, 0.5f, false, 0.5f));
            l.Add(Smoke("smoke dummy late", CelAnchor.Origin, new Vector3(0f, 1.3f, 0.8f), 3.1f, 0.5f, 1.0f, 10, 1, 0.36f, 0.6f, true, 0.7f));
            l.Add(Smoke("smoke burst", CelAnchor.Target, new Vector3(0f, 0.8f, 0.3f), 1.8f, 1.0f, 0.8f, 12, 1, 0.34f, 0.55f, false, 0.8f));
            // the big late cloud right of the dummy: green-lit lumps first, then olive-grey ones (ref f95-f126), about 2x the dummy tall
            l.Add(Smoke("smoke burst lit", CelAnchor.Target, new Vector3(0f, 1.0f, 0.2f), 2.95f, 0.45f, 0.8f, 14, 1, 0.42f, 0.66f, false, 1.1f));
            l.Add(Smoke("smoke burst late", CelAnchor.Target, new Vector3(0f, 1.0f, 0.3f), 3.25f, 0.5f, 1.05f, 18, 2, 0.42f, 0.7f, true, 1.2f));
            l.Add(new CelLayer
            {
                // small green embers glinting at the feet after the volley (reference frames 112-125)
                Name = "embers", Kind = CelKinds.StreakName, Offset = new Vector3(0f, 0.05f, 0f), Start = 3.95f, Duration = 0.45f, Life = 0.55f, LifeJitter = 0.3f,
                Close = 30, Mid = 8, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.9f, 0.1f, 0f), Aim = CelAim.Up, Speed = 0.5f, Spread = 50f, Drag = 1f, Gravity = 0.6f,
                Size = new CelCurve(0.09f, 0.05f), SizeJitter = 0.4f, Stretch = 0.02f, Erode = new CelCurve(0.05f, 0.6f, 2f), Palette = "gl_bolt", Core = 1.8f, Extra = new Vector4(1f, 0.05f, 0.6f, 0f),
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
            Vector3 hit1 = chest + Vector3.down * Drop1, hit2 = chest + Vector3.down * Drop2;
            Vector3 a = Vector3.Lerp(Vector3.LerpUnclamped(muzzle, muzzleHigh, Lane1), hit1, Enter);
            Vector3 b = Vector3.Lerp(Vector3.LerpUnclamped(muzzle, muzzleHigh, Lane2), hit2, Enter);
            CelFx.Play(BoltsRecipe, a, hit1 - a, hit1, seed);
            CelFx.Play(Lane2Recipe, b, hit2 - b, hit2, seed + 57u);
            Vector3 look = foot - eye; look.y = 0f;
            if (look.sqrMagnitude < 1e-4f) look = chest - muzzle;
            look.y = 0f;
            look = look.sqrMagnitude > 1e-6f ? look.normalized : Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, look).normalized;
            Vector3 burst = foot + right * 2.9f + look * 0.3f;
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
