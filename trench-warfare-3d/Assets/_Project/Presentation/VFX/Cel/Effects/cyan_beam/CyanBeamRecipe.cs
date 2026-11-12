// Phase: CelFx cyan-beam lab effect, the owner's green charge and sustained cyan reference.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    /// <summary>Lab-only energy beam: green charge, held cyan shaft, impact spikes, then drifting sparks.</summary>
    public static class CyanBeamRecipe
    {
        public const float BeamStart = 0.25f;
        public const float BeamLife = 5.0f;
        public const float BeamEnd = BeamStart + BeamLife;

        public static readonly int BeamRow = CelPalette.Add("cyan_beam_shaft",
            CelPalette.Hex(0x3061EA), CelPalette.Hex(0x267CFF), CelPalette.Hex(0x54F5F7), CelPalette.Hex(0x79FAFF),
            new Color(0.64f, 1f, 1.55f), 0.4f, 4, CelPalette.Hex(0x03111B));
        public static readonly int HitRow = CelPalette.Add("cyan_beam_hit",
            CelPalette.Hex(0x159FC4), CelPalette.Hex(0x45EAF2), CelPalette.Hex(0x85FFFF), CelPalette.Hex(0xC4FFFF),
            new Color(0.64f, 1f, 1.55f), 0.55f, 4, CelPalette.Hex(0x03151C));
        public static readonly int SparkRow = CelPalette.Add("cyan_beam_spark",
            CelPalette.Hex(0xD54B08), CelPalette.Hex(0xFF9D2D), CelPalette.Hex(0xFFC56B), CelPalette.Hex(0xFFE6A5),
            CelPalette.Hex(0xFFE0A0), 0.65f, 4, CelPalette.Hex(0x561A04));
        public static readonly int ChargeRow = CelPalette.Add("cyan_beam_charge",
            CelPalette.Hex(0x178A2A), CelPalette.Hex(0x4DFF7B), CelPalette.Hex(0x8AFFAF), CelPalette.Hex(0xCBFFDC),
            CelPalette.Hex(0xF8FFF0), 0.38f, 4, CelPalette.Hex(0x103C17));
        public static readonly int PoolRow = CelPalette.Add("cyan_beam_pool",
            CelPalette.Hex(0x065F78), CelPalette.Hex(0x158B9C), CelPalette.Hex(0x22B9C5), CelPalette.Hex(0x55DFDF),
            CelPalette.Hex(0xBCFFFF), 0.95f, 4, CelPalette.Hex(0x000000));

        static CelLayer BeamLayer(string name, string palette, float start, float life, float width, float arcs)
        {
            return new CelLayer
            {
                Name = name, Kind = CyanBeamKind.KindName, Start = start, Life = life, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.Target, Arrive = 1f, Spread = 0f,
                Size = new CelCurve(width, width), SizeJitter = 0f, Stretch = 1f,
                Erode = new CelCurve(0f, 0f), Feather = 0f, Palette = palette, Core = 2.3f, Opacity = 1f,
                Extra = new Vector4(arcs, 1f, 1f, 1f),
            };
        }

        static CelLayer Dot(string name, string palette, int mode, float start, float duration, float life,
            int close, int mid, int far, CelCurve size, float softness, float points, float core)
        {
            return new CelLayer
            {
                Name = name, Kind = CyanBeamKind.KindName, Anchor = CelAnchor.Target,
                Start = start, Duration = duration, Life = life, LifeJitter = 0f,
                Close = close, Mid = mid, Far = far, Shape = CelShape.Point,
                Size = size, SizeJitter = 0f, Stretch = mode < 0.5f ? -7f : -5f, Palette = palette,
                Erode = new CelCurve(0f, 0.15f, 8f), Feather = 0f,
                Core = 2.3f, Opacity = 1f, Extra = new Vector4(mode, softness, points, core),
            };
        }

        public static readonly CelFxRecipe Charge = new CelFxRecipe("cyan_beam_charge", 0.32f,
            BeamLayer("green charge shaft", "cyan_beam_charge", 0f, 0.30f, 0.68f, 0.8f),
            Dot("green muzzle bloom", "cyan_beam_charge", 1, 0f, 0f, 0.30f, 1, 1, 0,
                new CelCurve(0.15f, 0.80f, 0.35f), 4f, 0f, 0.28f),
            Dot("green charge spikes", "cyan_beam_charge", 4, 0.03f, 0f, 0.27f, 1, 1, 0,
                new CelCurve(0.55f, 1.1f), 0.08f, 9f, 0.15f));

        static CelLayer TravelingHead(string name, int mode, CelCurve size, float softness, float points, float core)
        {
            CelLayer layer = Dot(name, "cyan_beam_charge", mode, 0.05f, 0f, 0.325f, 1, 1, 0, size, softness, points, core);
            layer.Anchor = CelAnchor.Origin;
            layer.Aim = CelAim.Target;
            layer.Arrive = 1f;
            layer.Spread = 0f;
            return layer;
        }

        static CelFxRecipe TravelingChargeRecipe()
        {
            CelLayer shaft = BeamLayer("traveling green shaft", "cyan_beam_charge", 0.05f, 0.325f, 0.68f, 0.8f);
            shaft.Far = 0;
            shaft.Extra = new Vector4(0.8f, 0f, 0f, 1f);
            return new CelFxRecipe("cyan_beam_traveling_charge", 0.375f, shaft,
                TravelingHead("traveling green bloom", 1, new CelCurve(0.15f, 0.80f, 0.35f), 4f, 0f, 0.28f),
                TravelingHead("traveling green spikes", 4, new CelCurve(0.55f, 1.1f), 0.08f, 9f, 0.15f));
        }

        public static readonly CelFxRecipe TravelingCharge = TravelingChargeRecipe();

        public static readonly CelFxRecipe Beam = new CelFxRecipe("cyan_beam_main", 7f,
            BeamLayer("held cyan beam", "cyan_beam_shaft", BeamStart, BeamLife, 0.76f, 1f),
            Dot("impact core", "cyan_beam_hit", 0, BeamStart, 0f, BeamLife, 1, 1, 1,
                new CelCurve(0.38f, 0.30f), 0f, 5f, 3f),
            Dot("impact bloom", "cyan_beam_hit", 1, BeamStart + 0.05f, 0f, BeamLife - 0.05f, 1, 1, 1,
                new CelCurve(1.5f, 1.3f), 2f, 0f, 0.24f),
            Dot("early target energy flash", "cyan_beam_pool", 3, BeamStart + 0.03f, 0f, 0.6f, 1, 1, 0,
                new CelCurve(6.7f, 7.0f), 1.5f, 1f, 0f),
            Dot("held target cyan atmosphere", "cyan_beam_pool", 3, BeamStart + 0.05f, 0f, BeamLife, 1, 1, 0,
                new CelCurve(3.1f, 3.0f), 2.5f, 0f, 0f),
            Dot("cyan impact spikes", "cyan_beam_hit", 4, BeamStart + 0.1f, 4.75f, 0.16f, 80, 32, 0,
                new CelCurve(2.7f, 3.45f), 0.05f, 8f, 0.045f),
            Dot("cooling target core", "cyan_beam_hit", 1, BeamEnd - 0.08f, 0f, 1.25f, 1, 1, 1,
                new CelCurve(1.1f, 0.04f, 1.5f), 3f, 0f, 0.25f),
            new CelLayer
            {
                Name = "orange impact sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target,
                Start = BeamStart + 0.05f, Duration = 5.15f, Life = 0.65f, LifeJitter = 0.15f,
                Close = 360, Mid = 96, Far = 0, Shape = CelShape.Shell, ShapeSize = new Vector3(0.38f, 0f, 0f),
                Aim = CelAim.Radial, Speed = 6.7f, SpeedJitter = 0.45f, UpBias = 0.3f, Gravity = 2f, Drag = 0.6f,
                Size = new CelCurve(0.075f, 0.025f), SizeJitter = 0.4f, Stretch = 0.09f,
                Erode = new CelCurve(0.02f, 0.55f), Palette = "cyan_beam_spark", Core = 2.2f,
                Extra = new Vector4(0.04f, 0.12f, 0.08f, 0f),
            },
            new CelLayer
            {
                Name = "cyan shaft motes", Kind = CyanBeamKind.KindName, Start = BeamStart + 0.2f,
                Duration = 4.95f, Life = 0.48f, LifeJitter = 0.3f, Close = 260, Mid = 45, Far = 0,
                Shape = CelShape.Line, Offset = new Vector3(0f, 0.12f, 0f), Aim = CelAim.Radial,
                Speed = 0.9f, SpeedJitter = 0.7f, UpBias = 0.7f, Drag = 1.2f, Gravity = -0.25f,
                Size = new CelCurve(0.095f, 0.03f), SizeJitter = 0.5f, Stretch = -0.001f,
                Erode = new CelCurve(0.05f, 0.8f), Palette = "cyan_beam_hit", Core = 2f,
                Extra = new Vector4(5f, 0f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "drifting cyan residue", Kind = CyanBeamKind.KindName, Start = BeamEnd - 0.12f,
                Duration = 0.3f, Life = 0.84f, LifeJitter = 0.22f, Close = 48, Mid = 16, Far = 0,
                Shape = CelShape.Line, Aim = CelAim.Up, Speed = 1.25f, SpeedJitter = 0.65f, Spread = 100f, Drag = 0.5f,
                Size = new CelCurve(0.065f, 0.012f), SizeJitter = 0.5f, Stretch = -0.001f,
                Erode = new CelCurve(0.02f, 0.85f, 3f), Palette = "cyan_beam_hit", Core = 2.8f,
                Extra = new Vector4(5f, 0f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "target cooling motes", Kind = CyanBeamKind.KindName, Anchor = CelAnchor.Target,
                Start = BeamEnd - 0.12f, Duration = 0.5f, Life = 0.70f, LifeJitter = 0.22f,
                Close = 28, Mid = 10, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.35f, 0f, 0f),
                Aim = CelAim.Radial, Speed = 0.65f, SpeedJitter = 0.6f, UpBias = 0.35f, Drag = 0.7f,
                Size = new CelCurve(0.055f, 0.010f), SizeJitter = 0.45f, Stretch = -0.001f,
                Erode = new CelCurve(0.02f, 0.85f, 3f), Palette = "cyan_beam_hit", Core = 2.8f,
                Extra = new Vector4(5f, 0f, 0f, 0f),
            });

        static CelFxRecipe GroundRecipe()
        {
            // j01: the near ground runs short. On the gate masks over 150 active frames the reference's mask
            // area per row is flat across the bottom third (14815 px at y560 -> 11461 at y752) while ours decays
            // monotonically (11376 -> 2725, ratio 0.77 -> 0.24): 69547 of our 116576 px/frame area shortfall sits
            // in the bottom 208 rows. This is the dominant ground primitive (Opacity 1, the largest Size, Far = 0),
            // so its radius is what carries the pool's near edge toward the camera. 5.1 -> 6.3 is +23.5% radius.
            // Size and not Opacity because h01 measured that halving this layer's Opacity moves only 5699 px/frame
            // of mask: the mask is driven by this disc's geometry, not its brightness.
            CelLayer endpoint = Dot("target ground pool", "cyan_beam_pool", 2, BeamStart + 0.06f, 0f, 5.95f, 1, 1, 0,
                new CelCurve(6.3f, 5.3f), 1.5f, BeamStart + 0.06f, 0f);
            return new CelFxRecipe("cyan_beam_ground", 6.3f, endpoint,
                new CelLayer
                {
                    Name = "cyan light along ground", Kind = CyanBeamKind.KindName,
                    Start = BeamStart + 0.1f, Life = BeamLife + 0.15f, LifeJitter = 0f,
                    Close = 4, Mid = 3, Far = 0, Shape = CelShape.Line,
                    Size = new CelCurve(3.7f, 3.5f), SizeJitter = 0f, Stretch = -0.001f,
                    Erode = new CelCurve(0.05f, 1f, 12f), Palette = "cyan_beam_pool", Core = 1f, Opacity = 0.45f,
                    Extra = new Vector4(2f, 1.5f, BeamStart + 0.1f, 0f),
                });
        }

        static CelFxRecipe OpeningGroundRecipe()
        {
            CelLayer pool = Dot("traveling green ground light", "cyan_beam_charge", 2, 0.05f, 0f, 0.325f, 1, 1, 0,
                new CelCurve(4.0f, 3.6f), 1.5f, -1f, 0f);
            pool.Anchor = CelAnchor.Origin;
            pool.Aim = CelAim.Target;
            pool.Arrive = 1f;
            pool.Spread = 0f;
            pool.Opacity = 0.35f;
            return new CelFxRecipe("cyan_beam_opening_ground", 0.375f, pool);
        }

        public static readonly CelFxRecipe Ground = GroundRecipe();
        public static readonly CelFxRecipe OpeningGround = OpeningGroundRecipe();

        public static void Play(Vector3 from, Vector3 to, Vector3 ground, uint seed)
        {
            Vector3 direction = to - from;
            Vector3 charge = Vector3.Lerp(from, to, 0.23f) + Vector3.up * 0.1f;
            bool traveling = CelFx.LodBlend(CelFx.Zoom) < 1.5f;
            CelFx.Play(traveling ? TravelingCharge : Charge, from, direction, traveling ? to : charge, seed);
            CelFx.Play(Beam, from, direction, to, seed + 1u);
            Vector3 lightStart = Vector3.Lerp(from, ground, 0.2f);
            lightStart.y = ground.y + 0.08f;
            CelFx.Play(Ground, lightStart, ground - lightStart, ground + Vector3.up * 0.08f, seed + 2u);
            if (traveling)
            {
                Vector3 lightOrigin = new Vector3(from.x, ground.y + 0.08f, from.z);
                CelFx.Play(OpeningGround, lightOrigin, ground - lightOrigin, ground + Vector3.up * 0.08f, seed + 3u);
            }
        }
    }
}
