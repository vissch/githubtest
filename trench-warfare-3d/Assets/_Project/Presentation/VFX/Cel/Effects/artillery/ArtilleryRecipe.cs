// Phase: CelFx artillery - seeded projectile and impact recipes, with fewer layers at each outer zoom tier.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class ArtilleryRecipe
    {
        public const float LeadIn = 0.2f;
        const string Fx = ArtilleryFxKind.KindName;
        const string Shape = ArtilleryShapeKind.KindName;

        static ArtilleryRecipe()
        {
            CelPalette.Add("art_hot", CelPalette.Hex(0x482719), CelPalette.Hex(0xB95828), CelPalette.Hex(0xFFAC5B), CelPalette.Hex(0xFFF6A3), CelPalette.Hex(0xFFFFBC), 0.72f, 4, CelPalette.Hex(0x25170F));
            CelPalette.Add("art_smoke", CelPalette.Hex(0x38271E), CelPalette.Hex(0x5E402E), CelPalette.Hex(0x805B3F), CelPalette.Hex(0xA47D56), CelPalette.Hex(0xA47D56), 1.1f, 4, CelPalette.Hex(0x221A14));
            CelPalette.Add("art_trail", CelPalette.Hex(0x5C5548), CelPalette.Hex(0x887B64), CelPalette.Hex(0xBAAD91), CelPalette.Hex(0xDDCEAD), CelPalette.Hex(0xEAD9B6), 1.1f, 3, CelPalette.Hex(0x383126));
            CelPalette.Add("art_chunk", CelPalette.Hex(0x1A1411), CelPalette.Hex(0x352318), CelPalette.Hex(0x674329), CelPalette.Hex(0xB87439), CelPalette.Hex(0xB87439), 1.1f, 4, CelPalette.Hex(0x100D0B));
            CelPalette.Add("art_shell", CelPalette.Hex(0x171616), CelPalette.Hex(0x3E2D17), CelPalette.Hex(0x82541B), CelPalette.Hex(0xBC852A), CelPalette.Hex(0xFFDA5B), 1.1f, 4, CelPalette.Hex(0x151312));
            CelPalette.Add("art_spark", CelPalette.Hex(0xB04B20), CelPalette.Hex(0xF98737), CelPalette.Hex(0xFFCF6E), CelPalette.Hex(0xFFF5AE), CelPalette.Hex(0xFFFFE0), 0.55f, 4, CelPalette.Hex(0x25170F));
            CelPalette.Add("art_pool", CelPalette.Hex(0x2A160A), CelPalette.Hex(0x603215), CelPalette.Hex(0xA75B27), CelPalette.Hex(0xDD8B42), CelPalette.Hex(0xFFD28A), 0.75f, 4, CelPalette.Hex(0x25170F));
            CelPalette.Add("art_billow_hot", CelPalette.Hex(0x694332), CelPalette.Hex(0xE99554), CelPalette.Hex(0xFFC57B), CelPalette.Hex(0xFFF1B1), CelPalette.Hex(0xFFF8CB), 0.88f, 4, CelPalette.Hex(0x3A2821));
            CelPalette.Add("art_billow_cool", CelPalette.Hex(0x594639), CelPalette.Hex(0x826349), CelPalette.Hex(0xA58363), CelPalette.Hex(0xBA9A78), CelPalette.Hex(0xBA9A78), 1.1f, 4, CelPalette.Hex(0x453328));
            CelPalette.Add("art_contact_ground", CelPalette.Hex(0x6C2510), CelPalette.Hex(0xF06B25), CelPalette.Hex(0xFFAD48), CelPalette.Hex(0xFFD278), CelPalette.Hex(0xFFE5A0), 0.6f, 4, CelPalette.Hex(0x25170F));
        }

        public static void Init() { }

        public static readonly CelFxRecipe Background = Burst("artillery_background", 1.0f, 1.95f);
        public static readonly CelFxRecipe Foreground = Burst("artillery_foreground", 1.08f, 2.8f);
        public static readonly CelFxRecipe Target = Burst("artillery_target", 0.86f, 2.05f);

        static readonly CelFxRecipe BackgroundClose = Burst("artillery_background_close", 1.0f, 1.95f, true);
        static readonly CelFxRecipe ForegroundClose = Burst("artillery_foreground_close", 1.08f, 2.8f, true);
        static readonly CelFxRecipe TargetClose = Burst("artillery_target_close", 0.86f, 2.05f, true);

        static CelFxRecipe Burst(string name, float scale, float life, bool roundedSmoke = false)
        {
            float smokeLife = roundedSmoke ? Mathf.Min(life, 2.2f) : life;
            return new CelFxRecipe(name, life + 0.3f,
                new CelLayer
                {
                    Name = "first white flash", Kind = Fx, Anchor = CelAnchor.Target, Offset = Vector3.up * (0.6f * scale),
                    Life = 0.28f, LifeJitter = 0f, Close = 1, Mid = 1, Far = 1,
                    Size = new CelCurve(2.4f * scale, 4.5f * scale, 0.3f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 1.3f),
                    Palette = "art_spark", Core = 4f, Opacity = 1.2f, Extra = new Vector4(ArtilleryFxKind.Star, 1.2f, 0.8f * scale, 11f),
                },
                new CelLayer
                {
                    Name = "inked blast spears", Kind = Shape, Anchor = CelAnchor.Target, Offset = Vector3.up * (1.2f * scale),
                    Life = 0.48f, LifeJitter = 0f, Close = 1, Mid = 1, Far = 0,
                    Size = new CelCurve(2.2f * scale, 4.3f * scale, 0.32f), SizeJitter = 0f, Stretch = 1.25f,
                    Erode = new CelCurve(0f, 1f, 2.2f), Palette = "art_spark", Core = 2.7f, Ink = 0.4f,
                    Extra = new Vector4(ArtilleryShapeKind.Burst, 13f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "ground light", Kind = Fx, Anchor = CelAnchor.Target, Offset = Vector3.up * 0.08f,
                    Life = roundedSmoke ? 1.50f : 0.62f, LifeJitter = 0f, Close = 1, Mid = 1, Far = 1,
                    Size = new CelCurve(6.3f * scale, 9.2f * scale, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 1.8f),
                    Palette = "art_contact_ground", Core = 2f, Opacity = roundedSmoke ? 1.15f : 2.8f,
                    Extra = new Vector4(roundedSmoke ? ArtilleryFxKind.GroundFan : ArtilleryFxKind.Pool, 1.4f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "rising crown", Kind = roundedSmoke ? ArtilleryBillowKind.KindName : CelKinds.LobeName, Anchor = CelAnchor.Target,
                    Offset = new Vector3(-0.35f * scale, 1.4f * scale, 0.3f * scale), Shape = CelShape.Box, ShapeSize = new Vector3(1.6f, 0.6f, 0.5f) * scale,
                    Start = 0.08f, Duration = 0.14f, Life = smokeLife, LifeJitter = 0.03f, Close = roundedSmoke ? 4 : 5, Mid = 2, Far = 0,
                    Aim = CelAim.Up, Speed = 1.45f * scale, SpeedJitter = 0.15f, Spread = 12f, Drag = 1.5f, Spin = 0.7f,
                    Size = new CelCurve((roundedSmoke ? 0.85f : 0.65f) * scale, (roundedSmoke ? 1.85f : 1.65f) * scale, 0.38f), SizeJitter = 0.16f, Stretch = 1.1f,
                    Erode = new CelCurve(0f, 1f, roundedSmoke ? 2.2f : 7f), Feather = roundedSmoke ? 0.015f : 0.06f,
                    Palette = roundedSmoke ? "art_billow_hot" : "art_hot", CoolTo = roundedSmoke ? "art_billow_cool" : "art_smoke", CoolFrom = roundedSmoke ? 0.2f : 0.28f,
                    Core = 1.05f, Ink = roundedSmoke ? 0.8f : 3f,
                    Extra = roundedSmoke ? new Vector4(0.72f, 0.07f, -0.15f, 0.2f) : new Vector4(1.8f, 0.26f, -0.15f, 0.28f),
                },
                new CelLayer
                {
                    Name = "rolling fire lobes", Kind = roundedSmoke ? ArtilleryBillowKind.KindName : CelKinds.LobeName, Anchor = CelAnchor.Target,
                    Offset = new Vector3(0f, 0.8f * scale, -0.35f * scale), Shape = CelShape.Box, ShapeSize = new Vector3(3.4f, 0.9f, 1.6f) * scale,
                    Start = 0.04f, Duration = 0.18f, Life = smokeLife * 0.93f, LifeJitter = 0.04f, Close = roundedSmoke ? 7 : 12, Mid = 5, Far = 0,
                    Aim = CelAim.Radial, Speed = 0.8f * scale, SpeedJitter = 0.3f, UpBias = 0.25f, Drag = 1.6f, Spin = 0.6f,
                    Size = new CelCurve((roundedSmoke ? 0.55f : 0.38f) * scale, (roundedSmoke ? 1.4f : 1.15f) * scale, 0.4f), SizeJitter = 0.3f, Stretch = 0.95f,
                    Erode = new CelCurve(0f, 1f, roundedSmoke ? 2.2f : 8f), Feather = roundedSmoke ? 0.015f : 0.07f,
                    Palette = roundedSmoke ? "art_billow_hot" : "art_hot", CoolTo = roundedSmoke ? "art_billow_cool" : "art_smoke", CoolFrom = roundedSmoke ? 0.2f : 0.28f,
                    Core = 1.05f, Ink = roundedSmoke ? 0.75f : 2.5f,
                    Extra = roundedSmoke ? new Vector4(0.8f, 0.08f, -0.2f, 0.2f) : new Vector4(1.65f, 0.28f, -0.2f, 0.28f),
                },
                new CelLayer
                {
                    Name = "low dust rolls", Kind = roundedSmoke ? ArtilleryBillowKind.KindName : CelKinds.LobeName, Anchor = CelAnchor.Target,
                    Offset = new Vector3(0f, 0.25f * scale, 0.2f * scale), Shape = CelShape.Disc, ShapeSize = new Vector3(2.3f * scale, 1.7f * scale, 0f),
                    Start = 0.18f, Duration = 0.12f, Life = smokeLife * 0.8f, LifeJitter = 0.03f, Close = roundedSmoke ? 4 : 8, Mid = 3, Far = 0,
                    Aim = CelAim.Radial, Speed = 0.6f * scale, Drag = 2f, Spin = 0.5f,
                    Size = new CelCurve((roundedSmoke ? 0.33f : 0.25f) * scale, (roundedSmoke ? 0.9f : 0.7f) * scale, 0.4f), Stretch = 0.6f,
                    Erode = new CelCurve(0f, 1f, roundedSmoke ? 2f : 8f), Feather = roundedSmoke ? 0.015f : 0.08f,
                    Palette = roundedSmoke ? "art_billow_hot" : "art_hot", CoolTo = roundedSmoke ? "art_billow_cool" : "art_smoke", CoolFrom = 0.2f,
                    Core = 1f, Ink = roundedSmoke ? 0.65f : 2f,
                    Extra = roundedSmoke ? new Vector4(0.7f, 0.06f, 0f, 0.2f) : new Vector4(1.7f, 0.22f, 0f, 0.2f),
                },
                new CelLayer
                {
                    Name = "angular debris", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = Vector3.up * (0.5f * scale),
                    Shape = CelShape.Sphere, ShapeSize = Vector3.one * (0.7f * scale), Start = 0.03f, Duration = 0.2f,
                    Life = 1.15f, LifeJitter = 0.2f, Close = 18, Mid = 5, Far = 0, Aim = CelAim.Radial,
                    Speed = 5.5f * scale, SpeedJitter = 0.4f, UpBias = 0.65f, Gravity = 9.8f, Drag = 0.8f, Spin = 3f,
                    Size = new CelCurve(0.12f * scale, 0.06f * scale), SizeJitter = 0.45f, Stretch = 1.5f,
                    Erode = new CelCurve(0f, 0.8f, 4f), Palette = "art_chunk", Core = 1f, Ink = 1.5f, Extra = new Vector4(4f, 0.8f, 0f, 0f),
                },
                new CelLayer
                {
                    Name = "ground sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = Vector3.up * 0.3f,
                    Shape = CelShape.Sphere, ShapeSize = Vector3.one * (0.5f * scale), Duration = 0.24f, Life = 0.65f, LifeJitter = 0.15f,
                    Close = 28, Mid = 8, Far = 0, Aim = CelAim.Radial, Speed = 11f * scale, SpeedJitter = 0.35f, UpBias = 0.2f,
                    Gravity = 8f, Drag = 1.3f, Size = new CelCurve(0.07f * scale, 0.025f * scale), Stretch = 0.045f,
                    Palette = "art_spark", Core = 3f, Extra = new Vector4(0.1f, 0.3f, 0.1f, 0f),
                },
                new CelLayer
                {
                    Name = "warm flash on nearby surfaces", Kind = CelKinds.LightName, Anchor = CelAnchor.Target,
                    Life = 0.55f, LifeJitter = 0f, Close = 1, Mid = 1, Far = 0,
                    Palette = "art_contact_ground", Core = 3.2f, Extra = new Vector4(12f * scale, 0f, 0f, 0f),
                });
        }

        public static void PlayAt(CelFxRecipe recipe, Vector3 at, Vector3 forward, uint seed, float delay)
        {
            if (CelFx.Zoom <= CelFx.CloseZoom)
            {
                if (recipe == Background) recipe = BackgroundClose;
                else if (recipe == Foreground) recipe = ForegroundClose;
                else if (recipe == Target) recipe = TargetClose;
            }
            CelFx.Play(recipe, at, forward, at, seed, delay);
        }

        public static void Projectile(Vector3 start, Vector3 end, float delay, float flight, float scale, uint seed, bool close)
        {
            Init();
            var rng = new CelRng(seed);
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(Shape);
            float born = CelFx.Now + delay;
            Vector3 velocity = (end - start) / flight;
            renderer.Spawn(kind, new CelRecord
            {
                Pos = start, Born = born, Vel = velocity, Life = flight, Axis = velocity.normalized, Seed = rng.Next(),
                Size = new Vector4(0.12f * scale, 0.12f * scale, 1f, 1.2f * scale),
                Tone = new Vector4(CelRecord.PackPalette(CelPalette.Find("art_shell")), 1f, 1f, 1.5f),
                Extra = new Vector4(ArtilleryShapeKind.Shell, 0f, 0f, 0f),
            });
            renderer.Spawn(CelKinds.Streak, new CelRecord
            {
                Pos = start - velocity.normalized * (0.55f * scale), Born = born, Vel = velocity, Life = flight, Axis = Vector3.up, Seed = rng.Next(),
                Size = new Vector4(1.6f * scale, 2.4f * scale, 1f, 0.40f), Erode = new Vector4(0f, 0.03f, 1f, 0f),
                Tone = new Vector4(CelRecord.PackPalette(CelPalette.Find("art_hot")), 1f, 1f, 0f),
                Extra = new Vector4(0f, 2f * scale, 0f, 0f),
            });
            if (!close) return;
            renderer.Spawn(kind, new CelRecord
            {
                Pos = start, Born = born, Life = 2.6f, Axis = end - start, Seed = rng.Next(),
                Size = new Vector4(0.07f * scale, 0.24f * scale, 0.45f, 0f), Motion = new Vector4(0f, 0f, 0.6f, 0f),
                Erode = new Vector4(0f, 1f, 5f, 0f), Tone = new Vector4(CelRecord.PackPalette(CelPalette.Find("art_trail")), 1f, 0.9f, 1.2f),
                Extra = new Vector4(ArtilleryShapeKind.Trail, flight, 0f, 0f),
            });
        }
    }
}
