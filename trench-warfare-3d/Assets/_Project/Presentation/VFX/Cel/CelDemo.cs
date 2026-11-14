// Phase: CelFx kit (2026-11-09) - the kit's own proof: one small recipe that uses every shared primitive (a tracer and
// sparks and embers as Streaks, a fireball cooling to smoke and a smoke wall as Lobes, a flash, a ground pool and two
// shock rings as Glows, clods through DebrisRenderer, a light through NightLights). Editor/Cel/CelFxStill renders it;
// it is also a worked example of a recipe for the effect agents. Not an effect of the game.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class CelDemo
    {
        public static readonly CelFxRecipe Recipe = new CelFxRecipe("cel_demo", 3.5f,
            new CelLayer
            {
                Name = "tracer", Kind = CelKinds.StreakName, Start = 0f, Duration = 0.12f, Life = 0.22f, LifeJitter = 0f,
                Close = 3, Mid = 2, Far = 1, Shape = CelShape.Point, Aim = CelAim.Target, Speed = 0f,
                Size = new CelCurve(0.14f, 0.10f), SizeJitter = 0f, Stretch = 0.06f, Erode = new CelCurve(0.05f, 0.3f), Palette = "spark", Core = 5f, Extra = new Vector4(0.1f, 0.6f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "flash", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = 0.22f, Life = 0.22f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(2.2f, 3.4f, 0.5f), SizeJitter = 0.1f, Erode = new CelCurve(0.02f, 0.9f, 1.5f),
                Palette = "fire_hot", Core = 6f, Spin = 1.5f, Extra = new Vector4(2f, 1.2f, 8f, 6f),
            },
            new CelLayer
            {
                Name = "fireball", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Start = 0.22f, Duration = 0.12f, Life = 1.3f,
                Close = 10, Mid = 5, Far = 1, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.7f, 0f, 0f), Aim = CelAim.Radial, Speed = 2.6f, UpBias = 0.5f, Drag = 3f, Buoyancy = 1.6f, Spin = 2f,
                Size = new CelCurve(0.7f, 1.5f, 0.4f), Erode = new CelCurve(0f, 1f, 3f), Feather = 0.1f,
                Palette = "fire", CoolTo = "smoke_tan", CoolFrom = 0.3f, Core = 3.5f, Ink = 2.5f, Extra = new Vector4(1.4f, 0.16f, 0.3f, 0.3f),
            },
            new CelLayer
            {
                Name = "smoke wall", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.4f, 0f), Start = 0.5f, Duration = 0.4f, Life = 2.6f,
                Close = 12, Mid = 6, Far = 2, Shape = CelShape.Disc, ShapeSize = new Vector3(2.6f, 0f, 0f), Aim = CelAim.Radial, Speed = 0.8f, Drag = 1.2f, Buoyancy = 0.35f, Spin = 0.6f,
                Size = new CelCurve(0.8f, 1.6f, 0.5f), Stretch = 0.8f, Erode = new CelCurve(0f, 1f, 4f), Feather = 0.12f,
                Palette = "smoke_tan", Core = 1f, Ink = 2.5f, Extra = new Vector4(1.3f, 0.14f, 0.9f, 0f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = 0.22f, Duration = 0.08f, Life = 0.6f,
                Close = 26, Mid = 10, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Radial, Speed = 14f, SpeedJitter = 0.5f, UpBias = 0.6f, Gravity = 9.8f, Drag = 1.6f,
                Size = new CelCurve(0.07f, 0.04f), Stretch = 0.045f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 4f, Extra = new Vector4(0f, 0.2f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "embers", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = 0.4f, Duration = 1.2f, Life = 2.2f,
                Close = 18, Mid = 6, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(1.4f, 0f, 0f), Aim = CelAim.Radial, Speed = 2.5f, UpBias = 1f, Drag = 0.9f, Buoyancy = 1.2f,
                Size = new CelCurve(0.06f, 0.03f), Stretch = 0.03f, Erode = new CelCurve(0.05f, 0.6f, 2f), Palette = "ember", Core = 3f, Extra = new Vector4(1f, 0.08f, 0.5f, 0f),
            },
            new CelLayer
            {
                Name = "ground pool", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1f, 0f), Start = 0.22f, Life = 2.2f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(4.5f, 5.5f, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0.02f, 1f, 1.5f),
                Palette = "glow_warm", Core = 1.5f, Opacity = 0.9f, Extra = new Vector4(0f, 1.4f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "ground ring", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -1f, 0f), Start = 0.22f, Life = 0.55f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.6f, 6.5f, 0.45f), SizeJitter = 0f, Erode = new CelCurve(0.05f, 0.95f, 1.5f),
                Palette = "fire_hot", Core = 3f, Extra = new Vector4(1f, 0.16f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "air ring", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Start = 0.22f, Life = 0.32f, LifeJitter = 0f,
                Close = 1, Mid = 0, Far = 0, Size = new CelCurve(0.8f, 3.8f, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0.05f, 0.95f),
                Palette = "white", Core = 2f, Opacity = 0.8f, Extra = new Vector4(3f, 0.1f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "clods", Kind = CelKinds.ChunksName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, -0.8f, 0f), Start = 0.22f, Life = 6f,
                Close = 8, Mid = 4, Far = 0, Speed = 7f, Size = new CelCurve(0.28f, 0.28f), Palette = "smoke_black", Extra = new Vector4(0f, 0.4f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = 0.22f, Life = 0.6f,
                Close = 1, Mid = 1, Far = 1, Palette = "fire", Core = 3f, Extra = new Vector4(14f, 0f, 0f, 0f),
            });

        /// <summary>The demo: shot from 14 m to the left (-x) of `at` (the hit, a metre above the ground).</summary>
        public static void Play(Vector3 at, uint seed = 1u)
        {
            Vector3 from = at + new Vector3(-14f, 0.2f, 0f);
            CelFx.Play(Recipe, from, at - from, at, seed);
        }
    }
}
