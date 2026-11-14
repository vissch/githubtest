// Phase: Explosion reference revision, 2026-11-14. Rebuilt against explosive_blasts_nolora.mp4:
// a compact hot impact beside the readable dummy, curved white whips and cream pressure jets,
// staggered ground blasts, a short orange fan, then pale billowing smoke with soot pockets and embers.
// Hold is the sustained hit; Lead is its pre-roll; Site/SiteNear/SiteFar are ground impacts;
// Cloud/MainCloud carry the smoke phase. ExplosionRange composes the reference staging.
// PlayShell is the reusable game-facing ground blast. Every layer uses the pooled CelFx renderer.
// Layer comments below retain prior tuning context; captured reference comparisons are the visual authority.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class ExplosionRecipe
    {
        static ExplosionRecipe()
        {
            // colours picked off the reference frames (f30 ground, f86 fireball, f110 smoke), as they look
            CelPalette.Add("exp_fire", CelPalette.Hex(0xF08A28), CelPalette.Hex(0xFFA030), CelPalette.Hex(0xFFC040), CelPalette.Hex(0xFFDC60), CelPalette.Hex(0xFFFFFF), 0.82f, 4, CelPalette.Hex(0xD05A18));
            CelPalette.Add("exp_dome", CelPalette.Hex(0xF0A050), CelPalette.Hex(0xC07030), CelPalette.Hex(0xFFD090), CelPalette.Hex(0xFFE8B0), CelPalette.Hex(0xFFFFFF), 0.5f, 4, CelPalette.Hex(0x000000));
            // the fireball's white heart and its bloom halo (soft ExpFx flashes: c1 edge, c3 middle, core above coreAt)
            // r03: the heart adds little at its edge (so it does not blow the lobes round it to white) and is white only
            // in its middle, one solid heart about 0.4x the fireball's width
            CelPalette.Add("exp_core", CelPalette.Hex(0x602008), CelPalette.Hex(0x602008), CelPalette.Hex(0x904010), CelPalette.Hex(0xC06020), CelPalette.Hex(0xFFFFFF), 0.3f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("exp_bloom", CelPalette.Hex(0x401808), CelPalette.Hex(0x602808), CelPalette.Hex(0x904010), CelPalette.Hex(0xC06424), CelPalette.Hex(0xFFB060), 0.85f, 4, CelPalette.Hex(0x000000));
            // r03: the fireball's lobes are orange to yellow with no white core band (the white is the heart's alone), and
            // an orange ink, not a dark rim
            CelPalette.Add("exp_ball", CelPalette.Hex(0xF08A28), CelPalette.Hex(0xFFA838), CelPalette.Hex(0xFFC848), CelPalette.Hex(0xFFE070), CelPalette.Hex(0xFFE478), 0.97f, 4, CelPalette.Hex(0xC85A1C));
            // r05: the same lobes with a dark brown ink (the ref's cel outlines read dark over the fire)
            // b03: white-yellow most of the way out (ref #f9e3a5), orange only in the darkest outer band
            CelPalette.Add("exp_ball_ink", CelPalette.Hex(0xD06A2A), CelPalette.Hex(0xF2B060), CelPalette.Hex(0xF9E3A5), CelPalette.Hex(0xFDEFC4), CelPalette.Hex(0xFFF6E0), 0.97f, 4, CelPalette.Hex(0x1C0A04));
            // the grey-tan puffs that wrap the fire from f40 on and take it over (warm-lit from the fire below)
            // b02: the fireball's pale heart lobes (ref #f9e3a5), drawn in front of the orange lobes
            // d15: one merged white-hot heart (ref f60-f86 core #fcf9b9, L>90): orange then #f9ce71 edge bands, white middle, no inner ink
            CelPalette.Add("exp_ball_hot", CelPalette.Hex(0xEC9448), CelPalette.Hex(0xF9CE71), CelPalette.Hex(0xFFF6DC), CelPalette.Hex(0xFFFBEC), CelPalette.Hex(0xFFFFF8), 0.9f, 4, CelPalette.Hex(0xEC9448));
            // d15: the outer fireball lobes orange (ref f60-f86 L65-80 #e49d6f, L45-65 #b77248), a thin yellow top band, dark ink on the rim only
            CelPalette.Add("exp_ball_out", CelPalette.Hex(0xED6D1C), CelPalette.Hex(0xFF9A22), CelPalette.Hex(0xFFD84C), CelPalette.Hex(0xFFF28A), CelPalette.Hex(0xFFF8AA), 0.97f, 4, CelPalette.Hex(0x1C0A04));
            CelPalette.Add("exp_wrap", CelPalette.Hex(0x9A8C80), CelPalette.Hex(0xB4A694), CelPalette.Hex(0xC8BAA6), CelPalette.Hex(0xDACCB6), CelPalette.Hex(0xDACCB6), 1.1f, 4, CelPalette.Hex(0x756C5F));
            // r04: the ragged white-yellow star over the fireball (additive: orange rim, yellow, white heart)
            CelPalette.Add("exp_star", CelPalette.Hex(0x702808), CelPalette.Hex(0xA04812), CelPalette.Hex(0xD08A30), CelPalette.Hex(0xFFC860), CelPalette.Hex(0xFFFFFF), 0.68f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("exp_ember_tail", CelPalette.Hex(0xB83A10), CelPalette.Hex(0xF07425), CelPalette.Hex(0xFFA44A), CelPalette.Hex(0xFFE6BC), CelPalette.Hex(0xFFF6EB), 0.8f, 3, CelPalette.Hex(0x401004));
            CelPalette.Add("exp_ember", CelPalette.Hex(0xC04010), CelPalette.Hex(0xF07020), CelPalette.Hex(0xFF9A30), CelPalette.Hex(0xFFB850), CelPalette.Hex(0xFFD080), 0.8f, 3, CelPalette.Hex(0x401004));
            CelPalette.Add("exp_arc", CelPalette.Hex(0xE08A40), CelPalette.Hex(0xFFD8A0), CelPalette.Hex(0xFFF4E4), CelPalette.Hex(0xFFFFFF), CelPalette.Hex(0xFFFFFF), 0.45f, 3, CelPalette.Hex(0x000000));
            CelPalette.Add("exp_spark", CelPalette.Hex(0xE08030), CelPalette.Hex(0xFFC070), CelPalette.Hex(0xFFE8B0), CelPalette.Hex(0xFFFFFF), CelPalette.Hex(0xFFFFFF), 0.6f, 3, CelPalette.Hex(0x401004));
            CelPalette.Add("exp_tracer", CelPalette.Hex(0xC04010), CelPalette.Hex(0xF07A20), CelPalette.Hex(0xFFB040), CelPalette.Hex(0xFFE090), CelPalette.Hex(0xFFF4D0), 0.75f, 3, CelPalette.Hex(0x401004));
            CelPalette.Add("exp_lead", CelPalette.Hex(0xE04A12), CelPalette.Hex(0xF07820), CelPalette.Hex(0xFFA430), CelPalette.Hex(0xFFE878), CelPalette.Hex(0xFFF86C), 0.75f, 3, CelPalette.Hex(0x401004));
            CelPalette.Add("exp_onset", CelPalette.Hex(0x602010), CelPalette.Hex(0x9C532F), CelPalette.Hex(0xDC8E58), CelPalette.Hex(0xFFD3A0), CelPalette.Hex(0xFFF0CB), 0.5f, 4, CelPalette.Hex(0x000000));
            // the pool is additive: these are what it adds to the dark plate (#282c37 -> #b77650 at the middle)
            CelPalette.Add("exp_pool", CelPalette.Hex(0x2A1A08), CelPalette.Hex(0x6A3C14), CelPalette.Hex(0x9A6A30), CelPalette.Hex(0xD09048), CelPalette.Hex(0xFFD890), 0.7f, 4, CelPalette.Hex(0x000000));
            // the warm light the blasts throw over the whole field (the clips' ground turns brown from 0.9 s to 4 s)
            CelPalette.Add("exp_wash", CelPalette.Hex(0x181008), CelPalette.Hex(0x22180E), CelPalette.Hex(0x4A3420), CelPalette.Hex(0x6A4A30), CelPalette.Hex(0x6A4A30), 1.1f, 4, CelPalette.Hex(0x000000));
            // r03: pale cream smoke (#c8b8a0 in the middle band) with a soft brown ink, as the ref's f105-f135
            // r04: the faint wide warmth over the whole near field (brown, low chroma), under the brighter pool
            CelPalette.Add("exp_wide", CelPalette.Hex(0x100A06), CelPalette.Hex(0x1A120A), CelPalette.Hex(0x3A2816), CelPalette.Hex(0x543A22), CelPalette.Hex(0x543A22), 1.1f, 4, CelPalette.Hex(0x000000));

            CelPalette.Add("exp_smoke", CelPalette.Hex(0x6D6054), CelPalette.Hex(0x9D8E7F), CelPalette.Hex(0xC9BCAE), CelPalette.Hex(0xD8CBC2), CelPalette.Hex(0xD8CBC2), 1.1f, 4, CelPalette.Hex(0x76685B));

            CelPalette.Add("exp_smoke_rear", CelPalette.Hex(0x695845), CelPalette.Hex(0x9C8C7C), CelPalette.Hex(0xC7B4A4), CelPalette.Hex(0xD3C2B0), CelPalette.Hex(0xD3C2B0), 1.1f, 4, CelPalette.Hex(0x6C5D4E));
            CelPalette.Add("exp_smoke_peak", CelPalette.Hex(0x2D2520), CelPalette.Hex(0x756554), CelPalette.Hex(0xBDA78E), CelPalette.Hex(0xD4B89C), CelPalette.Hex(0xD4B89C), 1.1f, 4, CelPalette.Hex(0x211A16));
            CelPalette.Add("exp_smoke_debris", CelPalette.Hex(0x49382E), CelPalette.Hex(0x74604F), CelPalette.Hex(0x967D68), CelPalette.Hex(0xAA927C), CelPalette.Hex(0xAA927C), 1.1f, 3, CelPalette.Hex(0x3D3029));
            CelPalette.Add("exp_soot", CelPalette.Hex(0x554B43), CelPalette.Hex(0x6B5E52), CelPalette.Hex(0x887666), CelPalette.Hex(0xA08A76), CelPalette.Hex(0xA08A76), 1.1f, 2, CelPalette.Hex(0x40372F));
            CelPalette.Add("exp_dirt", CelPalette.Hex(0x1E1714), CelPalette.Hex(0x2E2420), CelPalette.Hex(0x44362E), CelPalette.Hex(0x5A463A), CelPalette.Hex(0x5A463A), 1.1f, 3, CelPalette.Hex(0x120C0A));
            // r05: the thrown chunks, near black with a dark brown lit side (the ref's debris over the fire and the domes)
            CelPalette.Add("exp_chunk", CelPalette.Hex(0x120C0A), CelPalette.Hex(0x1A1210), CelPalette.Hex(0x261A14), CelPalette.Hex(0x3A2A20), CelPalette.Hex(0x3A2A20), 1.1f, 3, CelPalette.Hex(0x080504));
            // r05: the brown light over the near field (adds about #43200a at the middle: the plate's #282c37 to the ref's #6b4c41)
            // d14: the early hazy spray round the body (ref f31-f49: cream-grey feathered wedges, additive over the ground)
            CelPalette.Add("exp_spray", CelPalette.Hex(0x3A342C), CelPalette.Hex(0x9C9282), CelPalette.Hex(0x9A8C78), CelPalette.Hex(0x9C9282), CelPalette.Hex(0x000000), 0.3f, 4, CelPalette.Hex(0x000000));
            // d14: the bright speck shower (opaque orange-yellow cut-out dots, no ink)
            CelPalette.Add("exp_speck", CelPalette.Hex(0xFF9A30), CelPalette.Hex(0xFFB040), CelPalette.Hex(0xFFE090), CelPalette.Hex(0xFFF0C0), CelPalette.Hex(0xFFF0C0), 1.1f, 3, CelPalette.Hex(0xFFA040));
            CelPalette.Add("exp_brown", CelPalette.Hex(0x140A04), CelPalette.Hex(0x281408), CelPalette.Hex(0x3C2812), CelPalette.Hex(0x50381E), CelPalette.Hex(0x50381E), 1.1f, 4, CelPalette.Hex(0x000000));
            CelPalette.Add("exp_fan_cream", CelPalette.Hex(0xE67016), CelPalette.Hex(0xFFC451), CelPalette.Hex(0xFFF190), CelPalette.Hex(0xFFFFE2), CelPalette.Hex(0xFFFFEF), 0.75f, 3, CelPalette.Hex(0x000000));
            CelPalette.Add("exp_fan_white", CelPalette.Hex(0xE67016), CelPalette.Hex(0xFFD488), CelPalette.Hex(0xFFFFE2), CelPalette.Hex(0xFFFFFF), CelPalette.Hex(0xFFFFFF), 0.75f, 3, CelPalette.Hex(0x000000));
            // ExpFire's heat islands use these rows directly, independent of a sphere-normal palette ramp.
            CelPalette.Add("exp_fire_body_v1", CelPalette.Hex(0xD86124), CelPalette.Hex(0xED7626), CelPalette.Hex(0xFFD44A), CelPalette.Hex(0xFFF38E), CelPalette.Hex(0xFFF5AD), 1.1f, 4, CelPalette.Hex(0x9B4E28));
            CelPalette.Add("exp_fire_core_v1", CelPalette.Hex(0xEF8E28), CelPalette.Hex(0xFFE16E), CelPalette.Hex(0xFFF49C), CelPalette.Hex(0xFFFCBF), CelPalette.Hex(0xFFFFE5), 0.72f, 4, CelPalette.Hex(0xEF8E28));
            CelPalette.Add("exp_target_afterglow", CelPalette.Hex(0xFF4008), CelPalette.Hex(0xFF4008), CelPalette.Hex(0xFF4008), CelPalette.Hex(0xFF4008), CelPalette.Hex(0xFF4008), 1.1f, 4, CelPalette.Hex(0x000000));
        }

        /// <summary>Touch the class so the palettes exist before the first play.</summary>
        public static void Init() { }

        const string Fx = ExpFxKind.KindName;
        const string Chunk = ExpChunkKind.KindName;
        const string Ribbon = ExpRibbonKind.KindName;
        const string Puff = ExpPuffKind.KindName;

        // Broad white accents pulse early and late. The middle interval keeps only fine arcs.
        // Angles are measured in the view plane; zero retains the ordinary radial spark path.
        static CelLayer MakeArcPhase(string name, float start, float duration, float life, int count,
            float width, float length, float angle, float bend)
        {
            return new CelLayer
            {
                Name = name, LodGroup = "white-arcs", Kind = angle > 0f ? ExpArcKind.KindName : Fx, Anchor = CelAnchor.Target,
                Offset = new Vector3(-0.25f, 0.15f, -0.2f), Shape = CelShape.Sphere,
                ShapeSize = new Vector3(angle > 0f ? 0.10f : 0.50f, 0f, 0f),
                Start = start, Duration = duration, Life = life, LifeJitter = 0f,
                Close = count, Mid = 0, Far = 0,
                Aim = angle > 0f ? CelAim.None : CelAim.Radial, Speed = angle > 0f ? 0f : 1.5f,
                SpeedJitter = 0f, Gravity = 0f, Drag = 0f, UpBias = 0.2f,
                Size = new CelCurve(width, width * 0.28f), SizeJitter = 0.10f, Stretch = length,
                Erode = new CelCurve(0.03f, 0.90f, 4f), Palette = "exp_arc", Core = 5f, Opacity = 2.5f,
                Extra = new Vector4(ExpFxKind.Arc, angle, bend, start >= 2f ? 2f : 1f),
            };
        }

        public static readonly CelFxRecipe Hold = new CelFxRecipe("explosion_hold", 3.4f,
            new CelLayer
            {
                Name = "tracers", Kind = Fx, Anchor = CelAnchor.Origin, Shape = CelShape.Box, ShapeSize = new Vector3(0.0f, 1.05f, 1.8f),
                Start = 0f, Duration = 2.6f, TimeJitter = 0.04f, BirthEnd = 2.6f, Life = 0.26f, LifeJitter = 0f, Close = 30, Mid = 10, Far = 0,
                Aim = CelAim.Target, Speed = 0.0f, Spread = 0f, Arrive = 0.846154f, Size = new CelCurve(0.15f, 0.15f), SizeJitter = 0.25f, Stretch = 0.11f,
                Erode = new CelCurve(0.05f, 0.25f), Palette = "exp_lead", Core = 2.2f, Extra = new Vector4(4.3f, 0.09f, 0.105f, 0.22f),
            },
            new CelLayer
            {
                // the first dome of light: soft, a little wider than the dummy is tall, gone in 8 frames
                Name = "flash", LodGroup = "fire-body", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.45f, -3.3f), Start = 0f, Life = 0.34f, LifeJitter = 0f,
                // d12: Mid/Far only; the Close film plays the onset dome below instead
                Close = 0, Mid = 0, Far = 0, Size = new CelCurve(1.9f, 2.1f, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 1.5f),
                Palette = "exp_dome", Core = 1.2f, Opacity = 0.6f, Extra = new Vector4(ExpFxKind.Flash, 0.7f, 1.845f, 0f),
            },
            new CelLayer
            {
                // d12: the white-hot ground dome over the impact (ref f18-f26), Close only, on the fire's own frames
                Name = "onset dome", LodGroup = "fire-body", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, -0.3f, -3.3f), Start = 0f, Life = 0.36f, LifeJitter = 0f,
                Close = 0, Mid = 0, Far = 0, Size = new CelCurve(2.9f, 3.2f, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 3.0f),
                Palette = "exp_dome", Core = 1.6f, Opacity = 0.9f, Extra = new Vector4(ExpFxKind.Flash, 0.4f, 0.95f, 0f),
            },
            new CelLayer
            {
                Name = "onset core", LodGroup = "fire-body", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, -0.1f, -3.4f), Start = 0f, Life = 0.3f, LifeJitter = 0f,
                Close = 0, Mid = 0, Far = 0, Size = new CelCurve(1.5f, 1.7f, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 2.5f),
                Palette = "exp_dome", Core = 2.0f, Opacity = 0.8f, Extra = new Vector4(ExpFxKind.Flash, 0.7f, 0f, 0f),
            },
            new CelLayer
            {
                // d12: a fainter afterglow holding the lit area through the f30-f44 sag, Close only
                Name = "onset afterglow", LodGroup = "fire-body", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, -0.3f, -3.2f), Start = 0.25f, Life = 0.55f, LifeJitter = 0f,
                Close = 0, Mid = 0, Far = 0, Size = new CelCurve(2.4f, 2.7f, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 2.0f),
                Palette = "exp_dome", Core = 1.0f, Opacity = 0.2f, Extra = new Vector4(ExpFxKind.Flash, 0.5f, 0.95f, 0f),
            },
            new CelLayer
            {
                // r04: the ragged white-yellow star over the fireball (10 uneven spikes that re-jump), re-born all hold
                Name = "star", LodGroup = "fire-body", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(-0.25f, 0.15f, -0.2f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.225f, 0.0f, 0.0f),
                Start = 0.05f, Duration = 2.75f, BirthEnd = 2.60f, Life = 0.3f, LifeJitter = 0.2f,
                Close = 12, Mid = 3, Far = 0, Size = new CelCurve(0.95f, 1.2f, 0.5f), SizeJitter = 0.12f, Erode = new CelCurve(0f, 0.7f, 2f),
                Palette = "exp_star", Core = 1.3f, Opacity = 0.40f, Extra = new Vector4(ExpFxKind.Star, 0f, 0f, 10f),
            },
            new CelLayer
            {
                // d14: Close only - the early hazy spray: cream-grey wedge bursts BEHIND the body (the ball hides their middle),
                // so only the feathered spikes show round it, ref f31-f49 (born 0.35-1.2 s = f27-f44)
                Name = "spray haze", Kind = ExpPressureKind.KindName, Anchor = CelAnchor.Target, Offset = new Vector3(-0.2f, 0.15f, 0.5f), Shape = CelShape.Point,
                Start = 0.12f, Duration = 0f, Life = 2.98f, LifeJitter = 0f,
                Close = 1, Mid = 0, Far = 0, Size = new CelCurve(4.4f, 4.4f), SizeJitter = 0f, Erode = new CelCurve(0f, 0f),
                Palette = "exp_spray", Core = 1.0f, Opacity = 1.10f, Extra = new Vector4(ExpFxKind.Star, -1f, 0f, 12f),
            },
            new CelLayer
            {
                // d14: Close only - a dense shower of small bright specks thrown out round the body f30-f50
                Name = "speck shower", Kind = Chunk, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.5f, -1.6f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0.0f, 0.0f),
                Start = 0.3f, Duration = 0.9f, Life = 0.55f, LifeJitter = 0.3f, Close = 90, Mid = 0, Far = 0, Aim = CelAim.Radial, Speed = 9f, SpeedJitter = 0.6f, UpBias = 0.3f,
                Gravity = 4f, Drag = 2f, Spin = 0f, Size = new CelCurve(0.038f, 0.02f), SizeJitter = 0.4f, Stretch = 1f,
                Palette = "exp_speck", Core = 0.5f, Ink = 0f, Extra = new Vector4(4f, 2f, 0.3f, 0f),
            },
            new CelLayer
            {
                // a soft warm bloom about 1.5x the fireball's radius (faint: it must not blow the lobes to white)
                Name = "bloom", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.825f, -2.7f), Start = 0f, Duration = 2.7f, Life = 0.6f, LifeJitter = 0.1f,
                Close = 6, Mid = 3, Far = 0, Size = new CelCurve(2.8f, 3.2f, 0.5f), SizeJitter = 0.08f, Erode = new CelCurve(0f, 1f, 2f),
                Palette = "exp_bloom", Core = 1f, Opacity = 0.12f, Extra = new Vector4(ExpFxKind.Flash, 2.0f, 0.0f, 0f),
            },
            new CelLayer
            {
                Name = "pool", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, -1.395f, 0.0f), Start = 0f, Life = 3.1f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(3.75f, 4.8f, 0.3f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 6f),
                Palette = "exp_pool", Core = 1f, Opacity = 0.15f, Extra = new Vector4(ExpFxKind.Pool, 1.2f, 0f, 0f),
            },
            new CelLayer
            {
                // the warm light over the field: full until ~2.9 s (the ref's ground is brown to f89), then gone by f95
                Name = "wash", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, -1.395f, -3.0f), Start = 0f, Life = 3.15f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(8.25f, 10.5f, 0.7f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 9f),
                Palette = "exp_brown", Core = 1f, Opacity = 1.1f, Extra = new Vector4(ExpFxKind.Pool, 0.5f, 0f, 0f),
            },
            new CelLayer
            {
                // r04: a faint wide warmth over the near field (low chroma brown), the brighter pool stays at the fire
                Name = "wide wash", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(-0.75f, -0.675f, -3.75f), Start = 0f, Life = 3.15f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(18.0f, 24.0f, 0.7f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 9f),
                Palette = "exp_brown", Core = 1f, Opacity = 1.4f, Extra = new Vector4(ExpFxKind.Pool, 0.4f, 0f, 0f),
            },
            new CelLayer
            {
                // the peak (ref f72-f89): the side blasts and the closing burst light the whole near field
                Name = "peak wash", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(-6.0f, -1.395f, 1.0f), Start = 1.9f, Life = 1.3f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(10.0f, 15.5f, 0.6f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 8f),
                Palette = "exp_wash", Core = 1f, Opacity = 1.9f, Extra = new Vector4(ExpFxKind.Pool, 0.6f, 0f, 0f),
            },
            new CelLayer
            {
                // b02: the early fireball, small (the blast builds; ref f31-f68)
                Name = "fireball", LodGroup = "fire-body", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(-0.3f, 0.1f, 0.6f), Shape = CelShape.Box, ShapeSize = new Vector3(0.58f, 0.3f, 0.2f),
                Start = 0.25f, Duration = 1.5f, Life = 0.5f, LifeJitter = 0.3f, Close = 0, Mid = 0, Far = 0,
                Aim = CelAim.Radial, Speed = 0.8f, Drag = 2.5f, Buoyancy = 0.22f, Spin = 2.0f,
                Size = new CelCurve(0.35f, 0.62f, 0.5f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 1f, 3.5f), Feather = 0.08f,
                Palette = "exp_ball_out", CoolTo = "", CoolFrom = 0.8f, Core = 1f, Ink = 1.5f, Extra = new Vector4(1.4f, 0.10f, 0.3f, 0.7f),
            },
            new CelLayer
            {
                // b02: the big fireball growing to the peak (ref f77-f90: 1.5-2x the dummy's height across)
                Name = "fireball big", LodGroup = "fire-body", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(-0.3f, 0.1f, 0.6f), Shape = CelShape.Box, ShapeSize = new Vector3(0.85f, 0.45f, 0.2f),
                Start = 1.1f, Duration = 1.65f, Life = 0.6f, LifeJitter = 0.3f, Close = 0, Mid = 0, Far = 0,
                Aim = CelAim.Radial, Speed = 0.8f, Drag = 2.5f, Buoyancy = 0.2f, Spin = 2.0f,
                Size = new CelCurve(0.44f, 0.78f, 0.5f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 1f, 3.5f), Feather = 0.08f,
                Palette = "exp_ball_out", CoolTo = "", CoolFrom = 0.6f, Core = 1f, Ink = 1.8f, Extra = new Vector4(1.4f, 0.10f, 0.3f, 0.7f),
            },
            new CelLayer
            {
                // b02: the pale white-yellow heart: lobes in front of the orange ones filling most of the ball
                Name = "fire heart", LodGroup = "fire-body", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(-0.65f, 0.45f, -0.1f), Shape = CelShape.Box, ShapeSize = new Vector3(0.15f, 1.1f, 0.2f),
                Start = 0.4f, Duration = 2.35f, Life = 0.45f, LifeJitter = 0.3f, Close = 0, Mid = 0, Far = 0,
                Aim = CelAim.Up, Speed = 0.2f, Drag = 2.5f, Buoyancy = 0.3f, Spin = 2.0f,
                Size = new CelCurve(0.33f, 0.52f, 0.5f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 1f, 3.0f), Feather = 0.08f,
                Palette = "exp_ball_hot", Core = 2.0f, Ink = 0.0f, Extra = new Vector4(1.2f, 0.12f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "first burst", LodGroup = "fire-body", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0.0f, 0.0f),
                Start = 0f, Duration = 0.06f, Life = 0.5f, Close = 0, Mid = 0, Far = 0, Aim = CelAim.Radial, Speed = 2.0f, Drag = 3f, Buoyancy = 0.75f, Spin = 2f,
                Size = new CelCurve(0.4f, 0.7f, 0.4f), Erode = new CelCurve(0f, 1f, 3.5f), Feather = 0.08f,
                Palette = "exp_ball_ink", CoolTo = "", CoolFrom = 0.6f, Core = 1f, Ink = 1.6f, Extra = new Vector4(1.2f, 0.12f, 0.3f, 0.6f),
            },
            new CelLayer
            {
                // grey-tan puffs in a slab just behind the fire: the fireball hides the middle ones, so they show round its
                // outline from ~f40 and grow until the fire sinks into them (f96-f105)
                Name = "wrap", LodGroup = "smoke-wrap", Kind = Puff, Anchor = CelAnchor.Target, Offset = new Vector3(0.55f, -0.3f, 2.3f), Shape = CelShape.Box, ShapeSize = new Vector3(3.2f, 2.1f, 0.6f),
                Start = 1.5f, Duration = 1.35f, Life = 1.5f, LifeJitter = 0.1f, Close = 34, Mid = 8, Far = 0, Aim = CelAim.Radial, Speed = 0.75f, Drag = 1.5f, Buoyancy = 0.18f, Spin = 0.5f,
                Size = new CelCurve(0.5f, 0.9f, 0.4f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 10f), Feather = 0.1f,
                Palette = "exp_wrap", Core = 1f, Ink = 1.5f, Extra = new Vector4(1.3f, 0.16f, 0.6f, 0f),
            },
            new CelLayer
            {
                Name = "wrap late", LodGroup = "smoke-wrap", Kind = Puff, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.0f, 2.4f), Shape = CelShape.Box, ShapeSize = new Vector3(4.5f, 1.15f, 0.6f),
                Start = 2.75f, Duration = 0.4f, Life = 1.5f, LifeJitter = 0.1f, Close = 20, Mid = 6, Far = 0, Aim = CelAim.Radial, Speed = 0.6f, Drag = 1.5f, Buoyancy = 0.04f, Spin = 0.5f,
                Size = new CelCurve(0.5f, 0.85f, 0.4f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 10f), Feather = 0.1f,
                Palette = "exp_wrap", Core = 1f, Ink = 1.5f, Extra = new Vector4(1.3f, 0.16f, 0.6f, 0f),
            },
            new CelLayer
            {
                // r04: grey-tan puffs on a shell round the fire, the front ones over its rim (the star is drawn over them)
                Name = "rim puffs", LodGroup = "smoke-wrap", Kind = Puff, Anchor = CelAnchor.Target, Offset = new Vector3(0.55f, 0.15f, 1.2f), Shape = CelShape.Box, ShapeSize = new Vector3(2f, 1.5f, 0.5f),
                Start = 0.80f, Duration = 1.1f, Life = 1.6f, LifeJitter = 0.2f, Close = 10, Mid = 2, Far = 0, Aim = CelAim.Up, Spread = 35f, Speed = 0.3f, Drag = 1.6f, Buoyancy = 0.05f, Spin = 0.5f,
                Size = new CelCurve(0.30f, 0.62f, 0.4f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 6f), Feather = 0.1f,
                Palette = "exp_wrap", Core = 1f, Ink = 1.0f, Extra = new Vector4(1.3f, 0.16f, 0.6f, 0f),
            },
            new CelLayer
            {
                Name = "sparks", LodGroup = "sparks", Kind = Ribbon, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.3f, -2.6f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.45f, 0.0f, 0.0f),
                Start = 0f, Duration = 2.8f, Life = 0.4f, Close = 80, Mid = 0, Far = 0, Aim = CelAim.Radial, Speed = 30.0f, SpeedJitter = 0.5f, UpBias = 0.7f,
                Gravity = 6.0f, Drag = 1.5f, Size = new CelCurve(0.07f, 0.035f), Stretch = 0.09f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_spark", Core = 2f, Extra = new Vector4(0f, 0.75f, 0.1f, 0.08f),
            },
            new CelLayer
            {
                // b02: ~30 white streaks alive at once spraying radially in front of the fire from ref f31 (orange tails)
                Name = "white streaks", LodGroup = "white-streaks", Kind = Ribbon, Anchor = CelAnchor.Target, Offset = new Vector3(-0.2f, 0.2f, -0.8f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0.0f, 0.0f),
                Start = 0.45f, Duration = 2.5f, Life = 0.3f, LifeJitter = 0.3f, Close = 24, Mid = 0, Far = 0, Aim = CelAim.Radial, Speed = 10.0f, SpeedJitter = 0.5f, UpBias = 0.3f,
                Gravity = 3.0f, Drag = 1.0f, Size = new CelCurve(0.085f, 0.018f), SizeJitter = 0.3f, Stretch = 0.12f, Erode = new CelCurve(0.03f, 0.4f),
                Palette = "exp_arc", Core = 2.5f, Ink = -1f, Extra = new Vector4(0f, 1.4f, 0.15f, 0.45f),
            },
            new CelLayer
            {
                // white lightning-like arcs out of the fireball: 4 px wide, three kinks, up to ~1.5x its radius long
                Name = "arcs", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(-0.2f, 0.15f, -0.8f), Shape = CelShape.Shell, ShapeSize = new Vector3(0.8f, 0.0f, 0.0f),
                Start = 0.4f, Duration = 2.55f, Life = 0.28f, LifeJitter = 0.3f, Close = 0, Mid = 0, Far = 0, Aim = CelAim.Radial, Speed = 2.5f, Drag = 3f, UpBias = 0.2f,
                Size = new CelCurve(0.075f, 0.022f), SizeJitter = 0.3f, Stretch = 2.6f, Erode = new CelCurve(0.03f, 0.3f),
                Palette = "exp_arc", Core = 5f, Opacity = 2.5f, Extra = new Vector4(ExpFxKind.Arc, 0f, 0.3f, 1f),
            },
            new CelLayer
            {
                // orange spark dots on both sides through the late part of the hold
                Name = "dots", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.9f, 0.0f, 0.0f),
                Start = 0.9f, Duration = 2.4f, Life = 0.9f, LifeJitter = 0.3f, Close = 90, Mid = 20, Far = 0, Aim = CelAim.Radial, Speed = 18.0f, SpeedJitter = 0.6f, UpBias = 0.45f,
                Gravity = 9.0f, Drag = 1.2f, Size = new CelCurve(0.112f, 0.06f), Stretch = 0.03f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 3f, Extra = new Vector4(1f, 0f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "debris", Kind = Chunk, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0.0f, 0.0f),
                Start = 0.05f, Duration = 2.7f, Life = 1.0f, Close = 10, Mid = 10, Far = 0, Aim = CelAim.Radial, Speed = 9.0f, SpeedJitter = 0.6f, UpBias = 0.35f,
                Gravity = 14.7f, Drag = 0.6f, Spin = 4f, Size = new CelCurve(0.065f, 0.045f), SizeJitter = 0.5f, Stretch = 2.5f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_chunk", Core = 0.35f, Ink = 0.5f, Extra = new Vector4(4f, 0.001f, 0.35f, 0f),
            },
            new CelLayer
            {
                // d11: a dense burst of small black angular chunks thrown out of the fireball and falling (ref f60-f115)
                Name = "chunk throw", Kind = Chunk, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.5f, -1.6f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.7f, 0.0f, 0.0f),
                Start = 1.35f, Duration = 1.6f, Life = 1.1f, LifeJitter = 0.3f, Close = 85, Mid = 20, Far = 0, Aim = CelAim.Radial, Speed = 5.5f, SpeedJitter = 0.65f, UpBias = 0.45f,
                Gravity = 12f, Drag = 0.6f, Spin = 7f, Size = new CelCurve(0.035f, 0.02f), SizeJitter = 0.55f, Stretch = 1.3f,
                Palette = "exp_chunk", Core = 0.3f, Ink = 1.5f, Extra = new Vector4(4f, 3f, 0.5f, 0f),
            },
            new CelLayer
            {
                Name = "closing burst", Kind = ExpFireKind.KindName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.65f, 0.0f, 0.0f),
                Start = 2.65f, Duration = 0.1f, Life = 0.6f, Close = 8, Mid = 4, Far = 0, Aim = CelAim.Radial, Speed = 1.6f, Drag = 2.5f, Buoyancy = 0.3f, Spin = 2f,
                Size = new CelCurve(0.28f, 0.48f, 0.4f), Erode = new CelCurve(0f, 1f, 3.5f), Feather = 0.08f,
                Palette = "exp_fire_body_v1", CoolTo = "exp_wrap", CoolFrom = 0.15f, Core = 1f, Ink = 1.0f, Extra = new Vector4(0f, 1.2f, 0.06f, 0.1f),
            },
            new CelLayer
            {
                Name = "closing star", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(-0.25f, 0.15f, -0.2f), Start = 2.65f, Duration = 0.35f, BirthEnd = 0.18f, Life = 0.3f,
                Close = 3, Mid = 1, Far = 0, Size = new CelCurve(1.1f, 1.3f, 0.5f), Erode = new CelCurve(0f, 0.7f, 2f),
                Palette = "exp_star", Core = 1.3f, Opacity = 0.50f, Extra = new Vector4(ExpFxKind.Star, 0f, 0f, 11f),
            },
            new CelLayer
            {
                Name = "closing sparks", LodGroup = "closing-sparks", Kind = Ribbon, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0.0f, 0.0f),
                Start = 2.65f, Duration = 0.12f, Life = 0.4f, Close = 12, Mid = 0, Far = 0, Aim = CelAim.Radial, Speed = 33.0f, SpeedJitter = 0.5f, UpBias = 0.4f,
                Gravity = 7.5f, Drag = 1.2f, Size = new CelCurve(0.07f, 0.035f), Stretch = 0.075f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_spark", Core = 1.8f, Ink = -1f, Extra = new Vector4(0f, 1.0f, 0.4f, 0.2f),
            },
            new CelLayer
            {
                Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = 0f, Life = 1.5f,
                Close = 1, Mid = 1, Far = 0, Palette = "exp_fire", Core = 1.6f, Extra = new Vector4(16f, 0.22f, 0f, 0f),
            },
            new CelLayer
            {
                // b02: the light grows to the peak (ref f86 lights the whole near field)
                Name = "light build", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = 1.2f, Life = 1.5f,
                Close = 1, Mid = 1, Far = 0, Palette = "exp_fire", Core = 3.4f, Extra = new Vector4(21f, 0.22f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "light peak", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Offset = new Vector3(-0.3f, 0.4f, -0.12f), Start = 2.65f, Life = 0.45f,
                Close = 1, Mid = 0, Far = 0, Palette = "exp_fire", Core = 1.64f, Extra = new Vector4(23.94f, 0.410f, 0f, 0f),
            },
            // d18: Mid/Far readability only (Close = 0, appended at the END so every Close layer keeps its seed): at
            // O120/O240 the Hold drew only the 0.34 s flash and the pool, a dot. A longer bright warm flash, a few big
            // inked fire lobes with a hot core, and a few big grey puffs read as flash + fireball + puff at distance.
            new CelLayer
            {
                Name = "far flash", LodGroup = "fire-body", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.6f, -3.3f), Start = 0f, Life = 0.7f, LifeJitter = 0f,
                Close = 0, Mid = 0, Far = 0, Size = new CelCurve(3.6f, 4.2f, 0.5f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 1.5f),
                Palette = "exp_dome", Core = 2.0f, Opacity = 0.9f, Extra = new Vector4(ExpFxKind.Flash, 0.6f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "far ball", LodGroup = "fire-body", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.5f, 0.0f), Shape = CelShape.Box, ShapeSize = new Vector3(1.2f, 0.45f, 0.6f),
                Start = 0.15f, Duration = 2.5f, Life = 0.8f, LifeJitter = 0.25f, Close = 0, Mid = 0, Far = 0,
                Aim = CelAim.Radial, Speed = 1.6f, Drag = 2.5f, Buoyancy = 0.2f, Spin = 1.5f,
                Size = new CelCurve(1.1f, 1.7f, 0.5f), SizeJitter = 0.2f, Erode = new CelCurve(0f, 1f, 3.5f), Feather = 0.08f,
                Palette = "exp_ball_out", CoolTo = "exp_wrap", CoolFrom = 0.7f, Core = 1f, Ink = 3.0f, Extra = new Vector4(1.6f, 0.24f, 0.3f, 0.7f),
            },
            new CelLayer
            {
                Name = "far core", LodGroup = "fire-body", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.7f, -3.0f), Start = 0.2f, Duration = 2.5f, Life = 0.6f, LifeJitter = 0.1f,
                Close = 0, Mid = 0, Far = 0, Size = new CelCurve(1.8f, 2.1f, 0.5f), SizeJitter = 0.1f, Erode = new CelCurve(0f, 1f, 2f),
                Palette = "exp_dome", Core = 2.0f, Opacity = 0.8f, Extra = new Vector4(ExpFxKind.Flash, 0.7f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "far puff", LodGroup = "smoke-wrap", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.6f, 0.9f), Shape = CelShape.Box, ShapeSize = new Vector3(3.6f, 1.2f, 0.6f),
                Start = 1.3f, Duration = 1.7f, Life = 1.8f, LifeJitter = 0.15f, Close = 0, Mid = 5, Far = 5, Aim = CelAim.Radial, Speed = 0.6f, Drag = 1.5f, Buoyancy = 0.15f, Spin = 0.4f,
                Size = new CelCurve(1.0f, 1.9f, 0.4f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 10f), Feather = 0.1f,
                Palette = "exp_wrap", Core = 1f, Ink = 3.0f, Extra = new Vector4(1.3f, 0.16f, 0.6f, 0f),
            },
            new CelLayer
            {
                // d20: Mid-only copy of the sparks as kit Streaks (the Close ones are ExpRibbons; appended so no seed re-rolls)
                Name = "sparks mid", LodGroup = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.3f, -2.6f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.45f, 0.0f, 0.0f),
                Start = 0f, Duration = 2.8f, Life = 0.4f, Close = 0, Mid = 20, Far = 0, Aim = CelAim.Radial, Speed = 30.0f, SpeedJitter = 0.5f, UpBias = 0.7f,
                Gravity = 6.0f, Drag = 1.5f, Size = new CelCurve(0.09f, 0.045f), Stretch = 0.09f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_spark", Core = 5f, Extra = new Vector4(0f, 0.3f, 0.5f, 0f),
            },
            new CelLayer
            {
                // d20: Mid-only copy of the white streaks as kit Streaks
                Name = "white streaks mid", LodGroup = "white-streaks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.4f, -2.9f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0.0f, 0.0f),
                Start = 0.45f, Duration = 2.5f, Life = 0.3f, LifeJitter = 0.3f, Close = 0, Mid = 15, Far = 0, Aim = CelAim.Radial, Speed = 13.0f, SpeedJitter = 0.5f, UpBias = 0.3f,
                Gravity = 3.0f, Drag = 1.0f, Size = new CelCurve(0.11f, 0.05f), SizeJitter = 0.3f, Stretch = 0.15f, Erode = new CelCurve(0.03f, 0.4f),
                Palette = "exp_arc", Core = 5f, Extra = new Vector4(0f, 0.4f, 0.6f, 0f),
            },
            new CelLayer
            {
                // d20: Mid-only copy of the closing sparks as kit Streaks
                Name = "closing sparks mid", LodGroup = "closing-sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.6f, 0.0f, 0.0f),
                Start = 2.65f, Duration = 0.12f, Life = 0.5f, Close = 0, Mid = 10, Far = 0, Aim = CelAim.Radial, Speed = 33.0f, SpeedJitter = 0.5f, UpBias = 0.4f,
                Gravity = 7.5f, Drag = 1.2f, Size = new CelCurve(0.105f, 0.053f), Stretch = 0.1f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_spark", Core = 5f, Extra = new Vector4(0f, 0.3f, 0.4f, 0f),
            },
            new CelLayer
            {
                Name = "cooling fire", Kind = ExpFireKind.KindName, Anchor = CelAnchor.Target,
                Offset = new Vector3(0.2f, 0.1f, 0.5f), Shape = CelShape.Box, ShapeSize = new Vector3(1.2f, 0.75f, 0.3f),
                Start = 2.8f, Duration = 0.75f, Life = 0.5f, LifeJitter = 0.15f, Close = 14, Mid = 4, Far = 0,
                Aim = CelAim.Up, Speed = 0.25f, Drag = 1.5f, Buoyancy = 0.05f, Spin = 1f,
                Size = new CelCurve(0.3f, 0.38f, 0.4f), SizeJitter = 0.3f, Erode = new CelCurve(0f, 1f, 2.5f),
                Palette = "exp_fire_body_v1", Core = 1f, Ink = 0.7f, Extra = new Vector4(0f, 1.2f, 0.06f, 0.1f),
            },
            // Close fire replaces only the three earlier Lobe layers; keep these at the end so every other
            // existing layer retains its seed index. Mid/Far still use the original recipe layers.
            new CelLayer
            {
                Name = "fire billows close", LodGroup = "fire-body", Kind = ExpFireKind.KindName,
                Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.12f, 0.65f),
                Shape = CelShape.Box, ShapeSize = new Vector3(1.4f, 1.4f, 0.25f),
                Start = 0.25f, Duration = 1.5f, Life = 0.20f, LifeJitter = 0.12f,
                Close = 0, Mid = 0, Far = 0, Aim = CelAim.Up, Speed = 0.20f, SpeedJitter = 0.20f,
                Spread = 20f, Drag = 2.5f, Buoyancy = 0.12f, Spin = 2.2f,
                Size = new CelCurve(0.72f, 0.99f, 0.65f), SizeJitter = 0.12f, Stretch = 1.02f,
                Erode = new CelCurve(0f, 0.85f, 3f), Feather = 0.012f,
                Palette = "exp_fire_body_v1", Core = 1f, Opacity = 0.98f, Ink = 0f,
                Extra = new Vector4(ExpFireKind.Body, 1.05f, 0.04f, 0.08f),
            },
            new CelLayer
            {
                Name = "fire billows climax close", LodGroup = "fire-body", Kind = ExpFireKind.KindName,
                Anchor = CelAnchor.Target, Offset = new Vector3(0.05f, 0.18f, 0.72f),
                Shape = CelShape.Box, ShapeSize = new Vector3(1.4f, 1.4f, 0.25f),
                Start = 1.1f, Duration = 1.65f, Life = 0.20f, LifeJitter = 0.12f,
                Close = 0, Mid = 0, Far = 0, Aim = CelAim.Up, Speed = 0.22f, SpeedJitter = 0.20f,
                Spread = 20f, Drag = 2.5f, Buoyancy = 0.12f, Spin = 2.4f,
                Size = new CelCurve(0.825f, 1.1325f, 0.65f), SizeJitter = 0.12f, Stretch = 1.08f,
                Erode = new CelCurve(0f, 0.85f, 3f), Feather = 0.012f,
                Palette = "exp_fire_body_v1", Core = 1f, Opacity = 0.98f, Ink = 0f,
                Extra = new Vector4(ExpFireKind.Body, 1.05f, 0.04f, 0.10f),
            },
            new CelLayer
            {
                Name = "vertical impact heart close", LodGroup = "fire-body", Kind = ExpFireKind.KindName,
                Anchor = CelAnchor.Target, Offset = new Vector3(-0.30f, 0.40f, -0.12f),
                Shape = CelShape.Box, ShapeSize = new Vector3(0.12f, 0.30f, 0.10f),
                Start = 0.4f, Duration = 2.35f, Life = 0.16f, LifeJitter = 0.10f,
                Close = 0, Mid = 0, Far = 0, Aim = CelAim.Up, Speed = 0.12f, SpeedJitter = 0.15f,
                Spread = 10f, Drag = 2.5f, Buoyancy = 0.08f, Spin = 2.8f,
                Size = new CelCurve(1.10f, 1.28f, 0.60f), SizeJitter = 0.08f, Stretch = 1.08f,
                Erode = new CelCurve(0f, 0.75f, 3f), Feather = 0.010f,
                Palette = "exp_fire_core_v1", Core = 1.4f, Opacity = 0.96f, Ink = 0f,
                Extra = new Vector4(ExpFireKind.Heart, 1.0f, 0.045f, -0.035f),
            },
            MakeImpactHeart("early impact heart", 0.4f, 0.65f, 0, 0.38f, 0.30f, 1.08f),
            MakeImpactHeart("sustained impact heart", 1.05f, 1.20f, 0, 0.40f, 0.30f, 1.08f),
            MakeImpactHeart("peak impact heart", 2.35f, 0.40f, 0, 0.38f, 0.75f, 1.50f),
            new CelLayer
            {
                Name = "continuous authored impact heart", LodGroup = "fire-body", Kind = ExpHeartKind.KindName,
                Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.12f), Shape = CelShape.Point,
                Start = 0.05f, Duration = 0f, Life = 3.35f, LifeJitter = 0f, Close = 1, Mid = 1, Far = 1,
                Aim = CelAim.None, Speed = 0f, SpeedJitter = 0f, Buoyancy = 0f, Spin = 0f,
                Size = new CelCurve(1f, 1f), SizeJitter = 0f, Stretch = 1f,
                Erode = new CelCurve(0f, 0f), Feather = 0.010f,
                Palette = "exp_fire_core_v1", Core = 1.4f, Opacity = 0.96f, Ink = 0f,
                Extra = new Vector4(ExpFireKind.Heart, -2f, 0.045f, 0f),
            },
            new CelLayer
            {
                Name = "continuous authored orange surround", LodGroup = "fire-body", Kind = ExpBodyUnderlayKind.KindName,
                Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, 0.65f), Shape = CelShape.Point,
                Start = 0.25f, Duration = 0f, Life = 3.75f, LifeJitter = 0f, Close = 1, Mid = 1, Far = 0,
                Aim = CelAim.None, Speed = 0f, SpeedJitter = 0f, Buoyancy = 0f, Spin = 0f,
                Size = new CelCurve(1f, 1f), SizeJitter = 0f, Stretch = 1f,
                Erode = new CelCurve(0f, 0f), Feather = 0.012f,
                Palette = "exp_fire_body_v1", Core = 1f, Opacity = 0.98f, Ink = 0f,
                Extra = new Vector4(ExpFireKind.Body, -2f, 0.035f, 0f),
            },
            MakeArcPhase("early upper hook", 0.30f, 0.55f, 0.40f, 2, 0.18f, 4.30f, 79f, 0.06f),
            MakeArcPhase("early right hook", 0.34f, 0.50f, 0.36f, 2, 0.18f, 2.90f, 22f, 0.16f),
            MakeArcPhase("early lower-left hook", 0.33f, 1.05f, 0.38f, 4, 0.20f, 3.40f, 215f, 0.12f),
            MakeArcPhase("quiet middle arcs", 1.35f, 0.75f, 0.24f, 6, 0.055f, 2.00f, 0f, 0.30f),
            MakeArcPhase("late upper-left hook", 2.12f, 0f, 0.48f, 1, 0.22f, 3.50f, 165f, -0.22f),
            MakeArcPhase("late diagonal-left hook", 2.13f, 0.33f, 0.38f, 1, 0.14f, 3.50f, 215f, 0.12f),
            MakeArcPhase("late lateral-left hook", 2.50f, 0.60f, 0.40f, 1, 0.13f, 4.00f, 190f, 0.20f),
            MakeArcPhase("late upper hook", 2.15f, 0.85f, 0.32f, 2, 0.09f, 3.20f, 108f, 0.10f),
            // Appended so the existing main fire/smoke/debris layer indices stay stable.
            MakeArcPhase("later upper-left hook", 2.67f, 0f, 0.36f, 1, 0.14f, 3.50f, 165f, -0.22f),
            new CelLayer
            {
                Name = "early upper paired tracer", Kind = Fx, Anchor = CelAnchor.Origin,
                Offset = new Vector3(0f, 0.35f, 0f), Shape = CelShape.Point,
                Start = 0f, Duration = 0f, Life = 0.28f, LifeJitter = 0f, Close = 1, Mid = 0, Far = 0,
                Aim = CelAim.Target, Speed = 0f, SpeedJitter = 0f, Spread = 0f, Arrive = 0.857143f,
                Size = new CelCurve(0.15f, 0.15f), SizeJitter = 0f, Stretch = 0.11f,
                Erode = new CelCurve(0.05f, 0.25f), Palette = "exp_lead", Core = 2.2f, Opacity = 1f,
                Extra = new Vector4(4.4f, 0.09f, 0.105f, 0.24f),
            },
            new CelLayer
            {
                Name = "onset pressure wisps", Kind = ExpPressureKind.KindName, Anchor = CelAnchor.Target,
                Offset = new Vector3(-0.45f, 0.05f, 0.5f), Shape = CelShape.Point,
                Start = 0.08f, Life = 0.22f, LifeJitter = 0f, Close = 1, Mid = 0, Far = 0,
                Size = new CelCurve(2.4f, 3.4f, 0.5f), SizeJitter = 0f,
                Erode = new CelCurve(0f, 0.7f, 2f), Palette = "exp_spray", Core = 1f, Opacity = 1.10f,
                Extra = new Vector4(ExpFxKind.Star, -2f, 0f, 12f),
            },
            new CelLayer
            {
                // Broad warm impact haze is independent of the opaque heart and its white pulses.
                Name = "opening warm haze", Kind = Fx, Anchor = CelAnchor.Target,
                Offset = new Vector3(-0.35f, 0.10f, -0.15f), Shape = CelShape.Point,
                Start = 0.05f, Life = 0.36f, LifeJitter = 0f, Close = 1, Mid = 0, Far = 0,
                Size = new CelCurve(3.1f, 4.1f, 0.4f), SizeJitter = 0f,
                Erode = new CelCurve(0f, 1f, 2f), Palette = "exp_bloom", Core = 0.65f, Opacity = 0.75f,
                Extra = new Vector4(ExpFxKind.Flash, 1.65f, 0f, 0.85f),
            },
            new CelLayer
            {
                // Appended: preserve every prior layer index and its seeded birth schedule.
                Name = "continuous warm body islands", LodGroup = "fire-body", Kind = ExpWarmBodyKind.KindName,
                Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, 0.65f), Shape = CelShape.Point,
                Start = 0.25f, Duration = 0f, Life = 3.30f, LifeJitter = 0f, Close = 1, Mid = 1, Far = 0,
                Aim = CelAim.None, Speed = 0f, SpeedJitter = 0f, Buoyancy = 0f, Spin = 0f,
                Size = new CelCurve(1f, 1f), SizeJitter = 0f, Stretch = 1f,
                Erode = new CelCurve(0f, 0f), Feather = 0.010f,
                Palette = "exp_fire_core_v1", Core = 1f, Opacity = 1f, Ink = 0f,
                Extra = new Vector4(3f, -1f, 0f, 0f),
            },
            // Close and Mid share the connected fire fields; Far retains only the continuous Heart.
            // MAIN_RADIANT_MATERIAL_35: appended warm target veil, below the narrow Heart centre.
            new CelLayer
            {
                Name = "main radiant warm veil", Kind = Fx, Anchor = CelAnchor.Target,
                Offset = new Vector3(-0.28f, 0.08f, -0.12f), Shape = CelShape.Point,
                Start = 0.28f, Duration = 0f, Life = 2.85f, LifeJitter = 0f, Close = 1, Mid = 0, Far = 0,
                Size = new CelCurve(1.8f, 2.2f, 0.65f), SizeJitter = 0f,
                Erode = new CelCurve(0.12f, 0.42f, 4f), Palette = "exp_fire_core_v1", Core = 1.3f, Opacity = 0.36f,
                Extra = new Vector4(ExpFxKind.Flash, 1.55f, 0f, 0.85f),
            },
            new CelLayer
            {
                Name = "main radiant peak veil", Kind = Fx, Anchor = CelAnchor.Target,
                Offset = new Vector3(-0.32f, 0.12f, -0.12f), Shape = CelShape.Point,
                Start = 2.55f, Duration = 0f, Life = 0.43f, LifeJitter = 0f, Close = 1, Mid = 0, Far = 0,
                Size = new CelCurve(1.8f, 2.2f, 0.55f), SizeJitter = 0f,
                Erode = new CelCurve(0f, 1f, 1.7f), Palette = "exp_fire_core_v1", Core = 1.2f, Opacity = 0.257f,
                Extra = new Vector4(ExpFxKind.Flash, 1.55f, 0f, 0.85f),
            },
            new CelLayer
            {
                // Bridge target illumination without an emissive glow card.
                Name = "main target light bridge", Kind = CelKinds.LightName, Anchor = CelAnchor.Target,
                Offset = new Vector3(-0.3f, 0.4f, -0.12f), Start = 2.35f, Life = 0.5f,
                Close = 1, Mid = 0, Far = 0, Palette = "exp_fire", Core = 3.6f,
                Extra = new Vector4(21f, 0.000001f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "impact warm splinters", Kind = Fx, Anchor = CelAnchor.Target,
                Offset = new Vector3(-0.62f, 0.06f, -0.12f), Shape = CelShape.Point,
                Start = 0.22f, Life = 2.90f, Close = 1, Mid = 0, Far = 0,
                Size = new CelCurve(1.85f, 2.05f, 0.8f), Erode = new CelCurve(0f, 0f),
                Palette = "exp_fire_core_v1", Core = 1f, Opacity = 0.95f,
                Extra = new Vector4(ExpFxKind.Star, 2f, 0f, 13f),
            },
            new CelLayer
            {
                // Near-white reflected heat on the target, independent of bloom cards.
                Name = "target face fill early", Kind = CelKinds.LightName, Anchor = CelAnchor.Target,
                Offset = new Vector3(-0.15f, 0.75f, -1.20f), Start = 0.30f, Life = 1.00f,
                Close = 1, Mid = 0, Far = 0, Palette = "exp_fire_core_v1", Core = 0.90f,
                Extra = new Vector4(3.0f, 0.000001f, 0f, 0f),
            },
            new CelLayer
            {
                // Near-white reflected heat on the target, independent of bloom cards.
                Name = "target face fill middle", Kind = CelKinds.LightName, Anchor = CelAnchor.Target,
                Offset = new Vector3(-0.15f, 0.75f, -1.20f), Start = 1.20f, Life = 1.00f,
                Close = 1, Mid = 0, Far = 0, Palette = "exp_fire_core_v1", Core = 1.30f,
                Extra = new Vector4(3.0f, 0.000001f, 0f, 0f),
            },
            new CelLayer
            {
                // Near-white reflected heat on the target, independent of bloom cards.
                Name = "target face fill peak", Kind = CelKinds.LightName, Anchor = CelAnchor.Target,
                Offset = new Vector3(-0.15f, 0.75f, -1.20f), Start = 2.65f, Life = 0.25f,
                Close = 1, Mid = 0, Far = 0, Palette = "exp_fire_core_v1", Core = 2.00f,
                Extra = new Vector4(3.0f, 0.000001f, 0f, 0f),
            },
            new CelLayer
            {
                // The target retains red reflected heat after the white peak has fallen away.
                Name = "target face fill decay", Kind = CelKinds.LightName, Anchor = CelAnchor.Target,
                Offset = new Vector3(-0.15f, 0.75f, -1.20f), Start = 3.00f, Life = 0.60f,
                Close = 1, Mid = 0, Far = 0, Palette = "exp_target_afterglow", Core = 0.36f,
                Extra = new Vector4(3.0f, 0.000001f, 0f, 0f),
            },
            new CelLayer
            {
                // Closing smoke-edge streak: born after the white peak; no added central flash.
                Name = "closing right smoke-edge hook", LodGroup = "white-arcs", Kind = ExpArcKind.KindName,
                Anchor = CelAnchor.Target, Offset = new Vector3(0.925557f, -0.556596f, -0.256872f), Shape = CelShape.Point,
                Start = 2.81f, Duration = 0f, Life = 0.72f, LifeJitter = 0f, Close = 1, Mid = 0, Far = 0,
                Aim = CelAim.None, Speed = 0f, SpeedJitter = 0f, Gravity = 0f, Drag = 0f,
                Size = new CelCurve(0.140f, 0.0392f), SizeJitter = 0f, Stretch = 3.25f,
                Erode = new CelCurve(0.03f, 0.90f, 4f), Palette = "exp_arc", Core = 5f, Opacity = 2.5f,
                Extra = new Vector4(ExpFxKind.Arc, 65.0f, -0.20f, 3f),
            },
            new CelLayer
            {
                // Closing smoke-edge streak: born after the white peak; no added central flash.
                Name = "closing lower-left tail hook", LodGroup = "white-arcs", Kind = ExpArcKind.KindName,
                Anchor = CelAnchor.Target, Offset = new Vector3(-0.170780f, 0.108589f, -0.202039f), Shape = CelShape.Point,
                Start = 2.81f, Duration = 0f, Life = 0.54f, LifeJitter = 0f, Close = 1, Mid = 0, Far = 0,
                Aim = CelAim.None, Speed = 0f, SpeedJitter = 0f, Gravity = 0f, Drag = 0f,
                Size = new CelCurve(0.150f, 0.0420f), SizeJitter = 0f, Stretch = 2.46f,
                Erode = new CelCurve(0.03f, 0.90f, 4f), Palette = "exp_arc", Core = 5f, Opacity = 2.5f,
                Extra = new Vector4(ExpFxKind.Arc, 239.5f, 0.07f, 2f),
            }
        );

        static CelLayer MakeImpactHeart(string name, float start, float duration, int count, float height, float spread, float aspect)
        {
            return new CelLayer
            {
                Name = name, LodGroup = "fire-body", Kind = ExpFireKind.KindName, Anchor = CelAnchor.Target,
                Offset = new Vector3(-0.18f, height, -0.12f), Shape = CelShape.Box, ShapeSize = new Vector3(0.12f, spread, 0.10f),
                Start = start, Duration = duration, Life = 0.16f, LifeJitter = 0.10f, Close = count, Mid = 0, Far = 0,
                Aim = CelAim.Up, Speed = 0.12f, SpeedJitter = 0.15f, Spread = 10f, Drag = 2.5f, Buoyancy = 0.08f, Spin = 2.8f,
                Size = new CelCurve(start < 0.5f ? 1.32f : 1.10f, start < 0.5f ? 1.54f : 1.28f, 0.60f), SizeJitter = 0.08f, Stretch = aspect,
                Erode = new CelCurve(0f, 0.75f, 3f), Feather = 0.010f,
                Palette = "exp_fire_core_v1", Core = 1.4f, Opacity = 0.96f, Ink = 0f,
                Extra = new Vector4(ExpFireKind.Heart, 1.0f, 0.045f, -0.035f),
            };
        }

        /// <summary>Incoming rounds and the contact-to-dome transition, timed independently of the sustained hit.</summary>
        public static readonly CelFxRecipe Lead = new CelFxRecipe("explosion_lead", 0.6f,
            new CelLayer
            {
                Name = "first incoming", Kind = Fx, Anchor = CelAnchor.Origin,
                Start = 0.063f, Life = 0.245f, LifeJitter = 0f, Close = 1, Mid = 1, Far = 0,
                Aim = CelAim.Target, Speed = 0f, SpeedJitter = 0f, Spread = 0f, Arrive = 0.873469f,
                Size = new CelCurve(0.18f, 0.18f), SizeJitter = 0f, Stretch = 0.12f,
                Palette = "exp_lead", Core = 2.2f, Opacity = 1f,
                Extra = new Vector4(4.2f, 0.11636f, 0.116f, 0.214f),
            },
            new CelLayer
            {
                Name = "contact", Kind = Fx, Anchor = CelAnchor.Target,
                Start = 0.3f, Life = 0.09f, LifeJitter = 0f, Close = 1, Mid = 1, Far = 0,
                Size = new CelCurve(0.3f, 0.75f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 2f),
                Palette = "exp_dome", Core = 1.8f, Opacity = 1f,
                Extra = new Vector4(ExpFxKind.Flash, 1f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "growing onset", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.55f, -0.3f, 0.2f),
                Start = 0.3f, Life = 0.3f, LifeJitter = 0f, Close = 1, Mid = 0, Far = 0,
                Aim = CelAim.Up, Speed = 8f, SpeedJitter = 0f, Spread = 0f, Drag = 0f,
                Size = new CelCurve(0.25f, 3.6f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 4f),
                Palette = "exp_onset", Core = 2.5f, Opacity = 1.1f,
                Extra = new Vector4(ExpFxKind.Onset + 0.1f, 0.65f, 0.8443f, 0f),
            },
            new CelLayer
            {
                Name = "contact light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target,
                Start = 0.3f, Life = 0.4f, Close = 1, Mid = 1, Far = 0,
                Palette = "exp_fire", Core = 0.8f, Extra = new Vector4(8f, 0.22f, 0f, 0f),

            });

        public static readonly CelFxRecipe LeadSecond = new CelFxRecipe("explosion_lead_second", 0.5f,
            new CelLayer
            {
                Name = "second incoming", Kind = Fx, Anchor = CelAnchor.Origin,
                Start = 0.153f, Life = 0.21f, LifeJitter = 0f, Close = 1, Mid = 1, Far = 0,
                Aim = CelAim.Target, Speed = 0f, SpeedJitter = 0f, Spread = 0f, Arrive = 0.861905f,
                Size = new CelCurve(0.14f, 0.14f), SizeJitter = 0f, Stretch = 0.10f,
                Palette = "exp_lead", Core = 2.2f, Opacity = 1f,
                Extra = new Vector4(4.1f, 0.09f, 0.068f, 0.181f),
            });


        // The third round has a faster measured crossing and an independent visibility gate.
        public static readonly CelFxRecipe LeadThird = new CelFxRecipe("explosion_lead_third", 0.52f,
            new CelLayer
            {
                Name = "third incoming", Kind = Fx, Anchor = CelAnchor.Origin,
                Start = 0.324407f, Life = 0.18f, LifeJitter = 0f, Close = 1, Mid = 1, Far = 0,
                Aim = CelAim.Target, Arrive = 0.763356f, Spread = 0f,
                Size = new CelCurve(0.20f, 0.20f), SizeJitter = 0f, Stretch = 0.105f,
                Palette = "exp_lead", Core = 2.2f, Opacity = 1f,
                Extra = new Vector4(ExpFxKind.Tracer, 0.05f, 0.0605f, 0.137404f),
            });

        // Secondary impact aftertrails grow upward from the flash before cooling to red.
        public static readonly CelFxRecipe ColumnLeft = MakeColumn("explosion_column_left", 0.23f, 2.6f, 7.5f, 0.20f, 0.292f);
        public static readonly CelFxRecipe ColumnRight = MakeColumn("explosion_column_right", 0.40f, 1.6f, 5.5f, 0.30f, 0.292f);
        public static readonly CelFxRecipe ColumnMiddle = MakeColumn("explosion_column_middle", 0.20f, 2.8f, 2.8f, 0.20f, 0.208f);
        static CelFxRecipe MakeColumn(string name, float width, float firstSpan, float lastSpan, float growth, float life)
        {
            return new CelFxRecipe(name, life, new CelLayer
            {
                Name = "impact aftertrail", Kind = Fx, Anchor = CelAnchor.Target, Close = 1, Mid = 1, Far = 0,
                Life = life, LifeJitter = 0f, Aim = CelAim.Dir, Speed = 1f, SpeedJitter = 0f, Spread = 0f,
                Size = new CelCurve(width, width), SizeJitter = 0f, Stretch = firstSpan,
                Palette = "exp_lead", Core = lastSpan < 3f ? 1.5f : 2.2f, Opacity = lastSpan < 3f ? 0.85f : 1f, Extra = new Vector4(7f, lastSpan, 0.55f, growth),
            });
        }

        static CelLayer MakeSiteFire(float scale, float start, float radiusMultiplier = 1f, float heightMultiplier = 1f, float lifeOverride = 0f)
        {
            return new CelLayer
            {
                Name = "fire transition", Kind = ExpSiteFireKind.KindName, Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, (scale > 1.1f ? 0.5f : 1.0f) * scale * heightMultiplier, 0f), Shape = CelShape.Box,
                ShapeSize = new Vector3(1.5f, scale > 1.1f ? 0.8f : 1.3f, 0.3f) * scale,
                Start = start, Duration = 0.24f, Life = lifeOverride > 0f ? lifeOverride : (scale > 1.1f ? 0.24f : 0.38f), LifeJitter = 0.08f,
                Close = 10, Mid = 3, Far = 0, Aim = CelAim.Up, Spread = 25f,
                Speed = 1.1f * scale, SpeedJitter = 0.2f, Drag = 1.8f, Gravity = 0.6f,
                Buoyancy = 0.1f, Spin = 2.3f,
                Size = new CelCurve(0.35f * scale * radiusMultiplier, 0.55f * scale * radiusMultiplier, 0.55f),
                SizeJitter = 0.18f, Stretch = 1.15f,
                Erode = new CelCurve(0f, 1f, 2.5f), Feather = 0.015f,
                Palette = "exp_fire_body_v1", Core = 1f, Opacity = 1f,
                Extra = new Vector4(ExpFireKind.SiteBody, 1.05f, 0.04f, 0.08f),
            };
        }

        public static readonly CelFxRecipe Site = new CelFxRecipe("explosion_site", 1.2f,
            new CelLayer
            {
                // a soft dome of white-yellow light, no wider than about twice the dummy's height, gone in 8 frames
                Name = "dome", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.312f, 0f), Life = 0.36f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Aim = CelAim.Up, Speed = 4f, SpeedJitter = 0f, Spread = 0f, Size = new CelCurve(2.05f, 2.65f, 0.4f), SizeJitter = 0.08f, Erode = new CelCurve(0f, 1f, 3.0f),
                Palette = "exp_dome", Core = 3.5f, Opacity = 1.15f, Extra = new Vector4(ExpFxKind.Onset, 0.65f, 0.312f, 0.55f),
            },
            new CelLayer
            {
                Name = "pool", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.025f, 0.0f), Life = 0.9f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(2.75f, 3.75f, 0.4f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 2f),
                Palette = "exp_pool", Core = 1f, Extra = new Vector4(ExpFxKind.Pool, 1.7f, 0f, 0f),
            },
            new CelLayer
            {
                // r03: no fireball at a side site; after the dome, an orange spark plume thrown up out of the ground
                Name = "plume", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.25f, 0.0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0.0f, 0.0f),
                Start = 0.05f, Duration = 0.45f, Life = 0.8f, LifeJitter = 0.3f, Close = 30, Mid = 10, Far = 0, Aim = CelAim.Up, Spread = 50f, Speed = 15.0f, SpeedJitter = 0.5f,
                Gravity = 11.25f, Drag = 0.8f, Size = new CelCurve(0.15f, 0.075f), SizeJitter = 0.3f, Stretch = 0.08f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 3f, Extra = new Vector4(0f, 0.3f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.625f, 0.0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.375f, 0.0f, 0.0f),
                Duration = 0.3f, Life = 0.55f, Close = 40, Mid = 12, Far = 0, Aim = CelAim.Radial, Speed = 22.5f, SpeedJitter = 0.5f, UpBias = 0.6f,
                Gravity = 7.5f, Drag = 1.2f, Size = new CelCurve(0.09f, 0.05f), Stretch = 0.07f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_spark", Core = 4f, Extra = new Vector4(0f, 0.3f, 0.4f, 0f),
            },
            new CelLayer
            {
                Name = "debris", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.5f, 0.0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0.0f, 0.0f),
                Start = 0.04f, Duration = 0.3f, Life = 1.3f, LifeJitter = 0.2f, Close = 20, Mid = 6, Far = 0, Aim = CelAim.Radial, Speed = 5.0f, SpeedJitter = 0.6f, UpBias = 0.45f,
                Gravity = 14f, Drag = 0.5f, Spin = 4f, Size = new CelCurve(0.09f, 0.055f), SizeJitter = 0.4f, Stretch = 0.7f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_chunk", Core = 1f, Ink = 2.5f, Extra = new Vector4(4.5f, 0.75f, 0f, 0f),
            },
            new CelLayer
            {
                // r05: big chunks thrown high, a cluster against the fog (ref f59-f87)
                Name = "big chunks", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.75f, -0.9f), Shape = CelShape.Sphere, ShapeSize = new Vector3(1.0f, 0.0f, 0.0f),
                Start = 0.06f, Duration = 0.5f, Life = 1.4f, LifeJitter = 0.25f, Close = 15, Mid = 4, Far = 0, Aim = CelAim.Radial, Speed = 3.0f, SpeedJitter = 0.5f, UpBias = 0.2f,
                Gravity = 14f, Drag = 0.6f, Spin = 3f, Size = new CelCurve(0.08f, 0.05f), SizeJitter = 0.4f, Stretch = 0.6f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_chunk", Core = 1f, Ink = 2.2f, Extra = new Vector4(4.5f, 0.75f, 0f, 0f),
            },
            new CelLayer
            {
                // d11: dense black angular chunks thrown up and out over the side blast, falling (ref f87-f91)
                Name = "chunk burst", Kind = Chunk, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.55f, 0f), Shape = CelShape.Disc, ShapeSize = new Vector3(0.5f, 0.0f, 0.0f),
                Start = 0.05f, Duration = 0.45f, Life = 1.3f, LifeJitter = 0.3f, Close = 30, Mid = 10, Far = 0, Aim = CelAim.Radial, Speed = 7.0f, SpeedJitter = 0.65f, UpBias = 0.55f,
                Gravity = 12f, Drag = 0.6f, Spin = 7f, Size = new CelCurve(0.045f, 0.025f), SizeJitter = 0.55f, Stretch = 1.2f,
                Palette = "exp_dirt", Core = 0.3f, Opacity = 0.55f, Ink = 1.5f, Extra = new Vector4(4f, 3f, 0.5f, 0.50f),
            },
            new CelLayer
            {
                // r05: grey puffs standing round the site after its dome
                Name = "puffs", Kind = Puff, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.875f, 0.0f), Shape = CelShape.Box, ShapeSize = new Vector3(2.5f, 1.0f, 1.0f),
                Start = 0.22f, Duration = 0.18f, Life = 1.4f, LifeJitter = 0.2f, Close = 13, Mid = 3, Far = 0, Aim = CelAim.Radial, Speed = 1.0f, Drag = 1.2f, Buoyancy = 0.188f, Spin = 0.4f,
                Size = new CelCurve(0.35f, 1.2f, 0.35f), SizeJitter = 0.3f, Erode = new CelCurve(0.04f, 1f, 5f), Feather = 0.1f,
                Palette = "exp_wrap", Core = 1f, Ink = 1.5f, Extra = new Vector4(1.6f, 0.24f, 0.6f, -0.65f),
            }, MakeSiteFire(1.0f, 0.18f),
            new CelLayer
            {
                // Normal rear sites: a few rounded dark masses stay within the smoke while fine grit flies out.
                Name = "rounded site debris", Kind = Chunk, Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, 1.0f, 0f), Shape = CelShape.Box, ShapeSize = new Vector3(2.5f, 1.2f, 0.25f),
                Start = 0.12f, Duration = 0.18f, Life = 1.10f, LifeJitter = 0.10f, Close = 8, Mid = 3, Far = 0,
                Aim = CelAim.Radial, Speed = 0.45f, SpeedJitter = 0.20f, UpBias = 0.35f,
                Gravity = 0.8f, Drag = 1.4f, Spin = 1.2f,
                Size = new CelCurve(0.20f, 0.035f, 2.5f), SizeJitter = 0.30f, Stretch = 1.15f,
                Palette = "exp_smoke_debris", Core = 0.30f, Opacity = 1.0f, Ink = 0.55f,
                Extra = new Vector4(-1f, 0f, 0.35f, 0.50f),
            });

        // d17: sites 0/1 (the left smoke cluster, ref f59-f87): Site with smaller puffs only - same exp_wrap
        // colour, life and layer order (so the seeds match Site)
        public static readonly CelFxRecipe SiteNear = new CelFxRecipe("explosion_site_near", 1.2f,
            new CelLayer
            {
                // a soft dome of white-yellow light, no wider than about twice the dummy's height, gone in 8 frames
                Name = "dome", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.312f, 0f), Life = 0.24f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Aim = CelAim.Up, Speed = 4f, SpeedJitter = 0f, Spread = 0f, Size = new CelCurve(2.05f, 2.65f, 0.4f), SizeJitter = 0.08f, Erode = new CelCurve(0f, 1f, 3.0f),
                Palette = "exp_dome", Core = 3.5f, Opacity = 1.15f, Extra = new Vector4(ExpFxKind.Onset, 0.65f, 0.312f, 0.55f),
            },
            new CelLayer
            {
                Name = "pool", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.025f, 0.0f), Life = 0.9f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(2.75f, 3.75f, 0.4f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 2f),
                Palette = "exp_pool", Core = 1f, Extra = new Vector4(ExpFxKind.Pool, 1.7f, 0f, 0f),
            },
            new CelLayer
            {
                // r03: no fireball at a side site; after the dome, an orange spark plume thrown up out of the ground
                Name = "plume", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.25f, 0.0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0.0f, 0.0f),
                Start = 0.05f, Duration = 0.45f, Life = 0.8f, LifeJitter = 0.3f, Close = 30, Mid = 10, Far = 0, Aim = CelAim.Up, Spread = 50f, Speed = 15.0f, SpeedJitter = 0.5f,
                Gravity = 11.25f, Drag = 0.8f, Size = new CelCurve(0.15f, 0.075f), SizeJitter = 0.3f, Stretch = 0.08f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 3f, Extra = new Vector4(0f, 0.3f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.625f, 0.0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.375f, 0.0f, 0.0f),
                Duration = 0.3f, Life = 0.55f, Close = 40, Mid = 12, Far = 0, Aim = CelAim.Radial, Speed = 22.5f, SpeedJitter = 0.5f, UpBias = 0.6f,
                Gravity = 7.5f, Drag = 1.2f, Size = new CelCurve(0.09f, 0.05f), Stretch = 0.07f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_spark", Core = 4f, Extra = new Vector4(0f, 0.3f, 0.4f, 0f),
            },
            new CelLayer
            {
                Name = "debris", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.5f, 0.0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.5f, 0.0f, 0.0f),
                Start = 0.04f, Duration = 0.3f, Life = 1.3f, LifeJitter = 0.2f, Close = 8, Mid = 6, Far = 0, Aim = CelAim.Radial, Speed = 5.0f, SpeedJitter = 0.6f, UpBias = 0.45f,
                Gravity = 14f, Drag = 0.5f, Spin = 4f, Size = new CelCurve(0.07f, 0.045f), SizeJitter = 0.4f, Stretch = 0.7f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_chunk", Core = 1f, Ink = 2.5f, Extra = new Vector4(4.5f, 0.75f, 0f, 0f),
            },
            new CelLayer
            {
                // r05: big chunks thrown high, a cluster against the fog (ref f59-f87)
                Name = "big chunks", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.75f, -0.9f), Shape = CelShape.Sphere, ShapeSize = new Vector3(1.0f, 0.0f, 0.0f),
                Start = 0.06f, Duration = 0.5f, Life = 1.4f, LifeJitter = 0.25f, Close = 8, Mid = 4, Far = 0, Aim = CelAim.Radial, Speed = 3.0f, SpeedJitter = 0.5f, UpBias = 0.2f,
                Gravity = 14f, Drag = 0.6f, Spin = 3f, Size = new CelCurve(0.07f, 0.045f), SizeJitter = 0.4f, Stretch = 0.6f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_chunk", Core = 1f, Ink = 2.2f, Extra = new Vector4(4.5f, 0.75f, 0f, 0f),
            },
            new CelLayer
            {
                // d11: dense black angular chunks thrown up and out over the side blast, falling (ref f87-f91)
                Name = "chunk burst", Kind = Chunk, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.55f, 0f), Shape = CelShape.Disc, ShapeSize = new Vector3(0.5f, 0.0f, 0.0f),
                Start = 0.05f, Duration = 0.45f, Life = 1.3f, LifeJitter = 0.3f, Close = 30, Mid = 12, Far = 0, Aim = CelAim.Radial, Speed = 7.0f, SpeedJitter = 0.65f, UpBias = 0.55f,
                Gravity = 12f, Drag = 0.6f, Spin = 7f, Size = new CelCurve(0.10f, 0.065f), SizeJitter = 0.55f, Stretch = 1.7f,
                Palette = "exp_dirt", Core = 0.3f, Opacity = 0.55f, Ink = 1.5f, Extra = new Vector4(4f, 3f, 0.5f, 0.50f),
            },
            new CelLayer
            {
                // r05: grey puffs standing round the site after its dome
                Name = "puffs", Kind = Puff, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.875f, 0.0f), Shape = CelShape.Box, ShapeSize = new Vector3(2.5f, 1.0f, 1.0f),
                Start = 0.22f, Duration = 0.18f, Life = 1.4f, LifeJitter = 0.2f, Close = 11, Mid = 3, Far = 0, Aim = CelAim.Radial, Speed = 1.0f, Drag = 1.2f, Buoyancy = 0.188f, Spin = 0.4f,
                Size = new CelCurve(0.28f, 0.75f, 0.35f), SizeJitter = 0.3f, Erode = new CelCurve(0.04f, 1f, 5f), Feather = 0.1f,
                Palette = "exp_wrap", Core = 1f, Ink = 1.5f, Extra = new Vector4(1.6f, 0.24f, 0.6f, -0.65f),
            }, MakeSiteFire(1.2f, 0.14f));

        // r04: a side blast far across the field (ref f77): the same pieces with a bigger, brighter dome
        public static readonly CelFxRecipe SiteFar = new CelFxRecipe("explosion_site_far", 1.2f,
            new CelLayer
            {
                // a soft dome of white-yellow light, no wider than about twice the dummy's height, gone in 8 frames
                Name = "dome", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.225f, 0f), Life = 0.30f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 1, Aim = CelAim.Up, Speed = 4f, SpeedJitter = 0f, Spread = 0f, Size = new CelCurve(2.0f, 2.55f, 0.4f), SizeJitter = 0.08f, Erode = new CelCurve(0f, 1f, 3f),
                Palette = "exp_dome", Core = 2.7f, Opacity = 1.2f, Extra = new Vector4(ExpFxKind.Onset + 0.2f, 0.55f, 0.225f, 0.3f),
            },
            new CelLayer
            {
                Name = "pool", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.015f, 0.0f), Life = 0.9f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(1.65f, 2.25f, 0.4f), SizeJitter = 0f, Erode = new CelCurve(0f, 1f, 2f),
                Palette = "exp_pool", Core = 1f, Extra = new Vector4(ExpFxKind.Pool, 1.7f, 0f, 0f),
            },
            new CelLayer
            {
                // r03: no fireball at a side site; after the dome, an orange spark plume thrown up out of the ground
                Name = "plume", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.15f, 0.0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0.0f, 0.0f),
                Start = 0.05f, Duration = 0.45f, Life = 0.8f, LifeJitter = 0.3f, Close = 30, Mid = 10, Far = 0, Aim = CelAim.Up, Spread = 70f, Speed = 9.0f, SpeedJitter = 0.5f,
                Gravity = 6.75f, Drag = 0.8f, Size = new CelCurve(0.16f, 0.08f), SizeJitter = 0.3f, Stretch = 0.11f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 3f, Opacity = 2.0f, Extra = new Vector4(0f, 0.3f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.375f, 0.0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.225f, 0.0f, 0.0f),
                Duration = 0.2f, Life = 0.45f, Close = 30, Mid = 12, Far = 0, Aim = CelAim.Radial, Speed = 13.5f, SpeedJitter = 0.5f, UpBias = 0.9f,
                Gravity = 4.5f, Drag = 1.2f, Size = new CelCurve(0.070f, 0.040f), Stretch = 0.08f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 4f, Opacity = 2.0f, Extra = new Vector4(0f, 0.3f, 0.4f, 0f),
            },
            new CelLayer
            {
                Name = "debris", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.3f, 0.0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0.0f, 0.0f),
                Start = 0.04f, Duration = 0.3f, Life = 1.3f, LifeJitter = 0.2f, Close = 16, Mid = 6, Far = 0, Aim = CelAim.Radial, Speed = 5.25f, SpeedJitter = 0.6f, UpBias = 0.8f,
                Gravity = 9.8f, Drag = 0.5f, Spin = 4f, Size = new CelCurve(0.05f, 0.03f), SizeJitter = 0.4f, Stretch = 0.7f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_chunk", Core = 1f, Ink = 2.5f, Extra = new Vector4(4.5f, 0.75f, 0f, 0f),
            },
            new CelLayer
            {
                // r05: big chunks thrown high, so the cluster reads at ~40 m against the fog (ref f87 left and right)
                Name = "big chunks", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.45f, -0.8f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.9f, 0.0f, 0.0f),
                Start = 0.06f, Duration = 0.5f, Life = 1.4f, LifeJitter = 0.25f, Close = 13, Mid = 5, Far = 0, Aim = CelAim.Radial, Speed = 2.2f, SpeedJitter = 0.5f, UpBias = 0.2f,
                Gravity = 11f, Drag = 0.6f, Spin = 3f, Size = new CelCurve(0.044f, 0.028f), SizeJitter = 0.4f, Stretch = 0.6f, Erode = new CelCurve(0f, 0.4f, 4f),
                Palette = "exp_chunk", Core = 1f, Ink = 2.2f, Extra = new Vector4(4.5f, 0.75f, 0f, 0f),
            },
            new CelLayer
            {
                // d11: dense black angular chunks over the far side blast
                Name = "chunk burst", Kind = Chunk, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.35f, 0f), Shape = CelShape.Disc, ShapeSize = new Vector3(0.3f, 0.0f, 0.0f),
                Start = 0.05f, Duration = 0.45f, Life = 1.3f, LifeJitter = 0.3f, Close = 30, Mid = 8, Far = 0, Aim = CelAim.Radial, Speed = 5.0f, SpeedJitter = 0.65f, UpBias = 0.6f,
                Gravity = 9.8f, Drag = 0.6f, Spin = 7f, Size = new CelCurve(0.06f, 0.04f), SizeJitter = 0.55f, Stretch = 1.7f,
                Palette = "exp_dirt", Core = 0.3f, Opacity = 0.35f, Ink = 1.2f, Extra = new Vector4(4f, 3f, 0.5f, 0.50f),
            },
            new CelLayer
            {
                // r05: grey puffs standing round the site after its dome
                Name = "puffs", Kind = Puff, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.35f, 0.0f), Shape = CelShape.Box, ShapeSize = new Vector3(3.375f, 1.05f, 0.75f),
                Start = 0.43f, Duration = 0.28f, Life = 1.4f, LifeJitter = 0.2f, Close = 13, Mid = 3, Far = 0, Aim = CelAim.Radial, Speed = 0.6f, Drag = 1.2f, Buoyancy = 0.112f, Spin = 0.4f,
                Size = new CelCurve(0.45f, 1.45f, 0.35f), SizeJitter = 0.3f, Erode = new CelCurve(0.04f, 1f, 5f), Feather = 0.1f,
                Palette = "exp_wrap", Core = 1f, Ink = 1.5f, Extra = new Vector4(1.6f, 0.24f, 1.6f, -0.65f),
            },
            new CelLayer
            {
                // A single soft eruption cools behind the soot. The original early dome stays unchanged.
                Name = "far cooling eruption", Kind = ExpFarSoftKind.KindName, Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, 0.745f, 0f), Start = 0.12f, Life = 0.58f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Aim = CelAim.Up, Speed = 0f, SpeedJitter = 0f, Spread = 0f,
                Size = new CelCurve(2.10f, 1.25f, 0.7f), SizeJitter = 0f, Erode = new CelCurve(0f, 0f),
                Palette = "exp_dome", Core = 1.10f, Opacity = 1.2f,
                Extra = new Vector4(ExpFarSoftKind.Cooling, 0.75f, 0.745f, 0.55f),
            },
            new CelLayer
            {
                // Reference88-92: one folded dark center over the pale bank, not more scattered debris.
                Name = "far coherent soot center", Kind = ExpFarSootKind.KindName, Anchor = CelAnchor.Target,
                Offset = new Vector3(-0.20f, 0.85f, 0f), Start = 0.30f, Life = 1.35f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Aim = CelAim.Up, Speed = 0f, SpeedJitter = 0f, Spread = 0f,
                Size = new CelCurve(0.22f, 0.51f), SizeJitter = 0f, Erode = new CelCurve(0f, 0f),
                Feather = 0.07f, Palette = "exp_smoke_peak", Core = 1f, Ink = 0f,
                Extra = new Vector4(1.3f, 0.10f, 0.64f, -0.55f),
            });

        // Reference decomposition: the banks do not share a fade schedule. Main and rear-left remain
        // solid at f128; foreground-left is gone at f126 and rear-right at f130. Short births keep each bank's
        // fade coherent instead of leaving islands from the final third-second of random births.
        public static readonly CelFxRecipe Cloud = new CelFxRecipe("explosion_cloud", 3.0f,
            new CelLayer
            {
                Name = "rear-left smoke", Kind = Puff, Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, 0.55f, 0f), Shape = CelShape.Box, ShapeSize = new Vector3(3.5f, 3.0f, 0.6f),
                Duration = 0.10f, Life = 2.35f, LifeJitter = 0.015f, Close = 40, Mid = 12, Far = 0,
                Aim = CelAim.Radial, Speed = 0.32f, Drag = 1.4f, Buoyancy = 0.04f, Spin = 0.4f,
                Size = new CelCurve(0.37f, 0.66f, 0.35f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 0.48f, 7f), Feather = 0.12f,
                Palette = "exp_smoke_rear", Core = 1f, Ink = 1.9f, Extra = new Vector4(1.3f, 0.16f, 0.8f, 0f),
            },
            new CelLayer
            {
                // r03: a few small debris specks in the cloud, not black holes
                Name = "dirt", Kind = Puff, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 0.65f, -0.85f), Shape = CelShape.Box, ShapeSize = new Vector3(3.5f, 2.5f, 0.4f),
                Duration = 0.3f, Life = 2.25f, LifeJitter = 0.05f, Close = 0, Mid = 0, Far = 0, Aim = CelAim.Radial, Speed = 0.3f, Drag = 1.5f, Spin = 0.5f,
                Size = new CelCurve(0.14f, 0.32f, 0.4f), SizeJitter = 0.4f, Erode = new CelCurve(0f, 0.5f, 7f), Feather = 0.1f,
                Palette = "exp_soot", Core = 1f, Ink = 0f, Extra = new Vector4(1.3f, 0.16f, 0f, 0f),
            },
            new CelLayer
            {
                // orange embers drifting up through and past the cloud, to the clip's f152 and after
                Name = "embers", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, 1.6f, -2.0f), Shape = CelShape.Box, ShapeSize = new Vector3(10.0f, 5.5f, 5.0f),
                Start = 0.05f, Duration = 0.7f, Life = 0.65f, LifeJitter = 0.08f, Close = 14, Mid = 4, Far = 0, Aim = CelAim.Up, Spread = 18f, Speed = 0.25f, SpeedJitter = 0.5f, Gravity = 1.1f,
                Size = new CelCurve(0.055f, 0.03f), SizeJitter = 0.3f, Stretch = 0.09f, Erode = new CelCurve(0.05f, 0.5f, 2f),
                Palette = "exp_ember", Core = 3f, Extra = new Vector4(ExpFxKind.Ember, 0f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "sparse ember tail", Kind = Fx, Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, 1.6f, -2f), Shape = CelShape.Box, ShapeSize = new Vector3(10f, 5.5f, 5f),
                Start = 0.75f, Duration = 1.05f, Life = 1.0f, LifeJitter = 0.08f, Close = 0, Mid = 0, Far = 0,
                Aim = CelAim.Up, Spread = 18f, Speed = 0.25f, SpeedJitter = 0.5f, Gravity = 1.1f,
                Size = new CelCurve(0.055f, 0.03f), SizeJitter = 0.3f,
                Palette = "exp_ember", Core = 3f, Extra = new Vector4(ExpFxKind.Ember, 0f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "lingering embers", Kind = Fx, Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, 1.6f, -2f), Shape = CelShape.Box, ShapeSize = new Vector3(10f, 5.5f, 5f),
                Start = 1.15f, Duration = 0.35f, Life = 0.8f, LifeJitter = 0.08f, Close = 0, Mid = 0, Far = 0,
                Aim = CelAim.Up, Spread = 18f, Speed = 0.25f, SpeedJitter = 0.5f, Gravity = 1.1f,
                Size = new CelCurve(0.055f, 0.03f), SizeJitter = 0.3f,
                Palette = "exp_ember", Core = 3f, Extra = new Vector4(ExpFxKind.Ember, 0f, 0f, 0f),
            });

        // Reference tail: a small set of falling hot fragments from the main impact only.
        // Standard GPU ballistic motion; no texture/video sampling and no per-frame CPU work.
        readonly struct LateEmber
        {
            public readonly CelFxRecipe Recipe;
            public readonly Vector3 Offset, Velocity;
            public LateEmber(string name, float start, float life, Vector3 offset, Vector3 velocity,
                float gravity, float drag, float radius, float core, float landingAge)
            {
                Offset = offset; Velocity = velocity;
                float fadeStart = name == "near_left_late" ? 0.72f : (name == "center_glint" ? 0.50f : 0.90f);
                Recipe = new CelFxRecipe("explosion_tail_" + name, start + life, new CelLayer
                {
                    Name = name, Kind = Fx, Anchor = CelAnchor.Target, Start = start,
                    Life = life, LifeJitter = 0f, Close = 1, Mid = name == "last_falling" ? 1 : 0, Far = 0,
                    Aim = CelAim.Dir, Spread = 0f, Speed = velocity.magnitude, SpeedJitter = 0f,
                    Gravity = gravity, Drag = drag, Size = new CelCurve(radius, radius * 0.97f), SizeJitter = 0f,
                    Palette = "exp_ember_tail", Core = core, Opacity = 1f,
                    Extra = new Vector4(ExpFxKind.Ember, fadeStart, 0f, landingAge),
                });
            }
        }
        static readonly LateEmber[] LateTail =
        {
            new LateEmber("last_falling", 4.450000f, 1.625000f, new Vector3(-1.348603f, 1.179482f, -2.294502f), new Vector3(0.190214f, -1.447405f, 0.000000f), 0.438809f, 0.000000f, 0.080000f, 3.000f, 1.375000f),
            new LateEmber("left_falling", 4.283333f, 0.791667f, new Vector3(-3.444690f, 1.041842f, -4.745975f), new Vector3(-1.834756f, -4.487923f, 0.000000f), 2.550193f, 2.063527f, 0.078000f, 3.000f, 0.666667f),
            new LateEmber("left_glint", 5.033333f, 0.416667f, new Vector3(-4.107519f, -1.036168f, -4.982361f), new Vector3(0.485116f, -0.449681f, 0.000000f), 0.000000f, 0.000000f, 0.043000f, 2.000f, 0.000000f),
            new LateEmber("middle_falling", 4.450000f, 0.875000f, new Vector3(-2.592087f, 1.326506f, -2.793504f), new Vector3(-0.688902f, -3.122442f, 0.000000f), 3.311390f, 1.157786f, 0.077000f, 3.000f, 0.000000f),
            new LateEmber("near_left_early", 4.450000f, 0.541667f, new Vector3(-4.238800f, 0.480006f, -0.072737f), new Vector3(0.283304f, -5.237926f, 0.000000f), 0.000000f, 1.417019f, 0.074000f, 3.000f, 0.000000f),
            new LateEmber("near_left_late", 5.075000f, 0.541667f, new Vector3(-3.237525f, 0.312066f, -3.642777f), new Vector3(-1.247278f, -7.226362f, 0.000000f), 0.000000f, 4.157407f, 0.072000f, 3.000f, 0.000000f),
            new LateEmber("right_falling_early", 4.616667f, 0.500000f, new Vector3(-1.606762f, 1.332626f, -2.935692f), new Vector3(-2.184163f, -5.419877f, 0.000000f), 8.742956f, 7.317188f, 0.072000f, 3.000f, 0.000000f),
            new LateEmber("right_falling_late", 5.033333f, 0.416667f, new Vector3(-1.925733f, -0.145917f, -2.935692f), new Vector3(1.567381f, -4.113470f, 0.000000f), 0.000000f, 1.805356f, 0.072000f, 3.000f, 0.000000f),
            new LateEmber("right_glint", 5.325000f, 0.333333f, new Vector3(-1.604341f, -1.366184f, -2.642095f), new Vector3(1.529999f, 2.190093f, 0.000000f), 0.000000f, 9.339749f, 0.032000f, 2.000f, 0.000000f),
            new LateEmber("center_falling", 4.616667f, 0.416667f, new Vector3(-1.914605f, 0.020970f, -2.590952f), new Vector3(0.290712f, -5.121084f, 0.000000f), 0.000000f, 2.332768f, 0.071000f, 3.000f, 0.000000f),
            new LateEmber("center_glint", 4.950000f, 0.833333f, new Vector3(-1.943320f, -0.657884f, -3.088946f), new Vector3(3.405362f, -2.870487f, 0.000000f), 0.000000f, 5.012623f, 0.062000f, 2.500f, 0.000000f),
            new LateEmber("left_of_last", 4.450000f, 0.583333f, new Vector3(-1.412059f, 0.761179f, -2.792720f), new Vector3(0.009399f, -5.412843f, 0.000000f), 0.000000f, 1.402371f, 0.070000f, 3.000f, 0.000000f),
            new LateEmber("right_of_last", 4.450000f, 0.583333f, new Vector3(-1.342970f, 1.048402f, -2.768120f), new Vector3(0.281704f, -3.581353f, 0.000000f), 3.269958f, 0.000000f, 0.072000f, 3.000f, 0.000000f),
        };

        /// <summary>One deterministic main-impact tail, positioned in the effect's level frame.</summary>
        public static void PlayLateTail(Vector3 anchor, Vector3 right, Vector3 forward, uint seed, float delay)
        {
            for (int i = 0; i < LateTail.Length; i++)
            {
                var e = LateTail[i];
                Vector3 p = anchor + right * e.Offset.x + Vector3.up * e.Offset.y + forward * e.Offset.z;
                Vector3 velocity = right * e.Velocity.x + Vector3.up * e.Velocity.y + forward * e.Velocity.z;
                PlayAt(e.Recipe, p, velocity, p, seed + (uint)i * 53u, delay);
            }
        }

        // Main bank: raise its cap about 35 px while keeping the grounded base; diffuse haze remains at f140.
        // One back billow closes the measured Main128 interior gap at screen(929,323).
        // A separate play preserves every existing Main layer seed and renders beneath its fragments.
        public static readonly CelFxRecipe MainCloudBridge = new CelFxRecipe("explosion_main_cloud_bridge", 2.45f,
            new CelLayer
            {
                Name = "main interior back billow", Kind = Puff, Anchor = CelAnchor.Target, PackTargetFrame = true,
                Offset = new Vector3(0.9852f, 1.2257f, 0.20f), Life = 2.45f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Aim = CelAim.Up, Speed = 0f, SpeedJitter = 0f, Spread = 0f,
                Spin = 0.4f, Size = new CelCurve(0.30f, 0.38f, 0.35f), SizeJitter = 0f,
                Erode = new CelCurve(0f, 0.48f, 7f), Feather = 0.12f,
                Palette = "exp_smoke", Core = 1f, Ink = 1.9f, Extra = new Vector4(1.3f, 0.16f, 0.81f, 0f),
            });

        public static readonly CelFxRecipe MainCloud = new CelFxRecipe("explosion_main_cloud", 3.0f,
            new CelLayer
            {
                Name = "central smoke", Kind = Puff, Anchor = CelAnchor.Target, PackTargetFrame = true,
                Offset = new Vector3(0f, 0.30f, 0f), Shape = CelShape.Box, ShapeSize = new Vector3(4.5f, 3.50f, 0.5f),
                Duration = 0.10f, Life = 2.45f, LifeJitter = 0.015f, Close = 52, Mid = 12, Far = 0,
                Aim = CelAim.Radial, Speed = 0.32f, Drag = 1.4f, Buoyancy = 0.04f, Spin = 0.4f,
                Size = new CelCurve(0.37f, 0.67f, 0.35f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 0.48f, 7f), Feather = 0.12f,
                Palette = "exp_smoke", Core = 1f, Ink = 1.9f, Extra = new Vector4(1.3f, 0.16f, 0.81f, 0f),
            }, Cloud.Layers[1], Cloud.Layers[2], Cloud.Layers[3], Cloud.Layers[4],
            new CelLayer
            {
                // Dark angular fragments overlap the pale bank through f98-f118, clearing before its haze.
                Name = "smoke debris", Kind = Puff, Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, 0.60f, -0.50f), Shape = CelShape.Box, ShapeSize = new Vector3(3.7f, 2.2f, 0.35f),
                Duration = 0.22f, Life = 1.45f, LifeJitter = 0.10f, Close = 22, Mid = 6, Far = 0,
                Aim = CelAim.Radial, Speed = 0.35f, SpeedJitter = 0.3f, Gravity = 0.4f, Drag = 1.2f, Spin = 0.7f,
                Size = new CelCurve(0.15f, 0.23f, 0.5f), SizeJitter = 0.35f, Stretch = 1.2f,
                Palette = "exp_smoke_debris", Core = 1f, Ink = 1.5f, Extra = new Vector4(1.3f, 0.16f, 0.61f, 0f),
            });

        // Rear-right bank: full at f112, fades over f114-f130 instead of persisting with the main bank.
        public static readonly CelFxRecipe RightCloud = new CelFxRecipe("explosion_right_cloud", 3.0f,
            new CelLayer
            {
                Name = "right smoke", Kind = Puff, Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, 0.50f, 0f), Shape = CelShape.Box, ShapeSize = new Vector3(3.5f, 2.85f, 0.6f),
                Duration = 0.06f, Life = 1.74f, LifeJitter = 0.015f, Close = 36, Mid = 10, Far = 0,
                Aim = CelAim.Radial, Speed = 0.32f, Drag = 1.4f, Buoyancy = 0.04f, Spin = 0.4f,
                Size = new CelCurve(0.37f, 0.66f, 0.35f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 0.48f, 7f), Feather = 0.12f,
                Palette = "exp_smoke_rear", Core = 1f, Ink = 1.9f, Extra = new Vector4(1.3f, 0.16f, 0.8f, 0f),
            }, Cloud.Layers[1], Cloud.Layers[2], Cloud.Layers[3], Cloud.Layers[4]);

        // Foreground-left: a broad low bank, clearing over f116-f126.
        public static readonly CelFxRecipe NearCloud = new CelFxRecipe("explosion_near_cloud", 3.0f,
            new CelLayer
            {
                Name = "foreground-left smoke", Kind = Puff, Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, 0.25f, 0f), Shape = CelShape.Box, ShapeSize = new Vector3(3.5f, 1.6f, 0.7f),
                Duration = 0.06f, Life = 1.58f, LifeJitter = 0.015f, Close = 12, Mid = 10, Far = 0,
                Aim = CelAim.Radial, Speed = 0.32f, Drag = 1.4f, Buoyancy = 0.04f, Spin = 0.4f,
                Size = new CelCurve(0.34f, 0.57f, 0.35f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 0.48f, 7f), Feather = 0.12f,
                Palette = "exp_smoke_rear", Core = 1f, Ink = 1.9f, Extra = new Vector4(1.3f, 0.16f, 0.8f, 0f),
            }, Cloud.Layers[1], Cloud.Layers[2], Cloud.Layers[3], Cloud.Layers[4]);

        // Earlier left-edge dust hands over to the foreground-left bank without becoming a tall lasting strip.
        public static readonly CelFxRecipe EarlyCloud = new CelFxRecipe("explosion_early_cloud", 3.0f,
            new CelLayer
            {
                Name = "early-left smoke", Kind = Puff, Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, 0.10f, 0f), Shape = CelShape.Box, ShapeSize = new Vector3(3.5f, 1.7f, 0.6f),
                Duration = 0.10f, Life = 2.25f, LifeJitter = 0.015f, Close = 24, Mid = 10, Far = 0,
                Aim = CelAim.Radial, Speed = 0.32f, Drag = 1.4f, Buoyancy = 0.04f, Spin = 0.4f,
                Size = new CelCurve(0.42f, 0.74f, 0.35f), SizeJitter = 0.25f, Erode = new CelCurve(0f, 0.48f, 7f), Feather = 0.12f,
                Palette = "exp_smoke_rear", Core = 1f, Ink = 1.9f, Extra = new Vector4(1.3f, 0.16f, 0.8f, -0.65f),
            }, Cloud.Layers[1], Cloud.Layers[2], Cloud.Layers[3], Cloud.Layers[4]);

        // Dark rear eruption is a substantial connected plume beside the pale left bank, not scattered brown beads.
        public static readonly CelFxRecipe RearPeakCloud = new CelFxRecipe("explosion_rear_peak_cloud", 3.0f,
            new CelLayer
            {
                Name = "rear eruption smoke", Kind = Puff, Anchor = CelAnchor.Target,
                Offset = new Vector3(0f, 0.65f, 0f), Shape = CelShape.Box, ShapeSize = new Vector3(3.4f, 2.6f, 0.6f),
                Duration = 0.10f, Life = 2.35f, LifeJitter = 0.015f, Close = 32, Mid = 8, Far = 0,
                Aim = CelAim.Radial, Speed = 0.32f, Drag = 1.4f, Buoyancy = 0.04f, Spin = 0.4f,
                Size = new CelCurve(0.36f, 0.62f, 0.35f), SizeJitter = 0.25f, Erode = new CelCurve(0.08f, 0.68f, 2.2f), Feather = 0.08f,
                Palette = "exp_smoke_peak", Core = 1f, Ink = 1.9f, Extra = new Vector4(1.3f, 0.16f, -1f, 0f),
            }, Cloud.Layers[1], Cloud.Layers[2], Cloud.Layers[3], Cloud.Layers[4]);

        public static CelFxRecipe CloudForSite(int site)
        {
            switch (site)
            {
                case 0: return MainCloud;
                case 1: return NearCloud;
                case 2: return RearPeakCloud;
                case 3: return RightCloud;
                case 4: return EarlyCloud;
                default: return Cloud;
            }
        }

        public static readonly CelFxRecipe Fan = new CelFxRecipe("explosion_fan", 1.6f,
            new CelLayer
            {
                // thick orange streaks thrown low to the lower left, kept coming through the clips' peak window
                Name = "ground fan", Kind = Ribbon, Anchor = CelAnchor.Target, Offset = new Vector3(0.0f, -0.675f, 0.0f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.525f, 0.0f, 0.0f),
                Duration = 0.48f, Life = 0.38f, LifeJitter = 0.2f, Close = 0, Mid = 0, Far = 0, Aim = CelAim.Dir, Spread = 24f, Speed = 42.0f, SpeedJitter = 0.4f,
                Gravity = 6.0f, Drag = 1f, Size = new CelCurve(1.55f, 0.12f), SizeJitter = 0.35f, Stretch = 0.24f, Erode = new CelCurve(0.05f, 0.45f),
                Palette = "exp_tracer", Core = 1.6f, Extra = new Vector4(0f, 5.5f, 0.1f, 0.42f),
            },
            new CelLayer
            {
                // d07: a dense layer of SHORT orange sparks ON TOP of the long rods (d01 lost by replacing them): born
                // f78-f100, each on its own low arc (wide spread, jittered speed, gravity), fading over ~0.5 s
                Name = "fan sparks", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.431624f, -0.282982f, 4.272311f), Shape = CelShape.Box, ShapeSize = new Vector3(4.0f, 0.3f, 5.0f),
                Start = 0.24f, Duration = 0.38f, Life = 0.42f, LifeJitter = 0.4f, Close = 82, Mid = 0, Far = 0, Aim = CelAim.Dir, Spread = 50f, Speed = 6.0f, SpeedJitter = 0.75f, UpBias = 0.35f,
                Gravity = 6.0f, Drag = 0.8f, Size = new CelCurve(0.075f, 0.03f), SizeJitter = 0.5f, Stretch = 0.006f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 4f, Extra = new Vector4(8f, 0.1f, 0.3f, 0f),
            },
            new CelLayer
            {
                Name = "fan dots", Kind = Fx, Anchor = CelAnchor.Target, Offset = new Vector3(0.443129f, -0.227196f, 0.690232f), Shape = CelShape.Sphere, ShapeSize = new Vector3(0.9f, 0.0f, 0.0f),
                Start = 0.04f, Duration = 0.42f, Life = 0.6f, LifeJitter = 0.3f, Close = 52, Mid = 20, Far = 0, Aim = CelAim.Dir, Spread = 55f, Speed = 16.5f, SpeedJitter = 0.7f,
                Gravity = 9.0f, Drag = 1.2f, Size = new CelCurve(0.09f, 0.053f), Stretch = 0.015f, Erode = new CelCurve(0.05f, 0.5f),
                Palette = "exp_tracer", Core = 3f, Extra = new Vector4(8f, 0f, 0.3f, 0f),
            });

        // Three distinct pressure sheets reproduce the upper, lateral and steep descending branches.
        public static readonly CelFxRecipe FanUpper = MakeFanSheet("explosion_fan_upper", 17.7f, 1.02f, 0.67f, 11f);
        public static readonly CelFxRecipe FanMiddle = MakeFanSheet("explosion_fan_middle", 13.5f, 1.02f, 0.64f, 8f);
        public static readonly CelFxRecipe FanLower = MakeFanSheet("explosion_fan_lower", 10.3f, 0.94f, 0.80f, 7f);
        static CelFxRecipe MakeFanSheet(string name, float speed, float life, float width, float length)
        {
            return new CelFxRecipe(name, life, new CelLayer
            {
                Name = "flame sheet", Kind = name == "explosion_fan_lower" ? Ribbon : ExpFanKind.KindName, Anchor = CelAnchor.Target,
                Life = life, LifeJitter = 0f, Close = 1, Mid = 1, Far = 0,
                Aim = CelAim.Dir, Spread = 0f, Speed = speed, SpeedJitter = 0f, Gravity = 0f, Drag = 0.5f,
                Size = new CelCurve(width, width * 0.8f, 1f), SizeJitter = 0f, Stretch = 0.65f,
                Erode = new CelCurve(0f, 0.35f, 8f), Palette = name == "explosion_fan_middle" ? "exp_tracer" : (name == "explosion_fan_lower" ? "exp_fan_white" : "exp_fan_cream"), Core = 1f, Opacity = name == "explosion_fan_middle" ? 1.6f : (name == "explosion_fan_lower" ? 1.8f : 2f),
                Extra = new Vector4(name == "explosion_fan_upper" ? -1f : (name == "explosion_fan_middle" ? -2f : -3f), length, 0f, 0.12f),
            });
        }

        /// <summary>Schedule a recipe without moving the active clock, including delayed lights. No allocation.</summary>
        public static void PlayAt(CelFxRecipe r, Vector3 origin, Vector3 dir, Vector3 target, uint seed, float delay)
        {
            CelFx.Play(r, origin, dir, target, seed, delay);
        }

        /// <summary>One shell in the game: a ground blast at `ground` and its smoke cloud. `dir` = the eye's flat forward.</summary>
        public static void PlayShell(Vector3 ground, Vector3 dir, uint seed)
        {
            CelFx.Play(Site, ground, dir, ground, seed);
            PlayAt(Cloud, ground, dir, ground + Vector3.up * 1.1f, seed + 17u, 0.3f);
        }
    }
}
