// Phase: A5d (2026-09-29, the owner's night look: decisions.md) — depends on: Knobs, BiomeProfile.
// The distance lifted into lighter haze, as the owner's colour edit has it. Today the fog, the bank round the field and the
// clear colour all close toward a dark saturated blue (NightMud Haze 0.075, 0.105, 0.17), so the far field is darker than
// the near: the top third of a standard night still reads 0.10 against 0.12 for the bottom third, where the colour edit
// reads 0.25 against 0.14. look.lift (0 today, 1 full) takes the three toward lighter, greyer blues, so the haze lifts the
// distance instead of burying it and the ground's silhouettes stand against it. The blue stays: the edit keeps it.
// Lifted fog over the whole view turns the frame milky (Play, 2026-09-29: the bottom third went 0.12 to 0.16), and the
// edit's foreground stays dark and crisp. The fog begins where it always has, just short of the ground the camera looks
// at, so what lies nearer stays clear; but today it closes some 230 m further on and barely reaches the top of the frame
// (Play: with only the fog lifted the top third moved 0.103 to 0.114). So while it lifts it closes sooner: at full lift,
// look.liftReach times the distance to the ground looked at past its start, and that reach grows with the distance
// itself (ReachFor): from further off the frame holds far more field beyond the focus, and at zoom 60 a reach of 1 turned
// the middle of the field milky (middle third 0.150 to 0.187) where 2 kept it (0.173); at zoom 30, 1 was best. The low mist, which lies over the near
// ground too, lifts by only look.liftMist of it; so does the bank's colour (look.liftBank), which the quiet fog
// wears over every stretch of the field without the player's men, near or far (TWAtmosphere.hlsl FieldFogAmount).
// look.wet (0 today, 1 full): the puddles mirror less of the pale sky (big pale patches were where the frame spent its
// highlights: the report's fingerprint), and the flames' pools throw hard glints of their own colour on the wet ground
// (TWLightPools.hlsl, with look.pools on), so the mud sparkles orange by a fire and moon-blue away from it.
// The fog's distances follow the view (how far the camera looks), set in LateUpdate from the camera's pose then. The
// capture rig poses the main camera after that, last in the frame, so its stills were fogged for the gameplay view
// (78 m) whatever they framed (Play, 2026-09-29). While lifted, they are set again as the main camera begins to
// render, from the pose it renders from; at look.lift 0 nothing is hooked and every picture is the old one.
// The reach was 1 until the loop's first critique (2026-09-30): the far third still read no lighter than the near
// (lift +0.00 to +0.04 against the edit's +0.106). The shaders' field fog closes far more gently than the linear fog these
// distances suggest, so a reach of 0.2 lifts the far third 0.14-0.17 to 0.21-0.23 at zoom 30 and 60 with the near third
// within 0.01; 0.05 to 0.18 all gave the same, 0.3 fell short. The pools go on after the fog (look.poolsThroughHaze), or the
// haze put the distance's lamps out. Round 2's critic: that was fog opacity, not depth; the fill of distant things went
// and their ink stayed, bare black outlines. So the haze is thinner (reach 0.5) and 1.45 times lighter, and the ink fades
// with the distance fog (look.inkFade, InkLines_URP): far third 0.21-0.23 -> 0.25-0.27 (the edit 0.25), edges up at every
// pose (a_z30 0.081 -> 0.097), the distance's silhouettes darker than the haze behind them. look.hazeLight scales it.
// look.grade (round 3's critic: "blue down to the shadows"): the owner's colour edit keeps blue for the sky, the haze and
// the wet sheen and grounds the mud in dark umber-grey; NightMud's grade pushes its shadows blue (0.92, 0.98, 1.12) at
// saturation +4. At look.grade 1 the shadows turn umber (GradeUmber) and darken by look.gradeDark, and the saturation
// drops by look.gradeSat; the fog, lamps and highlights keep their colours. Re-applied to the volume live when a knob moves.
// Swept at zoom 30 (2026-09-30): at 1 the saturation fell 0.54 -> 0.28, past the edit's 0.49, the blue shadow tint alone
// doing most of it; 0.35 with no saturation cut gives 0.45-0.48, the near third 0.156 -> 0.145, the far third unchanged.
// look.puddleSky (critique rounds 1-3, each time second: "pale flat slabs brighter than the mud, ice or plastic"): the
// puddles mirror NightMud's SkyMirror (0.44, 0.54, 0.74), far lighter than the night ground; at night that mirror is
// scaled by look.puddleSky, so still water reads as dark glass that shows the lamps' glints and the lightning, not paper.
// The big pale slabs were the flooded ground's water sheet (Water_URP), bright mostly from its own moonlit body, which a
// third of mirrored sky barely changes: look.waterDim darkens that body at night (0 by day).
// Round 7's critic: the big slab still pale at 0.45 / 0.5; at 0.7 / 0.4 it reads as dark water (b_z30 crop p90 0.212 -> 0.200,
// near third within 0.01); 0.9 / 0.1 darker still.
// Round 10's critic still read the slab as pale lilac. Measured beside the mud (pose b, medians): water 0.133 against mud
// 0.161 at 0.7 / 0.4; 0.85 / 0.25 takes it to about 0.04 under the mud. The low mist over the sheet was not the cause
// (scaling its share to zero moved nothing).
// look.rainCurtain (round 7's critic, the loudest defect at the wide view: vertical stripes over the sky): the distant rain
// curtains are the haze colour times 1.9, near white once the haze was lifted and lightened; while lifted they are 1.15
// times it at look.rainCurtain of their alpha.
// Dark fields only, never lava: its magenta fog is its own look.
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;
using TW.Presentation;

namespace TW.Presentation.Terrain
{
    public sealed partial class Atmosphere
    {
        public const string LiftKnob = "look.lift";
        /// <summary>What the haze (the fog and the clear colour), the bank round the field and the low mist become at
        /// look.lift 1: lighter than the night ground and less saturated than today's, still blue.</summary>
        public static readonly Color LiftHaze = new Color(0.36f, 0.42f, 0.54f), LiftBank = new Color(0.30f, 0.36f, 0.48f), LiftMist = new Color(0.39f, 0.46f, 0.59f);
        public const float LiftReach = 0.5f, LiftMistShare = 0.3f, LiftBankShare = 0.35f;
        /// <summary>The owner's word (2026-09-29, "its good"): the night look on by default; each knob at 0 is the old night.</summary>
        public const float DefaultLift = 1f, DefaultWet = 1f;
        /// <summary>The view distance at which the reach is LiftReach itself (zoom 30 looks about 28 m), and the most it grows.</summary>
        public const float ReachAt = 30f, ReachGrowMax = 3f;
        static readonly int WetLookId = Shader.PropertyToID("_TWWetLook"), InkFogFadeId = Shader.PropertyToID("_TWInkFogFade");
        /// <summary>look.hazeLight: the lifted haze's colours times this (1 = LiftHaze as tuned); look.inkFade: how far the
        /// ink lines fade with the distance fog (InkLines_URP).</summary>
        public const float DefaultHazeLight = 1f, DefaultInkFade = 1f;
        float hazeLight = DefaultHazeLight, inkFade = DefaultInkFade;
        float wetLook;
        float fogSquall, fogLifted; bool fogHooked;
        float lift, liftReach = LiftReach, liftMist = LiftMistShare, liftBank = LiftBankShare; int liftKnobs = -1;

        /// <summary>look.lift now, 0..1, and 0 on a lit field or on lava.</summary>
        float Lift()
        {
            if (liftKnobs != Knobs.Generation)
            {
                liftKnobs = Knobs.Generation;
                lift = Mathf.Clamp01(Knobs.Get(LiftKnob, DefaultLift));
                liftReach = Mathf.Clamp(Knobs.Get("look.liftReach", LiftReach), 0.25f, 20f);
                liftMist = Mathf.Clamp01(Knobs.Get("look.liftMist", LiftMistShare));
                liftBank = Mathf.Clamp01(Knobs.Get("look.liftBank", LiftBankShare));
                wetLook = Mathf.Clamp01(Knobs.Get("look.wet", DefaultWet));
                hazeLight = Mathf.Clamp(Knobs.Get("look.hazeLight", DefaultHazeLight), 0.5f, 2f);
                inkFade = Mathf.Clamp01(Knobs.Get("look.inkFade", DefaultInkFade));
            }
            bool night = Look == Mood.Night && Profile.HeatStrength <= 0f;
            if (!fogHooked && night && lift > 0f) { RenderPipelineManager.beginCameraRendering += FogAtRender; fogHooked = true; }
            Shader.SetGlobalFloat(WetLookId, night ? wetLook : 0f);
            Shader.SetGlobalFloat(InkFogFadeId, night && lift > 0f ? inkFade : 0f);
            return night ? lift : 0f;
        }

        /// <summary>Where the fog closes: today's end, drawn in toward start + reach x the distance to the ground looked at
        /// as the lift grows (lift 0: today's end exactly).</summary>
        public static float LiftedFogEnd(float start, float today, float toFocus, float reach, float k) =>
            k <= 0f ? today : Mathf.Lerp(today, start + reach * toFocus, Mathf.Clamp01(k));

        /// <summary>The reach for a view this far from the ground it looks at: the knob's own at ReachAt and nearer, growing
        /// in step with the distance beyond, to at most ReachGrowMax times it.</summary>
        public static float ReachFor(float reach, float toFocus) => reach * Mathf.Clamp(toFocus / ReachAt, 1f, ReachGrowMax);

        /// <summary>The fog's start and end and the mist's reach for a camera, from how far it looks.</summary>
        void FogDistances(Camera c)
        {
            float height = Mathf.Max(1f, c.transform.position.y);
            float toFocus = ViewGround.Along(height, c.transform.forward, 0.12f);   // distance to the ground along the view
            RenderSettings.fogStartDistance = toFocus * StartFactor;
            RenderSettings.fogEndDistance = LiftedFogEnd(toFocus * StartFactor, toFocus * StartFactor + Depth * (1f - .38f * fogSquall) + toFocus, toFocus, ReachFor(liftReach, toFocus), fogLifted);
            float water = RenderGround.Map != null && RenderGround.Map.WaterLevel > TW.Sim.Terrain.MapData.NoWater ? RenderGround.Map.WaterLevel : 0f;
            Shader.SetGlobalVector(MistId, new Vector4(water + MistTop, 1f / Mathf.Max(0.05f, MistDepth), toFocus * 0.8f, 1f / Mathf.Max(10f, toFocus * 0.55f)));
        }

        /// <summary>While lifted: the main camera's fog from the pose it renders from.</summary>
        void FogAtRender(ScriptableRenderContext context, Camera c)
        {
            if (fogLifted > 0f && c == Camera.main) FogDistances(c);
        }

        void UnhookFog()
        {
            if (fogHooked) { RenderPipelineManager.beginCameraRendering -= FogAtRender; fogHooked = false; }
        }

        /// <summary>A haze colour taken toward its lifted one by k (k 0: the colour unchanged).</summary>
        public static Color Lifted(Color c, Color to, float k) => k <= 0f ? c : Color.Lerp(c, to, Mathf.Clamp01(k));

        /// <summary>look.grade's shadows: umber-grey, warm against the blue haze (ShadowsMidtonesHighlights weights).</summary>
        public static readonly Vector4 GradeUmber = new Vector4(1.08f, 1.0f, 0.90f, 0f);
        public const float DefaultGrade = 0.35f, DefaultGradeSat = 0f, DefaultGradeDark = 0.04f;
        ColorAdjustments gradeAdjust; ShadowsMidtonesHighlights gradeTones; int gradeLookAt = -1; bool gradeNight;

        /// <summary>look.grade on the volume: the shadows toward umber and darker, the saturation down, only on a dark field.</summary>
        void GradeLook()
        {
            if (gradeTones == null || gradeAdjust == null) return;
            bool night = Look == Mood.Night && Profile.HeatStrength <= 0f;
            if (gradeLookAt == Knobs.Generation && gradeNight == night) return;
            gradeLookAt = Knobs.Generation; gradeNight = night;
            float g = night ? Mathf.Clamp01(Knobs.Get("look.grade", DefaultGrade)) : 0f;
            float sat = Knobs.Get("look.gradeSat", DefaultGradeSat), dark = Knobs.Get("look.gradeDark", DefaultGradeDark);
            Vector4 sh = Vector4.Lerp(gradeShadows, GradeUmber, g); sh.w = gradeShadows.w - dark * g;
            gradeTones.shadows.Override(sh);
            gradeAdjust.saturation.Override(saturation - sat * g);
        }

        public const float DefaultPuddleSky = 0.25f, DefaultWaterDim = 0.85f;
        static readonly int WaterDimId = Shader.PropertyToID("_TWWaterDim"), CurtainId = Shader.PropertyToID("_TWCurtain");
        /// <summary>look.rainCurtain: the distant rain curtains' alpha while lifted (their colour 1.15 times the haze, was 1.9).</summary>
        public const float DefaultCurtain = 0.75f, CurtainPale = 1.15f;
        float curtain = DefaultCurtain;
        float puddleSky = DefaultPuddleSky, waterDim = DefaultWaterDim; int puddleKnobs = -1;

        /// <summary>What the puddles mirror of the sky at night (look.puddleSky), 1 by day and on lava.</summary>
        float PuddleSky()
        {
            if (puddleKnobs != Knobs.Generation)
            {
                puddleKnobs = Knobs.Generation;
                puddleSky = Mathf.Clamp(Knobs.Get("look.puddleSky", DefaultPuddleSky), 0.1f, 1f);
                waterDim = Mathf.Clamp(Knobs.Get("look.waterDim", DefaultWaterDim), 0f, 0.9f);
                curtain = Mathf.Clamp01(Knobs.Get("look.rainCurtain", DefaultCurtain));
            }
            bool night = Look == Mood.Night && Profile.HeatStrength <= 0f;
            Shader.SetGlobalFloat(WaterDimId, night ? waterDim : 0f);
            Shader.SetGlobalVector(CurtainId, night && fogLifted > 0f ? new Vector4(CurtainPale, curtain, 0f, 0f) : Vector4.zero);
            return night ? puddleSky : 1f;
        }

        /// <summary>A lifted colour at look.hazeLight's lightness.</summary>
        Color Light(Color c) => new Color(Mathf.Min(1f, c.r * hazeLight), Mathf.Min(1f, c.g * hazeLight), Mathf.Min(1f, c.b * hazeLight), c.a);
    }
}
