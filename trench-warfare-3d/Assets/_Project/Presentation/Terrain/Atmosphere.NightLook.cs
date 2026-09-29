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
// haze put the distance's lamps out.
// Dark fields only, never lava: its magenta fog is its own look.
using UnityEngine;
using UnityEngine.Rendering;
using TW.Presentation;

namespace TW.Presentation.Terrain
{
    public sealed partial class Atmosphere
    {
        public const string LiftKnob = "look.lift";
        /// <summary>What the haze (the fog and the clear colour), the bank round the field and the low mist become at
        /// look.lift 1: lighter than the night ground and less saturated than today's, still blue.</summary>
        public static readonly Color LiftHaze = new Color(0.25f, 0.29f, 0.37f), LiftBank = new Color(0.21f, 0.25f, 0.33f), LiftMist = new Color(0.27f, 0.32f, 0.41f);
        public const float LiftReach = 0.2f, LiftMistShare = 0.3f, LiftBankShare = 0.35f;
        /// <summary>The owner's word (2026-09-29, "its good"): the night look on by default; each knob at 0 is the old night.</summary>
        public const float DefaultLift = 1f, DefaultWet = 1f;
        /// <summary>The view distance at which the reach is LiftReach itself (zoom 30 looks about 28 m), and the most it grows.</summary>
        public const float ReachAt = 30f, ReachGrowMax = 3f;
        static readonly int WetLookId = Shader.PropertyToID("_TWWetLook");
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
            }
            bool night = Look == Mood.Night && Profile.HeatStrength <= 0f;
            if (!fogHooked && night && lift > 0f) { RenderPipelineManager.beginCameraRendering += FogAtRender; fogHooked = true; }
            Shader.SetGlobalFloat(WetLookId, night ? wetLook : 0f);
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
    }
}
