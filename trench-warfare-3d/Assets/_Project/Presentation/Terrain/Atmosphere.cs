// Phase: B2 (implemented) — the overcast day of the visual target: grey-blue haze that swallows the far ground and
// the horizon, a sky of the same colour, a soft warm key light with pale shadows. The haze starts behind what the
// camera is looking at and keeps its depth as the view zooms, so the overview is not a wall of fog.
// Ground mist: a cold pale layer that lies in the hollows and the far trenches (TW/Toon and the soldier shader read
// _TWMist / _TWMistColor); it starts beyond the focus so the men you are commanding stay clear.
// Fog bank: the ground nobody fights over, beyond the battlefield's edges, stands in a wall of fog that closes the
// view on every side (TWAtmosphere.hlsl: a function of world position, so it adds no geometry and no overdraw).
// Mood: Look picks the whole set. Night (owner's target, 2026-09-21) is a cold blue moon low behind the field, so every
// wet plane facing the camera glints; near-black blue shade; dark blue haze, mist and fog bank; soaked, shining mud;
// a grade that keeps the blues cold and lets the warm lights (NightLights) bloom. OvercastDay is the earlier look.
// Grade: one global volume. Shadows and midtones lean to sepia, highlights to cold slate, a touch less saturation,
// a light vignette. It is a single LUT pass, so it costs the same on the minimum GPU whatever it does.
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;

namespace TW.Presentation.Terrain
{
    public sealed class Atmosphere : MonoBehaviour
    {
        public enum Mood { OvercastDay, Night }
        [Tooltip("Applied in Start: Night overwrites the colour and light fields below with the night set.")]
        public Mood Look = Mood.Night;
        public static Mood Current { get; private set; }
        [Header("Mood")]
        public Color ShadeTint = Color.white;
        public Color SkyMirror = new Color(0.60f, 0.61f, 0.60f);
        [Range(0f, 1f)] public float Wetness = 0f, WetGlint = 0f;
        [Range(0f, 1f)] public float Rain = 0f;
        [Tooltip("How much the rain swells and slackens: 0 steady, 1 from almost nothing to a downpour.")]
        [Range(0f, 1f)] public float Squalls = 0.85f;
        /// <summary>The rain right now, 0..1, and the wind it falls in (m/s, world XZ). Rain.cs and the shaders follow these.</summary>
        public static float RainNow { get; private set; }
        public static Vector2 WindNow { get; private set; }
        public Vector3 KeyEuler = new Vector3(52f, 35f, 0f);   // cross-light reveals rounded bags and timber depth from the standard view
        public Color Ambient = new Color(0.52f, 0.52f, 0.56f);
        float exposure = 0.32f, contrast = 12f, saturation = -18f, vignetteAmount = 0.24f, bloom = 0f;
        Vector4 gradeShadows = new Vector4(1.03f, 0.99f, 0.95f, 0f), gradeMids = new Vector4(1.03f, 1.0f, 0.95f, 0f), gradeHighs = new Vector4(0.95f, 0.99f, 1.05f, 0f);
        public Color Haze = new Color(0.60f, 0.61f, 0.60f);
        public Color Key = new Color(1.0f, 0.95f, 0.86f);
        public float KeyIntensity = 1.0f;
        [Range(0f, 1f)] public float ShadowStrength = 0.40f;
        [Tooltip("Haze begins this far behind the point the camera looks at, as a fraction of the camera's distance to it.")]
        public float StartFactor = 0.85f;
        public float Depth = 300f;
        [Header("Ground mist")]
        public Color Mist = new Color(0.74f, 0.76f, 0.75f);
        [Range(0f, 1f)] public float MistDensity = 0.55f;
        [Tooltip("Metres above the water table where the mist ends, and how deep it takes to reach full density.")]
        public float MistTop = 2.3f, MistDepth = 2.2f;
        [Header("Fog bank round the battlefield")]
        public Color Bank = new Color(0.66f, 0.67f, 0.66f);
        [Range(0f, 1f)] public float BankDensity = 0.97f;
        [Tooltip("Metres outside the map edge where the bank begins (negative = inside), and how far it takes to close.")]
        public float BankStart = -5f, BankRange = 28f;
        [Tooltip("Height of the bank at the edge, and how much taller it stands for each metre further out.")]
        public float BankTop = 7f, BankRise = 0.45f;
        [Header("Grade")]
        public bool Grade = true;
        Camera cam;
        VolumeProfile profile;
        static readonly int MistId = Shader.PropertyToID("_TWMist"), MistColorId = Shader.PropertyToID("_TWMistColor");
        static readonly int ShadeTintId = Shader.PropertyToID("_TWShadeTint"), SkyId = Shader.PropertyToID("_TWSky"), WetId = Shader.PropertyToID("_TWWet");
        static readonly int FieldId = Shader.PropertyToID("_TWField"), FieldFogId = Shader.PropertyToID("_TWFieldFog"), FieldFogColorId = Shader.PropertyToID("_TWFieldFogColor");

        void ApplyNight()
        {
            Haze = new Color(0.075f, 0.105f, 0.17f);
            Key = new Color(0.56f, 0.70f, 1.0f); KeyIntensity = 1.0f; ShadowStrength = 0.66f;
            KeyEuler = new Vector3(30f, 122f, 0f);   // low, and coming toward the standard view: rims, long shadows, glints on the wet
            Ambient = new Color(0.10f, 0.13f, 0.20f);
            ShadeTint = new Color(0.20f, 0.29f, 0.56f);
            SkyMirror = new Color(0.44f, 0.54f, 0.74f);
            Wetness = 0.85f; WetGlint = 0.75f; Rain = 0.65f;
            Depth = 230f;
            Mist = new Color(0.17f, 0.23f, 0.35f); MistDensity = 0.55f;
            Bank = new Color(0.10f, 0.14f, 0.22f);
            exposure = 0.45f; contrast = 20f; saturation = 4f; vignetteAmount = 0.34f; bloom = 0.9f;
            gradeShadows = new Vector4(0.92f, 0.98f, 1.12f, 0f); gradeMids = new Vector4(0.98f, 1.0f, 1.04f, 0f); gradeHighs = new Vector4(1.08f, 1.0f, 0.90f, 0f);
        }

        void Start()
        {
            if (Look == Mood.Night) ApplyNight();
            Current = Look; SceneMood.Night = Look == Mood.Night;
            RenderSettings.fog = true;
            RenderSettings.fogMode = FogMode.Linear;
            RenderSettings.fogColor = Haze;
            RenderSettings.ambientMode = AmbientMode.Flat;
            RenderSettings.ambientLight = Ambient;
            foreach (var l in FindObjectsByType<Light>(FindObjectsSortMode.None))
            {
                if (l.type != LightType.Directional) continue;
                l.color = Key; l.intensity = KeyIntensity; l.shadows = LightShadows.Soft; l.shadowStrength = ShadowStrength;
                l.transform.rotation = Quaternion.Euler(KeyEuler);
            }
            if (Grade) BuildGrade();
        }

        void BuildGrade()
        {
            profile = ScriptableObject.CreateInstance<VolumeProfile>();
            profile.hideFlags = HideFlags.HideAndDontSave;
            var adjust = profile.Add<ColorAdjustments>(true);
            adjust.postExposure.Override(exposure); adjust.contrast.Override(contrast); adjust.saturation.Override(saturation);
            var tones = profile.Add<ShadowsMidtonesHighlights>(true);
            tones.shadows.Override(gradeShadows);      // day: umber in the dark; night: blue
            tones.midtones.Override(gradeMids);        // day: sepia through the middle
            tones.highlights.Override(gradeHighs);     // day: cold slate on the brightest planes; night: warm, for the lamps
            if (bloom > 0f)
            {
                var glow = profile.Add<Bloom>(true);   // muzzle flashes, lamps, tracers, the flare
                glow.threshold.Override(0.85f); glow.intensity.Override(bloom); glow.scatter.Override(0.62f); glow.tint.Override(new Color(1f, 0.92f, 0.82f));
            }
            var vignette = profile.Add<Vignette>(true);
            vignette.intensity.Override(vignetteAmount); vignette.smoothness.Override(0.5f); vignette.color.Override(new Color(0.10f, 0.08f, 0.06f));
            var go = new GameObject("Grade") { hideFlags = HideFlags.DontSave };
            go.transform.SetParent(transform, false);
            var volume = go.AddComponent<Volume>();
            volume.isGlobal = true; volume.priority = 10f; volume.sharedProfile = profile;
        }

        void OnDestroy()
        {
            Shader.SetGlobalVector(MistColorId, Vector4.zero);
            Shader.SetGlobalVector(ShadeTintId, Vector4.zero); Shader.SetGlobalVector(SkyId, Vector4.zero); Shader.SetGlobalVector(WetId, Vector4.zero);
            Shader.SetGlobalVector(FieldFogColorId, Vector4.zero);
            if (profile != null) Destroy(profile);
        }

        void LateUpdate()
        {
            if (cam == null)
            {
                cam = Camera.main;
                if (cam != null && Grade) cam.GetUniversalAdditionalCameraData().renderPostProcessing = true;
            }
            if (cam == null) return;
            cam.clearFlags = CameraClearFlags.SolidColor;
            cam.backgroundColor = Haze;
            float height = Mathf.Max(1f, cam.transform.position.y);
            float pitch = Mathf.Max(0.12f, -cam.transform.forward.y);
            float toFocus = height / pitch;   // distance to the ground along the view
            RenderSettings.fogStartDistance = toFocus * StartFactor;
            RenderSettings.fogEndDistance = toFocus * StartFactor + Depth + toFocus;

            float water = RenderGround.Map != null && RenderGround.Map.WaterLevel > TW.Sim.Terrain.MapData.NoWater ? RenderGround.Map.WaterLevel : 0f;
            Shader.SetGlobalVector(MistId, new Vector4(water + MistTop, 1f / Mathf.Max(0.05f, MistDepth), toFocus * 0.8f, 1f / Mathf.Max(10f, toFocus * 0.55f)));
            Shader.SetGlobalVector(MistColorId, new Vector4(Mist.r, Mist.g, Mist.b, MistDensity));

            Shader.SetGlobalVector(ShadeTintId, new Vector4(ShadeTint.r, ShadeTint.g, ShadeTint.b, 1f));
            Shader.SetGlobalVector(SkyId, new Vector4(SkyMirror.r, SkyMirror.g, SkyMirror.b, 1f));
            // Weather: two slow noises make the rain swell to a downpour and slacken to a drizzle over a minute or so, with
            // shorter gusts on top; the wind swings and freshens with it. Everything that shows rain reads the same number.
            float clock = Time.time;
            float swell = Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(.28f, .72f, Mathf.PerlinNoise(clock * .021f, 3.7f)));
            float gust = Mathf.PerlinNoise(clock * .13f, 11.3f);
            float level = Mathf.Clamp01(swell * .8f + gust * .35f - .05f);
            RainNow = Rain * Mathf.Lerp(1f, Mathf.Lerp(.12f, 1.25f, level), Squalls);
            float heading = (Mathf.PerlinNoise(clock * .017f, 23.1f) - .5f) * 2.4f + .6f;
            float blow = Mathf.Lerp(1.5f, 9f, Mathf.Clamp01(level * .7f + gust * .5f));
            WindNow = new Vector2(Mathf.Cos(heading), Mathf.Sin(heading)) * blow;
            Shader.SetGlobalVector(WetId, new Vector4(Wetness, WetGlint, Mathf.Clamp01(RainNow), 0f));

            var map = RenderGround.Map;
            if (map != null) Shader.SetGlobalVector(FieldId, new Vector4(0f, 0f, map.SizeMeters.x, map.SizeMeters.y));
            Shader.SetGlobalVector(FieldFogId, new Vector4(BankStart, 1f / Mathf.Max(1f, BankRange), BankTop, BankRise));
            Shader.SetGlobalVector(FieldFogColorId, new Vector4(Bank.r, Bank.g, Bank.b, map != null ? BankDensity : 0f));
        }
    }
}
