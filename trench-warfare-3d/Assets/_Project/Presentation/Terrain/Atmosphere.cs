// Phase: B2 (implemented) — the overcast day of the visual target: grey-blue haze that swallows the far ground and
// the horizon, a sky of the same colour, a soft warm key light with pale shadows. The haze starts behind what the
// camera is looking at and keeps its depth as the view zooms, so the overview is not a wall of fog.
// Ground mist: a cold pale layer that lies in the hollows and the far trenches (TW/Toon and the soldier shader read
// _TWMist / _TWMistColor); it starts beyond the focus so the men you are commanding stay clear.
// Fog bank: the ground nobody fights over, beyond the battlefield's edges, stands in a wall of fog that closes the
// view on every side (TWAtmosphere.hlsl: a function of world position, so it adds no geometry and no overdraw).
// Grade: one global volume. Shadows and midtones lean to sepia, highlights to cold slate, a touch less saturation,
// a light vignette. It is a single LUT pass, so it costs the same on the minimum GPU whatever it does.
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;

namespace TW.Presentation.Terrain
{
    public sealed class Atmosphere : MonoBehaviour
    {
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
        [Range(0f, 1f)] public float BankDensity = 0.94f;
        [Tooltip("Metres outside the map edge where the bank begins (negative = inside), and how far it takes to close.")]
        public float BankStart = -5f, BankRange = 28f;
        [Tooltip("Height of the bank at the edge, and how much taller it stands for each metre further out.")]
        public float BankTop = 7f, BankRise = 0.45f;
        [Header("Grade")]
        public bool Grade = true;
        Camera cam;
        VolumeProfile profile;
        static readonly int MistId = Shader.PropertyToID("_TWMist"), MistColorId = Shader.PropertyToID("_TWMistColor");
        static readonly int FieldId = Shader.PropertyToID("_TWField"), FieldFogId = Shader.PropertyToID("_TWFieldFog"), FieldFogColorId = Shader.PropertyToID("_TWFieldFogColor");

        void Start()
        {
            RenderSettings.fog = true;
            RenderSettings.fogMode = FogMode.Linear;
            RenderSettings.fogColor = Haze;
            RenderSettings.ambientMode = AmbientMode.Flat;
            RenderSettings.ambientLight = new Color(0.52f, 0.52f, 0.56f);
            foreach (var l in FindObjectsByType<Light>(FindObjectsSortMode.None))
            {
                if (l.type != LightType.Directional) continue;
                l.color = Key; l.intensity = KeyIntensity; l.shadows = LightShadows.Soft; l.shadowStrength = ShadowStrength;
                l.transform.rotation = Quaternion.Euler(52f, 35f, 0f);   // cross-light reveals rounded bags and timber depth from the standard view
            }
            if (Grade) BuildGrade();
        }

        void BuildGrade()
        {
            profile = ScriptableObject.CreateInstance<VolumeProfile>();
            profile.hideFlags = HideFlags.HideAndDontSave;
            var adjust = profile.Add<ColorAdjustments>(true);
            adjust.postExposure.Override(0.32f); adjust.contrast.Override(12f); adjust.saturation.Override(-18f);
            var tones = profile.Add<ShadowsMidtonesHighlights>(true);
            tones.shadows.Override(new Vector4(1.03f, 0.99f, 0.95f, 0f));      // umber in the dark
            tones.midtones.Override(new Vector4(1.03f, 1.0f, 0.95f, 0f));     // sepia through the middle
            tones.highlights.Override(new Vector4(0.95f, 0.99f, 1.05f, 0f));  // cold slate on the brightest planes
            var vignette = profile.Add<Vignette>(true);
            vignette.intensity.Override(0.24f); vignette.smoothness.Override(0.5f); vignette.color.Override(new Color(0.10f, 0.08f, 0.06f));
            var go = new GameObject("Grade") { hideFlags = HideFlags.DontSave };
            go.transform.SetParent(transform, false);
            var volume = go.AddComponent<Volume>();
            volume.isGlobal = true; volume.priority = 10f; volume.sharedProfile = profile;
        }

        void OnDestroy()
        {
            Shader.SetGlobalVector(MistColorId, Vector4.zero);
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

            var map = RenderGround.Map;
            if (map != null) Shader.SetGlobalVector(FieldId, new Vector4(0f, 0f, map.SizeMeters.x, map.SizeMeters.y));
            Shader.SetGlobalVector(FieldFogId, new Vector4(BankStart, 1f / Mathf.Max(1f, BankRange), BankTop, BankRise));
            Shader.SetGlobalVector(FieldFogColorId, new Vector4(Bank.r, Bank.g, Bank.b, map != null ? BankDensity : 0f));
        }
    }
}
