// Phase: B2 (implemented) — the overcast day of the visual target: grey-blue haze that swallows the far ground and
// the horizon, a sky of the same colour, a soft warm key light with pale shadows. The haze starts behind what the
// camera is looking at and keeps its depth as the view zooms, so the overview is not a wall of fog.
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public sealed class Atmosphere : MonoBehaviour
    {
        public Color Haze = new Color(0.58f, 0.61f, 0.64f);
        public Color Key = new Color(1.0f, 0.95f, 0.86f);
        public float KeyIntensity = 1.0f;
        [Range(0f, 1f)] public float ShadowStrength = 0.55f;
        [Tooltip("Haze begins this far behind the point the camera looks at, as a fraction of the camera's distance to it.")]
        public float StartFactor = 0.85f;
        public float Depth = 420f;
        Camera cam;

        void Start()
        {
            RenderSettings.fog = true;
            RenderSettings.fogMode = FogMode.Linear;
            RenderSettings.fogColor = Haze;
            RenderSettings.ambientMode = UnityEngine.Rendering.AmbientMode.Flat;
            RenderSettings.ambientLight = new Color(0.52f, 0.52f, 0.56f);
            foreach (var l in FindObjectsByType<Light>(FindObjectsSortMode.None))
            {
                if (l.type != LightType.Directional) continue;
                l.color = Key; l.intensity = KeyIntensity; l.shadows = LightShadows.Soft; l.shadowStrength = ShadowStrength;
                l.transform.rotation = Quaternion.Euler(48f, 320f, 0f);   // the view looks along -X: light from over the viewer's left shoulder, shadows fall to the right
            }
        }

        void LateUpdate()
        {
            if (cam == null) cam = Camera.main;
            if (cam == null) return;
            cam.clearFlags = CameraClearFlags.SolidColor;
            cam.backgroundColor = Haze;
            float height = Mathf.Max(1f, cam.transform.position.y);
            float pitch = Mathf.Max(0.12f, -cam.transform.forward.y);
            float toFocus = height / pitch;   // distance to the ground along the view
            RenderSettings.fogStartDistance = toFocus * StartFactor;
            RenderSettings.fogEndDistance = toFocus * StartFactor + Depth + toFocus;
        }
    }
}
