// Phase: B6 (implemented) — the key light both diorama and map views borrow and re-tint: finds the scene's one
// directional light, saves its colour/intensity the first time, and restores it when released.
using UnityEngine;

namespace TW.Presentation.Meta
{
    public sealed class MetaKeyLight
    {
        Light key; Color savedKeyColor; float savedKeyIntensity; bool keyHeld;

        public bool Held => keyHeld;
        public Light Key => key;

        public void Take(Color color, float intensity)
        {
            if (!keyHeld)
            {
                key = null;
                foreach (var l in UnityEngine.Object.FindObjectsByType<Light>(FindObjectsSortMode.None)) if (l.type == LightType.Directional) { key = l; break; }
                if (key == null) return;
            }
            Take(key, color, intensity);
        }

        public void Take(Light light, Color color, float intensity)
        {
            if (!keyHeld)
            {
                key = light;
                if (key == null) return;
                savedKeyColor = key.color; savedKeyIntensity = key.intensity; keyHeld = true;
            }
            key.color = color; key.intensity = intensity;
        }

        public void Release()
        {
            if (keyHeld && key != null) { key.color = savedKeyColor; key.intensity = savedKeyIntensity; }
            keyHeld = false;
        }
    }
}
