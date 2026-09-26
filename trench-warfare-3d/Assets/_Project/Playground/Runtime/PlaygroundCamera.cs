// Phase: Playground (2026-09-26, lane/show/playground) — the orbit camera and its presets
// An orbit camera for looking at one thing from every side and at every distance: right-drag turns, wheel zooms,
// middle-drag (or WASD) moves the focus. Presets put it where the game's cameras would be: the standard battle view
// (fov 25, pitch 25, TacticalCamera's), the super zoom, and far out where the last LODs take over.
using UnityEngine;
using UnityEngine.InputSystem;

namespace TW.Playground
{
    public sealed class PlaygroundCamera : MonoBehaviour
    {
        public Vector3 Focus = new Vector3(0f, 1.5f, 0f);
        public float Yaw = 205f, Pitch = 22f, Distance = 18f, Fov = 35f;
        public bool Input = true;
        /// <summary>Set by the host while the pointer is over its panel, so a drag on the panel does not turn the view.</summary>
        public bool PointerOverUi;
        Camera cam;

        void Awake()
        {
            cam = GetComponent<Camera>();
            if (cam == null) cam = gameObject.AddComponent<Camera>();
        }

        public void Preset(string name, Vector3 at)
        {
            Focus = at;
            switch (name)
            {
                case "close": Fov = 42f; Pitch = 14f; Distance = 9f; break;
                case "standard": Fov = 25f; Pitch = 25f; Distance = 75f; break;   // the battle's standard view
                case "far": Fov = 25f; Pitch = 25f; Distance = 220f; break;       // where LOD2/LOD3 draw
                case "top": Fov = 35f; Pitch = 80f; Distance = 30f; break;
                default: Fov = 35f; Pitch = 22f; Distance = 18f; break;
            }
        }

        void LateUpdate()
        {
            if (Input && !PointerOverUi)
            {
                var m = Mouse.current; var k = Keyboard.current;
                if (m != null)
                {
                    var d = m.delta.ReadValue();
                    if (m.rightButton.isPressed) { Yaw += d.x * 0.25f; Pitch = Mathf.Clamp(Pitch - d.y * 0.25f, -5f, 88f); }
                    if (m.middleButton.isPressed)
                    {
                        var r = transform.right; var f = Vector3.Cross(r, Vector3.up);
                        Focus -= (r * d.x + f * d.y) * Distance * 0.0016f;
                    }
                    float w = m.scroll.ReadValue().y;
                    if (Mathf.Abs(w) > 0.01f) Distance = Mathf.Clamp(Distance * Mathf.Pow(0.9f, Mathf.Sign(w)), 2f, 900f);
                }
                if (k != null)
                {
                    var r = transform.right; var f = Vector3.Cross(r, Vector3.up);
                    float s = Distance * 0.9f * Time.unscaledDeltaTime;
                    if (k.wKey.isPressed) Focus -= f * s;
                    if (k.sKey.isPressed) Focus += f * s;
                    if (k.aKey.isPressed) Focus -= r * s;
                    if (k.dKey.isPressed) Focus += r * s;
                }
            }
            var rot = Quaternion.Euler(Pitch, Yaw, 0f);
            transform.SetPositionAndRotation(Focus - rot * Vector3.forward * Distance, rot);
            cam.fieldOfView = Fov;
            cam.nearClipPlane = Mathf.Clamp(Distance * 0.01f, 0.05f, 2f);
            cam.farClipPlane = 3000f;
        }
    }
}
