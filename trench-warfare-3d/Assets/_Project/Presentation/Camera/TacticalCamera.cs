// Phase: B1 (implemented)
// High-angle isometric camera: WASD/arrows or edge scroll to pan, wheel to zoom, Q/E to rotate ±45° around the
// tug-of-war axis. Bounds come from the map size on SimHost.
using UnityEngine;
using UnityEngine.InputSystem;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed class TacticalCamera : MonoBehaviour
    {
        public SimHost Host;
        public float PanSpeed = 60f;
        public float EdgeScrollMargin = 12f;
        public float ZoomMin = 20f, ZoomMax = 600f;
        public float Zoom = 50f;
        public float Pitch = 52f;
        public float YawLimit = 45f;
        public float RotateSpeed = 60f;
        public Vector2 Focus = new Vector2(150f, 110f);
        float yaw;

        /// <summary>Snap to a point on the map at a zoom distance; used by the test panel's presets.</summary>
        public void Frame(Vector2 focus, float zoom) { Focus = focus; Zoom = Mathf.Clamp(zoom, ZoomMin, ZoomMax); yaw = 0f; }

        void LateUpdate()
        {
            var kb = Keyboard.current;
            var mouse = Mouse.current;
            Vector2 pan = Vector2.zero;
            if (kb != null)
            {
                if (kb.wKey.isPressed || kb.upArrowKey.isPressed) pan.y += 1f;
                if (kb.sKey.isPressed || kb.downArrowKey.isPressed) pan.y -= 1f;
                if (kb.dKey.isPressed || kb.rightArrowKey.isPressed) pan.x += 1f;
                if (kb.aKey.isPressed || kb.leftArrowKey.isPressed) pan.x -= 1f;
                if (kb.qKey.isPressed) yaw -= RotateSpeed * Time.deltaTime;
                if (kb.eKey.isPressed) yaw += RotateSpeed * Time.deltaTime;
            }
            if (mouse != null)
            {
                Vector2 m = mouse.position.ReadValue();
                bool inside = Application.isFocused && m.x >= 0f && m.y >= 0f && m.x <= Screen.width && m.y <= Screen.height;   // no edge scroll while unfocused or with the cursor outside the view
                if (inside)
                {
                    if (m.x < EdgeScrollMargin) pan.x -= 1f; else if (m.x > Screen.width - EdgeScrollMargin) pan.x += 1f;
                    if (m.y < EdgeScrollMargin) pan.y -= 1f; else if (m.y > Screen.height - EdgeScrollMargin) pan.y += 1f;
                }
                float wheel = mouse.scroll.ReadValue().y;
                if (Mathf.Abs(wheel) > 0.01f) Zoom = Mathf.Clamp(Zoom - wheel * 0.08f * Zoom, ZoomMin, ZoomMax);
            }
            yaw = Mathf.Clamp(yaw, -YawLimit, YawLimit);
            var rot = Quaternion.Euler(0f, yaw, 0f);
            Vector3 fwd = rot * Vector3.forward, right = rot * Vector3.right;
            Vector3 delta = (fwd * pan.y + right * pan.x) * PanSpeed * Time.deltaTime * (Zoom / 90f);
            Focus += new Vector2(delta.x, delta.z);
            if (Host != null && Host.Local != null)
            {
                var size = Host.Local.Map.SizeMeters;
                Focus.x = Mathf.Clamp(Focus.x, 0f, size.x);
                Focus.y = Mathf.Clamp(Focus.y, 0f, size.y);
            }
            // flatten the pitch when zoomed in so units read as figures, steepen it for the overview
            float pitch = Mathf.Lerp(Pitch - 12f, Pitch + 20f, Mathf.InverseLerp(ZoomMin, ZoomMax, Zoom));
            var camRot = Quaternion.Euler(pitch, yaw, 0f);
            transform.rotation = camRot;
            transform.position = new Vector3(Focus.x, 0f, Focus.y) - camRot * Vector3.forward * Zoom;
        }
    }
}
