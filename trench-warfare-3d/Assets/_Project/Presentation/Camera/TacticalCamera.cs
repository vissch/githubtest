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
        public float ZoomMin = 25f, ZoomMax = 220f;
        public float Zoom = 90f;
        public float Pitch = 58f;
        public float YawLimit = 45f;
        public float RotateSpeed = 60f;
        public Vector2 Focus = new Vector2(150f, 100f);
        float yaw;

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
                if (m.x < EdgeScrollMargin) pan.x -= 1f; else if (m.x > Screen.width - EdgeScrollMargin) pan.x += 1f;
                if (m.y < EdgeScrollMargin) pan.y -= 1f; else if (m.y > Screen.height - EdgeScrollMargin) pan.y += 1f;
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
            var camRot = Quaternion.Euler(Pitch, yaw, 0f);
            transform.rotation = camRot;
            transform.position = new Vector3(Focus.x, 0f, Focus.y) - camRot * Vector3.forward * Zoom;
        }
    }
}
