// Phase: B1 (implemented)
// Near-top-down camera matching the 2D game's view at ~75 %: the battle axis (world Z) runs left → right on
// screen (you on the left, the enemy on the right), trenches run vertically, pitch 72° keeps a slight 3D feel.
// WASD/arrows or edge scroll to pan, wheel to zoom, Q/E to rotate a little around the base yaw. Bounds come from
// the map size on SimHost.
// Super zoom: below CloseZoom the camera keeps going in to ZoomMin, tilting down towards ClosePitch and opening the
// lens to CloseFov, so the last stretch of the wheel ends among the men instead of above them. Z jumps there and back.
// Right mouse drag turns and tilts the view freely, middle mouse drag pans, Home puts the view back. Panning speeds
// up the longer a direction is held (PanAccel times faster after PanAccelSeconds).
using UnityEngine;
using UnityEngine.InputSystem;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed class TacticalCamera : MonoBehaviour, IZoomSource
    {
        public float CurrentZoom => Zoom;
        public SimHost Host;
        public float PanSpeed = 60f;
        public float PanAccel = 3.5f, PanAccelSeconds = 2f;
        [Tooltip("Degrees per pixel of right-mouse drag.")]
        public float LookSpeed = 0.22f;
        public float PitchMin = 18f, PitchMax = 89f;
        public float EdgeScrollMargin = 12f;
        [Tooltip("Pan when the cursor touches the screen edge. Off in the editor: the Game view is a panel, and a cursor resting on its border drags the view into the map corner.")]
        public bool EdgeScroll = true;
        public float ZoomMin = 6f, ZoomMax = 600f;
        [Tooltip("Below this zoom the view tilts and widens towards ClosePitch / CloseFov (super zoom).")]
        public float CloseZoom = 15f;
        public float ClosePitch = 32f, CloseFov = 42f, CloseYawLimit = 100f;
        public float Zoom = 50f;
        [Tooltip("90 = straight down. 72 keeps a slight 3D feel over the 2D game's top-down view.")]
        public float Pitch = 72f;
        [Tooltip("Yaw that puts world +Z (the enemy) on screen-right, as in the 2D game.")]
        public float BaseYaw = -90f;
        public float YawLimit = 15f;
        [Tooltip("Narrow lens = flatter, closer to the 2D game's orthographic look. Zoom keeps its meaning (visible height of a 60 degree lens at that distance).")]
        public float Fov = 25f;
        public float RotateSpeed = 60f;
        public Vector2 Focus = new Vector2(90f, 104f);   // both of your trenches in view: men arrive at the left one
        float yaw, zoomBeforeSuper, pitchOffset, panHeld;
        bool freeLook;   // the right mouse button has turned the view past the Q/E limits
        Camera cam;

        void Start()
        {
            cam = GetComponent<Camera>();
            if (cam != null) { cam.fieldOfView = Fov; cam.farClipPlane = 4000f; }
            if (Application.isEditor) EdgeScroll = false;
        }

        /// <summary>Snap to a point on the map at a zoom distance; used by the test panel's presets.</summary>
        public void Frame(Vector2 focus, float zoom) { Focus = focus; Zoom = Mathf.Clamp(zoom, ZoomMin, ZoomMax); yaw = 0f; pitchOffset = 0f; freeLook = false; }

        void LateUpdate()
        {
            bool focused = Application.isFocused;   // an unfocused window still reports stale keys and wheel deltas
            var kb = focused ? Keyboard.current : null;
            var mouse = focused ? Mouse.current : null;
            Vector2 pan = Vector2.zero;
            if (kb != null)
            {
                if (kb.wKey.isPressed || kb.upArrowKey.isPressed) pan.y += 1f;
                if (kb.sKey.isPressed || kb.downArrowKey.isPressed) pan.y -= 1f;
                if (kb.dKey.isPressed || kb.rightArrowKey.isPressed) pan.x += 1f;
                if (kb.aKey.isPressed || kb.leftArrowKey.isPressed) pan.x -= 1f;
                if (kb.qKey.isPressed) yaw -= RotateSpeed * Time.deltaTime;
                if (kb.eKey.isPressed) yaw += RotateSpeed * Time.deltaTime;
                if (kb.homeKey.wasPressedThisFrame) { yaw = 0f; pitchOffset = 0f; freeLook = false; }
                if (kb.zKey.wasPressedThisFrame)
                {
                    if (Zoom > ZoomMin * 1.5f) { zoomBeforeSuper = Zoom; Zoom = ZoomMin; }
                    else Zoom = zoomBeforeSuper > CloseZoom ? zoomBeforeSuper : 30f;
                }
            }
            if (mouse != null)
            {
                Vector2 m = mouse.position.ReadValue();
                bool inside = EdgeScroll && Application.isFocused && m.x > 0f && m.y > 0f && m.x < Screen.width - 1f && m.y < Screen.height - 1f;   // a cursor clamped to the border is outside   // no edge scroll while unfocused or with the cursor outside the view
                if (inside)
                {
                    if (m.x < EdgeScrollMargin) pan.x -= 1f; else if (m.x > Screen.width - EdgeScrollMargin) pan.x += 1f;
                    if (m.y < EdgeScrollMargin) pan.y -= 1f; else if (m.y > Screen.height - EdgeScrollMargin) pan.y += 1f;
                }
                Vector2 drag = mouse.delta.ReadValue();
                if (mouse.rightButton.isPressed && drag.sqrMagnitude > 0f)
                {
                    yaw += drag.x * LookSpeed; pitchOffset -= drag.y * LookSpeed;
                    freeLook = true;
                }
                if (mouse.middleButton.isPressed) { pan.x -= drag.x * 0.12f; pan.y -= drag.y * 0.12f; }
                float wheel = mouse.scroll.ReadValue().y;
                if (Mathf.Abs(wheel) > 0.01f) Zoom = Mathf.Clamp(Zoom - wheel * 0.08f * Zoom, ZoomMin, ZoomMax);
            }
            float close = 1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(ZoomMin, CloseZoom, Zoom));   // 0 tactical, 1 among the men
            float yawLimit = Mathf.Lerp(YawLimit, CloseYawLimit, close);   // up close Q/E can turn to face the enemy
            if (freeLook) yaw = Mathf.Repeat(yaw + 180f, 360f) - 180f; else yaw = Mathf.Clamp(yaw, -yawLimit, yawLimit);
            bool keysPan = kb != null && (kb.wKey.isPressed || kb.aKey.isPressed || kb.sKey.isPressed || kb.dKey.isPressed || kb.upArrowKey.isPressed
                || kb.downArrowKey.isPressed || kb.leftArrowKey.isPressed || kb.rightArrowKey.isPressed);
            bool steadyPan = keysPan || (pan.sqrMagnitude > 0f && (mouse == null || !mouse.middleButton.isPressed));   // keys or edge scroll, not a drag
            panHeld = steadyPan ? panHeld + Time.unscaledDeltaTime : 0f;
            float accel = steadyPan ? Mathf.Lerp(1f, PanAccel, Mathf.SmoothStep(0f, 1f, panHeld / Mathf.Max(0.01f, PanAccelSeconds))) : 1f;
            var rot = Quaternion.Euler(0f, BaseYaw + yaw, 0f);
            Vector3 fwd = rot * Vector3.forward, right = rot * Vector3.right;
            Vector3 delta = (fwd * pan.y + right * pan.x) * PanSpeed * accel * Time.unscaledDeltaTime * (Zoom / 90f);
            Focus += new Vector2(delta.x, delta.z);
            if (Host != null && Host.Local != null)
            {
                var size = Host.Local.Map.SizeMeters;
                Focus.x = Mathf.Clamp(Focus.x, 0f, size.x);
                Focus.y = Mathf.Clamp(Focus.y, 0f, size.y);
            }
            float fov = Mathf.Lerp(Fov, CloseFov, close);
            if (cam != null) { cam.fieldOfView = fov; cam.nearClipPlane = 0.2f; }
            float pitch = Mathf.Clamp(Mathf.Lerp(Pitch, ClosePitch, close) + pitchOffset, PitchMin, PitchMax);
            pitchOffset = pitch - Mathf.Lerp(Pitch, ClosePitch, close);   // do not wind up past the limits
            var camRot = Quaternion.Euler(pitch, BaseYaw + yaw, 0f);
            transform.rotation = camRot;
            float distance = Zoom * Mathf.Tan(30f * Mathf.Deg2Rad) / Mathf.Tan(fov * 0.5f * Mathf.Deg2Rad);
            float ground = Host != null && Host.Local != null ? Host.Local.Map.Height.Sample(Focus.x, Focus.y) : 0f;
            transform.position = new Vector3(Focus.x, ground * close + 1.1f * close, Focus.y) - camRot * Vector3.forward * distance;
        }
    }
}
