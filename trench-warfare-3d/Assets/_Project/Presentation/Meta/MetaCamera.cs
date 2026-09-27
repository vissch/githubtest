// Phase: B6 / docs/21 phase 6 (implemented) — the camera behind the campaign menus. It borrows the menu scene's camera
// (which draws nothing on its own: culling mask 0) while a view is up and gives it back untouched when the view
// hides. Two modes: Orbit for the Home Front (a slow turn round the block, paused after a right-drag, the wheel
// zooming) and Map for the continent (a high tilted look, panned by the middle button or WASD, eased onto a node by
// Focus). Reads the Input System directly; the view says whether the pointer is over the UI.
using UnityEngine;
using UnityEngine.InputSystem;

namespace TW.Presentation.Meta
{
    public sealed class MetaCamera
    {
        public enum Mode { Orbit, Map }

        public const float Fov = 30f;
        public const float OrbitPitch = 32f, OrbitZoom = 26f, OrbitZoomMin = 14f, OrbitZoomMax = 44f;
        public const float AutoOrbitDegPerSecond = 2f, AutoOrbitPauseSeconds = 6f, DragDegPerPixel = 0.25f, PitchMin = 15f, PitchMax = 70f;
        public const float MapPitch = 55f, MapZoom = 40f, MapZoomMin = 20f, MapZoomMax = 110f, PanMetresPerPixel = 0.08f, PanMetresPerSecond = 30f;
        public const float Ease = 4f, WheelZoomPerNotch = 3f;

        Camera cam;
        bool held; Vector3 savedPos; Quaternion savedRot; float savedFov; int savedMask;

        public Mode CurrentMode { get; private set; }
        public Vector3 Target, GoalTarget;
        public float Yaw = 35f, Pitch = OrbitPitch, Zoom = OrbitZoom, GoalZoom = OrbitZoom;
        public bool Held => held;
        float pausedUntil;

        /// <summary>Borrow the camera for a mode, looking at <paramref name="target"/>. Saves what the scene had once.</summary>
        public void Take(Camera c, Mode mode, Vector3 target)
        {
            if (c == null) return;
            if (!held || cam != c)
            {
                if (held) Release();
                cam = c; held = true;
                savedPos = c.transform.position; savedRot = c.transform.rotation; savedFov = c.fieldOfView; savedMask = c.cullingMask;
            }
            CurrentMode = mode;
            Target = GoalTarget = target;
            Pitch = mode == Mode.Orbit ? OrbitPitch : MapPitch;
            Zoom = GoalZoom = mode == Mode.Orbit ? OrbitZoom : MapZoom;
            Yaw = mode == Mode.Orbit ? 35f : 0f;
            pausedUntil = 0f;
            cam.cullingMask = ~0;
            cam.fieldOfView = Fov;
            Apply();
        }

        /// <summary>Give the camera back as it was.</summary>
        public void Release()
        {
            if (!held) return;
            held = false;
            if (cam != null)
            {
                cam.transform.SetPositionAndRotation(savedPos, savedRot);
                cam.fieldOfView = savedFov; cam.cullingMask = savedMask;
            }
            cam = null;
        }

        public void Focus(Vector3 target, bool animate)
        {
            GoalTarget = target;
            if (!animate) Target = target;
        }

        /// <summary>One frame: input (drags and the wheel only while the pointer is free), the easing, the transform.</summary>
        public void Tick(float dt, bool pointerFree, float now)
        {
            if (!held || cam == null) return;
            var mouse = Mouse.current; var kb = Keyboard.current;
            if (mouse != null && pointerFree)
            {
                Vector2 drag = mouse.delta.ReadValue();
                float wheel = mouse.scroll.ReadValue().y;
                if (CurrentMode == Mode.Orbit)
                {
                    if (mouse.rightButton.isPressed && drag.sqrMagnitude > 0f)
                    {
                        Yaw += drag.x * DragDegPerPixel; Pitch = Mathf.Clamp(Pitch - drag.y * DragDegPerPixel, PitchMin, PitchMax);
                        pausedUntil = now + AutoOrbitPauseSeconds;
                    }
                }
                else if ((mouse.middleButton.isPressed || mouse.rightButton.isPressed) && drag.sqrMagnitude > 0f)
                {
                    var flat = FlatAxes();
                    GoalTarget -= (flat.right * drag.x + flat.forward * drag.y) * PanMetresPerPixel * (Zoom / MapZoom);
                    Target = GoalTarget;
                }
                if (Mathf.Abs(wheel) > 0.01f) GoalZoom = Mathf.Clamp(GoalZoom - Mathf.Sign(wheel) * WheelZoomPerNotch, ZoomMin, ZoomMax);
            }
            if (kb != null && CurrentMode == Mode.Map)
            {
                var flat = FlatAxes(); Vector3 pan = Vector3.zero;
                if (kb.wKey.isPressed || kb.upArrowKey.isPressed) pan += flat.forward;
                if (kb.sKey.isPressed || kb.downArrowKey.isPressed) pan -= flat.forward;
                if (kb.dKey.isPressed || kb.rightArrowKey.isPressed) pan += flat.right;
                if (kb.aKey.isPressed || kb.leftArrowKey.isPressed) pan -= flat.right;
                if (pan.sqrMagnitude > 0f) { GoalTarget += pan.normalized * PanMetresPerSecond * dt; Target = GoalTarget; }
            }
            if (CurrentMode == Mode.Orbit && now >= pausedUntil) Yaw += AutoOrbitDegPerSecond * dt;
            float k = 1f - Mathf.Exp(-Ease * dt);
            Target = Vector3.Lerp(Target, GoalTarget, k);
            Zoom = Mathf.Lerp(Zoom, GoalZoom, k);
            Apply();
        }

        float ZoomMin => CurrentMode == Mode.Orbit ? OrbitZoomMin : MapZoomMin;
        float ZoomMax => CurrentMode == Mode.Orbit ? OrbitZoomMax : MapZoomMax;

        (Vector3 forward, Vector3 right) FlatAxes()
        {
            var rot = Quaternion.Euler(0f, Yaw, 0f);
            return (rot * Vector3.forward, rot * Vector3.right);
        }

        void Apply()
        {
            var rot = Quaternion.Euler(Pitch, Yaw, 0f);
            cam.transform.SetPositionAndRotation(Target - rot * Vector3.forward * Zoom, rot);
        }

        /// <summary>A ray from the borrowed camera through a screen point (bottom-left origin), or false without one.</summary>
        public bool TryRay(Vector2 screen, out Ray ray)
        {
            if (!held || cam == null) { ray = default; return false; }
            ray = cam.ScreenPointToRay(new Vector3(screen.x, screen.y, 0f));
            return true;
        }
    }
}
