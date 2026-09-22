// Phase: B1 (implemented)
// The standard view (owner, 2026-09-21; everything on screen is built for it): a 25 degree lens 25 degrees above the
// horizon, 30 m of zoom, looking across the front and turned 21 degrees towards the enemy, so you stand behind your
// own men and see their backs. The view follows the battle along the front axis (world Z):
//   behind your own men   -> turned towards the enemy (+StandardYaw), Pitch
//   between the two sides -> square to the front (0), flatter (FlatPitch)
//   beyond the enemy      -> turned back towards your side (-StandardYaw), Pitch
// "Your men" and "the enemy" are the mean Z of each side's living units, so the turning point moves with the fight.
// Zooming far out lifts the view to OverviewPitch and takes the turn out; the last stretch of zooming in (below
// CloseZoom) drops to ClosePitch and opens the lens, ending among the men. Z jumps there and back.
// WASD/arrows or edge scroll pan and speed up while held, the wheel zooms, Q/E turn a little, right mouse drag turns
// and tilts freely on top of all this, middle mouse drag pans, Home puts the view back.
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
        public float PitchMin = 8f, PitchMax = 89f;
        public float EdgeScrollMargin = 12f;
        [Tooltip("Pan when the cursor touches the screen edge. Off in the editor: the Game view is a panel, and a cursor resting on its border drags the view into the map corner.")]
        public bool EdgeScroll = true;
        public float ZoomMin = 6f, ZoomMax = 600f;
        [Tooltip("Below this zoom the view tilts and widens towards ClosePitch / CloseFov (super zoom).")]
        public float CloseZoom = 15f;
        [Tooltip("The small things of the field (grit, footprints, brass, litter) are all there at DetailFullZoom and gone by DetailGoneZoom, so the standard view never pays for them.")]
        public float DetailFullZoom = 12f, DetailGoneZoom = 28f;
        public float ClosePitch = 13f, CloseFov = 42f, CloseYawLimit = 100f;
        public float Zoom = 30f;
        [Tooltip("Degrees above the horizon in the standard view (behind your own men, or beyond the enemy).")]
        public float Pitch = 25f;
        [Tooltip("Turn towards the enemy when the view is behind your own men; mirrored when it is beyond the enemy.")]
        public float StandardYaw = 21f;
        [Tooltip("Pitch when the view sits between the two sides.")]
        public float FlatPitch = 17f;
        [Tooltip("Zoomed far out the view lifts to this pitch and loses its turn.")]
        public float OverviewPitch = 62f;
        public float OverviewFromZoom = 60f, OverviewFullZoom = 240f;
        [Tooltip("How fast the view eases to a new angle, per second.")]
        public float FollowRate = 2.2f;
        [Tooltip("Yaw that puts world +Z (the enemy) on screen-right, as in the 2D game.")]
        public float BaseYaw = -90f;
        public float YawLimit = 15f;
        [Tooltip("Narrow lens = flatter, closer to the 2D game's orthographic look. Zoom keeps its meaning (visible height of a 60 degree lens at that distance).")]
        public float Fov = 25f;
        public float RotateSpeed = 60f;
        public Vector2 Focus = new Vector2(38f, 34f);   // the owner's standard view: on your rear trench, where men arrive
        float yaw, zoomBeforeSuper, pitchOffset, panHeld;
        float autoYaw, autoPitch, mineZ, theirsZ, nextArmies;
        bool autoPrimed;
        /// <summary>Where the view sits between the sides: 0 behind your men, 0.5 between, 1 beyond the enemy (for the HUD and tests).</summary>
        public float Along { get; private set; }
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
        /// <summary>Frame, turned by yawDeg about the focus (free look, past the Q/E limits): the captures that walk round a man.</summary>
        public void FrameFrom(Vector2 focus, float zoom, float yawDeg) { Frame(focus, zoom); yaw = yawDeg; freeLook = true; }

        /// <summary>The angle the battle asks for at this focus and zoom, eased in.</summary>
        void FollowBattle(float close)
        {
            if (Host != null && Host.Local != null && Time.unscaledTime >= nextArmies)
            {
                nextArmies = Time.unscaledTime + 0.25f;
                var w = Host.Local.World;
                float length = Host.Local.Map.SizeMeters.y, sumA = 0f, sumB = 0f; int a = 0, b = 0;
                for (int i = 0; i < w.HighWater; i++)
                {
                    if ((w.Flags[i] & (uint)TW.Sim.UnitFlags.Alive) == 0) continue;
                    if (w.Team[i] == 0) { sumA += w.Position[i].z; a++; } else { sumB += w.Position[i].z; b++; }
                }
                mineZ = a > 0 ? sumA / a : length * 0.2f;      // nobody on the field yet: the trench lines
                theirsZ = b > 0 ? sumB / b : length * 0.8f;
                if (theirsZ < mineZ + 40f) { float mid = (mineZ + theirsZ) * 0.5f; mineZ = mid - 20f; theirsZ = mid + 20f; }   // a melee: keep a span to turn across
            }
            Along = Mathf.InverseLerp(mineZ, theirsZ, Focus.y);
            float side = 1f - 2f * Mathf.SmoothStep(0f, 1f, Along);   // +1 behind your men, 0 between, -1 beyond the enemy
            float overview = Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(OverviewFromZoom, OverviewFullZoom, Zoom));
            float wantYaw = StandardYaw * side * (1f - overview);
            float wantPitch = Mathf.Lerp(Mathf.Lerp(Mathf.Lerp(Pitch, FlatPitch, 1f - Mathf.Abs(side)), OverviewPitch, overview), ClosePitch, close);
            float k = autoPrimed ? 1f - Mathf.Exp(-FollowRate * Time.unscaledDeltaTime) : 1f;
            autoYaw = Mathf.Lerp(autoYaw, wantYaw, k); autoPitch = Mathf.Lerp(autoPitch, wantPitch, k);
            autoPrimed = true;
        }

        static readonly int CloseId = Shader.PropertyToID("_TWClose");
        void OnDisable() { SceneHooks.CloseUp = 0f; Shader.SetGlobalFloat(CloseId, 0f); }

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
                if (kb.qKey.isPressed) yaw -= RotateSpeed * Time.unscaledDeltaTime;   // unscaled: the view still turns in a lightning freeze
                if (kb.eKey.isPressed) yaw += RotateSpeed * Time.unscaledDeltaTime;
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
            // the small things of the field (grit, footprints, brass, litter) come in over a wider band and cost nothing at the standard view
            SceneHooks.CloseUp = 1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(DetailFullZoom, DetailGoneZoom, Zoom));
            Shader.SetGlobalFloat(CloseId, SceneHooks.CloseUp);
            float yawLimit = Mathf.Lerp(YawLimit, CloseYawLimit, close);   // up close Q/E can turn to face the enemy
            if (freeLook) yaw = Mathf.Repeat(yaw + 180f, 360f) - 180f; else yaw = Mathf.Clamp(yaw, -yawLimit, yawLimit);
            bool keysPan = kb != null && (kb.wKey.isPressed || kb.aKey.isPressed || kb.sKey.isPressed || kb.dKey.isPressed || kb.upArrowKey.isPressed
                || kb.downArrowKey.isPressed || kb.leftArrowKey.isPressed || kb.rightArrowKey.isPressed);
            bool steadyPan = keysPan || (pan.sqrMagnitude > 0f && (mouse == null || !mouse.middleButton.isPressed));   // keys or edge scroll, not a drag
            panHeld = steadyPan ? panHeld + Time.unscaledDeltaTime : 0f;
            float accel = steadyPan ? Mathf.Lerp(1f, PanAccel, Mathf.SmoothStep(0f, 1f, panHeld / Mathf.Max(0.01f, PanAccelSeconds))) : 1f;
            var rot = Quaternion.Euler(0f, BaseYaw + autoYaw + yaw, 0f);
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
            FollowBattle(close);
            float pitch = Mathf.Clamp(autoPitch + pitchOffset, PitchMin, PitchMax);
            pitchOffset = pitch - autoPitch;   // do not wind up past the limits
            var camRot = Quaternion.Euler(pitch, BaseYaw + autoYaw + yaw, 0f);
            transform.rotation = camRot;
            float distance = Zoom * Mathf.Tan(30f * Mathf.Deg2Rad) / Mathf.Tan(fov * 0.5f * Mathf.Deg2Rad);
            float ground = Host != null && Host.Local != null ? Host.Local.Map.Height.Sample(Focus.x, Focus.y) : 0f;
            transform.position = new Vector3(Focus.x, ground * close + 1.1f * close, Focus.y) - camRot * Vector3.forward * distance;
        }
    }
}
