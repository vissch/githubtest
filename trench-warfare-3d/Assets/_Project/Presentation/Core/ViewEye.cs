// Phase: maintenance (2026-10-16) — the main camera and its zoom, read once a frame, in one place.
// Camera.main is a tagged find and IZoomSource comes off it with a component lookup; the pair was written out
// fifteen times under Presentation/Camera and twice more outside, and CombatFx.Weapons.cs names what that cost:
// the same idiom ran "for each of about sixty events a second". ViewGround is the precedent for folding an idiom
// written out many times into one place. This file is the only place that may call TryGetComponent<IZoomSource>
// (ViewEyeTests greps the presentation sources for it). Every site it replaced read Camera.main itself, so inside
// one frame the value is the same: this is a cost change, not a look change.
using UnityEngine;

namespace TW.Presentation
{
    public static class ViewEye
    {
        static int frame = -1;
        static Camera cam;
        static float zoom;

        /// <summary>Read the camera and its zoom if this frame has not read them yet. The null arm also covers a
        /// camera destroyed when Play ended, which keeps no stale reference past the session.</summary>
        static void Refresh()
        {
            if (frame == Time.frameCount && cam != null) return;
            cam = Camera.main;
            zoom = cam != null && cam.TryGetComponent<IZoomSource>(out var source) ? source.CurrentZoom : 0f;
            frame = Time.frameCount;
        }

        /// <summary>The main camera this frame.</summary>
        public static Camera Main { get { Refresh(); return cam; } }

        /// <summary>How wide a strip of ground is in view (IZoomSource.CurrentZoom), 0 when there is no camera.</summary>
        public static float Zoom { get { Refresh(); return zoom; } }

        /// <summary>Forget this frame's read: when Play ends, and between tests that hold their own camera.</summary>
        public static void Invalidate() { frame = -1; cam = null; zoom = 0f; }

        static ViewEye() { SceneStatics.Register(nameof(ViewEye), Invalidate); }
    }
}
