// Phase: B6 (implemented) — the pick-collider naming and the per-frame pointer/hover/pick block both the diorama
// and the strategic map share: a ray against the pick colliders while the pointer is not over the UI.
using System;
using UnityEngine;
using UnityEngine.InputSystem;

namespace TW.Presentation.Meta
{
    public static class MetaPick
    {
        public const string Prefix = "pick:";

        public static string NameFor(string id) => Prefix + id;

        public static string IdOf(string colliderName) =>
            colliderName != null && colliderName.StartsWith(Prefix, StringComparison.Ordinal) ? colliderName.Substring(Prefix.Length) : null;

        public static string Tick(MetaCamera rig, float dt, float now, float reach, out bool pressed)
        {
            var mouse = Mouse.current;
            Vector2 at = mouse != null ? mouse.position.ReadValue() : default;
            bool free = mouse != null && !HudBridge.IsPointerOverUi(at);
            rig.Tick(dt, free, now);
            string hover = null;
            if (free && rig.TryRay(at, out var ray) && Physics.Raycast(ray, out var hit, reach))
                hover = IdOf(hit.collider != null ? hit.collider.name : "");
            pressed = hover != null && mouse.leftButton.wasPressedThisFrame;
            return hover;
        }
    }
}
