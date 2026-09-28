// Phase: VFX pass (owner, 2026-09-28; tw3d-board catalogue IN-11, look spec L16) — part of CombatFx (see CombatFx.cs for
// the event dispatch). The medic at work: on the sim's UnitHealed (SupportSystem, a sweep every few ticks), a soft glint
// at the patient's chest, at most once per HealEvery a medic, and only near enough to see a man (a glint over the
// overview would be noise). The engineer's sparks are TankRenderer's (VehicleHullMended). Behind fx.recipes; no random
// draws, so the recipes A/B keeps the shared stream.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        public const float HealEvery = 1.5f;     // s between one medic's glints
        public const float HealNearZoom = 120f;  // past this (the overview) no glint

        readonly Dictionary<int, float> healNext = new Dictionary<int, float>();

        /// <summary>Whether a medic's glint shows now, and if so when his next may: pure, so a test can hold it.</summary>
        public static bool HealGlint(float now, float next, float zoom, float recipes) => recipes >= 0.5f && zoom < HealNearZoom && now >= next;

        void OnHealed(SimEvent e)
        {
            if (books == null || !books.Ready) return;
            var w = Host.Local.World;
            if (e.B < 0 || e.B >= w.HighWater) return;
            var cam = Camera.main;
            float zoom = cam != null && cam.TryGetComponent<IZoomSource>(out var source) ? source.CurrentZoom : 0f;
            float now = Time.time;
            healNext.TryGetValue(e.A, out float next);
            if (!HealGlint(now, next, zoom, recipes)) return;
            healNext[e.A] = now + HealEvery;
            float scale = units != null ? units.UnitScale * Mathf.Clamp(zoom / Mathf.Max(1f, units.GrowFromZoom), 1f, units.MaxGrow) : 1f;
            Vector3 p;
            if (units == null || !units.Sockets(e.B, out _, out _, out p)) p = EstimateChest(e.B, scale);
            books.Add(FlipbookFx.Book.Star, p, 0.5f * scale, 0.35f, glow: SceneMood.Night ? 2.2f : 1.4f, pop: 0.3f);
        }
    }
}
