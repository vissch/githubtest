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

        // L15: the jetpack men in the air (LeapStarted to its scalar's seconds later)
        struct Leap { public int Slot; public float Until, NextFlame, NextPuff; }
        readonly List<Leap> leaps = new List<Leap>();
        public const float LeapFlameEvery = 1f / 12f, LeapPuffEvery = 0.08f;
        public const float LeapBackUp = 1.1f;   // his pack above his feet (m): where the jet is rooted

        /// <summary>The leaping man's pack: LeapBackUp over where he is drawn standing (CombatFx's drawnAt: the render ground
        /// sampled under him, the bank he climbs, his hop). Never over the presenter's point: its y is the sim's zero, and
        /// a jet hung from that burned under any ground above zero.</summary>
        public static Vector3 LeapBack(Vector3 standing) => standing + Vector3.up * LeapBackUp;

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
    
        /// <summary>L15 take-off (LeapStarted: pos where he lands, dir where he left from, scalar seconds in the air): a ring of
        /// dust blown flat round his feet and two spurts of earth; then he is followed in the air (TickLeaps).</summary>
        void OnLeap(SimEvent e)
        {
            if (recipes < 0.5f || books == null || !books.Ready || e.A < 0) return;
            Vector3 from = new Vector3(e.Dir.x, 0f, e.Dir.z);
            from.y = RenderGround.Sample(Host.Local.Map, from.x, from.z);
            books.Add(FlipbookFx.Book.GroundRing, from + Vector3.up * 0.15f, 3f, 0.5f, FlipbookFx.Kind.Flat, grow: 1f, alpha: 0.4f);
            for (int k = 0; k < 2; k++)
                books.Add(FlipbookFx.Book.Spurt, from + new Vector3(k == 0 ? 0.3f : -0.3f, 0f, 0f), 1.2f, 0.5f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (k == 1 ? FlipbookFx.Kind.Mirror : 0));
            float now = Time.time;
            leaps.Add(new Leap { Slot = e.A, Until = now + Mathf.Max(0.2f, e.Scalar), NextFlame = now, NextPuff = now });
        }

        /// <summary>L15 flight: a jet of fire pointing down from his back, re-lit every 1/12 s the way the flamethrower's root
        /// is, and a smoke puff left behind every 0.08 s, so his path hangs over no man's land as an arc of smoke.</summary>
        void TickLeaps(float now)
        {
            if (leaps.Count == 0) return;
            var w = Host.Local.World;
            var cam = Camera.main;
            for (int i = leaps.Count - 1; i >= 0; i--)
            {
                var l = leaps[i];
                if (now > l.Until || books == null || !books.Ready || l.Slot >= w.HighWater || (w.Flags[l.Slot] & (uint)UnitFlags.Alive) == 0) { leaps.RemoveAt(i); continue; }   // killed in the air: the jet goes with him
                Vector3 back = LeapBack(drawnAt(l.Slot));   // where he is drawn, as every other effect hung on a man
                if (now >= l.NextFlame)
                {
                    l.NextFlame = now + LeapFlameEvery;
                    const float Flame = 1.2f;   // the Jet book is drawn rooted at its left edge: centred half its length below him
                    books.Add(FlipbookFx.Book.Jet, back + Vector3.down * (Flame * 0.5f), Flame, LeapFlameEvery * 1.5f, roll: cam != null ? FlipbookFx.ScreenRoll(cam, Vector3.down) : 0f,
                        glow: SceneMood.Night ? 3f : 2.5f, startFrame: 11f + Mathf.Repeat(now * 12f, 7f));   // the book's hold (f11-18), not its start or stop
                }
                if (now >= l.NextPuff)
                {
                    l.NextPuff = now + LeapPuffEvery;
                    books.Add(FlipbookFx.Book.Smoke, back + Vector3.down * 0.6f, 0.6f, 2.5f, (Mathf.FloorToInt(now * 37f) & 1) == 0 ? FlipbookFx.Kind.Mirror : FlipbookFx.Kind.None,
                        velocity: Vector3.up * 0.3f, grow: 1.8f, alpha: 0.6f);
                }
                leaps[i] = l;
            }
        }
    
        /// <summary>L13 (fx.recipes): a round stopped by a shield bearer's plate (ShieldBlocked: pos is the bearer, dir the
        /// round's way): a strike on the plate, 0.6 m in front of him toward the shooter and 1.1 m up, and the round going
        /// off it, a quick star thrown up and back the way it came.</summary>
        void OnShieldBlocked(SimEvent e)
        {
            if (recipes < 0.5f || books == null || !books.Ready) return;
            Vector3 dir = new Vector3(e.Dir.x, 0f, e.Dir.z);
            if (dir.sqrMagnitude < 1e-4f) return;
            dir.Normalize();
            Vector3 plate = (Vector3)e.Pos; plate.y = RenderGround.Sample(Host.Local.Map, plate.x, plate.z) + 1.1f;
            plate -= dir * 0.6f;
            float glow = (SceneMood.Night ? 3.2f : 1.8f) * SceneTints.Now.Glow;
            books.Add(FlipbookFx.Book.Star, plate, 0.9f, 0.08f, glow: glow);
            books.Add(FlipbookFx.Book.Star, plate, 0.5f, 0.15f, velocity: (-dir + Vector3.up * 0.8f) * 9f, glow: glow);
        }

        /// <summary>L18 (fx.recipes): a charging Breaker's round found the mark (CriticalHit, pos the target): a bigger strike
        /// and a flash on the one it hit.</summary>
        void OnCriticalHit(SimEvent e)
        {
            if (recipes < 0.5f || books == null || !books.Ready) return;
            Vector3 at = (Vector3)e.Pos; at.y = RenderGround.Sample(Host.Local.Map, at.x, at.z) + 1.2f;
            float glow = (SceneMood.Night ? 4f : 2f) * SceneTints.Now.Glow;
            books.Add(FlipbookFx.Book.Star, at, 2.1f, 0.12f, glow: glow);
            books.Add(FlipbookFx.Book.Flash, at, 2.6f, 0.12f, glow: glow, pop: 0.5f);
        }
    }
}
