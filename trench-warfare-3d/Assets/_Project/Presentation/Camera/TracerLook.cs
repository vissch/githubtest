// Phase: C104 (AOSA, juice J03/J01) - where a tracer is drawn among the smoke, and its shape.
// A blind critic (runs 9/c101 frames) read the night tracers as a sticker layer: the side's halo is additive at queue
// 3100, after the smoke books (3010, FlipbookFx), so a green ribbon lay at full strength over the densest cloud and
// flattened the depth; and at 0.24 m wide and 11.5 m long the halo read as a laser, not a round. C53 asked for tracers
// drawn inside the smoke. One knob, read once where CombatFx makes the tracer materials:
//   fx.tracerInSmoke  1 = the halos are drawn at queue 3005, before the smoke, and write depth: a cloud in front of a
//                     tracer composites over it (dense smoke hides it, thin smoke dims it) and a cloud behind it is
//                     rejected by the halo's depth, so a round in clear air is untouched. The white-hot core is opaque
//                     (queue 2000) and was already under the smoke. And the night streak is shorter (7 m, not 10), its
//                     halo narrower (0.15 m, not 0.24) and only on the streak's front half, a hot coloured head on a
//                     white tail instead of a neon ribbon the length of the shot. The core keeps its width (J03's 2-3 px)
//                     and colour (white by design, runs 9/c73.md); the halo keeps its side's colour (whose fire).
//                     The day tracer is opaque and already inside the smoke: it is not changed.
//                     0 = the old look bit for bit (the order and the shape), and the default.
//   fx.tracerShape    (AOSA C104s) the shape half on its own, 0-1: 0 = the old night streak and halo, 1 = C104's, and
//                     between them a lerp of every shape number (streak length, halo width, halo length, where the halo
//                     sits). Unset, it is 0 (DefaultShape), except when fx.tracerInSmoke is itself set and on: then it is
//                     1, so the run label `fx.tracerInSmoke=1` alone still means C104's order and slim shape bit for bit
//                     (runs 9/s3, b3), and fx.tracerInSmoke=1,fx.tracerShape=0 is the new order with the old wide halos
//                     (runs 9/t1). Neither set is the old look.
// Cycle 9: t1 (runs 9/t1, c104s-critic.md: tracers +2.17, weight -0.33, men never lower) passed alone, but the cycle 9
// candidate set (smokeSoft 0.6, burstGlow 0.5, tracerInSmoke 1 with tracerShape 0) failed together on weight (a0050,
// runs 9/d9k-critic.md: -1.0, lower in 8 of 8), so the default stays the old look.
// Same materials, same instances: no draw is added.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public static class TracerLook
    {
        public const string Knob = "fx.tracerInSmoke";
        public const float DefaultInSmoke = 0f, OldInSmoke = 0f;   // cycle 9: t1 passed alone, failed with the set on weight (a0050)

        /// <summary>fx.tracerInSmoke, clamped to 0-1; read once (CombatFx.Start). Above 0.5 is the new order and shape.</summary>
        public static float Read() => Mathf.Clamp01(Knobs.Get(Knob, DefaultInSmoke));
        public static bool On(float inSmoke) => inSmoke > 0.5f;

        public const string ShapeKnob = "fx.tracerShape";
        public const float DefaultShape = 0f, OldShape = 0f;   // the old wide halo (with the order on, runs 9/t1's look)

        /// <summary>fx.tracerShape, clamped to 0-1 (read once, CombatFx.Start). Unset it is DefaultShape (0), or 1 when the
        /// order is on and fx.tracerInSmoke was itself set (C104's `fx.tracerInSmoke=1` run label keeps its meaning).</summary>
        public static float ReadShape(bool inSmoke)
        {
            float unset = inSmoke && Knobs.Overrides.ContainsKey(Knob) ? 1f : DefaultShape;
            return Mathf.Clamp01(Knobs.Get(ShapeKnob, unset));
        }

        /// <summary>The night halo's queue: after every transparent (the old look), or just before the smoke books (3010).</summary>
        public const int OldHaloQueue = 3100, InSmokeHaloQueue = 3005;
        public static int HaloQueue(bool inSmoke) => inSmoke ? InSmokeHaloQueue : OldHaloQueue;
        /// <summary>The night halo's _ZWrite: off (the old look), or on, so a cloud behind the round is not drawn over it.</summary>
        public static float HaloZWrite(bool inSmoke) => inSmoke ? 1f : 0f;

        // the new night shape (the old numbers stay written out in Matrix below)
        public const float NightStreak = 7f;        // m, the old 10
        public const float NightHalo = 0.15f;       // m wide, the old 0.24
        public const float NightHaloLength = 0.5f;  // x the streak: the front half (the old 1.15, centred on the core)
        public const float NightHaloLead = 0.05f;   // x the streak: how far the head's glow runs ahead of the core's tip

        /// <summary>
        /// One layer of one tracer: side 0 / 1 the night halo in its side's colour, 2 the night's white-hot core, and any
        /// side by day the one streak. d = to - from and len = |d| (at least 0.1), k = its age over its life (0-1),
        /// closeUp = SceneHooks.CloseUp. With inSmoke false this is the code before C104 expression for expression.
        /// </summary>
        public static Matrix4x4 Matrix(Vector3 from, Vector3 d, float len, float k, bool night, int side, float closeUp, bool inSmoke)
            => Matrix(from, d, len, k, night, side, closeUp, inSmoke ? 1f : 0f);

        /// <summary>
        /// As above with the night shape as a 0-1 blend (fx.tracerShape): 0 is the old code and 1 C104's, each bit for bit
        /// (their own branches); between them the streak, the halo's width and length and its tip's lead are lerped.
        /// </summary>
        public static Matrix4x4 Matrix(Vector3 from, Vector3 d, float len, float k, bool night, int side, float closeUp, float shape)
        {
            if (shape <= 0f || !night)
            {
                float streak = Mathf.Min(len, night ? 10f : 6f);
                Vector3 mid = from + d.normalized * Mathf.Lerp(streak * 0.5f, len - streak * 0.5f, k);
                float thick = (!night ? 0.045f : side == 2 ? 0.075f : 0.24f) * Mathf.Lerp(1f, 0.30f, closeUp);   // sized for the standard view; among the men a round is a thin line
                return Matrix4x4.TRS(mid, Quaternion.LookRotation(d), new Vector3(thick, thick, side == 2 ? streak * 0.8f : streak * 1.15f));
            }
            if (shape < 1f) return Blend(from, d, len, k, side, closeUp, shape);
            {
                float streak = Mathf.Min(len, NightStreak);
                Vector3 along = d.normalized;
                Vector3 mid = from + along * Mathf.Lerp(streak * 0.5f, len - streak * 0.5f, k);
                float thick = (side == 2 ? 0.075f : NightHalo) * Mathf.Lerp(1f, 0.30f, closeUp);
                if (side == 2) return Matrix4x4.TRS(mid, Quaternion.LookRotation(d), new Vector3(thick, thick, streak * 0.8f));
                // the head: the halo's front sits a little ahead of the core's tip (the core is 0.8 x the streak, centred)
                float halo = streak * NightHaloLength;
                Vector3 head = mid + along * (streak * (0.4f + NightHaloLead) - halo * 0.5f);
                return Matrix4x4.TRS(head, Quaternion.LookRotation(d), new Vector3(thick, thick, halo));
            }
        }

        static Matrix4x4 Blend(Vector3 from, Vector3 d, float len, float k, int side, float closeUp, float s)
        {
            float streak = Mathf.Min(len, Mathf.Lerp(10f, NightStreak, s));
            Vector3 along = d.normalized;
            Vector3 mid = from + along * Mathf.Lerp(streak * 0.5f, len - streak * 0.5f, k);
            float thick = (side == 2 ? 0.075f : Mathf.Lerp(0.24f, NightHalo, s)) * Mathf.Lerp(1f, 0.30f, closeUp);
            if (side == 2) return Matrix4x4.TRS(mid, Quaternion.LookRotation(d), new Vector3(thick, thick, streak * 0.8f));
            // the old halo is centred (its tip 0.575 x the streak ahead of mid), C104's tip is 0.4 + lead ahead: lerp both
            float halo = streak * Mathf.Lerp(1.15f, NightHaloLength, s);
            float tip = streak * Mathf.Lerp(0.575f, 0.4f + NightHaloLead, s);
            return Matrix4x4.TRS(mid + along * (tip - halo * 0.5f), Quaternion.LookRotation(d), new Vector3(thick, thick, halo));
        }
    }
}
