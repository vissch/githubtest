// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts) - fire bolts on the Range (effect id "fire_bolts", lab
// only). Fires FireBolts.Volley: each bolt launched from a spot near the stage's MuzzleHigh (off-screen left, at the
// dummy's depth) and aimed at the dummy's board or at the ground beside and behind it, plus a soft warm light pool on
// the ground under the volley, left of and in front of the dummy. Places are laid out in the eye's frame (screen right / away from the eye on level
// ground), so the battle stills, seen from another heading, keep the same picture. Spawns everything at the fire.
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Presentation.VFX
{
    static class FireBoltsRange
    {
        // ground hits: (screen right m, away from the eye m) from the dummy's foot; dummy hits: (right, up) from its chest
        // (three fire spots about a metre right of and in front of the dummy, the reference's bursts, so every bolt ends at its hit)
        static readonly Vector2[] GroundAt = { new Vector2(1.3f, -0.5f), new Vector2(2.0f, -1.0f), new Vector2(1.7f, 0.3f) };
        static readonly Vector2[] DummyAt = { new Vector2(0f, 0f), new Vector2(0.05f, 0.3f), new Vector2(-0.1f, -0.1f), new Vector2(0.1f, 0.15f),
            new Vector2(0f, 0.05f), new Vector2(-0.05f, 0.25f), new Vector2(0.08f, -0.05f), new Vector2(0f, 0.1f) };
        // launch spots: (screen right m, up m) from MuzzleHigh
        // (all at or above it: the bolts come down 10-15 degrees on the screen onto the dummy and the ground)
        static readonly Vector2[] FromAt = { new Vector2(0f, -0.1f), new Vector2(0.6f, 0.4f), new Vector2(-0.4f, -0.5f), new Vector2(0.3f, 0.1f),
            new Vector2(-0.2f, 0.5f), new Vector2(0.5f, -0.3f), new Vector2(0f, 0.3f), new Vector2(-0.5f, -0.2f) };
        // the opening bolts start further along their line (already on screen at the fire, as in the reference's f19)
        static readonly float[] AlongAt = { 0.6f, 0.5f, 0.4f, 0.3f, 0.2f };

        /// <summary>The point on the ground: the highest terrain within `radius` (a flat glow laid there is not
        /// buried by a bump), or the stage's ground height when no map is loaded.</summary>
        static Vector3 OnGround(Vector3 p, float radius, float fallback)
        {
            var map = RenderGround.Map;
            if (map == null) { p.y = fallback; return p; }
            float h = RenderGround.Sample(map, p.x, p.z);
            if (radius > 0f)
                for (int k = 0; k < 8; k++)
                {
                    float a = k * 0.785398f, r = (k & 1) == 0 ? radius * 0.8f : radius * 0.45f;
                    h = Mathf.Max(h, RenderGround.Sample(map, p.x + Mathf.Cos(a) * r, p.z + Mathf.Sin(a) * r));
                }
            p.y = h + (radius > 0f ? 0.04f : 0f);
            return p;
        }

        [CelRangeEffect("fire_bolts")]
        static void Fire(RangeShot s)
        {
            Vector3 look = s.Target - (s.Eye.sqrMagnitude > 1e-6f ? s.Eye : s.MuzzleHigh);
            look.y = 0f;
            Vector3 away = look.sqrMagnitude > 1e-6f ? look.normalized : Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, away);
            uint seed = (uint)s.Seed;

            // The Range promises frame k at k/24 s, but the shot clock arrives stepping 1/20 s (Gym's CaptureFps wins
            // over RangeStage's 24; reported to the stage). On the close film, put the clock back to 24 fps and start
            // the volley late by the frames the fast clock skipped, so it lands on the reference's frames.
            var volley = FireBolts.Volley; var pool = FireBolts.Pool;
            if (s.Lod == RangeLod.Close && Time.captureFramerate > 0 && Time.captureFramerate != 24)
            {
                float lateBy = Mathf.Max(0f, s.T0 - s.T0 * Time.captureFramerate / 24f);
                Debug.Log("fire_bolts: the shot clock stepped 1/" + Time.captureFramerate + " s; set to 1/24 and the volley starts " + lateBy.ToString("0.000") + " s late");
                Time.captureFramerate = 24;
                // r04: no late start any more: the fire then lands on frame 16, nearer the reference's onset (f13)
                // than frame 19, and the opening bolts start well along their line (AlongAt)
            }

            // warm light on the ground under the volley: one soft pool left of and a little in front of the dummy (the
            // reference's lit oval over the lower-left third)
            Vector3 p0 = OnGround(s.Ground + right * -1.6f + away * -0.5f, 2.5f, s.Ground.y);
            CelFx.Play(pool, p0, away, p0, seed);

            int gi = 0, di = 0;
            for (int i = 0; i < volley.Length; i++)
            {
                if (s.Lod == RangeLod.Far && (i & 1) == 1) continue;
                Vector3 target;
                if (FireBolts.OnGround[i]) { var g = GroundAt[gi++ % GroundAt.Length]; target = OnGround(s.Ground + right * g.x + away * g.y, 0f, s.Ground.y); }
                else { var d = DummyAt[di++ % DummyAt.Length]; target = s.Target + right * d.x + Vector3.up * d.y; }
                var f = FromAt[i % FromAt.Length];
                Vector3 from = s.MuzzleHigh + right * f.x + Vector3.up * f.y;
                if (i < AlongAt.Length) from = Vector3.Lerp(from, target, AlongAt[i]);
                CelFx.Play(volley[i], from, target - from, target, seed * 7u + (uint)i * 101u);
            }
        }
    }
}
