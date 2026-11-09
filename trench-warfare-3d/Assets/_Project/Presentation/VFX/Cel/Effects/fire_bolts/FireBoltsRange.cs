// Phase: CelFx job (2026-11-09, lane/show/cel-fire-bolts; b04: lanes spread in height, two smoke columns per ground hit; b03: low parallel lanes, smoke columns) - fire bolts on the Range (effect id "fire_bolts", lab
// only). Fires FireBolts.Comets and .Hits: each bolt launched from a spot near the stage's MuzzleHigh (off-screen left, at the
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
        // (r05: 1-1.5 dummy widths right of and in front of the dummy)
        // (b01, beach, measured on the reference's f79-f96: one fire ~1.2 dummy widths right and well behind the dummy, one
        // ~2.5 widths right and a little behind, one between)
        // (b03: a fourth fire far behind, the reference's upper burst at plate ~(965, 344))
        static readonly Vector2[] GroundAt = { new Vector2(1.7f, 4.0f), new Vector2(3.6f, 0.9f), new Vector2(3.2f, 7.0f), new Vector2(2.7f, 2.3f) };
        // b03: each ground spot's launch height from MuzzleHigh (m): so the bolt's lane on screen slopes about as gently as the
        // reference's (it enters the left edge ~115 px above where it lands); dummy hits launch DummyUp + their offset
        static readonly float[] GroundUp = { -1.4f, -2.0f, -0.9f, -1.7f };
        const float DummyUp = -1.3f;
        // b04: each bolt's extra launch height (m): about half the lanes start higher, so they cross the upper-left quarter
        // from near the horizon and arc down onto the dummy and the ground (b03: one low band)
        static readonly float[] LaneUp = { 0f, 2.6f, 0.8f, 3.8f, 0.3f, 1.7f, 3.1f, 0.5f, 4.4f, 1.2f, 2.2f };   // b05: wider (b04 up to 2.6 m: one band)
        static readonly Vector2[] DummyAt = { new Vector2(0f, 0f), new Vector2(0.05f, 0.3f), new Vector2(-0.1f, -0.1f), new Vector2(0.1f, 0.15f),
            new Vector2(0f, 0.05f), new Vector2(-0.05f, 0.25f), new Vector2(0.08f, -0.05f), new Vector2(0f, 0.1f),
            new Vector2(-0.12f, 0.2f), new Vector2(0.12f, 0.35f), new Vector2(-0.08f, -0.15f) };
        // launch spots: (screen right m, up m, away m) from MuzzleHigh, staggered in height and depth so the volley's
        // paths cross at different angles instead of flying as a parallel row
        // b03: small per-bolt jitter of the launch spot (screen right m, up m, away m): the lanes stay parallel bands, never
        // one file (b02's spread of up to 1.7 m above MuzzleHigh+0.4 sent every bolt in from the top-left corner)
        static readonly Vector3[] FromAt = { new Vector3(0f, 0f, 0f), new Vector3(0.6f, 0.25f, 0.5f), new Vector3(-0.4f, -0.2f, -0.6f), new Vector3(0.3f, 0.12f, 0.3f),
            new Vector3(-0.2f, 0.3f, -0.4f), new Vector3(0.5f, -0.25f, 0.7f), new Vector3(0f, 0.18f, -0.2f), new Vector3(-0.5f, -0.1f, 0.4f),
            new Vector3(0.2f, 0.32f, 0.1f), new Vector3(-0.3f, 0.05f, -0.7f), new Vector3(0.4f, -0.18f, 0.2f) };

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
            var pool = FireBolts.Pool;
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
            Vector3 p0 = OnGround(s.Ground + right * -2.4f + away * -1.5f, 1.4f, s.Ground.y);
            CelFx.Play(pool, p0, away, p0, seed);

            int gi = 0, di = 0;
            for (int i = 0; i < FireBolts.Launch.Length; i++)
            {
                if (s.Lod == RangeLod.Far && (i & 1) == 1) continue;
                Vector3 target; float lane;
                if (FireBolts.OnGround[i]) { int g0 = gi++ % GroundAt.Length; var g = GroundAt[g0]; lane = GroundUp[g0]; target = OnGround(s.Ground + right * g.x + away * g.y, 0f, s.Ground.y); }
                else { var d = DummyAt[di++ % DummyAt.Length]; lane = DummyUp + d.y; target = s.Target + right * d.x + Vector3.up * d.y - away * 0.3f; }   // b01: on the board's front, so its sparks are not hidden behind it
                var f = FromAt[i % FromAt.Length];
                // b02: 0.4 m above MuzzleHigh (b01 was 1.1 m below; the reference's bolts come in from the upper left
                // about 2.5 m above the chest and arc down onto it)
                Vector3 from = s.MuzzleHigh + right * f.x + Vector3.up * (f.y + lane + LaneUp[(i * 5) % LaneUp.Length]) + away * f.z;   // b03: low lanes   // b02: 0.4 m above MuzzleHigh (a 1.4-2.2x spread scored worse) (the ref's bolts never share one line)
                if (i < FireBolts.AlongAt.Length) from = Vector3.Lerp(from, target, FireBolts.AlongAt[i]);
                // the comet is aimed above its hit by what its gravity drops over the flight: it arcs down onto it
                Vector3 aim = target + Vector3.up * FireBolts.ArcLift(FireBolts.Flights[i]);
                uint sd = seed * 7u + (uint)i * 101u;
                CelFx.Play(FireBolts.Comets[i], from, aim - from, aim, sd);
                CelFx.Play(FireBolts.Hits[i], target, target - from, target, sd + 13u);
                // b03: a smoke column on each ground hit, leaning up and along the shot (screen right)
                if (FireBolts.OnGround[i] && s.Lod != RangeLod.Far)
                {
                    CelFx.Play(FireBolts.Smokes[i], target, Vector3.up + right * 0.35f, target, sd + 29u);
                    // b04: a second, leaner column beside it on the near-dummy fires (Close only: Mid keeps one)
                    if (s.Lod == RangeLod.Close && (gi & 1) == 0) CelFx.Play(FireBolts.Smokes[i], target + right * 0.35f, Vector3.up + right * 0.6f, target, sd + 31u);
                }
            }
        }
    }
}
