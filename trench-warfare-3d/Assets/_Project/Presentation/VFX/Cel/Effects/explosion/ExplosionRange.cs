// Phase: CelFx job (2026-11-09, lane/show/cel-explosion) - the explosion on the Range (CelRange id "explosion"):
// the reference shot rebuilt from ExplosionRecipe's pieces. Shells stream into the dummy for ~2.7 s (Hold), secondary
// ground blasts go off around it (Site, at the places and times read off explosive_blasts_nolora), a closing spark fan
// is thrown low to the left, then smoke clouds stand (Cloud) and cut away by ~4.8 s.
// The Range films at Time.captureFramerate; when that is not the clips' 24 (the stage steps 20 today), the close film
// pins CelFx.Clock so the effect still moves one 24th of a second a frame and starts on the clips' frame (t0 x 24): its
// frames then line up with the reference frame for frame. The game and the battle stills keep real seconds.
// Places are in the eye's flat frame (x right on screen, z away from the camera), metres from the dummy's foot,
// solved from the reference frames on the Range camera. Mid fires the same with the kit's Mid counts; Far fires only
// the Hold (a flash and a pool).
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    static class ExplosionRange
    {
        // right, depth (m from the foot), seconds after the fire
        static readonly Vector3[] Sites =
        {
            // b01: re-solved for the beach camera (each old place projected through the corridor camera and cast back
            // onto the beach ground from the same screen point)
            new Vector3(-12.10075f, 0.69706f, 0.95f),
            new Vector3(-9.42f, -5.05f, 1.6f),
            new Vector3(-3.9289f, 16.8851f, 2.366667f),
            new Vector3(9.4f, 19.25f, 2.6f),
            new Vector3(-6.9235f, 8.2451f, 2.2f),
            new Vector3(7.2862f, 9.4993f, 2.283333f),
        };
        // smoke clouds: right, depth, seconds after the fire (the dummy's own cloud stands behind it)
        // r04: sites 2 and 3 are far across the field (SiteFar)
        // r05: every site throws big chunks and grey puffs; site 5 is new (right of the fire, mid field), so debris
        // clusters stand ~40% of the frame either side of the fire through ref f59-f87
        static bool FarSite(int i) => i == 2 || i == 3;
        // r05: the clouds stand from 2.95 s (ref f90: no smoke wall before it), and an early one at the left edge
        static readonly Vector3[] Clouds =
        {
            new Vector3(0.35f, 0.3f, 2.95f),   // main, reference base at (828,480)
            new Vector3(-9.3f, -4.1f, 2.95f),  // foreground-left widened into the frame, top lowered by NearCloud
            new Vector3(-2.3f, 8.3f, 3.0f),    // dark peak behind the right end of the pale rear-left bank
            new Vector3(6.4f, 5.5f, 2.98f),    // rear-right, independent shorter lifetime
            new Vector3(-12.10075f, 0.69706f, 1.15f),// early left edge
            new Vector3(-6.2f, 5.3f, 3.02f),   // pale rear-left bank
        };

        const float ClipFps = 24f;
        sealed class ShotClock { public float Origin; }
        static readonly System.Action<RangeShot> FireCb = Fire;
        static readonly System.Action<RangeShot, float> UpdateCb = Update;

        [CelRangeEffect("explosion", 0.4f)]
        static void Fire(RangeShot s)
        {
            // from now on CelRange also calls Update every frame of the shot (the attribute alone registers no update)
            CelRange.Register("explosion", FireCb, UpdateCb, 0.4f);
            ExplosionRecipe.Init();
            // b03: the beach camera reads ViewEye.Zoom 30 (T1), so without a pin the close film played the Mid counts and
            // Mid ink (no arcs at all). The close film pins the Close tier; the other shots keep the eye's own zoom.
            // (CelFx's own reset clears the pin between scenes; nothing else on the Range sets it.)
            CelFx.ZoomOverride = s.Lod == RangeLod.Close ? 7.5f : float.NaN;
            // Schedule against the intended shot time, rather than the next Update that happened to cross it.
            // Otherwise a 0.55 s lead becomes 0.583 s at 24 fps and its first visible tracer is one frame late.
            if (Pinned(s))
            {
                var clock = new ShotClock { Origin = s.FiredAt - CelRange.Now };
                s.State = clock;
                CelFx.Clock = clock.Origin + s.T0 - s.Preroll;
            }
            else CelFx.Clock = float.NaN;
            Vector3 fh = s.Eye.sqrMagnitude > 1e-6f ? s.Target - s.Eye : s.Direction;
            fh.y = 0f;
            fh = fh.sqrMagnitude > 1e-6f ? fh.normalized : Vector3.forward;
            Vector3 rh = Vector3.Cross(Vector3.up, fh).normalized;
            uint seed = (uint)s.Seed;

            // the rounds come in from the left frame edge (b01: through screen (0, ~500) on the beach camera, so from
            // the left and a little toward the eye, under the chest)
            Vector3 from = s.Target - rh * 12.5f - fh * 5f - Vector3.up * 1.2f;
            ExplosionRecipe.PlayAt(ExplosionRecipe.Hold, from, fh, s.Target, seed, s.Preroll);
            ExplosionRecipe.PlayAt(ExplosionRecipe.Lead, from - Vector3.up * 0.06f, fh, s.Target - rh * 0.4f - Vector3.up * 0.1f, seed + 3u, 0f);
            ExplosionRecipe.PlayAt(ExplosionRecipe.LeadSecond, from + Vector3.up * 0.72f, fh, s.Target - rh * 0.4f + Vector3.up * 0.45f, seed + 5u, 0f);
            ExplosionRecipe.PlayAt(ExplosionRecipe.LeadThird, from + Vector3.up * 0.595445f, fh, s.Target - rh * 0.4f + Vector3.up * 0.334984f, seed + 11u, 0f);
            if (s.Lod == RangeLod.Far) { Update(s, 0f); return; }

            for (int i = 0; i < Sites.Length; i++)
            {
                if (i == 3) continue; // The reference has five secondary impacts; no extra far-right flash.
                Vector3 g = GroundAt(s.Ground + rh * Sites[i].x + fh * Sites[i].y, s.Ground.y);
                ExplosionRecipe.PlayAt(FarSite(i) ? ExplosionRecipe.SiteFar : (i <= 1 ? ExplosionRecipe.SiteNear : ExplosionRecipe.Site), g, fh, g, seed + 101u * (uint)(i + 1), Sites[i].z + s.Preroll);
            }
            // the late fan goes low to the lower left of the frame: left and toward the eye, a little down
            Vector3 fanDir = (-rh - 0.75f * fh - 0.22f * Vector3.up).normalized;
            ExplosionRecipe.PlayAt(ExplosionRecipe.Fan, s.Target, fanDir, s.Target, seed + 7u, 2.35f + s.Preroll);
            var camera = ViewEye.Main;
            Vector3 viewForward = camera != null ? camera.transform.forward : (s.Target - s.Eye).normalized;
            Vector3 viewRight = camera != null ? camera.transform.right : rh;
            Vector3 viewUp = camera != null ? camera.transform.up : Vector3.Cross(viewForward, rh).normalized;
            Vector3 leftColumn = GroundAt(s.Ground + rh * Sites[4].x + fh * Sites[4].y, s.Ground.y) + Vector3.up * 1.6f;
            Vector3 rightColumn = GroundAt(s.Ground + rh * Sites[5].x + fh * Sites[5].y, s.Ground.y) + Vector3.up * 1.61f + viewRight * 0.05f;
            Vector3 middleColumn = GroundAt(s.Ground + rh * Sites[2].x + fh * Sites[2].y, s.Ground.y) + Vector3.up * 2.05f - viewRight * 0.32f;
            ExplosionRecipe.PlayAt(ExplosionRecipe.ColumnLeft, leftColumn, (viewUp - viewRight * 0.05f).normalized, leftColumn, seed + 501u, 2.153333f + s.Preroll);
            ExplosionRecipe.PlayAt(ExplosionRecipe.ColumnRight, rightColumn, (viewUp + viewRight * 0.36f).normalized, rightColumn, seed + 503u, 2.278333f + s.Preroll);
            ExplosionRecipe.PlayAt(ExplosionRecipe.ColumnMiddle, middleColumn, (viewUp - viewRight * 0.18f).normalized, middleColumn, seed + 509u, 2.361667f + s.Preroll);
            Vector3 fanRoot = s.Target - rh * 0.8f - Vector3.up * 0.25f;
            ExplosionRecipe.PlayAt(ExplosionRecipe.FanUpper, fanRoot + viewUp * 0.50f, -viewRight, fanRoot + viewUp * 0.50f, seed + 401u, 2.22f + s.Preroll);
            ExplosionRecipe.PlayAt(ExplosionRecipe.FanMiddle, fanRoot + viewRight * 0.20f + viewUp * 0.16f, (-viewRight - viewUp * 0.20f).normalized, fanRoot + viewRight * 0.20f + viewUp * 0.16f, seed + 403u, 2.20f + s.Preroll);
            ExplosionRecipe.PlayAt(ExplosionRecipe.FanLower, fanRoot + viewRight * 0.22f, (-viewRight * 0.70f - viewUp * 0.714f).normalized, fanRoot + viewRight * 0.22f, seed + 409u, 2.20f + s.Preroll);
            for (int i = 0; i < Clouds.Length; i++)
            {
                Vector3 g = GroundAt(s.Ground + rh * Clouds[i].x + fh * Clouds[i].y, s.Ground.y);
                if (i == 0)
                    ExplosionRecipe.PlayAt(ExplosionRecipe.MainCloudBridge, g, fh, g + Vector3.up * 1.0f, seed + 1543u, Clouds[i].z + s.Preroll);
                ExplosionRecipe.PlayAt(ExplosionRecipe.CloudForSite(i), g, fh, g + Vector3.up * 1.0f, seed + 211u * (uint)(i + 1), Clouds[i].z + s.Preroll);
            }
            ExplosionRecipe.PlayLateTail(s.Target, rh, fh, seed + 997u, s.Preroll);
            Update(s, 0f);
            LogPools();
        }

        /// <summary>b03: records spawned against each pool's cap (past the cap the oldest, the Hold's, are overwritten).</summary>
        static void LogPools()
        {
            var r = CelFx.Renderer;
            if (r == null) return;
            for (int k = 0; k < r.KindCount; k++)
                Debug.Log("ExplosionRange pool " + r.KindName(k) + " spawned " + r.SpawnedOf(k) + " cap " + r.CapacityOf(k) + " tier " + FxQuality.Tier);
        }

        /// <summary>The close film only, and only when the Range does not step at the clips' rate.</summary>
        static bool Pinned(RangeShot s) => s.Lod == RangeLod.Close && Time.captureFramerate > 0;

        /// <summary>The effect clock on the clips' frames: frame k of the film shows the effect at k/24 - t0.</summary>
        static void Update(RangeShot s, float sinceFire)
        {
            if (!Pinned(s)) return;
            if (s.State is ShotClock clock)
                CelFx.Clock = clock.Origin + CelRange.Now * (Time.captureFramerate / ClipFps);
        }

        /// <summary>The ground under p (a ray down; the foot's height when nothing near it is hit).</summary>
        static Vector3 GroundAt(Vector3 p, float fallbackY)
        {
            p.y = fallbackY;
            if (Physics.Raycast(new Vector3(p.x, fallbackY + 20f, p.z), Vector3.down, out RaycastHit hit, 40f, ~0, QueryTriggerInteraction.Ignore)
                && Mathf.Abs(hit.point.y - fallbackY) < 4f)
                p.y = hit.point.y;
            return p;
        }
    }
}
