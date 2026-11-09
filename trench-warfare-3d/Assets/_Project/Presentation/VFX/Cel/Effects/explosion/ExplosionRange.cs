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
            new Vector3(-15.0f, 5.6f, 0.95f),
            new Vector3(-7.7f, -4.1f, 1.6f),
            new Vector3(-11.0f, 20.5f, 2.65f),
            new Vector3(9.4f, 19.25f, 2.6f),
            new Vector3(-6.9f, 8.3f, 2.35f),
            new Vector3(7.6f, 8.5f, 2.3f),
        };
        // smoke clouds: right, depth, seconds after the fire (the dummy's own cloud stands behind it)
        // r04: sites 2 and 3 are far across the field (SiteFar)
        // r05: every site throws big chunks and grey puffs; site 5 is new (right of the fire, mid field), so debris
        // clusters stand ~40% of the frame either side of the fire through ref f59-f87
        static bool FarSite(int i) => i == 2 || i == 3;
        // r05: the clouds stand from 2.95 s (ref f90: no smoke wall before it), and an early one at the left edge
        static readonly Vector3[] Clouds =
        {
            // b01: the dummy's cloud stands further back (the dummy shows in front of it, as the ref's f105-f133), and
            // the back row is two clouds wide on the left (ref back-left cloud ~430 px)
            new Vector3(0f, 1.8f, 2.95f),
            new Vector3(-14.5f, -3.6f, 2.95f),
            new Vector3(-4.2f, 8.6f, 3.0f),
            new Vector3(9.95f, 8.7f, 2.98f),
            new Vector3(-13.45f, 5.85f, 1.15f),
            new Vector3(-9.6f, 8.4f, 3.02f),
        };

        const float ClipFps = 24f;
        static readonly System.Action<RangeShot> FireCb = Fire;
        static readonly System.Action<RangeShot, float> UpdateCb = Update;

        [CelRangeEffect("explosion")]
        static void Fire(RangeShot s)
        {
            // from now on CelRange also calls Update every frame of the shot (the attribute alone registers no update)
            CelRange.Register("explosion", FireCb, UpdateCb);
            ExplosionRecipe.Init();
            // b03: the beach camera reads ViewEye.Zoom 30 (T1), so without a pin the close film played the Mid counts and
            // Mid ink (no arcs at all). The close film pins the Close tier; the other shots keep the eye's own zoom.
            // (CelFx's own reset clears the pin between scenes; nothing else on the Range sets it.)
            CelFx.ZoomOverride = s.Lod == RangeLod.Close ? 7.5f : float.NaN;
            CelFx.Clock = Pinned(s) ? s.FiredAt : float.NaN;
            Vector3 fh = s.Eye.sqrMagnitude > 1e-6f ? s.Target - s.Eye : s.Direction;
            fh.y = 0f;
            fh = fh.sqrMagnitude > 1e-6f ? fh.normalized : Vector3.forward;
            Vector3 rh = Vector3.Cross(Vector3.up, fh).normalized;
            uint seed = (uint)s.Seed;

            // the rounds come in from the left frame edge (b01: through screen (0, ~500) on the beach camera, so from
            // the left and a little toward the eye, under the chest)
            Vector3 from = s.Target - rh * 12.5f - fh * 5f - Vector3.up * 0.6f;
            CelFx.Play(ExplosionRecipe.Hold, from, fh, s.Target, seed);
            ExplosionRecipe.PlayAt(ExplosionRecipe.Lead, from, fh, s.Target, seed + 3u, -0.2f);
            if (s.Lod == RangeLod.Far) { Update(s, 0f); return; }

            for (int i = 0; i < Sites.Length; i++)
            {
                Vector3 g = GroundAt(s.Ground + rh * Sites[i].x + fh * Sites[i].y, s.Ground.y);
                ExplosionRecipe.PlayAt(FarSite(i) ? ExplosionRecipe.SiteFar : ExplosionRecipe.Site, g, fh, g, seed + 101u * (uint)(i + 1), Sites[i].z);
            }
            // the late fan goes low to the lower left of the frame: left and toward the eye, a little down
            Vector3 fanDir = (-rh - 0.75f * fh - 0.22f * Vector3.up).normalized;
            ExplosionRecipe.PlayAt(ExplosionRecipe.Fan, s.Target, fanDir, s.Target, seed + 7u, 2.3f);
            for (int i = 0; i < Clouds.Length; i++)
            {
                Vector3 g = GroundAt(s.Ground + rh * Clouds[i].x + fh * Clouds[i].y, s.Ground.y);
                ExplosionRecipe.PlayAt(ExplosionRecipe.Cloud, g, fh, g + Vector3.up * 1.0f, seed + 211u * (uint)(i + 1), Clouds[i].z);
            }
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
        static bool Pinned(RangeShot s) => s.Lod == RangeLod.Close && Time.captureFramerate > 0 && Mathf.Abs(Time.captureFramerate - ClipFps) > 0.01f;

        /// <summary>The effect clock on the clips' frames: frame k of the film shows the effect at k/24 - t0.</summary>
        static void Update(RangeShot s, float sinceFire)
        {
            if (!Pinned(s)) return;
            CelFx.Clock = s.FiredAt + (sinceFire + s.T0) * (Time.captureFramerate / ClipFps) - s.T0;
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
