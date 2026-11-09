// Phase: CelFx job (2026-11-09, lane/show/cel-explosion) - the explosion on the Range (CelRange id "explosion"):
// the reference shot rebuilt from ExplosionRecipe's pieces. Shells stream into the dummy for ~2.7 s (Hold), secondary
// ground blasts go off around it (Site, at the places and times read off explosive_blasts_nolora), a closing spark fan
// is thrown low to the left, then five smoke clouds stand (Cloud) and cut away by ~4.8 s.
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
            new Vector3(-12.8f, 5.7f, 0.95f),
            new Vector3(-5.4f, -2.5f, 1.74f),
            new Vector3(-4.45f, 21f, 2.24f),
            new Vector3(6.2f, 9.3f, 2.5f),
            new Vector3(-6.2f, 8.8f, 2.55f),
        };
        // smoke clouds: right, depth, seconds after the fire (the dummy's own cloud stands behind it)
        static readonly Vector3[] Clouds =
        {
            new Vector3(0f, 0.6f, 2.75f),
            new Vector3(-5.6f, -2.0f, 2.75f),
            new Vector3(-6.2f, 8.8f, 2.8f),
            new Vector3(-4.45f, 21f, 2.75f),
            new Vector3(6.2f, 9.3f, 2.78f),
        };

        [CelRangeEffect("explosion")]
        static void Fire(RangeShot s)
        {
            ExplosionRecipe.Init();
            Vector3 fh = s.Eye.sqrMagnitude > 1e-6f ? s.Target - s.Eye : s.Direction;
            fh.y = 0f;
            fh = fh.sqrMagnitude > 1e-6f ? fh.normalized : Vector3.forward;
            Vector3 rh = Vector3.Cross(Vector3.up, fh).normalized;
            uint seed = (uint)s.Seed;

            // the rounds come in flat from the left at the dummy's depth, a little under its chest
            Vector3 from = s.Target - rh * 9f - Vector3.up * 0.7f;
            CelFx.Play(ExplosionRecipe.Hold, from, fh, s.Target, seed);
            if (s.Lod == RangeLod.Far) return;

            for (int i = 0; i < Sites.Length; i++)
            {
                Vector3 g = GroundAt(s.Ground + rh * Sites[i].x + fh * Sites[i].y, s.Ground.y);
                ExplosionRecipe.PlayAt(ExplosionRecipe.Site, g, fh, g, seed + 101u * (uint)(i + 1), Sites[i].z);
            }
            Vector3 fanDir = (-rh - 0.35f * fh - 0.12f * Vector3.up).normalized;
            ExplosionRecipe.PlayAt(ExplosionRecipe.Fan, s.Target, fanDir, s.Target, seed + 7u, 2.7f);
            for (int i = 0; i < Clouds.Length; i++)
            {
                Vector3 g = GroundAt(s.Ground + rh * Clouds[i].x + fh * Clouds[i].y, s.Ground.y);
                ExplosionRecipe.PlayAt(ExplosionRecipe.Cloud, g, fh, g + Vector3.up * 0.9f, seed + 211u * (uint)(i + 1), Clouds[i].z);
            }
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
