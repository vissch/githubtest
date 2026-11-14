// Phase: CelFx artillery - lab-only salvo, matched to the reference's independent impact sites and two volleys.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    static class ArtilleryRange
    {
        readonly struct Shell
        {
            public readonly Vector2 Site;
            public readonly float ImpactFrame, FlightFrames, Scale;
            public readonly int Recipe;
            public Shell(float right, float depth, float impactFrame, float flightFrames, float scale, int recipe)
            {
                Site = new Vector2(right, depth); ImpactFrame = impactFrame; FlightFrames = flightFrames;
                Scale = scale; Recipe = recipe;
            }
        }

        // Ground rays inverted from the reference pixels through the beach camera in dummy.json.
        static readonly Shell[] Salvo =
        {
            new Shell(-21.5853f, 19.0760f, 17f, 12f, 1f, 0),
            new Shell(-3.0710f, 15.8668f, 20f, 12f, 1f, 0),
            new Shell(9.2852f, 7.8640f, 21f, 12f, 1f, 0),
            new Shell(-4.5241f, -5.7050f, 28f, 12f, 1.08f, 1),
            new Shell(-21.5853f, 19.0760f, 59f, 12f, 1f, 0),
            new Shell(-3.0710f, 15.8668f, 59f, 12f, 1f, 0),
            new Shell(0.3183f, 0.0598f, 60f, 12f, 0.86f, 2),
            new Shell(8.3385f, 13.0314f, 65f, 12f, 1f, 0),
        };

        [CelRangeEffect("artillery")]
        static void Fire(RangeShot shot)
        {
            ArtilleryRecipe.Init();
            CelFx.ZoomOverride = shot.Lod == RangeLod.Close ? CelFx.CloseZoom : float.NaN;
            Vector3 forward = ViewEye.Main != null ? ViewEye.Main.transform.forward : shot.Direction;
            forward.y = 0f;
            forward = forward.sqrMagnitude > 1e-6f ? forward.normalized : Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, forward);
            for (int i = 0; i < Salvo.Length; i++)
            {
                Shell shell = Salvo[i];
                Vector3 at = shot.Ground + right * shell.Site.x + forward * shell.Site.y;
                float impact = shell.ImpactFrame / 24f - ArtilleryRecipe.LeadIn;
                CelFxRecipe recipe = shell.Recipe == 1 ? ArtilleryRecipe.Foreground : shell.Recipe == 2 ? ArtilleryRecipe.Target : ArtilleryRecipe.Background;
                ArtilleryRecipe.PlayAt(recipe, at, forward, (uint)shot.Seed + (uint)i * 101u, impact);
                if (shot.Lod == RangeLod.Far) continue;
                float path = shell.Recipe == 1 ? 7f : 18f;
                Vector3 start = at + right * path + Vector3.up * (path * 0.5f);
                float flight = shell.FlightFrames / 24f;
                ArtilleryRecipe.Projectile(start, at + Vector3.up * 0.15f, impact - flight, flight, shell.Scale,
                    (uint)shot.Seed + (uint)i * 617u + 7u, shot.Lod == RangeLod.Close);
            }
        }
    }
}
