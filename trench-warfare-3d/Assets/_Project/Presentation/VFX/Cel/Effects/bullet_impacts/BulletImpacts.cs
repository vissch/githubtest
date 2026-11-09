// Phase: CelFx job (2026-11-09, lane/show/cel-bullet-impacts) - bullet impacts: a machine-gun burst into the target
// dummy and the ground round it, after the owner's reference clip bullet_impacts_cel. Each round is three small
// recipes played at once: the tracer (a thin orange-white Streak from the muzzle to the hit), the hit (a star flash,
// a few sparks, dark flecks, a warm light through NightLights) and the ground (a warm Glow pool under the hit and tan
// cel dust lobes with ink that rise a little, drift and erode away). Dust stays where the round struck, so the
// clusters build up beside and behind the dummy as the burst goes on, as they do in the clip.
// LOD: Close = everything; Mid = one tracer, a flash, the pool, fewer and bigger puffs, no flecks; Far = the tracer
// line and the flash (two primitives). Draws: Streak + Glow + Lobe (two passes) = 4 at most, shared with every other
// cel effect drawn that frame.
// The Range fires the burst (CelRange id "bullet_impacts"); in the game each round is one call to PlayRound.
using UnityEngine;
using TW.Presentation.VFX.Cel;

namespace TW.Presentation.VFX.Cel.Effects
{
    public static class BulletImpacts
    {
        static BulletImpacts()
        {
            // the clip's dust: a warm grey-brown in the dark, tan where lit, dark brown ink
            CelPalette.Add("bi_dust", CelPalette.Hex(0x4A3C30), CelPalette.Hex(0x6E5C48), CelPalette.Hex(0x96805F), CelPalette.Hex(0xBCA27C),
                CelPalette.Hex(0xD2B88E), 1.1f, 3, CelPalette.Hex(0x22180F));
            // the tracer: white core, orange edge
            CelPalette.Add("bi_tracer", CelPalette.Hex(0xB0400C), CelPalette.Hex(0xF07A20), CelPalette.Hex(0xFFB860), CelPalette.Hex(0xFFE6B8),
                CelPalette.Hex(0xFFF8EC), 0.6f, 3, CelPalette.Hex(0x401004));
            // the ground pool: the warm light the hits throw on the field
            CelPalette.Add("bi_pool", CelPalette.Hex(0x2A1208), CelPalette.Hex(0x5A2A10), CelPalette.Hex(0x8A4A1E), CelPalette.Hex(0xB06A30),
                CelPalette.Hex(0xD08848), 0.95f, 3, CelPalette.Hex(0x000000));
            CelRange.Register("bullet_impacts", Init, (s, dt) => Step(s));
        }

        // ------------------------------------------------------------------------------------------ the recipes
        public static readonly CelFxRecipe Tracer = new CelFxRecipe("bi_tracer", 0.2f,
            new CelLayer
            {
                Name = "tracer", Kind = CelKinds.StreakName, Start = 0f, Life = 0.13f, LifeJitter = 0.1f,
                Close = 1, Mid = 1, Far = 1, Shape = CelShape.Point, Aim = CelAim.Target, Speed = 0f, Arrive = 0.8f, Spread = 0f,
                Size = new CelCurve(0.055f, 0.035f), SizeJitter = 0.1f, Stretch = 0.12f, Erode = new CelCurve(0f, 0.2f, 2f),
                Palette = "bi_tracer", Core = 6f, Extra = new Vector4(0.15f, 0.8f, 0.15f, 0f),
            });

        public static readonly CelFxRecipe Hit = new CelFxRecipe("bi_hit", 0.8f,
            new CelLayer
            {
                Name = "flash", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.06f, -0.12f), Start = 0.06f, Life = 0.1f, LifeJitter = 0.1f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.42f, 0.62f, 0.5f), SizeJitter = 0.15f, Erode = new CelCurve(0.02f, 0.85f, 1.4f),
                Palette = "fire_hot", Core = 5f, Spin = 2f, Extra = new Vector4(2f, 1.3f, 7f, 5f),
            },
            new CelLayer
            {
                Name = "core", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.14f), Start = 0.06f, Life = 0.07f, LifeJitter = 0f,
                Close = 1, Mid = 0, Far = 0, Size = new CelCurve(0.16f, 0.22f), SizeJitter = 0.1f, Erode = new CelCurve(0f, 0.8f),
                Palette = "white", Core = 4f, Extra = new Vector4(2f, 1f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "sparks", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Start = 0.06f, Duration = 0.02f, Life = 0.22f,
                Close = 5, Mid = 2, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.05f, 0f, 0f), Aim = CelAim.Back, Spread = 65f,
                Speed = 7f, SpeedJitter = 0.45f, Gravity = 9.8f, Drag = 2f,
                Size = new CelCurve(0.022f, 0.012f), Stretch = 0.035f, Erode = new CelCurve(0.05f, 0.5f), Palette = "spark", Core = 4f, Extra = new Vector4(0f, 0.1f, 0.2f, 0f),
            },
            new CelLayer
            {
                Name = "flecks", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Start = 0.06f, Duration = 0.04f, Life = 0.7f,
                Close = 6, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.08f, 0f, 0f), Aim = CelAim.Radial, Speed = 3.5f, SpeedJitter = 0.5f,
                UpBias = 0.9f, Gravity = 9.8f, Drag = 0.6f, Spin = 4f,
                Size = new CelCurve(0.035f, 0.025f), SizeJitter = 0.4f, Erode = new CelCurve(0f, 0.3f, 3f),
                Palette = "smoke_black", Core = 1f, Ink = 1f, Extra = new Vector4(2.2f, 0.3f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = 0.06f, Life = 0.16f,
                Close = 1, Mid = 1, Far = 0, Palette = "fire", Core = 3.2f, Extra = new Vector4(8f, 0f, 0f, 0f),
            });

        public static readonly CelFxRecipe Ground = new CelFxRecipe("bi_ground", 2.4f,
            new CelLayer
            {
                Name = "dust", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.12f, 0f), Start = 0.07f, Duration = 0.1f, Life = 1.45f, LifeJitter = 0.15f,
                Close = 4, Mid = 2, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.3f, 0f, 0f), Aim = CelAim.Radial, Speed = 1.2f, SpeedJitter = 0.4f,
                UpBias = 0.8f, Drag = 2.4f, Buoyancy = 0.22f, Spin = 0.5f,
                Size = new CelCurve(0.2f, 0.44f, 0.3f), SizeJitter = 0.25f, Stretch = 0.9f, Erode = new CelCurve(0f, 1f, 3.2f), Feather = 0f,
                Palette = "bi_dust", Core = 1f, Ink = 2.4f, Extra = new Vector4(1.1f, 0.18f, 0.6f, 0f),
            },
            new CelLayer
            {
                Name = "spurt", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.05f, 0f), Start = 0.06f, Duration = 0.03f, Life = 0.45f,
                Close = 3, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.1f, 0f, 0f), Aim = CelAim.Up, Spread = 35f, Speed = 3.2f, SpeedJitter = 0.4f,
                Gravity = 6f, Drag = 2f,
                Size = new CelCurve(0.09f, 0.05f), SizeJitter = 0.3f, Stretch = 1.8f, Erode = new CelCurve(0f, 0.9f, 2f), Feather = 0f,
                Palette = "bi_dust", Core = 1f, Ink = 1.6f, Extra = new Vector4(2f, 0.25f, 0.6f, 0f),
            });

        static CelLayer Pool(float r0, float r1, float core, int far) => new CelLayer
        {
            Name = "pool", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.03f, 0f), Start = 0.05f, Life = 0.16f, LifeJitter = 0.1f,
            Close = 1, Mid = 1, Far = far, Size = new CelCurve(r0, r1, 0.5f), SizeJitter = 0.1f, Erode = new CelCurve(0.0f, 0.7f, 1.6f),
            Palette = "bi_pool", Core = core, Opacity = 1f, Extra = new Vector4(0f, 1.1f, 0f, 0f),
        };
        public static readonly CelFxRecipe PoolSmall = new CelFxRecipe("bi_pool_small", 0.3f, Pool(2.6f, 3.1f, 1.4f, 0));
        public static readonly CelFxRecipe PoolBig = new CelFxRecipe("bi_pool_big", 0.3f, Pool(3.6f, 4.3f, 1.7f, 0));
        public static readonly CelFxRecipe PoolNear = new CelFxRecipe("bi_pool_near", 0.3f, Pool(1.8f, 2.2f, 1.5f, 0),
            new CelLayer
            {
                Name = "near light", Kind = CelKinds.LightName, Anchor = CelAnchor.Target, Start = 0.02f, Life = 0.14f,
                Close = 1, Mid = 0, Far = 0, Palette = "fire", Core = 2.5f, Extra = new Vector4(5f, 0f, 0f, 0f),
            });

        // ------------------------------------------------------------------------------------------ one round
        /// <summary>One round: from `muzzle` to `hit`, with `ground` where its dust and pool go
        /// (the same as `hit` for a round into the field; behind the board for a round into the dummy). No allocation.
        /// The near pool assumes flat ground between the gun and the hit (it is laid at the hit's ground height).
        /// `heat` 0..1 is how long the gun has been firing: the pool under the hits grows with it; `nearPool` also lights
        /// the field the tracer passes near the gun.</summary>
        public static void PlayRound(Vector3 muzzle, Vector3 hit, Vector3 ground, uint seed, float heat = 0.5f, bool nearPool = false)
        {
            Vector3 dir = hit - muzzle;
            CelFx.Play(Tracer, muzzle, dir, hit, seed);
            CelFx.Play(Hit, muzzle, dir, hit, seed + 101u);
            CelFx.Play(Ground, ground - dir.normalized * 0.01f, dir, ground, seed + 211u);
            CelFx.Play(heat > 0.5f ? PoolBig : PoolSmall, ground, dir, ground, seed + 307u);
            if (nearPool)
            {
                Vector3 near = Vector3.Lerp(muzzle, hit, 0.45f);
                near.y = ground.y;
                CelFx.Play(PoolNear, near, dir, near, seed + 401u);
            }
        }

        // ------------------------------------------------------------------------------------------ the Range burst
        const int Rounds = 16;
        const float Every = 0.172f;

        sealed class Burst { public int Next; }

        static float Hash01(int seed, int k, int salt)
        {
            uint h = (uint)seed * 747796405u + (uint)k * 2891336453u + (uint)salt * 1181783497u;
            h ^= h >> 16; h *= 0x7feb352du; h ^= h >> 15; h *= 0x846ca68bu; h ^= h >> 16;
            return (h & 0xFFFFFF) / 16777216f;
        }

        // where the rounds go, in the camera's frame at the dummy (x screen right, z away from the eye; m):
        // two clusters on the ground left of the dummy (one nearer, one behind), one to its right and behind, the board
        static readonly Vector2[] Spots = { new Vector2(-1.15f, -0.5f), new Vector2(1.45f, 0.45f), new Vector2(0f, 0f), new Vector2(-0.95f, 0.9f), new Vector2(1.9f, 0.1f), new Vector2(0f, 0f) };

        static void Init(RangeShot s) { EnsureRenderer(); s.State = new Burst(); Step(s); }

        /// <summary>The kit's CelFx.Renderer makes its renderer with AddComponent, whose Awake asks CelFx.Renderer again
        /// before it is set and recurses into a stack overflow (reported to the kit). Make it here first, on an
        /// inactive object, adopt it, then wake it: Awake then finds it adopted.</summary>
        public static void EnsureRenderer()
        {
            if (Object.FindFirstObjectByType<CelFxRenderer>() != null) return;
            var go = new GameObject("CelFx") { hideFlags = HideFlags.DontSave };
            go.SetActive(false);
            var r = go.AddComponent<CelFxRenderer>();
            CelFx.Adopt(r);
            go.SetActive(true);
        }

        static void Step(RangeShot s)
        {
            if (!(s.State is Burst b)) return;
            float since = Time.time - s.FiredAt;
            Vector3 look = s.Target - s.Eye; look.y = 0f;
            if (look.sqrMagnitude < 1e-4f) { look = s.Direction; look.y = 0f; }
            look = look.sqrMagnitude > 1e-6f ? look.normalized : Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, look).normalized;
            float keep = CelFx.ZoomOverride;
            CelFx.ZoomOverride = s.Zoom;   // the shot's tier, not whatever the camera reads while the stage poses it
            while (b.Next < Rounds && b.Next * Every + (Hash01(s.Seed, b.Next, 9) - 0.5f) * 0.05f <= since + 1e-4f)
            {
                int k = b.Next++;
                float a = Hash01(s.Seed, k, 1), c = Hash01(s.Seed, k, 2), d = Hash01(s.Seed, k, 3);
                Vector2 spot = Spots[(k + (int)(d * 2f)) % Spots.Length];
                bool onBody = spot.sqrMagnitude < 1e-6f;
                Vector3 hit, ground;
                if (onBody)
                {
                    hit = s.Target + right * ((a - 0.5f) * 0.3f) + Vector3.up * ((c - 0.5f) * 0.45f);
                    ground = s.Ground + right * (0.9f + 0.5f * a) + look * (0.9f + 0.4f * c);
                }
                else
                {
                    ground = s.Ground + right * (spot.x + (a - 0.5f) * 0.55f) + look * (spot.y + (c - 0.5f) * 0.5f);
                    hit = ground;
                }
                Vector3 from = s.Muzzle + Vector3.up * ((d - 0.5f) * 0.2f);
                PlayRound(from, hit, ground, (uint)(s.Seed * 13 + k * 977), k / (float)(Rounds - 1), k >= 4 && k % 3 != 0);
            }
            CelFx.ZoomOverride = keep;
        }

        // the scan finds this; its first call runs the static constructor, which registers the burst again with its
        // per-frame update (CelRange reads the entry afresh every frame), then starts it
        [CelRangeEffect("bullet_impacts")]
        static void RangeFire(RangeShot s) => Init(s);
    }
}
