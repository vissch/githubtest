// Phase: CelFx job (2026-11-09, lane/show/cel-bullet-impacts) - bullet impacts: a machine-gun burst into the target
// dummy and the ground round it, after the owner's reference clip bullet_impacts_cel. Each round is a few small recipes
// played at once:
//   the tracers   two thin orange-white Streaks from the gun that end exactly at the hit (they arrive as they die);
//   the near bolt on some rounds, a short thick streak near the camera whose head clips to white, with a soft halo;
//   the hit       a small white core, a small sharp five-point star, five thin orange spark spikes, dark flecks (no
//                 point light: its blob read as a sun at the hit);
//   the dust      on five rounds a cauliflower puff (4 merged tan lobes, one shadow band, thick dark ink) that
//                 drifts, shrinks and erodes away (the Range burst: five, at and behind the dummy), plus dirt spurts;
//   the washes    soft warm light (BiWash: the scene depth is lit by distance, no disc, no ring) under the hit, and on
//                 the near-bolt rounds a second one over the foreground the bolt passes.
// LOD: Close = everything; Mid = one tracer, flash, two spikes, the wash under the hit, two puffs; Far = the tracer and
// the flash (two primitives). Draws: Streak + Glow + Lobe (two passes) + BiWash = 5 at most, shared with every other
// cel effect drawn that frame.
// The Range fires the burst (CelRange id "bullet_impacts"); in the game each round is one call to PlayRound.
using UnityEngine;
using TW.Presentation.VFX.Cel;

namespace TW.Presentation.VFX.Cel.Effects
{
    /// <summary>The soft light wash and the halo (TW/Cel/BiWash): see the shader's header for its record.</summary>
    public sealed class BiWashKind : CelKind
    {
        public override string Name => "BiWash";
        public override Shader FindShader() => Shader.Find("TW/Cel/BiWash");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel BiWash");
        public override int Capacity => 256;
        public override int Order => 15;
    }

    public static class BulletImpacts
    {
        public const string WashName = "BiWash";

        static BulletImpacts()
        {
            // the clip's dust: a flat light tan fill (#dfb78f) with a slightly darker shade band and a thin dark brown ink
            CelPalette.Add("bi_dust", CelPalette.Hex(0x8A6A50), CelPalette.Hex(0xA8825E), CelPalette.Hex(0xC49A74), CelPalette.Hex(0xEDCDA4),
                CelPalette.Hex(0xF4D6AE), 0.42f, 2, CelPalette.Hex(0x0E0804));
            // the dirt spurts: a little darker than the puffs
            CelPalette.Add("bi_dirt", CelPalette.Hex(0x5A4633), CelPalette.Hex(0x7A6046), CelPalette.Hex(0x98795A), CelPalette.Hex(0xB89670),
                CelPalette.Hex(0xC8A67E), 1.1f, 2, CelPalette.Hex(0x22180F));
            // the tracer: a warm white core, orange edges (no neutral white: it read blue in the fog)
            CelPalette.Add("bi_tracer", CelPalette.Hex(0xB0400C), CelPalette.Hex(0xF07A20), CelPalette.Hex(0xFF9C40), CelPalette.Hex(0xFFC070),
                CelPalette.Hex(0xFFE2B0), 0.45f, 3, CelPalette.Hex(0x401004));
            // the wash: orange light, a pale hot centre
            CelPalette.Add("bi_wash", CelPalette.Hex(0x40281A), CelPalette.Hex(0x806048), CelPalette.Hex(0xC09A78), CelPalette.Hex(0xEEC49C),
                CelPalette.Hex(0xFFE6CC), 1.1f, 2, CelPalette.Hex(0x000000));
            CelRange.Register("bullet_impacts", Init, (s, dt) => Step(s));
        }

        // ------------------------------------------------------------------------------------------ the recipes
        // Tracers: Speed 0 + Arrive 1 makes each arrive at the hit on the frame it dies, so none flies past it.
        public static readonly CelFxRecipe Tracer = new CelFxRecipe("bi_tracer", 0.3f,
            new CelLayer
            {
                Name = "tracer", Kind = CelKinds.StreakName, Start = 0f, Duration = 0.07f, Life = 0.19f, LifeJitter = 0.12f,
                Close = 2, Mid = 1, Far = 1, Shape = CelShape.Box, ShapeSize = new Vector3(0.5f, 0.3f, 0f), Aim = CelAim.Target, Speed = 0f, Arrive = 1f, Spread = 0f,
                Size = new CelCurve(0.03f, 0.022f), SizeJitter = 0.15f, Stretch = 0.075f, Erode = new CelCurve(0f, 0.15f, 2f),
                Palette = "bi_tracer", Core = 5f, Extra = new Vector4(0.1f, 0.8f, 0.1f, 0f),
            });

        public static readonly CelFxRecipe NearBolt = new CelFxRecipe("bi_near_bolt", 0.3f,
            new CelLayer
            {
                Name = "bolt", Kind = CelKinds.StreakName, Start = 0f, Life = 0.14f, LifeJitter = 0f,
                Close = 1, Mid = 0, Far = 0, Aim = CelAim.Target, Speed = 0f, Arrive = 1f, Spread = 0f,
                Size = new CelCurve(0.13f, 0.07f, 0.7f), SizeJitter = 0.1f, Stretch = 0.06f, Erode = new CelCurve(0f, 0.1f, 2f),
                Palette = "bi_tracer", Core = 10f, Extra = new Vector4(0.05f, 0.6f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "bolt halo", Kind = WashName, Start = 0f, Life = 0.14f, LifeJitter = 0f,
                Close = 1, Mid = 0, Far = 0, Aim = CelAim.Target, Speed = 0f, Arrive = 1f, Spread = 0f,
                Size = new CelCurve(0.16f, 0.1f), SizeJitter = 0.1f, Palette = "bi_wash", Core = 1.4f, Stretch = 1f,
                Extra = new Vector4(1f, 1.4f, 0f, 0.05f),
            });

        public static readonly CelFxRecipe Hit = new CelFxRecipe("bi_hit", 0.8f,
            new CelLayer
            {
                Name = "flash", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.04f, -0.12f), Start = 0f, Life = 0.1f, LifeJitter = 0.1f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.07f, 0.09f, 0.5f), SizeJitter = 0.15f, Erode = new CelCurve(0.05f, 0.85f, 1.4f),
                Palette = "fire_hot", Core = 2f, Spin = 2f, Extra = new Vector4(2f, 1.6f, 5f, 9f),
            },
            new CelLayer
            {
                Name = "core", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.15f), Start = 0f, Life = 0.08f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.035f, 0.045f), SizeJitter = 0.1f, Erode = new CelCurve(0f, 0.7f),
                Palette = "white", Core = 2.5f, Extra = new Vector4(2f, 0.6f, 0f, 0f),
            },
            new CelLayer
            {
                // short spikes out of the flash, mostly sideways and up, back toward the gun
                Name = "spikes", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.1f), Start = 0f, Duration = 0.01f, Life = 0.13f, LifeJitter = 0.2f,
                Close = 5, Mid = 2, Far = 0, Shape = CelShape.Shell, ShapeSize = new Vector3(0.04f, 0f, 0f), Aim = CelAim.Radial, UpBias = 0.25f,
                Speed = 3.2f, SpeedJitter = 0.3f, Drag = 3f,
                Size = new CelCurve(0.02f, 0.012f), Stretch = 0.05f, Erode = new CelCurve(0.02f, 0.4f), Palette = "spark", Core = 8f, Extra = new Vector4(0f, 0.12f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "flecks", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Start = 0.02f, Duration = 0.04f, Life = 0.7f,
                Close = 4, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.08f, 0f, 0f), Aim = CelAim.Radial, Speed = 3.5f, SpeedJitter = 0.5f,
                UpBias = 0.9f, Gravity = 9.8f, Drag = 0.6f, Spin = 4f,
                Size = new CelCurve(0.03f, 0.022f), SizeJitter = 0.4f, Erode = new CelCurve(0f, 0.3f, 3f),
                Palette = "smoke_black", Core = 1f, Ink = 1f, Extra = new Vector4(2.2f, 0.3f, 0f, 0f),
            }
            // no NightLights light: it draws its own sun-like blob at the hit (r03 f22/f68); the washes light the field
            );

        // the dust: small separate round puffs in a flat row across the hit, thrown left and right, shrinking into balls
        public static readonly CelFxRecipe Ground = new CelFxRecipe("bi_ground", 2.6f,
            new CelLayer
            {
                Name = "spurt", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.05f, 0f), Start = 0.0f, Duration = 0.03f, Life = 0.4f,
                Close = 3, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.08f, 0f, 0f), Aim = CelAim.Up, Spread = 35f, Speed = 3.2f, SpeedJitter = 0.4f,
                Gravity = 6f, Drag = 2f,
                Size = new CelCurve(0.07f, 0.04f), SizeJitter = 0.3f, Stretch = 1.8f, Erode = new CelCurve(0f, 0.9f, 2f), Feather = 0f,
                Palette = "bi_dirt", Core = 1f, Ink = 1.2f, Extra = new Vector4(2f, 0.25f, 0.6f, 0f),
            });

        // one cauliflower puff: 4 round lobes merged into one lump about the dummy's torso wide, one flat tan fill with one
        // shadow band and a thick dark-brown ink, drifting slowly sideways; it shrinks and erodes away over its life
        public static readonly CelFxRecipe Puff = new CelFxRecipe("bi_puff", 2.8f,
            new CelLayer
            {
                Name = "puff", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.3f, 0f), Start = 0.0f, Duration = 0.1f, Life = 3.2f, LifeJitter = 0.08f,
                Close = 5, Mid = 2, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(0.46f, 0.22f, 0.1f), Aim = CelAim.Radial, Speed = 0.3f, SpeedJitter = 0.3f,
                UpBias = 0.3f, Drag = 1.1f, Buoyancy = 0.05f, Spin = 0.25f,
                Size = new CelCurve(0.34f, 0.2f, 1.6f), SizeJitter = 0.25f, Stretch = 0.9f, Erode = new CelCurve(0f, 0.6f, 6f), Feather = 0f,
                Palette = "bi_dust", Core = 1.3f, Ink = 4f, Extra = new Vector4(0.8f, 0.16f, 0.75f, 0f),
            });

        static CelLayer Wash(string name, float r0, float r1, float power, float life, int mid, float squash) => new CelLayer
        {
            Name = name, Kind = WashName, Start = 0f, Life = life, LifeJitter = 0.1f,
            Close = 1, Mid = mid, Far = 0, Size = new CelCurve(r0, r1, 0.5f), SizeJitter = 0.08f,
            Palette = "bi_wash", Core = power, Opacity = 1f, Stretch = 1.6f, Extra = new Vector4(0f, 1.2f, squash, 0.12f),
        };
        // under the hit: about three dummy heights across, brighter as the burst goes on
        public static readonly CelFxRecipe WashSmall = new CelFxRecipe("bi_wash_small", 0.4f, Wash("wash", 2.4f, 2.8f, 1.5f, 0.24f, 1, 3.0f));
        public static readonly CelFxRecipe WashBig = new CelFxRecipe("bi_wash_big", 0.4f, Wash("wash", 2.6f, 3.0f, 1.9f, 0.26f, 1, 3.0f));
        // the dummy's facing side: a small round wash just in front of the board on the gun's side (no point light)
        public static readonly CelFxRecipe WashFace = new CelFxRecipe("bi_wash_face", 0.4f, Wash("face wash", 0.75f, 0.85f, 1.3f, 0.2f, 1, 1.0f));
        // the foreground the near bolt passes
        public static readonly CelFxRecipe WashNear = new CelFxRecipe("bi_wash_near", 0.4f, Wash("near wash", 2.4f, 2.8f, 2.4f, 0.28f, 0, 1.5f));   // no NightLights light here: a point light that near the ground burnt a sun-like spot

        // ------------------------------------------------------------------------------------------ one round
        /// <summary>One round: from `muzzle` to `hit`, with `ground` where its dust and wash go (the same as `hit` for a
        /// round into the field; behind the board for a round into the dummy). No allocation. `heat` 0..1 is how long the
        /// gun has been firing: the wash under the hits brightens with it. `nearBolt` adds the thick bolt near the gun and
        /// the light it throws on the foreground.</summary>
        public static void PlayRound(Vector3 muzzle, Vector3 hit, Vector3 ground, uint seed, float heat = 0.5f, bool nearBolt = false)
            => PlayRound(muzzle, hit, ground, ground, seed, heat, nearBolt);

        /// <summary>As above, with the warm wash centred on `washAt` (the Range: the dummy's foot, where the clip's light sits
        /// whichever side the round strikes).</summary>
        public static void PlayRound(Vector3 muzzle, Vector3 hit, Vector3 ground, Vector3 washAt, uint seed, float heat, bool nearBolt)
        {
            Vector3 dir = hit - muzzle;
            // the thin tracers start a little down the line so they stay thin; the near bolt starts at the gun
            CelFx.Play(Tracer, Vector3.Lerp(muzzle, hit, 0.18f), dir, hit, seed);
            CelFx.Play(Hit, muzzle, dir, hit, seed + 101u);
            CelFx.Play(Ground, ground - dir.normalized * 0.01f, dir, ground, seed + 211u);
            Vector3 under = washAt + Vector3.up * 0.05f;
            CelFx.Play(heat > 0.5f ? WashBig : WashSmall, under, dir, under, seed + 307u);
            Vector3 flat = dir; flat.y = 0f; flat = flat.sqrMagnitude > 1e-6f ? flat.normalized : Vector3.forward;
            Vector3 face = washAt + Vector3.up * 0.85f - flat * 0.55f;
            CelFx.Play(WashFace, face, dir, face, seed + 353u);
            if (nearBolt)
            {
                // the bolt crosses the near foreground only: it dies half way to the hit (the clip's f22/f37/f76 bolt
                // sits in the lower left), its thin tracer goes on to the hit
                CelFx.Play(NearBolt, muzzle, dir, Vector3.Lerp(muzzle, hit, 0.5f), seed + 401u);
                Vector3 near = Vector3.Lerp(muzzle, hit, 0.32f);   // on the bolt's own path near the gun, over the near foreground
                near.y = Mathf.Lerp(near.y, ground.y, 0.6f);
                CelFx.Play(WashNear, near, dir, near, seed + 503u);
            }
        }

        // ------------------------------------------------------------------------------------------ the Range burst
        const int Rounds = 16;
        const float Every = 0.2075f;   // the clip's 4.15-frame pulse on the shot clock (0.05 s a frame, close.json)

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
        // the rounds that come as a thick bolt past the camera (the clip's f22, f37, f45, f53, f62, f76)
        static bool IsNear(int k) => k == 1 || k == 5 || k == 7 || k == 8 || k == 11 || k == 14;   // round k flies at about f16 + 4.15 k

        // the dust clusters, in the same frame: at and behind the dummy (the clip's f76-f110 lumps left, right and behind)
        static readonly Vector3[] Clusters = { new Vector3(1.0f, 0.3f, 0f), new Vector3(-1.1f, 0.6f, 0.12f), new Vector3(2.1f, 0.9f, 0f), new Vector3(-1.0f, 1.8f, 0.2f), new Vector3(1.7f, 2.0f, 0.1f) };
        static bool IsPuff(int k) => k % 2 == 0 && k <= 8;   // rounds 0, 2, 4, 6, 8: five clusters, all gone by about f113

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
                uint sd = (uint)(s.Seed * 13 + k * 977);
                PlayRound(from, hit, ground, s.Ground, sd, k / (float)(Rounds - 1), IsNear(k));
                if (IsPuff(k))
                {
                    Vector3 cl = Clusters[(k / 2) % Clusters.Length];
                    Vector3 at = s.Ground + right * (cl.x + (a - 0.5f) * 0.2f) + look * cl.y + Vector3.up * cl.z;
                    CelFx.Play(Puff, at - look * 0.01f, look, at, sd + 601u);
                }
            }
            CelFx.ZoomOverride = keep;
        }

        // the scan finds this; its first call runs the static constructor, which registers the burst again with its
        // per-frame update (CelRange reads the entry afresh every frame), then starts it
        [CelRangeEffect("bullet_impacts")]
        static void RangeFire(RangeShot s) => Init(s);
    }
}
