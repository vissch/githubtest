// Phase: CelFx job (2026-11-09, lane/show/cel-bullet-impacts) - bullet impacts: a machine-gun burst into the target
// dummy and the ground round it, after the owner's reference clip bullet_impacts_cel. Each round is a shot and, when the
// tracer arrives, an impact:
//   the shot      two thin orange-white Streaks from the gun that end exactly at the hit (they arrive as they die); on
//                 one round in three a thick bolt past the camera, from the lower left corner, white head, orange bloom;
//   the impact    a small white core, a small star, five thin red-orange spark spikes, dark flecks, a crown of dirt
//                 spurts, and on ground hits a dust puff: a few small inked tan lobes that burst from the ground and rise;
//   the light     BiWash multiplies the scene by a warm light by distance (no disc, no ring, no haze: dark ground stays
//                 dark, the wooden dummy flares): a short pool at the dummy's feet, a short flare on its facing side, and
//                 on the bolt rounds the foreground along the bolt's path.
// LOD: Close = everything; Mid = one tracer, flash, two spikes, the pool, a few puff lobes; Far = the tracer and the
// flash (two primitives). Draws: Streak + Glow + Lobe (two passes) + BiWash = 5 at most, shared with every other cel
// effect drawn that frame.
// The Range fires the burst (CelRange id "bullet_impacts"); in the game each round is PlayShot then PlayImpact (or
// PlayRound for both at once).
using UnityEngine;
using TW.Presentation.VFX.Cel;

namespace TW.Presentation.VFX.Cel.Effects
{
    /// <summary>The warm light (TW/Cel/BiWash): see the shader's header for its record.</summary>
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
            // the clip's dust: a mid-tan body (#89604c), the light tan (#dfb78f) only where the lobe faces up, a darker
            // brown shade, and a dark brown ink
            CelPalette.Add("bi_dust", CelPalette.Hex(0x5A4030), CelPalette.Hex(0x7A5A46), CelPalette.Hex(0x9A7458), CelPalette.Hex(0xE2BE96),
                CelPalette.Hex(0xDFB78F), 1.1f, 3, CelPalette.Hex(0x1A0D06));
            // the dirt crowns: darker than the puffs
            CelPalette.Add("bi_dirt", CelPalette.Hex(0x3A281C), CelPalette.Hex(0x5A3E2C), CelPalette.Hex(0x7A5840), CelPalette.Hex(0xA07C5A),
                CelPalette.Hex(0xA07C5A), 1.1f, 3, CelPalette.Hex(0x140A04));
            // the tracer: a warm white core, orange edges (no neutral white: it read blue in the fog)
            CelPalette.Add("bi_tracer", CelPalette.Hex(0xB0400C), CelPalette.Hex(0xF07A20), CelPalette.Hex(0xFF9C40), CelPalette.Hex(0xFFC070),
                CelPalette.Hex(0xFFE2B0), 0.45f, 3, CelPalette.Hex(0x401004));
            // the hit's spark spikes: red-orange with a white tip
            CelPalette.Add("bi_spark", CelPalette.Hex(0xA02008), CelPalette.Hex(0xE04A14), CelPalette.Hex(0xFF7A2A), CelPalette.Hex(0xFFB060),
                CelPalette.Hex(0xFFF0D0), 0.85f, 3, CelPalette.Hex(0x300802));
            // the light: c3 is the light's colour (saturated orange; the scene is multiplied by 1 + it)
            CelPalette.Add("bi_wash", CelPalette.Hex(0x40200A), CelPalette.Hex(0x804018), CelPalette.Hex(0xC06A28), CelPalette.Hex(0xFF8A3C),
                CelPalette.Hex(0xFFB060), 1.1f, 2, CelPalette.Hex(0x000000));
            // the dummy's flare: more yellow
            CelPalette.Add("bi_flare", CelPalette.Hex(0x40280A), CelPalette.Hex(0x805018), CelPalette.Hex(0xC08A30), CelPalette.Hex(0xFF8030),
                CelPalette.Hex(0xFFC870), 1.1f, 2, CelPalette.Hex(0x000000));
            CelRange.Register("bullet_impacts", Init, (s, dt) => Step(s));
        }

        // ------------------------------------------------------------------------------------------ the shot
        /// <summary>Seconds from the shot to the impact: the tracers arrive at the hit on the frame they die.</summary>
        public const float Flight = 0.15f;

        // Tracers: Speed 0 + Arrive 1 makes each arrive at the hit on the frame it dies, so none flies past it.
        public static readonly CelFxRecipe Tracer = new CelFxRecipe("bi_tracer", 0.3f,
            new CelLayer
            {
                Name = "tracer", Kind = CelKinds.StreakName, Start = 0f, Duration = 0.03f, Life = Flight, LifeJitter = 0f,
                Close = 2, Mid = 1, Far = 1, Shape = CelShape.Box, ShapeSize = new Vector3(0.5f, 0.3f, 0f), Aim = CelAim.Target, Speed = 0f, Arrive = 1f, Spread = 0f,
                Size = new CelCurve(0.03f, 0.022f), SizeJitter = 0.15f, Stretch = 0.075f, Erode = new CelCurve(0f, 0.15f, 2f),
                Palette = "bi_tracer", Core = 2.5f, Extra = new Vector4(0.1f, 0.8f, 0.1f, 0f),
            });

        // the near bolt: thick (3x a tracer as it looks), a long trail back to the gun so it enters from the frame's
        // lower left corner, a white-clipped head and an orange bloom that rides on the head
        public static readonly CelFxRecipe NearBolt = new CelFxRecipe("bi_near_bolt", 0.3f,
            new CelLayer
            {
                Name = "bolt", Kind = CelKinds.StreakName, Start = 0f, Life = 0.2f, LifeJitter = 0f,
                Close = 1, Mid = 0, Far = 0, Aim = CelAim.Target, Speed = 0f, Arrive = 1f, Spread = 0f,
                Size = new CelCurve(0.07f, 0.05f, 0.7f), SizeJitter = 0.1f, Stretch = 0.14f, Erode = new CelCurve(0f, 0.1f, 2f),
                Palette = "bi_tracer", Core = 8f, Extra = new Vector4(0.15f, 0.8f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "bolt bloom", Kind = CelKinds.GlowName, Start = 0f, Life = 0.2f, LifeJitter = 0f,
                Close = 1, Mid = 0, Far = 0, Aim = CelAim.Target, Speed = 0f, Arrive = 1f, Spread = 0f,
                Size = new CelCurve(0.1f, 0.08f), Erode = new CelCurve(0.05f, 0.3f), Palette = "fire", Core = 1.5f, Opacity = 0.8f,
                Extra = new Vector4(2f, 1.8f, 0f, 0f),
            });

        // ------------------------------------------------------------------------------------------ the impact
        public static readonly CelFxRecipe Hit = new CelFxRecipe("bi_hit", 0.8f,
            new CelLayer
            {
                Name = "flash", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.02f, -0.12f), Start = 0f, Life = 0.08f, LifeJitter = 0.1f,
                Close = 1, Mid = 1, Far = 1, Size = new CelCurve(0.05f, 0.06f, 0.5f), SizeJitter = 0.15f, Erode = new CelCurve(0.1f, 0.85f, 1.4f),
                Palette = "fire_hot", Core = 1.4f, Spin = 2f, Extra = new Vector4(2f, 2.2f, 6f, 12f),
            },
            new CelLayer
            {
                Name = "core", Kind = CelKinds.GlowName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.15f), Start = 0f, Life = 0.08f, LifeJitter = 0f,
                Close = 1, Mid = 1, Far = 0, Size = new CelCurve(0.025f, 0.03f), SizeJitter = 0.1f, Erode = new CelCurve(0f, 0.7f),
                Palette = "white", Core = 2.5f, Extra = new Vector4(2f, 0.6f, 0f, 0f),
            },
            new CelLayer
            {
                // thin red-orange spikes about half the dummy's head long, mostly sideways and up, 2-3 frames
                Name = "spikes", Kind = CelKinds.StreakName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0f, -0.1f), Start = 0f, Duration = 0.005f, Life = 0.12f, LifeJitter = 0.15f,
                Close = 5, Mid = 2, Far = 0, Shape = CelShape.Shell, ShapeSize = new Vector3(0.03f, 0f, 0f), Aim = CelAim.Radial, UpBias = 0.2f,
                Speed = 4.2f, SpeedJitter = 0.25f, Drag = 2f,
                Size = new CelCurve(0.012f, 0.008f), Stretch = 0.05f, Erode = new CelCurve(0f, 0.3f), Palette = "bi_spark", Core = 3f, Extra = new Vector4(0f, 0.12f, 0f, 0f),
            },
            new CelLayer
            {
                Name = "flecks", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Start = 0.02f, Duration = 0.04f, Life = 0.6f,
                Close = 4, Mid = 0, Far = 0, Shape = CelShape.Sphere, ShapeSize = new Vector3(0.08f, 0f, 0f), Aim = CelAim.Radial, Speed = 3.5f, SpeedJitter = 0.5f,
                UpBias = 0.9f, Gravity = 9.8f, Drag = 0.6f, Spin = 4f,
                Size = new CelCurve(0.025f, 0.018f), SizeJitter = 0.4f, Erode = new CelCurve(0f, 0.3f, 3f),
                Palette = "smoke_black", Core = 1f, Ink = 1f, Extra = new Vector4(2.2f, 0.3f, 0f, 0f),
            });

        // the crown: dark dirt spurts thrown up in a ring from the ground point, a few frames
        public static readonly CelFxRecipe Ground = new CelFxRecipe("bi_ground", 0.6f,
            new CelLayer
            {
                Name = "crown", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.03f, 0f), Start = 0.0f, Duration = 0.02f, Life = 0.4f, LifeJitter = 0.2f,
                Close = 6, Mid = 0, Far = 0, Shape = CelShape.Disc, ShapeSize = new Vector3(0.1f, 0.05f, 0f), Aim = CelAim.Up, Spread = 40f, Speed = 2.6f, SpeedJitter = 0.35f,
                Gravity = 7f, Drag = 2.5f,
                Size = new CelCurve(0.04f, 0.02f), SizeJitter = 0.3f, Stretch = 2.6f, Erode = new CelCurve(0f, 0.9f, 2f), Feather = 0f,
                Palette = "bi_dirt", Core = 1f, Ink = 4f, Extra = new Vector4(2f, 0.25f, 1f, 0f),
            });

        // a dust puff: small round lobes that burst out of the ground at the hit and rise, together about the dummy's
        // torso wide, mid-tan with the light tan only on top, a thick dark ink on every lobe; they shrink and erode away
        static CelLayer PuffLobes(int close, int mid, float width, float r1) => new CelLayer
        {
            Name = "puff", Kind = CelKinds.LobeName, Anchor = CelAnchor.Target, Offset = new Vector3(0f, 0.04f, 0f), Start = 0.0f, Duration = 0.12f, Life = 1.9f, LifeJitter = 0.15f,
            Close = close, Mid = mid, Far = 0, Shape = CelShape.Box, ShapeSize = new Vector3(width, 0.04f, width * 0.6f), Aim = CelAim.Up, Spread = 55f,
            Speed = 1.1f, SpeedJitter = 0.5f, Drag = 2.6f, Buoyancy = 0.12f, Spin = 0.3f,
            Size = new CelCurve(r1 * 0.45f, r1, 0.4f), SizeJitter = 0.3f, Stretch = 1f, Erode = new CelCurve(0f, 0.7f, 5f), Feather = 0f,
            Palette = "bi_dust", Core = 1f, Ink = 4f, Extra = new Vector4(1.0f, 0.07f, 0.55f, 0f),
        };
        public static readonly CelFxRecipe PuffBig = new CelFxRecipe("bi_puff_big", 2.1f, PuffLobes(8, 3, 0.2f, 0.18f));
        public static readonly CelFxRecipe PuffSmall = new CelFxRecipe("bi_puff_small", 2.1f, PuffLobes(5, 2, 0.16f, 0.15f));

        // ------------------------------------------------------------------------------------------ the light
        // power: the light's gain (the scene x (1 + power x colour)); fill: an added share of it so black ground warms too
        static CelLayer Wash(string name, string palette, float r0, float r1, float power, float life, int mid, float squash, float fill) => new CelLayer
        {
            Name = name, Kind = WashName, Start = 0f, Life = life, LifeJitter = 0.1f,
            Close = 1, Mid = mid, Far = 0, Size = new CelCurve(r0, r1, 0.5f), SizeJitter = 0.08f,
            Palette = palette, Core = power, Opacity = 1f, Ink = fill, Stretch = 1.4f, Extra = new Vector4(0f, 1.3f, squash, 0.1f),
        };
        // the pool at the dummy's feet: about two dummy heights across, flat, 2-4 frames
        public static readonly CelFxRecipe WashPool = new CelFxRecipe("bi_wash_pool", 0.3f, Wash("pool", "bi_wash", 1.5f, 1.8f, 10f, 0.16f, 1, 2.5f, 0.03f));
        // the same late in the burst, when the clip's light is at its brightest (f60-f80)
        public static readonly CelFxRecipe WashPoolBig = new CelFxRecipe("bi_wash_pool_big", 0.3f, Wash("pool", "bi_wash", 1.9f, 2.2f, 13f, 0.18f, 1, 2.5f, 0.045f));
        // the dummy's facing side: a round light just in front of the board on the gun's side, 2-3 frames
        public static readonly CelFxRecipe WashFace = new CelFxRecipe("bi_wash_face", 0.3f, Wash("face", "bi_flare", 1.1f, 1.2f, 9f, 0.13f, 1, 1.0f, 0f));
        // the same, softer, when the round strikes the ground beside the dummy
        public static readonly CelFxRecipe WashFaceSoft = new CelFxRecipe("bi_wash_face_soft", 0.3f, Wash("face", "bi_flare", 1.1f, 1.2f, 4f, 0.11f, 0, 1.0f, 0f));
        // the foreground along the near bolt's path, at most 6 frames
        public static readonly CelFxRecipe WashNear = new CelFxRecipe("bi_wash_near", 0.4f, Wash("near", "bi_wash", 3.4f, 3.9f, 12f, 0.25f, 0, 1.2f, 0.1f));

        // ------------------------------------------------------------------------------------------ one round
        /// <summary>The shot: tracers from `muzzle` to `hit` (they arrive after Flight s); `nearBolt` adds the thick bolt
        /// past the camera and the light it throws on the foreground. No allocation.</summary>
        public static void PlayShot(Vector3 muzzle, Vector3 hit, Vector3 ground, uint seed, bool nearBolt)
        {
            Vector3 dir = hit - muzzle;
            // the thin tracers start a little down the line so they stay thin; the near bolt starts at the gun
            CelFx.Play(Tracer, Vector3.Lerp(muzzle, hit, 0.18f), dir, hit, seed);
            if (!nearBolt) return;
            // the bolt crosses the near foreground only: its head dies a third of the way to the hit (the clip's
            // f22/f37/f76 bolt sits in the lower left corner); its trail reaches back off the frame
            Vector3 end = Vector3.Lerp(muzzle, hit, 0.55f);
            CelFx.Play(NearBolt, muzzle, dir, end, seed + 401u);
            Vector3 near = Vector3.Lerp(muzzle, hit, 0.36f);
            near.y = Mathf.Lerp(near.y, ground.y, 0.8f);
            CelFx.Play(WashNear, near, dir, near, seed + 503u);
        }

        /// <summary>The impact at `hit`, with `ground` where its dust goes and `washAt` the foot the light pools at.
        /// `onBody` hits flare the dummy harder; `puff` 0 none, 1 small, 2 big; `heat` 0..1 how long the gun has fired (the
        /// pool grows past half). No allocation.</summary>
        public static void PlayImpact(Vector3 muzzle, Vector3 hit, Vector3 ground, Vector3 washAt, uint seed, bool onBody, int puff, float heat = 0.5f)
        {
            Vector3 dir = hit - muzzle;
            CelFx.Play(Hit, muzzle, dir, hit, seed + 101u);
            CelFx.Play(Ground, ground - dir.normalized * 0.01f, dir, ground, seed + 211u);
            if (puff > 0) CelFx.Play(puff > 1 ? PuffBig : PuffSmall, ground - dir.normalized * 0.01f, dir, ground, seed + 601u);
            Vector3 under = washAt + Vector3.up * 0.1f;
            CelFx.Play(heat > 0.5f ? WashPoolBig : WashPool, under, dir, under, seed + 307u);
            Vector3 flat = dir; flat.y = 0f; flat = flat.sqrMagnitude > 1e-6f ? flat.normalized : Vector3.forward;
            // the light on the dummy's facing side: on every round, strong when the round strikes the board
            Vector3 face = (onBody ? hit : washAt + Vector3.up * 0.9f) - flat * 0.35f;
            CelFx.Play(onBody ? WashFace : WashFaceSoft, face, dir, face, seed + 353u);
        }

        /// <summary>Shot and impact at once (the game, where the flight is shorter than a frame or not wanted).</summary>
        public static void PlayRound(Vector3 muzzle, Vector3 hit, Vector3 ground, uint seed, bool nearBolt = false)
        {
            PlayShot(muzzle, hit, ground, seed, nearBolt);
            PlayImpact(muzzle, hit, ground, ground, seed, false, 1);
        }

        // ------------------------------------------------------------------------------------------ the Range burst
        const int Rounds = 16;
        const float Every = 0.2075f;   // the clip's 4.15-frame pulse on the shot clock (0.05 s a frame, close.json)

        sealed class Burst { public int NextShot, NextHit; }

        static float Hash01(int seed, int k, int salt)
        {
            uint h = (uint)seed * 747796405u + (uint)k * 2891336453u + (uint)salt * 1181783497u;
            h ^= h >> 16; h *= 0x7feb352du; h ^= h >> 15; h *= 0x846ca68bu; h ^= h >> 16;
            return (h & 0xFFFFFF) / 16777216f;
        }

        // where the rounds go, in the camera's frame at the dummy (x screen right, z away from the eye; m); uneven on
        // purpose: one near cluster left of the dummy, two behind it on the right, one far left, the board
        static readonly Vector2[] Spots = { new Vector2(-0.9f, -0.35f), new Vector2(1.25f, 0.55f), new Vector2(0f, 0f), new Vector2(-1.7f, 1.1f), new Vector2(1.9f, 0.15f), new Vector2(0f, 0f) };
        // the rounds that come as a thick bolt past the camera (the clip's f22, f37, f45, f53, f62, f76)
        static bool IsNear(int k) => k == 1 || k == 5 || k == 7 || k == 8 || k == 11 || k == 14;

        static float ShotTime(RangeShot s, int k) => k * Every + (Hash01(s.Seed, k, 9) - 0.5f) * 0.05f;

        static void Places(RangeShot s, int k, Vector3 look, Vector3 right, out Vector3 from, out Vector3 hit, out Vector3 ground, out bool onBody)
        {
            float a = Hash01(s.Seed, k, 1), c = Hash01(s.Seed, k, 2), d = Hash01(s.Seed, k, 3);
            Vector2 spot = Spots[(k + (int)(d * 2f)) % Spots.Length];
            onBody = spot.sqrMagnitude < 1e-6f;
            if (onBody)
            {
                hit = s.Target + right * ((a - 0.5f) * 0.3f) + Vector3.up * ((c - 0.5f) * 0.45f);
                ground = s.Ground + right * (0.8f + 0.5f * a) + look * (0.9f + 0.4f * c);
            }
            else
            {
                ground = s.Ground + right * (spot.x + (a - 0.5f) * 0.45f) + look * (spot.y + (c - 0.5f) * 0.4f);
                hit = ground;
            }
            // on the field's own surface (the ground left of the dummy rises: a flat height buried the puffs there)
            if (RenderGround.Map != null) ground.y = RenderGround.Sample(RenderGround.Map, ground.x, ground.z);
            if (!onBody) hit = ground;
            from = s.Muzzle + Vector3.up * ((d - 0.5f) * 0.2f);
        }

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
            Vector3 from, hit, ground; bool onBody;
            while (b.NextShot < Rounds && ShotTime(s, b.NextShot) <= since + 1e-4f)
            {
                int k = b.NextShot++;
                Places(s, k, look, right, out from, out hit, out ground, out onBody);
                PlayShot(from, hit, ground, (uint)(s.Seed * 13 + k * 977), IsNear(k));
            }
            while (b.NextHit < b.NextShot && ShotTime(s, b.NextHit) + Flight <= since + 1e-4f)
            {
                int k = b.NextHit++;
                Places(s, k, look, right, out from, out hit, out ground, out onBody);
                // dust on the ground hits up to round 13 (all gone by about f124); big puffs on the right, small on the left
                int puff = onBody || k > 13 ? 0 : (Vector3.Dot(ground - s.Ground, right) > 0f || Hash01(s.Seed, k, 5) > 0.7f ? 2 : 1);
                PlayImpact(from, hit, ground, s.Ground, (uint)(s.Seed * 13 + k * 977), onBody, puff, k / (float)(Rounds - 1));
            }
            CelFx.ZoomOverride = keep;
        }

        // the scan finds this; its first call runs the static constructor, which registers the burst again with its
        // per-frame update (CelRange reads the entry afresh every frame), then starts it
        [CelRangeEffect("bullet_impacts")]
        static void RangeFire(RangeShot s) => Init(s);
    }
}
