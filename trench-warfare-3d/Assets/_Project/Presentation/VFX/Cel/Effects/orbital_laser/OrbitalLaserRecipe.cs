// Phase: CelFx orbital laser (2026-11-09) - a sweeping beam and two smoke rings leave a lasting crescent scorch.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class OrbitalLaserRecipe
    {
        public const float BeamSeconds = 3.55f, SweepSeconds = 3.36f, ScorchSeconds = float.MaxValue;
        public const float RadiusRight = 3f, RadiusForward = 2.24f;
        public const float SweepFrom = 0.45f, SweepTo = 3.22f;
        public const float SweepCentreRight = 0.45f;

        static readonly int Energy = CelPalette.Add("orbital_energy",
            CelPalette.Hex(0x2854BC), CelPalette.Hex(0x568FF7), CelPalette.Hex(0x8DE9F5),
            CelPalette.Hex(0xD7FAFD), Color.white, 0.72f, 4, CelPalette.Hex(0x14315B));
        static readonly int Smoke = CelPalette.Add("orbital_smoke",
            CelPalette.Hex(0x7F8486), CelPalette.Hex(0xADB4B6), CelPalette.Hex(0xCDD3D4),
            CelPalette.Hex(0xE1E7E8), CelPalette.Hex(0xE1E7E8), 1.1f, 3, CelPalette.Hex(0x7F8486));
        static readonly int Rock = CelPalette.Add("orbital_rock",
            CelPalette.Hex(0x070D1B), CelPalette.Hex(0x17253D), CelPalette.Hex(0x2D4058),
            CelPalette.Hex(0x627486), CelPalette.Hex(0x627486), 1.1f, 4, CelPalette.Hex(0x060B11));
        static readonly int RimStone = CelPalette.Add("orbital_rim_stone",
            CelPalette.Hex(0x171922), CelPalette.Hex(0x3D4249), CelPalette.Hex(0x777B82),
            CelPalette.Hex(0x8B8F95), Color.black, 1f, 3, CelPalette.Hex(0x11131A));

        public static Vector3 SweepOffset(float seconds, Vector3 right)
        {
            float k = Mathf.Clamp01((seconds - 0.2f) / (SweepSeconds - 0.2f));
            float angle = Mathf.Lerp(SweepFrom, SweepTo, Mathf.Pow(k, 0.88f));
            float terminal = Mathf.Lerp(3.18f, 3.06f, Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(3.20f, 3.36f, seconds)));
            float correction = Mathf.Lerp(0.55f, 0.75f, Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(3.30f, 3.50f, seconds)));
            angle = Mathf.Lerp(angle, terminal, Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(2.92f, 3.17f, seconds)) * correction);
            Vector3 forward = Vector3.Cross(right, Vector3.up);
            return right * (SweepCentreRight - Mathf.Sin(angle) * RadiusRight) + forward * (Mathf.Cos(angle) * RadiusForward);
        }

        public static void Play(Vector3 centre, Vector3 right, uint seed)
        {
            right.y = 0f;
            right = right.sqrMagnitude > 0.001f ? right.normalized : Vector3.right;
            Vector3 forward = Vector3.Cross(right, Vector3.up);
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(OrbitalLaserKind.KindName);
            int ground = renderer.KindId(OrbitalGroundKind.KindName);
            int debris = renderer.KindId(OrbitalDebrisKind.KindName);
            if (kind < 0 || ground < 0 || debris < 0) throw new System.InvalidOperationException("OrbitalLaser kind is not registered");
            float lod = CelFx.LodBlend(CelFx.Zoom);
            float now = CelFx.Now;
            bool far = lod >= 2f;
            float quality = CelFx.QualityShare(FxQuality.Tier);
            var rng = new CelRng(seed);
            centre.y = Ground(centre);
            Vector3 firstContact = centre + SweepOffset(0.40f, right);
            firstContact.y = Ground(firstContact);
            // v13: the clip's ground stays dark until f19 and is fully lit by f22 (front box 26,29,39 to 47,64,108).
            OrbitalLaserLighting.Add(centre, now, BeamSeconds - 1f / 24f, new Color(0.56f, 1.58f, 3.60f), 9.3f, right, // v15: green was 1.92 (G/B 0.66-0.78 vs the clip's 0.58)
                attackDelay: 0.15f);

            Spawn(far ? kind : ground, centre, right, now + 0.13f, ScorchSeconds, OrbitalLaserKind.Scorch,
                3.7f, 3.7f, 0.65f, Rock, 1f, 1f, ref rng);
            if (!far)
                Spawn(ground, centre, right, now + 0.04f, 3.47f, OrbitalLaserKind.Pool,
                    9.0f, 9.0f, 1.05f, Energy, 1.5f, 0.8f, ref rng, 0.04f);
            Spawn(kind, centre, right, now + 1f / 24f, BeamSeconds - 1f / 24f, OrbitalLaserKind.Beam,
                1.55f, 1.3f, 220f, Energy, 3.2f, 1f, ref rng, 1f / 24f);
            if (far) return;

            Spawn(kind, centre, right, now + 0.04f, 3.44f, OrbitalLaserKind.Impact,
                4.3f, 4.0f, 1f, Energy, 3.4f, 1f, ref rng, 0.04f);
            // v14: the clip has 2-3 broad pale ribbons round the shaft at once (f60-f67), 25-37 px wide; ours one
            // thin strip. The two added ribbons draw from their own stream so every later record keeps its seed.
            Spawn(kind, centre, right, now + 0.22f, BeamSeconds - 0.22f, OrbitalLaserKind.Spiral,
                0.08f, 0.105f, 12f, Energy, 2.2f, 0.95f, ref rng, 0.22f, 0f); // v15: v14's 0.15-0.20 x3 gave 2-3x the clip's area
            var swirlRng = new CelRng(seed ^ 0x2C1F5A93u);
            for (int i = 1; i < 2; i++)
                Spawn(kind, centre, right, now + 0.22f + i * 0.06f, BeamSeconds - 0.22f - i * 0.06f,
                    OrbitalLaserKind.Spiral, 0.08f, 0.105f, 12f, Energy, 2.2f, 0.95f, ref swirlRng,
                    0.22f + i * 0.06f, i * 2.09f);

            Spawn(kind, firstContact, right, now + 0.17f, 0.85f, OrbitalLaserKind.Shock,
                1.7f, 10.5f, 1f, Energy, 2f, 0.75f, ref rng);
            Vector3 secondContact = centre + SweepOffset(3.46f, right) * 0.85f - right * 0.30f;
            secondContact.y = Ground(secondContact);
            Spawn(kind, secondContact, right, now + 3.44f, 1.10f, OrbitalLaserKind.SmokeRibbon,
                2.97f, 38.7f, 1f, Smoke, 1f, 1.0f, ref rng, phase: 1f, drag: 6f);
            Vector3 lastContact = centre + SweepOffset(3.46f, right) - right * 0.20f;
            lastContact.y = Ground(lastContact) + 0.03f;
            // v03: the clip's burst is ~110 px at f100-f101 (r26's star ~270 px from f98).
            lastContact.y += 0.5f; // v07: the clip's burst sits above the tip
            var endFlash = Record(lastContact, right, now + 3.50f, 0.18f, OrbitalLaserKind.EndFlash,
                1.6f, 0.08f, 1.6f, Energy, 1f, 1f, ref rng);
            endFlash.Size.z = 2f;

            int smokeCount = Mathf.Max(1, Mathf.RoundToInt((lod < 0.5f ? 22 : 12) * quality));
            // v09: ring sizes, speeds and centres are r26's (measured against the clip: right); see _eye_v/patch_v09.py.
            // v13: measured - ring 1's puffs are ~0.7x v12's across (clip 50-95 px at f30) and break up instead of
            // growing (f38-f44 small clumps); its smoke area was 3x the clip's (f30 164k vs 45k px).
            SmokeRing(kind, firstContact, right, forward, now + 0.23f, smokeCount, 2.6f, 15f, 1.10f, ref rng,
                sizeScale: 1.0f, endScale: 0.8f); // v15: v13's 0.9 / 0.6 halved the clip's smoke area
            SmokeRing(kind, secondContact, right, forward, now + 3.44f, smokeCount, 2.97f, 38.7f, 1.10f, ref rng,
                drag: 6f, spawnJitter: 0.04f, speedJitter: 0.15f, radiusJitter: 0.4f, depth: 0.85f, sizeScale: 0.95f); // v19: 0.85 (v15) too small late, 1.05 (v16) too big at f104-f108
            // v01: the clip has a few small dark bits, not a hail of black pebbles (r26 f40: 7.2k dark px vs 1k).
            int shards = Mathf.Max(1, Mathf.RoundToInt((lod < 0.5f ? 14 : 6) * quality));
            for (int i = 0; i < shards; i++)
            {
                float start = rng.Range(0.18f, BeamSeconds - 0.2f);
                float a = rng.Range(0f, Mathf.PI * 2f);
                Vector3 radial = right * Mathf.Cos(a) + forward * Mathf.Sin(a);
                Vector3 at = centre + SweepOffset(start, right);
                at.y = Ground(at) + 0.18f;
                var rec = Record(at, right,
                    now + start, rng.Range(0.5f, 1.1f), OrbitalLaserKind.Rock,
                    rng.Range(0.07f, 0.18f), 0.06f, 1f, Rock, 1f, 1f, ref rng);
                rec.Vel = radial * rng.Range(3f, 8f) + Vector3.up * rng.Range(3.5f, 7.5f);
                rec.Motion = new Vector4(10f, 0.15f, rng.Range(2f, 7f), 0f);
                renderer.Spawn(debris, rec);
            }
            // v21 (owner on v20: "the sparks should wrap around the beam"): the clip's sparks (~27 a frame, balanced
            // left / right, 0-300 px up the shaft) drift right to left across its front at ~4 m/s and rise ~1.5 m/s -
            // a slow widening swirl, clockwise seen from above like the ribbons (_eye_v/sparktrack.py). v06-v20's
            // streak sparks were flung up and to the right at 8-15 m/s. Each orbit spark is shed from the shaft
            // (low more often), just outside it, wider the higher it starts; the shader rides it round the moving shaft.
            var orbitRng = new CelRng(seed ^ 0x6A09E667u);
            float orbitSlopeX = (Ground(centre + Vector3.right * 4f) - Ground(centre - Vector3.right * 4f)) / 8f;
            float orbitSlopeZ = (Ground(centre + Vector3.forward * 4f) - Ground(centre - Vector3.forward * 4f)) / 8f;
            // v21b: fitted offline (_eye_v/orbitsim.py) - each circles where it was shed, 2.2-3.6 m out.
            int orbitCount = Mathf.Max(1, Mathf.RoundToInt((lod < 0.5f ? 150 : 45) * quality)); // v21d: 120 (v21c) = the clip's count, but dim
            for (int i = 0; i < orbitCount; i++)
            {
                float start = orbitRng.Range(0.30f, 3.38f);
                float lift = orbitRng.Range(0f, 1f);
                float height = 0.3f + 3.0f * lift; // v22d: was 0.6-4.5 (v21e); they now rise faster
                var rec = Record(centre, right, now + start, orbitRng.Range(0.45f, 0.95f), OrbitalLaserKind.OrbitSpark,
                    orbitRng.Range(0.075f, 0.095f), 0.06f, 1f, CelPalette.Spark, orbitRng.Range(0.75f, 1f), 0.95f, ref orbitRng); // v22c: the clip's streaks 3.6 px wide (v22b 2.9)
                rec.Vel = new Vector3(orbitRng.Range(2.2f, 3.6f) + 0.35f * height,
                    orbitRng.Range(2.0f, 4.5f), orbitRng.Range(1.5f, 3.2f)); // v22d: rise 0.6-2 flew level (the clip's 30-60 deg up)
                rec.Motion = new Vector4(1f, 1f, orbitRng.Range(0.7f, 1.3f), 0.3f);
                rec.Erode = new Vector4(orbitRng.Range(0f, Mathf.PI * 2f), height, orbitSlopeX, orbitSlopeZ);
                rec.Size.w = orbitRng.Range(0.07f, 0.12f); // v22b: the clip's bright streaks are 15-30 px (v22's 0.028-0.045 s: 4-6 px specks)
                rec.Extra.y = start;
                renderer.Spawn(kind, rec);
            }
            var ejectRng = new CelRng(seed ^ 0xD17E21A3u);
            int sheets = Mathf.Max(1, Mathf.RoundToInt((lod < 0.5f ? 60 : 24) * quality)); // v23: needles 60 (140: a pile of sticks)
            for (int i = 0; i < sheets; i++)
            {
                float start = ejectRng.Range(0.18f, 3.30f);
                Vector3 at = centre + SweepOffset(start, right);
                // v21c: out of the billow mound's top once it is up (v21 showed them climbing through it as a
                // white coral at the foot; the clip's splinters fan out of the top of its foam, f72, f80).
                at.y = Ground(at) + Mathf.Lerp(0.18f, 1.25f + 0.40f * Mathf.Sin(i * 1.7f),
                    Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.30f, 0.60f, start)));
                var rec = Record(at, right, now + start, ejectRng.Range(0.18f, 0.40f),
                    OrbitalLaserKind.Ejecta, ejectRng.Range(0.12f, 0.22f), 0.07f, ejectRng.Range(3.5f, 7.0f), // v22b: needles, end 0.07 (0.02: hairlines)
                    Energy, 1f, 1f, ref ejectRng);
                float side = ejectRng.Range(-1f, 1f);
                rec.Vel = right * side * ejectRng.Range(2f, 6f) + forward * ejectRng.Range(-2f, 2f)
                    + Vector3.up * ejectRng.Range(3f, 6f);
                rec.Motion = new Vector4(4f, 1.5f, 0f, 0f);
                rec.Extra.z = side * 2.5f;
                renderer.Spawn(debris, rec);
            }
            var rimRng = new CelRng(seed ^ 0x31B7E843u);
            float slopeX = (Ground(centre + Vector3.right * 4f) - Ground(centre - Vector3.right * 4f)) / 8f;
            float slopeZ = (Ground(centre + Vector3.forward * 4f) - Ground(centre - Vector3.forward * 4f)) / 8f;
            int stones = Mathf.Max(1, Mathf.RoundToInt((lod < 0.5f ? 24 : 12) * quality));
            for (int i = 0; i < stones; i++)
            {
                float a = Mathf.Lerp(0.36f, 3.30f, (i + rimRng.Range(0.1f, 0.9f)) / stones);
                float radius = i % 3 == 0 ? 0.56f : 0.92f;
                radius += rimRng.Range(-0.025f, 0.025f);
                Vector3 at = centre + right * (0.12f - Mathf.Sin(a) * 3.7f * 0.95f * radius)
                    + forward * (Mathf.Cos(a) * 3.7f * 0.65f * radius - 0.60f);
                float size = rimRng.Range(0.08f, 0.17f); // v07: was 0.14-0.28 (a scalloped black edge)
                float front = Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(1.3f, 2.0f, a));
                size *= Mathf.Lerp(0.85f, 0.80f, front) * (i % 3 == 0 ? 0.70f : 1f);
                at.y = centre.y + (at.x - centre.x) * slopeX + (at.z - centre.z) * slopeZ
                    + 0.28f + size * 0.35f;
                var rec = Record(at, right, now + rimRng.Range(3.25f, 3.50f), ScorchSeconds,
                    OrbitalLaserKind.Rock, size, size, 1f, RimStone, Mathf.Lerp(1.0f, 0.60f, front), 1f, ref rimRng);
                rec.Extra.z = 1f;
                renderer.Spawn(debris, rec);
            }
            // v14: the clip's end glints - a pink-red cluster at the crescent's tip f104-f106, a few flying low
            // toward the camera by f108 (r26's blue flakes died by f102).
            int flakes = Mathf.Max(1, Mathf.RoundToInt((lod < 0.5f ? 12 : 4) * quality));
            for (int i = 0; i < flakes; i++)
            {
                float start = rimRng.Range(3.62f, 3.75f);
                Vector3 at = centre + SweepOffset(start, right);
                at.y = Ground(at) + rimRng.Range(0.30f, 0.60f);
                var rec = Record(at, right, now + start, rimRng.Range(0.28f, 0.42f), OrbitalLaserKind.Rock,
                    rimRng.Range(0.05f, 0.09f), 0.02f, 1f, Energy, 1f, 1f, ref rimRng);
                rec.Vel = -forward * (i % 3 == 0 ? rimRng.Range(6f, 10f) : rimRng.Range(0.5f, 2.5f))
                    + right * rimRng.Range(-3.0f, 0.4f) + Vector3.up * rimRng.Range(0.6f, 2.4f);
                rec.Motion = new Vector4(6f, 1f, rimRng.Range(-3f, 3f), 0f);
                rec.Extra.z = -1f;
                renderer.Spawn(debris, rec);
            }
            // A separate stream preserves the established shard and spark records.
            var smokeRng = new CelRng(seed ^ 0x51A7E2D3u);
            // v17: the clip's ring-1 torus front (centre column, as ground radius) is 3.8 m at f25, 6.1 m at f27,
            // 6.6 m at f28 - born on the crater rim, fast, then braking (ours was 5.4 / 6.6 / 7.1).
            Spawn(kind, firstContact, right, now + 0.3125f, 1.0f, OrbitalLaserKind.SmokeRibbon,
                0.05f, 42.4f, 1f, Smoke, 1f, 1.0f, ref smokeRng, drag: 3f); // v18: from f22.5 (v17 f23.5 was late)
            // v13: born with the first ring-2 ribbon and just inside it, so the two read as one thick torus at f99.
            Spawn(kind, secondContact, right, now + 3.45f, 0.98f, OrbitalLaserKind.SmokeRibbon,
                2.60f, 40.95f, 1f, Smoke, 1f, 0.85f, ref smokeRng, phase: 1f, drag: 6f);
            // Small buoyant embers keep shedding from the cooling channel after the shaft stops.
            var emberRng = new CelRng(seed ^ 0x93B7A15Du);
            int emberCount = Mathf.Max(1, Mathf.RoundToInt((lod < 0.5f ? 56 : 16) * quality));
            for (int i = 0; i < emberCount; i++)
            {
                float a = emberRng.Range(0.45f, 3.15f);
                float radius = emberRng.Range(0.71f, 0.79f);
                Vector3 at = centre + right * (0.12f - Mathf.Sin(a) * 3.7f * 0.95f * radius)
                    + forward * (Mathf.Cos(a) * 3.7f * 0.65f * radius - 0.60f);
                at.y = centre.y + (at.x - centre.x) * slopeX + (at.z - centre.z) * slopeZ + 0.24f;
                float start = emberRng.Range(3.55f, 5.05f);
                var ember = Record(at, right, now + start, emberRng.Range(0.65f, 1.05f),
                    0f, emberRng.Range(0.045f, 0.105f), 0.003f, 0.02f, CelPalette.Ember, 6f, 0.90f, ref emberRng);
                ember.Life = Mathf.Min(ember.Life, 5.80f - start);
                ember.Size.x *= Mathf.Lerp(1f, 0.5f, Mathf.InverseLerp(4.65f, 5.05f, start));
                ember.Vel = right * emberRng.Range(-0.35f, 0.35f) + forward * emberRng.Range(-0.35f, 0.35f)
                    + Vector3.up * emberRng.Range(1.2f, 3f);
                ember.Motion = new Vector4(0.4f, 0.6f, 0f, 0.2f);
                ember.Erode = new Vector4(0.10f, 1f, 1f, 0.02f);
                ember.Extra = new Vector4(0f, 0.015f, 0.45f, 0f);
                renderer.Spawn(CelKinds.Streak, ember);
            }
            // The white final flash overlays the colored fragments in this transparent batch.
            renderer.Spawn(debris, endFlash);
        }

        static void SmokeRing(int kind, Vector3 centre, Vector3 right, Vector3 forward, float born,
            int count, float radius, float speed, float life, ref CelRng rng, float drag = 0.8f,
            float spawnJitter = 0.12f, float speedJitter = 0.28f, float radiusJitter = 0.8f, float depth = 1f,
            Vector3 drift = default, float sizeScale = 1f, float endScale = 1f)
        {
            for (int i = 0; i < count; i++)
            {
                float angle = (i + rng.Range(-0.47f, 0.47f)) * Mathf.PI * 2f / count;
                Vector3 radial = right * Mathf.Cos(angle) + forward * Mathf.Sin(angle);
                float radialScale = 1f;
                if (depth < 0.999f)
                {
                    radial = right * Mathf.Cos(angle) + forward * (Mathf.Sin(angle) * depth);
                    radialScale = radial.magnitude;
                    radial /= radialScale;
                }
                Vector3 p = centre + radial * ((radius + rng.Range(-radiusJitter, radiusJitter)) * radialScale);
                p.y = Ground(p) + 0.15f;
                var r = Record(p, right, born + rng.Range(0f, spawnJitter), rng.Jitter(life, 0.2f), OrbitalLaserKind.Puff,
                    rng.Range(0.55f, 1.20f) * sizeScale, rng.Range(1.0f, 1.9f) * sizeScale * endScale, rng.Range(0.7f, 1.35f),
                    Smoke, 1f, 1f, ref rng);
                r.Vel = radial * (rng.Jitter(speed, speedJitter) * radialScale) + Vector3.up * rng.Range(0.1f, 0.75f);
                if (drag > 0f)
                {
                    // Both rings cross the uneven ground. Follow the local rise with the same
                    // drag integral instead of letting far puffs enter terrain or near puffs float.
                    float radialSpeed = Vector3.Dot(r.Vel, radial);
                    float travel = radialSpeed * (1f - Mathf.Exp(-drag * r.Life)) / drag;
                    if (travel > 0.01f)
                    {
                        float rise = Ground(p + radial * travel) - Ground(p);
                        r.Vel.y += radialSpeed * rise / travel;
                    }
                }
                r.Vel += drift;
                r.Motion = new Vector4(0f, drag, 0f, 0.05f);
                r.Extra.z = drag > 1f ? 1f : 0f;
                CelFx.Renderer.Spawn(kind, r);
            }
        }

        static void Spawn(int kind, Vector3 centre, Vector3 right, float born, float life, float mode,
            float size, float endSize, float stretch, int palette, float core, float opacity,
            ref CelRng rng, float timeOffset = 0f, float phase = 0f, float drag = 0f)
        {
            var r = Record(centre, right, born, life, mode, size, endSize, stretch, palette, core, opacity, ref rng);
            r.Extra.y = timeOffset;
            r.Extra.z = phase;
            r.Motion.z = drag;
            CelFx.Renderer.Spawn(kind, r);
        }

        /// <summary>Lab only (v22): a card of HDR swatches facing the eye, for a film of what the Range's night grade
        /// shows of energy values above 1 (celfx/runs/orbital_laser/_grade_hdr; ice_ball's chart stops at 1.0).</summary>
        public static void Chart(Vector3 at, float halfExtent, float seconds)
        {
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(OrbitalLaserKind.KindName);
            if (kind < 0) throw new System.InvalidOperationException("OrbitalLaser kind is not registered");
            var rng = new CelRng(1u);
            var rec = Record(at, Vector3.right, CelFx.Now, seconds, OrbitalLaserKind.Chart, halfExtent, halfExtent, 1f,
                CelPalette.Spark, 1f, 1f, ref rng);
            renderer.Spawn(kind, rec);
        }

        static CelRecord Record(Vector3 centre, Vector3 right, float born, float life, float mode,
            float size, float endSize, float stretch, int palette, float core, float opacity, ref CelRng rng)
        {
            Vector4 plane = Vector4.zero;
            if (mode < OrbitalLaserKind.Rock && RenderGround.Map != null)
            {
                // Fit the local beach slope once at emission; every GPU ground layer uses the same plane.
                plane.x = (Ground(centre + Vector3.right * 4f) - Ground(centre - Vector3.right * 4f)) / 8f;
                plane.y = (Ground(centre + Vector3.forward * 4f) - Ground(centre - Vector3.forward * 4f)) / 8f;
            }
            return new CelRecord
            {
                Pos = centre, Born = born, Axis = right, Seed = rng.Next(), Life = life,
                Size = new Vector4(size, endSize, 1f, stretch), Erode = new Vector4(0f, 0f, 1f, 0f),
                Tone = new Vector4(CelRecord.PackPalette(palette), core, opacity, 1f),
                Extra = new Vector4(mode, 0f, 0f, 0f),
                Motion = plane,
            };
        }

        static float Ground(Vector3 p) => RenderGround.Map != null
            ? RenderGround.Sample(RenderGround.Map, p.x, p.z) : p.y;
    }
}
