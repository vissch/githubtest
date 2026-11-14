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

        /// <summary>One real-time sweep frame at the simulation's head; no reference-shot volley or scripted path.</summary>
        public static void PlayBattleCharge(Vector3 at, float strength, float seconds)
        {
            var record = new CelRecord
            {
                Pos = at + Vector3.up * .35f, Born = CelFx.Now, Life = Mathf.Max(.02f, seconds),
                Axis = Vector3.up, Size = new Vector4(.3f + strength * 1.6f, .3f + strength * 1.6f, 1f, 0f),
                Tone = new Vector4(CelRecord.PackPalette(Energy), 1.5f + strength, .4f + strength * .5f, 0f),
                Extra = new Vector4(0f, 1.8f, 0f, 0f),
            };
            CelFx.Renderer.Spawn(CelKinds.Glow, record);
        }

        public static void PlayBattleFrame(Vector3 head, Vector3 right, float seconds, uint seed)
        {
            var rng = new CelRng(seed);
            const float elapsed = .3f;
            float phase = Mathf.Repeat(CelFx.Now, 2.6f);
            Vector3 centre = head - SweepOffset(elapsed + phase, right);
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(OrbitalLaserKind.KindName);
            var shaft = Record(centre, right, CelFx.Now - elapsed, elapsed + Mathf.Max(.01f, seconds),
                OrbitalLaserKind.Beam, 1.55f, 1.3f, 220f, Energy, 3.2f, 1f, ref rng);
            shaft.Extra.y = phase; shaft.Motion = Vector4.zero;
            renderer.Spawn(kind, shaft);
            if (CelFx.Zoom < CelFx.FarZoom)
            {
                var spiral = Record(centre, right, shaft.Born, shaft.Life, OrbitalLaserKind.Spiral,
                    .09f, .13f, 12f, Energy, 2.2f, .95f, ref rng);
                spiral.Extra.y = phase; spiral.Motion = Vector4.zero;
                renderer.Spawn(kind, spiral);
            }
            OrbitalLaserLighting.Add(head, CelFx.Now, seconds, new Color(.56f, 1.92f, 3.6f), 9.3f, right);
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
            OrbitalLaserLighting.Add(centre, now, BeamSeconds - 1f / 24f, new Color(0.56f, 1.92f, 3.60f), 9.3f, right);

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
            int spirals = 1;
            for (int i = 0; i < spirals; i++)
                Spawn(kind, centre, right, now + 0.22f, BeamSeconds - 0.22f, OrbitalLaserKind.Spiral,
                    0.09f, 0.13f, 12f, Energy, 2.2f, 0.95f, ref rng, 0.22f, i * 2.73f);

            Spawn(kind, firstContact, right, now + 0.17f, 0.85f, OrbitalLaserKind.Shock,
                1.7f, 10.5f, 1f, Energy, 2f, 0.75f, ref rng);
            Vector3 secondContact = centre + SweepOffset(3.46f, right) * 0.85f - right * 0.30f;
            secondContact.y = Ground(secondContact);
            Spawn(kind, secondContact, right, now + 3.44f, 1.10f, OrbitalLaserKind.SmokeRibbon,
                2.97f, 38.7f, 1f, Smoke, 1f, 0.6f, ref rng, phase: 1f, drag: 6f);
            Vector3 lastContact = centre + SweepOffset(3.46f, right) - right * 0.20f;
            lastContact.y = Ground(lastContact) + 0.03f;
            var endFlash = Record(lastContact, right, now + 3.46f, 0.18f, OrbitalLaserKind.EndFlash,
                1.35f, 0.08f, 1.6f, Energy, 1f, 1f, ref rng);
            endFlash.Size.z = 2f;

            int smokeCount = Mathf.Max(1, Mathf.RoundToInt((lod < 0.5f ? 22 : 12) * quality));
            SmokeRing(kind, firstContact, right, forward, now + 0.23f, Mathf.RoundToInt(smokeCount * 1.18f), 2.6f, 15f, 1.10f, ref rng);
            SmokeRing(kind, secondContact, right, forward, now + 3.44f, smokeCount, 2.97f, 38.7f, 1.10f, ref rng,
                drag: 6f, spawnJitter: 0.04f, speedJitter: 0.15f, radiusJitter: 0.4f, depth: 0.85f);
            int shards = Mathf.Max(1, Mathf.RoundToInt((lod < 0.5f ? 36 : 12) * quality));
            for (int i = 0; i < shards; i++)
            {
                float start = rng.Range(0.18f, BeamSeconds - 0.2f);
                float a = rng.Range(0f, Mathf.PI * 2f);
                Vector3 radial = right * Mathf.Cos(a) + forward * Mathf.Sin(a);
                Vector3 at = centre + SweepOffset(start, right);
                at.y = Ground(at) + 0.18f;
                var rec = Record(at, right,
                    now + start, rng.Range(0.5f, 1.1f), OrbitalLaserKind.Rock,
                    rng.Range(0.10f, 0.28f), 0.08f, 1f, Rock, 1f, 1f, ref rng);
                rec.Vel = radial * rng.Range(3f, 8f) + Vector3.up * rng.Range(3.5f, 7.5f);
                rec.Motion = new Vector4(10f, 0.15f, rng.Range(2f, 7f), 0f);
                renderer.Spawn(debris, rec);
            }
            int sparkCount = Mathf.Max(1, Mathf.RoundToInt((lod < 0.5f ? 260 : 55) * quality));
            for (int i = 0; i < sparkCount; i++)
            {
                float start = rng.Range(0.15f, BeamSeconds + 0.5f);
                Vector3 at = centre + SweepOffset(start, right);
                at.y = Ground(at) + 0.35f;
                var rec = Record(at, right,
                    now + start, rng.Range(0.35f, 0.80f), 0f, rng.Range(0.056f, 0.105f), 0.005f,
                    0.07f, i % 5 == 0 ? Energy : CelPalette.Spark, 6.0f, 0.9f, ref rng);
                Vector3 d = rng.OnSphere(); d.y = Mathf.Abs(d.y) + 0.35f;
                rec.Vel = d.normalized * rng.Range(8f, 15f);
                float lateSpark = Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(3.05f, 3.40f, start));
                rec.Vel *= Mathf.Lerp(1f, 0.30f, lateSpark);
                rec.Size.w *= Mathf.Lerp(1f, 0.45f, lateSpark);
                rec.Size.x *= Mathf.Lerp(1f, 0.60f, lateSpark);
                rec.Life *= Mathf.Lerp(1f, 0.70f, lateSpark);
                rec.Motion = new Vector4(5f, 0.3f, 0f, 0f);
                rec.Erode = new Vector4(0.05f, 0.8f, 2f, 0.025f);
                rec.Extra = new Vector4(0f, 0.06f, 0.2f, 0f);
                renderer.Spawn(CelKinds.Streak, rec);
            }
            var ejectRng = new CelRng(seed ^ 0xD17E21A3u);
            int sheets = Mathf.Max(1, Mathf.RoundToInt((lod < 0.5f ? 140 : 36) * quality));
            for (int i = 0; i < sheets; i++)
            {
                float start = ejectRng.Range(0.18f, 3.30f);
                Vector3 at = centre + SweepOffset(start, right);
                at.y = Ground(at) + 0.18f;
                var rec = Record(at, right, now + start, ejectRng.Range(0.18f, 0.40f),
                    OrbitalLaserKind.Ejecta, ejectRng.Range(0.16f, 0.34f), 0.025f, ejectRng.Range(1.5f, 3.4f),
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
                float size = rimRng.Range(0.14f, 0.28f);
                float front = Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(1.3f, 2.0f, a));
                size *= Mathf.Lerp(0.85f, 0.80f, front) * (i % 3 == 0 ? 0.70f : 1f);
                at.y = centre.y + (at.x - centre.x) * slopeX + (at.z - centre.z) * slopeZ
                    + 0.28f + size * 0.35f;
                var rec = Record(at, right, now + rimRng.Range(3.25f, 3.50f), ScorchSeconds,
                    OrbitalLaserKind.Rock, size, size, 1f, RimStone, Mathf.Lerp(1.0f, 0.60f, front), 1f, ref rimRng);
                rec.Extra.z = 1f;
                renderer.Spawn(debris, rec);
            }
            int flakes = Mathf.Max(1, Mathf.RoundToInt((lod < 0.5f ? 10 : 4) * quality));
            for (int i = 0; i < flakes; i++)
            {
                float start = rimRng.Range(3.36f, 3.52f);
                Vector3 at = centre + SweepOffset(start, right);
                at.y = Ground(at) + rimRng.Range(0.35f, 0.70f);
                var rec = Record(at, right, now + start, rimRng.Range(0.20f, 0.38f), OrbitalLaserKind.Rock,
                    rimRng.Range(0.16f, 0.31f), 0.02f, 1f, Energy, 1f, 1f, ref rimRng);
                rec.Life = Mathf.Min(rec.Life, 3.64f - start);
                rec.Vel = -forward * rimRng.Range(5f, 8f) + right * rimRng.Range(0.3f, 1.4f);
                rec.Motion = new Vector4(1f, 0.2f, rimRng.Range(-3f, 3f), 0f);
                rec.Extra.z = -1f;
                renderer.Spawn(debris, rec);
            }
            // A separate stream preserves the established shard and spark records.
            var smokeRng = new CelRng(seed ^ 0x51A7E2D3u);
            Spawn(kind, firstContact, right, now + 0.23f, 1.10f, OrbitalLaserKind.SmokeRibbon,
                2.9f, 15f, 1f, Smoke, 1f, 0.6f, ref smokeRng);
            Spawn(kind, secondContact, right, now + 3.48f, 0.95f, OrbitalLaserKind.SmokeRibbon,
                2.79f, 40.95f, 1f, Smoke, 1f, 0.35f, ref smokeRng, phase: 1f, drag: 6f);
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
                    0f, emberRng.Range(0.025f, 0.060f), 0.003f, 0.02f, CelPalette.Ember, 6f, 0.90f, ref emberRng);
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
            float spawnJitter = 0.12f, float speedJitter = 0.28f, float radiusJitter = 0.8f, float depth = 1f)
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
                    rng.Range(0.55f, 1.20f), rng.Range(1.0f, 1.9f), rng.Range(0.7f, 1.35f),
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
