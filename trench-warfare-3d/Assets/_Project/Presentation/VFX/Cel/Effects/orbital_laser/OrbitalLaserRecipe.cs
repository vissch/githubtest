// Phase: CelFx orbital laser (2026-11-09) - a sweeping beam and two smoke rings leave a lasting crescent scorch.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class OrbitalLaserRecipe
    {
        public const float BeamSeconds = 3.55f, SweepSeconds = 3.36f, ScorchSeconds = float.MaxValue;
        public const float RadiusRight = 3f, RadiusForward = 2.24f;
        public const float SweepFrom = 0.45f, SweepTo = 3.22f;

        static readonly int Energy = CelPalette.Add("orbital_energy",
            CelPalette.Hex(0x2854BC), CelPalette.Hex(0x568FF7), CelPalette.Hex(0x8DE9F5),
            CelPalette.Hex(0xD7FAFD), Color.white, 0.72f, 4, CelPalette.Hex(0x14315B));
        static readonly int Smoke = CelPalette.Add("orbital_smoke",
            CelPalette.Hex(0x5B6166), CelPalette.Hex(0x858A8C), CelPalette.Hex(0xA5ABAD),
            CelPalette.Hex(0xC3CCD1), CelPalette.Hex(0xC3CCD1), 1.1f, 3, CelPalette.Hex(0x5B6166));
        static readonly int Rock = CelPalette.Add("orbital_rock",
            CelPalette.Hex(0x070D1B), CelPalette.Hex(0x17253D), CelPalette.Hex(0x2D4058),
            CelPalette.Hex(0x627486), CelPalette.Hex(0x627486), 1.1f, 4, CelPalette.Hex(0x060B11));

        public static Vector3 SweepOffset(float seconds, Vector3 right)
        {
            float k = Mathf.Clamp01((seconds - 0.2f) / (SweepSeconds - 0.2f));
            float angle = Mathf.Lerp(SweepFrom, SweepTo, Mathf.Pow(k, 0.88f));
            Vector3 forward = Vector3.Cross(right, Vector3.up);
            return -right * (Mathf.Sin(angle) * RadiusRight) + forward * (Mathf.Cos(angle) * RadiusForward);
        }

        public static void Play(Vector3 centre, Vector3 right, uint seed)
        {
            right.y = 0f;
            right = right.sqrMagnitude > 0.001f ? right.normalized : Vector3.right;
            Vector3 forward = Vector3.Cross(right, Vector3.up);
            var renderer = CelFx.Renderer;
            int kind = renderer.KindId(OrbitalLaserKind.KindName);
            if (kind < 0) throw new System.InvalidOperationException("OrbitalLaser kind is not registered");
            float lod = CelFx.LodBlend(CelFx.Zoom);
            float now = CelFx.Now;
            bool far = lod >= 2f;
            var rng = new CelRng(seed);
            centre.y = Ground(centre);
            SceneHooks.FirePool?.Invoke(centre + SweepOffset(1.3f, right), new Color(0.06f, 0.2f, 1f), 0.8f, 9f, SweepSeconds);

            Spawn(kind, centre, right, now + 0.13f, ScorchSeconds, OrbitalLaserKind.Scorch,
                3.7f, 3.7f, 0.75f, Rock, 1f, 1f, ref rng);
            if (!far)
                Spawn(kind, centre, right, now + 0.04f, 3.4f, OrbitalLaserKind.Pool,
                    6.7f, 6.7f, 1.05f, Energy, 1.5f, 0.8f, ref rng, 0.04f);
            Spawn(kind, centre, right, now, BeamSeconds, OrbitalLaserKind.Beam,
                1.55f, 1.3f, 220f, Energy, 3.2f, 1f, ref rng);
            if (far) return;

            Spawn(kind, centre, right, now + 0.04f, 3.32f, OrbitalLaserKind.Impact,
                3.1f, 2.7f, 1f, Energy, 3.4f, 1f, ref rng, 0.04f);
            int spirals = lod < 0.5f ? 2 : 1;
            for (int i = 0; i < spirals; i++)
                Spawn(kind, centre, right, now + 0.22f, BeamSeconds - 0.22f, OrbitalLaserKind.Spiral,
                    0.09f, 0.13f, 12f, Energy, 2.2f, 0.95f, ref rng, 0.22f, i * 2.73f);

            Spawn(kind, centre, right, now + 0.17f, 0.85f, OrbitalLaserKind.Shock,
                1.7f, 10.5f, 1f, Energy, 2f, 0.75f, ref rng);
            Spawn(kind, centre, right, now + 3.35f, 1.10f, OrbitalLaserKind.SmokeRibbon,
                5.5f, 13f, 1f, Smoke, 1f, 0.6f, ref rng);
            Spawn(kind, centre, right, now + 3.46f, 0.23f, OrbitalLaserKind.Impact,
                1.25f, 0.05f, 1f, Energy, 3.6f, 1f, ref rng, 3.46f);

            int smokeCount = lod < 0.5f ? 22 : 12;
            SmokeRing(kind, centre, right, forward, now + 0.18f, smokeCount, 4f, 7.5f, 1.10f, ref rng);
            SmokeRing(kind, centre, right, forward, now + 3.35f, smokeCount, 4.8f, 7.5f, 1.10f, ref rng);
            int shards = lod < 0.5f ? 36 : 12;
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
                renderer.Spawn(kind, rec);
            }
            int sparkCount = lod < 0.5f ? 100 : 28;
            for (int i = 0; i < sparkCount; i++)
            {
                float start = rng.Range(0.15f, BeamSeconds + 0.5f);
                Vector3 at = centre + SweepOffset(start, right);
                at.y = Ground(at) + 0.35f;
                var rec = Record(at, right,
                    now + start, rng.Range(0.18f, 0.5f), 0f, rng.Range(0.018f, 0.04f), 0.005f,
                    0.05f, i % 3 == 0 ? Energy : CelPalette.Spark, 2.5f, 0.9f, ref rng);
                Vector3 d = rng.OnSphere(); d.y = Mathf.Abs(d.y) + 0.35f;
                rec.Vel = d.normalized * rng.Range(2.5f, 6f);
                rec.Motion = new Vector4(5f, 0.3f, 0f, 0f);
                rec.Erode = new Vector4(0.05f, 0.8f, 2f, 0.025f);
                rec.Extra = new Vector4(0f, 0.06f, 0.2f, 0f);
                renderer.Spawn(CelKinds.Streak, rec);
            }
            // A separate stream preserves the established shard and spark records.
            var smokeRng = new CelRng(seed ^ 0x51A7E2D3u);
            Spawn(kind, centre, right, now + 0.18f, 1.10f, OrbitalLaserKind.SmokeRibbon,
                4.5f, 12.5f, 1f, Smoke, 1f, 0.6f, ref smokeRng);
        }

        static void SmokeRing(int kind, Vector3 centre, Vector3 right, Vector3 forward, float born,
            int count, float radius, float speed, float life, ref CelRng rng)
        {
            for (int i = 0; i < count; i++)
            {
                float angle = (i + rng.Range(-0.47f, 0.47f)) * Mathf.PI * 2f / count;
                Vector3 radial = right * Mathf.Cos(angle) + forward * Mathf.Sin(angle);
                Vector3 p = centre + radial * (radius + rng.Range(-0.8f, 0.8f));
                p.y = Ground(p) + 0.15f;
                var r = Record(p, right, born + rng.Range(0f, 0.12f), rng.Jitter(life, 0.2f), OrbitalLaserKind.Puff,
                    rng.Range(0.65f, 1.5f), rng.Range(1.3f, 2.4f), rng.Range(0.7f, 1.35f),
                    Smoke, 1f, 1f, ref rng);
                r.Vel = radial * rng.Jitter(speed, 0.28f) + Vector3.up * rng.Range(0.1f, 0.75f);
                r.Motion = new Vector4(0f, 0.8f, 0f, 0.05f);
                CelFx.Renderer.Spawn(kind, r);
            }
        }

        static void Spawn(int kind, Vector3 centre, Vector3 right, float born, float life, float mode,
            float size, float endSize, float stretch, int palette, float core, float opacity,
            ref CelRng rng, float timeOffset = 0f, float phase = 0f)
        {
            var r = Record(centre, right, born, life, mode, size, endSize, stretch, palette, core, opacity, ref rng);
            r.Extra.y = timeOffset;
            r.Extra.z = phase;
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
