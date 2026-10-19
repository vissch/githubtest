// Phase: deaths (2026-10-19, implemented) — part of CombatFx: a splash where a man dies in water, on every path a
// death can take (a plain fall, a flung body, a gag's bounce, a gag with no flight at all). OnDeath works out where
// and when he first touches the water and queues one splash; this ticks the queue and draws it, so a delayed throw's
// ring and droplets land WITH the body, not when the event fires.
using UnityEngine;
using TW.Presentation.Units;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        public const float SplashLife = 0.7f, SplashSize = 1.5f, SplashRing = 1.3f, SplashFlown = 1.4f;
        public const float SplashReach = 160f;
        public const int MaxWaterSplashes = 16;

        struct WaterSplash { public Vector3 At; public float Due, Seed; public bool Flown; }
        readonly System.Collections.Generic.List<WaterSplash> waterSplashes = new System.Collections.Generic.List<WaterSplash>(MaxWaterSplashes);

        /// <summary>Where he first meets the water, and when (seconds after he died): the plain fall, the first landing of a
        /// gag's flight (later arcs are dry ground, so only the first counts), a skid's start, or where he fell with no
        /// flight at all.</summary>
        public static Vector3 DeathLanding(in FallenFlight.Plan path, bool planned, Vector3 fell, out float delay)
        {
            if (!planned) { delay = 0f; return fell; }
            if (path.Arcs > 0) { delay = path.Delay + path.A0.End; return path.A0.To; }
            if (path.SkidDur > 0f) { delay = path.Delay + path.SkidStart; return path.SkidFrom; }
            delay = path.Delay;
            return path.Origin;
        }

        /// <summary>Wet at a point: either notion of water counts, the hook (which also sees painted puddles) or the map's
        /// table (which is what FallenFlight read when it planned the path).</summary>
        public static bool Wet(float groundY, float waterLevel, bool hook) => hook || (waterLevel > TW.Sim.Terrain.MapData.NoWater && groundY < waterLevel - 0.05f);

        void WaterDeath(Vector3 at, float delay, bool flown)
        {
            if (waterSplashes.Count >= MaxWaterSplashes) waterSplashes.RemoveAt(0);
            waterSplashes.Add(new WaterSplash { At = at, Due = Time.time + delay, Seed = Hash01(at.x, at.z, 4), Flown = flown });
        }

        void TickWaterSplashes(float now)
        {
            for (int i = waterSplashes.Count - 1; i >= 0; i--)
            {
                var s = waterSplashes[i];
                if (now < s.Due) continue;
                waterSplashes.RemoveAt(i);
                if (Host == null || Host.Local == null) continue;
                float ringSize = SplashRing * (s.Flown ? SplashFlown : 1f);
                SceneHooks.AddRing?.Invoke(s.At.x, s.At.z, ringSize);
                if (books == null || !books.Ready) continue;
                var p = s.At;
                p.y = Mathf.Max(RenderGround.Sample(Host.Local.Map, p.x, p.z), Host.Local.Map.WaterLevel);
                if (!Near(p, SplashReach)) continue;
                bool mirror = s.Seed < 0.5f;
                float scale = (s.Flown ? SplashFlown : 1f) * FigureScale();
                books.Add(FlipbookFx.Book.Splash, p, SplashSize * scale, SplashLife, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored | (mirror ? FlipbookFx.Kind.Mirror : 0), grow: 0.5f, alpha: 0.9f, pop: 0.15f);
                Throw(p + Vector3.up * 0.3f, 7, 4, 6f, 0.07f);
            }
        }
    }
}
