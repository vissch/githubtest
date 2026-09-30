// Phase: A5d (2026-09-29, the owner's night look: decisions.md) — depends on: Knobs, TWLightPools.hlsl.
// Warm light pools: the ground under every lantern, prop lamp, trench lamp, torch and fire lit in a painted pool, as the
// owner's effects edit lights a dozen torches' patches of mud. The real lights stop at eight an object (Forward), so each
// camera, as it begins to render, hands the nearest MaxPools of the flames in `lanterns` (all of them hang there, with
// their flicker) to the Toon shader as a global array (_TWPools, _TWPoolTint, _TWPoolCount). Chosen per camera, not in
// Update: the capture rig poses the camera after Update, and the pools then gathered round the spot it had left (Play,
// 2026-09-29). Behind knobs:
//   look.pools      how strong the pools are (1 by default since the owner's word, 2026-09-29); 0 sets no pool: the old night. At 1 a lantern's pool
//                   is PoolGain times its light over the mud: the mud is near black when wet, and at a gain of 1 the pools
//                   barely showed (Play, 2026-09-29: warm pixels 0.03 to 0.08 %); 4 reads as the edit's torch patches, 8
//                   flattens them into yellow discs
//   look.poolReach  a lantern's pool across its radius, in metres (fires and torches reach further by their range)
//   look.firePools  how strong the pools of the fires that come and go are (1): a burning machine or wreck, a flamethrower's
//                   fires. Before, only the lamps built with the scene had pools, and a wreck burning for a minute lit
//                   nothing round it (the owner's colour edit lights the mud round every burning wreck); 0 leaves them out
//   look.poolSoft   the bands toward a soft falloff (1; round 5's critic: "cut-out discs"). Soft alone spread the light
//                   thinner (warm pixels 1.9 -> 1.4 % at pose c), so it waited at 0.35 until round 13's critic asked for a
//                   hot core fading to the edge: fully soft with the lamps' gain 4 -> 6.4 lifts warm pixels (pose a 2.8 ->
//                   3.5 %, c 2.2 -> 2.6 %) and the pool falls off from the lamp instead of lying flat
//   look.poolShoulder  a roll-off for bright sums (0: no fire pool clipped, 0.00 % of pixels over 0.95 at zoom 14)
//   look.poolsThroughHaze  the share of a pool's light the Toon shader adds after the fog (1): with the haze lifted
//                   the distance's lamps went out in it (warm pixels 2.8 to 1.6 %, 2026-09-30); 0 fogs them as before
// Fires reach SceneHooks.FirePool; a big fire counts as nearer than a lamp at the same distance, by its reach squared
// against a lantern's, so the pools a camera keeps are the ones that light the most mud it sees.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;

namespace TW.Presentation.Terrain
{
    public sealed partial class NightLights
    {
        public const int MaxPools = 32;   // TW_MAX_POOLS in TWLightPools.hlsl
        public const float PoolReach = 6.5f, PoolGain = 5.2f, FirePoolGain = 3.2f;   // fires 4 -> 3.2 in round 17 (their pools clipped near wrecks)   // lamps 5.2 (6.4 in round 13 went white-hot: critique round 15); fires kept at 4, they already flood
        /// <summary>On by default since the owner's word (2026-09-29); look.pools 0 sets no pool: the old night.</summary>
        public const float DefaultPools = 1f;
        /// <summary>look.poolsThroughHaze: the share of a pool's light the Toon shader adds after the fog (1), so the
        /// lifted haze does not put out the lamps in the distance; 0 fogs the pools with the ground as before.</summary>
        public const float DefaultThroughHaze = 1f;
        /// <summary>What a pool does to its flame's colour: deeper toward orange. The lantern's own colour on the mud read
        /// as pale sand (Play, 2026-09-29); the owner's edits light the mud orange.</summary>
        public static readonly Vector3 PoolWarmth = new Vector3(1f, 0.74f, 0.5f);
        /// <summary>look.poolAmber (1; 0 PoolWarmth alone): the lamps' pools deeper amber (round 17's critic: "flat cream
        /// stamps"), and each lamp's reach varied by look.poolVary (+-30 %, seeded by where it hangs) so no two pools match.</summary>
        public static readonly Vector3 PoolAmber = new Vector3(1f, 0.64f, 0.34f);
        public const float DefaultPoolAmber = 1f, DefaultPoolVary = 0.3f;
        float poolAmber = DefaultPoolAmber, poolVary = DefaultPoolVary;
        static readonly int PropRimId = Shader.PropertyToID("_TWPropRim"), UnblueId = Shader.PropertyToID("_TWPoolUnblue");
        public const float DefaultPoolUnblue = 0.6f;   // 1.2 tinged the lamp-lit sandbags acid yellow; 0.6 keeps the edge amber
        public const float DefaultPropRim = 0.18f;   // 0.35 blew a concrete slab by a fire to yellow (p995 0.79 -> 0.90); 0.18 keeps it orange
        static readonly int ThroughHazeId = Shader.PropertyToID("_TWPoolsThroughHaze"), PoolSoftId = Shader.PropertyToID("_TWPoolSoft"), PoolShoulderId = Shader.PropertyToID("_TWPoolShoulder");
        /// <summary>look.poolSoft: the bands toward a soft falloff; look.poolShoulder: the roll-off that keeps a fire's pool
        /// from clipping (critique round 5: "cut-out discs", fire pools clipping).</summary>
        public const float DefaultPoolSoft = 1f, DefaultPoolShoulder = 0f;
        static readonly int PoolsId = Shader.PropertyToID("_TWPools"), PoolTintId = Shader.PropertyToID("_TWPoolTint"), PoolCountId = Shader.PropertyToID("_TWPoolCount");
        readonly Vector4[] poolAt = new Vector4[MaxPools], poolTint = new Vector4[MaxPools];
        readonly float[] poolD = new float[MaxPools];
        float poolStrength, poolReach = PoolReach, firePoolStrength = 1f; int poolKnobs = -1; bool poolsSet, poolHooked;

        /// <summary>A fire's pool: where, its colour times its strength, its reach, and when it goes out (a frame stamp for a
        /// pool renewed every frame, else a time).</summary>
        struct FirePoolEntry { public Vector3 At; public Vector3 Tint; public float Reach, Until; public int Frame; }
        public const int MaxFirePools = 64;
        readonly List<FirePoolEntry> firePools = new List<FirePoolEntry>(MaxFirePools);
        /// <summary>Fire pools alive now (for tests and captures).</summary>
        public int FirePools => firePools.Count;

        /// <summary>SceneHooks.FirePool: a fire's pool, held for this frame (life 0) or for life seconds.</summary>
        public void AddFirePool(Vector3 at, Color color, float strength, float reach, float life)
        {
            if (strength <= 0f || reach <= 0f || firePools.Count >= MaxFirePools) return;
            float k = Mathf.Min(strength, 4f);
            firePools.Add(new FirePoolEntry
            {
                At = at, Reach = Mathf.Min(reach, 20f),
                Tint = new Vector3(color.r * PoolWarmth.x * k, color.g * PoolWarmth.y * k, color.b * PoolWarmth.z * k),
                Frame = life <= 0f ? Time.frameCount : -1, Until = Time.time + life,
            });
        }

        /// <summary>Fire pools that have gone out: last frame's, and the timed ones past their time.</summary>
        void ExpireFirePools()
        {
            int f = Time.frameCount; float t = Time.time;
            for (int i = firePools.Count - 1; i >= 0; i--)
            {
                var e = firePools[i];
                if (e.Frame >= 0 ? e.Frame < f : e.Until < t) firePools.RemoveAt(i);
            }
        }

        /// <summary>Reads the pool knobs; with pools on, each camera picks its own nearest flames as it begins to render.</summary>
        void PushPools()
        {
            if (!poolHooked)
            {
                RenderPipelineManager.beginCameraRendering += PoolsFor; poolHooked = true;
                SceneHooks.FirePool = AddFirePool;
            }
            ExpireFirePools();
            if (poolKnobs != Knobs.Generation)
            {
                poolKnobs = Knobs.Generation;
                poolStrength = Mathf.Max(0f, Knobs.Get("look.pools", DefaultPools));
                poolReach = Mathf.Clamp(Knobs.Get("look.poolReach", PoolReach), 1f, 20f);
                firePoolStrength = Mathf.Max(0f, Knobs.Get("look.firePools", 1f));
                poolAmber = Mathf.Clamp01(Knobs.Get("look.poolAmber", DefaultPoolAmber));
                poolVary = Mathf.Clamp(Knobs.Get("look.poolVary", DefaultPoolVary), 0f, 0.6f);
                Shader.SetGlobalFloat(ThroughHazeId, Mathf.Clamp01(Knobs.Get("look.poolsThroughHaze", DefaultThroughHaze)));
                Shader.SetGlobalFloat(UnblueId, Mathf.Clamp(Knobs.Get("look.poolUnblue", DefaultPoolUnblue), 0f, 2f));
                Shader.SetGlobalFloat(PropRimId, Mathf.Clamp(Knobs.Get("look.propRim", DefaultPropRim), 0f, 2f));
                Shader.SetGlobalFloat(PoolSoftId, Mathf.Clamp01(Knobs.Get("look.poolSoft", DefaultPoolSoft)));
                Shader.SetGlobalFloat(PoolShoulderId, Mathf.Clamp(Knobs.Get("look.poolShoulder", DefaultPoolShoulder), 0f, 4f));
            }
            if (poolStrength <= 0f || !SceneMood.Night)
            {
                if (poolsSet) { Shader.SetGlobalFloat(PoolCountId, 0f); poolsSet = false; }
                return;
            }
        }

        /// <summary>The nearest flames to this camera, as pools.</summary>
        void PoolsFor(ScriptableRenderContext context, Camera cam)
        {
            if (poolStrength <= 0f || !SceneMood.Night || cam.cameraType != CameraType.Game) return;
            Vector3 eye = cam.transform.position;
            int n = 0;
            for (int i = 0; i < lanterns.Count; i++)
            {
                var l = lanterns[i];
                if (l == null || !l.enabled) continue;
                Vector3 at = l.transform.position;
                float vary = 1f + poolVary * (2f * Hash(Mathf.RoundToInt(at.x * 3f), Mathf.RoundToInt(at.z * 3f)) - 1f);
                float reach = poolReach * Mathf.Clamp(l.range / LanternRange, 0.8f, 1.3f) * vary;
                float strength = poolStrength * PoolGain * l.intensity / Mathf.Max(0.01f, LanternIntensity);
                Vector3 warmth = Vector3.Lerp(PoolWarmth, PoolAmber, poolAmber);
                Keep(ref n, eye, at, reach, new Vector3(l.color.r * warmth.x, l.color.g * warmth.y, l.color.b * warmth.z) * strength);
            }
            float fireGain = poolStrength * FirePoolGain * firePoolStrength;
            if (fireGain > 0f)
                for (int i = 0; i < firePools.Count; i++) { var e = firePools[i]; Keep(ref n, eye, e.At, e.Reach, e.Tint * fireGain); }
            Shader.SetGlobalVectorArray(PoolsId, poolAt);
            Shader.SetGlobalVectorArray(PoolTintId, poolTint);
            Shader.SetGlobalFloat(PoolCountId, n);
            poolsSet = true;
        }

        /// <summary>How a pool ranks for a camera, lower first: its distance squared, over its reach squared against a
        /// lantern's once it reaches further than one.</summary>
        public static float PoolRank(float sqrDist, float reach) { float big = reach / PoolReach; return sqrDist / Mathf.Max(1f, big * big); }

        /// <summary>Keeps a pool among the MaxPools a camera sees best: nearest first, a bigger one counting as nearer by its
        /// reach squared against a lantern's (a burning wreck 12 m across wins over a lamp at the same distance).</summary>
        void Keep(ref int n, Vector3 eye, Vector3 p, float reach, Vector3 tint)
        {
            float d = PoolRank((p - eye).sqrMagnitude, reach);
            int at = n < MaxPools ? n++ : MaxPools;       // keep the best MaxPools, worst last
            if (at == MaxPools) { if (d >= poolD[MaxPools - 1]) return; at = MaxPools - 1; }
            while (at > 0 && poolD[at - 1] > d) { poolD[at] = poolD[at - 1]; poolAt[at] = poolAt[at - 1]; poolTint[at] = poolTint[at - 1]; at--; }
            poolD[at] = d; poolAt[at] = new Vector4(p.x, p.y, p.z, reach); poolTint[at] = new Vector4(tint.x, tint.y, tint.z, 0f);
        }

        /// <summary>look.horizonFires (1; 0 the old): the fires on the horizon read as pale slivers floating in the lifted
        /// haze (critique round 5), where the owner's edit has far fires as small hot points punching through it. Each gets
        /// a small orange core standing on its glow, and the wide glow goes deeper orange and fades to a third. Read once, as the lamps are built.</summary>
        static bool HorizonFiresLook => Knobs.Get("look.horizonFires", 1f) > 0f;

        static void HorizonCore(Vector3 p, float a, float b, int k, List<Vector3> centres, List<Vector4> shapes, List<Color> colors)
        {
            if (!HorizonFiresLook) return;
            centres.Add(p + Vector3.up * (0.6f + 0.5f * b));
            shapes.Add(new Vector4(1.1f + 0.6f * a, .9f, k * .31f + .5f, .9f));
            colors.Add(new Color(1f, .5f, .16f, .38f + .12f * b));   // at .75-.95 the core blew out to white
        }

        /// <summary>look.warmFlare (1; 0 the old): the star shell lights the field neutral white instead of cold blue, and its
        /// glow card is smaller (5 m, was 9) and warm, so it reads as a burning flare, not a blue orb. Read once, at Start.</summary>
        static bool WarmFlareLook => Knobs.Get("look.warmFlare", 1f) > 0f;
        public static readonly Color FlareNeutral = new Color(1f, 0.96f, 0.88f), FlareGlowWarm = new Color(1f, 0.82f, 0.55f);

        /// <summary>look.moreFires (6; 0 none): burning trees in no man's land past the MaxFires that carry a real light,
        /// each with its flames, its glow card and a painted pool, but no light: the renderer takes eight an object, and the
        /// pool is what shows. Round 7's critic: warm light too sparse in an ordinary view (0.7 % against the edit's 2.4 %).</summary>
        public const int DefaultMoreFires = 10;   // 6 until round 14 (pose b still 0.8 % warm)

        void MoreFires(TW.Sim.Terrain.MapData map, float len, List<Vector3> centres, List<Vector4> shapes, List<Color> colors, List<Vector3> flameFeet, List<Vector4> flameShapes)
        {
            int want = Mathf.Clamp((int)Knobs.Get("look.moreFires", (float)DefaultMoreFires), 0, 24), made = 0;
            for (int i = 0; i < map.Props.Length && made < want; i++)
            {
                var prop = map.Props[i];
                if (prop.Kind != TW.Sim.Terrain.PropKind.BrokenTree && prop.Kind != TW.Sim.Terrain.PropKind.Stump) continue;
                float h = Hash(i, 61);
                if (prop.Pos.z < len * .2f || prop.Pos.z > len * .8f || h <= .16f || h > .42f) continue;   // not the lit ones (<= .16)
                bool stump = prop.Kind == TW.Sim.Terrain.PropKind.Stump;
                Vector3 at = new Vector3(prop.Pos.x, RenderGround.Sample(map, prop.Pos.x, prop.Pos.z) + (stump ? .5f : 1.3f), prop.Pos.z);
                centres.Add(at); shapes.Add(new Vector4(2.6f, .6f, i * .173f, .4f)); colors.Add(new Color(1f, .45f, .12f, .75f));
                flameFeet.Add(at - Vector3.up * .15f); flameShapes.Add(new Vector4(.95f, 1.5f, Hash(i, 97), 0f));
                AddFirePool(at + Vector3.up * .4f, Burst, 0.9f, 9f, 1e7f);   // held for the scene's life
                made++;
            }
        }

        /// <summary>No pools once the lights are gone.</summary>
        void ClearPools()
        {
            if (poolHooked) { RenderPipelineManager.beginCameraRendering -= PoolsFor; poolHooked = false; }
            SceneHooks.FirePool = null;
            Shader.SetGlobalFloat(ThroughHazeId, 0f); Shader.SetGlobalFloat(PoolSoftId, 0f); Shader.SetGlobalFloat(PropRimId, 0f); Shader.SetGlobalFloat(UnblueId, 0f); Shader.SetGlobalFloat(PoolShoulderId, 0f);
            firePools.Clear();
            Shader.SetGlobalFloat(PoolCountId, 0f);
        }
    }
}
