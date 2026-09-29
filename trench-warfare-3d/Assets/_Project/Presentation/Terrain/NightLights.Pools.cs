// Phase: A5d (2026-09-29, the owner's night look: decisions.md) — depends on: Knobs, TWLightPools.hlsl.
// Warm light pools: the ground under every lantern, prop lamp, trench lamp, torch and fire lit in a painted pool, as the
// owner's effects edit lights a dozen torches' patches of mud. The real lights stop at eight an object (Forward), so each
// camera, as it begins to render, hands the nearest MaxPools of the flames in `lanterns` (all of them hang there, with
// their flicker) to the Toon shader as a global array (_TWPools, _TWPoolTint, _TWPoolCount). Chosen per camera, not in
// Update: the capture rig poses the camera after Update, and the pools then gathered round the spot it had left (Play,
// 2026-09-29). Behind knobs:
//   look.pools      how strong the pools are; 0 (the default) sets no pool at all: today's picture. At 1 a lantern's pool
//                   is PoolGain times its light over the mud: the mud is near black when wet, and at a gain of 1 the pools
//                   barely showed (Play, 2026-09-29: warm pixels 0.03 to 0.08 %); 4 reads as the edit's torch patches, 8
//                   flattens them into yellow discs
//   look.poolReach  a lantern's pool across its radius, in metres (fires and torches reach further by their range)
using UnityEngine;
using UnityEngine.Rendering;

namespace TW.Presentation.Terrain
{
    public sealed partial class NightLights
    {
        public const int MaxPools = 32;   // TW_MAX_POOLS in TWLightPools.hlsl
        public const float PoolReach = 6.5f, PoolGain = 4f;
        /// <summary>What a pool does to its flame's colour: deeper toward orange. The lantern's own colour on the mud read
        /// as pale sand (Play, 2026-09-29); the owner's edits light the mud orange.</summary>
        public static readonly Vector3 PoolWarmth = new Vector3(1f, 0.74f, 0.5f);
        static readonly int PoolsId = Shader.PropertyToID("_TWPools"), PoolTintId = Shader.PropertyToID("_TWPoolTint"), PoolCountId = Shader.PropertyToID("_TWPoolCount");
        readonly Vector4[] poolAt = new Vector4[MaxPools], poolTint = new Vector4[MaxPools];
        readonly float[] poolD = new float[MaxPools];
        float poolStrength, poolReach = PoolReach; int poolKnobs = -1; bool poolsSet, poolHooked;

        /// <summary>Reads the pool knobs; with pools on, each camera picks its own nearest flames as it begins to render.</summary>
        void PushPools()
        {
            if (!poolHooked) { RenderPipelineManager.beginCameraRendering += PoolsFor; poolHooked = true; }
            if (poolKnobs != Knobs.Generation)
            {
                poolKnobs = Knobs.Generation;
                poolStrength = Mathf.Max(0f, Knobs.Get("look.pools", 0f));
                poolReach = Mathf.Clamp(Knobs.Get("look.poolReach", PoolReach), 1f, 20f);
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
                Vector3 p = l.transform.position;
                float d = (p - eye).sqrMagnitude;
                int at = n < MaxPools ? n++ : MaxPools;       // keep the nearest MaxPools, farthest last
                if (at == MaxPools) { if (d >= poolD[MaxPools - 1]) continue; at = MaxPools - 1; }
                while (at > 0 && poolD[at - 1] > d) { poolD[at] = poolD[at - 1]; poolAt[at] = poolAt[at - 1]; poolTint[at] = poolTint[at - 1]; at--; }
                float reach = poolReach * Mathf.Clamp(l.range / LanternRange, 0.8f, 1.3f);
                float strength = poolStrength * PoolGain * l.intensity / Mathf.Max(0.01f, LanternIntensity);
                poolD[at] = d; poolAt[at] = new Vector4(p.x, p.y, p.z, reach);
                poolTint[at] = new Vector4(l.color.r * PoolWarmth.x * strength, l.color.g * PoolWarmth.y * strength, l.color.b * PoolWarmth.z * strength, 0f);
            }
            Shader.SetGlobalVectorArray(PoolsId, poolAt);
            Shader.SetGlobalVectorArray(PoolTintId, poolTint);
            Shader.SetGlobalFloat(PoolCountId, n);
            poolsSet = true;
        }

        /// <summary>No pools once the lights are gone.</summary>
        void ClearPools()
        {
            if (poolHooked) { RenderPipelineManager.beginCameraRendering -= PoolsFor; poolHooked = false; }
            Shader.SetGlobalFloat(PoolCountId, 0f);
        }
    }
}
