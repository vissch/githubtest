// Phase: B2 (implemented) — fog that follows the fighting. Inside the battlefield a thin, low fog lies over ground
// where nothing is happening and lifts where the player's men are, round their HQ, and for a while where a shell has
// burst. Presentation only: it hides nothing from the sim, selection or the HUD, and men in it are hazed, not removed.
// How: a presence map, one texel per 4 m (23 x 60 on the standard field). Four times a second the player's living men
// are binned into it and each occupied cell stamps a soft disc; the map then eases toward that target (it lifts in
// about 2 s and rolls back in over about 15 s), and the shaders read it through TWAtmosphere.hlsl (_TWPresence).
// Cost: binning is one pass over the unit slots; stamping is per occupied cell, not per man, so 3,000 men in a few
// hundred cells cost the same as 300.
using System.Collections.Generic;
using UnityEngine;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed class QuietFog : MonoBehaviour
    {
        public SimHost Host;
        public byte LocalTeam = 0;
        [Range(0f, 1f)] public float Density = 0.5f;
        [Tooltip("Metres above the water table where the quiet fog ends.")]
        public float Top = 5f;
        [Tooltip("How far a man keeps the fog back, metres; a shell burst clears a third of that for BurstSeconds.")]
        public float Reach = 55f, BurstSeconds = 14f;
        public float LiftSeconds = 2f, SettleSeconds = 15f;

        const float Cell = 4f;
        int gw, gl, kernelRadius;
        float[] level, target, kernel;
        bool[] occupied;
        byte[] px;
        Texture2D tex;
        float nextScan;
        bool subscribed;
        struct Burst { public Vector2 Pos; public float Until; }
        readonly List<Burst> bursts = new List<Burst>();
        static readonly int PresenceId = Shader.PropertyToID("_TWPresence"), QuietId = Shader.PropertyToID("_TWQuiet");

        void Start()
        {
            if (Host == null || Host.Local == null) { enabled = false; return; }
            var map = Host.Local.Map;
            gw = Mathf.CeilToInt(map.SizeMeters.x / Cell); gl = Mathf.CeilToInt(map.SizeMeters.y / Cell);
            level = new float[gw * gl]; target = new float[gw * gl]; occupied = new bool[gw * gl]; px = new byte[gw * gl];
            kernelRadius = Mathf.CeilToInt(Reach / Cell);
            int side = kernelRadius * 2 + 1;
            kernel = new float[side * side];
            for (int z = 0; z < side; z++)
            for (int x = 0; x < side; x++)
            {
                float d = Mathf.Sqrt((x - kernelRadius) * (x - kernelRadius) + (z - kernelRadius) * (z - kernelRadius)) * Cell / Reach;
                kernel[z * side + x] = 1f - Mathf.SmoothStep(0f, 1f, (d - .5f) / .5f);   // clear to half the reach, then fading
            }
            tex = new Texture2D(gw, gl, TextureFormat.R8, false) { name = "Presence", wrapMode = TextureWrapMode.Clamp, filterMode = FilterMode.Bilinear, hideFlags = HideFlags.HideAndDontSave };
            Scan(); System.Array.Copy(target, level, level.Length);   // the opening view is already settled
            Upload();
        }

        void OnDestroy()
        {
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
            Shader.SetGlobalVector(QuietId, Vector4.zero);
            if (tex != null) Destroy(tex);
        }

        void OnSimEvent(SimEvent e)
        {
            if (e.Type != SimEventType.Explosion) return;
            if (bursts.Count >= 48) bursts.RemoveAt(0);
            bursts.Add(new Burst { Pos = new Vector2(e.Pos.x, e.Pos.z), Until = Time.time + BurstSeconds });
        }

        void Stamp(float wx, float wz, float scale, float strength)
        {
            int cx = Mathf.Clamp((int)(wx / Cell), 0, gw - 1), cz = Mathf.Clamp((int)(wz / Cell), 0, gl - 1);
            int side = kernelRadius * 2 + 1, r = Mathf.Max(1, Mathf.CeilToInt(kernelRadius * scale));
            for (int dz = -r; dz <= r; dz++)
            {
                int z = cz + dz; if (z < 0 || z >= gl) continue;
                for (int dx = -r; dx <= r; dx++)
                {
                    int x = cx + dx; if (x < 0 || x >= gw) continue;
                    // a smaller disc reads the same kernel, stretched
                    int kx = kernelRadius + Mathf.RoundToInt(dx / scale), kz = kernelRadius + Mathf.RoundToInt(dz / scale);
                    if (kx < 0 || kz < 0 || kx >= side || kz >= side) continue;
                    float v = kernel[kz * side + kx] * strength;
                    if (v > target[z * gw + x]) target[z * gw + x] = v;
                }
            }
        }

        void Scan()
        {
            var sim = Host.Local; var w = sim.World; var map = sim.Map;
            System.Array.Clear(target, 0, target.Length); System.Array.Clear(occupied, 0, occupied.Length);
            for (int i = 0; i < w.HighWater; i++)
            {
                if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0 || w.Team[i] != LocalTeam) continue;
                var p = w.Position[i];
                occupied[Mathf.Clamp((int)(p.z / Cell), 0, gl - 1) * gw + Mathf.Clamp((int)(p.x / Cell), 0, gw - 1)] = true;
            }
            for (int z = 0; z < gl; z++)
            for (int x = 0; x < gw; x++)
                if (occupied[z * gw + x]) Stamp((x + .5f) * Cell, (z + .5f) * Cell, 1f, 1f);
            for (int o = 0; o < map.Objectives.Length; o++)   // home is never fogged
            {
                var def = map.Objectives[o];
                if (def.Kind != ObjectiveKind.HQ || def.SideTeam != LocalTeam || def.CellCount == 0) continue;
                int cell = map.ObjectiveCells[def.CellStart + def.CellCount / 2];
                Stamp((cell % map.NavWidth + .5f) * MapData.NavCellSize, (cell / map.NavWidth + .5f) * MapData.NavCellSize, 1.25f, 1f);
            }
            for (int b = bursts.Count - 1; b >= 0; b--)
            {
                float left = (bursts[b].Until - Time.time) / BurstSeconds;
                if (left <= 0f) { bursts.RemoveAt(b); continue; }
                Stamp(bursts[b].Pos.x, bursts[b].Pos.y, .34f, Mathf.Clamp01(left * 2f));
            }
        }

        void Upload()
        {
            for (int i = 0; i < level.Length; i++) px[i] = (byte)(Mathf.Clamp01(level[i]) * 255f);
            tex.SetPixelData(px, 0); tex.Apply(false, false);
        }

        void Update()
        {
            using var perf = TW.Sim.PerfMarkers.FogUpdate.Auto();
            if (Host == null || Host.Local == null || tex == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            if (Time.time >= nextScan) { nextScan = Time.time + .25f; Scan(); }
            float up = Time.deltaTime / Mathf.Max(.1f, LiftSeconds), down = Time.deltaTime / Mathf.Max(.1f, SettleSeconds);
            for (int i = 0; i < level.Length; i++)
                level[i] = target[i] > level[i] ? Mathf.Min(target[i], level[i] + up) : Mathf.Max(target[i], level[i] - down);
            Upload();
            float water = Host.Local.Map.WaterLevel > MapData.NoWater ? Host.Local.Map.WaterLevel : 0f;
            Shader.SetGlobalTexture(PresenceId, tex);
            Shader.SetGlobalVector(QuietId, new Vector4(Density, water + Top, 1f / Mathf.Max(.5f, Top * .6f), 0f));
        }
    }
}
