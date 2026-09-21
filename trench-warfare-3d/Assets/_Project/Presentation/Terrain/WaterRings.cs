// Phase: B2 (implemented) — rings on the water: small ones round men wading a ford or a puddle, a wide one where a
// shell bursts in the river. Sixteen rings at most, kept in one shader array (_TWRings, read by TWWater.hlsl from both
// TW/Water and the ground's painted puddles); nothing is drawn, spawned or pooled. A ring that has run its life costs
// the shader nothing visible and its slot is simply reused, oldest first.
using UnityEngine;
using TW.Presentation;
using TW.Sim;

namespace TW.Presentation.Terrain
{
    public sealed class WaterRings : MonoBehaviour
    {
        public SimHost Host;
        /// <summary>Is there standing water at this point? GreyboxTerrainView answers (water table or a painted puddle).</summary>
        public System.Func<float, float, bool> IsWater;
        const int Count = 16, SlotsPerStep = 3;
        readonly Vector4[] rings = new Vector4[Count];
        int next, cursor;
        float nextStep;
        bool subscribed;
        static readonly int RingsId = Shader.PropertyToID("_TWRings"), NowId = Shader.PropertyToID("_TWNow");

        void Awake()
        {
            for (int i = 0; i < Count; i++) rings[i] = new Vector4(0f, 0f, -1000f, 0f);
        }

        void OnDestroy()
        {
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
            SceneHooks.IsWater = null; SceneHooks.AddRing = null;
            Awake(); Shader.SetGlobalVectorArray(RingsId, rings);
        }

        public void Add(float x, float z, float size)
        {
            rings[next] = new Vector4(x, z, Time.time, size);
            next = (next + 1) % Count;
        }

        void OnSimEvent(SimEvent e)
        {
            if (e.Type != SimEventType.Explosion || IsWater == null || !IsWater(e.Pos.x, e.Pos.z)) return;
            Add(e.Pos.x, e.Pos.z, Mathf.Clamp(e.Scalar * 1.1f, 3f, 8f));
        }

        void Update()
        {
            if (Host == null || Host.Local == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; SceneHooks.IsWater = IsWater; SceneHooks.AddRing = Add; }
            if (Time.time >= nextStep && IsWater != null)
            {
                // a few wading men a step, taken in turn round the unit slots so a whole column ripples, not its first three
                nextStep = Time.time + .22f;
                var w = Host.Local.World;
                int found = 0;
                for (int n = 0; n < w.HighWater && n < 400 && found < SlotsPerStep; n++)   // bounded: 3,000 men on dry land must not cost 3,000 lookups
                {
                    int i = (cursor + n) % w.HighWater;
                    if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0) continue;
                    var v = w.Velocity[i];
                    if (v.x * v.x + v.z * v.z < .09f) continue;
                    var p = w.Position[i];
                    if (!IsWater(p.x, p.z)) continue;
                    Add(p.x, p.z, (w.Flags[i] & (uint)UnitFlags.Vehicle) != 0 ? 3.2f : 1.15f);
                    found++; cursor = i + 1;
                }
                if (found < SlotsPerStep) cursor = w.HighWater > 0 ? (cursor + 400) % w.HighWater : 0;
            }
            Shader.SetGlobalVectorArray(RingsId, rings);
            Shader.SetGlobalFloat(NowId, Time.time);
        }
    }
}
