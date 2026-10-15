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
        public const int Count = 16;
        const int SlotsPerStep = 3;
        readonly Vector4[] rings = new Vector4[Count];
        int next, cursor;
        readonly int[] picks = new int[SlotsPerStep];
        System.Func<int, bool> rippling;
        SimWorld scanWorld;
        float nextStep;
        bool subscribed;
        static readonly int RingsId = Shader.PropertyToID("_TWRings"), NowId = Shader.PropertyToID("_TWNow");

        void Awake()
        {
            for (int i = 0; i < Count; i++) rings[i] = new Vector4(0f, 0f, -1000f, 0f);
            rippling = Rippling;   // built once: the step itself must allocate nothing
        }

        bool Rippling(int i)
        {
            if ((scanWorld.Flags[i] & (uint)UnitFlags.Alive) == 0) return false;
            var v = scanWorld.Velocity[i];
            if (v.x * v.x + v.z * v.z < .09f) return false;
            var p = scanWorld.Position[i];
            return IsWater(p.x, p.z);
        }

        /// <summary>The slots that ripple this step: walks `scan` slots from `cursor` round `high`, keeps the first
        /// `slots` that `ripples` says yes to in `into`, and leaves `next` one past the last one it took (W6: the
        /// cursor used to move inside the walk, which also added n, so the walk skipped slots). Fewer than `slots`
        /// found: `next` catches up past the whole scanned run, as before. Returns how many it found.</summary>
        public static int RipplesThisStep(int high, int scan, int slots, System.Func<int, bool> ripples, int[] into, int cursor, out int next)
        {
            int found = 0, last = -1;
            for (int n = 0; n < high && n < scan && found < slots; n++)   // bounded: 3,000 men on dry land must not cost 3,000 lookups
            {
                int i = ((cursor + n) % high + high) % high;
                if (!ripples(i)) continue;
                into[found++] = i; last = i;
            }
            if (found >= slots && slots > 0) next = last + 1;
            else next = high > 0 ? (((cursor + scan) % high) + high) % high : 0;
            return found;
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
            using var perf = TW.Sim.PerfMarkers.WaterRingsUpdate.Auto();
            if (Host == null || Host.Local == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; SceneHooks.IsWater = IsWater; SceneHooks.AddRing = Add; }
            if (Time.time >= nextStep && IsWater != null)
            {
                // a few wading men a step, taken in turn round the unit slots so a whole column ripples, not its first three
                nextStep = Time.time + .22f;
                scanWorld = Host.Local.World;
                int found = RipplesThisStep(scanWorld.HighWater, 400, SlotsPerStep, rippling, picks, cursor, out cursor);
                for (int s = 0; s < found; s++)
                {
                    int i = picks[s];
                    var p = scanWorld.Position[i];
                    Add(p.x, p.z, (scanWorld.Flags[i] & (uint)UnitFlags.Vehicle) != 0 ? 3.2f : 1.15f);
                }
            }
            Shader.SetGlobalVectorArray(RingsId, rings);
            Shader.SetGlobalFloat(NowId, Time.time);
        }
    }
}
