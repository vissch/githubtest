// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — depends on: SceneHooks, MachineLightSlots, Knobs.
// The machines' own lights. Glow cards for their running lamps, hot exhausts and the Maw's furnace ride in the flash-glow
// mesh after the pool's cards (lights.maxMachineGlows of them, 128), zero-sized until TankRenderer lights one, so they
// cost no draw call. And a small pool of real lights of their own (lights.machinePool, 4 by default since the owner
// saw the captures, 2026-09-29; 0 none, as before):
// a machine's gun, its fire, its cook-off, the Maw's furnace, apart from the shared flash pool, so a barrage cannot take a
// burning machine's light. The pool follows its knob while the game runs, so a capture can turn it on and off.
// And a lamp whose lantern post has gone goes out (SceneHooks.LampOut, from PropDestruction).
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public sealed partial class NightLights
    {
        public const int MaxMachineGlows = 128, MaxMachinePool = 8, DefaultMachinePool = 4;
        int machineCards;                                           // card slots after the pool's, fixed at Start
        Vector3[] mPos; Color[] mCol; float[] mSize; int mCount;    // the cards TankRenderer sent last
        MachineLightSlots machineSlots; Light[] machineLights; int machineKnobs = -1;
        Mesh nightGlows;                                            // the lamps' glow cards (Build), for LampOut
        readonly Vector3[] mLightAt = new Vector3[MaxMachinePool];
        readonly Color[] mLightColor = new Color[MaxMachinePool];
        readonly float[] mLightReach = new float[MaxMachinePool];

        void StartMachines()
        {
            machineCards = Mathf.Clamp(Knobs.Get("lights.maxMachineGlows", MaxMachineGlows), 0, 512);
            mPos = new Vector3[machineCards]; mCol = new Color[machineCards]; mSize = new float[machineCards];
            SceneHooks.MachineGlows = (pos, col, size, n) =>
            {
                mCount = Mathf.Clamp(n, 0, machineCards);
                System.Array.Copy(pos, mPos, mCount); System.Array.Copy(col, mCol, mCount); System.Array.Copy(size, mSize, mCount);
            };
            MachineKnobs();
            SceneHooks.LampOut = LampOut;
        }

        /// <summary>A lantern post has gone (PropDestruction): the lamps hung within `reach` of it go out, their light and
        /// their card in the night glows mesh (card j is lanterns[j]). Before, the light burned on over the flattened post.</summary>
        void LampOut(Vector3 at, float reach)
        {
            Color[] cols = null;
            for (int j = 0; j < lanterns.Count && j < lanternHome.Count; j++)
            {
                float dx = lanternHome[j].x - at.x, dz = lanternHome[j].z - at.z;
                if (dx * dx + dz * dz > reach * reach || lampOut[j]) continue;
                lampOut[j] = true; lanterns[j].enabled = false;   // out for good: the real-lamp rule never lights it again
                if (nightGlows == null) continue;
                if (cols == null) cols = nightGlows.colors;
                for (int k = 0; k < 4 && j * 4 + k < cols.Length; k++) cols[j * 4 + k] = Color.clear;
            }
            if (cols != null) nightGlows.colors = cols;
        }

        /// <summary>Sizes the machine pool to its knob (lights.machinePool) whenever a knob changes.</summary>
        void MachineKnobs()
        {
            if (machineKnobs == Knobs.Generation) return;
            machineKnobs = Knobs.Generation;
            int want = Mathf.Clamp(Knobs.Get("lights.machinePool", DefaultMachinePool), 0, MaxMachinePool);
            int have = machineSlots == null ? 0 : machineSlots.Count;
            if (want == have) return;
            if (machineLights != null) foreach (var l in machineLights) if (l != null) Destroy(l.gameObject);
            machineSlots = want > 0 ? new MachineLightSlots(want) : null;
            machineLights = want > 0 ? new Light[want] : null;
            for (int i = 0; i < want; i++) { machineLights[i] = MakeLight("Machine light " + i, Muzzle, 0f, 8f); machineLights[i].enabled = false; }
            if (want > 0) SceneHooks.MachineLight = MachineLight; else SceneHooks.MachineLight = null;
        }

        void MachineLight(int key, int priority, Vector3 at, Color color, float peak, float reach, float life)
        {
            if (machineSlots == null) return;
            int s = machineSlots.Request(key, priority, peak, life, Time.time);
            if (s < 0) return;
            mLightAt[s] = at; mLightColor[s] = color; mLightReach[s] = Mathf.Min(reach, 10f);   // past ~10 m the per-object limit pops the lamps near it
            if (priority != 2) return;
            // a burning machine lights the smoke standing over it, as a fire does, while it is the strongest one
            float live = hearthPeak * Mathf.Max(0f, 1f - (Time.time - hearthSeen) / HearthHold);
            if (peak >= live) { hearthAt = at; hearthPeak = peak; hearthSeen = Time.time; hearthRange = reach * 0.55f; hearthTint = color; }
        }

        /// <summary>The machines' cards into the flash-glow mesh (after the pool's), and their lights. The cards are sent
        /// again every frame: none sent, none drawn.</summary>
        void UpdateMachines()
        {
            MachineKnobs();
            for (int j = 0; j < machineCards; j++)
            {
                int i = poolSize + j;
                bool on = j < mCount;
                var card = on ? mCol[j] : Color.clear;
                var shape = new Vector4(on ? mSize[j] : 0f, 0f, i * .19f, .15f);
                Vector3 at = on ? mPos[j] : Vector3.zero;
                for (int k = 0; k < 4; k++) { flashPos[i * 4 + k] = at; flashCol[i * 4 + k] = card; flashShape[i * 4 + k] = shape; }
            }
            mCount = 0;
            if (machineSlots == null) return;
            float now = Time.time;
            machineSlots.Sweep(now);
            for (int i = 0; i < machineSlots.Count; i++)
            {
                float level = machineSlots.Level(i, now);
                var l = machineLights[i];
                if (level <= 0f) { if (l.enabled) l.enabled = false; continue; }
                l.enabled = true; l.transform.position = mLightAt[i]; l.color = mLightColor[i]; l.range = mLightReach[i]; l.intensity = level;
            }
        }
    }
}
