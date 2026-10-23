// Phase: fire pass (owner, 2026-10-23: "the ground the flamethrower sets alight does not burn on screen") — the holder
// behind CombatFx.BurningGround.cs. Pure: no MonoBehaviour, no draw call, no Random and no Time, so an EditMode test
// can drive it tick by tick (BurningGroundTests).
//
// The sim's BurningSystem lights NAV CELLS and emits SimEventType.CellBurning once per cell, only for a cell that was
// not already burning (Burning.cs, IgniteCell). A flamethrower sweeping a line re-lights the same cells again and
// again, so the event's seconds are the FIRST ignition's and not how long the sim will really burn there. That is why
// Sync exists: every frame the live cell list raises each held fire's end, and a fire whose cell the sim has dropped
// is retired that frame and listed in WentOut for the caller to scorch. Driven from the event alone, the drawn flame
// dies while the sim is still burning men on that square.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed class BurningGround
    {
        /// <summary>One burning nav cell, as the drawing holds it. Next/NextLight/Seed/Stood are the drawing's.</summary>
        public struct Fire
        {
            public int Cell;
            public Vector3 At;          // the cell centre, stood on the ground once the terrain is known
            public float BornSim, EndSim;
            public float Next, NextLight;
            public float Seed;
            public bool Stood;
        }

        /// <summary>A cell the sim says is burning now, and the sim second it stops.</summary>
        public struct SimCell { public int Cell; public float EndSim; }

        readonly List<Fire> fires = new List<Fire>(64);

        /// <summary>The held fires, for the drawing (same assembly) to step in place.</summary>
        public List<Fire> Live => fires;
        public int Count => fires.Count;

        /// <summary>Retired by the last Sync: the caller scorches each one and nothing draws on them again.</summary>
        public readonly List<Fire> WentOut = new List<Fire>(16);

        /// <summary>Fires retired over the match, for BurnReport and the tests.</summary>
        public int Retired { get; private set; }

        /// <summary>How far a place is from what the camera is looking at; null (a test, a bare scene) evicts the oldest.</summary>
        public System.Func<Vector3, float> Distance;

        /// <summary>Light a cell for <paramref name="seconds"/>. A cell already held keeps the later end. At the cap the
        /// fire FURTHEST from the look point goes, and a new fire further off than all of them is simply not drawn.</summary>
        public void Ignite(int cell, Vector3 at, float simNow, float seconds, int cap)
        {
            float end = simNow + Mathf.Max(0.1f, seconds);
            for (int i = 0; i < fires.Count; i++)
            {
                if (fires[i].Cell != cell) continue;
                var held = fires[i];
                if (end > held.EndSim) held.EndSim = end;
                fires[i] = held;
                return;
            }
            if (fires.Count >= Mathf.Max(1, cap))
            {
                int worst = 0; float far = -1f;
                for (int i = 0; i < fires.Count; i++)
                {
                    float d = Distance != null ? Distance(fires[i].At) : fires.Count - i;   // no camera: the oldest is the worst
                    if (d > far) { far = d; worst = i; }
                }
                if (Distance != null && Distance(at) > far) return;
                fires.RemoveAt(worst);
            }
            fires.Add(new Fire { Cell = cell, At = at, BornSim = simNow, EndSim = end, Seed = (cell & 1023) * 0.0171f });
        }

        /// <summary>Once a frame, before the drawing. <paramref name="sim"/> is the sim's live cells (null in a bare world or
        /// a replay: expiry then falls back to the event's own seconds). Fills WentOut with what retired this frame.</summary>
        public void Sync(float simNow, List<SimCell> sim)
        {
            WentOut.Clear();
            for (int i = fires.Count - 1; i >= 0; i--)
            {
                var f = fires[i];
                bool live = false;
                if (sim != null)
                {
                    for (int k = 0; k < sim.Count; k++)
                    {
                        if (sim[k].Cell != f.Cell) continue;
                        live = true;
                        if (sim[k].EndSim > f.EndSim) f.EndSim = sim[k].EndSim;
                        fires[i] = f;
                        break;
                    }
                }
                else live = simNow < f.EndSim;
                if (live) continue;
                WentOut.Add(f);
                Retired++;
                fires.RemoveAt(i);
            }
        }

        /// <summary>Is a flame held within <paramref name="radius"/> of this place (on the flat)? What the probe and the test ask.</summary>
        public bool Covers(Vector3 p, float radius)
        {
            float r2 = radius * radius;
            for (int i = 0; i < fires.Count; i++)
            {
                float dx = fires[i].At.x - p.x, dz = fires[i].At.z - p.z;
                if (dx * dx + dz * dz <= r2) return true;
            }
            return false;
        }

        public void Clear() { fires.Clear(); WentOut.Clear(); Retired = 0; }
    }
}
