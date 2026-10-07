// Phase: night lights (2026-10-07, the owner's word: decisions.md, "At night the 8 lamps nearest the view stay real
// lights") — depends on: RealLampSet (the rule), Knobs, ViewGround, RenderGround.
// Of the fixed lamps in `lanterns` (lanterns, prop lamps, trench lamps, burning trees, torches) only the few nearest
// the view keep their Light; the others keep their painted pool, glow card and glass and their Light is switched off.
// Each game camera chooses as it begins to render (ForCamera, NightLights.Pools.cs), by where it looks, and a lamp
// fades in or out as the set changes. Flashes, crater glows, the star shell, the machines' lights and the lightning
// are not in `lanterns` and are not touched. Knobs, followed while the game runs:
//   lights.realLamps    how many fixed lamps keep their real light (8); 43 or more: every lamp, the night as it was
//   lights.waterLamps   lamps standing by the water sheet, in the picture, that stay real beyond those (4; 0 none):
//                       TW/Water takes real lights only, so a painted lamp has no reflection in the water beside it
//   look.poolsByView    the painted pools go first to the flames whose pool reaches into the picture, nearest the
//                       camera first, then to the rest, nearest the middle of the picture (1); or to the flames
//                       nearest the camera itself, in the picture or not, as before (0)
// The rule only holds where the painted pools are drawn (night, look.pools above 0): without them a lamp with no
// real light would light nothing, so by day, and with look.pools 0, every lamp keeps its light.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed partial class NightLights
    {
        /// <summary>How far from a lamp the water sheet may lie for the lamp to count as standing by water: about
        /// the reach of its bright core on the surface.</summary>
        public const float WaterNear = 6f;
        /// <summary>How far outside the picture (in picture widths and heights) a lamp by water still counts as in it:
        /// its light reaches into the frame before its glass does.</summary>
        public const float PictureMargin = 0.15f;

        readonly RealLampSet realLamps = new RealLampSet();
        readonly List<bool> lampOut = new List<bool>(), lampByWater = new List<bool>(), lampExtra = new List<bool>();
        int realBudget = RealLampSet.DefaultBudget, waterBudget = RealLampSet.DefaultExtra, lampKnobs = -1, waterCursor;
        bool poolsByView = true;
        // The fade's clock: the picture's time, not the game's (the lamps fade while the game is held), and a tool's
        // fixed step when one holds the clock (Storm does the same). A step takes the time since the step before it,
        // not one frame's: in batch mode no camera renders until a tool asks, and a still taken a second after the
        // view moved must not catch the lamps one frame into their fade.
        double lampClock, lampStepped;

        /// <summary>The rule's state, for tests and captures: which lamps are chosen and how far each has faded.</summary>
        public RealLampSet RealLamps => realLamps;
        /// <summary>The fixed lamps built with the scene, burning or out.</summary>
        public int FixedLamps => lanterns.Count;
        /// <summary>Fixed lamps whose Light is on now.</summary>
        public int RealLampsLit { get { int n = 0; for (int i = 0; i < lanterns.Count; i++) if (lanterns[i] != null && lanterns[i].enabled) n++; return n; } }
        /// <summary>Does lamp i stand by the water sheet?</summary>
        public bool LampByWater(int i) => i >= 0 && i < lampByWater.Count && lampByWater[i];
        /// <summary>Has lamp i gone out (its post went down)?</summary>
        public bool LampIsOut(int i) => i >= 0 && i < lampOut.Count && lampOut[i];

        /// <summary>The next camera to render is a cut: the lamps are chosen afresh for it and set at once, none left
        /// fading from the view before. CaptureRig calls it as it poses a still.</summary>
        public void CutLamps() => realLamps.Reset();

        /// <summary>The budget in force: the knob where the painted pools are drawn, else every lamp.</summary>
        public static int BudgetFor(int knob, bool night, float pools) => night && pools > 0f ? Mathf.Max(0, knob) : RealLampSet.Everything;

        /// <summary>After Build: every lamp burning, and which stand by water.</summary>
        void BuiltLamps(MapData map)
        {
            lampOut.Clear(); lampByWater.Clear(); lampExtra.Clear();
            for (int i = 0; i < lanterns.Count; i++) { lampOut.Add(false); lampByWater.Add(ByWater(map, lanternHome[i])); lampExtra.Add(false); }
            realLamps.Reset();
        }

        /// <summary>Is the water sheet within WaterNear of this point? The ground under the water level, as TW/Water draws
        /// it (not a painted puddle: that is ground, and takes the pools).</summary>
        public static bool ByWater(MapData map, Vector3 at)
        {
            if (map == null || map.WaterLevel <= MapData.NoWater) return false;
            if (Under(map, at.x, at.z)) return true;
            for (int ring = 1; ring <= 2; ring++)
                for (int k = 0; k < 8; k++)
                {
                    float a = k * Mathf.PI * 0.25f + (ring == 1 ? Mathf.PI * 0.125f : 0f), r = WaterNear * 0.5f * ring;
                    if (Under(map, at.x + Mathf.Cos(a) * r, at.z + Mathf.Sin(a) * r)) return true;
                }
            return false;
        }

        static bool Under(MapData map, float x, float z)
        {
            if (x < 0f || z < 0f || x >= map.SizeMeters.x || z >= map.SizeMeters.y) return false;
            return RenderGround.Sample(map, x, z) < map.WaterLevel - .04f;
        }

        /// <summary>Each frame: the knobs when one changed, and one lamp's water looked at again (a crater can let the
        /// water in beside a lamp that stood dry).</summary>
        void UpdateRealLamps()
        {
            lampClock += Time.captureDeltaTime > 0f ? Time.captureDeltaTime : Time.unscaledDeltaTime;
            if (lampKnobs != Knobs.Generation)
            {
                lampKnobs = Knobs.Generation;
                realBudget = Knobs.Get("lights.realLamps", RealLampSet.DefaultBudget);
                waterBudget = Mathf.Max(0, Knobs.Get("lights.waterLamps", RealLampSet.DefaultExtra));
                poolsByView = Knobs.Get("look.poolsByView", DefaultPoolsByView) > 0f;
            }
            if (lampByWater.Count == 0) return;
            waterCursor = (waterCursor + 1) % lampByWater.Count;
            lampByWater[waterCursor] = ByWater(Host.Local.Map, lanternHome[waterCursor]);
        }

        /// <summary>look.poolsByView: on by default.</summary>
        public const float DefaultPoolsByView = 1f;

        /// <summary>What a flame outside the picture is ranked behind: more than any distance on a field, squared.</summary>
        public const float OutOfPicture = 1e7f;

        /// <summary>How a pool ranks for a camera, lower first. By the view (look.poolsByView): a pool that reaches into
        /// the picture by its distance from the lens (the nearest is the largest on screen), and every other pool
        /// behind all of those, by its distance from the middle of the picture. As before: by its distance from the
        /// lens, seen or not. Ranked by the picture's middle alone, the widest view left a lamp at the bottom of the
        /// picture with no pool while lamps deep in the haze had theirs (stills, 2026-10-07).</summary>
        public static float PoolKey(bool byView, bool inPicture, float sqrToLens, float sqrToFocus, float reach)
            => !byView || inPicture ? PoolRank(sqrToLens, reach) : OutOfPicture + PoolRank(sqrToFocus, reach);

        /// <summary>Where a view from `lens` along `forward` meets ground that lies at height `ground`.</summary>
        public static Vector3 FocusOn(Vector3 lens, Vector3 forward, float ground) => lens + forward * ViewGround.Along(Mathf.Max(0f, lens.y - ground), forward);

        /// <summary>Where a camera's view meets the ground: the middle of its picture. ViewGround's plane is y = 0 and
        /// the ground lies above it, so the height there is taken off once.</summary>
        Vector3 ViewFocus(Camera cam)
        {
            var lens = cam.transform;
            Vector3 p = ViewGround.Point(lens);
            var map = Host != null && Host.Local != null ? Host.Local.Map : null;
            if (map == null) return p;
            float ground = RenderGround.Sample(map, Mathf.Clamp(p.x, 0f, map.SizeMeters.x - 0.01f), Mathf.Clamp(p.z, 0f, map.SizeMeters.y - 0.01f));
            return FocusOn(lens.position, lens.forward, ground);
        }

        /// <summary>Chooses for this camera and sets every fixed lamp's Light: on with its faded share, or off.</summary>
        void RealLampsFor(Camera cam, Vector3 focus)
        {
            int n = Mathf.Min(lanterns.Count, lampOut.Count);
            if (n == 0) return;
            int budget = BudgetFor(realBudget, SceneMood.Night, poolStrength);
            for (int i = 0; i < n; i++)
            {
                bool extra = false;
                if (waterBudget > 0 && lampByWater[i] && !lampOut[i])
                {
                    Vector3 v = cam.WorldToViewportPoint(lanternHome[i]);
                    extra = v.z > 0f && v.x > -PictureMargin && v.x < 1f + PictureMargin && v.y > -PictureMargin && v.y < 1f + PictureMargin;
                }
                lampExtra[i] = extra;
            }
            // the time since the last step: nothing for a second camera in the same frame
            float dt = (float)(lampClock - lampStepped); lampStepped = lampClock;
            realLamps.Step(lanternHome, lampOut, lampExtra, focus, budget, waterBudget, dt);
            for (int i = 0; i < n; i++)
            {
                var l = lanterns[i];
                if (l == null) continue;
                bool on = !lampOut[i] && realLamps.Weight(i) > 0f;
                if (l.enabled != on) l.enabled = on;
                l.intensity = lanternLevel[i] * realLamps.Level(i);
            }
        }
    }
}
