// Phase: night lights (2026-10-07, the owner's word on "Fewer real lights at night: which way?": "The 8 lamps nearest
// your view stay real, the other 35 painted only") — the choice behind NightLights' fixed lamps (lanterns, prop lamps,
// trench lamps, burning trees, torches), kept apart from the Light components so a test can hold it, as
// MachineLightSlots is. The renderer gives one object eight lights at most, so of the 43 lamps that always burn most
// lit almost nothing with their real light; their painted pool, glow card and glass are what showed.
//  - the `budget` lamps nearest the view's focus (the middle of the picture, on the ground) are chosen; distance is
//    measured along the ground; two lamps as far off as each other go by their place in the list;
//  - a lamp already chosen keeps its place until another is nearer by KeepMetres, so a tie or a shaking camera does
//    not swap two lamps back and forth;
//  - after those, the lamps flagged `extra` (NightLights flags a lamp by the water sheet that is in the picture: the
//    water takes real lights only, so a painted lamp has no reflection in it) are chosen too, nearest first, at most
//    `extraBudget` of them;
//  - a chosen lamp's weight climbs to 1 and any other's sinks to 0 over FadeSeconds, eased (Level), so a lamp never
//    pops as the view moves; one lamp fading out while another fades in keeps the sum of their levels at 1;
//  - a cut (the first step, or the focus more than CutMetres from where it was: a jump on the map, a posed still)
//    sets every weight at once: the whole picture changed, there is nothing to fade from;
//  - a lamp that is out (its post went down) is dark at once and is never chosen;
//  - a budget of Everything (43, the lamps on the field the owner decided on) or more chooses every lamp at full
//    weight: the night as it was before this rule.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public sealed class RealLampSet
    {
        /// <summary>lights.realLamps: how many fixed lamps keep their real light (the owner, 2026-10-07).</summary>
        public const int DefaultBudget = 8;
        /// <summary>A budget this high or higher is every lamp, as before the rule.</summary>
        public const int Everything = 43;
        /// <summary>lights.waterLamps: lamps by water in the picture that stay real beyond the budget.</summary>
        public const int DefaultExtra = 4;
        public const float FadeSeconds = 0.6f, KeepMetres = 2f, CutMetres = 30f;

        float[] weight = new float[0], key = new float[0];
        bool[] chosen = new bool[0];
        int[] order = new int[0];
        int count;
        bool primed;
        Vector3 lastFocus;

        /// <summary>How many lamps the last step saw.</summary>
        public int Count => count;

        /// <summary>0 painted only .. 1 its real light whole; it moves evenly with time. 1 for a lamp no step has seen.</summary>
        public float Weight(int i) => i >= 0 && i < count ? weight[i] : 1f;

        /// <summary>What a lamp's real light is multiplied by: its weight, eased at both ends. Exactly 1 at weight 1.</summary>
        public float Level(int i) { float w = Weight(i); return w * w * (3f - 2f * w); }

        /// <summary>Is lamp i among those that keep (or are getting) their real light?</summary>
        public bool Chosen(int i) => i >= 0 && i < count && chosen[i];

        /// <summary>Lamps chosen now.</summary>
        public int ChosenCount { get { int n = 0; for (int i = 0; i < count; i++) if (chosen[i]) n++; return n; } }

        /// <summary>Lamps with any real light now: the chosen, and those still fading out.</summary>
        public int LitCount { get { int n = 0; for (int i = 0; i < count; i++) if (weight[i] > 0f) n++; return n; } }

        /// <summary>Forget everything: the next step is a cut.</summary>
        public void Reset() { count = 0; primed = false; }

        /// <summary>One step: choose for this focus, then move every weight toward its lamp's place by dt seconds.
        /// `isOut` and `extra` may be null (no lamp out, none flagged).</summary>
        public void Step(IReadOnlyList<Vector3> at, IReadOnlyList<bool> isOut, IReadOnlyList<bool> extra, Vector3 focus, int budget, int extraBudget, float dt)
        {
            int n = at == null ? 0 : at.Count;
            Resize(n);
            bool all = budget >= Everything;
            if (all)
                for (int i = 0; i < n; i++) chosen[i] = !Out(isOut, i);
            else
            {
                // every burning lamp, nearest first: by its distance, less KeepMetres for a lamp that holds a place
                int m = 0;
                for (int i = 0; i < n; i++)
                {
                    if (Out(isOut, i)) { chosen[i] = false; continue; }
                    float dx = at[i].x - focus.x, dz = at[i].z - focus.z;
                    float k = Mathf.Sqrt(dx * dx + dz * dz) - (chosen[i] ? KeepMetres : 0f);
                    key[i] = k;
                    int p = m++;
                    while (p > 0 && key[order[p - 1]] > k) { order[p] = order[p - 1]; p--; }   // an equal key stays behind: list order
                    order[p] = i;
                }
                int want = Mathf.Max(0, budget), more = Mathf.Max(0, extraBudget);
                for (int r = 0; r < m; r++)
                {
                    int i = order[r];
                    if (r < want) chosen[i] = true;
                    else if (more > 0 && extra != null && i < extra.Count && extra[i]) { chosen[i] = true; more--; }
                    else chosen[i] = false;
                }
            }
            float ox = focus.x - lastFocus.x, oz = focus.z - lastFocus.z;
            bool cut = !primed || all || ox * ox + oz * oz > CutMetres * CutMetres;
            float step = Mathf.Max(0f, dt) / FadeSeconds;
            for (int i = 0; i < n; i++)
            {
                float target = chosen[i] ? 1f : 0f;
                if (cut || Out(isOut, i)) weight[i] = target;
                else weight[i] = Mathf.MoveTowards(weight[i], target, step);
            }
            primed = true; lastFocus = focus;
        }

        static bool Out(IReadOnlyList<bool> isOut, int i) => isOut != null && i < isOut.Count && isOut[i];

        void Resize(int n)
        {
            if (n > weight.Length)
            {
                System.Array.Resize(ref weight, n); System.Array.Resize(ref key, n);
                System.Array.Resize(ref chosen, n); System.Array.Resize(ref order, n);
            }
            for (int i = count; i < n; i++) { weight[i] = 0f; chosen[i] = false; }   // a lamp hung since the last step starts dark, unless this step is a cut
            if (n != count && count == 0) primed = false;
            count = n;
        }
    }
}
