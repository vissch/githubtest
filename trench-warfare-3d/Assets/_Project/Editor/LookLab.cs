// Phase: A5d (2026-09-29, the owner's night look) — depends on: Knobs.
// Knobs set in the running game from `unity command eval`, for before-and-after stills of the night look:
//   LookLab.Knob("look.lift", "1")   set on the lab's next frame
//   LookLab.Clear()                  every knob back to its default, on the next frame
// An eval's own statics never reach compiled code (RiderLab.cs says why), so a scene object makes the call.
// Presentation only; nothing here is hashed or replayed.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Editor
{
    public static class LookLab
    {
        /// <summary>A knob set in the game on the lab's next frame.</summary>
        public static string Knob(string name, string value) { Get().Pending.Add((name, value)); return $"{name}={value} (next frame)"; }

        /// <summary>Every knob back to its default on the lab's next frame.</summary>
        public static string Clear() { Get().ClearAll = true; return "knobs cleared (next frame)"; }

        static Lab Get()
        {
            if (Lab.Instance != null) return Lab.Instance;
            var found = Object.FindFirstObjectByType<Lab>(FindObjectsInactive.Include);
            if (found == null) found = new GameObject("LookLab") { hideFlags = HideFlags.DontSave }.AddComponent<Lab>();
            Lab.Instance = found;
            return found;
        }

        public sealed class Lab : MonoBehaviour
        {
            public static Lab Instance;
            public readonly List<(string name, string value)> Pending = new List<(string, string)>();
            public bool ClearAll;

            void Awake()
            {
                if (Instance != null && Instance != this) { Destroy(gameObject); return; }
                Instance = this;
            }

            void Update()
            {
                if (ClearAll) { TW.Presentation.Knobs.Clear(); ClearAll = false; }
                if (Pending.Count == 0) return;
                foreach (var (name, value) in Pending) TW.Presentation.Knobs.Set(name, value);
                Pending.Clear();
            }
        }
    }
}
