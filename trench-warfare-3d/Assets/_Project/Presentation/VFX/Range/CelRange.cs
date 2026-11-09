// Phase: CelFx job (2026-11-09, lane/show/cel-vfx) - the Range: one fixed shot that every cel effect is fired into, so
// it can be filmed and scored against the owner's reference clips (C:/Users/PC/Downloads/vfx_h3/final/finalfinal).
// The clips are 8 s at 24 fps over a night field with a wooden target dummy; the shot comes from the left and the
// effect starts at 0.8 s. Editor/RangeStage.cs builds the stage, films it and fires the effect through this registry.
//
// How an effect joins (no edit to this file or to RangeStage):
//   [CelRangeEffect("explosion")] static void Fire(RangeShot s) { ... }            // found by reflection, or
//   CelRange.Register("explosion", s => ..., (s, dt) => ...);                      // from any static constructor
// The fire callback runs once, on the frame the shot clock passes t0, from an Update (before every LateUpdate, so
// whatever it adds is drawn that frame). The optional update callback runs every later frame of the shot with the
// seconds since the fire; an effect that holds (a flamethrower) drives itself from it. RangeShot carries the places
// (Muzzle, MuzzleHigh, Target, Ground), the LOD tier the shot is filmed at, the zoom and the seed. Nothing here
// draws: every draw an effect makes still goes through FrameBudget.
// Built-in: "none" (the empty plate). The old looks ("old_shell", "old_bullets") are registered by RangeStage,
// which can reach CombatFx.
using System;
using System.Collections.Generic;
using System.Reflection;
using UnityEngine;

namespace TW.Presentation.VFX
{
    /// <summary>The detail tier a Range shot is filmed at: Close = T3/T2, Mid = T1 (zoom 30), Far = O120 and out.</summary>
    public enum RangeLod : byte { Close = 0, Mid = 1, Far = 2 }

    /// <summary>One firing of one effect on the Range, handed to the effect's callbacks.</summary>
    public sealed class RangeShot
    {
        public string Id = "none";
        /// <summary>When the effect starts, seconds on the shot clock (the clips: 0.8).</summary>
        public float T0;
        /// <summary>Where the rounds come from: off-screen lower left, on the line the clips' tracers take
        /// (enter the frame at the lower-left corner, rise to the dummy). About 1.3 m above the ground.</summary>
        public Vector3 Muzzle;
        /// <summary>A second origin off-screen left on the line the clips' bolts and beams take (they enter at the left
        /// edge a little above the dummy's head and fall gently onto it): the dummy's depth from the camera.</summary>
        public Vector3 MuzzleHigh;
        /// <summary>The dummy's chest: the middle of its target board.</summary>
        public Vector3 Target;
        /// <summary>The ground at the dummy's foot.</summary>
        public Vector3 Ground;
        /// <summary>The camera that films the shot (its position, for an effect that faces it).</summary>
        public Vector3 Eye;
        public RangeLod Lod;
        /// <summary>The zoom the effect is seen at (TacticalCamera zoom; the close shot is filmed at 7.5, T3).</summary>
        public float Zoom = 7.5f;
        /// <summary>The same every run unless the capture asks for another: seed your own random numbers from it.</summary>
        public int Seed = 1917;
        /// <summary>Time.time at the fire (set by CelRange).</summary>
        public float FiredAt;
        /// <summary>From the muzzle to the target, unit length.</summary>
        public Vector3 Direction => (Target - Muzzle).normalized;
        public bool Fired;
        /// <summary>The effect's own state between its fire and its updates (CelRange never reads it).</summary>
        public object State;
    }

    /// <summary>Marks a static method `void M(RangeShot s)` as the fire callback of a Range effect id.</summary>
    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public sealed class CelRangeEffectAttribute : Attribute
    {
        public readonly string Id;
        public CelRangeEffectAttribute(string id) { Id = id; }
    }

    public static class CelRange
    {
        struct Entry { public Action<RangeShot> Fire; public Action<RangeShot, float> Update; }

        // the registry is registration data, filled once per domain; the shot in flight is per session (reset below)
        static readonly Dictionary<string, Entry> effects = new Dictionary<string, Entry>(StringComparer.Ordinal);
        static readonly List<RangeShot> live = new List<RangeShot>(8);
        static bool scanned;
        static float origin;

        /// <summary>The places of the Range as it was last staged (zero before a stage).</summary>
        public static Vector3 Muzzle, MuzzleHigh, Target, Ground;

        static CelRange()
        {
            SceneStatics.Register(nameof(CelRange), () => { live.Clear(); origin = 0f; Muzzle = MuzzleHigh = Target = Ground = Vector3.zero; });
            Register("none", s => { });
        }

        /// <summary>Add (or replace) an effect. `update` is optional: called every frame after the fire with the
        /// seconds since it.</summary>
        public static void Register(string id, Action<RangeShot> fire, Action<RangeShot, float> update = null)
        {
            if (string.IsNullOrEmpty(id) || fire == null) throw new ArgumentException("an effect needs an id and a fire callback");
            effects[id] = new Entry { Fire = fire, Update = update };
        }

        /// <summary>Is there an effect by this id (after scanning for [CelRangeEffect] methods)?</summary>
        public static bool Has(string id) { Scan(); return id != null && effects.ContainsKey(id); }

        /// <summary>Every registered id, sorted.</summary>
        public static List<string> Ids()
        {
            Scan();
            var ids = new List<string>(effects.Keys); ids.Sort(StringComparer.Ordinal);
            return ids;
        }

        /// <summary>Start the shot clock now (Time.time) and record the stage's places.</summary>
        public static void Begin(Vector3 muzzle, Vector3 muzzleHigh, Vector3 target, Vector3 ground)
        {
            live.Clear();
            origin = Time.time;
            Muzzle = muzzle; MuzzleHigh = muzzleHigh; Target = target; Ground = ground;
        }

        /// <summary>Seconds on the shot clock.</summary>
        public static float Now => Time.time - origin;

        /// <summary>Queue effect `id` to fire when the shot clock passes t0, at the stage's places. Returns the shot
        /// (fill Lod, Zoom, Seed, Eye before it fires), or null for an unknown id.</summary>
        public static RangeShot Fire(string effectId, float t0)
        {
            if (!Has(effectId)) { Debug.LogError("CelRange: no effect '" + effectId + "' (known: " + string.Join(", ", Ids()) + ")"); return null; }
            var s = new RangeShot { Id = effectId, T0 = t0, Muzzle = Muzzle, MuzzleHigh = MuzzleHigh, Target = Target, Ground = Ground };
            live.Add(s);
            return s;
        }

        /// <summary>Run the queued effects: call once a frame from an Update. Returns how many have fired.</summary>
        public static int Tick()
        {
            int fired = 0;
            float now = Now;
            for (int i = 0; i < live.Count; i++)
            {
                var s = live[i];
                if (!effects.TryGetValue(s.Id, out var e)) continue;
                if (!s.Fired)
                {
                    if (now + 1e-4f < s.T0) continue;
                    s.Fired = true; s.FiredAt = Time.time;
                    try { e.Fire(s); } catch (Exception ex) { Debug.LogException(ex); }
                }
                else if (e.Update != null)
                {
                    try { e.Update(s, Time.time - s.FiredAt); } catch (Exception ex) { Debug.LogException(ex); }
                }
                fired++;
            }
            return fired;
        }

        /// <summary>Forget the shots in flight (the registry stays).</summary>
        public static void Clear() => live.Clear();

        /// <summary>Find every static method marked [CelRangeEffect] in the TW assemblies, once.</summary>
        static void Scan()
        {
            if (scanned) return;
            scanned = true;
            foreach (var asm in AppDomain.CurrentDomain.GetAssemblies())
            {
                string n = asm.GetName().Name;
                if (!n.StartsWith("TW.", StringComparison.Ordinal)) continue;
                Type[] types;
                try { types = asm.GetTypes(); } catch (ReflectionTypeLoadException ex) { types = ex.Types; }
                foreach (var t in types)
                {
                    if (t == null) continue;
                    foreach (var m in t.GetMethods(BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic))
                    {
                        var marks = m.GetCustomAttributes(typeof(CelRangeEffectAttribute), false);
                        if (marks.Length == 0) continue;
                        var ps = m.GetParameters();
                        if (ps.Length != 1 || ps[0].ParameterType != typeof(RangeShot)) { Debug.LogError("CelRange: " + t.Name + "." + m.Name + " must be static void (RangeShot)"); continue; }
                        var fire = (Action<RangeShot>)Delegate.CreateDelegate(typeof(Action<RangeShot>), m);
                        foreach (CelRangeEffectAttribute a in marks)
                            if (!effects.ContainsKey(a.Id)) effects[a.Id] = new Entry { Fire = fire };
                    }
                }
            }
        }
    }
}
