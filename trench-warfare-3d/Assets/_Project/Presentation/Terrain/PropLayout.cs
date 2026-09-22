// Phase: B2 (hand edits over the composed props; presentation only, the sim never reads it)
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Terrain
{
    /// <summary>
    /// Hand edits laid over one battlefield's composed imported props (BattlefieldProps): a prop moved, turned, rescaled or
    /// removed, props added, and a look for every prop of a kind. One asset per decoration seed in Resources/Layouts, written
    /// by TW > Env Props (EnvPropEditor) and read in the game. A generated prop is found by its kind and the spot the
    /// composer gave it, so an edit outlives recomposing (every crater) but lapses if the composer stops putting it there.
    /// </summary>
    [CreateAssetMenu(menuName = "TW/Prop Layout", fileName = "Battlefield1917")]
    public sealed class PropLayout : ScriptableObject
    {
        [System.Serializable]
        public sealed class Edit
        {
            /// <summary>"Set/Prop@x,z" in centimetres (and "#n" for the n-th at one spot) for a generated prop, "Set/Prop+id" for an added one.</summary>
            public string Key;
            public string Module;
            public bool Added, Removed;
            /// <summary>x and z on the map; y is the height above the ground there, so the prop follows craters dug under it.</summary>
            public Vector3 Position;
            public Quaternion Rotation = Quaternion.identity;
            /// <summary>The prop's own scale, before the size of its kind (Look.Size).</summary>
            public Vector3 Scale = Vector3.one;
        }

        /// <summary>
        /// How every generated prop of one kind is drawn: a baseline scale, turn and lean, and the range each prop strays
        /// from it by (hashed per prop, so a battlefield always looks the same). Hand-edited props keep their own transform.
        /// </summary>
        [System.Serializable]
        public sealed class Look
        {
            public string Module;
            [Tooltip("Multiplies the size of every prop of the kind, hand-edited ones included.")]
            public float Size = 1f;
            [Tooltip("The kind's baseline scale, replacing the composer's own; zero keeps the composer's.")]
            public Vector3 Scale;
            [Tooltip("Each prop is this fraction bigger or smaller than the baseline.")]
            [Range(0f, .5f)] public float ScaleRange;
            [Tooltip("Degrees added to the heading the composer gives it.")]
            public float Yaw;
            [Tooltip("Each prop turns up to this many degrees either way.")]
            public float YawRange;
            [Tooltip("Pitch (x) and roll (y) in degrees, tipped either way per prop.")]
            public Vector2 Lean;
            [Tooltip("Each prop leans this fraction more or less.")]
            [Range(0f, 1f)] public float LeanRange;
            [Tooltip("Metres below the ground at the baseline scale (a bigger prop sinks more); below zero keeps the composer's height.")]
            public float Sink = -1f;

            /// <summary>The kind's drawn scale before the per-prop range: what the composer spaces its neighbours by.</summary>
            public Vector3 Baseline => (Scale == Vector3.zero ? Vector3.one : Scale) * Size;
        }

        public int Seed = 1917;
        public List<Edit> Edits = new List<Edit>();
        public List<Look> Looks = new List<Look>();
        public int NextId = 1;

        public static string ResourcePath(int seed) => "Layouts/Battlefield" + seed;
        public static string GeneratedKey(string module, Vector3 position)
            => module + "@" + Mathf.RoundToInt(position.x * 100f) + "," + Mathf.RoundToInt(position.z * 100f);

        Dictionary<string, Edit> index;
        void OnValidate() => index = null;

        public Edit Find(string key)
        {
            if (index == null) { index = new Dictionary<string, Edit>(); foreach (var e in Edits) if (e != null && e.Key != null) index[e.Key] = e; }
            return index.TryGetValue(key, out var edit) ? edit : null;
        }

        public Edit Get(string key, string module)
        {
            var edit = Find(key);
            if (edit == null) { edit = new Edit { Key = key, Module = module }; Edits.Add(edit); index[key] = edit; }
            return edit;
        }

        public void Forget(string key)
        {
            var edit = Find(key);
            if (edit != null) { Edits.Remove(edit); index.Remove(key); }
        }

        public void Clear() { Edits.Clear(); Looks.Clear(); index = null; }

        public Look LookOf(string module)
        {
            foreach (var look in Looks) if (look.Module == module) return look;
            return null;
        }

        public Look GetLook(string module)
        {
            var look = LookOf(module);
            if (look == null) { look = new Look { Module = module }; Looks.Add(look); }
            return look;
        }

        public float SizeOf(string module) => LookOf(module)?.Size ?? 1f;

        public void SetSize(string module, float size) => GetLook(module).Size = size;
    }
}
