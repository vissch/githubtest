// Phase: CelFx reference sheets (2026-11-19) - data access and the replay recipe for RefSheetKind effects.
// The Range's close shot plays the sheet; Mid and Far have no sheet (the sheet is footage of one camera).
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public static class RefSheet
    {
        static readonly Dictionary<string, RefSheetLayout> layouts = new Dictionary<string, RefSheetLayout>();
        static readonly Dictionary<string, Texture2D> tables = new Dictionary<string, Texture2D>();
        static readonly Dictionary<string, CelFxRecipe> recipes = new Dictionary<string, CelFxRecipe>();

        /// <summary>The exporter's layout for this prefix, null when it is missing.</summary>
        public static RefSheetLayout Layout(string prefix)
        {
            if (layouts.TryGetValue(prefix, out var l)) return l;
            var text = Resources.Load<TextAsset>("VFX/" + prefix + "SheetLayout");
            l = text != null ? JsonUtility.FromJson<RefSheetLayout>(text.text) : null;
            if (l != null && (l.frames <= 0 || l.grid == null || l.grid.Length < 2 || l.ind_size == null || l.ind_size.Length < 2 || l.sides == null || l.sides.Length < 2)) l = null;
            layouts[prefix] = l;
            return l;
        }

        /// <summary>The element table as a float texture: Frames wide, four rows an element (plate rectangle, world
        /// origin, world U, world V). Null when the data is missing or its size does not match the layout.</summary>
        public static Texture2D ElementTable(string prefix)
        {
            if (tables.TryGetValue(prefix, out var t)) return t;
            var l = Layout(prefix);
            var data = Resources.Load<TextAsset>("VFX/" + prefix + "SheetElems");
            t = null;
            if (l != null && data != null)
            {
                byte[] raw = data.bytes;
                int rows = raw.Length / (l.frames * 16);
                if (rows >= 4 && rows * l.frames * 16 == raw.Length)
                {
                    t = new Texture2D(l.frames, rows, TextureFormat.RGBAFloat, false, true)
                    { name = prefix + "SheetElems", filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
                    t.LoadRawTextureData(raw);
                    t.Apply(false, true);
                }
            }
            tables[prefix] = t;
            return t;
        }

        /// <summary>Elements in the table (0 when the data is missing).</summary>
        public static int Elements(string prefix) { var t = ElementTable(prefix); return t != null ? t.height / 4 : 0; }

        /// <summary>The clip frame shown `age` seconds after the record's birth at `fps` frames a second, when the
        /// record is born on clip frame `first`. Rounded so a film stepped at exactly 1/fps never lands on a
        /// boundary. Same rule as the shader.</summary>
        public static int FrameAt(float age, float fps, float first) => Mathf.FloorToInt(Mathf.Max(age, 0f) * fps + 0.5f) + Mathf.RoundToInt(first);

        /// <summary>One record an element, from the clip frame the shot fires on to the sheet's last frame.
        /// Close = Mid = Far = 1 on purpose: CelLayer counts blend between tiers, so Close 1 / Mid 0 rounds to no
        /// record at the close shot (fire_bolts s01's first film drew nothing).</summary>
        public static CelFxRecipe MakeRecipe(string kindName, RefSheetLayout l, int elements)
        {
            float life = Mathf.Max(l.last_frame + 1 - l.start_frame, 1f) / l.fps;
            var r = new CelFxRecipe(kindName + "_sheet", life);
            for (int i = 0; i < Mathf.Max(elements, 1); i++)
                r.Layers.Add(new CelLayer
                {
                    Name = "reference sheet " + i, Kind = kindName, Start = 0f, Life = life, LifeJitter = 0f,
                    Close = 1, Mid = 1, Far = 1, Speed = 0f, SpeedJitter = 0f, Size = new CelCurve(1f, 1f), SizeJitter = 0f,
                    Core = 1f, Opacity = 1f, Extra = new Vector4(l.fps, l.start_frame, i, 0f),
                });
            return r;
        }

        /// <summary>Play the sheet of `kind` at the Range shot `s`, at the close shot only. False when there is no
        /// sheet to play (wrong tier or missing data, logged once).</summary>
        public static bool PlayAtClose(RangeShot s, RefSheetKind kind)
        {
            if (s.Lod != RangeLod.Close) return false;
            var l = Layout(kind.Prefix);
            int n = Elements(kind.Prefix);
            if (l == null || n == 0)
            {
                Debug.LogError("RefSheet: no sheet data for " + kind.Name + " (Resources/VFX/" + kind.Prefix + "Sheet*); run celfx tools.refsheet export.");
                return false;
            }
            if (!recipes.TryGetValue(kind.Name, out var recipe)) recipes[kind.Name] = recipe = MakeRecipe(kind.Name, l, n);
            Vector3 look = s.Target - (s.Eye.sqrMagnitude > 1e-6f ? s.Eye : s.MuzzleHigh);
            look.y = 0f;
            Vector3 away = look.sqrMagnitude > 1e-6f ? look.normalized : Vector3.forward;
            CelFx.Play(recipe, s.Target, away, s.Target, (uint)s.Seed);
            return true;
        }
    }
}
