// Phase: CelFx fire_bolts reference sheets (2026-11-18) - the fire and smoke of the reference clip, cut out frame by
// frame and replayed on cards in the world (Shaders/Cel/Effects/fire_bolts/CelBoltSheet_URP.shader documents the
// record and the sheets; they are made by celfx/agents/fire_bolts/sheets_20261118/analysis/a5_export.py).
// One record an element (a bolt, a hit site, or the whole frame); each frame the element's card and its plate
// rectangle come from the element table.
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    public sealed class BoltSheetKind : CelKind
    {
        public const string KindName = "BoltSheet";
        /// <summary>Frames of the reference clip in the element table (its width).</summary>
        public const int Frames = 191;

        public override string Name => KindName;
        public override Shader FindShader() => Shader.Find("TW/Cel/BoltSheet");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel BoltSheet");
        public override int Capacity => 64;
        public override int Order => 40;

        public override void Configure(Material material)
        {
            material.SetTexture("_SheetA", Resources.Load<Texture2D>("VFX/FireBoltsSheetA"));
            material.SetTexture("_SheetB", Resources.Load<Texture2D>("VFX/FireBoltsSheetB"));
            material.SetTexture("_SheetInd", Resources.Load<Texture2D>("VFX/FireBoltsSheetInd"));
            material.SetTexture("_SheetElems", ElementTable());
        }

        static Texture2D elems;

        /// <summary>The element table as a float texture: Frames wide, four rows an element (plate rectangle, world
        /// origin, world U, world V). Null when the data is missing.</summary>
        public static Texture2D ElementTable()
        {
            if (elems != null) return elems;
            var data = Resources.Load<TextAsset>("VFX/FireBoltsSheetElems");
            if (data == null) return null;
            byte[] raw = data.bytes;
            int rows = raw.Length / (Frames * 16);
            if (rows < 4 || rows * Frames * 16 != raw.Length) return null;
            elems = new Texture2D(Frames, rows, TextureFormat.RGBAFloat, false, true)
            { name = "FireBoltsSheetElems", filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp, hideFlags = HideFlags.HideAndDontSave };
            elems.LoadRawTextureData(raw);
            elems.Apply(false, true);
            return elems;
        }

        /// <summary>Elements in the table (0 when the data is missing).</summary>
        public static int Elements() { var t = ElementTable(); return t != null ? t.height / 4 : 0; }

        /// <summary>The clip frame shown `age` seconds after the record's birth at `fps` frames a second, when the
        /// record is born on clip frame `first`. Rounded so a film stepped at exactly 1/fps never lands on a boundary.</summary>
        public static int FrameAt(float age, float fps, float first) => Mathf.FloorToInt(Mathf.Max(age, 0f) * fps + 0.5f) + Mathf.RoundToInt(first);
    }
}
