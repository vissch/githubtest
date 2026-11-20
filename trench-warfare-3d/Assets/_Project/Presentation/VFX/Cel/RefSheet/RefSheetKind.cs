// Phase: CelFx reference sheets (2026-11-19) - the shared kind for effects cut out of their approved reference clip
// (celfx/tools/refsheet; generalised from fire_bolts' BoltSheetKind, owner-approved 2026-11-18). One subclass an
// effect names its textures (Prefix); Shaders/Cel/RefSheet/CelRefSheet_URP.shader documents the record and sheets.
// Data in Resources/VFX: <Prefix>SheetA, <Prefix>SheetB, <Prefix>SheetInd (PNG), <Prefix>SheetElems (.bytes),
// <Prefix>SheetLayout (.json: the layout and the clip frames the sheet covers). All of it is written by the exporter.
using System;
using UnityEngine;

namespace TW.Presentation.VFX.Cel
{
    /// <summary>The layout the exporter writes beside the sheets.</summary>
    [Serializable]
    public sealed class RefSheetLayout
    {
        public int frames, elems, tile, stored, ind_cols, first_frame, last_frame, sheets = 2;
        public int[] grid, ind_size, sides;
        public float fps = 24f, start_frame, gain = 1f;
        public int border = 1, filter;
    }

    public abstract class RefSheetKind : CelKind
    {
        /// <summary>The texture names' prefix in Resources/VFX (FireBolts for FireBoltsSheetA ...).</summary>
        public abstract string Prefix { get; }

        public override Shader FindShader() => Shader.Find("TW/Cel/RefSheet");
        public override Mesh BuildMesh() => CelMeshes.Quad01("Cel RefSheet " + Prefix);
        public override int Capacity => 64;
        public override int Order => 40;

        public override void Configure(Material material)
        {
            var l = RefSheet.Layout(Prefix);
            var elems = RefSheet.ElementTable(Prefix);
            if (l == null || elems == null)
            {
                Debug.LogError("RefSheetKind " + Name + ": Resources/VFX/" + Prefix + "SheetLayout or SheetElems is missing or bad; the sheet will not draw.");
                return;
            }
            for (int i = 0; i < Mathf.Max(l.sheets, 2); i++)
                material.SetTexture("_Sheet" + "ABCDEF"[i], Resources.Load<Texture2D>("VFX/" + Prefix + "Sheet" + "ABCDEF"[i]));
            material.SetTexture("_SheetInd", Resources.Load<Texture2D>("VFX/" + Prefix + "SheetInd"));
            material.SetTexture("_SheetElems", elems);
            material.SetFloat("_RS_Frames", l.frames);
            material.SetFloat("_RS_Elems", l.elems);
            material.SetFloat("_RS_Tile", l.tile);
            material.SetFloat("_RS_Stored", l.stored);
            material.SetFloat("_RS_IndCols", l.ind_cols);
            material.SetVector("_RS_Grid", new Vector4(l.grid[0], l.grid[1], 0f, 0f));
            material.SetVector("_RS_IndSize", new Vector4(l.ind_size[0], l.ind_size[1], 0f, 0f));
            material.SetFloat("_RS_Border", l.border);
            material.SetFloat("_RS_Gain", l.gain);
            material.SetFloat("_RS_Filter", l.filter);
            float Side(int i) => l.sides.Length > i ? l.sides[i] : 1f;
            material.SetVector("_RS_Sides", new Vector4(Side(0), Side(1), Side(2), Side(3)));
            material.SetVector("_RS_Sides2", new Vector4(Side(4), Side(5), 0f, 0f));
        }
    }
}
