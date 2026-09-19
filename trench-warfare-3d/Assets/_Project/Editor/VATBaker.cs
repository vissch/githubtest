// Phase: B3 (stub) — depends on: AnimRow order (P0 contract), source rigs with the 18 clips
// Samples each clip at 30 Hz into an RGBAHalf position (+ normal) atlas: U = vertex index, V = frame; rows are
// stacked in AnimRow order with a metadata asset (row start/length). Shared skeleton across factions; uniforms are
// texture swaps.
using UnityEditor;
using UnityEngine;

namespace TW.Editor
{
    public static class VATBaker
    {
        public const int SampleHz = 30;

        [MenuItem("TW/VAT/Bake Selected Rig")]
        public static void BakeSelected() => throw new System.NotImplementedException("Phase B3: VATBaker.BakeSelected");
    }
}
