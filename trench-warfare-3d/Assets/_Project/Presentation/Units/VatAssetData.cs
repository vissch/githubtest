// Phase: B3 (implemented), C1 (clip atlas)
// What VATBaker writes and VATRenderer loads (Resources/Units/InfantryVat): the mesh and the gzip'd atlas bytes
// (VatCodec), one row per controller Clip. Without one the renderer falls back to ProceduralSoldier.
using UnityEngine;

namespace TW.Presentation.Units
{
    public sealed class VatAssetData : ScriptableObject
    {
        public const string ResourcePath = "Units/InfantryVat";

        public Mesh Mesh;
        public TextAsset Atlas;
        public int Rows, TotalFrames, VertexCount;   // what the bytes hold, for the inspector and the tests

        public bool Valid => Mesh != null && Atlas != null && Atlas.bytes != null && Atlas.bytes.Length > 0;
        /// <summary>
        /// Decode into textures named after THIS figure. They used to all be called "InfantryVat", so a memory listing
        /// showed several identical names and could not tell the Soldier's atlas from the Sniper's — which is exactly
        /// what made "four position atlases where there should be two" take a day to read (docs/05).
        /// </summary>
        public VatAsset ToAsset() => VatCodec.Decode(Atlas.bytes, Mesh, string.IsNullOrEmpty(name) ? "InfantryVat" : name);
    }
}
