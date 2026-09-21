// Phase: B3 (implemented)
// What VATBaker writes and VATRenderer loads (Resources/Units/InfantryVat): the mesh, the two RGBAHalf atlases and the
// row table in AnimRow order. Without one the renderer falls back to ProceduralSoldier.
using UnityEngine;

namespace TW.Presentation.Units
{
    public sealed class VatAssetData : ScriptableObject
    {
        public const string ResourcePath = "Units/InfantryVat";

        public Mesh Mesh;
        public Texture2D Positions, Normals;
        public Vector2[] RowTable;
        public int TotalFrames;

        public VatAsset ToAsset() => new VatAsset { Mesh = Mesh, Positions = Positions, Normals = Normals, RowTable = RowTable, TotalFrames = TotalFrames };
    }
}
