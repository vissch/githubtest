// Phase: B2 (stub) — depends on: Heightfield, CraterStamp events, TrenchDef splines
// 32 m chunks; dirty-chunk re-upload when CraterStamp events touch them; GPU vertex displacement from an R16
// height texture (Shaders/TerrainDisplace.shader); trench kit instancing along TrenchDef cells; mud/wire/crater decals.
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public sealed class TerrainChunkRenderer : MonoBehaviour
    {
        public const int ChunkMeters = 32;
        void Start() => throw new System.NotImplementedException("Phase B2: TerrainChunkRenderer");
    }

    public sealed class TrenchKitPlacer : MonoBehaviour
    {
        void Start() => throw new System.NotImplementedException("Phase B2: TrenchKitPlacer (parapet, fire-step, duckboards, revetment)");
    }
}
