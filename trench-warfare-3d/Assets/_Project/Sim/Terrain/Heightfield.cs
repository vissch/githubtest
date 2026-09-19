// Phase: P0 (implemented; crater carving in A4)
// 1 m grid of int16 centimetres. X = lateral (width), Z = longitudinal (length). Index = z * Width + x.
using Unity.Collections;
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public struct Heightfield
    {
        public int Width;      // cells along X
        public int Length;     // cells along Z
        public float CellSize; // metres (1.0)
        public NativeArray<short> Cm;

        public Heightfield(int width, int length, float cellSize, Allocator allocator)
        {
            Width = width; Length = length; CellSize = cellSize;
            Cm = new NativeArray<short>(width * length, allocator);
        }

        public bool IsCreated => Cm.IsCreated;
        public int Index(int x, int z) => z * Width + x;
        public bool InBounds(int x, int z) => x >= 0 && z >= 0 && x < Width && z < Length;

        public float HeightAtCell(int x, int z)
        {
            x = math.clamp(x, 0, Width - 1); z = math.clamp(z, 0, Length - 1);
            return Cm[Index(x, z)] * 0.01f;
        }

        /// <summary>Bilinear height in metres at a world XZ position.</summary>
        public float Sample(float wx, float wz)
        {
            float fx = wx / CellSize - 0.5f, fz = wz / CellSize - 0.5f;
            int x0 = (int)math.floor(fx), z0 = (int)math.floor(fz);
            float tx = fx - x0, tz = fz - z0;
            float h00 = HeightAtCell(x0, z0), h10 = HeightAtCell(x0 + 1, z0);
            float h01 = HeightAtCell(x0, z0 + 1), h11 = HeightAtCell(x0 + 1, z0 + 1);
            return math.lerp(math.lerp(h00, h10, tx), math.lerp(h01, h11, tx), tz);
        }

        public void Set(int x, int z, float metres)
        {
            if (!InBounds(x, z)) return;
            Cm[Index(x, z)] = (short)math.clamp((int)math.round(metres * 100f), short.MinValue, short.MaxValue);
        }

        public void Dispose() { if (Cm.IsCreated) Cm.Dispose(); }
    }
}
