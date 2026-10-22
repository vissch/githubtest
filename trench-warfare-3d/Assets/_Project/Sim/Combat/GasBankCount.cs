// The chlorine bank's count. The field itself is unchanged: this only answers how many live cells
// belong to the bank a check is looking at. The owner, 2026-10-22: count only the gas within 25 m of
// the bank's centre. A cell further out is the rest of the field, not this bank.
// A cell's place is its centre, (column + 0.5) * cell size, the same point a gas card stands on.
namespace TW.Sim.Combat
{
    public static class GasBankCount
    {
        /// <summary>A live cell further than this from the bank's centre is not part of the bank.</summary>
        public const float RadiusM = 25f;

        /// <summary>The least concentration a cell must hold to stand. The drawn bank uses the same floor.</summary>
        public const float StandFloor = 0.8f;

        /// <summary>Live cells of the bank centred at (centreX, centreZ). cellSize is the field grid, 4 m.
        /// A cell counts when it is thick enough to stand and its centre is within <see cref="RadiusM"/>.</summary>
        public static int LiveCells(float[] field, int width, float centreX, float centreZ, float cellSize)
        {
            if (field == null || width <= 0 || cellSize <= 0f) return 0;
            float r2 = RadiusM * RadiusM;
            int n = 0;
            for (int i = 0; i < field.Length; i++)
            {
                if (field[i] < StandFloor) continue;
                float cx = (i % width + 0.5f) * cellSize;
                float cz = (i / width + 0.5f) * cellSize;
                float dx = cx - centreX, dz = cz - centreZ;
                if (dx * dx + dz * dz <= r2) n++;
            }
            return n;
        }
    }
}
