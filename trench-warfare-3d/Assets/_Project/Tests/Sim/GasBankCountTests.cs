// The chlorine bank's count keeps only the gas within 25 m of the bank's centre.
using NUnit.Framework;
using TW.Sim.Combat;

namespace TW.Tests
{
    public class GasBankCountTests
    {
        // Cell size 2 m so a cell centre lands on a whole metre. Bank centre (0, 1): cell 0's centre is
        // (1, 1), one metre off; cell 12's centre is (25, 1), exactly on the radius; cell 13's is (27, 1).
        const int Width = 20;
        const float Cell = 2f;
        const float CentreX = 0f, CentreZ = 1f;

        static float[] Grid()
        {
            return new float[Width];
        }

        [Test]
        public void GasPast25mIsLeftOutOfTheBanksCount()
        {
            float[] field = Grid();
            field[0] = 1f;
            field[12] = 1f;
            field[13] = 1f;
            Assert.AreEqual(2, GasBankCount.LiveCells(field, Width, CentreX, CentreZ, Cell),
                "the cell at 27 m is the field, not this bank");
        }

        [Test]
        public void GasAtExactly25mStillCounts()
        {
            float[] field = Grid();
            field[12] = GasBankCount.StandFloor;
            Assert.AreEqual(1, GasBankCount.LiveCells(field, Width, CentreX, CentreZ, Cell),
                "a cell whose centre is on the 25 m circle still belongs to the bank");
        }

        [Test]
        public void GasUnderTheStandFloorDoesNotCount()
        {
            float[] field = Grid();
            field[0] = GasBankCount.StandFloor - 0.01f;
            Assert.AreEqual(0, GasBankCount.LiveCells(field, Width, CentreX, CentreZ, Cell),
                "a cell too thin to stand is not part of the bank");
        }

        [Test]
        public void TheRadiusIs25Metres()
        {
            Assert.AreEqual(25f, GasBankCount.RadiusM);
        }
    }
}
