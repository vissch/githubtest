// Phase: readability (2026-10-16). Proof for CaptureRig.IsFootFigure, the predicate that decides which slots
// CaptureRig.Measure's close-range mud-lift bar counts (the owner: "count only foot figures"). Expected values
// are UnitPose.cs's own bits (Alive = 1 << 0, Vehicle = 1 << 4) and the figure path's own rule, VATRenderer.cs:312.
using NUnit.Framework;
using TW.Editor;
using TW.Sim;

namespace TW.Tests
{
    public class CaptureRigMeasureTests
    {
        [Test]
        public void AliveFootUnitIsAFigure()
        {
            Assert.That(CaptureRig.IsFootFigure((uint)UnitFlags.Alive), Is.True);
        }

        [Test]
        public void AliveVehicleIsNotAFigure()
        {
            Assert.That(CaptureRig.IsFootFigure((uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle), Is.False);
        }

        [Test]
        public void DeadSlotIsNotAFigure()
        {
            Assert.That(CaptureRig.IsFootFigure((uint)UnitFlags.None), Is.False);
        }

        [Test]
        public void AliveInTrenchIsStillAFigure()
        {
            Assert.That(CaptureRig.IsFootFigure((uint)UnitFlags.Alive | (uint)UnitFlags.InTrench), Is.True);
        }

        /// <summary>Must agree with the figure path's own mask (VATRenderer.Sockets, VATRenderer.cs:312): a slot
        /// that gets sockets for the figure's rifle is a slot CaptureRig now counts as a man.</summary>
        [Test]
        public void AgreesWithTheFigurePathsOwnVehicleMask()
        {
            uint aliveFoot = (uint)UnitFlags.Alive;
            uint aliveVehicle = (uint)UnitFlags.Alive | (uint)UnitFlags.Vehicle;
            bool figurePathSocketsFoot = (aliveFoot & (uint)UnitFlags.Vehicle) == 0;
            bool figurePathSocketsVehicle = (aliveVehicle & (uint)UnitFlags.Vehicle) == 0;

            Assert.That(CaptureRig.IsFootFigure(aliveFoot), Is.EqualTo(figurePathSocketsFoot));
            Assert.That(CaptureRig.IsFootFigure(aliveVehicle), Is.EqualTo(figurePathSocketsVehicle));
        }

        /// <summary>Worked example: 1.5 m half-height / tan(12.5 deg) = 1.5 / 0.221695 = 6.766 m. Also checks a
        /// taller ask (the 17.2 m CloseFitHeight box) stands further off than a shorter one at the same fov.</summary>
        [Test]
        public void StandoffFramesTheHeightAsked()
        {
            Assert.That(CaptureRig.Standoff(3.0f, 25f), Is.EqualTo(6.766f).Within(0.01f));
            Assert.That(CaptureRig.Standoff(17.2f, 42f), Is.GreaterThan(CaptureRig.Standoff(3.0f, 42f)));
        }
    }
}
