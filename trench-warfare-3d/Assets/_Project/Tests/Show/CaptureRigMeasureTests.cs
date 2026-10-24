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

        /// <summary>Straight down from above, head and foot land on the same pixel, so the head-to-foot span is ~0
        /// and a man sized by it alone is dropped (men_in_frame 0 while the renderer drew six). 58.4 px is a real
        /// reading: build 19f946794d's px_per_m 32.8 times a man's 1.78 m.</summary>
        [Test]
        public void StraightDownAManIsSizedAcrossNotTall()
        {
            Assert.That(CaptureRig.ManPixels(0.4f, 58.4f), Is.EqualTo(58.4f).Within(0.001f));
            Assert.That(CaptureRig.ManPixels(0.4f, 58.4f), Is.GreaterThan(CaptureRig.MinManPixels));
        }

        /// <summary>Sizing him across must not let a genuinely tiny man through: under MinManPixels either way he is
        /// still skipped.</summary>
        [Test]
        public void AManTooSmallToJudgeIsStillSkipped()
        {
            Assert.That(CaptureRig.ManPixels(0.4f, 3f), Is.LessThan(CaptureRig.MinManPixels));
        }
    }
}
