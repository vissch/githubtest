// Phase: readability (2026-10-16). Proof for CaptureRig.IsFootFigure, the predicate that decides which slots
// CaptureRig.Measure's close-range mud-lift bar counts (the owner: "count only foot figures"). Expected values
// are UnitPose.cs's own bits (Alive = 1 << 0, Vehicle = 1 << 4) and the figure path's own rule, VATRenderer.cs:312.
using NUnit.Framework;
using TW.Editor;
using TW.Sim;
using UnityEngine;

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

        /// <summary>Proof for CaptureRig.ManBox against arithmetic done by hand, not against the code's own: a man
        /// 10 m in front of the camera, level with it, through a 60 deg vertical fov on a 900 px frame. The 2.6 m
        /// of him subtend 900 * (2.6 / 10) / (2 * tan 30 deg) = 202.6 px, and a foot point level with the camera
        /// lands half the frame down, imgY = 449.5 counting from the top.</summary>
        [Test]
        public void ManBoxMatchesAHandComputedProjection()
        {
            var projection = Matrix4x4.Perspective(60f, 16f / 9f, 0.1f, 1000f);
            var view = Matrix4x4.Scale(new Vector3(1f, 1f, -1f));   // Unity looks down -z; the rest is identity

            bool ok = CaptureRig.ManBox(view, projection, 1600, 900, new Vector3(0f, 0f, 10f),
                out float imgX, out float imgY, out float tallPx);

            Assert.That(ok, Is.True);
            Assert.That(tallPx, Is.EqualTo(202.6f).Within(1f));
            Assert.That(imgY, Is.EqualTo(449.5f).Within(1f));
            Assert.That(imgX, Is.EqualTo(800f).Within(1f));
        }

        /// <summary>The horizontal half of the same hand computation, and the reason Measure builds its own
        /// projection instead of reading the camera's. A man 2 m to the right at 10 m, 60 deg vertical fov:
        /// through the IMAGE's 16:9 he sits 2 / (10 * tan 30 * 16/9) of a half-frame right of the middle, so
        /// imgX = 800 + 800 * 0.1949 = 955.9. Through a batch-mode game view's 0.605 aspect the same man reads
        /// 2.94x further out, off at x 1258 - which is how a mask built on the box came up empty on 2026-10-20.
        /// The vertical is the same in both, because fieldOfView is the vertical one.</summary>
        [Test]
        public void ManBoxFollowsTheImagesAspectAndNotTheGameViews()
        {
            var view = Matrix4x4.Scale(new Vector3(1f, 1f, -1f));
            var man = new Vector3(2f, 0f, 10f);

            CaptureRig.ManBox(view, Matrix4x4.Perspective(60f, 16f / 9f, 0.1f, 1000f), 1600, 900, man,
                out float imgX, out float imgY, out float tallPx);
            CaptureRig.ManBox(view, Matrix4x4.Perspective(60f, 0.6051475f, 0.1f, 1000f), 1600, 900, man,
                out float narrowX, out float narrowY, out float narrowTall);

            Assert.That(imgX, Is.EqualTo(955.9f).Within(1f));
            Assert.That(narrowX, Is.EqualTo(1258f).Within(2f));     // the wrong aspect, named so it cannot creep back
            Assert.That(narrowY, Is.EqualTo(imgY).Within(0.01f));   // the vertical never depended on the aspect
            Assert.That(narrowTall, Is.EqualTo(tallPx).Within(0.01f));
        }

        /// <summary>A point behind the camera gets no box at all: a projection there is a number with no meaning,
        /// and a mask placed on it would measure whatever happens to live at those pixels.</summary>
        [Test]
        public void ManBoxRefusesAPointBehindTheCamera()
        {
            var projection = Matrix4x4.Perspective(60f, 16f / 9f, 0.1f, 1000f);
            var view = Matrix4x4.Scale(new Vector3(1f, 1f, -1f));

            Assert.That(CaptureRig.ManBox(view, projection, 1600, 900, new Vector3(0f, 0f, -10f),
                out _, out _, out _), Is.False);
        }
    }
}
