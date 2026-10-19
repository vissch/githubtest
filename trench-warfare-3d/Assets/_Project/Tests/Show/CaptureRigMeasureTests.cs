// Phase: readability (2026-10-16). Proof for CaptureRig.IsFootFigure, the predicate that decides which slots
// CaptureRig.Measure's close-range mud-lift bar counts (the owner: "count only foot figures"). Expected values
// are UnitPose.cs's own bits (Alive = 1 << 0, Vehicle = 1 << 4) and the figure path's own rule, VATRenderer.cs:312.
using NUnit.Framework;
using UnityEngine;
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

        // ---- the mark (2026-10-19) ---------------------------------------------------------------------------
        // CaptureRig.Mark names one world point a still is measured against, and every sidecar then carries its
        // projection. That projection is what turns a probe's pixel centroid into metres, so it is checked here
        // against the standard view's own arithmetic rather than against a rendered picture.

        /// <summary>The standard view (fov 25, pitch 25, zoom 30), posed the way CaptureRig.Pose poses it:
        /// distance = zoom * tan(30) / tan(fov / 2), looking at the aim point.</summary>
        static void StandardPose(Vector2 focus, float aimY, out Vector3 pos, out Quaternion rot)
        {
            rot = Quaternion.Euler(Pitch, 0f, 0f);
            float d = Zoom * Mathf.Tan(30f * Mathf.Deg2Rad) / Mathf.Tan(Fov * 0.5f * Mathf.Deg2Rad);
            pos = new Vector3(focus.x, aimY, focus.y) - rot * Vector3.forward * d;
        }

        const float Fov = 25f, Zoom = 30f, Pitch = 25f;
        const int W = 1600, H = 900;

        [Test]
        public void TheMarkAtTheShotsFocusProjectsToTheImageCentre()
        {
            var focus = new Vector2(70f, 40f);
            StandardPose(focus, 0f, out var pos, out var rot);
            CaptureRig.ProjectMark(new Vector3(focus.x, 0f, focus.y), pos, rot, Fov, W, H,
                                   out float px, out float py, out float _);

            Assert.That(px, Is.EqualTo(W * 0.5f).Within(2f));
            Assert.That(py, Is.EqualTo(H * 0.5f).Within(2f));
        }

        /// <summary>The worked example. Pixels per metre across the view is f / d with f = (h / 2) / tan(fov / 2)
        /// = 2029.8 px and d = 78.13 m, so 25.98 px/m; a metre ALONG the view is foreshortened by the pitch,
        /// 25.98 * sin(25) = 10.98 px/m. mark_px_per_m is the average of the two ground axes: 18.48 px/m.</summary>
        [Test]
        public void TheMarksPixelsPerMetreIsTheWorkedExample()
        {
            var focus = new Vector2(70f, 40f);
            StandardPose(focus, 0f, out var pos, out var rot);
            CaptureRig.ProjectMark(new Vector3(focus.x, 0f, focus.y), pos, rot, Fov, W, H,
                                   out float _, out float _, out float pxPerM);

            float f = (H * 0.5f) / Mathf.Tan(Fov * 0.5f * Mathf.Deg2Rad);
            float d = Zoom * Mathf.Tan(30f * Mathf.Deg2Rad) / Mathf.Tan(Fov * 0.5f * Mathf.Deg2Rad);
            float across = f / d, along = across * Mathf.Sin(Pitch * Mathf.Deg2Rad);
            float want = (across + along) * 0.5f;

            Assert.That(want, Is.EqualTo(18.48f).Within(0.1f), "the worked example moved");
            Assert.That(pxPerM, Is.EqualTo(want).Within(want * 0.05f));
        }

        /// <summary>A mark half a frame to the right is drawn to the right of centre, and the pixel offset agrees
        /// with its own px/m: the number the probe divides by is the number the picture obeys.</summary>
        [Test]
        public void AMarkOffCentreMovesByItsOwnPixelsPerMetre()
        {
            var focus = new Vector2(70f, 40f);
            StandardPose(focus, 0f, out var pos, out var rot);
            CaptureRig.ProjectMark(new Vector3(focus.x + 4f, 0f, focus.y), pos, rot, Fov, W, H,
                                   out float px, out float py, out float pxPerM);

            float f = (H * 0.5f) / Mathf.Tan(Fov * 0.5f * Mathf.Deg2Rad);
            float d = Zoom * Mathf.Tan(30f * Mathf.Deg2Rad) / Mathf.Tan(Fov * 0.5f * Mathf.Deg2Rad);
            float across = f / d;                                  // px per metre ACROSS the view: 25.98

            Assert.That(py, Is.EqualTo(H * 0.5f).Within(2f));      // level with the aim point, so no vertical move
            Assert.That(px - W * 0.5f, Is.EqualTo(4f * across).Within(4f * across * 0.05f));
            Assert.That(pxPerM, Is.LessThan(across));              // the average is pulled down by the pitched axis
        }
    }
}
