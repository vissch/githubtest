// Phase: anim-critique-camera (2026-10-13, lane/show/anim-critique) — [A5] a hull that coasts to a stop must stay
// where it coasted to, not snap back to its start; [A6] a standing rig's wheel pose must not be overwritten every
// frame. Drive only ADDS to the pose VehicleRig.Pose set this frame, so the test puts the hull and the wheel back
// to their rest pose by hand before each call, the way Pose does, instead of running the full rig.
using NUnit.Framework;
using TW.Playground;
using UnityEngine;

namespace TW.Tests.Playground
{
    public sealed class GroundDriveTests
    {
        GameObject host, hullGo, wheelGo;
        VehicleRig rig;
        VehicleRig.Part hull, wheel;
        GroundDrive drive;

        [SetUp]
        public void SetUp()
        {
            host = new GameObject("rig");
            rig = host.AddComponent<VehicleRig>();
            rig.Size = 1f;

            hullGo = new GameObject("Hull");
            hullGo.transform.SetParent(host.transform);
            hull = new VehicleRig.Part { Name = "Hull", T = hullGo.transform, RestLocal = Vector3.zero, RestRot = Quaternion.identity };
            rig.Parts.Add(hull);

            wheelGo = new GameObject("Wheel_L");
            wheelGo.transform.SetParent(host.transform);
            wheel = new VehicleRig.Part
            {
                Name = "Wheel_L", T = wheelGo.transform, RestLocal = Vector3.zero, RestRot = Quaternion.identity,
                Box = new Bounds(Vector3.zero, new Vector3(0.6f, 0.6f, 0.6f)),
            };
            rig.Parts.Add(wheel);

            drive = host.AddComponent<GroundDrive>().Init(rig);
        }

        [TearDown]
        public void TearDown() => Object.DestroyImmediate(host);

        void ResetPose()
        {
            hull.T.localPosition = hull.RestLocal; hull.T.localRotation = hull.RestRot;
            wheel.T.localPosition = wheel.RestLocal; wheel.T.localRotation = wheel.RestRot;
        }

        void Run(float speed, int frames)
        {
            drive.Speed = speed;
            for (int i = 0; i < frames; i++)
            {
                ResetPose();
                drive.Drive(1f / 60f);
            }
        }

        [Test]
        public void A_hull_that_coasts_to_a_stop_stays_where_it_coasted_to()
        {
            // [A5] drive, then stop: the old code's r went to 0 once carried fell under 0.01, snapping `at` (and so
            // the hull) straight back to rest even though it had driven well away from it.
            Run(4f, 120);
            float driven = hull.T.localPosition.magnitude;
            Assert.Greater(driven, 1f, "120 frames at speed 4 should have carried the hull over a metre from rest");

            Run(0f, 180);
            float stopped = hull.T.localPosition.magnitude;
            Assert.Greater(stopped, 1f, "a hull that has coasted to a stop must stay away from rest, not snap back to it");
        }

        [Test]
        public void A_standing_wheel_keeps_the_pose_Sag_left_set_on_it()
        {
            // [A6] the old code rewrote p.T.localRotation from RestRot every frame, even at Speed 0 with no spin
            // accumulated yet, replacing whatever lean VehicleRig.Pose had set (Sag left, a missing wheel's tilt).
            drive.Speed = 0f;
            var sag = wheel.RestRot * Quaternion.Euler(0f, 0f, 10f);
            wheel.T.localRotation = sag;
            drive.Drive(1f / 60f);
            Assert.AreEqual(sag, wheel.T.localRotation, "a standing wheel's pose must be left alone");
        }
    }
}
