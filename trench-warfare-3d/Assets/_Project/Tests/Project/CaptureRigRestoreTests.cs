// Phase: review-fix (2026-10-16) — [found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] CaptureRig.Rig.Run() only restored tc.enabled,
// Zoom, Focus, fieldOfView and the transform after its while loop over Queue drained normally. Play stopped mid-set,
// a domain reload, or an exception from Pose/Render inside the loop all skipped that restore in silence, leaving the
// tactical camera disabled and parked at the last shot's pose for the rest of the editor session. These tests drive
// the fix's Take/GiveBack directly, from outside Run(), so they do not need Play.
using System.Reflection;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class CaptureRigRestoreTests
    {
        // Reached by reflection, not called directly: on the tree before the fix, CaptureRig.Rig has no public
        // static Take/GiveBack, so this must fail on IsNotNull — a clean red naming the finding — and not a
        // compile error.
        static MethodInfo Method(string name) =>
            typeof(CaptureRig.Rig).GetMethod(name, BindingFlags.Public | BindingFlags.Static);

        [Test]
        public void A_Stopped_Set_Hands_The_Tactical_Camera_Back()
        {
            var take = Method("Take");
            var giveBack = Method("GiveBack");
            Assert.IsNotNull(take, "[found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] CaptureRig.Rig cannot hand the camera back from outside Run()");
            Assert.IsNotNull(giveBack, "[found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] CaptureRig.Rig cannot hand the camera back from outside Run()");

            var go = new GameObject("cap", typeof(Camera), typeof(TacticalCamera));
            try
            {
                var cam = go.GetComponent<Camera>();
                var tc = go.GetComponent<TacticalCamera>();

                tc.Zoom = 42f; tc.Focus = new Vector2(7f, 9f); cam.fieldOfView = 25f;
                var pos = new Vector3(11f, 22f, 33f); var rot = Quaternion.Euler(5f, 60f, 0f);
                cam.transform.SetPositionAndRotation(pos, rot);

                take.Invoke(null, new object[] { tc, cam });
                Assert.IsFalse(tc.enabled, "[found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] Take must disable the tactical camera for the length of a set");

                // what a shot writes while the set is running
                tc.Zoom = 5f; tc.Focus = Vector2.zero; cam.fieldOfView = 60f;
                cam.transform.SetPositionAndRotation(new Vector3(999f, 999f, 999f), Quaternion.identity);

                LogAssert.Expect(LogType.Warning, new Regex("tactical camera back"));
                giveBack.Invoke(null, new object[] { "test" });

                Assert.IsTrue(tc.enabled, "[found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] an interrupted set must leave the tactical camera enabled");
                Assert.That(tc.Zoom, Is.EqualTo(42f).Within(1e-4f), "[found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] zoom must be the pre-capture value");
                Assert.That(tc.Focus.x, Is.EqualTo(7f).Within(1e-4f), "[found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] focus must be the pre-capture value");
                Assert.That(tc.Focus.y, Is.EqualTo(9f).Within(1e-4f), "[found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] focus must be the pre-capture value");
                Assert.That(cam.fieldOfView, Is.EqualTo(25f).Within(1e-4f), "[found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] field of view must be the pre-capture value");
                Assert.That(Vector3.Distance(cam.transform.position, pos), Is.EqualTo(0f).Within(1e-3f), "[found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] the pose must be the pre-capture pose");
                Assert.That(Quaternion.Angle(cam.transform.rotation, rot), Is.EqualTo(0f).Within(1e-3f), "[found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] the pose must be the pre-capture pose");
            }
            finally { Object.DestroyImmediate(go); }
        }

        [Test]
        public void Giving_Back_Twice_Does_Not_Overwrite_A_Live_Camera()
        {
            var take = Method("Take");
            var giveBack = Method("GiveBack");
            Assert.IsNotNull(take, "[found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] CaptureRig.Rig cannot hand the camera back from outside Run()");
            Assert.IsNotNull(giveBack, "[found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] CaptureRig.Rig cannot hand the camera back from outside Run()");

            var go = new GameObject("cap", typeof(Camera), typeof(TacticalCamera));
            try
            {
                var cam = go.GetComponent<Camera>();
                var tc = go.GetComponent<TacticalCamera>();
                tc.Zoom = 42f; tc.Focus = new Vector2(7f, 9f); cam.fieldOfView = 25f;

                take.Invoke(null, new object[] { tc, cam });
                LogAssert.Expect(LogType.Warning, new Regex("tactical camera back"));
                giveBack.Invoke(null, new object[] { "test" });

                cam.transform.SetPositionAndRotation(new Vector3(1f, 2f, 3f), Quaternion.identity);
                tc.Zoom = 11f;
                giveBack.Invoke(null, new object[] { (string)null });

                Assert.That(tc.Zoom, Is.EqualTo(11f).Within(1e-4f), "[found-restore-the-tactical-camera-and-zoom-when-a-20261016-0] a second GiveBack must not fight a live camera once holding is clear");
            }
            finally { Object.DestroyImmediate(go); }
        }
    }
}
