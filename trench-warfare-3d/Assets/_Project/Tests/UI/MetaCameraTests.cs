// Phase: B6 / docs/21 phase 6 (implemented) — MetaCamera's Take/Release contract: Take saves the menu camera's
// position, rotation, fov and cullingMask and borrows the whole scene (mask ~0); Release must give all four back.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Meta;

namespace TW.Tests
{
    public class MetaCameraTests
    {
        static Camera NewCam()
        {
            var go = new GameObject("meta-cam");
            var cam = go.AddComponent<Camera>();
            go.transform.SetPositionAndRotation(new Vector3(5f, 7f, -9f), Quaternion.Euler(11f, 22f, 33f));
            cam.fieldOfView = 17f;
            cam.cullingMask = 5;
            return cam;
        }

        [Test]
        public void Release_Gives_Back_Position_Rotation_Fov_And_Mask()
        {
            var cam = NewCam();
            try
            {
                var sceneRot = cam.transform.rotation;
                var rig = new MetaCamera();
                rig.Take(cam, MetaCamera.Mode.Orbit, Vector3.zero);
                Assert.That(cam.transform.position, Is.Not.EqualTo(new Vector3(5f, 7f, -9f)), "Take moves the camera onto the rig");
                Assert.That(cam.cullingMask, Is.EqualTo(~0), "Take borrows the whole scene");

                rig.Release();
                Assert.That(cam.transform.position, Is.EqualTo(new Vector3(5f, 7f, -9f)));
                Assert.That(cam.transform.rotation, Is.EqualTo(sceneRot));
                Assert.That(cam.fieldOfView, Is.EqualTo(17f));
                Assert.That(cam.cullingMask, Is.EqualTo(5));
            }
            finally { Object.DestroyImmediate(cam.gameObject); }
        }

        [Test]
        public void Take_Borrows_The_Whole_Scene_And_Pins_The_Fov()
        {
            var cam = NewCam();
            try
            {
                var rig = new MetaCamera();
                var target = new Vector3(0f, 1.5f, 0f);
                rig.Take(cam, MetaCamera.Mode.Orbit, target);

                Assert.That(cam.fieldOfView, Is.EqualTo(30f));
                Assert.That(cam.cullingMask, Is.EqualTo(~0));
                Assert.That(rig.Held, Is.True);
                Assert.That(rig.CurrentMode, Is.EqualTo(MetaCamera.Mode.Orbit));
                Assert.That(rig.Pitch, Is.EqualTo(32f));
                Assert.That(rig.Yaw, Is.EqualTo(35f));
                Assert.That(rig.Zoom, Is.EqualTo(26f));
                Assert.That(rig.GoalZoom, Is.EqualTo(26f));
                Assert.That(Vector3.Distance(cam.transform.position, target), Is.EqualTo(26f).Within(1e-3f));
                var toTarget = (target - cam.transform.position).normalized;
                Assert.That(Vector3.Dot(cam.transform.forward, toTarget), Is.EqualTo(1f).Within(1e-3f));

                rig.Take(cam, MetaCamera.Mode.Map, new Vector3(3f, 0f, 4f));
                Assert.That(rig.Pitch, Is.EqualTo(55f));
                Assert.That(rig.Yaw, Is.EqualTo(0f));
                Assert.That(rig.Zoom, Is.EqualTo(40f));
                Assert.That(Vector3.Distance(cam.transform.position, new Vector3(3f, 0f, 4f)), Is.EqualTo(40f).Within(1e-3f));
            }
            finally { Object.DestroyImmediate(cam.gameObject); }
        }

        [Test]
        public void Take_Twice_Keeps_The_Scene_Camera_Saved()
        {
            var cam = NewCam();
            try
            {
                var sceneRot = cam.transform.rotation;
                var rig = new MetaCamera();
                rig.Take(cam, MetaCamera.Mode.Orbit, Vector3.zero);
                for (int i = 0; i < 3; i++) rig.Tick(0.1f, false, i * 0.1f);
                rig.Take(cam, MetaCamera.Mode.Map, Vector3.zero);
                rig.Release();

                Assert.That(cam.transform.position, Is.EqualTo(new Vector3(5f, 7f, -9f)));
                Assert.That(cam.transform.rotation, Is.EqualTo(sceneRot));
                Assert.That(cam.fieldOfView, Is.EqualTo(17f));
                Assert.That(cam.cullingMask, Is.EqualTo(5));
            }
            finally { Object.DestroyImmediate(cam.gameObject); }
        }

        [Test]
        public void Take_On_A_Second_Camera_Gives_The_First_One_Back()
        {
            var a = NewCam();
            var b = new GameObject("meta-cam-b").AddComponent<Camera>();
            b.transform.SetPositionAndRotation(new Vector3(1f, 2f, 3f), Quaternion.Euler(4f, 5f, 6f));
            b.fieldOfView = 44f;
            b.cullingMask = 9;
            try
            {
                var aRot = a.transform.rotation;
                var bRot = b.transform.rotation;
                var rig = new MetaCamera();
                rig.Take(a, MetaCamera.Mode.Orbit, Vector3.zero);
                rig.Take(b, MetaCamera.Mode.Orbit, Vector3.zero);

                Assert.That(a.transform.position, Is.EqualTo(new Vector3(5f, 7f, -9f)), "Taking b gives a back");
                Assert.That(a.transform.rotation, Is.EqualTo(aRot));
                Assert.That(a.fieldOfView, Is.EqualTo(17f));
                Assert.That(a.cullingMask, Is.EqualTo(5));
                Assert.That(b.cullingMask, Is.EqualTo(~0));

                rig.Release();
                Assert.That(b.transform.position, Is.EqualTo(new Vector3(1f, 2f, 3f)));
                Assert.That(b.transform.rotation, Is.EqualTo(bRot));
                Assert.That(b.fieldOfView, Is.EqualTo(44f));
                Assert.That(b.cullingMask, Is.EqualTo(9));
            }
            finally { Object.DestroyImmediate(a.gameObject); Object.DestroyImmediate(b.gameObject); }
        }

        [Test]
        public void Release_Twice_Does_Not_Touch_The_Camera()
        {
            var cam = NewCam();
            try
            {
                var rig = new MetaCamera();
                rig.Take(cam, MetaCamera.Mode.Orbit, Vector3.zero);
                rig.Release();

                var handPos = new Vector3(100f, 200f, 300f);
                var handRot = Quaternion.Euler(1f, 2f, 3f);
                cam.transform.SetPositionAndRotation(handPos, handRot);
                cam.fieldOfView = 61f;
                cam.cullingMask = 13;

                rig.Release();

                Assert.That(cam.transform.position, Is.EqualTo(handPos));
                Assert.That(cam.transform.rotation, Is.EqualTo(handRot));
                Assert.That(cam.fieldOfView, Is.EqualTo(61f));
                Assert.That(cam.cullingMask, Is.EqualTo(13));
            }
            finally { Object.DestroyImmediate(cam.gameObject); }
        }

        [Test]
        public void Focus_Without_Animation_Moves_The_Target_At_Once()
        {
            var cam = NewCam();
            try
            {
                var rig = new MetaCamera();
                rig.Take(cam, MetaCamera.Mode.Map, Vector3.zero);
                var p = new Vector3(10f, 0f, 20f);
                rig.Focus(p, animate: false);
                Assert.That(rig.Target, Is.EqualTo(p));
                Assert.That(rig.GoalTarget, Is.EqualTo(p));

                rig.Take(cam, MetaCamera.Mode.Map, Vector3.zero);
                var takeTarget = rig.Target;
                rig.Focus(p, animate: true);
                Assert.That(rig.GoalTarget, Is.EqualTo(p));
                Assert.That(rig.Target, Is.EqualTo(takeTarget), "animate:true does not jump the target");

                float distBefore = Vector3.Distance(rig.Target, p);
                rig.Tick(0.1f, false, 1f);
                float distAfter = Vector3.Distance(rig.Target, p);
                Assert.That(distAfter, Is.LessThan(distBefore), "eases toward the goal");
                Assert.That(distAfter, Is.GreaterThan(0f), "does not overshoot past the goal");
            }
            finally { Object.DestroyImmediate(cam.gameObject); }
        }

        [Test]
        public void Zoom_Clamps_Per_Mode()
        {
            var cam = NewCam();
            try
            {
                var rig = new MetaCamera();
                rig.Take(cam, MetaCamera.Mode.Orbit, Vector3.zero);
                Assert.That(rig.ClampZoom(9f), Is.EqualTo(14f));
                Assert.That(rig.ClampZoom(99f), Is.EqualTo(44f));
                Assert.That(rig.ClampZoom(26f), Is.EqualTo(26f));

                rig.Take(cam, MetaCamera.Mode.Map, Vector3.zero);
                Assert.That(rig.ClampZoom(9f), Is.EqualTo(20f));
                Assert.That(rig.ClampZoom(999f), Is.EqualTo(110f));
            }
            finally { Object.DestroyImmediate(cam.gameObject); }
        }
    }
}
