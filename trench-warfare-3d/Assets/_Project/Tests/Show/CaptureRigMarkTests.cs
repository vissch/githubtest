// Phase: frog-death-flop-r3 (2026-10-19). Proof for CaptureRig.MarkPixels, the projection every marked still's
// sidecar reports (mark_px, mark_py, mark_px_per_m) and a probe reads its crop from. Critic r1 (12/100) threw out a
// set whose numbers came from a box guessed by hand, so this arithmetic is held by the pinhole formula worked out
// here, not by the code under test: a camera 10 m up with a 60 deg vertical fov sees 2 * 10 * tan(30 deg) m across
// the 900 px of its height, so a metre there is 900 / (2 * 10 * tan(30 deg)) = 77.94 px, and the point it looks
// straight down at sits at (800, 450) of a 1600x900 image.
using NUnit.Framework;
using UnityEngine;
using TW.Editor;

namespace TW.Tests
{
    public class CaptureRigMarkTests
    {
        Camera cam;
        RenderTexture rt;

        [SetUp]
        public void Up()
        {
            rt = new RenderTexture(1600, 900, 0);
            cam = new GameObject("mark test cam").AddComponent<Camera>();
            cam.enabled = false;                         // never renders: only its projection is asked for
            cam.targetTexture = rt;                      // so pixelWidth/Height are 1600x900, not the game view's
            cam.fieldOfView = 60f;
            cam.transform.position = new Vector3(0f, 10f, 0f);
            cam.transform.rotation = Quaternion.Euler(90f, 0f, 0f);   // straight down
        }

        [TearDown]
        public void Down()
        {
            if (cam != null) UnityEngine.Object.DestroyImmediate(cam.gameObject);
            if (rt != null) UnityEngine.Object.DestroyImmediate(rt);
        }

        [Test]
        public void ThePointUnderTheCameraIsTheCentreOfTheImage()
        {
            bool seen = CaptureRig.MarkPixels(cam, Vector3.zero, 1600, 900, out float px, out float py, out float perM);
            Assert.That(seen, Is.True);
            Assert.That(px, Is.EqualTo(800f).Within(0.5f));
            Assert.That(py, Is.EqualTo(450f).Within(0.5f));
            Assert.That(perM, Is.EqualTo(900f / (2f * 10f * Mathf.Tan(30f * Mathf.Deg2Rad))).Within(0.5f));
            Assert.That(perM, Is.EqualTo(77.94f).Within(0.5f));
        }

        /// <summary>y counts DOWN from the top, the order PIL reads a PNG in: a point 2 m further along +z, which the
        /// camera's own up points at, is HIGHER in the image, so its py is smaller.</summary>
        [Test]
        public void YCountsDownFromTheTopOfTheImage()
        {
            CaptureRig.MarkPixels(cam, Vector3.zero, 1600, 900, out _, out float py0, out float perM);
            bool seen = CaptureRig.MarkPixels(cam, new Vector3(0f, 0f, 2f), 1600, 900, out float px, out float py, out _);
            Assert.That(seen, Is.True);
            Assert.That(px, Is.EqualTo(800f).Within(0.5f));
            Assert.That(py, Is.EqualTo(py0 - 2f * perM).Within(1f));
        }

        /// <summary>Behind the camera, and off the side of the image: not in frame, so a still can be failed on it.</summary>
        [Test]
        public void OutsideTheImageIsNotInFrame()
        {
            Assert.That(CaptureRig.MarkPixels(cam, new Vector3(0f, 30f, 0f), 1600, 900, out _, out _, out _), Is.False);
            Assert.That(CaptureRig.MarkPixels(cam, new Vector3(40f, 0f, 0f), 1600, 900, out _, out _, out _), Is.False);
        }

        [Test]
        public void NoCameraIsNotInFrame()
        {
            Assert.That(CaptureRig.MarkPixels(null, Vector3.zero, 1600, 900, out _, out _, out _), Is.False);
        }
    }
}
