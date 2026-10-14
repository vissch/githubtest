// Phase: tooling (code review 2026-10-04, E5). CaptureRig.Render restores the camera's targetTexture before it
// measures the still, so the readability metrics (contrast_median, men_in_frame) projected every man with the Game
// view's viewport instead of the shot's: wrong whenever the Game view was not the shape of the picture, and
// workflow.md tells agents to judge a frame by those numbers.
using NUnit.Framework;
using UnityEngine;
using TW.Editor;

namespace TW.Tests
{
    public class CaptureRigMetricsTests
    {
        Camera cam;
        RenderTexture rt;

        [SetUp]
        public void Make()
        {
            cam = new GameObject("E5Cam").AddComponent<Camera>();
            cam.transform.SetPositionAndRotation(Vector3.zero, Quaternion.identity);
            cam.fieldOfView = 60f;
            cam.nearClipPlane = 0.3f;
            cam.farClipPlane = 1000f;
        }

        [TearDown]
        public void Clean()
        {
            if (cam != null) Object.DestroyImmediate(cam.gameObject);
            if (rt != null) Object.DestroyImmediate(rt);
        }

        [Test]
        public void ShotPointUsesTheStillsAspectNotTheGameViews()   // [E5]
        {
            // A vertical fov of 60 degrees puts the half-height at z = 1 at tan(30) = 0.57735. The half-width is that
            // times the aspect, so the same point (0.5, 0, 1) sits at a different fraction across a wide picture than
            // across a square-ish one: 16:9 -> half-width 1.02641, x fraction 0.74357 of 1600 px; 4:3 -> half-width
            // 0.76980, x fraction 0.82476 of 800 px. Both from the fov, not from a run of the rig.
            Vector3 wide = CaptureRig.ShotPoint(cam, new Vector3(0.5f, 0f, 1f), 1600, 900);
            Assert.AreEqual(1189.71f, wide.x, 0.5f, "[E5] the 16:9 still does not place the man where its own aspect says");
            Assert.AreEqual(450f, wide.y, 0.5f, "[E5] the 16:9 still does not place the man where its own aspect says");

            Vector3 narrow = CaptureRig.ShotPoint(cam, new Vector3(0.5f, 0f, 1f), 800, 600);
            Assert.AreEqual(659.81f, narrow.x, 0.5f, "[E5] the 4:3 still used the wide aspect: the metrics measure the wrong pixels");
            Assert.AreEqual(300f, narrow.y, 0.5f, "[E5] the 4:3 still used the wide aspect: the metrics measure the wrong pixels");

            // and the camera's own viewport must not enter the sum at all
            rt = new RenderTexture(320, 240, 24);
            cam.targetTexture = rt;
            Vector3 again = CaptureRig.ShotPoint(cam, new Vector3(0.5f, 0f, 1f), 1600, 900);
            Assert.AreEqual(wide.x, again.x, 0.001f, "[E5] the camera's viewport moved the answer; the still's size must decide it");
            Assert.AreEqual(wide.y, again.y, 0.001f, "[E5] the camera's viewport moved the answer; the still's size must decide it");
        }
    }
}
