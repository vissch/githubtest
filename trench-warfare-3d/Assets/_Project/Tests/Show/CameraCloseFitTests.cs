// Phase: the closest zoom (owner's two-view finding, 2026-10-16): TWO-VIEW 37-Bullfrog-b and 29-MarkIV-a, both shot
// at the rig's zoom 9, pitch 25 (above the camera's own ZoomMin of 6), show the Bullfrog cut off above the
// shoulders and the Mark IV down to one track. The camera framed the feet, so the taller the machine the more of
// it left the top of the screen. These tests hold the fix: at the closest zoom the whole machine fits with
// headroom (NearZoom, CloseFitHeight, CloseAimHeight in TacticalCamera.cs).
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class CameraCloseFitTests
    {
        const System.Reflection.BindingFlags Any = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Public;

        /// <summary>The margin the goal asks for: every corner of the machine's bounds stays this far inside the
        /// viewport's edges.</summary>
        const float Margin = 0.05f;

        static void CallLate(TacticalCamera tc)
        {
            var start = tc.GetType().GetMethod("Start", Any);
            start.Invoke(tc, null);
            var late = tc.GetType().GetMethod("LateUpdate", Any);
            late.Invoke(tc, null);
        }

        [Test]
        public void Bullfrog_Fits_Whole_At_The_Closest_Zoom()
        {
            InputFocus.Modal = true;
            try
            {
                var prefab = Resources.Load<GameObject>("Vehicles/Bullfrog/Bullfrog_LOD0");
                Assert.NotNull(prefab, "the Bullfrog's LOD0 model did not load from Resources");
                var model = Object.Instantiate(prefab);
                model.transform.localScale = Vector3.one * TankRenderer.BullfrogScale;

                var camGo = new GameObject("CameraCloseFitTests.Camera");
                var cam = camGo.AddComponent<Camera>();
                cam.aspect = 16f / 9f;
                var tc = camGo.AddComponent<TacticalCamera>();
                // no Host, so FollowBattle defaults to "behind your own men" (side 1) and turns the view
                // StandardYaw degrees for no reason: there is no battle to orient around for a lone machine, and the
                // two-view finding's own captures were shot at yaw 0.
                tc.StandardYaw = 0f;

                try
                {
                    var bounds = new Bounds();
                    bool any = false;
                    foreach (var r in model.GetComponentsInChildren<Renderer>())
                    {
                        if (!any) { bounds = r.bounds; any = true; }
                        else bounds.Encapsulate(r.bounds);
                    }
                    Assert.IsTrue(any, "the Bullfrog's model draws no renderers to measure");

                    tc.Frame(Vector2.zero, 0f);   // Frame clamps to NearZoom: as close as the camera will go
                    CallLate(tc);

                    Vector3[] corners =
                    {
                        new Vector3(bounds.min.x, bounds.min.y, bounds.min.z), new Vector3(bounds.max.x, bounds.min.y, bounds.min.z),
                        new Vector3(bounds.min.x, bounds.max.y, bounds.min.z), new Vector3(bounds.max.x, bounds.max.y, bounds.min.z),
                        new Vector3(bounds.min.x, bounds.min.y, bounds.max.z), new Vector3(bounds.max.x, bounds.min.y, bounds.max.z),
                        new Vector3(bounds.min.x, bounds.max.y, bounds.max.z), new Vector3(bounds.max.x, bounds.max.y, bounds.max.z),
                    };
                    for (int i = 0; i < corners.Length; i++)
                    {
                        Vector3 vp = cam.WorldToViewportPoint(corners[i]);
                        TestContext.Out.WriteLine($"corner {i} {corners[i]} -> viewport {vp}");
                        Assert.Greater(vp.z, 0f, $"corner {i} of the Bullfrog's bounds is behind the camera at the closest zoom ({vp})");
                        Assert.That(vp.x, Is.InRange(Margin, 1f - Margin), $"corner {i} of the Bullfrog's bounds is off screen left/right at the closest zoom ({vp})");
                        Assert.That(vp.y, Is.InRange(Margin, 1f - Margin), $"corner {i} of the Bullfrog's bounds is off screen top/bottom at the closest zoom ({vp}): the top of the machine leaves the frame");
                    }
                }
                finally
                {
                    Object.DestroyImmediate(camGo);
                    Object.DestroyImmediate(model);
                }
            }
            finally { InputFocus.Modal = false; }
        }

        [Test]
        public void The_Standard_View_Keeps_Its_Distance()
        {
            InputFocus.Modal = true;
            try
            {
                var camGo = new GameObject("CameraCloseFitTests.Camera");
                var cam = camGo.AddComponent<Camera>();
                var tc = camGo.AddComponent<TacticalCamera>();
                try
                {
                    tc.Frame(Vector2.zero, 30f);
                    CallLate(tc);
                    float dist = Vector3.Distance(camGo.transform.position, Vector3.zero);
                    TestContext.Out.WriteLine($"distance at the standard view: {dist:0.00} m");
                    Assert.AreEqual(78.12f, dist, 0.1f, "the standard view's distance moved: the near-end fix reached a zoom it should not have touched");
                }
                finally { Object.DestroyImmediate(camGo); }
            }
            finally { InputFocus.Modal = false; }
        }
    }
}
