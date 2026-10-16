// Phase: maintenance (2026-10-16) — the one place the camera's zoom is read, and a ratchet so the idiom is not
// written out again. VATRenderer still carries its own copy until its lane lands (the same arrangement
// ViewGroundTests has for Rain and NightLights); take it off Allowed when it moves to ViewEye.
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;

namespace TW.Tests
{
    public class ViewEyeTests
    {
        static readonly string[] Allowed = { "ViewEye.cs", "VATRenderer.cs" };

        /// <summary>A camera that reports a zoom and counts who asked.</summary>
        sealed class CountingZoom : MonoBehaviour, IZoomSource
        {
            public int Reads;
            public float CurrentZoom { get { Reads++; return 42f; } }
        }

        GameObject eye;
        Camera lens;
        CountingZoom source;
        readonly List<Camera> switchedOff = new List<Camera>();

        [SetUp]
        public void SetUp()
        {
            // Camera.main picks any enabled camera tagged MainCamera, and the EditMode scene has one of its own:
            // switch the others off for this test so the camera under test is the one it finds.
            switchedOff.Clear();
            foreach (var other in Camera.allCameras)
                if (other.CompareTag("MainCamera")) { other.enabled = false; switchedOff.Add(other); }

            eye = new GameObject("test camera", typeof(Camera));
            eye.tag = "MainCamera";
            lens = eye.GetComponent<Camera>();
            source = eye.AddComponent<CountingZoom>();
            ViewEye.Invalidate();
        }

        [TearDown]
        public void TearDown()
        {
            // EditMode holds Time.frameCount still, so a cached read would leak into the next test
            if (eye != null) Object.DestroyImmediate(eye);
            foreach (var other in switchedOff) if (other != null) other.enabled = true;
            switchedOff.Clear();
            ViewEye.Invalidate();
        }

        [Test]
        public void The_Zoom_Is_Read_Once_A_Frame_However_Often_It_Is_Asked_For()
        {
            Assert.That(Camera.main, Is.SameAs(lens), "the camera under test is the one Camera.main finds");

            Assert.That(ViewEye.Zoom, Is.EqualTo(42f), "the camera's zoom");
            Assert.That(ViewEye.Zoom, Is.EqualTo(42f), "and the same the second time");
            Assert.That(ViewEye.Zoom, Is.EqualTo(42f), "and the third");
            Assert.That(ViewEye.Main, Is.SameAs(lens), "the main camera comes off the same read");
            Assert.That(source.Reads, Is.EqualTo(1), "the zoom is asked of the camera once a frame, not once an ask");
        }

        [Test]
        public void No_Presentation_File_Looks_The_Zoom_Source_Up_Again()
        {
            var root = Path.Combine(Application.dataPath, "_Project", "Presentation");
            var copy = new Regex(@"TryGetComponent<\s*IZoomSource\s*>");
            var found = Directory.GetFiles(root, "*.cs", SearchOption.AllDirectories)
                .Where(f => !Allowed.Contains(Path.GetFileName(f)) && copy.IsMatch(File.ReadAllText(f)))
                .Select(Path.GetFileName).ToList();
            Assert.That(found, Is.Empty, "read the zoom through ViewEye.Zoom");
        }
    }
}
