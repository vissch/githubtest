// Phase: tooling (code review 2026-10-04, E14 and E9). Two faults of the editor tools that nothing could see from the
// outside: a failed build still rewrote build-info.json with the new git sha while the old exe stayed beside it, and
// the VAT bake's "does this clip bind" probe only watched the hips and a shin, which on the 5 composed clips come from
// the fixed lower clip at a fixed time, so every composed clip was reported unbound and a real unbound one hid in the log.
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using NUnit.Framework;
using UnityEditor.Build.Reporting;
using UnityEngine;
using TW.Editor;

namespace TW.Tests
{
    public class EditorToolFixTests
    {
        [Test]
        public void AFailedBuildKeepsTheOldBuildInfo()   // [E14]
        {
            var m = typeof(BuildWindows).GetMethod("WriteInfo", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);
            Assert.IsNotNull(m, "[E14] BuildWindows.WriteInfo is gone");
            var ps = m.GetParameters();
            Assert.AreEqual(typeof(BuildResult), ps[2].ParameterType,
                "[E14] WriteInfo does not take the build result, so it cannot refuse a failed build");

            string dir = Path.Combine(Path.GetTempPath(), "tw-e14-" + System.Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(dir);
            string info = Path.Combine(dir, "build-info.json");
            try
            {
                File.WriteAllText(info, "{\"git_sha\":\"old\"}");
                object failed = m.Invoke(null, new object[] { dir, false, BuildResult.Failed, 0L, 1.0, new List<string>() });
                Assert.IsFalse((bool)failed, "[E14] WriteInfo claimed it wrote the info for a failed build");
                StringAssert.Contains("old", File.ReadAllText(info),
                    "[E14] a failed build overwrote build-info.json while the old exe was still there");

                object ok = m.Invoke(null, new object[] { dir, false, BuildResult.Succeeded, 0L, 1.0, new List<string>() });
                Assert.IsTrue((bool)ok, "[E14] WriteInfo refused a build that succeeded");
                string wrote = File.ReadAllText(info);
                Assert.IsFalse(wrote.Contains("\"old\""), "[E14] a build that succeeded did not rewrite build-info.json");
                StringAssert.Contains("\"result\": \"Succeeded\"", wrote, "[E14] build-info.json does not name the result");
            }
            finally { Directory.Delete(dir, true); }
        }

        [Test]
        public void AComposedClipBindsThroughItsUpperBody()   // [E9]
        {
            var v = new Vector3(0.1f, -0.4f, 0f);
            Assert.IsTrue(VATBaker.Bound(Quaternion.identity, Quaternion.identity, v, v,
                    Quaternion.identity, Quaternion.Euler(0f, 10f, 0f), 1f),
                "[E9] a composed clip whose upper body moves is still reported unbound");
            Assert.IsFalse(VATBaker.Bound(Quaternion.identity, Quaternion.identity, v, v,
                    Quaternion.identity, Quaternion.identity, 1f),
                "[E9] a clip that does not move at all is reported bound");
            Assert.IsTrue(VATBaker.Bound(Quaternion.identity, Quaternion.Euler(0f, 1f, 0f), v, v,
                    Quaternion.identity, Quaternion.identity, 1f),
                "[E9] the hips turning no longer counts as bound");
        }

        [Test]
        public void ADroppedShotSaysSo()   // [E15]
        {
            var was = new List<Camera>();
            foreach (var c in Camera.allCameras) { was.Add(c); c.enabled = false; }
            var go = new GameObject("e15-rig");
            try
            {
                var rig = go.AddComponent<CaptureRig.Rig>();
                var render = typeof(CaptureRig.Rig).GetMethod("Render", BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
                Assert.IsNotNull(render, "[E15] CaptureRig.Rig.Render is gone");
                string path = Path.Combine(Path.GetTempPath(), "e15-dropped.png");
                var shot = new CaptureRig.Rig.Shot { Path = path, W = 8, H = 8 };
                UnityEngine.TestTools.LogAssert.Expect(LogType.Error, new System.Text.RegularExpressions.Regex("e15-dropped"));
                render.Invoke(rig, new object[] { shot });
                Assert.IsFalse(File.Exists(path), "[E15] the test wrote a picture, so Camera.main was not null after all");
            }
            finally
            {
                Object.DestroyImmediate(go);
                foreach (var c in was) if (c != null) c.enabled = true;
            }
        }

        [Test]
        public void TheCrowdLineSaysHowManyMenStoodThere()   // [E15]
        {
            // the value comes from ShotCrowd's own promise: "the focus it chose and how many men were standing in that square"
            Assert.AreEqual("queued a.png on the crowd at 10.0, 20.0 (7 men)",
                CaptureRig.CrowdLine("d/a.png", new Vector2(10f, 20f), 7),
                "[E15] ShotCrowd's answer does not say how many men stood in the square");
        }

        [Test]
        public void ASheetTilesOnlyTheFramesOfItsOwnSeries()   // [E15]
        {
            string dir = Path.Combine(Path.GetTempPath(), "tw-e15-" + System.Guid.NewGuid().ToString("N"));
            Directory.CreateDirectory(dir);
            var png = new Texture2D(8, 8, TextureFormat.RGB24, false);
            try
            {
                var bytes = png.EncodeToPNG();
                foreach (var name in new[] { "s_00.png", "s_01.png", "s_sheet.png" })
                    File.WriteAllBytes(Path.Combine(dir, name), bytes);
                string answer = CaptureRig.Sheet(dir, "s", Path.Combine(dir, "out.png"), 2, 16);
                StringAssert.Contains("sheet of 2", answer,
                    "[E15] the sheet tiled in a file that only starts with the stem: " + answer);
            }
            finally { Object.DestroyImmediate(png); Directory.Delete(dir, true); }
        }

        [Test]
        public void ASecondProfileDoesNotTakeOverTheFirst()   // [E15]
        {
            Assert.AreEqual(typeof(string), typeof(CaptureRig.Profiler).GetMethod("Begin").ReturnType,
                "[E15] Profiler.Begin cannot refuse: it answers nothing");
            var go = new GameObject("e15-prof");
            try
            {
                var prof = go.AddComponent<CaptureRig.Profiler>();
                Assert.IsNull(prof.Begin("a.json", 3), "[E15] a free profiler refused the first run");
                string busy = prof.Begin("b.json", 3);
                Assert.IsNotNull(busy, "[E15] a second Begin overwrote five live recorders without disposing them");
                StringAssert.Contains("a.json", busy, "[E15] the refusal does not name the run already in flight");
            }
            finally { Object.DestroyImmediate(go); }
        }

        [Test]
        public void TheFilmTextureGoesWithTheBench()   // [E15]
        {
            var go = new GameObject("e15-bench");
            var tex = new Texture2D(8, 8, TextureFormat.RGB24, false);
            try
            {
                var bench = go.AddComponent<RiderLab.Bench>();
                var f = typeof(RiderLab.Bench).GetField("tex", BindingFlags.Instance | BindingFlags.NonPublic);
                Assert.IsNotNull(f, "[E15] RiderLab.Bench.tex is gone");
                f.SetValue(bench, tex);
                // OnDestroy is called directly: Unity refuses a DestroyImmediate made while it is tearing the
                // GameObject down, so destroying the object here would hide the fix rather than show it.
                var d = typeof(RiderLab.Bench).GetMethod("OnDestroy", BindingFlags.Instance | BindingFlags.NonPublic);
                Assert.IsNotNull(d, "[E15] RiderLab.Bench.OnDestroy is gone");
                d.Invoke(bench, null);
                Assert.IsTrue(tex == null, "[E15] the bench left its film texture behind");
                Assert.IsNull(f.GetValue(bench), "[E15] the bench still holds its destroyed film texture");
            }
            finally
            {
                if (go != null) Object.DestroyImmediate(go);
                if (tex != null) Object.DestroyImmediate(tex);
                typeof(RiderLab.Bench).GetField("Instance", BindingFlags.Static | BindingFlags.Public)?.SetValue(null, null);
            }
        }
    }
}
