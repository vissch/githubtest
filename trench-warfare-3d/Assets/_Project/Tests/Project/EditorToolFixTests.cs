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
    }
}
