// Phase: maintenance (2026-09-27) — the one formula for where the camera's view meets the ground, and a ratchet so
// it is not copied again. Rain and NightLights still carry their own copy until their lanes land (they are being
// edited on other branches); take them off Allowed when you move them to ViewGround.
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;

namespace TW.Tests
{
    public class ViewGroundTests
    {
        static readonly string[] Allowed = { "ViewGround.cs", "Rain.cs", "NightLights.cs" };

        [Test]
        public void A_Lens_Looking_Down_Meets_The_Ground_Where_Trigonometry_Says()
        {
            var go = new GameObject("lens");
            try
            {
                go.transform.position = new Vector3(3f, 50f, -7f);
                go.transform.rotation = Quaternion.Euler(45f, 30f, 0f);
                Assert.That(ViewGround.Distance(go.transform), Is.EqualTo(50f / Mathf.Sin(45f * Mathf.Deg2Rad)).Within(1e-3f));
                Assert.That(ViewGround.Point(go.transform).y, Is.EqualTo(0f).Within(1e-3f), "the point is on the ground plane");

                go.transform.rotation = Quaternion.Euler(8f, 0f, 0f);   // TacticalCamera's flattest pitch
                Assert.That(ViewGround.Distance(go.transform), Is.EqualTo(50f / 0.15f).Within(1e-2f), "clamped at MinDrop");
                Assert.That(ViewGround.Distance(go.transform, 0.12f), Is.EqualTo(50f / Mathf.Sin(8f * Mathf.Deg2Rad)).Within(1e-2f),
                    "Atmosphere's 0.12 does not clamp at 8 degrees");
            }
            finally { Object.DestroyImmediate(go); }
        }

        [Test]
        public void No_Presentation_File_Writes_The_Formula_Out_Again()
        {
            var root = Path.Combine(Application.dataPath, "_Project", "Presentation");
            var copy = new Regex(@"Mathf\.Max\([^;]*-[\w.]*forward\.y\)");
            var found = Directory.GetFiles(root, "*.cs", SearchOption.AllDirectories)
                .Where(f => !Allowed.Contains(Path.GetFileName(f)) && copy.IsMatch(File.ReadAllText(f)))
                .Select(Path.GetFileName).ToList();
            Assert.That(found, Is.Empty, "use ViewGround.Distance / Point / Along instead of a copy of its formula");
        }
    }
}
