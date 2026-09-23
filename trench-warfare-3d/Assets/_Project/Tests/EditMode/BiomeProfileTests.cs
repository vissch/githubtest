// Phase: B8 - every value a biome declares is actually read by something.
//
// Why this exists: BiomeProfile.GlowScale was declared, given a nine-line docstring explaining exactly what it
// was for, set to 0.55 on lava and 0.8 on winter, and READ BY NOTHING. It sat dead through a gate of 191 tests
// and through several rounds of "the lava field blows out", which is the problem it was written to solve.
// Nothing noticed, because nothing in the suite touched a biome at all.
//
// A field on that profile is a promise that a biome can change the thing it names. An unread field is a promise
// the code does not keep, and it fails invisibly: the look stays wrong in a way that tuning the OTHER values
// cannot fix, because the value being tuned never arrives anywhere.
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;

namespace TW.Tests
{
    public class BiomeProfileTests
    {
        static string ProjectRoot => Path.GetFullPath(Path.Combine(Application.dataPath, "_Project"));
        static string ProfilePath => Path.Combine(ProjectRoot, "Presentation", "Terrain", "BiomeProfile.cs");

        [Test]
        public void EveryFieldOnTheProfileIsReadBySomething()
        {
            Assert.IsTrue(File.Exists(ProfilePath), ProfilePath);
            var declared = Regex.Matches(File.ReadAllText(ProfilePath),
                    @"^\s*public\s+(?:Color|float|bool|int|Biome)\s+(\w+)\s*[=;]", RegexOptions.Multiline)
                .Cast<Match>().Select(m => m.Groups[1].Value).Distinct().ToList();
            Assert.Greater(declared.Count, 15, "the profile's fields were found");

            var body = new StringBuilder();
            foreach (var file in Directory.EnumerateFiles(ProjectRoot, "*.cs", SearchOption.AllDirectories))
            {
                var name = Path.GetFileName(file);
                if (name == "BiomeProfile.cs" || name == "BiomeProfileTests.cs") continue;
                body.Append(File.ReadAllText(file)).Append('\n');
            }
            var all = body.ToString();
            var dead = declared.Where(n => !Regex.IsMatch(all, @"\b" + Regex.Escape(n) + @"\b")).ToList();
            Assert.IsEmpty(dead,
                "declared on BiomeProfile and read by nothing, so a biome cannot change what they name: "
                + string.Join(", ", dead));
        }

        /// <summary>
        /// Effects drawn in another assembly read the tint set, so clearing a biome has to put the night colours
        /// back rather than leave the last field's behind for whatever loads next - and it has to bump the epoch,
        /// or consumers never repaint and the old biome's colours survive a scene change.
        /// </summary>
        [Test]
        public void ClearingTheBiomeRestoresTheDefaultTints_AndTellsConsumersToRepaint()
        {
            var hot = new SceneTints.Set
            {
                Splash = new Color(1.00f, 0.46f, 0.12f), Column = new Color(0.22f, 0.19f, 0.18f),
                Dust = new Color(0.24f, 0.20f, 0.19f), Smoke = new Color(0.28f, 0.17f, 0.20f), Glow = 0.55f,
            };
            try
            {
                int start = SceneTints.Epoch;
                SceneTints.Push(hot);
                Assert.AreEqual(hot.Splash, SceneTints.Now.Splash, "the push took");
                Assert.Greater(SceneTints.Epoch, start, "a push is a change");

                int before = SceneTints.Epoch;
                SceneTints.Reset();
                Assert.AreEqual(SceneTints.Default.Splash, SceneTints.Now.Splash);
                Assert.AreEqual(SceneTints.Default.Smoke, SceneTints.Now.Smoke);
                Assert.AreEqual(1f, SceneTints.Now.Glow, "glow goes back to neutral, not to the last biome's");
                Assert.Greater(SceneTints.Epoch, before, "a reset is a change: consumers must be told to repaint");
            }
            finally { SceneTints.Reset(); }
        }
    }
}
