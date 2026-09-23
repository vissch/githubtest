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
using System.Collections.Generic;
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
        /// Every shader global a biome PUSHES is a shader global a biome must PUT BACK.
        ///
        /// ClearBiome used to reset five of the seven PushBiome sets: _TWSnowColor and _TWHeatColor survived a
        /// change of field. That was safe only by accident - their consumers happen to be guarded by _TWSnow.x and
        /// _TWHeat.x, which are cleared - and an accident is not a design. The bug found in the same cycle was made
        /// of exactly this: TWLocalLights asked "is _TWSnowColor set" as a stand-in for "is there snow here", and on
        /// the lava field, which sets no snow colour and therefore carried the default WHITE one, every lantern
        /// faded to snow at the edge of its pool.
        ///
        /// Read out of the source because TW.Presentation.Terrain is not in this assembly's references (see the
        /// note on the test above), and because what is being checked is a source-level property anyway: these are
        /// two lists of names that have to match.
        /// </summary>
        [Test]
        public void EveryGlobalABiomePushesIsAGlobalABiomeClears()
        {
            var src = File.ReadAllText(Path.Combine(ProjectRoot, "Presentation", "Terrain", "Atmosphere.cs"));
            var pushed = GlobalsSetIn(src, "void PushBiome(");
            var cleared = GlobalsSetIn(src, "void ClearBiome(");
            Assert.Greater(pushed.Count, 4, "PushBiome's Shader.SetGlobal calls were found");
            Assert.Greater(cleared.Count, 4, "ClearBiome's Shader.SetGlobal calls were found");
            var leaked = pushed.Where(id => !cleared.Contains(id)).ToList();
            Assert.IsEmpty(leaked,
                "pushed by a biome and never put back, so it survives into the next field: " + string.Join(", ", leaked));
        }

        /// <summary>The Shader.SetGlobal* argument names inside one method, found by matching its braces.</summary>
        static List<string> GlobalsSetIn(string src, string signature)
        {
            int at = src.IndexOf(signature, System.StringComparison.Ordinal);
            Assert.Greater(at, 0, signature + " is in Atmosphere.cs");
            // The braces are written as character CODES, not as literals. The repo's own source validator counts
            // brace characters per file to catch a truncated edit, and a matcher that contains four unpaired ones
            // fails it - which is how this test first came back red.
            const char Open = (char)123, Close = (char)125;
            int open = src.IndexOf(Open, at);
            int depth = 0, end = open;
            for (int i = open; i < src.Length; i++)
            {
                if (src[i] == Open) depth++;
                else if (src[i] == Close && --depth == 0) { end = i; break; }
            }
            return Regex.Matches(src.Substring(open, end - open), @"Shader\.SetGlobal\w+\s*\(\s*(\w+)")
                .Cast<Match>().Select(m => m.Groups[1].Value).Distinct().ToList();
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
