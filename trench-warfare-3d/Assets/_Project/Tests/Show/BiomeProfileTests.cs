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

        // [TS1] A field is read only where some non-test .cs reads it THROUGH a BiomeProfile-typed holder.
        // The old check was a word match over the raw text of every .cs: comments, other classes' members, a
        // test's own use and writes all counted, so a dead field with a common name (Id, Dark, Flooding, Radius)
        // stayed green. The old declaration regex also listed only Color/float/bool/int/Biome and no comma
        // lists, so most of the profile - Name, Haze, Bank, KeyEuler, GradeHighs - was never even looked for.
        [Test]
        public void EveryFieldOnTheProfileIsReadBySomething()
        {
            Assert.IsTrue(File.Exists(ProfilePath), ProfilePath);
            var declared = DeclaredFields(File.ReadAllText(ProfilePath));
            // 58 public instance fields, counted by hand from BiomeProfile.cs on 2026-10-14.
            Assert.GreaterOrEqual(declared.Count, 55,
                "[TS1] the profile's fields were found, comma lists and all: " + declared.Count);
            foreach (var n in new[] { "Name", "Haze", "Bank", "KeyEuler", "GradeHighs" })
                Assert.Contains(n, declared, "[TS1] a field form the old regex dropped is now parsed: " + n);

            // The holders are worked out PER FILE, and a read only counts in the file its holder is declared in.
            // Pooled across the project the short names would let anything through: TankRenderer reads p.Radius
            // off a projectile, which is how a planted "public float Radius;" on the profile first came back green.
            var sources = Directory.EnumerateFiles(ProjectRoot, "*.cs", SearchOption.AllDirectories)
                // Read only by a test is dead for the game, which is half of why the old check could not fail.
                .Where(f => !f.Replace('\\', '/').Contains("/Tests/"))
                .Select(File.ReadAllText).Where(s => s.Contains("BiomeProfile")).ToList();

            // A BiomeProfile-typed MEMBER (Atmosphere.Profile, GreyboxTerrainView.Profile) is read from other
            // files too - PerfBench reads Atmosphere.Profile.Name - so those names hold anywhere.
            var members = sources.SelectMany(s =>
                    Regex.Matches(s, @"public\s+(?:static\s+)?BiomeProfile\s+(\w+)\s*(?:\{|=>|=)").Cast<Match>())
                .Select(m => m.Groups[1].Value).Distinct().ToList();
            Assert.Contains("Profile", members, "[TS1] the BiomeProfile-typed members were found");

            var scanned = 0;
            var read = new HashSet<string>();
            foreach (var src in sources)
            {
                scanned++;
                // Everything a BiomeProfile can be held in here: the parameter of Atmosphere.Apply/PushBiome,
                // the Atmosphere/GreyboxTerrainView Profile properties, any local assigned from BiomeProfile.*.
                var holders = Regex.Matches(src, @"BiomeProfile\s+(\w+)").Cast<Match>()
                    .Select(m => m.Groups[1].Value)
                    .Concat(Regex.Matches(src, @"var\s+(\w+)\s*=\s*[^;\n]*BiomeProfile\.").Cast<Match>()
                        .Select(m => m.Groups[1].Value))
                    .Concat(members).Distinct().ToList();
                if (holders.Count == 0) continue;
                var through = "(?:" + string.Join("|", holders.Select(Regex.Escape))
                    + @"|BiomeProfile\.For\([^)]*\))\.";
                // (?!\s*=[^=]) drops a write: "p.Haze = c" sets the field, it does not read it.
                foreach (var n in declared)
                    if (Regex.IsMatch(src, through + Regex.Escape(n) + @"\b(?!\s*=[^=])")) read.Add(n);
            }
            Assert.Greater(scanned, 5, "[TS1] the non-test sources that mention a BiomeProfile were found");
            var dead = declared.Where(n => !read.Contains(n)).ToList();
            Assert.IsEmpty(dead,
                "[TS1] declared on BiomeProfile and read by no non-test code, so a biome cannot change what they "
                + "name: " + string.Join(", ", dead));
        }

        /// <summary>
        /// The public instance field names on the profile, comma lists split out. Any type is accepted (string,
        /// Vector3 and Vector4 are all declared there); the non-field public members - the enum, the class, the
        /// static factories - have no "name[,name] =" or ";" tail and so never match.
        /// </summary>
        static List<string> DeclaredFields(string src)
        {
            var names = new List<string>();
            foreach (Match m in Regex.Matches(src, @"^\s*public\s+(\w+)\s+([\w\s,]+?)\s*[=;]",
                         RegexOptions.Multiline))
            {
                var type = m.Groups[1].Value;
                if (type == "enum" || type == "sealed" || type == "static" || type == "class"
                    || type == "const" || type == "readonly") continue;
                foreach (var n in m.Groups[2].Value.Split(','))
                    if (n.Trim().Length > 0 && !names.Contains(n.Trim())) names.Add(n.Trim());
            }
            return names;
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
