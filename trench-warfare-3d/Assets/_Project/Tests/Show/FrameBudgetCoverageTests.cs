// Phase: tooling (AOSA C31, 2026-09-25) - FrameBudget sees every gameplay draw. TW.Presentation.FrameBudget counts
// the frame's submissions only because every submitter goes through it. The first version of the counter could not see
// six of nine submitters, and on this branch four more (VATRenderer, TankRenderer, BattlefieldProps, PropDestruction)
// plus SelectionMarkers still called Graphics.* directly: frame_budget_draws read 41 against Unity's 331. A counter that
// misses a submitter rewards adding cost there, and nothing in the picture shows it. So this test reads the source:
// outside RenderGround.cs (FrameBudget itself) and DebugOverlay.cs (debug gizmos, deliberately uncounted), no file in
// Presentation or UI may call a Graphics draw API or a CommandBuffer draw directly.
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using NUnit.Framework;

namespace TW.Tests
{
    public class FrameBudgetCoverageTests
    {
        static readonly string[] Roots = {
            "Assets/_Project/Presentation", "Assets/_Project/UI",
            "Assets/_Project/Playground", "Assets/_Project/Perf"
        };
        // [TS9] relative paths, matched against the scanned path (\ turned into /) with EndsWith, not a filename
        // compare: a same-named file elsewhere in the tree must not slip through unseen.
        static readonly string[] Allowed = {
            "Assets/_Project/Presentation/Core/RenderGround.cs",
            "Assets/_Project/Presentation/Camera/DebugOverlay.cs"
        };

        // Graphics.RenderMesh*, RenderPrimitives*, DrawMesh*, DrawProcedural*, DrawTexture...; a CommandBuffer's
        // DrawMesh*, DrawProcedural*, DrawRenderer*, DrawMultipleMeshes; the using-static that would hide either;
        // and [TS9] the using-alias form (using G = UnityEngine.Graphics;) that hides every call behind it too.
        static readonly Regex DrawApi = new Regex(
            @"\bGraphics\s*\.\s*(?:Render|Draw)\w*\s*\(" +
            @"|\.\s*(?:DrawMesh|DrawProcedural|DrawRenderer|DrawMultipleMeshes)\w*\s*\(" +
            @"|using\s+static\s+UnityEngine\.Graphics\b" +
            @"|using\s+\w+\s*=\s*UnityEngine\.Graphics\s*;");

        /// <summary>The source with comments blanked, newlines kept so a match's line number is still right.</summary>
        static string Code(string path)
        {
            string src = File.ReadAllText(path);
            src = Regex.Replace(src, @"/\*.*?\*/", m => Regex.Replace(m.Value, @"[^\n]", " "), RegexOptions.Singleline);
            return Regex.Replace(src, @"//[^\n]*", "");
        }

        static int LineOf(string s, int index)
        {
            int line = 1;
            for (int i = 0; i < index; i++) if (s[i] == '\n') line++;
            return line;
        }

        [Test]
        public void EveryDrawGoesThroughFrameBudget()
        {
            // [TS9] an allow entry that no longer exists would widen the scan silently (a rename or a typo)
            foreach (string allow in Allowed)
                Assert.That(File.Exists(allow), Is.True, allow + " does not exist: Allowed would widen the scan silently");

            var direct = new List<string>();
            int scanned = 0;
            foreach (string root in Roots)
            {
                Assert.That(Directory.Exists(root), Is.True, root + " is missing: the test would pass by reading nothing");
                foreach (string rawPath in Directory.GetFiles(root, "*.cs", SearchOption.AllDirectories))
                {
                    string path = rawPath.Replace(System.IO.Path.DirectorySeparatorChar, '/');
                    bool allowed = false;
                    foreach (string allow in Allowed) if (path.EndsWith(allow)) { allowed = true; break; }
                    if (allowed) continue;
                    scanned++;
                    string code = Code(path);
                    foreach (Match m in DrawApi.Matches(code))
                        direct.Add(path + ":" + LineOf(code, m.Index) + "  " + m.Value.Trim());
                }
            }
            Assert.That(scanned, Is.GreaterThan(20), "the scan found almost no source; the roots moved");
            Assert.That(direct, Is.Empty,
                "these submit draws that FrameBudget cannot see, so frame_budget_draws and rule 7 are blind to them. " +
                "Call FrameBudget.Draw / DrawIndirect with the same arguments (add an overload to FrameBudget if the " +
                "signature is missing):\n" + string.Join("\n", direct));
        }

        [Test]
        public void ThePatternSeesFrameBudgetsOwnCalls()
        {
            // proves the regex still matches the real calls, so an empty result above means clean, not blind
            string code = Code("Assets/_Project/Presentation/Core/RenderGround.cs");
            Assert.That(DrawApi.Matches(code).Count, Is.GreaterThanOrEqualTo(3),
                "FrameBudget's own RenderMeshInstanced / RenderMeshIndirect / RenderMesh calls no longer match the pattern");
        }
    }
}
