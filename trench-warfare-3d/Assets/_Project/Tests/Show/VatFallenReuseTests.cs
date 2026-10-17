// Phase: tooling (perf pass, 2026-10-17) — the near tier used to call FallenAt twice per corpse per frame (once for
// the frustum/distance test, once to build the drawn instance); the fallen loop now reuses the position the cull
// loop already had. Shadows used to be decided only inside the DrawnInfantry>0 branch, so a frame with no infantry
// drawn left DrawExtras reading last frame's decision; it is now decided every frame. The picture is the same
// either way, so nothing but this test would notice either regression coming back.
using System.IO;
using System.Text.RegularExpressions;
using NUnit.Framework;

namespace TW.Tests
{
    public class VatFallenReuseTests
    {
        const string FallenPath = "Assets/_Project/Presentation/Units/VATRenderer.Fallen.cs";
        const string RendererPath = "Assets/_Project/Presentation/Units/VATRenderer.cs";

        static string StripComments(string src) => Regex.Replace(src, @"//[^\n]*", "");

        [Test]
        public void TheNearTierReusesTheComputedPosition()
        {
            string src = StripComments(File.ReadAllText(FallenPath));
            var foreachMatch = Regex.Match(src, @"foreach \(int i in fallenByFigure\[k\]\)[^\n]*");
            Assert.That(foreachMatch.Success, Is.True, "the near loop's foreach statement was not found");
            Assert.That(foreachMatch.Value, Does.Not.Contain("FallenAt("),
                "the near loop calls FallenAt again for a man whose position was already computed by the cull loop");
            Assert.That(Regex.Matches(src, @"\bFallenAt\((?!in FallenMan)").Count, Is.EqualTo(1),
                "FallenAt must be called exactly once per fallen man per frame (the cull loop), not once more per near corpse");
        }

        [Test]
        public void TheShadowDecisionIsMadeEveryFrame()
        {
            string src = StripComments(File.ReadAllText(RendererPath));
            int assign = src.IndexOf("ShadowsThisFrame =");
            int guard = src.IndexOf("if (DrawnInfantry > 0)");
            Assert.That(assign, Is.GreaterThanOrEqualTo(0), "ShadowsThisFrame is never assigned");
            Assert.That(guard, Is.GreaterThanOrEqualTo(0), "the DrawnInfantry > 0 guard was not found");
            Assert.That(assign, Is.LessThan(guard),
                "ShadowsThisFrame must be decided before the DrawnInfantry > 0 guard, or a frame with no infantry drawn reads last frame's decision");
        }
    }
}
