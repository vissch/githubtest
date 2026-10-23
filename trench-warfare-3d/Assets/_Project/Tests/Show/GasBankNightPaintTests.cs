// Phase: ability-fx-captures-r2-3-g2-g2-g2-g2 (2026-10-23) - the moon must not light the gas bank, and must not
// stop lighting anything else. The gas bank's drawn colour was its paint times the night key (0.56, 0.70, 1.0),
// through both of the shader's colour bands (Flipbook_URP.shader, the light band's _MainLightColor and the shade
// band's mood tint), so each capture seed landed on its own hue. NightGas paints it the way NightSmoke paints the
// Burst: no ink in the light band, a plain grey shade, no mood - a scalar on the paint, so the pixel's hue is the
// paint's hue. Lives in Tests/Show (not beside GasBankCards in Tests/Stills: that module is EXPLICIT_ONLY).
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class GasBankNightPaintTests
    {
        /// <summary>
        /// The expected values come from a measurement, not from the code: on the committed s0 still the card's own
        /// colour, read over the top 2% of plate difference, is (71.6, 87.2, 61.1) where the paint's ratio at that
        /// luma is (81, 90, 23) - the blue channel arrives 2.7x too high, which is the whole hue gap.
        /// It is a GAS-ONLY change: every other book's paint here is "unchanged from the build before this commit",
        /// snapshotted from the same instance before NightGas runs. The captures in
        /// docs/evidence/ability-fx-captures-r2-3-g2-g2-g2-g2 prove the look; this guards the blast radius.
        /// </summary>
        [Test]
        public void TheMoonDoesNotLightTheGasBank()
        {
            var fx = new FlipbookFx();
            var field = typeof(FlipbookFx).GetField("mats", BindingFlags.NonPublic | BindingFlags.Instance);
            var mats = (Material[])field.GetValue(fx);
            try
            {
                Assume.That(fx.Ready, "the flipbook shader and sheets load in the editor");

                var levels = new Vector4[mats.Length];
                var shade = new Color[mats.Length];
                var tint = new Color[mats.Length];
                var mood = new float[mats.Length];
                var lit = new float[mats.Length];
                for (int b = 0; b < mats.Length; b++)
                {
                    levels[b] = mats[b].GetVector("_Levels");
                    shade[b] = mats[b].GetColor("_Shade");
                    tint[b] = mats[b].GetColor("_Tint");
                    mood[b] = mats[b].GetFloat("_ShadeMood");
                    lit[b] = mats[b].GetFloat("_Lit");
                }

                fx.NightGas(CombatFx.GasBankNight);

                var gas = mats[(int)FlipbookFx.Book.GasBank];
                Assert.GreaterOrEqual(gas.GetVector("_Levels").x, 1f, "no ink reaches the light band, so the moon (_MainLightColor) is never read");
                Assert.AreEqual(0f, gas.GetFloat("_ShadeMood"), "and no night-blue shade tint either");
                var g = gas.GetColor("_Shade");
                Assert.AreEqual(g.r, g.g, 1e-5f, "the shade is a plain grey, so it scales the paint instead of tinting it");
                Assert.AreEqual(g.r, g.b, 1e-5f, "the shade is a plain grey, so it scales the paint instead of tinting it");
                Assert.AreEqual(FlipbookFx.NightLit, gas.GetFloat("_Lit"), 1e-5f);
                Assert.GreaterOrEqual(gas.GetFloat("_Lit"), 0.5f, "NightLit stays in the shader's alpha-blended branch");
                Assert.AreEqual(CombatFx.GasBankNight, gas.GetColor("_Tint"), "PaintNight sets the paint it was given");

                for (int b = 0; b < mats.Length; b++)
                {
                    if ((FlipbookFx.Book)b == FlipbookFx.Book.GasBank) continue;
                    string who = ((FlipbookFx.Book)b) + " draws as it did before this commit";
                    Assert.AreEqual(levels[b], mats[b].GetVector("_Levels"), who);
                    Assert.AreEqual(shade[b], mats[b].GetColor("_Shade"), who);
                    Assert.AreEqual(tint[b], mats[b].GetColor("_Tint"), who);
                    Assert.AreEqual(mood[b], mats[b].GetFloat("_ShadeMood"), 1e-5f, who);
                    Assert.AreEqual(lit[b], mats[b].GetFloat("_Lit"), 1e-5f, who);
                }
            }
            finally
            {
                // DestroyImmediate: Dispose's Object.Destroy is not allowed in edit mode (ColumnLightTests' lesson)
                foreach (var m in mats) if (m != null) Object.DestroyImmediate(m);
                var quad = (Mesh)typeof(FlipbookFx).GetField("quad", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                if (quad != null) Object.DestroyImmediate(quad);
            }
        }
    }
}
