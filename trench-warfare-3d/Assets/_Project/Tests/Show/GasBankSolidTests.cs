// Phase: ability-fx-captures-r2-3-g2-g2-g2-g2 (2026-10-23) - the gas bank's near-binary alpha (_Solid), and the
// guard that it is the gas bank's alone. A blob pixel is a*card + (1-a)*ground, so a wide soft alpha shoulder mixes
// the card's tint with the grass behind it and each capture seed, mixing its own share, lands on its own hue.
// Pulling the shoulder up to full makes the pixels inside the silhouette the card's own colour. Lives in Tests/Show
// (not beside GasBankCards in Tests/Stills: that module is EXPLICIT_ONLY, so no gate run would select it).
using System.IO;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class GasBankSolidTests
    {
        /// <summary>
        /// r2-3-g2-g2-g2-g2: _Solid pulls a bank's soft alpha shoulder up to full so its pixels are its own colour
        /// and not a mix with the ground behind. It is a GAS-ONLY change: every other book must still draw bit for
        /// bit as it did, so the expected 0 here is "unchanged from the build before this commit", not a copy of the
        /// new constant. This guards the one thing the shader edit could break by accident - another effect moving.
        /// It does not prove the look; the captures in docs/evidence/ability-fx-captures-r2-3-g2-g2-g2-g2 do.
        /// </summary>
        [Test]
        public void Solid_IsTheGasBanksAlone_AndZeroOnEveryOtherBook()
        {
            string src = File.ReadAllText(Path.Combine(Application.dataPath, "_Project", "Shaders", "Flipbook_URP.shader"));
            StringAssert.Contains("Range(0, 0.95)) = 0", src.Substring(src.IndexOf("_Solid (")), "the shader's own default is the drawing's own alpha");
            StringAssert.Contains("alpha = saturate(alpha / max(0.05, 1.0 - _Solid));", src);

            var fx = new FlipbookFx();
            try
            {
                Assume.That(fx.Ready, "the flipbook shader and sheets load in the editor");
                var mats = (Material[])typeof(FlipbookFx).GetField("mats", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                for (int b = 0; b < mats.Length; b++)
                {
                    float solid = mats[b].GetFloat("_Solid");
                    if ((FlipbookFx.Book)b == FlipbookFx.Book.GasBank)
                        Assert.Greater(solid, 0f, "the gas bank's alpha is pulled up");
                    else
                        Assert.IsTrue(solid == 0f, (FlipbookFx.Book)b + " draws on its own alpha, as it did before this commit");
                }
            }
            finally
            {
                // DestroyImmediate: Dispose's Object.Destroy is not allowed in edit mode (ColumnLightTests' lesson)
                var mats = (Material[])typeof(FlipbookFx).GetField("mats", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                foreach (var m in mats) if (m != null) Object.DestroyImmediate(m);
                var quad = (Mesh)typeof(FlipbookFx).GetField("quad", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                if (quad != null) Object.DestroyImmediate(quad);
            }
        }
    }
}
