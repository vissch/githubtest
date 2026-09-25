// Phase: B5 — the one thing that can go wrong with the shared environment atlas and leave no trace.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Terrain;

namespace TW.Tests
{
    public sealed class EnvAtlasTests
    {
        /// <summary>
        /// Every prop set samples one cell of Resources/Env/EnvAtlas, chosen by its index in EnvSets and the grid in
        /// EnvCols/EnvRows. Tools/envatlas.py packs that atlas from its own copy of the same list and the same grid,
        /// and NOTHING joins the two: change the grid in one and every set silently samples the wrong cell, which does
        /// not throw, does not fail a test, and looks like the art was reimported wrong.
        ///
        /// This bit once already. The grid was 4 x 2 and the comment in envatlas.py said it left "two cells spare for
        /// a seventh set"; by 2026-09-24 there were eight sets in eight cells and the ninth had nowhere to go. The
        /// overflow was silent too — Array.IndexOf simply returned a cell past the end of the sheet.
        /// </summary>
        [Test]
        public void Every_Set_Has_A_Cell_In_The_Atlas_And_The_Atlas_Is_The_Shape_The_Grid_Says()
        {
            Assert.LessOrEqual(BattlefieldKit.EnvSets.Length, BattlefieldKit.EnvCols * BattlefieldKit.EnvRows,
                "there are more prop sets than cells — grow the grid in BOTH BattlefieldKit and Tools/envatlas.py, then rerun it");

            var atlas = Resources.Load<Texture2D>("Env/EnvAtlas");
            Assert.IsNotNull(atlas, "Resources/Env/EnvAtlas is missing — run: python Tools/envatlas.py");

            // the packer makes square cells, so the sheet's shape is the grid's shape. If someone changes ROWS in the
            // packer and not here (or the other way about), this is what catches it.
            Assert.AreEqual(BattlefieldKit.EnvCols / (float)BattlefieldKit.EnvRows, atlas.width / (float)atlas.height, 1e-4f,
                "the atlas is " + atlas.width + "x" + atlas.height + ", which is not a " + BattlefieldKit.EnvCols + "x" + BattlefieldKit.EnvRows +
                " grid of square cells — BattlefieldKit and Tools/envatlas.py have drifted apart");

            Assert.IsTrue(Mathf.IsPowerOfTwo(atlas.width) && Mathf.IsPowerOfTwo(atlas.height),
                "a non-power-of-two atlas loses block compression entirely (measured 2026-09-22: 3072 wide came back as RGB24 at 25 MB)");

            // and every cell lands inside the sheet
            for (int i = 0; i < BattlefieldKit.EnvSets.Length; i++)
            {
                var uv = BattlefieldKit.EnvOffset(i);
                Assert.That(uv.x, Is.InRange(0f, 1f - 1f / BattlefieldKit.EnvCols + 1e-4f), BattlefieldKit.EnvSets[i] + " sits inside the sheet across");
                Assert.That(uv.y, Is.InRange(0f, 1f - 1f / BattlefieldKit.EnvRows + 1e-4f), BattlefieldKit.EnvSets[i] + " sits inside the sheet up");
            }
        }
    }
}
