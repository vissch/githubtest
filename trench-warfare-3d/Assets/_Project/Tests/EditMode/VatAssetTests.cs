// Phase: B3 (implemented)
// The placeholder soldier has to honour the atlas contract VATBaker will follow: one texel column per vertex, one
// texel row per frame, a row table entry per AnimRow, and poses that stay inside the mesh bounds.
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class VatAssetTests
    {
        [Test]
        public void ProceduralSoldier_AtlasMatchesMeshAndAnimRows()
        {
            var asset = ProceduralSoldier.Build();
            Assert.AreEqual((int)AnimRow.Count, asset.RowTable.Length);
            Assert.AreEqual(asset.Mesh.vertexCount, asset.Positions.width);
            Assert.AreEqual(asset.TotalFrames, asset.Positions.height);
            Assert.AreEqual(asset.Positions.width, asset.Normals.width);
            Assert.AreEqual(asset.Positions.height, asset.Normals.height);
            Assert.AreEqual(TextureFormat.RGBAHalf, asset.Positions.format);
            float next = 0f;
            foreach (var row in asset.RowTable)
            {
                Assert.AreEqual(next, row.x, "rows are stacked without gaps, in AnimRow order");
                Assert.Greater(row.y, 0f);
                next = row.x + row.y;
            }
            Assert.AreEqual(asset.TotalFrames, (int)next);
            Assert.Less(asset.Mesh.vertexCount, 400, "3,000 of these have to stay under about a million vertices");
        }
    }
}
