// Phase: V5 (code review 2026-10-04) — VATRenderer.cs:258-263: the counting sort of the near records by figure ran as
// five managed loops on the main thread every LateUpdate. It is now VATRenderer.GroupByFigure, called from the Burst
// fill job; this guards that the grouping it produces is the same one the indirect draws need.
using NUnit.Framework;
using Unity.Collections;
using Unity.Mathematics;
using TW.Presentation.Units;

namespace TW.Tests
{
    public sealed class VatFigureGroupingTests
    {
        [Test]
        public void The_Near_Records_Come_Out_Grouped_By_Figure()   // [V5]
        {
            const int count = 5, figures = 3;
            var src = new NativeArray<VatInstance>(count, Allocator.Temp);
            var figureOf = new NativeArray<byte>(count, Allocator.Temp);
            var dst = new NativeArray<VatInstance>(count, Allocator.Temp);
            var group = new NativeArray<int>(figures * 2, Allocator.Temp);
            var of = new byte[] { 1, 0, 1, 2, 0 };
            for (int i = 0; i < count; i++) { src[i] = new VatInstance { Pos = new float3(i, 0f, 0f) }; figureOf[i] = of[i]; }

            VATRenderer.GroupByFigure(src, figureOf, count, figures, dst, group);

            var order = new int[count];
            for (int i = 0; i < count; i++) order[i] = (int)dst[i].Pos.x;
            Assert.AreEqual(new[] { 1, 4, 0, 2, 3 }, order, "[V5] the records must come out grouped by figure, stable within a figure");
            Assert.AreEqual(new[] { 2, 2, 1 }, new[] { group[0], group[1], group[2] }, "[V5] group[k] is figure k's count");
            Assert.AreEqual(new[] { 0, 2, 4 }, new[] { group[3], group[4], group[5] }, "[V5] group[figures + k] is figure k's first index, the cursors wound back");

            src.Dispose(); figureOf.Dispose(); dst.Dispose(); group.Dispose();
        }
    }
}
