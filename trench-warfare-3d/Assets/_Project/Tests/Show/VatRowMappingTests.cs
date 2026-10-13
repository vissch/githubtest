// Phase: V2 (code review 2026-10-04) — P/Presentation/Units/VATRenderer.cs:403-410, VATRenderer.Fallen.cs:108: with a clip atlas and no controller (or
// a missed clip lookup on a fallen man) the raw procedural AnimRow value must not index the Clip-ordered near atlas
// directly; it has to go through the same Clip -> AnimRow fallback table, inverted.
using NUnit.Framework;
using TW.Presentation;
using TW.Presentation.Units;

namespace TW.Tests
{
    public sealed class VatRowMappingTests
    {
        /// <summary>[V2] Without a clip atlas the near tier is procedural-ordered, so the raw AnimRow value is already
        /// the right row: the map must be the identity, not a lookup into Clips.Table.</summary>
        [Test]
        public void Without_A_Clip_Atlas_The_Map_Is_The_Identity()
        {
            var map = VATRenderer.AnimRowNearOf(false, Clips.Table);
            for (int r = 0; r < map.Length; r++)
                Assert.AreEqual((ushort)r, map[r], "[V2] AnimRow " + r + " must pass straight through without a clip atlas");
        }

        /// <summary>[V2] With a clip atlas, AnimRow.Death0 (13) must land on a Clip whose own Fallback is Death0, not
        /// on the raw value 13 used as a Clip id (Clip 13 is Walk, which plays once and freezes: "a standing dead man").</summary>
        [Test]
        public void With_A_Clip_Atlas_A_Death_Row_Maps_To_A_Death_Clip()
        {
            var map = VATRenderer.AnimRowNearOf(true, Clips.Table);
            int death0 = (int)TW.Sim.AnimRow.Death0;
            ushort mappedClip = map[death0];
            Assert.AreEqual(TW.Sim.AnimRow.Death0, Clips.Table[mappedClip].Fallback,
                "[V2] the clip the raw Death0 row maps to must itself fall back to Death0");
            Assert.AreNotEqual((ushort)death0, mappedClip, "[V2] the old code used the raw AnimRow value (13, Walk) as the Clip id directly");
        }
    }
}
