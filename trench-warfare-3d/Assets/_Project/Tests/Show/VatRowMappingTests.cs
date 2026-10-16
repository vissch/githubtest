// Phase: V2 (code review 2026-10-04) — P/Presentation/Units/VATRenderer.cs:403-410, VATRenderer.Fallen.cs:108: with a clip atlas and no controller (or
// a missed clip lookup on a fallen man) the raw procedural AnimRow value must not index the Clip-ordered near atlas
// directly; it has to go through the same Clip -> AnimRow fallback table, inverted.
using System.Collections;
using System.Reflection;
using NUnit.Framework;
using Unity.Collections;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Units;

namespace TW.Tests
{
    public sealed class VatRowMappingTests
    {
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        /// <summary>[V2][W2.3] the missed-clip path at a real call site: AddFallen(clip: Clip.None) with a clip atlas,
        /// the branch VatRowMappingTests' other two tests do not reach (they call VATRenderer.AnimRowNearOf directly).
        /// Clip 13 is Walk (AnimationController.cs:81, L(1.07f, AnimRow.Walk, 1.4f)): a looping 1.07 s walk cycle
        /// authored at 1.4x, so the old bug (the raw row 13 used as a Clip id) drew a corpse walking on the spot for
        /// ever, not "a short clip that plays once and freezes".</summary>
        [Test]
        public void AMissedClipLookupOnAFallenManStillMapsThroughTheFallbackTable()
        {
            var go = new GameObject("vat row rig");
            NativeArray<ushort> animRowNearOf = default;
            VATRenderer r = null;
            try
            {
                r = go.AddComponent<VATRenderer>();
                var type = r.GetType();
                Set(r, "clipAtlas", true);
                animRowNearOf = new NativeArray<ushort>(VATRenderer.AnimRowNearOf(true, Clips.Table), Allocator.Persistent);
                Set(r, "animRowNearOf", animRowNearOf);

                r.AddFallen(new Vector3(20f, 0f, 20f), 0f, 0, 0, Clip.None);

                var fallenMen = (IList)type.GetField("fallenMen", Any).GetValue(r);
                Assert.AreEqual(1, fallenMen.Count, "the premise: AddFallen recorded one fallen man");
                var f = fallenMen[0];
                ushort row = (ushort)f.GetType().GetField("Row", Any).GetValue(f);

                Assert.AreEqual(TW.Sim.AnimRow.Death0, Clips.Table[row].Fallback,
                    "[V2][W2.3] a fallen man with a missed clip lookup must land on a clip whose own Fallback is Death0");
                Assert.AreNotEqual(13, row, "[V2][W2.3] the old code used the raw AnimRow value (13, Walk) as the Clip id directly");
            }
            finally
            {
                if (r != null)
                {
                    var m = r.GetType().GetMethod("OnDestroy", Any);
                    try { m.Invoke(r, null); }
                    catch
                    {
                        if (animRowNearOf.IsCreated) animRowNearOf.Dispose();
                        var pileField = r.GetType().GetField("pile", Any);
                        var pile = (NativeArray<byte>)pileField.GetValue(r);
                        if (pile.IsCreated) pile.Dispose();
                    }
                }
                UnityEngine.Object.DestroyImmediate(go);
            }
        }

        static void Set(object v, string field, object value)
        {
            var f = v.GetType().GetField(field, Any);
            Assert.NotNull(f, $"{v.GetType().Name} has no field {field}");
            f.SetValue(v, value);
        }

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
        /// on the raw value 13 used as a Clip id (Clip 13 is Walk, a looping 1.07 s walk cycle authored at 1.4x: the
        /// old bug drew a corpse walking on the spot for ever, not "a short clip that plays once and freezes").</summary>
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
