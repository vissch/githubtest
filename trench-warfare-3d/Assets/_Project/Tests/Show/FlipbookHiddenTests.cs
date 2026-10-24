// Phase: VFX pass (the flame-jet seam, 2026-10-24) - FlipbookFx.Hidden, the capture switch that keeps a book's cards
// out of a still. The flame-jet stills measure the seam of the stream IN THE AIR; the burning pool, pyre and fan lie
// on the ground and their flat top edge is the straightest line in the picture, so the seam measure read the pool and
// not the jet. Hiding the ground books takes them out of the capture instead of out of the software.
//
// What is asserted here is the contract of the new branch - a hidden book stores no card, every other book is
// untouched, and the switch is off in the game - not numbers the code hands back.
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class FlipbookHiddenTests
    {
        [Test]
        public void TheMaskHasABitForEveryBook()
        {
            Assert.LessOrEqual((int)FlipbookFx.Book.Count, 64,
                               "Hidden is a ulong, one bit per Book ordinal: a 65th book needs a wider mask");
        }

        [Test]
        public void NothingIsHiddenByDefault()
        {
            Assert.AreEqual(0UL, FlipbookFx.Hidden, "the game draws every book: Hidden is a capture switch, off by default");
        }

        [Test]
        public void AHiddenBookStoresNoCard_AndTheRestAreUntouched()
        {
            var fx = new FlipbookFx();
            ulong was = FlipbookFx.Hidden;
            try
            {
                Assume.That(fx.Ready, "the flipbook shader and sheets load in the editor");

                FlipbookFx.Hidden = 0UL;
                int before = fx.Alive;
                fx.Add(FlipbookFx.Book.Pool, Vector3.zero, 2f, 1f);
                Assert.AreEqual(before + 1, fx.Alive, "with nothing hidden a pool card is stored");

                FlipbookFx.Hidden = 1UL << (int)FlipbookFx.Book.Pool;
                int mid = fx.Alive;
                fx.Add(FlipbookFx.Book.Pool, Vector3.zero, 2f, 1f);
                Assert.AreEqual(mid, fx.Alive, "a hidden book's card is never stored, so nothing can draw it");

                fx.Add(FlipbookFx.Book.JetCore, Vector3.zero, 2f, 1f);
                Assert.AreEqual(mid + 1, fx.Alive, "hiding one book leaves every other book alone");
            }
            finally
            {
                FlipbookFx.Hidden = was;
                // DestroyImmediate: Dispose's Object.Destroy is not allowed in edit mode
                var mats = (Material[])typeof(FlipbookFx).GetField("mats", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
                foreach (var m in mats) if (m != null) Object.DestroyImmediate(m);
            }
        }
    }
}
