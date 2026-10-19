// Phase: VFX pass (owner, 2026-10-19) - the flamethrower jet's near third as an opaque white-hot core.
// Harsh critic r1 (41/100) fix 1 on flame-jet-core, 2026-10-19: by eye the jet's near third was see-through -
// hedgehogs and stakes read straight through it at x 330-480 of jet-t2-after.png - while the old see_through_frac
// probe called any 6/255 of tint "opaque" and so measured 0.0. The near third is the owner's opaque white-hot core
// ("an opaque white-yellow core over the first third, orange towards the tip", 2026-10-19), so what the jet flies
// over must not be visible through it. Three things had to be true for that, and this guards all three:
//   1. the near third is drawn from Book.JetCore, both in the envelope and in the core chain;
//   2. that book is _Solid - the shader keeps its keyed fringe opaque and never thins it against the scene depth
//      behind it - and no other book is, so every other flipbook renders bit for bit;
//   3. nothing else in the Sheets table turned Solid on by accident.
using NUnit.Framework;
using TW.Presentation.Tactical;
using Book = TW.Presentation.Tactical.FlipbookFx.Book;

namespace TW.Tests
{
    public class FlameJetCoreTests
    {
        [Test]
        public void FlameJetNearThirdHidesGround()
        {
            // the near third, in both chains, is the solid book
            foreach (float u in new[] { 0f, 0.1f, 0.2f, 0.33f })
            {
                Assert.AreEqual(Book.JetCore, Flamethrower.JetLink(u, false),
                                $"the envelope at u={u} is in the hot third, so it draws the solid core book");
                Assert.AreEqual(Book.JetCore, Flamethrower.CoreLink(u),
                                $"the core chain at u={u} is in the hot third, so it draws the solid core book");
            }
            // past it the stream hands over to the orange drawings
            Assert.AreEqual(Book.Jet, Flamethrower.JetLink(0.5f, false), "past the hot third the envelope cools to Jet");
            Assert.AreEqual(Book.Core, Flamethrower.CoreLink(0.5f), "past the hot third the core cools to Core");
            Assert.AreEqual(Book.Head, Flamethrower.JetLink(0f, true), "the head is its own closed bolus whatever u says");

            // the owner's "first third": a third of the run, give or take
            Assert.That(Flamethrower.HotThird, Is.InRange(0.30f, 0.40f), "the hot third is a third of the run");

            // and the book it draws is solid: opaque where the artist drew, never thinned against what it flies over
            Assert.IsTrue(FlipbookFx.SheetSolid(Book.JetCore),
                          "Book.JetCore must be _Solid, or the keyed fringe and the depth soften let the ground through the near third");
        }

        [Test]
        public void OnlyTheJetCoreIsSolid()
        {
            for (var b = (Book)0; b < Book.Count; b++)
            {
                if (b == Book.JetCore) continue;
                Assert.IsFalse(FlipbookFx.SheetSolid(b),
                               $"Book.{b} must not be _Solid: the shader edit is gated on it, so any other row turning it on changes that book's look");
            }
        }
    }
}
