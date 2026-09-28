// Phase: VFX pass (owner, 2026-09-28) - FlipbookFx.Sheets is indexed by the Book enum's number, so a row out of place
// draws another book's file with no error. Core, Head and Bloom drew FireHead, FireBloom and FireCore until the rows
// were put back in the enum's order (tw3d-board finding BUG-desktop-20260928-flipbook-ordinals).
using NUnit.Framework;
using TW.Presentation.Tactical;
using Book = TW.Presentation.Tactical.FlipbookFx.Book;

namespace TW.Tests
{
    public class FlipbookOrdinalTests
    {
        [Test]
        public void EveryRow_HasItsBook()
        {
            Assert.AreEqual((int)Book.Count, FlipbookFx.SheetCount, "one Sheets row per Book, no more, no fewer");
        }

        [Test]
        public void EveryBookFromJetOn_DrawsTheSheetNamedAfterIt()
        {
            // the round-3 books and the VFX pass's are each cut from <Book> or Fire<Book>
            for (var b = Book.Jet; b < Book.Count; b++)
            {
                string name = FlipbookFx.SheetName(b);
                Assert.IsTrue(name == b.ToString() || name == "Fire" + b, $"Book.{b} is row {(int)b}, which draws {name}: the Sheets row there is another book's");
            }
        }

        [Test]
        public void TheOlderBooks_KeepTheirRows()
        {
            Assert.AreEqual("Burst", FlipbookFx.SheetName(Book.Burst));
            Assert.AreEqual("Muzzle", FlipbookFx.SheetName(Book.Muzzle));
            // the first fire books are named for their drawing, not their book
            Assert.AreEqual("FireBall", FlipbookFx.SheetName(Book.Fire));
            Assert.AreEqual("FireColumn", FlipbookFx.SheetName(Book.Pyre));
            Assert.AreEqual("FireBurst", FlipbookFx.SheetName(Book.Fireball));
        }
    }
}
