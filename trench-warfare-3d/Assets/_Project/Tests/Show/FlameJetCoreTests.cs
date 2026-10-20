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
using System.Reflection;
using UnityEngine;
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

        // Second try (flame-jet-opaque-near-2, 2026-10-20). The first try's three captures of ONE build spread
        // 0.157-0.211 on near_edge_corr and 1.366-1.609 on core_over_tip_luma, wider than the distance left to
        // either bar, so no look change could be judged. The cause was the clock: the burst's birth, its valve
        // ramp and every Perlin and Mathf.Repeat phase in the drawing were sampled at absolute Time.time, which a
        // session cannot rewind. Flamethrower.PinnedNow pins the lot, and this is what it has to buy:
        //   1. the pinned clock, not the session's, decides when a burst is born and when it dies;
        //   2. at the sampled moment the valve is wide OPEN, so the capture photographs a full stream;
        //   3. the same pinned moment lays the same cards twice over, which is the repeatability the probe needs.
        [Test]
        public void PinnedNowDrivesTheJetClock()
        {
            // the valve, as the code states it, with this capture's own numbers put in by hand: a burst born at
            // 100 s and sampled at 101.5 s has age 1.5 s and left 1.5 s, and
            //     valve = Clamp01(age / 0.10) * Clamp01(left / 0.14) = Clamp01(15) * Clamp01(10.7) = 1
            // so the stream is at full pressure. This is the worked example, not a number the code handed back.
            const float Birth = 100f, Age = 1.5f, Seconds = 3f;
            Assert.AreEqual(1f, Mathf.Clamp01(Age / 0.10f) * Mathf.Clamp01((Seconds - Age) / 0.14f), 1e-6f,
                            "at age 1.5 s of a 3 s burst both ends of the valve are fully open");

            var books = new FlipbookFx();
            var camGo = new GameObject("flame jet clock test camera");
            var cam = camGo.AddComponent<Camera>();
            float was = Flamethrower.PinnedNow;
            try
            {
                Assert.IsTrue(books.Ready, "the flipbook sheets did not load, so no card can be laid");

                int first = Lay(books, cam, Birth, Age, Seconds);
                Assert.Greater(first, 0, "at valve 1 the stream lays cards");

                // the same pinned moment again, from a fresh jet and the same dice: the same stream, card for card.
                // Nothing of the session's own Time.time, which has moved on between the two, enters it.
                int second = Lay(books, cam, Birth, Age, Seconds);
                Assert.AreEqual(first, second, "the pinned clock must lay the same stream twice, or a capture cannot repeat");

                // and the pinned clock is what the burst lives and dies by
                var f = new Flamethrower();
                Flamethrower.PinnedNow = Birth;
                f.Burst(-1, new Vector3(0f, 1.1f, 0f), Vector3.right, Vector3.zero, Seconds);
                Assert.AreEqual(1, f.Jets, "the burst is born on the pinned clock");
                Flamethrower.PinnedNow = Birth + Age;
                f.Update(0f, cam, books, null, Flat);
                Assert.AreEqual(1, f.Jets, "half way through a 3 s burst the jet is still alive");
                Flamethrower.PinnedNow = Birth + Seconds + 0.5f;   // past Until, whatever the session's clock says
                f.Update(0f, cam, books, null, Flat);
                Assert.AreEqual(0, f.Jets, "past the burst's end on the pinned clock the jet is gone");
            }
            finally
            {
                Flamethrower.PinnedNow = was;
                Object.DestroyImmediate(camGo);
                // not books.Dispose(): its Object.Destroy is forbidden in edit mode (ColumnLightTests
                // and BannerFireFitTests carry the same note)
                const BindingFlags Priv = BindingFlags.NonPublic | BindingFlags.Instance;
                var mats = (Material[])typeof(FlipbookFx).GetField("mats", Priv).GetValue(books);
                if (mats != null) foreach (var m in mats) if (m != null) Object.DestroyImmediate(m);
                var quad = (Mesh)typeof(FlipbookFx).GetField("quad", Priv).GetValue(books);
                if (quad != null) Object.DestroyImmediate(quad);
            }
        }

        static float Flat(float x, float z) => 0f;

        /// <summary>One burst born at birth and drawn age seconds later, on the pinned clock alone. The cards it laid.</summary>
        static int Lay(FlipbookFx books, Camera cam, float birth, float age, float seconds)
        {
            int before = books.Alive;
            Random.InitState(20250925);
            var f = new Flamethrower();
            Flamethrower.PinnedNow = birth;
            f.Burst(-1, new Vector3(0f, 1.1f, 0f), Vector3.right, Vector3.zero, seconds);
            Flamethrower.PinnedNow = birth + age;
            f.Update(0f, cam, books, null, Flat);
            return books.Alive - before;
        }
    }
}
