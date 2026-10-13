// Phase: anim-critique (2026-10-13) — the film camera's aim, in worked numbers. The critique of 2026-10-06 blamed the
// film: the Maw was shot from behind because FrameFrom's yaw is added to the camera's own two angles. These hold
// FilmAim to the arithmetic that undoes that, so the next batch of films cannot quietly go back to the wrong side.
using NUnit.Framework;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class FilmFollowTests
    {
        [Test]
        public void YawForTakesTheCamerasOwnAnglesBackOut()
        {
            Assert.AreEqual(150f, FilmAim.YawFor(60f, -90f, 0f), 0.01f, "a bearing of 60 with BaseYaw -90 needs yaw 150");
            Assert.AreEqual(138f, FilmAim.YawFor(60f, -90f, 12f), 0.01f, "the eased AutoYaw comes out too");
        }

        [Test]
        public void AMachineIsSeenFromItsFrontQuarter()
        {
            Assert.AreEqual(125f, FilmAim.QuarterFront(0f), 0.01f, "facing north (+Z), seen from 125 degrees");
            Assert.IsTrue(FilmAim.ShowsGuns(125f, 0f), "the front quarter shows the guns");
            Assert.IsFalse(FilmAim.ShowsGuns(0f, 0f), "straight behind hides them (the critique's Maw)");
        }

        [Test]
        public void EveryFacingHasAQuarterThatShowsTheGuns()
        {
            for (float facing = -180f; facing < 180f; facing += 15f)
                Assert.IsTrue(FilmAim.ShowsGuns(FilmAim.QuarterFront(facing), facing), "facing " + facing);
        }
    }
}
