// Phase: anim-critique (tooling support) — depends on: nothing. Pure arithmetic, so the film camera's aim can be
// proved in an EditMode test instead of being read off a film.
// Why it exists: TacticalCamera.FrameFrom's yawDeg is ADDED to BaseYaw (-90) and to the battle-following AutoYaw
// (which eases every frame), so a capture that asks for "yaw 150" does not look along 150 degrees. The critique's
// films were filmed from the wrong side because of it (the Maw from behind, its guns hidden).
using UnityEngine;

namespace TW.Presentation.Tactical
{
    /// <summary>Turning a world bearing into the yaw TacticalCamera.FrameFrom wants, and judging whether a view shows a machine's guns.</summary>
    public static class FilmAim
    {
        /// <summary>An angle in degrees folded into -180..180 (so 270 is -90, and -190 is 170).</summary>
        public static float Wrap(float deg) => Mathf.Repeat(deg + 180f, 360f) - 180f;

        /// <summary>
        /// The yawDeg to hand FrameFrom so the camera really looks along <paramref name="bearingDeg"/>.
        /// FrameFrom's angle lands as BaseYaw + AutoYaw + yaw, so the two the camera adds are taken back out.
        /// Worked: YawFor(60, -90, 0) = 60 + 90 - 0 = 150. YawFor(60, -90, 12) = 138.
        /// </summary>
        public static float YawFor(float bearingDeg, float baseYaw, float autoYaw) => Wrap(bearingDeg - baseYaw - autoYaw);

        /// <summary>
        /// The world bearing a camera must look ALONG to see a machine facing <paramref name="facingDeg"/> from its
        /// front quarter: opposite its facing (so it comes towards the lens), turned <paramref name="quarterDeg"/>
        /// off head-on so the flank and its guns are in view too.
        /// Worked: a machine facing 0 (north, +Z) is seen from QuarterFront(0) = 0 + 180 - 55 = 125.
        /// </summary>
        public static float QuarterFront(float facingDeg, float quarterDeg = 55f) => Wrap(facingDeg + 180f - quarterDeg);

        /// <summary>
        /// True when a camera looking along <paramref name="lookBearing"/> sees the guns of a machine facing
        /// <paramref name="facingDeg"/>: the angle between the two is 40..160 degrees. Straight behind (0) is the
        /// critique's complaint and fails; head-on (180) hides the hull sides and fails too.
        /// Worked: ShowsGuns(125, 0) true; ShowsGuns(0, 0) false.
        /// </summary>
        public static bool ShowsGuns(float lookBearing, float facingDeg)
        {
            float between = Mathf.Abs(Wrap(lookBearing - facingDeg));
            return between >= 40f && between <= 160f;
        }
    }
}
