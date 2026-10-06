"""Is the gym's cyan selection bracket in this picture?

  python Tools/gymcheck.py <png|jpg> [more pictures...]
  python Tools/gymcheck.py <sheet.jpg> --max 0.0005     # the share of the frame allowed

Exit 1 when any picture given holds more than --max (0.02 % by default) strongly cyan pixels, 0 when none does.

WHY (look-08, fault 1). round3/FLAGS.md section 0 says a bright cyan bracket lies across the hull and the ground in
Events/Vehicle*, Units/Maw, Breaker, Pincer, Banner and Skimmer - TankRenderer's team-coloured contact ring and its
rider pips, drawn in the WORLD, which is why Gym.Overlays' sweep over the UIDocument tree never touched it. The gym
now hides it in a strip (GymStrip.ShowsSelection), and the claim "it is gone" has to be read off the pictures
rather than off the code, by something other than the eye that missed it for three rounds.

THE MEASURE. TankRenderer.TeamA is (0.35, 0.85, 1.00) - a saturated, bright cyan with nothing else like it in a
battlefield of mud, rust, khaki and smoke. A pixel counts when, in HSV,

  hue        170-200 degrees    cyan proper; the sky's blue sits past 200 and fire and rust well under 170
  saturation > 0.35             mud and smoke are grey, so they never reach it however bright they are
  value      > 0.6              the ring is emissive; a cyan-tinted shadow is not what anyone complained about

and the verdict is the share of the frame those pixels hold.

WHERE THE LIMIT COMES FROM. Measured over all 22 of round3/Units, before round 4 was filmed, the two groups do not
overlap and there is nothing in between: every machine entry holds some (Breaker 0.0231, Maw 0.0220, Tusk 0.0058,
Skimmer 0.0051, Pavise 0.0044, Censer 0.0037, Salvo 0.0028, Pincer 0.0015, Kettle 0.0013, Redoubt 0.0008, Banner
0.0007) and every infantry entry holds EXACTLY NONE - Rifle, MG, Sniper, Assault, Medic, Shield, Engineer, Officer,
Para, Jetpack and Vehicle are all 0 pixels, not a few. So the limit is set at 0.0002, five times under the smallest
ring (Banner, whose machine is small in a wide frame) and above anything a clean picture has ever shown. The first
draft used 0.005 and passed Banner and Redoubt: a share tuned to the close frames misses the far ones.

WHAT THIS CANNOT DO. It cannot tell the contact ring from any other saturated cyan the game may draw - a future
cyan muzzle flash would read as a ring. It reads a whole sheet, not a cell, so it says "this entry has the ring
somewhere", not which moment. And a ring hidden BEHIND the hull it lies under passes, which is the right answer for
a picture but not proof the renderer was told to hide it.
"""
import argparse, sys
import numpy as np
from PIL import Image

HUE_LO, HUE_HI, SAT_MIN, VAL_MIN = 170.0, 200.0, 0.35, 0.6
MAX_SHARE = 0.0002


def cyan_share(path):
    """The share of the picture's pixels that are strongly cyan, and their count."""
    hsv = np.asarray(Image.open(path).convert("RGB").convert("HSV")).astype(np.float32)
    hue = hsv[..., 0] * (360.0 / 255.0)
    sat = hsv[..., 1] / 255.0
    val = hsv[..., 2] / 255.0
    mask = (hue >= HUE_LO) & (hue <= HUE_HI) & (sat > SAT_MIN) & (val > VAL_MIN)
    n = int(mask.sum())
    return n / float(mask.size), n


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("images", nargs="+")
    ap.add_argument("--max", type=float, default=MAX_SHARE, help="the share of the frame allowed (default 0.0002)")
    a = ap.parse_args(argv)

    worst = 0
    for path in a.images:
        try:
            share, n = cyan_share(path)
        except Exception as ex:                     # a missing or unreadable picture is a failure, not a pass
            print("UNREAD %s: %s" % (path, ex))
            worst = 1
            continue
        bad = share > a.max
        if bad:
            worst = 1
        print("%s cyan=%.4f of the frame (%d px, limit %.4f) %s" % (
            "RING  " if bad else "CLEAR ", share, n, a.max, path))
    return worst


if __name__ == "__main__":
    sys.exit(main())
