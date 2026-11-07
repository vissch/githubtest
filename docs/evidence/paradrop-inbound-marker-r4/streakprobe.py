"""Counts the drop's falling streaks per man at the zone, from a before/after pair either side of
OnDropInbound (CaptureRig.Shot, one pose, weather pinned, overlays and HUD off, the tactical camera itself
at zoom 120 so FlipbookFx.FarGrow sizes the cards for the band they are photographed at). usage:
python docs/evidence/paradrop-inbound-marker-r4/streakprobe.py

Method. A per-pixel |after - before| carries the men walking and the muzzle flashes as well as the drop, so
a component only counts as a streak when its SHAPE and its PLACE both fit a ShellFall card standing on the
zone's ring. Every threshold is anchored to the pose, never fitted to the answer:

  zoom 120 over 900 px -> view height 1.1547 * 120 = 138.6 m, so PX_PER_M = 6.50 px/m;
  a card is 3 * FlipbookFx.FarGrow(120) = 4.5 m wide = 29 px, and stands upright on the ground;
  the two rings (DropStreakRingNear 7 m, DropStreakRingFar 11.5 m) are 45 px and 75 px from the mark on the
  ground, and at pitch 25 the ground's screen y is squashed by sin(25) = 0.4226, so the rings are ELLIPSES:
  the elliptical radius of a card's ground anchor (its component's BOTTOM edge, not its centre) is
  hypot(dx, dy / sin(25)), and a streak's anchor lies on 45 px or 75 px.

The mark is CaptureRig.Rig.Mark projected by the rig itself (mark_px_x/y in the sidecar), so the window is
found by projection and never by eye. The same counter is run on the control pair - two stills of the
unchanged scene, the same number of frames apart as before -> after - for the noise floor.

Exit 0 only when streak_ratio >= 0.8 and noise_streaks <= 1.
"""
import glob
import json
import math
import os
import sys

import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))

PX_PER_M = 6.50            # 900 px / (1.1547 * 120 m)
PITCH_SIN = math.sin(math.radians(25.0))
RING_NEAR_PX = 7.0 * PX_PER_M      # DropStreakRingNear
RING_FAR_PX = 11.5 * PX_PER_M      # DropStreakRingFar
RING_TOL_PX = 12.0         # half a card's width: a card's anchor is on the ring, give or take its own size
HALF = 140                 # window half-size: the far ring is 75 px out, a card ~30 px tall over it
FLOOR = 60.0               # sum over RGB of |after - before|, 0-765
MIN_AREA_PX = 40
MIN_H_PX = 10


def streak_components(before_path, after_path, mark_px):
    a = np.asarray(Image.open(before_path).convert("RGB")).astype(np.float32)
    b = np.asarray(Image.open(after_path).convert("RGB")).astype(np.float32)
    mx, my = int(round(mark_px[0])), int(round(mark_px[1]))
    diff = np.abs(b - a).sum(axis=2)[my - HALF:my + HALF, mx - HALF:mx + HALF]
    mask = ndimage.binary_opening(diff >= FLOOR, np.ones((2, 2)))
    labels, n = ndimage.label(mask)
    if n == 0:
        return 0
    objs = ndimage.find_objects(labels)
    sizes = ndimage.sum(mask, labels, index=range(1, n + 1))
    streaks = 0
    for sl, sz in zip(objs, sizes):
        h = sl[0].stop - sl[0].start
        w = sl[1].stop - sl[1].start
        if sz < MIN_AREA_PX or h < MIN_H_PX or h < w:
            continue
        cx = (sl[1].start + sl[1].stop) / 2.0 - HALF      # the card's ground anchor: bottom edge, middle
        by = sl[0].stop - HALF
        rr = math.hypot(cx, by / PITCH_SIN)
        if abs(rr - RING_NEAR_PX) <= RING_TOL_PX or abs(rr - RING_FAR_PX) <= RING_TOL_PX:
            streaks += 1
    return streaks


def main():
    here = HERE
    with open(os.path.join(here, "z120-after.json"), encoding="utf-8") as f:
        sidecar = json.load(f)
    mark_px = (sidecar["mark_px_x"], sidecar["mark_px_y"])
    before = os.path.join(here, "z120-before.png")
    noise = streak_components(os.path.join(here, "z120-control.png"),
                              os.path.join(here, "z120-control-b.png"), mark_px)
    series = {os.path.basename(p): streak_components(before, p, mark_px)
              for p in sorted(glob.glob(os.path.join(here, "after_*.png")))}
    raw = streak_components(before, os.path.join(here, "z120-after.png"), mark_px)
    men = 8
    streaks = max(0, raw - noise)
    out = {
        "streaks": streaks,
        "men": men,
        "streak_ratio": streaks / men,
        "raw_streaks": raw,
        "noise_streaks": noise,
        "series": series,
        "window_half_px": HALF,
        "floor": FLOOR,
        "px_per_m": PX_PER_M,
        "ring_near_px": RING_NEAR_PX,
        "ring_far_px": RING_FAR_PX,
        "ring_tol_px": RING_TOL_PX,
    }
    with open(os.path.join(here, "probe.json"), "w", encoding="utf-8", newline="
") as f:
        json.dump(out, f, indent=2)
    print(json.dumps(out, indent=2))
    sys.exit(0 if out["streak_ratio"] >= 0.8 and noise <= 1 else 1)


if __name__ == "__main__":
    main()
