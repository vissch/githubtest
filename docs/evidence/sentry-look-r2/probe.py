#!/usr/bin/env python
"""Measure the three plate-mask stills shoot.py shot, in pixels (sentry-look-r2, step 8).

The harsh critic scored r1's Sentry 12/100 on three facts about his plate: its post ended 0.455 m above the ground,
the plate swung round with his gun as he traversed, and it vanished the frame he died. None of those can be settled
by arithmetic in an EditMode test, which is what r1 offered - so they are settled here, off the pixels of stills the
real CaptureRig shot in GreyboxCorridor.

CombatFx.PlateMask draws the plate and its post in flat magenta and, standing on the same ground as the post, a
calibration bar exactly 0.50 m tall in flat cyan. That bar is what turns pixels into metres, in the same still, under
the same camera - so no focal length, no projection and no camera pose has to be trusted.

    px_per_m             = (cyan bottom row - cyan top row) / 0.50
    foot_gap_m           = max(0, cyan bottom row - magenta bottom row) / px_per_m     <= 0.05 passes
    plate_shift_px       = |magenta bbox centre of plate_t - that of plate_t60|        <= 2    passes
    plate_px_after_death = magenta pixels in plate_dead                                >  0    passes

The bar's bottom and the post's bottom are the same ground point by construction (CombatFx.PlatePieces centres the
post half its own height up from the foot), so a floating post shows up as magenta stopping short of the cyan.

Run from the repo root:  python docs/evidence/sentry-look-r2/probe.py
Writes probe.json beside the stills. Exit 0 when all three thresholds pass, 1 when any fails.
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
BAR_M = 0.50          # the calibration bar's height in metres (CombatFx.PlateBar)
TOL = 40              # per-channel tolerance out of 255: the mask is unlit and flat, so it needs no more
MAGENTA = (255, 0, 255)
CYAN = (0, 255, 255)

GAP_MAX = 0.05        # m
SHIFT_MAX = 2.0       # px
STILLS = ("plate_t", "plate_t60", "plate_dead")


def mask(rgb, want):
    """Where the image is within TOL of `want` on every channel."""
    d = np.abs(rgb.astype(np.int16) - np.array(want, dtype=np.int16))
    return (d <= TOL).all(axis=2)


def extent(m):
    """count, top row, bottom row, and the bounding-box centre (row, col); rows count down from the top."""
    rows, cols = np.nonzero(m)
    if rows.size == 0:
        return dict(count=0, top=None, bottom=None, centre=None)
    return dict(count=int(rows.size), top=int(rows.min()), bottom=int(rows.max()),
                centre=[(float(rows.min()) + float(rows.max())) / 2.0,
                        (float(cols.min()) + float(cols.max())) / 2.0])


def read(stem):
    path = os.path.join(HERE, stem + ".png")
    if not os.path.exists(path):
        raise SystemExit("missing still: " + path)
    rgb = np.asarray(Image.open(path).convert("RGB"))
    return dict(file=stem + ".png", size=[int(rgb.shape[1]), int(rgb.shape[0])],
                magenta=extent(mask(rgb, MAGENTA)), cyan=extent(mask(rgb, CYAN)))


def main():
    shots = {s: read(s) for s in STILLS}
    t, t60, dead = shots["plate_t"], shots["plate_t60"], shots["plate_dead"]

    # the scale, from the bar in plate_t: the same still, so nothing about the camera has to be assumed
    bar = t["cyan"]
    if not bar["count"] or bar["bottom"] == bar["top"]:
        raise SystemExit("no calibration bar in plate_t: is CombatFx.PlateMask on, and the plate in frame?")
    px_per_m = (bar["bottom"] - bar["top"]) / BAR_M

    plate = t["magenta"]
    if not plate["count"]:
        raise SystemExit("no plate in plate_t: nothing to measure")
    # the post's bottom should sit on the bar's bottom. Magenta reaching below the cyan is the post's front face
    # standing 0.04 m nearer the camera than the thin bar, not a gap, so only magenta stopping short counts.
    foot_gap_m = max(0.0, (bar["bottom"] - plate["bottom"]) / px_per_m)

    if not t60["magenta"]["count"]:
        raise SystemExit("no plate in plate_t60: nothing to compare")
    a, b = plate["centre"], t60["magenta"]["centre"]
    plate_shift_px = float(np.hypot(a[0] - b[0], a[1] - b[1]))

    plate_px_after_death = dead["magenta"]["count"]

    out = {
        "foot_gap_m": round(foot_gap_m, 4),
        "plate_shift_px": round(plate_shift_px, 3),
        "plate_px_after_death": plate_px_after_death,
        "px_per_m": round(px_per_m, 2),
        "bar_m": BAR_M,
        "tolerance_per_channel": TOL,
        "thresholds": {"foot_gap_m": GAP_MAX, "plate_shift_px": SHIFT_MAX, "plate_px_after_death": "> 0"},
        "shots": shots,
    }
    out["pass"] = bool(foot_gap_m <= GAP_MAX and plate_shift_px <= SHIFT_MAX and plate_px_after_death > 0)

    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")

    print("px_per_m             %8.2f  (cyan bar %d px for %.2f m)" % (px_per_m, bar["bottom"] - bar["top"], BAR_M))
    print("foot_gap_m           %8.4f  (<= %.2f)   %s" % (foot_gap_m, GAP_MAX, "pass" if foot_gap_m <= GAP_MAX else "FAIL"))
    print("plate_shift_px       %8.3f  (<= %.1f)    %s" % (plate_shift_px, SHIFT_MAX, "pass" if plate_shift_px <= SHIFT_MAX else "FAIL"))
    print("plate_px_after_death %8d  (>  0)      %s" % (plate_px_after_death, "pass" if plate_px_after_death > 0 else "FAIL"))
    for s in STILLS:
        m, c = shots[s]["magenta"], shots[s]["cyan"]
        print("  %-11s magenta %6d px rows %s..%s | cyan %6d px rows %s..%s"
              % (s, m["count"], m["top"], m["bottom"], c["count"], c["top"], c["bottom"]))
    return 0 if out["pass"] else 1


if __name__ == "__main__":
    sys.exit(main())
