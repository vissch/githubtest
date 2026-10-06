#!/usr/bin/env python3
"""Tools/flameband.py's own tests. Hermetic: every picture is drawn here with PIL, nothing is read off disk.

Run by Tools/toolcheck.py (which finds it by name) as a plain script; the exit code is the verdict.

The three cases are the three ways the measure has to behave:
  a tapering band muzzle -> target        BAND, exit 0
  a blob of fire round the man            BLOB  - this is look-03's fault, and what the check exists to catch
  an empty frame                          BLOB  - no lit pixels at all spans nothing
"""
import os
import sys
import tempfile

from PIL import Image, ImageDraw

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from flameband import band, fire_mask, CENTROID_MIN, REACH_MIN, WIDTH_MAX   # noqa: E402

W, H = 800, 600
MUZZLE, TARGET = (100, 300), (700, 300)
FIRE = (255, 140, 40)       # rb = 215, lum = 163: inside flamecheck.py's fire mask
failures = []


def measure(draw_it):
    """Draw a picture, save it, and run the real mask + the real measure over it."""
    img = Image.new("RGB", (W, H), (18, 20, 34))
    draw_it(ImageDraw.Draw(img))
    path = os.path.join(tempfile.gettempdir(), "flameband_case.png")
    img.save(path)
    try:
        return band(fire_mask(path), MUZZLE, TARGET)
    finally:
        os.remove(path)


def check(name, cond, detail):
    if not cond:
        failures.append("%s: %s" % (name, detail))
    print("  %s  %s  %s" % ("ok  " if cond else "FAIL", name, detail))


def tapering_band(d):
    """A stream: a hard rod at the nozzle boiling out to ~3.5 m (half-width 0.16 of the run) at the target."""
    x0, x1 = MUZZLE[0], TARGET[0]
    for x in range(x0, x1 + 1):
        t = (x - x0) / float(x1 - x0)
        half = 3 + 27 * t                      # 6 px across at the muzzle, 60 px at the target
        d.line([(x, 300 - half), (x, 300 + half)], fill=FIRE)


def blob_round_the_man(d):
    """look-03: a glow sat on the man, with none of it out along the stream."""
    r = 70
    d.ellipse([MUZZLE[0] - r, MUZZLE[1] - r, MUZZLE[0] + r, MUZZLE[1] + r], fill=FIRE)


def main():
    print("flameband: a tapering band from muzzle to target")
    reach, centroid, width, n = measure(tapering_band)
    check("band reach", reach >= REACH_MIN, "reach=%.2f (>=%.2f)" % (reach, REACH_MIN))
    check("band centroid", centroid >= CENTROID_MIN, "centroid=%.2f (>=%.2f)" % (centroid, CENTROID_MIN))
    check("band width", width <= WIDTH_MAX, "width=%.2f (<=%.2f)" % (width, WIDTH_MAX))
    check("band is lit", n > 1000, "lit=%d px" % n)

    print("flameband: a blob round the man")
    reach, centroid, width, n = measure(blob_round_the_man)
    # It fails on BOTH of the two that say "out along the stream": its weight is at the muzzle (centroid ~0) and
    # it covers one bin of ten. Its width passes, and that is correct - a blob is narrow, it is just not anywhere.
    check("blob fails centroid", centroid < CENTROID_MIN, "centroid=%.2f (<%.2f)" % (centroid, CENTROID_MIN))
    check("blob fails reach", reach < REACH_MIN, "reach=%.2f (<%.2f)" % (reach, REACH_MIN))
    check("blob is lit at all", n > 1000, "lit=%d px" % n)

    print("flameband: an empty frame")
    reach, centroid, width, n = measure(lambda d: None)
    check("empty fails reach", reach < REACH_MIN, "reach=%.2f (<%.2f)" % (reach, REACH_MIN))
    check("empty fails width", width > WIDTH_MAX, "width=%.2f (>%.2f)" % (width, WIDTH_MAX))
    check("empty is unlit", n == 0, "lit=%d px" % n)

    if failures:
        print("test_flameband FAILED: " + "; ".join(failures))
        return 1
    print("test_flameband OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
