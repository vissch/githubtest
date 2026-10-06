#!/usr/bin/env python3
"""Tools/flameband.py's own tests. Hermetic: every picture is drawn here with PIL, nothing is read off disk.

Run by Tools/toolcheck.py (which finds it by name) as a plain script; the exit code is the verdict.

The four cases are the four ways the measure has to behave:
  a tapering band muzzle -> target        BAND, exit 0
  a blob of fire round the man            BLOB  - look-03's fault, and what the check first existed to catch
  a blob out at the target, over a WASH   BLOB  - look-06's case: a bright blob four metres out with a wide sheet
                                                  of lit mud under it and nothing joining either to the nozzle.
                                                  The first version of this check called the wash the stream and
                                                  measured ITS width, which is why it could never pass a jet that
                                                  lights the ground. See the header of flameband.py.
  an empty frame                          BLOB  - nothing spans anything

and two more for the EDGE verdict (--wiggle, look-09), which is a different statistic on the same piece:
  the same tapering wedge, drawn with STRAIGHT edges     RULER,  red - what the master rejected
  the same wedge with a per-column random edge of +-6 px RAGGED, green - what fire's silhouette looks like
"""
import os
import random
import sys
import tempfile

from PIL import Image, ImageDraw

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from flameband import (band, core_mask, wiggle, ROOT_MAX, SPAN_MIN, WIDTH_MAX,   # noqa: E402
                       WIGGLE_MIN, WIGGLE_COLUMNS)

W, H = 800, 600
MUZZLE, TARGET = (100, 300), (700, 300)
CORE = (255, 240, 205)      # lum 240, rb = 50: an over-bright warm core, which is what a stream is
WASH = (210, 130, 60)       # lum 150, rb = 150: warm, and nowhere near bright enough to be the fire itself
failures = []


def measure(draw_it):
    """Draw a picture, save it, and run the real mask + the real measure over it."""
    img = Image.new("RGB", (W, H), (18, 20, 34))
    draw_it(ImageDraw.Draw(img))
    path = os.path.join(tempfile.gettempdir(), "flameband_case.png")
    img.save(path)
    try:
        return band(core_mask(path), MUZZLE, TARGET)
    finally:
        os.remove(path)


def measure_edge(draw_it):
    """The same drawing path, measured with the EDGE statistic instead of the band one."""
    img = Image.new("RGB", (W, H), (18, 20, 34))
    draw_it(ImageDraw.Draw(img))
    path = os.path.join(tempfile.gettempdir(), "flameband_edge_case.png")
    img.save(path)
    try:
        return wiggle(core_mask(path))
    finally:
        os.remove(path)


def ragged_band(d):
    """The same wedge, with the edge pushed +-6 px: a silhouette noise has eaten into.

    The offset is HELD FOR THREE COLUMNS, not redrawn every column, and that detail is the measure's own doing
    rather than a convenience. core_mask ends in a 3x3 binary_opening, so a one-column spike is eroded away before
    wiggle ever sees it: an edge redrawn every column measures 2.03 px, the same amplitude held for three columns
    measures 2.77 px. Real fire noise has a scale too. The seed is fixed, so both numbers are the same every run.
    """
    rng = random.Random(90901)
    x0, x1 = MUZZLE[0], TARGET[0]
    hi = lo = 0.0
    for x in range(x0, x1 + 1):
        t = (x - x0) / float(x1 - x0)
        half = 3 + 27 * t
        if (x - x0) % 3 == 0:
            hi, lo = rng.uniform(-6, 6), rng.uniform(-6, 6)
        d.line([(x, 300 - half + hi), (x, 300 + half + lo)], fill=CORE)


def check(name, cond, detail):
    if not cond:
        failures.append("%s: %s" % (name, detail))
    print("  %s  %s  %s" % ("ok  " if cond else "FAIL", name, detail))


def tapering_band(d):
    """A stream: a hard rod at the nozzle boiling out to ~3.9 m (half-width 0.18 of the run) at the target."""
    x0, x1 = MUZZLE[0], TARGET[0]
    for x in range(x0, x1 + 1):
        t = (x - x0) / float(x1 - x0)
        half = 3 + 27 * t                      # 6 px across at the muzzle, 60 px at the target
        d.line([(x, 300 - half), (x, 300 + half)], fill=CORE)


def blob_round_the_man(d):
    """look-03: a glow sat on the man, with none of it out along the stream."""
    r = 70
    d.ellipse([MUZZLE[0] - r, MUZZLE[1] - r, MUZZLE[0] + r, MUZZLE[1] + r], fill=CORE)


def blob_over_a_wash(d):
    """look-05: a bright blob out at the target, a WIDE SHEET of lit mud under the whole run, nothing joining them."""
    d.polygon([(90, 330), (710, 330), (760, 560), (40, 560)], fill=WASH)     # the firelight on the ground
    d.ellipse([480, 250, 620, 360], fill=CORE)                              # the fire, detached, four metres out
    d.ellipse([MUZZLE[0] - 22, MUZZLE[1] - 22, MUZZLE[0] + 22, MUZZLE[1] + 22], fill=CORE)   # a flare at the man


def main():
    print("flameband: a tapering band from muzzle to target")
    root, span, width, n = measure(tapering_band)
    check("band root", root <= ROOT_MAX, "root=%.2f (<=%.2f)" % (root, ROOT_MAX))
    check("band span", span >= SPAN_MIN, "span=%.2f (>=%.2f)" % (span, SPAN_MIN))
    check("band width", width <= WIDTH_MAX, "width=%.2f (<=%.2f)" % (width, WIDTH_MAX))
    check("band is lit", n > 1000, "core=%d px" % n)

    print("flameband: a blob round the man")
    root, span, width, n = measure(blob_round_the_man)
    # Its root passes - it IS at the nozzle - and that is correct: what a blob is not is a RUN.
    check("blob fails span", span < SPAN_MIN, "span=%.2f (<%.2f)" % (span, SPAN_MIN))
    check("blob is lit at all", n > 1000, "core=%d px" % n)

    print("flameband: a blob out at the target over a wash of lit mud")
    root, span, width, n = measure(blob_over_a_wash)
    # THE CASE THE FIRST VERSION GOT WRONG. The wash is the largest warm region in the frame by far, but it is not
    # over-bright, so it is not in the core at all; the core's largest piece is the detached blob, which starts
    # four metres out and spans almost nothing.
    check("wash fails root", root > ROOT_MAX, "root=%.2f (>%.2f)" % (root, ROOT_MAX))
    check("wash fails span", span < SPAN_MIN, "span=%.2f (<%.2f)" % (span, SPAN_MIN))
    check("wash does not count as the stream", n < 30000, "core=%d px (the wash is 180000 px)" % n)

    print("flameband: an empty frame")
    root, span, width, n = measure(lambda d: None)
    check("empty fails root", root > ROOT_MAX, "root=%.2f (>%.2f)" % (root, ROOT_MAX))
    check("empty fails width", width > WIDTH_MAX, "width=%.2f (>%.2f)" % (width, WIDTH_MAX))
    check("empty is unlit", n == 0, "core=%d px" % n)

    print("flameband --wiggle: the straight-edged wedge is a RULER")
    w, cols = measure_edge(tapering_band)
    check("straight edge is long enough to judge", cols >= WIGGLE_COLUMNS, "columns=%d (>=%d)" % (cols, WIGGLE_COLUMNS))
    check("straight edge fails wiggle", w < WIGGLE_MIN, "wiggle=%.2f px (<%.2f)" % (w, WIGGLE_MIN))

    print("flameband --wiggle: the same wedge with a +-6 px ragged edge")
    w, cols = measure_edge(ragged_band)
    check("ragged edge is long enough to judge", cols >= WIGGLE_COLUMNS, "columns=%d (>=%d)" % (cols, WIGGLE_COLUMNS))
    check("ragged edge passes wiggle", w >= WIGGLE_MIN, "wiggle=%.2f px (>=%.2f)" % (w, WIGGLE_MIN))
    # The band verdict must still pass on the ragged wedge: making the edge noisy is not allowed to cost the stream
    # its band. (This is the pair that would catch a "fix" that simply broke the jet into pieces.)
    root, span, width, n = measure(ragged_band)
    check("ragged edge is still a band", root <= ROOT_MAX and span >= SPAN_MIN and width <= WIDTH_MAX,
          "root=%.2f span=%.2f width=%.2f core=%d px" % (root, span, width, n))

    if failures:
        print("test_flameband FAILED: " + "; ".join(failures))
        return 1
    print("test_flameband OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
