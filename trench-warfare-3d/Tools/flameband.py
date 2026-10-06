"""Is there a STREAM from the muzzle to the target - judged apart from the light that stream throws?

  python Tools/flameband.py <png|jpg> --muzzle x,y --target x,y
  python Tools/flameband.py <png|jpg> --sidecar <json>     # {"muzzle":[x,y],"target":[x,y]}

Tools/flamecheck.py answers "is there fire in this picture". This answers "is the fire a stream leaving the
weapon", which is the question the master keeps asking and the one three rounds of the jet kept failing.

WHY THIS WAS REWRITTEN (look-06). The first version projected every LIT pixel onto the muzzle->target axis and
asked for reach, centroid and a narrow 90th-percentile width. It was red on all twelve of flame2's pictures, and
the reason it was red is that at close range it was not measuring the stream at all. A flamethrower lights the mud
it passes over (two FireLight stations, 9 m), leaves burning pools, and sets men on the parapet alight. All of that
is deliberate, the owner has said it stays, and all of it is lit, warm and spread over half the frame - so the
width of "the lit pixels" is the width of the FIRELIGHT, not of the jet, and no jet that lights anything could ever
pass. The measure was asking the picture to give up a design decision.

Two changes, and they are a different statistic rather than a looser one:

  1. THE CORE, not everything warm. Luminance >= 215 as well as warm. A stream is self-luminous and over-bright;
     firelight on mud is a tint on a surface and sits well under that. This alone is not enough - the chain of
     flipbook cards the jet used to be drawn with is over-bright too.
  2. ONE CONNECTED PIECE. The largest connected component of that core IS the candidate stream, and only it is
     measured. A stream is one object. A glow at the man with a separate blob of fire four metres out - which is
     exactly what the master described in flame2/after_day_side.jpg - is two objects, and the biggest of them is
     short and detached.

and then three numbers off that one piece, all of which must pass:

  root   the smallest u the piece reaches, 0 at the muzzle and 1 at the target. <= 0.30: the stream is ATTACHED TO
         THE NOZZLE. This is the number that separates the two generations of the jet most cleanly - measured, the
         card runs 0.16 by night and by day, the chain 0.52 by night and 0.97 by day (a detached blob near the
         target with nothing joining it to the weapon).
  span   u95 - u05 over the piece. >= 0.28: it is a RUN, not a bolus. Card 0.37 / 0.31, chain 0.25 / 0.08.
  width  the 90th percentile of |v| over the piece, in units of the run. <= 0.16: the jet reaches 11 m
         (Flamethrower.Reach) and the owner's head width is 3.90 m, so half of it is 1.95 m, 0.18 of the run, and
         nearly all of a stream's core sits inside that. Unchanged from the first version and derived the same way.

Nothing here was tuned until a picture passed. The thresholds sit between the two generations with margin on both
sides, and the pre-registered verdict for look-06 was: red on before_side and before_day_side (the chain), green on
after_side and after_day_side (the card). Measured: root 0.52 / 0.97 against 0.16 / 0.16; span 0.25 / 0.08 against
0.37 / 0.31.

A SECOND VERDICT, --wiggle (look-09). The band measure says the fire is a stream; it says nothing about whether
that stream is SHAPED like fire. look-06's card passed the band and the master still rejected it: "a RULER-STRAIGHT
wedge, its top and bottom edges straight lines for hundreds of pixels". So --wiggle measures the one thing that
sentence is about - how far the stream's UPPER CONTOUR departs from a straight line - and it is deliberately the
HARSHEST reading available: per column the topmost row of the stream piece, then a 150-column window slid along it,
a least-squares line fitted inside each window, and the SMALLEST residual RMS of any window reported. One ruler
-straight stretch anywhere along the stream is enough to fail it, which is exactly the complaint.

  wiggle >= 2.5 px in every 150-column window: the edge is BROKEN, exit 0.
  wiggle <  2.5 px somewhere: RULER, exit 1.
  the piece spans fewer than 400 columns: CANNOT JUDGE, exit 2, never a pass. A 150 px window needs a long
  contour to mean anything, and the close shot (over the shoulder) and the 120 m shot do not have one.

Pre-registered, and measured on the pictures the master rejected: flame3/after_side.jpg 1.59 px and
flame3/after_day_side.jpg 0.32 px - both RULER, both red. The threshold sits above both with margin and below the
+-6 px ragged edge of test_flameband.py's hermetic case.

What this measure still cannot do: the CLOSE shot is taken over the man's shoulder, where the target projects
behind the muzzle and u runs past 1.7, so root and span mean nothing there; and at the standard view (120 m) the
whole run is sixty pixels and the core is a few dozen. Measure the SIDE shots with this and read the other two by
eye. The fire mask flamecheck.py uses - "is there fire at all" - is untouched and still lives in that file.
"""
import argparse, json, sys
import numpy as np
from PIL import Image
from scipy import ndimage

ROOT_MAX, SPAN_MIN, WIDTH_MAX = 0.30, 0.28, 0.16
WIGGLE_MIN, WIGGLE_WINDOW, WIGGLE_COLUMNS = 2.5, 150, 400
CORE_LUM, CORE_WARM = 215.0, 25.0


def core_mask(path):
    """The over-bright, warm pixels: the body of the fire itself, not the light it lends the ground."""
    x = np.asarray(Image.open(path).convert("RGB")).astype(np.float32)
    lum = 0.299 * x[..., 0] + 0.587 * x[..., 1] + 0.114 * x[..., 2]
    rb = x[..., 0] - x[..., 2]
    return ndimage.binary_opening((lum >= CORE_LUM) & (rb >= CORE_WARM), np.ones((3, 3)))


def stream(mask):
    """The largest connected piece of the core. A stream is ONE object; a wash and a blob are several."""
    labels, count = ndimage.label(mask, np.ones((3, 3)))
    if count == 0:
        return np.zeros_like(mask), 0
    sizes = ndimage.sum(mask, labels, range(1, count + 1))
    return labels == (int(np.argmax(sizes)) + 1), count


def band(mask, muzzle, target):
    """root, span, width, and the piece's pixel count.  Pixel coordinates are (x = column, y = row)."""
    piece, _ = stream(mask)
    ys, xs = np.nonzero(piece)
    n = int(xs.size)
    if n == 0:
        return 9.9, 0.0, 9.9, 0
    mx, my = float(muzzle[0]), float(muzzle[1])
    ax, ay = float(target[0]) - mx, float(target[1]) - my
    length2 = ax * ax + ay * ay
    if length2 <= 0:
        raise SystemExit("muzzle and target are the same pixel")
    dx, dy = xs - mx, ys - my
    u = (dx * ax + dy * ay) / length2
    v = np.abs(dx * ay - dy * ax) / length2          # the cross product over L*L: |v| in units of the run
    root = float(u.min())
    span = float(np.percentile(u, 95) - np.percentile(u, 5))
    width = float(np.percentile(v, 90))
    return root, span, width, n


def wiggle(mask):
    """The smallest 150-column straight-line residual RMS of the stream's upper contour, in pixels.

    Returns (wiggle, columns). `columns` is how many columns the piece spans; under WIGGLE_COLUMNS the number
    cannot be judged and main() says so rather than passing it.
    """
    piece, _ = stream(mask)
    ys, xs = np.nonzero(piece)
    if xs.size == 0:
        return 0.0, 0
    cols = np.unique(xs)
    top = np.array([ys[xs == c].min() for c in cols], dtype=np.float64)   # the topmost row per column
    n = int(cols.size)
    if n < WIGGLE_WINDOW:
        return 0.0, n
    worst = None
    for i in range(0, n - WIGGLE_WINDOW + 1):
        wx, wy = cols[i:i + WIGGLE_WINDOW].astype(np.float64), top[i:i + WIGGLE_WINDOW]
        m, c = np.polyfit(wx, wy, 1)
        rms = float(np.sqrt(np.mean((wy - (m * wx + c)) ** 2)))
        worst = rms if worst is None else min(worst, rms)
    return float(worst), n


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("image")
    ap.add_argument("--muzzle", help="x,y in pixels")
    ap.add_argument("--target", help="x,y in pixels")
    ap.add_argument("--sidecar", help='json beside the picture: {"muzzle":[x,y],"target":[x,y]}')
    ap.add_argument("--wiggle", action="store_true",
                    help="judge the EDGE instead: is the stream's upper contour ruler-straight anywhere?")
    a = ap.parse_args(argv)

    if a.wiggle:
        # The edge verdict needs no muzzle and no target: it is a property of the stream's own contour.
        w, cols = wiggle(core_mask(a.image))
        if cols < WIGGLE_COLUMNS:
            print("CANNOT JUDGE  wiggle: the stream spans %d columns (<%d) - too short a contour to fit a %d px "
                  "window to  %s" % (cols, WIGGLE_COLUMNS, WIGGLE_WINDOW, a.image))
            return 2
        print("%s wiggle=%.2f px (>=%.2f %s)  over %d columns, worst %d px window  %s" % (
            "RAGGED" if w >= WIGGLE_MIN else "RULER ",
            w, WIGGLE_MIN, "ok" if w >= WIGGLE_MIN else "FAIL", cols, WIGGLE_WINDOW, a.image))
        return 0 if w >= WIGGLE_MIN else 1

    if a.sidecar:
        with open(a.sidecar, "r", encoding="utf-8") as fh:
            side = json.load(fh)
        muzzle, target = side["muzzle"], side["target"]
    elif a.muzzle and a.target:
        muzzle = [float(t) for t in a.muzzle.split(",")]
        target = [float(t) for t in a.target.split(",")]
    else:
        ap.error("give --muzzle and --target, or --sidecar")

    root, span, width, n = band(core_mask(a.image), muzzle, target)
    ok = root <= ROOT_MAX and span >= SPAN_MIN and width <= WIDTH_MAX
    print("%s root=%.2f (<=%.2f %s)  span=%.2f (>=%.2f %s)  width=%.2f (<=%.2f %s)  core=%d px  %s" % (
        "BAND " if ok else "BLOB ",
        root, ROOT_MAX, "ok" if root <= ROOT_MAX else "FAIL",
        span, SPAN_MIN, "ok" if span >= SPAN_MIN else "FAIL",
        width, WIDTH_MAX, "ok" if width <= WIDTH_MAX else "FAIL",
        n, a.image))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
