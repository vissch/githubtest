"""Do the lit pixels form a BAND from muzzle to target, or a blob round the man?

  python Tools/flameband.py <png|jpg> --muzzle x,y --target x,y
  python Tools/flameband.py <png|jpg> --sidecar <json>     # {"muzzle":[x,y],"target":[x,y]}

Tools/flamecheck.py answers "is there fire in this picture" and look-03 passed it with 143028 lit pixels while the
master still said "no shaped stream from muzzle to target": a glow round the man and firelight washed over the mud
count the same as a jet. So this measures the SHAPE, projecting every lit pixel onto the muzzle->target axis:

  u   along the axis, 0 at the muzzle, 1 at the target
  v   across it, in the same units (one unit = the whole run)

and reports three numbers, all three of which must pass:

  reach     the fraction of 10 bins of u in [0.05, 0.95] that hold at least 1% of the lit pixels.
            >= 0.8: the lit pixels actually SPAN the run.  A glow round the man covers the first bin or two.
  centroid  the mean u.  >= 0.30: the light's weight is out along the stream, not sat on the man (where it is ~0).
  width     the 90th percentile of |v| over the lit pixels in the covered bins.  <= 0.16: the jet reaches 11 m
            (Flamethrower.Reach) and is about 3.5 m across at its widest, so half of it is 1.75 m, 0.16 of the run,
            and a stream keeps essentially ALL of its light inside that.  A wash over the mud does not.

`width` was derived from the jet, not chosen to make a picture pass, and it was derived AFTER the first draft of
this check said BAND on look-03's after_close.jpg - a picture the master had already rejected by eye. What moved,
and why, so the next reader can judge the measure rather than trust it:
  - the threshold is 0.16, read off Reach = 11 m and a widest thickness of ~3.5 m; the round 0.20 guessed first has
    no geometry behind it.
  - the statistic is the 90th percentile, not the median. A MEDIAN only asks that half the light be near the axis,
    and look-03's picture passes that honestly (p50 = 0.15 inside the run): the man's glow and the lit mud happen to
    lie along the line to the trench. It is the other half that is the wash - p90 = 0.42, two and a half times the
    corridor a jet would occupy. A band is tight for nearly all of its light, not for half of it.
On look-03's picture it reads reach=1.00 centroid=0.63 width=0.42 -> BLOB, exit 1. Note that reach and centroid
both PASS there: the firelight really does run the whole way to the trench. What it is not is narrow.

The fire mask is flamecheck.py's, copied verbatim: that file is a script, not a module, and the two must agree on
what "lit" means or the two verdicts cannot be read side by side.
"""
import argparse, json, sys
import numpy as np
from PIL import Image
from scipy import ndimage

REACH_MIN, CENTROID_MIN, WIDTH_MAX = 0.8, 0.30, 0.16
BINS, BIN_SHARE, LO, HI = 10, 0.01, 0.05, 0.95


def fire_mask(path):
    """flamecheck.py's four lines, unchanged."""
    x = np.asarray(Image.open(path).convert("RGB")).astype(np.float32)
    lum = 0.299 * x[..., 0] + 0.587 * x[..., 1] + 0.114 * x[..., 2]
    rb = x[..., 0] - x[..., 2]
    fire = ((rb >= 60) & (lum >= 70)) | ((lum >= 190) & (rb >= 15))
    return ndimage.binary_closing(ndimage.binary_opening(fire, np.ones((3, 3))), np.ones((5, 5)))


def band(mask, muzzle, target):
    """reach, centroid, width, lit-pixel count.  Pixel coordinates are (x = column, y = row)."""
    ys, xs = np.nonzero(mask)
    n = int(xs.size)
    if n == 0:
        return 0.0, 0.0, 9.9, 0
    mx, my = float(muzzle[0]), float(muzzle[1])
    ax, ay = float(target[0]) - mx, float(target[1]) - my
    length2 = ax * ax + ay * ay
    if length2 <= 0:
        raise SystemExit("muzzle and target are the same pixel")
    length = length2 ** 0.5
    dx, dy = xs - mx, ys - my
    u = (dx * ax + dy * ay) / length2
    v = np.abs(dx * ay - dy * ax) / length2          # the cross product over L*L: |v| in units of the run
    centroid = float(u.mean())

    edges = np.linspace(LO, HI, BINS + 1)
    covered = 0
    pooled = np.zeros(u.shape, dtype=bool)   # the lit pixels of every covered bin, pooled: see the note above
    for i in range(BINS):
        inside = (u >= edges[i]) & (u < edges[i + 1])
        if int(inside.sum()) >= BIN_SHARE * n:
            covered += 1
            pooled |= inside
    reach = covered / float(BINS)
    width = float(np.percentile(v[pooled], 90)) if covered else 9.9
    return reach, centroid, width, n


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("image")
    ap.add_argument("--muzzle", help="x,y in pixels")
    ap.add_argument("--target", help="x,y in pixels")
    ap.add_argument("--sidecar", help='json beside the picture: {"muzzle":[x,y],"target":[x,y]}')
    a = ap.parse_args(argv)

    if a.sidecar:
        with open(a.sidecar, "r", encoding="utf-8") as fh:
            side = json.load(fh)
        muzzle, target = side["muzzle"], side["target"]
    elif a.muzzle and a.target:
        muzzle = [float(t) for t in a.muzzle.split(",")]
        target = [float(t) for t in a.target.split(",")]
    else:
        ap.error("give --muzzle and --target, or --sidecar")

    reach, centroid, width, n = band(fire_mask(a.image), muzzle, target)
    ok = reach >= REACH_MIN and centroid >= CENTROID_MIN and width <= WIDTH_MAX
    print("%s reach=%.2f (>=%.2f %s)  centroid=%.2f (>=%.2f %s)  width=%.2f (<=%.2f %s)  lit=%d px  %s" % (
        "BAND " if ok else "BLOB ",
        reach, REACH_MIN, "ok" if reach >= REACH_MIN else "FAIL",
        centroid, CENTROID_MIN, "ok" if centroid >= CENTROID_MIN else "FAIL",
        width, WIDTH_MAX, "ok" if width <= WIDTH_MAX else "FAIL",
        n, a.image))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
