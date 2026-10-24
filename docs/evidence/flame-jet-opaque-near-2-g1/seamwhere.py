"""WHERE is the jet's internal ridge, in image pixels? A look-only diagnostic (round 5, 2026-10-24).

Round 4 left `seam_score_max` at 11.427, carried entirely by capture 1 (age 1.3 s), and blamed the glyph being
"clipped by the quad". That reading does not hold on the code: `Flamethrower.cs:655` sets the sheet's width to
`HotThird * len * 1.30f / sFill` and the height divides by the Ink band, so the quad is LARGER than the ink and
`Flipbook_URP.shader:105` samples the whole cell. Nothing is clipped.

The anomaly is in the committed numbers instead: capture 1's near mask is 46558 px against 28063 and 31154, and its
mask starts at x 720 against 645 and 654, while all three x spans agree within 13% (530 / 574 / 598). Half again as
much fire, in a window shifted 70 px right, says the largest connected piece at age 1.3 s may not be the stream from
the nozzle at all but a merged blob - most likely the burning ground pool (`Flamethrower.cs:1165`, `Book.Pool`, a
`Ground` card on a flat plane, whose top and bottom are straight and horizontal). `probe.py`'s docstring already
promises its mask is "the stream, not the pool of burning fuel on the ground".

This script changes nothing and shoots nothing. It reuses probe.py's own `firemask`, `edges`, `near_and_tip` and
`seam_score` unchanged, and for each of the six committed stills prints: every connected component of the raw
warm-and-bright mask (top three, size and bbox, and which one firemask picks); the near-third x window, its pixel
count and the inner px left after the 13x13 erosion; `seam_score`'s best angle and score and, at that angle, the
three rows carrying the most edge energy mapped back to image y; and the inner mask's top and bottom y in ten x
buckets, so a flat-bottomed ground band shows up as a constant bottom.

Given a folder it reads that folder's stills instead, skipping the ones it does not have, so a single fresh capture
outside the checkout can be read without touching committed evidence (round 6, 2026-10-24).

Round 7 ("the mask comes from the renderer") changed `probe.firemask` to take the still's own `.mask.png` stencil
and refuse without one; this script follows through `probe.mask_for`, skipping a still with no mask beside it.
`raw_components` still reimplements the bare warm-and-bright rule on purpose: it is what the stencil is meant to
narrow, and showing it unnarrowed is the point of this diagnostic.

  usage: python docs/evidence/flame-jet-opaque-near-2-g1/seamwhere.py [stills dir]
"""
import os, sys
import numpy as np
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import probe  # __main__-guarded: the import measures nothing


def raw_components(x):
    """firemask's mask before the largest-piece pick, so we can see what it threw away."""
    lum, rb = probe.luma(x), x[..., 0] - x[..., 2]
    m = ((rb >= 60) & (lum >= 70)) | ((lum >= 190) & (rb >= 15))
    m = ndimage.binary_closing(ndimage.binary_opening(m, np.ones((3, 3))), np.ones((5, 5)))
    lab, n = ndimage.label(m)
    out = []
    for i in range(1, n + 1):
        c = lab == i
        ys, xs = np.where(c)
        out.append((int(c.sum()), (int(xs.min()), int(xs.max()), int(ys.min()), int(ys.max()))))
    out.sort(key=lambda t: -t[0])
    return out, n


def ridge_rows(em, near):
    """seam_score's own geometry, then the three strongest rows at its best angle, mapped back to image y."""
    inner = ndimage.binary_erosion(near, np.ones((13, 13)))
    if inner.sum() < 1000:
        return None
    band = np.where(inner, em, 0.0).astype(np.float32)
    cov = inner.astype(np.float32)
    ys, xs = np.where(inner)
    y0, y1, x0, x1 = int(ys.min()), int(ys.max()) + 1, int(xs.min()), int(xs.max()) + 1
    band, cov = band[y0:y1, x0:x1], cov[y0:y1, x0:x1]
    width = x1 - x0
    base = float(band.sum()) / max(1.0, float(cov.sum()))
    best = None
    for ang in range(-90, 90, 2):
        rb = ndimage.rotate(band, ang, order=1, reshape=True, cval=0.0)
        rc = ndimage.rotate(cov, ang, order=1, reshape=True, cval=0.0)
        run = rc.sum(axis=1)
        keep = run >= 0.5 * width
        if not keep.any():
            continue
        idx = np.where(keep)[0]
        means = rb.sum(axis=1)[keep] / run[keep]
        top = float(means.max()) / base
        if best is None or top > best[0]:
            order = np.argsort(-means)[:3]
            rows = [(int(idx[k]), float(means[k] / base), rb.shape[0]) for k in order]
            best = (top, float(ang), rows)
    if best is None:
        return None
    top, ang, rows = best
    named = []
    for r, ratio, nrows in rows:
        if ang == 0.0:
            named.append((ratio, "image y %d" % (y0 + r), r / max(1, nrows - 1)))
        else:
            named.append((ratio, "row %d of %d" % (r, nrows), r / max(1, nrows - 1)))
    return top, ang, named, inner, (y0, y1, x0, x1)


def buckets(inner, x0, x1):
    out = []
    step = max(1, (x1 - x0) // 10)
    for b in range(10):
        a, z = x0 + b * step, min(x1, x0 + (b + 1) * step)
        if z <= a:
            continue
        sub = inner[:, a:z]
        ys = np.where(sub.any(axis=1))[0]
        out.append("%d-%d:%s" % (a, z, "none" if ys.size == 0 else "%d..%d" % (ys.min(), ys.max())))
    return out


def report(root, label):
    jets, plate = probe.paths(root)
    jets = [p for p in jets if os.path.exists(p)]
    if not jets:
        print(chr(10) + "%s: no jet-t2-*.png in %s" % (label, root))
    for path in jets:
        print("\n%s %s" % (label, os.path.basename(path)))
        x = probe.rgb(path)
        comps, n = raw_components(x)
        print("  raw components: %d; top three (size, x0..x1, y0..y1):" % n)
        for i, (size, bb) in enumerate(comps[:3]):
            print("    #%d %7d px  x %4d..%-4d  y %4d..%-4d%s"
                  % (i + 1, size, bb[0], bb[1], bb[2], bb[3], "   <- firemask picks this" if i == 0 else ""))
        stencil, reason = probe.mask_for(path)
        if stencil is None:
            print("  %s - skipping" % reason)
            continue
        m = probe.firemask(x, stencil)
        near, tip, mx0, mx1, span = probe.near_and_tip(m)
        inner = ndimage.binary_erosion(near, np.ones((13, 13)))
        nxs = np.where(near.any(axis=0))[0]
        print("  mask x %d..%d (%d px span); near third x %d..%d, near %d px, inner after 13x13 erosion %d px"
              % (mx0, mx1, span, int(nxs[0]), int(nxs[-1]), int(near.sum()), int(inner.sum())))
        r = ridge_rows(probe.edges(probe.luma(x)), near)
        if r is None:
            print("  seam_score: refuses, under 1000 inner px")
            continue
        top, ang, named, inner2, (y0, y1, ix0, ix1) = r
        print("  seam_score %.3f @ %+.0f deg; inner bbox y %d..%d, x %d..%d" % (top, ang, y0, y1 - 1, ix0, ix1 - 1))
        for ratio, where, frac in named:
            print("    row ratio %6.3f  %-16s  (%.2f down the inner bbox)" % (ratio, where, frac))
        print("  inner top..bottom y per x bucket:")
        print("    " + "  ".join(buckets(inner2, ix0, ix1)))


def main(argv):
    if argv:
        report(os.path.abspath(argv[0]), "fresh")
        return 0
    report(HERE, "new")
    report(os.path.join(HERE, "baseline"), "baseline")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
