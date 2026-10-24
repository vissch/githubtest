"""Where is the white in the jet, and what kind of white is it? (flame-jet-opaque-near-2-g2, step 6)

`probe.py` says HOW MUCH white sits in the last third; this says WHERE along the stream, WHAT it is made of, and
WHETHER it is a loop. Read only: it prints and exits 0, writes nothing, and changes no bar.

Three readings per still, all on `probe.py`'s own fire mask and white rule so the two tools agree about what is
fire and what is white (white = luma >= 215 and r - b <= 45: bright and not warm):

  per tenth      white px / fire px in each tenth of the fire span, nozzle (1) to tip (10). Part 2 of
                 measurements.md measured 5-15% in every tenth with the peak at the sixth or seventh, so the core
                 chain past the hot third cannot be the whole story.
  clipped/cream  of the white px, the share that is CLIPPED (r, g and b all >= 250 - a blown-out pixel, which is
                 what several additive cards stacking makes) against the share that is CREAM (r - b in 20..45 - a
                 slightly warm off-white, the signature of Book.Core's cream crescent). Clipped accuses the
                 stacking of bright cards anywhere in the chain; cream accuses the Core glyph itself.
  components     the connected pieces (8-neighbour) of white inside the LAST THIRD, biggest first, with bbox,
                 area, and how much of each piece lies within 3 px of the fire mask's own contour. A piece that
                 hugs the contour is a card's outline drawn round the stream - the "loop" the owner saw. A piece
                 sitting inside the mask is a bright card face, not an outline.

  usage: python docs/evidence/flame-jet-opaque-near-2-g2/whitemap.py [image.png ...]
         (no arguments: before-1/2/3.png and jet-t2-1/2/3.png, whichever exist)
"""
import os, sys
import numpy as np
from scipy import ndimage

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe                                     # the fire mask, the white rule and rgb(), one definition only

HERE = os.path.dirname(os.path.abspath(__file__))
HUG_PX = 3.0          # within this of the mask contour counts as hugging it
HUG_SHARE = 0.5       # a piece more than half hugging is called a loop
TOP = 5               # how many pieces to print per still


def tenths(mask, x0, span):
    """The fire mask cut into ten bands along the aim, nozzle first."""
    for k in range(10):
        a = x0 + int(round(span * k / 10.0))
        b = x0 + int(round(span * (k + 1) / 10.0))
        band = np.zeros_like(mask)
        band[:, a:b] = True
        yield band & mask


def report(path):
    img = probe.rgb(path)
    m = probe.firemask(img)
    w = probe.white(img) & m
    xs = np.where(m.any(axis=0))[0]
    x0, x1 = int(xs[0]), int(xs[-1])
    span = x1 - x0 + 1
    print("%s   fire x %d..%d (%d px), fire %d px, white %d px (%.4f of the fire)"
          % (os.path.basename(path), x0, x1, span, m.sum(), w.sum(), w.sum() / float(max(1, m.sum()))))

    bins = []
    for band in tenths(m, x0, span):
        f = band.sum()
        bins.append((w & band).sum() / float(f) if f else 0.0)
    print("  white per tenth (nozzle -> tip): " + " ".join("%.3f" % b for b in bins))

    r, g, b = img[..., 0], img[..., 1], img[..., 2]
    clipped = (r >= 250) & (g >= 250) & (b >= 250)
    cream = ((r - b) >= 20) & ((r - b) <= 45)
    n = float(max(1, w.sum()))
    print("  of the white: clipped %.3f (%d px), cream %.3f (%d px), neither %.3f"
          % ((clipped & w).sum() / n, (clipped & w).sum(), (cream & w).sum() / n, (cream & w).sum(),
             (w & ~clipped & ~cream).sum() / n))

    # how far every pixel is from the OUTSIDE of the fire mask: 0 outside, 1 on the contour, upwards inwards
    depth = ndimage.distance_transform_edt(m)
    last = m.copy()
    last[:, : x1 - span // 3] = False
    wl = w & last
    lab, cnt = ndimage.label(wl, structure=np.ones((3, 3)))
    print("  last third: white %d px in %d pieces" % (wl.sum(), cnt))
    if cnt == 0:
        return
    areas = ndimage.sum(wl, lab, range(1, cnt + 1))
    for k in np.argsort(areas)[::-1][:TOP]:
        piece = lab == (k + 1)
        ys, pxs = np.where(piece)
        hug = float((depth[piece] <= HUG_PX).mean())
        print("    piece area %5d px  bbox x %4d..%-4d y %4d..%-4d  %4.0f%% within %.0f px of the contour  -> %s"
              % (piece.sum(), pxs.min(), pxs.max(), ys.min(), ys.max(), 100 * hug, HUG_PX,
                 "a loop on the contour" if hug > HUG_SHARE else "inside the mask: a card face"))


def main(argv):
    paths = argv[1:]
    if not paths:
        paths = [os.path.join(HERE, n + ".png")
                 for n in ("before-1", "before-2", "before-3", "jet-t2-1", "jet-t2-2", "jet-t2-3")]
        paths = [p for p in paths if os.path.exists(p)]
    if not paths:
        print("no stills to map", file=sys.stderr)
        return 0
    for p in paths:
        if not os.path.exists(p):
            print("missing: %s" % p, file=sys.stderr)
            continue
        report(p)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
