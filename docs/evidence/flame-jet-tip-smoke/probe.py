"""Measure the dark plume past the flamethrower jet's tip, from the pixels of the T2 night stills beside this file.

Harsh critic r1 (41/100), fix 2 on flame-jet-core (owner, 2026-10-19): "no dark smoke past the jet's tip (jet-t2
before/after: orange cards only) and no probe key for it." The owner's rule, verbatim:

    smoke_px_past_tip: pixels within 200 px right of the stream's end whose luma is >= 15 below the plate

The stream's end (its tip) is found from the fire mask in jet-t2.png - the rightmost column any fire pixel reaches,
since the capture frames the jet aimed screen-right (flameshots' own convention). The window is the 200 px strip
immediately to its right. A pixel in that strip counts if the jet still's luma is at least 15 (of 255, Rec.601,
shotstats.py's own scale) below the SAME pixel in plate-t2.png, the identical pose with no jet fired at all.

Writes probe.json beside itself; nothing in it is typed by hand.   usage: python docs/evidence/flame-jet-tip-smoke/probe.py
"""
import json, os, sys
import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))
JET = os.path.join(HERE, "jet-t2.png")
PLATE = os.path.join(HERE, "plate-t2.png")
WINDOW_PX = 200
DROP = 15.0


def rgb(path):
    return np.asarray(Image.open(path).convert("RGB")).astype(np.float32)


def luma(x):
    return 0.299 * x[..., 0] + 0.587 * x[..., 1] + 0.114 * x[..., 2]


def firemask(x):
    lum, rb = luma(x), x[..., 0] - x[..., 2]
    m = ((rb >= 60) & (lum >= 70)) | ((lum >= 190) & (rb >= 15))
    m = ndimage.binary_closing(ndimage.binary_opening(m, np.ones((3, 3))), np.ones((5, 5)))
    lab, n = ndimage.label(m)
    if n == 0:
        return m
    sizes = ndimage.sum(m, lab, range(1, n + 1))
    return lab == (int(np.argmax(sizes)) + 1)


def main():
    jet, plate = rgb(JET), rgb(PLATE)
    if jet.shape != plate.shape:
        print("the jet still and the plate are different sizes", file=sys.stderr)
        return 1
    m = firemask(jet)
    if m.sum() < 15000:
        print("no fire in %s - do not measure this capture" % JET, file=sys.stderr)
        return 1
    h, w, _ = jet.shape
    xs = np.where(m.any(axis=0))[0]
    tip_x = int(xs[-1])   # the stream's end: the rightmost fire pixel, aimed screen-right
    x0, x1 = tip_x + 1, min(w, tip_x + 1 + WINDOW_PX)

    jet_lum, plate_lum = luma(jet), luma(plate)
    drop = plate_lum[:, x0:x1] - jet_lum[:, x0:x1]
    smoke = drop >= DROP
    smoke_px_past_tip = int(smoke.sum())

    print("stream tip at x=%d   window x %d..%d (%d px wide)" % (tip_x, x0, x1, x1 - x0))
    print("smoke_px_past_tip = %d   (want >= 3000)" % smoke_px_past_tip)
    out = {
        "tip_x": tip_x,
        "window_px": WINDOW_PX,
        "drop_bar": DROP,
        "smoke_px_past_tip": smoke_px_past_tip,
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    return 0 if smoke_px_past_tip >= 3000 else 1


if __name__ == "__main__":
    sys.exit(main())
