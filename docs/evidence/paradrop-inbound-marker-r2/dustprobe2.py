"""Measures the paradrop landing dust at zoom 240 from a before/after pair either side of the landing tick
(DropLandingDustStills.cs: CaptureRig.Shot, lamps and weather pinned so the frame differs by the drop and
nothing else), in a window centred on CaptureRig's own screen projection of the landing point (mark_px_x/y
in the after sidecar), not a hand-found window: usage: python docs/evidence/paradrop-inbound-marker-r2/dustprobe2.py

r1's probe flagged any big |after - before| change, which is exactly what a camp lamp rotating out of
NightLights.RealLamps' real-light budget looks like too (151,87,33 -> 34,34,42: a strong darkening, no colour
left behind). The landing dust is a puff of pale, washed-out smoke: it either gets brighter, or it stays about
as bright but loses its colour (the channel spread max-min falls). A pixel counts as dust only under one of
those two tests, so a darkening lamp and the newly landed men (also dark, also losing nothing of their colour)
both fail it.
"""
import json
import os
import sys
import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))
WINDOW = 90   # px either side of the mark


def luma(rgb):
    # Rec.709, 0..1: CaptureRig's own scale (CaptureRig.cs luma_mean/p50/p95)
    return (rgb[..., 0] * 0.2126 + rgb[..., 1] * 0.7152 + rgb[..., 2] * 0.0722) / 255.0


def main():
    before_path = os.path.join(HERE, "z240-before.png")
    after_path = os.path.join(HERE, "z240-after.png")
    after_sidecar = os.path.join(HERE, "z240-after.json")
    with open(after_sidecar, "r", encoding="utf-8") as f:
        sidecar = json.load(f)
    mark_x, mark_y = sidecar["mark_px_x"], sidecar["mark_px_y"]

    a = np.asarray(Image.open(before_path).convert("RGB")).astype(np.float32)
    b = np.asarray(Image.open(after_path).convert("RGB")).astype(np.float32)
    h_img, w_img = a.shape[0], a.shape[1]

    cx, cy = int(round(mark_x)), int(round(mark_y))
    x0, x1 = max(0, cx - WINDOW), min(w_img, cx + WINDOW)
    y0, y1 = max(0, cy - WINDOW), min(h_img, cy + WINDOW)
    wa, wb = a[y0:y1, x0:x1], b[y0:y1, x0:x1]

    luma_a, luma_b = luma(wa), luma(wb)
    luma_delta_px = luma_b - luma_a
    spread_a = wa.max(axis=2) - wa.min(axis=2)
    spread_b = wb.max(axis=2) - wb.min(axis=2)
    spread_delta = spread_b - spread_a

    brighter = luma_delta_px >= 0.04
    greyed = (luma_delta_px >= 0.0) & (spread_delta <= -0.06)
    mask = brighter | greyed

    opened = ndimage.binary_opening(mask, np.ones((3, 3)))
    closed = ndimage.binary_closing(opened, np.ones((3, 3)))
    labels, n = ndimage.label(closed)

    blob_w = blob_h = 0
    noise_px = int(closed.sum())
    if n > 0:
        sizes = ndimage.sum(closed, labels, index=range(1, n + 1))
        biggest = int(np.argmax(sizes)) + 1
        ys, xs = np.where(labels == biggest)
        blob_w = int(xs.max() - xs.min() + 1)
        blob_h = int(ys.max() - ys.min() + 1)
        noise_px = int(closed.sum() - sizes.max())

    ys_all, xs_all = np.where(mask)
    if len(xs_all) > 0:
        centroid_x = float(xs_all.mean()) + x0
        centroid_y = float(ys_all.mean()) + y0
        centre_off_px = float(np.hypot(centroid_x - mark_x, centroid_y - mark_y))
        luma_delta = float(luma_delta_px[ys_all, xs_all].mean())
    else:
        centre_off_px = 9999.0
        luma_delta = 0.0

    landing_dust_px_z240 = max(blob_w, blob_h)
    out = {
        "landing_dust_px_z240": landing_dust_px_z240,
        "centre_off_px": round(centre_off_px, 2),
        "luma_delta": round(luma_delta, 5),
        "blob_w": blob_w,
        "blob_h": blob_h,
        "noise_px": noise_px,
        "mark_px": [mark_x, mark_y],
    }
    out_path = os.path.join(HERE, "probe.json")
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
    print(json.dumps(out, indent=2))
    ok = landing_dust_px_z240 >= 12 and centre_off_px <= 30 and luma_delta >= 0.02
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
