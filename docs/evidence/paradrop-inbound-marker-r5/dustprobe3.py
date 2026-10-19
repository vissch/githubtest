"""Measures the paradrop landing dust at zoom 240 against a measured noise floor, the support disc masked, and the
landed men split from the puff (paradrop-inbound-marker-r5, the harsh critic's fixes 1-3 on r2's dustprobe2.py):
usage: python docs/evidence/paradrop-inbound-marker-r5/dustprobe3.py

r2's run measured one before/after pair and a hand threshold; the harsh critic found the "after" shot's own render
noise was never measured (so a 0.003 luma edge case cannot be told from the puff), the support disc's HUD marker
moved 173 px between the two shots (CombatFx.ShowOverlays, now held false for every shot), and the 8 landed men in
the window were never split from the puff. This probe runs the same dust-mask pipeline (Rec.709 luma 0..1; a pixel
counts when it brightened by >= 0.04 or did not darken and its channel spread fell by >= 0.06; 3x3 open then close;
connected components; blob = max(w, h)) over three pairs shot by DropLandingDustStills.cs in one Play session:
  z240-before  -> z240-after   : the landing dust itself
  z240-after   -> z240-after2  : the noise floor (same frozen state, one build, no landing in between)
  z240-nopuff  -> z240-after   : the dust with the puff's book hidden, so the delta is the men alone; the puff's own
                                  contribution is landing_dust_px_z240 minus this blob (dust_minus_nopuff_px)
and the support disc (markMine, CombatFx.cs ~281, (1, 0.85, 0.3) at alpha 0.35) counted in the same window on
z240-before and z240-after by its own blended hue/saturation band (yellow_mask below: a plain R-B/G-B test also
catches this scene's brown dirt at full strength and the landing flare's much brighter orange, so it is narrowed to
the disc's own band and, like the dust mask, opened then closed before counting — scattered dirt or flare pixels in
that band do not form a blob, a real disc would). ShowOverlays held false for every shot should hold
yellow_px_delta at 0.
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


def window(img, mark_x, mark_y):
    h_img, w_img = img.shape[0], img.shape[1]
    cx, cy = int(round(mark_x)), int(round(mark_y))
    x0, x1 = max(0, cx - WINDOW), min(w_img, cx + WINDOW)
    y0, y1 = max(0, cy - WINDOW), min(h_img, cy + WINDOW)
    return img[y0:y1, x0:x1]


def dust_mask(wa, wb):
    luma_a, luma_b = luma(wa), luma(wb)
    luma_delta_px = luma_b - luma_a
    spread_a = wa.max(axis=2) - wa.min(axis=2)
    spread_b = wb.max(axis=2) - wb.min(axis=2)
    spread_delta = spread_b - spread_a
    brighter = luma_delta_px >= 0.04
    greyed = (luma_delta_px >= 0.0) & (spread_delta <= -0.06)
    return luma_delta_px, (brighter | greyed)


def biggest_blob(mask):
    opened = ndimage.binary_opening(mask, np.ones((3, 3)))
    closed = ndimage.binary_closing(opened, np.ones((3, 3)))
    labels, n = ndimage.label(closed)
    if n == 0:
        return 0, 0.0
    sizes = ndimage.sum(closed, labels, index=range(1, n + 1))
    biggest = int(np.argmax(sizes)) + 1
    ys, xs = np.where(labels == biggest)
    blob_w, blob_h = int(xs.max() - xs.min() + 1), int(ys.max() - ys.min() + 1)
    return max(blob_w, blob_h), float(sizes.max())


def mean_luma_delta(luma_delta_px, mask):
    ys, xs = np.where(mask)
    if len(xs) == 0:
        return 0.0
    return float(luma_delta_px[ys, xs].mean())


def rgb_to_hsv(rgb01):
    r, g, b = rgb01[..., 0], rgb01[..., 1], rgb01[..., 2]
    mx, mn = np.maximum(np.maximum(r, g), b), np.minimum(np.minimum(r, g), b)
    diff = mx - mn
    h = np.zeros_like(mx)
    safe = diff > 1e-6
    rmax, gmax, bmax = safe & (mx == r), safe & (mx == g) & ~(mx == r), safe & (mx == b) & ~(mx == r) & ~(mx == g)
    h[rmax] = (60 * (((g[rmax] - b[rmax]) / diff[rmax]) % 6))
    h[gmax] = (60 * (((b[gmax] - r[gmax]) / diff[gmax]) + 2))
    h[bmax] = (60 * (((r[bmax] - g[bmax]) / diff[bmax]) + 4))
    s = np.where(mx > 1e-6, diff / np.maximum(mx, 1e-6), 0.0)
    return h, s, mx


def yellow_mask(w):
    # markMine, (1, 0.85, 0.3) at alpha 0.35 over the ground: blended, its hue sits near 45 deg and its
    # saturation near 0.4-0.5, well inside the dirt's own (lower-saturation, lower-hue) brown. A loose
    # R-B/G-B test alone also catches the brown dirt itself (its own R-B spans 25-255 in this scene) and
    # the landing flare's much more saturated orange, so this narrows to the disc's own band, then — like
    # the dust mask — opens then closes (3x3) before counting: scattered dirt/flare pixels that happen to
    # fall in the band do not form a blob, where a real disc would.
    h, s, v = rgb_to_hsv(w / 255.0)
    raw = (h >= 35) & (h <= 55) & (s >= 0.35) & (s <= 0.55) & (v >= 0.3)
    opened = ndimage.binary_opening(raw, np.ones((3, 3)))
    return ndimage.binary_closing(opened, np.ones((3, 3)))


def yellow_count(w):
    return int(yellow_mask(w).sum())


def load(name):
    return np.asarray(Image.open(os.path.join(HERE, name)).convert("RGB")).astype(np.float32)


def main():
    with open(os.path.join(HERE, "z240-after.json"), "r", encoding="utf-8") as f:
        sidecar = json.load(f)
    mark_x, mark_y = sidecar["mark_px_x"], sidecar["mark_px_y"]

    before, after = load("z240-before.png"), load("z240-after.png")
    after2, nopuff = load("z240-after2.png"), load("z240-nopuff.png")

    w_before, w_after = window(before, mark_x, mark_y), window(after, mark_x, mark_y)
    w_after2, w_nopuff = window(after2, mark_x, mark_y), window(nopuff, mark_x, mark_y)

    dust_delta, dust_m = dust_mask(w_before, w_after)
    landing_dust_px_z240, _ = biggest_blob(dust_m)
    luma_delta = mean_luma_delta(dust_delta, dust_m)

    noise_delta, noise_m = dust_mask(w_after, w_after2)
    noise_blob_px, _ = biggest_blob(noise_m)
    noise_luma_delta = mean_luma_delta(noise_delta, noise_m)

    # the puff is a diffuse cloud with a soft falloff, not the before/after pair's sharp edge: opened then closed
    # (biggest_blob's pipeline) it erodes to nothing even where it plainly changed the frame, so this is the raw
    # mask area (px changed, not a blob's longest side) between the puff hidden and drawn.
    _, nopuff_m = dust_mask(w_nopuff, w_after)
    dust_minus_nopuff_px = int(nopuff_m.sum())

    yellow_before, yellow_after = yellow_count(w_before), yellow_count(w_after)
    yellow_px_delta = abs(yellow_after - yellow_before)

    ys_all, xs_all = np.where(dust_m)
    if len(xs_all) > 0:
        centroid_x, centroid_y = float(xs_all.mean()) + max(0, int(round(mark_x)) - WINDOW), float(ys_all.mean()) + max(0, int(round(mark_y)) - WINDOW)
        centre_off_px = float(np.hypot(centroid_x - mark_x, centroid_y - mark_y))
    else:
        centre_off_px = 9999.0

    out = {
        "landing_dust_px_z240": landing_dust_px_z240,
        "noise_blob_px": noise_blob_px,
        "luma_delta": round(luma_delta, 5),
        "noise_luma_delta": round(noise_luma_delta, 5),
        "yellow_px_delta": yellow_px_delta,
        "dust_minus_nopuff_px": dust_minus_nopuff_px,
        "centre_off_px": round(centre_off_px, 2),
        "yellow_before": yellow_before,
        "yellow_after": yellow_after,
        "mark_px": [mark_x, mark_y],
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
    print(json.dumps(out, indent=2))

    ok = (
        landing_dust_px_z240 >= 12
        and landing_dust_px_z240 >= 3 * noise_blob_px
        and luma_delta >= 2 * noise_luma_delta
        and yellow_px_delta == 0
        and dust_minus_nopuff_px >= 12
    )
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
