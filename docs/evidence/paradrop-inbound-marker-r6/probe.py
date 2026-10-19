"""Measures the paradrop landing dust at zoom 240 in honest units, at two ticks, with the puff's book hidden in a
twin shot of each (paradrop-inbound-marker-r6, the harsh critic's fix 3 on r5's dustprobe3.py):
usage: python docs/evidence/paradrop-inbound-marker-r6/probe.py

r5's probe had a unit bug the critic found: `dust_mask` took the channel spread from 0-255 pixel values but compared
its delta against -0.06, the scale of the 0..1 luma beside it. Any 1-level drop in spread therefore counted as dust,
and `dust_minus_nopuff_px` reached 1442 on pure render noise while the puff itself was invisible to the eye. Two
things change here:
  * the spread is divided by 255, so both terms of the mask are on 0..1 ("spread_scale": 1 in probe.json says so);
  * `dust_minus_nopuff_px` is no longer a mask area that mixes in the `greyed` term. It is the plain count of window
    pixels that got BRIGHTER by at least 0.05 luma when the puff's book was drawn rather than hidden - one term,
    no opening or closing, nothing that can be fattened by morphology. It is reported as the MINIMUM of the +3 and
    +6 pairs (each also kept as `_p3` / `_p6`), so the one number proves the puff at both ticks, not at its best one.

The stills come from DropLandingDustStills.cs in one Play session, all at the same pose (GreyboxCorridor at night,
zoom 240, yaw 21, pitch 25, lamps and rain pinned, the support disc masked), in the same +-90 px window around
CaptureRig's own screen projection of the landing point (`mark_px_x/y` in the sidecar):
  z240-before    -> z240-p3   : the landing dust itself, three ticks after the stick lands
  z240-p3        -> z240-p3b  : the noise floor (same frozen state, timeScale 0, no landing in between)
  z240-nopuff-p3 -> z240-p3   : the puff alone (the men and the ground are identical in both)
  z240-nopuff-p6 -> z240-p6   : the same, three ticks later
r5's other keys are kept, computed with the fixed spread, so fixes 1-2 stay honest: `landing_dust_px_z240` (the
biggest blob of the before/after pair), `noise_blob_px`, `luma_delta`, `noise_luma_delta`, `centre_off_px`, and
`yellow_px_delta` (the support disc's own hue/saturation band, which ShowOverlays = false should hold at 0).

Pass: dust_minus_nopuff_px >= 150 (the owner's number for this unit) with the r5 gates still holding. The 0.05 luma
threshold and the 3.25 px/m it is read against come from r2's measured pose
(docs/evidence/paradrop-inbound-marker-r2), not from re-deriving the renderer's own arithmetic.
"""
import json
import os
import sys
import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))
WINDOW = 90        # px either side of the mark
PUFF_LUMA = 0.05   # a pixel shows the puff when it brightened by this much luma (0..1)


def luma(rgb):
    # Rec.709, 0..1: CaptureRig's own scale (CaptureRig.cs luma_mean/p50/p95)
    return (rgb[..., 0] * 0.2126 + rgb[..., 1] * 0.7152 + rgb[..., 2] * 0.0722) / 255.0


def spread(rgb):
    # max channel minus min, divided by 255 so it is on 0..1 like the luma it is compared against. r5 left this on
    # 0-255 and compared it against 0.06, which is the bug this probe exists to fix.
    return (rgb.max(axis=2) - rgb.min(axis=2)) / 255.0


def window(img, mark_x, mark_y):
    h_img, w_img = img.shape[0], img.shape[1]
    cx, cy = int(round(mark_x)), int(round(mark_y))
    x0, x1 = max(0, cx - WINDOW), min(w_img, cx + WINDOW)
    y0, y1 = max(0, cy - WINDOW), min(h_img, cy + WINDOW)
    return img[y0:y1, x0:x1]


def dust_mask(wa, wb):
    luma_delta_px = luma(wb) - luma(wa)
    spread_delta = spread(wb) - spread(wa)
    brighter = luma_delta_px >= 0.04
    greyed = (luma_delta_px >= 0.0) & (spread_delta <= -0.06)
    return luma_delta_px, (brighter | greyed)


def puff_px(w_nopuff, w_puff):
    """Window pixels the puff brightened by >= PUFF_LUMA luma. One term, no morphology: nothing here can grow a
    count that the picture does not hold."""
    return int(((luma(w_puff) - luma(w_nopuff)) >= PUFF_LUMA).sum())


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
    # markMine, (1, 0.85, 0.3) at alpha 0.35 over the ground: blended, its hue sits near 45 deg and its saturation
    # near 0.4-0.5, well inside the dirt's own (lower-saturation, lower-hue) brown. A loose R-B/G-B test alone also
    # catches the brown dirt itself and the landing flare's much more saturated orange, so this narrows to the disc's
    # own band, then opens and closes (3x3) before counting: scattered dirt/flare pixels that fall in the band do not
    # form a blob, where a real disc would.
    h, s, v = rgb_to_hsv(w / 255.0)
    raw = (h >= 35) & (h <= 55) & (s >= 0.35) & (s <= 0.55) & (v >= 0.3)
    opened = ndimage.binary_opening(raw, np.ones((3, 3)))
    return ndimage.binary_closing(opened, np.ones((3, 3)))


def yellow_count(w):
    return int(yellow_mask(w).sum())


def load(name):
    return np.asarray(Image.open(os.path.join(HERE, name)).convert("RGB")).astype(np.float32)


def main():
    with open(os.path.join(HERE, "z240-p3.json"), "r", encoding="utf-8") as f:
        sidecar = json.load(f)
    mark_x, mark_y = sidecar["mark_px_x"], sidecar["mark_px_y"]

    def win(name):
        return window(load(name), mark_x, mark_y)

    w_before = win("z240-before.png")
    w_p3, w_p3b, w_nopuff3 = win("z240-p3.png"), win("z240-p3b.png"), win("z240-nopuff-p3.png")
    w_p6, w_nopuff6 = win("z240-p6.png"), win("z240-nopuff-p6.png")

    dust_delta, dust_m = dust_mask(w_before, w_p3)
    landing_dust_px_z240, _ = biggest_blob(dust_m)
    luma_delta = mean_luma_delta(dust_delta, dust_m)

    noise_delta, noise_m = dust_mask(w_p3, w_p3b)
    noise_blob_px, _ = biggest_blob(noise_m)
    noise_luma_delta = mean_luma_delta(noise_delta, noise_m)

    puff_p3, puff_p6 = puff_px(w_nopuff3, w_p3), puff_px(w_nopuff6, w_p6)
    dust_minus_nopuff_px = min(puff_p3, puff_p6)
    # the same count over the noise pair: the puff is drawn in both shots, so this is what the threshold alone picks
    # up on an unchanged frame. It must be far below the puff's own count.
    noise_puff_px = puff_px(w_p3, w_p3b)

    yellow_before, yellow_after = yellow_count(w_before), yellow_count(w_p3)
    yellow_px_delta = abs(yellow_after - yellow_before)

    ys_all, xs_all = np.where(dust_m)
    if len(xs_all) > 0:
        centroid_x = float(xs_all.mean()) + max(0, int(round(mark_x)) - WINDOW)
        centroid_y = float(ys_all.mean()) + max(0, int(round(mark_y)) - WINDOW)
        centre_off_px = float(np.hypot(centroid_x - mark_x, centroid_y - mark_y))
    else:
        centre_off_px = 9999.0

    out = {
        "spread_scale": 1,
        "dust_minus_nopuff_px": dust_minus_nopuff_px,
        "dust_minus_nopuff_px_p3": puff_p3,
        "dust_minus_nopuff_px_p6": puff_p6,
        "noise_puff_px": noise_puff_px,
        "puff_luma_threshold": PUFF_LUMA,
        "landing_dust_px_z240": landing_dust_px_z240,
        "noise_blob_px": noise_blob_px,
        "luma_delta": round(luma_delta, 5),
        "noise_luma_delta": round(noise_luma_delta, 5),
        "yellow_px_delta": yellow_px_delta,
        "centre_off_px": round(centre_off_px, 2),
        "yellow_before": yellow_before,
        "yellow_after": yellow_after,
        "mark_px": [mark_x, mark_y],
        "window_px": 2 * WINDOW,
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
    print(json.dumps(out, indent=2))

    ok = (
        dust_minus_nopuff_px >= 150
        and noise_puff_px * 3 <= dust_minus_nopuff_px
        and landing_dust_px_z240 >= 12
        and landing_dust_px_z240 >= 3 * noise_blob_px
        and luma_delta >= 2 * noise_luma_delta
        and yellow_px_delta == 0
    )
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
