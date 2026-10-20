#!/usr/bin/env python3
"""The measuring code of this set, in one place so the staging sweep and the probe measure alike.

Every function here is lifted unchanged from the previous set's probe (docs/evidence/
atrifle-look-r3-g1/probe.py, commit 1eb46aa6aa), which is what makes a sweep reading and a probe
reading comparable: the same luma threshold, the same open/close, the same box the capture itself
projected for the man it drew, the same largest-component rule and the same centroid-aligned 1-IoU.
Nothing here decides anything; staging.py and probe.py do that, each with its own gates.
"""
import json
import os

import numpy as np
import scipy.ndimage as nd
from PIL import Image

THRESHOLD = 0.04            # Rec.709 luma, as in r3
MAN_TALL_M = 2.6            # the height CaptureRig.ManBox projects, so tall_px is 2.6 m of frame
MIN_AREA_M2 = 0.2
MAX_BOX_SHARE = 0.5         # of his own box: a man who fills half his box is not a silhouette
BOX_HALF_W = 0.8            # of tall_px, either side of his feet
BOX_UP = 1.0                # of tall_px, above his feet
BOX_DOWN = 0.2              # of tall_px, below them
DOMINANCE = 1.5


def sidecar(d, stem):
    with open(os.path.join(d, stem + ".json"), encoding="utf-8") as f:
        return json.load(f)


def luma(d, stem):
    a = np.asarray(Image.open(os.path.join(d, stem + ".png")).convert("RGB"), float) / 255.0
    return a[..., 0] * 0.2126 + a[..., 1] * 0.7152 + a[..., 2] * 0.0722


def raw_diff(d, stem, plate):
    """The difference mask of one still against a plate: luma threshold, 3x3 open, 7x7 close."""
    m = np.abs(luma(d, stem) - luma(d, plate)) > THRESHOLD
    return nd.binary_closing(nd.binary_opening(m, np.ones((3, 3))), np.ones((7, 7)))


def drawn_men(d, stem, shape):
    """Every man the capture says it drew INSIDE this frame: slot -> (img_x, img_y, tall_px).

    A man written off the map still gets a projection, far outside the picture; those are dropped
    here, so "the men in this still" is the capture's own answer and not a typed slot number."""
    s = sidecar(d, stem)
    h, w = shape
    out = {}
    i = 0
    while ("man%d_slot" % i) in s:
        x, y = float(s["man%d_img_x" % i]), float(s["man%d_img_y" % i])
        tall = float(s["man%d_tall_px" % i])
        if 0 <= x < w and 0 <= y < h and tall > 1.0:
            out[int(s["man%d_slot" % i])] = (x, y, tall)
        i += 1
    return out


def box_of(x, y, tall, shape):
    """His own box, and whether each side of it is a real frame edge (so a cut can be told apart
    from a mask that simply stops where the box does). img_y is his FEET, counted from the top."""
    h, w = shape
    x0, x1 = x - BOX_HALF_W * tall, x + BOX_HALF_W * tall
    y0, y1 = y - BOX_UP * tall, y + BOX_DOWN * tall
    i0, i1 = max(0, int(round(x0))), min(w, int(round(x1)) + 1)
    j0, j1 = max(0, int(round(y0))), min(h, int(round(y1)) + 1)
    m = np.zeros(shape, bool)
    m[j0:j1, i0:i1] = True
    cuts = {"left": i0 == 0 and x0 < 0, "right": i1 == w and x1 >= w,
            "top": j0 == 0 and y0 < 0, "bottom": j1 == h and y1 >= h}
    return m, (i0, i1, j0, j1), cuts


def biggest_two(m):
    """(the largest component, its area, the second area) of a boolean mask; (None, 0, 0) if empty."""
    lab, n = nd.label(m)
    if n == 0:
        return None, 0, 0
    sizes = np.bincount(lab.ravel())
    sizes[0] = 0
    order = np.argsort(sizes)[::-1]
    second = int(sizes[order[1]]) if n > 1 else 0
    return lab == int(order[0]), int(sizes[order[0]]), second


def centred(m):
    """The mask shifted so its centroid is at the origin, on a common canvas. No scale, no rotation."""
    h, w = m.shape
    ys, xs = np.nonzero(m)
    cy, cx = int(round(ys.mean())), int(round(xs.mean()))
    out = np.zeros((2 * h, 2 * w), bool)
    out[ys - cy + h, xs - cx + w] = True
    return out


def diff(a, b):
    """1 - IoU of two masks after each has been centred on its own centroid."""
    a, b = centred(a), centred(b)
    union = int((a | b).sum())
    return 1.0 - float((a & b).sum()) / union if union else 0.0


def man_in_box(d, stem, plate, noise, shape):
    """The largest surviving blob inside the box the capture drew for the one man in this still.

    Returns a dict with the box, the mask, its area, the runner-up and the weather that falls inside
    the box at all. `why` says what went wrong when there is nothing to measure."""
    men = drawn_men(d, stem, shape)
    if len(men) != 1:
        return {"why": "the capture drew %d men in frame, want 1" % len(men)}
    slot, (x, y, tall) = next(iter(men.items()))
    bm, span, cuts = box_of(x, y, tall, shape)
    raw = raw_diff(d, stem, plate) & bm
    cleaned = raw & ~noise
    m, px, second = biggest_two(cleaned)
    return {"slot": slot, "box": list(span), "cuts": cuts, "box_px": int(bm.sum()),
            "tall_px": tall, "px_per_m": tall / MAN_TALL_M, "mask": m, "mask_px": px,
            "second_px": second, "noise_in_box_px": int((noise & bm).sum()),
            "dropped_px": int(raw.sum() - cleaned.sum()),
            "why": None if m is not None else "nothing in his box differs from the plate"}
