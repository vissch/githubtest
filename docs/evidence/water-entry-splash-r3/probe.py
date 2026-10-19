"""Counts how many pixels the fix changed above the waterline at the death point.

The stills were shot with CaptureRig.Series(..., aimY: map.WaterLevel), so the camera aims at the water's
surface at the death point and that point lands on the image's centre row. before/after/noise.png are crops
taken symmetrically around the centre of the 1920x1080 frame, so their own centre row is still that waterline.

Above that row, inside the middle 30 % of the width (where the splash cards are), a pixel counts as changed
when its Rec.709 luma differs by more than 0.02.
  before vs after  = the Splash card at the riverbed vs at the water's surface (the fix).
  after vs noise   = the same build, same stage, shot twice: the floor the fix has to beat.
Writes probe.json beside this file.
"""
import json
import os

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 0.02


def luma(name):
    a = np.asarray(Image.open(os.path.join(HERE, name)).convert("RGB"), dtype=np.float32) / 255.0
    return 0.2126 * a[..., 0] + 0.7152 * a[..., 1] + 0.0722 * a[..., 2]


def changed(a, b):
    la, lb = luma(a), luma(b)
    if la.shape != lb.shape:
        raise SystemExit("the two stills are not the same size")
    h, w = la.shape
    d = np.abs(la - lb)[:h // 2, int(w * 0.35):int(w * 0.65)]   # above the waterline, middle 30 % of the width
    return int((d > THRESHOLD).sum())


splash = changed("before.png", "after.png")
noise = changed("after.png", "noise.png")
print("splash_px_t1", splash, " noise_px", noise)
with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
    json.dump({"splash_px_t1": splash, "noise_px": noise, "threshold_luma": THRESHOLD,
               "region": "above the centre row (the waterline at the death point), middle 30 % of the width"},
              f, indent=2)
