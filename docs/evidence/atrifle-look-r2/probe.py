#!/usr/bin/env python3
"""Measures the pixels of the stills committed beside this file.

The question: does the AT rifle read as its own man, or as the Sniper with another name?
Each single-man still has an empty plate shot from the same camera pose seconds later, with
the man moved off the map and nothing else touched, so the two differ by the man and by the
night's own flicker. A pixel is "the man" when its Rec.709 luma moved by more than 0.04.

Two guards against the flicker reading as a man:
  * a 3x3 opening drops the grain speckle and the rain streaks, then a 7x7 closing joins the
    parts of one man that the mud happens to match (his boots, his back, his helmet);
  * only the window around the focus point is read (x 640..960, y 150..520 of 1600x900). Both
    men stand at the focus of their own shot, framed at the same zoom, yaw and pitch, so the
    window holds the man and nothing else; the lamps and the far trench, which flicker between
    any two frames, fall outside it. No alignment is needed: frame coordinates are enough.
Blobs under 150 px inside the window are dropped as leftover speckle.

Exits non-zero if either mask is under 2,000 px: that means the man was not in frame, which
must never read as a pass.
"""
import json
import os
import sys

import numpy as np
import scipy.ndimage as nd
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 0.04
WINDOW = (slice(150, 520), slice(640, 960))   # y, x
MIN_BLOB = 150
MIN_MASK_PX = 2000
PASS_IOU = 0.7


def luma(path):
    a = np.asarray(Image.open(os.path.join(HERE, path)).convert("RGB"), float) / 255.0
    return a[..., 0] * 0.2126 + a[..., 1] * 0.7152 + a[..., 2] * 0.0722


def mask(still, plate):
    d = np.abs(luma(still) - luma(plate)) > THRESHOLD
    d = nd.binary_closing(nd.binary_opening(d, np.ones((3, 3))), np.ones((7, 7)))
    windowed = np.zeros_like(d)
    windowed[WINDOW] = d[WINDOW]
    lab, n = nd.label(windowed)
    sizes = np.bincount(lab.ravel())
    sizes[0] = 0
    keep = [i for i in range(1, n + 1) if sizes[i] >= MIN_BLOB]
    return np.isin(lab, keep)


def pose_error(*names):
    out = []
    for n in names:
        with open(os.path.join(HERE, n), encoding="utf-8") as f:
            out.append(float(json.load(f)["pose_error_m"]))
    return max(out)


def main():
    at = mask("atrifle.png", "empty-a.png")
    sn = mask("sniper.png", "empty-b.png")
    union = int((at | sn).sum())
    iou = float((at & sn).sum()) / union if union else 1.0
    report = {
        "silhouette_iou_vs_sniper": round(iou, 4),
        "mask_px_atrifle": int(at.sum()),
        "mask_px_sniper": int(sn.sum()),
        "pose_error_m_max": pose_error("atrifle.json", "sniper.json", "empty-a.json",
                                       "empty-b.json", "t2-pair.json"),
        "threshold_luma": THRESHOLD,
        "window_xywh": [640, 150, 320, 370],
        "pass": bool(iou <= PASS_IOU),
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(report, f, indent=2)
        f.write("\n")
    print(json.dumps(report, indent=2))
    if report["mask_px_atrifle"] < MIN_MASK_PX or report["mask_px_sniper"] < MIN_MASK_PX:
        print("FAIL: a man is missing from his still", file=sys.stderr)
        return 2
    return 0 if report["pass"] else 1


if __name__ == "__main__":
    sys.exit(main())
