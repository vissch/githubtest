#!/usr/bin/env python3
"""Measures the pixels of the stills committed beside this file.

The question: does the AT rifleman read as his own man, or as the Sniper with another name?

r2 answered it with an IoU of 0.43, and the harsh critic was right to throw that number out: the
two stills were framed on two different focus points (atrifle 50.37, sniper 52.09), so the man sat
at a different place in the frame and the overlap it measured was partly the men and partly that
offset. Here the focus is (50, 25) for every still in the set — the AT rifleman, the Sniper and both
their plates — with zoom 30 (T1), pitch 22 and 1600x900 shared too, and the MAN moved to that focus
rather than the camera moved to the man. The focus point projects to the same pixels at any camera
yaw, so both men sit inside one window and the masks are comparable.

One thing could not be made equal: the men's own heading. A man at a post is snapped to that POST's
facing, and the two do not take the same post at the same point (the prone AT rifleman a kind-2 post
facing -2.29 rad, the kneeling Sniper a kind-1 post facing 2.04 rad); neither budges, and a written
Yaw is overwritten. So the camera yaw is turned by exactly the difference between the two headings
and nothing else is touched — AT rifleman yaw -2.29 at camera yaw 243.6, Sniper yaw 2.04 at camera
yaw 131.69 — which holds cam_yaw minus man_yaw equal for both. Each man is therefore seen from the
same relative angle, side-on, at the same height and distance through the same lens, and the two
silhouettes are comparable as silhouettes. The ground behind them differs, because the camera
orbited; that never reaches the measure, because every mask is a difference against a plate shot
from that man's OWN camera with that man, and only that man, moved off the map. frames.txt says all
of this again at length.

A pixel is "the man" when its Rec.709 luma differs from the plate's by more than 0.04. The plate is
the same camera pose with that man, and only that man, moved off the map. Two guards against the
night's flicker reading as a man, carried over from r2:
  * a 3x3 opening drops grain speckle, then a 7x7 closing joins the parts of one man (boots, back,
    helmet) that the mud happens to match;
  * only the window around the focus is read. The lamps, the fires and the far trench, which flicker
    between any two frames, fall outside it.

Three things are stricter than r2:
  * ONE window for every still, because every still now shares one focus, lens and size;
  * a loud failure if any mask pixel touches the window border — a cut mask is not a silhouette;
  * MIN_MASK_PX is 400, not 2000: a man at T1 (zoom 30) is about a fifth of his T2 size, and
    MIN_BLOB is 300, because the plate is shot with the sim let off its freeze for a moment (the
    presenter will not move a man while the sim is frozen), so the mud and the pools near him
    flicker in blobs of up to 270 px. The man measures about 876 px: nothing smaller is him.

Before anything was changed, the current build was captured three times at that pose
(base-at-1..3.png). The spread of those repeats is written as
`silhouette_diff_vs_sniper_base_max` — the largest 1 - IoU between any two of them, measured the
same way the claim is. It is the noise floor of the measure itself: a difference the fixed build
claims has to sit above it. The fixed build is captured three
times too (fix-at-1..3.png) and `silhouette_diff_vs_sniper` is the MIN of its three readings, so the
claim is the worst of the three, never the luckiest.

Every number here is read from the pixels. None is typed.
"""
import json
import os
import sys

import numpy as np
import scipy.ndimage as nd
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 0.04
WINDOW = (slice(300, 405), slice(720, 880))   # y, x — one window, every still (one focus)
MIN_BLOB = 300
MIN_MASK_PX = 400
PASS_DIFF = 0.6

BASE = ["base-at-1.png", "base-at-2.png", "base-at-3.png"]
FIX = ["fix-at-1.png", "fix-at-2.png", "fix-at-3.png"]


def here(name):
    return os.path.join(HERE, name)


def luma(path):
    a = np.asarray(Image.open(here(path)).convert("RGB"), float) / 255.0
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


def border_touch(m):
    """True when the mask reaches the edge of the window, which means it is cut."""
    ys, xs = WINDOW
    box = m[ys, xs]
    if not box.any():
        return False
    return bool(box[0].any() or box[-1].any() or box[:, 0].any() or box[:, -1].any())


def diff(a, b):
    """1 - IoU of two masks in the same frame coordinates."""
    union = int((a | b).sum())
    return 1.0 - float((a & b).sum()) / union if union else 0.0


def pose_errors(names):
    out = {}
    for n in names:
        p = here(os.path.splitext(n)[0] + ".json")
        if os.path.exists(p):
            with open(p, encoding="utf-8") as f:
                d = json.load(f)
            out[n] = (float(d["pose_error_m"]), float(d["rain"]))
    return out


def main():
    report = {}
    problems = []

    def built(stills, plate):
        masks = []
        for s in stills:
            if not os.path.exists(here(s)) or not os.path.exists(here(plate)):
                continue
            m = mask(s, plate)
            px = int(m.sum())
            report["mask_px_" + os.path.splitext(s)[0]] = px
            if px < MIN_MASK_PX:
                problems.append(f"{s}: only {px} px of man in the window (want {MIN_MASK_PX})")
            if border_touch(m):
                problems.append(f"{s}: the mask touches the window border — it is cut")
            masks.append(m)
        return masks

    base = built(BASE, "base-plate.png")
    fix = built(FIX, "fix-plate.png")

    sniper = None
    if os.path.exists(here("sniper.png")) and os.path.exists(here("sniper-plate.png")):
        sniper = mask("sniper.png", "sniper-plate.png")
        report["mask_px_sniper"] = int(sniper.sum())
        if int(sniper.sum()) < MIN_MASK_PX:
            problems.append(f"sniper.png: only {int(sniper.sum())} px of man in the window")
        if border_touch(sniper):
            problems.append("sniper.png: the mask touches the window border — it is cut")

    if len(base) >= 2:
        pairs = [diff(base[i], base[j])
                 for i in range(len(base)) for j in range(i + 1, len(base))]
        report["silhouette_diff_vs_sniper_base_max"] = round(max(pairs), 4)
    report["n_captures"] = len(fix)
    if fix and sniper is not None:
        readings = [diff(m, sniper) for m in fix]
        report["silhouette_diff_vs_sniper_readings"] = [round(r, 4) for r in readings]
        report["silhouette_diff_vs_sniper"] = round(min(readings), 4)

    poses = pose_errors(BASE + FIX + ["base-plate.png", "fix-plate.png",
                                      "sniper.png", "sniper-plate.png"])
    if poses:
        report["pose_error_m_max"] = max(v[0] for v in poses.values())
        report["rain_max"] = max(v[1] for v in poses.values())
        if report["pose_error_m_max"] >= 0.5:
            problems.append(f"pose_error_m is {report['pose_error_m_max']}, want under 0.5")
        if report["rain_max"] != 0:
            problems.append(f"rain is {report['rain_max']}, want 0")
    report["threshold_luma"] = THRESHOLD
    report["window_xywh"] = [WINDOW[1].start, WINDOW[0].start,
                             WINDOW[1].stop - WINDOW[1].start,
                             WINDOW[0].stop - WINDOW[0].start]
    report["problems"] = problems

    with open(here("probe.json"), "w", encoding="utf-8") as f:
        json.dump(report, f, indent=2)
        f.write("\n")
    print(json.dumps(report, indent=2))

    if problems:
        for p in problems:
            print("FAIL: " + p, file=sys.stderr)
        return 2
    if "silhouette_diff_vs_sniper" in report:
        base_max = report.get("silhouette_diff_vs_sniper_base_max")
        if base_max is None or not base_max < PASS_DIFF <= report["silhouette_diff_vs_sniper"]:
            print("FAIL: the AT rifleman does not read as his own man", file=sys.stderr)
            return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
