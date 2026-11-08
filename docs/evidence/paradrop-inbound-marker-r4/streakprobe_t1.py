"""Counts the drop's falling streaks per man at the zone at the TACTICAL band (zoom 30, "T1"), from a
before/after pair either side of OnDropInbound. usage:
python docs/evidence/paradrop-inbound-marker-r4/streakprobe_t1.py

Why T1 and not the overview. r4 measured the overview band (zoom 120) and found 0 streaks of 8 even with the
8 ring points taken from CaptureRig's own projection (`streakprobe.py` -> `probe-o120.json`). Making the marker
read at the overview is options (b)/(c) of the open owner question in `docs/reference/decisions.md`, which says
"do not build any of these without asking", so the look is untouched. Default (a) claims the marker "reads at
T1" - never measured until this probe. At zoom 30 the view is 1.1547 * 30 = 34.6 m over 900 px = 26.0 px/m and
FlipbookFx.FarGrow(30) = 1, so a man's 3 m ShellFall card is ~78 px wide, against 29 px at zoom 120.

Method, the same counter as streakprobe.py with its window and tolerance scaled to T1. A per-pixel
|after - before| carries the weather and anything moving as well as the drop, so a component only counts as a
streak when its SHAPE and its PLACE both fit a ShellFall card: area >= 40 px, >= 10 px tall, taller than wide,
and its ground anchor (bottom edge, middle) within RING_TOL_PX of one of the 8 men's measured screen points
(`t1px.json`, measured by Rig.Mark at this exact camera - the stills' cam_x/y/z and the centre mark_px match it).
Each man is counted at most once. The same counter is run on the control pair (two stills of the untouched
scene, about the same frame gap as before -> after) for the noise floor.

Exit 0 only when streak_ratio >= 0.8 and noise_streaks <= 1.
"""
import glob
import json
import os
import sys

import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))

with open(os.path.join(HERE, "t1px.json"), encoding="utf-8") as f:
    RING_POINTS = [(p["px_x"], p["px_y"]) for p in json.load(f)["points"]]

RING_TOL_PX = 45.0         # half a 78 px card (39) + the 0.3 rad jitter CombatFx adds (~36 px at 7 m), rounded
HALF = 360                 # window half-size: the far ring is 11.5 m = 299 px out at 26.0 px/m
FLOOR = 60.0               # sum over RGB of |after - before|, 0-765
MIN_AREA_PX = 40
MIN_H_PX = 10


def streak_components(before_path, after_path, mark_px):
    a = np.asarray(Image.open(before_path).convert("RGB")).astype(np.float32)
    b = np.asarray(Image.open(after_path).convert("RGB")).astype(np.float32)
    mx, my = int(round(mark_px[0])), int(round(mark_px[1]))
    x0, y0 = max(0, mx - HALF), max(0, my - HALF)
    diff = np.abs(b - a).sum(axis=2)[y0:my + HALF, x0:mx + HALF]
    mask = ndimage.binary_opening(diff >= FLOOR, np.ones((2, 2)))
    labels, n = ndimage.label(mask)
    if n == 0:
        return 0
    objs = ndimage.find_objects(labels)
    sizes = ndimage.sum(mask, labels, index=range(1, n + 1))
    matched = set()
    for sl, sz in zip(objs, sizes):
        h = sl[0].stop - sl[0].start
        w = sl[1].stop - sl[1].start
        if sz < MIN_AREA_PX or h < MIN_H_PX or h < w:
            continue
        cx = (sl[1].start + sl[1].stop) / 2.0 + x0            # the card's ground anchor: bottom edge, middle
        by = sl[0].stop + y0
        for i, (px, py) in enumerate(RING_POINTS):
            if i in matched:
                continue
            if ((cx - px) ** 2 + (by - py) ** 2) ** 0.5 <= RING_TOL_PX:
                matched.add(i)
                break
    return len(matched)


def disc_stats(before_path, after_path, px, py, r=20):
    a = np.asarray(Image.open(before_path).convert("RGB")).astype(np.float32)
    b = np.asarray(Image.open(after_path).convert("RGB")).astype(np.float32)
    diff = np.abs(b - a).sum(axis=2)
    x, y = int(round(px)), int(round(py))
    disc = diff[y - r:y + r, x - r:x + r]
    return {"max": float(disc.max()), "p999": float(np.percentile(disc, 99.9)), "mean": float(disc.mean())}


def main():
    here = HERE
    with open(os.path.join(here, "t1-after.json"), encoding="utf-8") as f:
        sidecar = json.load(f)
    mark_px = (sidecar["mark_px_x"], sidecar["mark_px_y"])
    before = os.path.join(here, "t1-before.png")
    after = os.path.join(here, "t1-after.png")        # t = 2.45 s, the frame nearest 2.5 s at the run's own 20.0 fps
    control_b = os.path.join(here, "t1-control.png")
    control_a = os.path.join(here, "t1-control-b.png")
    noise = streak_components(control_b, control_a, mark_px)
    series = {os.path.basename(p): streak_components(before, p, mark_px)
              for p in sorted(glob.glob(os.path.join(here, "t1-after_*.png")))}
    series[os.path.basename(after)] = streak_components(before, after, mark_px)
    raw = streak_components(before, after, mark_px)
    men = 8
    streaks = max(0, raw - noise)

    # per man, in a 20 px disc on the measured point: the drop's own amplitude against the control pair's
    # amplitude in the same disc - a measurement, not a fitted threshold.
    amplitudes = []
    for i, (px, py) in enumerate(RING_POINTS):
        drop = disc_stats(before, after, px, py)
        ctrl = disc_stats(control_b, control_a, px, py)
        ratio = drop["max"] / ctrl["max"] if ctrl["max"] > 0 else float("inf")
        amplitudes.append({"k": i, "px_x": px, "px_y": py, "drop": drop, "control": ctrl,
                           "max_ratio": ratio, "clears_3x": ratio >= 3.0})

    out = {
        "band": "T1 (zoom 30), focus (45.00, 82.20), yaw 30, pitch 25, 1600x900, 26.0 px/m",
        "streaks": streaks,
        "men": men,
        "streak_ratio": streaks / men,
        "raw_streaks": raw,
        "noise_streaks": noise,
        "series": series,
        "window_half_px": HALF,
        "floor": FLOOR,
        "ring_tol_px": RING_TOL_PX,
        "amplitudes": amplitudes,
        "amplitudes_clearing_3x": sum(1 for a in amplitudes if a["clears_3x"]),
    }
    with open(os.path.join(here, "probe.json"), "w", encoding="utf-8", newline="\n") as f:
        json.dump(out, f, indent=2)
    print(json.dumps(out, indent=2))
    sys.exit(0 if out["streak_ratio"] >= 0.8 and noise <= 1 else 1)


if __name__ == "__main__":
    main()
