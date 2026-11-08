"""Counts the drop's falling streaks per man at the zone, from a before/after pair either side of
OnDropInbound (CaptureRig.Shot, one pose, weather pinned, overlays and HUD off, the tactical camera itself
at zoom 120 so FlipbookFx.FarGrow sizes the cards for the band they are photographed at). usage:
python docs/evidence/paradrop-inbound-marker-r4/streakprobe.py    (writes probe-o120.json; the T1 band is streakprobe_t1.py -> probe.json)

Method. A per-pixel |after - before| carries the men walking and the muzzle flashes as well as the drop, so
a component only counts as a streak when its SHAPE and its PLACE both fit a ShellFall card. r2 fix 3's
PLACE test computed where the 8 men's rings should be from the pose by formula (PX_PER_M, an ellipse for
pitch 25); r4 replaces that with `ringpx.json`, the 8 points measured by CaptureRig's own screen projection
(Rig.Mark) at this same pose (see `ringpx.cs`). A man counts once when a diff component's bottom edge
(its ground anchor) is within RING_TOL_PX of that man's measured point - half a 29 px card (3 m x
FarGrow(120) = 4.5 m at 6.50 px/m) plus the 0.3 rad jitter CombatFx adds, which moves a point at 7 m by
up to ~14 px.

The mark is CaptureRig.Rig.Mark projected by the rig itself (mark_px_x/y in the sidecar), so the window is
found by projection and never by eye. The same counter is run on the control pair - two stills of the
unchanged scene, the same number of frames apart as before -> after - for the noise floor.

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

with open(os.path.join(HERE, "ringpx.json"), encoding="utf-8") as f:
    RING_POINTS = [(p["px_x"], p["px_y"]) for p in json.load(f)["points"]]

RING_TOL_PX = 20.0         # half a 29 px card (14.5) + up to ~14 px of jitter at 7 m, rounded up
HALF = 140                 # window half-size: the far ring is 75 px out, a card ~30 px tall over it
FLOOR = 60.0               # sum over RGB of |after - before|, 0-765
MIN_AREA_PX = 40
MIN_H_PX = 10


def streak_components(before_path, after_path, mark_px):
    a = np.asarray(Image.open(before_path).convert("RGB")).astype(np.float32)
    b = np.asarray(Image.open(after_path).convert("RGB")).astype(np.float32)
    mx, my = int(round(mark_px[0])), int(round(mark_px[1]))
    diff = np.abs(b - a).sum(axis=2)[my - HALF:my + HALF, mx - HALF:mx + HALF]
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
        cx = (sl[1].start + sl[1].stop) / 2.0 - HALF + mx    # the card's ground anchor: bottom edge, middle
        by = sl[0].stop - HALF + my
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
    with open(os.path.join(here, "z120-after.json"), encoding="utf-8") as f:
        sidecar = json.load(f)
    mark_px = (sidecar["mark_px_x"], sidecar["mark_px_y"])
    before = os.path.join(here, "z120-before.png")
    after = os.path.join(here, "after_04.png")
    control_b = os.path.join(here, "z120-control.png")
    control_a = os.path.join(here, "z120-control-b.png")
    noise = streak_components(control_b, control_a, mark_px)
    series = {os.path.basename(p): streak_components(before, p, mark_px)
              for p in sorted(glob.glob(os.path.join(here, "after_*.png")))}
    raw = streak_components(before, os.path.join(here, "z120-after.png"), mark_px)
    men = 8
    streaks = max(0, raw - noise)

    # step 4: per man, in a 20 px disc on the measured point, the drop's own amplitude against the
    # control pair's amplitude in the same disc - a measurement, not a fitted threshold.
    amplitudes = []
    for i, (px, py) in enumerate(RING_POINTS):
        drop = disc_stats(before, after, px, py)
        ctrl = disc_stats(control_b, control_a, px, py)
        ratio = drop["max"] / ctrl["max"] if ctrl["max"] > 0 else float("inf")
        amplitudes.append({"k": i, "px_x": px, "px_y": py, "drop": drop, "control": ctrl,
                            "max_ratio": ratio, "clears_3x": ratio >= 3.0})

    out = {
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
    with open(os.path.join(here, "probe-o120.json"), "w", encoding="utf-8", newline="\n") as f:
        json.dump(out, f, indent=2)
    print(json.dumps(out, indent=2))
    sys.exit(0 if out["streak_ratio"] >= 0.8 and noise <= 1 else 1)


if __name__ == "__main__":
    main()
