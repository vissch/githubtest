"""Counts the falling streaks per man at the paradrop's zone, from a before/after pair either side of
OnDropInbound (CaptureRig.Shot, same pose, weather pinned, overlays off). usage:
python docs/evidence/paradrop-inbound-marker-r4/streakprobe.py

A per-pixel |after - before| still carries rain speckle (scattered single pixels); the streaks are small,
roughly-upright blobs (taller than wide) clustered around the drop's mark (CaptureRig.Rig.Mark, read from the
after sidecar's mark_px_x/y, not a hand-found window). Connected-component labelling on the opened/closed diff
gives each blob's own bounding box; a component counts as a streak when its height is at least its width and
it sits within the search window above the zone.
"""
import json
import os
import sys
import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))


def streak_components(before_path, after_path, mark_px, floor=40.0, half=300):
    """mark_px = (x, y) from the after sidecar's mark_px_x/y. half: the search window's half-width/height in
    pixels around the mark, found once by projection (not by eye) and kept fixed for the same pose."""
    a = np.asarray(Image.open(before_path).convert("RGB")).astype(np.float32)
    b = np.asarray(Image.open(after_path).convert("RGB")).astype(np.float32)
    diff = np.abs(b - a).sum(axis=2)
    mx, my = mark_px
    y0, y1 = max(0, int(my - half)), int(my + half)
    x0, x1 = max(0, int(mx - half)), int(mx + half)
    win = diff[y0:y1, x0:x1]
    mask = win >= floor
    mask = ndimage.binary_opening(mask, np.ones((2, 2)))
    mask = ndimage.binary_closing(mask, np.ones((2, 2)))
    labels, n = ndimage.label(mask)
    if n == 0:
        return 0
    objs = ndimage.find_objects(labels)
    sizes = ndimage.sum(mask, labels, index=range(1, n + 1))
    streaks = 0
    for sl, sz in zip(objs, sizes):
        if sl is None or sz < 8:
            continue
        h = sl[0].stop - sl[0].start
        w = sl[1].stop - sl[1].start
        if h >= w:
            streaks += 1
    return streaks


def main():
    here = HERE
    before = os.path.join(here, "z120-before.png")
    after = os.path.join(here, "z120-after.png")
    with open(os.path.join(here, "z120-after.json"), encoding="utf-8") as f:
        after_sidecar = json.load(f)
    mark_px = (after_sidecar["mark_px_x"], after_sidecar["mark_px_y"])
    streaks = streak_components(before, after, mark_px)
    men = 8
    out = {"streaks": streaks, "men": men, "streak_ratio": streaks / men}
    out_path = os.path.join(here, "probe.json")
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
    print(json.dumps(out, indent=2))
    sys.exit(0 if out["streak_ratio"] >= 0.8 else 1)


if __name__ == "__main__":
    main()
