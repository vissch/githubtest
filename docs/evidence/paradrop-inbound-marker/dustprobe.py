"""Measures the paradrop landing dust in pixels at zoom 240, from a before/after pair either side of the
landing tick (CaptureRig.Shot, weather pinned so the frame differs by the drop and nothing else). usage:
python docs/evidence/paradrop-inbound-marker/dustprobe.py

A per-pixel |after - before| still carries the rain streaks moving between the two frames: scattered single
pixels all over the image, never a solid patch. The landing dust is the opposite — a single, dense, local
blob where the new DustPuff cards are drawn — so closing+opening (as flamecheck.py does for fire) separates
the one from the other, and connected-component labelling gives the blob's own bounding box in pixels,
not the whole frame's.
"""
import json
import os
import sys
import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))


def blob_width_px(before_path, after_path, floor=100.0, search=None):
    """search = (y0, y1, x0, x1): the still is full of rain, a thin speckle scattered over the whole frame at
    every threshold tried; the landing dust is the one place it is a solid patch, found first in the full
    frame (the biggest connected blob survives rain at a high floor) and then re-measured in a window around
    it, where the opening that kills the rain's single pixels no longer also eats the dust's edges."""
    a = np.asarray(Image.open(before_path).convert("RGB")).astype(np.float32)
    b = np.asarray(Image.open(after_path).convert("RGB")).astype(np.float32)
    diff = np.abs(b - a).sum(axis=2)
    if search is not None:
        y0, y1, x0, x1 = search
        diff = diff[y0:y1, x0:x1]
    mask = diff >= floor
    mask = ndimage.binary_opening(mask, np.ones((3, 3)))
    mask = ndimage.binary_closing(mask, np.ones((3, 3)))
    labels, n = ndimage.label(mask)
    if n == 0:
        return 0, 0, 0
    sizes = ndimage.sum(mask, labels, index=range(1, n + 1))
    biggest = int(np.argmax(sizes)) + 1
    ys, xs = np.where(labels == biggest)
    w = int(xs.max() - xs.min() + 1)
    h = int(ys.max() - ys.min() + 1)
    noise_px = int(mask.sum() - sizes.max())
    return w, h, noise_px


def main():
    before = os.path.join(HERE, "z240-before2.png")
    after = os.path.join(HERE, "z240-after2.png")
    # the drop was called and shot at (45, 200), focus = the still's centre; the window below is where that
    # point actually renders for this CaptureRig pose (found once by locating the single strongest diff cluster
    # in the full frame, then kept fixed: the pose is deterministic for the same x, z, zoom, yaw, pitch).
    w, h, noise_px = blob_width_px(before, after, floor=100.0, search=(480, 550, 190, 280))
    px = max(w, h)
    out = {"landing_dust_px_z240": px, "blob_w": w, "blob_h": h, "noise_px": noise_px}
    out_path = os.path.join(HERE, "probe.json")
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
    print(json.dumps(out, indent=2))
    ok = px >= 12 and noise_px < px / 3
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
