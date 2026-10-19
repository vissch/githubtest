"""Measures the blast knockback throw from the captured stills, not typed.

before.png/before.json: a row of men standing, shot top-down (CaptureRig, pitch ~75, yaw 90) so world x and z map
almost linearly onto screen pixels and a thrown body's displacement is not badly foreshortened.
after.png/after.json: the same pose, moments after a shell killed them (DeathLab.Shell): some are thrown outward.
calib.png/calib.json: the same kind of shot, at a different zoom, with the row's men standing clearly apart, used
to read pixels-per-metre from their known 3 m spacing (DeathLab.Row spacing) and scale it to before/after's zoom
by the camera height the sidecars carry (top-down: the view's scale is inverse to the camera's height).

Method: threshold each still against its own background colour to get a foreground mask; the mass present in
`before` but gone in `after` (bodies that left their spot) and the mass present in `after` but not in `before`
(where they ended up) each get a centroid; the distance between those two centroids, converted to metres, is the
median throw this scene shows. Writes probe.json next to this script: {"throw_m": x}.
"""
import json
import os

import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))


def foreground_mask(path, roi, thresh=12):
    im = np.array(Image.open(path).convert("RGB")).astype(float)
    x0, y0, x1, y1 = roi
    sub = im[y0:y1, x0:x1]
    bg = np.median(sub.reshape(-1, 3), axis=0)
    dist = np.sqrt(((sub - bg) ** 2).sum(axis=2))
    return dist > thresh


def row_spacing_px(calib_path, roi, thresh=10, min_size=50):
    """Pixels between adjacent standing men in a known-3m-spaced row (DeathLab.Row spacing=3)."""
    im = np.array(Image.open(calib_path).convert("RGB")).astype(float)
    x0, y0, x1, y1 = roi
    sub = im[y0:y1, x0:x1]
    bg = np.median(sub.reshape(-1, 3), axis=0)
    dist = np.sqrt(((sub - bg) ** 2).sum(axis=2))
    mask = dist > thresh
    lbl, n = ndimage.label(mask)
    sizes = ndimage.sum(mask, lbl, range(1, n + 1))
    coms = ndimage.center_of_mass(mask, lbl, range(1, n + 1))
    xs = sorted(c[1] + x0 for s, c in zip(sizes, coms) if s > min_size)
    if len(xs) < 2:
        raise RuntimeError("could not find separated men in the calibration still")
    span = xs[-1] - xs[0]
    gaps = len(xs) - 1
    return span / gaps


def main():
    before_json = json.load(open(os.path.join(HERE, "before.json")))
    after_json = json.load(open(os.path.join(HERE, "after.json")))
    calib_json = json.load(open(os.path.join(HERE, "calib.json")))

    # the calibration still: 6 men in a row, 3 m apart, well separated by its zoom
    calib_roi = (400, 380, 1200, 500)
    px_per_m_calib = row_spacing_px(os.path.join(HERE, "calib.png"), calib_roi) / 3.0

    # top-down: pixel scale is inverse to the camera's height above the ground
    px_per_m = px_per_m_calib * calib_json["cam_y"] / after_json["cam_y"]

    roi = (300, 300, 650, 650)
    before_mask = foreground_mask(os.path.join(HERE, "before.png"), roi)
    after_mask = foreground_mask(os.path.join(HERE, "after.png"), roi)

    removed = before_mask & ~after_mask   # where the standing row was, now empty
    added = after_mask & ~before_mask     # where the thrown bodies (and blast debris) ended up

    ry, rx = np.nonzero(removed)
    ay, ax = np.nonzero(added)
    if len(rx) == 0 or len(ax) == 0:
        raise RuntimeError("no measurable change between before and after")

    removed_centroid = (rx.mean() + roi[0], ry.mean() + roi[1])
    added_centroid = (ax.mean() + roi[0], ay.mean() + roi[1])

    dx = added_centroid[0] - removed_centroid[0]
    dy = added_centroid[1] - removed_centroid[1]
    throw_px = (dx * dx + dy * dy) ** 0.5
    throw_m = throw_px / px_per_m

    out = {"throw_m": round(throw_m, 3)}
    with open(os.path.join(HERE, "probe.json"), "w") as f:
        json.dump(out, f, indent=2)
    print("px_per_m", round(px_per_m, 2), "throw_px", round(throw_px, 1), "throw_m", out["throw_m"])


if __name__ == "__main__":
    main()
