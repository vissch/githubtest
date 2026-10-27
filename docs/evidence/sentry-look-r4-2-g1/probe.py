#!/usr/bin/env python
"""Measure the stills shoot.py shot, in pixels, for three seeds (sentry-look-r4-2-g1).

The owner on r4: "the unmasked night still still shows a gunner at the gun. The man who vanished was kneeling off
to the side." r4 stood the Sentry 2.5 m behind his plate, so the figure that disappeared was not the gunner at the
gun. Here he stands 0.6 m behind it - at the gun - and three independent battles (seeds 1, 2, 3: 6, 8 and 10
enemy riflemen, so a different traverse, place and tick each time) are each shot unmasked before and after his
death, with three identical unmasked captures before it to measure the build's own noise.

Per seed, read only from the committed stills (no Unity, no sim):

    px_per_m    the median per-column run of cyan pixels in plate_t.png, over 0.50 m. CombatFx.PlateBar draws a
                cube scaled (1.40, 0.50, 0.02): at a yaw that is not side-on, the 1.40 m width's far end lands
                higher on screen than its near end, so the cyan bbox's height carries that skew on top of the
                0.50 m. The per-column run is free of it: it measures the bar's own vertical extent column by
                column, so pixels become metres in that very still, under that very camera, at any yaw.
    gun box     the magenta bbox of the plate and its post in plate_t.png, grown 1.2 m on all four sides and
                clipped to the frame. Measured off the same frozen instant through the same camera, so no pose has
                to be trusted either. Everything below is counted inside this box only, which is what makes the
                change "at the gun" and not somewhere else in a 1920x1080 night.
    changed(a,b) pixels inside the gun box whose largest per-channel difference is over 12 of 255.
    noise_max   the biggest of the three pairs among night_t, night_t2, night_t3 - three captures with nothing
                whatever changed between them (the clock is stopped). That is this build's spread.
    change      changed(night_t, night_dead): what his death did to the frame, with its bbox and centroid.
    bar         max(3 * noise_max, floor), floor = 0.25 * (1.7 * px_per_m) * (0.6 * px_per_m) - a quarter of a
                man's silhouette (1.7 m by 0.6 m), turned into pixels by the cyan bar in that same still. The 3x
                keeps the bar outside a spread read from only three captures; the floor stops a noiseless pair
                letting a single pixel pass.

A seed passes when all five hold:
    fallen_before == 0        nobody was lying before the kill (VATRenderer.ClearFallen; r4's diff had a body down
                              already, so the change in frame did not have to be his)
    fallen_after >= 1         the sim's own Death really reached VATRenderer.AddFallen
    change > bar              he visibly fell, by more than noise and more than a quarter of a man
    centroid within 2.0 m     of the plate's bbox centre: he falls AT THE GUN
    bbox 8 px clear of every frame edge: the body STAYS IN THE FRAME, not half out of it

pass when all three seeds pass. Exit 0 / 1. Writes probe.json beside the stills.

Run from the repo root:  python docs/evidence/sentry-look-r4-2-g1/probe.py
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
SEEDS = (1, 2, 3)
BAR_M = 0.50        # the calibration bar's height in metres (CombatFx.PlateBar)
TOL = 40            # per-channel tolerance out of 255 for the flat, unlit mask colours
MAGENTA = (255, 0, 255)
CYAN = (0, 255, 255)

GROW_M = 1.2        # the gun box is the plate's bbox grown this far on all four sides
DIFF = 12           # a pixel counts as changed when its largest channel difference is over this, of 255
NOISE_K = 3.0       # the bar sits this far outside a spread read from only three captures
MAN_H_M, MAN_W_M = 1.7, 0.6
FLOOR_FRAC = 0.25   # a quarter of a man's silhouette
AT_GUN_M = 2.0      # the change's centroid must be this near the plate's centre
EDGE_PX = 8         # and its bbox this far clear of every frame edge


def rgb_of(seed, stem):
    path = os.path.join(HERE, "s%d" % seed, stem + ".png")
    if not os.path.exists(path):
        raise SystemExit("missing still: " + path)
    return np.asarray(Image.open(path).convert("RGB"))


def mask(rgb, want):
    d = np.abs(rgb.astype(np.int16) - np.array(want, dtype=np.int16))
    return (d <= TOL).all(axis=2)


def bbox(m):
    rows, cols = np.nonzero(m)
    if rows.size == 0:
        return None
    return [int(rows.min()), int(rows.max()), int(cols.min()), int(cols.max())]


def changed(a, b, box):
    """Pixels inside the gun box whose largest channel difference is over DIFF."""
    t, bm, l, r = box
    d = np.abs(a[t:bm + 1, l:r + 1].astype(np.int16) - b[t:bm + 1, l:r + 1].astype(np.int16)).max(axis=2)
    return d > DIFF


def one_seed(seed):
    plate = rgb_of(seed, "plate_t")
    h, w = plate.shape[0], plate.shape[1]

    cy = mask(plate, CYAN)
    bar = bbox(cy)
    if bar is None or bar[1] == bar[0]:
        raise SystemExit("seed %d: no calibration bar in plate_t: is CombatFx.PlateMask on?" % seed)
    cols = cy.sum(axis=0)
    runs = cols[cols > 0]
    if runs.size == 0:
        raise SystemExit("seed %d: no calibration bar in plate_t: is CombatFx.PlateMask on?" % seed)
    bar_bbox_h = bar[1] - bar[0]
    px_per_m = float(np.median(runs)) / BAR_M
    bar_skew_px = int(bar_bbox_h - np.median(runs))

    pl = bbox(mask(plate, MAGENTA))
    if pl is None:
        raise SystemExit("seed %d: no plate in plate_t: nothing to measure" % seed)
    grow = int(round(GROW_M * px_per_m))
    box = [max(0, pl[0] - grow), min(h - 1, pl[1] + grow), max(0, pl[2] - grow), min(w - 1, pl[3] + grow)]
    plate_centre = ((pl[0] + pl[1]) / 2.0, (pl[2] + pl[3]) / 2.0)

    nights = {s: rgb_of(seed, s) for s in ("night_t", "night_t2", "night_t3", "night_dead")}
    pairs = (("night_t", "night_t2"), ("night_t", "night_t3"), ("night_t2", "night_t3"))
    noise = {"%s-%s" % p: int(changed(nights[p[0]], nights[p[1]], box).sum()) for p in pairs}
    noise_max = max(noise.values())

    ch = changed(nights["night_t"], nights["night_dead"], box)
    change = int(ch.sum())
    cb, centroid, centroid_m, edge_clear = None, None, None, None
    if change:
        rows, cols = np.nonzero(ch)
        rows, cols = rows + box[0], cols + box[2]
        cb = [int(rows.min()), int(rows.max()), int(cols.min()), int(cols.max())]
        centroid = (float(rows.mean()), float(cols.mean()))
        centroid_m = float(np.hypot(centroid[0] - plate_centre[0], centroid[1] - plate_centre[1]) / px_per_m)
        edge_clear = int(min(cb[0], cb[2], (h - 1) - cb[1], (w - 1) - cb[3]))

    floor = FLOOR_FRAC * (MAN_H_M * px_per_m) * (MAN_W_M * px_per_m)
    threshold = max(NOISE_K * noise_max, floor)

    meta_path = os.path.join(HERE, "s%d" % seed, "meta.json")
    meta = json.load(open(meta_path, encoding="utf-8")) if os.path.exists(meta_path) else {}
    fallen_before, fallen_after = meta.get("fallen_before"), meta.get("fallen_after")

    checks = {
        "fallen_before_is_0": fallen_before == 0,
        "fallen_after_at_least_1": (fallen_after or 0) >= 1,
        "change_over_bar": change > threshold,
        "at_the_gun": centroid_m is not None and centroid_m <= AT_GUN_M,
        "stays_in_frame": edge_clear is not None and edge_clear >= EDGE_PX,
    }
    return {
        "seed": seed, "foes": meta.get("foes"), "tick": meta.get("tick"), "yaw": meta.get("yaw"),
        "kill_path": meta.get("kill_path"), "stand_back_m": meta.get("stand_back_m"),
        "px_per_m": round(px_per_m, 2), "bar_bbox_h": bar_bbox_h, "bar_skew_px": bar_skew_px,
        "plate_bbox": pl, "gun_box": box, "grow_px": grow,
        "noise": noise, "noise_max": noise_max,
        "change": change, "change_bbox": cb,
        "centroid_m_from_plate": None if centroid_m is None else round(centroid_m, 3),
        "edge_clear_px": edge_clear,
        "floor_px": int(round(floor)), "bar": int(round(threshold)),
        "fallen_before": fallen_before, "fallen_after": fallen_after,
        "checks": checks, "pass": all(checks.values()),
    }


def main():
    out = {"diff_threshold_of_255": DIFF, "noise_multiplier": NOISE_K, "grow_m": GROW_M,
           "at_gun_m": AT_GUN_M, "edge_px": EDGE_PX, "seeds": []}
    for seed in SEEDS:
        out["seeds"].append(one_seed(seed))
    ppm = [s["px_per_m"] for s in out["seeds"]]
    px_per_m_even = (max(ppm) / min(ppm)) <= 1.05
    out["px_per_m_even"] = px_per_m_even
    out["pass"] = all(s["pass"] for s in out["seeds"]) and px_per_m_even

    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")

    print("seed  noise_max      bar     change  centroid_m  edge_px  fallen  verdict")
    for s in out["seeds"]:
        print("  s%-2d %9d %8d %10d %11s %8s %3s->%-3s %s"
              % (s["seed"], s["noise_max"], s["bar"], s["change"],
                 s["centroid_m_from_plate"], s["edge_clear_px"], s["fallen_before"], s["fallen_after"],
                 "pass" if s["pass"] else "FAIL " + ",".join(k for k, v in s["checks"].items() if not v)))
    print("px_per_m_even %s (%s)" % (px_per_m_even, ppm))
    print("pass" if out["pass"] else "FAIL")
    return 0 if out["pass"] else 1


if __name__ == "__main__":
    sys.exit(main())
