#!/usr/bin/env python
"""Measure the Sentry-plate stills shoot.py shot, in pixels (sentry-look-r4, fix 2).

The harsh critic's r1 finding fix 2 on sentry-look-r2: its plate_dead.json read drawn_infantry 1 and fallen 0 after
the Despawn, so the death never played and "the plate outlives him" was never actually shown. These stills were shot
with the clock running through the death (shoot.py collects the Death off SimWorld's one-tick event ring itself, so
CombatFx.OnDeath hears it), and the numbers below come off their pixels and their CaptureRig sidecars - not off
arithmetic.

CombatFx.PlateMask draws the plate and its post in flat magenta and, standing on the same ground as the post, a
calibration bar exactly 0.50 m tall in flat cyan.

    plate_px_t                 = magenta pixels in plate_t.png (he is alive, his plate is emplaced)
    plate_px_after_death       = magenta pixels in plate_dead.png (same camera pose, after his death played)
    fallen_after_death         = plate_dead.json "fallen"          (VATRenderer.FallenCount: a body is down)
    drawn_infantry_after_death = plate_dead.json "drawn_infantry"  (VATRenderer.DrawnInfantry: no living man drawn)

Passes when the plate is still there, to within 2% of the pixels it had while he lived:

    fallen_after_death         >= 1
    drawn_infantry_after_death == 0
    |plate_px_after_death - plate_px_t| <= 2% of plate_px_t
    every sidecar's pose_error_m < 0.5        (the camera stood where it was asked to)

plate_look.png is the same held frame with the mask off - the unmasked night look in GreyboxCorridor. It is not
measured; it is there to be looked at. Because CaptureRig cannot be zoomed past TacticalCamera.NearZoom (derived,
14 m up on this branch), the plate is small in it, so this also writes plate_look_crop.png: the same pixels, cut out
around the magenta bounding box plate_dead.png gives for the plate (same pose, so the same place in the frame).

Run from the repo root:  python docs/evidence/sentry-look-r4/probe.py
Writes probe.json beside the stills. Exit 0 when every threshold passes, 1 when any fails; it raises when a still or
a sidecar is missing.
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
BAR_M = 0.50          # the calibration bar's height in metres (CombatFx.PlateBar)
TOL = 40              # per-channel tolerance out of 255: the mask is unlit and flat, so it needs no more
MAGENTA = (255, 0, 255)
CYAN = (0, 255, 255)

BAND = 0.02           # the plate may differ by this fraction of its pixels between the two stills
POSE_MAX = 0.5        # m: the sidecar's pose_error_m
MASKED = ("plate_t", "plate_dead")
STILLS = MASKED + ("plate_look",)


def mask(rgb, want):
    """Where the image is within TOL of `want` on every channel."""
    d = np.abs(rgb.astype(np.int16) - np.array(want, dtype=np.int16))
    return (d <= TOL).all(axis=2)


def extent(m):
    """count, top row, bottom row, and the bounding-box centre (row, col); rows count down from the top."""
    rows, cols = np.nonzero(m)
    if rows.size == 0:
        return dict(count=0, top=None, bottom=None, centre=None)
    return dict(count=int(rows.size), top=int(rows.min()), bottom=int(rows.max()),
                centre=[(float(rows.min()) + float(rows.max())) / 2.0,
                        (float(cols.min()) + float(cols.max())) / 2.0])


def sidecar(stem):
    path = os.path.join(HERE, stem + ".json")
    if not os.path.exists(path):
        raise SystemExit("missing sidecar: " + path)
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def read(stem, measure=True):
    path = os.path.join(HERE, stem + ".png")
    if not os.path.exists(path):
        raise SystemExit("missing still: " + path)
    side = sidecar(stem)
    out = dict(file=stem + ".png", pose_error_m=side.get("pose_error_m"),
               fallen=side.get("fallen"), drawn_infantry=side.get("drawn_infantry"),
               men_in_frame=side.get("men_in_frame"), luma_mean=side.get("luma_mean"))
    if measure:
        rgb = np.asarray(Image.open(path).convert("RGB"))
        out["size"] = [int(rgb.shape[1]), int(rgb.shape[0])]
        out["magenta"] = extent(mask(rgb, MAGENTA))
        out["cyan"] = extent(mask(rgb, CYAN))
    return out


def main():
    shots = {s: read(s, measure=s in MASKED) for s in STILLS}
    t, dead = shots["plate_t"], shots["plate_dead"]

    plate_px_t = t["magenta"]["count"]
    if not plate_px_t:
        raise SystemExit("no plate in plate_t: is CombatFx.PlateMask on, and the plate in frame?")
    plate_px_after_death = dead["magenta"]["count"]

    # the scale, from the cyan bar in plate_t: the same still, so nothing about the camera has to be assumed
    bar = t["cyan"]
    px_per_m = (bar["bottom"] - bar["top"]) / BAR_M if bar["count"] and bar["bottom"] != bar["top"] else None

    fallen_after_death = dead["fallen"]
    drawn_infantry_after_death = dead["drawn_infantry"]
    if fallen_after_death is None or drawn_infantry_after_death is None:
        raise SystemExit("plate_dead.json carries no fallen/drawn_infantry: CaptureRig found no VATRenderer")

    drift = abs(plate_px_after_death - plate_px_t)
    allowed = BAND * plate_px_t
    poses = {s: shots[s]["pose_error_m"] for s in STILLS}
    worst_pose = max(float(v if v is not None else 9.9) for v in poses.values())

    checks = {
        "fallen_after_death >= 1": fallen_after_death >= 1,
        "drawn_infantry_after_death == 0": drawn_infantry_after_death == 0,
        "plate within 2%% of plate_px_t": drift <= allowed,
        "every pose_error_m < 0.5": worst_pose < POSE_MAX,
    }

    out = {
        "fallen_after_death": fallen_after_death,
        "drawn_infantry_after_death": drawn_infantry_after_death,
        "plate_px_t": plate_px_t,
        "plate_px_after_death": plate_px_after_death,
        "plate_px_drift": int(drift),
        "plate_px_allowed": round(allowed, 1),
        "px_per_m": round(px_per_m, 2) if px_per_m else None,
        "bar_m": BAR_M,
        "tolerance_per_channel": TOL,
        "pose_error_m": poses,
        "thresholds": {"fallen_after_death": ">= 1", "drawn_infantry_after_death": "== 0",
                       "plate_px_band": BAND, "pose_error_m": POSE_MAX},
        "shots": shots,
    }
    # the unmasked still, cut out around where the mask says the plate is: the same pose, so the same pixels
    box = dead["magenta"]
    pad = 180
    look = Image.open(os.path.join(HERE, "plate_look.png")).convert("RGB")
    rows, cols = box["top"], box["centre"][1]
    left = int(max(0, cols - pad * 1.6)); right = int(min(look.width, cols + pad * 1.6))
    top = int(max(0, rows - pad * 0.5)); bottom = int(min(look.height, box["bottom"] + pad * 0.8))
    look.crop((left, top, right, bottom)).save(os.path.join(HERE, "plate_look_crop.png"))
    out["look_crop"] = dict(file="plate_look_crop.png", box=[left, top, right, bottom])

    out["pass"] = bool(all(checks.values()))

    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")

    print("plate_px_t                 %8d  (magenta in plate_t.png, px_per_m %s)" % (plate_px_t, out["px_per_m"]))
    print("plate_px_after_death       %8d  (drift %d px, allowed %.1f)" % (plate_px_after_death, drift, allowed))
    print("fallen_after_death         %8s" % fallen_after_death)
    print("drawn_infantry_after_death %8s" % drawn_infantry_after_death)
    for name, ok in checks.items():
        print("  %-34s %s" % (name.replace("%%", "%"), "pass" if ok else "FAIL"))
    for s in STILLS:
        d = shots[s]
        m = d.get("magenta", {}).get("count", "-")
        print("  %-11s magenta %6s px | pose_error_m %s fallen %s drawn %s"
              % (s, m, d["pose_error_m"], d["fallen"], d["drawn_infantry"]))
    return 0 if out["pass"] else 1


if __name__ == "__main__":
    sys.exit(main())
