#!/usr/bin/env python3
"""Is the splash water - a crown and foam - or a blue column?

The owner, 2026-10-21: "The splash stills are a blue column, not water... Paint a crown and foam on the water at
night, at zoom 30 and at zoom 240, and reshoot splash three times at each zoom with the barrage off. Bar outside the
spread of three captures of the current build."

So this probe reads pixels, never code, exactly as the r3-2-2 probe does: a still against a no-effect plate of the
same pose and the same sim tick. The frame-area maths and the ground floor below are copied verbatim from
docs/evidence/ability-fx-captures-r3-2-2/probe.py - they are good and already argued there.

    python docs/evidence/ability-fx-captures-r3-2-2-g1/probe.py              # the bars
    python docs/evidence/ability-fx-captures-r3-2-2-g1/probe.py --baseline   # the harness only, and write baseline.json

Six rows: splash x {T1, O240} x {r1, r2, r3}, written by AbilityFxStills with TW_ABILITYFX_SUBJECT=splash and
TW_ABILITYFX_REPS=3 (that is what "with the barrage off" means: no HeBarrage is staged and ShellsPerMinute is zeroed,
and the still's own sidecar now records night and ambient_shells_per_min so neither is assumed).

Two measures per still, pixels only:

  foam_frac     of the changed pixels, the share that are PALE (min(R,G,B) >= 0.55 * max(R,G,B)) and brighter than
                the plate. Foam is white; the translucent blue wisp of splash_T1.png is not.
  crown_spread  the width of the changed region in its BOTTOM QUARTER divided by its full height. A column is tall
                and narrow (small); a crown is wide at the base (large).

--baseline checks only the harness - 6 rows, a plate each, the same sim tick, pose < 0.5, night 1,
ambient_shells_per_min 0 - and writes every row's measures to baseline.json. The bars below are then set at 1.5x the
LARGEST of that band's three baseline rows, and the probe REFUSES a bar that is not above every baseline row at its
band: "outside the spread" is checked here, not asserted in prose.

The r3-2-2 400 m^2 ground floor is kept as a third check, so the fix cannot win by making the splash smaller.

Exit 1 when a row fails, and say which.
"""
import argparse
import json
import math
import os
import sys

import numpy as np
from PIL import Image

BANDS = ["T1", "O240"]
REPS = [1, 2, 3]
DIFF = 0.08             # how far a pixel's luma must move to count as the effect
MIN_GROUND_M2 = 400.0   # the ground an effect must cover: about 20 x 20 m
FRAC_CAP = 0.10         # never stricter than the old share-of-frame floor
MIN_PX = 150            # and never fewer changed pixels than this
MAX_POSE = 0.5
PALE = 0.55             # min(R,G,B) >= PALE * max(R,G,B) is not a blue wisp

# The bars, from a capture of commit 849d908a2b (the current build), never from the code that draws the splash.
# The three baseline rows of a band (baseline.json, captured from 849d908a2b): foam 0.1275/0.0965/0.1074 at T1 and
# 0.0520/0.0477/0.0439 at O240; crown 1.089/0.814/0.975 at T1 and 0.657/0.938/0.987 at O240. The bar is 1.5x the
# largest of the three, so it is outside the spread, not at its edge - and the check below proves that for each row.
FOAM_BAR = {"T1": 0.1913, "O240": 0.0780}
CROWN_BAR = {"T1": 1.633, "O240": 1.480}


def floor_for(area):
    """The share of the frame the effect must cover at this band. r3-2-2's, verbatim."""
    return min(FRAC_CAP, MIN_GROUND_M2 / area)


def frame_area_m2(side):
    """The ground area the frame covers at the scale of its centre, from the sidecar's optics. r3-2-2's, verbatim."""
    fov = float(side.get("fov", 0.0))
    aspect = float(side.get("aspect", 0.0))
    height = float(side.get("cam_height_m", 0.0))
    pitch = float(side.get("cam_pitch", 0.0))
    if fov <= 0.0 or aspect <= 0.0 or height <= 0.0:
        return 0.0
    sp = math.sin(math.radians(pitch))
    if sp <= 0.01:                       # a level or rising camera frames no bounded ground
        return 0.0
    vh = math.radians(fov) / 2.0
    hh = math.atan(aspect * math.tan(vh))
    d = height / sp
    return 4.0 * d * d * math.tan(hh) * math.tan(vh) / sp


def rgb(path):
    """The image as floats 0-1, HxWx3."""
    return np.asarray(Image.open(path).convert("RGB"), dtype=np.float32) / 255.0


def luma(a):
    """Rec.709 luma, the matrix CaptureRig uses."""
    return a[:, :, 0] * 0.2126 + a[:, :, 1] * 0.7152 + a[:, :, 2] * 0.0722


def measure(still, plate):
    """effect_px, effect_area_frac, foam_frac, crown_spread for one pair."""
    sl, pl = luma(still), luma(plate)
    moved = np.abs(sl - pl) > DIFF
    px = int(moved.sum())
    frac = px / float(moved.size)
    if px == 0:
        return px, frac, 0.0, 0.0

    lo = still.min(axis=2)
    hi = np.maximum(still.max(axis=2), 1e-6)
    pale = (lo >= PALE * hi) & (sl > pl)          # white, and brighter than the plate: foam, not a blue wisp
    foam = float((moved & pale).sum()) / float(px)

    # the changed region's shape: full height, and how wide it is across its bottom quarter
    rows = np.flatnonzero(moved.any(axis=1))
    top, bot = int(rows[0]), int(rows[-1])
    height = bot - top + 1
    band = moved[bot - max(0, height // 4 - 1):bot + 1]    # the bottom quarter of it, at least one row
    cols = np.flatnonzero(band.any(axis=0))
    width = int(cols[-1] - cols[0] + 1) if cols.size else 0
    return px, frac, foam, width / float(height)


def sidecar(path):
    side = os.path.splitext(path)[0] + ".json"
    if not os.path.exists(side):
        return {}
    with open(side, encoding="utf-8") as fh:
        return json.load(fh)


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", default=here, help="the folder with the stills, the plates and their sidecars")
    ap.add_argument("--baseline", action="store_true",
                    help="check only the harness and write baseline.json (the capture of the unfixed build)")
    ap.add_argument("--tick-tol", type=int, default=2,
                    help="how many sim ticks a still and its plate may be apart (0 = exactly the same tick)")
    ap.add_argument("--out", default=None, help="where probe.json goes (default <dir>/probe.json)")
    a = ap.parse_args()
    out = a.out or os.path.join(a.dir, "baseline.json" if a.baseline else "probe.json")

    rows, fails = [], []
    for band in BANDS:
        for rep in REPS:
            name = "splash_%s_r%d" % (band, rep)
            still = os.path.join(a.dir, name + ".png")
            plate = os.path.join(a.dir, name + "_plate.png")
            if not os.path.exists(still) or not os.path.exists(plate):
                fails.append("%s: missing %s" % (name, "still" if not os.path.exists(still) else "plate"))
                continue
            sv, pv = rgb(still), rgb(plate)
            if sv.shape != pv.shape:
                fails.append("%s: still is %s, plate is %s" % (name, sv.shape, pv.shape))
                continue
            px, frac, foam, crown = measure(sv, pv)

            ss, sp = sidecar(still), sidecar(plate)
            tick, ptick = int(ss.get("sim_tick", -1)), int(sp.get("sim_tick", -1))
            delta = abs(tick - ptick) if tick >= 0 and ptick >= 0 else -1
            pose = max(float(ss.get("pose_error_m", 9.0)), float(sp.get("pose_error_m", 9.0)))
            night = int(ss.get("night", -1))
            shells = float(ss.get("ambient_shells_per_min", -1.0))
            area = frame_area_m2(ss)

            rows.append({"name": name, "band": band, "rep": rep,
                         "foam_frac": round(foam, 5), "crown_spread": round(crown, 4),
                         "effect_area_frac": round(frac, 5), "effect_px": px,
                         "frame_area_m2": round(area, 1), "effect_ground_m2": round(frac * area, 1),
                         "floor_frac": round(floor_for(area), 5) if area > 0 else None,
                         "pose_error_m": round(pose, 4), "sim_tick": tick, "plate_sim_tick": ptick,
                         "tick_delta": delta, "night": night, "ambient_shells_per_min": shells})

            # the harness: the same moment, the same pose, at night, with the barrage off
            if delta < 0:
                fails.append("%s: no sim_tick in a sidecar, so the pair cannot be shown to be the same moment" % name)
            elif delta > a.tick_tol:
                fails.append("%s: still is on tick %d, plate on %d (%d apart, tolerance %d)"
                             % (name, tick, ptick, delta, a.tick_tol))
            if pose >= MAX_POSE:
                fails.append("%s: pose_error_m %.3f is %.1f or more (the camera was not where it was asked to be)"
                             % (name, pose, MAX_POSE))
            if night != 1:
                fails.append("%s: night is %s, not 1 (the owner asked for the water at night)" % (name, night))
            if shells != 0.0:
                fails.append("%s: ambient_shells_per_min is %s, not 0 (the barrage was not off)" % (name, shells))

            # the r3-2-2 ground floor, so the fix cannot win by shrinking the splash
            if area <= 0.0:
                fails.append("%s: the sidecar has no fov/aspect/cam_height_m, so the framed ground cannot be measured" % name)
            elif frac < floor_for(area):
                fails.append("%s: effect_area_frac %.4f below %.4f (%.0f m^2 of ground framed; the effect covers "
                             "%.0f m^2, under the %.0f m^2 asked for)"
                             % (name, frac, floor_for(area), area, frac * area, MIN_GROUND_M2))
            if px < MIN_PX:
                fails.append("%s: only %d changed pixels, fewer than %d (nothing to see)" % (name, px, MIN_PX))

            # the look itself, only once the bars are set
            if not a.baseline:
                fb, cb = FOAM_BAR.get(band), CROWN_BAR.get(band)
                if fb is None or cb is None:
                    fails.append("%s: no bar is set for %s yet (run --baseline on a capture of the unfixed build first)"
                                 % (name, band))
                else:
                    if foam < fb:
                        fails.append("%s: foam_frac %.4f below the %s bar %.4f (the splash is still a blue wisp, not foam)"
                                     % (name, foam, band, fb))
                    if crown < cb:
                        fails.append("%s: crown_spread %.3f below the %s bar %.3f (tall and narrow: a column, not a crown)"
                                     % (name, crown, band, cb))

    if len(rows) < 6:
        fails.append("only %d rows: 6 were asked for" % len(rows))

    # "a bar outside the spread of three captures of the current build", checked: every bar must clear every
    # baseline row of its band. Without baseline.json there is nothing to check it against, and that is a failure too.
    base_path = os.path.join(a.dir, "baseline.json")
    if not a.baseline:
        if not os.path.exists(base_path):
            fails.append("no baseline.json beside the probe: the bars cannot be shown to be outside the spread")
        else:
            with open(base_path, encoding="utf-8") as fh:
                base = json.load(fh)
            for b in base.get("rows", []):
                fb, cb = FOAM_BAR.get(b["band"]), CROWN_BAR.get(b["band"])
                if fb is not None and fb <= b["foam_frac"]:
                    fails.append("FOAM_BAR[%s] %.4f is not above baseline %s (%.4f): the bar is inside the spread"
                                 % (b["band"], fb, b["name"], b["foam_frac"]))
                if cb is not None and cb <= b["crown_spread"]:
                    fails.append("CROWN_BAR[%s] %.3f is not above baseline %s (%.3f): the bar is inside the spread"
                                 % (b["band"], cb, b["name"], b["crown_spread"]))

    doc = {"rows": rows, "diff_threshold": DIFF, "pale_ratio": PALE, "min_ground_m2": MIN_GROUND_M2,
           "frac_cap": FRAC_CAP, "min_effect_px": MIN_PX, "tick_tolerance": a.tick_tol,
           "foam_bar": FOAM_BAR, "crown_bar": CROWN_BAR, "baseline": a.baseline, "failures": fails}
    with open(out, "w", encoding="utf-8") as fh:
        json.dump(doc, fh, indent=2)
        fh.write("\n")

    for r in rows:
        print("%-14s foam %.4f (bar %s)  crown %.3f (bar %s)  frac %.4f (floor %s)  px %-7d  "
              "effect %6.0f m^2  pose %.3f  ticks %d/%d  night %d  shells %g"
              % (r["name"], r["foam_frac"],
                 "%.4f" % FOAM_BAR[r["band"]] if FOAM_BAR[r["band"]] is not None else "  -   ",
                 r["crown_spread"],
                 "%.3f" % CROWN_BAR[r["band"]] if CROWN_BAR[r["band"]] is not None else " -   ",
                 r["effect_area_frac"],
                 "%.4f" % r["floor_frac"] if r["floor_frac"] is not None else "   ?  ",
                 r["effect_px"], r["effect_ground_m2"], r["pose_error_m"],
                 r["sim_tick"], r["plate_sim_tick"], r["night"], r["ambient_shells_per_min"]))
    print("rows %d  %s  wrote %s" % (len(rows), "baseline (harness only)" if a.baseline else "bars checked", out))
    for f in fails:
        print("FAIL " + f)
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
