#!/usr/bin/env python3
"""Does the effect actually show up in the frame?

Harsh critic r1 fix 3 (the owner, 2026-10-19): "the probe never finds the effect in the frame". So this probe does
not ask the code anything. It reads the pixels of the stills AbilityFxStills wrote and the pixels of the no-effect
plates taken at the same pose and the same sim tick, and reports, per still, the share of pixels whose Rec.709 luma
moved by more than 0.08 - the effect's area in the frame.

    python docs/evidence/ability-fx-captures-r3/probe.py --dir docs/evidence/ability-fx-captures-r3

Pass (the owner's thresholds, from this unit's goal): 8 rows, every T1 >= 0.10, every O240 >= 0.03, every
pose_error_m < 0.5. Exit 1 when one of them fails, and say which.
"""
import argparse
import json
import os
import sys

from PIL import Image, ImageChops

SUBJECTS = ["smoke", "barrage", "splash", "drop"]
BANDS = ["T1", "O240"]
DIFF = 0.08          # how far a pixel's luma must move to count as the effect
MIN_T1 = 0.10        # the owner's thresholds
MIN_O240 = 0.03
MAX_POSE = 0.5


def luma(path):
    """Rec.709 luma as an 8-bit image, and its size. The matrix is the one CaptureRig uses, not PIL's default."""
    im = Image.open(path).convert("RGB").convert("L", (0.2126, 0.7152, 0.0722, 0.0))
    return im, im.size[0], im.size[1]


def moved_fraction(still, plate):
    """The share of pixels whose luma moved by more than DIFF between the plate and the still."""
    diff = ImageChops.difference(still, plate)
    cut = int(round(DIFF * 255.0))
    hist = diff.histogram()
    return sum(hist[cut + 1:]) / float(sum(hist))


def sidecar(path):
    side = os.path.splitext(path)[0] + ".json"
    if not os.path.exists(side):
        return {}
    with open(side, encoding="utf-8") as fh:
        return json.load(fh)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", required=True, help="the folder with the stills, the plates and their sidecars")
    ap.add_argument("--tick-tol", type=int, default=2,
                    help="how many sim ticks a still and its plate may be apart (0 = exactly the same tick)")
    ap.add_argument("--out", default=None, help="where probe.json goes (default <dir>/probe.json)")
    a = ap.parse_args()
    out = a.out or os.path.join(a.dir, "probe.json")

    rows, fails = [], []
    for subject in SUBJECTS:
        for band in BANDS:
            still = os.path.join(a.dir, "%s_%s.png" % (subject, band))
            plate = os.path.join(a.dir, "%s_%s_plate.png" % (subject, band))
            if not os.path.exists(still) or not os.path.exists(plate):
                fails.append("%s %s: missing %s" % (subject, band, "still" if not os.path.exists(still) else "plate"))
                continue
            sv, sw, sh = luma(still)
            pv, pw, ph = luma(plate)
            if (sw, sh) != (pw, ph):
                fails.append("%s %s: still is %dx%d, plate is %dx%d" % (subject, band, sw, sh, pw, ph))
                continue
            frac = moved_fraction(sv, pv)

            ss, sp = sidecar(still), sidecar(plate)
            tick, ptick = int(ss.get("sim_tick", -1)), int(sp.get("sim_tick", -1))
            delta = abs(tick - ptick) if tick >= 0 and ptick >= 0 else -1
            pose = float(ss.get("pose_error_m", 9.0))
            ppose = float(sp.get("pose_error_m", 9.0))

            rows.append({"subject": subject, "band": band, "effect_area_frac": round(frac, 5),
                         "pose_error_m": round(max(pose, ppose), 4), "sim_tick": tick, "plate_sim_tick": ptick,
                         "tick_delta": delta})

            floor = MIN_T1 if band == "T1" else MIN_O240
            if frac < floor:
                fails.append("%s %s: effect_area_frac %.4f below %.2f (the effect does not read)" % (subject, band, frac, floor))
            if max(pose, ppose) >= MAX_POSE:
                fails.append("%s %s: pose_error_m %.3f is %.1f or more (the camera was not where it was asked to be)"
                             % (subject, band, max(pose, ppose), MAX_POSE))
            if delta < 0:
                fails.append("%s %s: no sim_tick in a sidecar, so the pair cannot be shown to be the same moment" % (subject, band))
            elif delta > a.tick_tol:
                fails.append("%s %s: still is on tick %d, plate on %d (%d apart, tolerance %d)"
                             % (subject, band, tick, ptick, delta, a.tick_tol))

    if len(rows) < 8:
        fails.append("only %d rows: 8 were asked for" % len(rows))

    t1 = [r["effect_area_frac"] for r in rows if r["band"] == "T1"]
    o240 = [r["effect_area_frac"] for r in rows if r["band"] == "O240"]
    doc = {"rows": rows,
           "min_t1": round(min(t1), 5) if t1 else 0.0,
           "min_o240": round(min(o240), 5) if o240 else 0.0,
           "diff_threshold": DIFF, "tick_tolerance": a.tick_tol, "failures": fails}
    with open(out, "w", encoding="utf-8") as fh:
        json.dump(doc, fh, indent=2)
        fh.write("\n")

    for r in rows:
        print("%-8s %-5s effect_area_frac %.4f  pose_error_m %.3f  ticks %d/%d"
              % (r["subject"], r["band"], r["effect_area_frac"], r["pose_error_m"], r["sim_tick"], r["plate_sim_tick"]))
    print("rows %d  min_t1 %.4f (>= %.2f)  min_o240 %.4f (>= %.2f)" % (len(rows), doc["min_t1"], MIN_T1, doc["min_o240"], MIN_O240))
    for f in fails:
        print("FAIL " + f)
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
