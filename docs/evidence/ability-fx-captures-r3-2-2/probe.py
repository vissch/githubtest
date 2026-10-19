#!/usr/bin/env python3
"""Does the effect actually show up in the frame, and is it big enough on the ground to be an effect?

Harsh critic r1 fix 3 (the owner, 2026-10-19): "the probe never finds the effect in the frame". So this probe asks
the code nothing. It reads the pixels of the stills AbilityFxStills wrote and the pixels of the no-effect plates
taken at the same pose and the same sim tick, and reports, per still, the share of pixels whose Rec.709 luma moved
by more than 0.08 - the effect's area in the frame - and how many pixels that is.

    python docs/evidence/ability-fx-captures-r3-2-2/probe.py --dir docs/evidence/ability-fx-captures-r3-2-2

The floor (R0 C1, a measuring choice taken by the coordinator 2026-10-19, not the owner; the row in
docs/reference/decisions.md): a fixed share of the frame is not a fair test of a point effect, because at the
overview the same share means a feature the size of a village. So the floor is a GROUND FOOTPRINT, capped at the
old share of the frame:

    effect_area_frac >= min(0.10, 400 / frame_area_m2)   and   effect_px >= 150

400 m^2 is about 20 x 20 m: a shell's crown of water and foam, honestly. The cap keeps the close bands as strict as
they were. The pixel count stops a tiny frame passing on a share alone.

HOW frame_area_m2 IS MEASURED, from the still's own sidecar (fov, aspect, cam_height_m, cam_pitch), no code read:

    vh = fov / 2                          the vertical half angle
    hh = atan(aspect * tan(vh))           the horizontal half angle
    d  = cam_height_m / sin(pitch)        the slant range to the aim point at the frame's centre
    frame_area_m2 = 4 * d^2 * tan(hh) * tan(vh) / sin(pitch)

That is the ground the frame covers AT THE SCALE OF ITS CENTRE: the width and height the frame subtends at range d,
with the height divided once more by sin(pitch) because the ground is seen at a slant.

Why not the true flat trapezoid the four corner rays cut out of the ground? At O240 the frame's top edge lands about
1.2 km away, far off a 90 x 276 m map. A corner-ray quad would credit that empty distance as framed ground, blow
frame_area_m2 up by an order of magnitude, drive the floor to nothing and pass the short water splash untouched. The
centre-scale area is the ground a viewer is actually looking at, and it is the honest denominator.

Exit 1 when a row fails, and say which.
"""
import argparse
import json
import math
import os
import sys

from PIL import Image, ImageChops

SUBJECTS = ["smoke", "barrage", "splash", "drop"]
BANDS = ["T1", "O240"]
DIFF = 0.08             # how far a pixel's luma must move to count as the effect
MIN_GROUND_M2 = 400.0   # the ground an effect must cover: about 20 x 20 m
FRAC_CAP = 0.10         # never stricter than the old share-of-frame floor
MIN_PX = 150            # and never fewer changed pixels than this
MAX_POSE = 0.5


def floor_for(area):
    """The share of the frame the effect must cover at this band. See the module docstring."""
    return min(FRAC_CAP, MIN_GROUND_M2 / area)


def frame_area_m2(side):
    """The ground area the frame covers at the scale of its centre, from the sidecar's optics. 0 when it cannot be told."""
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


def luma(path):
    """Rec.709 luma as an 8-bit image, and its size. The matrix is the one CaptureRig uses, not PIL's default."""
    im = Image.open(path).convert("RGB").convert("L", (0.2126, 0.7152, 0.0722, 0.0))
    return im, im.size[0], im.size[1]


def moved(still, plate):
    """How many pixels moved by more than DIFF between the plate and the still, and the share of the frame they are."""
    diff = ImageChops.difference(still, plate)
    cut = int(round(DIFF * 255.0))
    hist = diff.histogram()
    px = sum(hist[cut + 1:])
    return px, px / float(sum(hist))


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
            px, frac = moved(sv, pv)

            ss, sp = sidecar(still), sidecar(plate)
            tick, ptick = int(ss.get("sim_tick", -1)), int(sp.get("sim_tick", -1))
            delta = abs(tick - ptick) if tick >= 0 and ptick >= 0 else -1
            pose = float(ss.get("pose_error_m", 9.0))
            ppose = float(sp.get("pose_error_m", 9.0))
            area = frame_area_m2(ss)

            rows.append({"subject": subject, "band": band, "effect_area_frac": round(frac, 5),
                         "frame_area_m2": round(area, 1), "effect_px": px,
                         "effect_ground_m2": round(frac * area, 1),
                         "floor_frac": round(floor_for(area), 5) if area > 0 else None,
                         "pose_error_m": round(max(pose, ppose), 4), "sim_tick": tick, "plate_sim_tick": ptick,
                         "tick_delta": delta})

            if area <= 0.0:
                fails.append("%s %s: the sidecar has no fov/aspect/cam_height_m, so the framed ground cannot be measured"
                             % (subject, band))
            else:
                floor = floor_for(area)
                if frac < floor:
                    fails.append("%s %s: effect_area_frac %.4f below %.4f (%.0f m^2 of ground framed; the effect covers "
                                 "%.0f m^2, under the %.0f m^2 asked for)"
                                 % (subject, band, frac, floor, area, frac * area, MIN_GROUND_M2))
            if px < MIN_PX:
                fails.append("%s %s: only %d changed pixels, fewer than %d (nothing to see)" % (subject, band, px, MIN_PX))
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

    doc = {"rows": rows, "diff_threshold": DIFF, "min_ground_m2": MIN_GROUND_M2, "frac_cap": FRAC_CAP,
           "min_effect_px": MIN_PX, "tick_tolerance": a.tick_tol, "failures": fails}
    with open(out, "w", encoding="utf-8") as fh:
        json.dump(doc, fh, indent=2)
        fh.write("\n")

    for r in rows:
        print("%-8s %-5s frac %.4f (floor %s)  px %-7d  framed %9.0f m^2  effect %7.0f m^2  pose %.3f  ticks %d/%d"
              % (r["subject"], r["band"], r["effect_area_frac"],
                 "%.4f" % r["floor_frac"] if r["floor_frac"] is not None else "   ?  ",
                 r["effect_px"], r["frame_area_m2"], r["effect_ground_m2"], r["pose_error_m"],
                 r["sim_tick"], r["plate_sim_tick"]))
    print("rows %d  floor = min(%.2f, %.0f / frame_area_m2), effect_px >= %d" % (len(rows), FRAC_CAP, MIN_GROUND_M2, MIN_PX))
    for f in fails:
        print("FAIL " + f)
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
