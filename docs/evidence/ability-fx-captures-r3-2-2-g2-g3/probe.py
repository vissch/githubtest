"""Does the effect actually show up in the frame, and for the drop: are the parachute canopies there?

Harsh critic r1 fix 3 (the owner, 2026-10-19): "the probe never finds the effect in the frame". So this probe asks
the code nothing. It reads the pixels of the stills AbilityFxStills wrote and the pixels of the no-effect plates
taken at the same pose and the same sim tick, and reports, per still, the share of pixels whose Rec.709 luma moved
by more than 0.08 - the effect's area in the frame - and how many pixels that is.

    python docs/evidence/ability-fx-captures-r3-2-2-g2-g3/probe.py
    python docs/evidence/ability-fx-captures-r3-2-2-g2-g3/probe.py --as-nochute   # the negative proof: exit 1

--dir defaults to this script's own folder, so the bare command above works from anywhere.

TWELVE ROWS. Smoke, barrage and splash are one capture a band (copied untouched from g2) and keep the ground
footprint floor:

    effect_area_frac >= min(0.10, 400 / frame_area_m2)   and   effect_px >= 150

400 m^2 is about 20 x 20 m: a shell's crown of water and foam, honestly. The cap keeps the close bands as strict as
they were (R0 C1, a measuring choice taken by the coordinator 2026-10-19; the row in docs/reference/decisions.md).

THE DROP IS SIX ROWS AND IS MEASURED ON ITS CANOPIES (g2-g3, this lane, 2026-10-21; the row in decisions.md). The
owner asked for parachute canopies that read at both zooms at night, and for a probe that "must fail when the
canopies are gone even if the smoke streak remains". So the drop is shot THREE TIMES at ONE paused sim tick a band
(TimeScale 0, so no tick passes between the shots), and then the same frozen frame is shot three times more with
CombatFx.DropCanopies off - drop_<band>_rN_nochute.png. Each drop row therefore reads three images:

    effect_area_frac   still vs _plate     the whole drop: the biplane's wake, the canopies and the landing dust
    canopy_area_frac   still vs _nochute   the canopies ALONE - and the number the bar sits on

Everything but the canopies is in the still and in the nochute alike, so it cancels: when the canopies are gone,
canopy_area_frac is zero however much wake and dust remain. That is the owner's negative proof, and --as-nochute
runs it: it reads each *_nochute.png as the still, and the six drop rows must fail on canopy_area_frac 0.000000
while their effect_area_frac still shows the wake and the dust.

Each band's bars are 0.8 x the smallest of its three captures of this lane's own tree (parent bf295b416d), rounded
down to four decimals:

    canopy  T1    r1 0.175710  r2 0.174276  r3 0.174630   smallest 0.174276  x 0.8 -> 0.1394
    canopy  O240  r1 0.010421  r2 0.010485  r3 0.009856   smallest 0.009856  x 0.8 -> 0.0078
    drop    T1    r1 0.175710  r2 0.174276  r3 0.174630   smallest 0.174276  x 0.8 -> 0.1394
    drop    O240  r1 0.020456  r2 0.020531  r3 0.021010   smallest 0.020456  x 0.8 -> 0.0163

At T1 the canopy mask and the whole-drop mask are the same number to six decimals: the plate and the nochute hold
the same frame there, because at zoom 30 the biplane's wake and the landing dust are off the edge of the shot and
the canopies are the whole of the drop's drawing. At O240 the two differ: the wake is in the frame.

The 0.8 margin, rounded down to four decimals, puts each bar under the whole spread, so a green build never
flickers red; a real loss of the drop's drawing still trips it. A drop row also fails when its three reps, their
plates and their nochutes do not all carry ONE sim_tick: the six shots are of a single frozen moment or they prove
nothing. The 150-pixel minimum (on the canopy mask for the drop), the pose check and the still-vs-plate tick check
apply to every row.

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

FOOTPRINT_SUBJECTS = ["smoke", "barrage", "splash"]
BANDS = ["T1", "O240"]
DROP_REPS = ["r1", "r2", "r3"]
DROP_BAR = {"T1": 0.1394, "O240": 0.0163}     # still vs plate: the whole drop. 0.8 x the smallest of three shots
CANOPY_BAR = {"T1": 0.1394, "O240": 0.0078}   # still vs nochute: the canopies alone, the number the owner asked for
DIFF = 0.08             # how far a pixel's luma must move to count as the effect
MIN_GROUND_M2 = 400.0   # the ground an effect must cover: about 20 x 20 m
FRAC_CAP = 0.10         # never stricter than the old share-of-frame floor
MIN_PX = 150            # and never fewer changed pixels than this
MAX_POSE = 0.5
ROWS_WANTED = 12


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


def wanted():
    """Every row: its name, its band and the stem of its PNG pair. The drop is three reps a band."""
    out = []
    for subject in FOOTPRINT_SUBJECTS:
        for band in BANDS:
            out.append((subject, band, "%s_%s" % (subject, band)))
    for band in BANDS:
        for rep in DROP_REPS:
            out.append(("drop_" + rep, band, "drop_%s_%s" % (band, rep)))
    return out


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", default=here, help="the folder with the stills, the plates and their sidecars")
    ap.add_argument("--tick-tol", type=int, default=2,
                    help="how many sim ticks a still and its plate may be apart (0 = exactly the same tick)")
    ap.add_argument("--out", default=None, help="where probe.json goes (default <dir>/probe.json)")
    ap.add_argument("--as-nochute", action="store_true",
                    help="read each drop's *_nochute.png AS the still: the negative proof, which must exit 1")
    a = ap.parse_args()
    out = a.out or os.path.join(a.dir, "probe.json")

    rows, fails = [], []
    band_ticks = {}            # band -> {tick: [which file]}: the drop's six shots are one frozen moment or nothing
    for subject, band, stem in wanted():
        who = "%s %s" % (subject, band)
        is_drop = subject.startswith("drop")
        nochute = os.path.join(a.dir, stem + "_nochute.png") if is_drop else None
        still = nochute if (is_drop and a.as_nochute) else os.path.join(a.dir, stem + ".png")
        plate = os.path.join(a.dir, stem + "_plate.png")
        if is_drop and not os.path.exists(nochute):
            fails.append("%s: missing the nochute frame %s" % (who, os.path.basename(nochute)))
            continue
        if not os.path.exists(still) or not os.path.exists(plate):
            fails.append("%s: missing %s" % (who, "still" if not os.path.exists(still) else "plate"))
            continue
        sv, sw, sh = luma(still)
        pv, pw, ph = luma(plate)
        if (sw, sh) != (pw, ph):
            fails.append("%s: still is %dx%d, plate is %dx%d" % (who, sw, sh, pw, ph))
            continue
        px, frac = moved(sv, pv)

        canopy_px, canopy_frac = -1, None
        if is_drop:
            nv, nw, nh = luma(nochute)
            if (nw, nh) != (sw, sh):
                fails.append("%s: still is %dx%d, nochute is %dx%d" % (who, sw, sh, nw, nh))
                continue
            canopy_px, canopy_frac = moved(sv, nv)

        ss, sp = sidecar(still), sidecar(plate)
        tick, ptick = int(ss.get("sim_tick", -1)), int(sp.get("sim_tick", -1))
        delta = abs(tick - ptick) if tick >= 0 and ptick >= 0 else -1
        pose = float(ss.get("pose_error_m", 9.0))
        ppose = float(sp.get("pose_error_m", 9.0))
        area = frame_area_m2(ss)
        bar = DROP_BAR[band] if is_drop else (floor_for(area) if area > 0 else None)
        if is_drop:
            for label, side in (("still", ss), ("plate", sp), ("nochute", sidecar(nochute))):
                t = int(side.get("sim_tick", -1))
                band_ticks.setdefault(band, {}).setdefault(t, []).append("%s %s" % (stem, label))

        rows.append({"subject": subject, "band": band, "effect_area_frac": round(frac, 6),
                     "canopy_area_frac": round(canopy_frac, 6) if canopy_frac is not None else None,
                     "canopy_px": canopy_px if is_drop else None,
                     "canopy_bar": CANOPY_BAR[band] if is_drop else None,
                     "frame_area_m2": round(area, 1), "effect_px": px,
                     "effect_ground_m2": round(frac * area, 1),
                     "floor_frac": round(bar, 6) if bar is not None else None,
                     "floor_kind": "fixed_bar" if is_drop else "ground_footprint",
                     "pose_error_m": round(max(pose, ppose), 4), "sim_tick": tick, "plate_sim_tick": ptick,
                     "tick_delta": delta})

        if is_drop:
            if canopy_frac < CANOPY_BAR[band]:
                fails.append("%s: canopy_area_frac %.6f below the canopy bar %.4f (the parachute canopies are gone; "
                             "effect_area_frac %.6f, which the wake and the dust alone can hold up)"
                             % (who, canopy_frac, CANOPY_BAR[band], frac))
            if canopy_px < MIN_PX:
                fails.append("%s: only %d canopy pixels, fewer than %d (no canopy to see)" % (who, canopy_px, MIN_PX))
            if frac < bar:
                fails.append("%s: effect_area_frac %.6f below the fixed bar %.4f (the drop's own drawing shrank)"
                             % (who, frac, bar))
        elif area <= 0.0:
            fails.append("%s: the sidecar has no fov/aspect/cam_height_m, so the framed ground cannot be measured" % who)
        elif frac < bar:
            fails.append("%s: effect_area_frac %.4f below %.4f (%.0f m^2 of ground framed; the effect covers "
                         "%.0f m^2, under the %.0f m^2 asked for)"
                         % (who, frac, bar, area, frac * area, MIN_GROUND_M2))
        if px < MIN_PX:
            fails.append("%s: only %d changed pixels, fewer than %d (nothing to see)" % (who, px, MIN_PX))
        if max(pose, ppose) >= MAX_POSE:
            fails.append("%s: pose_error_m %.3f is %.1f or more (the camera was not where it was asked to be)"
                         % (who, max(pose, ppose), MAX_POSE))
        if delta < 0:
            fails.append("%s: no sim_tick in a sidecar, so the pair cannot be shown to be the same moment" % who)
        elif delta > a.tick_tol:
            fails.append("%s: still is on tick %d, plate on %d (%d apart, tolerance %d)"
                         % (who, tick, ptick, delta, a.tick_tol))

    for band, seen in band_ticks.items():
        if len(seen) > 1:
            fails.append("drop %s: the three reps, their plates and their nochutes are not one sim tick: %s"
                         % (band, "; ".join("tick %d = %s" % (t, ", ".join(v)) for t, v in sorted(seen.items()))))
        elif -1 in seen:
            fails.append("drop %s: a sidecar has no sim_tick, so the six shots cannot be shown to be one moment" % band)

    if len(rows) < ROWS_WANTED:
        fails.append("only %d rows: %d were asked for" % (len(rows), ROWS_WANTED))

    doc = {"rows": rows, "diff_threshold": DIFF, "min_ground_m2": MIN_GROUND_M2, "frac_cap": FRAC_CAP,
           "min_effect_px": MIN_PX, "drop_bar": DROP_BAR, "canopy_bar": CANOPY_BAR,
           "as_nochute": bool(a.as_nochute), "tick_tolerance": a.tick_tol, "failures": fails}
    with open(out, "w", encoding="utf-8") as fh:
        json.dump(doc, fh, indent=2)
        fh.write("\n")

    for r in rows:
        print("%-9s %-5s frac %.6f (bar %s, %-16s)  px %-7d  canopy %-34s  framed %9.0f m^2  pose %.3f  ticks %d/%d"
              % (r["subject"], r["band"], r["effect_area_frac"],
                 "%.6f" % r["floor_frac"] if r["floor_frac"] is not None else "   ?    ", r["floor_kind"],
                 r["effect_px"],
                 "%.6f (bar %.4f, px %d)" % (r["canopy_area_frac"], r["canopy_bar"], r["canopy_px"])
                 if r["canopy_area_frac"] is not None else "-",
                 r["frame_area_m2"], r["pose_error_m"], r["sim_tick"], r["plate_sim_tick"]))
    print("rows %d  footprint bar = min(%.2f, %.0f / frame_area_m2); drop bar T1 %.4f O240 %.4f; canopy bar T1 %.4f "
          "O240 %.4f; px >= %d%s"
          % (len(rows), FRAC_CAP, MIN_GROUND_M2, DROP_BAR["T1"], DROP_BAR["O240"], CANOPY_BAR["T1"],
             CANOPY_BAR["O240"], MIN_PX, "  [--as-nochute: the canopies are off]" if a.as_nochute else ""))
    for f in fails:
        print("FAIL " + f)
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
