#!/usr/bin/env python3
"""Does the effect actually show up in the frame, and is it big enough on the ground to be an effect?

Harsh critic r1 fix 3 (the owner, 2026-10-19): "the probe never finds the effect in the frame". So this probe asks
the code nothing. It reads the pixels of the stills AbilityFxStills wrote and the pixels of the no-effect plates
taken at the same pose and the same sim tick, and reports, per still, the share of pixels whose Rec.709 luma moved
by more than 0.08 - the effect's area in the frame - and how many pixels that is.

    python docs/evidence/ability-fx-captures-r3-2-2-g2-g2-g2/probe.py

--dir defaults to this script's own folder, so the bare command above works from anywhere.

SIXTEEN ROWS. Smoke and barrage are one capture a band, as before, and keep the ground footprint floor; their eight
captures are carried over from ability-fx-captures-r3-2-2-g2-g2 unchanged (the change in this lane is gated on `wet`,
so no dry capture can have moved). The drop is THREE captures a band (drop_T1_r1..r3, drop_O240_r1..r3), carried over
too. THE SPLASH IS NOW THREE CAPTURES A BAND as well (splash_T1_r1..r3, splash_O240_r1..r3), and it is held against
BOTH its footprint floor and a fixed bar: max(footprint, SPLASH_BAR[band]).

The footprint floor (R0 C1, a measuring choice taken by the coordinator 2026-10-19, not the owner; the row in
docs/reference/decisions.md): a fixed share of the frame is not a fair test of a point effect, because at the
overview the same share means a feature the size of a village. So the floor is a GROUND FOOTPRINT, capped at the
old share of the frame:

    effect_area_frac >= min(0.10, 400 / frame_area_m2)   and   effect_px >= 150

400 m^2 is about 20 x 20 m: a shell's crown of water and foam, honestly. The cap keeps the close bands as strict as
they were. The pixel count stops a tiny frame passing on a share alone.

THE DROP IS MEASURED AGAINST A FIXED BAR, NOT THE FOOTPRINT (g2, this lane, 2026-10-21; the row in
docs/reference/decisions.md). The old drop stills measured the squad: drop_T1.png had 19 men and green tracer
beams, its plate 6 men, so the diff counted soldiers, not the drop. The plate now stages the same drop with the
drop's own look off and the beams off, so the same men stand in both frames; what the diff leaves is the biplane's
wake, the canopies and the landing dust.

That drawing is not a patch of churned ground, so the ground footprint is the wrong question to ask of it: it would
pass or fail by accident. The owner asked instead for a bar outside the spread of three captures of the build.

AT O240 THE THREE CAPTURES ARE OF ONE SIM TICK (g2-g2, this lane, 2026-10-21). The earlier three O240 captures were
shot at ticks 199, 695 and 1191 of one match, so their plumes piled up in a frame that holds the whole map and the
reading drifted; the bar was then 0.8 x the smallest, 20% under a cluster that moved 2.5%. The three O240 captures
are now three separate editor runs, each starting the match from scratch, each a still and a plate of the SAME sim
tick 199, beams off, the plate's men held:

    O240  r1 0.020909  r2 0.020909  r3 0.020909   all at sim tick 199, 19270 changed pixels each

Three runs of one tree gave the same number to six decimals, so the spread is 0. The bar is the smallest capture
less twice the spread, never less than 0.0002 under it, rounded down to four decimals:

    bar = floor_4(min - max(2 * (max - min), 0.0002))  ->  floor_4(0.020909 - 0.0002) = 0.0207

With a spread of 0 the 0.0002 floor IS the whole margin: a bar 1% under the capture. A build that still draws the
drop reads 0.020909 and passes; a real loss of the drop's drawing trips it at once.

The probe now also CHECKS the "one tick" claim: the three drop rows of a band must share one sim_tick, or it fails
and names the ticks.

T1 keeps the old rule, 0.8 x the smallest of its three captures -> 0.0771, and its three captures at ticks 447, 943
and 1439. The owner asked about zoom 240; nothing at T1 was reshot or moved.

THE SPLASH IS A CROWN OF WATER NOW (g2-g2-g2, this lane, 2026-10-21). The owner asked in the goal: "make the close
splash a crown of water, not the blue column in splash_T1.png". The wet column was one upright Splash card and read as
a tall blue shaft; it is now a low centre mass inside a ring of six Splash jets thrown outward and up
(CombatFx.ShellFire.cs, the Crown* constants).

A look that changed has to be measured again, so the splash is three captures a band of this build, shot back to back
on one water point in one editor run, and its bar is the drop's T1 rule - 0.8 x the smallest of the three, rounded
down to four decimals - because these three reps are of ONE match at different ticks, not of one tick:

    T1    r1 0.098052  r2 0.103355  r3 0.106249   ticks 109, 357, 605;  spread 8.4%,  bar floor_4(0.8 * 0.098052) = 0.0784
    O240  r1 0.010805  r2 0.011056  r3 0.011756   ticks 109, 357, 605;  spread 8.8%,  bar floor_4(0.8 * 0.010805) = 0.0086

Both spreads are under 10%, so three reps of one run stand; they did not have to be split into separate runs of one
sim tick the way the drop's O240 was. The splash keeps its footprint floor AS WELL: its bar is
max(floor_for(area), SPLASH_BAR[band]), floor_kind ground_footprint+fixed_bar. At T1 the footprint (about 0.0636) is
the stricter of the two; at O240 the fixed bar is.

The first crown shot covered only 165-221 m^2 of ground against the 400 m^2 the footprint asks, where the old blue
shaft covered 508. The bar was NOT lowered to fit the effect (decisions.md, 2026-10-19): the crown was made bigger
instead, in three steps, until it covered 617-695 m^2. It is a wider, denser crown than the first draft, and it is
tall: that height is what the footprint rule costs an effect drawn as a ring of jets rather than as one card.

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

FOOTPRINT_SUBJECTS = ["smoke", "barrage"]
BANDS = ["T1", "O240"]
REPS = ["r1", "r2", "r3"]
ONE_TICK_BANDS = ["O240"]   # the bands whose three drop captures are separate runs of the same sim tick
DROP_BAR = {"T1": 0.0771, "O240": 0.0207}   # see the docstring: T1 0.8 x the smallest of three; O240 just under
                                            # the smallest of three captures of ONE sim tick
SPLASH_BAR = {"T1": 0.0784, "O240": 0.0086}  # 0.8 x the smallest of this build's three captures a band, rounded down
DIFF = 0.08             # how far a pixel's luma must move to count as the effect
MIN_GROUND_M2 = 400.0   # the ground an effect must cover: about 20 x 20 m
FRAC_CAP = 0.10         # never stricter than the old share-of-frame floor
MIN_PX = 150            # and never fewer changed pixels than this
MAX_POSE = 0.5
ROWS_WANTED = 16


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
    """Every row: its name, its band and the stem of its PNG pair. The drop and the splash are three reps a band."""
    out = []
    for subject in FOOTPRINT_SUBJECTS:
        for band in BANDS:
            out.append((subject, band, "%s_%s" % (subject, band)))
    for band in BANDS:
        for rep in REPS:
            out.append(("drop_" + rep, band, "drop_%s_%s" % (band, rep)))
    for band in BANDS:
        for rep in REPS:
            out.append(("splash_" + rep, band, "splash_%s_%s" % (band, rep)))
    return out


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", default=here, help="the folder with the stills, the plates and their sidecars")
    ap.add_argument("--tick-tol", type=int, default=2,
                    help="how many sim ticks a still and its plate may be apart (0 = exactly the same tick)")
    ap.add_argument("--out", default=None, help="where probe.json goes (default <dir>/probe.json)")
    a = ap.parse_args()
    out = a.out or os.path.join(a.dir, "probe.json")

    rows, fails = [], []
    for subject, band, stem in wanted():
        who = "%s %s" % (subject, band)
        still = os.path.join(a.dir, stem + ".png")
        plate = os.path.join(a.dir, stem + "_plate.png")
        if not os.path.exists(still) or not os.path.exists(plate):
            fails.append("%s: missing %s" % (who, "still" if not os.path.exists(still) else "plate"))
            continue
        sv, sw, sh = luma(still)
        pv, pw, ph = luma(plate)
        if (sw, sh) != (pw, ph):
            fails.append("%s: still is %dx%d, plate is %dx%d" % (who, sw, sh, pw, ph))
            continue
        px, frac = moved(sv, pv)

        ss, sp = sidecar(still), sidecar(plate)
        tick, ptick = int(ss.get("sim_tick", -1)), int(sp.get("sim_tick", -1))
        delta = abs(tick - ptick) if tick >= 0 and ptick >= 0 else -1
        pose = float(ss.get("pose_error_m", 9.0))
        ppose = float(sp.get("pose_error_m", 9.0))
        area = frame_area_m2(ss)
        is_drop = subject.startswith("drop")
        is_splash = subject.startswith("splash")
        # the splash answers to both: the ground it churns AND a bar outside the spread of this build's three captures
        if is_drop:
            bar = DROP_BAR[band]
        elif area <= 0:
            bar = None
        elif is_splash:
            bar = max(floor_for(area), SPLASH_BAR[band])
        else:
            bar = floor_for(area)

        rows.append({"subject": subject, "band": band, "effect_area_frac": round(frac, 6),
                     "frame_area_m2": round(area, 1), "effect_px": px,
                     "effect_ground_m2": round(frac * area, 1),
                     "floor_frac": round(bar, 6) if bar is not None else None,
                     "floor_kind": "fixed_bar" if is_drop else
                                   "ground_footprint+fixed_bar" if is_splash else "ground_footprint",
                     "pose_error_m": round(max(pose, ppose), 4), "sim_tick": tick, "plate_sim_tick": ptick,
                     "tick_delta": delta})

        if is_drop:
            if frac < bar:
                fails.append("%s: effect_area_frac %.6f below the fixed bar %.4f (the drop's own drawing shrank)"
                             % (who, frac, bar))
        elif area <= 0.0:
            fails.append("%s: the sidecar has no fov/aspect/cam_height_m, so the framed ground cannot be measured" % who)
        elif frac < bar:
            fails.append("%s: effect_area_frac %.4f below %.4f (%.0f m^2 of ground framed; the effect covers "
                         "%.0f m^2, under the %.0f m^2 asked for%s)"
                         % (who, frac, bar, area, frac * area, MIN_GROUND_M2,
                            ", or under the fixed bar %.4f" % SPLASH_BAR[band] if is_splash else ""))
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

    # The O240 bar is read off three captures of one sim tick, so "one tick" is a check, not a claim. T1's three
    # captures are of one match at ticks 447, 943 and 1439 and keep the 0.8 rule, so the check does not ask it.
    for band in ONE_TICK_BANDS:
        ticks = [r["sim_tick"] for r in rows if r["subject"].startswith("drop") and r["band"] == band]
        if len(ticks) == len(REPS) and len(set(ticks)) > 1:
            fails.append("drop %s: the three captures are on ticks %s, not one tick (the bar is set from three "
                         "captures of the same moment)" % (band, ", ".join(str(t) for t in ticks)))

    if len(rows) < ROWS_WANTED:
        fails.append("only %d rows: %d were asked for" % (len(rows), ROWS_WANTED))

    doc = {"rows": rows, "diff_threshold": DIFF, "min_ground_m2": MIN_GROUND_M2, "frac_cap": FRAC_CAP,
           "min_effect_px": MIN_PX, "drop_bar": DROP_BAR, "splash_bar": SPLASH_BAR, "tick_tolerance": a.tick_tol, "failures": fails}
    with open(out, "w", encoding="utf-8") as fh:
        json.dump(doc, fh, indent=2)
        fh.write("\n")

    for r in rows:
        print("%-10s %-5s frac %.6f (bar %s, %-16s)  px %-7d  framed %9.0f m^2  effect %7.0f m^2  pose %.3f  ticks %d/%d"
              % (r["subject"], r["band"], r["effect_area_frac"],
                 "%.6f" % r["floor_frac"] if r["floor_frac"] is not None else "   ?    ", r["floor_kind"],
                 r["effect_px"], r["frame_area_m2"], r["effect_ground_m2"], r["pose_error_m"],
                 r["sim_tick"], r["plate_sim_tick"]))
    print("rows %d  footprint bar = min(%.2f, %.0f / frame_area_m2); drop bar T1 %.4f O240 %.4f; splash bar T1 %.4f "
          "O240 %.4f on top of its footprint; effect_px >= %d"
          % (len(rows), FRAC_CAP, MIN_GROUND_M2, DROP_BAR["T1"], DROP_BAR["O240"], SPLASH_BAR["T1"],
             SPLASH_BAR["O240"], MIN_PX))
    for f in fails:
        print("FAIL " + f)
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
