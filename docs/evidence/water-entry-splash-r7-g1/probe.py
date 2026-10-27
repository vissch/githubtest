"""Measures the water entry's three beats in the stills beside this file, and writes probe.json.

What is being proved. r7 won its two bars - a ring that registers on its own (ring_only_px 1289 against a baseline
0) and a crown that stands as a value step over the water it is in (0.296 at land + 0.05 s against 0.032) - and the
owner still read the stills as "a brighter copy of the old wisp". He is right, and it is measured on r7's own
committed stills at commit 0fb59286ce:

  1. SHAPE. In run-6/land.png the changed-pixel mask is 3.38 m wide by 5.83 m tall - width over height 0.58 - and
     it is 32-40 px wide at every row. That is a column, the same shape as the dim column in run-1/land.png (0.32).
     The two wings sit at +/-1.26 m and the mask only reaches +/-1.7 m, so they add almost nothing.
  2. FALLEN FOAM. At land + 0.3 s only 41 % of the mask lies within 0.5 m of the water line and its top is still
     5.77 m up. Nothing has fallen back; the column has merely dimmed.
  3. THE BODY BEAT PROVED NOTHING. pre.png minus pre-nosplash.png was exactly ZERO pixels, because at land - 0.2 s
     the body is still over the dry bank and the ring and cards are still queued.

So the moments are the owner's three beats - body, then crown, then fallen foam - and the new keys measure the
shape of each one, not just that something changed:

  contact_px    changed pixels in the body box between body.png and body-nosplash.png: at land - 0.05 s, is the
                water already breaking where he goes in. The baseline is 0 and structurally so: the cards and the
                ring are queued for the landing instant, so nothing at all is up before it.
  body_px       body.png against body-nobody.png in the same box: the man himself reads, and he reads OVER the
                creek rather than over the bank. It is a floor carried from the baseline, not a bar: the baseline
                already draws a man there, and contact_px is the key that proves the beat.
  crown_aspect  the crown.png mask's bounding box, width over height. "Wider than tall" is the plainest statement
                of a crown instead of a column: the bar is 1.0 against 0.579-0.596 over three captures of the
                current build.
  foam_flat     the share of the foam.png mask within 0.5 m of the mark's row, and foam_top_m, how far above the
                mark the mask still reaches at land + 0.3 s. Foam that has fallen lies on the water: the bars are
                0.70 (current build 0.291-0.294) and 2.5 m (current build 5.77 m, three captures out of three).

Every r7 key is carried unchanged, so r7's wins cannot regress: ring_only_px, crown_contrast_land / _post,
noise_px, ring_px_land / _post and ring_centroid_off_m. The three that had bars keep them as GUARDS - the current
build already clears them, because they are what r7 won, so the fixed build must still meet them but the baseline
is not asked to sit below them. Only this round's four bars have to sit outside the baseline's spread.

The stills. AgentScripts/water_entry_fling.cs stages them and CaptureRig renders them, in Play in GreyboxCorridor at
night (the owner's target look), camera T1 - zoom 30, yaw 21, pitch 25 - aimed at the ford, 1600 x 900, rain 0, the
sky and water clocks pinned and the sim frozen. A body is flung off the dry bank at (12, 128) into knee-deep water
at (15, 130) with the presentation clock running; fly.y is bisected until the flight is exactly 43 frames at 1/60 s.
CaptureRig.Mark named (15, 130) before the shots, so every sidecar carries that point's projection (mark_px_x/y and
mark_px_per_m) and pixels can be turned into metres. Each run writes its own `run-N/` folder beside this file:

  plate.png            the ford with no throw at all, for the record
  body.png             land - 0.05 s: the man going in
  body-nobody.png      the same frozen instant with the fallen men list emptied, cards and rings KEPT
  body-nosplash.png    the same frozen instant with the cards and rings out, the man KEPT
  crown.png            land + 0.05 s
  foam.png             land + 0.3 s
  crown-run2.png       land + 0.05 s of a SECOND throw with the same seed, the scene put back to before run 1: its
                       difference from crown.png is the splash's own repeatability, the noise floor, measured live
  <name>-noring.png    crown and foam with the ring slots parked but the cards KEPT, which is what lets
                       ring_only_px be the min of two moments
  <name>-nosplash.png  body, crown and foam re-rendered at that SAME frozen instant with the water entry lifted
                       out - WaterRings' ring array parked and FlipbookFx's card list emptied - and nothing else
                       touched

Why each moment has its own reference instead of a plate taken earlier: the water's foam and lapping are animated
from the water clock in TWWater.hlsl, so a still taken a second after the plate differs from it across the whole
water sheet (about 1,600 changed pixels in the disc at the landing point whether anything landed there or not). A
reference rendered at the same frozen instant with the splash lifted out has the same foam, the same body, the same
shadows and the same sky, so the difference between the pair is the water entry and nothing else.

The four regions.
  The 6 m window (r6's, unchanged) counts changed pixels: within 3 m across the view of the mark, and from 3 m of
  height above the mark's pixel row down to 3 m below it. A water entry is a ring lying flat ON the surface and a
  crown standing UP out of it, so the window has to hold both. Every r7 key is measured in it, unchanged.
  The shape box is wider and much taller - 4 m across either way, 7 m of height above the mark's row and 1 m below
  it - and only the shape keys use it. The 6 m window will not do for them: the current build's splash reaches 5.2
  to 5.8 m above the water line, so a mask measured in the window is CLIPPED by its 3 m top and its height reads
  3.02 m in every run, baseline and fixed alike. An aspect taken off a clipped mask is the window's shape, not the
  splash's. Measured in the shape box, the current build reads 3.4-3.5 m wide by 5.9 m tall at the crown moment -
  the same column the owner read off r7's stills by hand (3.38 x 5.83 m) - so the box is big enough to hold
  whatever the fix makes of it too.
  The crown box is where the crown stands: mark_px_x +/- 1.4 m across the view, and the rows from 2.8 m of height
  above the mark down to 0.2 m below it. That is about 3,100 px at this projection, which the crown card itself
  fills, so a p90 taken in it sits inside the crown and not on its edge. p90 is coverage-limited by design: a bright
  but thin crown misses the bar, which is the point - the owner asked for a crown a player sees, not a hot line.
  The body box is where the man goes in: mark_px_x +/- 1.5 m across the view, and from 3 m of height above the
  mark's row down to 0.5 m below it. It is narrower and lower than the window on purpose: a man still over the bank
  at that instant falls outside it, so body_px cannot be earned by a body that has not reached the creek.

The centroid is measured ACROSS the view only (the pixel column against mark_px_x). mark_px_per_m converts ground
metres; a splash stands up out of the water and its height in pixels is not a distance along the ground, so turning
a vertical offset into metres with the same number would report a splash exactly on its mark as a metre away. The
shape keys (crown_aspect, foam_flat, foam_top_m) use the same number for height, which is what the owner's own
reading of the stills used: they compare a mask with a mask, so the convention cancels.

A pixel counts as changed when its Rec.709 luma (0..1, the scale CaptureRig's own numbers use) moves by more than
0.02. The threshold is r5's, unchanged: it is never raised to make a bar pass.

The baseline, and why it is three runs. The owner's rule, kept from r7: capture the CURRENT build three times
BEFORE changing anything, and record each bar key's worst-for-the-baseline value over those three. Each bar must sit
outside that spread, and the fixed build must clear it with at least three runs of its own, each key taken the
pessimistic way. The baseline gets its best three-run showing and the new build its worst, in both directions: for
a bar with a floor the baseline is the MAX and the fix the MIN; for a bar with a ceiling (foam_top_m) it is the
other way round.

Run:
  python docs/evidence/water-entry-splash-r7-g1/probe.py --base  run-1 run-2 run-3   # the unchanged build, first
  python docs/evidence/water-entry-splash-r7-g1/probe.py --fixed run-4 run-5 run-6   # the fix; exit 0 when it clears
  python docs/evidence/water-entry-splash-r7-g1/probe.py                             # re-measure the committed runs

With no arguments it re-measures the runs named in the committed probe.json (base_runs and fixed_runs), re-checks
every bar and exits 0 only when the fixed block passes. That is the leg's done-when, so the numbers in probe.json
can never drift from the stills beside it.
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 0.02       # luma move that counts as a changed pixel
WINDOW_M = 3.0         # half-width and half-height of the counting window on the landing point
CROWN_HALF_M = 1.4     # the crown box's half-width across the view
CROWN_UP_M = 2.8       # how far above the mark's row the crown box reaches
CROWN_DOWN_M = 0.2     # and how far below it
BODY_HALF_M = 1.5      # the body box's half-width across the view
BODY_UP_M = 3.0        # how far above the mark's row the body box reaches
BODY_DOWN_M = 0.5      # and how far below it
FLAT_M = 0.5           # "lying on the water": within this many metres of the mark's row
SHAPE_HALF_M = 4.0     # the shape box's half-width across the view
SHAPE_UP_M = 7.0       # how far above the mark's row the shape box reaches
SHAPE_DOWN_M = 1.0     # and how far below it

# The three bars of THIS round, the shape of the beats the owner asked for. FLOORS must be reached, CEILINGS must
# not be exceeded, and each has to sit outside the spread of three captures of the unchanged build.
FLOORS = {"contact_px": 250, "crown_aspect": 1.0, "foam_flat": 0.70}
CEILINGS = {"foam_top_m": 2.5}
# r7's bars, carried unchanged. The current build already clears these - they are what r7 won - so they are GUARDS:
# the fixed build must still meet them, and the baseline is not asked to sit below them.
GUARDS = {"ring_only_px": 300, "crown_contrast_land": 0.15, "crown_contrast_post": 0.09}
# What each number was read off the CURRENT build's own stills for, at commit 0fb59286ce: the bars are set from
# these, never fitted to the capture of a fixed build.
BASELINE = {"contact_px": 0, "crown_aspect": 0.580, "foam_flat": 0.294, "foam_top_m": 5.77,
            "ring_only_px": 1287, "crown_contrast_land": 0.2935, "crown_contrast_post": 0.2092}


def luma(run, name):
    """Rec.709 luma in 0..1 as a 2-D array, read from the image's top-left the way the sidecar counts pixels."""
    im = Image.open(os.path.join(HERE, run, name)).convert("RGB")
    a = np.asarray(im, dtype=np.float64) / 255.0
    return 0.2126 * a[:, :, 0] + 0.7152 * a[:, :, 1] + 0.0722 * a[:, :, 2]


def box(shape, cx, cy, left, right, up, down):
    """A slice pair for an upright box on the mark, clamped to the image."""
    h, w = shape
    x0, x1 = max(0, int(cx - left)), min(w, int(cx + right) + 1)
    y0, y1 = max(0, int(cy - up)), min(h, int(cy + down) + 1)
    return (slice(y0, y1), slice(x0, x1))


def mask(a, b, sel):
    """The changed-pixel mask inside `sel`, as a boolean array of that box's shape."""
    return np.abs(a[sel] - b[sel]) > THRESHOLD


def changed(a, b, sel):
    """Changed pixels in `sel`, their mean column, and the largest luma move seen there."""
    d = np.abs(a[sel] - b[sel])
    peak = float(d.max()) if d.size else 0.0
    hit = d > THRESHOLD
    n = int(hit.sum())
    col = float((np.nonzero(hit)[1] + sel[1].start).mean()) if n else None
    return n, col, peak


def shape_of(m, sel, cy, ppm):
    """The mask's bounding box as width over height, how far above the mark it reaches, and its flat share.

    Rows and columns are the mask's own; `cy` is the mark's pixel row, so `top_m` is a height above the water line
    and `flat` is the share of the mask lying within FLAT_M of that row - foam that has fallen back onto the water.
    """
    rows, cols = np.nonzero(m)
    if rows.size == 0:
        return {"aspect": 0.0, "top_m": 0.0, "flat": 0.0, "px": 0, "w_m": 0.0, "h_m": 0.0}
    y = rows + sel[0].start
    w_px = cols.max() - cols.min() + 1
    h_px = y.max() - y.min() + 1
    flat = int((np.abs(y - cy) <= FLAT_M * ppm).sum())
    return {"aspect": round(float(w_px) / float(h_px), 3),
            "top_m": round(float(cy - y.min()) / ppm, 2),
            "flat": round(flat / float(rows.size), 3),
            "px": int(rows.size),
            "w_m": round(float(w_px) / ppm, 2),
            "h_m": round(float(h_px) / ppm, 2)}


def timings(run):
    """The achieved moments, as the harness measured them: `<name> <delta>` lines in timings.txt."""
    out = {}
    path = os.path.join(HERE, run, "timings.txt")
    if not os.path.exists(path):
        return out
    with open(path, encoding="utf-8") as f:
        for line in f:
            bits = line.split()
            if len(bits) == 2:
                try:
                    out[bits[0]] = float(bits[1])
                except ValueError:
                    pass
    return out


def measure(run):
    """Every number one capture run yields."""
    side = json.load(open(os.path.join(HERE, run, "crown.json"), encoding="utf-8"))
    cx, cy, ppm = side["mark_px_x"], side["mark_px_y"], side["mark_px_per_m"]

    body, body_ref = luma(run, "body.png"), luma(run, "body-nosplash.png")
    body_nobody = luma(run, "body-nobody.png")
    crown_i, crown_ref = luma(run, "crown.png"), luma(run, "crown-nosplash.png")
    crown_noring = luma(run, "crown-noring.png")
    crown_run2 = luma(run, "crown-run2.png")
    foam_i, foam_ref = luma(run, "foam.png"), luma(run, "foam-nosplash.png")
    foam_noring = luma(run, "foam-noring.png")

    win = box(crown_i.shape, cx, cy, WINDOW_M * ppm, WINDOW_M * ppm, WINDOW_M * ppm, WINDOW_M * ppm)
    crown_b = box(crown_i.shape, cx, cy, CROWN_HALF_M * ppm, CROWN_HALF_M * ppm, CROWN_UP_M * ppm, CROWN_DOWN_M * ppm)
    body_b = box(crown_i.shape, cx, cy, BODY_HALF_M * ppm, BODY_HALF_M * ppm, BODY_UP_M * ppm, BODY_DOWN_M * ppm)
    shape_b = box(crown_i.shape, cx, cy, SHAPE_HALF_M * ppm, SHAPE_HALF_M * ppm, SHAPE_UP_M * ppm, SHAPE_DOWN_M * ppm)

    contact_n, _, contact_peak = changed(body, body_ref, body_b)
    body_n, _, body_peak = changed(body, body_nobody, body_b)
    land_n, land_col, land_peak = changed(crown_i, crown_ref, win)
    noise_n, _, noise_peak = changed(crown_i, crown_run2, win)
    post_n, _, post_peak = changed(foam_i, foam_ref, win)
    only_land, _, only_land_peak = changed(crown_i, crown_noring, win)
    only_post, _, only_post_peak = changed(foam_i, foam_noring, win)

    crown_shape = shape_of(mask(crown_i, crown_ref, shape_b), shape_b, cy, ppm)
    foam_shape = shape_of(mask(foam_i, foam_ref, shape_b), shape_b, cy, ppm)

    def p90(img):
        return float(np.percentile(img[crown_b], 90))

    t = timings(run)
    return {
        "run": run,
        "contact_px": contact_n,
        "body_px": body_n,
        "crown_aspect": crown_shape["aspect"],
        "foam_flat": foam_shape["flat"],
        "foam_top_m": foam_shape["top_m"],
        "ring_only_px": min(only_land, only_post),
        "crown_contrast_land": round(p90(crown_i) - p90(crown_ref), 4),
        "crown_contrast_post": round(p90(foam_i) - p90(foam_ref), 4),
        "crown_w_m": crown_shape["w_m"],
        "crown_h_m": crown_shape["h_m"],
        "crown_mask_px": crown_shape["px"],
        "crown_flat": crown_shape["flat"],
        "crown_top_m": crown_shape["top_m"],
        "foam_aspect": foam_shape["aspect"],
        "foam_w_m": foam_shape["w_m"],
        "foam_h_m": foam_shape["h_m"],
        "foam_mask_px": foam_shape["px"],
        "ring_only_px_land": only_land,
        "ring_only_px_post": only_post,
        "ring_px_land": land_n,
        "ring_px_post": post_n,
        "noise_px": noise_n,
        "ring_centroid_off_m": round(9.0 if land_col is None else abs(land_col - cx) / ppm, 3),
        "contact_peak_luma": round(contact_peak, 4),
        "body_peak_luma": round(body_peak, 4),
        "ring_peak_luma_land": round(land_peak, 4),
        "ring_peak_luma_post": round(post_peak, 4),
        "ring_peak_luma_noise": round(noise_peak, 4),
        "ring_peak_luma_only_land": round(only_land_peak, 4),
        "ring_peak_luma_only_post": round(only_post_peak, 4),
        "crown_p90_land": round(p90(crown_i), 4),
        "crown_p90_post": round(p90(foam_i), 4),
        "crown_water_p90_land": round(p90(crown_ref), 4),
        "crown_water_p90_post": round(p90(foam_ref), 4),
        "t_body_s": t.get("body"),
        "t_crown_s": t.get("crown"),
        "t_foam_s": t.get("foam"),
        "t_crown_run2_s": t.get("crown-run2"),
        "flight_s": t.get("flight_s"),
        "flight_frames": t.get("flight_frames"),
        "pose_error_m": side.get("pose_error_m"),
        "rain": side.get("rain"),
        "window_px": int((win[0].stop - win[0].start) * (win[1].stop - win[1].start)),
        "crown_px": int((crown_b[0].stop - crown_b[0].start) * (crown_b[1].stop - crown_b[1].start)),
        "body_box_px": int((body_b[0].stop - body_b[0].start) * (body_b[1].stop - body_b[1].start)),
        "shape_box_px": int((shape_b[0].stop - shape_b[0].start) * (shape_b[1].stop - shape_b[1].start)),
        "mark_px_x": cx,
        "mark_px_y": cy,
        "mark_px_per_m": ppm,
    }


# The keys that are not bars, and which way to take each over a set of runs: the pessimistic one in every case.
WORST = {
    "body_px": min, "ring_px_land": min, "ring_px_post": min, "noise_px": max,
    "ring_centroid_off_m": max, "ring_peak_luma_noise": max,
    "contact_peak_luma": min, "body_peak_luma": min,
    "ring_peak_luma_land": min, "ring_peak_luma_post": min,
    "ring_peak_luma_only_land": min, "ring_peak_luma_only_post": min,
    "ring_only_px_land": min, "ring_only_px_post": min,
    "crown_p90_land": min, "crown_p90_post": min,
    "crown_water_p90_land": max, "crown_water_p90_post": max,
    "crown_w_m": min, "crown_h_m": max, "crown_mask_px": min, "crown_flat": min, "crown_top_m": max,
    "foam_aspect": min, "foam_w_m": min, "foam_h_m": max, "foam_mask_px": min,
}
CARRIED = ("t_body_s", "t_crown_s", "t_foam_s", "t_crown_run2_s", "flight_s", "flight_frames",
           "window_px", "crown_px", "body_box_px", "shape_box_px", "mark_px_x", "mark_px_y", "mark_px_per_m",
           "pose_error_m", "rain")


def fold(rows, key, how):
    return how(r[key] for r in rows)


def base_block(out, rows, runs):
    """The baseline's own best three-run showing, which every bar has to sit outside of."""
    for key in list(FLOORS) + list(GUARDS):
        out[key + "_base_max"] = fold(rows, key, max)
    for key in CEILINGS:
        out[key + "_base_min"] = fold(rows, key, min)
    out["n_base_captures"] = len(rows)
    out["base_runs"] = list(runs)
    out["runs"]["base"] = rows
    ok = (len(rows) >= 3
          and all(out[k + "_base_max"] < v for k, v in FLOORS.items())
          and all(out[k + "_base_min"] > v for k, v in CEILINGS.items()))
    out["base_below_bars"] = ok
    return ok


def fixed_block(out, rows, runs):
    """The fixed build's worst three-run showing, which has to clear every bar and the baseline's spread."""
    for key in list(FLOORS) + list(GUARDS):
        out[key] = fold(rows, key, min)
    for key in CEILINGS:
        out[key] = fold(rows, key, max)
    for key, how in WORST.items():
        out[key] = fold(rows, key, how)
    for key in CARRIED:
        out[key] = rows[0][key]
    out["n_captures"] = len(rows)
    out["fixed_runs"] = list(runs)
    out["runs"]["fixed"] = rows
    ok = (len(rows) >= 3
          and all(out[k] >= v for k, v in FLOORS.items())
          and all(out[k] >= v for k, v in GUARDS.items())
          and all(out[k] <= v for k, v in CEILINGS.items())
          and all(k + "_base_max" in out and out[k + "_base_max"] < out[k] for k in FLOORS)
          and all(k + "_base_min" in out and out[k + "_base_min"] > out[k] for k in CEILINGS)
          and out["body_px"] >= 200
          and out["ring_px_land"] >= max(1000, 3 * out["noise_px"])
          and out["ring_px_post"] >= 600
          and out["ring_centroid_off_m"] <= 0.5)
    out["pass"] = ok
    return ok


def main():
    path = os.path.join(HERE, "probe.json")
    out = json.load(open(path, encoding="utf-8")) if os.path.exists(path) else {}
    out["luma_threshold"] = THRESHOLD
    out["window_m"] = WINDOW_M
    out["crown_box_m"] = [CROWN_HALF_M, CROWN_UP_M, CROWN_DOWN_M]
    out["shape_box_m"] = [SHAPE_HALF_M, SHAPE_UP_M, SHAPE_DOWN_M]
    out["body_box_m"] = [BODY_HALF_M, BODY_UP_M, BODY_DOWN_M]
    out["flat_m"] = FLAT_M
    out["bars"] = {"floors": FLOORS, "ceilings": CEILINGS, "guards_carried_from_r7": GUARDS}
    out["bars_read_off_current_build"] = BASELINE
    out["noise_source"] = "second_throw"
    out.setdefault("runs", {})

    args = sys.argv[1:]
    if not args:
        # The committed numbers, re-measured from the committed stills: both blocks again, and the verdict is the
        # fixed one's. A run folder named in probe.json that is not on disk is a failure, not a skip.
        base, fixed = out.get("base_runs") or [], out.get("fixed_runs") or []
        if not base or not fixed:
            print("probe.json names no base_runs/fixed_runs yet: run --base, then --fixed")
            return 2
        base_ok = base_block(out, [measure(r) for r in base], base)
        ok = fixed_block(out, [measure(r) for r in fixed], fixed) and base_ok
    elif args[0] in ("--base", "--fixed") and len(args) >= 2:
        mode, runs = args[0], args[1:]
        rows = [measure(r) for r in runs]
        ok = base_block(out, rows, runs) if mode == "--base" else fixed_block(out, rows, runs)
    else:
        print("usage: probe.py [--base|--fixed <run dir> [<run dir> ...]]")
        return 2

    with open(path, "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    print(json.dumps({k: v for k, v in out.items() if k != "runs"}, indent=2))
    for block in ("base", "fixed"):
        for row in out["runs"].get(block, []):
            print("  %-5s %-8s contact %-6s body %-6s aspect %-6s foam_flat %-6s foam_top_m %-5s ring_only %-6s "
                  "crown_land %s"
                  % (block, row["run"], row["contact_px"], row["body_px"], row["crown_aspect"], row["foam_flat"],
                     row["foam_top_m"], row["ring_only_px"], row["crown_contrast_land"]))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
