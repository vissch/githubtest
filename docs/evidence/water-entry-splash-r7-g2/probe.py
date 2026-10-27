"""Measures the water entry's ring ON THE WATER and the crown's FALL, in the stills beside this file.

What is being proved, and why this round has its own folder. r7 proved a crown and a ring in pixels and the owner
looked at the result: "The ring is a faint disc and the crown does not come down. land-noring.png keeps the column
and only loses the disc, and post.png at +0.3 s still holds a white sprite in the water." So two of r7's own
answers are overturned by the owner's eye:

  1. r7 measured the ring as `ring_only_px` - a COUNT of pixels the ring alone changed, anywhere in a 6 m window
     that also holds the standing crown. A faint disc wide enough passes that count. This round measures the ring
     as a VALUE STEP, p90 over p90, inside the ring's OWN footprint: a thin band lying flat on the water with the
     crown's column cut out, so the crown can never pay for the ring.
  2. r7 barred the crown to STILL READ at land + 0.3 s (`crown_contrast_post >= 0.09`) and the build cleared it at
     0.208 - which is exactly the white sprite still standing in the water the owner now objects to. The owner's
     latest word wins: at the post frame the crown must be DOWN, measured as its topmost lit row in the crown box.

`crown_contrast_land` is carried over from r7 unchanged, so the fix cannot pass by simply deleting the crown: the
crown must still STAND at the landing frame and be GONE a third of a second later.

r7's folder, its numbers and its README are left exactly as they are; nothing here edits them.

The stills. Unchanged from r7, and every still these bars need is already shot by the same harness:
`AgentScripts/water_entry_fling.cs` stages them and CaptureRig renders them, in Play in GreyboxCorridor at night,
camera T1 - zoom 30, yaw 21, pitch 25 - aimed at the ford, 1600 x 900, rain 0, the sky and water clocks pinned and
the sim frozen. A body is flung off the dry bank at (12, 128) into knee-deep water at (15, 130) with the
presentation clock running; fly.y is bisected until the flight is exactly 43 frames at 1/60 s. CaptureRig.Mark
named (15, 130) before the shots, so every sidecar carries that point's projection (mark_px_x/y and mark_px_per_m)
and pixels can be turned into metres. Each run writes its own `run-N/` folder beside this file:

  plate.png            the ford with no throw at all, for the record
  pre.png              land - 0.2 s: the body is still in the air
  land.png             land + 0.05 s
  post.png             land + 0.3 s
  land-run2.png        land + 0.05 s of a SECOND throw with the same seed: its difference from land.png is the
                       splash's own repeatability, the noise floor, measured live
  land-noring.png      the frozen landing with the ring slots parked but the cards KEPT
  post-noring.png      the same at land + 0.3 s
  <name>-nosplash.png  pre, land and post re-rendered at that SAME frozen instant with the water entry lifted out -
                       WaterRings' ring array parked and FlipbookFx's card list emptied - and nothing else touched

Why each moment has its own reference instead of a plate taken earlier: the water's foam and lapping are animated
from the water clock in TWWater.hlsl, so a still taken a second after the plate differs from it across the whole
water sheet. A reference rendered at the same frozen instant with the splash lifted out has the same foam, the same
body, the same shadows and the same sky, so the difference between the pair is the water entry and nothing else.

The three regions.
  The WATER BAND is new, and it is the ring's own footprint, flat on the surface: columns mark_px_x +/- 2.2 m, rows
  from 0.3 m of height above the mark's row down to 1.0 m below it, with the crown's own column (mark_px_x +/- 0.5
  m) CUT OUT. About 1,600 px at this projection. A ring a player sees on the water has to lift the value of this
  band; a crown standing above it cannot, because the band is 1.3 m tall and the crown's column is not in it.
  p90 is coverage-limited by design, as in r7: a hot one-pixel line misses it, which is the point.
  The CROWN BOX is r7's, unchanged: mark_px_x +/- 1.4 m across the view, and the rows from 2.8 m of height above
  the mark down to 0.2 m below it.
  The 6 m WINDOW is r6's, unchanged, and only r6's carried guards use it: within 3 m across the view of the mark
  and 3 m of height either side of its row.

Heights are in PIXELS, never in metres. mark_px_per_m converts ground metres; a crown stands UP out of the water
and its height in pixels is not a distance along the ground, so `crown_top_px_post` is a pixel count and the camera
is asserted to be r7's exact projection (mark_px_per_m 19.22227) so the number means the same thing in every run.
The metres are reported beside it for the reader only.

A pixel counts as changed when its Rec.709 luma (0..1, the scale CaptureRig's own numbers use) moves by more than
0.02. The threshold is r5's, unchanged: it is never raised to make a bar pass.

The bars, and which way each is aggregated. For a `>=` bar the baseline takes the MAX over its three runs and the
fix the MIN; for a `<=` bar the baseline takes the MIN and the fix the MAX. Conservative in both directions: the
old build gets its best three-run showing, the new one its worst.

  ring_step           >= 0.08  p90(moment) - p90(moment-noring) over the water band; the MIN of land and post
  ring_px_band        >= 1000  pixels in the band whose ring-only move passes 0.02; the MIN of the two moments.
                               The plan's bar was 600 and the baseline MEASURED 702, 704 and 705 - the faint disc
                               the owner is objecting to already covers that much of the band - so the bar was made
                               HARDER, not softened: 1000 of the band's 1650 px, 1.42x the baseline's best showing
                               and far outside its 3 px spread.
  ring_hollow         <= 0.5   mean ring-only lift inside 0.45 r over the mean on the crest annulus, at post
  crown_top_px_post   <= 10    topmost row above the mark row in the crown box where post-noring - post-nosplash
                               passes 0.02, in pixels: the crown and its wings have fallen back into the water.
                               The ring is PARKED in both halves of that pair, because a ring lying flat on the
                               water shows its far arc above the mark's row and would otherwise pay this bar
  crown_contrast_land >= 0.15  r7's key, unchanged: the crown still stands at the landing frame

The first four are REGRESSION bars: the current build is on the failing side of each, and `--base` proves it over
three runs. `crown_contrast_land` is a GUARD: the current build already clears it, and it is kept so the fix cannot
reach the other four by deleting the crown. The baseline check covers the regression bars only; a guard the
baseline passes is not a soft bar, it is a thing that must not break.

Run:
  python docs/evidence/water-entry-splash-r7-g2/probe.py --base  run-1 run-2 run-3   # the unchanged build, first
  python docs/evidence/water-entry-splash-r7-g2/probe.py --fixed run-4 run-5 run-6   # the fix
  python docs/evidence/water-entry-splash-r7-g2/probe.py                             # re-measure both from
                                                                                     # probe.json; writes nothing
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 0.02       # luma move that counts as a changed pixel
WINDOW_M = 3.0         # r6's counting window: half-width and half-height on the landing point
CROWN_HALF_M = 1.4     # the crown box's half-width across the view
CROWN_UP_M = 2.8       # how far above the mark's row the crown box reaches
CROWN_DOWN_M = 0.2     # and how far below it
BAND_HALF_M = 2.2      # the water band's half-width across the view
BAND_UP_M = 0.3        # how far above the mark's row the band reaches
BAND_DOWN_M = 1.0      # and how far below it
BAND_CUT_M = 0.5       # the crown's own column, cut out of the band
ANNULUS_M = 0.15       # half-width of the crest annulus the hollow is measured against
HOLLOW_INNER = 0.45    # the hole is the disc inside this fraction of the crest radius
PPM_EXPECTED = 19.22227  # r7's pinned projection; a different camera makes the pixel bars meaningless

# The regression bars: the current build is on the failing side of each. A `<=` bar is written as a negative number
# here and compared the other way round by passes(); LOWER_IS_BETTER names them.
BARS = {"ring_step": 0.08, "ring_px_band": 1000, "ring_hollow": -0.5, "crown_top_px_post": -10}
LOWER_IS_BETTER = ("ring_hollow", "crown_top_px_post")
# Carried from r7: the baseline already clears it, and it is kept so the fix cannot delete the crown.
GUARDS = {"crown_contrast_land": 0.15}


def bar_of(key):
    """The bar as a positive number, and True when the test is `<=` rather than `>=`."""
    v = BARS[key] if key in BARS else GUARDS[key]
    return abs(v), key in LOWER_IS_BETTER


def passes(key, value):
    bar, lower = bar_of(key)
    return value <= bar if lower else value >= bar


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


def changed(a, b, sel):
    """Changed pixels in `sel`, their mean column, and the largest luma move seen there."""
    d = np.abs(a[sel] - b[sel])
    peak = float(d.max()) if d.size else 0.0
    hit = d > THRESHOLD
    n = int(hit.sum())
    col = float((np.nonzero(hit)[1] + sel[1].start).mean()) if n else None
    return n, col, peak


def band_mask(shape, cx, cy, ppm):
    """The water band as a mask, the same band WITHOUT the crown cut, and every pixel's metres across the view."""
    h, w = shape
    xs = (np.arange(w) - cx) / ppm                       # metres across the view, signed
    ys = np.arange(h) - cy                               # pixel rows; below the mark is positive
    gx = np.broadcast_to(xs, (h, w))
    gy = np.broadcast_to(ys[:, None], (h, w))
    inside = (np.abs(gx) <= BAND_HALF_M) & (gy >= -BAND_UP_M * ppm) & (gy <= BAND_DOWN_M * ppm)
    return inside & (np.abs(gx) > BAND_CUT_M), inside, np.abs(gx)


def hollow(lift, inside, radius_m):
    """The hole in the ring at the post frame: mean lift inside 0.45 r over the mean on the crest annulus.

    The crest radius is measured, not assumed: the 0.1 m ring of radius with the greatest mean lift. The crown cut
    is NOT applied here - the ring-only difference holds no crown at all, so the centre is readable - and a crest
    that lifts nothing reports 9.0, the failing side, rather than dividing by zero.
    """
    edges = np.arange(0.0, BAND_HALF_M + 0.1001, 0.1)
    best_r, best_v = 0.0, -1e9
    for lo, hi in zip(edges[:-1], edges[1:]):
        sel = inside & (radius_m >= lo) & (radius_m < hi)
        if sel.sum() < 20:
            continue
        v = float(lift[sel].mean())
        if v > best_v:
            best_r, best_v = float((lo + hi) / 2.0), v
    if best_v <= 0.0:
        return 9.0, round(best_r, 3), 0.0, 0.0
    inner = inside & (radius_m < HOLLOW_INNER * best_r)
    crest = inside & (np.abs(radius_m - best_r) <= ANNULUS_M)
    if inner.sum() < 20 or crest.sum() < 20:
        return 9.0, round(best_r, 3), 0.0, round(best_v, 4)
    im, cm = float(lift[inner].mean()), float(lift[crest].mean())
    return round(im / cm, 3), round(best_r, 3), round(im, 4), round(cm, 4)


def crown_top_px(post_noring, post_ref, crown, cy):
    """How far above the mark's row, in pixels, the CARDS' own change still reaches at the post frame.

    The pair is post-noring (ring slots parked, cards kept) against post-nosplash (both gone), so what is measured
    is the crown and its wings and nothing else. post.png itself will not do: a ring lying flat on the water shows
    its far arc ABOVE the mark's row on screen - at this projection a 1.4 m ring reaches about 12 px up, and a
    widened crest more - so measuring post against post-nosplash let the RING pay the crown's bar. It read 22 px
    with the cards already gone. The bar names the crown, so it measures the crown; the baseline's 53 px is
    re-measured the same way, from the same committed stills.
    """
    hit = np.abs(post_noring[crown] - post_ref[crown]) > THRESHOLD
    rows = np.nonzero(hit)[0] + crown[0].start
    above = rows[rows < int(cy)]
    return int(int(cy) - above.min()) if above.size else 0


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
    side = json.load(open(os.path.join(HERE, run, "land.json"), encoding="utf-8"))
    cx, cy, ppm = side["mark_px_x"], side["mark_px_y"], side["mark_px_per_m"]
    if abs(ppm - PPM_EXPECTED) > 0.01:
        raise SystemExit("%s: mark_px_per_m is %s, not r7's %s - the camera moved, so the pixel bars "
                         "(crown_top_px_post, ring_px_band) do not mean the same thing. Re-shoot at T1."
                         % (run, ppm, PPM_EXPECTED))

    pre, pre_ref = luma(run, "pre.png"), luma(run, "pre-nosplash.png")
    land, land_ref = luma(run, "land.png"), luma(run, "land-nosplash.png")
    land_noring = luma(run, "land-noring.png")
    land_run2 = luma(run, "land-run2.png")
    post, post_ref = luma(run, "post.png"), luma(run, "post-nosplash.png")
    post_noring = luma(run, "post-noring.png")

    win = box(land.shape, cx, cy, WINDOW_M * ppm, WINDOW_M * ppm, WINDOW_M * ppm, WINDOW_M * ppm)
    crown = box(land.shape, cx, cy, CROWN_HALF_M * ppm, CROWN_HALF_M * ppm, CROWN_UP_M * ppm, CROWN_DOWN_M * ppm)
    bandm, inside, radius = band_mask(land.shape, cx, cy, ppm)

    # r6's carried guards, on r6's 6 m window.
    pre_n, _, pre_peak = changed(pre, pre_ref, win)
    land_n, land_col, land_peak = changed(land, land_ref, win)
    noise_n, _, noise_peak = changed(land, land_run2, win)
    post_n, _, post_peak = changed(post, post_ref, win)

    def p90c(img):
        return float(np.percentile(img[crown], 90))

    def p90b(img):
        return float(np.percentile(img[bandm], 90))

    step_land = p90b(land) - p90b(land_noring)
    step_post = p90b(post) - p90b(post_noring)
    px_land = int(((np.abs(land - land_noring) > THRESHOLD) & bandm).sum())
    px_post = int(((np.abs(post - post_noring) > THRESHOLD) & bandm).sum())
    hol, crest_r, inner_lift, crest_lift = hollow(post - post_noring, inside, radius)
    top_px = crown_top_px(post_noring, post_ref, crown, cy)

    t = timings(run)
    return {
        "run": run,
        "ring_step": round(min(step_land, step_post), 4),
        "ring_px_band": min(px_land, px_post),
        "ring_hollow": hol,
        "crown_top_px_post": top_px,
        "crown_contrast_land": round(p90c(land) - p90c(land_ref), 4),
        "ring_step_land": round(step_land, 4),
        "ring_step_post": round(step_post, 4),
        "ring_px_band_land": px_land,
        "ring_px_band_post": px_post,
        "ring_crest_r_m": crest_r,
        "ring_inner_lift": inner_lift,
        "ring_crest_lift": crest_lift,
        "crown_top_m_post": round(top_px / ppm, 3),
        "band_p90_land": round(p90b(land), 4),
        "band_p90_land_noring": round(p90b(land_noring), 4),
        "band_p90_post": round(p90b(post), 4),
        "band_p90_post_noring": round(p90b(post_noring), 4),
        "crown_contrast_post": round(p90c(post) - p90c(post_ref), 4),
        "crown_p90_land": round(p90c(land), 4),
        "crown_p90_post": round(p90c(post), 4),
        "crown_water_p90_land": round(p90c(land_ref), 4),
        "crown_water_p90_post": round(p90c(post_ref), 4),
        "ring_px_pre": pre_n,
        "ring_px_land": land_n,
        "ring_px_post": post_n,
        "noise_px": noise_n,
        "ring_centroid_off_m": round(9.0 if land_col is None else abs(land_col - cx) / ppm, 3),
        "ring_peak_luma_pre": round(pre_peak, 4),
        "ring_peak_luma_land": round(land_peak, 4),
        "ring_peak_luma_post": round(post_peak, 4),
        "ring_peak_luma_noise": round(noise_peak, 4),
        "t_pre_s": t.get("pre"),
        "t_land_s": t.get("land"),
        "t_post_s": t.get("post"),
        "t_land_run2_s": t.get("land-run2"),
        "flight_s": t.get("flight_s"),
        "flight_frames": t.get("flight_frames"),
        "pose_error_m": side.get("pose_error_m"),
        "rain": side.get("rain"),
        "window_px": int((win[0].stop - win[0].start) * (win[1].stop - win[1].start)),
        "crown_px": int((crown[0].stop - crown[0].start) * (crown[1].stop - crown[1].start)),
        "ring_band_px": int(bandm.sum()),
        "mark_px_x": cx,
        "mark_px_y": cy,
        "mark_px_per_m": ppm,
    }


# Which way to take each reported key over a set of runs: the pessimistic one in every case.
WORST = {
    "ring_step_land": min, "ring_step_post": min, "ring_px_band_land": min, "ring_px_band_post": min,
    "ring_inner_lift": max, "ring_crest_lift": min, "crown_top_m_post": max,
    "crown_contrast_post": min, "crown_p90_land": min, "crown_p90_post": min,
    "crown_water_p90_land": max, "crown_water_p90_post": max,
    "ring_px_pre": max, "ring_px_land": min, "ring_px_post": min, "noise_px": max,
    "ring_centroid_off_m": max, "ring_peak_luma_pre": max, "ring_peak_luma_noise": max,
    "ring_peak_luma_land": min, "ring_peak_luma_post": min,
}
CARRIED = ("ring_crest_r_m", "band_p90_land", "band_p90_land_noring", "band_p90_post", "band_p90_post_noring",
           "t_pre_s", "t_land_s", "t_post_s", "t_land_run2_s", "flight_s", "flight_frames", "window_px",
           "crown_px", "ring_band_px", "mark_px_x", "mark_px_y", "mark_px_per_m", "pose_error_m", "rain")


def worst_of(key, rows):
    """A bar key over a set of runs, taken the conservative way for a build that must CLEAR it."""
    _, lower = bar_of(key)
    return (max if lower else min)(r[key] for r in rows)


def best_of(key, rows):
    """The same key taken the other way: the baseline's BEST showing."""
    _, lower = bar_of(key)
    return (min if lower else max)(r[key] for r in rows)


def fill(out, mode, rows):
    """The aggregated block one mode yields, and whether it passes."""
    if mode == "base":
        for key in list(BARS) + list(GUARDS):
            out[key + "_base_best"] = best_of(key, rows)
        out["n_base_captures"] = len(rows)
        out["base_runs"] = [r["run"] for r in rows]
        ok = len(rows) >= 3 and not any(passes(k, out[k + "_base_best"]) for k in BARS)
        out["base_below_bars"] = ok
        return ok
    for key in list(BARS) + list(GUARDS):
        out[key] = worst_of(key, rows)
    for key, how in WORST.items():
        out[key] = how(r[key] for r in rows)
    for key in CARRIED:
        out[key] = rows[0][key]
    out["n_captures"] = len(rows)
    out["fixed_runs"] = [r["run"] for r in rows]
    ok = (len(rows) >= 3
          and all(passes(k, out[k]) for k in list(BARS) + list(GUARDS))
          # every regression bar cleared past the baseline's BEST showing, so clearing it cannot be capture noise
          and all(k + "_base_best" in out and not passes(k, out[k + "_base_best"]) for k in BARS)
          # r6's carried guards
          and out["ring_px_pre"] <= out["noise_px"]
          and out["ring_px_land"] >= max(1000, 3 * out["noise_px"])
          and out["ring_px_post"] >= 600
          and out["ring_centroid_off_m"] <= 0.5)
    out["pass"] = ok
    return ok


def bar_text():
    """Every bar and guard as the text a reader checks the table against."""
    out = {}
    for key in list(BARS) + list(GUARDS):
        bar, lower = bar_of(key)
        out[key] = ("<= %g" if lower else ">= %g") % bar
    return out


def table(rows, keys):
    for row in rows:
        bits = ["%s %s %s" % ("ok " if passes(k, row[k]) else "NO ", k, row[k]) for k in keys]
        print("  %-8s %s" % (row["run"], "  ".join(bits)))


def main():
    argv = sys.argv[1:]
    keys = list(BARS) + list(GUARDS)

    if not argv:
        # The unit's done-when: re-measure what probe.json names, re-check every bar, write nothing.
        path = os.path.join(HERE, "probe.json")
        if not os.path.exists(path):
            print("no probe.json beside this script: run --base and --fixed first")
            return 1
        saved = json.load(open(path, encoding="utf-8"))
        base_runs, fixed_runs = saved.get("base_runs") or [], saved.get("fixed_runs") or []
        if len(base_runs) < 3 or len(fixed_runs) < 3:
            print("probe.json names %d baseline and %d fixed runs; three of each are required"
                  % (len(base_runs), len(fixed_runs)))
            return 1
        out = {}
        base_rows = [measure(r) for r in base_runs]
        fixed_rows = [measure(r) for r in fixed_runs]
        base_ok = fill(out, "base", base_rows)
        fixed_ok = fill(out, "fixed", fixed_rows)
        print("bars and guards: " + json.dumps(bar_text()))
        print("baseline (the unchanged build; every run must be on the FAILING side of every bar):")
        table(base_rows, keys)
        print("fixed:")
        table(fixed_rows, keys)
        print(json.dumps({k: out[k] for k in keys + [k + "_base_best" for k in keys]
                          + ["base_below_bars", "pass"]}, indent=2))
        if not base_ok:
            print("FAIL: the baseline does not sit outside the bars")
        if not fixed_ok:
            print("FAIL: the fixed runs do not clear every bar and guard")
        return 0 if (base_ok and fixed_ok) else 1

    if argv[0] not in ("--base", "--fixed"):
        print("usage: probe.py [--base|--fixed <run dir> [<run dir> ...]]")
        return 2
    mode, runs = argv[0][2:], argv[1:]
    rows = [measure(r) for r in runs]

    path = os.path.join(HERE, "probe.json")
    out = json.load(open(path, encoding="utf-8")) if os.path.exists(path) else {}
    out["luma_threshold"] = THRESHOLD
    out["window_m"] = WINDOW_M
    out["crown_box_m"] = [CROWN_HALF_M, CROWN_UP_M, CROWN_DOWN_M]
    out["water_band_m"] = [BAND_HALF_M, BAND_UP_M, BAND_DOWN_M, BAND_CUT_M]
    out["bars"] = bar_text()
    out["noise_source"] = "second_throw"
    out.setdefault("runs", {})
    out["runs"][mode] = rows
    ok = fill(out, mode, rows)

    with open(path, "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    print(json.dumps({k: v for k, v in out.items() if k != "runs"}, indent=2))
    table(rows, keys)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
