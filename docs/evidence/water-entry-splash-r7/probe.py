"""Measures the water entry's splash in the stills beside this file, and writes probe.json.

What is being proved. r6 photographed a crown, two wings and a ring on the spec moments and measured its own noise
from a second live throw; the harsh critic still scored it 56/100, on two counts that this round answers in pixels:

  1. "The ring adds nothing." r6's ring_only_px was 0 and its peak luma move 0.0191, just under the 0.02 threshold,
     so the ring's own contribution to the landing frame was indistinguishable from nothing. The owner's bar is
     ring_only_px >= 300, and it is the MIN of two moments (land and post), so a ring that only shows while the
     crown card happens to be out of the way cannot pass on one lucky frame.
  2. "The crown is blue-white on blue water, the same hue as the foam flecks, and nearly gone at +0.3 s." A count of
     changed pixels at luma 0.02 proves CHANGE, not that a player sees it. So the crown is now measured as a VALUE
     STEP over the water it stands in: crown_contrast_land = the splash region's p90 luma minus the p90 of the SAME
     region at the SAME frozen instant with the splash lifted out. The bars are the owner's: >= 0.15 at land + 0.05
     s and >= 0.09 still at land + 0.3 s.

Measured on r6's committed stills, the starting point of this round is crown_contrast_land 0.0335,
crown_contrast_post 0.0237 and ring_only_px 0 - about 4.5x short on the crown and nothing at all on the ring.

The stills. AgentScripts/water_entry_fling.cs stages them and CaptureRig renders them, in Play in GreyboxCorridor at
night (the owner's target look), camera T1 - zoom 30, yaw 21, pitch 25 - aimed at the ford, 1600 x 900, rain 0, the
sky and water clocks pinned and the sim frozen. A body is flung off the dry bank at (12, 128) into knee-deep water
at (15, 130) with the presentation clock running; fly.y is bisected until the flight is exactly 43 frames at 1/60 s.
CaptureRig.Mark named (15, 130) before the shots, so every sidecar carries that point's projection (mark_px_x/y and
mark_px_per_m) and pixels can be turned into metres. Each run writes its own `run-N/` folder beside this file:

  plate.png            the ford with no throw at all, for the record
  pre.png              land - 0.2 s: the body is still in the air
  land.png             land + 0.05 s
  post.png             land + 0.3 s
  land-run2.png        land + 0.05 s of a SECOND throw with the same seed, the scene put back to before run 1: its
                       difference from land.png is the splash's own repeatability, the noise floor, measured live
  land-noring.png      the frozen landing with the ring slots parked but the cards KEPT
  post-noring.png      the same at land + 0.3 s, which is what lets ring_only_px be the min of two moments
  <name>-nosplash.png  pre, land and post re-rendered at that SAME frozen instant with the water entry lifted out -
                       WaterRings' ring array parked and FlipbookFx's card list emptied - and nothing else touched

Why each moment has its own reference instead of a plate taken earlier: the water's foam and lapping are animated
from the water clock in TWWater.hlsl, so a still taken a second after the plate differs from it across the whole
water sheet (about 1,600 changed pixels in the disc at the landing point whether anything landed there or not). A
reference rendered at the same frozen instant with the splash lifted out has the same foam, the same body, the same
shadows and the same sky, so the difference between the pair is the water entry and nothing else.

The two regions.
  The 6 m window (r6's, unchanged) counts changed pixels: within 3 m across the view of the mark, and from 3 m of
  height above the mark's pixel row down to 3 m below it. A water entry is a ring lying flat ON the surface and a
  crown standing UP out of it, so the window has to hold both.
  The crown box is where the crown stands: mark_px_x +/- 1.4 m across the view, and the rows from 2.8 m of height
  above the mark down to 0.2 m below it. That is about 3,100 px at this projection, which the crown card itself
  fills, so a p90 taken in it sits inside the crown and not on its edge. p90 is coverage-limited by design: a bright
  but thin crown misses the bar, which is the point - the owner asked for a crown a player sees, not a hot line.

The centroid is measured ACROSS the view only (the pixel column against mark_px_x). mark_px_per_m converts ground
metres; a splash stands up out of the water and its height in pixels is not a distance along the ground, so turning
a vertical offset into metres with the same number would report a splash exactly on its mark as a metre away.

A pixel counts as changed when its Rec.709 luma (0..1, the scale CaptureRig's own numbers use) moves by more than
0.02. The threshold is r5's, unchanged: it is never raised to make a bar pass.

The baseline, and why it is three runs. The owner's rule for this round: capture the CURRENT build three times
BEFORE changing anything, and record each bar key's max over those three as <key>_base_max. Each bar must sit above
that spread, and the fixed build must clear it with at least three runs of its own, each key taken as the MIN over
them. Max for the baseline and min for the fix is the conservative pair in both directions: the old build gets its
best three-run showing, the new one its worst.

What it writes, per bar key: the key itself, <key>_base_max, and a `runs` block with every run's own three numbers
so the spread is visible rather than summarised. It also keeps r6's keys (ring_px_pre / _land / _post, noise_px,
ring_centroid_off_m, the peak lumas and the achieved moments), reported for every run and aggregated the
conservative way. Writing the fixed block keeps the baseline block already in probe.json.

Run:
  python docs/evidence/water-entry-splash-r7/probe.py --base  run-1 run-2 run-3   # the unchanged build, first
  python docs/evidence/water-entry-splash-r7/probe.py --fixed run-4 run-5 run-6   # the fix; exit 0 when it clears
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

BARS = {"ring_only_px": 300, "crown_contrast_land": 0.15, "crown_contrast_post": 0.09}


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

    pre, pre_ref = luma(run, "pre.png"), luma(run, "pre-nosplash.png")
    land, land_ref = luma(run, "land.png"), luma(run, "land-nosplash.png")
    land_noring = luma(run, "land-noring.png")
    land_run2 = luma(run, "land-run2.png")
    post, post_ref = luma(run, "post.png"), luma(run, "post-nosplash.png")
    post_noring = luma(run, "post-noring.png")

    win = box(land.shape, cx, cy, WINDOW_M * ppm, WINDOW_M * ppm, WINDOW_M * ppm, WINDOW_M * ppm)
    crown = box(land.shape, cx, cy, CROWN_HALF_M * ppm, CROWN_HALF_M * ppm, CROWN_UP_M * ppm, CROWN_DOWN_M * ppm)

    pre_n, _, pre_peak = changed(pre, pre_ref, win)
    land_n, land_col, land_peak = changed(land, land_ref, win)
    noise_n, _, noise_peak = changed(land, land_run2, win)
    post_n, _, post_peak = changed(post, post_ref, win)
    only_land, _, only_land_peak = changed(land, land_noring, win)
    only_post, _, only_post_peak = changed(post, post_noring, win)

    def p90(img):
        return float(np.percentile(img[crown], 90))

    t = timings(run)
    return {
        "run": run,
        "ring_only_px": min(only_land, only_post),
        "crown_contrast_land": round(p90(land) - p90(land_ref), 4),
        "crown_contrast_post": round(p90(post) - p90(post_ref), 4),
        "ring_only_px_land": only_land,
        "ring_only_px_post": only_post,
        "ring_px_pre": pre_n,
        "ring_px_land": land_n,
        "ring_px_post": post_n,
        "noise_px": noise_n,
        "ring_centroid_off_m": round(9.0 if land_col is None else abs(land_col - cx) / ppm, 3),
        "ring_peak_luma_pre": round(pre_peak, 4),
        "ring_peak_luma_land": round(land_peak, 4),
        "ring_peak_luma_post": round(post_peak, 4),
        "ring_peak_luma_noise": round(noise_peak, 4),
        "ring_peak_luma_only_land": round(only_land_peak, 4),
        "ring_peak_luma_only_post": round(only_post_peak, 4),
        "crown_p90_land": round(p90(land), 4),
        "crown_p90_post": round(p90(post), 4),
        "crown_water_p90_land": round(p90(land_ref), 4),
        "crown_water_p90_post": round(p90(post_ref), 4),
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
        "mark_px_x": cx,
        "mark_px_y": cy,
        "mark_px_per_m": ppm,
    }


# r6's own keys, and which way to take each over a set of runs: the pessimistic one in every case.
WORST = {
    "ring_px_pre": max, "ring_px_land": min, "ring_px_post": min, "noise_px": max,
    "ring_centroid_off_m": max, "ring_peak_luma_pre": max, "ring_peak_luma_noise": max,
    "ring_peak_luma_land": min, "ring_peak_luma_post": min,
    "ring_peak_luma_only_land": min, "ring_peak_luma_only_post": min,
    "ring_only_px_land": min, "ring_only_px_post": min,
    "crown_p90_land": min, "crown_p90_post": min,
    "crown_water_p90_land": max, "crown_water_p90_post": max,
}
CARRIED = ("t_pre_s", "t_land_s", "t_post_s", "t_land_run2_s", "flight_s", "flight_frames",
           "window_px", "crown_px", "mark_px_x", "mark_px_y", "mark_px_per_m", "pose_error_m", "rain")


def main():
    if len(sys.argv) < 3 or sys.argv[1] not in ("--base", "--fixed"):
        print("usage: probe.py --base|--fixed <run dir> [<run dir> ...]")
        return 2
    mode, runs = sys.argv[1], sys.argv[2:]
    rows = [measure(r) for r in runs]

    path = os.path.join(HERE, "probe.json")
    out = json.load(open(path, encoding="utf-8")) if os.path.exists(path) else {}
    out["luma_threshold"] = THRESHOLD
    out["window_m"] = WINDOW_M
    out["crown_box_m"] = [CROWN_HALF_M, CROWN_UP_M, CROWN_DOWN_M]
    out["bars"] = BARS
    out["noise_source"] = "second_throw"
    out.setdefault("runs", {})

    if mode == "--base":
        for key in BARS:
            out[key + "_base_max"] = max(r[key] for r in rows)
        out["n_base_captures"] = len(rows)
        out["base_runs"] = runs
        out["runs"]["base"] = rows
        ok = len(rows) >= 3 and all(out[k + "_base_max"] < v for k, v in BARS.items())
        out["base_below_bars"] = ok
    else:
        for key in BARS:
            out[key] = min(r[key] for r in rows)
        for key, how in WORST.items():
            out[key] = how(r[key] for r in rows)
        for key in CARRIED:
            out[key] = rows[0][key]
        out["n_captures"] = len(rows)
        out["fixed_runs"] = runs
        out["runs"]["fixed"] = rows
        ok = (len(rows) >= 3
              and all(out[k] >= v for k, v in BARS.items())
              and all(k + "_base_max" in out and out[k + "_base_max"] < out[k] for k in BARS)
              and out["ring_px_pre"] <= out["noise_px"]
              and out["ring_px_land"] >= max(1000, 3 * out["noise_px"])
              and out["ring_px_post"] >= 600
              and out["ring_centroid_off_m"] <= 0.5)
        out["pass"] = ok

    with open(path, "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    print(json.dumps({k: v for k, v in out.items() if k != "runs"}, indent=2))
    for row in rows:
        print("  %-8s ring_only_px %-6s crown_land %-8s crown_post %s"
              % (row["run"], row["ring_only_px"], row["crown_contrast_land"], row["crown_contrast_post"]))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
