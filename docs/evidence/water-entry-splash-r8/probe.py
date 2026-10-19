"""Measures the water entry's splash in the stills under this folder, and writes probe.json.

What is being proved. Harsh critic r1 scored r7's water entry 61/100. One thing it granted: the crown now reads -
crown_contrast at land + 0.05 s went from 0.03 to 0.30 and held over three runs - and r7's measurement of that is
kept here unchanged, because it is the proof the owner already accepted and a measurement is not re-tuned to suit a
later round. Three things it did not grant, and each becomes a number:

  1. "The ring is a static filled disc under the body." Two numbers answer it, and both are about SHAPE, not about
     how many pixels moved (r7's ring_only_px proved change, which a filled disc also does):
       centre_rim_ratio - the lit luma at the middle of the ring over the lit luma on its crest. A ring is hollow,
         so the middle must be darker: the bar is <= 0.6. A filled disc scores about 1.
       ring_growth - the ring's radius at land + 0.3 s over its radius at land + 0.05 s. A ring expands: the bar is
         >= 1.5. A static disc scores 1.
  2. "Only T1 was shot." The owner's standing rule is that a look never loses detail as the camera comes in, so the
     crown's contrast is now read at all five gym bands off ONE frozen world (T3 7.5, T2 16, T1 30, O120 120,
     O240 240, all yaw 21, aimY = the water): crown_contrast_t3 .. crown_contrast_o240, the bar o120 >= 0.09, and
     bands_monotone, true when T3 >= T2 >= T1 >= O120 >= O240 within band_tol.
  3. "The crown is about 60 px, with no fall-back shown." Two more:
       crown_height_m - how tall the crown card actually stands out of the water, in metres. Bar >= 1.3 m.
       crown_fall_ratio_06 - crown_contrast at land + 0.6 s over crown_contrast at land + 0.05 s. A splash is an
         event, not a decal: the bar is <= 0.5, half gone half a second later.

The stills. AgentScripts/water_entry_fling_r8.cs stages them and CaptureRig renders them, in Play in
GreyboxCorridor at night (the owner's target look), aimed at the ford, 1600 x 900, rain 0, the sky and water clocks
pinned and the sim frozen. A body is flung off the dry bank at (12, 128) into knee-deep water at (15, 130) with the
presentation clock running; fly.y is bisected until the flight is exactly 43 frames at 1/60 s. CaptureRig.Mark
named (15, 130) before the shots, so EVERY still's sidecar carries that point's own projection for that still's
camera (mark_px_x/y and mark_px_per_m) - which is what lets the same patch of world be measured at five different
zooms. Each run writes its own run-N/ folder:

  plate.png                 the ford with no throw at all, for the record
  pre.png                   land - 0.2 s: the body is still in the air
  land.png                  land + 0.05 s, T1 (zoom 30, yaw 21, pitch 25)
  t3/t2/o120/o240.png       the SAME frozen land + 0.05 s world from the other four bands
  post.png                  land + 0.3 s
  p06.png                   land + 0.6 s
  land-run2.png             land + 0.05 s of a SECOND throw with the same seed, the scene put back to before run 1
  <name>-noring.png         that frozen moment with the ring slots parked but the CARDS kept
  <name>-nosplash.png       that frozen moment with the water entry lifted out entirely

Why each moment has its own reference instead of a plate taken earlier: the water's foam and lapping are animated
from the water clock in TWWater.hlsl, so a still taken a second after a plate differs from it across the whole
water sheet whether anything landed in it or not. A reference rendered at the SAME frozen instant with the splash
lifted out has the same foam, the same body, the same shadows and the same sky.

Three masks come out of each moment's three pictures, and every new number is read inside one of them:
  the splash mask  |lit - nosplash| > 0.02   the ring and the cards together (r7's counting window)
  the ring mask    |lit - noring|   > 0.02   the ring alone
  the card mask    |noring - nosplash| > 0.02   the crown and wings alone

Two pieces of projection, both needed and both easy to get wrong:
  - The ring lies FLAT on the water, so its circle reaches the camera as an ellipse squashed along the view by
    sin(pitch). Measuring a radius or an annulus without that squash would put the "crest" ring half on the crest
    and half on open water. Every ring measurement here is in that ellipse's coordinates.
  - The crown stands UP, so a metre of crown covers mark_px_per_m * cos(pitch) pixels, not mark_px_per_m: that
    number converts a metre lying on the GROUND. Using the ground number would report a 1.3 m crown as 1.2 m.
  - The ring is measured only in the rows at or below mark_px_y - the near half of the ellipse. The crown card is
    anchored on the water and stands in the rows above, so the near half is the half no card can cover.

The baseline, and why it is three runs. The owner's rule for this round, as in r7: capture the CURRENT build three
times BEFORE changing anything, and set each bar outside that spread. --base writes <key>_base_max / _base_min and
base_below_bars. --fixed takes each key the pessimistic way over at least three runs - the min where higher is
better, the max where lower is better - and writes pass.

Two rules about bars, both the owner's and neither negotiable:
  - A bar is never loosened to let a result through.
  - A bar that sits INSIDE the unchanged build's three-run spread - cleared on some runs and missed on others -
    does not on its own tell a change from luck, so for such a key the fixed build must clear the bar AND beat the
    baseline's best run. A bar every baseline run clears is a different case: it is the critic's claim rebutted
    with a number, and the README says so plainly instead of a change being invented to satisfy it. The bar still
    stays, and base_already_clears names those keys. --base exits 0 either way: a baseline is a measurement, not a
    gate, and a round must not be blocked by its own build turning out better than the critic said.

band_tol is measured, not chosen: the largest spread any one band's crown_contrast shows over the three baseline
runs. The ladder check allows exactly that much slack and no more.

Run:
  python docs/evidence/water-entry-splash-r8/probe.py --base  <run dir> <run dir> <run dir>   # unchanged build
  python docs/evidence/water-entry-splash-r8/probe.py --fixed <run dir> <run dir> <run dir>   # exit 0 when clear

A run dir may be a folder beside this file or any path (the captures themselves live in the git-ignored
trench-warfare-3d/Captures/water-entry-r8/; only the committed JPGs and sidecars land here).
"""
import json
import math
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 0.02       # luma move that counts as a changed pixel; r5's, never raised to make a bar pass
WINDOW_M = 3.0         # half-width and half-height of the counting window on the landing point
CROWN_HALF_M = 1.4     # the crown box's half-width across the view
CROWN_UP_M = 2.8       # how far above the mark's row the crown box reaches
CROWN_DOWN_M = 0.2     # and how far below it
CROWN_WIDE_M = 4.0     # the fall-back half-width and height for a band too far out to resolve the normal box
CROWN_MIN_PX = 25      # under this many pixels a p90 is noise, so that band's box is widened to CROWN_WIDE_M
HEIGHT_HALF_M = 0.6    # how far either side of the mark the crown's top is looked for
CENTRE_FRAC = 0.35     # the middle of the ring: inside 0.35 of the measured crest radius
RIM_LO, RIM_HI = 0.85, 1.15   # the crest: the annulus around the measured radius
RADIUS_PCTL = 95       # the ring's radius is the 95th percentile of its pixels' distance, not the furthest one

BANDS = ("t3", "t2", "t1", "o120", "o240")      # closest in to furthest out; t1's stills are named "land"
BAND_FILE = {"t1": "land"}                      # T1 is the landing moment's own picture

# Higher is better: the bar is a floor, the worst of a set is its min, the baseline's best is its max.
BARS_HIGH = {
    "ring_growth": 1.5,
    "o120_crown_contrast": 0.09,
    "crown_height_m": 1.3,
    "ring_only_px": 300,
    "crown_contrast_land": 0.15,
    "crown_contrast_post": 0.09,
}
# Lower is better: the bar is a ceiling, the worst of a set is its max, the baseline's best is its min.
BARS_LOW = {
    "centre_rim_ratio": 0.6,
    "crown_fall_ratio_06": 0.5,
}
# The five the owner named for this round; probe.json carries each as a per-run list so the spread stays visible.
OWNER_KEYS = ("centre_rim_ratio", "ring_growth", "o120_crown_contrast", "crown_height_m", "crown_fall_ratio_06")


def runpath(run, *parts):
    """A run dir named beside this file, or given as a path of its own."""
    base = run if os.path.isabs(run) or os.path.isdir(run) else os.path.join(HERE, run)
    return os.path.join(base, *parts)


def luma(run, name):
    """Rec.709 luma in 0..1 as a 2-D array, read from the image's top-left the way the sidecar counts pixels."""
    im = Image.open(runpath(run, name)).convert("RGB")
    a = np.asarray(im, dtype=np.float64) / 255.0
    return 0.2126 * a[:, :, 0] + 0.7152 * a[:, :, 1] + 0.0722 * a[:, :, 2]


def side(run, name):
    return json.load(open(runpath(run, name), encoding="utf-8"))


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
    path = runpath(run, "timings.txt")
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


def grids(shape, cx, cy):
    """Column and row offsets from the mark, for every pixel of the frame."""
    h, w = shape
    dx = np.arange(w, dtype=np.float64)[None, :] - cx
    dy = np.arange(h, dtype=np.float64)[:, None] - cy
    return dx, dy


def ring_radius_m(lit, noring, cx, cy, ppm, win):
    """The ring's radius in metres, read across the view in the near half of the frame.

    Only the rows at or below the mark are used: the crown card is anchored on the water and stands in the rows
    above it, so the near half of the ellipse is the half no card can hide. p95 of |column - mark| rather than the
    furthest pixel, so one stray lit fleck cannot set the radius.
    """
    d = np.abs(lit - noring)
    hit = d > THRESHOLD
    keep = np.zeros_like(hit)
    keep[win] = True
    rows = np.arange(hit.shape[0])[:, None] >= cy
    hit = hit & keep & rows
    n = int(hit.sum())
    if n < 20:
        return 0.0, n
    cols = np.nonzero(hit)[1]
    return float(np.percentile(np.abs(cols - cx), RADIUS_PCTL)) / ppm, n


def centre_rim(lit, noring, nosplash, cx, cy, ppm, pitch_deg, radius_m):
    """median(lit in the middle of the ring) / median(lit on its crest). A hollow ring scores well under 1.

    The ring is a circle lying on the water, so it reaches the camera as an ellipse squashed by sin(pitch) along
    the view. Both regions are taken in that ellipse's own coordinates, in the rows at or below the mark, and with
    every pixel the cards cover thrown out - otherwise the crown standing in the middle would fill the hole the
    measurement is looking for and a hollow ring would score as a filled disc.
    """
    rpx = radius_m * ppm
    if rpx < 4:
        return None
    sp = max(0.05, math.sin(math.radians(pitch_deg)))
    dx, dy = grids(lit.shape, cx, cy)
    e = np.sqrt((dx / rpx) ** 2 + (dy / (rpx * sp)) ** 2)
    near = (np.arange(lit.shape[0])[:, None] >= cy) & np.ones_like(dx, dtype=bool)
    cards = np.abs(noring - nosplash) > THRESHOLD
    ok = near & ~cards
    centre = ok & (e <= CENTRE_FRAC)
    rim = ok & (e >= RIM_LO) & (e <= RIM_HI)
    if int(centre.sum()) < 20 or int(rim.sum()) < 20:
        return None
    cm, rm = float(np.median(lit[centre])), float(np.median(lit[rim]))
    if rm <= 1e-6:
        return None
    return cm / rm, int(centre.sum()), int(rim.sum()), cm, rm


def crown_height(mask, cx, cy, ppm, pitch_deg, win):
    """How far the crown's top stands above the water line, in metres.

    An upright metre covers ppm * cos(pitch) pixels; ppm itself converts a metre lying on the ground, and using it
    here would under-report a standing crown. p02 of the mask's rows, not the single topmost pixel, so one stray
    fleck cannot inflate the height.

    The mask is cut to the 6 m counting window before anything is read off it. Measured without that cut, run 1 of
    the unchanged build reported a 5.5 m crown: the card mask is a difference over the WHOLE frame, so a card's
    faint glow in the sky and the rim light it throws on the far bank are both in it, and a percentile over the
    frame's full height answers a question about the sky rather than about the splash. The window is the probe's own
    region for exactly this - 3 m of height above the water line and 3 m below - so a height read inside it is a
    height of the thing standing in the water.
    """
    cols = np.abs(np.arange(mask.shape[1], dtype=np.float64)[None, :] - cx) <= HEIGHT_HALF_M * ppm
    above = np.arange(mask.shape[0])[:, None] <= cy
    inwin = np.zeros(mask.shape, dtype=bool)
    inwin[win] = True
    sel = mask & cols & above & inwin
    n = int(sel.sum())
    if n < 20:
        return 0.0, n
    top = float(np.percentile(np.nonzero(sel)[0], 2))
    return (cy - top) / (ppm * max(0.05, math.cos(math.radians(pitch_deg)))), n


def crown_contrast(lit, ref, cx, cy, ppm):
    """r7's crown value step, in whatever band's pixels it is handed: p90(lit box) - p90(the same box, splash out).

    The box is 1.4 m either side of the mark and from 2.8 m above its row down to 0.2 m below, sized by THAT
    band's own mark_px_per_m, so every band measures the same patch of world. A band so far out that the box is
    under CROWN_MIN_PX pixels gets a wider one: a wider box can only LOWER its p90, which is the safe direction
    for the ladder - it can never let a far band beat a near one by accident.
    """
    b = box(lit.shape, cx, cy, CROWN_HALF_M * ppm, CROWN_HALF_M * ppm, CROWN_UP_M * ppm, CROWN_DOWN_M * ppm)
    px = (b[0].stop - b[0].start) * (b[1].stop - b[1].start)
    wide = px < CROWN_MIN_PX
    if wide:
        b = box(lit.shape, cx, cy, CROWN_WIDE_M * ppm, CROWN_WIDE_M * ppm, CROWN_WIDE_M * ppm, CROWN_DOWN_M * ppm)
        px = (b[0].stop - b[0].start) * (b[1].stop - b[1].start)
    return float(np.percentile(lit[b], 90) - np.percentile(ref[b], 90)), px, wide


def measure(run):
    """Every number one capture run yields: r7's, unchanged, and r8's five new ones."""
    s = side(run, "land.json")
    cx, cy, ppm = s["mark_px_x"], s["mark_px_y"], s["mark_px_per_m"]
    pitch = s.get("cam_pitch", s.get("pitch", 25.0))
    if pitch > 180.0:
        pitch -= 360.0

    pre, pre_ref = luma(run, "pre.png"), luma(run, "pre-nosplash.png")
    land, land_ref = luma(run, "land.png"), luma(run, "land-nosplash.png")
    land_noring = luma(run, "land-noring.png")
    land_run2 = luma(run, "land-run2.png")
    post, post_ref = luma(run, "post.png"), luma(run, "post-nosplash.png")
    post_noring = luma(run, "post-noring.png")
    p06, p06_ref = luma(run, "p06.png"), luma(run, "p06-nosplash.png")

    win = box(land.shape, cx, cy, WINDOW_M * ppm, WINDOW_M * ppm, WINDOW_M * ppm, WINDOW_M * ppm)
    crown = box(land.shape, cx, cy, CROWN_HALF_M * ppm, CROWN_HALF_M * ppm, CROWN_UP_M * ppm, CROWN_DOWN_M * ppm)

    # ---- r7's numbers, kept exactly as they were ----------------------------------------------------------
    pre_n, _, pre_peak = changed(pre, pre_ref, win)
    land_n, land_col, land_peak = changed(land, land_ref, win)
    noise_n, _, noise_peak = changed(land, land_run2, win)
    post_n, _, post_peak = changed(post, post_ref, win)
    only_land, _, only_land_peak = changed(land, land_noring, win)
    only_post, _, only_post_peak = changed(post, post_noring, win)

    def p90(img):
        return float(np.percentile(img[crown], 90))

    # ---- r8, fix 1: the ring has a hole and it opens -------------------------------------------------------
    r_land, ring_px_mask_land = ring_radius_m(land, land_noring, cx, cy, ppm, win)
    r_post, ring_px_mask_post = ring_radius_m(post, post_noring, cx, cy, ppm, win)
    growth = (r_post / r_land) if r_land > 1e-6 else 0.0

    cr_land = centre_rim(land, land_noring, land_ref, cx, cy, ppm, pitch, r_land)
    cr_post = centre_rim(post, post_noring, post_ref, cx, cy, ppm, pitch, r_post)
    ratios = [c[0] for c in (cr_land, cr_post) if c]
    centre_rim_ratio = max(ratios) if ratios else 9.0   # unmeasurable is a failure, not a free pass

    # ---- r8, fix 3: the crown is tall and it falls back ----------------------------------------------------
    # The card mask is the crown and wings alone. p06 has no -noring twin (nothing at that moment turns on
    # whether the ring is in or out), so there the splash mask restricted to the rows ABOVE the water line is
    # used instead: the ring lies at or below that line, so above it the splash mask IS the cards.
    card_land = np.abs(land_noring - land_ref) > THRESHOLD
    card_post = np.abs(post_noring - post_ref) > THRESHOLD
    card_p06 = np.abs(p06 - p06_ref) > THRESHOLD
    h_land, hn_land = crown_height(card_land, cx, cy, ppm, pitch, win)
    h_post, hn_post = crown_height(card_post, cx, cy, ppm, pitch, win)
    h_p06, hn_p06 = crown_height(card_p06, cx, cy, ppm, pitch, win)
    crown_h = max(h_land, h_post, h_p06)

    c_land = round(p90(land) - p90(land_ref), 4)
    c_post = round(p90(post) - p90(post_ref), 4)
    c_p06 = round(p90(p06) - p90(p06_ref), 4)
    fall06 = (c_p06 / c_land) if c_land > 1e-6 else 9.0

    # ---- r8, fix 2: every band, off the one frozen world ----------------------------------------------------
    bands, band_px, band_wide = {}, {}, []
    for b in BANDS:
        stem = BAND_FILE.get(b, b)
        bs = side(run, stem + ".json")
        bl, br = luma(run, stem + ".png"), luma(run, stem + "-nosplash.png")
        v, px, wide = crown_contrast(bl, br, bs["mark_px_x"], bs["mark_px_y"], bs["mark_px_per_m"])
        bands["crown_contrast_" + b] = round(v, 4)
        band_px["crown_px_" + b] = px
        if wide:
            band_wide.append(b)

    t = timings(run)
    out = {
        "run": run,
        # the five the owner named
        "centre_rim_ratio": round(centre_rim_ratio, 4),
        "ring_growth": round(growth, 4),
        "o120_crown_contrast": bands["crown_contrast_o120"],
        "crown_height_m": round(crown_h, 4),
        "crown_fall_ratio_06": round(fall06, 4),
        # how each of them was arrived at
        "ring_radius_m_land": round(r_land, 4),
        "ring_radius_m_post": round(r_post, 4),
        "ring_mask_px_land": ring_px_mask_land,
        "ring_mask_px_post": ring_px_mask_post,
        "centre_rim_ratio_land": round(cr_land[0], 4) if cr_land else None,
        "centre_rim_ratio_post": round(cr_post[0], 4) if cr_post else None,
        "centre_px_land": cr_land[1] if cr_land else 0,
        "rim_px_land": cr_land[2] if cr_land else 0,
        "centre_luma_land": round(cr_land[3], 4) if cr_land else None,
        "rim_luma_land": round(cr_land[4], 4) if cr_land else None,
        "crown_height_m_land": round(h_land, 4),
        "crown_height_m_post": round(h_post, 4),
        "crown_height_m_p06": round(h_p06, 4),
        "crown_height_px_land": hn_land,
        "crown_height_px_post": hn_post,
        "crown_height_px_p06": hn_p06,
        "crown_contrast_p06": c_p06,
        "cam_pitch_deg": round(pitch, 3),
        # r7's, unchanged
        "ring_only_px": min(only_land, only_post),
        "crown_contrast_land": c_land,
        "crown_contrast_post": c_post,
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
        "t_p06_s": t.get("p06"),
        "t_land_run2_s": t.get("land-run2"),
        "flight_s": t.get("flight_s"),
        "flight_frames": t.get("flight_frames"),
        "pose_error_m": s.get("pose_error_m"),
        "rain": s.get("rain"),
        "window_px": int((win[0].stop - win[0].start) * (win[1].stop - win[1].start)),
        "crown_px": int((crown[0].stop - crown[0].start) * (crown[1].stop - crown[1].start)),
        "mark_px_x": cx,
        "mark_px_y": cy,
        "mark_px_per_m": ppm,
        "crown_box_widened": band_wide,
    }
    out.update(bands)
    out.update(band_px)
    return out


def monotone(row, tol):
    """True when the crown reads at least as strongly at every band as at the one zoomed out from it."""
    vals = [row["crown_contrast_" + b] for b in BANDS]
    return all(vals[i] >= vals[i + 1] - tol for i in range(len(vals) - 1))


# r6/r7's own keys, and which way to take each over a set of runs: the pessimistic one in every case.
WORST = {
    "ring_px_pre": max, "ring_px_land": min, "ring_px_post": min, "noise_px": max,
    "ring_centroid_off_m": max, "ring_peak_luma_pre": max, "ring_peak_luma_noise": max,
    "ring_peak_luma_land": min, "ring_peak_luma_post": min,
    "ring_peak_luma_only_land": min, "ring_peak_luma_only_post": min,
    "ring_only_px_land": min, "ring_only_px_post": min,
    "crown_p90_land": min, "crown_p90_post": min,
    "crown_water_p90_land": max, "crown_water_p90_post": max,
    "ring_radius_m_land": max, "ring_radius_m_post": min,
    "crown_contrast_p06": max,
}
CARRIED = ("t_pre_s", "t_land_s", "t_post_s", "t_p06_s", "t_land_run2_s", "flight_s", "flight_frames",
           "window_px", "crown_px", "mark_px_x", "mark_px_y", "mark_px_per_m", "pose_error_m", "rain",
           "cam_pitch_deg", "crown_box_widened")


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
    out["bars_high"] = BARS_HIGH
    out["bars_low"] = BARS_LOW
    out["bands"] = list(BANDS)
    out["noise_source"] = "second_throw"
    out.setdefault("runs", {})

    if mode == "--base":
        for k in BARS_HIGH:
            out[k + "_base_max"] = max(r[k] for r in rows)
            out[k + "_base_min"] = min(r[k] for r in rows)
        for k in BARS_LOW:
            out[k + "_base_max"] = max(r[k] for r in rows)
            out[k + "_base_min"] = min(r[k] for r in rows)
        # The unchanged build's spread per band is the ladder's tolerance: measured here, not chosen.
        out["band_tol"] = round(max(max(r["crown_contrast_" + b] for r in rows)
                                    - min(r["crown_contrast_" + b] for r in rows) for b in BANDS), 4)
        # The owner's five, under base_ names so the done-when one-liner can never be satisfied by a baseline.
        for k in OWNER_KEYS:
            out["base_" + k] = [r[k] for r in rows]
        out["base_crown_contrast_by_band"] = {b: [r["crown_contrast_" + b] for r in rows] for b in BANDS}
        out["base_bands_monotone"] = [monotone(r, out["band_tol"]) for r in rows]
        out["n_base_captures"] = len(rows)
        out["base_runs"] = runs
        out["runs"]["base"] = rows
        already = ([k for k, v in BARS_HIGH.items() if out[k + "_base_min"] >= v]
                   + [k for k, v in BARS_LOW.items() if out[k + "_base_max"] <= v])
        out["base_already_clears"] = already
        out["base_below_bars"] = len(already) == 0
        # A baseline is a measurement, not a gate: it exits 0 once three runs have been read. A key the unchanged
        # build already clears is named in base_already_clears, its bar stays, and the fix must also beat the
        # baseline's best run on it.
        ok = len(rows) >= 3
    else:
        tol = out.get("band_tol", 0.0)
        for k in BARS_HIGH:
            out[k + "_worst"] = min(r[k] for r in rows)
        for k in BARS_LOW:
            out[k + "_worst"] = max(r[k] for r in rows)
        # Every owner key as a per-run list, so the spread is on the page and not summarised away.
        for k in OWNER_KEYS:
            out[k] = [r[k] for r in rows]
        out["crown_contrast_by_band"] = {b: [r["crown_contrast_" + b] for r in rows] for b in BANDS}
        out["bands_monotone"] = all(monotone(r, tol) for r in rows)
        out["band_tol_used"] = tol
        for key, how in WORST.items():
            out[key] = how(r[key] for r in rows)
        for key in CARRIED:
            out[key] = rows[0][key]
        out["n_captures"] = len(rows)
        out["fixed_runs"] = runs
        out["runs"]["fixed"] = rows

        def beats_base(k, bar, high):
            """Does this key also have to beat the baseline's best run, and if so does it?

            The extra condition is for a bar that sits INSIDE the unchanged build's spread - some baseline runs
            clear it and some do not - because there the bar alone does not tell a change from luck. A bar the
            baseline clears on every one of its three runs is a different thing: it is the critic's claim rebutted
            with a number, the README says so, the bar stays, and nothing has to be improved to pass it. A bar no
            baseline run comes near is already outside their spread and needs nothing added either.
            """
            lo, hi = out.get(k + "_base_min"), out.get(k + "_base_max")
            if lo is None or hi is None:
                return True
            straddles = (lo < bar <= hi) if high else (lo <= bar < hi)
            if not straddles:
                return True
            best = hi if high else lo
            return out[k + "_worst"] > best if high else out[k + "_worst"] < best

        ok = (len(rows) >= 3
              and all(out[k + "_worst"] >= v and beats_base(k, v, True) for k, v in BARS_HIGH.items())
              and all(out[k + "_worst"] <= v and beats_base(k, v, False) for k, v in BARS_LOW.items())
              and out["bands_monotone"]
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
        print("  %-10s centre/rim %-7s growth %-7s height_m %-7s fall06 %-7s o120 %-7s bands %s"
              % (os.path.basename(row["run"].rstrip("/\\")), row["centre_rim_ratio"], row["ring_growth"],
                 row["crown_height_m"], row["crown_fall_ratio_06"], row["o120_crown_contrast"],
                 " ".join(str(row["crown_contrast_" + b]) for b in BANDS)))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
