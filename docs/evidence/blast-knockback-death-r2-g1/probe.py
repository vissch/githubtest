"""Measures the blast knockback throw PER BODY from the captured stills, not typed.

r2's "post" set turned out to be nine renamed "pre" stills (`git show --stat 19f946794d`: Bin unchanged for
blast1/blast2/calm1-3/calm2's pair/calm3), so its `blast_median_m` ([1.181, 2.142, 1.212]) and its
`BASELINE_THROW_M` were the same numbers measured twice, and blast3's nearest body (0.124 m, ~4 px) never moved.
This set (`g1`) is a genuine reshoot of all 18 stills on the same tree, from one pinned camera, and it replaces r2's
baseline-in-the-file with a measured bar: the spread of three calm captures of THIS build. See frames.txt for what
each still is.

g1's first cut stood the row along x and threw it along x too: `DeathLab.Shell` takes a point only, so
`Explosion.Dir` is zero and `AnimationController.Death.cs:219` throws every man straight away from the burst. With
the shell on the row's own middle, six collinear men flew along their own line, the crater covered the ground the
measurement needs, and no matcher could read it (a shifted row matches onto itself). The fix is geometry: the
harness (`BlastThrowStills.cs`, `ShellOffsetX` 5 m) bursts the shell 5 m to -x of a row standing along z, so every
recorded throw carries a large +x part (2.1 to 7.4 m on this build, asserted by the test) and the crater sits off
the row line.

The bar is one rule, the same for blast and calm, with NO assignment between before and after blobs: for each of
the six standing men at `p_i` the run's value is `d_i`, the distance in world metres from `p_i` to the NEAREST
candidate body in the +3 s still. An extra blob can only shrink a `d_i`, so it can only ever make a blast run
harder to pass, and no permutation of a collinear row can fool it, whatever axis the throw takes.

What it reads (shot by Tests/Stills/BlastThrowStills.cs, `g1_` prefix):
  g1_blast1..3_{m1_0,p0_5,p3_0}.png  a row of six men 2.5 m apart along z, straight down (pitch 89, zoom 20), at
                                      -1 s, +0.5 s and +3 s around a shell 5 m to -x of the row line;
  g1_calm1..3_{...}.png              the same three moments with no shell: the noise floor, what a standing row
                                      drifts by on its own.
Each PNG has a .json sidecar beside it (CaptureRig) carrying `frame`, `fallen`, `zoom`, `pitch`, `yaw`, `cam_x`,
`cam_y`, `cam_z`, `cam_pitch`, `cam_yaw`, `pose_error_m` and `contrast_median`.

Method, per run, on the (-1 s, +3 s) pair:
  1. background = the ROI's median colour; mask = colour distance > THRESH, opened with a 3x3 so a plank's one-pixel
     outline is not a blob; label with scipy.ndimage and keep blobs of MIN_BLOB..MAX_BLOB px.
  2. `merge()` collapses mask fragments, on both stills of the pair: from straight above, one man's mask breaks into
     two or three pieces half a metre apart (the first g1 cut read the six men of blast1 as nine blobs, and the
     -1 s frames carried 35-55 blobs where six men stood). Any two blob centres closer than MERGE_M become one
     blob, area summed and centre area-weighted, until nothing merges. MERGE_M 1.0 m against the row's 2.5 m
     spacing cannot swallow two men.
  3. The standing men in the -1 s still are found as a six-point LATTICE of SPACING_M, and the lattice gap is NOT
     fitted from a blob pair (that is how the first cut found a false 49.8 px gap where the camera says 77.94): the
     gap is `SPACING_M * scale`, known from the camera. Only the band x and the phase are searched.
  4. The scale is NOT re-derived from the row's own pixel spacing (r2 did that and got px/m drifting 29.8-33.3
     across runs, a 10% spread that is 0.2 m on a 2 m throw -- the size of the whole calm floor). CaptureRig poses a
     perspective camera at `distance = zoom * tan(30deg) / tan(fov/2)` (CaptureRig.cs:190), so whatever the fov, the
     ground half-height visible is `zoom * tan(30deg)` metres: `px_per_m = (h/2) / (zoom * tan(30deg))`. At zoom 20,
     720 px high: 360 / (20 * tan(30deg)) = 360 / 11.547 = 31.18. Every sidecar in this set carries the same `zoom`
     (checked below), so ONE scale serves the whole set. The standing row's own span is kept only as a check: if it
     disagrees with the camera-derived scale by more than ROW_SCALE_TOLERANCE, the run raises -- a silent scale
     error is worse than a loud one.
  5. One pinned camera, checked, not assumed: every sidecar must carry the same `zoom`, `pitch`, `yaw` and `cam_y`
     (within POSE_TOLERANCE_M / POSE_TOLERANCE_DEG) and `pose_error_m` within MAX_POSE_ERROR_M. `cam_x`/`cam_z` are
     still read per still (different patches of ground), which is what `to_world` uses to make two patches
     comparable in one coordinate frame.
  6. The candidates of a run are the merged +3 s blobs whose area is in MAN_AREA_MIN..MAX -- a window measured from
     the six standing men of the three calm -1 s stills (see frames.txt), not from any result, and an order of
     magnitude below the crater's 14 000-64 000 px. The run's values are the six `d_i` above; its `median_m` is
     their median.

The bar: calm_spread_m is the largest `d_i` over every calm1..3 man -- how far a standing man's nearest candidate
drifts on its own, at its worst, on this capture. worst_throw_m is the smallest `d_i` over blast1..3. The set passes
only if every blast man clears the calm spread (worst_throw_m > calm_spread_m); otherwise the failing run, man and
distance are printed and the script exits 1. There is no baseline constant: a number copied out of the run it judges
proves nothing (that is exactly how r2's circular baseline happened).

Reported in probe.json and printed: throw_m, per_body_m, blast_median_m, noise_floor_m, calm_spread_m,
worst_throw_m, px_per_m, frame_gap, seconds_gap, fps_implied, fallen_after and the per-run rows. Raises if a run
has fewer than MIN_LATTICE standing men or fewer than MIN_BODIES candidates, or if the pinned-camera check fails.
"""
import glob
import json
import math
import os
import statistics
import sys

import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))

SPACING_M = 2.5        # DeathLab.Row spacing in BlastThrowStills
MEN = 6                # BlastThrowStills `Men`: the row is six men, so the lattice is six points wide and no
                       # wider. Both numbers come from the harness that shot the stills, never from the result.
MIN_LATTICE = 5        # how many of the six lattice points must carry a blob before a run is measured at all:
                       # a mask that missed two men is a bad capture, to be reshot, not a thin measurement
THRESH = 60.0          # colour distance from the snow that counts as foreground (a man reads 60-150 dark of it)
MIN_BLOB = 120         # px; a blob smaller than this is debris or blood, not a man
MAX_BLOB = 2000        # px; bigger than this is a shell hole, a bunker or two bodies run together. Raised from
                       # r2's 1600: a standing man can read up to ~1900 px when his idle pose spreads the mask
                       # (measured on this build's calm2 row, body at x~434), and excluding that blob entirely
                       # left the matcher no honest candidate near his own spot -- it picked a distant unrelated
                       # blob instead and read a 2.6 m "move" out of a man who never left his position.
BAND_PX = 22           # how far off one line the row's men may sit (the shot is pitch 89, not quite 90)
MERGE_M = 1.0          # blob centres closer than this are one man's mask broken into pieces, not two men: from
                       # straight above a figure's mask splits into two or three fragments ~0.5 m apart, and the
                       # row's own spacing is 2.5 m, so this cannot join two men
MAN_AREA_MIN, MAN_AREA_MAX = 62, 3692    # px, after merging: a candidate body's area window, measured as half the
                       # smallest to twice the largest merged standing man in the three calm -1 s stills (125-1846
                       # px; the 17 men are listed in frames.txt). MIN_BLOB already floors a fragment at 120 px, so
                       # only the ceiling bites, and the crater and the scorch read 14 100-63 800 px -- an order of
                       # magnitude out, so the window separates a body from the burst's own mark.
MIN_BODIES = 4         # the bar the leg card sets: n_bodies >= 4 per run
MAX_POSE_ERROR_M = 0.6     # how far off its asked pose a still may have been shot: the translation is taken
                           # out exactly from the sidecar's own camera, so this only has to catch a wild pose
POSE_TOLERANCE_M = 0.01    # how tightly cam_y must agree across every still: "one pinned camera" is the pose
POSE_TOLERANCE_DEG = 0.01  # same, for pitch and yaw
ROW_SCALE_TOLERANCE = 0.12  # the standing row's own median gap may not disagree with the camera-derived scale
                            # by more than this fraction, or the scale read is not trusted
BLAST_RUNS = ("blast1", "blast2", "blast3")
CALM_RUNS = ("calm1", "calm2", "calm3")
# the ground inside the frame, away from the edges where a thrown body is half cut off
ROI = (40, 40, 1240, 680)
FRAME_H_PX = 720


def blobs(path, roi=ROI, thresh=THRESH, min_blob=MIN_BLOB, max_blob=MAX_BLOB):
    """Every foreground blob in the ROI: (centre x, centre y, area), in full-image pixel coordinates."""
    im = np.array(Image.open(path).convert("RGB")).astype(float)
    x0, y0, x1, y1 = roi
    sub = im[y0:y1, x0:x1]
    bg = np.median(sub.reshape(-1, 3), axis=0)
    mask = np.sqrt(((sub - bg) ** 2).sum(axis=2)) > thresh
    mask = ndimage.binary_opening(mask, np.ones((3, 3)))
    lbl, n = ndimage.label(mask)
    if n == 0:
        return []
    ids = range(1, n + 1)
    sizes = ndimage.sum(mask, lbl, ids)
    coms = ndimage.center_of_mass(mask, lbl, ids)
    out = []
    for size, (cy, cx) in zip(sizes, coms):
        if min_blob <= size <= max_blob:
            out.append((cx + x0, cy + y0, float(size)))
    return out


def merge(found, scale, merge_m=MERGE_M):
    """Blob fragments joined: any two centres closer than `merge_m` metres become one blob, area summed and centre
    area-weighted, repeated until nothing merges. One man seen from straight above breaks into two or three mask
    pieces about half a metre apart, which is why the first g1 cut read six men as nine blobs and fitted a false
    lattice gap. The row's spacing is 2.5 m, well clear of `merge_m`."""
    out = [(x, y, a) for x, y, a in found]
    limit = merge_m * scale
    changed = True
    while changed:
        changed = False
        for i in range(len(out)):
            for j in range(i + 1, len(out)):
                xi, yi, ai = out[i]
                xj, yj, aj = out[j]
                if math.hypot(xi - xj, yi - yj) > limit:
                    continue
                total = ai + aj
                out[i] = ((xi * ai + xj * aj) / total, (yi * ai + yj * aj) / total, total)
                del out[j]
                changed = True
                break
            if changed:
                break
    return out


def standing_row(found, scale):
    """The row of men as the scenario's OWN geometry: the best fit of a six-point lattice of SPACING_M to the blobs.

    r2 and this leg's first set took "the longest chain of evenly spaced blobs", which let a stray body or a man who
    wandered into the frame join the measured set -- from straight above, a corpse already lying on the ground reads
    the same as a man standing, and it does not move when the shell goes off, so it measured as a throw of nothing.
    BlastThrowStills stands exactly `MEN` men `SPACING_M` apart, so the row is a lattice of at most MEN points
    spanning at most MEN - 1 gaps. Only lattice members are returned: a blob off the lattice is not a man of this row,
    however blob-like it is.

    The row stands along z (a column in the image, since straight down maps world z to screen y), so the band is on
    x and the lattice on y. The gap is not fitted: it is `SPACING_M * scale`, known from the camera's own pose. Only
    the band x and the phase are searched, so no blob pair can sell the fit a gap the camera contradicts.

    Returns (members sorted by y, the number of SPACING_M steps the members span).
    """
    gap = SPACING_M * scale
    best, best_steps = [], 0
    for anchor in found:
        band = sorted((q for q in found if abs(q[0] - anchor[0]) <= BAND_PX), key=lambda q: q[1])
        for phase in band:
            y0 = phase[1]
            at = {}
            for q in band:
                k = int(round((q[1] - y0) / gap))
                if not 0 <= k <= MEN - 1:
                    continue
                residual = abs(q[1] - (y0 + k * gap))
                if residual > 0.3 * gap:
                    continue
                if k not in at or residual < at[k][0]:
                    at[k] = (residual, q)
            members = [at[k][1] for k in sorted(at)]
            steps = max(at) - min(at) if at else 0
            if len(members) > len(best):
                best, best_steps = members, steps
    return best, best_steps


def row_scale_px_per_m(standing, steps):
    """Pixels per metre from the standing row's own span: its span is `steps` lots of SPACING_M. A check against
    the camera-derived scale, never the scale itself (see the module docstring)."""
    if len(standing) < 2 or steps < 1:
        return None
    return (standing[-1][1] - standing[0][1]) / (steps * SPACING_M)


def camera_px_per_m(zoom, frame_h_px=FRAME_H_PX):
    """px/m from the camera's own pose: CaptureRig.cs:190 poses a perspective camera so the ground half-height in
    view is `zoom * tan(30deg)` metres, whatever the fov. px_per_m = (h/2) / (zoom * tan(30deg))."""
    return (frame_h_px / 2.0) / (zoom * math.tan(math.radians(30.0)))


def to_world(found, path, scale, size=(1280, 720)):
    """Blob centres as ground metres, using the camera the sidecar says the still was actually taken from.

    Straight down at yaw 0: screen x grows with world x, screen y grows as world z falls, both at `scale` px/m, and
    the frame's centre is the camera's own (x, z). Working in world metres is what makes a pair comparable when the
    rig was still easing: a camera drift would otherwise read as throw on every body at once."""
    cam_x, cam_z = sidecar(path, "cam_x"), sidecar(path, "cam_z")
    if cam_x is None or cam_z is None:
        raise RuntimeError(f"{os.path.basename(path)} has no camera pose in its sidecar")
    w, h = size
    return [(cam_x + (bx - w / 2.0) / scale, cam_z + (h / 2.0 - by) / scale, area) for bx, by, area in found]


def throws(standing, candidates, before_path, after_path, scale):
    """One distance per standing man: `d_i` is the world-metre distance from man i's own standing spot to the
    NEAREST candidate body in the +3 s still. No assignment, so no permutation of a collinear row can fool it, and
    an extra candidate can only shrink a `d_i` -- which can only ever make a blast run harder to pass."""
    pts = to_world(standing, before_path, scale)
    cands = to_world(candidates, after_path, scale)
    out = []
    for px, pz, _a in pts:
        out.append(min(math.hypot(cx - px, cz - pz) for cx, cz, _ca in cands))
    return sorted(out)


def sidecar(path, key):
    side = os.path.splitext(path)[0] + ".json"
    if not os.path.exists(side):
        return None
    with open(side, encoding="utf-8") as fh:
        return json.load(fh).get(key)


def check_one_pinned_camera(tag, names):
    """Raise unless every still of this set was shot from the same camera pose (zoom, pitch, yaw, cam_y) within
    tolerance, and with pose_error_m under the bar. cam_x/cam_z are left out: they vary per run on purpose (six
    patches of ground), and to_world folds them back into one coordinate frame."""
    zooms, pitches, yaws, cam_ys, errs = [], [], [], [], []
    for name in names:
        for moment in ("m1_0", "p0_5", "p3_0"):
            path = os.path.join(HERE, f"{tag}_{name}_{moment}.png")
            if not os.path.exists(path):
                raise RuntimeError(f"missing still: {os.path.basename(path)}")
            zooms.append(sidecar(path, "zoom"))
            pitches.append(sidecar(path, "pitch"))
            yaws.append(sidecar(path, "yaw"))
            cam_ys.append(sidecar(path, "cam_y"))
            err = sidecar(path, "pose_error_m")
            if err is not None:
                errs.append((os.path.basename(path), err))
    if any(z is None for z in zooms) or max(zooms) - min(zooms) > 1e-6:
        raise RuntimeError(f"not one pinned camera: zoom varies across the set ({sorted(set(zooms))})")
    if any(p is None for p in pitches) or max(pitches) - min(pitches) > POSE_TOLERANCE_DEG:
        raise RuntimeError(f"not one pinned camera: pitch varies by more than {POSE_TOLERANCE_DEG} deg")
    if any(y is None for y in yaws) or max(yaws) - min(yaws) > POSE_TOLERANCE_DEG:
        raise RuntimeError(f"not one pinned camera: yaw varies by more than {POSE_TOLERANCE_DEG} deg")
    if any(c is None for c in cam_ys) or max(cam_ys) - min(cam_ys) > POSE_TOLERANCE_M:
        raise RuntimeError(f"not one pinned camera: cam_y varies by more than {POSE_TOLERANCE_M} m")
    for name, err in errs:
        if err > MAX_POSE_ERROR_M:
            raise RuntimeError(f"{name} was shot {err} m off its pose, not comparable")
    return zooms[0]


def run(tag, name, scale):
    """One run measured: the six `d_i`, and what its sidecars say. `scale` (px/m) comes from the camera pose, shared
    by the whole set; the row's own span is read only as a check on it."""
    before = os.path.join(HERE, f"{tag}_{name}_m1_0.png")
    after = os.path.join(HERE, f"{tag}_{name}_p3_0.png")
    for p in (before, after):
        if not os.path.exists(p):
            raise RuntimeError(f"missing still: {os.path.basename(p)}")

    standing, steps = standing_row(merge(blobs(before), scale), scale)
    if len(standing) < MIN_LATTICE:
        raise RuntimeError(f"{name}: only {len(standing)} of the row's {MEN} men fit the {SPACING_M} m lattice in the "
                           f"-1 s still, under the {MIN_LATTICE} a measurement needs: reshoot this set")
    row_scale = row_scale_px_per_m(standing, steps)
    if row_scale is not None:
        if abs(row_scale - scale) / scale > ROW_SCALE_TOLERANCE:
            raise RuntimeError(
                f"{name}: row-measured scale {row_scale:.2f} px/m disagrees with the camera-derived "
                f"{scale:.2f} px/m by more than {ROW_SCALE_TOLERANCE:.0%}")

    merged_after = merge(blobs(after), scale)
    candidates = [b for b in merged_after if MAN_AREA_MIN <= b[2] <= MAN_AREA_MAX]
    if len(candidates) < MIN_BODIES:
        raise RuntimeError(f"{name}: only {len(candidates)} candidate bodies in the +3 s still "
                           f"(area {MAN_AREA_MIN}-{MAN_AREA_MAX} px), under the {MIN_BODIES} the bar asks for")
    per_man = throws(standing, candidates, before, after, scale)
    fb, fa = sidecar(before, "frame"), sidecar(after, "frame")
    return {
        "run": name,
        "n_standing": len(standing),
        "n_candidates": len(candidates),
        "standing_area_px": [round(a) for _x, _y, a in standing],
        "per_man_m": [round(d, 3) for d in per_man],
        "median_m": round(statistics.median(per_man), 3),
        "px_per_m": round(scale, 2),
        "row_px_per_m": round(row_scale, 2) if row_scale is not None else None,
        "frame_gap": (fa - fb) if (fa is not None and fb is not None) else None,
        "fallen_after": sidecar(after, "fallen"),
        "contrast_median_after": sidecar(after, "contrast_median"),
    }


def main():
    tags = sorted({os.path.basename(p).split("_")[0] for p in glob.glob(os.path.join(HERE, "*_blast1_m1_0.png"))})
    if not tags:
        raise RuntimeError("no <tag>_blast1_m1_0.png here: run BlastThrowStills and copy its stills in")
    tag = "g1" if "g1" in tags else tags[0]

    zoom = check_one_pinned_camera(tag, BLAST_RUNS + CALM_RUNS)
    scale = camera_px_per_m(zoom)

    blast = [run(tag, name, scale) for name in BLAST_RUNS]
    calm = [run(tag, name, scale) for name in CALM_RUNS]

    pooled = [d for r in blast for d in r["per_man_m"]]
    calm_spread_m = max(d for r in calm for d in r["per_man_m"])
    worst_throw_m = min(d for r in blast for d in r["per_man_m"])
    seconds_gap = 4.0   # -1 s to +3 s, the pair that is measured
    gaps = [r["frame_gap"] for r in blast + calm if r["frame_gap"] is not None]
    frame_gap = statistics.median(gaps) if gaps else None
    report = {
        "tag": tag,
        "throw_m": round(statistics.median(pooled), 3),
        "per_body_m": [round(d, 3) for d in pooled],
        "noise_floor_m": [r["median_m"] for r in calm],
        "blast_median_m": [r["median_m"] for r in blast],
        "calm_spread_m": round(calm_spread_m, 3),
        "worst_throw_m": round(worst_throw_m, 3),
        "man_area_px": [MAN_AREA_MIN, MAN_AREA_MAX],
        "px_per_m": round(scale, 2),
        "frame_gap": frame_gap,
        "seconds_gap": seconds_gap,
        "fps_implied": round(frame_gap / seconds_gap, 2) if frame_gap else None,
        "fallen_after": [r["fallen_after"] for r in blast],
        "contrast_median_after": [r["contrast_median_after"] for r in blast],
        "runs": blast + calm,
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as fh:
        json.dump(report, fh, indent=2)
        fh.write("\n")
    for key in ("tag", "throw_m", "per_body_m", "noise_floor_m", "blast_median_m", "calm_spread_m", "worst_throw_m",
                "man_area_px", "px_per_m", "frame_gap", "seconds_gap", "fps_implied", "fallen_after",
                "contrast_median_after"):
        print(f"{key}: {report[key]}")
    for r in report["runs"]:
        print(f"  {r['run']}: {r['n_standing']} standing (areas {r['standing_area_px']}), "
              f"{r['n_candidates']} candidates, per man {r['per_man_m']}, median {r['median_m']} m, "
              f"{r['px_per_m']} px/m (row {r['row_px_per_m']})")

    failures = []
    for r in blast:
        for i, d in enumerate(r["per_man_m"]):
            if d <= calm_spread_m:
                failures.append((r["run"], i, d))
    if failures:
        for run_name, i, d in failures:
            print(f"FAIL: {run_name} man {i} is {d} m from the nearest body, inside the calm spread of "
                  f"{calm_spread_m} m")
        return 1
    if worst_throw_m < 2.0 * calm_spread_m:
        print(f"NOTE: the worst blast throw {worst_throw_m} m and the calm spread {calm_spread_m} m are within a "
              f"factor of two of each other -- the margin is thin, read the stills before trusting it")
    print(f"PASS: worst blast body {worst_throw_m} m clears the calm spread {calm_spread_m} m")
    return 0


if __name__ == "__main__":
    sys.exit(main())
