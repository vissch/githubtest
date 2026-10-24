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
  g1_blast1..3_{b0_0,m1_0,p0_5,p3_0}.png  the bare patch, then a row of six men 2.5 m apart along z, straight down (pitch 89, zoom 20), at
                                      -1 s, +0.5 s and +3 s around a shell 5 m to -x of the row line;
  g1_calm1..3_{...}.png              the same four moments with no shell: the noise floor, what a standing row
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
  3. The six standing men are NOT found in the pixels at all: `spots()` reads them off the scenario's own geometry
     and the still's own sidecar (see that function). A lattice fit from straight above picks terrain, and that was
     the probe's first false zero.
  4. The scale is NOT re-derived from the row's own pixel spacing (r2 did that and got px/m drifting 29.8-33.3
     across runs, a 10% spread that is 0.2 m on a 2 m throw -- the size of the whole calm floor). CaptureRig poses a
     perspective camera at `distance = zoom * tan(30deg) / tan(fov/2)` (CaptureRig.cs:190), so whatever the fov, the
     ground half-height visible is `zoom * tan(30deg)` metres: `px_per_m = (h/2) / (zoom * tan(30deg))`. At zoom 20,
     720 px high: 360 / (20 * tan(30deg)) = 360 / 11.547 = 31.18. Every sidecar in this set carries the same `zoom`
     (checked below), so ONE scale serves the whole set.
  5. One pinned camera, checked, not assumed: every sidecar must carry the same `zoom`, `pitch`, `yaw` and `cam_y`
     (within POSE_TOLERANCE_M / POSE_TOLERANCE_DEG) and `pose_error_m` within MAX_POSE_ERROR_M. `cam_x`/`cam_z` are
     still read per still (different patches of ground), which is what `to_world` uses to make two patches
     comparable in one coordinate frame.
  6. The candidates of a run are the merged, WARM +3 s blobs (`WARM_RB`: the uniform is warm, the ground is not)
     whose area is in MAN_AREA_MIN..MAX -- a window measured from the six standing men of the three calm -1 s
     stills (see frames.txt), not from any result, and an order of magnitude below the crater's 14 000-64 000 px --
     and whose centre has NO blob of the run's own bare-ground control still (`<tag>_<run>_b0_0.png`, the same
     patch from the same pose with nobody on it) within CONTROL_M: a blob already in the control is ground.
     A calm spot with no candidate within LOST_M raises instead of setting the bar: a man the mask lost and a body
     that flew that far are the same number otherwise. The run's values are the six `d_i` above; its `median_m` is
     their median.

The bar: calm_spread_m is the largest `d_i` over every calm1..3 man -- how far a standing man's nearest candidate
drifts on its own, at its worst, on this capture. worst_throw_m is the smallest `d_i` over blast1..3. The set passes
only if every blast man clears the calm spread (worst_throw_m > calm_spread_m); otherwise the failing run, man and
distance are printed and the script exits 1. There is no baseline constant: a number copied out of the run it judges
proves nothing (that is exactly how r2's circular baseline happened).

Reported in probe.json and printed: throw_m, per_body_m, blast_median_m, noise_floor_m, calm_spread_m,
worst_throw_m, px_per_m, frame_gap, seconds_gap, fps_implied, fallen_after and the per-run rows. Raises if a run
has fewer than MIN_BODIES candidates, if a calm spot lost its man, or if the pinned-camera check fails.
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
THRESH = 60.0          # colour distance from the snow that counts as foreground (a man reads 60-150 dark of it)
MIN_BLOB = 120         # px; a blob smaller than this is debris or blood, not a man
MAX_BLOB = 2000        # px; bigger than this is a shell hole, a bunker or two bodies run together. Raised from
                       # r2's 1600: a standing man can read up to ~1900 px when his idle pose spreads the mask
                       # (measured on this build's calm2 row, body at x~434), and excluding that blob entirely
                       # left the matcher no honest candidate near his own spot -- it picked a distant unrelated
                       # blob instead and read a 2.6 m "move" out of a man who never left his position.
WARM_RB = 3.7          # the uniform is warm, the ground is not: a blob's mean colour must have R - B at least this
                       # much (0-255) to be a man or a body. Measured, not tuned: the 22 blobs within 0.6 m of a
                       # known standing spot in the three calm -1 s stills run R-B 3.7 to 15.9 (mean 9.6), while
                       # the 134 other blobs of the same frames -- terrain, all of it -- run -24.7 to 28.1 with a
                       # median of -0.2 and only its top fifth over 3.7. The bar is the smallest certainly-man
                       # value of that set, so the rule cannot drop a man it was measured on. It is what turned
                       # blast1 from 50 candidate blobs into 6: see frames.txt.
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
CONTROL_M = 0.75       # m; a +3 s blob with a control-still blob this close is the same piece of ground, not a
                       # body. The control (`<tag>_<run>_b0_0.png`) is the run's own patch with nobody on it, shot
                       # after the off-map sweep and ClearFallen. 0.75 m is under the throw (2.1-7.4 m recorded)
                       # and under the row's 2.5 m spacing, so the control cannot delete a man's own spot.
LOST_M = 1.0           # m; a calm spot with no candidate this close means the mask lost a standing man: that run
                       # is reshot, not measured. Today such a man (2.24 m on calm1) reads exactly like a body
                       # that flew 2.24 m, and it is what set calm_spread_m.
BLAST_RUNS = ("blast1", "blast2", "blast3")
CALM_RUNS = ("calm1", "calm2", "calm3")
# the ground inside the frame, away from the edges where a thrown body is half cut off
ROI = (40, 40, 1240, 680)
FRAME_H_PX = 720


def blobs(path, roi=ROI, thresh=THRESH, min_blob=MIN_BLOB, max_blob=MAX_BLOB, warm_rb=WARM_RB):
    """Every warm foreground blob in the ROI: (centre x, centre y, area), in full-image pixel coordinates.

    Two rules, both measured on the calm stills: the blob stands out from the snow by `thresh`, and its mean colour
    is warm by `warm_rb` (R - B), which is the uniform and not the ground. Without the colour rule the frames hold
    36-57 man-sized blobs of churned terrain with no shell fired at all, and at that density the nearest blob to
    any point on the map sits 1-2 m away -- the size of the throw itself."""
    im = np.array(Image.open(path).convert("RGB")).astype(float)
    x0, y0, x1, y1 = roi
    sub = im[y0:y1, x0:x1]
    bg = np.median(sub.reshape(-1, 3), axis=0)
    mask = np.sqrt(((sub - bg) ** 2).sum(axis=2)) > thresh
    mask = ndimage.binary_opening(mask, np.ones((3, 3)))
    lbl, n = ndimage.label(mask)
    if n == 0:
        return []
    out = []
    for i in range(1, n + 1):
        this = lbl == i
        size = float(this.sum())
        if not min_blob <= size <= max_blob:
            continue
        colour = sub[this].mean(axis=0)
        if colour[0] - colour[2] < warm_rb:
            continue
        cy, cx = ndimage.center_of_mass(this)
        out.append((cx + x0, cy + y0, size))
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


def spots(path):
    """The six standing spots in world metres, from the scenario's own geometry -- not found in the pixels.

    `BlastThrowStills` stands man i at `(x, z + i*SPACING_M)` and pins the camera at `(x, z + (MEN-1)*SPACING_M/2)`,
    so man i stands at `(cam_x, cam_z - (MEN-1)*SPACING_M/2 + i*SPACING_M)`, read from the still's own sidecar.
    Checked against the pixels: 32 of this set's 36 spots carry a -1 s blob within 0.79 m.

    Finding the men as blobs is what made the probe red for four legs: from straight above a lattice fit happily
    picks terrain (blast3's "man" was the same 141 px clod as its "body", so `d_i` read 0.000), and the frames hold
    36-48 man-sized blobs of terrain with no shell fired at all. The geometry is known exactly, so it is used.
    """
    cam_x, cam_z = sidecar(path, "cam_x"), sidecar(path, "cam_z")
    if cam_x is None or cam_z is None:
        raise RuntimeError(f"{os.path.basename(path)} has no camera pose in its sidecar")
    z0 = cam_z - (MEN - 1) * SPACING_M / 2.0
    return [(cam_x, z0 + i * SPACING_M) for i in range(MEN)]


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


def distances(pts, cands):
    """One distance per standing spot: `d_i` is the world-metre distance from spot i to the NEAREST candidate body
    in the +3 s still. No assignment, so no permutation of a collinear row can fool it, and an extra candidate can
    only shrink a `d_i` -- which can only ever make a blast run harder to pass."""
    return [min(math.hypot(cx - px, cz - pz) for cx, cz, _ca in cands) for px, pz in pts]


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
        for moment in ("b0_0", "m1_0", "p0_5", "p3_0"):
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


def run(tag, name, scale, calm):
    """One run measured: the six `d_i`, and what its sidecars say. `scale` (px/m) comes from the camera pose, shared
    by the whole set. The men are not found in the pixels (see `spots`); what the pixels give is the candidate
    bodies, and the run's own bare-ground control still is what says which of them are bodies at all."""
    before = os.path.join(HERE, f"{tag}_{name}_m1_0.png")
    after = os.path.join(HERE, f"{tag}_{name}_p3_0.png")
    control = os.path.join(HERE, f"{tag}_{name}_b0_0.png")
    for p in (before, after, control):
        if not os.path.exists(p):
            raise RuntimeError(f"missing still: {os.path.basename(p)}")

    pts = spots(before)
    ground = to_world(merge(blobs(control), scale), control, scale)
    in_window = [b for b in merge(blobs(after), scale) if MAN_AREA_MIN <= b[2] <= MAN_AREA_MAX]
    candidates = []
    for cx, cz, area in to_world(in_window, after, scale):
        if any(math.hypot(gx - cx, gz - cz) <= CONTROL_M for gx, gz, _ga in ground):
            continue
        candidates.append((cx, cz, area))
    removed = len(in_window) - len(candidates)
    if len(candidates) < MIN_BODIES:
        raise RuntimeError(f"{name}: only {len(candidates)} candidate bodies in the +3 s still "
                           f"(area {MAN_AREA_MIN}-{MAN_AREA_MAX} px, {removed} removed by the control), under the "
                           f"{MIN_BODIES} the bar asks for")
    per_man = distances(pts, candidates)
    if calm:
        for i, d in enumerate(per_man):
            if d > LOST_M:
                raise RuntimeError(f"{name} spot {i}: the mask lost a standing man, reshoot "
                                   f"(nearest candidate {d:.3f} m, over the {LOST_M} m a standing man may drift)")
    fb, fa = sidecar(before, "frame"), sidecar(after, "frame")
    return {
        "run": name,
        "n_control": len(ground),
        "n_in_window": len(in_window),
        "n_removed_by_control": removed,
        "n_candidates": len(candidates),
        "candidate_area_px": [round(a) for _x, _z, a in candidates],
        "per_man_m": [round(d, 3) for d in per_man],
        "median_m": round(statistics.median(per_man), 3),
        "px_per_m": round(scale, 2),
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

    blast = [run(tag, name, scale, calm=False) for name in BLAST_RUNS]
    calm = [run(tag, name, scale, calm=True) for name in CALM_RUNS]

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
        print(f"  {r['run']}: {r['n_control']} control blobs, {r['n_in_window']} in-window blobs minus "
              f"{r['n_removed_by_control']} on control ground = {r['n_candidates']} candidates "
              f"(areas {r['candidate_area_px']}), per spot {r['per_man_m']}, median {r['median_m']} m, "
              f"{r['px_per_m']} px/m")

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
