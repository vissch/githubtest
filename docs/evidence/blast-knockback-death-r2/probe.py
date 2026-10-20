"""Measures the blast knockback throw PER BODY from the captured stills, not typed.

Harsh critic r1 (12/100) on blast-knockback-death found that round's probe measured the distance between the
centroids of ALL changed pixels — debris, blood and the crater included — and called it the throw. This one matches
bodies one by one instead, and it only ever looks at pixels.

What it reads (shot by Tests/Stills/BlastThrowStills.cs, `pre_`/`post_` prefix):
  <tag>_blast1..3_{m1_0,p0_5,p3_0}.png  a row of six men 2.5 m apart, straight down (pitch 89, zoom 20), at -1 s,
                                        +0.5 s and +3 s around a shell at the row's middle;
  <tag>_calm1..3_{...}.png              the same three moments with no shell: the noise floor, what a standing row
                                        drifts by on its own.
Each PNG has a .json sidecar beside it (CaptureRig) carrying `frame`, `fallen` and `contrast_median`.

Method, per run, on the (-1 s, +3 s) pair:
  1. background = the ROI's median colour; mask = colour distance > THRESH, opened with a 3x3 so a plank's one-pixel
     outline is not a blob; label with scipy.ndimage and keep blobs of MIN_BLOB..MAX_BLOB px.
  2. The standing men in the -1 s still are found as a ROW, not as "every blob": the horizontal line (y within
     BAND_PX) holding the longest chain of blobs evenly spaced along x. A top-down winter field is full of planks,
     crates, sandbags and shell holes that are blobs too, and they do not stand in an evenly spaced line. The scale
     is the chain's MEDIAN gap: px_per_m = gap / SPACING_M, which survives a man the mask missed.
  3. Match the -1 s blobs to the +3 s blobs ONE TO ONE (scipy.optimize.linear_sum_assignment) on
     cost = pixel distance + AREA_W * |area ratio - 1|, dropping any match over MAX_MATCH_M. The area term and that
     cap are what keep debris and the crater from being read as a body.
  4. The run's value is the MEDIAN of its per-body distances in metres.

Reported in probe.json and printed: throw_m (the median of the three blast runs' per-body distances pooled),
per_body_m, noise_floor_m (the three calm runs' medians), baseline_throw_m, px_per_m, frame_gap, seconds_gap,
fps_implied, fallen_after, contrast_median_after. Raises if a run matched fewer than MIN_BODIES bodies.
"""
import glob
import json
import os
import statistics
import sys

import numpy as np
from PIL import Image
from scipy import ndimage
from scipy.optimize import linear_sum_assignment

HERE = os.path.dirname(os.path.abspath(__file__))

SPACING_M = 2.5        # DeathLab.Row spacing in BlastThrowStills
THRESH = 60.0          # colour distance from the snow that counts as foreground (a man reads 60-150 dark of it)
MIN_BLOB = 120         # px; a blob smaller than this is debris or blood, not a man
MAX_BLOB = 1600        # px; bigger than this is a shell hole, a bunker or two bodies run together
BAND_PX = 22           # how far off one line the row's men may sit (the shot is pitch 89, not quite 90)
GAP_MIN_PX, GAP_MAX_PX = 50, 110   # one SPACING_M gap at this zoom is ~79 px: narrow enough that a chain cannot
                                   # skip every other man and call 2.5 m a 5 m gap (calm2 did, at a 160 px cap)
AREA_W = 40.0          # how much a change of area costs, against one pixel of distance
MAX_MATCH_M = 12.0     # a "body" that moved further than this is not the same body
MIN_BODIES = 4         # the bar the leg card sets: n_bodies >= 4 per run
MAX_POSE_ERROR_M = 0.6     # how far off its asked pose a still may have been shot: the translation is taken
                           # out exactly from the sidecar's own camera, so this only has to catch a wild pose
MAX_POSE_TILT_DEG = 0.5    # and how far off in pitch or yaw
BLAST_RUNS = ("blast1", "blast2", "blast3")
CALM_RUNS = ("calm1", "calm2", "calm3")
# the ground inside the frame, away from the edges where a thrown body is half cut off
ROI = (40, 40, 1240, 680)


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


def standing_row(found):
    """The row of men: the longest chain of evenly spaced blobs on one horizontal line, and how many SPACING_M steps
    it spans. A man the mask missed leaves a double gap rather than ending the row, so a step of one or two counts
    (blast2 had five men on the line and a sixth lost in a shell hole: a one-gap-only chain found three)."""
    best, best_steps = [], 0
    for anchor in found:
        band = sorted((p for p in found if abs(p[1] - anchor[1]) <= BAND_PX), key=lambda p: p[0])
        for i in range(len(band)):
            for j in range(i + 1, len(band)):
                gap = band[j][0] - band[i][0]
                if not GAP_MIN_PX <= gap <= GAP_MAX_PX:
                    continue
                chain, last, steps = [band[i], band[j]], band[j], 1
                for p in band[j + 1:]:
                    for k in (1, 2):
                        if abs(p[0] - last[0] - k * gap) <= 0.3 * gap:
                            chain.append(p)
                            last = p
                            steps += k
                            break
                if len(chain) > len(best):
                    best, best_steps = chain, steps
    return best, best_steps


def px_per_m(standing, steps):
    """Pixels per metre from the standing row: its span is `steps` lots of SPACING_M."""
    if len(standing) < 2 or steps < 1:
        raise RuntimeError("fewer than two standing men found: no scale can be read")
    return (standing[-1][0] - standing[0][0]) / (steps * SPACING_M)


def to_world(found, path, scale, size=(1280, 720)):
    """Blob centres as ground metres, using the camera the sidecar says the still was actually taken from.

    Straight down at yaw 0: screen x grows with world x, screen y grows as world z falls, both at `scale` px/m, and
    the frame's centre is the camera's own (x, z). Working in world metres is what makes a pair comparable when the
    rig was still easing: a 0.24 m camera drift would otherwise read as 0.24 m of throw on every body at once."""
    cam_x, cam_z = sidecar(path, "cam_x"), sidecar(path, "cam_z")
    if cam_x is None or cam_z is None:
        raise RuntimeError(f"{os.path.basename(path)} has no camera pose in its sidecar")
    w, h = size
    return [(cam_x + (bx - w / 2.0) / scale, cam_z + (h / 2.0 - by) / scale, area) for bx, by, area in found]


def matched_distances(before, after, before_path, after_path, scale):
    """One-to-one matches from `before` blobs to `after` blobs; the distances in metres, nearest first."""
    if not before or not after:
        return []
    bw = to_world(before, before_path, scale)
    aw = to_world(after, after_path, scale)
    cost = np.zeros((len(bw), len(aw)))
    for i, (bx, bz, ba) in enumerate(bw):
        for j, (ax, az, aa) in enumerate(aw):
            d = float(np.hypot(ax - bx, az - bz)) * scale      # in pixels, so AREA_W keeps its meaning
            ratio = aa / ba if ba > 0 else 0.0
            cost[i, j] = d + AREA_W * abs(ratio - 1.0)
    rows, cols = linear_sum_assignment(cost)
    out = []
    for i, j in zip(rows, cols):
        metres = float(np.hypot(aw[j][0] - bw[i][0], aw[j][1] - bw[i][1]))
        if metres <= MAX_MATCH_M:
            out.append(metres)
    return sorted(out)


def sidecar(path, key):
    side = os.path.splitext(path)[0] + ".json"
    if not os.path.exists(side):
        return None
    with open(side, encoding="utf-8") as fh:
        return json.load(fh).get(key)


def run(tag, name):
    """One run measured: its per-body distances, its scale, and what its sidecars say."""
    before = os.path.join(HERE, f"{tag}_{name}_m1_0.png")
    after = os.path.join(HERE, f"{tag}_{name}_p3_0.png")
    for p in (before, after):
        if not os.path.exists(p):
            raise RuntimeError(f"missing still: {os.path.basename(p)}")
    # a pair is only comparable from the same camera: the sidecars carry where the camera actually ended up, and a
    # still taken while the rig was still easing into its pose moves every blob in the frame at once
    for p in (before, after):
        err = sidecar(p, "pose_error_m")
        if err is not None and err > MAX_POSE_ERROR_M:
            raise RuntimeError(f"{name}: {os.path.basename(p)} was shot {err} m off its pose, not comparable")
    pitches = [sidecar(p, "cam_pitch") for p in (before, after)]
    yaws = [sidecar(p, "cam_yaw") for p in (before, after)]
    if None not in pitches and abs(pitches[0] - pitches[1]) > MAX_POSE_TILT_DEG:
        raise RuntimeError(f"{name}: the pair was shot at pitches {pitches}, not comparable")
    if None not in yaws and min(abs(yaws[0] - yaws[1]), 360 - abs(yaws[0] - yaws[1])) > MAX_POSE_TILT_DEG:
        raise RuntimeError(f"{name}: the pair was shot at yaws {yaws}, not comparable")

    standing, steps = standing_row(blobs(before))
    if len(standing) < MIN_BODIES:
        raise RuntimeError(f"{name}: only {len(standing)} men found standing in the -1 s still")
    scale = px_per_m(standing, steps)
    per_body = matched_distances(standing, blobs(after), before, after, scale)
    if len(per_body) < MIN_BODIES:
        raise RuntimeError(f"{name}: only {len(per_body)} bodies matched, under the {MIN_BODIES} the bar asks for")
    fb, fa = sidecar(before, "frame"), sidecar(after, "frame")
    return {
        "run": name,
        "n_standing": len(standing),
        "n_bodies": len(per_body),
        "per_body_m": [round(d, 3) for d in per_body],
        "median_m": round(statistics.median(per_body), 3),
        "px_per_m": round(scale, 2),
        "frame_gap": (fa - fb) if (fa is not None and fb is not None) else None,
        "fallen_after": sidecar(after, "fallen"),
        "contrast_median_after": sidecar(after, "contrast_median"),
    }


def main():
    tags = sorted({os.path.basename(p).split("_")[0] for p in glob.glob(os.path.join(HERE, "*_blast1_m1_0.png"))})
    if not tags:
        raise RuntimeError("no <tag>_blast1_m1_0.png here: run BlastThrowStills and copy its stills in")
    tag = "post" if "post" in tags else tags[0]
    blast = [run(tag, name) for name in BLAST_RUNS]
    calm = [run(tag, name) for name in CALM_RUNS]

    pooled = [d for r in blast for d in r["per_body_m"]]
    seconds_gap = 4.0   # -1 s to +3 s, the pair that is measured
    gaps = [r["frame_gap"] for r in blast + calm if r["frame_gap"] is not None]
    frame_gap = statistics.median(gaps) if gaps else None
    report = {
        "tag": tag,
        "throw_m": round(statistics.median(pooled), 3),
        "per_body_m": [round(d, 3) for d in pooled],
        "noise_floor_m": [r["median_m"] for r in calm],
        "baseline_throw_m": [r["median_m"] for r in blast],
        "px_per_m": round(statistics.median([r["px_per_m"] for r in blast + calm]), 2),
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
    for key in ("tag", "throw_m", "per_body_m", "noise_floor_m", "baseline_throw_m", "px_per_m",
                "frame_gap", "seconds_gap", "fps_implied", "fallen_after", "contrast_median_after"):
        print(f"{key}: {report[key]}")
    for r in report["runs"]:
        print(f"  {r['run']}: {r['n_bodies']} bodies, median {r['median_m']} m, {r['px_per_m']} px/m")
    return 0


if __name__ == "__main__":
    sys.exit(main())
