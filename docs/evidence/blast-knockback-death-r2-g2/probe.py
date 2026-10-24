"""Measures the blast knockback throw PER BODY against the EMPTY GROUND, and reports the nearest body, not the median.

Round 2's probe (docs/evidence/blast-knockback-death-r2/probe.py) took the background as the ROI's median colour, so
every plank, crate, sandbag and old shell hole on the patch was foreground and could be matched as a body: blast3
held 89 blobs over 120 px against blast1's 52, and the 1.712 m it reported was partly clutter, not men. Two things
change here, plus one the plate itself forced (the scale, the world mapping, the one-to-one match and the pose and
tilt guards are copied unchanged -- they were not the fault):

  1. The background is a PLATE: the same patch of ground photographed from the same pose BEFORE a single man stood on
     it (BlastThrowStills shoots `<tag>_<run>_plate.png` first). The mask is colour distance from the plate's own
     pixel, opened with a 3x3, so the planks subtract out and the man does not.
  2. The size window is READ OFF THE STILL, not typed: a candidate body's area must lie in
     [AREA_LO, AREA_HI] x the median area of the standing row found in the -1 s frame. A crater and a scorch patch
     are the wrong size for a man by that measure, whatever the zoom.
  3. Forced by (1): the plate's exposure is matched to the frame's before the two are subtracted, and the standing
     row is looked for on the FRAME'S CENTRE LINE only (see `blobs` and `centre_line` for what each of those cost
     when it was missing).

And the verdict is the WORST body, not the typical one: `nearest_body_m` is the minimum over the three blast runs'
matched bodies. A median hides the man who did not move; the player sees him.

What it reads (shot by Tests/Stills/BlastThrowStills.cs):
  <tag>_blast1..3_{plate,m1_0,p0_5,p3_0}.png  a row of six men 2.5 m apart, straight down (pitch 89, zoom 20): the
                                              bare ground, then -1 s, +0.5 s and +3 s around a shell at the row's
                                              middle;
  <tag>_calm1..3_{...}.png                    the same, with no shell: the noise floor.
Each PNG has a .json sidecar beside it (CaptureRig) carrying `frame`, `fallen`, `drawn_fallen` and
`corpses_in_frame`.

It exits 0 only when every blast run drew at least MIN_DRAWN_FALLEN corpses in its +3 s frame AND the nearest
matched body is at least NEAREST_BAR_M from where that man stood.
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
THRESH = 60.0          # colour distance from the PLATE's pixel that counts as foreground
MIN_BLOB = 120         # px; a blob smaller than this is debris or blood, not a man
MAX_BLOB = 1600        # px; bigger than this is a shell hole, a bunker or two bodies run together
AREA_LO, AREA_HI = 0.35, 2.5   # a body's area, as a fraction of the standing row's median area: this is what keeps
                               # a crater and a scorch patch out. Tighten before touching MAX_MATCH_M.
BAND_PX = 22           # how far off one line the row's men may sit (the shot is pitch 89, not quite 90)
CENTRE_BAND_PX = 40    # and how far off the FRAME's centre line that row itself may sit: the shot is centred
                       # on the row's middle, so this is a prior, not a fit
GAP_MIN_PX, GAP_MAX_PX = 50, 110   # one SPACING_M gap at this zoom is ~79 px
AREA_W = 40.0          # how much a change of area costs, against one pixel of distance
MAX_MATCH_M = 12.0     # a "body" that moved further than this is not the same body. Never raised.
MIN_BODIES = 4         # n_bodies >= 4 per run
MIN_DRAWN_FALLEN = 4   # corpses the +3 s frame must actually have drawn (sidecar `drawn_fallen`)
MAX_POSE_ERROR_M = 0.6     # how far off its asked pose a still may have been shot
MAX_POSE_TILT_DEG = 0.5    # and how far off in pitch or yaw
# The bar the nearest body must clear. Three passes of THIS build (commit a7bb5d9c77 plus this unit's plate shot),
# same field, tags a/b/c, each a fresh batch editor, gave nearest_body_m 0.352, 0.352 and 0.352 m -- the stills
# differ pixel for pixel (snow drifts) but the measurement does not move at all, so the spread is 0.000 m. The bar is
# 0.30 m: 0.052 m, 15%, clear BELOW that spread's minimum, and well above the 0.005-0.106 m a standing row's nearest
# man drifts by on his own (the calm runs). Outside the spread, not inside it.
NEAREST_BAR_M = 0.30
BLAST_RUNS = ("blast1", "blast2", "blast3")
CALM_RUNS = ("calm1", "calm2", "calm3")
# the ground inside the frame, away from the edges where a thrown body is half cut off
ROI = (40, 40, 1240, 680)


def blobs(path, plate, roi=ROI, thresh=THRESH, min_blob=MIN_BLOB, max_blob=MAX_BLOB):
    """Every blob in the ROI that is not on the PLATE: (centre x, centre y, area), in full-image pixels.

    The plate is the same patch from the same pose with nobody on it, so the difference is men, debris and what the
    shell did to the ground -- never the planks that were always there."""
    im = np.array(Image.open(path).convert("RGB")).astype(float)
    bg = np.array(Image.open(plate).convert("RGB")).astype(float)
    if im.shape != bg.shape:
        raise RuntimeError(f"{os.path.basename(path)} and its plate are different sizes")
    x0, y0, x1, y1 = roi
    sub, plt = im[y0:y1, x0:x1], bg[y0:y1, x0:x1]
    # the plate is shot a second or two before the frame and the sky is not perfectly still: on blast3 the whole
    # picture had darkened by (6, 12, 20) of 255 between the two, which is enough to make a light man on light snow
    # read as no change at all (four of six men vanished from the mask). The plate's exposure is matched to the
    # frame's first, by the MEDIAN difference over the ROI -- men and debris are a few percent of the pixels, so the
    # median is the ground's own shift and nothing else.
    plt = plt + np.median((sub - plt).reshape(-1, 3), axis=0)
    diff = np.sqrt(((sub - plt) ** 2).sum(axis=2))
    mask = ndimage.binary_opening(diff > thresh, np.ones((3, 3)))
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


def centre_line(found, mid_y=360.0, band=CENTRE_BAND_PX):
    """Only the blobs on the frame's own centre line. The harness centres the shot on the row's middle and looks
    straight down, so the standing men are on that line by construction. blast3's patch borders a frozen pond whose
    edge gives a longer evenly spaced chain than the men do -- "the longest chain anywhere in the frame" picked the
    pond and found no men at all."""
    return [p for p in found if abs(p[1] - mid_y) <= band]


def standing_row(found):
    """The row of men: the longest chain of evenly spaced blobs on one horizontal line, and how many SPACING_M steps
    it spans. A man the mask missed leaves a double gap rather than ending the row, so a step of one or two counts."""
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
    the frame's centre is the camera's own (x, z)."""
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
    """One run measured against its own plate: its per-body distances, its scale, and what its sidecars say."""
    plate = os.path.join(HERE, f"{tag}_{name}_plate.png")
    before = os.path.join(HERE, f"{tag}_{name}_m1_0.png")
    after = os.path.join(HERE, f"{tag}_{name}_p3_0.png")
    for p in (plate, before, after):
        if not os.path.exists(p):
            raise RuntimeError(f"missing still: {os.path.basename(p)}")
    # a pair is only comparable from the same camera, and the plate must be from that same pose too
    for p in (plate, before, after):
        err = sidecar(p, "pose_error_m")
        if err is not None and err > MAX_POSE_ERROR_M:
            raise RuntimeError(f"{name}: {os.path.basename(p)} was shot {err} m off its pose, not comparable")
    pitches = [sidecar(p, "cam_pitch") for p in (plate, before, after)]
    yaws = [sidecar(p, "cam_yaw") for p in (plate, before, after)]
    if None not in pitches and max(pitches) - min(pitches) > MAX_POSE_TILT_DEG:
        raise RuntimeError(f"{name}: the set was shot at pitches {pitches}, not comparable")
    if None not in yaws and max(yaws) - min(yaws) > MAX_POSE_TILT_DEG:
        raise RuntimeError(f"{name}: the set was shot at yaws {yaws}, not comparable")

    standing, steps = standing_row(centre_line(blobs(before, plate)))
    if len(standing) < MIN_BODIES:
        raise RuntimeError(f"{name}: only {len(standing)} men found standing in the -1 s still")
    scale = px_per_m(standing, steps)
    # the size of a man, measured in this very frame: the row's median blob area. The after frame's candidates are
    # then cut to that size, which is what a crater and a scorch patch fail.
    man_px = statistics.median([p[2] for p in standing])
    cand = blobs(after, plate, min_blob=max(MIN_BLOB, AREA_LO * man_px), max_blob=min(MAX_BLOB, AREA_HI * man_px))
    per_body = matched_distances(standing, cand, before, after, scale)
    if len(per_body) < MIN_BODIES:
        raise RuntimeError(f"{name}: only {len(per_body)} bodies matched, under the {MIN_BODIES} the bar asks for")
    fb, fa = sidecar(before, "frame"), sidecar(after, "frame")
    return {
        "run": name,
        "n_standing": len(standing),
        "n_candidates": len(cand),
        "n_bodies": len(per_body),
        "man_px": round(man_px, 1),
        "per_body_m": [round(d, 3) for d in per_body],
        "nearest_m": round(min(per_body), 3),
        "median_m": round(statistics.median(per_body), 3),
        "px_per_m": round(scale, 2),
        "frame_gap": (fa - fb) if (fa is not None and fb is not None) else None,
        "fallen_after": sidecar(after, "fallen"),
        "drawn_fallen": sidecar(after, "drawn_fallen"),
        "corpses_in_frame": sidecar(after, "corpses_in_frame"),
        "contrast_median_after": sidecar(after, "contrast_median"),
    }


def main():
    tags = sorted({os.path.basename(p).split("_")[0] for p in glob.glob(os.path.join(HERE, "*_blast1_plate.png"))})
    if not tags:
        raise RuntimeError("no <tag>_blast1_plate.png here: run BlastThrowStills and copy its stills in")
    tag = tags[0]
    blast = [run(tag, name) for name in BLAST_RUNS]
    calm = [run(tag, name) for name in CALM_RUNS]

    pooled = [d for r in blast for d in r["per_body_m"]]
    seconds_gap = 4.0   # -1 s to +3 s, the pair that is measured
    gaps = [r["frame_gap"] for r in blast + calm if r["frame_gap"] is not None]
    frame_gap = statistics.median(gaps) if gaps else None
    nearest = min(r["nearest_m"] for r in blast)
    drawn = [r["drawn_fallen"] for r in blast]
    report = {
        "tag": tag,
        "nearest_body_m": round(nearest, 3),
        "nearest_bar_m": NEAREST_BAR_M,
        "throw_m": round(statistics.median(pooled), 3),
        "per_body_m": [round(d, 3) for d in pooled],
        "noise_floor_m": [r["median_m"] for r in calm],
        "blast_median_m": [r["median_m"] for r in blast],
        "blast_nearest_m": [r["nearest_m"] for r in blast],
        "px_per_m": round(statistics.median([r["px_per_m"] for r in blast + calm]), 2),
        "frame_gap": frame_gap,
        "seconds_gap": seconds_gap,
        "fps_implied": round(frame_gap / seconds_gap, 2) if frame_gap else None,
        "fallen_after": [r["fallen_after"] for r in blast],
        "drawn_fallen": drawn,
        "corpses_in_frame": [r["corpses_in_frame"] for r in blast],
        "runs": blast + calm,
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as fh:
        json.dump(report, fh, indent=2)
        fh.write("\n")
    for key in ("tag", "nearest_body_m", "nearest_bar_m", "throw_m", "noise_floor_m", "blast_median_m",
                "blast_nearest_m", "px_per_m", "frame_gap", "seconds_gap", "fps_implied", "fallen_after",
                "drawn_fallen", "corpses_in_frame"):
        print(f"{key}: {report[key]}")
    for r in report["runs"]:
        print(f"  {r['run']}: {r['n_bodies']}/{r['n_candidates']} bodies, nearest {r['nearest_m']} m, "
              f"median {r['median_m']} m, man {r['man_px']} px, {r['px_per_m']} px/m")

    bad = []
    for r in blast:
        if r["drawn_fallen"] is None or r["drawn_fallen"] < MIN_DRAWN_FALLEN:
            bad.append(f"{r['run']}: drew {r['drawn_fallen']} corpses, under {MIN_DRAWN_FALLEN}")
    if nearest < NEAREST_BAR_M:
        bad.append(f"the nearest body rests {nearest:.3f} m from where he stood, under the {NEAREST_BAR_M} m bar")
    for line in bad:
        print("FAIL " + line)
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
