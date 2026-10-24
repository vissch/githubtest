"""Measures the pixels of the nine stills committed under atrifle-look-r3-g1-g1-g2 (NOT re-shot here).

The question: does the prone AT rifleman read as his own man, or as the Sniper with another name?

WHAT THIS ROUND MOVES
  The owner's words: "The three readings are 0.763, 0.785 and 0.785. That is the size gap of a
  272-296 px helmet against a 1136-1264 px kneel after centering with no scale, and the 0.6 bar
  sits 0.16 under a cluster that only moves 0.022. Scale both masks to one height before the IoU,
  and set the bar just outside that spread. The sniper's 0.235 spread is the lamp (127 noise px in
  round 3), not a kneel a player can see change."

  g1-g1-g2's probe centred each mask on its own centroid and stopped there: no scale, no rotation.
  But the AT rifleman's mask is 17-18 px tall (272-296 px at a ~0.47 m2 crouch footprint) and the
  Sniper's is 39-43 px tall (1136-1264 px, a kneel standing clear of the parapet) - three quarters
  of the 0.76-0.78 reading was that size gap, not shape. This round adds exactly one step before the
  IoU: scale each mask to the same height first. The nine stills, their sidecars, poses.json and
  every gate upstream of the reading (one frame, one camera, the boxes from the capture's own
  projection, the weather subtraction, the phase and heading gates) are untouched and read again
  from atrifle-look-r3-g1-g1-g2 - this folder holds no PNGs or JSONs of its own.

  PASS_DIFF moves to 0.20 (see THE BAR below) because a scaled reading is not comparable to an
  unscaled one: every round back to 13d1158177's 0.6 compared *unscaled* masks, so none of them are
  a baseline for this number. The 0.6 lineage ends here.

ONE FRAME, ONE CAMERA / BOTH MEN IN ONE FRAME / WHO IS WHO / THE MASK / THE HONEST LIMIT
  Unchanged from atrifle-look-r3-g1-g1-g2; see that folder's probe.py and frames.txt for the staging,
  the box projection, the weather subtraction and the per-round gates. Only THE READING changes.

THE READING
  The two men stand five metres apart in the same frame by design and are not the same size, so a
  raw pixel IoU would measure the gap and the size both. Each mask is cropped to its own bounding
  box, resampled so its height is NORM_HEIGHT_PX (bilinear, re-thresholded at 0.5), then centred on
  a common canvas by its own centroid - translation only, after the one resize. The reading of a
  round is `1 - IoU` of the AT mask against the Sniper mask of the SAME pair still, both scaled this
  way. `spread()` runs the three masks of one man through the same `scaled` + `diff`, so the
  self-spreads are scale-free too: they are no longer a mix of "did he move" and "is he the same
  size as himself", because he always is.

THE BAR
  PASS_DIFF is 0.20, derived from the three scaled readings of THIS build (checked, not asserted,
  by check 2 in frames.txt): 0.3119 / 0.4124 / 0.3697. Their spread (max - min) is 0.1005.
  0.3119 - 0.1005 = 0.2114, snapped down to the nearest 0.05, is 0.20. So a reading has to fall a
  whole capture-to-capture spread below the worst of three captures before it fails - the bar sits
  just outside the spread the owner asked for, not inside it.

  Pass needs min(readings) >= PASS_DIFF AND max(spread_readings, spread_at) < PASS_DIFF.

  spread_sniper is reported and NOT gated. The owner ruled its 0.235 under the old (unscaled) measure
  the lamp, and the pixels do not contradict that call either way: the Sniper's KneelIdle phases in
  this set are 0.65 / 0.25 / 0.98, and the three pairwise self-diffs of his SCALED masks are 0.164
  (round 1-2), 0.183 (1-3) and 0.278 (2-3) - even the two rounds furthest from the noisiest round
  (round 3, 127 noise px in its box, see noise_in_box_px_sniper_pair-3 in g1-g1-g2's probe.json)
  differ by 0.16. A man read at three live phases of a kneel is a pose difference, not the
  repeatability of the measure, so it is reported as `spread_sniper` and `spread_sniper_above_bar`
  (true here) rather than folded into the pass/fail.

THE SELF-TEST
  `python probe.py --selftest` checks the scaling arithmetic against two worked examples with no
  stills involved - see `selftest()` below.

READING ANOTHER SET
  `python probe.py <folder>` measures the nine stills of another folder (with the same `pair-{r}` /
  `empty-{r}` / `floor-{r}` / poses.json layout) and prints the report WITHOUT writing probe.json.
"""
import json
import os
import re
import sys

import numpy as np
import scipy.ndimage as nd
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
STILLS = os.path.normpath(os.path.join(HERE, "..", "atrifle-look-r3-g1-g1-g2"))
if len(sys.argv) > 1 and sys.argv[1] != "--selftest":
    SET = os.path.abspath(sys.argv[1])
else:
    if not os.path.isdir(STILLS):
        print(f"FAIL: the stills this probe re-measures are gone: {STILLS}", file=sys.stderr)
        sys.exit(2)
    SET = STILLS
WRITE_JSON = SET == STILLS   # a foreign set is printed only; this set's number is not clobbered

THRESHOLD = 0.04
MAN_TALL_M = 2.6            # the height CaptureRig.ManBox projects, so tall_px is 2.6 m of frame
MIN_AREA_M2 = 0.2
MAX_BOX_SHARE = 0.5         # of his own box; see "the ceiling" in the docstring
BOX_HALF_W = 0.8            # of tall_px, either side of his feet
BOX_UP = 1.0                # of tall_px, above his feet
BOX_DOWN = 0.2              # of tall_px, below them
SCALE_TOLERANCE = 0.10      # how far the men of the rounds may disagree about px_per_m
DOMINANCE = 1.5
MIN_PHASE_GAP = 0.2       # the owner's number, on BOTH men, in the frames that are scored
HEADING_TOL = 0.05        # rad a man's yaw may differ from his own yaw in round 1
SNIPER_CLIP = "KneelIdle"
AT_CLIP = "ProneIdle"
AT_ARCHETYPE = 27         # the AT rifleman, drawn as the Soldier figure
SNIPER_ARCHETYPE = 3      # the Sniper, the only archetype with a figure of its own
NORM_HEIGHT_PX = 128      # both masks are scaled to this height before the IoU
# 0.3119, 0.4124, 0.3697 measured on this leg at NORM_HEIGHT_PX 128; spread 0.1005;
# 0.3119 - 0.1005 = 0.2114, snapped down to the nearest 0.05 -> 0.20. See THE BAR above.
PASS_DIFF = 0.20

ROUNDS = (1, 2, 3)
KINDS = ("pair", "empty", "floor")
MEN = {"pair": 2, "empty": 0, "floor": 0}
WHO = {"at": (AT_ARCHETYPE, AT_CLIP), "sniper": (SNIPER_ARCHETYPE, SNIPER_CLIP)}
FRAME_KEYS = ("zoom", "yaw", "pitch", "focus_x", "focus_z")


def here(name):
    return os.path.join(SET, name)


def sidecar(stem):
    with open(here(stem + ".json"), encoding="utf-8") as f:
        return json.load(f)


def luma(stem):
    a = np.asarray(Image.open(here(stem + ".png")).convert("RGB"), float) / 255.0
    return a[..., 0] * 0.2126 + a[..., 1] * 0.7152 + a[..., 2] * 0.0722


def raw_diff(stem, plate):
    """The difference mask of one still against a plate: luma threshold, 3x3 open, 7x7 close."""
    d = np.abs(luma(stem) - luma(plate)) > THRESHOLD
    return nd.binary_closing(nd.binary_opening(d, np.ones((3, 3))), np.ones((7, 7)))


def drawn_men(stem, shape):
    """Every man the capture says it drew INSIDE this frame: archetype -> [(slot, x, y, tall_px)].

    A man written off the map still gets a projection, far outside the picture; those are dropped
    here, so "the men in this still" is the capture's own answer. Keyed by archetype, so which box
    belongs to which man is the capture's answer too and no slot number is typed."""
    d = sidecar(stem)
    h, w = shape
    out = {}
    i = 0
    while ("man%d_slot" % i) in d:
        x, y = float(d["man%d_img_x" % i]), float(d["man%d_img_y" % i])
        tall = float(d["man%d_tall_px" % i])
        if 0 <= x < w and 0 <= y < h and tall > 1.0:
            out.setdefault(int(d["man%d_archetype" % i]), []).append(
                (int(d["man%d_slot" % i]), x, y, tall))
        i += 1
    return out


def box_of(x, y, tall, shape):
    """His own box, and whether each side of it is a real frame edge (so a cut can be told apart
    from a mask that simply stops where the box does)."""
    h, w = shape
    x0, x1 = x - BOX_HALF_W * tall, x + BOX_HALF_W * tall
    y0, y1 = y - BOX_UP * tall, y + BOX_DOWN * tall
    i0, i1 = max(0, int(round(x0))), min(w, int(round(x1)) + 1)
    j0, j1 = max(0, int(round(y0))), min(h, int(round(y1)) + 1)
    m = np.zeros(shape, bool)
    m[j0:j1, i0:i1] = True
    cuts = {"left": i0 == 0 and x0 < 0, "right": i1 == w and x1 >= w,
            "top": j0 == 0 and y0 < 0, "bottom": j1 == h and y1 >= h}
    return m, (i0, i1, j0, j1), cuts


def biggest_two(m):
    """(the largest component, its area, the second area) of a boolean mask; (None, 0, 0) if empty."""
    lab, n = nd.label(m)
    if n == 0:
        return None, 0, 0
    sizes = np.bincount(lab.ravel())
    sizes[0] = 0
    order = np.argsort(sizes)[::-1]
    second = int(sizes[order[1]]) if n > 1 else 0
    return lab == int(order[0]), int(sizes[order[0]]), second


def judge(stem, m, px, second, lo, hi, cuts, problems):
    """Does the largest component in the box earn the name "a man"? Size, dominance, not cut."""
    if m is None:
        return None
    ok = True
    if px < lo:
        problems.append(f"{stem}: the largest blob in his box is {px} px, under the frame's own "
                        f"floor of {lo} px ({MIN_AREA_M2} m2) - too small to be a man")
        ok = False
    if px > hi:
        problems.append(f"{stem}: the largest blob in his box is {px} px, over {hi} px - more "
                        f"than {MAX_BOX_SHARE:.0%} of his own box, so it is the patch and not him")
        ok = False
    if second * DOMINANCE > px:
        problems.append(f"{stem}: the largest blob in his box is {px} px and the next is {second} "
                        f"px, under {DOMINANCE}x - the mask is ambiguous")
        ok = False
    touched = [side for side, cut in cuts.items() if cut and (
        (side == "left" and m[:, 0].any()) or (side == "right" and m[:, -1].any()) or
        (side == "top" and m[0].any()) or (side == "bottom" and m[-1].any()))]
    if touched:
        problems.append(f"{stem}: the mask reaches the {', '.join(touched)} frame border - it is cut")
        ok = False
    return m if ok else None


def scaled(m):
    """`m` cropped to its own bounding box, resized so its height is NORM_HEIGHT_PX, re-thresholded.

    One height before the IoU is the whole point of this round: a 17-px-tall mask and a 40-px-tall
    mask disagree mostly about size, not shape, until both are put on the same footing."""
    ys, xs = np.nonzero(m)
    y0, y1, x0, x1 = ys.min(), ys.max() + 1, xs.min(), xs.max() + 1
    crop = m[y0:y1, x0:x1].astype(float)
    h = crop.shape[0]
    factor = NORM_HEIGHT_PX / h
    resized = nd.zoom(crop, factor, order=1)
    return resized > 0.5


# A canvas big enough that no scaled mask (height NORM_HEIGHT_PX, any width) can run off it once
# shifted to centre its centroid - shared by both masks in a diff() so their centroids land at the
# SAME pixel and an OR/AND of the two is an OR/AND of the right pixels, not of two independent crops.
CANVAS = (4 * NORM_HEIGHT_PX, 4 * NORM_HEIGHT_PX)


def centred(m):
    """`m` placed on the shared CANVAS with its centroid at the canvas centre. No rotation."""
    ys, xs = np.nonzero(m)
    cy, cx = int(round(ys.mean())), int(round(xs.mean()))
    ch, cw = CANVAS[0] // 2, CANVAS[1] // 2
    out = np.zeros(CANVAS, bool)
    out[ys - cy + ch, xs - cx + cw] = True
    return out


def diff(a, b):
    """1 - IoU of two masks, each scaled to NORM_HEIGHT_PX and centred on its own centroid."""
    a, b = centred(scaled(a)), centred(scaled(b))
    union = int((a | b).sum())
    return 1.0 - float((a & b).sum()) / union if union else 0.0


def spread(masks):
    return max(diff(masks[i], masks[j])
               for i in range(len(masks)) for j in range(i + 1, len(masks)))


def selftest():
    """Two worked examples a reader can check on paper, no stills involved.

    A 40x40 block against an 80x80 block: scaled to one height both are HxH, same shape, so the
    reading is 0.000. A 40x40 square against a 40-wide, 80-tall column: scaled to one height the
    square becomes HxH and the column H/2 x H, so their IoU is exactly a half and the reading is
    0.500. Tolerance 0.02 for the resample's edge pixels."""
    a = np.ones((40, 40), bool)
    b = np.ones((80, 80), bool)
    d1 = diff(a, b)
    print(f"block vs block: {d1:.3f} (want 0.000)")

    square = np.ones((40, 40), bool)
    column = np.ones((80, 40), bool)
    d2 = diff(square, column)
    print(f"square vs column: {d2:.3f} (want 0.500)")

    bad = []
    if abs(d1 - 0.000) > 0.02:
        bad.append(f"block vs block read {d1:.3f}, want 0.000 +-0.02")
    if abs(d2 - 0.500) > 0.02:
        bad.append(f"square vs column read {d2:.3f}, want 0.500 +-0.02")
    if bad:
        for b_ in bad:
            print("FAIL: " + b_, file=sys.stderr)
        return 2
    return 0


def read_pose(text, field):
    m = re.search(field + r"\s+(-?\d+\.?\d*)", text)
    return float(m.group(1)) if m else None


def clip_of(text):
    m = re.search(r"animation clip (\S+)", text)
    return m.group(1) if m else "?"


def phase_gap(a, b):
    """Distance between two phases on a LOOPING clip: 0.98 and 0.02 are 0.04 apart, not 0.96."""
    d = abs(a - b) % 1.0
    return min(d, 1.0 - d)


def wrap(a):
    """An angle folded into (-pi, pi], as a size."""
    return float(abs((a + np.pi) % (2 * np.pi) - np.pi))


def main():
    report = {"set": os.path.basename(SET), "pass_diff": PASS_DIFF, "threshold_luma": THRESHOLD,
              "min_area_m2": MIN_AREA_M2, "max_box_share": MAX_BOX_SHARE,
              "dominance": DOMINANCE, "man_tall_m": MAN_TALL_M, "norm_height_px": NORM_HEIGHT_PX,
              "archetypes": {"at": AT_ARCHETYPE, "sniper": SNIPER_ARCHETYPE},
              "box": {"half_width_x_tall": BOX_HALF_W, "up_x_tall": BOX_UP, "down_x_tall": BOX_DOWN}}
    problems = []

    stems = [f"{k}-{r}" for r in ROUNDS for k in KINDS]
    missing = [s for s in stems
               if not (os.path.exists(here(s + ".png")) and os.path.exists(here(s + ".json")))]
    if missing:
        print("FAIL: the set is incomplete: " + ", ".join(missing), file=sys.stderr)
        return 2

    # --- the sidecars gate the frame -------------------------------------------------------------
    frame = None
    for s in stems:
        d = sidecar(s)
        got = {k: float(d[k]) for k in FRAME_KEYS}
        got["size"] = list(Image.open(here(s + ".png")).size)
        if frame is None:
            frame = got
            report["camera"] = got
        elif got != frame:
            problems.append(f"{s}: camera {got} is not the set's {frame} - this is not one frame")
        if float(d["pose_error_m"]) != 0:
            problems.append(f"{s}: pose_error_m is {d['pose_error_m']}, want 0")
        if float(d["rain"]) != 0:
            problems.append(f"{s}: rain is {d['rain']}, want 0")
        if d["warnings"]:
            problems.append(f"{s}: the capture warns {d['warnings']}")
        kind = s.rsplit("-", 1)[0]
        if int(d["men_in_frame"]) != MEN[kind]:
            problems.append(f"{s}: men_in_frame is {d['men_in_frame']}, want {MEN[kind]}")
        if MEN[kind] == 0 and int(d.get("drawn_infantry", 0)) != 0:
            problems.append(f"{s}: a plate with no man in frame still drew "
                            f"{d['drawn_infantry']} - that is not a plate")

    shape = (frame["size"][1], frame["size"][0])

    # --- both men out of ONE pair still ----------------------------------------------------------
    masks = {"at": [], "sniper": []}
    scales = []
    for r in ROUNDS:
        stem = f"pair-{r}"
        noise = raw_diff(f"empty-{r}", f"floor-{r}")      # NOT dilated: it ate real man last round
        report[f"noise_px_{r}"] = int(noise.sum())
        men = drawn_men(stem, shape)
        boxes = {}
        for k, (arch, _) in WHO.items():
            got = men.get(arch, [])
            if len(got) != 1:
                problems.append(f"{stem}: the capture drew {len(got)} men of archetype {arch} in "
                                f"frame (the {k}), want exactly 1 - there is no single box to "
                                f"measure in")
                continue
            slot, x, y, tall = got[0]
            bm, span, cuts = box_of(x, y, tall, shape)
            boxes[k] = (bm, span, cuts)
            report[f"slot_{k}_{stem}"] = slot
            report[f"box_{k}_{stem}"] = list(span)
            report[f"px_per_m_{k}_{stem}"] = round(tall / MAN_TALL_M, 3)
            scales.append(tall / MAN_TALL_M)
            report[f"noise_in_box_px_{k}_{stem}"] = int((noise & bm).sum())
        extra = sorted(a for a in men if a not in (AT_ARCHETYPE, SNIPER_ARCHETYPE))
        if extra:
            problems.append(f"{stem}: the capture also drew archetype(s) {extra} in frame - this "
                            f"is not a still of those two men alone")
        if "at" in boxes and "sniper" in boxes and (boxes["at"][0] & boxes["sniper"][0]).any():
            problems.append(f"{stem}: the AT box {boxes['at'][1]} and the Sniper box "
                            f"{boxes['sniper'][1]} overlap - one mask would hold both men")
        for k in WHO:
            if k not in boxes:
                continue
            bm, _, cuts = boxes[k]
            raw = raw_diff(stem, f"empty-{r}") & bm
            cleaned = raw & ~noise
            report[f"dropped_px_{k}_{stem}"] = int(raw.sum() - cleaned.sum())
            m, px, second = biggest_two(cleaned)
            report[f"mask_px_{k}_{stem}"] = px
            report[f"second_blob_px_{k}_{stem}"] = second
            if m is None:
                problems.append(f"{stem}: nothing in the {k}'s box differs from the plate once the "
                                f"weather is dropped")
                continue
            masks[k].append((f"{k} of {stem}", m, px, second, cuts, int(bm.sum())))

    # --- one frame has one scale -----------------------------------------------------------------
    if scales:
        px_per_m = float(np.median(scales))
        report["px_per_m"] = round(px_per_m, 3)
        off = max(abs(s - px_per_m) / px_per_m for s in scales)
        report["px_per_m_disagreement"] = round(off, 4)
        if off > SCALE_TOLERANCE:
            problems.append(f"the stills disagree about the frame's scale by {off:.1%}, over "
                            f"{SCALE_TOLERANCE:.0%} - these are not one camera on one set of men")
        lo = int(round(MIN_AREA_M2 * px_per_m ** 2))
        report["mask_px_min"] = lo
    else:
        problems.append("no still gave a man a box, so the frame has no scale and no bounds")
        lo = 0

    at, sniper = [], []
    for k, bag in (("at", at), ("sniper", sniper)):
        for label, m, px, second, cuts, box_px in masks[k]:
            hi = int(round(MAX_BOX_SHARE * box_px))
            report["mask_px_max_" + label] = hi
            report["mask_m2_" + label] = round(px / px_per_m ** 2, 3) if scales else None
            kept = judge(label, m, px, second, lo, hi, cuts, problems)
            if kept is not None:
                bag.append(kept)

    # --- three live poses per man, read out of the SCORED frames ---------------------------------
    with open(here("poses.json"), encoding="utf-8") as f:
        poses = json.load(f)
    for who, (_, want_clip) in WHO.items():
        recs = [poses.get(f"pair-{r}", {}).get(who, "") for r in ROUNDS]
        ph = [read_pose(t, "phase") for t in recs]
        clips = [clip_of(t) for t in recs]
        report[f"{who}_phases"] = ph
        report[f"{who}_clips"] = clips
        wrong = [f"round {r}: {c}" for r, c in zip(ROUNDS, clips) if c != want_clip]
        if wrong:
            problems.append(f"the {who} in the pair still is not {want_clip} in every round ("
                            f"{', '.join(wrong)}) - a different clip is a different pose, "
                            f"not a different phase of one")
        if any(p is None for p in ph):
            problems.append(f"the {who} has no phase in every pair still ({ph}), so his poses "
                            f"cannot be told apart")
            continue
        gaps = [phase_gap(ph[i], ph[j]) for i in range(3) for j in range(i + 1, 3)]
        report[f"{who}_phase_gap_min"] = round(min(gaps), 4)
        if min(gaps) < MIN_PHASE_GAP:
            problems.append(f"the {who}'s {want_clip} pair phases {ph} come within {min(gaps):.3f} "
                            f"of each other, want {MIN_PHASE_GAP} - this is one pose, not three")

    for who, bag in (("at", at), ("sniper", sniper)):
        if len(bag) == 3:
            same = [f"{i + 1} and {j + 1}" for i in range(3) for j in range(i + 1, 3)
                    if np.array_equal(bag[i], bag[j])]
            if same:
                problems.append(f"the {who} masks of pair rounds " + ", ".join(same) +
                                " are pixel-identical - that is one frozen pose, not three "
                                "captures")

    # --- the honest limit, reported and not gated ------------------------------------------------
    heads = {}
    for r in ROUNDS:
        p = poses.get(f"pair-{r}", {})
        ya, ys = read_pose(p.get("at", ""), "yaw"), read_pose(p.get("sniper", ""), "yaw")
        if ya is not None and ys is not None:
            heads[f"pair-{r}"] = round(wrap(ya - ys), 4)
    report["heading_diff_rad"] = heads

    # One frame per round leaves no second still of the same man to compare him with, so the gate
    # is each man against HIMSELF ACROSS the rounds: a man who turned between pair-1 and pair-3 is
    # at a different heading, and a self-spread over three headings measures the turn, not the pose.
    for who in WHO:
        yaws = [read_pose(poses.get(f"pair-{r}", {}).get(who, ""), "yaw") for r in ROUNDS]
        if any(y is None for y in yaws):
            problems.append(f"the {who} has no yaw in every pair still ({yaws}), so his heading "
                            f"cannot be checked")
            continue
        offs = {f"pair-{r}": round(wrap(y - yaws[0]), 4) for r, y in zip(ROUNDS, yaws)}
        report[f"{who}_heading_offset_rad"] = offs
        bad = {k: v for k, v in offs.items() if v > HEADING_TOL}
        if bad:
            problems.append(f"the {who} is turned in {bad} rad against his heading in pair-1, over "
                            f"{HEADING_TOL} - that is a heading, not a pose")

    # --- the readings and the bar ----------------------------------------------------------------
    verdict = None
    if len(at) == 3 and len(sniper) == 3:
        readings = [diff(at[i], sniper[i]) for i in range(3)]
        report["silhouette_diff_readings"] = [round(x, 4) for x in readings]
        report["silhouette_diff"] = round(min(readings), 4)
        report["spread_readings"] = round(max(readings) - min(readings), 4)
        report["spread_at"] = round(spread(at), 4)
        report["spread_sniper"] = round(spread(sniper), 4)   # reported, NOT gated - see THE BAR
        report["spread_sniper_above_bar"] = report["spread_sniper"] >= PASS_DIFF
        worst = max(report["spread_readings"], report["spread_at"])
        report["spread_max"] = worst
        # The goal's words: the bar sits OUTSIDE the spread of three captures of the current build.
        report["bar_headroom"] = round(PASS_DIFF - worst, 4)
        if not worst < PASS_DIFF:
            verdict = (f"the spreads reach {worst}, not under {PASS_DIFF}: the measure is too "
                       f"noisy to carry the claim")
        elif not report["silhouette_diff"] >= PASS_DIFF:
            verdict = (f"the worst round reads {report['silhouette_diff']}, under {PASS_DIFF}: "
                       f"the AT rifleman does not read as his own man")
    elif not problems:
        verdict = "a mask was rejected, so there is no reading"

    report["problems"] = problems
    report["verdict"] = verdict or ("the prone AT rifleman reads as his own man" if not problems
                                    else "the set is not fit to measure")
    if WRITE_JSON:
        with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
            json.dump(report, f, indent=2)
            f.write("\n")
    print(json.dumps(report, indent=2))

    if problems:
        for p in problems:
            print("FAIL: " + p, file=sys.stderr)
        return 2
    if verdict:
        print("FAIL: " + verdict, file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--selftest":
        sys.exit(selftest())
    sys.exit(main())
