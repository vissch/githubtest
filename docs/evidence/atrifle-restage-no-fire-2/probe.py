#!/usr/bin/env python3
"""Measures the pixels of the fifteen stills committed beside this file.

The question: does the prone AT rifleman read as his own man, or as the Sniper with another name?

r3 answered 0.827 and the owner threw it out, for three reasons in his words: the two men were shot
from "two places" (camera yaw 243.6 against 131.69), "the sniper still is flagged unreadable", and
"the base crop has two men and the base plate still reports a man in the frame, so 0.827 is not one
man against one man". On top of that r3's three repeat captures of the AT rifleman masked 546, 546
and 546 px: one frozen pose photographed three times, not three readings.

This set answers all four, and this probe gates on each of them. frames.txt tells the staging at
length; what follows is only what reaches the number.

ONE FRAME, ONE CAMERA
  All fifteen stills share one camera pose: focus (47.5, 25), zoom 30 (T1, the owner's number), yaw
  180 with CaptureRig.Rig.YawPin = 0, pitch 22, 1600x900. The probe reads zoom, yaw, pitch, focus_x,
  focus_z and the image size out of every sidecar and fails if any one of the fifteen differs, so
  "one camera" is checked, not asserted. It also requires pose_error_m 0, rain 0 and an empty
  `warnings` list everywhere.

ONE MAN AGAINST ONE MAN
  Five stills per round: `pair` (both men, men_in_frame 2), `at` (the Sniper off the map, 1),
  `sniper` (the AT rifleman off the map, 1), `empty` (both off, 0) and `floor` (both off again, 0).
  The probe requires exactly those counts, AND `drawn_infantry` 0 in every plate - round 1 of the
  previous set reported `drawn_infantry` 1 with `men_in_frame` 0, a man drawn while the plate
  claimed none, and a plate that holds a man is not a plate.

EACH MAN INSIDE HIS OWN BOX, FROM THE CAPTURE THAT DREW HIM
  The frame has braziers in it. A frame-wide reading measured a fire every time: leg 07 dropped the
  pixels that move on their own and the largest surviving blob was STILL a brazier, 2241-5501 px
  against a few hundred for the man. So the reading no longer looks at the whole frame. Every shot
  now records where the game drew each man - `manN_slot`, `manN_img_x`, `manN_img_y`, `manN_tall_px`
  - and this probe masks inside that box and nowhere else:

      x in img_x +- 0.8 * tall_px,    y in [img_y - 1.0 * tall_px, img_y + 0.2 * tall_px]

  (`img_y` counts from the top and is the man's FEET, hence the long reach upwards and the short one
  down.) The box is not typed here and not read off the pixels it is about to measure: it is the
  capture's own projection of the man it drew. Which slot is which man is read off the stills too -
  the one man in frame on `at-{r}` is the AT rifleman, the one on `sniper-{r}` is the Sniper - so no
  slot number is typed either. The two boxes must not overlap: the men are five metres apart, so
  "only each man" is checked rather than asserted, and if a restaging ever puts them on top of each
  other this probe goes red instead of quietly measuring both at once.

THE MASK
  A pixel is "the man" when its Rec.709 luma differs from that round's `empty` plate by more than
  0.04, as in r3; then a 3x3 opening drops grain speckle and a 7x7 closing joins the parts of one man
  (boots, back, helmet) that the mud happens to match. WHAT MOVES ON ITS OWN IS NOT THE MAN:
  `|empty-{r} - floor-{r}|` is two man-less plates of the same frame a few world-seconds apart, so
  everything in it is weather, and it is subtracted before any blob is labelled. It is NOT dilated -
  leg 07's 3x3 dilation ate 1.1k to 8.3k px of real man. `noise_in_box_px_*` reports how much of that
  weather falls inside each man's box at all; on this set it is 0 to 57 px, because the boxes sit
  well clear of the fires.

  Only the LARGEST surviving component inside the box is kept, and it has to earn the name "a man":
    * an area over 0.2 m2 of frame, and under half his own box. THE CEILING changed here. Leg 07
      wrote it as 2.0 m2 and read the scale off the distance between the two largest blobs, which
      were two fires: it had the frame 3x too small. With the capture's own scale a kneeling man at
      this camera measures 2.3-2.6 m2, so 2.0 m2 now rejects a man for being a man. What the ceiling
      is actually for is "the mask is not simply the whole patch", and the patch is his box, so it
      is now half of it - a man who fills half the box the renderer drew him in is not a
      silhouette. Both bounds are reported in pixels;
    * at least 1.5x the second-largest blob in the box, so an ambiguous mask fails loudly;
    * no pixel on a frame border, checked only along a box edge that the frame actually cuts - the
      box edge itself is not a border, and a mask that stops at it is not a cut silhouette.

  THE FRAME'S SCALE comes from the capture as well: `px_per_m = tall_px / 2.6`, a man being 2.6 m of
  world in the box the renderer drew. Leg 07 took it from the distance between the two largest blobs
  and read 44, 65 and 145 px/m for one frame whose scale is one number - it was measuring between two
  fires. The probe reports `px_per_m` for every still and fails if the rounds disagree by over 10 %.

THE READING
  The two men stand five metres apart in the same frame by design, so a raw pixel IoU between them
  would mostly measure that gap. Each mask is therefore translated so its centroid sits at the
  origin - translation only, no scale and no rotation, so the comparison is of shape and not of
  place, and a man who is simply bigger or turned still reads as different. The reading of a round
  is `1 - IoU` of the AT mask against the Sniper mask after that shift.

THREE LIVE POSES, NOT ONE
  Before each round the sim was let run for a different spell (0.4 s, 1.2 s, 2.0 s) and refrozen.
  The probe checks the result two ways: the three AT masks must not be pixel-identical (r3's were),
  and the AT rifleman's ProneIdle phase, read out of poses.json at the moment of each capture, must
  differ by at least 0.05 between every pair of rounds. The Sniper's kneeling phases are reported but
  not gated: he was never the frozen one.

THE BAR
  PASS_DIFF is 0.6, kept unchanged from r3's probe (commit 13d1158177) so the rounds compare. Three
  spreads are printed and all three have to sit below it while the reading stays above it:
    * `spread_readings`   = max - min of the three AT-against-Sniper readings;
    * `spread_at`         = the largest 1 - IoU between any two of the three AT masks;
    * `spread_sniper`     = the same among the three Sniper masks.
  The last two are the noise floor of the measure itself: the same man, same camera, a different live
  pose. Pass needs every spread < PASS_DIFF and min(readings) >= PASS_DIFF, so the claim is made on
  the WORST of the three rounds and only when it clears the floor by the whole bar.

THE HONEST LIMIT
  One camera yaw cannot be side-on to two men facing different ways, and a man's heading is snapped
  to his post's. The probe reports the per-round heading difference out of poses.json as
  `heading_diff_rad` and DOES NOT gate on it: one frame was the owner's call, and this is its price,
  stated rather than hidden. At yaw 180 the prone man also lies behind the parapet while the kneeling
  Sniper stands clear of it, so what is measured is "a prone man half behind bags" against "a
  kneeling man in the open" - which is what the game shows from here. And at zoom 30 the 1.65 m
  anti-tank rifle is a few dark pixels: no number here isolates it.
"""
import json
import os
import re
import sys

import numpy as np
import scipy.ndimage as nd
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 0.04
MAN_TALL_M = 2.6            # the height CaptureRig.ManBox projects, so tall_px is 2.6 m of frame
MIN_AREA_M2 = 0.2
MAX_BOX_SHARE = 0.5         # of his own box; see "the ceiling" in the docstring
BOX_HALF_W = 0.8            # of tall_px, either side of his feet
BOX_UP = 1.0                # of tall_px, above his feet
BOX_DOWN = 0.2              # of tall_px, below them
SCALE_TOLERANCE = 0.10      # how far the rounds' px_per_m may disagree
DOMINANCE = 1.5
MIN_PHASE_GAP = 0.05
PASS_DIFF = 0.6

ROUNDS = (1, 2, 3)
KINDS = ("pair", "at", "sniper", "empty", "floor")
MEN = {"pair": 2, "at": 1, "sniper": 1, "empty": 0, "floor": 0}
FRAME_KEYS = ("zoom", "yaw", "pitch", "focus_x", "focus_z")


def here(name):
    return os.path.join(HERE, name)


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
    """Every man the capture says it drew INSIDE this frame: slot -> (img_x, img_y, tall_px).

    A man written off the map still gets a projection, far outside the picture; those are dropped
    here, so "the men in this still" is the capture's own answer and not a typed slot number."""
    d = sidecar(stem)
    h, w = shape
    out = {}
    i = 0
    while ("man%d_slot" % i) in d:
        x, y = float(d["man%d_img_x" % i]), float(d["man%d_img_y" % i])
        tall = float(d["man%d_tall_px" % i])
        if 0 <= x < w and 0 <= y < h and tall > 1.0:
            out[int(d["man%d_slot" % i])] = (x, y, tall)
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


def centred(m):
    """The mask shifted so its centroid is at the origin, on a common canvas. No scale, no rotation."""
    h, w = m.shape
    ys, xs = np.nonzero(m)
    cy, cx = int(round(ys.mean())), int(round(xs.mean()))
    out = np.zeros((2 * h, 2 * w), bool)
    out[ys - cy + h, xs - cx + w] = True
    return out


def diff(a, b):
    """1 - IoU of two masks after each has been centred on its own centroid."""
    a, b = centred(a), centred(b)
    union = int((a | b).sum())
    return 1.0 - float((a & b).sum()) / union if union else 0.0


def spread(masks):
    return max(diff(masks[i], masks[j])
               for i in range(len(masks)) for j in range(i + 1, len(masks)))


def read_pose(text, field):
    m = re.search(field + r"\s+(-?\d+\.?\d*)", text)
    return float(m.group(1)) if m else None


def main():
    report = {"pass_diff": PASS_DIFF, "threshold_luma": THRESHOLD,
              "min_area_m2": MIN_AREA_M2, "max_box_share": MAX_BOX_SHARE,
              "dominance": DOMINANCE, "man_tall_m": MAN_TALL_M,
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
        # This set is the re-stage with no fire in the frame: the still must say so itself.
        if int(d.get("flames_in_frame", -1)) != 0:
            problems.append(f"{s}: flames_in_frame is {d.get('flames_in_frame')}, want 0")
        for knob in ("knob_maxFires", "knob_maxTorches", "knob_moreFires"):
            if float(d.get(knob, -1)) != 0:
                problems.append(f"{s}: {knob} is {d.get(knob)}, want 0")
        kind = s.rsplit("-", 1)[0]
        if int(d["men_in_frame"]) != MEN[kind]:
            problems.append(f"{s}: men_in_frame is {d['men_in_frame']}, want {MEN[kind]}")
        if MEN[kind] == 0 and int(d.get("drawn_infantry", 0)) != 0:
            problems.append(f"{s}: a plate with no man in frame still drew "
                            f"{d['drawn_infantry']} - that is not a plate")

    shape = (frame["size"][1], frame["size"][0])

    # --- who is who, and where, from the captures themselves -------------------------------------
    masks = {"at": [], "sniper": []}
    scales = []
    for r in ROUNDS:
        noise = raw_diff(f"empty-{r}", f"floor-{r}")      # NOT dilated: it ate real man last round
        report[f"noise_px_{r}"] = int(noise.sum())
        boxes = {}
        for k in ("at", "sniper"):
            stem = f"{k}-{r}"
            men = drawn_men(stem, shape)
            if len(men) != 1:
                problems.append(f"{stem}: the capture drew {len(men)} men in frame, want 1 - "
                                f"there is no single box to measure in")
                boxes[k] = None
                continue
            slot, (x, y, tall) = next(iter(men.items()))
            bm, span, cuts = box_of(x, y, tall, shape)
            boxes[k] = (bm, span, cuts)
            report[f"slot_{stem}"] = slot
            report[f"box_{stem}"] = list(span)
            report[f"px_per_m_{stem}"] = round(tall / MAN_TALL_M, 3)
            scales.append(tall / MAN_TALL_M)
            report[f"noise_in_box_px_{stem}"] = int((noise & bm).sum())
        if boxes.get("at") and boxes.get("sniper") and (boxes["at"][0] & boxes["sniper"][0]).any():
            problems.append(f"round {r}: the AT box {boxes['at'][1]} and the Sniper box "
                            f"{boxes['sniper'][1]} overlap - a mask would hold both men")
        for k in ("at", "sniper"):
            if boxes.get(k) is None:
                continue
            bm, _, cuts = boxes[k]
            stem = f"{k}-{r}"
            raw = raw_diff(stem, f"empty-{r}") & bm
            cleaned = raw & ~noise
            report[f"dropped_px_{stem}"] = int(raw.sum() - cleaned.sum())
            m, px, second = biggest_two(cleaned)
            report[f"mask_px_{stem}"] = px
            report[f"second_blob_px_{stem}"] = second
            if m is None:
                problems.append(f"{stem}: nothing in his box differs from the plate once the "
                                f"weather is dropped")
                continue
            masks[k].append((stem, m, px, second, cuts, int(bm.sum())))

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
        for stem, m, px, second, cuts, box_px in masks[k]:
            hi = int(round(MAX_BOX_SHARE * box_px))
            report["mask_px_max_" + stem] = hi
            report["mask_m2_" + stem] = round(px / px_per_m ** 2, 3) if scales else None
            kept = judge(stem, m, px, second, lo, hi, cuts, problems)
            if kept is not None:
                bag.append(kept)

    # --- three live poses, not one ---------------------------------------------------------------
    with open(here("poses.json"), encoding="utf-8") as f:
        poses = json.load(f)
    at_phase = [read_pose(poses[f"at-{r}"]["at"], "phase") for r in ROUNDS]
    report["at_phases"] = at_phase
    report["at_clips"] = [re.search(r"animation clip (\S+)", poses[f"at-{r}"]["at"]).group(1)
                          for r in ROUNDS]
    report["sniper_phases"] = [read_pose(poses[f"sniper-{r}"]["sniper"], "phase") for r in ROUNDS]
    gaps = [abs(at_phase[i] - at_phase[j]) for i in range(3) for j in range(i + 1, 3)]
    report["at_phase_gap_min"] = round(min(gaps), 4)
    if min(gaps) < MIN_PHASE_GAP:
        problems.append(f"the AT rifleman's ProneIdle phases {at_phase} come within "
                        f"{min(gaps):.3f} of each other, want {MIN_PHASE_GAP} - this is one pose")
    if len(at) == 3:
        same = [f"{i + 1} and {j + 1}" for i in range(3) for j in range(i + 1, 3)
                if np.array_equal(at[i], at[j])]
        if same:
            problems.append("the AT masks of rounds " + ", ".join(same) +
                            " are pixel-identical - that is one frozen pose, not three captures")

    # --- the honest limit, reported and not gated ------------------------------------------------
    heads = {}
    for r in ROUNDS:
        p = poses.get(f"pair-{r}", {})
        ya, ys = read_pose(p.get("at", ""), "yaw"), read_pose(p.get("sniper", ""), "yaw")
        if ya is not None and ys is not None:
            heads[f"pair-{r}"] = round(float(abs((ya - ys + np.pi) % (2 * np.pi) - np.pi)), 4)
    report["heading_diff_rad"] = heads

    # --- the readings and the bar ----------------------------------------------------------------
    verdict = None
    if len(at) == 3 and len(sniper) == 3:
        readings = [diff(at[i], sniper[i]) for i in range(3)]
        report["silhouette_diff_readings"] = [round(x, 4) for x in readings]
        report["silhouette_diff"] = round(min(readings), 4)
        report["spread_readings"] = round(max(readings) - min(readings), 4)
        report["spread_at"] = round(spread(at), 4)
        report["spread_sniper"] = round(spread(sniper), 4)
        worst = max(report["spread_readings"], report["spread_at"], report["spread_sniper"])
        report["spread_max"] = worst
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
    with open(here("probe.json"), "w", encoding="utf-8") as f:
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
    sys.exit(main())
