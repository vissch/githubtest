#!/usr/bin/env python3
"""Measures the pixels of the fifteen stills committed beside this file.

The question: does the prone AT rifleman read as his own man, and can a player see him at all?

Round 2 (the owner's words): "at zoom 30 yaw 180 the prone AT rifleman is a dark lump in the
sandbag gap. at-2 looks empty, his mask is 286 to 383 px, and the kneeling man at 1327 to 1633 px
is the only figure a player can see. The 0.77 reading is the bags plus a fixed 1.11 rad heading,
not an AT rifleman." So three asks became three new gates here, on top of everything r3's probe
already checked: the prone body has to be in front of the bags and mask at least half the kneel's,
no weather may fall in either man's box at all, and the heading difference has to be near zero so a
reading can no longer be a man against a man turned a radian away.

THE GATE IS WRITTEN BEFORE THE SET IS SHOT. This file is committed on its own, red, with none of
the fifteen stills in place (it exits 2, "the set is incomplete"). The numbers it demands therefore
cannot have been tuned to the numbers the shoot came back with. Where each bound comes from:
  * PASS_DIFF 0.6 is r3's bar (commit 13d1158177), unchanged so the rounds compare;
  * MIN_MASK_RATIO 0.5 is the owner's "half the kneel's";
  * MAX_NOISE_IN_BOX 0 is the owner's "clear the 1057 weather pixels", and part 1 of this round
    measured 0 px in either box when two man-less plates were shot 7 frames apart (frames.txt);
  * MAX_HEADING_DIFF 0.15 rad throws out the fixed 1.11 rad he rejected; the staging sweep measured
    0.00 rad at the place this set is shot from (staging.json, candidate 9).

The measuring code itself lives in mask.py, shared with staging.py, so the sweep that chose the
place and this probe measure the same way: the same luma threshold 0.04, the same 3x3 opening and
7x7 closing, the same box the capture projected for the man it drew, the same largest-component
rule and the same centroid-aligned 1-IoU.

ONE FRAME, ONE CAMERA
  All fifteen stills share one camera pose: focus (47.5, 25), zoom 30 (T1, the owner's number), yaw
  180 with CaptureRig.Rig.YawPin = 0, pitch 22, 1600x900. The probe reads zoom, yaw, pitch, focus_x,
  focus_z and the image size out of every sidecar and fails if any one of the fifteen differs, so
  "one camera" is checked, not asserted. It also requires pose_error_m 0, rain 0 and an empty
  `warnings` list everywhere.

ONE MAN AGAINST ONE MAN
  Five stills per round: `pair` (both men, men_in_frame 2), `at` (the Sniper off the map, 1),
  `sniper` (the AT rifleman off the map, 1), `empty` (both off, 0) and `floor` (both off again, 0).
  The probe requires exactly those counts, AND `drawn_infantry` 0 in every plate - a man drawn while
  the plate claims none is not a plate.

EACH MAN INSIDE HIS OWN BOX, FROM THE CAPTURE THAT DREW HIM
  The frame has braziers in it, and a frame-wide reading measured a fire every time. So every shot
  records where the game drew each man - `manN_slot`, `manN_img_x`, `manN_img_y`, `manN_tall_px` -
  and this probe masks inside that box and nowhere else:

      x in img_x +- 0.8 * tall_px,    y in [img_y - 1.0 * tall_px, img_y + 0.2 * tall_px]

  (`img_y` counts from the top and is the man's FEET, hence the long reach upwards and the short one
  down.) The box is not typed here and not read off the pixels it is about to measure: it is the
  capture's own projection of the man it drew. Which slot is which man is read off the stills too -
  the one man in frame on `at-{r}` is the AT rifleman, the one on `sniper-{r}` is the Sniper - so no
  slot number is typed either. The two boxes must not overlap.

THE MASK
  A pixel is "the man" when its Rec.709 luma differs from that round's `empty` plate by more than
  0.04, as in r3; then a 3x3 opening drops grain speckle and a 7x7 closing joins the parts of one
  man (boots, back, helmet) that the mud happens to match. WHAT MOVES ON ITS OWN IS NOT THE MAN:
  `|empty-{r} - floor-{r}|` is two man-less plates of the same frame, so everything in it is
  weather, and it is subtracted before any blob is labelled. It is NOT dilated - a 3x3 dilation ate
  1.1k to 8.3k px of real man in leg 07. `noise_in_box_px_*` reports how much of that weather falls
  inside each man's box, and now has to be 0 for both men in all three rounds: the owner asked for
  his box cleared, so the box is cleared rather than the gate widened.

  Only the LARGEST surviving component inside the box is kept, and it has to earn the name "a man":
    * an area over 0.2 m2 of frame, and under half his own box (a man who fills half the box the
      renderer drew him in is not a silhouette). Both bounds are reported in pixels;
    * at least 1.5x the second-largest blob in the box, so an ambiguous mask fails loudly;
    * no pixel on a frame border, checked only along a box edge that the frame actually cuts - the
      box edge itself is not a border, and a mask that stops at it is not a cut silhouette.

  THE FRAME'S SCALE comes from the capture as well: `px_per_m = tall_px / 2.6`, a man being 2.6 m of
  world in the box the renderer drew. The probe reports `px_per_m` for every still and fails if the
  rounds disagree by over 10 %.

A MAN A PLAYER CAN SEE
  `mask_ratio` is `mask_px_at / mask_px_sniper` for each round, and every round must be at least
  MIN_MASK_RATIO. That is the owner's ask turned into a number: his 286-383 px against the kneel's
  1327-1633 px was a ratio near 0.2, a dark lump in a gap. Nothing about the measure changed to
  raise it - the body was staged in front of the parapet instead of behind it.

THE READING
  The two men stand a few metres apart in the same frame by design, so a raw pixel IoU between them
  would mostly measure that gap. Each mask is therefore translated so its centroid sits at the
  origin - translation only, no scale and no rotation, so the comparison is of shape and not of
  place, and a man who is simply bigger or turned still reads as different. The reading of a round
  is `1 - IoU` of the AT mask against the Sniper mask after that shift.

THREE LIVE POSES, NOT ONE
  Before each round the sim is let run for a different spell and refrozen. The probe checks the
  result two ways: the three AT masks must not be pixel-identical (r3's were), and the AT
  rifleman's ProneIdle phase, read out of poses.json at the moment of each capture, must differ by
  at least 0.05 between every pair of rounds. The Sniper's kneeling phases are reported but not
  gated: he was never the frozen one.

THE HEADING
  r3 reported the per-round heading difference and did not gate on it; the owner threw the reading
  out for exactly that. It is now a gate: `heading_diff_rad` per round, out of poses.json, must be
  at most 0.15 rad. Two men facing the same way at one camera yaw is the only way a shape difference
  can be about the men and not about where they happen to be pointing.

THE BAR
  PASS_DIFF is 0.6, kept unchanged from r3's probe (commit 13d1158177) so the rounds compare. Three
  spreads are printed and all three have to sit below it while the reading stays above it:
    * `spread_readings`   = max - min of the three AT-against-Sniper readings;
    * `spread_at`         = the largest 1 - IoU between any two of the three AT masks;
    * `spread_sniper`     = the same among the three Sniper masks.
  The last two are the noise floor of the measure itself: the same man, same camera, a different
  live pose. Pass needs every spread < PASS_DIFF and min(readings) >= PASS_DIFF, so the claim is
  made on the WORST of the three rounds and only when it clears the floor by the whole bar.

THE HONEST LIMIT
  At zoom 30 the 1.65 m anti-tank rifle is a few dark pixels, and no number here isolates it: what
  is measured is a prone body against a kneeling one, not a weapon. One seed, one ground, one
  camera.
"""
import json
import os
import re
import sys

import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import mask as M                                                      # noqa: E402  shared measure
from PIL import Image                                                 # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
SCALE_TOLERANCE = 0.10      # how far the rounds' px_per_m may disagree
MIN_PHASE_GAP = 0.05
PASS_DIFF = 0.6             # r3's bar, commit 13d1158177
MIN_MASK_RATIO = 0.5        # the owner: his mask at least half the kneel's
MAX_NOISE_IN_BOX = 0        # the owner: clear the weather out of his box
MAX_HEADING_DIFF = 0.15     # rad: the 1.11 rad he threw out can be no part of a reading

ROUNDS = (1, 2, 3)
KINDS = ("pair", "at", "sniper", "empty", "floor")
MEN = {"pair": 2, "at": 1, "sniper": 1, "empty": 0, "floor": 0}
FRAME_KEYS = ("zoom", "yaw", "pitch", "focus_x", "focus_z")


def here(name):
    return os.path.join(HERE, name)


def judge(stem, m, px, second, lo, hi, cuts, problems):
    """Does the largest component in the box earn the name "a man"? Size, dominance, not cut."""
    if m is None:
        return None
    ok = True
    if px < lo:
        problems.append(f"{stem}: the largest blob in his box is {px} px, under the frame's own "
                        f"floor of {lo} px ({M.MIN_AREA_M2} m2) - too small to be a man")
        ok = False
    if px > hi:
        problems.append(f"{stem}: the largest blob in his box is {px} px, over {hi} px - more "
                        f"than {M.MAX_BOX_SHARE:.0%} of his own box, so it is the patch not him")
        ok = False
    if second * M.DOMINANCE > px:
        problems.append(f"{stem}: the largest blob in his box is {px} px and the next is {second} "
                        f"px, under {M.DOMINANCE}x - the mask is ambiguous")
        ok = False
    touched = [side for side, cut in cuts.items() if cut and (
        (side == "left" and m[:, 0].any()) or (side == "right" and m[:, -1].any()) or
        (side == "top" and m[0].any()) or (side == "bottom" and m[-1].any()))]
    if touched:
        problems.append(f"{stem}: the mask reaches the {', '.join(touched)} frame border - cut")
        ok = False
    return m if ok else None


def spread(masks):
    return max(M.diff(masks[i], masks[j])
               for i in range(len(masks)) for j in range(i + 1, len(masks)))


def read_pose(text, field):
    m = re.search(field + r"\s+(-?\d+\.?\d*)", text)
    return float(m.group(1)) if m else None


def main():
    report = {"pass_diff": PASS_DIFF, "threshold_luma": M.THRESHOLD,
              "min_area_m2": M.MIN_AREA_M2, "max_box_share": M.MAX_BOX_SHARE,
              "dominance": M.DOMINANCE, "man_tall_m": M.MAN_TALL_M,
              "min_mask_ratio": MIN_MASK_RATIO, "max_noise_in_box_px": MAX_NOISE_IN_BOX,
              "max_heading_diff_rad": MAX_HEADING_DIFF,
              "box": {"half_width_x_tall": M.BOX_HALF_W, "up_x_tall": M.BOX_UP,
                      "down_x_tall": M.BOX_DOWN}}
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
        d = M.sidecar(HERE, s)
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

    # --- who is who, and where, from the captures themselves -------------------------------------
    found = {"at": [], "sniper": []}
    scales = []
    for r in ROUNDS:
        noise = M.raw_diff(HERE, f"empty-{r}", f"floor-{r}")  # NOT dilated: it ate real man once
        report[f"noise_px_{r}"] = int(noise.sum())
        got = {}
        for k in ("at", "sniper"):
            stem = f"{k}-{r}"
            got[k] = res = M.man_in_box(HERE, stem, f"empty-{r}", noise, shape)
            if "slot" not in res:
                problems.append(f"{stem}: {res['why']} - there is no single box to measure in")
                continue
            report[f"slot_{stem}"] = res["slot"]
            report[f"box_{stem}"] = res["box"]
            report[f"px_per_m_{stem}"] = round(res["px_per_m"], 3)
            report[f"noise_in_box_px_{stem}"] = res["noise_in_box_px"]
            report[f"dropped_px_{stem}"] = res["dropped_px"]
            report[f"mask_px_{stem}"] = res["mask_px"]
            report[f"second_blob_px_{stem}"] = res["second_px"]
            scales.append(res["px_per_m"])
            if res["noise_in_box_px"] > MAX_NOISE_IN_BOX:
                problems.append(f"{stem}: {res['noise_in_box_px']} weather pixels fall inside his "
                                f"box, want {MAX_NOISE_IN_BOX} - the owner asked for it cleared")
            if res["mask"] is None:
                problems.append(f"{stem}: {res['why']} once the weather is dropped")
                continue
            found[k].append((r, res))
        if got.get("at", {}).get("box") and got.get("sniper", {}).get("box"):
            ax0, ax1, ay0, ay1 = got["at"]["box"]
            sx0, sx1, sy0, sy1 = got["sniper"]["box"]
            if ax0 < sx1 and sx0 < ax1 and ay0 < sy1 and sy0 < ay1:
                problems.append(f"round {r}: the AT box {got['at']['box']} and the Sniper box "
                                f"{got['sniper']['box']} overlap - a mask would hold both men")
        if got.get("at", {}).get("mask_px") and got.get("sniper", {}).get("mask_px"):
            ratio = got["at"]["mask_px"] / got["sniper"]["mask_px"]
            report[f"mask_ratio_{r}"] = round(ratio, 4)
            if ratio < MIN_MASK_RATIO:
                problems.append(f"round {r}: his mask is {got['at']['mask_px']} px against the "
                                f"kneel's {got['sniper']['mask_px']} px, a ratio of {ratio:.3f} "
                                f"under {MIN_MASK_RATIO} - a player cannot see him")

    # --- one frame has one scale -----------------------------------------------------------------
    if scales:
        px_per_m = float(np.median(scales))
        report["px_per_m"] = round(px_per_m, 3)
        off = max(abs(s - px_per_m) / px_per_m for s in scales)
        report["px_per_m_disagreement"] = round(off, 4)
        if off > SCALE_TOLERANCE:
            problems.append(f"the stills disagree about the frame's scale by {off:.1%}, over "
                            f"{SCALE_TOLERANCE:.0%} - these are not one camera on one set of men")
        lo = int(round(M.MIN_AREA_M2 * px_per_m ** 2))
        report["mask_px_min"] = lo
    else:
        problems.append("no still gave a man a box, so the frame has no scale and no bounds")
        px_per_m, lo = 0.0, 0

    at, sniper = [], []
    for k, bag in (("at", at), ("sniper", sniper)):
        for r, res in found[k]:
            stem = f"{k}-{r}"
            hi = int(round(M.MAX_BOX_SHARE * res["box_px"]))
            report["mask_px_max_" + stem] = hi
            report["mask_m2_" + stem] = round(res["mask_px"] / px_per_m ** 2, 3) if scales else None
            kept = judge(stem, res["mask"], res["mask_px"], res["second_px"], lo, hi, res["cuts"],
                         problems)
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

    # --- the heading, now a gate -----------------------------------------------------------------
    heads = {}
    for r in ROUNDS:
        p = poses.get(f"pair-{r}", {})
        ya, ys = read_pose(p.get("at", ""), "yaw"), read_pose(p.get("sniper", ""), "yaw")
        if ya is None or ys is None:
            problems.append(f"pair-{r}: poses.json does not give both men a yaw, so the heading "
                            f"difference cannot be checked")
            continue
        h = round(float(abs((ya - ys + np.pi) % (2 * np.pi) - np.pi)), 4)
        heads[f"pair-{r}"] = h
        if h > MAX_HEADING_DIFF:
            problems.append(f"pair-{r}: the men's headings differ by {h} rad, over "
                            f"{MAX_HEADING_DIFF} - this reading would be about where they point")
    report["heading_diff_rad"] = heads

    # --- the readings and the bar ----------------------------------------------------------------
    verdict = None
    if len(at) == 3 and len(sniper) == 3:
        readings = [M.diff(at[i], sniper[i]) for i in range(3)]
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
    report["verdict"] = verdict or ("the prone AT rifleman reads as his own man, and a player can "
                                    "see him" if not problems else "the set is not fit to measure")
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
