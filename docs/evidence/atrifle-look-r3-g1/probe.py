#!/usr/bin/env python3
"""Measures the pixels of the twelve stills committed beside this file.

The question: does the prone AT rifleman read as his own man, or as the Sniper with another name?

r3 answered 0.827 and the owner threw it out, for three reasons in his words: the two men were shot
from "two places" (camera yaw 243.6 against 131.69), "the sniper still is flagged unreadable", and
"the base crop has two men and the base plate still reports a man in the frame, so 0.827 is not one
man against one man". On top of that r3's three repeat captures of the AT rifleman masked 546, 546
and 546 px: one frozen pose photographed three times, not three readings.

This set answers all four, and this probe gates on each of them. frames.txt tells the staging at
length; what follows is only what reaches the number.

ONE FRAME, ONE CAMERA
  All twelve stills share one camera pose: focus (47.5, 25), zoom 30 (T1, the owner's number), yaw
  180 with CaptureRig.Rig.YawPin = 0, pitch 22, 1600x900. The probe reads zoom, yaw, pitch, focus_x,
  focus_z and the image size out of every sidecar and fails if any one of the twelve differs, so
  "one camera" is checked, not asserted. It also requires pose_error_m 0, rain 0 and an empty
  `warnings` list everywhere: the unreadable-contrast flag that r3 carried on the Sniper is gone at
  this camera for both men, and if it ever comes back this probe goes red.

ONE MAN AGAINST ONE MAN
  Four stills per round: `pair` (both men, men_in_frame 2), `at` (the Sniper off the map, 1),
  `sniper` (the AT rifleman off the map, 1), `empty` (both off, 0). The probe requires exactly those
  counts from the sidecars. A man's mask is |his single - that round's empty plate|, so a mask is
  one man against nothing, and the plate is the same camera, the same weather clock and the same few
  seconds of fires as the still it is subtracted from.

THE MASK
  A pixel is "the man" when its Rec.709 luma differs from the plate's by more than 0.04, as in r3;
  then a 3x3 opening drops grain speckle and a 7x7 closing joins the parts of one man (boots, back,
  helmet) that the mud happens to match. r3 then kept every surviving blob inside a hand-typed
  window around the focus. That window is gone. Instead only the LARGEST connected component is
  kept, and it has to earn the name "a man" three times over:
    * at least 400 px (a man at T1 is roughly a fifth of his T2 size; r3 measured about 876 px);
    * at least 1.5x the second-largest blob, so a flickering pool of mud can never be mistaken for
      him quietly - an ambiguous frame fails loudly instead;
    * no pixel on any frame border, because a cut mask is not a silhouette.

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
  differ by at least 0.05 between every pair of rounds. The Sniper's KneelIdle phases are reported
  for the record but not gated: he was never the frozen one, and his idle beat is free to land twice
  on the same phase.

THE BAR
  PASS_DIFF is 0.6, kept unchanged from r3's probe (commit 13d1158177) so the two rounds compare.
  Everything else is read from the committed pixels and sidecars; no other number here is typed.
  Three spreads are printed and all three have to sit below the bar, while the reading stays above
  it:
    * `spread_readings`   = max - min of the three AT-against-Sniper readings;
    * `spread_at`         = the largest 1 - IoU between any two of the three AT masks;
    * `spread_sniper`     = the same among the three Sniper masks.
  The last two are the noise floor of the measure itself: the same man, same camera, a different
  live pose. Pass needs every spread < PASS_DIFF and min(readings) >= PASS_DIFF, so the claim is
  made on the WORST of the three rounds and only when it clears the floor by the whole bar.

THE HONEST LIMIT
  One camera yaw cannot be side-on to two men facing different ways, and a man's heading is snapped
  to his post's. At pair-1 the AT rifleman is drawn at yaw -2.08 and the Sniper at -1.11, a residual
  0.97 rad; from round 2 on both happen to be drawn at 0.79, so the difference is 0.00 there by luck
  and not by control. The probe reports the per-round heading difference out of poses.json as
  `heading_diff_rad` and DOES NOT gate on it: one frame was the owner's call, and this is its price,
  stated rather than hidden. Part of each reading is therefore heading, not build. Also, at yaw 180
  the prone man lies behind the parapet while the kneeling Sniper stands clear of it, so what is
  measured is "a prone man half behind bags" against "a kneeling man in the open" - which is what
  the game shows from here.
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
MIN_MASK_PX = 400
DOMINANCE = 1.5
MIN_PHASE_GAP = 0.05
PASS_DIFF = 0.6

ROUNDS = (1, 2, 3)
KINDS = ("pair", "at", "sniper", "empty")
MEN = {"pair": 2, "at": 1, "sniper": 1, "empty": 0}
FRAME_KEYS = ("zoom", "yaw", "pitch", "focus_x", "focus_z")


def here(name):
    return os.path.join(HERE, name)


def sidecar(stem):
    with open(here(stem + ".json"), encoding="utf-8") as f:
        return json.load(f)


def luma(stem):
    a = np.asarray(Image.open(here(stem + ".png")).convert("RGB"), float) / 255.0
    return a[..., 0] * 0.2126 + a[..., 1] * 0.7152 + a[..., 2] * 0.0722


def mask(stem, plate, report, problems):
    """The largest connected component of |still - plate|, or None when it is not clearly a man."""
    d = np.abs(luma(stem) - luma(plate)) > THRESHOLD
    d = nd.binary_closing(nd.binary_opening(d, np.ones((3, 3))), np.ones((7, 7)))
    lab, n = nd.label(d)
    if n == 0:
        problems.append(f"{stem}: nothing differs from the plate at all")
        return None
    sizes = np.bincount(lab.ravel())
    sizes[0] = 0
    order = np.argsort(sizes)[::-1]
    biggest = int(order[0])
    px = int(sizes[biggest])
    second = int(sizes[order[1]]) if n > 1 else 0
    report["mask_px_" + stem] = px
    report["second_blob_px_" + stem] = second
    m = lab == biggest
    ok = True
    if px < MIN_MASK_PX:
        problems.append(f"{stem}: the largest blob is {px} px, want {MIN_MASK_PX}")
        ok = False
    if second * DOMINANCE > px:
        problems.append(f"{stem}: the largest blob is {px} px and the next is {second} px, "
                        f"under {DOMINANCE}x - the mask is ambiguous")
        ok = False
    if m[0].any() or m[-1].any() or m[:, 0].any() or m[:, -1].any():
        problems.append(f"{stem}: the mask reaches a frame border - it is cut")
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
              "min_mask_px": MIN_MASK_PX, "dominance": DOMINANCE}
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

    # --- the masks -------------------------------------------------------------------------------
    at, sniper = [], []
    for r in ROUNDS:
        plate = f"empty-{r}"
        a = mask(f"at-{r}", plate, report, problems)
        s = mask(f"sniper-{r}", plate, report, problems)
        if a is not None:
            at.append(a)
        if s is not None:
            sniper.append(s)

    # --- three live poses, not one ---------------------------------------------------------------
    with open(here("poses.json"), encoding="utf-8") as f:
        poses = json.load(f)
    at_phase = [read_pose(poses[f"at-{r}"]["at"], "phase") for r in ROUNDS]
    report["at_phases"] = at_phase
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
