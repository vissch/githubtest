"""Measures the pixels of the fifteen stills committed beside this file.

The question: does the prone AT rifleman read as his own man, or as the Sniper with another name?

WHAT THIS ROUND FIXES
  The previous set (docs/evidence/atrifle-look-r3-g1-g1/) read 0.770 and the owner threw it out:
  "the three KneelIdle stills are one pose". He was right, and that set's own probe.json says so in
  two numbers: `sniper_clips ["KneelIdle","KneelIdle","KneelIdle"]` and `spread_sniper 0.1605`. The
  phases 0.15, 0.48 and 0.80 cleared every gate the old probe had - one clip, a 0.32 phase gap, no
  turn - and the crops of sniper-1, sniper-2 and sniper-3 are still the same kneel to the eye. The
  0.16 is that frozen-looking man plus the 40 weather pixels the old probe itself reported inside
  sniper-2's box, so the 0.6 bar sat 0.44 outside a spread the player cannot see. A phase gap on one
  looping clip is not a pose: KneelIdle is a 1.7 s crouch-walk loop with almost no travel in the
  silhouette, so three phases of it are three nearly identical men.

  This round shoots three kneels a player can tell apart, and this probe replaces the Sniper's
  "one clip, three phases" gates with four that only a visibly different pose passes:

    * `SNIPER_CLIP` is gone. A kneel is read off the trace's own word instead: every `sniper-{r}`
      line must say `stance Crouch`, and the three clip names must be THREE DIFFERENT clips;
    * `PHASE_BAND` (0.10, 0.80): each sniper phase must sit inside it. `KneelInspect` (3.2 s) and
      `KneelLookAround` (5.3 s) are once-clips (`AnimationController.Clips.Table`, `O(...)`) and
      hold their last frame, and the set the owner threw out two rounds ago was three JumpDown
      stills at 0.87-0.89 - exactly that held frame. The band stops a still shot after its clip ended;
    * `MIN_POSE_SPREAD` 0.25: each of the three pairwise `1 - IoU` values among the sniper masks
      must be at or above it, reported as `sniper_pose_spreads`. 0.25 is above the 0.16 the owner
      calls invisible and well under the bar;
    * `MIN_NOISE_RATIO` 10: each sniper pair also reports `pose_diff_px`, the symmetric difference
      of the two centred masks, and it must be at least 10x the larger of the two rounds'
      `noise_in_box_px_sniper-{r}`. So a pair can never pass on weather: the old set's 40 noise
      pixels would need 400 px of moved man.

  The squeeze is deliberate and it is from both sides: the Sniper's self-spread must now be AT LEAST
  0.25 and still UNDER 0.6. A set of kneels that spreads past the bar is a red probe and a note -
  never a lower bar. PASS_DIFF stays 0.6, untouched since commit 13d1158177, so the rounds compare.

  The AT rifleman's gates are unchanged: `AT_CLIP` ProneIdle in all three rounds and a 0.2 phase
  gap. His spread (0.0782) was never the complaint, and ProneIdle is a 4.9 s loop of a man lying
  down - three phases of it are three phases of one lying man, which is what this set compares
  against.

  frames.txt tells the staging at length; what follows is only what reaches the number.

ONE FRAME, ONE CAMERA
  All fifteen stills share one camera pose: focus (47.5, 25), zoom 30 (T1, the owner's number), yaw
  180 with CaptureRig.Rig.YawPin = 0, pitch 22, 1600x900. The probe reads zoom, yaw, pitch, focus_x,
  focus_z and the image size out of every sidecar and fails if any one of the fifteen differs, so
  "one camera" is checked, not asserted. It also requires pose_error_m 0, rain 0 and an empty
  `warnings` list everywhere.

ONE MAN AGAINST ONE MAN
  Five stills per round: `pair` (both men, men_in_frame 2), `at` (the Sniper off the map, 1),
  `sniper` (the AT rifleman off the map, 1), `empty` (both off, 0) and `floor` (both off again, 0).
  The probe requires exactly those counts, AND `drawn_infantry` 0 in every plate - a plate that
  holds a man is not a plate.

EACH MAN INSIDE HIS OWN BOX, FROM THE CAPTURE THAT DREW HIM
  The frame has braziers in it, and a frame-wide reading measures a fire every time: an earlier leg
  dropped everything that moves on its own and the largest surviving blob was STILL a brazier,
  2241-5501 px against a few hundred for the man. So the reading does not look at the whole frame.
  Every shot records where the game drew each man - `manN_slot`, `manN_img_x`, `manN_img_y`,
  `manN_tall_px` - and this probe masks inside that box and nowhere else:

      x in img_x +- 0.8 * tall_px,    y in [img_y - 1.0 * tall_px, img_y + 0.2 * tall_px]

  (`img_y` counts from the top and is the man's FEET, hence the long reach upwards and the short one
  down.) The box is not typed here and not read off the pixels it is about to measure: it is the
  capture's own projection of the man it drew. Which slot is which man is read off the stills too -
  the one man in frame on `at-{r}` is the AT rifleman, the one on `sniper-{r}` is the Sniper - so no
  slot number is typed either. The two boxes must not overlap: the men are five metres apart, so
  "only each man" is checked rather than asserted.

THE MASK
  A pixel is "the man" when its Rec.709 luma differs from that round's `empty` plate by more than
  0.04; then a 3x3 opening drops grain speckle and a 7x7 closing joins the parts of one man (boots,
  back, helmet) that the mud happens to match. WHAT MOVES ON ITS OWN IS NOT THE MAN:
  `|empty-{r} - floor-{r}|` is two man-less plates of the same frame a few world-seconds apart, so
  everything in it is weather, and it is subtracted before any blob is labelled. It is NOT dilated -
  a 3x3 dilation ate 1.1k to 8.3k px of real man. `noise_in_box_px_*` reports how much of that
  weather falls inside each man's box at all.

  Only the LARGEST surviving component inside the box is kept, and it has to earn the name "a man":
    * an area over 0.2 m2 of frame, and under half his own box - a man who fills half the box the
      renderer drew him in is not a silhouette. Both bounds are reported in pixels;
    * at least 1.5x the second-largest blob in the box, so an ambiguous mask fails loudly;
    * no pixel on a frame border, checked only along a box edge that the frame actually cuts - the
      box edge itself is not a border, and a mask that stops at it is not a cut silhouette.

  THE FRAME'S SCALE comes from the capture as well: `px_per_m = tall_px / 2.6`, a man being 2.6 m of
  world in the box the renderer drew. The probe reports `px_per_m` for every still and fails if the
  rounds disagree by over 10 %.

THE READING
  The two men stand five metres apart in the same frame by design, so a raw pixel IoU between them
  would mostly measure that gap. Each mask is therefore translated so its centroid sits at the
  origin - translation only, no scale and no rotation, so the comparison is of shape and not of
  place, and a man who is simply bigger or turned still reads as different. The reading of a round
  is `1 - IoU` of the AT mask against the Sniper mask after that shift.

THREE LIVE POSES PER MAN, NOT ONE
  Before each round the sim was let run for a different spell and refrozen. The AT rifleman is
  driven to a target phase of ProneIdle per round; the Sniper is driven to a DIFFERENT KNEEL per
  round - a different clip of `stance Crouch`, not another phase of one - and each still is shot
  inside the phase band so no once-clip is caught on its held last frame. Nothing is written to the
  world during a drift: a staging write is what pinned the phases two rounds ago. The probe checks
  the AT man with his clip gate, the 0.2 phase gap and the heading gate, and the Sniper with the
  stance gate, the three-distinct-clips gate, the phase band, `sniper_pose_spreads` >= 0.25 against
  his own noise, and the same heading gate. The three AT masks must also not be pixel-identical.

THE BAR
  PASS_DIFF is 0.6. Three spreads are printed and all three have to sit below it while the reading
  stays above it:
    * `spread_readings`   = max - min of the three AT-against-Sniper readings;
    * `spread_at`         = the largest 1 - IoU between any two of the three AT masks;
    * `spread_sniper`     = the same among the three Sniper masks.
  The last two are the noise floor of the measure itself: the same man, same camera, a different
  live pose. `spread_sniper` is now squeezed from BOTH sides - its three pairwise values each have
  to reach MIN_POSE_SPREAD 0.25, because three kneels a player cannot tell apart are not three
  poses, while the whole spread still has to stay under PASS_DIFF. Pass needs every spread
  < PASS_DIFF (so `bar_headroom` > 0) and min(readings) >= PASS_DIFF, so the claim is made on the
  WORST of the three rounds and only when it clears the floor by the whole bar.

THE HONEST LIMIT
  One camera yaw cannot be side-on to two men facing different ways, and a man's heading is snapped
  to his post's. The probe reports the per-round AT-against-Sniper heading difference as
  `heading_diff_rad` and DOES NOT gate on it: one frame was the owner's call, and this is its price,
  stated rather than hidden. (What IS gated is each man against HIMSELF across the stills of one
  round - a man must not turn between them.) At yaw 180 the prone man also lies behind the parapet
  while the kneeling Sniper stands clear of it, so what is measured is "a prone man half behind
  bags" against "a kneeling man in the open" - which is what the game shows from here. And at zoom
  30 the 1.65 m anti-tank rifle is a few dark pixels: no number here isolates it.
"""
import json
import os
import re
import sys

import numpy as np
import scipy.ndimage as nd
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
# With no argument the probe measures the set committed beside it, which is what the goal runs. An
# argument points it at another set - that is how this round's gates are shown to bite on the set
# the owner threw out. probe.json is only written when it is measuring its own folder, so pointing
# it at an older set cannot overwrite that set's committed numbers.
DIR = os.path.abspath(sys.argv[1]) if len(sys.argv) > 1 else HERE
THRESHOLD = 0.04
MAN_TALL_M = 2.6            # the height CaptureRig.ManBox projects, so tall_px is 2.6 m of frame
MIN_AREA_M2 = 0.2
MAX_BOX_SHARE = 0.5         # of his own box; see "the ceiling" in the docstring
BOX_HALF_W = 0.8            # of tall_px, either side of his feet
BOX_UP = 1.0                # of tall_px, above his feet
BOX_DOWN = 0.2              # of tall_px, below them
SCALE_TOLERANCE = 0.10      # how far the rounds' px_per_m may disagree
DOMINANCE = 1.5
MIN_PHASE_GAP = 0.2       # the AT man's gate, the owner's number two rounds ago; untouched here
HEADING_TOL = 0.05       # rad a man's yaw in his own still may differ from his yaw in the pair still
AT_CLIP = "ProneIdle"
SNIPER_STANCE = "Crouch"   # a kneel in the trace's own word, instead of one typed clip name
PHASE_BAND = (0.10, 0.80)  # a once-clip holds its last frame; a still outside the band may be it
MIN_POSE_SPREAD = 0.25     # each pairwise 1 - IoU among the sniper masks: poses, not phases
MIN_NOISE_RATIO = 10       # px of moved man per px of weather in the pair's worse box
PASS_DIFF = 0.6          # unchanged since commit 13d1158177, so the rounds compare

ROUNDS = (1, 2, 3)
KINDS = ("pair", "at", "sniper", "empty", "floor")
MEN = {"pair": 2, "at": 1, "sniper": 1, "empty": 0, "floor": 0}
FRAME_KEYS = ("zoom", "yaw", "pitch", "focus_x", "focus_z")


def here(name):
    return os.path.join(DIR, name)


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


def clip_of(text):
    m = re.search(r"animation clip (\S+)", text)
    return m.group(1) if m else "?"


def stance_of(text):
    """The stance on the trace's `animation clip ...` line: the pose's own word for a kneel."""
    m = re.search(r"animation clip \S+ rung \S+ stance (\S+)", text)
    return m.group(1) if m else "?"


def phase_gap(a, b):
    """Distance between two phases on a LOOPING clip: 0.98 and 0.02 are 0.04 apart, not 0.96."""
    d = abs(a - b) % 1.0
    return min(d, 1.0 - d)


def wrap(a):
    """An angle folded into (-pi, pi]."""
    return float(abs((a + np.pi) % (2 * np.pi) - np.pi))


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
    # The AT rifleman: one lying clip, three phases of it, a 0.2 gap. His spread was never the
    # complaint, so these two gates are exactly as they were last round.
    at_ph = [read_pose(poses[f"at-{r}"]["at"], "phase") for r in ROUNDS]
    at_clips = [clip_of(poses[f"at-{r}"]["at"]) for r in ROUNDS]
    report["at_phases"] = at_ph
    report["at_clips"] = at_clips
    at_gaps = [phase_gap(at_ph[i], at_ph[j]) for i in range(3) for j in range(i + 1, 3)]
    report["at_phase_gap_min"] = round(min(at_gaps), 4)
    wrong = [f"round {r}: {c}" for r, c in zip(ROUNDS, at_clips) if c != AT_CLIP]
    if wrong:
        problems.append(f"the at still is not {AT_CLIP} in every round ({', '.join(wrong)}) - a "
                        f"different clip is a different pose, not a different phase of one")
    if min(at_gaps) < MIN_PHASE_GAP:
        problems.append(f"the at's {AT_CLIP} phases {at_ph} come within {min(at_gaps):.3f} of each "
                        f"other, want {MIN_PHASE_GAP} - this is one pose, not three")

    # The Sniper: three DIFFERENT kneels. The owner threw out three KneelIdle phases, so no clip
    # name is typed here any more - the kneel is the trace's own `stance Crouch`, and the three
    # clips have to be three clips. The phase band keeps a once-clip off its held last frame.
    sn_ph = [read_pose(poses[f"sniper-{r}"]["sniper"], "phase") for r in ROUNDS]
    sn_clips = [clip_of(poses[f"sniper-{r}"]["sniper"]) for r in ROUNDS]
    sn_stance = [stance_of(poses[f"sniper-{r}"]["sniper"]) for r in ROUNDS]
    report["sniper_phases"] = sn_ph
    report["sniper_clips"] = sn_clips
    report["sniper_stances"] = sn_stance
    report["sniper_phase_band"] = list(PHASE_BAND)
    not_kneeling = [f"round {r}: stance {st} ({c})"
                    for r, c, st in zip(ROUNDS, sn_clips, sn_stance) if st != SNIPER_STANCE]
    if not_kneeling:
        problems.append(f"the sniper is not at stance {SNIPER_STANCE} in every round "
                        f"({', '.join(not_kneeling)}) - that still is not a kneel")
    if len(set(sn_clips)) != 3:
        problems.append(f"the sniper's three clips are {sn_clips} - {len(set(sn_clips))} distinct, "
                        f"want 3: phases of one looping clip are one pose, which is the set the "
                        f"owner threw out")
    out_of_band = [f"round {r}: {p}" for r, p in zip(ROUNDS, sn_ph)
                   if p is None or not PHASE_BAND[0] <= p <= PHASE_BAND[1]]
    if out_of_band:
        problems.append(f"the sniper's phase is outside {PHASE_BAND} in {', '.join(out_of_band)} - "
                        f"a once-clip holds its last frame, so that still may be a frozen pose")
    if len(at) == 3:
        same = [f"{i + 1} and {j + 1}" for i in range(3) for j in range(i + 1, 3)
                if np.array_equal(at[i], at[j])]
        if same:
            problems.append("the AT masks of rounds " + ", ".join(same) +
                            " are pixel-identical - that is one frozen pose, not three captures")

    # Three kneels a player can tell apart: each PAIR of sniper masks has to differ by at least
    # MIN_POSE_SPREAD, and by at least MIN_NOISE_RATIO times the weather in the worse of the two
    # boxes - so no pair can ever pass on rain. 0.25 is above the 0.16 the owner called invisible.
    if len(sniper) == 3:
        pair_spreads, pair_px, ratios = {}, {}, {}
        for i in range(3):
            for j in range(i + 1, 3):
                key = f"sniper-{ROUNDS[i]} vs sniper-{ROUNDS[j]}"
                a, b = centred(sniper[i]), centred(sniper[j])
                d = diff(sniper[i], sniper[j])
                px = int((a ^ b).sum())
                noise = max(report.get(f"noise_in_box_px_sniper-{ROUNDS[i]}", 0),
                            report.get(f"noise_in_box_px_sniper-{ROUNDS[j]}", 0))
                pair_spreads[key] = round(d, 4)
                pair_px[key] = px
                ratios[key] = round(px / noise, 1) if noise else None
                if d < MIN_POSE_SPREAD:
                    problems.append(f"{key}: the two kneels differ by {d:.4f}, under "
                                    f"{MIN_POSE_SPREAD} - the player cannot tell them apart, "
                                    f"which is what the owner threw out")
                if noise and px < MIN_NOISE_RATIO * noise:
                    problems.append(f"{key}: {px} px of moved man against {noise} px of weather in "
                                    f"the worse box, under {MIN_NOISE_RATIO}x - this pair's "
                                    f"spread could be the rain")
        report["sniper_pose_spreads"] = pair_spreads
        report["pose_diff_px"] = pair_px
        report["pose_diff_noise_ratio"] = ratios

    # --- the honest limit, reported and not gated ------------------------------------------------
    heads = {}
    for r in ROUNDS:
        p = poses.get(f"pair-{r}", {})
        ya, ys = read_pose(p.get("at", ""), "yaw"), read_pose(p.get("sniper", ""), "yaw")
        if ya is not None and ys is not None:
            heads[f"pair-{r}"] = round(float(abs((ya - ys + np.pi) % (2 * np.pi) - np.pi)), 4)
    report["heading_diff_rad"] = heads

    # A man who TURNED between his pair still and his own still is at a different heading, and a
    # self-spread over three headings measures the turn, not the pose. The old Sniper stills were
    # at yaw -0.30, -0.97 and -1.03 against 0.00 in the pair stills: JumpDown turns him. So each
    # man's yaw in his own still now has to match his yaw in that round's pair still.
    for who, stem in (("at", "at"), ("sniper", "sniper")):
        offs = {}
        for r in ROUNDS:
            own = read_pose(poses.get(f"{stem}-{r}", {}).get(who, ""), "yaw")
            ref = read_pose(poses.get(f"pair-{r}", {}).get(who, ""), "yaw")
            if own is None or ref is None:
                problems.append(f"{stem}-{r}: no yaw for the {who} in his own or the pair still, "
                                f"so his heading cannot be checked")
                continue
            offs[f"{stem}-{r}"] = round(wrap(own - ref), 4)
        report[f"{who}_heading_offset_rad"] = offs
        bad = {k: v for k, v in offs.items() if v > HEADING_TOL}
        if bad:
            problems.append(f"the {who} is turned in {bad} rad against his heading in that round's "
                            f"pair still, over {HEADING_TOL} - that is a heading, not a pose")

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
        # The goal's words: the bar sits OUTSIDE the worse self-spread of these three captures.
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
    if DIR == HERE:
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
