"""Measures the pixels of the nine stills committed beside this file.

The question: does the prone AT rifleman read as his own man, or as the Sniper with another name?

WHAT THIS ROUND MOVES
  The owner threw out atrifle-look-r3-g1-g1-g2 in his own words: "At zoom 30 the prone man is a
  helmet and a shoulder behind the sandbags, and empty-1 and empty-3 still show the rifle, so the
  272-296 px mask is the helmet and not the gun. Put him in front of the bags in pair-1, pair-2 and
  pair-3 so the body and the rifle are in the frame, and shoot the three empty plates with that
  rifle gone."

  He is right twice, and the old set's own numbers say so. First the mask: the AT mask of g1-g1-g2's
  pair-1 is 296 px in a bbox 23x18 px - 0.9 x 0.7 m at 25.0 px/m - which is a helmet and a shoulder,
  not a 2 m man with a 1.65 m gun. It sits directly on a bright band at y 381-390 that is
  pixel-identical in pair-1, empty-1, floor-1, empty-2 and empty-3: the parapet, with the man behind
  it. Second the plates: in every plate of that set the AT man's box is parked at img (1754.7,
  216.8) while the Sniper's is the (-5000,-5000) projection, so he was moved NEARBY and kept ALIVE -
  `DrawAtRifles` still drew his gun into the plate, and the probe had no number that would have
  caught it.

  So this round changes the staging, not the measure's shape: the AT rifleman is written to
  (54, 0, Z) with +z toward the camera, which puts him in FRONT of the sandbags, and for both plates
  of every round both men are parked at (-5000, 0, -5000) AND written Hp 0 with the Alive flag
  cleared, so `CombatFx.Draws` is false and no gun is drawn. frames.txt names the Z that was chosen
  and the ones rejected.

  Two new gates hold that, so a later round cannot quietly slide back:

    * `at_rifles` (the capture's own `CombatFx.AtRiflesDrawn`, the guns of the last frame) must be 1
      in each `pair-{r}` and 0 in all six plates. A plate that drew a gun is not a plate;
    * `at_area_m2` 0.7 (reported with `at_span_m`, the mask's bbox width over `px_per_m`). A prone
      man is about 2.0 m long and 0.45 m off the ground and lies 1.11 rad off the view, so his side
      profile projects about 1.8 x 0.45 = 0.81 m2. 0.7 leaves room for mud over his legs and still
      bites: the set the owner threw out reads 0.43-0.47 m2 and fails this probe.

  `BOX_HALF_W` goes 0.8 -> 1.2 of `tall_px` with it. At 0.8 the box reaches 2.08 m either side of
  his feet, and a prone man plus a 1.65 m gun is about 2.5 m ahead of them, so the old box would cut
  him and the reading would be a reading of the box. The cut is now checked: a mask that touches its
  OWN box edge fails, not only one that touches a frame border.

  PASS_DIFF stays 0.6, untouched since commit 13d1158177, so the rounds compare, and the three
  spread gates are unchanged. If an honest set of three pair frames will not clear it, the answer is
  a red probe, not a lower bar.

  frames.txt tells the staging at length; what follows is only what reaches the number.

ONE FRAME, ONE CAMERA
  All nine stills share one camera pose: focus (47.5, 25), zoom 30 (T1, the owner's number), yaw
  180 with CaptureRig.Rig.YawPin = 0, pitch 22, 1600x900. The probe reads zoom, yaw, pitch,
  focus_x, focus_z and the image size out of every sidecar and fails if any one of the nine
  differs, so "one camera" is checked, not asserted. It also requires pose_error_m 0, rain 0 and an
  empty `warnings` list everywhere.

BOTH MEN IN ONE FRAME
  Three stills per round: `pair` (both men, men_in_frame 2), `empty` (both off the map, 0) and
  `floor` (both off again, a few world-seconds later, 0). The probe requires exactly those counts,
  AND `drawn_infantry` 0 AND `at_rifles` 0 in both plates - a plate that holds a man, or his gun, is
  not a plate. In the pair stills `at_rifles` must be exactly 1: the one gun of the one AT rifleman.

WHO IS WHO, AND WHERE, FROM THE CAPTURE THAT DREW THEM
  The frame has braziers in it, and a frame-wide reading measures a fire every time: an earlier leg
  dropped everything that moves on its own and the largest surviving blob was STILL a brazier,
  2241-5501 px against a few hundred for the man. So the reading does not look at the whole frame.
  Every shot records where the game drew each man and WHICH man it was - `manN_slot`,
  `manN_archetype`, `manN_img_x`, `manN_img_y`, `manN_tall_px` - and this probe masks inside that
  box and nowhere else:

      x in img_x +- 1.2 * tall_px,    y in [img_y - 1.0 * tall_px, img_y + 0.2 * tall_px]

  (`img_y` counts from the top and is the man's FEET, hence the long reach upwards and the short
  one down.) The box is not typed here and not read off the pixels it is about to measure: it is
  the capture's own projection of the man it drew. Nor is any slot number typed: the AT rifleman is
  the drawn man whose `manN_archetype` is 27 and the Sniper the one whose is 3 - the two archetypes
  `TankCapture.Spawn` was given, and `VATRenderer.FigureOfArchetype` makes 3 the Sniper figure and
  27 the Soldier figure. A pair still that does not hold exactly one of each fails.

  The two boxes must not overlap: the men are five metres apart, so "only each man" is checked
  rather than asserted. This is the gate the move onto one frame leans on hardest - in a solo still
  a mask could not hold the other man, because the other man was not there.

THE MASK
  A pixel is "the man" when its Rec.709 luma differs from that round's `empty` plate by more than
  0.04; then a 3x3 opening drops grain speckle and a 7x7 closing joins the parts of one man (boots,
  back, helmet) that the mud happens to match. WHAT MOVES ON ITS OWN IS NOT THE MAN:
  `|empty-{r} - floor-{r}|` is two man-less plates of the same frame a few world-seconds apart, so
  everything in it is weather, and it is subtracted before any blob is labelled. It is NOT dilated
  - a 3x3 dilation ate 1.1k to 8.3k px of real man. `noise_in_box_px_*` reports how much of that
  weather falls inside each man's box at all.

  Only the LARGEST surviving component inside the box is kept, and it has to earn the name "a man":
    * an area over 0.2 m2 of frame, and for the AT rifleman over `AT_MIN_AREA_M2` 0.7 as well -
      `at_area_m2` and `at_span_m` are printed per round - and under half his own box - a man who fills half the box the
      renderer drew him in is not a silhouette. Both bounds are reported in pixels;
    * at least 1.5x the second-largest blob in the box, so an ambiguous mask fails loudly. On one
      frame this is also the gate that catches the Sniper's shadow reaching into the AT man's box:
      the boxes are tens of pixels apart and shadows are on;
    * no pixel on a frame border, checked only along a box edge that the frame actually cuts; AND
      no pixel on its OWN box edge either. A mask that reaches the box it is measured in has been
      truncated by the box, and then the reading is a reading of the box and not of the man.

  THE FRAME'S SCALE comes from the capture as well: `px_per_m = tall_px / 2.6`, a man being 2.6 m
  of world in the box the renderer drew. The probe reports `px_per_m` for every man of every round
  and fails if they disagree by over 10 %.

THE READING
  The two men stand five metres apart in the same frame by design, so a raw pixel IoU between them
  would mostly measure that gap. Each mask is therefore translated so its centroid sits at the
  origin - translation only, no scale and no rotation, so the comparison is of shape and not of
  place, and a man who is simply bigger or turned still reads as different. The reading of a round
  is `1 - IoU` of the AT mask against the Sniper mask of the SAME pair still after that shift.

THREE LIVE POSES PER MAN, NOT ONE
  The poses that are gated are now the poses OF THE SCORED FRAMES. Before each pair shot both men
  were driven, in the same running sim and with no world write in between (a write is what pinned
  an earlier round's phases), to a moment that suits BOTH: round 1 is shot as soon as both are in
  their idle clip, round 2 at the first moment each man is a loop-distance 0.28-0.42 from his own
  round-1 phase, round 3 at the first moment each is at least 0.25 from his own round-1 AND round-2
  phases. The probe checks the result four ways per man, all out of the `pair-{r}` records:

    * the clip gate - ProneIdle for the AT rifleman and KneelIdle for the Sniper in all three
      rounds. A different clip is a different pose, not another phase of one;
    * `MIN_PHASE_GAP` 0.2 (the owner's number), as `at_phase_gap_min` and `sniper_phase_gap_min`,
      measured around the loop so 0.98 and 0.02 count as 0.04 apart;
    * the HEADING gate: each man's yaw must be within 0.05 rad of his own yaw in round 1. One frame
      per round leaves no second still of the same man to check him against, so what is checked is
      that he did not turn BETWEEN the rounds - a self-spread over three headings would measure the
      turn and not the pose;
    * the three masks of one man must not be pixel-identical.

THE BAR
  PASS_DIFF is 0.6. Three spreads are printed and all three have to sit below it while the reading
  stays above it:
    * `spread_readings`   = max - min of the three AT-against-Sniper readings;
    * `spread_at`         = the largest 1 - IoU between any two of the three AT masks;
    * `spread_sniper`     = the same among the three Sniper masks.
  The last two are the noise floor of the measure itself: the same man, same camera, a different
  live pose, and now all three cut out of the frames that are scored. Pass needs every spread <
  PASS_DIFF (so `bar_headroom` > 0) and min(readings) >= PASS_DIFF, so the claim is made on the
  WORST of the three rounds and only when it clears the floor by the whole bar.

THE HONEST LIMIT
  One camera yaw cannot be side-on to two men facing different ways, and a man's heading is snapped
  to his post's. The probe reports the per-round AT-against-Sniper heading difference as
  `heading_diff_rad` (1.11 rad on this set) and DOES NOT gate on it: one frame was the owner's
  call, and this is its price, stated rather than hidden. The parapet is no longer on this list:
  the AT rifleman is staged in FRONT of the bags, `at_area_m2` 0.7 is the gate that says his body
  is in the frame, and the number is checked rather than excused. At zoom 30 the 1.65 m anti-tank
  rifle is still only a few dark pixels of the mask: `at_rifles` says the game DREW it, and
  `at_area_m2` says the mask is man-sized, but no number here isolates the gun's own shape. One seed, one ground, one time of day.
  And two figures, not two models: every infantry archetype except the sniper is drawn as the
  Soldier figure (`VATRenderer.FigureOfArchetype`), so this is a pose-and-figure difference, not a
  per-unit model.

READING ANOTHER SET
  `python probe.py <folder>` measures the nine stills of another folder and prints the report
  WITHOUT writing probe.json, so the negative control cannot clobber this set's number.
"""
import json
import os
import re
import sys

import numpy as np
import scipy.ndimage as nd
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
SET = os.path.abspath(sys.argv[1]) if len(sys.argv) > 1 else HERE
WRITE_JSON = SET == HERE      # a foreign set is printed only; this set's number is not clobbered

THRESHOLD = 0.04
MAN_TALL_M = 2.6            # the height CaptureRig.ManBox projects, so tall_px is 2.6 m of frame
MIN_AREA_M2 = 0.2
AT_MIN_AREA_M2 = 0.7        # a prone man's side profile is about 1.8 x 0.45 m; see the docstring
MAX_BOX_SHARE = 0.5         # of his own box; see "the ceiling" in the docstring
BOX_HALF_W = 1.2            # of tall_px, either side of his feet: a prone man plus a 1.65 m gun
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
PASS_DIFF = 0.6           # unchanged since commit 13d1158177, so the rounds compare

ROUNDS = (1, 2, 3)
KINDS = ("pair", "empty", "floor")
MEN = {"pair": 2, "empty": 0, "floor": 0}
AT_RIFLES = {"pair": 1, "empty": 0, "floor": 0}   # CombatFx.AtRiflesDrawn: the guns of the last frame
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


def box_edges(m, span):
    """The sides of his OWN box the mask reaches. A truncated mask measures the box, not the man."""
    i0, i1, j0, j1 = span
    return [side for side, reached in (
        ("left", m[j0:j1, i0].any()), ("right", m[j0:j1, i1 - 1].any()),
        ("top", m[j0, i0:i1].any()), ("bottom", m[j1 - 1, i0:i1].any())) if reached]


def judge(stem, m, px, second, lo, lo_m2, hi, cuts, span, problems):
    """Does the largest component in the box earn the name "a man"? Size, dominance, not cut."""
    if m is None:
        return None
    ok = True
    if px < lo:
        problems.append(f"{stem}: the largest blob in his box is {px} px, under the frame's own "
                        f"floor of {lo} px ({lo_m2} m2) - too small to be a man")
        ok = False
    if px > hi:
        problems.append(f"{stem}: the largest blob in his box is {px} px, over {hi} px - more "
                        f"than {MAX_BOX_SHARE:.0%} of his own box, so it is the patch and not him")
        ok = False
    if second * DOMINANCE > px:
        problems.append(f"{stem}: the largest blob in his box is {px} px and the next is {second} "
                        f"px, under {DOMINANCE}x - the mask is ambiguous")
        ok = False
    on_box = box_edges(m, span)
    if on_box:
        problems.append(f"{stem}: the mask reaches the {', '.join(on_box)} edge of his OWN box - it "
                        f"is truncated by the box, so the reading would be of the box and not of him")
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


def phase_gap(a, b):
    """Distance between two phases on a LOOPING clip: 0.98 and 0.02 are 0.04 apart, not 0.96."""
    d = abs(a - b) % 1.0
    return min(d, 1.0 - d)


def wrap(a):
    """An angle folded into (-pi, pi], as a size."""
    return float(abs((a + np.pi) % (2 * np.pi) - np.pi))


def main():
    report = {"set": os.path.basename(SET), "pass_diff": PASS_DIFF, "threshold_luma": THRESHOLD,
              "min_area_m2": MIN_AREA_M2, "at_min_area_m2": AT_MIN_AREA_M2,
              "max_box_share": MAX_BOX_SHARE,
              "dominance": DOMINANCE, "man_tall_m": MAN_TALL_M,
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
        if "at_rifles" not in d:
            problems.append(f"{s}: the sidecar has no at_rifles, so nothing says whether the game "
                            f"drew the anti-tank rifle in this still")
        else:
            want_guns = AT_RIFLES[kind]
            if int(d["at_rifles"]) != want_guns:
                problems.append(f"{s}: at_rifles is {d['at_rifles']}, want {want_guns}" + (
                    " - a plate that drew a gun is not a plate" if want_guns == 0 else
                    " - the pair still must hold exactly the one gun of the one AT rifleman"))

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
            if k == "at":
                ys, xs = np.nonzero(m)
                report[f"at_bbox_{stem}"] = [int(xs.min()), int(xs.max()), int(ys.min()), int(ys.max())]
                report[f"at_span_px_{stem}"] = int(xs.max() - xs.min() + 1)
            masks[k].append((f"{k} of {stem}", m, px, second, cuts, int(bm.sum()), span))

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
        for label, m, px, second, cuts, box_px, span in masks[k]:
            hi = int(round(MAX_BOX_SHARE * box_px))
            report["mask_px_max_" + label] = hi
            area = round(px / px_per_m ** 2, 3) if scales else None
            report["mask_m2_" + label] = area
            floor, floor_m2 = lo, MIN_AREA_M2
            if k == "at" and scales:
                report["at_area_m2_" + label] = area
                report["at_span_m_" + label] = round(
                    report["at_span_px_" + label.split(" of ")[1]] / px_per_m, 3)
                floor, floor_m2 = max(lo, int(round(AT_MIN_AREA_M2 * px_per_m ** 2))), AT_MIN_AREA_M2
                if area < AT_MIN_AREA_M2:
                    problems.append(
                        f"{label}: the AT mask is {area} m2, under {AT_MIN_AREA_M2} m2 - that is a "
                        f"helmet and a shoulder, not a prone man with his gun in the frame")
            kept = judge(label, m, px, second, floor, floor_m2, hi, cuts, span, problems)
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
        report["spread_sniper"] = round(spread(sniper), 4)
        worst = max(report["spread_readings"], report["spread_at"], report["spread_sniper"])
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
    sys.exit(main())
