"""Measures the pixels of the fifteen stills committed beside this file.

The question this round adds: at zoom 30, does the prone AT rifleman's RIFLE read, or is he only a
man-shaped blob? Everything else is the previous round's question - does he read as his own man, or
as the Sniper with another name - and every gate that answered it is untouched, so the two sets
compare number for number.

WHAT THE OWNER THREW OUT
  atrifle-look-r3-g1-g1-g1 passed its own bar (0.647 against 0.6, self-spread 0.521) and the owner
  still sent it back: "Frame the prone AT rifleman so the rifle reads at zoom 30." That set's own
  frames.txt admits the fault in one line - "at zoom 30 the 1.65 m anti-tank rifle is a few dark
  pixels. No number here isolates it." It was shot 1600x900, and its capture recorded no gun at all,
  so nothing in it could have isolated the rifle even in principle.

THE ARITHMETIC OF "READS AT ZOOM 30"
  At zoom 30 the rig's framing is FIXED. `CaptureRig` puts the camera at
  `distance = Zoom*tan30/tan(fov/2)` with fov 25 (CloseZoom is 15), so the half-height it frames on
  the ground is always `Zoom*tan30` = 17.32 m and

      px_per_m = HPX / 34.64

  whatever the pitch, the yaw or the focus. H 900 gave the old set its measured 25.0 px/m, and a
  1.65 m rifle 41 px long. Pitch, yaw and focus change only WHERE the camera looks. So the frame is
  the only lever, and the gun needs a taller one.

  The gun is our own prop, not part of the baked figure: `CombatFx.AtRifle.cs`, `AtRifleLength`
  1.65 m, laid by `CombatFx.Lay`. Its barrel box is 0.035 half-extent, 0.07 m thick, which is 0.042
  of the gun's length - a scale-free ratio. A gun drawn 100 px long therefore has a 4.2 px barrel,
  and that is where `MIN_GUN_LEN_PX` 100 comes from: below it the barrel is under four pixels and
  there is nothing to see.

  This set is shot 1100x2340: 67.6 px per metre ON THE GROUND. The rifle measures 123.5 px, which is
  three times the old set's 41 px and 4.2 px of barrel over the gate. (`px_per_m` in the report
  below is 50.9, a different number for a different thing: it is `tall_px / 2.6 m`, so it measures a
  man standing UP, and at pitch 45 a vertical metre projects to fewer pixels than a metre laid flat.
  The gun lies flat, so the ground scale is the one that governs it.)

  A taller frame cannot be bought: `men_in_frame` drops a man nearer an edge than his own `tall_px`
  (CaptureRig ~334) and the two men stand 4.5 m either side of the focus, so the width has to grow
  with the height - `W >= 2*(tall_px + 5.5 m * px_per_m)` - and 1100x2340 is already at that limit
  (2*(133 + 5.5*67.6) = 1010 against W 1100). H 3280, the height a 150 px gun would need, would want
  W 1424 and cost ~6.5 MB a still, ~98 MB for the set, in a repo with no LFS. At 1100x2340 the set
  costs 39 MB.

THE PITCH, MEASURED AND NOT GUESSED
  Four pitches were shot at yaw 180 with the same staging, each one an AT-only still against its own
  plate, and the gun segment the capture itself recorded was sampled against his silhouette:

      pitch   gun_len_px   gun_on_mask   contrast_median   capture warning
        22       117.7        0.405          0.827         -
        35       120.9        0.620          0.111         men barely separate from the ground
        45       123.5        0.680          0.986         -
        55       126.1        0.615          0.422         -

  Pitch 22 - the previous set's pitch - is the one that fails: at that angle the sandbags cut the
  gun and only 40 % of it lands on the man. Pitch 35 clears the gun gates and fails the picture: the
  capture itself calls the man unreadable against the ground at contrast 0.111, under its own 0.12
  bar. Pitch 55 clears everything but reads less of the gun than 45 does. **Pitch 45 is the
  smallest pitch that clears every rule**, and it is also the best of the four on both the gun and
  the contrast. The gun grows with the pitch because the rifle lies flat beside a prone man: the
  steeper the camera, the less of its length is foreshortened.

WHAT THIS PROBE ADDS
  Three numbers per round, all read from the capture's own record of where it drew the gun
  (`manN_gun_x0/y0/x1/y1`, written by `CaptureRig.Measure` since 38f0f8ce43 for every man with
  `CombatFx.Carries(archetype)`, projected with the same man-box projection the boxes use):

    * `gun_px_at-{r}` - the segment itself, muzzle and butt, in this image's pixels;
    * `gun_len_px_at-{r}` - its length, gated at `MIN_GUN_LEN_PX` 100 (the 4.2 px barrel above);
    * `gun_on_mask_at-{r}` - of 200 points evenly spaced along that segment, the fraction that land
      on the AT man's own cleaned mask, gated at `MIN_GUN_ON_MASK` 0.6. A gun the capture drew 120
      px long but that the parapet hides is not a gun that reads, and this is the number that
      catches it: at pitch 22 it is 0.405.

  A set whose sidecars carry no `gun_*` key for the archetype-27 man is named as a problem, not
  silently passed: that is the state of every set before 38f0f8ce43, and it is what the negative
  control shows.

  `BOX_DOWN` is raised from 0.2 to 0.6 of `tall_px`. A prone man's gun lies on the ground BELOW the
  point the capture calls his feet (the box is projected from his drawn position, and 2.6 m of man
  laid flat reaches well under it), and at 0.2 the lower third of the rifle fell outside his own box
  and was dropped from his mask. 0.6 was measured, not guessed: it is what it takes for the whole
  laid gun to sit inside the box at pitch 45.

EVERY OTHER GATE IS LAST ROUND'S, UNTOUCHED
  `PASS_DIFF` 0.6 (unchanged since 13d1158177), `MIN_POSE_SPREAD` 0.25, `MIN_NOISE_RATIO` 10,
  `PHASE_BAND` (0.10, 0.80), `AT_CLIP` ProneIdle, `MIN_PHASE_GAP` 0.2, `HEADING_TOL` 0.05,
  `DOMINANCE` 1.5, `MIN_AREA_M2` 0.2, `MAX_BOX_SHARE` 0.5. The Sniper is still driven to three
  DIFFERENT kneels, each one `stance Crouch` in the trace's own word, each inside the phase band,
  each pair at least 0.25 apart and at least 10x the weather in its box. The claim is still made on
  the WORST of the three rounds.

  Which three kneels DID change, and the bar is why. Round 2 was StoopIdle last round and the new
  pitch killed it: looking down at 45 degrees the squat and the kneel read almost alike, and the
  pair spread 0.2444 and then 0.2331 at two different phases - under 0.25 both times, so it was the
  angle and not the moment. `KneelInspect`, the plan's fallback, went too far the other way: 0.6345
  against KneelIdle and 0.6965 against KneelLookAround, so the Sniper's own self-spread would have
  cleared the 0.6 bar and the reading would have meant nothing. `FidgetRubEyes` - the third fidget
  of the same `TrenchRoutine` beat (AnimationController ~880), a 1.8 s Crouch clip - sits between
  them: 0.3221 and 0.4063. No bar was moved to get there.

THE HONEST LIMITS
  One seed, one night, one ground, one camera, and now one pitch as well: nothing here says the
  rifle reads from another angle or in rain. The two men face different ways and one camera yaw
  cannot be side-on to both, so `heading_diff_rad` is reported and NOT gated. The gun measure is the
  capture's own projection of the prop the presentation lays, so it says the rifle was DRAWN there
  and how much of it falls on the man - it does not say a player picks it out. And the stills cost
  ~2.8 MB each, ~42 MB for the set, which is the price of the frame that makes the gun measurable.
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
BOX_DOWN = 0.6              # of tall_px, below them: a prone man's laid gun hangs under his feet
                            # point, and 0.2 cut the butt off his mask (see the docstring)
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
MIN_GUN_LEN_PX = 100     # 0.042 of the gun's length is its barrel, so 100 px of gun is 4.2 px of barrel
MIN_GUN_ON_MASK = 0.6    # of GUN_SAMPLES points along the drawn gun, landing on his own mask
GUN_SAMPLES = 200

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


def gun_of(stem, arch=27):
    """Where the capture says it DREW this man's gun: (x0, y0, x1, y1) of the archetype-27 man's
    muzzle and butt, in this image's pixels, or None when the sidecar carries no gun at all."""
    d = sidecar(stem)
    i = 0
    while ("man%d_slot" % i) in d:
        k = "man%d" % i
        if int(float(d[k + "_archetype"])) == arch and (k + "_gun_x0") in d:
            return (float(d[k + "_gun_x0"]), float(d[k + "_gun_y0"]),
                    float(d[k + "_gun_x1"]), float(d[k + "_gun_y1"]))
        i += 1
    return None


def gun_on(seg, m):
    """Of GUN_SAMPLES points evenly spaced along the drawn gun, the fraction that land on his mask.
    A gun the capture drew at full length but that the parapet hides reads low here."""
    x0, y0, x1, y1 = seg
    h, w = m.shape
    on = 0
    for i in range(GUN_SAMPLES):
        t = i / float(GUN_SAMPLES - 1)
        x, y = int(round(x0 + t * (x1 - x0))), int(round(y0 + t * (y1 - y0)))
        if 0 <= x < w and 0 <= y < h and m[y, x]:
            on += 1
    return on / float(GUN_SAMPLES)


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
              "box": {"half_width_x_tall": BOX_HALF_W, "up_x_tall": BOX_UP, "down_x_tall": BOX_DOWN},
              "min_gun_len_px": MIN_GUN_LEN_PX, "min_gun_on_mask": MIN_GUN_ON_MASK,
              "gun_samples": GUN_SAMPLES}
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
            # Does his RIFLE read? Only the AT man carries one, and only his own mask can answer it.
            if k == "at":
                seg = gun_of(stem)
                if seg is None:
                    problems.append(f"{stem}: the capture recorded no gun for the archetype-27 man "
                                    f"(no manN_gun_x0 key) - this set was shot before "
                                    f"CombatFx.GunEnds, so nothing in it measures the rifle")
                else:
                    length = float(np.hypot(seg[2] - seg[0], seg[3] - seg[1]))
                    report[f"gun_px_{stem}"] = [round(v, 2) for v in seg]
                    report[f"gun_len_px_{stem}"] = round(length, 2)
                    frac = gun_on(seg, m) if m is not None else 0.0
                    report[f"gun_on_mask_{stem}"] = round(frac, 4)
                    if length < MIN_GUN_LEN_PX:
                        problems.append(f"{stem}: the capture drew his gun {length:.1f} px long, "
                                        f"under {MIN_GUN_LEN_PX} - its barrel is "
                                        f"{0.042 * length:.1f} px and the rifle does not read")
                    if frac < MIN_GUN_ON_MASK:
                        problems.append(f"{stem}: only {frac:.1%} of the drawn gun lands on his own "
                                        f"silhouette, under {MIN_GUN_ON_MASK:.0%} - the parapet "
                                        f"hides the rifle from this angle")
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
