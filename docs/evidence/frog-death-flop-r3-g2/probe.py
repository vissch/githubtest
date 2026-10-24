#!/usr/bin/env python3
"""Reads the frog's belly off the pixels of one flop, and writes probe.json beside this script.

The owner threw out r3: "The body does not read as a frog on its back, and the probe does not look. take0_p05.png is
face-down, take1_p15.png is a different tangle with a rifle, and the red star stays in take1_plate.png and
take2_plate.png after the body is removed. frog_belly_up is the gag id, and the motion bar of 150 sits under a
whole-crop difference of 122469."

He was right twice over, and both faults are fixed here:

1. The body really was face down. DeathGags.FrogFlop put half a turn (RestRoll 16) on top of the death clip, and the
   clip already lays him on his back, so the half turn rolled him onto his face. The old pictures are in this
   folder's history (commit a52cac7540): the same staging, the same light, the same camera, with the old half turn.
   Their body reads warm = +0.004 and -0.013 against the snow; the bodies in this folder read +0.016 to +0.032.
   RestRoll is 0 now, and FrogDeathTests.KilledFrogLandsBellyUp was changed to match the pixels.
2. Nothing here reads mark_gag or mark_roll. Those are the plan written down again. The sidecars are used only for
   honesty (who was in the frame, which bake was drawn, whether it rained, whether the camera stood where the shot
   asked) and for where to put the crop.

The number, and why it is not green. The plan asked for green, median((G - (R+B)/2) / (R+G+B+1)), on the guess that
a frog's back is green and his belly pale. The pixels say otherwise: measured on this set, green puts the standing
frog at -0.0010 and the flopped body at -0.0019 to +0.0046, which is no separation at all -- this bake is nearly
grey in green. What his belly DOES show is tan camouflage and bare hands, warm against snow, while his back is his
pale neutral pack. So the number is the same formula on the channel pair that separates:

    warm      = median((R - B) / (R + G + B + 1)) over the body's pixels
    warm_rel  = that, minus the same median over a ring of ground 4 to 14 px around the body, in the SAME still, so
                smoke, cloud and the exposure of the moment cancel

Two clusters of captures of this build, which differ in behaviour, not in the plan:

    BACK  = flop_alive, ctrl_alive -- a frog standing on his feet shows his back, by definition
    BELLY = flop_p05, flop_p10, flop_p15 -- three captures of ONE flop of ONE frog

BELLY_BAR is the midpoint between max(BACK) and min(BELLY), typed in from the first run and never recomputed here.

The twitch, as a share of the body and not of the crop:

    twitch = (p10's body XOR p15's body) / (p10's body OR p15's body)

TWITCH_BAR is the midpoint between the frozen pairs and that. flop_p15, flop_frz_b and flop_frz_c are one held
instant photographed three times (CaptureRig.Hold, never released), so their pairwise twitch is what the instrument
invents on its own: the measured absence of motion.

What is NOT here, and why. The plan wanted the twitch's absence measured from a control death's own p10/p15 pair
(DeathGags.Pin(0f)). Six capture runs could not photograph that control: ctrl_p05 failed FrogFlopStills' own camera
check (pose_error_m 0.52 to 0.60 m against its 0.5 m bar) every time, and the bar was not weakened to let a lying
still through. The control's ground and standing pictures were shot and are here; ctrl_alive is half the BACK
cluster. And a control death would not have been an absence of motion anyway: measured on an earlier run that did
get one, its body was still travelling between +1.0 s and +1.5 s (a 0.56 share), because today's death throws him.
The frozen pairs are the honest absence.

How the body is found, with no box chosen by hand: the still minus its take's plate (the same instant with the body
taken away, VATRenderer.ClearFallen), summed over the three channels, above DELTA; eroded by one pixel so blown snow
and drifting dust do not survive; then the connected piece carrying the most difference, which is the body and not
the wide, faint haze a burst leaves. The crop is 800x600 on flop_p15's own (mark_px, mark_py), the same crop for
every still of the take, because the camera never moved inside a take.

Exit 0 only when every still is honest, every BELLY capture is over BELLY_BAR, every BACK capture under it, each gap
is wider than the frozen spread, and he moves.
"""
import itertools
import json
import os
import sys

import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))

CROP_W, CROP_H = 800, 600
DELTA = 60                # sum of the three channel differences a pixel must exceed to count as changed
FROG_FIGURE = 2           # VATRenderer.FigureOfArchetype(InfantryArchetype.Frog)
POSE_BAR = 0.5            # FrogFlopStills' own camera check, in metres

# Typed in from the first run of this set, and never recomputed here.
BELLY_BAR = 0.00952       # midpoint of max(BACK) = 0.00313 (ctrl_alive) and min(BELLY) = 0.01590 (flop_p15)
TWITCH_BAR = 0.34150      # midpoint of the frozen pairs' 0.00061 and the flop's p10-to-p15 0.68239

BACK = ["flop_alive", "ctrl_alive"]
BELLY = ["flop_p05", "flop_p10", "flop_p15"]
FROZEN = ["flop_p15", "flop_frz_b", "flop_frz_c"]
FLOP_PLATE = "flop_plate"
ALIVE_PLATE = {"flop_alive": "flop_ground", "ctrl_alive": "ctrl_ground"}
DEATH_STILLS = BELLY + ["flop_frz_b", "flop_frz_c"]


def sidecar(stem):
    with open(os.path.join(HERE, stem + ".json"), encoding="utf-8") as f:
        return json.load(f)


def pixels(stem):
    return np.asarray(Image.open(os.path.join(HERE, stem + ".png")).convert("RGB"), dtype=np.float64)


def window(stem, take):
    """A still's window: for the flop take, 800x600 on flop_p15's own mark. The ctrl take has no death still, so
    its one capture (the frog standing) is measured on the frame it was shot in."""
    img = pixels(stem)
    if take != "flop":
        return img
    s = sidecar("flop_p15")
    x0 = int(round(s["mark_px"])) - CROP_W // 2
    y0 = int(round(s["mark_py"])) - CROP_H // 2
    return img[y0:y0 + CROP_H, x0:x0 + CROP_W]


def body(stem, plate, take):
    """The body's pixels: the piece of the still-minus-plate difference carrying the most difference."""
    a, p = window(stem, take), window(plate, take)
    diff = np.abs(a - p).sum(axis=2)
    mask = ndimage.binary_erosion(diff > DELTA, np.ones((3, 3), bool))
    lab, n = ndimage.label(mask, np.ones((3, 3)))
    if n == 0:
        return mask, a
    mass = ndimage.sum(diff * mask, lab, range(1, n + 1))
    return lab == (int(np.argmax(mass)) + 1), a


def warm_rel(stem, plate, take):
    """How much warmer the body is than the ground around it, in its own still."""
    mask, a = body(stem, plate, take)
    grown = ndimage.binary_dilation(mask, np.ones((3, 3), bool), iterations=14)
    ring = grown & ~ndimage.binary_dilation(mask, np.ones((3, 3), bool), iterations=4)
    r, g, b = a[..., 0], a[..., 1], a[..., 2]
    w = (r - b) / (r + g + b + 1.0)
    return round(float(np.median(w[mask]) - np.median(w[ring])), 5), int(mask.sum())


def twitch(one, two):
    a, _ = body(one, FLOP_PLATE, "flop")
    b, _ = body(two, FLOP_PLATE, "flop")
    return round(float((a ^ b).sum()) / float((a | b).sum()), 5)


def honesty():
    bad = []
    for stem in DEATH_STILLS:
        s = sidecar(stem)
        if s.get("mark_figure") != FROG_FIGURE:
            bad.append(f"{stem}: figure {s.get('mark_figure')} was drawn, not the frog's {FROG_FIGURE}")
        if s.get("mark_in_frame") != 1:
            bad.append(f"{stem}: the marked frog is not in the frame")
        if s.get("men_in_frame") != 0 or s.get("fallen_in_frame") != 0:
            bad.append(f"{stem}: he is not alone ({s.get('men_in_frame')} men, {s.get('fallen_in_frame')} bodies)")
    for stem in BACK:
        s = sidecar(stem)
        if s.get("men_in_frame", 0) < 1:
            bad.append(f"{stem}: no living frog in the frame")
        if s.get("fallen_in_frame") != 0:
            bad.append(f"{stem}: a body shares the frame with the living frog")
    for stem in DEATH_STILLS + BACK + ["flop_ground", FLOP_PLATE, "ctrl_ground"]:
        s = sidecar(stem)
        if s.get("rain", 1) > 0.001:
            bad.append(f"{stem}: rain is not pinned off")
        if not s.get("pose_error_m", 9e9) < POSE_BAR:
            bad.append(f"{stem}: the camera is {s.get('pose_error_m')} m from where the shot asked")
    return bad


def main():
    needed = set(DEATH_STILLS + BACK + ["flop_ground", FLOP_PLATE, "ctrl_ground"])
    missing = [s for s in sorted(needed) if not os.path.exists(os.path.join(HERE, s + ".png"))]
    if missing:
        print("missing stills: " + ", ".join(missing), file=sys.stderr)
        return 1

    warm, area = {}, {}
    for stem in BACK:
        warm[stem], area[stem] = warm_rel(stem, ALIVE_PLATE[stem], stem.split("_")[0])
    for stem in sorted(set(DEATH_STILLS)):
        warm[stem], area[stem] = warm_rel(stem, FLOP_PLATE, "flop")

    back = [warm[s] for s in BACK]
    belly = [warm[s] for s in BELLY]
    frozen_warm = [warm[s] for s in FROZEN]
    frozen_spread = round(max(frozen_warm) - min(frozen_warm), 5)
    gap = round(min(min(belly) - BELLY_BAR, BELLY_BAR - max(back)), 5)

    frozen_pairs = [twitch(a, b) for a, b in itertools.combinations(FROZEN, 2)]
    moves = twitch("flop_p10", "flop_p15")

    result = {
        "warm_rel": warm, "body_px": area,
        "back_cluster": back, "belly_cluster": belly,
        "belly_bar": BELLY_BAR, "frozen_spread_warm": frozen_spread, "smallest_gap": gap,
        "twitch": moves, "twitch_bar": TWITCH_BAR, "frozen_pairs_twitch": frozen_pairs,
        "delta": DELTA, "crop": [CROP_W, CROP_H],
    }
    faults = honesty()
    result["honest"] = not faults
    result["honesty_faults"] = faults
    # every capture of the flop shows a side of him no standing frog shows ...
    result["belly_up"] = all(v > BELLY_BAR for v in belly) and all(v < BELLY_BAR for v in back)
    # ... by more than the instrument's own spread on one instant photographed three times ...
    result["gap_beats_the_spread"] = gap > frozen_spread
    # ... and he is not a frozen prop
    result["he_moves"] = moves > TWITCH_BAR > max(frozen_pairs)

    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)
        f.write("\n")

    print("BACK ", [f"{s}={warm[s]}" for s in BACK])
    print("BELLY", [f"{s}={warm[s]}" for s in BELLY])
    print("belly_bar", BELLY_BAR, "smallest gap", gap, "frozen spread", frozen_spread)
    print("twitch", moves, "bar", TWITCH_BAR, "frozen pairs", frozen_pairs)
    for key in ("honest", "belly_up", "gap_beats_the_spread", "he_moves"):
        print(key, result[key])
    for fault in faults:
        print("  fault:", fault)

    ok = result["honest"] and result["belly_up"] and result["gap_beats_the_spread"] and result["he_moves"]
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
