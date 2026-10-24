#!/usr/bin/env python3
"""Measures the frog-death-flop-r3-g2 stills and writes probe.json beside this script.

The owner threw out r3: "The body does not read as a frog on its back, and the probe does not look. take0_p05.png is
face-down, take1_p15.png is a different tangle with a rifle, and the red star stays in take1_plate.png and
take2_plate.png after the body is removed. frog_belly_up is the gag id, and the motion bar of 150 sits under a
whole-crop difference of 122469."

What is different here:
  - the set is ONE flop of ONE frog, lit (Biome.Winter, daylight), shot by one camera that never moves, with
    DebrisRenderer.Gore = 0 so no blood splat is left on the ground behind him;
  - every still has its own plate of the same place: <take>_ground.png is the patch before a frog is on it, and
    <take>_plate.png is the held instant with the body taken away. The body is what differs from its own plate;
  - nothing here reads mark_gag or mark_roll. Those are the plan, not the pixels. The sidecars are used only for the
    honesty keys (who was in the frame, which bake was drawn, whether it rained);
  - no bar is typed. Each bar is the midpoint of two clusters measured in this same set, of captures that differ in
    behaviour: the frog standing on his feet against the same frog flopped, and the flop against a control death
    (DeathGags.Pin(0), the same shell, the same light, the same zoom, 16 m away).

The stills (Tests/Stills/FrogFlopStills.cs):
    flop_ground.png  the empty patch            ctrl_ground.png  the control's empty patch
    flop_alive.png   the frog standing on it    ctrl_alive.png   the control frog standing
    flop_p05/p10/p15 death +0.5 / +1.0 / +1.5 s ctrl_p05/p10/p15 the same, with the gag pinned off
    flop_frz_b/frz_c the SAME held instant as flop_p15: three captures of one flop
    flop_plate.png   that instant, body cleared ctrl_plate.png   the control's plate

How the body is found, without a box: the still minus its plate, thresholded, eroded by one pixel (speckle of blown
snow and drifting dust does not survive it), and the LARGEST connected blob of what is left, inside a 500x500 window
at the middle of the frame, which is where the camera was aimed.

    body_lum       the mean luminance of that blob: which side of him the picture is showing.
    turn_bar       the midpoint between the standing cluster (flop_alive, ctrl_alive) and the flop cluster
                   (flop_p15, flop_frz_b, flop_frz_c). He has turned over when all three captures of the flop sit on
                   the far side of it from both standing pictures.
    gag_bar        the midpoint between the flop cluster and the control death's lowest reading. Below it is a side
                   of him today's death never shows.
    motion_px      how much of the window differs between +0.5 s and +1.5 s, against a floor measured from the three
                   captures of the one held instant (they are the same instant, so the floor is what the instrument
                   invents on its own).

What this probe CANNOT say, and nobody should read into it: it measures which side of the frog is drawn, not whether
that side is his belly. Belly-up against face-down is a judgement on the pictures, and flop_p15.png is in this folder
for exactly that. A green probe on a build whose flop still reads face-down means the body turned, not that it turned
the right way.

Exit 0 only when every still is honest and every bar is cleared.
"""
import itertools
import json
import os
import sys
from collections import deque

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))

CX, CY, HALF = 960, 470, 250      # the window: the middle of the frame, where the camera was aimed
DELTA = 30                        # sum of the three channel differences a pixel must exceed to count as changed
FROG_FIGURE = 2                   # VATRenderer.FigureOfArchetype(InfantryArchetype.Frog)

STANDING = [("flop_alive", "flop_ground"), ("ctrl_alive", "ctrl_ground")]
FLOP = [("flop_p15", "flop_plate"), ("flop_frz_b", "flop_plate"), ("flop_frz_c", "flop_plate")]
CONTROL = [("ctrl_p05", "ctrl_plate"), ("ctrl_p10", "ctrl_plate"), ("ctrl_p15", "ctrl_plate")]
DEATH_STILLS = ["flop_p05", "flop_p10", "flop_p15", "flop_frz_b", "flop_frz_c",
                "ctrl_p05", "ctrl_p10", "ctrl_p15"]


def sidecar(stem):
    with open(os.path.join(HERE, stem + ".json"), encoding="utf-8") as f:
        return json.load(f)


def window(stem):
    img = np.asarray(Image.open(os.path.join(HERE, stem + ".png")).convert("RGB"), dtype=np.int16)
    return img[CY - HALF:CY + HALF, CX - HALF:CX + HALF]


def changed(a, b):
    """The pixels of the window that differ, with single-pixel speckle eroded away."""
    m = np.abs(a - b).sum(axis=2) > DELTA
    e = m.copy()
    for dy in (-1, 0, 1):
        for dx in (-1, 0, 1):
            e &= np.roll(np.roll(m, dy, 0), dx, 1)
    return e


def biggest_blob(mask):
    """The largest four-connected piece of the mask: the body, rather than a box somebody chose."""
    seen = np.zeros(mask.shape, bool)
    best = np.zeros(mask.shape, bool)
    h, w = mask.shape
    for sy in range(h):
        for sx in range(w):
            if not mask[sy, sx] or seen[sy, sx]:
                continue
            here = np.zeros(mask.shape, bool)
            q = deque([(sy, sx)])
            seen[sy, sx] = True
            n = 0
            while q:
                y, x = q.popleft()
                here[y, x] = True
                n += 1
                for yy, xx in ((y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)):
                    if 0 <= yy < h and 0 <= xx < w and mask[yy, xx] and not seen[yy, xx]:
                        seen[yy, xx] = True
                        q.append((yy, xx))
            if n > best.sum():
                best = here
    return best


def body(stem, plate):
    """One still's body: its blob, how big it is and how bright."""
    a = window(stem)
    mask = biggest_blob(changed(a, window(plate)))
    lum = a[:, :, 0] * 0.299 + a[:, :, 1] * 0.587 + a[:, :, 2] * 0.114
    n = int(mask.sum())
    return {"px": n, "lum": round(float(lum[mask].mean()), 2) if n else None}


def motion(a, b):
    return int(changed(window(a), window(b)).sum())


def honest(result):
    """Every death still says the frog's own bake was drawn, that he was alone, and that it was dry."""
    bad = []
    for stem in DEATH_STILLS:
        s = sidecar(stem)
        if s.get("mark_figure") != FROG_FIGURE:
            bad.append(f"{stem}: figure {s.get('mark_figure')} was drawn, not the frog's {FROG_FIGURE}")
        if s.get("mark_in_frame") != 1:
            bad.append(f"{stem}: the marked frog is not in the frame")
        if s.get("men_in_frame") != 0 or s.get("fallen_in_frame") != 0:
            bad.append(f"{stem}: he is not alone ({s.get('men_in_frame')} men, {s.get('fallen_in_frame')} bodies)")
        if s.get("rain", 1) > 0.001:
            bad.append(f"{stem}: rain is not pinned off")
    for stem, _ in STANDING:
        s = sidecar(stem)
        if s.get("men_in_frame", 0) < 1:
            bad.append(f"{stem}: no living frog in the frame")
        if s.get("fallen_in_frame") != 0:
            bad.append(f"{stem}: a body shares the frame with the living frog")
    result["honest"] = not bad
    result["honesty_faults"] = bad


def main():
    for stem in [s for s, _ in STANDING + FLOP + CONTROL] + ["flop_p05", "flop_p10",
                                                             "flop_ground", "flop_plate", "ctrl_ground", "ctrl_plate"]:
        if not os.path.exists(os.path.join(HERE, stem + ".png")):
            print("missing still: " + stem + ".png", file=sys.stderr)
            return 1

    standing = {s: body(s, p) for s, p in STANDING}
    flop = {s: body(s, p) for s, p in FLOP}
    control = {s: body(s, p) for s, p in CONTROL}

    stand_lum = [v["lum"] for v in standing.values()]
    flop_lum = [v["lum"] for v in flop.values()]
    ctrl_lum = [v["lum"] for v in control.values()]

    turn_bar = (sum(stand_lum) / len(stand_lum) + sum(flop_lum) / len(flop_lum)) / 2
    gag_bar = (max(flop_lum) + min(ctrl_lum)) / 2

    frozen = [motion(a, b) for a, b in itertools.combinations(["flop_p15", "flop_frz_b", "flop_frz_c"], 2)]
    flop_motion = motion("flop_p05", "flop_p15")
    ctrl_motion = motion("ctrl_p05", "ctrl_p15")
    motion_bar = (max(frozen) + flop_motion) / 2

    result = {
        "standing": standing, "flop": flop, "control": control,
        "turn_bar": round(turn_bar, 2), "gag_bar": round(gag_bar, 2),
        "flop_lum_spread": round(max(flop_lum) - min(flop_lum), 2),
        "frozen_pair_diffs_px": frozen,
        "flop_motion_px": flop_motion, "ctrl_motion_px": ctrl_motion, "motion_bar": motion_bar,
        "window": [CX, CY, HALF], "delta": DELTA,
    }
    honest(result)

    # the body the flop leaves is a side of him neither standing picture shows ...
    result["turned_over"] = all(v < turn_bar for v in flop_lum) and all(v > turn_bar for v in stand_lum)
    # ... and a side today's death never shows either, so it is the gag that turns him
    result["gag_turns_him"] = max(flop_lum) < gag_bar < min(ctrl_lum)
    # ... and he is not a frozen prop: the three captures of the one instant agree to the pixel
    result["he_moves"] = flop_motion > motion_bar > max(frozen)

    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)
        f.write("\n")

    for k in ("honest", "turned_over", "gag_turns_him", "he_moves", "turn_bar", "gag_bar",
              "flop_lum_spread", "frozen_pair_diffs_px", "flop_motion_px", "ctrl_motion_px"):
        print(k, result[k])
    for fault in result["honesty_faults"]:
        print("  fault:", fault)
    print("standing lum", stand_lum, "flop lum", flop_lum, "control lum", ctrl_lum)

    ok = result["honest"] and result["turned_over"] and result["gag_turns_him"] and result["he_moves"]
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
