#!/usr/bin/env python3
"""Measures the frog-death-flop-r3 stills and writes probe.json beside this script.

Critic r1 threw out r2 (12/100): no frog could be found at the frame centre, the stills were not "frog alone", and
the leg motion had no noise floor. Nothing here is typed in: every box, every threshold region and every bar comes
out of the pixels and the .json sidecars CaptureRig wrote with the stills.

Each take (take0, take1, take2) is one staged frog, alone on empty ground, photographed by
Tests/Stills/FrogFlopStills.cs:
    <t>_p05.png / <t>_p10.png / <t>_p15.png   death +0.5 / +1.0 / +1.5 s
    <t>_frz_b.png / <t>_frz_c.png             the SAME held instant as p15 (Time.timeScale 0, weather pinned)
    <t>_plate.png                             that instant with the body taken away (VATRenderer.ClearFallen)

The frog's mask is p15 minus the plate: two frames of one instant, with and without the body, so what differs IS
the frog. No box is guessed.

    frog_area_frac       mask pixels / 480000, in the 800x600 crop centred on the sidecar's (mark_px, mark_py);
                         the min over the three takes. Bar 0.05.
    frog_leg_motion_px   pixels of that crop that differ between p05 and p15; the min over the three takes. Bar 150.
    frog_belly_up        p10's mark_gag is DeathGag.FrogFlop (13) and its mark_roll is in [8, 24] (a roll nearer the
                         upside-down half turn, 16 of 32 steps, than right-side-up) in every take.
    *_base_max           the noise floor, measured, not assumed:
                         area from the three base captures (base0..base2: the r2 framing, zoom 30, pitch 25, same
                         instrument, same plate trick), motion from the three pairwise differences of p15 / frz_b /
                         frz_c, which are one frozen pose photographed three times.

Exit 0 only when every bar is cleared and sits above its floor.
"""
import glob
import itertools
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))

CROP_W, CROP_H = 800, 600
CROP_PX = CROP_W * CROP_H
DELTA = 30            # sum of the three channel differences a pixel must exceed to count as changed
AREA_BAR = 0.05
MOTION_BAR = 150
FROG_FLOP_GAG = 13    # DeathGag.FrogFlop's ordinal (DeathGags.cs)
ROLL_LO, ROLL_HI = 8, 24


def sidecar(stem):
    with open(os.path.join(HERE, stem + ".json"), encoding="utf-8") as f:
        return json.load(f)


def pixels(stem):
    return np.asarray(Image.open(os.path.join(HERE, stem + ".png")).convert("RGB"), dtype=np.int16)


def crop_box(img, cx, cy):
    """The 800x600 window centred on (cx, cy) in the written image's own pixels (sidecar y counts DOWN from the
    top, PIL's order), slid back inside the picture if the frog lies near an edge."""
    h, w = img.shape[:2]
    x0 = int(round(cx)) - CROP_W // 2
    y0 = int(round(cy)) - CROP_H // 2
    x0 = max(0, min(x0, w - CROP_W))
    y0 = max(0, min(y0, h - CROP_H))
    return x0, y0


def changed(a, b, cx, cy):
    """How many pixels of the crop differ by more than DELTA, and the mask itself."""
    x0, y0 = crop_box(a, cx, cy)
    pa = a[y0:y0 + CROP_H, x0:x0 + CROP_W]
    pb = b[y0:y0 + CROP_H, x0:x0 + CROP_W]
    mask = np.abs(pa - pb).sum(axis=2) > DELTA
    return int(mask.sum()), mask


def take_numbers(stem_prefix, live="p15", first="p05"):
    """One take: the frog's area against its own plate, and the motion between first and live."""
    side = sidecar(f"{stem_prefix}_{live}")
    cx, cy = side["mark_px"], side["mark_py"]
    area, _ = changed(pixels(f"{stem_prefix}_{live}"), pixels(f"{stem_prefix}_plate"), cx, cy)
    out = {"area_frac": area / CROP_PX, "area_px": area, "mark_px": cx, "mark_py": cy,
           "px_per_m": side.get("mark_px_per_m"), "zoom": side.get("zoom")}
    if first:
        motion, _ = changed(pixels(f"{stem_prefix}_{first}"), pixels(f"{stem_prefix}_{live}"), cx, cy)
        out["motion_px"] = motion
    return out


def frozen_floor(stem_prefix):
    """The noise floor r1 said was missing: the same pose, three photographs, every pair compared."""
    side = sidecar(f"{stem_prefix}_p15")
    cx, cy = side["mark_px"], side["mark_py"]
    frames = {n: pixels(f"{stem_prefix}_{n}") for n in ("p15", "frz_b", "frz_c")}
    return [changed(frames[a], frames[b], cx, cy)[0] for a, b in itertools.combinations(frames, 2)]


def main():
    takes = sorted({os.path.basename(p).split("_")[0] for p in glob.glob(os.path.join(HERE, "take*_p15.png"))})
    bases = sorted({os.path.basename(p).split("_")[0] for p in glob.glob(os.path.join(HERE, "base*_still.png"))})
    if len(takes) < 3 or len(bases) < 3:
        print(f"need three takes and three base captures, found {len(takes)} and {len(bases)}", file=sys.stderr)
        return 1

    per_take = {t: take_numbers(t) for t in takes}
    floors = [n for t in takes for n in frozen_floor(t)]
    base_area = [take_numbers(b, live="still", first=None)["area_frac"] for b in bases]

    belly = []
    for t in takes:
        s = sidecar(f"{t}_p10")
        belly.append(s.get("mark_gag") == FROG_FLOP_GAG and ROLL_LO <= s.get("mark_roll", -1) <= ROLL_HI)

    result = {
        "n_captures": len(takes),
        "frog_area_frac": round(min(v["area_frac"] for v in per_take.values()), 5),
        "frog_area_frac_base_max": round(max(base_area), 5),
        "frog_leg_motion_px": min(v["motion_px"] for v in per_take.values()),
        "frog_leg_motion_px_base_max": max(floors),
        "frog_belly_up": all(belly),
        "frozen_pair_diffs_px": floors,
        "base_area_frac": [round(v, 5) for v in base_area],
        "per_take": {t: {k: (round(v, 5) if isinstance(v, float) else v) for k, v in n.items()} for t, n in per_take.items()},
        "crop": [CROP_W, CROP_H],
        "delta": DELTA,
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)
        f.write("\n")

    for k in ("n_captures", "frog_area_frac", "frog_area_frac_base_max", "frog_leg_motion_px",
              "frog_leg_motion_px_base_max", "frog_belly_up"):
        print(k, result[k])

    ok = (result["frog_belly_up"] is True
          and result["frog_area_frac"] >= AREA_BAR > result["frog_area_frac_base_max"]
          and result["frog_leg_motion_px"] >= MOTION_BAR > result["frog_leg_motion_px_base_max"])
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
