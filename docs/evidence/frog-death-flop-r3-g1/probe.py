#!/usr/bin/env python3
"""Measures the frog-death-flop-r3-g1 stills and writes probe.json beside this script.

"At zoom 30 the frog is not in the picture ... Shoot him alone at the player's zoom at night, three times, so a
player can find the body." (the owner). r3's set proved the flop at zoom 4.66; this one shoots the SAME flop at the
standard view the player actually plays at (TacticalCamera's own Zoom 30 / Pitch 25, read off the camera, not
typed) and measures whether the body still reads there.

Each take (take0, take1, take2) is one staged frog, alone on empty ground, at night, photographed by
Tests/Stills/FrogFlopStills.cs with TW_FROG_PLAYER=1:
    <t>_p05.png / <t>_p10.png / <t>_p15.png   death +0.5 / +1.0 / +1.5 s
    <t>_frz_b.png                              the SAME held instant as p15 (CaptureRig.Hold): one frozen pair,
                                                the probe's noise floor for "the legs moved"
    <t>_plate.png                              that instant with the body taken away (VATRenderer.ClearFallen)

body_px is p15 minus its own plate, in a crop sized in metres (6.0 x 4.5 m) rather than fixed pixels: a fixed
800x600 window at zoom 30 is 25 m of mostly empty field and buries the body in its own denominator (that is why
r3's base captures at this same view read area_frac 0.002-0.005). A metre-sized crop stays comparable to r3's
1114 / 1944 / 2304 changed-pixel counts at the same view regardless of px/m.

    body_px        pixels of the metre-sized crop that differ between p15 and its own plate; the min over the
                   three takes. Bar 500 (above the 1114/1944/2304 the current build already wrote at this view,
                   and above a lying man's unfindable ~395 px worked example).
    floor_px       the one frozen pair (p15 vs frz_b) per take: the same held pose photographed twice, so
                   anything there is the renderer's own noise.
    twitch_px      p05 vs p15 in that crop; the min over takes. Its bar is the measured floor, not a typed number.
    belly_up       every take's p10 sidecar has mark_gag == 13 (DeathGag.FrogFlop) and mark_roll in [8, 24].
    alone_and_night  every sidecar has men_in_frame == 0, fallen_in_frame == 0, rain < 0.001, luma_mean <= 0.30,
                   and every non-plate one mark_in_frame == 1.
    zoom / pitch   read back as 30 / 25 (+/- 0.01) in every sidecar: the player's view, not a staged one.

Exit 0 only when every bar is cleared and sits above its floor.
"""
import glob
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))

CROP_M_W, CROP_M_H = 6.0, 4.5
DELTA = 30            # sum of the three channel differences a pixel must exceed to count as changed
BODY_BAR = 500
FROG_FLOP_GAG = 13    # DeathGag.FrogFlop's ordinal (DeathGags.cs)
ROLL_LO, ROLL_HI = 8, 24
WANT_ZOOM, WANT_PITCH = 30.0, 25.0


def sidecar(stem):
    with open(os.path.join(HERE, stem + ".json"), encoding="utf-8") as f:
        return json.load(f)


def pixels(stem):
    return np.asarray(Image.open(os.path.join(HERE, stem + ".png")).convert("RGB"), dtype=np.int16)


def crop_box(img, cx, cy, w, h):
    """The w x h window centred on (cx, cy), slid back inside the picture if the frog lies near an edge."""
    ih, iw = img.shape[:2]
    x0 = int(round(cx)) - w // 2
    y0 = int(round(cy)) - h // 2
    x0 = max(0, min(x0, iw - w))
    y0 = max(0, min(y0, ih - h))
    return x0, y0


def changed(a, b, cx, cy, w, h):
    x0, y0 = crop_box(a, cx, cy, w, h)
    pa = a[y0:y0 + h, x0:x0 + w]
    pb = b[y0:y0 + h, x0:x0 + w]
    mask = np.abs(pa - pb).sum(axis=2) > DELTA
    return int(mask.sum())


def take_numbers(stem_prefix):
    side = sidecar(f"{stem_prefix}_p15")
    cx, cy = side["mark_px"], side["mark_py"]
    per_m = side["mark_px_per_m"]
    w, h = round(CROP_M_W * per_m), round(CROP_M_H * per_m)

    body = changed(pixels(f"{stem_prefix}_p15"), pixels(f"{stem_prefix}_plate"), cx, cy, w, h)
    floor = changed(pixels(f"{stem_prefix}_p15"), pixels(f"{stem_prefix}_frz_b"), cx, cy, w, h)
    twitch = changed(pixels(f"{stem_prefix}_p05"), pixels(f"{stem_prefix}_p15"), cx, cy, w, h)
    return {"body_px": body, "floor_px": floor, "twitch_px": twitch,
            "mark_px": cx, "mark_py": cy, "px_per_m": per_m, "crop": [w, h]}


def main():
    takes = sorted({os.path.basename(p).split("_")[0] for p in glob.glob(os.path.join(HERE, "take*_p15.png"))})
    if len(takes) < 3:
        print(f"need three takes, found {len(takes)}", file=sys.stderr)
        return 1

    per_take = {t: take_numbers(t) for t in takes}

    belly = []
    for t in takes:
        s = sidecar(f"{t}_p10")
        belly.append(s.get("mark_gag") == FROG_FLOP_GAG and ROLL_LO <= s.get("mark_roll", -1) <= ROLL_HI)

    sidecars = [sidecar(f"{t}_{n}") for t in takes for n in ("p05", "p10", "p15", "frz_b", "plate")]
    alone_and_night = all(
        s.get("men_in_frame") == 0 and s.get("fallen_in_frame") == 0 and s.get("rain", 1) < 0.001
        and s.get("luma_mean", 1) <= 0.30
        and (s.get("path", "").endswith("_plate.png") or s.get("mark_in_frame") == 1)
        for s in sidecars
    )
    zoom_pitch_ok = all(
        abs(s.get("zoom", -1) - WANT_ZOOM) < 0.01 and abs(s.get("pitch", -1) - WANT_PITCH) < 0.01
        for s in sidecars
    )

    result = {
        "n_captures": len(takes),
        "body_px": min(v["body_px"] for v in per_take.values()),
        "floor_px": max(v["floor_px"] for v in per_take.values()),
        "twitch_px": min(v["twitch_px"] for v in per_take.values()),
        "belly_up": all(belly),
        "alone_and_night": alone_and_night,
        "zoom_pitch_ok": zoom_pitch_ok,
        "per_take": {t: n for t, n in per_take.items()},
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)
        f.write("\n")

    for k in ("n_captures", "body_px", "floor_px", "twitch_px", "belly_up", "alone_and_night", "zoom_pitch_ok"):
        print(k, result[k])

    ok = (result["belly_up"] is True
          and result["alone_and_night"] is True
          and result["zoom_pitch_ok"] is True
          and result["body_px"] >= BODY_BAR
          and result["twitch_px"] > result["floor_px"])
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
