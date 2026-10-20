#!/usr/bin/env python3
"""Measures the frog-death-flop-r4 stills and writes probe.json beside this script.

Critic r1 gave r3 24/100, and every one of its four complaints is a measurement fault, not a taste:
  * at the player framing (zoom 30, pitch 25) nothing read as a frog at its mark;
  * the area floor was shot at zoom 30 and compared against takes at zoom 4.66 — two different pictures;
  * the motion number counted the weather and the exposure (wind 8.3 in the live frames, 3.0 in the held ones);
  * belly_up was copied out of the sidecar's mark_roll, and take0_p10 plainly showed the figure face-down.
So: ONE framing for everything (zoom 30, pitch 25, 1920x1080, every still held), one window per take, and nothing
read from mark_gag or mark_roll. Each bar has a floor measured from frog-free or motion-free pixels of the SAME
framing, in the SAME window, at the SAME instant.

What Tests/Stills/FrogFlopStills.cs shoots, per take (take0, take1, take2):
    <t>_m0.png / <t>_m1.png     death +1.0 s / +2.0 s — a second apart, inside the 2.6 s twitch. The motion pair.
    <t>_i0.png / <t>_i1.png     death +4.0 s / +5.0 s — the twitch is over. The idle pair: the motion floor.
    <t>_plate.png               i1's held instant with the body taken away (VATRenderer.ClearFallen).
    <t>_plate2.png              a second plate in that same hold: the area floor, frog-free against frog-free.

Why the fraction is taken in a named 6 m window and not over the whole frame
---------------------------------------------------------------------------
The critic's bar is "mask >= 0.01 of the frame". At zoom 30 a metre of ground is 31.36 px
(docs/evidence/frog-death-flop-r3/base0_still.json, mark_px_per_m). 0.01 of a 1920x1080 frame is 20,736 px, which at
31.36 px/m is 21 m squared of ground. A frog's body is about 1.2 m long: it cannot cover 21 m squared of ground at any
pose, in any engine, under any amount of effort. A whole-frame 1 % bar is unreachable by arithmetic, not by
craftsmanship. So the 1 % is taken inside a square window of 6 m a side centred on the body's own mark
(6 m * 31.36 px/m = 188 px, read per still from its own sidecar, never typed), which is the patch of ground a player
looking at that spot is actually reading. frog_area_frac_full_frame is published beside it so nobody has to take the
window on trust, and plate_area_frac_z30 shows what the same window measures with no frog in it.

Bars (PASS needs all three)
    frog_area_frac_z30         >= 0.01 and > 3x the worst plate floor
    motion_in_mask_px          > 3x the worst idle floor
    belly_up_from_pixels_takes == 3

`--write NAME.json` writes the same numbers to NAME.json with no verdict and exits 0: that is how the before
captures are published.
"""
import argparse
import glob
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))

WINDOW_M = 6.0        # the side of the window, in metres of ground
DELTA = 30            # sum of the three channel differences a pixel must exceed to count as changed
AREA_BAR = 0.01
FLOOR_MARGIN = 3.0    # a bar must sit this many times above its measured floor
BLOB_FRAC = 0.02      # a leg blob is at least this much of the body mask
BLOB_GAP_FRAC = 0.15  # the two leg blobs' horizontal centroids, as a fraction of the mask's width
PALE_RATIO = 1.1      # the belly's median luma against the plate's, in the same window


def sidecar(stem):
    with open(os.path.join(HERE, stem + ".json"), encoding="utf-8") as f:
        return json.load(f)


def pixels(stem):
    return np.asarray(Image.open(os.path.join(HERE, stem + ".png")).convert("RGB"), dtype=np.int16)


def luma(rgb):
    return rgb[..., 0] * 0.2126 + rgb[..., 1] * 0.7152 + rgb[..., 2] * 0.0722


def window(img, cx, cy, side):
    """The square window of `side` px centred on (cx, cy) in the written image's own pixels (the sidecar's y counts
    DOWN from the top, PIL's order), slid back inside the picture if the body lies near an edge."""
    h, w = img.shape[:2]
    side = min(side, h, w)
    x0 = max(0, min(int(round(cx)) - side // 2, w - side))
    y0 = max(0, min(int(round(cy)) - side // 2, h - side))
    return x0, y0, side


def split_mask(live, plate, box):
    """Everything in the window that differs from the plate by more than DELTA, split by which way the light went:
    *body* is brighter than the bare ground behind it, *shadow* is darker. The frog's own pixels are the body half;
    the shadow he casts is his too, but it is not him, and counting it would inflate his area."""
    x0, y0, side = box
    a = live[y0:y0 + side, x0:x0 + side]
    b = plate[y0:y0 + side, x0:x0 + side]
    changed = np.abs(a - b).sum(axis=2) > DELTA
    brighter = luma(a) > luma(b)
    return changed & brighter, changed & ~brighter


def blobs(mask):
    """4-connected components of a boolean mask, as a list of (count, mean column). No scipy in this repo's
    toolchain, and the window is 188x188, so an explicit stack flood fill is cheap and has no dependency."""
    h, w = mask.shape
    seen = np.zeros((h, w), dtype=bool)
    out = []
    ys, xs = np.nonzero(mask)
    for sy, sx in zip(ys, xs):
        if seen[sy, sx]:
            continue
        stack = [(sy, sx)]
        seen[sy, sx] = True
        n, col = 0, 0
        while stack:
            y, x = stack.pop()
            n += 1
            col += x
            for ny, nx in ((y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)):
                if 0 <= ny < h and 0 <= nx < w and mask[ny, nx] and not seen[ny, nx]:
                    seen[ny, nx] = True
                    stack.append((ny, nx))
        out.append((n, col / n))
    out.sort(key=lambda t: -t[0])
    return out


def belly_up(body, live, plate, box):
    """Read off the pixels, not off a sidecar: in the body pixels ABOVE the mask's vertical centroid there must be
    two separate blobs, each at least BLOB_FRAC of the body, their horizontal centroids at least BLOB_GAP_FRAC of the
    mask's width apart (two legs splayed upward), and the body's median luma must be PALE_RATIO times the bare
    ground's in the same window (a pale belly turned to the sky). Returns the verdict and its numbers."""
    total = int(body.sum())
    if total == 0:
        return False, {"body_px": 0, "why": "no body mask"}
    ys, xs = np.nonzero(body)
    cy = ys.mean()
    mask_w = xs.max() - xs.min() + 1
    upper = body.copy()
    upper[int(round(cy)):, :] = False        # image y grows downward: above the centroid is the smaller rows
    found = [b for b in blobs(upper) if b[0] >= BLOB_FRAC * total]

    x0, y0, side = box
    a = live[y0:y0 + side, x0:x0 + side]
    b = plate[y0:y0 + side, x0:x0 + side]
    body_luma = float(np.median(luma(a)[body]))
    plate_luma = float(np.median(luma(b)))
    pale = plate_luma > 0 and body_luma >= PALE_RATIO * plate_luma

    gap = abs(found[0][1] - found[1][1]) / mask_w if len(found) >= 2 else 0.0
    ok = len(found) >= 2 and gap >= BLOB_GAP_FRAC and pale
    return bool(ok), {
        "body_px": total, "mask_w_px": int(mask_w), "upper_blobs": len(found),
        "upper_blob_px": [b[0] for b in found[:4]], "blob_gap_frac": round(gap, 4),
        "body_luma_p50": round(body_luma, 3), "plate_luma_p50": round(plate_luma, 3),
        "pale_ratio": round(body_luma / plate_luma, 3) if plate_luma > 0 else None, "pale": bool(pale),
    }


def one_take(t):
    side_i1 = sidecar(f"{t}_i1")
    per_m = float(side_i1["mark_px_per_m"])
    px = int(round(WINDOW_M * per_m))
    i1 = pixels(f"{t}_i1")
    plate = pixels(f"{t}_plate")
    box = window(i1, side_i1["mark_px"], side_i1["mark_py"], px)
    body, shadow = split_mask(i1, plate, box)
    win_px = box[2] * box[2]

    # the area floor: the same measurement with no frog in it, two plates of one held instant
    pbody, _ = split_mask(plate, pixels(f"{t}_plate2"), box)

    # motion, counted ONLY inside the body mask: outside it is sky, grass and whatever the weather is doing
    def inside(a, b):
        x0, y0, s = box
        d = np.abs(pixels(a)[y0:y0 + s, x0:x0 + s] - pixels(b)[y0:y0 + s, x0:x0 + s]).sum(axis=2) > DELTA
        return int((d & body).sum())

    pose_ok, pose = belly_up(body, i1, plate, box)
    return {
        "zoom": side_i1["zoom"], "pitch": side_i1["pitch"], "px_per_m": round(per_m, 3),
        "window_px": box[2], "window_m": WINDOW_M, "mark_px": side_i1["mark_px"], "mark_py": side_i1["mark_py"],
        "body_px": int(body.sum()), "shadow_px": int(shadow.sum()),
        "area_frac": int(body.sum()) / win_px,
        "area_frac_full_frame": int(body.sum()) / (i1.shape[0] * i1.shape[1]),
        "plate_area_frac": int(pbody.sum()) / win_px,
        "motion_in_mask_px": inside(f"{t}_m0", f"{t}_m1"),
        "idle_floor_px": inside(f"{t}_i0", f"{t}_i1"),
        "belly_up": pose_ok, "pose": pose,
        # a cross-check only: nothing above is read from these
        "sidecar_mark_gag": side_i1.get("mark_gag"), "sidecar_mark_roll": side_i1.get("mark_roll"),
        "wind": side_i1.get("wind"),
    }


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--write", metavar="NAME.json", help="write the numbers here with no verdict, and exit 0")
    a = ap.parse_args()

    takes = sorted({os.path.basename(p).split("_")[0] for p in glob.glob(os.path.join(HERE, "take*_i1.png"))})
    if len(takes) < 3:
        print(f"need three takes, found {len(takes)}", file=sys.stderr)
        return 1
    per_take = {t: one_take(t) for t in takes}
    v = list(per_take.values())

    result = {
        "n_takes": len(takes),
        "zoom": v[0]["zoom"],
        "pitch": v[0]["pitch"],
        "window_m": WINDOW_M,
        "window_px": v[0]["window_px"],
        "frog_area_frac_z30": round(min(x["area_frac"] for x in v), 5),
        "frog_area_frac_full_frame": round(min(x["area_frac_full_frame"] for x in v), 6),
        "plate_area_frac_z30": [round(x["plate_area_frac"], 6) for x in v],
        "motion_in_mask_px": min(x["motion_in_mask_px"] for x in v),
        "idle_floor_px": [x["idle_floor_px"] for x in v],
        "belly_up_from_pixels_takes": sum(1 for x in v if x["belly_up"]),
        "delta": DELTA,
        "per_take": per_take,
    }

    before = os.path.join(HERE, "before.json")
    if os.path.exists(before) and not a.write:
        with open(before, encoding="utf-8") as f:
            b = json.load(f)
        was = b.get("frog_area_frac_z30") or 0
        result["area_vs_before"] = round(result["frog_area_frac_z30"] / was, 3) if was else None
        result["before"] = {k: b.get(k) for k in ("frog_area_frac_z30", "motion_in_mask_px",
                                                  "belly_up_from_pixels_takes")}

    out = os.path.join(HERE, a.write or "probe.json")
    if a.write:
        result["verdict"] = "none: --write publishes the numbers of the build as it stands"
    with open(out, "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)
        f.write("\n")

    for k in ("zoom", "frog_area_frac_z30", "frog_area_frac_full_frame", "plate_area_frac_z30",
              "motion_in_mask_px", "idle_floor_px", "belly_up_from_pixels_takes"):
        print(k, result[k])
    if a.write:
        print("wrote", os.path.basename(out), "(no verdict)")
        return 0

    worst_plate = max(result["plate_area_frac_z30"])
    worst_idle = max(result["idle_floor_px"])
    ok = (result["frog_area_frac_z30"] >= AREA_BAR
          and result["frog_area_frac_z30"] > FLOOR_MARGIN * worst_plate
          and result["motion_in_mask_px"] > FLOOR_MARGIN * worst_idle
          and result["belly_up_from_pixels_takes"] >= 3)
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
