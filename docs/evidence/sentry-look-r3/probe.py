#!/usr/bin/env python
"""Measure the stills shoot.py shot, in pixels (sentry-look-r3, step 10).

The harsh critic's fix 3 on sentry-look: the plate was drawn at the near tier only and flushed in `dirtMat`, the night
ground's own colour, so at T1 and O120 the Sentry was an MG man double. Only two figures are baked, so the Sentry and
the Machinegunner *are* the same soldier mesh - the whole of his own look is the plate drawn over him. That makes the
claim measurable without any judgement: photograph the two of them side by side, cut each out of the bare ground, and
ask how much of the two silhouettes disagrees.

    silhouette(man)       inside his crop box, where |luma(pair) - luma(empty)| > 18/255
    xor_<band>            the two silhouettes' XOR / their union, for t3, t1 and o120    >= 0.15 passes
    xor_control_<band>    the same measure between the two Machinegunners: the floor this run's own pose noise,
                          shadow and flash put under it, with no plate in the difference at all. Not a threshold -
                          it is there so `xor_<band>` can be read against something. Two men of one archetype in one
                          stance still differ by the phase of their clip, and without this number a critic cannot
                          tell a plate that reads from a pair of men who happened to be mid-stride.
    plate_ground_contrast_t3   |mean luma of pair_t3 inside the plate's own pixels
                                - mean luma of a ring 2-6 px outside them|, both men cut out   >= 0.10 passes

The plate's own pixels come from `mask_t3.png`, where CombatFx.PlateMask draws plate and post in flat magenta: it is
the same pose and the same held weather as `pair_t3.png`, so the magenta is exactly where the steel is in the pair.
That is what makes the contrast a measurement of the plate against the ground it stands on, rather than of a box
somebody drew by hand.

The crop boxes come from `crops.json`, which shoot.py wrote from the pose each sidecar recorded: each man's feet and
head as screen points, padded 0.2 m below the feet, 0.4 m above the head and 2.0 m either side (CombatFx.PlateGrow
stretches the 0.90 m plate up to 3x far off), with a floor of 6, 10 and 14 px so the far band's box still contains a
plate that is deliberately sized in pixels. The three boxes are then grown to one size so the XOR compares equal
areas. Screen y counts up and image rows count down, so shoot.py already wrote rows, not y.

Thresholds are the goal's, not arithmetic: every xor >= 0.15, contrast >= 0.10.

Run from the repo root:  python docs/evidence/sentry-look-r3/probe.py
Writes probe.json beside the stills. Exit 0 when all four pass, 1 when any fails.
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))

MAN_M = 1.70          # m: the height shoot.py projected as the head point, so pixels per metre follow from the box
PAD_BELOW_M = 0.20    # m under the feet: boots, and the ground the post stands on
PAD_ABOVE_M = 0.40    # m over the head: a helmet, and the muzzle flash's room
PAD_SIDE_M = 2.00     # m either side of him: CombatFx.PlateGrow stretches a 0.90 m plate up to 3x far off
# ...and a floor in pixels, because the whole point of PlateGrow is a plate measured in pixels rather than in metres:
# at O120 a man is 8.6 px tall, so a box drawn in metres alone is about 12 px by 9 and does not contain the grown
# plate at all. The first probe run found union=0 there and said so.
PAD_BELOW_PX, PAD_ABOVE_PX, PAD_SIDE_PX = 6, 10, 14
CUT = 18.0 / 255.0    # how far a pixel's luma must move off the bare ground to count as part of a man
RING_IN, RING_OUT = 2, 6   # px: the ground ring sits just clear of the plate, not touching it

XOR_MIN = 0.15
CONTRAST_MIN = 0.10
BANDS = ("t3", "t1", "o120")
MAGENTA, MAGENTA_TOL = (255, 0, 255), 40


def luma(path):
    """The still as luma in 0..1, rows counting down from the top."""
    full = os.path.join(HERE, path)
    if not os.path.exists(full):
        raise SystemExit("missing still: " + full)
    rgb = np.asarray(Image.open(full).convert("RGB")).astype(np.float32) / 255.0
    return rgb[:, :, 0] * 0.2126 + rgb[:, :, 1] * 0.7152 + rgb[:, :, 2] * 0.0722


def magenta_mask(path):
    full = os.path.join(HERE, path)
    if not os.path.exists(full):
        raise SystemExit("missing still: " + full)
    rgb = np.asarray(Image.open(full).convert("RGB")).astype(np.int16)
    d = np.abs(rgb - np.array(MAGENTA, dtype=np.int16))
    return (d <= MAGENTA_TOL).all(axis=2)


def grow(mask, r):
    """Dilate by a disc of radius r, with nothing but numpy: the max over every offset inside the disc."""
    if r <= 0:
        return mask.copy()
    h, w = mask.shape
    out = np.zeros_like(mask)
    for dy in range(-r, r + 1):
        for dx in range(-r, r + 1):
            if dx * dx + dy * dy > r * r:
                continue
            ys, ye = max(0, dy), min(h, h + dy)
            xs, xe = max(0, dx), min(w, w + dx)
            out[ys:ye, xs:xe] |= mask[ys - dy:ye - dy, xs - dx:xe - dx]
    return out


def box_of(man):
    """One man's crop box in rows and columns, from his feet and head screen points.

    His own drawn height in pixels is the measuring stick: shoot.py put the head point MAN_M metres over the foot
    point, so the metre paddings above are pixels without any camera being trusted.
    """
    fx, fr = man["foot"]
    hx, hr = man["head"]
    tall = abs(fr - hr)
    if tall < 2.0:
        raise SystemExit("a man only %.2f px tall: the band is too far to measure honestly" % tall)
    per_m = tall / MAN_M
    cx = (fx + hx) * 0.5
    above = max(PAD_ABOVE_M * per_m, PAD_ABOVE_PX)
    below = max(PAD_BELOW_M * per_m, PAD_BELOW_PX)
    side = max(PAD_SIDE_M * per_m, PAD_SIDE_PX)
    return dict(r0=min(fr, hr) - above, r1=max(fr, hr) + below,
                c0=cx - side, c1=cx + side, tall_px=tall, px_per_m=per_m)


def one_size(boxes):
    """Every box grown to one integer size about its own centre, so an XOR compares equal areas."""
    hgt = int(round(max(b["r1"] - b["r0"] for b in boxes)))
    wid = int(round(max(b["c1"] - b["c0"] for b in boxes)))
    out = []
    for box in boxes:
        rc, cc = (box["r0"] + box["r1"]) * 0.5, (box["c0"] + box["c1"]) * 0.5
        out.append((int(round(rc - hgt / 2.0)), int(round(cc - wid / 2.0))))
    return hgt, wid, out


def cut(img, top, left, hgt, wid):
    """The crop, zero-padded where it runs off the picture, so a man at the edge still gives a same-sized box."""
    out = np.zeros((hgt, wid), dtype=img.dtype)
    r0, c0 = max(0, top), max(0, left)
    r1, c1 = min(img.shape[0], top + hgt), min(img.shape[1], left + wid)
    if r1 > r0 and c1 > c0:
        out[r0 - top:r1 - top, c0 - left:c1 - left] = img[r0:r1, c0:c1]
    return out


def main():
    with open(os.path.join(HERE, "crops.json"), encoding="utf-8") as f:
        crops = json.load(f)

    bands, out_bands = {}, {}
    sil_full = {}
    for band in BANDS:
        c = crops["bands"][band]
        pair, empty = luma(c["pair"]), luma(c["empty"])
        if pair.shape != empty.shape:
            raise SystemExit("%s: the pair and the bare ground are different sizes" % band)
        moved = np.abs(pair - empty) > CUT                       # the whole picture's difference, once

        boxes = [box_of(c["men"][n]) for n in ("sentry", "gunner", "control")]
        hgt, wid, corners = one_size(boxes)
        a, b, ctl = [cut(moved, t, l, hgt, wid) for (t, l) in corners]
        union = int(np.count_nonzero(a | b))
        if union == 0:
            raise SystemExit("%s: neither man differs from the bare ground - did the field really clear?" % band)
        xor = int(np.count_nonzero(a ^ b)) / float(union)
        u_ctl = int(np.count_nonzero(b | ctl))
        xor_ctl = int(np.count_nonzero(b ^ ctl)) / float(u_ctl) if u_ctl else float("nan")

        # the two men's own pixels in the full picture, for the contrast ring to avoid
        full = np.zeros_like(moved)
        for (top, left) in corners:
            r0, c0 = max(0, top), max(0, left)
            r1, c1 = min(moved.shape[0], top + hgt), min(moved.shape[1], left + wid)
            if r1 > r0 and c1 > c0:
                full[r0:r1, c0:c1] |= moved[r0:r1, c0:c1]
        sil_full[band] = (full, pair)

        bands[band] = dict(xor=round(xor, 4), xor_control=round(xor_ctl, 4),
                           sentry_px=int(np.count_nonzero(a)), gunner_px=int(np.count_nonzero(b)),
                           control_px=int(np.count_nonzero(ctl)),
                           union_px=union, xor_px=int(np.count_nonzero(a ^ b)),
                           box=[hgt, wid], tall_px=[round(bx["tall_px"], 2) for bx in boxes],
                           pose_check_m=c.get("pose_check_m"))
        out_bands["xor_" + band] = round(xor, 4)
        out_bands["xor_control_" + band] = round(xor_ctl, 4)

    # the plate against the ground it stands on, at T3
    plate = magenta_mask("mask_t3.png")
    if not plate.any():
        raise SystemExit("no magenta in mask_t3.png: was CombatFx.PlateMask on, and the plate in frame?")
    men_t3, pair_t3 = sil_full["t3"]
    ring = grow(plate, RING_OUT) & ~grow(plate, RING_IN) & ~men_t3 & ~plate
    inside = plate & ~men_t3
    if not inside.any() or not ring.any():
        raise SystemExit("the plate or its ground ring is entirely hidden behind the men: nothing to compare")
    mean_in, mean_ring = float(pair_t3[inside].mean()), float(pair_t3[ring].mean())
    contrast = abs(mean_in - mean_ring)

    out = {
        "xor_t3": out_bands["xor_t3"],
        "xor_t1": out_bands["xor_t1"],
        "xor_o120": out_bands["xor_o120"],
        "plate_ground_contrast_t3": round(contrast, 4),
        "xor_control_t3": out_bands["xor_control_t3"],
        "xor_control_t1": out_bands["xor_control_t1"],
        "xor_control_o120": out_bands["xor_control_o120"],
        "thresholds": {"xor": XOR_MIN, "plate_ground_contrast_t3": CONTRAST_MIN},
        "settings": {"man_m": MAN_M, "pad_below_m": PAD_BELOW_M, "pad_above_m": PAD_ABOVE_M,
                     "pad_side_m": PAD_SIDE_M,
                     "pad_px_floor": [PAD_BELOW_PX, PAD_ABOVE_PX, PAD_SIDE_PX], "luma_cut": round(CUT, 5),
                     "ring_px": [RING_IN, RING_OUT], "magenta_tol": MAGENTA_TOL},
        "bands": bands,
        "plate_t3": {"plate_px": int(np.count_nonzero(plate)), "measured_px": int(np.count_nonzero(inside)),
                     "ring_px": int(np.count_nonzero(ring)),
                     "mean_luma_plate": round(mean_in, 4), "mean_luma_ground_ring": round(mean_ring, 4)},
    }
    xors = [out["xor_t3"], out["xor_t1"], out["xor_o120"]]
    out["pass"] = bool(min(xors) >= XOR_MIN and contrast >= CONTRAST_MIN)

    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")

    for band in BANDS:
        b = bands[band]
        print("xor_%-5s %8.4f  (>= %.2f)   %s   control %.4f (no plate in it)   sentry %d px, gunner %d px, "
              "xor %d of %d in a %dx%d box"
              % (band, b["xor"], XOR_MIN, "pass" if b["xor"] >= XOR_MIN else "FAIL", b["xor_control"],
                 b["sentry_px"], b["gunner_px"], b["xor_px"], b["union_px"], b["box"][0], b["box"][1]))
    print("plate_ground_contrast_t3 %.4f  (>= %.2f)   %s   plate %.4f vs ground ring %.4f over %d/%d px"
          % (contrast, CONTRAST_MIN, "pass" if contrast >= CONTRAST_MIN else "FAIL",
             mean_in, mean_ring, int(np.count_nonzero(inside)), int(np.count_nonzero(ring))))
    return 0 if out["pass"] else 1


if __name__ == "__main__":
    sys.exit(main())
