#!/usr/bin/env python
"""Measure the stills shoot.py shot, in pixels (sentry-look-r3-2, step 3).

Only two figures are baked, so the Sentry and the Machinegunner *are* the same soldier mesh - the whole of his own look
is the plate drawn over him. r3 measured that by standing three men side by side, and its own control (MG man against
MG man) came back higher than the claim: the XOR was reading stride, not steel. Here the same frozen man is shot three
times a band, so the numbers are the plate and nothing else:

    silhouette(still)     inside his crop box, where |luma(still) - luma(empty)| > 18/255
    xor_<band>            |sentry ^ mg| / |sentry | mg|                                     >= 0.15 passes
    xor_control_<band>    |mg ^ mg2| / |mg | mg2|: two stills of one frozen frame with nothing changed between them
    xor_control_max       the worst of those three                                          <= 0.03 passes
    plate_ground_contrast_t3   |mean luma of sentry_t3 inside the plate's own pixels
                                - mean luma of a ring 2-6 px outside them|                  >= 0.10 passes

**r3's contrast bug, fixed.** r3 took `inside = plate & ~men`, where `men` was the silhouette of the *Sentry* - which
already holds the plate's own pixels. So it averaged only the 706 of 2519 plate pixels that did *not* differ from the
bare ground, i.e. exactly the pixels where the plate reads like dirt, and came back with 0.0259. The occluder here is
the man **without** his plate, `mg_t3`:

    inside = plate & ~mg_sil
    ring   = grow(plate, 6) & ~grow(plate, 2) & ~mg_sil & ~plate

and both are read off `sentry_t3.png`.

The magenta classifier is also loosened. `CombatFx.PlateMask` draws the plate unlit magenta, but the night grade moves
it: the tight distance-40 test found 2519 px where the plate's projected area at T3 is about 10,500 (0.90 x 0.62 m at
137 px/m). The test is now `r > g + 40 and b > g + 40 and min(r, b) > 60`, and `plate_px` is printed beside the
expected area - under half of it, fix the mask before judging the material.

**The crop box is measured, not projected.** r3 built it from a camera rebuilt out of the sidecar's pose; in this run
that arithmetic came back 13.3 m off the camera the rig actually used (`pose_check_m` in crops.json), which would have
put the box in the wrong place. There is one man in the frame and nothing else moves, so the box is simply the bounding
box of everything that differs from the bare ground - the three stills' silhouettes unioned - grown by 8 px. One box
per band, used for all three stills, so the XOR compares equal areas.

Metres come from the plate's own 0.90 m width in `mask_t3.png`: the mask mode's cyan calibration bar is too dark in
the night grade to measure (510 scattered pixels over 235 rows).

Thresholds are the goal's, not arithmetic: every xor >= 0.15, xor_control_max <= 0.03, contrast >= 0.10.

Run from the repo root:  python docs/evidence/sentry-look-r3-2/probe.py
Writes probe.json beside the stills. Exit 0 when all five bars pass, 1 when any fails.
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))

PAD_PX = 8            # px grown round the measured bounding box, so nothing of the man sits on its edge
CUT = 18.0 / 255.0    # how far a pixel's luma must move off the bare ground to count as part of a man
BAR_M = 0.50          # m: CombatFx.PlateBar, the cyan calibration bar of the mask mode
RING_IN, RING_OUT = 2, 6   # px: the ground ring sits just clear of the plate, not touching it

XOR_MIN = 0.15
CONTROL_MAX = 0.03
CONTRAST_MIN = 0.10
BANDS = ("t3", "t1", "o120")
PLATE_W_M, PLATE_H_M = 0.90, 0.62     # CombatFx.PlatePieces, before UnitScale: what plate_px is read against


def rgb_of(path):
    full = os.path.join(HERE, path)
    if not os.path.exists(full):
        raise SystemExit("missing still: " + full)
    return np.asarray(Image.open(full).convert("RGB"))


def luma(path):
    """The still as luma in 0..1, rows counting down from the top."""
    rgb = rgb_of(path).astype(np.float32) / 255.0
    return rgb[:, :, 0] * 0.2126 + rgb[:, :, 1] * 0.7152 + rgb[:, :, 2] * 0.0722


def magenta_mask(path):
    """The flat-magenta plate of the mask mode, classified by hue rather than by distance to pure magenta: the night
    grade darkens and tints it, so a tight distance test loses most of the steel."""
    rgb = rgb_of(path).astype(np.int16)
    r, g, b = rgb[:, :, 0], rgb[:, :, 1], rgb[:, :, 2]
    return (r > g + 40) & (b > g + 40) & (np.minimum(r, b) > 60)


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


def cyan_mask(path):
    """The calibration bar of the mask mode, by the same hue test the other way round."""
    rgb = rgb_of(path).astype(np.int16)
    r, g, b = rgb[:, :, 0], rgb[:, :, 1], rgb[:, :, 2]
    return (g > r + 40) & (b > r + 40) & (np.minimum(g, b) > 60)


def dense_span(count, pad):
    """The first and last index where `count` is dense, grown by `pad`.

    The night battle rains, and two stills of one frozen frame still differ by a few hundred scattered droplet pixels
    (that is what `xor_control` measures). A plain bounding box of everything that differs is therefore most of the
    picture - 760 x 1642 px in the first measured run, for a man 108 px tall - and the man's own silhouette drowns in
    it. A row or column only counts as his when at least a tenth of his busiest one differs, and never under 3.
    """
    peak = int(count.max())
    thr = max(3, int(round(peak * 0.1)))
    hit = np.flatnonzero(count >= thr)
    if hit.size == 0:
        raise SystemExit("nothing dense differs from the bare ground: did the field really clear?")
    return int(hit[0]) - pad, int(hit[-1]) + pad


def bbox_of(mask, pad=PAD_PX):
    """The box round the man and his plate, grown by `pad` px: the dense rows and the dense columns of everything that
    differs from the bare ground. There is one man in the frame and the clock is held, so this is him - no camera is
    trusted anywhere in it."""
    top, bot = dense_span(mask.sum(axis=1), pad)
    left, right = dense_span(mask.sum(axis=0), pad)
    return dict(top=top, left=left, hgt=bot - top + 1, wid=right - left + 1)


def cut(img, box):
    """The crop, zero-padded where it runs off the picture."""
    top, left, hgt, wid = box["top"], box["left"], box["hgt"], box["wid"]
    out = np.zeros((hgt, wid), dtype=img.dtype)
    r0, c0 = max(0, top), max(0, left)
    r1, c1 = min(img.shape[0], top + hgt), min(img.shape[1], left + wid)
    if r1 > r0 and c1 > c0:
        out[r0 - top:r1 - top, c0 - left:c1 - left] = img[r0:r1, c0:c1]
    return out


def in_box(mask, box):
    """The same mask, zeroed everywhere outside the box: the box's own pixels in full-picture coordinates."""
    out = np.zeros_like(mask)
    top, left, hgt, wid = box["top"], box["left"], box["hgt"], box["wid"]
    r0, c0 = max(0, top), max(0, left)
    r1, c1 = min(mask.shape[0], top + hgt), min(mask.shape[1], left + wid)
    if r1 > r0 and c1 > c0:
        out[r0:r1, c0:c1] = mask[r0:r1, c0:c1]
    return out


def ratio(a, b):
    """|a ^ b| / |a | b|, and the two counts behind it."""
    union = int(np.count_nonzero(a | b))
    x = int(np.count_nonzero(a ^ b))
    return (x / float(union) if union else float("nan")), x, union


def main():
    with open(os.path.join(HERE, "crops.json"), encoding="utf-8") as f:
        crops = json.load(f)

    bands, out_bands = {}, {}
    sil_t3 = {}
    for band in BANDS:
        c = crops["bands"][band]
        empty = luma(c["empty"])
        moved = {}
        for who in ("sentry", "mg", "mg2"):
            img = luma(c[who])
            if img.shape != empty.shape:
                raise SystemExit("%s: %s and the bare ground are different sizes" % (band, who))
            moved[who] = np.abs(img - empty) > CUT          # the whole picture's difference, once
        box = bbox_of(moved["sentry"] | moved["mg"] | moved["mg2"])
        sils = {who: cut(moved[who], box) for who in moved}
        xor, xor_px, union = ratio(sils["sentry"], sils["mg"])
        ctl, ctl_px, ctl_union = ratio(sils["mg"], sils["mg2"])
        if band == "t3":
            sil_t3 = dict(mg=in_box(moved["mg"], box), sentry_luma=luma(c["sentry"]))

        bands[band] = dict(xor=round(xor, 4), xor_control=round(ctl, 4),
                           sentry_px=int(np.count_nonzero(sils["sentry"])),
                           mg_px=int(np.count_nonzero(sils["mg"])),
                           mg2_px=int(np.count_nonzero(sils["mg2"])),
                           union_px=union, xor_px=xor_px,
                           control_union_px=ctl_union, control_xor_px=ctl_px,
                           box=[box["hgt"], box["wid"]], box_at=[box["top"], box["left"]],
                           pose_check_m=c.get("pose_check_m"))
        out_bands["xor_" + band] = round(xor, 4)
        out_bands["xor_control_" + band] = round(ctl, 4)

    # the plate against the ground it stands on, at T3. The occluder is the man WITHOUT his plate (mg_t3), so the
    # plate's own pixels are never cut out of their own measurement - r3's bug.
    plate = magenta_mask("mask_t3.png")
    if not plate.any():
        raise SystemExit("no magenta in mask_t3.png: was CombatFx.PlateMask on, and the plate in frame?")
    mg_sil, pair_t3 = sil_t3["mg"], sil_t3["sentry_luma"]
    ring = grow(plate, RING_OUT) & ~grow(plate, RING_IN) & ~mg_sil & ~plate
    inside = plate & ~mg_sil
    if not inside.any() or not ring.any():
        raise SystemExit("the plate or its ground ring is entirely hidden behind the man: nothing to compare")
    mean_in, mean_ring = float(pair_t3[inside].mean()), float(pair_t3[ring].mean())
    contrast = abs(mean_in - mean_ring)
    # Metres from the plate's own width. The mask mode also draws a 0.50 m cyan calibration bar, but in the night
    # grade it comes back as 510 scattered pixels over 235 rows - not a bar - so the scale here is the magenta
    # plate's column span against the 0.90 m CombatFx.PlatePieces gives it.
    plate_cols = np.flatnonzero(plate.any(axis=0))
    per_m = (int(plate_cols[-1]) - int(plate_cols[0]) + 1) / PLATE_W_M
    expect_px = PLATE_W_M * PLATE_H_M * per_m ** 2
    plate_px = int(np.count_nonzero(plate))

    control_max = max(out_bands["xor_control_" + b] for b in BANDS)
    out = {
        "xor_t3": out_bands["xor_t3"],
        "xor_t1": out_bands["xor_t1"],
        "xor_o120": out_bands["xor_o120"],
        "xor_control_max": round(control_max, 4),
        "plate_ground_contrast_t3": round(contrast, 4),
        "xor_control_t3": out_bands["xor_control_t3"],
        "xor_control_t1": out_bands["xor_control_t1"],
        "xor_control_o120": out_bands["xor_control_o120"],
        "thresholds": {"xor": XOR_MIN, "xor_control_max": CONTROL_MAX,
                       "plate_ground_contrast_t3": CONTRAST_MIN},
        "settings": {"luma_cut": round(CUT, 5), "ring_px": [RING_IN, RING_OUT], "pad_px": PAD_PX,
                     "magenta": "r > g+40 and b > g+40 and min(r,b) > 60",
                     "cyan": "g > r+40 and b > r+40 and min(g,b) > 60"},
        "bands": bands,
        "plate_t3": {"plate_px": plate_px, "expected_px": int(round(expect_px)),
                     "px_per_m_from_bar": round(per_m, 2),
                     "measured_px": int(np.count_nonzero(inside)),
                     "ring_px": int(np.count_nonzero(ring)),
                     "mean_luma_plate": round(mean_in, 4), "mean_luma_ground_ring": round(mean_ring, 4)},
    }
    xors = [out["xor_t3"], out["xor_t1"], out["xor_o120"]]
    out["pass"] = bool(min(xors) >= XOR_MIN and control_max <= CONTROL_MAX and contrast >= CONTRAST_MIN)

    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")

    for band in BANDS:
        b = bands[band]
        print("xor_%-5s %8.4f  (>= %.2f)   %s   control %.4f (%d of %d px, one frozen frame twice)   "
              "sentry %d px, mg %d px, xor %d of %d in a %dx%d measured box"
              % (band, b["xor"], XOR_MIN, "pass" if b["xor"] >= XOR_MIN else "FAIL", b["xor_control"],
                 b["control_xor_px"], b["control_union_px"], b["sentry_px"], b["mg_px"], b["xor_px"],
                 b["union_px"], b["box"][0], b["box"][1]))
    print("xor_control_max %.4f  (<= %.2f)   %s"
          % (control_max, CONTROL_MAX, "pass" if control_max <= CONTROL_MAX else "FAIL"))
    print("plate_ground_contrast_t3 %.4f  (>= %.2f)   %s   plate %.4f vs ground ring %.4f over %d/%d px"
          % (contrast, CONTRAST_MIN, "pass" if contrast >= CONTRAST_MIN else "FAIL",
             mean_in, mean_ring, int(np.count_nonzero(inside)), int(np.count_nonzero(ring))))
    print("plate mask %d px, the plate's projected area at T3 is about %d px (%.0f px/m)"
          % (plate_px, int(round(expect_px)), per_m))
    if plate_px < expect_px * 0.5:
        print("  !! the mask holds under half the plate: fix the mask before judging the material")
    return 0 if out["pass"] else 1


if __name__ == "__main__":
    sys.exit(main())
