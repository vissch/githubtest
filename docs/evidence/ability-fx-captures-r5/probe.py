#!/usr/bin/env python3
"""Pack and measure the dry-ground HE barrage stills, so the harsh critic's fix 2 is a number and not an opinion.

WHY. Critic r1: the HE barrage on dry ground showed a blue water plume, the same drawing as the water ability
(barrage-t1.jpg, barrage-o240.jpg). The cause was CombatFx reading SceneHooks.IsWater, which calls a painted damp
patch and every dressed shell hole on the flooded night field "water". On dry ground the barrage has to read as
earth, fire and dark smoke.

HOW IT IS MEASURED. BarrageDryStills shoots four stills at ONE pose: a no-effect plate and the barrage, at T1
(zoom 30) and at O240. Per band the plate is subtracted from the shot:

    mask         a pixel where any channel differs from the plate by more than DIFF (20/255) - the effect itself
    blue_share   of the mask pixels, the share with b > r+38 and b > g+38 (0.15 on 0-1) - the water plume's tint -
                 AFTER a speckle filter: a blue pixel counts only when at least 6 of its 8 neighbours are blue too
    blue_share_raw  the same before the filter, always reported, never the gate
    effect_area  the mask as a share of the whole frame - proof something was actually drawn

WHY THE SPECKLE FILTER. At O240 the night field's moonlit ground is a dithered blue speckle that shimmers between
two frames taken a few seconds apart: single pixels, scattered over ground the burst never touched, enter the mask
and are blue. Measured on the stills here they are 8.6% of the O240 mask raw and 1.4% solid. A water plume is a
drawn CARD - a connected patch - so wanting six blue neighbours keeps a plume and drops the shimmer. It is a filter
on the SHAPE, not on the colour rule, and the raw number stays in probe.json beside it.

NEW IN R5 (critic r1, fixes 1-2). The r4 shots were taken with the ability's yellow aim disc on screen, so
effect_area 0.8097 measured the marker, and the baseline had no sidecar and was 0.36% of pixels from the after shot.
The stills are now shot with CaptureRig.QuietFrame() (overlays off, shake 0), the barrage is staged on damp-but-
shallow ground where the old code really does splash, and the baseline run keeps its own plate and both sidecars.
This script adds:

    outside_bbox_luma_diff   the burst's bbox is the 1st-99th percentile of the mask's sampled coordinates per axis,
                             padded 8 samples; outside it, the mean |luma(shot) - luma(plate)| on 0-255 Rec.601.
                             A marker, a HUD element or a global brightening lands outside the burst and shows here.
                             A mask under 0.1% of the frame has no meaningful bbox: reported 0.0, and the area gate
                             catches that shot instead.

PASS (the owner's list for this unit, and nothing else): barrage_effect_area_t1 in 0.05-0.4,
barrage_effect_area_o240 >= 0.01, baseline_blue_share_t1 >= 0.10, barrage_blue_share_t1 < 0.02,
outside_bbox_luma_diff <= 3. The O240 blue share is still measured and reported, but it is the moonlit ground's
dithered speckle as much as anything drawn (r4's note below), and this unit's pass list does not name it. (A shot with no effect in it would pass the
blue test for free, which is why the area is part of the gate.)

USAGE (PIL only, as Tools/shotstats.py)
    python docs/evidence/ability-fx-captures-r5/probe.py --pack trench-warfare-3d/Captures/barragedry

It copies every PNG from the capture folder next to itself as a JPG at most 400 KB (quality stepping down from 92
until it fits), copies each CaptureRig sidecar beside it as <stem>.json, then measures THE JPGS IT WROTE - never the
PNGs - and writes probe.json and frames.txt.

THE BEFORE. baseline-barrage-t1.jpg, kept beside this script, is the same still from a run with
TW_BARRAGEDRY_BASELINE=1 (SceneHooks.WaterDepth nulled, so CombatFx is back on the old IsWater test). When it is
there it is measured against plate-t1 too and reported as baseline_blue_share_t1: the A/B, same pose, same clock.

    --keep-baseline   a baseline run: keep barrage-t1 as baseline-barrage-t1.jpg and plate-t1 as
                      baseline-plate-t1.jpg, each with its CaptureRig sidecar as .json, and stop. The baseline row is
                      then measured against baseline-plate-t1.jpg - its own run, its own pose.

Stdlib + PIL only. ASCII only.
"""
import argparse
import glob
import json
import os
import shutil
import sys

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
MAX_BYTES = 400 * 1024
DIFF = 20           # 0-255: a channel has to move this much before a pixel counts as the effect
BLUE_LEAD = 38      # 0-255 (0.15 on 0-1): how far blue has to lead red and green to be the water plume's tint
NEIGHBOURS = 6      # of the 8 around it, how many must be blue too before a blue pixel is a plume and not shimmer
BLUE_LIMIT = 0.02
AREA_MIN = 0.05
AREA_MAX = 0.4             # over this the frame is smoke, not a burst (or a marker is still in it)
AREA_MIN_O240 = 0.01       # at 240 m the burst is small, but it has to be there
BASELINE_BLUE_MIN = 0.10   # the old code's blue plume, the "before" half of the A/B
OUTSIDE_DIFF_MAX = 3.0     # 0-255 luma: outside the burst's bbox the shot and its plate are the same picture
BBOX_PAD = 8               # samples of slack around the percentile box
BBOX_MIN_AREA = 0.001      # under this there is no burst to put a box around
BANDS = ("t1", "o240")


def to_jpg(png, stem):
    jpg = os.path.join(HERE, stem + ".jpg")
    img = Image.open(png).convert("RGB")
    for q in range(92, 29, -6):
        img.save(jpg, "JPEG", quality=q, optimize=True)
        if os.path.getsize(jpg) <= MAX_BYTES:
            break
    return jpg


def pack(src):
    """Copy each PNG in src beside this script as a JPG under MAX_BYTES, and its sidecar as <stem>.json. Returns
    the JPG paths it wrote, in name order."""
    out = []
    for png in sorted(glob.glob(os.path.join(src, "*.png"))):
        stem = os.path.splitext(os.path.basename(png))[0]
        out.append(to_jpg(png, stem))
        side = os.path.splitext(png)[0] + ".json"
        if os.path.exists(side):
            shutil.copyfile(side, os.path.join(HERE, stem + ".json"))
    return out


def outside_luma_diff(pa, pb, inmask, sw, sh):
    """The mean |luma(shot) - luma(plate)| on 0-255 Rec.601 OUTSIDE the burst's bounding box. The box is the
    1st-99th percentile of the mask's sampled coordinates per axis (so a few stray pixels do not stretch it to the
    whole frame), padded BBOX_PAD samples. Everything outside it should be the same picture twice: an aim marker, a
    HUD element or a global brightening shows up here as a number above zero."""
    xs = [i for j in range(sh) for i in range(sw) if inmask[j * sw + i]]
    if len(xs) < BBOX_MIN_AREA * sw * sh:
        return 0.0
    ys = [j for j in range(sh) for i in range(sw) if inmask[j * sw + i]]
    xs.sort()
    ys.sort()

    def band(v):
        lo = v[int(0.01 * (len(v) - 1))]
        hi = v[int(0.99 * (len(v) - 1))]
        return lo - BBOX_PAD, hi + BBOX_PAD

    x0, x1 = band(xs)
    y0, y1 = band(ys)
    total = 0.0
    count = 0
    for j in range(sh):
        inrow = y0 <= j <= y1
        for i in range(sw):
            if inrow and x0 <= i <= x1:
                continue
            r, g, bl = pa[i * 2, j * 2]
            r0, g0, b0 = pb[i * 2, j * 2]
            total += abs((0.299 * r + 0.587 * g + 0.114 * bl) - (0.299 * r0 + 0.587 * g0 + 0.114 * b0))
            count += 1
    return round(total / count, 3) if count else 0.0


def diff_row(name, shot, plate):
    """The effect in `shot` against its own plate: how much of the frame it covers, and how blue it is."""
    a = Image.open(shot).convert("RGB")
    b = Image.open(plate).convert("RGB")
    if a.size != b.size:
        raise SystemExit("%s and its plate are different sizes" % name)
    pa, pb = a.load(), b.load()
    w, h = a.size
    sw, sh = w // 2, h // 2                 # every second pixel, as shotstats samples
    inmask = bytearray(sw * sh)
    isblue = bytearray(sw * sh)
    mask = 0
    for j in range(sh):
        for i in range(sw):
            r, g, bl = pa[i * 2, j * 2]
            r0, g0, b0 = pb[i * 2, j * 2]
            if abs(r - r0) <= DIFF and abs(g - g0) <= DIFF and abs(bl - b0) <= DIFF:
                continue
            inmask[j * sw + i] = 1
            mask += 1
            if bl > r + BLUE_LEAD and bl > g + BLUE_LEAD:
                isblue[j * sw + i] = 1
    raw = sum(isblue)
    outside = outside_luma_diff(pa, pb, inmask, sw, sh)
    solid = 0
    for j in range(1, sh - 1):
        for i in range(1, sw - 1):
            if not isblue[j * sw + i]:
                continue
            near = sum(isblue[(j + dj) * sw + i + di] for dj in (-1, 0, 1) for di in (-1, 0, 1)) - 1
            if near >= NEIGHBOURS:
                solid += 1
    return {
        "still": name,
        "plate": os.path.basename(plate),
        "effect_area": round(mask / float(sw * sh), 4),
        "blue_share": round(solid / float(mask), 4) if mask else 0.0,
        "blue_share_raw": round(raw / float(mask), 4) if mask else 0.0,
        "mask_pixels": mask,
        "sampled_pixels": sw * sh,
        "outside_bbox_luma_diff": outside,
    }


def pose_errors():
    """Every sidecar's pose_error_m, by still. The plate and the shot share one pose, so the mask is the effect."""
    out = {}
    for j in sorted(glob.glob(os.path.join(HERE, "*.json"))):
        if os.path.basename(j) == "probe.json":
            continue
        try:
            side = json.load(open(j, encoding="utf-8"))
        except ValueError:
            continue
        if "pose_error_m" in side:
            out[os.path.splitext(os.path.basename(j))[0]] = side["pose_error_m"]
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--pack", required=True, help="the capture folder BarrageDryStills wrote (Captures/barragedry)")
    ap.add_argument("--keep-baseline", action="store_true",
                    help="a baseline run: keep barrage-t1 as baseline-barrage-t1.jpg and stop")
    a = ap.parse_args()
    pack(a.pack)
    if a.keep_baseline:
        for src_stem, dst_stem in (("barrage-t1", "baseline-barrage-t1"), ("plate-t1", "baseline-plate-t1")):
            src = os.path.join(a.pack, src_stem + ".png")
            if not os.path.exists(src):
                print("NOT OK: no %s.png in %s" % (src_stem, a.pack))
                return 1
            to_jpg(src, dst_stem)
            side = os.path.join(a.pack, src_stem + ".json")
            if not os.path.exists(side):
                print("NOT OK: no sidecar %s.json: the baseline pose would go unreported" % src_stem)
                return 1
            shutil.copyfile(side, os.path.join(HERE, dst_stem + ".json"))
            print("baseline kept as %s.jpg with its sidecar" % dst_stem)
        return 0

    rows = []
    for band in BANDS:
        shot = os.path.join(HERE, "barrage-%s.jpg" % band)
        plate = os.path.join(HERE, "plate-%s.jpg" % band)
        if not (os.path.exists(shot) and os.path.exists(plate)):
            print("NOT OK: missing barrage-%s.jpg or plate-%s.jpg" % (band, band))
            return 1
        rows.append(diff_row("barrage-%s" % band, shot, plate))
    base = os.path.join(HERE, "baseline-barrage-t1.jpg")
    baseline = None
    if os.path.exists(base):
        # its own plate when the baseline run kept one (same run, same pose), else the after run's
        bplate = os.path.join(HERE, "baseline-plate-t1.jpg")
        if not os.path.exists(bplate):
            bplate = os.path.join(HERE, "plate-t1.jpg")
        baseline = diff_row("baseline-barrage-t1", base, bplate)
        rows.append(baseline)

    by = dict((r["still"], r) for r in rows)
    report = {
        "barrage_blue_share_t1": by["barrage-t1"]["blue_share"],
        "barrage_blue_share_o240": by["barrage-o240"]["blue_share"],
        "barrage_effect_area_t1": by["barrage-t1"]["effect_area"],
        "barrage_effect_area_o240": by["barrage-o240"]["effect_area"],
        "outside_bbox_luma_diff": by["barrage-t1"]["outside_bbox_luma_diff"],
        "blue_limit": BLUE_LIMIT,
        "effect_area_min": AREA_MIN,
        "effect_area_max": AREA_MAX,
        "effect_area_min_o240": AREA_MIN_O240,
        "baseline_blue_min": BASELINE_BLUE_MIN,
        "outside_bbox_luma_diff_max": OUTSIDE_DIFF_MAX,
        "pose_error_m": pose_errors(),
        "rows": rows,
    }
    if baseline:
        report["baseline_blue_share_t1"] = baseline["blue_share"]
        report["baseline_effect_area_t1"] = baseline["effect_area"]
    with open(os.path.join(HERE, "probe.json"), "w") as f:
        json.dump(report, f, indent=2, sort_keys=True)
        f.write("\n")

    moments = ""
    mpath = os.path.join(a.pack, "moments.txt")
    if os.path.exists(mpath):
        moments = open(mpath, encoding="utf-8").read()
    poses = report["pose_error_m"]
    with open(os.path.join(HERE, "frames.txt"), "w") as f:
        f.write("GreyboxCorridor, NightMud (the owner's night look), clock running, weather pinned at 30 s.\n")
        f.write(moments if moments.endswith("\n") or not moments else moments + "\n")
        for r in rows:
            f.write("%s: against %s | effect_area %.4f, blue_share %.4f (raw %.4f), outside_bbox_luma_diff %.3f"
                    " | pose_error_m %s\n"
                    % (r["still"], r["plate"], r["effect_area"], r["blue_share"], r["blue_share_raw"],
                       r["outside_bbox_luma_diff"], poses.get(r["still"], "no sidecar")))

    for r in rows:
        print("  %-22s effect_area %.4f blue_share %.4f (raw %.4f) outside_bbox_luma_diff %.3f"
              % (r["still"], r["effect_area"], r["blue_share"], r["blue_share_raw"], r["outside_bbox_luma_diff"]))
    checks = [
        ("barrage_effect_area_t1 in %.2f-%.2f" % (AREA_MIN, AREA_MAX),
         AREA_MIN <= report["barrage_effect_area_t1"] <= AREA_MAX),
        ("barrage_effect_area_o240 >= %.2f" % AREA_MIN_O240,
         report["barrage_effect_area_o240"] >= AREA_MIN_O240),
        ("baseline_blue_share_t1 >= %.2f" % BASELINE_BLUE_MIN,
         report.get("baseline_blue_share_t1", 0.0) >= BASELINE_BLUE_MIN),
        ("barrage_blue_share_t1 < %.2f" % BLUE_LIMIT, report["barrage_blue_share_t1"] < BLUE_LIMIT),
        ("outside_bbox_luma_diff <= %.1f" % OUTSIDE_DIFF_MAX,
         report["outside_bbox_luma_diff"] <= OUTSIDE_DIFF_MAX),
    ]
    for what, good in checks:
        print("  %-34s %s" % (what, "ok" if good else "FAILED"))
    ok = all(good for _, good in checks)
    print("OK" if ok else "NOT OK")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
