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

PASS: both blue shares under 0.02 and the T1 effect area 0.05 or more. (A shot with no effect in it would pass the
blue test for free, which is why the area is part of the gate.)

USAGE (PIL only, as Tools/shotstats.py)
    python docs/evidence/ability-fx-captures-r4/probe.py --pack trench-warfare-3d/Captures/barragedry

It copies every PNG from the capture folder next to itself as a JPG at most 400 KB (quality stepping down from 92
until it fits), copies each CaptureRig sidecar beside it as <stem>.json, then measures THE JPGS IT WROTE - never the
PNGs - and writes probe.json and frames.txt.

THE BEFORE. baseline-barrage-t1.jpg, kept beside this script, is the same still from a run with
TW_BARRAGEDRY_BASELINE=1 (SceneHooks.WaterDepth nulled, so CombatFx is back on the old IsWater test). When it is
there it is measured against plate-t1 too and reported as baseline_blue_share_t1: the A/B, same pose, same clock.

    --keep-baseline   also copy barrage-t1 from the capture folder to baseline-barrage-t1.jpg (the baseline run)

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
        src = os.path.join(a.pack, "barrage-t1.png")
        if not os.path.exists(src):
            print("NOT OK: no barrage-t1.png in %s" % a.pack)
            return 1
        to_jpg(src, "baseline-barrage-t1")
        print("baseline kept as baseline-barrage-t1.jpg")

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
        baseline = diff_row("baseline-barrage-t1", base, os.path.join(HERE, "plate-t1.jpg"))
        rows.append(baseline)

    by = dict((r["still"], r) for r in rows)
    report = {
        "barrage_blue_share_t1": by["barrage-t1"]["blue_share"],
        "barrage_blue_share_o240": by["barrage-o240"]["blue_share"],
        "barrage_effect_area_t1": by["barrage-t1"]["effect_area"],
        "blue_limit": BLUE_LIMIT,
        "effect_area_min": AREA_MIN,
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
            f.write("%s: against %s | effect_area %.4f, blue_share %.4f (raw %.4f) | pose_error_m %s\n"
                    % (r["still"], r["plate"], r["effect_area"], r["blue_share"], r["blue_share_raw"],
                       poses.get(r["still"], "no sidecar")))

    for r in rows:
        print("  %-22s effect_area %.4f blue_share %.4f (raw %.4f)"
              % (r["still"], r["effect_area"], r["blue_share"], r["blue_share_raw"]))
    ok = (report["barrage_blue_share_t1"] < BLUE_LIMIT
          and report["barrage_blue_share_o240"] < BLUE_LIMIT
          and report["barrage_effect_area_t1"] >= AREA_MIN)
    print("blue limit %.2f, effect area at least %.2f: %s" % (BLUE_LIMIT, AREA_MIN, "OK" if ok else "NOT OK"))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
