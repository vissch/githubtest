#!/usr/bin/env python3
"""Pack and measure the ability/environment FX stills (gas, the smoke screen, the HE barrage, the water splash and the
ParaDrop), so the critic's two complaints are numbers and not an opinion.

WHY. The harsh critic scored this group 29/100 with "no capture at all", naming two faults: gas floats instead of
pooling, and the night water splash is the most saturated thing in frame. Both are measurable from the pixels:
`green_low_share` is how much of a gas still's green sits in the lower third of its column (pooling, not floating),
and `sat_p99` is how saturated the brightest-coloured 1% of a frame is.

USAGE (PIL only, as Tools/shotstats.py)
    python docs/evidence/ability-fx-captures/probe.py --pack trench-warfare-3d/Captures/abilityfx

It copies every PNG from the capture folder next to itself as a JPG at most 400 KB (quality stepping down from 92
until it fits), copies each CaptureRig sidecar beside it as <subject>-<band>.json, then measures THE JPGS IT WROTE -
never the PNGs - and writes probe.json and frames.txt. Exit 0 when at least ten stills were packed and every one is
under 2% blown out.

READING IT
    luma_mean      0-255 Rec.601 (shotstats' scale, not CaptureRig's 0-1 Rec.709).
    blown_frac     share of pixels at 250+, 0-1. Over 0.02 is a blown still: reshoot off the flash peak.
    sat_p99        99th percentile HSV saturation, 0-1. The water splash should not top the frame it sits in.
    green_low_share  gas stills only: of the pixels in the gas hue window (HSV hue 45-95), the share in the lower
                   third of the subject's own screen column (the middle third of the frame's width). 1 is a bank
                   lying on the ground, 0 a cloud floating over it.
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
GAS_HUE_LOW, GAS_HUE_HIGH = 45, 95          # PIL HSV hue is 0-255: the gas window the banks are tinted into
BLOWN_LIMIT = 0.02
WANT_STILLS = 10


def pack(src):
    """Copy each PNG in src beside this script as a JPG under MAX_BYTES, and its sidecar as <stem>.json. Returns the
    JPG paths it wrote, in name order."""
    out = []
    for png in sorted(glob.glob(os.path.join(src, "*.png"))):
        stem = os.path.splitext(os.path.basename(png))[0]
        jpg = os.path.join(HERE, stem + ".jpg")
        img = Image.open(png).convert("RGB")
        for q in range(92, 29, -6):
            img.save(jpg, "JPEG", quality=q, optimize=True)
            if os.path.getsize(jpg) <= MAX_BYTES:
                break
        side = os.path.splitext(png)[0] + ".json"
        if os.path.exists(side):
            shutil.copyfile(side, os.path.join(HERE, stem + ".json"))
        out.append(jpg)
    return out


def percentile(hist, p):
    n = sum(hist)
    if n == 0:
        return 0
    acc, target = 0, n * p
    for v, c in enumerate(hist):
        acc += c
        if acc >= target:
            return v
    return 255


def green_low_share(img):
    """Of the gas-hue pixels in the middle third of the frame (the subject's own column), the share sitting in the
    lower third of it."""
    w, h = img.size
    col = img.crop((w // 3, 0, 2 * w // 3, h))
    hsv = col.convert("HSV")
    H, S, V = hsv.split()
    cw, ch = col.size
    hp, sp, vp = H.load(), S.load(), V.load()
    total = low = 0
    for y in range(0, ch, 2):
        for x in range(0, cw, 2):
            if sp[x, y] < 50 or vp[x, y] < 30:
                continue
            if not (GAS_HUE_LOW <= hp[x, y] <= GAS_HUE_HIGH):
                continue
            total += 1
            if y >= 2 * ch // 3:
                low += 1
    return round(low / total, 3) if total else None


def measure(jpg):
    img = Image.open(jpg).convert("RGB")
    name = os.path.splitext(os.path.basename(jpg))[0]
    subject, _, band = name.rpartition("-")
    L = img.convert("L")                      # PIL's L is Rec.601, as shotstats.py
    hist = L.histogram()
    n = sum(hist)
    row = {
        "still": name, "subject": subject, "band": band,
        "luma_mean": round(sum(v * c for v, c in enumerate(hist)) / n, 1),
        "blown_frac": round(sum(hist[250:]) / n, 4),
        "sat_p99": round(percentile(img.convert("HSV").split()[1].histogram(), 0.99) / 255.0, 3),
        "bytes": os.path.getsize(jpg),
    }
    if subject == "gas":
        row["green_low_share"] = green_low_share(img)
    return row


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--pack", required=True, help="the capture folder the stills test wrote (Captures/abilityfx)")
    a = ap.parse_args()
    jpgs = pack(a.pack)
    rows = [measure(j) for j in jpgs]
    worst = max((r["blown_frac"] for r in rows), default=1.0)
    report = {"stills": len(rows), "max_blown_frac": worst, "rows": rows}
    with open(os.path.join(HERE, "probe.json"), "w") as f:
        json.dump(report, f, indent=2, sort_keys=True)
        f.write("\n")
    moments = {}
    mpath = os.path.join(a.pack, "moments.txt")
    if os.path.exists(mpath):
        for line in open(mpath):
            if ":" in line:
                head, _, tail = line.partition(":")
                moments[head.split(" at ")[0].strip()] = (head.strip(), tail.strip())
    with open(os.path.join(HERE, "frames.txt"), "w") as f:
        for r in rows:
            where, moment = moments.get(r["subject"], ("", "unrecorded"))
            f.write("%s: %s, band %s | %s | clock running, weather pinned | %s\n"
                    % (r["still"], where or r["subject"], r["band"], moment,
                       "luma %.1f, blown %.4f, sat_p99 %.3f" % (r["luma_mean"], r["blown_frac"], r["sat_p99"])))
    print("stills %d" % len(rows))
    print("max_blown_frac %.4f" % worst)
    for r in rows:
        print("  %-14s luma %5.1f blown %.4f sat_p99 %.3f%s"
              % (r["still"], r["luma_mean"], r["blown_frac"], r["sat_p99"],
                 "" if "green_low_share" not in r else " green_low_share %s" % r["green_low_share"]))
    ok = len(rows) >= WANT_STILLS and worst < BLOWN_LIMIT
    if not ok:
        print("NOT OK: %d stills (want %d), max blown %.4f (want under %.2f)" % (len(rows), WANT_STILLS, worst, BLOWN_LIMIT))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
