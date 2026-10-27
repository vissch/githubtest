#!/usr/bin/env python
"""Measure the Sentry's plate in the picture the player actually sees (sentry-look-r4-2-g2).

The critic's point about r4: its probe counted magenta only, and in its unmasked night still magenta was 0. So the
number it published (11415 px alive against 11421 px dead) was measured in a flat debug mask, never in the look the
player sees. In play the plate is drawn lit with dirtMat - a dirt-coloured box in a dirt-coloured night - so there is
no colour to count it by.

The honest measure of such a thing is a difference pair: the same frozen instant rendered with the plate and without
it (CombatFx.PlateOff skips the draw after the emplacement loop, so the plate returns in exactly the same place).
shoot.py shoots three pairs with the clock never released between them, so nothing but the plate differs inside a
pair, plus one masked still whose magenta silhouette says which pixels are the plate itself and not the shadow it
casts.

    plate_px_i   pixels inside the magenta silhouette where night_i differs from off_i by >= THRESH on any channel
    cover_i      plate_px_i / silhouette pixels: how much of the plate really changes the night picture
    diff_all_i   the same count over the whole frame (plate plus its shadow), context only
    noise_px     the same count between night_1 and night_2 - two identical renders, the renderer's own flicker

BAR is hard-coded below from the first good capture run, at 90 % of the smallest of the three, and the probe asserts
the owner's rule on itself: the bar has to sit outside the spread of three captures of the current build
(spread <= 5 % of min, and BAR <= min - spread), and above the renderer's own flicker (BAR > noise_px). A later
build whose plate fades into the night fails on BAR; a bar pulled down inside the noise fails on the relations.

Run from the repo root:  python docs/evidence/sentry-look-r4-2-g2/probe.py
Writes probe.json beside the stills. Exit 0 when every check passes, 1 when any fails.
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
BAR_M = 0.50          # the calibration bar's height in metres (CombatFx.PlateBar)
TOL = 40              # per-channel tolerance out of 255 for the flat unlit mask colours
MAGENTA = (255, 0, 255)
CYAN = (0, 255, 255)

THRESH = 6            # a pixel counts as changed when any channel moves by this much out of 255
COVER = 0.50          # at least half the plate's silhouette must really change the night picture
SPREAD = 0.05         # three captures of one build may differ by this much of the smallest, no more
BAR = 10281          # the floor, in night pixels, set at 0.90 * min from the capture run of 2026-10-27

PAIRS = (1, 2, 3)
UNMASKED = tuple("night_%d" % i for i in PAIRS) + tuple("off_%d" % i for i in PAIRS)


def mask(rgb, want):
    """Where the image is within TOL of `want` on every channel."""
    d = np.abs(rgb.astype(np.int16) - np.array(want, dtype=np.int16))
    return (d <= TOL).all(axis=2)


def extent(m):
    """count, top row, bottom row, and the bounding-box centre (row, col); rows count down from the top."""
    rows, cols = np.nonzero(m)
    if rows.size == 0:
        return dict(count=0, top=None, bottom=None, centre=None)
    return dict(count=int(rows.size), top=int(rows.min()), bottom=int(rows.max()),
                centre=[(float(rows.min()) + float(rows.max())) / 2.0,
                        (float(cols.min()) + float(cols.max())) / 2.0])


def rgb_of(stem):
    path = os.path.join(HERE, stem + ".png")
    if not os.path.exists(path):
        raise SystemExit("missing still: " + path)
    return np.asarray(Image.open(path).convert("RGB"))


def sidecar(stem):
    path = os.path.join(HERE, stem + ".json")
    if not os.path.exists(path):
        raise SystemExit("missing sidecar: " + path)
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def counter(side, *names):
    """A CaptureRig sidecar counter, wherever the rig put it (top level, or under a nested dict)."""
    for n in names:
        if n in side:
            return side[n]
    for v in side.values():
        if isinstance(v, dict):
            for n in names:
                if n in v:
                    return v[n]
    return None


def changed(a, b):
    """Where two stills differ by at least THRESH on any channel."""
    return (np.abs(a.astype(np.int16) - b.astype(np.int16)) >= THRESH).any(axis=2)


def main():
    m_rgb = rgb_of("mask")
    sil = mask(m_rgb, MAGENTA)
    sil_x = extent(sil)
    if not sil_x["count"]:
        raise SystemExit("no plate silhouette in mask.png: is CombatFx.PlateMask reaching the batch?")
    bar = extent(mask(m_rgb, CYAN))
    if not bar["count"] or bar["bottom"] == bar["top"]:
        raise SystemExit("no calibration bar in mask.png: is the plate in frame?")
    px_per_m = (bar["bottom"] - bar["top"]) / BAR_M
    foot_gap_m = max(0.0, (bar["bottom"] - sil_x["bottom"]) / px_per_m)

    pairs, stray = {}, {}
    for i in PAIRS:
        night, off = rgb_of("night_%d" % i), rgb_of("off_%d" % i)
        if night.shape != off.shape or night.shape[:2] != m_rgb.shape[:2]:
            raise SystemExit("pair %d is not the same size as mask.png" % i)
        d = changed(night, off)
        pairs[i] = dict(plate_px=int((d & sil).sum()), diff_all=int(d.sum()))
        pairs[i]["cover"] = round(pairs[i]["plate_px"] / float(sil_x["count"]), 4)
    for s in UNMASKED:
        stray[s] = dict(magenta=int(mask(rgb_of(s), MAGENTA).sum()),
                        cyan=int(mask(rgb_of(s), CYAN).sum()))

    noise_px = int((changed(rgb_of("night_1"), rgb_of("night_2")) & sil).sum())
    counts = [pairs[i]["plate_px"] for i in PAIRS]
    lo, hi = min(counts), max(counts)
    spread = hi - lo

    checks = {
        "spread_within_5pct_of_min": spread <= SPREAD * lo,
        "bar_outside_the_spread": BAR <= lo - spread,
        "bar_above_the_flicker": BAR > noise_px,
        "every_capture_over_the_bar": all(c >= BAR for c in counts),
        "every_capture_covers_half_the_plate": all(pairs[i]["cover"] >= COVER for i in PAIRS),
        "no_mask_colour_in_the_unmasked_stills": all(v["magenta"] == 0 and v["cyan"] == 0
                                                     for v in stray.values()),
    }
    out = {
        "plate_px": {("night_%d" % i): pairs[i]["plate_px"] for i in PAIRS},
        "cover": {("night_%d" % i): pairs[i]["cover"] for i in PAIRS},
        "diff_all": {("night_%d" % i): pairs[i]["diff_all"] for i in PAIRS},
        "plate_px_min": lo, "plate_px_max": hi, "plate_px_spread": spread,
        "noise_px": noise_px,
        "bar": BAR,
        "silhouette_px": sil_x["count"],
        "px_per_m": round(px_per_m, 2),
        "foot_gap_m": round(foot_gap_m, 4),
        "threshold_per_channel": THRESH,
        "cover_floor": COVER,
        "spread_allowed": SPREAD,
        "bar_m": BAR_M,
        "mask_tolerance_per_channel": TOL,
        "mask": dict(file="mask.png", magenta=sil_x, cyan=bar),
        "unmasked_mask_colours": stray,
        "fallen": counter(sidecar("night_1"), "fallen", "fallen_count", "fallen_men"),
        "drawn_infantry": counter(sidecar("night_1"), "drawn_infantry", "men_in_frame", "drawn"),
        "pose_error_m": {s: sidecar(s).get("pose_error_m") for s in ("mask",) + UNMASKED},
        "checks": checks,
    }
    out["pass"] = all(checks.values())

    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")

    ok = lambda b: "pass" if b else "FAIL"
    print("px_per_m                   %8.2f  (cyan bar %d px for %.2f m)"
          % (px_per_m, bar["bottom"] - bar["top"], BAR_M))
    print("silhouette_px              %8d  (the plate's own pixels, mask.png)" % sil_x["count"])
    print("foot_gap_m                 %8.4f  (context)" % foot_gap_m)
    for i in PAIRS:
        print("plate_px night_%d           %8d  cover %.3f   diff_all %d"
              % (i, pairs[i]["plate_px"], pairs[i]["cover"], pairs[i]["diff_all"]))
    print("plate_px_min / max         %8d / %d" % (lo, hi))
    print("plate_px_spread            %8d  (<= %d)      %s"
          % (spread, int(SPREAD * lo), ok(checks["spread_within_5pct_of_min"])))
    print("noise_px                   %8d  (two identical renders)" % noise_px)
    print("bar                        %8d  (<= %d, > %d)  %s"
          % (BAR, lo - spread, noise_px,
             ok(checks["bar_outside_the_spread"] and checks["bar_above_the_flicker"])))
    print("every capture over the bar  %7s              %s"
          % ("yes" if checks["every_capture_over_the_bar"] else "no",
             ok(checks["every_capture_over_the_bar"])))
    print("every capture covers >= %.2f %7s              %s"
          % (COVER, "yes" if checks["every_capture_covers_half_the_plate"] else "no",
             ok(checks["every_capture_covers_half_the_plate"])))
    print("no mask colour unmasked     %7s              %s"
          % ("yes" if checks["no_mask_colour_in_the_unmasked_stills"] else "no",
             ok(checks["no_mask_colour_in_the_unmasked_stills"])))
    print("fallen / drawn_infantry    %8s / %s  (context)" % (out["fallen"], out["drawn_infantry"]))
    return 0 if out["pass"] else 1


if __name__ == "__main__":
    sys.exit(main())
