#!/usr/bin/env python3
"""Measure each class's muzzle flash against the shooter's own drawn height, from the pixels.

WHY. The special-units critique (2026-10-19) said the rifle muzzle starburst is torso-sized and hides the shooter.
"Torso-sized" is a ratio, and a ratio read off a picture by eye is worth nothing. This measures it: the flash's
bounding box height in pixels over the shooter's bounding box height in pixels, in the same pose. The man box is
MEASURED (his drawn silhouette), not the 2.0 m FigureMetrics constant, so camera, zoom and pose cancel out.

The caps are the owner's, through the critic: rifle <= 0.4, mg <= 0.6; the flamer and the AT may be bigger.

INPUT, per class, all at one pose for that class (CaptureRig.Bench; the exact lines are in README.md beside this file)
    <class>_empty.png            the pose with no men at all (no scenario=)
    <class>_idle.png             the men standing, nobody firing small arms (knobs=bench.lineupFire=2)
    <class>_fire.png             that class alone firing (knobs=bench.lineupOnly=N shot_frames=10): the frame of
                                 the ten-frame burst whose flash was largest, the only one kept
All three are resampled to 1280x720 before they are committed (228 MB of 2560x1440 stills has no business in the
repo). A ratio does not care about scale.

WHY A POSE PER CLASS, AND WHY TEN FRAMES (both found the hard way, 2026-10-19)
  * The lineup row is 28 m wide once the flamer and the AT rifle are in it, and at T2 (zoom 16) only the middle of
    it is on screen: at the shared focus the rifleman and the AT man were off the frame. So each class is shot at
    ITS OWN man's x, with its own empty and idle plates.
  * A single still at shot_tick caught the machine gun (it fires every tick) and MISSED the rifle, whose card had
    already gone. shot_frames=10 takes ten held frames and the one with the largest flash is the one kept.
  * Both boxes are looked for in a STRIP down the middle of the frame only. The camera is focused on this class's
    own man, so he is in the middle; the rest of the frame is a live battle full of burning ground, and the largest
    hot blob in a machine-gun frame was a lit trench forty metres away.

HOW
    hot pixel   changed from idle by more than 24 luma AND luma > 120 AND R > B AND G >= 0.45*R. The last two are
                what separate a muzzle flash (orange to white) from BLOOD, which is red with G and B together and
                which the plain warm rule of Tools/shotstats.py swallowed whole (it picked a blood spurt as the
                rifle's "flash").
    flash box   label the hot mask, drop any blob wider than 3x its height (a tracer streak), take the largest.
                Flicker the whole set shares (a burning wreck, a lamp) is removed first: a pixel hot in three or
                more of the four classes is not a muzzle flash.
    man box     |idle - empty| luma > 16, labelled, blobs under 40 px tall dropped; the blob whose x range is
                nearest the flash's centre column. That is the shooter's drawn silhouette.
    ratio       flash box height / man box height, 3 places.

OUTPUT  probe.json ({"rifle":..., "mg":..., "flamer":..., "at":...}), a table of the pixel boxes, and
        <class>_boxes.png with both boxes drawn, so a human can see the measurement is right.
        --check exits 1 when rifle > 0.4 or mg > 0.6, or when any blob is missing (never silently 0).
"""
import glob
import json
import os
import sys

import numpy as np
from PIL import Image, ImageDraw
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))
CLASSES = ("rifle", "mg", "flamer", "at")
CAPS = {"rifle": 0.4, "mg": 0.6}
FLASH_DIFF, MAN_DIFF, HOT_LUMA, MAN_MIN_H = 24.0, 16.0, 120.0, 20
STRIP = 0.17          # half-width of the middle strip, as a share of the frame width


def rgb(path):
    return np.asarray(Image.open(path).convert("RGB"), dtype=np.float32)


def luma(a):
    return 0.299 * a[..., 0] + 0.587 * a[..., 1] + 0.114 * a[..., 2]


def hot_mask(a):
    """Orange-to-white and bright: a muzzle flash, not blood and not a dark red prop."""
    r, g, b = a[..., 0], a[..., 1], a[..., 2]
    return (luma(a) > HOT_LUMA) & (r > b) & (g >= 0.45 * r)


def frames(c):
    """The fire stills of class `c`: the still itself and its held-clock siblings."""
    first = os.path.join(HERE, c + "_fire.png")
    return ([first] if os.path.exists(first) else []) + sorted(glob.glob(os.path.join(HERE, c + "_fire.f*.png")))


def box_of(lab, label):
    ys, xs = np.where(lab == label)
    return int(xs.min()), int(ys.min()), int(xs.max()), int(ys.max())


def strip_only(mask):
    """Everything outside the middle strip dropped: only the man the camera is focused on is measured."""
    out = np.zeros_like(mask)
    w = mask.shape[1]
    x0, x1 = int(w * (0.5 - STRIP)), int(w * (0.5 + STRIP))
    out[:, x0:x1] = mask[:, x0:x1]
    return out


def flash_in(mask):
    """The largest blob that is not a streak; None when there is none."""
    if not mask.any():
        return None
    lab, n = ndimage.label(mask)
    sizes = ndimage.sum(mask, lab, range(1, n + 1))
    for t in np.argsort(sizes)[::-1]:
        x0, y0, x1, y1 = box_of(lab, int(t) + 1)
        if (x1 - x0 + 1) > 3 * (y1 - y0 + 1):
            continue
        return x0, y0, x1, y1
    return None


def man_box(idle, empty, flash):
    mask = strip_only(np.abs(luma(idle) - luma(empty)) > MAN_DIFF)
    if not mask.any():
        return None
    lab, n = ndimage.label(mask)
    cx = (flash[0] + flash[2]) * 0.5
    best, bestd = None, None
    for l in range(1, n + 1):
        x0, y0, x1, y1 = box_of(lab, l)
        if (y1 - y0 + 1) < MAN_MIN_H:
            continue
        dist = 0.0 if x0 <= cx <= x1 else min(abs(cx - x0), abs(cx - x1))
        if bestd is None or dist < bestd:
            best, bestd = (x0, y0, x1, y1), dist
    return best


def overlay(path, png, flash, man):
    img = Image.open(png).convert("RGB")
    d = ImageDraw.Draw(img)
    d.rectangle(flash, outline=(255, 64, 64), width=3)
    d.rectangle(man, outline=(64, 160, 255), width=3)
    img.save(path)


def main():
    check = "--check" in sys.argv
    missing = []
    for c in CLASSES:
        for p in (c + "_idle.png", c + "_empty.png"):
            if not os.path.exists(os.path.join(HERE, p)):
                missing.append(p)
        if not frames(c):
            missing.append(c + "_fire.png")
    if missing:
        print("missing stills: " + ", ".join(missing))
        return 1

    # the flicker the whole set shares (a burning wreck, a lamp) is not a muzzle flash
    hot, idles = {}, {}
    for c in CLASSES:
        idles[c] = rgb(os.path.join(HERE, c + "_idle.png"))
        li = luma(idles[c])
        hot[c] = [((np.abs(luma(rgb(p)) - li) > FLASH_DIFF) & hot_mask(rgb(p)), p) for p in frames(c)]
    shared = {c: (sum(m.astype(np.int8) for m, _ in hot[c]) > 0) for c in CLASSES}
    common = sum(s.astype(np.int8) for s in shared.values()) >= 3

    out, bad = {}, []
    print("%-8s %26s %8s %7s %7s %6s" % ("class", "frame", "flash h", "man h", "ratio", "cap"))
    for c in CLASSES:
        best = None
        for m, png in hot[c]:
            fb = flash_in(strip_only(m & ~common))
            if fb is not None and (best is None or (fb[3] - fb[1]) > (best[0][3] - best[0][1])):
                best = (fb, png)
        if best is None:
            bad.append(c + ": no flash blob in any frame")
            print("%-8s %26s %8s %7s %7s" % (c, "-", "-", "-", "-"))
            continue
        fb, png = best
        mb = man_box(idles[c], rgb(os.path.join(HERE, c + "_empty.png")), fb)
        if mb is None:
            bad.append(c + ": no man blob")
            print("%-8s %26s %8d %7s %7s" % (c, os.path.basename(png), fb[3] - fb[1] + 1, "-", "-"))
            continue
        fh, mh = fb[3] - fb[1] + 1, mb[3] - mb[1] + 1
        ratio = round(fh / mh, 3)
        out[c] = ratio
        cap = CAPS.get(c)
        verdict = "-" if cap is None else ("ok" if ratio <= cap else "OVER")
        print("%-8s %26s %8d %7d %7.3f %6s" % (c, os.path.basename(png), fh, mh, ratio, verdict))
        if cap is not None and ratio > cap:
            bad.append("%s: %s over the cap %s" % (c, ratio, cap))
        overlay(os.path.join(HERE, c + "_boxes.png"), png, fb, mb)

    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as fh:
        json.dump(out, fh, indent=2, sort_keys=True)
        fh.write("\n")
    print("wrote probe.json: " + json.dumps(out, sort_keys=True))
    for b in bad:
        print("FAIL " + b)
    return 1 if (check and bad) else 0


if __name__ == "__main__":
    sys.exit(main())
