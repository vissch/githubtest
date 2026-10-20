#!/usr/bin/env python3
"""Measures the gun alone, not the pose.

r3 proved the AT rifleman no longer reads as the Sniper, but said plainly that the 0.827 it measured is carried
by the POSE (he lies flat, the Sniper kneels) and that the 1.65 m anti-tank rifle itself is not separately
legible at T1 in those crops. This probe isolates the gun: two stills of the SAME frozen man, from the SAME
camera, one with the gun drawn and one with `atrifle.draw` set to 0 (CombatFx.AtRifle.cs). Every pixel that
differs between that pair is the gun — the pose does not move, so it cannot enter the number.

The mask, the window, the morphology and the guards are r3's unchanged: THRESHOLD 0.04, WINDOW
(slice(300,405), slice(720,880)), a 3x3 opening then 7x7 closing, the blob filter, a loud failure if the mask
touches the window border, and the pose_error_m / rain sidecar guards.

Per still the probe measures, from the mask alone:
  gun_px          mask pixels
  gun_len_px      the bounding-box diagonal of the largest blob
  gun_width_px    gun_px / gun_len_px — the gun's average thickness across its length
  gun_contrast    the median of |delta luma| over the mask

Three base stills (the current build, before this unit's fix) give gun_px_base_min/max and
gun_px_base_spread — the noise floor of three repeats of one frozen pose. With no fix stills present yet the
probe only reports the base numbers and exits 0; once fix-gun-1/2/3.png and fix-nogun.png exist it also enforces
the bar: the fixed build's gun has to read outside the base spread, and on its own terms —
MIN_GUN_PX (a smaller mask is not a gun), gun_width_px >= 2.5 (PlaygroundFx's rule for a line that must read) and
gun_contrast >= 0.10.

Every number here is read from the pixels. None is typed.
"""
import json
import os
import sys

import numpy as np
import scipy.ndimage as nd
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 0.04
WINDOW = (slice(300, 430), slice(700, 900))   # y, x — r3's window widened: the fattened gun's own diff bbox at
                                               # this leg's pose reaches y415, past r3's y405, so the window grew
                                               # by 12+ px on every side rather than crop the thing being measured
MIN_BLOB = 300
MIN_GUN_PX = 25
MIN_WIDTH_PX = 2.5
MIN_CONTRAST = 0.10

BASE_GUN = ["base-gun-1.png", "base-gun-2.png", "base-gun-3.png"]
FIX_GUN = ["fix-gun-1.png", "fix-gun-2.png", "fix-gun-3.png"]


def here(name):
    return os.path.join(HERE, name)


def luma(path):
    a = np.asarray(Image.open(here(path)).convert("RGB"), float) / 255.0
    return a[..., 0] * 0.2126 + a[..., 1] * 0.7152 + a[..., 2] * 0.0722


def delta(gun_on, gun_off):
    return luma(gun_on) - luma(gun_off)


def mask_from_delta(d):
    m = np.abs(d) > THRESHOLD
    m = nd.binary_closing(nd.binary_opening(m, np.ones((3, 3))), np.ones((7, 7)))
    windowed = np.zeros_like(m)
    windowed[WINDOW] = m[WINDOW]
    lab, n = nd.label(windowed)
    sizes = np.bincount(lab.ravel())
    sizes[0] = 0
    keep = [i for i in range(1, n + 1) if sizes[i] >= MIN_BLOB]
    return np.isin(lab, keep)


def border_touch(m):
    """True when the mask reaches the edge of the window, which means it is cut."""
    ys, xs = WINDOW
    box = m[ys, xs]
    if not box.any():
        return False
    return bool(box[0].any() or box[-1].any() or box[:, 0].any() or box[:, -1].any())


def largest_blob_bbox_diag(m):
    lab, n = nd.label(m)
    if n == 0:
        return 0.0
    sizes = np.bincount(lab.ravel())
    sizes[0] = 0
    best = int(np.argmax(sizes))
    ys, xs = np.where(lab == best)
    return float(np.hypot(ys.max() - ys.min() + 1, xs.max() - xs.min() + 1))


def measure(gun_on, gun_off, report, tag):
    d = delta(gun_on, gun_off)
    m = mask_from_delta(d)
    gun_px = int(m.sum())
    gun_len = largest_blob_bbox_diag(m)
    gun_width = gun_px / gun_len if gun_len > 0 else 0.0
    gun_contrast = float(np.median(np.abs(d[m]))) if gun_px > 0 else 0.0
    report["gun_px_" + tag] = gun_px
    report["gun_len_px_" + tag] = round(gun_len, 2)
    report["gun_width_px_" + tag] = round(gun_width, 3)
    report["gun_contrast_" + tag] = round(gun_contrast, 4)
    return m, gun_px, gun_len, gun_width, gun_contrast


def pose_errors(names):
    out = {}
    for n in names:
        p = here(os.path.splitext(n)[0] + ".json")
        if os.path.exists(p):
            with open(p, encoding="utf-8") as f:
                d = json.load(f)
            out[n] = (float(d["pose_error_m"]), float(d["rain"]))
    return out


def main():
    report = {}
    problems = []

    def built(stills, off, tag_prefix, enforce):
        results = []
        for s in stills:
            if not os.path.exists(here(s)) or not os.path.exists(here(off)):
                continue
            tag = tag_prefix + "_" + os.path.splitext(s)[0].rsplit("-", 1)[-1]
            m, gun_px, gun_len, gun_width, gun_contrast = measure(s, off, report, tag)
            if enforce:
                if gun_px < MIN_GUN_PX:
                    problems.append(f"{s}: only {gun_px} px of gun (want {MIN_GUN_PX}) — that is not a gun")
                if border_touch(m):
                    problems.append(f"{s}: the gun mask touches the window border — it is cut")
            results.append((gun_px, gun_width, gun_contrast))
        return results

    # base measures the CURRENT, unfixed build — it may show next to no gun at all, which is the bug this unit
    # measures; only the fix stills (once they exist) are held to MIN_GUN_PX and the border guard.
    base = built(BASE_GUN, "base-nogun.png", "base", enforce=False)
    fix = built(FIX_GUN, "fix-nogun.png", "fix", enforce=True)

    if base:
        pxs = [r[0] for r in base]
        report["gun_px_base_min"] = min(pxs)
        report["gun_px_base_max"] = max(pxs)
        report["gun_px_base_spread"] = max(pxs) - min(pxs)

    if fix:
        report["n_fix_captures"] = len(fix)
        worst_width = min(r[1] for r in fix)
        worst_contrast = min(r[2] for r in fix)
        report["gun_width_px"] = round(worst_width, 3)
        report["gun_contrast"] = round(worst_contrast, 4)
        if worst_width < MIN_WIDTH_PX:
            problems.append(f"gun_width_px is {worst_width:.3f}, want at least {MIN_WIDTH_PX}")
        if worst_contrast < MIN_CONTRAST:
            problems.append(f"gun_contrast is {worst_contrast:.4f}, want at least {MIN_CONTRAST}")
        if base:
            min_fix_px = min(r[0] for r in fix)
            if not min_fix_px > report["gun_px_base_max"]:
                problems.append(
                    f"the fixed gun's smallest reading ({min_fix_px} px) does not sit outside the base "
                    f"spread (max {report['gun_px_base_max']} px)")

    poses = pose_errors(BASE_GUN + FIX_GUN + ["base-nogun.png", "fix-nogun.png"])
    if poses:
        report["pose_error_m_max"] = max(v[0] for v in poses.values())
        report["rain_max"] = max(v[1] for v in poses.values())
        if report["pose_error_m_max"] >= 0.5:
            problems.append(f"pose_error_m is {report['pose_error_m_max']}, want under 0.5")
        if report["rain_max"] != 0:
            problems.append(f"rain is {report['rain_max']}, want 0")

    report["threshold_luma"] = THRESHOLD
    report["window_xywh"] = [WINDOW[1].start, WINDOW[0].start,
                             WINDOW[1].stop - WINDOW[1].start,
                             WINDOW[0].stop - WINDOW[0].start]
    report["problems"] = problems

    with open(here("probe.json"), "w", encoding="utf-8") as f:
        json.dump(report, f, indent=2)
        f.write("\n")
    print(json.dumps(report, indent=2))

    if problems:
        for p in problems:
            print("FAIL: " + p, file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
