#!/usr/bin/env python3
"""Is a Death Battalion man a different shape on screen than a rifleman? Measured three times, against the noise.

The first round (docs/evidence/death-battalion-look/) reported 0.728 against a rifleman-versus-rifleman noise floor
of 0.404, and its three masks were 3460, 2300 and 4337 px: two men who ARE the same rifleman differed by 1.9x in
size, so most of what it measured was where each man stood. `stage.py` fixes the staging instead of the number --
one patch of ground, one held instant, one background per instant, all three men in the same animation clip -- and
shoots each man at three instants. This probe then reads nine masks, not three.

A pixel that differs by more than TOL between the man's still and the background still of the SAME instant is the
man. The largest blob is him; it is recentred in a BOX x BOX window, so the shape difference does not care where
in the crop he stood:

    silhouette_diff = count(maskA xor maskB) / count(maskA or maskB)

Two gates before any number is believed:

* The BAND. All nine masks must be within +/-15 % of their median, and none may touch the crop border. A mask that
  is half the size of its neighbours, or one that the crop cut in half, means the staging failed (an occluder, a
  clipped man) -- that is a restage, not a looser tolerance. Exit 2.
* The BAR is outside the noise. `noise[i]` is rifleman against rifleman at instant i: the same measurement between
  two men who should look identical, so it is pose, shadow and rain. BAR is asserted to sit a full spread-width
  above the top of the three noise numbers, so it cannot be quietly written under the noise it came from. Pass is
  the WORST of the three signals clearing it.

    python docs/evidence/death-battalion-look-g2/probe.py     writes probe.json beside this file and prints it
"""
import json
import sys
from pathlib import Path

import numpy as np
from PIL import Image
from scipy import ndimage

HERE = Path(__file__).resolve().parent
# Measured on the stills committed at a207416d50: an instant's background is pixel-identical between its four
# stills away from the man (median per-pixel diff 1.0 of 765, p90 3.0). TOL = 80 was rejecting 70% of the man
# (1818 px of a 6334 px man); TOL = 6 keeps the whole man and still clears the rain/ground noise.
TOL = 6       # summed over RGB, 0-765: just above the pixel-identical background's own noise (p90 3.0)
BOX = 200     # the box each man's mask is recentred into, px: a man at this zoom and 3840x2160 is about 70 px tall
BAND = 0.15   # every mask within +/-15 % of the median OF ITS OWN INSTANT, or the staging failed
INSTANTS = 3

# Measured on the build at 10162a2b95 by this probe with BAR = 0, on the g2-staged stills (pose matched per
# instant): the three rifleman-versus-rifleman numbers were 0.0008, 0.006 and 0.0023. BAR = max + (max - min) =
# 0.006 + 0.0052 = 0.0112, rounded up to 0.012.
BAR = 0.012


def man(stem, i):
    """The man's own pixels: the largest blob of what changed when he left this instant's frame."""
    a = np.asarray(Image.open(HERE / f'{stem}_t1_{i}.png').convert('RGB'), dtype=float)
    b = np.asarray(Image.open(HERE / f'bg_{i}.png').convert('RGB'), dtype=float)
    if a.shape != b.shape:
        raise SystemExit(f'{stem}_t1_{i}: the still and its background are different sizes')
    moved = np.abs(a - b).sum(axis=2) > TOL
    moved = ndimage.binary_opening(moved, np.ones((3, 3)))     # single rain pixels are not a man
    moved = ndimage.binary_closing(moved, np.ones((5, 5)))     # but the holes inside him are
    labels, n = ndimage.label(moved)
    if n == 0:
        raise SystemExit(f'{stem}_t1_{i}: nothing changed when the man left: he was not in frame')
    sizes = [int(np.count_nonzero(labels == k)) for k in range(1, n + 1)]
    blob = labels == int(np.argmax(sizes)) + 1
    touches = bool(blob[0].any() or blob[-1].any() or blob[:, 0].any() or blob[:, -1].any())
    padded = np.pad(blob, BOX)
    cy, cx = (int(round(v)) for v in ndimage.center_of_mass(padded))
    return padded[cy - BOX // 2:cy + BOX // 2, cx - BOX // 2:cx + BOX // 2], touches


def diff(a, b):
    union = np.count_nonzero(a | b)
    if union == 0:
        raise SystemExit('both masks are empty')
    return np.count_nonzero(a ^ b) / union


def main():
    stems = ['rifle', 'rifle2', 'deathbn']
    masks, areas, clipped = {}, {}, []
    for stem in stems:
        for i in range(INSTANTS):
            m, touches = man(stem, i)
            masks[(stem, i)] = m
            areas[f'{stem}_{i}'] = int(np.count_nonzero(m))
            if touches:
                clipped.append(f'{stem}_t1_{i}.png')

    median = float(np.median(list(areas.values())))
    ratio = round(max(areas.values()) / min(areas.values()), 3)
    noise = [round(diff(masks[('rifle', i)], masks[('rifle2', i)]), 4) for i in range(INSTANTS)]
    signal = [round(diff(masks[('rifle', i)], masks[('deathbn', i)]), 4) for i in range(INSTANTS)]

    # Were the three men photographed alike within EACH instant? Across instants the pose is meant to vary
    # (clip phase, body yaw), so compare each mask only to the median of its own instant.
    band_worst_per_instant = []
    band_fail = None
    for i in range(INSTANTS):
        instant_areas = {f'{stem}_{i}': areas[f'{stem}_{i}'] for stem in stems}
        instant_median = float(np.median(list(instant_areas.values())))
        worst_key = max(instant_areas, key=lambda k: abs(instant_areas[k] - instant_median))
        worst_dev = abs(instant_areas[worst_key] - instant_median) / instant_median
        band_worst_per_instant.append(round(worst_dev, 4))
        if worst_dev > BAND and band_fail is None:
            band_fail = (worst_key, instant_areas[worst_key], instant_median)

    out = {
        'noise_rifleman_vs_rifleman': noise,
        'signal_death_battalion_vs_rifleman': signal,
        'bar': BAR,
        'min_signal': min(signal),
        'mask_px': areas,
        'mask_median_px': median,
        'mask_band_ratio_max_over_min': ratio,
        'mask_band_worst_per_instant': band_worst_per_instant,
        'band': BAND,
        'tolerance': TOL,
        'box_px': BOX,
        'clipped_by_crop': clipped,
        'build': '5ba0fafb5a',
        'files': sorted(f'{stem}_t1_{i}.png' for stem in stems for i in range(INSTANTS))
                 + [f'bg_{i}.png' for i in range(INSTANTS)],
    }
    (HERE / 'probe.json').write_text(json.dumps(out, indent=1), encoding='utf-8')
    print(json.dumps(out, indent=1))

    if clipped:
        print('BAND FAIL: the crop cut these masks at its border, restage them: ' + ', '.join(clipped))
        return 2
    if band_fail:
        worst_key, worst_px, instant_median = band_fail
        print(f'BAND FAIL: {worst_key} is {worst_px} px against its instant median of {instant_median:.0f} px '
              f'(> {BAND:.0%}): the men were not photographed alike, restage')
        return 2
    if BAR < max(noise) + (max(noise) - min(noise)):
        print(f'BAR FAIL: {BAR} is not a full spread above the noise {noise}: '
              f'it must be at least {max(noise) + (max(noise) - min(noise)):.4f}')
        return 3
    if min(signal) < BAR:
        print(f'FAIL: the worst of the three silhouette differences, {min(signal)}, is under the bar {BAR}')
        return 1
    print(f'PASS: the worst of {signal} clears the bar {BAR}, set above the noise {noise}')
    return 0


sys.exit(main())
