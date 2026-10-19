#!/usr/bin/env python3
"""Is a Death Battalion man a different shape on screen than a rifleman?

Three men were spawned in one match (GreyboxCorridor) and shot at ONE held instant, same camera (zoom 30, yaw 21,
pitch 25), each camera aimed at its own man: a rifleman, a second rifleman (the noise floor) and a Death Battalion
man. Each was then taken out of the frame without a tick of the world (his position written away and the presenter
re-captured, the clock still held) and the same frame shot again. So `<name>_t1.png` and `<name>_bg.png` are the
same ground, the same light and the same weather, with and without the man; the committed pair is the median of
three frames, cropped to 960x720 around the aim point (the whole frames are `*_t1_whole.png`).

A pixel that differs between the two by more than TOL is the man. Comparing the two masks, each centred on its own
middle, gives a shape difference that does not care about where he stood or what the ground under him looks like:

    silhouette_diff = count(maskA xor maskB) / count(maskA or maskB)

READ THE NOISE FLOOR TOO. The second rifleman against the first is the same measurement between two men who should
look the same; whatever it reports is pose, shadow and rain, not identity. A silhouette difference is only worth
something well above it.

    python docs/evidence/death-battalion-look/probe.py     writes probe.json beside this file and prints it
"""
import json
import sys
from pathlib import Path

import numpy as np
from PIL import Image
from scipy import ndimage

HERE = Path(__file__).resolve().parent
TOL = 80     # summed over RGB, 0-765: a tenth of the range, above what the rain and the dark ground do on their own
BOX = 200    # the box each man's mask is recentred into, px: a man at this zoom and 3840x2160 is about 70 px tall
THRESHOLD = 0.10


def mask(stem):
    """The man's own pixels, recentred: the largest blob of what changed when he left the frame."""
    a = np.asarray(Image.open(HERE / f'{stem}_t1.png').convert('RGB'), dtype=float)
    b = np.asarray(Image.open(HERE / f'{stem}_bg.png').convert('RGB'), dtype=float)
    if a.shape != b.shape:
        raise SystemExit(f'{stem}: the two stills are different sizes')
    moved = np.abs(a - b).sum(axis=2) > TOL
    moved = ndimage.binary_opening(moved, np.ones((3, 3)))     # single rain pixels are not a man
    moved = ndimage.binary_closing(moved, np.ones((5, 5)))     # but the holes inside him are
    labels, n = ndimage.label(moved)
    if n == 0:
        raise SystemExit(f'{stem}: nothing changed when the man left: he was not in frame')
    sizes = [int(np.count_nonzero(labels == i)) for i in range(1, n + 1)]
    man = np.pad(labels == int(np.argmax(sizes)) + 1, BOX)
    cy, cx = (int(round(v)) for v in ndimage.center_of_mass(man))
    return man[cy - BOX // 2:cy + BOX // 2, cx - BOX // 2:cx + BOX // 2]


def diff(a, b):
    union = np.count_nonzero(a | b)
    if union == 0:
        raise SystemExit('both masks are empty')
    return np.count_nonzero(a ^ b) / union


def main():
    rifle, rifle2, death = mask('rifle'), mask('rifle2'), mask('deathbn')
    out = {
        'silhouette_diff_vs_rifleman': round(diff(rifle, death), 4),
        'noise_rifleman_vs_rifleman': round(diff(rifle, rifle2), 4),
        'pixels_rifleman': int(np.count_nonzero(rifle)),
        'pixels_rifleman2': int(np.count_nonzero(rifle2)),
        'pixels_death_battalion': int(np.count_nonzero(death)),
        'tolerance': TOL,
        'box_px': BOX,
        'threshold': THRESHOLD,
        'files': {
            'rifleman': ['rifle_t1.png', 'rifle_bg.png'],
            'rifleman2': ['rifle2_t1.png', 'rifle2_bg.png'],
            'death_battalion': ['deathbn_t1.png', 'deathbn_bg.png'],
        },
    }
    (HERE / 'probe.json').write_text(json.dumps(out, indent=1), encoding='utf-8')
    print(json.dumps(out, indent=1))
    return 0 if out['silhouette_diff_vs_rifleman'] >= THRESHOLD else 1


sys.exit(main())
