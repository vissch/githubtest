#!/usr/bin/env python3
"""Can a player tell the Death Battalion man from a rifleman standing next to him?

One rifleman and one Death Battalion man, SAME team, 1.6 m apart, same yaw, at night, zoom 30, yaw 21, pitch 25,
shot at one held instant by `stage.cs`. Three frames per set, nothing ticking between them:

    <tag>_pair.png          both men
    <tag>_minus_death.png   the Death Battalion man written off the map, the clock still held
    <tag>_minus_rifle.png   the rifleman written off the map

So each man's own pixels are |pair - minus| > TOL: same ground, same light, same rain, same pose. Same team on
purpose - "a pale coat in a crowd of blue riflemen" is partly team colour, and the kit has to win with the team
held equal.

Gated:
    coat_width_gap  (hip width / shoulder width) of the Death Battalion mask minus the rifleman's - the greatcoat
    luma_gap        mean luma of the rifleman's pixels minus the Death Battalion man's - positive means darker

Reported, not gated: silhouette_diff, head_bright_px (the skull speck), mask pixel counts.

The bars are NOT chosen: `--baseline` shoots the same measures over three captures of the build as it stood
(5ba0fafb5a) and sets each bar one full spread-width above the top of that spread (baseline.json). A plain run
measures the `after` set and exits 0 only when both bars are cleared.

    python docs/evidence/death-battalion-look-g1/probe.py              the after set against the bars
    python docs/evidence/death-battalion-look-g1/probe.py --baseline   rewrite baseline.json from base1..base3
    python docs/evidence/death-battalion-look-g1/probe.py --crop SRCDIR TAG   the set's three stills, cut to 640x480
"""
import json
import sys
from pathlib import Path

import numpy as np
from PIL import Image
from scipy import ndimage

HERE = Path(__file__).resolve().parent
TOL = 15       # summed over RGB, 0-765. Nothing ticks between a pair and its minus, so the untouched ground is
               # identical to the byte and the count of changed pixels is flat from 10 to 30; at the first probe's
               # 80 half of a dark man against dark mud was thrown away. Shadows are off for the set (stage.cs).
CROP_W, CROP_H = 640, 480
GATED = ('coat_width_gap', 'luma_gap')


def crop_set(srcdir, tag):
    """Cut the three 3840x2160 stills of one set down to the same 640x480 window around the two men.

    Around the MEN, not around the middle of the frame: the camera aims at a point on the ground between them and
    at this pitch they stand well above it, so a centre crop cut their heads off. The window is the same for all
    three stills of the set, so the committed crops still subtract pixel for pixel."""
    srcdir = Path(srcdir)
    full = {k: np.asarray(Image.open(srcdir / f'{tag}_{k}.png').convert('RGB'), dtype=float)
            for k in ('pair', 'minus_death', 'minus_rifle')}
    both = (mask(full['pair'], full['minus_death'], f'{tag} death battalion')
            | mask(full['pair'], full['minus_rifle'], f'{tag} rifleman'))
    ys, xs = np.nonzero(both)
    cy, cx = (ys.min() + ys.max()) // 2, (xs.min() + xs.max()) // 2
    h, w = both.shape
    x = int(np.clip(cx - CROP_W // 2, 0, w - CROP_W))
    y = int(np.clip(cy - CROP_H // 2, 0, h - CROP_H))
    out = []
    for k in full:
        dst = HERE / f'{tag}_{k}.png'
        Image.open(srcdir / f'{tag}_{k}.png').convert('RGB').crop((x, y, x + CROP_W, y + CROP_H)).save(dst)
        out.append(dst.name)
    return f'{tag}: {", ".join(out)} from ({x},{y})"'.rstrip('"')


def rgb(path):
    p = HERE / path
    if not p.exists():
        raise SystemExit(f'missing {p.name}: shoot the set with stage.cs and crop it with --crop')
    return np.asarray(Image.open(p).convert('RGB'), dtype=float)


def mask(pair, minus, name):
    """The man's own pixels: the largest blob of what changed when he left the frame."""
    moved = np.abs(pair - minus).sum(axis=2) > TOL
    moved = ndimage.binary_opening(moved, np.ones((3, 3)))
    moved = ndimage.binary_closing(moved, np.ones((5, 5)))
    labels, n = ndimage.label(moved)
    if n == 0:
        raise SystemExit(f'{name}: nothing changed when he left the frame: he was not in it')
    sizes = [int(np.count_nonzero(labels == i)) for i in range(1, n + 1)]
    return labels == int(np.argmax(sizes)) + 1


def band_width(m, lo, hi):
    """Mean width of the mask over the rows from `lo` to `hi` of its height, 0 at the top of the head."""
    rows = np.flatnonzero(m.any(axis=1))
    top, bot = rows[0], rows[-1]
    h = bot - top + 1
    a, b = top + int(round(lo * h)), top + int(round(hi * h))
    b = max(b, a + 1)
    widths = []
    for r in range(a, b):
        cols = np.flatnonzero(m[r])
        if cols.size:
            widths.append(cols[-1] - cols[0] + 1)
    return float(np.mean(widths)) if widths else 0.0


def coat_ratio(m):
    """Hip over shoulder: the greatcoat skirt makes him wider below the belt than a rifleman is."""
    shoulder = band_width(m, 0.15, 0.30)
    hip = band_width(m, 0.35, 0.55)
    return hip / shoulder if shoulder > 0 else 0.0


def luma(pair, m):
    """Mean luma over the man's own pixels, not the median: half of a man at night is his unlit side and his dark
    outline, so the median sits in the dark and barely moves when his cloth is halved. The kit lives in the pixels
    the moon reaches, and the mean counts them."""
    l = (0.2126 * pair[:, :, 0] + 0.7152 * pair[:, :, 1] + 0.0722 * pair[:, :, 2]) / 255.0
    return float(np.mean(l[m]))


def head_bright(pair, m):
    """Mask pixels in the top fifth of him brighter than luma 0.5: the skull mark is a speck at this zoom."""
    rows = np.flatnonzero(m.any(axis=1))
    top, h = rows[0], rows[-1] - rows[0] + 1
    head = np.zeros_like(m)
    head[top:top + max(1, int(round(0.2 * h)))] = True
    l = (0.2126 * pair[:, :, 0] + 0.7152 * pair[:, :, 1] + 0.0722 * pair[:, :, 2]) / 255.0
    return int(np.count_nonzero((m & head) & (l > 0.5)))


def measure(tag):
    pair = rgb(f'{tag}_pair.png')
    death = mask(pair, rgb(f'{tag}_minus_death.png'), f'{tag} death battalion')
    rifle = mask(pair, rgb(f'{tag}_minus_rifle.png'), f'{tag} rifleman')
    union = np.count_nonzero(death | rifle)
    return {
        'coat_width_gap': round(coat_ratio(death) - coat_ratio(rifle), 4),
        'luma_gap': round(luma(pair, rifle) - luma(pair, death), 4),
        'silhouette_diff': round(np.count_nonzero(death ^ rifle) / union, 4) if union else 0.0,
        'head_bright_px_death': head_bright(pair, death),
        'head_bright_px_rifle': head_bright(pair, rifle),
        'pixels_death': int(np.count_nonzero(death)),
        'pixels_rifle': int(np.count_nonzero(rifle)),
        'coat_ratio_death': round(coat_ratio(death), 4),
        'coat_ratio_rifle': round(coat_ratio(rifle), 4),
    }


def baseline():
    sets = {t: measure(t) for t in ('base1', 'base2', 'base3')}
    bars = {}
    for k in GATED:
        vals = [sets[t][k] for t in sets]
        bars[k] = {'values': vals, 'bar': round(max(vals) + (max(vals) - min(vals)), 4)}
    out = {
        'what': 'three captures of the build as it stood (5ba0fafb5a): the bar is one spread-width above the top '
                'of the spread, so a measure that clears it is outside what pose, rain and wind do on their own',
        'commit': '5ba0fafb5a',
        'sets': sets,
        'bars': bars,
    }
    (HERE / 'baseline.json').write_text(json.dumps(out, indent=1), encoding='utf-8')
    print(json.dumps(out, indent=1))
    return 0


def main():
    if '--crop' in sys.argv:
        i = sys.argv.index('--crop')
        print(crop_set(sys.argv[i + 1], sys.argv[i + 2]))
        return 0
    if '--baseline' in sys.argv:
        return baseline()
    base = HERE / 'baseline.json'
    if not base.exists():
        raise SystemExit('no baseline.json: run --baseline over base1..base3 first')
    bars = json.loads(base.read_text(encoding='utf-8'))['bars']
    after = measure('after')
    out = {'after': after, 'bars': bars, 'verdict': {}}
    ok = True
    for k in GATED:
        bar, vals = bars[k]['bar'], bars[k]['values']
        cleared = after[k] >= bar and bar > max(vals)
        ok &= cleared
        out['verdict'][k] = {'value': after[k], 'bar': bar, 'baseline': vals, 'cleared': bool(cleared)}
    out['pass'] = bool(ok)
    (HERE / 'probe.json').write_text(json.dumps(out, indent=1), encoding='utf-8')
    print(json.dumps(out, indent=1))
    for k in GATED:
        v = out['verdict'][k]
        print(f"{k}: {v['value']} vs bar {v['bar']} (baseline {v['baseline']}) -> {'CLEARED' if v['cleared'] else 'MISSED'}")
    return 0 if ok else 1


sys.exit(main())
