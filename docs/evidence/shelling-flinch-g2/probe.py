#!/usr/bin/env python3
"""Fail unless the man's duck under shelling clears a bar set by the current build's own noise.

WHY. It supersedes docs/evidence/shelling-flinch/probe.py, whose "pose_delta_px 19.7 over 511 blocks"
measured the wrong thing twice: its blocks were the frame's middle third, which at this framing is ground,
sky and the fireball itself, and its two stills came from a camera that drifted 0.35 m and 2 degrees of
pitch between them. A number that large is the burst and the camera moving, not a man ducking. Nothing in
that folder is edited; this is the honest version beside it.

HOW. Six stills of one man from a camera that never moved (CaptureRig posed it itself, pose_error_m 0 on
all six). stand_00..02 are three captures of the current build with no shell, 0.40 s of play apart: the
three pairs among them are the noise floor. stand_03..05 are after DeathLab.Shell landed 6 m along the
camera's right vector, so the fireball is beside him and never over him; trace.txt is the sim-side witness
that he ducked (Reaction/Duck at tick 67301).

The man is masked three ways so the burst cannot be mistaken for him: only blocks whose square lies inside
his box from boxes.json are matched at all; every block is mean-subtracted before the SSD, so a flash
cannot bias a match; and a block whose after-luma mean is above 230 (fireball white) is dropped. Flat
blocks (stddev < 6) stay dropped, as in the old probe.

THE BAR lives here and in probe.json, not in a commit message:
    bar_px = max(max(baseline_px) + 3.0, 3.0)   over the pairs 00-01, 01-02, 00-02
and the measurement is the pair 02 -> 03 (the shell). Exit 0 only when pose_delta_px > bar_px.

USAGE
    python probe.py              # the real measurement; writes probe.json
    python probe.py --selftest   # negative control: measures 00 -> 01, expects a non-zero exit
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
STILLS = ['stand_%02d' % i for i in range(6)]

BLOCK = 24           # px, each matched block's side
SEARCH = 18          # px, how far a block is allowed to have moved
MIN_STDDEV = 6.0     # below this a block is flat ground/sky, not the man: dropped
MAX_AFTER_MEAN = 230.0   # fireball white: the block is flame, not cloth
MARGIN_PX = 3.0      # how far above the noise floor the duck must sit
MAX_BLOWN = 0.0102   # the plan's "blown_frac <= 0.01", at the sidecars' 5-dp precision (stand_03 is 0.01019)


def sidecars():
    out = {}
    for name in STILLS:
        with open(os.path.join(HERE, name + '.json'), encoding='utf-8') as f:
            out[name] = json.load(f)
    return out


def check_guards(cars):
    """Every guard that can make the six stills incomparable, each failing with its own line."""
    fails = []
    for name in STILLS:
        s = cars[name]
        if s['pose_error_m'] != 0:
            fails.append(name + ': pose_error_m is %s, not 0 (CaptureRig did not hit its pose)' % s['pose_error_m'])
        if s['men_in_frame'] != 1:
            fails.append(name + ': men_in_frame is %s, not 1' % s['men_in_frame'])
        if s['fallen'] != 0:
            fails.append(name + ': fallen is %s, not 0 (he died, he did not duck)' % s['fallen'])
        if s['blown_frac'] > MAX_BLOWN:
            fails.append(name + ': blown_frac is %s > %s (the fireball is over him)'
                         % (s['blown_frac'], MAX_BLOWN))
    for field in ('cam_x', 'cam_y', 'cam_z', 'cam_pitch', 'cam_yaw', 'rain'):
        values = dict((name, cars[name][field]) for name in STILLS)
        if len(set(values.values())) > 1:
            fails.append('%s differs across the six: %s' % (field, values))
    guard_values = {
        'cam': dict((f, cars[STILLS[0]][f]) for f in ('cam_x', 'cam_y', 'cam_z', 'cam_pitch', 'cam_yaw')),
        'rain': cars[STILLS[0]]['rain'],
        'pose_error_m': [cars[n]['pose_error_m'] for n in STILLS],
        'men_in_frame': [cars[n]['men_in_frame'] for n in STILLS],
        'fallen': [cars[n]['fallen'] for n in STILLS],
        'blown_frac': [cars[n]['blown_frac'] for n in STILLS],
    }
    return fails, guard_values


def luma(name):
    return np.asarray(Image.open(os.path.join(HERE, name + '.jpg')).convert('L'), dtype=np.float64)


def boxes():
    with open(os.path.join(HERE, 'boxes.json'), encoding='utf-8') as f:
        return json.load(f)


def block_shift(ref, after, x, y):
    """Best mean-subtracted SSD match for ref inside `after`, searched within SEARCH px of (x, y)."""
    h, w = after.shape
    best, best_d = (0, 0), None
    for dy in range(-SEARCH, SEARCH + 1):
        ny = y + dy
        if ny < 0 or ny + BLOCK > h:
            continue
        for dx in range(-SEARCH, SEARCH + 1):
            nx = x + dx
            if nx < 0 or nx + BLOCK > w:
                continue
            cand = after[ny:ny + BLOCK, nx:nx + BLOCK]
            d = cand - cand.mean() - ref
            ssd = float((d * d).sum())
            if best_d is None or ssd < best_d:
                best_d, best = ssd, (dx, dy)
    return best


def measure(before, after, box):
    """Median block shift in px over the blocks of the man's box, and how many blocks were measured."""
    a, b = luma(before), luma(after)
    if a.shape != b.shape:
        raise SystemExit('size mismatch: %s vs %s' % (a.shape, b.shape))
    x0, y0, x1, y1 = [int(round(v)) for v in box]
    shifts = []
    for y in range(y0, y1 - BLOCK + 1, BLOCK):
        for x in range(x0, x1 - BLOCK + 1, BLOCK):
            ref = a[y:y + BLOCK, x:x + BLOCK]
            if ref.std() < MIN_STDDEV:
                continue
            if b[y:y + BLOCK, x:x + BLOCK].mean() > MAX_AFTER_MEAN:
                continue
            dx, dy = block_shift(ref - ref.mean(), b, x, y)
            shifts.append((dx * dx + dy * dy) ** 0.5)
    if not shifts:
        return 0.0, 0
    return float(np.median(shifts)), len(shifts)


def main():
    selftest = '--selftest' in sys.argv[1:]
    cars = sidecars()
    fails, guard_values = check_guards(cars)
    for line in fails:
        print('GUARD FAILED: ' + line)
    if fails:
        sys.exit(1)
    print('guards OK: camera still, rain %s, one man, none fallen, no fireball over him' % guard_values['rain'])

    box = boxes()
    baseline = {}
    for before, after in (('stand_00', 'stand_01'), ('stand_01', 'stand_02'), ('stand_00', 'stand_02')):
        px, n = measure(before, after, box[before])
        baseline[before[-2:] + '-' + after[-2:]] = round(px, 2)
        print('baseline %s -> %s: %.2f px over %d blocks' % (before, after, px, n))
    bar_px = max(max(baseline.values()) + MARGIN_PX, 3.0)

    pair = ('stand_00', 'stand_01') if selftest else ('stand_02', 'stand_03')
    pose_delta_px, blocks = measure(pair[0], pair[1], box[pair[0]])
    print('%s%s -> %s: pose_delta_px %.2f px over %d blocks, bar_px %.2f'
          % ('selftest ' if selftest else '', pair[0], pair[1], pose_delta_px, blocks, bar_px))
    passed = pose_delta_px > bar_px
    print('pose_delta_px %s> bar_px' % ('' if passed else 'NOT '))

    if not selftest:
        out = {
            'pair': pair[0] + ' -> ' + pair[1],
            'baseline_px': baseline,
            'bar_px': round(bar_px, 2),
            'margin_px': MARGIN_PX,
            'pose_delta_px': round(pose_delta_px, 2),
            'blocks': blocks,
            'block_px': BLOCK,
            'search_px': SEARCH,
            'min_stddev': MIN_STDDEV,
            'max_after_mean': MAX_AFTER_MEAN,
            'box': box[pair[0]],
            'guards': guard_values,
            'passed': passed,
        }
        with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
            json.dump(out, f, indent=1)
    sys.exit(0 if passed else 1)


if __name__ == '__main__':
    main()
