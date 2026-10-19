#!/usr/bin/env python3
"""Measure how far the man moved between two CaptureRig stills, inside his box only.

WHY. Harsh critic r1 (6/100) on shelling-flinch/probe.py: its 19.7 px number was mostly the camera moving
(pitch 25 vs 22.9, pose_error_m 0.35) plus a fireball over the man, because it matched blocks over the middle
third of the whole frame. This version only looks inside each case's box (boxes.json: his feet-to-head rect,
widened for the walking cases by his travel), so a camera slip or something lighting up elsewhere in the frame
cannot move the number. It also subtracts each block's own mean before matching (SSD on the mean-subtracted
block), so a flash or a brightness change cannot bias a match toward "no motion" or toward "huge motion".

The number is the median of |shift - ref|, where ref is the median shift vector of the box's LOWER THIRD (his
feet: ground truth for his own travel, and any camera pan a stationary capture should not have). The upper
two-thirds of the box carry the posture change (torso bending, head ducking) relative to that travel, so one
metric serves both the standing case (ref should be ~0) and the walking case (ref is his stride).

No arguments but the evidence folder: fixed names, so this is rerunnable by anyone from the repo.
    python probe.py docs/evidence/shelling-flinch-r2

Reads boxes.json in that folder for each stem's box. Writes probe.json:
    baseline_px     - the 3 no-shell pairs of stand-base (00-01, 02-03, 04-05), same tick gap as stand
    pose_delta_px   - stand_00 vs stand_01 (standing man, shell)
    walk_baseline_px - the 3 no-shell pairs of walk-base
    walk_delta_px   - walk_00 vs walk_01 (walking man, shell)
    max_pose_error  - the largest pose_error_m over every sidecar in the folder
    frame_gap       - the frame gap of every pair (fails loudly if they differ by more than 1)
    fps, blocks
"""
import glob
import json
import os
import sys
from PIL import Image, ImageStat

BLOCK = 24          # px, each matched block's side
SEARCH = 18          # px, how far a block is allowed to have moved
MIN_STDDEV = 6.0     # below this a block is flat ground/sky, not the man: dropped


def luma(path):
    return Image.open(path).convert('L')


def mean_sub(vals):
    m = sum(vals) / len(vals)
    return [v - m for v in vals]


def block_shift(a, b, x, y, w, h):
    """Best mean-subtracted-SSD match for the (x, y, BLOCK, BLOCK) block of `a` inside `b`, within SEARCH px."""
    ref = mean_sub(list(a.crop((x, y, x + BLOCK, y + BLOCK)).getdata()))
    best, best_d = None, None
    for dy in range(-SEARCH, SEARCH + 1):
        ny = y + dy
        if ny < 0 or ny + BLOCK > h:
            continue
        for dx in range(-SEARCH, SEARCH + 1):
            nx = x + dx
            if nx < 0 or nx + BLOCK > w:
                continue
            cand = mean_sub(list(b.crop((nx, ny, nx + BLOCK, ny + BLOCK)).getdata()))
            ssd = sum((p - q) * (p - q) for p, q in zip(ref, cand))
            if best_d is None or ssd < best_d:
                best_d, best = ssd, (dx, dy)
    return best


def shifts_in_box(a, b, box):
    """Every block's motion vector inside `box` (x0, y0, x1, y1), dropped if too flat to trust."""
    w, h = a.size
    x0, y0, x1, y1 = (int(round(v)) for v in box)
    x0 = max(0, x0); y0 = max(0, y0); x1 = min(w, x1); y1 = min(h, y1)
    out = []
    for y in range(y0, max(y0, y1 - BLOCK), BLOCK):
        for x in range(x0, max(x0, x1 - BLOCK), BLOCK):
            if ImageStat.Stat(a.crop((x, y, x + BLOCK, y + BLOCK))).stddev[0] < MIN_STDDEV:
                continue
            dx, dy = block_shift(a, b, x, y, w, h)
            out.append((x, y, dx, dy))
    return out


def median(vals):
    vals = sorted(vals)
    n = len(vals)
    if n == 0:
        return 0.0
    return vals[n // 2] if n % 2 else (vals[n // 2 - 1] + vals[n // 2]) / 2


def residual_px(before_path, after_path, box):
    a = luma(before_path)
    b = luma(after_path)
    if b.size != a.size:
        raise SystemExit(f'size mismatch: {before_path} vs {after_path}')
    vecs = shifts_in_box(a, b, box)
    if not vecs:
        return 0.0, 0
    y0, y1 = box[1], box[3]
    lower_cut = y0 + (y1 - y0) * (2.0 / 3.0)
    lower = [(dx, dy) for (x, y, dx, dy) in vecs if y >= lower_cut]
    pool = lower if lower else [(dx, dy) for (x, y, dx, dy) in vecs]
    ref_dx = median([v[0] for v in pool])
    ref_dy = median([v[1] for v in pool])
    residuals = []
    for (x, y, dx, dy) in vecs:
        rdx, rdy = dx - ref_dx, dy - ref_dy
        residuals.append((rdx * rdx + rdy * rdy) ** 0.5)
    return median(residuals), len(vecs)


def frame_of(sidecar_path):
    d = json.load(open(sidecar_path, encoding='utf-8'))
    return d.get('frame'), d.get('pose_error_m')


def main():
    argv = sys.argv[1:]
    if len(argv) < 1:
        print(__doc__)
        sys.exit(2)
    folder = argv[0]
    boxes = json.load(open(os.path.join(folder, 'boxes.json'), encoding='utf-8'))

    max_pose_error = 0.0
    for jf in glob.glob(os.path.join(folder, '*.json')):
        if os.path.basename(jf) in ('boxes.json', 'probe.json'):
            continue
        try:
            d = json.load(open(jf, encoding='utf-8'))
        except (ValueError, OSError):
            continue
        pe = d.get('pose_error_m')
        if pe is not None:
            max_pose_error = max(max_pose_error, pe)

    def pair(stem_a, stem_b):
        a_png = os.path.join(folder, stem_a + '.png')
        b_png = os.path.join(folder, stem_b + '.png')
        if not os.path.exists(a_png):
            a_png = os.path.join(folder, stem_a + '.jpg')
            b_png = os.path.join(folder, stem_b + '.jpg')
        box = boxes[stem_a]
        px, n = residual_px(a_png, b_png, box)
        fa, _ = frame_of(os.path.join(folder, stem_a + '.json'))
        fb, _ = frame_of(os.path.join(folder, stem_b + '.json'))
        return px, n, (fb - fa if fa is not None and fb is not None else None)

    baseline_px, blocks, gaps = [], 0, []
    for a, b in (('stand-base_00', 'stand-base_01'), ('stand-base_02', 'stand-base_03'), ('stand-base_04', 'stand-base_05')):
        px, n, gap = pair(a, b)
        baseline_px.append(round(px, 2))
        blocks += n
        gaps.append(gap)

    pose_delta_px, n, gap = pair('stand_00', 'stand_01')
    blocks += n
    gaps.append(gap)

    walk_baseline_px = []
    for a, b in (('walk-base_00', 'walk-base_01'), ('walk-base_02', 'walk-base_03'), ('walk-base_04', 'walk-base_05')):
        px, n, g = pair(a, b)
        walk_baseline_px.append(round(px, 2))
        blocks += n
        gaps.append(g)

    walk_delta_px, n, gap = pair('walk_00', 'walk_01')
    blocks += n
    gaps.append(gap)

    gaps = [g for g in gaps if g is not None]
    frame_gap = gaps[0] if gaps else None
    gap_mismatch = any(abs(g - frame_gap) > 1 for g in gaps) if frame_gap is not None else True

    out = {
        'baseline_px': baseline_px,
        'pose_delta_px': round(pose_delta_px, 2),
        'walk_baseline_px': walk_baseline_px,
        'walk_delta_px': round(walk_delta_px, 2),
        'max_pose_error': round(max_pose_error, 5),
        'frame_gap': frame_gap,
        'frame_gap_mismatch': gap_mismatch,
        'fps': 20.0,
        'blocks': blocks,
    }
    print(out)
    with open(os.path.join(folder, 'probe.json'), 'w', encoding='utf-8') as f:
        json.dump(out, f, indent=1)
    sys.exit(0)


if __name__ == '__main__':
    main()
