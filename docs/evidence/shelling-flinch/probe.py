#!/usr/bin/env python3
"""Measure how far the man moved on screen between two CaptureRig stills, in pixels.

WHY. "He flinches" is a claim about pixels, not a typed number: this reads the two frames (before the
blast, and +0.3 s after) and block-matches the centre of the frame — where CaptureRig's Shot focused the
man — between them. Each block's best-matching offset within a search window is a motion vector; flat,
textureless blocks (open ground, sky) are dropped by their own low variance so they cannot pull the number
down, and the result is the median magnitude of what is left: the man's pose shift in pixels.

USAGE (PIL only, no numpy, same convention as Tools/shotstats.py)
    python probe.py before.png after.png --out probe.json

Writes probe.json: {"pose_delta_px": <median block-shift, px>, "blocks": <n measured>}.
"""
import json
import sys
from PIL import Image, ImageStat

BLOCK = 24          # px, each matched block's side
SEARCH = 18          # px, how far a block is allowed to have moved
MIN_STDDEV = 6.0     # below this a block is flat ground/sky, not the man: dropped


def luma(path):
    return Image.open(path).convert('L')


def block_shift(a, b, x, y, w, h):
    """Best SSD match for the (x, y, BLOCK, BLOCK) block of `a` inside `b`, searched within SEARCH px."""
    ref = list(a.crop((x, y, x + BLOCK, y + BLOCK)).getdata())
    best, best_d = None, None
    for dy in range(-SEARCH, SEARCH + 1):
        ny = y + dy
        if ny < 0 or ny + BLOCK > h:
            continue
        for dx in range(-SEARCH, SEARCH + 1):
            nx = x + dx
            if nx < 0 or nx + BLOCK > w:
                continue
            cand = list(b.crop((nx, ny, nx + BLOCK, ny + BLOCK)).getdata())
            ssd = sum((p - q) * (p - q) for p, q in zip(ref, cand))
            if best_d is None or ssd < best_d:
                best_d, best = ssd, (dx, dy)
    return best


def measure(before_path, after_path):
    a = luma(before_path)
    b = luma(after_path)
    w, h = a.size
    if b.size != (w, h):
        raise SystemExit(f'size mismatch: {a.size} vs {b.size}')

    # the man sits where CaptureRig focused the shot: the middle third of the frame, where his figure fills
    # most of the height at zoom 16 on a 1600x900 capture
    cx0, cx1 = w // 3, 2 * w // 3
    cy0, cy1 = h // 6, 5 * h // 6

    shifts = []
    for y in range(cy0, cy1 - BLOCK, BLOCK):
        for x in range(cx0, cx1 - BLOCK, BLOCK):
            if ImageStat.Stat(a.crop((x, y, x + BLOCK, y + BLOCK))).stddev[0] < MIN_STDDEV:
                continue
            dx, dy = block_shift(a, b, x, y, w, h)
            shifts.append((dx * dx + dy * dy) ** 0.5)

    shifts.sort()
    n = len(shifts)
    median = shifts[n // 2] if n % 2 else (shifts[n // 2 - 1] + shifts[n // 2]) / 2 if n else 0.0
    return {'pose_delta_px': round(median, 1), 'blocks': n}


def main():
    argv = sys.argv[1:]
    positional = [a for a in argv if not a.startswith('--')]
    if len(positional) < 2:
        print(__doc__)
        sys.exit(2)
    before_path, after_path = positional[0], positional[1]
    out = measure(before_path, after_path)
    print(out)
    if '--out' in argv:
        out_path = argv[argv.index('--out') + 1]
        with open(out_path, 'w', encoding='utf-8') as f:
            json.dump(out, f, indent=1)
    sys.exit(0)


if __name__ == '__main__':
    main()
