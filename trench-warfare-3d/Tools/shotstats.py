#!/usr/bin/env python3
"""Numbers for a screenshot, so an agent can judge a capture without trusting its eyes.

WHY. Three "too bright" faults on 2026-09-22 (soldiers lit 3x the world, a shell light washing the field yellow,
a burst blowing out to white) and a whole critique round spent on a capture that contained no fire at all were
all visible as a number before anyone opened the PNG. A model reading a picture is worse at "is this brighter
than before" than a histogram is. Read these first; look at the picture second.

CaptureRig already writes a .json beside every still it takes (luminance, blown-out share, figure/ground). Use
this for any other PNG: `tw shot` output, player bench `shot=` frames, or two captures to compare.

USAGE (PIL only, no numpy)
    python Tools/shotstats.py shot.png                  stats for one capture
    python Tools/shotstats.py after.png before.png      stats for both, plus what changed between them
    python Tools/shotstats.py shot.png --json           machine-readable

READING IT
    luma mean / p5 / p50 / p95   0-255. The night look sits low (mean roughly 40-70). A jump of 10+ in the mean
                                 or p95 between two captures of the same pose is a lighting change, not noise.
    blown %                      share of pixels at 250+. Above ~1% something is blowing out to white.
    black %                      share under 8. Above ~30% the frame is mostly void (camera in the ground, fog).
    warm %  / warm span          saturated red-orange-yellow pixels (fire, muzzle flash, lamps) and the width in
                                 px of where they sit. A fire capture with a tiny span caught the wrong moment.
    grid                         3x3 mean luma, top row first: where in the frame the light is.
    diff mean / changed %        between two captures: mean absolute luma change and share of pixels that moved
                                 by more than 16. Rain and wind make ~1-3% change between identical poses; use a
                                 held clock (CaptureRig.Hold) before comparing, or read the grid instead.
"""
import json
import sys
from PIL import Image, ImageChops, ImageStat


def luma_stats(L):
    h = L.histogram()
    n = sum(h)

    def pct(p):
        acc, target = 0, n * p
        for v, c in enumerate(h):
            acc += c
            if acc >= target:
                return v
        return 255

    mean = sum(v * c for v, c in enumerate(h)) / n
    return {
        'luma_mean': round(mean, 1), 'luma_p5': pct(0.05), 'luma_p50': pct(0.5), 'luma_p95': pct(0.95),
        'blown_pct': round(100.0 * sum(h[250:]) / n, 2), 'black_pct': round(100.0 * sum(h[:8]) / n, 2),
    }


def warm(img):
    H, S, V = img.convert('HSV').split()
    hue = H.point(lambda h: 255 if h <= 30 or h >= 235 else 0)
    sat = S.point(lambda s: 255 if s >= 80 else 0)
    val = V.point(lambda v: 255 if v >= 60 else 0)
    mask = ImageChops.multiply(ImageChops.multiply(hue, sat), val)
    count = mask.histogram()[255]
    box = mask.getbbox()
    return {
        'warm_pct': round(100.0 * count / (img.width * img.height), 2),
        'warm_span_px': (box[2] - box[0]) if box else 0,
    }


def grid(L):
    w, h = L.size
    rows = []
    for r in range(3):
        row = []
        for c in range(3):
            cell = L.crop((c * w // 3, r * h // 3, (c + 1) * w // 3, (r + 1) * h // 3))
            row.append(round(ImageStat.Stat(cell).mean[0]))
        rows.append(row)
    return rows


def stats(path):
    img = Image.open(path).convert('RGB')
    L = img.convert('L')
    out = {'file': path, 'size': f'{img.width}x{img.height}'}
    out.update(luma_stats(L))
    out.update(warm(img))
    out['grid'] = grid(L)
    return out, L


def diff(La, Lb):
    if La.size != Lb.size:
        return {'diff_error': f'sizes differ: {La.size} vs {Lb.size}'}
    d = ImageChops.difference(La, Lb)
    h = d.histogram()
    n = sum(h)
    moved = d.point(lambda v: 255 if v > 16 else 0)
    return {
        'diff_mean': round(sum(v * c for v, c in enumerate(h)) / n, 2),
        'changed_pct': round(100.0 * sum(h[17:]) / n, 2),
        'changed_box': moved.getbbox(),
    }


def show(s):
    print(f"{s['file']}  {s['size']}")
    print(f"  luma mean {s['luma_mean']}  p5 {s['luma_p5']}  p50 {s['luma_p50']}  p95 {s['luma_p95']}"
          f"  blown {s['blown_pct']}%  black {s['black_pct']}%")
    print(f"  warm {s['warm_pct']}%  warm span {s['warm_span_px']} px")
    print('  grid ' + ' / '.join(' '.join(f'{v:3d}' for v in row) for row in s['grid']))


def main():
    args = [a for a in sys.argv[1:] if not a.startswith('--')]
    if not args:
        print(__doc__)
        sys.exit(2)
    a, La = stats(args[0])
    result = {'a': a}
    if len(args) > 1:
        b, Lb = stats(args[1])
        result['b'] = b
        result['diff'] = diff(La, Lb)
    if '--json' in sys.argv:
        print(json.dumps(result, indent=1))
        return
    show(a)
    if 'b' in result:
        show(result['b'])
        d = result['diff']
        if 'diff_error' in d:
            print('  ' + d['diff_error'])
        else:
            print(f"  diff mean {d['diff_mean']}  changed {d['changed_pct']}%  box {d['changed_box']}"
                  f"  (luma mean {result['a']['luma_mean'] - result['b']['luma_mean']:+.1f})")


if __name__ == '__main__':
    main()
