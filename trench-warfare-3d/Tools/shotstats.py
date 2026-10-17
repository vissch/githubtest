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
    python Tools/shotstats.py shot.png --line 905,417,945,613        how wide the edge along that line is
    python Tools/shotstats.py shot.png --crop 700,480,200,200 --crop 300,480,200,200   spread in two boxes
    ... --min-fall 40 --min-spread 0.6                  exit 1 when the edge is too hard or the box too flat

READING IT
    luma mean / p5 / p50 / p95   0-255. The night look sits low (mean roughly 40-70). A jump of 10+ in the mean
                                 or p95 between two captures of the same pose is a lighting change, not noise.
    blown %                      share of pixels at 250+. Above ~1% something is blowing out to white.
    black %                      share under 8. Above ~30% the frame is mostly void (camera in the ground, fog).
    warm %  / warm span          saturated red-orange-yellow pixels (fire, muzzle flash, lamps) and the width in
                                 px of where they sit. A fire capture with a tiny span caught the wrong moment.
    grid                         3x3 mean luma, top row first: where in the frame the light is.
    fall px                      --line only: how many px the luma takes to cross between the line's two
                                 plateaus, measured as the full width of that slope (the 90% point to the 10%
                                 point and the shoulders either side of them). A hard edge gives 1-3 px, light
                                 falling off gives tens. The plateaus are the line's own 10th and 90th
                                 percentile, so the line must start inside the bright patch and end outside it.
    spread                       --crop only: standard deviation of luma in the box. Flat ground gives ~0, mud
                                 with shading and marbling gives 8-20. Two --crop boxes also print spread_ratio,
                                 the second box over the first.
    --min-fall N / --min-spread R   exit 1 when fall px is under N, or when the FIRST box's spread is under R
                                 times the second's. Pass the lit box first and the unlit reference second.
    diff mean / changed %        between two captures: mean absolute luma change and share of pixels that moved
                                 by more than 16. Rain and wind make ~1-3% change between identical poses; use a
                                 held clock (CaptureRig.Hold) before comparing, or read the grid instead.
"""
import json
import sys
from PIL import Image, ImageChops, ImageStat

# How close to a plateau's level still counts as standing on it, in luma steps. 2 sits under a capture's noise
# and is still tight enough that a one-pixel step reads as a one-pixel fall.
PLATEAU_TOL = 2.0


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


def profile(img, x0, y0, x1, y1):
    """Luma (Rec.601, 0-255) sampled one step per pixel along the line, both endpoints included."""
    px = img.load()
    w, h = img.size
    n = max(abs(x1 - x0), abs(y1 - y0))
    out = []
    for i in range(n + 1):
        t = i / n if n else 0.0
        x = min(w - 1, max(0, int(round(x0 + (x1 - x0) * t))))
        y = min(h - 1, max(0, int(round(y0 + (y1 - y0) * t))))
        r, g, b = px[x, y][:3]
        out.append(0.299 * r + 0.587 * g + 0.114 * b)
    return out


def smooth(vals, k=5):
    if len(vals) < k:
        return list(vals)
    half = k // 2
    return [sum(vals[max(0, i - half):i + half + 1]) / len(vals[max(0, i - half):i + half + 1])
            for i in range(len(vals))]


def fall(vals):
    """Width in px of the transition between the line's low and high plateau.

    The plateaus are the line's own 10th and 90th percentile, so one bright speck of noise cannot set them. The
    transition is the gap between the last sample still on one plateau and the nearest sample on the other: the
    whole slope, not just its middle. Returns 0 when there is no edge on the line at all.
    """
    v = smooth(vals)
    if not v:
        return {'fall_px': 0, 'fall_hi': 0.0, 'fall_lo': 0.0}
    srt = sorted(v)
    lo = srt[int(0.10 * (len(srt) - 1))]
    hi = srt[int(0.90 * (len(srt) - 1))]
    out = {'fall_px': 0, 'fall_hi': round(hi, 1), 'fall_lo': round(lo, 1)}
    if hi - lo < 4 * PLATEAU_TOL:
        return out
    on_lo = [i for i, x in enumerate(v) if x <= lo + PLATEAU_TOL]
    on_hi = [i for i, x in enumerate(v) if x >= hi - PLATEAU_TOL]
    if on_lo and on_hi:
        out['fall_px'] = min(abs(i - j) for i in on_lo for j in on_hi)
    return out


def spread(L, box):
    x, y, w, h = box
    return round(ImageStat.Stat(L.crop((x, y, x + w, y + h))).stddev[0], 2)


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


TAKES_VALUE = ('--line', '--crop', '--min-fall', '--min-spread')


def opt(name):
    """Every `--name value` and `--name=value` on the command line, in the order they were given."""
    out, argv = [], sys.argv[1:]
    for i, a in enumerate(argv):
        if a == name and i + 1 < len(argv):
            out.append(argv[i + 1])
        elif a.startswith(name + '='):
            out.append(a.split('=', 1)[1])
    return out


def nums(text, count, name):
    parts = [p for p in text.replace(' ', ',').split(',') if p]
    if len(parts) != count:
        print(f'{name} wants {count} numbers, got "{text}"')
        sys.exit(2)
    return [int(float(p)) for p in parts]


def measure(path):
    """The --line and --crop measures for one PNG, with the --min-fall / --min-spread failures under 'fails'."""
    img = Image.open(path).convert('RGB')
    L = img.convert('L')
    out, bad = {}, []
    for text in opt('--line'):
        x0, y0, x1, y1 = nums(text, 4, '--line')
        out['line'] = f'{x0},{y0},{x1},{y1}'
        out.update(fall(profile(img, x0, y0, x1, y1)))
    boxes = [nums(t, 4, '--crop') for t in opt('--crop')]
    if boxes:
        out['crops'] = boxes
        out['spreads'] = [spread(L, b) for b in boxes]
        if len(boxes) >= 2:
            first, second = out['spreads'][0], out['spreads'][1]
            # A flat reference box (spread 0) would divide by zero: the ratio is then "as mottled as it gets".
            out['spread_ratio'] = round(second / first, 3) if first else (0.0 if not second else 999.0)
            out['spread_first_over_second'] = round(first / second, 3) if second else (0.0 if not first else 999.0)
    for text in opt('--min-fall'):
        if 'fall_px' not in out:
            print('--min-fall needs --line')
            sys.exit(2)
        if out['fall_px'] < float(text):
            bad.append(f"fall {out['fall_px']} px is under {float(text):g}")
    for text in opt('--min-spread'):
        got = out.get('spread_first_over_second')
        if got is None:
            print('--min-spread needs two --crop boxes')
            sys.exit(2)
        if got < float(text):
            bad.append(f'spread ratio {got} (first box over second) is under {float(text):g}')
    out['fails'] = bad
    return out


def show_measures(s):
    if 'line' in s:
        print(f"  line {s['line']}  fall {s['fall_px']} px  (plateaus {s['fall_lo']} / {s['fall_hi']})")
    for box, sp in zip(s.get('crops', []), s.get('spreads', [])):
        print(f"  crop {','.join(str(v) for v in box)}  spread {sp}")
    if s.get('spread_ratio') is not None:
        print(f"  spread_ratio {s['spread_ratio']}  (second box over first; "
              f"first over second {s['spread_first_over_second']})")


def show(s):
    print(f"{s['file']}  {s['size']}")
    print(f"  luma mean {s['luma_mean']}  p5 {s['luma_p5']}  p50 {s['luma_p50']}  p95 {s['luma_p95']}"
          f"  blown {s['blown_pct']}%  black {s['black_pct']}%")
    print(f"  warm {s['warm_pct']}%  warm span {s['warm_span_px']} px")
    print('  grid ' + ' / '.join(' '.join(f'{v:3d}' for v in row) for row in s['grid']))


def main():
    argv = sys.argv[1:]
    args = [a for i, a in enumerate(argv)
            if not a.startswith('--') and not (i > 0 and argv[i - 1] in TAKES_VALUE)]
    if not args:
        print(__doc__)
        sys.exit(2)
    a, La = stats(args[0])
    m = measure(args[0])
    a.update({k: v for k, v in m.items() if k != 'fails'})
    result = {'a': a}
    if len(args) > 1:
        b, Lb = stats(args[1])
        result['b'] = b
        result['diff'] = diff(La, Lb)
    if '--json' in sys.argv:
        result['fails'] = m['fails']
        print(json.dumps(result, indent=1))
        sys.exit(1 if m['fails'] else 0)
    show(a)
    show_measures(a)
    if 'b' in result:
        show(result['b'])
        d = result['diff']
        if 'diff_error' in d:
            print('  ' + d['diff_error'])
        else:
            print(f"  diff mean {d['diff_mean']}  changed {d['changed_pct']}%  box {d['changed_box']}"
                  f"  (luma mean {result['a']['luma_mean'] - result['b']['luma_mean']:+.1f})")
    for why in m['fails']:
        print('  FAIL ' + why)
    sys.exit(1 if m['fails'] else 0)


if __name__ == '__main__':
    main()
