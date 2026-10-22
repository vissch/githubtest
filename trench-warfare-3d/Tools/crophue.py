#!/usr/bin/env python3
"""Hue and value of a crop's brightest quarter, so a silhouette fault is a number, not a guess.

WHY. "A man in a lamp pool takes the pool's hue and loses his outline" was found by reading two crops' RGB by eye.
`shotstats.py` answers "is the frame too bright"; this answers "does this thing still look like itself against
what is behind it" — a man against the mud he stands on, as hue (deg) and value (0-1), from the pixels most likely
to be his lit side rather than shadow: the crop's top quartile by HSV value.

USAGE (PIL only, no numpy)
    python Tools/crophue.py shot.png --crop x0,y0,x1,y1 [--crop x0,y0,x1,y1 ...]
    python Tools/crophue.py shot.png --crop ... --crop ... --sep 12,0.2
    python Tools/crophue.py shot.png --crop ... --json

Crops are PIL coordinates: (x0,y0) top-left, (x1,y1) bottom-right, y down.

READING IT
    mean RGB / hue / saturation / value   of the brightest 25% of the crop's pixels by HSV value.
    blown_frac                            from the .json CaptureRig writes beside the .png, if present.
    --sep H,V                             compares crop 1 and crop 2: exits 1 when BOTH the hue gap (shortest way
                                           round the circle) and the value gap are under H/V, or when blown_frac
                                           is 0.02 or more (frame invalid); exits 0 otherwise. Prints both gaps.
"""
import json
import os
import sys
import colorsys

from PIL import Image


def brightest_quartile(img):
    px = list(img.get_flattened_data())
    hsv = [colorsys.rgb_to_hsv(r / 255.0, g / 255.0, b / 255.0) for r, g, b in px]
    order = sorted(range(len(px)), key=lambda i: hsv[i][2], reverse=True)
    top = order[:max(1, len(order) // 4)]
    n = len(top)
    r = sum(px[i][0] for i in top) / n
    g = sum(px[i][1] for i in top) / n
    b = sum(px[i][2] for i in top) / n
    h, s, v = colorsys.rgb_to_hsv(r / 255.0, g / 255.0, b / 255.0)
    return {'rgb': [round(r, 1), round(g, 1), round(b, 1)], 'hue': round(h * 360, 1),
            'saturation': round(s, 3), 'value': round(v, 3)}


def blown_frac(path):
    sidecar = os.path.splitext(path)[0] + '.json'
    if not os.path.exists(sidecar):
        return None
    try:
        data = json.loads(open(sidecar, encoding='utf-8').read())
    except (OSError, ValueError):
        return None
    return data.get('blown_frac')


def hue_gap(a, b):
    d = abs(a - b) % 360
    return min(d, 360 - d)


def parse_crop(s):
    x0, y0, x1, y1 = (int(v) for v in s.split(','))
    return x0, y0, x1, y1


def main():
    argv = sys.argv[1:]
    if not argv or argv[0].startswith('--'):
        print(__doc__)
        sys.exit(2)
    path = argv[0]
    crops = []
    sep = None
    i = 1
    while i < len(argv):
        if argv[i] == '--crop':
            crops.append(parse_crop(argv[i + 1]))
            i += 2
        elif argv[i] == '--sep':
            h, v = argv[i + 1].split(',')
            sep = (float(h), float(v))
            i += 2
        elif argv[i] == '--json':
            i += 1
        else:
            i += 1
    if not crops:
        print(__doc__)
        sys.exit(2)

    img = Image.open(path).convert('RGB')
    results = [brightest_quartile(img.crop(c)) for c in crops]
    bf = blown_frac(path)

    as_json = '--json' in argv
    if as_json:
        out = {'file': path, 'crops': results, 'blown_frac': bf}
    if not as_json:
        for crop, res in zip(crops, results):
            print(f"crop {crop}: rgb {res['rgb']} hue {res['hue']} sat {res['saturation']} value {res['value']}")
        if bf is not None:
            print(f"blown_frac {bf}")

    if sep is not None:
        if len(results) < 2:
            print('--sep needs two --crop')
            sys.exit(2)
        hg = hue_gap(results[0]['hue'], results[1]['hue'])
        vg = abs(results[0]['value'] - results[1]['value'])
        blown_bad = bf is not None and bf >= 0.02
        separated = hg >= sep[0] or vg >= sep[1]
        if as_json:
            out['sep'] = {'hue_gap': round(hg, 1), 'value_gap': round(vg, 3), 'blown_bad': blown_bad,
                           'separated': separated}
            print(json.dumps(out, indent=1))
        else:
            print(f"hue gap {round(hg, 1)}  value gap {round(vg, 3)}")
        sys.exit(0 if (separated and not blown_bad) else 1)

    if as_json:
        print(json.dumps(out, indent=1))


if __name__ == '__main__':
    main()
