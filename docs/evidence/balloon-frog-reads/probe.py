#!/usr/bin/env python3
"""Does the ballooning frog's throat sac actually read at zoom 30? Measured off the stills, never typed.

WHY. The deaths critique scored the balloon death 27/100 with "a ~20 px pale blob at zoom 30". A claim like that
is settled by pixels, not by the art: the sac is baked into a VAT atlas, so its on-screen size and its contrast
against the mud behind it can only be known from a capture. This counts, per tile, whether the sac is a real
blob and whether it stands off the ground by the owner's 2.5:1.

USAGE (PIL only, no numpy; the shotstats.py idiom)
    python docs/evidence/balloon-frog-reads/probe.py                  writes probe.json beside the tiles
    python docs/evidence/balloon-frog-reads/probe.py --out NAME.json  writes somewhere else (probe-before.json)

HOW IT MEASURES
    sac mask     a pixel is sac when its Rec.709 *linear* luma >= 0.45, it is greener than it is blue
                 (g >= b + 8/255) and it is not a dark pixel (r >= 120/255). The sac is the palest yellow-green
                 thing in frame; mud, sky and cloth all fail one of the three.
    blob         the largest 4-connected run of mask pixels, and only if it is 25 px or more. A smaller one is
                 noise (a helmet rim, a highlight on wet ground), so the tile counts as not reading.
    ground ring  the non-mask pixels between 1.5 and 3.0 blob radii of the blob's centre: what the eye compares
                 the sac against.
    contrast     (Lsac + 0.05) / (Lring + 0.05) on the mean linear luma of each, the WCAG form. >= 2.5 passes.
    sac_across_m the blob's width in metres: px * 2 * zoom * tan(fov/2) / image_height, zoom and the image
                 height read from the tile's CaptureRig sidecar, fov 25 (the T1 band's, workflow.md section 6).
"""
import json
import math
import os
import sys
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
FOV_DEG = 25.0            # the T1 band's vertical fov
LUMA_MIN = 0.45           # linear Rec.709
GREEN_OVER_BLUE = 8 / 255.0
RED_MIN = 120 / 255.0
MIN_BLOB_PX = 25
CONTRAST_OK = 2.5
RING_INNER, RING_OUTER = 1.5, 3.0

_SRGB = [((c / 255.0 / 12.92) if c / 255.0 <= 0.04045 else (((c / 255.0 + 0.055) / 1.055) ** 2.4))
         for c in range(256)]


def linear_luma(r, g, b):
    return 0.2126 * _SRGB[r] + 0.7152 * _SRGB[g] + 0.0722 * _SRGB[b]


def largest_blob(mask, w, h):
    """The biggest 4-connected component of mask (a flat bytearray), as a list of indices."""
    seen = bytearray(w * h)
    best = []
    for start in range(w * h):
        if not mask[start] or seen[start]:
            continue
        seen[start] = 1
        comp, stack = [start], [start]
        while stack:
            i = stack.pop()
            x, y = i % w, i // w
            if x > 0:
                j = i - 1
                if mask[j] and not seen[j]:
                    seen[j] = 1
                    comp.append(j)
                    stack.append(j)
            if x < w - 1:
                j = i + 1
                if mask[j] and not seen[j]:
                    seen[j] = 1
                    comp.append(j)
                    stack.append(j)
            if y > 0:
                j = i - w
                if mask[j] and not seen[j]:
                    seen[j] = 1
                    comp.append(j)
                    stack.append(j)
            if y < h - 1:
                j = i + w
                if mask[j] and not seen[j]:
                    seen[j] = 1
                    comp.append(j)
                    stack.append(j)
        if len(comp) > len(best):
            best = comp
    return best


def read_sidecar(path):
    """zoom and image height out of a CaptureRig .json sidecar, whatever it calls them."""
    zoom, img_h = None, None
    if not os.path.exists(path):
        return zoom, img_h
    with open(path, encoding='utf-8', errors='replace') as f:
        try:
            meta = json.load(f)
        except ValueError:
            return zoom, img_h
    if not isinstance(meta, dict):
        return zoom, img_h
    for key in ('zoom', 'camera_zoom', 'Zoom', 'cameraZoom'):
        if isinstance(meta.get(key), (int, float)):
            zoom = float(meta[key])
            break
    for key in ('height', 'image_height', 'Height', 'px_height'):
        if isinstance(meta.get(key), (int, float)):
            img_h = float(meta[key])
            break
    return zoom, img_h


def probe_tile(stem):
    jpg = os.path.join(HERE, stem + '.jpg')
    img = Image.open(jpg).convert('RGB')
    w, h = img.size
    px = img.load()

    zoom, img_h = read_sidecar(os.path.join(HERE, stem + '.json'))
    if img_h is None:
        img_h = float(h)

    mask = bytearray(w * h)
    lum = [0.0] * (w * h)
    for y in range(h):
        row = y * w
        for x in range(w):
            r, g, b = px[x, y]
            L = linear_luma(r, g, b)
            lum[row + x] = L
            if L >= LUMA_MIN and (g - b) / 255.0 >= GREEN_OVER_BLUE and r / 255.0 >= RED_MIN:
                mask[row + x] = 1

    blob = largest_blob(mask, w, h)
    out = {'tile': stem, 'width': w, 'height': h, 'zoom': zoom,
           'blob_px': len(blob), 'contrast': None, 'sac_across_m': None, 'ok': False}
    if len(blob) < MIN_BLOB_PX:
        return out

    xs = [i % w for i in blob]
    ys = [i // w for i in blob]
    cx, cy = sum(xs) / len(xs), sum(ys) / len(ys)
    blob_w = max(xs) - min(xs) + 1
    radius = math.sqrt(len(blob) / math.pi)

    l_sac = sum(lum[i] for i in blob) / len(blob)
    ring, inner, outer = [], (RING_INNER * radius) ** 2, (RING_OUTER * radius) ** 2
    y0, y1 = max(0, int(cy - RING_OUTER * radius)), min(h - 1, int(cy + RING_OUTER * radius))
    x0, x1 = max(0, int(cx - RING_OUTER * radius)), min(w - 1, int(cx + RING_OUTER * radius))
    for y in range(y0, y1 + 1):
        row = y * w
        for x in range(x0, x1 + 1):
            if mask[row + x]:
                continue
            d = (x - cx) ** 2 + (y - cy) ** 2
            if inner <= d <= outer:
                ring.append(lum[row + x])
    if not ring:
        return out
    l_ring = sum(ring) / len(ring)

    out['contrast'] = round((l_sac + 0.05) / (l_ring + 0.05), 2)
    out['blob_width_px'] = blob_w
    out['ring_px'] = len(ring)
    out['luma_sac'] = round(l_sac, 4)
    out['luma_ring'] = round(l_ring, 4)
    if zoom:
        m_per_px = 2.0 * zoom * math.tan(math.radians(FOV_DEG) / 2.0) / img_h
        out['sac_across_m'] = round(blob_w * m_per_px, 3)
    out['ok'] = out['contrast'] >= CONTRAST_OK
    return out


def main():
    out_name = 'probe.json'
    if '--out' in sys.argv:
        out_name = sys.argv[sys.argv.index('--out') + 1]
    stems = ['t%d' % k for k in range(8)]
    missing = [s for s in stems if not os.path.exists(os.path.join(HERE, s + '.jpg'))]
    if missing:
        print('missing tiles: ' + ' '.join(s + '.jpg' for s in missing))
        return 1
    per = [probe_tile(s) for s in stems]
    result = {'tiles': len(per),
              'sac_contrast_ok_tiles': sum(1 for t in per if t['ok']),
              'contrast_ok_threshold': CONTRAST_OK,
              'min_blob_px': MIN_BLOB_PX,
              'per_tile': per}
    with open(os.path.join(HERE, out_name), 'w', encoding='utf-8', newline='\n') as f:
        json.dump(result, f, indent=2)
        f.write('\n')
    for t in per:
        print('%-3s blob %5d px  contrast %-6s across %-6s %s'
              % (t['tile'], t['blob_px'], t['contrast'], t['sac_across_m'], 'OK' if t['ok'] else '--'))
    print('%s: %d of %d tiles read the sac at >= %.1f:1'
          % (out_name, result['sac_contrast_ok_tiles'], result['tiles'], CONTRAST_OK))
    return 0


if __name__ == '__main__':
    sys.exit(main())
