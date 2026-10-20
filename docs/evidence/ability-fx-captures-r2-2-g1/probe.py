#!/usr/bin/env python3
"""Measure the chlorine gas bank at the play zoom (T1, 30) at night, three takes before and three after.

WHY. The owner looked at r2-2's stills and said: "at zoom 30 at night there is no chlorine bank". The r2 probe
measured a bank that its own numbers called present (the plate difference was there), but at a difference bar of
20 it counted a haze the eye does not read as a bank. This pass asks a harder question: does a lift the EYE reads
(RGB distance >= 40 from the no-gas plate) cover enough of the frame, and is it bright enough, to be a bank?

And it asks it THREE TIMES per build, because one capture is not a measurement. The three takes of the CURRENT
build fix the bars: a change only counts when all three after takes clear max(before) by four times the
run-to-run spread of the before takes. Those bars are frozen in baseline.json and are never recomputed.

HOW
  the takes    t1-before-1..3.jpg   three captures of the build at commit f032514063, nothing changed
               t1-after-1..3.jpg    three captures after the look change
               t1-plate.jpg         ONE no-gas plate, the control for all six (the harness is frame-locked, so
                                    the plate of one run is the plate of every run)
  gas mask     a pixel whose RGB distance from the plate is >= 40 (DIFF_BAR). r2 used 20; a 20/255 lift on a
               night field is a haze, not a bank, which is exactly the gap between r2's green numbers and the
               owner's eye.
  the blob     only the single largest coherent patch of that mask (`largest_blob`, r2's): JPEG block noise and
               moving infantry scatter thin diff threads across the whole frame; the bank is the one dense patch.
  the numbers  bank_area_frac      blob pixels / 921,600 (the whole 1280x720 frame). How much of the play frame
                                   the bank visibly covers. This is the owner's complaint, in one number.
               bank_contrast_p50   median RGB distance from the plate inside the blob. How strong the lift is.
               low_share           share of the blob's pixels at or under 1.8 m, a man's head. r2's pooling
                                   check, off the same blob: a bank lies on the ground.
               height_frac         45th-percentile column height / the frame's world height at the blob's
                                   distance. r2's other pooling check: a bank is not a wall.
               pose_error_m        out of each still's sidecar; a still from the wrong pose is worthless.
  the camera   rebuilt from the sidecar (r2's `ray` / `ground_distance` / `height_at`), so heights are metres and
               not screen space. `--selftest` checks that maths against a hand calculation.

USAGE
    python probe.py --baseline <d1> <d2> <d3> --plate <d>   pack the three before takes and the plate, measure
                                                            them, and FREEZE the bars into baseline.json
    python probe.py --after <d1> <d2> <d3>                  pack the three after takes
    python probe.py --plate-drift <d>                       pack the changed build's own plate (t1-plate2) and
                                                            report plate_drift_px: a plate holds no gas cards, so
                                                            a look change must not move it. A ROW, not a bar.
    python probe.py                                         measure the three after takes against the frozen bars
    python probe.py --selftest                              the camera maths, no images

Exit 0 only when all three after takes clear every bar: bank_area_frac >= bank_area_bar,
bank_contrast_p50 >= contrast_bar, low_share >= 0.30, height_frac <= 0.15, and pose_error_m < 0.5.
"""
import json
import math
import os
import sys

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
FOV_V_DEG = 25.0            # TacticalCamera.Fov, vertical
ASPECT = 16.0 / 9.0
HEAD_M = 1.8                # a man's head: gas at or under this is on the ground with him
DIFF_BAR = 40               # RGB distance from the plate that the eye reads as a lift (r2 used 20: a haze)
BLOB_DILATE = 1             # largest_blob's bridge width; 2 chained the bank into terrain-edge noise (r2)
FRAME_PX = 1280 * 720       # 921,600: bank_area_frac's denominator
LOW_BAR = 0.30              # r2's pooling bars, unchanged
HEIGHT_BAR = 0.15
POSE_BAR = 0.5
AREA_FLOOR = 0.03           # the bar floors; see README step 5 for the 3% footprint working
CONTRAST_FLOOR = 30.0
SPREAD_K = 4                # bars sit this many run-to-run spreads above the best before take
BASELINE = os.path.join(HERE, 'baseline.json')


def ray(ndc_x, ndc_y, pitch_deg, yaw_deg):
    """World direction of the pixel at (ndc_x, ndc_y), both -1..1, ndc_y up.

    Unity's camera: euler X pitches the forward vector DOWN, euler Y turns it clockwise seen from above.
      Rx(p) . (x, y, z) = (x, y cos p - z sin p, y sin p + z cos p)
      Ry(w) . (x, y, z) = (x cos w + z sin w, y, -x sin w + z cos w)
    """
    tan_v = math.tan(math.radians(FOV_V_DEG * 0.5))
    tan_h = tan_v * ASPECT
    x, y, z = ndc_x * tan_h, ndc_y * tan_v, 1.0
    p, w = math.radians(pitch_deg), math.radians(yaw_deg)
    y, z = y * math.cos(p) - z * math.sin(p), y * math.sin(p) + z * math.cos(p)
    x, z = x * math.cos(w) + z * math.sin(w), -x * math.sin(w) + z * math.cos(w)
    n = math.sqrt(x * x + y * y + z * z)
    return x / n, y / n, z / n


def ground_distance(cam, pitch, yaw, ground_y, ndc_x, ndc_y):
    """Horizontal distance from the camera to where this pixel's ray crosses y = ground_y (None if it never does)."""
    dx, dy, dz = ray(ndc_x, ndc_y, pitch, yaw)
    if dy >= -1e-6:
        return None
    t = (ground_y - cam[1]) / dy
    if t <= 0:
        return None
    return t * math.hypot(dx, dz)


def height_at(cam, pitch, yaw, ground_y, ndc_x, ndc_y, dist):
    """Height above the ground of this pixel, taken in the vertical plane `dist` metres out from the camera."""
    dx, dy, dz = ray(ndc_x, ndc_y, pitch, yaw)
    flat = math.hypot(dx, dz)
    if flat <= 1e-9:
        return None
    t = dist / flat
    return cam[1] + t * dy - ground_y


def selftest():
    """Hand calculation, worked here and not taken from the code it checks.

    Camera at (0, 10, 0), pitch 45 deg (looking down), yaw 0, ground at y = 0.
      The CENTRE pixel's ray leaves at 45 deg below the horizon, so it drops 10 m over 10 m of ground:
      it crosses y = 0 at a horizontal distance of exactly 10.000 m.
      A pixel ONE DEGREE ABOVE centre leaves at 45 - 1 = 44 deg below the horizon. Followed out to the same
      vertical plane, 10 m from the camera, it has dropped 10 * tan(44 deg) = 10 * 0.965689 = 9.65689 m, so it
      stands 10 - 9.65689 = 0.34311 m above the ground.
    """
    cam, pitch, yaw = (0.0, 10.0, 0.0), 45.0, 0.0
    tan_v = math.tan(math.radians(FOV_V_DEG * 0.5))
    d = ground_distance(cam, pitch, yaw, 0.0, 0.0, 0.0)
    assert d is not None and abs(d - 10.0) < 1e-6, 'centre pixel hits the ground at %r, wanted 10.000' % d
    one_deg = math.tan(math.radians(1.0)) / tan_v          # one degree up, in ndc
    h = height_at(cam, pitch, yaw, 0.0, 0.0, one_deg, d)
    assert abs(h - 0.34311) < 1e-3, 'one degree up came back %.5f m, hand calculation says 0.34311' % h
    print('selftest ok: centre pixel 10.000 m out, one degree up %.5f m high (hand: 0.34311)' % h)
    return 0


def jpg(png, out):
    """`png` saved as `out`, quality stepped down until it is under 400 KB."""
    img = Image.open(png).convert('RGB')
    for q in range(92, 40, -6):
        img.save(out, 'JPEG', quality=q)
        if os.path.getsize(out) <= 400 * 1024:
            break


def ground_of(src, still):
    """ground_y for `still` out of the capture dir's manifest (the CaptureRig sidecar does not carry it)."""
    for name in ('manifest.json', 'manifest-plate.json'):
        path = os.path.join(src, name)
        if not os.path.exists(path):
            continue
        with open(path, encoding='utf-8') as f:
            for row in json.load(f).get('stills', []):
                if row.get('still', '') == still:
                    return float(row.get('ground_y', 0.0))
    return None


def pack(src, src_still, stem):
    """Pack one capture dir's still in here as `stem`.jpg plus `stem`.json, with ground_y folded into the sidecar.

    The sidecar carries the pose; ground_y lives in the run's manifest. Merging them here keeps every number a
    measurement needs inside this folder, so the probe runs on a fresh clone with no capture dirs left.
    """
    png = os.path.join(src, src_still + '.png')
    if not os.path.exists(png):
        raise SystemExit('no %s in %s' % (src_still + '.png', src))
    jpg(png, os.path.join(HERE, stem + '.jpg'))
    side = os.path.join(src, src_still + '.json')
    if not os.path.exists(side):
        raise SystemExit('no sidecar beside ' + png)
    with open(side, encoding='utf-8') as f:
        data = json.load(f)
    g = ground_of(src, src_still + '.png')
    if g is not None:
        data['ground_y'] = g
    data['capture_dir'] = os.path.abspath(src)
    with open(os.path.join(HERE, stem + '.json'), 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=1, sort_keys=True)
    return stem


def load(stem):
    img = Image.open(os.path.join(HERE, stem + '.jpg')).convert('RGB')
    with open(os.path.join(HERE, stem + '.json'), encoding='utf-8') as f:
        side = json.load(f)
    return img, side


def distances(img, plate):
    """RGB distance from the plate, per pixel, as a flat list of floats."""
    return [math.sqrt((p[0] - q[0]) ** 2 + (p[1] - q[1]) ** 2 + (p[2] - q[2]) ** 2)
            for p, q in zip(img.getdata(), plate.getdata())]


def largest_blob(mask, w, h, dilate=BLOB_DILATE):
    """Keep only the mask pixels inside the single biggest coherent patch (r2's, unchanged).

    Dilate the mask to bridge the small gaps JPEG leaves inside the bank's patch, flood-fill the dilated pixels
    into components, keep the biggest, then intersect back with the ORIGINAL mask so only pixels that really
    differed from the plate are counted.
    """
    from collections import deque
    n = w * h
    dil = bytearray(1 if m else 0 for m in mask)
    for _ in range(dilate):
        nxt = bytearray(dil)
        for y in range(h):
            row = y * w
            for x in range(w):
                i = row + x
                if dil[i]:
                    continue
                if ((x > 0 and dil[i - 1]) or (x < w - 1 and dil[i + 1])
                        or (y > 0 and dil[i - w]) or (y < h - 1 and dil[i + w])):
                    nxt[i] = 1
        dil = nxt

    seen = bytearray(n)
    best_size, best_members = 0, []
    for i in range(n):
        if not dil[i] or seen[i]:
            continue
        seen[i] = 1
        q = deque([i])
        members = []
        while q:
            j = q.popleft()
            members.append(j)
            x, y = j % w, j // w
            for nj in ((j - 1 if x > 0 else -1), (j + 1 if x < w - 1 else -1),
                       (j - w if y > 0 else -1), (j + w if y < h - 1 else -1)):
                if nj >= 0 and dil[nj] and not seen[nj]:
                    seen[nj] = 1
                    q.append(nj)
        if len(members) > best_size:
            best_size = len(members)
            best_members = members
    keep = bytearray(n)
    for j in best_members:
        keep[j] = 1
    return [bool(mask[i]) and bool(keep[i]) for i in range(n)]


def measure(stem, plate_stem='t1-plate'):
    """Every number for one take, off its own still and the shared plate."""
    img, side = load(stem)
    plate, _ = load(plate_stem)
    w, h = img.size
    dist = distances(img, plate)
    mask = largest_blob([d >= DIFF_BAR for d in dist], w, h)
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ground = float(side.get('ground_y', 0.0))

    blob = [i for i, on in enumerate(mask) if on]
    out = {'bank_area_frac': round(len(blob) / float(FRAME_PX), 5),
           'bank_px': len(blob),
           'pose_error_m': round(float(side.get('pose_error_m', 99.0)), 4)}
    if not blob:
        out.update({'bank_contrast_p50': 0.0, 'low_share': 0.0, 'height_frac': 1.0,
                    'bank_height_m': 0.0, 'bank_dist_m': 0.0, 'columns': 0})
        return out
    inside = sorted(dist[i] for i in blob)
    out['bank_contrast_p50'] = round(inside[len(inside) // 2], 2)

    by_column = {}
    for i in blob:
        by_column.setdefault(i % w, []).append(i // w)

    MIN_COLUMN_PX = 8
    columns = {px: max(ys) for px, ys in by_column.items() if len(ys) >= MIN_COLUMN_PX}

    def ndc(px, py):
        return (2.0 * (px + 0.5) / w - 1.0, 1.0 - 2.0 * (py + 0.5) / h)

    dists = {}
    for px, py in columns.items():
        nx, ny = ndc(px, py)
        d = ground_distance(cam, pitch, yaw, ground, nx, ny)
        if d is not None and 1.0 < d < 2000.0:
            dists[px] = d

    low, total, col_heights = 0, 0, {}
    for i in blob:
        px, py = i % w, i // w
        if px not in dists:
            continue
        nx, ny = ndc(px, py)
        y = height_at(cam, pitch, yaw, ground, nx, ny, dists[px])
        if y is None:
            continue
        total += 1
        if y <= HEAD_M:
            low += 1
        col_heights.setdefault(px, []).append(y)
    heights = []
    for px, ys in col_heights.items():
        ys.sort()
        heights.append(ys[min(len(ys) - 1, int(round(0.45 * (len(ys) - 1))))])
    heights.sort()
    if not heights or total == 0:
        out.update({'low_share': 0.0, 'height_frac': 1.0, 'bank_height_m': 0.0, 'bank_dist_m': 0.0,
                    'columns': 0})
        return out
    med_h = heights[len(heights) // 2]
    dvals = sorted(dists.values())
    med_d = dvals[len(dvals) // 2]
    frame_h = 2.0 * med_d * math.tan(math.radians(FOV_V_DEG * 0.5))
    out.update({'low_share': round(low / float(total), 4),
                'height_frac': round(med_h / frame_h, 4),
                'bank_height_m': round(med_h, 3),
                'bank_dist_m': round(med_d, 2),
                'frame_height_m': round(frame_h, 2),
                'columns': len(heights)})
    return out


def arg_dirs(flag, count):
    """The `count` directory arguments after `flag`, or None when the flag is absent."""
    if flag not in sys.argv:
        return None
    at = sys.argv.index(flag) + 1
    got = sys.argv[at:at + count]
    if len(got) < count or any(g.startswith('--') for g in got):
        raise SystemExit('%s wants %d directories' % (flag, count))
    return got


def spread_bar(values, floor):
    """The frozen bar: four run-to-run spreads above the best before take, never under `floor`."""
    s = max(values) - min(values)
    return round(max(floor, max(values) + SPREAD_K * s), 5)


def do_baseline(dirs, plate_dir):
    pack(plate_dir, 'gas-t1-plate', 't1-plate')
    rows = {}
    for k, d in enumerate(dirs, start=1):
        stem = 't1-before-%d' % k
        pack(d, 'gas-t1', stem)
        rows[stem] = measure(stem)
    areas = [rows[s]['bank_area_frac'] for s in sorted(rows)]
    contrasts = [rows[s]['bank_contrast_p50'] for s in sorted(rows)]
    out = {
        'built_from_commit': 'f032514063',
        'diff_bar': DIFF_BAR,
        'before': rows,
        'spread': {'bank_area_frac': round(max(areas) - min(areas), 5),
                   'bank_contrast_p50': round(max(contrasts) - min(contrasts), 2)},
        'bars': {'bank_area_bar': spread_bar(areas, AREA_FLOOR),
                 'contrast_bar': spread_bar(contrasts, CONTRAST_FLOOR),
                 'low_share': LOW_BAR, 'height_frac': HEIGHT_BAR, 'pose_error_max': POSE_BAR},
        'frozen': 'computed once, from the three before takes of commit f032514063; never recomputed',
    }
    with open(BASELINE, 'w', encoding='utf-8') as f:
        json.dump(out, f, indent=1, sort_keys=True)
    for stem in sorted(rows):
        r = rows[stem]
        print('%s  area %.5f  contrast %6.2f  low %.4f  height %.4f  pose %.3f'
              % (stem, r['bank_area_frac'], r['bank_contrast_p50'], r['low_share'], r['height_frac'],
                 r['pose_error_m']))
    print('spread  area %.5f  contrast %.2f' % (out['spread']['bank_area_frac'],
                                                out['spread']['bank_contrast_p50']))
    print('FROZEN bars  bank_area_bar %.5f  contrast_bar %.2f'
          % (out['bars']['bank_area_bar'], out['bars']['contrast_bar']))
    return 0


def do_plate_drift(src):
    """Pack the changed build's no-gas plate and count how many pixels moved from the before plate.

    A plate pass skips every gas call, so the gas cards are not drawn at all and the look change cannot reach the
    image. Any pixel that moves is JPEG or a stray in the scene, and the number is reported as a row: it is not a
    bar and it cannot fail the probe.
    """
    pack(src, 'gas-t1-plate', 't1-plate2')
    before, _ = load('t1-plate')
    after, _ = load('t1-plate2')
    d = distances(after, before)
    moved = sum(1 for v in d if v >= DIFF_BAR)
    print('plate_drift_px %d of %d (>= %d RGB distance, before plate vs the changed build plate)'
          % (moved, FRAME_PX, DIFF_BAR))
    return moved


def main():
    if '--selftest' in sys.argv:
        return selftest()
    drift = arg_dirs('--plate-drift', 1)
    if drift is not None:
        do_plate_drift(drift[0])
    base = arg_dirs('--baseline', 3)
    if base is not None:
        plate = arg_dirs('--plate', 1)
        if plate is None:
            raise SystemExit('--baseline also needs --plate <dir>')
        return do_baseline(base, plate[0])
    after = arg_dirs('--after', 3)
    if after is not None:
        for k, d in enumerate(after, start=1):
            print('packed ' + pack(d, 'gas-t1', 't1-after-%d' % k))

    if not os.path.exists(BASELINE):
        print('no baseline.json: run --baseline <d1> <d2> <d3> --plate <d> first')
        return 1
    with open(BASELINE, encoding='utf-8') as f:
        bars = json.load(f)['bars']
    stems = ['t1-after-%d' % k for k in (1, 2, 3)]
    missing = [s for s in stems + ['t1-plate'] if not os.path.exists(os.path.join(HERE, s + '.jpg'))]
    if missing:
        print('missing stills: ' + ', '.join(missing))
        return 1

    rows = {s: measure(s) for s in stems}
    ok = True
    for s in stems:
        r = rows[s]
        good = (r['bank_area_frac'] >= bars['bank_area_bar']
                and r['bank_contrast_p50'] >= bars['contrast_bar']
                and r['low_share'] >= bars['low_share']
                and r['height_frac'] <= bars['height_frac']
                and r['pose_error_m'] < bars['pose_error_max'])
        ok = ok and good
        print('%s  area %.5f (>= %.5f)  contrast %6.2f (>= %.2f)  low %.4f (>= %.2f)  height %.4f (<= %.2f)'
              '  pose %.3f (< %.2f)  %s'
              % (s, r['bank_area_frac'], bars['bank_area_bar'], r['bank_contrast_p50'], bars['contrast_bar'],
                 r['low_share'], bars['low_share'], r['height_frac'], bars['height_frac'],
                 r['pose_error_m'], bars['pose_error_max'], 'ok' if good else 'FAILED'))
    result = {'bars': bars, 'after': rows, 'ok': ok}
    with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
        json.dump(result, f, indent=1, sort_keys=True)
    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
