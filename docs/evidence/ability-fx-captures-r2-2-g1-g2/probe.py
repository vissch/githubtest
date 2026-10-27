#!/usr/bin/env python3
"""Measure the chlorine gas bank at the play zoom (T1, 30) at night: the paint a player sees, on the ground.

WHY. g1 passed its own bars and the owner threw the numbers out, one by one:

  "The probe calls 29975 pixels one bank."   `largest_blob` bridged the mask through one-pixel gaps, so the thin
                                             gas stripes blown across the whole field chained into a single
                                             690-column "blob". 29,975 scattered pixels are not a bank.
  "while the stripes cover the field"        the same thing seen from the field's side. The stripes are real
                                             paint, but they are not the bank; they are now a named ROW
                                             (`stripe_frac`) and no longer counted as bank.
  "it clears a bar of 0.030 by 0.00252"      a bar it only cleared because the blob swept up the stripes.
  "The three after takes are one frame, so   the harness shot three stills of the SAME frame. Three takes of one
   the spread is zero"                       frame are one take, and a bar built on a zero spread is the before
                                             take's own number ("contrast_bar 50.57 is the before take's own
                                             contrast"). The harness now shoots at three frames 15 apart, and
                                             `--baseline` REFUSES to freeze a bar on a zero spread.
  "low_share fell from 0.9874 to 0.4553 and  the old height measure read a DEEPER flat bank as a taller one: it
   the mask rose from 0.39 m to 1.798 m"     placed every pixel of a column at that column's own ground crossing
                                             and then took the column's top. Grow the footprint and the numbers
                                             move without the gas leaving the ground.

So: count only the paint inside the bank's own footprint, judge height one-sidedly against the footprint's far
rim, and put the bars outside the spread of three captures that can differ.

HOW
  the takes    t1-before-1..3.jpg   three captures of the current build (g1's tip), 15 frames apart
               t1-plate-1..3.jpg    three NO-GAS plates from the same frames of a plate run; each take is
                                    measured against ITS OWN plate, so a moving man is not read as gas
               t1-after-1..3.jpg    three captures after the look change, same three frames
  the mask     a pixel whose RGB distance from its plate is >= 40 (DIFF_BAR): a lift the eye reads
  the window   "the paint a player sees". The bank's footprint is the disc of radius R round the gas centre
               (`focus_x`, `focus_z`, `ground_y` out of the sidecar), with R = sqrt(cells * 16 / pi) clamped
               to 7..40 m (the sim's gas grid is 4 m cells, so each live cell is 16 m^2). `cells` is
               `gas_cells_near`, the live cells within 25 m of the gas centre, because the whole field's
               `gas_cells` sized a 40 m disc that was 95% of the frame and counted the stripes as bank again
               (decisions.md, the open footprint row of 2026-10-20, option (a)). 64 points on that
               disc's rim are projected to pixels (`to_ndc`, the inverse of `ray`); the window is their bounding
               box, grown upward by the pixel height of 2 m at the near rim. Mask pixels INSIDE it are paint.
  the numbers  bank_area_frac    paint / 921,600 (the whole 1280x720 frame). The owner's complaint in one number.
               bank_fill         paint / window pixels. How solid the bank is inside its own footprint.
               bank_contrast_p50 median RGB distance from the plate, over the paint.
               low_share         share of paint whose ground crossing lies between the window's near and far rim,
                                 i.e. gas that CAN be lying on the ground inside the footprint.
               bank_height_m     p95 of each paint pixel's LOWEST possible gas. For a ray pointing down, the
                                 nearer you place a pixel the higher it sits, so the footprint's far rim gives a
                                 pixel its minimum height: a pixel crossing the ground beyond the far rim stands
                                 at least `height_at(..., d_far)` high, and one crossing inside the footprint can
                                 be flat on the ground (0). One-sided, so depth can never be read as height.
               pose_error_m      out of the sidecar; a still from the wrong pose is worthless.
               stripe_frac       ROW, not a bar: mask pixels OUTSIDE the window / 921,600. The thin paint over
                                 the rest of the field, named and not counted as bank.

USAGE
    python probe.py --baseline <d1> <d2> <d3> --plate <d>   pack the three before takes and the plate run's three
                                                            plates, measure them, and FREEZE the bars
    python probe.py --after <d1> <d2> <d3>                  pack the three after takes
    python probe.py                                         measure the three after takes against the frozen bars
    python probe.py --selftest                              the camera maths, no images

Exit 0 only when all three after takes clear every bar: bank_area_frac >= bank_area_bar,
bank_fill >= bank_fill_bar, bank_contrast_p50 >= contrast_bar, low_share >= 0.85, bank_height_m <= 1.0, and
pose_error_m < 0.5.
"""
import json
import math
import os
import sys

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
FOV_V_DEG = 25.0            # TacticalCamera.Fov, vertical
ASPECT = 16.0 / 9.0
DIFF_BAR = 40               # RGB distance from the plate that the eye reads as a lift (r2 used 20: a haze)
FRAME_PX = 1280 * 720       # 921,600: bank_area_frac's denominator
GAS_CELL_M2 = 16.0          # the sim's gas grid is 4 m cells
R_MIN_M, R_MAX_M = 7.0, 40.0
WINDOW_LIFT_M = 2.0         # the window is grown upward by this much of world height at the near rim
LOW_BAR = 0.85              # fixed bars: the bank lies on the ground inside its own footprint
HEIGHT_BAR_M = 1.0
POSE_BAR = 0.5
AREA_FLOOR = 0.03           # the bar floors; see README for the 3% footprint working
FILL_FLOOR = 0.35
CONTRAST_FLOOR = 30.0
SPREAD_K = 4                # bars sit this many run-to-run spreads above the best before take
RIM_POINTS = 64
# A before take whose window is wider than this much of the frame is not a footprint, it is the frame. The two cases
# this has to tell apart are both measured, on the 2026-10-21 captures of 6da3ac9872: the 25 m near footprint
# (gas_cells_near 124) gives a window of 576,288 px of 921,600 = 0.625, and the old whole-field footprint, R clamped
# to 40 m, gave 871,680 = 0.946 with stripe_frac 0. The bound sits between them. (The plan's 0.60 came from scaling
# the 40 m window's AREA by (25/40)^2; the window is the bounding BOX of a disc seen at a 25 deg pitch, and a box
# does not shrink that way -- hence the measured number here.)
WINDOW_MAX_FRAC = 0.70
BASELINE = os.path.join(HERE, 'baseline.json')
SRC_GAS = ('gas-t1', 'gas-t1-b', 'gas-t1-c')
SRC_PLATE = ('gas-t1-plate', 'gas-t1-plate-b', 'gas-t1-plate-c')


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


def to_ndc(cam, pitch_deg, yaw_deg, point):
    """The ndc of a world point, the inverse of `ray` (None when the point is behind the camera).

    `ray` turns a camera-space direction by Rx(pitch) and then Ry(yaw), so undo them the other way round:
    Ry(-yaw), then Rx(-pitch), then divide out the depth.
    """
    x, y, z = point[0] - cam[0], point[1] - cam[1], point[2] - cam[2]
    p, w = math.radians(pitch_deg), math.radians(yaw_deg)
    x, z = x * math.cos(w) - z * math.sin(w), x * math.sin(w) + z * math.cos(w)
    y, z = y * math.cos(p) + z * math.sin(p), -y * math.sin(p) + z * math.cos(p)
    if z <= 1e-6:
        return None
    tan_v = math.tan(math.radians(FOV_V_DEG * 0.5))
    return x / z / (tan_v * ASPECT), y / z / tan_v


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
      Backwards, through `to_ndc`: the forward axis at yaw 0 is +z, so that ground point is (0, 0, 10) and it must
      come back as the centre of the frame, (0.000, 0.000). The point 10 m out and 0.34311 m high is one degree
      above the centre, so its ndc_y is tan(1 deg) / tan(12.5 deg) = 0.017455 / 0.221695 = 0.0788.
    """
    cam, pitch, yaw = (0.0, 10.0, 0.0), 45.0, 0.0
    tan_v = math.tan(math.radians(FOV_V_DEG * 0.5))
    d = ground_distance(cam, pitch, yaw, 0.0, 0.0, 0.0)
    assert d is not None and abs(d - 10.0) < 1e-6, 'centre pixel hits the ground at %r, wanted 10.000' % d
    one_deg = math.tan(math.radians(1.0)) / tan_v          # one degree up, in ndc
    h = height_at(cam, pitch, yaw, 0.0, 0.0, one_deg, d)
    assert abs(h - 0.34311) < 1e-3, 'one degree up came back %.5f m, hand calculation says 0.34311' % h

    centre = to_ndc(cam, pitch, yaw, (0.0, 0.0, 10.0))
    assert centre is not None and abs(centre[0]) < 1e-6 and abs(centre[1]) < 1e-6, \
        'the ground point 10 m out came back at %r, hand calculation says (0.000, 0.000)' % (centre,)
    up = to_ndc(cam, pitch, yaw, (0.0, 0.34311, 10.0))
    assert up is not None and abs(up[0]) < 1e-6 and abs(up[1] - 0.0788) < 5e-4, \
        'the point 0.34311 m high came back at %r, hand calculation says ndc_y 0.0788' % (up,)
    print('selftest ok: centre pixel 10.000 m out, one degree up %.5f m high (hand: 0.34311); '
          'to_ndc gives (%.3f, %.3f) and ndc_y %.4f (hand: 0.0788)' % (h, centre[0], centre[1], up[1]))

    side_path = os.path.join(HERE, 't1-after-1.json')
    with open(side_path, encoding='utf-8') as f:
        side = json.load(f)
    win = window(side, 1280, 720)
    assert win is not None, 'window() on t1-after-1.json returned None'
    near_c = max(win['cols'], key=lambda c: win['cols'][c][1])        # the column holding the near rim
    far_c = min(win['cols'], key=lambda c: win['cols'][c][0])         # the column holding the far rim
    near_band, far_band = win['cols'][near_c], win['cols'][far_c]
    assert abs(near_band[1] - win['y1']) <= 1.0, \
        'near rim column bottom %.2f, box y1 %d (want within 1 px)' % (near_band[1], win['y1'])
    assert abs(far_band[0] - win['y0']) <= 1.0, \
        'far rim column top %.2f, box y0 %d (want within 1 px)' % (far_band[0], win['y0'])
    print('selftest ok: silhouette near rim column %d bottom %.2f (box y1 %d), far rim column %d top %.2f '
          '(box y0 %d)' % (near_c, near_band[1], win['y1'], far_c, far_band[0], win['y0']))
    return 0


def jpg(png, out):
    """`png` saved as `out`, quality stepped down until it is under 400 KB."""
    img = Image.open(png).convert('RGB')
    for q in range(92, 40, -6):
        img.save(out, 'JPEG', quality=q)
        if os.path.getsize(out) <= 400 * 1024:
            break


def manifest_row(src, still):
    """The capture dir's manifest row for `still`, or None."""
    for name in ('manifest.json', 'manifest-plate.json'):
        path = os.path.join(src, name)
        if not os.path.exists(path):
            continue
        with open(path, encoding='utf-8') as f:
            for row in json.load(f).get('stills', []):
                if row.get('still', '') == still:
                    return row
    return None


def pack(src, src_still, stem):
    """Pack one capture dir's still in here as `stem`.jpg plus `stem`.json.

    The sidecar carries the pose; `ground_y` and `gas_cells` live in the run's manifest. Merging them here keeps
    every number a measurement needs inside this folder, so the probe runs on a fresh clone with no capture dirs.
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
    row = manifest_row(src, src_still + '.png')
    if row is not None:
        if 'ground_y' in row:
            data['ground_y'] = float(row['ground_y'])
        if 'gas_cells' in row:
            data['gas_cells'] = int(row['gas_cells'])
        if 'gas_cells_near' in row:
            data['gas_cells_near'] = int(row['gas_cells_near'])
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


def footprint_radius(gas_cells, gas_cells_near=-1):
    """The bank's footprint radius: the disc of the same area as the live gas cells, clamped to 7..40 m.

    `gas_cells_near` is the live cells within NearRadiusM of the gas centre (GasBankStills; decisions.md's open
    footprint row of 2026-10-20, option (a)) and is used whenever the sidecar has one. The whole field's count
    sized a 40 m disc that covered 95% of the frame and counted the field-wide stripes as bank. A take packed
    before the harness wrote the near count has -1, and falls back to the old wide window so it still measures.
    """
    cells = float(gas_cells_near) if gas_cells_near is not None and gas_cells_near > -1 else float(gas_cells)
    r = math.sqrt(max(1.0, cells) * GAS_CELL_M2 / math.pi)
    return min(R_MAX_M, max(R_MIN_M, r))


def silhouette_cols(xs, ys, x0, x1, lift_px):
    """Per-column band of the rim polygon (xs, ys): `{c: (y_top, y_bot)}` for c in x0..x1.

    Scan-converts the (possibly open, when a rim point was dropped behind the camera) polygon edge by
    edge: for each column `c`, the polygon's edges crossing the vertical line x = c give its y-extent
    there. `lift_px` (the same 2 m WINDOW_LIFT_M the box uses) is subtracted from y_top only, so the
    silhouette gets the same upward grow as the box did.
    """
    n = len(xs)
    cols = {}
    for i in range(n):
        xa, ya, xb, yb = xs[i], ys[i], xs[(i + 1) % n], ys[(i + 1) % n]
        if xa == xb:
            continue
        lo, hi = (xa, xb) if xa < xb else (xb, xa)
        c0, c1 = max(x0, int(math.ceil(lo))), min(x1, int(math.floor(hi)))
        for c in range(c0, c1 + 1):
            t = (c - xa) / (xb - xa)
            y = ya + t * (yb - ya)
            top, bot = cols.get(c, (y, y))
            cols[c] = (min(top, y), max(bot, y))
    return {c: (top - lift_px, bot) for c, (top, bot) in cols.items()}


def window(side, w, h):
    """The pixel box of the bank's footprint, plus its near/far rim distance and column silhouette.

    Returns {x0, y0, x1, y1, d_near, d_far, radius, cols}, the box in pixel coordinates (clamped to the
    frame) and, per column `c` in x0..x1, `cols[c] = (y_top, y_bot)`: the rim polygon's own y-extent at
    that column, grown upward by the same lift as the box. The box is unchanged by this; `cols` is new.
    """
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ground = float(side.get('ground_y', 0.0))
    cx, cz = float(side['focus_x']), float(side['focus_z'])
    r = footprint_radius(side.get('gas_cells', 0), int(side.get('gas_cells_near', -1)))

    xs, ys, near_pt, near_d = [], [], None, None
    for k in range(RIM_POINTS):
        a = 2.0 * math.pi * k / RIM_POINTS
        p = (cx + r * math.cos(a), ground, cz + r * math.sin(a))
        nd = to_ndc(cam, pitch, yaw, p)
        if nd is None:
            continue
        xs.append((nd[0] + 1.0) * 0.5 * w - 0.5)
        ys.append((1.0 - nd[1]) * 0.5 * h - 0.5)
        d = math.hypot(p[0] - cam[0], p[2] - cam[2])
        if near_d is None or d < near_d:
            near_d, near_pt = d, p
    if not xs:
        return None
    lift_px = 0.0
    if near_pt is not None:
        low = to_ndc(cam, pitch, yaw, near_pt)
        high = to_ndc(cam, pitch, yaw, (near_pt[0], near_pt[1] + WINDOW_LIFT_M, near_pt[2]))
        if low is not None and high is not None:
            lift_px = max(0.0, (high[1] - low[1]) * 0.5 * h)      # ndc_y is up, pixel rows go down
    d_centre = math.hypot(cx - cam[0], cz - cam[2])
    x0 = max(0, int(math.floor(min(xs))))
    x1 = min(w - 1, int(math.ceil(max(xs))))
    y0 = max(0, int(math.floor(min(ys) - lift_px)))
    y1 = min(h - 1, int(math.ceil(max(ys))))
    if x1 < x0 or y1 < y0:
        return None
    cols = silhouette_cols(xs, ys, x0, x1, lift_px)
    return {'x0': x0, 'y0': y0, 'x1': x1, 'y1': y1, 'd_near': max(1.0, d_centre - r), 'd_far': d_centre + r,
            'radius': r, 'cols': cols}


def measure(stem, plate_stem):
    """Every number for one take, off its own still and its OWN plate."""
    img, side = load(stem)
    plate, _ = load(plate_stem)
    w, h = img.size
    dist = distances(img, plate)
    mask = [d >= DIFF_BAR for d in dist]
    mask_px = sum(1 for m in mask if m)

    out = {'pose_error_m': round(float(side.get('pose_error_m', 99.0)), 4),
           'mask_px': mask_px,
           'gas_cells': int(side.get('gas_cells', 0)),
           'gas_cells_near': int(side.get('gas_cells_near', -1))}
    win = window(side, w, h)
    if win is None:
        out.update({'bank_area_frac': 0.0, 'bank_fill': 0.0, 'bank_contrast_p50': 0.0, 'low_share': 0.0,
                    'bank_height_m': 99.0, 'stripe_frac': round(mask_px / float(FRAME_PX), 5),
                    'window_px': 0, 'bank_px': 0})
        return out
    x0, y0, x1, y1 = win['x0'], win['y0'], win['x1'], win['y1']
    d_near, d_far, radius, cols = win['d_near'], win['d_far'], win['radius'], win['cols']
    win_px = (x1 - x0 + 1) * (y1 - y0 + 1)

    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ground = float(side.get('ground_y', 0.0))

    paint = []                                    # only the window is ever walked pixel by pixel
    for py in range(y0, y1 + 1):
        row = py * w
        for px in range(x0, x1 + 1):
            if mask[row + px]:
                paint.append(row + px)
    out['bank_px'] = len(paint)
    out['window_px'] = win_px
    out['window_radius_m'] = round(radius, 2)
    out['window_near_m'] = round(d_near, 2)
    out['window_far_m'] = round(d_far, 2)
    out['bank_area_frac'] = round(len(paint) / float(FRAME_PX), 5)
    out['bank_fill'] = round(len(paint) / float(win_px), 4)
    out['stripe_frac'] = round((mask_px - len(paint)) / float(FRAME_PX), 5)
    if not paint:
        out.update({'bank_contrast_p50': 0.0, 'low_share': 0.0, 'bank_height_m': 99.0})
        return out

    inside = sorted(dist[i] for i in paint)
    out['bank_contrast_p50'] = round(inside[len(inside) // 2], 2)

    low, mins, sil_px = 0, [], 0
    for i in paint:
        px, py = i % w, i // w
        nx, ny = 2.0 * (px + 0.5) / w - 1.0, 1.0 - 2.0 * (py + 0.5) / h
        d_pix = ground_distance(cam, pitch, yaw, ground, nx, ny)
        if d_pix is not None and d_near <= d_pix <= d_far:
            low += 1
        band = cols.get(px)
        in_sil = band is not None and band[0] <= py <= band[1]
        if in_sil:
            sil_px += 1
        if d_pix is not None and d_pix <= d_far:
            if in_sil:
                mins.append(0.0)                  # its ground crossing is inside the footprint: it can be flat
            continue
        if not in_sil:
            continue
        y = height_at(cam, pitch, yaw, ground, nx, ny, d_far)
        mins.append(max(0.0, y if y is not None else 0.0))
    out['low_share'] = round(low / float(len(paint)), 4)
    out['sil_px'] = sil_px
    out['height_px'] = len(mins)
    if not mins:
        out['bank_height_m'] = 99.0
        return out
    mins.sort()
    out['bank_height_m'] = round(mins[min(len(mins) - 1, int(round(0.95 * (len(mins) - 1))))], 3)
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


def line(stem, r):
    return ('%s  area %.5f  fill %.4f  contrast %6.2f  low %.4f  height %6.3f m  pose %.3f  stripes %.5f'
            % (stem, r['bank_area_frac'], r['bank_fill'], r['bank_contrast_p50'], r['low_share'],
               r['bank_height_m'], r['pose_error_m'], r['stripe_frac']))


def do_baseline(dirs, plate_dir):
    """Pack and measure the three before takes against the plate run's three plates, then freeze the bars."""
    for k in (1, 2, 3):
        pack(plate_dir, SRC_PLATE[k - 1], 't1-plate-%d' % k)
        pack(dirs[k - 1], SRC_GAS[k - 1], 't1-before-%d' % k)
    rows = {'t1-before-%d' % k: measure('t1-before-%d' % k, 't1-plate-%d' % k) for k in (1, 2, 3)}
    areas = [rows[s]['bank_area_frac'] for s in sorted(rows)]
    fills = [rows[s]['bank_fill'] for s in sorted(rows)]
    contrasts = [rows[s]['bank_contrast_p50'] for s in sorted(rows)]
    spread = {'bank_area_frac': round(max(areas) - min(areas), 5),
              'bank_fill': round(max(fills) - min(fills), 4),
              'bank_contrast_p50': round(max(contrasts) - min(contrasts), 2)}
    for stem in sorted(rows):
        print(line(stem, rows[stem]))
    print('spread  area %.5f  fill %.4f  contrast %.2f'
          % (spread['bank_area_frac'], spread['bank_fill'], spread['bank_contrast_p50']))
    wide = [s for s in sorted(rows)
            if rows[s]['window_px'] > WINDOW_MAX_FRAC * FRAME_PX or rows[s]['stripe_frac'] <= 0.0]
    if wide:
        print('REFUSED: %s measure(s) the window as the frame again (window_px over %.0f%% of the frame, or '
              'stripe_frac 0): %s. Then the field-wide stripes are counted as bank - the fault the owner threw '
              'out on g1. The footprint must be sized from the gas NEAR the focus (gas_cells_near).'
              % (len(wide), 100.0 * WINDOW_MAX_FRAC, ', '.join(
                  '%s window_px %d stripe_frac %.5f' % (s, rows[s]['window_px'], rows[s]['stripe_frac'])
                  for s in wide)))
        return 1
    if spread['bank_area_frac'] <= 0.0 or spread['bank_contrast_p50'] <= 0.0:
        print('REFUSED: the %s spread is zero. Three takes of one frame are one take, and a bar on top of a zero '
              'spread is the before take\'s own number (the owner, on g1). Shoot three frames that can differ.'
              % ('area' if spread['bank_area_frac'] <= 0.0 else 'contrast'))
        return 1
    out = {
        'built_from_commit': os.environ.get('TW_G2_COMMIT', 'be5e6444fc'),
        'diff_bar': DIFF_BAR,
        'before': rows,
        'spread': spread,
        'bars': {'bank_area_bar': spread_bar(areas, AREA_FLOOR),
                 'bank_fill_bar': spread_bar(fills, FILL_FLOOR),
                 'contrast_bar': spread_bar(contrasts, CONTRAST_FLOOR),
                 'low_share': LOW_BAR, 'bank_height_max_m': HEIGHT_BAR_M, 'pose_error_max': POSE_BAR},
        'frozen': "computed once, from the three before takes of g1's tip; never recomputed",
    }
    with open(BASELINE, 'w', encoding='utf-8') as f:
        json.dump(out, f, indent=1, sort_keys=True)
    print('FROZEN bars  bank_area_bar %.5f  bank_fill_bar %.4f  contrast_bar %.2f'
          % (out['bars']['bank_area_bar'], out['bars']['bank_fill_bar'], out['bars']['contrast_bar']))
    return 0


def main():
    if '--selftest' in sys.argv:
        return selftest()
    base = arg_dirs('--baseline', 3)
    if base is not None:
        plate = arg_dirs('--plate', 1)
        if plate is None:
            raise SystemExit('--baseline also needs --plate <dir>')
        return do_baseline(base, plate[0])
    after = arg_dirs('--after', 3)
    if after is not None:
        for k, d in enumerate(after, start=1):
            print('packed ' + pack(d, SRC_GAS[k - 1], 't1-after-%d' % k))

    if not os.path.exists(BASELINE):
        print('no baseline.json: run --baseline <d1> <d2> <d3> --plate <d> first')
        return 1
    with open(BASELINE, encoding='utf-8') as f:
        bars = json.load(f)['bars']
    stems = ['t1-after-%d' % k for k in (1, 2, 3)]
    plate_stems = ['t1-plate-%d' % k for k in (1, 2, 3)]
    missing = [s for s in stems + plate_stems if not os.path.exists(os.path.join(HERE, s + '.jpg'))]
    if missing:
        print('missing stills: ' + ', '.join(missing))
        return 1

    rows = {s: measure(s, p) for s, p in zip(stems, plate_stems)}
    ok = True
    for s in stems:
        r = rows[s]
        good = (r['bank_area_frac'] >= bars['bank_area_bar']
                and r['bank_fill'] >= bars['bank_fill_bar']
                and r['bank_contrast_p50'] >= bars['contrast_bar']
                and r['low_share'] >= bars['low_share']
                and r['bank_height_m'] <= bars['bank_height_max_m']
                and r['pose_error_m'] < bars['pose_error_max'])
        ok = ok and good
        print('%s  area %.5f (>= %.5f)  fill %.4f (>= %.4f)  contrast %6.2f (>= %.2f)  low %.4f (>= %.2f)'
              '  height %6.3f m (<= %.2f)  pose %.3f (< %.2f)  stripes %.5f  %s'
              % (s, r['bank_area_frac'], bars['bank_area_bar'], r['bank_fill'], bars['bank_fill_bar'],
                 r['bank_contrast_p50'], bars['contrast_bar'], r['low_share'], bars['low_share'],
                 r['bank_height_m'], bars['bank_height_max_m'], r['pose_error_m'], bars['pose_error_max'],
                 r['stripe_frac'], 'ok' if good else 'FAILED'))
    result = {'bars': bars, 'after': rows, 'ok': ok}
    with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
        json.dump(result, f, indent=1, sort_keys=True)
    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
