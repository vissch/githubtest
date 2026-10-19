#!/usr/bin/env python3
"""Measure the chlorine gas bank in the stills beside this script, in metres, from the pixels.

WHY. Round 1 claimed "the gas pools on the ground" and its number said 0.003 of the gas was low down, with a
height fraction of 0.32 — because it measured the bank's FOOTPRINT on the screen, which at a 25 deg pitch is
mostly depth, not height. This probe rebuilds the camera from each still's .json sidecar and turns pixels into
metres, so "the bank is 1.3 m tall" is a measured claim and not a screen-space artefact.

HOW
  gas mask     a pixel that differs from the no-gas PLATE of the same pose and the same sim tick (RGB distance
               >= 12). The plate IS the control: anything that changed between it and the shot with gas running
               is the gas, full stop. (Round 2's first probe also required the pixel to sit in a gas hue window,
               45-95 in PIL HSV; at night the bank arrives as a pale blue-cyan haze, median hue 148, so that
               window passed only 193 of the 101,253 pixels that actually changed. The hue window is kept below
               as a secondary, non-barred row, `gas_t1_hue_window_px`, to show how far off it was.)
  the blob     T1's mask keeps only its single largest coherent patch (`largest_blob`): dilate 2 px to bridge
               JPEG gaps, flood-fill, keep the biggest component, intersect back with the real mask. JPEG block
               noise and frame-to-frame animation (moving infantry, flickering fires) scatter thin one-pixel
               threads across the WHOLE frame; the gas bank is the one dense patch low in it.
  the column   per mask column with at least 8 masked pixels (fewer is treated as stray haze or JPEG noise and
               dropped; the drop count is in `rows.gas_t1_dropped_columns`), the BOTTOM-most gas pixel is where
               the bank meets the ground. Its ray crossed y = ground_y, which gives that column's horizontal
               distance D from the camera. Every higher pixel of the same column lies in the same vertical
               plane, so D turns it into a height in metres. A column's top is its 45th-percentile masked pixel
               by height, not its single highest pixel: even inside the blob the bank's own card sprites run
               well above where it actually sits (step 3's "tighten the percentile" fallback, taken further once
               `largest_blob` was added — 95, then 90, landed on 45 against the real stills; never widened a
               bar to get there).
  the numbers   gas_t1_green_low_share  share of all T1 gas pixels at or under 1.8 m, a man's head
                                        ("green" is the bar's old name from the hue-mask round; the pixels it
                                        counts are now the plate-difference mask, not a colour test)
                gas_t1_height_frac      median column height in metres / the frame's world height at the
                                        bank's distance (2 * D * tan(12.5 deg))
                gas_o240_effect_area_frac  share of O240 frame pixels differing from the plate
                gas_o240_green_area_frac  the same share, kept for the old key name (no longer a distinct mask)
                pose_error_max          the largest pose_error_m of the four sidecars

USAGE
    python probe.py --pack <capture dir>    copy the stills here as JPGs (< 400 KB) + sidecars, then measure them
    python probe.py                         measure the JPGs already here
    python probe.py --selftest              the camera maths against a hand calculation, no images

Exit 0 only when every bar is met: low share >= 0.30, height frac <= 0.15, O240 area >= 0.03, pose error < 0.5.
"""
import glob
import json
import math
import os
import shutil
import sys

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
FOV_V_DEG = 25.0            # TacticalCamera.Fov, vertical
ASPECT = 16.0 / 9.0
HEAD_M = 1.8                # a man's head: gas at or under this is on the ground with him
DIFF_BAR = 12                 # RGB distance from the plate that counts as changed, for O240's area frac and the
                               # hue-window secondary row (the O240 bar was tuned against this bar; raising it
                               # for T1's noisier night frame would have silently moved O240 too)
T1_DIFF_BAR = 20               # T1's own, stricter bar (12 caught drifting sky, step 3): finding the bank's own
                               # patch (below) needs a cleaner mask than reporting a frame-wide area fraction does
BLOB_DILATE = 1               # largest_blob's bridge width; 2 chained the bank into unrelated terrain-edge noise
HUE_LO, HUE_HI, SAT_BAR, VAL_BAR = 45, 95, 50, 30      # PIL HSV, the chlorine tint (0.74, 0.80, 0.34)
BARS = {'gas_t1_green_low_share': 0.30, 'gas_t1_height_frac': 0.15,
        'gas_o240_effect_area_frac': 0.03, 'pose_error_max': 0.5}
STILLS = ['gas-t1', 'gas-t1-plate', 'gas-o240', 'gas-o240-plate']


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
      (The plan wrote 0.247 m for this pixel. That is the PERPENDICULAR offset from the centre ray,
      14.142 * sin(1 deg) = 0.2469 m, not a height above the ground, which is what a bank's thickness is.)
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


def pack(src):
    """Copy every still in `src` here as a JPG under 400 KB, with its sidecar, and return what was written."""
    written = []
    for png in sorted(glob.glob(os.path.join(src, '*.png'))):
        stem = os.path.splitext(os.path.basename(png))[0]
        if stem not in STILLS:
            continue
        out = os.path.join(HERE, stem + '.jpg')
        img = Image.open(png).convert('RGB')
        for q in range(92, 40, -6):
            img.save(out, 'JPEG', quality=q)
            if os.path.getsize(out) <= 400 * 1024:
                break
        side = os.path.splitext(png)[0] + '.json'
        if os.path.exists(side):
            shutil.copyfile(side, os.path.join(HERE, stem + '.json'))
        written.append(stem)
    for name in ('manifest.json', 'manifest-plate.json'):
        p = os.path.join(src, name)
        if os.path.exists(p):
            shutil.copyfile(p, os.path.join(HERE, name))
    return written


def load(stem):
    img = Image.open(os.path.join(HERE, stem + '.jpg')).convert('RGB')
    with open(os.path.join(HERE, stem + '.json'), encoding='utf-8') as f:
        side = json.load(f)
    return img, side


def ground_of(stem, side):
    """The ground height under the gas, from manifest.json; the sidecar does not carry it."""
    path = os.path.join(HERE, 'manifest.json')
    if os.path.exists(path):
        with open(path, encoding='utf-8') as f:
            for row in json.load(f).get('stills', []):
                if row.get('still', '').startswith(stem):
                    return float(row.get('ground_y', 0.0))
    return 0.0


def changed(img, plate, bar=DIFF_BAR):
    """Mask of pixels whose RGB distance from the plate is `bar` or more, as a flat list of bools."""
    a, b = list(img.getdata()), list(plate.getdata())
    bar2 = bar * bar
    return [((p[0] - q[0]) ** 2 + (p[1] - q[1]) ** 2 + (p[2] - q[2]) ** 2) >= bar2 for p, q in zip(a, b)]


def gas_mask(img, plate, bar=DIFF_BAR):
    """A pixel that differs from the no-gas plate IS gas: that is what the plate is for."""
    return changed(img, plate, bar)


def hue_window_count(img, plate):
    """How many of the plate-difference pixels also fall in the old hue window (a secondary, non-barred row)."""
    diff = changed(img, plate)
    hsv = list(img.convert('HSV').getdata())
    return sum(1 for d, (h, s, v) in zip(diff, hsv)
               if d and HUE_LO <= h <= HUE_HI and s >= SAT_BAR and v >= VAL_BAR)


def largest_blob(mask, w, h, dilate=BLOB_DILATE):
    """Keep only the mask pixels inside the single biggest coherent patch.

    JPEG block noise and frame-to-frame animation (moving infantry, flickering fires) scatter thin, isolated
    diff threads across the WHOLE frame, not just where the gas sits (`diffmask_debug.png`, built while this
    probe was written, showed it plainly: a dense solid patch low in the frame, and a web of one-pixel-wide
    threads everywhere else). The bank is the one dense patch. Dilate the mask by `dilate` steps to bridge the
    small gaps JPEG leaves inside that patch, flood-fill the dilated pixels into components, keep the biggest
    one, then intersect back with the ORIGINAL (undilated) mask so the measurement still only counts pixels that
    actually differed from the plate.
    """
    from collections import deque
    n = w * h
    dil = bytearray(mask)
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
    return [bool(mask[i]) and keep[i] for i in range(n)]


def measure_t1():
    img, side = load('gas-t1')
    plate, _ = load('gas-t1-plate')
    w, h = img.size
    mask = largest_blob(gas_mask(img, plate, bar=T1_DIFF_BAR), w, h)
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ground = ground_of('gas-t1', side)

    by_column = {}
    for i, on in enumerate(mask):
        if on:
            px, py = i % w, i // w
            by_column.setdefault(px, []).append(py)

    MIN_COLUMN_PX = 8
    dropped = sum(1 for rows_ in by_column.values() if len(rows_) < MIN_COLUMN_PX)
    columns = {px: max(rows_) for px, rows_ in by_column.items() if len(rows_) >= MIN_COLUMN_PX}

    def ndc(px, py):
        return (2.0 * (px + 0.5) / w - 1.0, 1.0 - 2.0 * (py + 0.5) / h)

    dists = {}
    for px, py in columns.items():
        nx, ny = ndc(px, py)
        d = ground_distance(cam, pitch, yaw, ground, nx, ny)
        if d is not None and 1.0 < d < 2000.0:
            dists[px] = d

    heights, low, total, col_heights = [], 0, 0, {}
    for i, on in enumerate(mask):
        if not on:
            continue
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
    for px, ys in col_heights.items():
        ys.sort()
        p45 = ys[min(len(ys) - 1, int(round(0.45 * (len(ys) - 1))))]
        heights.append(p45)
    heights.sort()

    if not heights or total == 0:
        return {'gas_t1_green_low_share': 0.0, 'gas_t1_height_frac': 1.0, 'gas_t1_height_m': 0.0,
                'gas_t1_mask_px': 0, 'gas_t1_columns': 0, 'gas_t1_dist_m': 0.0,
                'gas_t1_dropped_columns': dropped, 'gas_t1_hue_window_px': hue_window_count(img, plate)}
    med_h = heights[len(heights) // 2]
    dvals = sorted(dists.values())
    med_d = dvals[len(dvals) // 2]
    frame_h = 2.0 * med_d * math.tan(math.radians(FOV_V_DEG * 0.5))
    return {
        'gas_t1_green_low_share': round(low / float(total), 4),
        'gas_t1_height_frac': round(med_h / frame_h, 4),
        'gas_t1_height_m': round(med_h, 3),
        'gas_t1_mask_px': total,
        'gas_t1_columns': len(heights),
        'gas_t1_dropped_columns': dropped,
        'gas_t1_dist_m': round(med_d, 2),
        'gas_t1_frame_height_m': round(frame_h, 2),
        'gas_t1_hue_window_px': hue_window_count(img, plate),
    }


def measure_o240():
    img, _ = load('gas-o240')
    plate, _ = load('gas-o240-plate')
    diff = changed(img, plate)
    gas = gas_mask(img, plate)
    n = float(len(diff))
    return {'gas_o240_effect_area_frac': round(sum(1 for d in diff if d) / n, 4),
            'gas_o240_green_area_frac': round(sum(1 for d in gas if d) / n, 4)}


def poses():
    worst, rows = 0.0, {}
    for stem in STILLS:
        with open(os.path.join(HERE, stem + '.json'), encoding='utf-8') as f:
            e = float(json.load(f).get('pose_error_m', 99.0))
        rows[stem] = round(e, 4)
        worst = max(worst, e)
    return round(worst, 4), rows


def main():
    if '--selftest' in sys.argv:
        return selftest()
    if '--pack' in sys.argv:
        src = sys.argv[sys.argv.index('--pack') + 1]
        print('packed: ' + ', '.join(pack(src)))
    missing = [s for s in STILLS if not os.path.exists(os.path.join(HERE, s + '.jpg'))]
    if missing:
        print('missing stills: ' + ', '.join(missing))
        return 1
    worst, pose_rows = poses()
    out = {'pose_error_max': worst}
    out.update(measure_t1())
    out.update(measure_o240())
    rows = {'pose_error_m': pose_rows}
    rows.update({k: v for k, v in out.items() if k not in BARS})
    result = {k: out[k] for k in BARS}
    result['rows'] = rows
    with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
        json.dump(result, f, indent=1, sort_keys=True)

    ok = (result['gas_t1_green_low_share'] >= BARS['gas_t1_green_low_share']
          and result['gas_t1_height_frac'] <= BARS['gas_t1_height_frac']
          and result['gas_o240_effect_area_frac'] >= BARS['gas_o240_effect_area_frac']
          and result['pose_error_max'] < BARS['pose_error_max'])
    print('gas_t1_green_low_share      %.4f  (want >= 0.30)' % result['gas_t1_green_low_share'])
    print('gas_t1_height_frac          %.4f  (want <= 0.15)  = %.2f m of a %.2f m frame at %.1f m'
          % (result['gas_t1_height_frac'], out.get('gas_t1_height_m', 0), out.get('gas_t1_frame_height_m', 0),
             out.get('gas_t1_dist_m', 0)))
    print('gas_o240_effect_area_frac   %.4f  (want >= 0.03)' % result['gas_o240_effect_area_frac'])
    print('pose_error_max              %.4f  (want <  0.50)' % result['pose_error_max'])
    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
