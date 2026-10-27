#!/usr/bin/env python3
"""Is the chlorine at zoom 30 at night ONE SOFT BANK, or a zebra of flat cards? Three takes before, three after.

WHY. g1 made the gas visible (0.13% of the play frame -> 3.25%) and the owner looked at its stills and said: "at
zoom 30 at night the play frame is a zebra of flat yellow-green cards, not a chlorine bank. The stripes run from
the tank across the trench and into the foreground. Draw one soft bank in one place, a volume a player can point
at." g1's probe could not see that fault: area and contrast both go UP when the whole field is painted with hard
lozenges. So this probe measures SHAPE, not reach, and its area number is a BAND - too small is invisible, too
big is paint.

It also fixes the second half of the owner's complaint. g1 shot the T1 band ONCE and packed that one still three
times, so "t1-after-1.jpg and t1-after-2.jpg are the same file" was literally true. The harness now shoots three
times, 9 frames apart (`GasBankStills.TakeGap`), each with its own frame-matched plate, so one capture dir holds
three real moments of the same cloud's boil and the bars have a spread to stand on.

HOW
  the takes    t1-before-1..3.jpg   three takes of the build at commit be5e6444fc, nothing changed
               t1-after-1..3.jpg    three takes after the look change
               t1-plate-1..3.jpg    one no-gas plate PER TAKE, from the same frame of the plate pass, so each
                                    take is subtracted against its own moment (g1 shared one plate for six)
  gas mask     a pixel whose RGB distance from its own plate is >= 40 (DIFF_BAR, g1's: a 20 lift on a night field
               is a haze, not a bank)
  the blob     only the single largest coherent patch of that mask (`largest_blob`, r2's)
  the numbers  bank_area_frac   blob px / 921,600. A BAND: [0.010, 0.090]. Below it the bank is invisible, above
                                it no single 14 m patch can account for the pixels - that is paint.
               stray_frac       masked px OUTSIDE the largest blob / all masked px. The zebra's stripes reach the
                                trench and the foreground; one bank leaves almost nothing outside itself.
               fill_frac        blob px / its bounding-box px. Stripes with gaps between them fill their box
                                poorly; a patch fills it.
               spread_m         the blob's bounding box across the GROUND in metres, at the blob's own median
                                row. "One place", in the metres the owner can point at.
               rim_px           softness. From the centroid, 180 rays: the last radius whose contrast is >= 40
                                and the first radius past it under 10; rim_px is the median gap. A card's edge is
                                a step of 1-2 px; a bank fades over many.
               low_share        share of the blob at or under 1.8 m, a man's head (r2's pooling check)
               height_frac      45th-percentile column height / the frame's world height there (not a wall)
               pose_error_m     out of the sidecar; a still from the wrong pose is worthless
               bank_contrast_p50  median lift inside the blob. A ROW here, not a bar: g1 already proved the lift,
                                and this pass may trade brightness for softness.
  the camera   rebuilt from each sidecar (r2's `ray` / `ground_distance` / `height_at`), so metres are metres.
               `--selftest` checks that maths against a hand calculation.

USAGE
    python probe.py --baseline <gas dir> --plate <plate dir>   pack the three before takes and their three
                                                               plates out of ONE capture dir each, measure, and
                                                               FREEZE the bars into baseline.json
    python probe.py --after <gas dir>                          pack the three after takes
    python probe.py                                            measure the three after takes against the bars
    python probe.py --selftest                                 the camera maths, no images

Exit 0 only when all three after takes clear every bar.
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
DIFF_BAR = 40               # RGB distance from the plate that the eye reads as a lift (g1's)
SOFT_BAR = 10               # under this a pixel is plate again: rim_px is the walk from DIFF_BAR down to here
BLOB_DILATE = 1             # largest_blob's bridge width; 2 chained the bank into terrain-edge noise (r2)
FRAME_PX = 1280 * 720       # 921,600: bank_area_frac's denominator
RAYS = 180                  # rim_px's rays out of the blob's centroid, one every 2 degrees
LOW_BAR = 0.30              # r2's pooling bars, unchanged
HEIGHT_BAR = 0.15
POSE_BAR = 0.5
AREA_FLOOR = 0.010          # the band and the shape bars; README "the bars, and where each number comes from"
AREA_CEILING = 0.090
STRAY_CEILING = 0.30
FILL_FLOOR = 0.45
SPREAD_CEILING_M = 35.0
RIM_FLOOR_PX = 3.0
SPREAD_K = 4                # a bar sits this many run-to-run spreads past the worst before take
BASELINE = os.path.join(HERE, 'baseline.json')

# The owner's ship-it call (decisions.md 2026-10-27, "Ship the gas bank as it stands: (a)"): these two numbers
# miss their frozen bar on this sheet and are accepted anyway, bounded one run-to-run spread past the worst of
# the three after takes the owner looked at, never past the limit the decision row itself quotes.
ACCEPTED = {
    'fill_frac': {'floor': 0.23,
                  'decision': 'decisions.md 2026-10-27, ship the gas bank as it stands: (a)'},
    'stray_frac': {'ceiling': 0.42,
                   'decision': 'decisions.md 2026-10-27, ship the gas bank as it stands: (a)'},
}


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

    # the acceptance table must be bounded: an accepted-row take passes, a take worse than either limit fails.
    bars = {'bank_area_bar': 0.010, 'bank_area_ceiling': 0.090, 'stray_bar': 0.30, 'fill_bar': 0.45,
            'spread_bar_m': 32.08, 'rim_bar_px': 3.0, 'low_share': 0.30, 'height_frac': 0.15,
            'pose_error_max': 0.5}

    def row(fill_frac, stray_frac):
        return {'bank_area_frac': 0.033, 'stray_frac': stray_frac, 'fill_frac': fill_frac, 'spread_m': 29.5,
                'rim_px': 3.0, 'low_share': 0.32, 'height_frac': 0.09, 'pose_error_m': 0.0}

    accepted_ok = verdict({'t1-after-1': row(0.235, 0.3975)}, bars)
    assert accepted_ok is True, 'the accepted values 0.235/0.3975 must pass, came back %r' % accepted_ok
    stray_too_high = verdict({'t1-after-1': row(0.30, 0.50)}, bars)
    assert stray_too_high is False, 'stray_frac 0.50 is past the 0.42 ceiling, must fail, came back %r' % stray_too_high
    fill_too_low = verdict({'t1-after-1': row(0.20, 0.38)}, bars)
    assert fill_too_low is False, 'fill_frac 0.20 is under the 0.23 floor, must fail, came back %r' % fill_too_low
    print('selftest ok: acceptance bounded (0.235/0.3975 pass, stray 0.50 and fill 0.20 each fail)')
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




def ground_point(cam, pitch, yaw, ground_y, ndc_x, ndc_y):
    """Where this pixel's ray crosses y = ground_y, as (x, z) in world metres (None if it never does)."""
    dx, dy, dz = ray(ndc_x, ndc_y, pitch, yaw)
    if dy >= -1e-6:
        return None
    t = (ground_y - cam[1]) / dy
    if t <= 0:
        return None
    return cam[0] + t * dx, cam[2] + t * dz


def rim_px(dist, blob, w, h):
    """Median softness of the blob's rim, in pixels, over RAYS rays out of its centroid.

    Along each ray: the LAST radius whose plate difference is still a lift (>= DIFF_BAR), then the FIRST radius
    past it that is plate again (< SOFT_BAR). The gap between them is how far the bank fades out in that
    direction. A flat card ends in a step of a pixel or two; a soft bank takes many.
    """
    cx = sum(i % w for i in blob) / float(len(blob))
    cy = sum(i // w for i in blob) / float(len(blob))
    reach = int(math.sqrt(len(blob))) * 3 + 40        # three blob-widths out: past any rim this bank can have
    gaps = []
    for k in range(RAYS):
        a = 2.0 * math.pi * k / RAYS
        ca, sa = math.cos(a), math.sin(a)
        last_lift, fade = -1, -1
        for r in range(1, reach):
            x, y = int(round(cx + ca * r)), int(round(cy + sa * r))
            if x < 0 or y < 0 or x >= w or y >= h:
                break
            d = dist[y * w + x]
            if d >= DIFF_BAR:
                last_lift, fade = r, -1
            elif last_lift > 0 and fade < 0 and d < SOFT_BAR:
                fade = r
        if last_lift > 0 and fade > last_lift:
            gaps.append(fade - last_lift)
    if not gaps:
        return 0.0
    gaps.sort()
    return round(float(gaps[len(gaps) // 2]), 2)


def measure(stem, plate_stem):
    """Every number for one take, off its own still and its OWN frame-matched plate."""
    img, side = load(stem)
    plate, _ = load(plate_stem)
    w, h = img.size
    dist = distances(img, plate)
    lit = [d >= DIFF_BAR for d in dist]
    mask = largest_blob(lit, w, h)
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ground = float(side.get('ground_y', 0.0))

    blob = [i for i, on in enumerate(mask) if on]
    all_lit = sum(1 for on in lit if on)
    out = {'bank_area_frac': round(len(blob) / float(FRAME_PX), 5),
           'bank_px': len(blob),
           'lit_px': all_lit,
           'pose_error_m': round(float(side.get('pose_error_m', 99.0)), 4)}
    if not blob:
        out.update({'bank_contrast_p50': 0.0, 'stray_frac': 1.0, 'fill_frac': 0.0, 'spread_m': 999.0,
                    'rim_px': 0.0, 'low_share': 0.0, 'height_frac': 1.0, 'bank_height_m': 0.0,
                    'bank_dist_m': 0.0, 'columns': 0})
        return out
    inside = sorted(dist[i] for i in blob)
    out['bank_contrast_p50'] = round(inside[len(inside) // 2], 2)
    out['stray_frac'] = round((all_lit - len(blob)) / float(all_lit), 4)

    xs = [i % w for i in blob]
    ys = [i // w for i in blob]
    x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
    out['fill_frac'] = round(len(blob) / float((x1 - x0 + 1) * (y1 - y0 + 1)), 4)
    out['bbox_px'] = [x0, y0, x1, y1]
    out['rim_px'] = rim_px(dist, blob, w, h)

    def ndc(px, py):
        return (2.0 * (px + 0.5) / w - 1.0, 1.0 - 2.0 * (py + 0.5) / h)

    # how wide the bank is ON THE GROUND: the bounding box's left and right edge, both taken at the blob's own
    # median row, projected onto the ground plane and measured apart. A number in metres, not in pixels.
    srt = sorted(ys)
    row = srt[len(srt) // 2]
    left = ground_point(cam, pitch, yaw, ground, *ndc(x0, row))
    right = ground_point(cam, pitch, yaw, ground, *ndc(x1, row))
    out['spread_m'] = (round(math.hypot(right[0] - left[0], right[1] - left[1]), 2)
                       if left and right else 999.0)

    by_column = {}
    for i in blob:
        by_column.setdefault(i % w, []).append(i // w)

    MIN_COLUMN_PX = 8
    columns = {px: max(cys) for px, cys in by_column.items() if len(cys) >= MIN_COLUMN_PX}

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
    for px, hs in col_heights.items():
        hs.sort()
        heights.append(hs[min(len(hs) - 1, int(round(0.45 * (len(hs) - 1))))])
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


TAKES = ('', '-b', '-c')    # GasBankStills' three T1 stems inside one capture dir


def arg_dirs(flag, count):
    """The `count` directory arguments after `flag`, or None when the flag is absent."""
    if flag not in sys.argv:
        return None
    at = sys.argv.index(flag) + 1
    got = sys.argv[at:at + count]
    if len(got) < count or any(g.startswith('--') for g in got):
        raise SystemExit('%s wants %d directories' % (flag, count))
    return got


def pack_three(src, kind):
    """Pack one capture dir's three T1 takes in here as t1-<kind>-1..3 (or t1-plate-1..3 for a plate dir)."""
    out = []
    for k, suffix in enumerate(TAKES, start=1):
        still = 'gas-t1' + suffix + ('-plate' if kind == 'plate' else '')
        out.append(pack(src, still, 't1-%s-%d' % (kind, k)))
    return out


def rise_bar(values, floor):
    """A bar a number must CLIMB: four run-to-run spreads above the worst before take, never under `floor`."""
    s = max(values) - min(values)
    return round(max(floor, max(values) + SPREAD_K * s), 5)


def fall_bar(values, ceiling):
    """A bar a number must DROP under: four spreads below the best before take, never over `ceiling`."""
    s = max(values) - min(values)
    return round(min(ceiling, min(values) - SPREAD_K * s), 5)


ROW = ('%s  area %.5f  stray %.4f  fill %.4f  spread %6.2f m  rim %5.2f px  low %.4f  height %.4f'
       '  contrast %6.2f  pose %.3f')


def show(stem, r):
    print(ROW % (stem, r['bank_area_frac'], r['stray_frac'], r['fill_frac'], r['spread_m'], r['rim_px'],
                 r['low_share'], r['height_frac'], r['bank_contrast_p50'], r['pose_error_m']))


def do_baseline(src, plate_dir):
    pack_three(plate_dir, 'plate')
    pack_three(src, 'before')
    rows = {'t1-before-%d' % k: measure('t1-before-%d' % k, 't1-plate-%d' % k) for k in (1, 2, 3)}
    keys = sorted(rows)

    def col(name):
        return [rows[s][name] for s in keys]

    out = {
        'built_from_commit': 'be5e6444fc',
        'diff_bar': DIFF_BAR,
        'before': rows,
        'spread': {name: round(max(col(name)) - min(col(name)), 5)
                   for name in ('bank_area_frac', 'stray_frac', 'fill_frac', 'spread_m', 'rim_px')},
        # bank_area_frac is the one number that is a BAND and not a direction, so the spread rule does not apply
        # to it: the fault this pass fixes is shape, and a bank that keeps the area it has while becoming one soft
        # patch is a pass. Pushed up by four spreads the band would be 0.087..0.090 - a window 0.003 wide that can
        # only be hit by painting MORE field, which is the owner's complaint. The band's two ends are absolute and
        # are the plan's: 0.010 (a 96 px square, the smallest patch the eye finds) to 0.090 (a 14 m patch's own
        # footprint at 50 m under a 25 deg pitch; more than that is paint, not a bank).
        'bars': {'bank_area_bar': AREA_FLOOR,
                 'bank_area_ceiling': AREA_CEILING,
                 'stray_bar': fall_bar(col('stray_frac'), STRAY_CEILING),
                 'fill_bar': rise_bar(col('fill_frac'), FILL_FLOOR),
                 'spread_bar_m': fall_bar(col('spread_m'), SPREAD_CEILING_M),
                 'rim_bar_px': rise_bar(col('rim_px'), RIM_FLOOR_PX),
                 'low_share': LOW_BAR, 'height_frac': HEIGHT_BAR, 'pose_error_max': POSE_BAR},
        'frozen': 'computed once, from the three before takes of commit be5e6444fc; never recomputed',
    }
    with open(BASELINE, 'w', encoding='utf-8') as f:
        json.dump(out, f, indent=1, sort_keys=True)
    for s in keys:
        show(s, rows[s])
    print('spread  ' + '  '.join('%s %.5f' % (k, v) for k, v in sorted(out['spread'].items())))
    print('FROZEN bars  ' + '  '.join('%s %s' % (k, v) for k, v in sorted(out['bars'].items())))
    return 0


def verdict(rows, bars):
    """True when every take clears every bar, or clears an ACCEPTED limit where it misses; prints a line per
    take, plus one line per acceptance actually used naming the bar, the limit and the decision."""
    ok = True
    used = set()

    def clears(name, value, bar_ok):
        """`bar_ok`: did it clear the frozen bar; if not, fall back to the ACCEPTED limit, if any."""
        if bar_ok:
            return True
        limit = ACCEPTED.get(name)
        if limit is None:
            return False
        if 'floor' in limit and value >= limit['floor']:
            used.add(name)
            return True
        if 'ceiling' in limit and value <= limit['ceiling']:
            used.add(name)
            return True
        return False

    for s in sorted(rows):
        r = rows[s]
        area_ok = bars['bank_area_bar'] <= r['bank_area_frac'] <= bars['bank_area_ceiling']
        stray_ok = clears('stray_frac', r['stray_frac'], r['stray_frac'] <= bars['stray_bar'])
        fill_ok = clears('fill_frac', r['fill_frac'], r['fill_frac'] >= bars['fill_bar'])
        spread_ok = r['spread_m'] <= bars['spread_bar_m']
        rim_ok = r['rim_px'] >= bars['rim_bar_px']
        low_ok = r['low_share'] >= bars['low_share']
        height_ok = r['height_frac'] <= bars['height_frac']
        pose_ok = r['pose_error_m'] < bars['pose_error_max']
        good = (area_ok and stray_ok and fill_ok and spread_ok and rim_ok and low_ok and height_ok and pose_ok)
        ok = ok and good

        def tag(name, cleared):
            if cleared and name in used:
                return 'accepted'
            return 'ok' if cleared else 'FAILED'

        print(('%s  area %.5f (%.5f..%.5f)  stray %.4f (<= %.4f) [%s]  fill %.4f (>= %.4f) [%s]'
               '  spread %.2f m (<= %.2f)  rim %.2f px (>= %.2f)  low %.4f  height %.4f  pose %.3f  %s')
              % (s, r['bank_area_frac'], bars['bank_area_bar'], bars['bank_area_ceiling'],
                 r['stray_frac'], bars['stray_bar'], tag('stray_frac', stray_ok),
                 r['fill_frac'], bars['fill_bar'], tag('fill_frac', fill_ok),
                 r['spread_m'], bars['spread_bar_m'], r['rim_px'], bars['rim_bar_px'],
                 r['low_share'], r['height_frac'], r['pose_error_m'], 'ok' if good else 'FAILED'))

    for name in sorted(used):
        limit = ACCEPTED[name]
        bound = 'floor %.2f' % limit['floor'] if 'floor' in limit else 'ceiling %.2f' % limit['ceiling']
        frozen = bars['fill_bar'] if name == 'fill_frac' else bars['stray_bar']
        print('accepted  %s  frozen bar %.4f  accepted %s  %s' % (name, frozen, bound, limit['decision']))
    return ok


def main():
    if '--selftest' in sys.argv:
        return selftest()
    base = arg_dirs('--baseline', 1)
    if base is not None:
        plate = arg_dirs('--plate', 1)
        if plate is None:
            raise SystemExit('--baseline also needs --plate <dir>')
        return do_baseline(base[0], plate[0])
    after = arg_dirs('--after', 1)
    if after is not None:
        print('packed ' + ', '.join(pack_three(after[0], 'after')))
        plate = arg_dirs('--plate', 1)
        if plate is not None:
            print('packed ' + ', '.join(pack_three(plate[0], 'plate')))

    if not os.path.exists(BASELINE):
        print('no baseline.json: run --baseline <gas dir> --plate <plate dir> first')
        return 1
    with open(BASELINE, encoding='utf-8') as f:
        bars = json.load(f)['bars']
    stems = ['t1-after-%d' % k for k in (1, 2, 3)]
    missing = [s for s in stems + ['t1-plate-%d' % k for k in (1, 2, 3)]
               if not os.path.exists(os.path.join(HERE, s + '.jpg'))]
    if missing:
        print('missing stills: ' + ', '.join(missing))
        return 1

    rows = {s: measure(s, 't1-plate-%d' % int(s[-1])) for s in stems}
    ok = verdict(rows, bars)
    with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
        json.dump({'bars': bars, 'accepted': ACCEPTED, 'after': rows, 'ok': ok}, f, indent=1, sort_keys=True)
    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
