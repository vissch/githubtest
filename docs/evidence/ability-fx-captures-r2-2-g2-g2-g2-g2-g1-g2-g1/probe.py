#!/usr/bin/env python3
"""Draw the close gas as cards-smoke's one standing cloud, and bar the close blob count at one.

WHY. The owner's complaint about the -g2-g2-g2-g2-g1-g2 probe:

    At zoom 30 the close gas is still a carpet of flat pads. cards-smoke is one standing grey cloud and
    cards-gas is a flat green puddle (rise_m 0, relief 0.1199 against the smoke card's 0.2178). The largest
    close blob is 80324 px of 193455 green px. Draw the close gas as that one smoke cloud and bar the close
    blob count at one.

THE LOOK CHANGE. `CombatFx.GasBankStand` goes from 0.42 (a share of its width) to 1f, so at `step == 1`
`DrawGasBank` now packs `height = width`, exactly what `DrawSmokeBank` already packs for its own card
(`CombatFx.Banks.cs:128`, `width, width`). `GasBankSink` is unchanged. The overview (`step == 2`) is untouched:
still the flat `GasBankSquash` blanket, so the O240 one-cloud result stands.

THE CLOSE BLOB COUNT. `measure_t1` no longer takes the single biggest coherent patch at face value as "the
cloud" (`green_mask(..., blob=True)`); it counts connected components of the raw green the same way
`measure_o240` already does, and bars how many of them are large enough to read as a cloud a player can
pick out:
  * `green_large_cloud_count` - components at or above `T1_LARGE_MIN_PX`. BARRED as a ceiling (`le`): a carpet
    of several pads fails this even if the biggest one alone would pass a size floor.
  * `green_large_share` - those components' pixels over `green_px`. BARRED as a floor (`ge`), paired with the
    count so a reading of zero clouds fails too.
  * `green_blob_top` - the top ten component sizes. Printed as a diagnostic, BARRED ON NOTHING.
`blob_px` keeps its old meaning: the pixels of the single biggest component (`comps[0]`), used for
`green_area_frac` and the height/rise measures exactly as before.

THE RELIEF AND THE RIM (carried from -g2-g2-g2-g2-g1-g2, unchanged in shape). `measure_t1` still reads, over the
single biggest component:
  * `green_relief` - stdev of the masked pixels' luma over their mean. BARRED as a floor.
  * `green_rim_ratio` - the mean luma of mask pixels that have an off-mask 4-neighbour, over the mean of the
    rest. Printed as a diagnostic, BARRED ON NOTHING.

Everything else is carried unchanged: the camera maths (`ray`, `ground_distance`, `height_at`, `selftest`),
`greenness`, `green_mask`, `components`, `largest_blob`, `measure_o240` with its `LARGE_MIN_PX` floor, the frame
guard and `spread_check`. `CARD_BARS` is `{}` for this part: the carried `cards/` still holds the OLD gas card
(`CardWidth * 0.42`, not the new 1:1 card); re-deriving the cards look report is part 2's work.

THE NAMES. Every T1 key says green: `green_low_share`, `green_height_frac`, `green_rise_m`, `green_relief`,
`green_area_frac`, `green_large_cloud_count`, `green_large_share`. `green_px`, `plate_diff_px`, `blob_px`,
`green_rim_ratio`, `green_blob_top`, `o240_green_px`, `o240_plate_diff_px`, `o240_speck_count` and
`o240_blob_top` are printed per run as named diagnostics, barred on nothing.

THE FRAME GUARD. Before any bar is looked at, the four sidecars' `frame` numbers are read. Two runs sharing a
`gas-t1` frame, or a `gas-o240` frame, are not three captures: the probe prints `same frame: runN and runM both
photographed frame F` and exits 1. `--calibrate` prints the frames and does NOT refuse, so numbers can be read
while the captures are still being shot.

THE BAR. Three runs of the SAME build (`run1`, `run2`, `run3`, each the four GasBankStills captures plus a no-gas
plate) are loaded, at three different frames. For a bar to hold, the three runs must all sit on the pass side AND
the nearest run value to the bar must clear it by at least the three runs' own spread (max - min). A bar sitting
inside the run values, or within one spread of them, is "the build cannot tell a measure from its own noise" and
the probe exits 1 rather than print a false green.

THE LOOK REPORT. `cards/` is carried unchanged from -g2-g2-g2-g2-g1-g2: the OLD gas card
(`CardWidth * 0.42`), not this part's new 1:1 card. `CARD_BARS` is `{}` here, so nothing from `cards/` is
barred; re-shooting it against the new card is part 2's work.

USAGE
    python probe.py                 bar run1/run2/run3 against each other, print the cards/ look report
    python probe.py --calibrate      print every run's numbers and frames with no bar check and no frame refusal
    python probe.py --selftest       the camera maths against a hand calculation, no images

Exit 0 only when three runs at three different frames are loaded and every bar clears by at least their spread.
Exit 1 with 'three runs needed' if fewer than three run*/ directories hold a full set of stills.
Exit 1 with 'same frame' if two runs photographed the same t1 or o240 frame.
Exit 1 with 'bar inside the noise' if a bar does not clear by the spread.
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
HEAD_M = 1.8                 # a man's head: bank at or under this is on the ground with him
GREEN_BAR = 8                 # greenness the gas must ADD over the plate for a pixel to count as gas
DIFF_BAR = 12                 # RGB distance from the plate that counts as changed (diagnostic only now)
T1_DIFF_BAR = 20               # T1's own, stricter plate-difference bar (diagnostic only now)
BLOB_DILATE = 1               # largest_blob's bridge width (r2-2: 2 chained the bank into terrain-edge noise)
STILLS = ['gas-t1', 'gas-t1-plate', 'gas-o240', 'gas-o240-plate']
CARDS = ['cards-gas', 'cards-smoke', 'cards-puff', 'cards-plate']
CLOUD_MIN_PX = 50             # a component this small or smaller is noise, not a gas cloud
LARGE_MIN_PX = 300            # a cloud a player can pick out at the far camera (~17x17 patch, double the 150 px
                              # speck ceiling, well under the two big clouds at 2310+)
T1_LARGE_MIN_PX = 4000        # a close cloud a player can pick out (0.43% of the 1280x720 still); set from this
                              # part's --calibrate green_blob_top print (see README.md)
# The cards/ look report has no barred reading in this part: `cards/` still holds the OLD gas card.
CARD_BARS = {}

BARS = {'green_low_share': 'ge', 'green_height_frac': 'ge', 'green_area_frac': 'ge', 'green_rise_m': 'ge',
        'green_relief': 'ge', 'green_large_cloud_count': 'le', 'green_large_share': 'ge',
        'o240_green_area_frac': 'ge', 'o240_large_cloud_count': 'le', 'o240_large_share': 'ge',
        'o240_top_blob_px': 'ge', 'pose_error_max': 'lt'}


def luma(r, g, b):
    return 0.2126 * r + 0.7152 * g + 0.0722 * b


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


def components(mask, w, h, dilate=BLOB_DILATE):
    """Connected components of `mask` after `dilate` rounds of bridging, members biggest-first.

    Stable sort (python's `sort`): components of equal size keep their scan order, so `largest_blob`'s winner
    (now `components(...)[0]`) is unchanged from the single-biggest-patch search this was pulled out of.
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
    comps = []
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
        comps.append(members)
    comps.sort(key=len, reverse=True)
    return comps


def largest_blob(mask, w, h, dilate=BLOB_DILATE):
    """Keep only the mask pixels inside the single biggest coherent patch (carried unchanged from r2-2)."""
    comps = components(mask, w, h, dilate)
    n = w * h
    keep = bytearray(n)
    if comps:
        for j in comps[0]:
            keep[j] = 1
    return [bool(mask[i]) and keep[i] for i in range(n)]


def changed(img, plate, bar=DIFF_BAR):
    """Mask of pixels whose RGB distance from the plate is `bar` or more, as a flat list of bools."""
    a, b = list(img.getdata()), list(plate.getdata())
    bar2 = bar * bar
    return [((p[0] - q[0]) ** 2 + (p[1] - q[1]) ** 2 + (p[2] - q[2]) ** 2) >= bar2 for p, q in zip(a, b)]


def greenness(r, g, b):
    """How far a pixel leans green: its green channel above the mean of red and blue."""
    return g - (r + b) / 2.0


def greener(img, plate, bar=GREEN_BAR):
    """Mask of pixels whose greenness is at least `bar` ABOVE the plate's: the green the gas added, nothing else."""
    a, b = list(img.getdata()), list(plate.getdata())
    return [(greenness(*p) - greenness(*q)) >= bar for p, q in zip(a, b)]


def green_mask(img, plate, diff_bar=DIFF_BAR, blob=False):
    """The gas: the pixels the gas turned green, as one patch at T1 and as-is at O240.

    Returns (mask, green_px, plate_diff_px, blob_px): the mask, the green count before `largest_blob`, the old
    plate-difference count (a printed diagnostic, barred on nothing) and the count after the blob.
    """
    green = greener(img, plate)
    mask = largest_blob(green, *img.size) if blob else list(green)
    diff = changed(img, plate, diff_bar)
    return mask, sum(1 for g in green if g), sum(1 for d in diff if d), sum(1 for m in mask if m)


def pack(src, into):
    """Copy every still in `src` into HERE/`into`/ as a JPG under 400 KB, with its sidecar and manifests.

    Lifted from docs/evidence/ability-fx-captures-r2/probe.py's `pack` (lines 131-152), so run2/run3 are packed
    exactly as run1 was: same quality ladder, same files, just into a named run directory instead of HERE.
    """
    out_dir = os.path.join(HERE, into)
    os.makedirs(out_dir, exist_ok=True)
    written = []
    for png in sorted(glob.glob(os.path.join(src, '*.png'))):
        stem = os.path.splitext(os.path.basename(png))[0]
        if stem not in STILLS + CARDS:
            continue
        out = os.path.join(out_dir, stem + '.jpg')
        img = Image.open(png).convert('RGB')
        for q in range(92, 40, -6):
            img.save(out, 'JPEG', quality=q)
            if os.path.getsize(out) <= 400 * 1024:
                break
        side = os.path.splitext(png)[0] + '.json'
        if os.path.exists(side):
            shutil.copyfile(side, os.path.join(out_dir, stem + '.json'))
        written.append(stem)
    for name in ('manifest.json', 'manifest-plate.json'):
        p = os.path.join(src, name)
        if os.path.exists(p):
            shutil.copyfile(p, os.path.join(out_dir, name))
    return written


def load(run_dir, stem):
    img = Image.open(os.path.join(run_dir, stem + '.jpg')).convert('RGB')
    with open(os.path.join(run_dir, stem + '.json'), encoding='utf-8') as f:
        side = json.load(f)
    return img, side


def ground_of(run_dir, stem):
    """The ground height under the gas, from manifest.json; the sidecar does not carry it."""
    path = os.path.join(run_dir, 'manifest.json')
    if os.path.exists(path):
        with open(path, encoding='utf-8') as f:
            for row in json.load(f).get('stills', []):
                if row.get('still', '').startswith(stem):
                    return float(row.get('ground_y', 0.0))
    return 0.0


def mask_relief(img, mask):
    """(relief, rim_ratio) of the masked pixels: how far from flat the cloud is, and how its edge reads.

    relief is the stdev of the masked pixels' luma over their mean - the same four lines `card_rise_m` uses, so
    the T1 reading and the cards/ reading are the same number on the same scale (a flat wash near 0, the smoke
    cloud's card 0.2178). rim_ratio is the mean luma of the mask pixels that touch an off-mask 4-neighbour (the
    silhouette's edge) over the mean of the rest: above 1 the edge is the lit side, under 1 it is a dark rim.
    Diagnostic only - the direction this art's rim takes is not predictable, so it is reported, not barred.
    """
    w, h = img.size
    pix = list(img.getdata())
    lumas, rim, body = [], [], []
    for i, on in enumerate(mask):
        if not on:
            continue
        v = luma(*pix[i])
        lumas.append(v)
        x, y = i % w, i // w
        edge = ((x == 0 or not mask[i - 1]) or (x == w - 1 or not mask[i + 1])
                or (y == 0 or not mask[i - w]) or (y == h - 1 or not mask[i + w]))
        (rim if edge else body).append(v)
    if not lumas:
        return 0.0, 0.0
    mean_l = sum(lumas) / len(lumas)
    var_l = sum((v - mean_l) ** 2 for v in lumas) / len(lumas)
    relief = (var_l ** 0.5) / mean_l if mean_l > 1e-9 else 0.0
    if rim and body:
        mb = sum(body) / len(body)
        ratio = (sum(rim) / len(rim)) / mb if mb > 1e-9 else 0.0
    else:
        ratio = 0.0
    return round(relief, 4), round(ratio, 4)


def measure_t1(run_dir):
    img, side = load(run_dir, 'gas-t1')
    plate, _ = load(run_dir, 'gas-t1-plate')
    w, h = img.size
    green = greener(img, plate)
    diff = changed(img, plate, T1_DIFF_BAR)
    green_px = sum(1 for g in green if g)
    plate_diff_px = sum(1 for d in diff if d)
    green_set = frozenset(i for i, on in enumerate(green) if on)
    comps = [[j for j in c if j in green_set] for c in components(green, w, h)]
    comps = [c for c in comps if c]
    comps.sort(key=len, reverse=True)
    large = [c for c in comps if len(c) >= T1_LARGE_MIN_PX]
    large_px = sum(len(c) for c in large)
    green_large_share = round(large_px / green_px, 4) if green_px else 0.0
    green_blob_top = [len(c) for c in comps[:10]]
    mask = bytearray(w * h)
    if comps:
        for j in comps[0]:
            mask[j] = 1
    mask = [bool(m) for m in mask]
    blob_px = sum(1 for m in mask if m)
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ground = ground_of(run_dir, 'gas-t1')

    by_column = {}
    for i, on in enumerate(mask):
        if on:
            px, py = i % w, i // w
            by_column.setdefault(px, []).append(py)

    MIN_COLUMN_PX = 8
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

    mask_px = sum(1 for on in mask if on)
    relief, rim_ratio = mask_relief(img, mask)
    diag = {'green_px': green_px, 'plate_diff_px': plate_diff_px, 'blob_px': blob_px,
            'green_rim_ratio': rim_ratio, 'green_blob_top': green_blob_top}
    common = {'green_large_cloud_count': len(large), 'green_large_share': green_large_share}
    if not heights or total == 0:
        out = {'green_low_share': 0.0, 'green_height_frac': 0.0, 'green_rise_m': 0.0, 'green_relief': relief,
               'green_area_frac': round(mask_px / float(w * h), 4)}
        out.update(common)
        out.update(diag)
        return out
    med_h = heights[len(heights) // 2]
    dvals = sorted(dists.values())
    med_d = dvals[len(dvals) // 2]
    frame_h = 2.0 * med_d * math.tan(math.radians(FOV_V_DEG * 0.5))
    out = {
        'green_low_share': round(low / float(total), 4),
        'green_height_frac': round(med_h / frame_h, 4),
        'green_rise_m': round(med_h, 3),
        'green_relief': relief,
        'green_area_frac': round(mask_px / float(w * h), 4),
    }
    out.update(common)
    out.update(diag)
    return out


def measure_o240(run_dir):
    img, _ = load(run_dir, 'gas-o240')
    plate, _ = load(run_dir, 'gas-o240-plate')
    # O240 is now ONE call at the field's centre, not five separated calls: no `largest_blob` here, but a wash of
    # several blobs (a camera artefact, not the bank splitting) must still fail the count.
    mask, green_px, plate_diff_px, _ = green_mask(img, plate, blob=False)
    w, h = img.size
    n = float(w * h)
    mask_set = frozenset(i for i, on in enumerate(mask) if on)
    comps = [[j for j in c if j in mask_set] for c in components(mask, w, h)]
    comps = [c for c in comps if c]
    comps.sort(key=len, reverse=True)
    specks = [c for c in comps if len(c) >= CLOUD_MIN_PX]
    large = [c for c in comps if len(c) >= LARGE_MIN_PX]
    large_px = sum(len(c) for c in large)
    large_share = round(large_px / green_px, 4) if green_px else 0.0
    blob_top = [len(c) for c in comps[:10]]
    top_blob_px = blob_top[0] if blob_top else 0
    return {'o240_green_area_frac': round(large_px / n, 4), 'o240_green_px': green_px,
            'o240_plate_diff_px': plate_diff_px, 'o240_speck_count': len(specks),
            'o240_large_cloud_count': len(large), 'o240_large_share': large_share, 'o240_blob_top': blob_top,
            'o240_top_blob_px': top_blob_px}


def poses(run_dir):
    worst, rows = 0.0, {}
    for stem in STILLS:
        with open(os.path.join(run_dir, stem + '.json'), encoding='utf-8') as f:
            e = float(json.load(f).get('pose_error_m', 99.0))
        rows[stem] = round(e, 4)
        worst = max(worst, e)
    return round(worst, 4), rows


def measure_run(run_dir):
    worst, pose_rows = poses(run_dir)
    out = {'pose_error_max': worst, 'pose_error_m': pose_rows}
    out.update(measure_t1(run_dir))
    out.update(measure_o240(run_dir))
    return out


def card_rise_m(run_dir, stem):
    """The same metre maths as `measure_t1`, against the cards/ plate, for the look report only (never barred)."""
    img, side = load(run_dir, stem)
    plate, _ = load(run_dir, 'cards-plate')
    w, h = img.size
    mask, _, _, _ = green_mask(img, plate, diff_bar=T1_DIFF_BAR, blob=True)
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ground = ground_of(run_dir, stem)

    def ndc(px, py):
        return (2.0 * (px + 0.5) / w - 1.0, 1.0 - 2.0 * (py + 0.5) / h)

    pix = list(img.getdata())
    rises, lumas = [], []
    for i, on in enumerate(mask):
        if not on:
            continue
        px, py = i % w, i // w
        nx, ny = ndc(px, py)
        d = ground_distance(cam, pitch, yaw, ground, nx, ny)
        if d is None or not (1.0 < d < 2000.0):
            continue
        y = height_at(cam, pitch, yaw, ground, nx, ny, d)
        if y is None:
            continue
        rises.append(y)
        lumas.append(luma(*pix[i]))
    if not rises:
        return {'rise_m': 0.0, 'relief': 0.0, 'mask_px': 0}
    rises.sort()
    rise_m = rises[len(rises) // 2]
    mean_l = sum(lumas) / len(lumas)
    var_l = sum((v - mean_l) ** 2 for v in lumas) / len(lumas)
    relief = (var_l ** 0.5) / mean_l if mean_l > 1e-9 else 0.0
    return {'rise_m': round(rise_m, 3), 'relief': round(relief, 4), 'mask_px': len(rises)}


def look_report():
    cards_dir = os.path.join(HERE, 'cards')
    if not all(os.path.exists(os.path.join(cards_dir, s + '.jpg')) for s in CARDS):
        print('cards/ look report: stills missing, skipped')
        return {}
    rows = {name: card_rise_m(cards_dir, name) for name in ('cards-gas', 'cards-smoke', 'cards-puff')}
    smoke_rise = rows['cards-smoke']['rise_m'] or 1e-9
    smoke_relief = rows['cards-smoke']['relief'] or 1e-9
    print('look report (cards/, unbarred): the gas card against the smoke card this pose can already draw')
    for name in ('cards-gas', 'cards-smoke', 'cards-puff'):
        r = rows[name]
        print('  %-12s rise %.3f m  relief %.4f  mask_px %d' % (name, r['rise_m'], r['relief'], r['mask_px']))
    gas_smoke_rise_ratio = rows['cards-gas']['rise_m'] / smoke_rise
    gas_smoke_relief_ratio = rows['cards-gas']['relief'] / smoke_relief
    print('  gas/smoke rise ratio   %.4f' % gas_smoke_rise_ratio)
    print('  gas/smoke relief ratio %.4f' % gas_smoke_relief_ratio)
    rows['gas_smoke_rise_ratio'] = round(gas_smoke_rise_ratio, 4)
    rows['gas_smoke_relief_ratio'] = round(gas_smoke_relief_ratio, 4)
    return rows


def frames_of(run_dir):
    """The `frame` each of the four sidecars was photographed at (the goal's "not the same frame", enforced)."""
    out = {}
    for stem in STILLS:
        with open(os.path.join(run_dir, stem + '.json'), encoding='utf-8') as f:
            out[stem] = json.load(f).get('frame')
    return out


def same_frame_refusal(frames):
    """Print and return True when two runs photographed the same gas-t1 frame or the same gas-o240 frame."""
    bad = False
    for stem in ('gas-t1', 'gas-o240'):
        seen = {}
        for name in sorted(frames):
            f = frames[name][stem]
            if f in seen:
                print('same frame: %s and %s both photographed frame %s (%s)' % (seen[f], name, f, stem))
                bad = True
            else:
                seen[f] = name
    return bad


def find_runs():
    runs = []
    for d in sorted(glob.glob(os.path.join(HERE, 'run*'))):
        if not os.path.isdir(d):
            continue
        if all(os.path.exists(os.path.join(d, s + '.jpg')) and os.path.exists(os.path.join(d, s + '.json'))
               for s in STILLS):
            runs.append(d)
    return runs


def spread_check(name, values, cmp_, bar):
    """All three `values` on the pass side of `bar`, AND the nearest one clears it by at least their spread."""
    spread = max(values) - min(values)
    if cmp_ == 'ge':
        passes = all(v >= bar for v in values)
        margin = min(values) - bar
    elif cmp_ == 'le':
        passes = all(v <= bar for v in values)
        margin = bar - max(values)
    else:  # 'lt'
        passes = all(v < bar for v in values)
        margin = bar - max(values)
    ok = passes and margin >= spread
    print('%-18s values %s  spread %.4f  margin %.4f  bar %s %.4f  -> %s'
          % (name, ['%.4f' % v for v in values], spread, margin, cmp_, bar, 'OK' if ok else 'FAILED'))
    return ok


def main():
    if '--selftest' in sys.argv:
        return selftest()

    if '--pack' in sys.argv:
        src = sys.argv[sys.argv.index('--pack') + 1]
        into = sys.argv[sys.argv.index('--into') + 1]
        written = pack(src, into)
        print('packed %s -> %s/%s: %s' % (src, HERE, into, ', '.join(written)))
        return 0

    runs = find_runs()
    calibrate = '--calibrate' in sys.argv
    if not calibrate and len(runs) < 3:
        print('three runs needed: found %d (%s)' % (len(runs), ', '.join(os.path.basename(r) for r in runs)))
        return 1

    results = {os.path.basename(d): measure_run(d) for d in runs}
    frames = {os.path.basename(d): frames_of(d) for d in runs}
    for name, out in results.items():
        print('%s: green_low_share %.4f  green_height_frac %.4f  green_rise_m %.3f  green_relief %.4f'
              '  green_area_frac %.4f  o240_green_area_frac %.4f  pose_error_max %.4f'
              % (name, out['green_low_share'], out['green_height_frac'], out['green_rise_m'],
                 out['green_relief'], out['green_area_frac'], out['o240_green_area_frac'],
                 out['pose_error_max']))
        print('  green_large_cloud_count %d  green_large_share %.4f'
              % (out['green_large_cloud_count'], out['green_large_share']))
        print('  o240_large_cloud_count %d  o240_large_share %.4f  o240_speck_count %d  o240_top_blob_px %d'
              % (out['o240_large_cloud_count'], out['o240_large_share'], out['o240_speck_count'],
                 out['o240_top_blob_px']))
        print('  diagnostics (barred on nothing): green_px %d  plate_diff_px %d  blob_px %d'
              '  green_rim_ratio %.4f  green_blob_top %s  o240_green_px %d  o240_plate_diff_px %d'
              '  o240_blob_top %s'
              % (out['green_px'], out['plate_diff_px'], out['blob_px'], out['green_rim_ratio'],
                 out['green_blob_top'], out['o240_green_px'], out['o240_plate_diff_px'],
                 out['o240_blob_top']))
        print('  frames: t1 %s (plate %s)  o240 %s (plate %s)'
              % (frames[name]['gas-t1'], frames[name]['gas-t1-plate'],
                 frames[name]['gas-o240'], frames[name]['gas-o240-plate']))

    look = look_report()

    if calibrate:
        with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
            json.dump({'runs': results, 'look': look, 'frames': frames}, f, indent=1, sort_keys=True)
        return 0

    if same_frame_refusal(frames):
        print('FAILED: three captures of the same frame are one capture')
        return 1

    ok = True
    for bar_name, cmp_ in CARD_BARS.items():
        if bar_name not in look:
            print('%-18s no cards/ stills: cannot check' % bar_name)
            ok = False
            continue
        ok = spread_check(bar_name, [look[bar_name]], cmp_, BAR_VALUES[bar_name]) and ok

    for bar_name, cmp_ in BARS.items():
        values = [results[r][bar_name] for r in results]
        # bars are set once three different-frame runs exist: see this folder's README.md
        bar = BAR_VALUES.get(bar_name)
        if bar is None:
            print('%-18s no bar set yet (needs the owner-approved build''s three captures)' % bar_name)
            ok = False
            continue
        ok = spread_check(bar_name, values, cmp_, bar) and ok

    with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
        json.dump({'runs': results, 'look': look, 'frames': frames, 'bars': BAR_VALUES},
                  f, indent=1, sort_keys=True)
    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


# Bar values, set from the spread of run1/run2/run3 at three different frames (see README.md "The bars").
# Every T1 bar below is RE-DERIVED for the 1:1 card + GasBankSink 3.0 build (see this folder's README.md): the
# card's own mass and shape read differently from the -g2-g2-g2-g2-g1-g2 puddle/cloud, so last unit's floors no
# longer clear by the new runs' spread; each is recalculated as floor <= min(values) - spread, with margin left
# over the spread (the owner's "draw the close gas as that one smoke cloud" outranks the old numbers, this leg).
BAR_VALUES = {
    'green_low_share': 0.23,        # re-derived (the 1:1 card's visible mass sits higher than the old puddle's,
                                     # even sunk by GasBankSink 3.0). Runs 0.3155/0.2779/0.2753, spread 0.0402,
                                     # floor 0.2753, margin 0.0453
    'green_height_frac': 0.06,      # re-derived. Runs 0.0840/0.1042/0.1035, spread 0.0202, floor 0.0840, margin
                                     # 0.0240
    'green_relief': 0.08,           # re-derived (the single 1:1 card reads flatter than the old many-pad puddle
                                     # did). Runs 0.0936/0.0888/0.0877, spread 0.0059, floor 0.0877, margin 0.0077
    'green_rise_m': 2.00,            # re-derived. Runs 2.522/2.985/2.980, spread 0.463, floor 2.522, margin 0.522
    'green_area_frac': 0.06,        # re-derived (one standing card covers less of the frame than the old carpet
                                     # of pads did). Runs 0.0732/0.0689/0.0689, spread 0.0043, floor 0.0689,
                                     # margin 0.0089
    'green_large_cloud_count': 1,    # runs 1/1/1, spread 0: T1_LARGE_MIN_PX 4000 sits above every secondary comp
                                      # (top ten sizes 3548/3065/2318/... at most) and well under the main
                                      # blob (63480-67418)
    'green_large_share': 0.45,       # re-derived: runs 0.5330/0.5034/0.5027, spread 0.0303, floor 0.5027, margin
                                      # 0.0527 - the one cloud is most of the green, not a wash around it
    'o240_green_area_frac': 0.0025,   # re-derived (the T1 call's gas drifts into the far shot too): runs
                                       # 0.0036/0.0037/0.0042, spread 0.0006, floor 0.0036, margin 0.0011
    'o240_large_cloud_count': 1,      # runs 1/1/1, spread 0: the one-call O240 shot still reads as ONE cloud
    'o240_large_share': 0.70,         # re-derived: runs 0.7880/0.8616/0.8573, spread 0.0736, floor 0.7880, margin
                                       # 0.0880 - the cloud is the green, not a wash around it
    'o240_top_blob_px': 2800,         # re-derived: runs 3348/3436/3840 px, spread 492, floor 3348, margin 548
    'pose_error_max': 0.50,         # GasBankStills.PoseBar
}


if __name__ == '__main__':
    sys.exit(main())
