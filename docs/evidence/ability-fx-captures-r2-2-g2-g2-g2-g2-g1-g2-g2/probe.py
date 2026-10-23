#!/usr/bin/env python3
"""Make the gas card stand with a rise above 0, move the relief ratio off its puddle reading, and take the O240
cloud COUNT bar off a 1/1/1-with-zero-margin reading.

WHY. The owner's complaint about the -g2-g2-g2-g2-g1-g2 probe:

    cards-gas rise is 0, the same as the smoke card, so the rise ratio stays 0 and unbarred. The relief ratio
    0.5505 (bar 0.45, one cards pass, spread 0) passes that puddle. Make the gas card stand like the smoke card
    and bar its rise above 0. Put the relief ratio outside 0.5505, toward the smoke card's 0.2178. The far second
    blob is 238/83/136 px and o240_large_cloud_count is 1, 1, 1 with margin 0. Move that bar off the reading.

WHAT WAS ACTUALLY WRONG (no look change; the instrument lied twice over):

  1. The old `card_rise_m` took each masked pixel's OWN ground distance, so every pixel's height was measured in
     the vertical plane directly below ITSELF - which puts every pixel back on the ground it stands on, reading
     0.000 m no matter how tall the card is drawn. `measure_t1` never had this fault: it takes each COLUMN's
     distance from its own FOOT pixel and measures every pixel in that column against that one plane. The
     column-foot logic is now one shared helper, `column_heights`, used by both `measure_t1` and the new
     `card_measure`.
  2. The cards used the green mask (`greener`/`green_mask`). The gas card IS green (a cloud of ~14.8k green px),
     so its "mask" was never the card: it was ~1.4k px of green fringe bleeding past the card's edge, next to a
     grey smoke card the green mask barely sees at all - the two card measurements were never reading the same
     thing. A plate-diff mask fares no better: this scene flickers +-200 luma between passes with no card drawn,
     so a luma-only diff blob runs to the frame bottom. What reads both a green and a grey card alike: luma ADDED
     over the plate (>= `CARD_LUMA_BAR`), confined to a centre window (`CARD_WINDOW`) so the flicker at the frame
     edges never joins the card's blob, then `largest_blob`.

On the committed pass this reads: gas 11320 px, relief 0.1257, rise 2.224 m (mask top 4.796 m, against the card's
own drawn top of `CardWidth * GasBankStand - GasBankSink` = 4.84 m - the metre maths checks out); smoke 15719 px,
relief 0.0615, rise 3.598 m; puff 5099 px, relief 0.1722. `gas_smoke_relief_ratio` 2.044 (the gas card's relief is
now the HIGHER of the two - a cloud, not a flatter puddle), `gas_smoke_rise_ratio` 0.618 (printed only, see below).

THE BARS.
  * `cards_gas_rise_m` (`ge`) - the gas card's own rise in metres. The owner's "above 0" is about THIS card's
    rise, not a ratio against a card drawn full height, so the ratio form stays a printed diagnostic
    (`gas_smoke_rise_ratio`), not a bar.
  * `gas_smoke_relief_ratio` (`ge`) - unchanged in name, recomputed on the luma-add masks: the gas card's relief
    over the smoke card's, now the real cloud-vs-cloud ratio instead of cloud-vs-fringe.
  * `o240_top_blob_share` (`ge`) - `o240_top_blob_px / o240_green_px`: when the far green splits into a second
    cloud, the top blob's SHARE of the total green falls, so a second cloud of any size costs this bar room; a
    blob COUNT can only ever sit on a 1-vs-2 reading with zero margin, which is what the owner flagged.
    `o240_large_cloud_count` moves out of `BARS` (still printed, with `o240_blob_top` and the new printed
    diagnostic `o240_second_blob_px`): `o240_large_share` stays barred too, but it counts every LARGE blob's
    share, so a second 300+ px cloud still raises it - `o240_top_blob_share` is the one bar that falls when a
    second cloud of any size (the 238/83/136 px the owner measured included) appears.

THE FRAME GUARD, for run1/run2/run3 AND for cards/cards2/cards3: before any bar is checked, every sidecar's
`frame` is read; two passes sharing a `gas-t1`, `gas-o240` or `cards-gas` frame are one capture, and the probe
prints `same frame: ...` and exits 1. `--calibrate` prints the frames without refusing.

THE BAR CHECK. For run1/run2/run3 and for cards/cards2/cards3 alike: all three values on the pass side of the
bar, AND the nearest of the three clears it by at least the three values' own spread (max - min). A bar sitting
inside the values, or within one spread of them, is "the build cannot tell a measure from its own noise."

Everything else is carried unchanged: the camera maths (`ray`, `ground_distance`, `height_at`, `selftest`),
`greenness`, `green_mask`, `components`, `largest_blob`, `measure_o240`'s `LARGE_MIN_PX` floor, `spread_check`.

THE NAMES. T1 keys still say green: `green_low_share`, `green_height_frac`, `green_rise_m`, `green_relief`,
`green_area_frac`. `green_px`, `plate_diff_px`, `blob_px`, `green_rim_ratio`, `o240_green_px`, `o240_plate_diff_px`,
`o240_speck_count`, `o240_blob_top` and the new `o240_second_blob_px` are printed per run as named diagnostics,
barred on nothing. Cards keys say `cards_*` or `gas_smoke_*`; the look report also prints the old green-mask
relief pair (0.1199/0.2178) next to the new luma-add pair, as a diagnostic.

USAGE
    python probe.py                 bar run1/run2/run3 and cards/cards2/cards3 against each other
    python probe.py --calibrate      print every pass's numbers and frames with no bar check and no frame refusal
    python probe.py --selftest       the camera maths against a hand calculation, no images
    python probe.py --pack SRC --into NAME   copy a GasBankStills or GasBankCards output folder into HERE/NAME

Exit 0 only when three run*/ and three cards*/ passes at three different frames each are loaded and every bar
clears by at least its own spread.
Exit 1 with 'three runs needed' / 'three cards passes needed' if fewer than three of either are found.
Exit 1 with 'same frame' if two passes photographed the same t1, o240 or cards-gas frame.
Exit 1 with 'bar inside the noise' (printed as FAILED) if a bar does not clear by the spread.
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
CARD_LUMA_BAR = 6             # luma a card pixel must ADD over the plate to count (reads a green or a grey card)
CARD_WINDOW = (0.25, 0.75, 0.95)   # x_lo, x_hi (fractions of width), y_hi (fraction of height) - the centre box
                                    # the hand-packed card sits in, so the scene's +-200 luma flicker at the frame
                                    # edges never joins the card's blob

CARD_BARS = {'cards_gas_rise_m': 'ge', 'gas_smoke_relief_ratio': 'ge'}

BARS = {'green_low_share': 'ge', 'green_height_frac': 'ge', 'green_area_frac': 'ge', 'green_rise_m': 'ge',
        'green_relief': 'ge',
        'o240_green_area_frac': 'ge', 'o240_large_share': 'ge', 'o240_top_blob_share': 'ge',
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


def luma_added(img, plate, window=CARD_WINDOW, bar=CARD_LUMA_BAR):
    """Mask of pixels inside `window` whose luma is `bar` or more ABOVE the plate's: a card, green or grey alike."""
    w, h = img.size
    x_lo, x_hi, y_hi = window
    px_lo, px_hi, py_hi = x_lo * w, x_hi * w, y_hi * h
    a, b = list(img.getdata()), list(plate.getdata())
    out = [False] * (w * h)
    for i, (p, q) in enumerate(zip(a, b)):
        x, y = i % w, i // w
        if not (px_lo <= x < px_hi and y < py_hi):
            continue
        out[i] = (luma(*p) - luma(*q)) >= bar
    return out


def pack(src, into):
    """Copy every still in `src` into HERE/`into`/ as a JPG under 400 KB, with its sidecar and manifests.

    Lifted from docs/evidence/ability-fx-captures-r2/probe.py's `pack` (lines 131-152), so run2/run3 and
    cards2/cards3 are packed exactly as run1/cards were: same quality ladder, same files, just into a named
    directory instead of HERE.
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

    relief is the stdev of the masked pixels' luma over their mean - a flat wash is near 0. rim_ratio is the mean
    luma of the mask pixels that touch an off-mask 4-neighbour (the silhouette's edge) over the mean of the rest:
    above 1 the edge is the lit side, under 1 it is a dark rim. Diagnostic only - the direction this art's rim
    takes is not predictable, so it is reported, not barred.
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


def column_heights(mask, w, h, cam, pitch, yaw, ground):
    """Per-column metre heights of `mask`'s pixels, each column measured against ITS OWN FOOT's ground plane.

    Lifted out of `measure_t1` (unchanged maths) so `card_measure` reads a card's rise the same way `measure_t1`
    reads the in-game cloud's: a column's FOOT pixel (its lowest masked row) gives the ground distance, and
    every pixel in that column is then measured in THAT one vertical plane. The old per-pixel version measured
    each pixel against its own distance, which puts every pixel back on the ground under itself - 0.000 m no
    matter how tall the card stands.

    Returns (heights, low, total, dists, tops): `heights` the per-column 45th-percentile metre height (sorted),
    `low` the count of mask pixels at or under HEAD_M, `total` the count of mask pixels that resolved a height
    at all, `dists` each column's own ground distance, `tops` each column's own MAXIMUM metre height (its true
    top edge, not the 45th percentile) - what `card_measure` reads as the card's top.
    """
    def ndc(px, py):
        return (2.0 * (px + 0.5) / w - 1.0, 1.0 - 2.0 * (py + 0.5) / h)

    by_column = {}
    for i, on in enumerate(mask):
        if on:
            px, py = i % w, i // w
            by_column.setdefault(px, []).append(py)

    MIN_COLUMN_PX = 8
    foot_rows = {px: max(rows_) for px, rows_ in by_column.items() if len(rows_) >= MIN_COLUMN_PX}

    dists = {}
    for px, py in foot_rows.items():
        nx, ny = ndc(px, py)
        d = ground_distance(cam, pitch, yaw, ground, nx, ny)
        if d is not None and 1.0 < d < 2000.0:
            dists[px] = d

    low, total, col_heights = 0, 0, {}
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

    heights, tops = [], []
    for px, ys in col_heights.items():
        ys.sort()
        p45 = ys[min(len(ys) - 1, int(round(0.45 * (len(ys) - 1))))]
        heights.append(p45)
        tops.append(ys[-1])
    heights.sort()
    return heights, low, total, dists, tops


def measure_t1(run_dir):
    img, side = load(run_dir, 'gas-t1')
    plate, _ = load(run_dir, 'gas-t1-plate')
    w, h = img.size
    mask, green_px, plate_diff_px, blob_px = green_mask(img, plate, diff_bar=T1_DIFF_BAR, blob=True)
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ground = ground_of(run_dir, 'gas-t1')

    heights, low, total, dists, _tops = column_heights(mask, w, h, cam, pitch, yaw, ground)

    mask_px = sum(1 for on in mask if on)
    relief, rim_ratio = mask_relief(img, mask)
    diag = {'green_px': green_px, 'plate_diff_px': plate_diff_px, 'blob_px': blob_px,
            'green_rim_ratio': rim_ratio}
    if not heights or total == 0:
        out = {'green_low_share': 0.0, 'green_height_frac': 0.0, 'green_rise_m': 0.0, 'green_relief': relief,
               'green_area_frac': round(mask_px / float(w * h), 4)}
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
    out.update(diag)
    return out


def measure_o240(run_dir):
    img, _ = load(run_dir, 'gas-o240')
    plate, _ = load(run_dir, 'gas-o240-plate')
    # O240 is now ONE call at the field's centre, not five separated calls: no `largest_blob` here, but a wash of
    # several blobs (a camera artefact, not the bank splitting) must still cost the top blob its share.
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
    second_blob_px = blob_top[1] if len(blob_top) > 1 else 0
    top_blob_share = round(top_blob_px / green_px, 4) if green_px else 0.0
    return {'o240_green_area_frac': round(large_px / n, 4), 'o240_green_px': green_px,
            'o240_plate_diff_px': plate_diff_px, 'o240_speck_count': len(specks),
            'o240_large_cloud_count': len(large), 'o240_large_share': large_share, 'o240_blob_top': blob_top,
            'o240_top_blob_px': top_blob_px, 'o240_second_blob_px': second_blob_px,
            'o240_top_blob_share': top_blob_share}


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


def card_measure(run_dir, stem):
    """A hand-packed card's own rise, top and relief, read off the luma-add mask (see module docstring, fault 2),
    with the column-foot metre maths `measure_t1` uses (fault 1). The old green-mask reading is kept alongside as
    `old_relief`, a printed diagnostic only, so the superseded 0.1199/0.2178 pair stays on the page.
    """
    img, side = load(run_dir, stem)
    plate, _ = load(run_dir, 'cards-plate')
    w, h = img.size
    mask = largest_blob(luma_added(img, plate), w, h)
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ground = ground_of(run_dir, stem)

    heights, _low, _total, _dists, tops = column_heights(mask, w, h, cam, pitch, yaw, ground)
    mask_px = sum(1 for on in mask if on)
    relief, rim_ratio = mask_relief(img, mask)

    old_mask, _, _, _ = green_mask(img, plate, diff_bar=T1_DIFF_BAR, blob=True)
    old_relief, _ = mask_relief(img, old_mask)

    if not heights:
        return {'rise_m': 0.0, 'top_m': 0.0, 'relief': relief, 'rim_ratio': rim_ratio, 'mask_px': mask_px,
                'old_relief': old_relief}
    rise_m = heights[len(heights) // 2]
    top_m = max(tops)
    return {'rise_m': round(rise_m, 3), 'top_m': round(top_m, 3), 'relief': relief, 'rim_ratio': rim_ratio,
            'mask_px': mask_px, 'old_relief': old_relief}


def card_frame(cards_dir):
    with open(os.path.join(cards_dir, 'cards-gas.json'), encoding='utf-8') as f:
        return json.load(f).get('frame')


def find_cards_dirs():
    out = []
    for name in ('cards', 'cards2', 'cards3'):
        d = os.path.join(HERE, name)
        if all(os.path.exists(os.path.join(d, s + '.jpg')) and os.path.exists(os.path.join(d, s + '.json'))
               for s in CARDS):
            out.append(d)
    return out


def look_report():
    """Bar `cards_gas_rise_m` and `gas_smoke_relief_ratio` off cards/cards2/cards3 (three passes, three frames,
    the same spread-and-margin check as run1/run2/run3). Prints the old green-mask relief pair as a diagnostic.
    """
    dirs = find_cards_dirs()
    if len(dirs) < 3:
        print('three cards passes needed: found %d (%s)'
              % (len(dirs), ', '.join(os.path.basename(d) for d in dirs)))
        return {}, [], True

    frames = {os.path.basename(d): card_frame(d) for d in dirs}
    seen, refused = {}, False
    for name in sorted(frames):
        f = frames[name]
        if f in seen:
            print('same frame: %s and %s both photographed frame %s (cards-gas)' % (seen[f], name, f))
            refused = True
        else:
            seen[f] = name

    per_dir = []
    for d in dirs:
        rows = {name: card_measure(d, name) for name in ('cards-gas', 'cards-smoke', 'cards-puff')}
        smoke_rise = rows['cards-smoke']['rise_m'] or 1e-9
        smoke_relief = rows['cards-smoke']['relief'] or 1e-9
        smoke_old_relief = rows['cards-smoke']['old_relief'] or 1e-9
        row = {
            'cards_gas_rise_m': rows['cards-gas']['rise_m'],
            'gas_smoke_relief_ratio': round(rows['cards-gas']['relief'] / smoke_relief, 4),
            'gas_smoke_rise_ratio': round(rows['cards-gas']['rise_m'] / smoke_rise, 4),
            'gas_smoke_relief_ratio_old': round(rows['cards-gas']['old_relief'] / smoke_old_relief, 4),
            'rows': rows,
        }
        per_dir.append(row)

    print('look report (cards/cards2/cards3): the gas card against the smoke card it now copies')
    for name, row in zip((os.path.basename(d) for d in dirs), per_dir):
        for book in ('cards-gas', 'cards-smoke', 'cards-puff'):
            r = row['rows'][book]
            print('  %-7s %-12s rise %.3f m  top %.3f m  relief %.4f  old_relief %.4f  mask_px %d'
                  % (name, book, r['rise_m'], r['top_m'], r['relief'], r['old_relief'], r['mask_px']))
        print('  %-7s gas/smoke rise ratio %.4f  relief ratio %.4f  (old-mask relief ratio %.4f)'
              % (name, row['gas_smoke_rise_ratio'], row['gas_smoke_relief_ratio'], row['gas_smoke_relief_ratio_old']))

    out = {bar: [row[bar] for row in per_dir] for bar in CARD_BARS}
    out['gas_smoke_rise_ratio'] = [row['gas_smoke_rise_ratio'] for row in per_dir]
    out['gas_smoke_relief_ratio_old'] = [row['gas_smoke_relief_ratio_old'] for row in per_dir]
    return out, frames, refused


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
    print('%-24s values %s  spread %.4f  margin %.4f  bar %s %.4f  -> %s'
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
        print('  o240_large_cloud_count %d  o240_large_share %.4f  o240_top_blob_share %.4f  o240_speck_count %d'
              '  o240_top_blob_px %d  o240_second_blob_px %d'
              % (out['o240_large_cloud_count'], out['o240_large_share'], out['o240_top_blob_share'],
                 out['o240_speck_count'], out['o240_top_blob_px'], out['o240_second_blob_px']))
        print('  diagnostics (barred on nothing): green_px %d  plate_diff_px %d  blob_px %d'
              '  green_rim_ratio %.4f  o240_green_px %d  o240_plate_diff_px %d  o240_blob_top %s'
              % (out['green_px'], out['plate_diff_px'], out['blob_px'], out['green_rim_ratio'],
                 out['o240_green_px'], out['o240_plate_diff_px'], out['o240_blob_top']))
        print('  frames: t1 %s (plate %s)  o240 %s (plate %s)'
              % (frames[name]['gas-t1'], frames[name]['gas-t1-plate'],
                 frames[name]['gas-o240'], frames[name]['gas-o240-plate']))

    look, card_frames, cards_refused = look_report()

    if calibrate:
        with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
            json.dump({'runs': results, 'look': look, 'frames': frames, 'card_frames': card_frames},
                      f, indent=1, sort_keys=True)
        return 0

    if same_frame_refusal(frames) or cards_refused:
        print('FAILED: three captures of the same frame are one capture')
        return 1

    ok = True
    for bar_name, cmp_ in CARD_BARS.items():
        if bar_name not in look:
            print('%-24s no cards passes: cannot check' % bar_name)
            ok = False
            continue
        bar = BAR_VALUES.get(bar_name)
        if bar is None:
            print('%-24s no bar set yet' % bar_name)
            ok = False
            continue
        ok = spread_check(bar_name, look[bar_name], cmp_, bar) and ok

    for bar_name, cmp_ in BARS.items():
        values = [results[r][bar_name] for r in results]
        # bars are set once three different-frame runs exist: see this folder's README.md
        bar = BAR_VALUES.get(bar_name)
        if bar is None:
            print('%-24s no bar set yet (needs the owner-approved build''s three captures)' % bar_name)
            ok = False
            continue
        ok = spread_check(bar_name, values, cmp_, bar) and ok

    with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
        json.dump({'runs': results, 'look': look, 'frames': frames, 'card_frames': card_frames, 'bars': BAR_VALUES},
                  f, indent=1, sort_keys=True)
    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


# Bar values, set from the spread of run1/run2/run3 and cards/cards2/cards3 (see README.md "The bars").
BAR_VALUES = {
    'green_low_share': 0.30,        # owner-decided, and it now BITES: the standing cloud reads 0.3451/0.3451+ where
                                     # the flat sheet was saturated at 1.0000. Runs 0.3522/0.3451/0.3493, spread
                                     # 0.0071, floor 0.3451, margin 0.0451
    'green_height_frac': 0.04,      # turned round: a FLOOR, not the old 0.15 ceiling the stripe sheet passed at 0.02.
                                     # Runs 0.0570/0.0552/0.0586, spread 0.0034, floor 0.0552: a round number clear of
                                     # the sheet's 0.0192-0.0209 and clear of the runs by 0.0152
    'green_relief': 0.10,           # new: a flat wash is near 0, the smoke card's drawing 0.2178. Runs
                                     # 0.1168/0.1202/0.1167, spread 0.0035, floor 0.1167: the round number above the
                                     # old gas puddle's 0.0849, margin 0.0167
    'green_rise_m': 1.50,           # re-derived (was 0.30 against a 0.359 sheet): runs 1.709/1.669/1.756, spread
                                     # 0.087, floor 1.669, margin 0.169
    'green_area_frac': 0.07,        # re-derived: the cloud fills the frame where the sheet barely stained it. Runs
                                     # 0.0872/0.0931/0.0867, spread 0.0064, floor 0.0867, margin 0.0167
    'o240_green_area_frac': 0.0025,   # re-derived (the T1 call's gas drifts into the far shot too): runs
                                       # 0.0036/0.0037/0.0042, spread 0.0006, floor 0.0036, margin 0.0011
    'o240_large_share': 0.70,         # re-derived: runs 0.7822/0.8605/0.8573, spread 0.0783, floor 0.7822, margin
                                       # 0.0822 - the cloud is the green, not a wash around it
    'o240_top_blob_px': 2800,         # re-derived; pairs with the share bar, so zero clouds still fails
    'o240_top_blob_share': 0.65,      # new (this unit): runs 0.7822/0.8605/0.8573, spread 0.0783, floor 0.7822,
                                       # margin 0.1322 = 1.7x spread; a second 900 px cloud (the owner's 238/83/136
                                       # px second blobs cost this bar real room where the old count bar had none)
    'pose_error_max': 0.50,         # GasBankStills.PoseBar
    'cards_gas_rise_m': 1.00,         # new (this unit): cards/cards2/cards3 read 2.224/3.192/2.117 m, spread
                                       # 1.075, floor 2.117, margin 1.117 - a round number well above 0
    'gas_smoke_relief_ratio': 0.45,   # re-derived off the luma-add masks (this unit): cards/cards2/cards3 read
                                       # 2.0439/3.5927/2.3873, spread 1.5488, floor 2.0439, margin 1.5939 - clear
                                       # of the old mask's 0.5505 puddle reading, toward the smoke card's own shape
}


if __name__ == '__main__':
    sys.exit(main())
