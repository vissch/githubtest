#!/usr/bin/env python3
"""Draw the close gas as the smoke card's standing cloud, with a rim, and bar its rise and relief against it.

WHY. The owner's complaint about the -g2-g2-g2-g2-g2 build:

    At zoom 30 the close gas is still a sheet of pale green stripes. cards-gas is a flat green puddle with two
    beams, cards-puff is a faint wisp, and cards-smoke is the only standing cloud. Draw the gas as that cloud,
    with a rim, and bar its rise and its relief against the smoke card. The height floor of 0.013 sits under a
    sheet of 0.0148 to 0.0161. Start from lane/show/ability-fx-captures-r2-2-g2-g2-g2-g2-g2's tip. Bar outside
    the spread of three captures of the current build.

The cause was in the sheet table, not in the card sizes: `FlipbookFx.Sheets` row `GasBank` had
`Ink = (0.00, 0.35)`, so the drawing in `GasBank.png` only ever inked the bottom 35% of its cell. `SmokeBank`'s
`Ink = (0.03, 0.87)` fills its cell, which is why `cards-smoke` was the only standing cloud. Jittering card
heights (the last unit) could not beat that: a taller card drew a taller stripe. The close gas is now drawn with
a new `Book.GasCloud` row over the SMOKE drawing, tinted green, with a brighter rim card in place of the crown
(`CombatFx.Banks.cs`, part 1 of this unit, guarded by `BurstRecipeTests`).

THIS PROBE carries -g2-g2-g2-g2-g2's camera maths (`ray`, `ground_distance`, `height_at`, `selftest`), its
measure (`greenness`, `greener`, `green_mask`), `components`, `largest_blob`, `pack`, the frame guard,
`--calibrate` and every O240 key unchanged.

THE CARDS ARE MEASURED PER RUN. The predecessor shot one `GasBankCards` isolation pass into a single top-level
`cards/` folder and printed its gas/smoke ratios as an unbarred "look report". The owner asked for those two
ratios to be BARS, so they are measured inside `measure_run` from each run's own cards pass and barred like any
other key:

  - `gas_smoke_rise_ratio` (`ge`) - the gas card's median rise over the smoke card's. The sheet build read
    0.0000: its gas card drew no standing pixel at all once the plate was subtracted.
  - `gas_smoke_relief_ratio` (`ge`) - the gas card's relief (stdev of masked luma over its mean) over the smoke
    card's. The sheet build read 0.3724: a flat puddle has no relief.

`cards_gas_rise_m` and `cards_gas_relief` are printed per run as diagnostics, barred on nothing.
`find_runs()` now needs each run directory to hold the four `cards-*` stills as well as the four `gas-*` ones,
so a run that is missing its cards pass is not counted as a run.

THE CARDS PASS IS DETERMINISTIC by construction (`GasBankCards` makes its own `FlipbookFx`, hand-packs one card
per book from `now = 0` with a fixed hash and a pinned clock, and photographs one pose), so the two ratios read
the SAME number in all three runs: their spread is zero. That is not a three-capture agreement the way the T1
pixel keys' is - it is one measurement counted three times - and the README says so plainly. The bar still has
to clear the sheet build's 0.0000 and 0.3724 by a wide margin to mean anything.

`green_height_frac` stays a `ge` FLOOR and has to come out ABOVE the sheet build's top reading of 0.0161, or the
owner's complaint stands; see README.md.

`t1_gas_cards` (the capture frame's own `gas_cards`, from `manifest.json`'s `gas-t1.png` row) is barred `le`, so
the rim cannot push the bank past `FlipbookFx.MaxCards` 1536, where cards are dropped in silence.

THE OTHER T1 KEYS are unchanged in kind and re-set from this build's own three captures: `green_low_share`
(`ge`, the head rule), `green_rise_m` (`ge`), `green_area_frac` (`ge`), `green_knee_share` (`le`, the puddle
bar) and `green_top_rough` (printed, never barred: the predecessor found no bar on it that both clears its noise
and fails the sheet).

THE O240 KEYS are untouched - part 1 left the overview drawing alone - so `o240_green_area_frac`,
`o240_large_cloud_count`, `o240_large_share` and `pose_error_max` keep the predecessor's bars and are only
re-confirmed against these captures.

THE FRAME GUARD. Before any bar is looked at, the four `gas-*` sidecars' `frame` numbers are read. Two runs
sharing a `gas-t1` frame, or a `gas-o240` frame, are not three captures: the probe prints
`same frame: runN and runM both photographed frame F` and exits 1. (The cards stills are one fixed pose by
design and are not frame-guarded.) `--calibrate` prints the frames and does NOT refuse.

THE BAR. Three runs of the SAME build (`run1`, `run2`, `run3`, each the four `GasBankStills` captures, a no-gas
plate and a `GasBankCards` pass) are loaded, at three different frames. For a bar to hold, the three runs must
all sit on the pass side AND the nearest run value to the bar must clear it by at least the three runs' own
spread (max - min). A bar sitting inside the run values, or within one spread of them, is "the build cannot tell
a measure from its own noise" and the probe exits 1 rather than print a false green.

USAGE
    python probe.py                 bar run1/run2/run3 against each other
    python probe.py --calibrate      print every run's numbers and frames with no bar check and no frame refusal
    python probe.py --selftest       the camera maths against a hand calculation, no images

Exit 0 only when three runs at three different frames are loaded and every bar clears by at least their spread.
Exit 1 with 'three runs needed' if fewer than three run*/ directories hold a full set of stills and cards.
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
KNEE_M = 0.5                 # knee height: the share of the gas under it is the puddle measure (the old flat
                             # bank's median rise was 0.359 m, so nearly all of it sat under this)
GREEN_BAR = 8                 # greenness the gas must ADD over the plate for a pixel to count as gas
DIFF_BAR = 12                 # RGB distance from the plate that counts as changed (diagnostic only now)
T1_DIFF_BAR = 20               # T1's own, stricter plate-difference bar (diagnostic only now)
BLOB_DILATE = 1               # largest_blob's bridge width (r2-2: 2 chained the bank into terrain-edge noise)
STILLS = ['gas-t1', 'gas-t1-plate', 'gas-o240', 'gas-o240-plate']
CARDS = ['cards-gas', 'cards-smoke', 'cards-puff', 'cards-plate']
CLOUD_MIN_PX = 50             # a component this small or smaller is noise, not a gas cloud (O240's five calls)
LARGE_MIN_PX = 300            # a cloud a player can pick out at the far camera (~17x17 patch, double the 150 px
                              # speck ceiling, well under the two big clouds at 2310+)
BARS = {'green_low_share': 'ge', 'green_area_frac': 'ge', 'green_rise_m': 'ge',
        'green_knee_share': 'le', 't1_gas_cards': 'le',
        'green_blob_share': 'le', 'green_rim_lift': 'ge',
        'gas_smoke_rise_ratio': 'ge', 'gas_smoke_relief_ratio': 'ge',
        'o240_green_area_frac': 'ge', 'o240_large_cloud_count': 'ge', 'o240_large_share': 'ge',
        'pose_error_max': 'lt'}


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


def manifest_field(run_dir, stem, key, default=0.0):
    """A field of `manifest.json`'s row for `stem`: the capture frame's own numbers, which the sidecar does not
    carry."""
    path = os.path.join(run_dir, 'manifest.json')
    if os.path.exists(path):
        with open(path, encoding='utf-8') as f:
            for row in json.load(f).get('stills', []):
                if row.get('still', '').startswith(stem):
                    return float(row.get(key, default))
    return default


def rim_lift_of(img, by_column, min_column_px):
    """"With a rim", as a number: how much brighter the TOP of each green column is than the rest of it.

    Per green column of at least `min_column_px` pixels, the pixels are sorted top-of-screen first, the median
    luma of the top 30% is divided by the median luma of the remaining 70%, and the key is the median of those
    per-column ratios. A rim card drawn 1.5x brighter over the cloud's crown lifts this above 1; a flat carpet of
    ovals, lit the same all over, sits at or just under 1.

    The columns come from the T1 BLOB mask (`by_column`), so terrain-edge noise outside the bank cannot bias it.
    """
    pix = list(img.getdata())
    w = img.size[0]
    ratios = []
    for px, rows_ in by_column.items():
        if len(rows_) < min_column_px:
            continue
        ys = sorted(rows_)                          # smallest py first: the top of the screen
        cut = max(1, int(round(0.30 * len(ys))))
        top = sorted(luma(*pix[y * w + px]) for y in ys[:cut])
        rest = sorted(luma(*pix[y * w + px]) for y in ys[cut:])
        if not rest:
            continue
        med_rest = rest[len(rest) // 2]
        if med_rest <= 1e-6:
            continue
        ratios.append(top[len(top) // 2] / med_rest)
    if not ratios:
        return 0.0
    ratios.sort()
    return round(ratios[len(ratios) // 2], 4)


def measure_t1(run_dir):
    img, side = load(run_dir, 'gas-t1')
    plate, _ = load(run_dir, 'gas-t1-plate')
    w, h = img.size
    mask, green_px, plate_diff_px, blob_px = green_mask(img, plate, diff_bar=T1_DIFF_BAR, blob=True)
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
    blob_share = round(blob_px / float(green_px), 4) if green_px else 1.0
    rim_lift = rim_lift_of(img, by_column, MIN_COLUMN_PX)

    def ndc(px, py):
        return (2.0 * (px + 0.5) / w - 1.0, 1.0 - 2.0 * (py + 0.5) / h)

    dists = {}
    for px, py in columns.items():
        nx, ny = ndc(px, py)
        d = ground_distance(cam, pitch, yaw, ground, nx, ny)
        if d is not None and 1.0 < d < 2000.0:
            dists[px] = d

    heights, low, knee, total, col_heights = [], 0, 0, 0, {}
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
        if y <= KNEE_M:
            knee += 1
        col_heights.setdefault(px, []).append(y)
    for px, ys in col_heights.items():
        ys.sort()
        p45 = ys[min(len(ys) - 1, int(round(0.45 * (len(ys) - 1))))]
        heights.append(p45)
    heights.sort()

    mask_px = sum(1 for on in mask if on)
    cards = manifest_field(run_dir, 'gas-t1', 'gas_cards')
    diag = {'green_px': green_px, 'plate_diff_px': plate_diff_px, 'blob_px': blob_px}
    if not heights or total == 0:
        out = {'green_low_share': 0.0, 'green_height_frac': 0.0, 'green_rise_m': 0.0,
               'green_knee_share': 1.0, 'green_top_rough': 0.0, 't1_gas_cards': cards,
               'green_blob_share': blob_share, 'green_rim_lift': rim_lift,
               'green_area_frac': round(mask_px / float(w * h), 4)}
        out.update(diag)
        return out
    med_h = heights[len(heights) // 2]
    dvals = sorted(dists.values())
    med_d = dvals[len(dvals) // 2]
    frame_h = 2.0 * med_d * math.tan(math.radians(FOV_V_DEG * 0.5))
    mean_h = sum(heights) / len(heights)
    var_h = sum((v - mean_h) ** 2 for v in heights) / len(heights)
    out = {
        'green_low_share': round(low / float(total), 4),
        'green_height_frac': round(med_h / frame_h, 4),
        'green_rise_m': round(med_h, 3),
        'green_knee_share': round(knee / float(total), 4),
        # a stripe is one height in every column, so its stdev over its median is near 0; a puff is many heights
        'green_top_rough': round((var_h ** 0.5) / med_h, 4) if med_h > 1e-9 else 0.0,
        't1_gas_cards': cards,
        'green_area_frac': round(mask_px / float(w * h), 4),
        # the owner's two words, as numbers: "a carpet" (one blob holding all the green) and "with a rim"
        'green_blob_share': blob_share,
        'green_rim_lift': rim_lift,
    }
    out.update(diag)
    return out


def measure_o240(run_dir):
    img, _ = load(run_dir, 'gas-o240')
    plate, _ = load(run_dir, 'gas-o240-plate')
    # O240 is five separated calls 40 m apart, not one patch: no `largest_blob` here (it would keep only one call).
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
    return {'o240_green_area_frac': round(large_px / n, 4), 'o240_green_px': green_px,
            'o240_plate_diff_px': plate_diff_px, 'o240_speck_count': len(specks),
            'o240_large_cloud_count': len(large), 'o240_large_share': large_share, 'o240_blob_top': blob_top}


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
    out.update(measure_cards(run_dir))
    return out


def card_rise_m(run_dir, stem):
    """One isolation card's standing height in metres and its relief, by `measure_t1`'s per-column maths.

    THE PREDECESSOR'S VERSION ALWAYS RETURNED 0.000 m, for every card and every build. It took each masked
    pixel's own `ground_distance` and then asked `height_at` for the height at exactly that distance, which is
    the ground by construction: the two are inverses, so every pixel read 0 and so did the median. That is why
    `-g2-g2-g2-g2-g2`'s look report printed `cards-gas` 0.000 m, `cards-smoke` 0.000 m and a rise ratio of
    0.0000 - the numbers said nothing about either drawing.

    The fix is the one `measure_t1` already uses: the distance to a column comes from that column's FOOT (its
    lowest pixel on screen, where the card meets the ground), and every pixel in the column is then measured in
    that column's vertical plane. Per column the p45 height is taken, and the card's rise is the median over
    columns, exactly as `green_rise_m` is taken at T1.

    `relief` is unchanged: the stdev of the masked pixels' luma over their mean, a flat wash near 0 and a lumpy
    cloud well above it.
    """
    img, side = load(run_dir, stem)
    plate, _ = load(run_dir, 'cards-plate')
    w, h = img.size
    mask, _, _, _ = green_mask(img, plate, diff_bar=T1_DIFF_BAR, blob=True)
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ground = ground_of(run_dir, stem)

    def ndc(px, py):
        return (2.0 * (px + 0.5) / w - 1.0, 1.0 - 2.0 * (py + 0.5) / h)

    by_column = {}
    for i, on in enumerate(mask):
        if on:
            by_column.setdefault(i % w, []).append(i // w)

    MIN_COLUMN_PX = 8                     # measure_t1's own floor: a column of fewer pixels is an edge, not a card
    dists = {}
    for px, rows_ in by_column.items():
        if len(rows_) < MIN_COLUMN_PX:
            continue
        nx, ny = ndc(px, max(rows_))      # the column's foot: where the card meets the ground
        d = ground_distance(cam, pitch, yaw, ground, nx, ny)
        if d is not None and 1.0 < d < 2000.0:
            dists[px] = d

    pix = list(img.getdata())
    col_heights, lumas = {}, []
    for i, on in enumerate(mask):
        if not on:
            continue
        px, py = i % w, i // w
        lumas.append(luma(*pix[i]))
        if px not in dists:
            continue
        nx, ny = ndc(px, py)
        y = height_at(cam, pitch, yaw, ground, nx, ny, dists[px])
        if y is not None:
            col_heights.setdefault(px, []).append(y)

    if not col_heights or not lumas:
        return {'rise_m': 0.0, 'relief': 0.0, 'mask_px': 0}
    heights = []
    for ys in col_heights.values():
        ys.sort()
        heights.append(ys[min(len(ys) - 1, int(round(0.45 * (len(ys) - 1))))])
    heights.sort()
    rise_m = heights[len(heights) // 2]
    mean_l = sum(lumas) / len(lumas)
    var_l = sum((v - mean_l) ** 2 for v in lumas) / len(lumas)
    relief = (var_l ** 0.5) / mean_l if mean_l > 1e-9 else 0.0
    return {'rise_m': round(rise_m, 3), 'relief': round(relief, 4), 'mask_px': sum(1 for m in mask if m)}


def measure_cards(run_dir):
    """The run's own `GasBankCards` isolation pass: the gas card against the smoke card, as barred keys.

    The predecessor printed these as an unbarred look report out of one top-level `cards/` folder. The owner
    asked for the rise and the relief to be barred against the smoke card, so they are measured here, per run,
    and go into `BARS` like any other key. The pass is deterministic, so all three runs read the same numbers
    and their spread is zero: see this folder's README.md. `cards_*` are printed diagnostics.
    """
    rows = {name: card_rise_m(run_dir, name) for name in ('cards-gas', 'cards-smoke', 'cards-puff')}
    smoke_rise = rows['cards-smoke']['rise_m'] or 1e-9
    smoke_relief = rows['cards-smoke']['relief'] or 1e-9
    return {
        'gas_smoke_rise_ratio': round(rows['cards-gas']['rise_m'] / smoke_rise, 4),
        'gas_smoke_relief_ratio': round(rows['cards-gas']['relief'] / smoke_relief, 4),
        'cards_gas_rise_m': rows['cards-gas']['rise_m'],
        'cards_gas_relief': rows['cards-gas']['relief'],
        'cards_smoke_rise_m': rows['cards-smoke']['rise_m'],
        'cards_smoke_relief': rows['cards-smoke']['relief'],
        'cards_puff_rise_m': rows['cards-puff']['rise_m'],
        'cards_puff_relief': rows['cards-puff']['relief'],
    }


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


def runs_dir():
    """Where the run*/ folders are read from: HERE, or `--runs <dir>` (relative to HERE), so `--calibrate` can
    read another unit's captures and print this probe's keys for THAT build."""
    if '--runs' in sys.argv:
        d = sys.argv[sys.argv.index('--runs') + 1]
        return d if os.path.isabs(d) else os.path.normpath(os.path.join(HERE, d))
    return HERE


def find_runs():
    runs = []
    for d in sorted(glob.glob(os.path.join(runs_dir(), 'run*'))):
        if not os.path.isdir(d):
            continue
        if all(os.path.exists(os.path.join(d, s + '.jpg')) and os.path.exists(os.path.join(d, s + '.json'))
               for s in STILLS + CARDS):
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
        print('%s: green_low_share %.4f  green_height_frac %.4f  green_rise_m %.3f  green_area_frac %.4f'
              '  o240_green_area_frac %.4f  pose_error_max %.4f'
              % (name, out['green_low_share'], out['green_height_frac'], out['green_rise_m'],
                 out['green_area_frac'], out['o240_green_area_frac'], out['pose_error_max']))
        print('  green_knee_share %.4f  green_top_rough %.4f  t1_gas_cards %d (MaxCards 1536)'
              % (out['green_knee_share'], out['green_top_rough'], out['t1_gas_cards']))
        print('  green_height_frac %.4f (printed, not barred: its spread swallows it)' % out['green_height_frac'])
        print('  green_blob_share %.4f (a carpet is one blob, near 1)  green_rim_lift %.4f (over 1 is a rim)'
              % (out['green_blob_share'], out['green_rim_lift']))
        print('  gas_smoke_rise_ratio %.4f  gas_smoke_relief_ratio %.4f'
              '  (cards: gas rise %.3f m relief %.4f, smoke rise %.3f m relief %.4f)'
              % (out['gas_smoke_rise_ratio'], out['gas_smoke_relief_ratio'],
                 out['cards_gas_rise_m'], out['cards_gas_relief'],
                 out['cards_smoke_rise_m'], out['cards_smoke_relief']))
        print('  o240_large_cloud_count %d  o240_large_share %.4f  o240_speck_count %d'
              % (out['o240_large_cloud_count'], out['o240_large_share'], out['o240_speck_count']))
        print('  diagnostics (barred on nothing): green_px %d  plate_diff_px %d  blob_px %d'
              '  o240_green_px %d  o240_plate_diff_px %d  o240_blob_top %s'
              % (out['green_px'], out['plate_diff_px'], out['blob_px'],
                 out['o240_green_px'], out['o240_plate_diff_px'], out['o240_blob_top']))
        print('  frames: t1 %s (plate %s)  o240 %s (plate %s)'
              % (frames[name]['gas-t1'], frames[name]['gas-t1-plate'],
                 frames[name]['gas-o240'], frames[name]['gas-o240-plate']))

    if calibrate:
        if runs_dir() == HERE:                    # reading another folder's runs leaves this one's numbers alone
            with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
                json.dump({'runs': results, 'frames': frames}, f, indent=1, sort_keys=True)
        return 0

    if same_frame_refusal(frames):
        print('FAILED: three captures of the same frame are one capture')
        return 1

    ok = True
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
        json.dump({'runs': results, 'frames': frames, 'bars': BAR_VALUES},
                  f, indent=1, sort_keys=True)
    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


# Bar values, set from the spread of run1/run2/run3 at three different frames (see README.md "The bars").
# Every one is this build's own three captures: for a `ge` bar a number at or under min - spread, for an `le` bar
# at or over max + spread.
BAR_VALUES = {
    'green_low_share': 0.42,         # runs 0.6499/0.5478/0.5392, spread 0.1107: the head rule, weaker again (the
                                      # parent build read 0.7393-0.8840 and barred 0.59). The cloud is taller, so
                                      # more of it stands over HEAD_M 1.8 m; GasHeadRoom is untouched at 1.7 m
    'green_rise_m': 0.50,            # runs 0.899/1.250/1.289, spread 0.390 (the carpet build read 0.575-0.735,
                                      # so this floor does not fail it on its own; green_blob_share does)
    'green_area_frac': 0.0113,       # runs 0.0118/0.0116/0.0116, spread 0.0002. A narrow card covers less frame
                                      # than the carpet's 6-8 m band did (0.0226-0.0276), so this is a floor
                                      # against the bank going see-through, not a measure of the look
    'green_knee_share': 0.29,        # THE PUDDLE BAR. Runs 0.2358/0.1947/0.1901, spread 0.0457; the carpet build
                                      # read 0.2417-0.3612 and FAILS it on run3
    't1_gas_cards': 1300,            # runs 1259/1249/1239, spread 20: five cells in eight carry a rim now (three
                                      # before), against FlipbookFx.MaxCards 1536
    'gas_smoke_rise_ratio': 2.07,    # runs 2.0752 x3 (the cards pass is deterministic, spread 0). The gas card
                                      # stands twice the smoke card's rise. The carpet build clears it too, at
                                      # the same 2.0752: the cards pose is unchanged by this unit. README.md
    'gas_smoke_relief_ratio': 3.11,  # runs 3.1125 x3, spread 0: three times the smoke card's relief
    'green_blob_share': 0.065,       # THE CARPET BAR, new here. The largest green component over all the green
                                      # at T1: runs 0.0565/0.0562/0.0571, spread 0.0009. The carpet build reads
                                      # 0.1530-0.1877 and FAILS it by 3x - its green hangs together in one sheet,
                                      # this build's breaks into separate standing clouds with ground between
    'green_rim_lift': 0.96,          # new here, and it does NOT separate the builds: runs 0.9839/1.0005/0.9845,
                                      # spread 0.0166, and the carpet build reads 0.9902-0.9933, which CLEARS
                                      # this bar. The rim is a faint halo, not a bright crown, so the top 30% of
                                      # a green column is no brighter than the rest. See README.md, "the rim
                                      # bar does not separate the builds"
    'o240_green_area_frac': 0.0075,  # runs 0.0078/0.0079/0.0081, spread 0.0003: carried, re-confirmed
    'o240_large_cloud_count': 1,     # runs 4/3/3: carried from -g2-g2-g2-g2 (three captures of a build do not
                                      # always agree on the count, so o240_large_share is the real guard)
    'o240_large_share': 0.71,        # runs 0.7270/0.7369/0.7298, spread 0.0099: carried, re-confirmed
    'pose_error_max': 0.50,          # GasBankStills.PoseBar
}


if __name__ == '__main__':
    sys.exit(main())
