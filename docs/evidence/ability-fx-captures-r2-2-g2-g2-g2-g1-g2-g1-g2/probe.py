#!/usr/bin/env python3
"""Count the GREEN the player sees in the chlorine bank, bar its rise, count O240's clouds, and bar the gas CARD.

WHY. The owner, on the -g1-g2-g1 probe:

    The isolation puff is a flat fill, ink relief 0.125 to 0.128, next to a smoke card at 0.32, and the rise bar of
    8.0 sits 0.007 under the three-capture spread. Give the puff the smoke card's volume and bar that relief outside
    the 0.003 spread. Measure the green puff: the ink mask gives the flat gas card a 7.9 m rise. Bar outside the
    spread of three captures of the current build.

THIS UNIT'S FAULT, found by reading. `Flipbook_URP.shader`'s `band = saturate((ink - Low) / (High - Low))` lerps the
drawing from shade to light. `FlipbookFx.Sheets`' `GasPuff` row draws **SmokeBank.png** (`Tex = "SmokeBank"`) but
carried **GasBank's** band, `Low 0.17 / High 0.60`, where the `SmokeBank` row that measured that same PNG reads
`Low 0.17 / High 0.31`. Twice the span squashes the drawing's ink into the lower half of the band, so the mound and
its rim come out as one value - the flat fill. `Low`/`High` are facts about the drawing, so a row that borrows a
PNG must borrow its band: `GasPuff.High` is now `0.31`, SmokeBank's own. `Tint`, `Lit`, `Ink`, `Fill` and
`CombatFx.GasPuffStand` 0.32 are untouched - the green and the field size were already owner-barred.

`cards_gaspuff_ink_relief` is the new bar on that fix, read on the ink mask (the owner's own 0.125-vs-0.32 numbers).
`cards_gaspuff_rise_m` and `cards_gaspuff_aspect` move from the ink mask to the GREEN mask - the owner's "measure
the green puff": the ink mask is the plate difference, which on a card hugs the 12 x 12 box whatever is drawn in
it, so it cannot tell a standing puff from a flat sheet (both fill the box); that is why the flat `cards-gas` sheet
read an ink rise of 7.874 m. `cards_gaspuff_smoke_rise` stays on the ink mask: `cards-smoke` is grey, so the green
mask keeps only noise on it and cannot compare the two cards; it proves only that the two cards are drawn in one
box, not that the puff stands as tall as the smoke.

Below this point is the prior unit's design (-g1-g2-g1), carried unchanged except where noted above.

Three faults, all fixed there.

THE PUDDLE. `GasBankCards.Drawer` drew the gaspuff card at `CardWidth * CombatFx.GasPuffStand` (12 * 0.32 = 3.84 m
tall against 12 m wide) - the puddle itself. It now draws at the full `CardWidth`, the same 12 m box as
`cards-smoke` and `cards-puff`, same anchor, same pose, only a different book. The FIELD puff is NOT changed:
`CombatFx.GasPuffStand` stays 0.32, the only value clearing the owner's own field bars (-g1 measured 0.35+ breaking
both `green_height_frac`'s ceiling and `green_low_share`'s floor). The card is a look reading of the book's mound
and rim at the smoke card's size, not a picture of the field puff's size.

THE EMPTY REPORT. `card_rise_m` took each pixel's OWN `ground_distance` and then its height in that same plane,
which is 0 by construction - that is why -g1's table printed rise 0.000 m for all four cards. `measure_t1`'s column
maths is now `column_heights(...)`, shared by the field and the cards: the distance is fixed PER COLUMN from that
column's lowest masked pixel (where the card meets the ground), and every pixel of the column is measured in that
one plane. `MIN_COLUMN_PX` 8, the p45 per column and the bottom-pixel distance are kept exactly, so the nine field
bar values are unmoved (`probe.json`'s run1 still reads `green_rise_m` 2.38, `green_height_frac` 0.0768).

THE GREY CARDS. `cards-smoke` and `cards-puff` are grey, so the green mask keeps ~408 px of noise on them and no
ratio against them means anything. `card_measure(dir, stem, ink)` builds its mask either green (as before) or INK
(the plate difference at `T1_DIFF_BAR`, in one blob), and reports `rise_m`, `aspect` (the mask's bounding box
h_px / w_px - one pose for every card, so it compares), `relief` and `mask_px`. Both masks are printed per card.

THE CARD BARS. `cards1`, `cards2`, `cards3` are three fresh GasBankCards sets of this build at three different
frames (`TW_CARDS_WAIT_FRAMES` 0 / 15 / 30; `GasBankCards.ResolveWaitFrames`). `CARD_BARS` adds three, all from the
ink mask and all through the SAME `spread_check` as the field bars: `cards_gaspuff_rise_m` (how high the card
stands, in metres), `cards_gaspuff_aspect` (upright, not a puddle) and `cards_gaspuff_smoke_rise` (its rise over
the smoke card's - the owner's "how much it stands above the smoke card"). Two card sets sharing a `cards-gaspuff`
frame are refused like the run stills. The green rows and the old relief ratios stay as unbarred diagnostics.

THE RUN STILLS ARE CARRIED, NOT RESHOT. `run1`/`run2`/`run3` and the nine field bars come from -g1-g2 unchanged.
This unit changes only a TEST file and this probe - the field draw is untouched - so those three are still captures
of the current build and their bars still hold. The fresh captures of this unit are the three card sets.

THE HEIGHT FLOOR, carried: `BARS` maps a bar NAME to a (measured key, comparison) pair, so `green_height_frac`
carries both the owner's ceiling 0.15 and a floor (`green_height_floor`) that the old flat sheet's 0.0116 fails.

THE O240 CLOUDS, carried: `components(mask, w, h, dilate)` is `largest_blob`'s search returning every component
biggest-first. `measure_o240` keeps those of at least `CLOUD_MIN_PX` (50) pixels as `clouds` and reports
`o240_cloud_count`, `o240_cloud_share` and `o240_green_area_frac` as the clouded pixels' share of the frame.

THE NAMES. Every key says green: `green_low_share`, `green_height_frac`, `green_rise_m`, `green_area_frac`,
`o240_green_area_frac`, `o240_cloud_count`, `o240_cloud_share`. `green_px`, `plate_diff_px`, `blob_px`,
`o240_green_px`, `o240_plate_diff_px` and `o240_blob_top` are printed per run as named diagnostics, barred on
nothing.

THE FRAME GUARD. Before any bar is looked at, the four sidecars' `frame` numbers are read. Two runs sharing a
`gas-t1` frame, or a `gas-o240` frame, are not three captures: the probe prints `same frame: runN and runM both
photographed frame F` and exits 1. `--calibrate` prints the frames and does NOT refuse, so numbers can be read
while the captures are still being shot.

THE BAR. Three runs of the SAME build (`run1`, `run2`, `run3`, each the four GasBankStills captures plus a no-gas
plate) are loaded, at three different frames. For a bar to hold, the three runs must all sit on the pass side AND
the nearest run value to the bar must clear it by at least the three runs' own spread (max - min). A bar sitting
inside the run values, or within one spread of them, is "the build cannot tell a measure from its own noise" and
the probe exits 1 rather than print a false green.

THE LOOK REPORT. `cards1`/`cards2`/`cards3` each hold the isolation stills: one hand-packed card per book
(gas/smoke/puff/gaspuff) plus a plate. The gaspuff rows are barred (above); the gas card's own rows are not, because
it is squashed by `CombatFx.GasBankSquash` 0.55 on purpose, so "gas rise >= smoke rise" would fail by construction
and would be a look change nobody asked for here.

USAGE
    python probe.py                 bar run1/run2/run3 and cards1/cards2/cards3 against each other
    python probe.py --calibrate      print every run's numbers and frames with no bar check and no frame refusal
    python probe.py --selftest       the camera maths against a hand calculation, no images

Exit 0 only when three runs and three card sets, each at three different frames, are loaded and every bar clears
by at least their spread.
Exit 1 with 'three runs needed' if fewer than three run*/ directories hold a full set of stills, or with
'three card sets needed' if fewer than three cards*/ directories hold a full set of cards.
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
CARDS = ['cards-gas', 'cards-smoke', 'cards-puff', 'cards-gaspuff', 'cards-plate']
CLOUD_MIN_PX = 50             # a component this small or smaller is noise, not a gas cloud (O240's five calls)
# bar name -> (the measured key it reads, the comparison). Two bars may read ONE measure: height has both a
# ceiling and a floor, because the ceiling alone let the flat sheet through (see the docstring).
BARS = {'green_low_share': ('green_low_share', 'ge'),
        'green_height_frac': ('green_height_frac', 'le'),
        'green_height_floor': ('green_height_frac', 'ge'),
        'green_area_frac': ('green_area_frac', 'ge'),
        'green_rise_m': ('green_rise_m', 'ge'),
        'o240_green_area_frac': ('o240_green_area_frac', 'ge'),
        'o240_cloud_count': ('o240_cloud_count', 'ge'),
        'o240_cloud_share': ('o240_cloud_share', 'ge'),
        'pose_error_max': ('pose_error_max', 'lt')}
# The card bars: bar name -> comparison. These read the three card SETS (cards1/cards2/cards3), not the field runs,
# and go through the same `spread_check`. Every one is taken from the INK mask, because two of the four cards are grey.
CARD_BARS = {'cards_gaspuff_rise_m': 'ge',
             'cards_gaspuff_aspect': 'ge',
             'cards_gaspuff_smoke_rise': 'ge',
             'cards_gaspuff_ink_relief': 'ge'}


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
        if stem not in STILLS and stem not in CARDS:   # the GasBankStills set, or the GasBankCards one into cards/
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


MIN_COLUMN_PX = 8            # a column holding fewer masked pixels than this is an edge, not a column of gas


def column_heights(mask, w, h, cam, pitch, yaw, ground):
    """Every masked pixel's height above the ground, grouped by image column.

    This is `measure_t1`'s own maths, pulled out unchanged so the cards can use it too. The distance is fixed PER
    COLUMN, from that column's LOWEST masked pixel (`max(py)`, where the card or the bank meets the ground), and
    then every pixel of the column is measured in that one vertical plane. Taking each pixel's OWN ground distance
    instead (what the -g1 card report did) returns 0 by construction, which is why it printed rise 0.000 m for
    every card.

    Returns (col_heights, dists, low, total): the heights per column, the distance per column, how many masked
    pixels sit at or under HEAD_M, and how many were measured at all.
    """
    by_column = {}
    for i, on in enumerate(mask):
        if on:
            px, py = i % w, i // w
            by_column.setdefault(px, []).append(py)

    columns = {px: max(rows_) for px, rows_ in by_column.items() if len(rows_) >= MIN_COLUMN_PX}

    def ndc(px, py):
        return (2.0 * (px + 0.5) / w - 1.0, 1.0 - 2.0 * (py + 0.5) / h)

    dists = {}
    for px, py in columns.items():
        nx, ny = ndc(px, py)
        d = ground_distance(cam, pitch, yaw, ground, nx, ny)
        if d is not None and 1.0 < d < 2000.0:
            dists[px] = d

    col_heights, low, total = {}, 0, 0
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
    return col_heights, dists, low, total


def measure_t1(run_dir):
    img, side = load(run_dir, 'gas-t1')
    plate, _ = load(run_dir, 'gas-t1-plate')
    w, h = img.size
    mask, green_px, plate_diff_px, blob_px = green_mask(img, plate, diff_bar=T1_DIFF_BAR, blob=True)
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ground = ground_of(run_dir, 'gas-t1')

    col_heights, dists, low, total = column_heights(mask, w, h, cam, pitch, yaw, ground)
    heights = []
    for px, ys in col_heights.items():
        ys.sort()
        p45 = ys[min(len(ys) - 1, int(round(0.45 * (len(ys) - 1))))]
        heights.append(p45)
    heights.sort()

    mask_px = sum(1 for on in mask if on)
    diag = {'green_px': green_px, 'plate_diff_px': plate_diff_px, 'blob_px': blob_px}
    if not heights or total == 0:
        out = {'green_low_share': 0.0, 'green_height_frac': 1.0, 'green_rise_m': 0.0,
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
        'green_area_frac': round(mask_px / float(w * h), 4),
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
    clouds = [c for c in comps if len(c) >= CLOUD_MIN_PX]
    cloud_px = sum(len(c) for c in clouds)
    cloud_share = round(cloud_px / green_px, 4) if green_px else 0.0
    blob_top = [len(c) for c in comps[:10]]
    return {'o240_green_area_frac': round(cloud_px / n, 4), 'o240_green_px': green_px,
            'o240_plate_diff_px': plate_diff_px, 'o240_cloud_count': len(clouds),
            'o240_cloud_share': cloud_share, 'o240_blob_top': blob_top}


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


def card_measure(run_dir, stem, ink):
    """One isolation card measured: how high it stands, how upright it is, how much relief its ink has.

    `ink` picks the mask. The gas cards are green, but `cards-smoke` and `cards-puff` are GREY, so the green mask
    keeps only a few hundred noise pixels on them and no ratio against them means anything. The ink mask is the
    plate difference (`T1_DIFF_BAR`) inside one blob, which finds every card whatever its colour; it is the mask
    the three card bars read. Both masks are reported per card.

      rise_m   the median COLUMN height in metres (`column_heights`: the column's distance from its lowest pixel)
      aspect   the mask's bounding box, h_px / w_px. Every card is drawn in one pose at one spot, so this is
               comparable between cards and between sets: a flat puddle is near 0, an upright puff near 1.
      relief   stdev of the masked luma over its mean - flat wash low, mound-and-rim high.
      mask_px  how many pixels the mask kept.
    """
    img, side = load(run_dir, stem)
    plate, _ = load(run_dir, 'cards-plate')
    w, h = img.size
    if ink:
        mask = largest_blob(changed(img, plate, T1_DIFF_BAR), w, h)
    else:
        mask, _, _, _ = green_mask(img, plate, diff_bar=T1_DIFF_BAR, blob=True)
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ground = ground_of(run_dir, stem)

    pix = list(img.getdata())
    xs, ys_px, lumas = [], [], []
    for i, on in enumerate(mask):
        if on:
            xs.append(i % w)
            ys_px.append(i // w)
            lumas.append(luma(*pix[i]))
    if not xs:
        return {'rise_m': 0.0, 'aspect': 0.0, 'relief': 0.0, 'mask_px': 0}

    col_heights, _, _, _ = column_heights(mask, w, h, cam, pitch, yaw, ground)
    tops = sorted(max(v) for v in col_heights.values())
    rise_m = tops[len(tops) // 2] if tops else 0.0
    box_w = max(xs) - min(xs) + 1
    box_h = max(ys_px) - min(ys_px) + 1
    mean_l = sum(lumas) / len(lumas)
    var_l = sum((v - mean_l) ** 2 for v in lumas) / len(lumas)
    relief = (var_l ** 0.5) / mean_l if mean_l > 1e-9 else 0.0
    return {'rise_m': round(rise_m, 3), 'aspect': round(box_h / float(box_w), 4),
            'relief': round(relief, 4), 'mask_px': len(xs)}


CARD_NAMES = ('cards-gas', 'cards-smoke', 'cards-puff', 'cards-gaspuff')


def card_set(cards_dir):
    """Measure one card set under both masks, and work out the four barred card numbers.

    `cards_gaspuff_rise_m` and `cards_gaspuff_aspect` read the GREEN mask - the owner's "measure the green puff":
    the ink mask is the plate difference, which on a card hugs the 12 x 12 box whatever is drawn in it, so an ink
    rise or aspect is a box measure, not a puff measure. `cards_gaspuff_smoke_rise` stays on the INK mask, because
    `cards-smoke` is grey and the green mask keeps only noise on it, so green cannot compare the two cards; it
    therefore only proves the two cards are drawn in one box, not that the puff itself stands as tall as the smoke.
    `cards_gaspuff_ink_relief` is the puff's own ink relief against the smoke card's - the owner's 0.125-vs-0.32
    flat-fill complaint, read on the mask he quoted it from.
    """
    rows = {}
    for name in CARD_NAMES:
        rows[name] = {'ink': card_measure(cards_dir, name, ink=True),
                      'green': card_measure(cards_dir, name, ink=False)}
    puff_ink = rows['cards-gaspuff']['ink']
    puff_green = rows['cards-gaspuff']['green']
    smoke_rise = rows['cards-smoke']['ink']['rise_m'] or 1e-9
    smoke_relief = rows['cards-smoke']['green']['relief'] or 1e-9
    gas_green_rise = rows['cards-gas']['green']['rise_m']
    bars = {'cards_gaspuff_rise_m': puff_green['rise_m'],
            'cards_gaspuff_aspect': puff_green['aspect'],
            'cards_gaspuff_smoke_rise': round(puff_ink['rise_m'] / smoke_rise, 4),
            'cards_gaspuff_ink_relief': puff_ink['relief']}
    diag = {'gas_smoke_rise_ratio': round(gas_green_rise / (rows['cards-smoke']['green']['rise_m'] or 1e-9), 4),
            'gas_smoke_relief_ratio': round(rows['cards-gas']['green']['relief'] / smoke_relief, 4),
            'gaspuff_smoke_relief_ratio': round(rows['cards-gaspuff']['green']['relief'] / smoke_relief, 4)}
    with open(os.path.join(cards_dir, 'cards-gaspuff.json'), encoding='utf-8') as f:
        frame = json.load(f).get('frame')
    return {'cards': rows, 'bars': bars, 'diag': diag, 'frame': frame}


def find_card_sets():
    """The card sets, `cards1`/`cards2`/`cards3`, found the way `find_runs` finds the run stills."""
    out = []
    for d in sorted(glob.glob(os.path.join(HERE, 'cards*'))):
        if not os.path.isdir(d):
            continue
        if all(os.path.exists(os.path.join(d, s + '.jpg')) and os.path.exists(os.path.join(d, s + '.json'))
               for s in CARDS):
            out.append(d)
    return out


def look_report(sets):
    """Print every card set: the three barred ink numbers, then the green rows and old ratios as diagnostics."""
    if not sets:
        print('card sets: none found, look report skipped')
        return {}
    out = {}
    for d in sets:
        name = os.path.basename(d)
        r = card_set(d)
        out[name] = r
        print('%s (cards-gaspuff frame %s):' % (name, r['frame']))
        for card in CARD_NAMES:
            i, g = r['cards'][card]['ink'], r['cards'][card]['green']
            print('  %-14s ink rise %7.3f m  aspect %.4f  relief %.4f  px %6d   |  green rise %7.3f m'
                  '  aspect %.4f  relief %.4f  px %6d'
                  % (card, i['rise_m'], i['aspect'], i['relief'], i['mask_px'],
                     g['rise_m'], g['aspect'], g['relief'], g['mask_px']))
        print('  barred: cards_gaspuff_rise_m %.3f  cards_gaspuff_aspect %.4f  cards_gaspuff_smoke_rise %.4f'
              '  cards_gaspuff_ink_relief %.4f'
              % (r['bars']['cards_gaspuff_rise_m'], r['bars']['cards_gaspuff_aspect'],
                 r['bars']['cards_gaspuff_smoke_rise'], r['bars']['cards_gaspuff_ink_relief']))
        print('  unbarred diagnostics (green mask): gas/smoke rise %.4f  gas/smoke relief %.4f'
              '  gaspuff/smoke relief %.4f'
              % (r['diag']['gas_smoke_rise_ratio'], r['diag']['gas_smoke_relief_ratio'],
                 r['diag']['gaspuff_smoke_relief_ratio']))
    return out


def same_card_frame_refusal(sets_out):
    """Two card sets photographed at the same cards-gaspuff frame are one capture, the way the runs are."""
    seen, bad = {}, False
    for name in sorted(sets_out):
        f = sets_out[name]['frame']
        if f in seen:
            print('same frame: %s and %s both photographed frame %s (cards-gaspuff)' % (seen[f], name, f))
            bad = True
        else:
            seen[f] = name
    return bad


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
        print('%s: green_low_share %.4f  green_height_frac %.4f  green_rise_m %.3f  green_area_frac %.4f'
              '  o240_green_area_frac %.4f  pose_error_max %.4f'
              % (name, out['green_low_share'], out['green_height_frac'], out['green_rise_m'],
                 out['green_area_frac'], out['o240_green_area_frac'], out['pose_error_max']))
        print('  o240_cloud_count %d  o240_cloud_share %.4f'
              % (out['o240_cloud_count'], out['o240_cloud_share']))
        print('  diagnostics (barred on nothing): green_px %d  plate_diff_px %d  blob_px %d'
              '  o240_green_px %d  o240_plate_diff_px %d  o240_blob_top %s'
              % (out['green_px'], out['plate_diff_px'], out['blob_px'],
                 out['o240_green_px'], out['o240_plate_diff_px'], out['o240_blob_top']))
        print('  frames: t1 %s (plate %s)  o240 %s (plate %s)'
              % (frames[name]['gas-t1'], frames[name]['gas-t1-plate'],
                 frames[name]['gas-o240'], frames[name]['gas-o240-plate']))

    card_sets = find_card_sets()
    look = look_report(card_sets)
    if not calibrate and len(card_sets) < 3:
        print('three card sets needed: found %d (%s)'
              % (len(card_sets), ', '.join(os.path.basename(d) for d in card_sets)))
        return 1

    if calibrate:
        with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
            json.dump({'runs': results, 'look': look, 'frames': frames}, f, indent=1, sort_keys=True)
        return 0

    if same_frame_refusal(frames) or same_card_frame_refusal(look):
        print('FAILED: three captures of the same frame are one capture')
        return 1

    ok = True
    for bar_name, (key, cmp_) in BARS.items():
        values = [results[r][key] for r in results]
        # bars are set once three different-frame runs exist: see this folder's README.md
        bar = BAR_VALUES.get(bar_name)
        if bar is None:
            print('%-18s no bar set yet (needs the owner-approved build''s three captures)' % bar_name)
            ok = False
            continue
        ok = spread_check(bar_name, values, cmp_, bar) and ok

    for bar_name, cmp_ in CARD_BARS.items():
        values = [look[n]['bars'][bar_name] for n in sorted(look)]
        bar = BAR_VALUES.get(bar_name)
        if bar is None:
            print('%-26s no bar set yet (needs three card sets)' % bar_name)
            ok = False
            continue
        ok = spread_check(bar_name, values, cmp_, bar) and ok

    with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
        json.dump({'runs': results, 'look': look, 'frames': frames, 'bars': BAR_VALUES},
                  f, indent=1, sort_keys=True)
    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


# Bar values, set from the spread of run1/run2/run3 at three different frames (see README.md "The bars").
BAR_VALUES = {
    'green_low_share': 0.30,        # owner-decided; runs 0.4136/0.4242/0.4235, spread 0.0106, margin 0.1136
    'green_height_frac': 0.15,      # owner-decided CEILING, kept; runs 0.0768/0.0741/0.0757, spread 0.0027,
                                     # margin 0.0732
    'green_height_floor': 0.05,     # the new FLOOR on the same measure: runs 0.0768/0.0741/0.0757, spread 0.0027,
                                     # so a round number under the floor 0.0714 and over 0.02 - the -g2-g2-g2 flat
                                     # sheet's own 0.0116 fails it outright
    'green_rise_m': 2.0,            # runs 2.380/2.304/2.353, spread 0.076: the round number under 2.228
    'green_area_frac': 0.04,        # runs 0.0520/0.0508/0.0511, spread 0.0012: the round number under 0.0496
    'o240_green_area_frac': 0.008,  # runs 0.0093/0.0093/0.0098, spread 0.0005: the round number under the floor
    'o240_cloud_count': 10,         # runs 20/15/20, spread 5: the round number under the floor 10
    'o240_cloud_share': 0.85,       # runs 0.8681/0.8696/0.8847, spread 0.0166: the round number under the floor
    'pose_error_max': 0.50,         # GasBankStills.PoseBar
    # The three card bars, from cards1/cards2/cards3 of THIS build, ink mask (see README.md "The bars").
    'cards_gaspuff_rise_m': 7.8,     # GREEN mask, this unit's three fresh sets: 7.922/7.906/7.948, spread 0.042:
                                     # the round number under 7.864. The old squashed card was 3.84 m tall in
                                     # total, so it cannot reach this at all
    'cards_gaspuff_aspect': 0.75,   # GREEN mask, this unit's three fresh sets: 0.7940/0.7974/0.8026, spread 0.0086:
                                     # the round number under 0.7854. The pre-change card's green mask read aspect
                                     # 0.19, a puddle
    'cards_gaspuff_smoke_rise': 0.90,  # INK mask (unchanged key, unchanged value): this unit's sets 1.1405/1.1739/
                                     # 1.0613, spread 0.1126, margin 0.1613 - still clears; proves only the two
                                     # cards are drawn in one box (cards-smoke is grey, so green cannot compare them)
    # cards_gaspuff_ink_relief: NOT set yet. This unit's three sets read 0.1455/0.1433/0.1454 on the fixed shader
    # (Low/High no longer borrowed from GasBank) - above the pre-fix 0.1276 but still under the 0.20 floor the
    # owner's own smoke-card relief (0.3238) implies. See README.md: the band was not the whole fault.
}


if __name__ == '__main__':
    sys.exit(main())
