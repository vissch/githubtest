#!/usr/bin/env python3
"""Measure the chlorine gas bank in metres, and bar it against the SPREAD of three repeat captures.

WHY. The build this probe bars is the gas bank's rewrite (CombatFx.Banks.cs, commit "wide overlapping cards at
low alpha, broken top/bright/roll"). g2's probe passed the ZEBRA: "at zoom 30 at night the play frame is a zebra of
flat yellow-green cards, not a chlorine bank" (the owner, on run1/run2/run3's gas-t1.jpg, all frame 804). Every bar
g2 set — the bank lies low, it is not tall, it covers area at O240, the pose is right — is true of a zebra too, so
none of them could see the complaint. This probe keeps g2's camera maths, mask, bars and three-run spread rule and
adds the numbers that DO see it, all three counted on the PAINT (see `paint_stats`): `paint_frac` (how much of the
frame the gas paints), `bank_paint_share` (how much of that paint sits in ONE patch) and `bank_patch_count` (how
many paint patches of 200 px or more there are at all). One bank a player can point at is most of the paint in one
patch and few patches; a zebra of separate cards is the opposite. g1 divided by the raw plate difference instead
(`bank_patch_share`, 4,324 px of 46,702) and so measured mostly infantry motion and JPEG noise; that key is gone.
Re-measured by this probe the zebra reads `bank_paint_share` 0.0782 and `bank_patch_count` 40 against this build's
0.1622-0.1936 and 7-13 — `--against` re-measures g2's own runs and must exit 1, because a bar the zebra also
clears proves nothing.

THE FRAME. g1's three runs were all t1 `sim_tick` 557 and two of its stills were the same file, so its 0.0001
spread was JPEG jitter and every bar looked safe. Here the three runs stand 40 ticks apart (557 / 597 / 637, the
`gasOffsetFrames` knob in GasBankStills) and `one_frame_fault` refuses the evidence — 'three captures, one frame',
exit 1, before a bar is read — if two runs share a t1 tick or any two stills are byte-identical. The price is an
honest spread: the bank rises as it ages, so `bank_low_share` runs 0.4371-0.2584 and its bar had to drop to 0.07.

THE MASK. r2-2's mask was any pixel that changed from the plate by >= 12-20 RGB, full stop — "any plate-difference
blob counts" (the owner's own words for why that is wrong: the six moving infantry and JPEG block noise are also
plate differences, anywhere in the frame). This probe adds one more requirement: the pixel must also be BRIGHTER
than the plate (Rec.709 luma of the shot minus the plate's luma >= `LUMA_BAR`). The bank is a lit translucent card
over a dark night field, so it LIFTS luma where it sits; infantry motion and JPEG noise change luma both ways.
`mask_px_raw` (the old plate-difference count) and `mask_px` (after the luma lift) are both printed per run, so
the cut this makes is visible, not asserted.

THE NAMES. `gas_t1_green_low_share` is renamed `bank_low_share` — it was never a colour test, even in r2-2, and
calling it "green" is exactly the owner's complaint ("what the probe calls gas does not read as gas"). The
duplicate `gas_o240_green_area_frac` key (same number as `gas_o240_effect_area_frac` under a second name) is
dropped. `gas_hue_window_px` is kept, unbarred: it is the plate-difference pixels that ALSO fall in the old
45-95 PIL hue window, a 50 px wide band — the owner called this "the hue window is 30 px" meaning it is a colour
test wide enough to pass almost anything; whatever its exact width, no run below ever bars on it.

THE BAR. Three runs of the SAME build (`run1`, `run2`, `run3`, each the four GasBankStills captures plus a
no-gas plate) are loaded. For a bar to hold, the three runs must all sit on the pass side AND the nearest run
value to the bar must clear it by at least the three runs' own spread (max - min). A bar sitting inside the run
values, or within one spread of them, is "the build cannot tell a measure from its own noise" and the probe exits
1 rather than print a false green. With fewer than three runs it exits 1 before looking at any bar at all: there
is no partial credit for one capture, which is what let r2-2's single JPEG stand in for a measure.

THE LOOK REPORT. `cards/` holds the r2-2 isolation stills (one hand-packed card per book, gas/smoke/puff, plus a
plate). These are never barred — the gas card is squashed by `CombatFx.GasBankSquash` 0.55 on purpose, so "gas
rise >= smoke rise" would fail by construction and would be a look change nobody asked for here. Instead this
prints, per card, its rise in metres and its relief (stdev of masked luma / its mean), and the gas/smoke ratio of
each: the owner's "cards-smoke.jpg is the volume this pose can already draw" becomes two numbers for his next call
on the look, not a pass bar.

USAGE
    python probe.py                 bar run1/run2/run3 against each other, print the cards/ look report
    python probe.py --calibrate      print every run's numbers with no bar check (even with only one run loaded)
    python probe.py --selftest       the camera maths against a hand calculation, no images
    python probe.py --against DIR     measure another evidence folder's runs with THIS probe (always exits 1)

Exit 0 only when three runs are loaded and every bar clears its run values by at least their spread.
Exit 1 with 'three runs needed' if fewer than three run*/ directories hold a full set of stills.
Exit 1 with 'three captures, one frame' if two runs stand on one frame, or two stills are the same file.
Exit 1 with FAILED if a bar does not clear the run values by at least their spread.
"""
import glob
import hashlib
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
DIFF_BAR = 12                 # RGB distance from the plate that counts as changed (O240 area frac, hue window)
T1_DIFF_BAR = 20               # T1's own, stricter plate-difference bar (carried from r2-2)
LUMA_BAR = 2.0                  # the shot must be at least this much brighter (Rec.709 luma) than the plate
BLOB_DILATE = 1               # largest_blob's bridge width (r2-2: 2 chained the bank into terrain-edge noise)
HUE_LO, HUE_HI, SAT_BAR, VAL_BAR = 45, 95, 50, 30      # the old hue window, kept only as an unbarred row
STILLS = ['gas-t1', 'gas-t1-plate', 'gas-o240', 'gas-o240-plate']
CARDS = ['cards-gas', 'cards-smoke', 'cards-puff', 'cards-plate']
BARS = {'bank_low_share': 'ge', 'bank_height_frac': 'le', 'o240_area_frac': 'ge', 'pose_error_max': 'lt'}
PATCH_MIN_PX = 200            # a paint patch this big or bigger counts as its own patch (a card)
BARS['paint_frac'] = 'ge'           # the paint in the frame: how much of 1280x720 the bank actually covers
BARS['bank_paint_share'] = 'ge'     # one bank: most of the frame's PAINT sits in ONE patch
BARS['bank_patch_count'] = 'le'     # one bank: few paint patches at all (the zebra is many separate cards)


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


def blobs(mask, w, h, dilate=BLOB_DILATE):
    """Every coherent patch in `mask`, as a list of member-index lists, biggest first.

    g2's `largest_blob` is this function keeping only the biggest patch; it is split out here because the zebra
    test needs the OTHER patches too: how many there are, and how much of the paint the biggest one holds.
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
    found = []
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
        found.append(members)
    found.sort(key=len, reverse=True)
    return found


def largest_blob(mask, w, h, dilate=BLOB_DILATE):
    """Keep only the mask pixels inside the single biggest coherent patch (g2's behaviour, now via `blobs`)."""
    found = blobs(mask, w, h, dilate)
    best_members = max(found, key=len) if found else []
    keep = bytearray(w * h)
    for j in best_members:
        keep[j] = 1
    return [bool(mask[i]) and keep[i] for i in range(w * h)]


def changed(img, plate, bar=DIFF_BAR):
    """Mask of pixels whose RGB distance from the plate is `bar` or more, as a flat list of bools."""
    a, b = list(img.getdata()), list(plate.getdata())
    bar2 = bar * bar
    return [((p[0] - q[0]) ** 2 + (p[1] - q[1]) ** 2 + (p[2] - q[2]) ** 2) >= bar2 for p, q in zip(a, b)]


def brighter(img, plate, luma_bar=LUMA_BAR):
    """Mask of pixels whose luma is at least `luma_bar` ABOVE the plate's: the bank lifts luma, noise does not."""
    a, b = list(img.getdata()), list(plate.getdata())
    return [(luma(*p) - luma(*q)) >= luma_bar for p, q in zip(a, b)]


def bank_mask(img, plate, bar=DIFF_BAR, blob=False):
    """The bank: the coherent plate-difference patch, kept only where it is also brighter than the plate.

    Applying the luma lift BEFORE `largest_blob` was tried first (what the owner described) and collapsed T1's
    patch from 14,805 px to 3,105: a translucent card has its own interior shading (tracer streaks, overlapping
    sprite edges), so plenty of its own pixels sit DARKER than the plate even though the card is there, and
    stripping them first breaks the patch into fragments `largest_blob` cannot bridge. `largest_blob` is run on
    the plate difference alone (r2-2's proven, coherent-patch test) and the brightness lift is applied to what it
    finds, not to what finds it: within the one patch the difference test already located, only the pixels that
    are also brighter than the plate count as the bank. See README.md, "The mask".
    """
    diff = changed(img, plate, bar)
    w, h = img.size
    found = blobs(diff, w, h) if blob else []
    if blob:
        keep = bytearray(w * h)
        for j in (found[0] if found else []):
            keep[j] = 1
        patch = [bool(diff[i]) and keep[i] for i in range(w * h)]
    else:
        patch = diff
    n_patches = sum(1 for m in found if len(m) >= PATCH_MIN_PX)
    lift = brighter(img, plate)
    return ([d and l for d, l in zip(patch, lift)], sum(1 for d in diff if d),
            sum(1 for d in patch if d), n_patches)


def paint_stats(img, plate, bar=T1_DIFF_BAR):
    """The PAINT: every pixel of the whole frame that differs from the plate AND is brighter than it.

    g1 barred `bank_patch_share`, the biggest patch's share of the UNLIFTED plate difference, and that is the
    fault this probe fixes: at `gas_hue_window_px` 1 the barred patch was 4,324 px of about 46,700 plate-difference
    pixels, so 90% of what the bar divided by was infantry motion and JPEG block noise, not gas. Here the
    denominator is the paint itself (difference AND brighter: the bank lifts luma, noise changes it both ways),
    taken over the WHOLE frame with no patch restriction, so `paint_frac` is literally how much of the frame the
    gas paints. The patches are counted on the paint mask too, not on the plate difference.
    """
    diff = changed(img, plate, bar)
    lift = brighter(img, plate)
    paint = [d and l for d, l in zip(diff, lift)]
    w, h = img.size
    sizes = [sum(1 for j in m if paint[j]) for m in blobs(paint, w, h)]
    paint_px = sum(1 for p in paint if p)
    biggest = max(sizes) if sizes else 0
    return {
        'paint_px': paint_px,
        'paint_frac': round(paint_px / float(w * h), 4),
        'bank_paint_share': round(biggest / float(max(paint_px, 1)), 4),
        'bank_patch_count': sum(1 for s in sizes if s >= PATCH_MIN_PX),
    }


def hue_window_count(img, plate):
    """How many of the plate-difference pixels also fall in the old hue window (unbarred: see module docstring)."""
    diff = changed(img, plate)
    hsv = list(img.convert('HSV').getdata())
    return sum(1 for d, (h, s, v) in zip(diff, hsv)
               if d and HUE_LO <= h <= HUE_HI and s >= SAT_BAR and v >= VAL_BAR)


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
        if stem not in STILLS:
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


def measure_t1(run_dir):
    img, side = load(run_dir, 'gas-t1')
    plate, _ = load(run_dir, 'gas-t1-plate')
    w, h = img.size
    mask, raw_count, patch_count, _ = bank_mask(img, plate, bar=T1_DIFF_BAR, blob=True)
    paint = paint_stats(img, plate)
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
    if not heights or total == 0:
        out = {'bank_low_share': 0.0, 'bank_height_frac': 1.0, 'bank_rise_m': 0.0,
               'bank_area_frac': round(mask_px / float(w * h), 4),
               'mask_px': mask_px, 'mask_px_raw': raw_count, 'mask_px_patch': patch_count,
               'gas_hue_window_px': hue_window_count(img, plate)}
        out.update(paint)
        return out
    med_h = heights[len(heights) // 2]
    dvals = sorted(dists.values())
    med_d = dvals[len(dvals) // 2]
    frame_h = 2.0 * med_d * math.tan(math.radians(FOV_V_DEG * 0.5))
    row = {
        'bank_low_share': round(low / float(total), 4),
        'bank_height_frac': round(med_h / frame_h, 4),
        'bank_rise_m': round(med_h, 3),
        'bank_area_frac': round(mask_px / float(w * h), 4),
        'mask_px': mask_px,
        'mask_px_raw': raw_count,
        'mask_px_patch': patch_count,
        'gas_hue_window_px': hue_window_count(img, plate),
    }
    # the zebra test, on the paint: how much of it sits in ONE patch, and how many patches there are at all
    row.update(paint)
    return row


def measure_o240(run_dir):
    img, _ = load(run_dir, 'gas-o240')
    plate, _ = load(run_dir, 'gas-o240-plate')
    # O240 is five separated calls 40 m apart, not one patch: no `largest_blob` here (it would keep only one call).
    mask, raw_count, patch_count, _ = bank_mask(img, plate, blob=False)
    n = float(img.size[0] * img.size[1])
    mask_px = sum(1 for on in mask if on)
    return {'o240_area_frac': round(mask_px / n, 4), 'o240_mask_px': mask_px, 'o240_mask_px_raw': raw_count,
            'o240_mask_px_patch': patch_count}


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
    mask, _, _, _ = bank_mask(img, plate, bar=T1_DIFF_BAR, blob=True)
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
    if not os.path.isdir(cards_dir):
        print('no cards/')
        return {}
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
    """Every still's `sim_tick` in this run, from manifest.json and manifest-plate.json."""
    ticks = {}
    for name in ('manifest.json', 'manifest-plate.json'):
        path = os.path.join(run_dir, name)
        if not os.path.exists(path):
            continue
        with open(path, encoding='utf-8') as f:
            for row in json.load(f).get('stills', []):
                stem = os.path.splitext(row.get('still', ''))[0]
                if stem:
                    ticks[stem] = row.get('sim_tick')
    return ticks


def one_frame_fault(runs, ticks):
    """'three captures, one frame' if two runs stand on the same t1 tick, or two stills are byte-identical.

    g1 shot its three runs on ONE frame (all t1 `sim_tick` 557) and `run2/gas-o240.jpg` and `run3/gas-o240.jpg`
    were literally the same file, so its 0.0001 spread measured JPEG jitter, not the capture. A spread that small
    makes any bar look safe. This refuses the evidence before a single bar is read.
    """
    seen = {}
    for d in runs:
        t = ticks[os.path.basename(d)].get('gas-t1')
        if t in seen:
            print('three captures, one frame: %s and %s are both t1 sim_tick %s'
                  % (seen[t], os.path.basename(d), t))
            return True
        seen[t] = os.path.basename(d)
    digests = {}
    for d in runs:
        for stem in STILLS:
            with open(os.path.join(d, stem + '.jpg'), 'rb') as f:
                h = hashlib.sha1(f.read()).hexdigest()
            where = '%s/%s.jpg' % (os.path.basename(d), stem)
            if h in digests:
                print('three captures, one frame: %s and %s are the same file' % (digests[h], where))
                return True
            digests[h] = where
    return False


def find_runs(root=None):
    runs = []
    for d in sorted(glob.glob(os.path.join(root or HERE, 'run*'))):
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

    if '--against' in sys.argv:
        other = os.path.abspath(sys.argv[sys.argv.index('--against') + 1])
        other_runs = find_runs(other)
        if not other_runs:
            print('--against %s: no runs there' % other)
            return 1
        print('--against %s, measured with THIS probe (never a pass: the old build must fail the new bars)' % other)
        for d in other_runs:
            out = measure_run(d)
            print('  %-6s paint_frac %.4f  bank_paint_share %.4f  bank_patch_count %d  (paint %d px)'
                  '  bank_low_share %.4f  t1 sim_tick %s'
                  % (os.path.basename(d), out['paint_frac'], out['bank_paint_share'], out['bank_patch_count'],
                     out['paint_px'], out['bank_low_share'], frames_of(d).get('gas-t1')))
        return 1

    runs = find_runs()
    calibrate = '--calibrate' in sys.argv
    if not calibrate and len(runs) < 3:
        print('three runs needed: found %d (%s)' % (len(runs), ', '.join(os.path.basename(r) for r in runs)))
        return 1

    ticks = {os.path.basename(d): frames_of(d) for d in runs}
    for name in sorted(ticks):
        print('%s: sim_tick %s' % (name, ', '.join('%s %s' % (k, ticks[name][k]) for k in sorted(ticks[name]))))
    if not calibrate and one_frame_fault(runs, ticks):
        return 1

    results = {os.path.basename(d): measure_run(d) for d in runs}
    for name, out in results.items():
        print('%s: bank_low_share %.4f  bank_height_frac %.4f  bank_rise_m %.3f  bank_area_frac %.4f'
              '  o240_area_frac %.4f  pose_error_max %.4f  mask_px %d (raw %d, patch %d)  hue_window_px %d'
              '  paint_px %d  paint_frac %.4f  bank_paint_share %.4f  bank_patch_count %d'
              % (name, out['bank_low_share'], out['bank_height_frac'], out['bank_rise_m'], out['bank_area_frac'],
                 out['o240_area_frac'], out['pose_error_max'], out['mask_px'], out['mask_px_raw'],
                 out['mask_px_patch'], out['gas_hue_window_px'], out['paint_px'], out['paint_frac'],
                 out['bank_paint_share'], out['bank_patch_count']))

    look = look_report()

    if calibrate:
        with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
            json.dump({'runs': results, 'look': look, 'sim_ticks': ticks}, f, indent=1, sort_keys=True)
        return 0

    ok = True
    for bar_name, cmp_ in BARS.items():
        values = [results[r][bar_name] for r in results]
        # bars are set once three runs exist: see docs/evidence/ability-fx-captures-r2-2-g2/README.md
        bar = BAR_VALUES.get(bar_name)
        if bar is None:
            print('%-18s no bar set yet (needs the owner-approved build''s three captures)' % bar_name)
            ok = False
            continue
        ok = spread_check(bar_name, values, cmp_, bar) and ok

    with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
        json.dump({'runs': results, 'look': look, 'bars': BAR_VALUES, 'sim_ticks': ticks},
                  f, indent=1, sort_keys=True)
    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


# Bar values. Every one of them is set from TWO measured sources and never from a formula: the three runs of this
# build (`probe.json`, t1 sim_tick 557 / 597 / 637) and the zebra re-measured by this probe
# (`--against ../ability-fx-captures-r2-2-g2`, commit 843a03570c). A bar must clear the nearest run by at least
# the three runs' own spread, and a zebra bar must ALSO leave the zebra failing by more than that spread.
# See README.md, "The bars", for the table these came out of.
BAR_VALUES = {
    'bank_low_share': 0.07,     # runs 0.4371/0.2584/0.2853, spread 0.1787 -> 0.2584 - 0.1787 = 0.0797 is the roof
    'bank_height_frac': 0.27,   # runs 0.0799/0.1711/0.1608, spread 0.0912 -> 0.1711 + 0.0912 = 0.2623 is the floor
    'o240_area_frac': 0.010,    # g2's bar, kept: runs 0.0130/0.0150/0.0153, spread 0.0023, margin 0.0030
    'pose_error_max': 0.5,      # GasBankStills.PoseBar; runs 0.0000, spread 0.0000
    'paint_frac': 0.0220,       # runs 0.0267/0.0308/0.0272, spread 0.0041, margin 0.0047: the paint in the frame
    'bank_paint_share': 0.12,   # zebra 0.0782 | bar | 0.1622 lowest run; spread 0.0314, both margins 0.042
    'bank_patch_count': 25,     # zebra 40 | bar | 13 highest run; spread 6, margins 12 (build) and 15 (zebra)
}



if __name__ == '__main__':
    sys.exit(main())
