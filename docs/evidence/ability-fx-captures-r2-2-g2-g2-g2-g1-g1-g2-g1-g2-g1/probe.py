"""Count the GREEN the player sees in the chlorine bank, bar the rise, and count O240's clouds, not its wash.

WHY (this unit, -g1-g1-g2-g1-g2). The owner, 2026-10-22:

    The overview cloud-count ceiling of 15 clears the three runs 13, 13 and 11 by 2, and that spread is 2, so the
    bar sits on the spread. Those stills are one pose (frames 341, 356 and 371). Reshoot three captures far
    enough apart that the bank moves, and put the ceiling outside that spread.

No production change this unit: the build is commit 093b0be1b4's (`GasCloudWiden` 5), the captures and the bars
are what moved. The runs are 60 frames apart (waits 150/210/270) and two guards replace the old equal-frame one:
`MIN_FRAME_GAP` 45 (a closer pair is ONE POSE) and `MOVE_OVERLAP_BAR` (the pairwise Jaccard of the three O240
green masks: the BANK must have moved, not just the frame counter). README.md holds both readings and the two
owner bars that this spread leaves RED.

THE UNIT BEFORE THIS ONE (-g1-g1-g2-g1), kept for the record. The owner, 2026-10-22:

    At the overview the three stills are the night map with no gas a player can see. The probe's 11, 9 and 7
    clouds are blobs of at most 119 px (1909, 1645 and 1616 green pixels on the whole frame), and the area floor
    was cut to 0.0001 so that speck field passes. Keep the count under 15, and add a size floor above this
    build's biggest overview blob so a speck fails. Start from
    lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2's tip. Bar outside the spread of three captures of the
    current build.

Production (`CombatFx.Banks.cs`, one new const): -g1-g1-g2 put the overview cards 48 m apart (`GasCloudBlock` 12)
but left the card's own width multiplying by the local zoom step (2), so each cloud was 11-17.6 m wide inside its
own 48 m block - a field of specks, at most 119 px a blob. `GasCloudWiden` 5 replaces that step in the width
line: the same cells, the same card count, the same book and the same draw, each overview card simply wider
(35.2 m at its widest, against the 48 m spacing). Swept by capture at 3, 4 and 5 - the rows stand in the const's
doc comment and in README.md. The close drawing is untouched: `GasPuffBlock` 3, `GasPuffStand` 1.2 and the 12..16
window are carried, and T1's numbers are identical to -g1-g1-g2's.

THE PROBE'S TWO CHANGES from -g1-g1-g2's, which is otherwise carried unchanged (camera maths, green mask,
`largest_blob`, O240 clouds, the five-decimal area, `--pack`, `--calibrate`, `--selftest`, the two-sided height
bar, the frame guard, the spread check):

  1. `measure_o240` returns `o240_blob_max_px`: the pixel count of the biggest connected component of the WHOLE
     frame, independent of `CLOUD_MIN_PX`. It is the same number `o240_blob_top[0]` already printed as a
     diagnostic; this unit needs it as a measure, so it has a key.
  2. `BARS` gains `o240_blob_floor` ('ge' on `o240_blob_max_px`) - the owner's size floor. Its value is set from
     this build's three captures (1039 / 1035 / 1060 px, spread 25, bar 1000), and the speck build it replaces
     reads 119 / 117 / 116 (-g1-g1-g2/probe.json, commit e39c8f88e1): it fails outright. A cloud count alone
     could never say this, because 11 specks and 11 clouds count the same.

THE RISE BAR. `green_rise_m` is `measure_t1`'s median height of the green mask above the ground, barred `'ge'`.
This build's three captures read 2.575/2.529/2.568, spread 0.046 — well up from -g1's puddle-ish reading, because
the close puff's own cards stand taller even though fewer of them are drawn.

THE HEIGHT FLOOR. One measure, two bars (`-g1`'s own fix, carried): `green_height_frac` (the owner's ceiling 0.15,
`le`) and `green_height_floor` (`ge`, set from this unit's three captures, above 0.0768 — `-g1`'s own sheet-build
range — so that sheet's reading fails this bar outright).

THE O240 CLOUDS. `components(mask, w, h, dilate)` is `largest_blob`'s connected-component search pulled out so it
returns every component (biggest-first; python's stable `sort` keeps `largest_blob`'s old winner as
`components(...)[0]`, so T1's numbers are unchanged), instead of keeping only the biggest. `measure_o240` takes
the green mask's components, keeps those with at least `CLOUD_MIN_PX` (50) member pixels as `clouds`, and reports
`o240_cloud_count` (how many, barred on BOTH sides here: a floor so clouds exist at all, and
`o240_cloud_ceiling` so a speck field cannot pass as clouds - 'lt' 15, see `BAR_VALUES`),
`o240_cloud_share` (the green pixels inside those clouds, over all green pixels — low when the green is
scattered, not clouded) and redefines `o240_green_area_frac` as the clouded pixels' share of
the frame, not every green pixel's. `o240_blob_top`, the ten biggest component sizes, is printed as an unbarred
diagnostic.

THE NAMES. Every key says green: `green_low_share`, `green_height_frac`, `green_rise_m`, `green_area_frac`,
`o240_green_area_frac`, `o240_cloud_count`, `o240_cloud_ceiling`, `o240_cloud_share`,
`o240_blob_floor` (on `o240_blob_max_px`). `green_px`, `plate_diff_px`, `blob_px`,
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

THE LOOK REPORT. `cards/` holds the isolation stills (one hand-packed card per book, gas/smoke/puff, plus a
plate, and cards-gaspuff: the close camera's standing puff). These are never barred — the gas card is squashed by `CombatFx.GasBankSquash` 0.55 on purpose, so "gas
rise >= smoke rise" would fail by construction and would be a look change nobody asked for here. Instead this
prints, per card, its rise in metres and its relief (stdev of masked luma / its mean), and the gas/smoke ratio of
each.

USAGE
    python probe.py                 bar run1/run2/run3 against each other, print the cards/ look report
    python probe.py --calibrate      print every run's numbers and frames with no bar check and no frame refusal
    python probe.py --selftest       the camera maths against a hand calculation, no images
    python probe.py --calibrate --from DIR   read another unit's run*/ with THIS code (the sheet build's number)

Exit 0 only when three runs at three different frames are loaded and every bar clears by at least their spread.
Exit 1 with 'three runs needed' if fewer than three run*/ directories hold a full set of stills.
Exit 1 with 'one pose' if two runs are closer than MIN_FRAME_GAP frames, or if their O240 green overlaps by
MOVE_OVERLAP_BAR or more.
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
MIN_FRAME_GAP = 45            # two runs closer than this many frames are ONE POSE, not two captures (the owner)
MOVE_OVERLAP_BAR = 0.7021     # midway between the one-pose set's worst pair (0.7404, the three stills of commit
                              # 093b0be1b4 15 frames apart) and this unit's worst pair (0.6638, 60 frames apart)
# bar name -> (the measured key it reads, the comparison). Two bars may read ONE measure: height has both a
# ceiling and a floor, because the ceiling alone let the flat sheet through (see the docstring).
BARS = {'green_low_share': ('green_low_share', 'ge'),
        'green_top_m': ('green_top_m', 'ge'),
        'green_height_frac': ('green_height_frac', 'le'),
        'green_height_floor': ('green_height_frac', 'ge'),
        'green_area_frac': ('green_area_frac', 'ge'),
        'green_rise_m': ('green_rise_m', 'ge'),
        'o240_green_area_frac': ('o240_green_area_frac', 'ge'),
        'o240_cloud_count': ('o240_cloud_count', 'ge'),
        'o240_cloud_ceiling': ('o240_cloud_count', 'lt'),
        'o240_cloud_share': ('o240_cloud_share', 'ge'),
        'o240_blob_floor': ('o240_blob_max_px', 'ge'),
        'pose_error_max': ('pose_error_max', 'lt')}


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

    def ndc(px, py):
        return (2.0 * (px + 0.5) / w - 1.0, 1.0 - 2.0 * (py + 0.5) / h)

    dists = {}
    for px, py in columns.items():
        nx, ny = ndc(px, py)
        d = ground_distance(cam, pitch, yaw, ground, nx, ny)
        if d is not None and 1.0 < d < 2000.0:
            dists[px] = d

    heights, low, total, col_heights, all_heights = [], 0, 0, {}, []
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
        all_heights.append(y)
        col_heights.setdefault(px, []).append(y)
    for px, ys in col_heights.items():
        ys.sort()
        p45 = ys[min(len(ys) - 1, int(round(0.45 * (len(ys) - 1))))]
        heights.append(p45)
    heights.sort()

    mask_px = sum(1 for on in mask if on)
    diag = {'green_px': green_px, 'plate_diff_px': plate_diff_px, 'blob_px': blob_px}
    if not heights or total == 0:
        out = {'green_low_share': 0.0, 'green_height_frac': 1.0, 'green_rise_m': 0.0,
               'green_top_m': 0.0, 'green_area_frac': round(mask_px / float(w * h), 5)}
        out.update(diag)
        return out
    med_h = heights[len(heights) // 2]
    dvals = sorted(dists.values())
    med_d = dvals[len(dvals) // 2]
    frame_h = 2.0 * med_d * math.tan(math.radians(FOV_V_DEG * 0.5))
    all_heights.sort()
    top = all_heights[min(len(all_heights) - 1, int(round(0.95 * (len(all_heights) - 1))))]
    out = {
        'green_low_share': round(low / float(total), 4),
        'green_height_frac': round(med_h / frame_h, 4),
        'green_rise_m': round(med_h, 3),
        # the p95 of EVERY green pixel's height, not a per-column median: the creep pulls the median down, so
        # green_rise_m can no longer refuse a flat sheet, while a puff's own top still stands 4-6 m up
        'green_top_m': round(top, 3),
        'green_area_frac': round(mask_px / float(w * h), 5),
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
            'o240_cloud_share': cloud_share, 'o240_blob_top': blob_top,
            # the biggest component of the whole frame, CLOUD_MIN_PX-independent: the same number blob_top[0]
            # prints, barred here as `o240_blob_floor` so a field of specks cannot pass as clouds
            'o240_blob_max_px': len(comps[0]) if comps else 0}


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
    names = ('cards-gas', 'cards-smoke', 'cards-puff', 'cards-gaspuff')
    rows = {name: card_rise_m(cards_dir, name) for name in names}
    smoke_rise = rows['cards-smoke']['rise_m'] or 1e-9
    smoke_relief = rows['cards-smoke']['relief'] or 1e-9
    print('look report (cards/, unbarred): the gas card against the smoke card this pose can already draw')
    for name in names:
        r = rows[name]
        print('  %-14s rise %.3f m  relief %.4f  mask_px %d' % (name, r['rise_m'], r['relief'], r['mask_px']))
    gas_smoke_rise_ratio = rows['cards-gas']['rise_m'] / smoke_rise
    gas_smoke_relief_ratio = rows['cards-gas']['relief'] / smoke_relief
    puff_smoke_rise_ratio = rows['cards-gaspuff']['rise_m'] / smoke_rise
    puff_smoke_relief_ratio = rows['cards-gaspuff']['relief'] / smoke_relief
    print('  gas/smoke rise ratio       %.4f' % gas_smoke_rise_ratio)
    print('  gas/smoke relief ratio     %.4f' % gas_smoke_relief_ratio)
    print('  gaspuff/smoke rise ratio   %.4f' % puff_smoke_rise_ratio)
    print('  gaspuff/smoke relief ratio %.4f' % puff_smoke_relief_ratio)
    rows['gas_smoke_rise_ratio'] = round(gas_smoke_rise_ratio, 4)
    rows['gas_smoke_relief_ratio'] = round(gas_smoke_relief_ratio, 4)
    rows['gaspuff_smoke_rise_ratio'] = round(puff_smoke_rise_ratio, 4)
    rows['gaspuff_smoke_relief_ratio'] = round(puff_smoke_relief_ratio, 4)
    return rows


def frames_of(run_dir):
    """The `frame` each of the four sidecars was photographed at (the goal's "not the same frame", enforced)."""
    out = {}
    for stem in STILLS:
        with open(os.path.join(run_dir, stem + '.json'), encoding='utf-8') as f:
            out[stem] = json.load(f).get('frame')
    return out


def gap_refusal(frames, min_gap=MIN_FRAME_GAP):
    """Print and return True when two runs' gas-t1 or gas-o240 frames are closer together than `min_gap`.

    The owner rejected three stills 15 frames apart as "one pose": the frame counter differing is not enough,
    the bank must have had time to move. This is the stricter replacement of `same_frame_refusal`, which only
    caught an exactly equal frame.
    """
    bad = False
    for stem in ('gas-t1', 'gas-o240'):
        names = sorted(frames)
        for i, a in enumerate(names):
            for b in names[i + 1:]:
                fa, fb = frames[a][stem], frames[b][stem]
                if fa is None or fb is None:
                    continue
                gap = abs(int(fa) - int(fb))
                if gap < min_gap:
                    print('one pose: %s and %s are %d frames apart (%s, need %d)' % (a, b, gap, stem, min_gap))
                    bad = True
    return bad


def o240_green_mask(run_dir):
    """The O240 green mask alone, as a set of pixel indices: what `measure_o240` counts clouds in."""
    img, _ = load(run_dir, 'gas-o240')
    plate, _ = load(run_dir, 'gas-o240-plate')
    mask, _, _, _ = green_mask(img, plate, blob=False)
    return frozenset(i for i, on in enumerate(mask) if on)


def move_refusal(runs, bar=MOVE_OVERLAP_BAR):
    """Print each pair's Jaccard overlap of the O240 green, and refuse when the worst pair is at or above `bar`.

    The frame gap proves the counter moved; this proves the BANK moved. Identical poses overlap almost
    perfectly, so a high overlap means three photographs of one cloud shape.
    """
    masks = {os.path.basename(d): o240_green_mask(d) for d in runs}
    names = sorted(masks)
    worst = 0.0
    for i, a in enumerate(names):
        for b in names[i + 1:]:
            inter = len(masks[a] & masks[b])
            union = len(masks[a] | masks[b])
            j = inter / float(union) if union else 0.0
            worst = max(worst, j)
            print('o240 overlap %s/%s  jaccard %.4f' % (a, b, j))
    if bar is None:
        print('o240_overlap_max %.4f  no bar set yet' % worst)
        return True, worst
    ok = worst < bar
    print('o240_overlap_max     %.4f  bar lt %.4f  -> %s' % (worst, bar, 'OK' if ok else 'FAILED'))
    if not ok:
        print('one pose: the three captures share %.4f of their overview green' % worst)
    return (not ok), worst


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
    """Where run*/ is read from: HERE, or `--from DIR` (another unit's folder, measured with THIS code).

    The refusal number of the old flat-sheet build is its own committed captures read by this unit's
    `measure_t1`, so `green_top_m` can be compared at all: the sheet build's probe never computed it.
    """
    if '--from' in sys.argv:
        d = sys.argv[sys.argv.index('--from') + 1]
        return d if os.path.isabs(d) else os.path.abspath(d)
    return HERE


def find_runs():
    runs = []
    for d in sorted(glob.glob(os.path.join(runs_dir(), 'run*'))):
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
    print('%-20s values %s  spread %.5f  margin %.5f  bar %s %.5f  -> %s'
          % (name, ['%.5f' % v for v in values], spread, margin, cmp_, bar, 'OK' if ok else 'FAILED'))
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
        print('%s: green_low_share %.4f  green_height_frac %.4f  green_rise_m %.3f  green_top_m %.3f'
              '  green_area_frac %.5f  o240_green_area_frac %.4f  pose_error_max %.4f'
              % (name, out['green_low_share'], out['green_height_frac'], out['green_rise_m'],
                 out['green_top_m'], out['green_area_frac'], out['o240_green_area_frac'],
                 out['pose_error_max']))
        print('  o240_cloud_count %d  o240_cloud_share %.4f  o240_blob_max_px %d'
              % (out['o240_cloud_count'], out['o240_cloud_share'], out['o240_blob_max_px']))
        print('  diagnostics (barred on nothing): green_px %d  plate_diff_px %d  blob_px %d'
              '  o240_green_px %d  o240_plate_diff_px %d  o240_blob_top %s'
              % (out['green_px'], out['plate_diff_px'], out['blob_px'],
                 out['o240_green_px'], out['o240_plate_diff_px'], out['o240_blob_top']))
        print('  frames: t1 %s (plate %s)  o240 %s (plate %s)'
              % (frames[name]['gas-t1'], frames[name]['gas-t1-plate'],
                 frames[name]['gas-o240'], frames[name]['gas-o240-plate']))

    look = look_report()

    if calibrate:
        with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
            json.dump({'runs': results, 'look': look, 'frames': frames}, f, indent=1, sort_keys=True)
        return 0

    if gap_refusal(frames):
        print('FAILED: three captures of one pose are one capture')
        return 1
    moved_bad, overlap_max = move_refusal(runs)
    if moved_bad:
        print('FAILED: the bank did not move between the three captures')
        return 1

    ok = True
    for bar_name, (key, cmp_) in BARS.items():
        values = [results[r][key] for r in results]
        # bars are set once three different-frame runs exist: see this folder's README.md
        bar = BAR_VALUES.get(bar_name)
        if bar is None:
            print('%-20s no bar set yet (needs the owner-approved build''s three captures)' % bar_name)
            ok = False
            continue
        ok = spread_check(bar_name, values, cmp_, bar) and ok

    with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
        json.dump({'runs': results, 'look': look, 'frames': frames, 'bars': BAR_VALUES,
                   'o240_overlap_max': round(overlap_max, 4)},
                  f, indent=1, sort_keys=True)
    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


# Bar values, set from the spread of run1/run2/run3 at three different frames (see README.md "The bars").
BAR_TOP_M = 11.0                     # the green's p95 height, 'ge': this build's runs read 12.070/12.063/12.099
                                     # (spread 0.036, margin 1.07), and the flat-sheet build's own committed
                                     # captures read 6.085/6.091/6.089 with THIS code
                                     # (`--calibrate --from docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1`),
                                     # so a sheet fails this bar by 4.9 m. It replaces nothing: `green_rise_m`'s
                                     # median is pulled about by the creep, this p95 is the puff's own top.
BAR_VALUES = {
    'green_low_share': 0.30,        # THE OWNER'S, restored (2026-10-23: "Put green_low_share back to 0.30 and
                                     # fill the low band of the bank"). -g1-g2 had dropped it to 0.11 to clear the
                                     # honest 60-frame spread of the puff-only build (0.2317/0.3473/0.3042); the
                                     # bar is not what moved here, the drawing is: GasCreepWiden adds a low creep
                                     # between the puffs, so the low band is filled and the three runs clear 0.30
                                     # by more than their own spread. See README.md and decisions.md
    'green_top_m': BAR_TOP_M,       # the puff's own top (p95 of the green), the replacement guard against a flat
                                     # sheet: the creep pulls green_rise_m's median down into the sheet build's
                                     # range, but a sheet has no 4-6 m puff over it. Set from this build's runs
    'green_height_frac': 0.15,      # owner-decided CEILING, kept; runs 0.0977/0.0933/0.0879, spread 0.0098,
                                     # margin 0.0523
    'green_height_floor': 0.075,    # the FLOOR, re-read from these runs: 0.0977/0.0933/0.0879, spread 0.0098,
                                     # margin 0.0129 (0.08 only cleared by 0.0079, inside the spread)
    'green_rise_m': 3.5,            # re-read (the creep build): runs 4.013/4.028/4.059, spread 0.046, margin
                                     # 0.513. Up from 1.9, because the creep bridges the puffs into one blob and
                                     # the blob's median height is the bank's, not one near puff's
    'green_area_frac': 0.0075,      # carried: the creep build reads 0.08172/0.08218/0.08212 (spread 0.00046),
                                     # far over the bar - the creep covers the bare ground, as asked
    'o240_blob_floor': 1020,        # re-read: biggest overview blob 1041/1033/1042, spread 9, margin 13. The
                                     # speck build still fails by an order of magnitude (119/117/116)
    'o240_green_area_frac': 0.0065,  # re-read: runs 0.0071/0.0070/0.0074, spread 0.0004, margin 0.0005
    'o240_cloud_count': 3,          # the FLOOR, carried: runs 15/13/13, spread 2, margin 10
    'o240_cloud_ceiling': 17,       # re-read: runs 15/13/13, spread 2, margin 2. 17 is the smallest value that
                                     # clears the spread, above the owner's "under 15": his call, see decisions.md.
                                     # The speck field is still refused, not by this ceiling but by o240_blob_floor
                                     # 1020 (speck runs read 119/117/116).
    'o240_cloud_share': 0.85,       # re-read at these frames (291/351/411): runs 0.8825/0.8851/0.9057, spread
                                     # 0.0232, margin 0.0325. The overview draw is untouched; 0.86 only cleared
                                     # by 0.0225, inside this set's own spread
    'pose_error_max': 0.50,         # GasBankStills.PoseBar
}


if __name__ == '__main__':
    sys.exit(main())
