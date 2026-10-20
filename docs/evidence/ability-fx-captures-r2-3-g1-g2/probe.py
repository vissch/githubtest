#!/usr/bin/env python3
"""Measure the chlorine gas bank in the stills beside this script: three seeds, from the pixels, in metres.

WHY (round 2-3-g1-g2). The owner, on g1's result: "texture_var_drop is 0.70 against a bar of 0.40, which rewards
the flat green fill, and gas_t1_height_m 8.3-9.9 has no side view a player can read as a cloud. Shoot each seed
from the side at zoom 30 and fail a bank whose blocks are flatter than the ground." Two answers, both new here:
  - every seed gains a THIRD still, `gas-side-sN` (and its plate): the same cloud, the same zoom, shot at pitch 8
    (TacticalCamera.PitchMin, the lowest a player can tilt). `side_relief_ratio` is the bank's own top-edge
    relief divided by the relief of its bottom edge, which is where the bank meets the ground and so traces the
    ground's own profile under it. At or under 1 the bank's blocks are flatter than the ground they sit on - the
    owner's fail. Higher is better.
  - `texture_var_drop` keeps its 0.40 floor (regression) and gains a CEILING, `DROP_CEILING`. A drop is
    1 - var_gas/var_plate, so a ceiling is a floor on how much of the ground texture is still read through the
    bank: the same number the owner says rewards the flat green fill, barred the other way round, so the bar
    cannot be won by painting the frame green.
  At 8 degrees the distance of a pixel is not worth taking from the pixel: a near-horizontal ray moves about 0.9 m
  of ground distance per pixel row. The side measure takes its distance from the SEED instead - the manifest row's
  x/z against the sidecar's camera - and a fixed plane is well conditioned there: an error of a metre in distance
  is about 0.01 m of height.

WHY (round 2-3-g1). At T1 the bank is a field of flat green cards with hard edges and mud showing in the gaps: it
reads as lily pads, not one low cloud. `t1_gap_frac` answers this: close the bank's outline (a dilate-then-erode
that bridges the gaps between cards but keeps the bank's own outer edge, which a row/column fill cannot - a
regular grid of pads fills to exactly the pads and reads gap 0), then measure the share of that outline the bank
did NOT change. Lower is better, and it is a NEW bar, kept separate from r2-3's three (regression-only, below).

WHY (r2-3). The harsh critic scored r2-2 at 22/100. Three faults, and this probe answers all three:
  1. "flat olive smears that read as moss, not a cloud" — r2-2 measured only WHERE the bank sat (how low, how
     far) and never whether it HID anything. A bank that hides the ground flattens the grass texture under it, so
     `texture_var_drop` compares the per-8x8-block luma variance under the bank against the same blocks of the
     no-gas plate, and `t1_changed_frac` says how much of the frame the bank actually moved.
  2. "every bar passed by under 0.025 on one capture" — one lucky patch carried the claim. Every reading here is a
     LIST, one value per seed, and a bar must be met by all three. GasBankStills shoots three patches in one
     pass, as far apart as the greybox corridor allows (it holds only two open patches 60 m apart, so the search
     steps 60 - 48 - 40 - 32 m and takes the first spacing that yields three; the chosen one is in its log).
  3. "the committed probe exits 1 on a clean checkout (gas-o240.json missing)" — the stills were committed without
     their sidecars. This probe reads NOTHING from outside its own folder and exits 1 the moment a still, a
     sidecar or a manifest row is absent, which is exactly what a clean `git archive` extract checks.
  4. "at O240 it cannot be told from grass" — `o240_hue_dist_deg` is the circular distance between the
     saturation-weighted mean hue of the pixels the bank changed and the mean hue of THE SAME PIXELS in the plate.
     The same pixels are the grass the bank covers, so the plate is the right control for "is it still grass".

The camera maths (`ray`, `ground_distance`, `height_at`, `selftest`) and the mask cleanup (`largest_blob`) are
r2-2's and unchanged: they were sound, and the selftest is a hand calculation worked outside the code it checks.
`gas_t1_height_m` is carried over as a REPORTED row, not a bar: it is the "visible height" claim in metres.

HOW
  the masks     T1: a pixel whose Rec.709 luma (0-1) moved from the no-gas PLATE of the same pose and the same sim
                tick by LUMA_BAR or more. The plate IS the control: whatever changed between it and the shot with
                gas running is the gas. O240 keeps r2-2's RGB-distance test (DIFF_BAR) instead, because the far
                bank's luma shift is small and the hue reading needs the pixels the bank covers, not a luma claim.
  the blob      the mask keeps only its single largest coherent patch (`largest_blob`): JPEG block noise and
                frame-to-frame animation scatter thin threads across the whole frame; the bank is the one dense
                patch. The 8x8 texture blocks and the height columns are both taken inside it.
  the column    per blob column with at least MIN_COLUMN_PX pixels, the bottom-most pixel is where the bank meets
                the ground; its ray crossed y = ground_y, which gives that column's horizontal distance, and every
                higher pixel of the column turns into a height in metres. A column's height is its 45th percentile
                (r2-2's finding: even inside the blob, card sprites run well above where the bank sits).

  the readings, each a list of one value per seed
                t1_changed_frac      share of the T1 frame whose luma moved from the plate by >= LUMA_BAR  (bar)
                texture_var_drop     1 - var_gas / var_plate over the 8x8 blocks centred inside the bank  (bar)
                o240_hue_dist_deg    degrees between the bank's mean hue and the grass it covers          (bar)
                t1_gap_frac          of the pixels inside the bank's CLOSED outline, the share unchanged  (bar)
                t1_luma_delta        mean absolute luma change over the changed pixels                 (row)
                gas_t1_height_m      median column height of the bank, in metres                       (row)
                pose_error_m         the worst pose error of the seed's four sidecars                   (row)
                t1_closed_px         pixel count inside the closed outline                             (row)
                t1_gap_px            pixel count inside the outline the bank did not change             (row)
                t1_close_radius_px   the closing radius used, converted from 3 m                        (row)
                t1_patch_count       patches of `changed` over 2000 px ("one bank per seed")            (row)
                side_relief_ratio    the bank's top-edge relief over its ground-edge relief (pitch 8)    (bar)
                side_bank_relief_m   p90 - p10 of the per-column top height, in metres                  (row)
                side_ground_relief_m p90 - p10 of the per-column base height, in metres                 (row)
                side_top_m           median per-column top height, in metres                            (row)
                side_base_m          median per-column base height, in metres                           (row)
                side_blob_px         pixels in the side blob                                            (row)
                side_cols            columns of the side blob with at least MIN_COLUMN_PX pixels        (row)

USAGE
    python probe.py --pack <capture dir>              copy the stills here as JPGs + sidecars, then measure
    python probe.py                                   measure the JPGs already here
    python probe.py --dir baseline --out baseline.json   measure a subfolder, write its own json
    python probe.py --selftest                        the camera maths against a hand calculation, no images

Exit 0 only when every bar's list holds three values and every bar is met by every seed.
"""
import glob
import json
import math
import os
import shutil
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
FOV_V_DEG = 25.0            # TacticalCamera.Fov, vertical
ASPECT = 16.0 / 9.0
SEEDS = 3
LUMA_BAR = 0.06             # the owner's "mean luma change >= 0.06": T1's changed test, Rec.709 on 0-1
DIFF_BAR = 12               # O240's changed test, r2-2's RGB distance (the far bank moves little luma)
BLOB_DILATE = 1             # largest_blob's bridge width; 2 chained the bank into terrain-edge noise (r2-2)
BLOCK = 8                   # the texture window: one luma variance per 8x8 block
MIN_COLUMN_PX = 8           # fewer masked pixels in a column is stray haze or JPEG noise
HEIGHT_PCTL = 0.45          # a column's top, as a percentile of its masked pixels (r2-2's finding)
GAP_CLOSE_RADIUS_M = 3.0    # half a hair under MapData.FieldCellSize (4 m): closes a one-cell gap, not a real hole
GAP_CLOSE_RADIUS_PX_MIN = 6
GAP_CLOSE_RADIUS_PX_MAX = 40
GAP_PATCH_MIN_PX = 2000     # a patch of `changed` this small is noise, not a bank

# The owner's bars, raised to clear the baseline spread of the UNCHANGED build (see README.md, "The bars"):
# chosen = max(owner's bar, baseline_max + margin), margins 0.03 / 0.10 / 5 deg. A bar is never lowered below
# the owner's, and never widened to make a run green.
OWNER_BARS = {'t1_changed_frac': 0.08, 'texture_var_drop': 0.40, 'o240_hue_dist_deg': 25.0}
BAR_MARGIN = {'t1_changed_frac': 0.03, 'texture_var_drop': 0.10, 'o240_hue_dist_deg': 5.0}
BASELINE_MAX = {'t1_changed_frac': 0.0436, 'texture_var_drop': 0.0958, 'o240_hue_dist_deg': 32.14}
# The baseline (the unchanged build, three seeds, `baseline.json`) misses all three of the owner's bars, so two of
# them stand as he set them. The hue reading is the exception: the old flat smear already sat 32.14 deg from the
# grass it covers on seed 1, so 25 deg would have been passed by the very build the critic threw out. That bar is
# raised to 32.14 + 5 = 38.0 deg. Raised, never lowered, and never widened to make a run green.
BARS = {'t1_changed_frac': 0.08, 'texture_var_drop': 0.40, 'o240_hue_dist_deg': 38.0}

# t1_gap_frac is lower-is-better (the share of the bank's own outline the bank did NOT change), so it is not in
# BARS above: it is checked with max(got) <= GAP_BAR, not min(got) >= bar. Set from THIS round's own baseline (the
# three captures of the unchanged build, 2ba27d4072, baseline.json): 0.6493 / 0.5026 / 0.5498. The spread is
# 0.6493 - 0.5026 = 0.1467, so margin = max(0.05, 0.1467) = 0.1467 and
# GAP_BAR = baseline_min - margin = 0.5026 - 0.1467 = 0.3559, a full spread-width below the whole spread.
# Never widened afterwards to make a run green.
GAP_BAR = 0.3559

SIDE_RELIEF_PCTL = (0.10, 0.90)   # `relief` is p90 - p10 by sorted index: the ends are JPEG noise, not relief

# The side bar and the drop ceiling, both set from THIS round's own baseline (`baseline.json`, three captures of
# f427286f0d plus the side still, no look change), the family's way and never widened afterwards:
#   SIDE_BAR     = baseline_max + max(0.25, spread)     higher is better
#   DROP_CEILING = baseline_min - max(0.05, spread)     lower is better (a ceiling)
# Unset until that baseline run exists: `main` prints MISSED and returns 1 while either is None, as GAP_BAR did.
# The baseline (f427286f0d plus the side still, `baseline/` and `baseline.json`):
#   side_relief_ratio  0.6163 / 1.0690 / 0.3252   spread 1.0690 - 0.3252 = 0.7438, margin max(0.25, 0.7438)
#                      SIDE_BAR = 1.0690 + 0.7438 = 1.8128, a full spread-width above the whole spread
#   texture_var_drop   0.7210 / 0.7237 / 0.6921   spread 0.7237 - 0.6921 = 0.0316, margin max(0.05, 0.0316)
#                      DROP_CEILING = 0.6921 - 0.05 = 0.6421, a margin below the whole spread
# Two of the three baseline seeds already read a bank FLATTER than the ground under it (ratio under 1), which is
# the owner's fail, and the third is 1.07. Every baseline ground relief is over 0.3 m (3.5-11.3 m), so the ratio
# is well conditioned and the metres fallback in the plan's risks is not needed.
SIDE_BAR = 1.8128
DROP_CEILING = 0.6421


def stems():
    """The 18 stills a pass writes: all three bands of every seed, each with its no-gas plate."""
    out = []
    for s in range(SEEDS):
        for band in ('gas-t1', 'gas-o240', 'gas-side'):
            out.append('%s-s%d' % (band, s))
            out.append('%s-s%d-plate' % (band, s))
    return out


# ----------------------------------------------------------------------------- the camera (r2-2, unchanged)

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
    Then two readings worked by hand on made-up pixels, to check the new measures:
      a 16x16 plate of alternating 0 / 255 columns has a per-block luma variance of 0.25 (every block holds half
      black and half white, so var = 0.25); flattened to a flat grey it is 0, a drop of 1.0.
      hue: pure red (0 deg) against pure green (120 deg) is 120.0 deg apart, and red against magenta (300 deg) is
      60.0 deg apart the short way round, not 300.
    """
    cam, pitch, yaw = (0.0, 10.0, 0.0), 45.0, 0.0
    tan_v = math.tan(math.radians(FOV_V_DEG * 0.5))
    d = ground_distance(cam, pitch, yaw, 0.0, 0.0, 0.0)
    assert d is not None and abs(d - 10.0) < 1e-6, 'centre pixel hits the ground at %r, wanted 10.000' % d
    one_deg = math.tan(math.radians(1.0)) / tan_v          # one degree up, in ndc
    h = height_at(cam, pitch, yaw, 0.0, 0.0, one_deg, d)
    assert abs(h - 0.34311) < 1e-3, 'one degree up came back %.5f m, hand calculation says 0.34311' % h

    stripes = np.zeros((16, 16, 3), dtype=np.uint8)
    stripes[:, ::2, :] = 255
    flat = np.full((16, 16, 3), 128, dtype=np.uint8)
    mask = np.ones((16, 16), dtype=bool)
    v_plate = block_variance(luma(stripes), mask)
    v_gas = block_variance(luma(flat), mask)
    assert abs(v_plate - 0.25) < 1e-6, 'striped plate variance %.6f, hand calculation says 0.250000' % v_plate
    assert abs(v_gas) < 1e-9, 'a flat grey has variance %.6f, wanted 0' % v_gas
    drop = 1.0 - v_gas / v_plate
    assert abs(drop - 1.0) < 1e-9, 'a flat grey over stripes is a drop of %.6f, wanted 1.0' % drop

    red = np.full((4, 4, 3), 0, dtype=np.uint8); red[:, :, 0] = 255
    green = np.full((4, 4, 3), 0, dtype=np.uint8); green[:, :, 1] = 255
    magenta = np.full((4, 4, 3), 0, dtype=np.uint8); magenta[:, :, 0] = 255; magenta[:, :, 2] = 255
    rg = hue_distance(red, green, mask[:4, :4])
    rm = hue_distance(red, magenta, mask[:4, :4])
    assert abs(rg - 120.0) < 1e-4, 'red against green came back %.4f deg, hand calculation says 120' % rg
    assert abs(rm - 60.0) < 1e-4, 'red against magenta came back %.4f deg, hand calculation says 60' % rm

    # t1_gap_frac: a 20x20 solid square with a 4x4 hole cut from its middle, closed at R = 3.
    # Dilating 3 px fills the 4x4 hole (any point in it is within 3 px of the square's own pixels, since the
    # hole's farthest point from the square is at most 2-3 px in, given the hole sits centred in the square) and
    # grows the square's outer edge by 3 px past the 20x20 frame; eroding 3 px then shrinks that back to exactly
    # the original 20x20 = 400 px square, now with no hole. blob is the ORIGINAL square minus its hole = 384 px.
    # gap = closed - blob = 400 - 384 = 16 px. gap_frac = 16 / 400 = 0.0400.
    square = np.zeros((20, 20), dtype=bool)
    square[:, :] = True
    square[8:12, 8:12] = False
    closed_sq = closing(square, 3)
    assert int(closed_sq.sum()) == 400, 'closed solid-with-hole square is %d px, hand calculation says 400' % closed_sq.sum()
    gap_sq = float((closed_sq & ~square).sum() / closed_sq.sum())
    assert abs(gap_sq - 0.0400) < 1e-9, 'square-with-hole gap_frac came back %.4f, hand calculation says 0.0400' % gap_sq
    # the same square with no hole: closing changes nothing, gap_frac = 0.
    solid = np.ones((20, 20), dtype=bool)
    closed_solid = closing(solid, 3)
    gap_solid = float((closed_solid & ~solid).sum() / closed_solid.sum())
    assert gap_solid == 0.0, 'solid square (no hole) gap_frac came back %.4f, hand calculation says 0.0' % gap_solid

    # the side measure's own plane: the SAME pitch-45 camera, centre pixel, measured at its own 10 m plane, is
    # 0.000 m high (the hand calculation above: that ray crosses the ground at exactly 10 m), and one degree up is
    # 0.34311 m - the same hand number, now read through the fixed-plane route the side measure uses.
    h_centre = height_at(cam, pitch, yaw, 0.0, 0.0, 0.0, d)
    assert abs(h_centre) < 1e-6, 'the centre pixel at its own 10 m plane came back %.6f m, hand says 0.000' % h_centre
    # relief: 11 values 0..10, p10 is sorted index 1 and p90 is index 9, so p90 - p10 = 9 - 1 = 8.0
    r11 = relief([float(i) for i in range(11)])
    assert abs(r11 - 8.0) < 1e-9, 'relief([0..10]) came back %.4f, hand calculation says 8.0' % r11
    assert relief([3.0]) == 0.0, 'relief of a single column is not 0'

    print('selftest ok: centre pixel 10.000 m out, one degree up %.5f m high (hand: 0.34311);' % h)
    print('             stripes -> flat grey is a texture drop of %.3f (hand: 1.000);' % drop)
    print('             red/green %.1f deg, red/magenta %.1f deg (hand: 120, 60)' % (rg, rm))
    print('             side plane: centre %.3f m (hand: 0.000), relief([0..10]) %.1f (hand: 8.0);' % (h_centre, r11))
    print('             20x20 square with a 4x4 hole, R=3: gap_frac %.4f (hand: 0.0400); no hole: %.4f (hand: 0.0)'
          % (gap_sq, gap_solid))
    return 0


# ----------------------------------------------------------------------------- the pixels

def luma(rgb):
    """Rec.709 luma on 0-1 from a uint8 HxWx3 array. CaptureRig's own scale: never mixed with 0-255 Rec.601."""
    a = rgb.astype(np.float64) / 255.0
    return 0.2126 * a[:, :, 0] + 0.7152 * a[:, :, 1] + 0.0722 * a[:, :, 2]


def hue_sat(rgb):
    """Hue in degrees (0-360) and saturation on 0-1, the usual HSV cone, from a uint8 HxWx3 array."""
    a = rgb.astype(np.float64) / 255.0
    r, g, b = a[:, :, 0], a[:, :, 1], a[:, :, 2]
    mx, mn = a.max(axis=2), a.min(axis=2)
    c = mx - mn
    h = np.zeros_like(mx)
    with np.errstate(divide='ignore', invalid='ignore'):
        h = np.where(mx == r, ((g - b) / c) % 6.0, h)
        h = np.where(mx == g, (b - r) / c + 2.0, h)
        h = np.where(mx == b, (r - g) / c + 4.0, h)
    h = np.where(c <= 1e-9, 0.0, h) * 60.0
    sat = np.where(mx <= 1e-9, 0.0, c / np.where(mx <= 1e-9, 1.0, mx))
    return h, sat


def mean_hue(rgb, mask):
    """Saturation-weighted circular mean hue, in degrees, of the masked pixels (None if nothing is masked)."""
    h, s = hue_sat(rgb)
    w = s[mask]
    ang = np.radians(h[mask])
    if w.size == 0 or w.sum() <= 1e-9:
        return None
    x = float((w * np.cos(ang)).sum())
    y = float((w * np.sin(ang)).sum())
    if abs(x) < 1e-12 and abs(y) < 1e-12:
        return None
    return math.degrees(math.atan2(y, x)) % 360.0


def hue_distance(rgb_a, rgb_b, mask):
    """Circular distance in degrees (0-180) between the two images' mean hue over the SAME masked pixels."""
    ha, hb = mean_hue(rgb_a, mask), mean_hue(rgb_b, mask)
    if ha is None or hb is None:
        return 0.0
    d = abs(ha - hb) % 360.0
    return min(d, 360.0 - d)


def block_variance(lum, mask):
    """Mean per-BLOCKxBLOCK luma variance over the blocks whose centre pixel is masked (0.0 if there are none)."""
    h, w = lum.shape
    bh, bw = h // BLOCK, w // BLOCK
    tiles = lum[:bh * BLOCK, :bw * BLOCK].reshape(bh, BLOCK, bw, BLOCK).swapaxes(1, 2)
    var = tiles.reshape(bh, bw, BLOCK * BLOCK).var(axis=2)
    centres = mask[BLOCK // 2::BLOCK, BLOCK // 2::BLOCK][:bh, :bw]
    return float(var[centres].mean()) if centres.any() else 0.0


def grow(mask, n):
    """Dilate a boolean mask by `n` steps of the 4-neighbour grow (r2-2's `largest_blob`, pulled out so `closing`
    can reuse it: dilate the complement by the same loop to erode)."""
    dil = mask.copy()
    for _ in range(n):
        grown = dil.copy()
        grown[1:, :] |= dil[:-1, :]
        grown[:-1, :] |= dil[1:, :]
        grown[:, 1:] |= dil[:, :-1]
        grown[:, :-1] |= dil[:, 1:]
        dil = grown
    return dil


def components(mask):
    """Every 4-connected component of `mask`, each as a list of flat pixel indices, biggest first."""
    from collections import deque
    h, w = mask.shape
    flat = mask.reshape(-1)
    seen = np.zeros(flat.size, dtype=bool)
    found = []
    for start in np.flatnonzero(flat):
        if seen[start]:
            continue
        seen[start] = True
        q = deque([int(start)])
        members = []
        while q:
            j = q.popleft()
            members.append(j)
            x, y = j % w, j // w
            for nj in ((j - 1 if x > 0 else -1), (j + 1 if x < w - 1 else -1),
                       (j - w if y > 0 else -1), (j + w if y < h - 1 else -1)):
                if nj >= 0 and flat[nj] and not seen[nj]:
                    seen[nj] = True
                    q.append(nj)
        found.append(members)
    found.sort(key=len, reverse=True)
    return found


def largest_blob(mask, dilate=BLOB_DILATE):
    """Keep only the mask pixels inside the single biggest coherent patch (r2-2's cleanup, on numpy arrays).

    JPEG block noise and frame-to-frame animation (moving infantry, flickering fires) scatter thin, isolated diff
    threads across the WHOLE frame, not just where the gas sits. The bank is the one dense patch. Dilate to bridge
    the small gaps JPEG leaves inside it, flood-fill the dilated pixels into components, keep the biggest, then
    intersect back with the ORIGINAL mask so only pixels that really differed from the plate are ever counted.
    """
    h, w = mask.shape
    dil = grow(mask, dilate)
    found = components(dil)
    keep = np.zeros(h * w, dtype=bool)
    if found:
        keep[found[0]] = True
    return mask & keep.reshape(h, w)


def closing(mask, radius):
    """Morphological closing: dilate by `radius`, then erode by `radius` (dilate the complement and invert).

    Joins separate cards across a gap of up to 2*radius while keeping the bank's own outer edge - a row/column
    fill cannot do this, since a regular grid of pads fills to exactly the pads and reads a gap of 0.
    """
    dilated = grow(mask, radius)
    return ~grow(~dilated, radius)


# ----------------------------------------------------------------------------- the stills

def pack(src, dst):
    """Copy every still in `src` into `dst` as a JPG under 400 KB, with its sidecar and the pass manifests."""
    if not os.path.isdir(dst):
        os.makedirs(dst)
    written, want = [], set(stems())
    for png in sorted(glob.glob(os.path.join(src, '*.png'))):
        stem = os.path.splitext(os.path.basename(png))[0]
        if stem not in want:
            continue
        out = os.path.join(dst, stem + '.jpg')
        img = Image.open(png).convert('RGB')
        for q in range(92, 40, -6):
            img.save(out, 'JPEG', quality=q)
            if os.path.getsize(out) <= 400 * 1024:
                break
        side = os.path.splitext(png)[0] + '.json'
        if os.path.exists(side):
            shutil.copyfile(side, os.path.join(dst, stem + '.json'))
        written.append(stem)
    for name in ('manifest.json', 'manifest-plate.json'):
        p = os.path.join(src, name)
        if os.path.exists(p):
            shutil.copyfile(p, os.path.join(dst, name))
    return written


def load(d, stem):
    """The still as a numpy RGB array and its sidecar, both from `d` (never from outside it)."""
    img = np.asarray(Image.open(os.path.join(d, stem + '.jpg')).convert('RGB'))
    with open(os.path.join(d, stem + '.json'), encoding='utf-8') as f:
        return img, json.load(f)


def row_of(d, stem):
    """This still's whole manifest row (ground_y, the seed's x/z, the call answers); the sidecar carries none of it."""
    for name in ('manifest.json', 'manifest-plate.json'):
        path = os.path.join(d, name)
        if not os.path.exists(path):
            continue
        with open(path, encoding='utf-8') as f:
            for row in json.load(f).get('stills', []):
                if row.get('still', '') == stem + '.png':
                    return row
    return None


def ground_of(d, stem):
    """The ground height under this still's gas, from its manifest row."""
    row = row_of(d, stem)
    return None if row is None else float(row.get('ground_y', 0.0))


def relief(values):
    """p90 - p10 of a list, by sorted index: how much an edge rises and falls across the bank, in metres."""
    if len(values) < 2:
        return 0.0
    v = sorted(values)
    lo = v[int(round(SIDE_RELIEF_PCTL[0] * (len(v) - 1)))]
    hi = v[int(round(SIDE_RELIEF_PCTL[1] * (len(v) - 1)))]
    return float(hi - lo)


def measure_side(d, seed):
    """One seed's side view (pitch 8): is the bank's own top edge rougher than the ground it sits on?

    Mask and blob are T1's exactly. The distance of a column is NOT taken from its pixel: at 8 degrees above the
    horizon a pixel row is about 0.9 m of ground distance, so a per-pixel `ground_distance` is ill-conditioned.
    It is taken from the seed the bank was called on (the manifest row's x/z against the sidecar's camera), and
    every column of the blob is measured in that one vertical plane: a near-horizontal ray moves only about
    0.01 m of height per metre of distance error, so a fixed plane is well conditioned where the pixel is not.
    The blob's BOTTOM edge is where the bank meets the ground, so it traces the ground's own profile under the
    bank; its TOP edge is the bank's own. The ratio of the two reliefs is the owner's bar.
    """
    stem = 'gas-side-s%d' % seed
    img, side = load(d, stem)
    plate, _ = load(d, stem + '-plate')
    lum_g, lum_p = luma(img), luma(plate)
    mask = np.abs(lum_g - lum_p) >= LUMA_BAR
    blob = largest_blob(mask)

    row = row_of(d, stem)
    tops, bases = [], []
    if row is not None and blob.any():
        ground = float(row.get('ground_y', 0.0))
        cam = (side['cam_x'], side['cam_y'], side['cam_z'])
        pitch, yaw = side['cam_pitch'], side['cam_yaw']
        dist = math.hypot(cam[0] - float(row.get('x', 0.0)), cam[2] - float(row.get('z', 0.0)))
        h, w = blob.shape
        ys, xs = np.nonzero(blob)
        for px in np.unique(xs):
            rows_ = ys[xs == px]
            if rows_.size < MIN_COLUMN_PX:
                continue
            nx = 2.0 * (px + 0.5) / w - 1.0
            for py, out in ((int(rows_.min()), tops), (int(rows_.max()), bases)):
                ny = 1.0 - 2.0 * (py + 0.5) / h
                y = height_at(cam, pitch, yaw, ground, nx, ny, dist)
                if y is not None:
                    out.append(y)

    bank_relief = relief(tops)
    ground_relief = relief(bases)
    ratio = float(bank_relief / ground_relief) if ground_relief > 1e-9 else 0.0
    return ({'side_relief_ratio': round(ratio, 4)},
            {'side_bank_relief_m': round(bank_relief, 3),
             'side_ground_relief_m': round(ground_relief, 3),
             'side_top_m': round(float(np.median(tops)), 3) if tops else 0.0,
             'side_base_m': round(float(np.median(bases)), 3) if bases else 0.0,
             'side_blob_px': int(blob.sum()),
             'side_cols': len(tops),
             'pose_error_m': round(float(side.get('pose_error_m', 99.0)), 4)})


def measure_t1(d, seed):
    """One seed's T1 frame: how much of it the bank moved, how flat the ground went under it, how tall it stands."""
    stem = 'gas-t1-s%d' % seed
    img, side = load(d, stem)
    plate, _ = load(d, stem + '-plate')
    lum_g, lum_p = luma(img), luma(plate)
    delta = np.abs(lum_g - lum_p)
    mask = delta >= LUMA_BAR

    changed_frac = float(mask.mean())
    luma_delta = float(delta[mask].mean()) if mask.any() else 0.0

    blob = largest_blob(mask)
    var_p = block_variance(lum_p, blob)
    var_g = block_variance(lum_g, blob)
    drop = float(1.0 - var_g / var_p) if var_p > 1e-12 else 0.0

    ground = ground_of(d, stem)
    radius_px = gap_close_radius_px(blob, side, ground, img.shape[1])
    closed = closing(blob, radius_px) if blob.any() else blob
    closed_px = int(closed.sum())
    gap_px = int((closed & ~blob).sum())
    gap_frac = float(gap_px / closed_px) if closed_px > 0 else 0.0
    patch_count = sum(1 for c in components(mask) if len(c) > GAP_PATCH_MIN_PX)

    return ({'t1_changed_frac': round(changed_frac, 4),
             'texture_var_drop': round(drop, 4),
             't1_gap_frac': round(gap_frac, 4)},
            {'t1_luma_delta': round(luma_delta, 4),
             't1_blob_px': int(blob.sum()),
             't1_block_var_plate': round(var_p, 6),
             't1_block_var_gas': round(var_g, 6),
             'gas_t1_height_m': height_of(blob, side, ground),
             'pose_error_m': round(float(side.get('pose_error_m', 99.0)), 4),
             't1_closed_px': closed_px,
             't1_gap_px': gap_px,
             't1_close_radius_px': radius_px,
             't1_patch_count': patch_count})


def gap_close_radius_px(blob, side, ground, width):
    """GAP_CLOSE_RADIUS_M converted to pixels at the blob's own distance (its bottom-centre pixel), clamped."""
    default = (GAP_CLOSE_RADIUS_PX_MIN + GAP_CLOSE_RADIUS_PX_MAX) // 2
    if ground is None or not blob.any():
        return default
    h, w = blob.shape
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ys, xs = np.nonzero(blob)
    y_bottom = int(ys.max())
    cols_at_bottom = xs[ys == y_bottom]
    x_centre = int(np.median(cols_at_bottom))
    tan_v = math.tan(math.radians(FOV_V_DEG * 0.5))
    tan_h = tan_v * ASPECT
    nx = 2.0 * (x_centre + 0.5) / w - 1.0
    ny = 1.0 - 2.0 * (y_bottom + 0.5) / h
    d = ground_distance(cam, pitch, yaw, ground, nx, ny)
    if d is None or d <= 1e-6:
        return default
    px_per_m = width / (2.0 * d * tan_h)
    r = int(round(GAP_CLOSE_RADIUS_M * px_per_m))
    return max(GAP_CLOSE_RADIUS_PX_MIN, min(GAP_CLOSE_RADIUS_PX_MAX, r))


def height_of(blob, side, ground):
    """The bank's median column height in metres, from the blob and the sidecar's camera (0.0 if it cannot be had)."""
    if ground is None or not blob.any():
        return 0.0
    h, w = blob.shape
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ys, xs = np.nonzero(blob)

    def ndc(px, py):
        return (2.0 * (px + 0.5) / w - 1.0, 1.0 - 2.0 * (py + 0.5) / h)

    heights = []
    for px in np.unique(xs):
        rows_ = ys[xs == px]
        if rows_.size < MIN_COLUMN_PX:
            continue
        nx, ny = ndc(px, rows_.max())
        dist = ground_distance(cam, pitch, yaw, ground, nx, ny)
        if dist is None or not (1.0 < dist < 2000.0):
            continue
        col = []
        for py in rows_:
            nx2, ny2 = ndc(px, py)
            y = height_at(cam, pitch, yaw, ground, nx2, ny2, dist)
            if y is not None:
                col.append(y)
        if not col:
            continue
        col.sort()
        heights.append(col[min(len(col) - 1, int(round(HEIGHT_PCTL * (len(col) - 1))))])
    if not heights:
        return 0.0
    heights.sort()
    return round(heights[len(heights) // 2], 3)


def measure_o240(d, seed):
    """One seed's overview frame: is the bank a different colour from the grass it covers?"""
    stem = 'gas-o240-s%d' % seed
    img, side = load(d, stem)
    plate, _ = load(d, stem + '-plate')
    diff = np.sqrt(((img.astype(np.float64) - plate.astype(np.float64)) ** 2).sum(axis=2))
    mask = largest_blob(diff >= DIFF_BAR)
    return ({'o240_hue_dist_deg': round(hue_distance(img, plate, mask), 2)},
            {'o240_changed_frac': round(float((diff >= DIFF_BAR).mean()), 4),
             'o240_blob_px': int(mask.sum()),
             'o240_bank_hue_deg': None if mean_hue(img, mask) is None else round(mean_hue(img, mask), 1),
             'o240_grass_hue_deg': None if mean_hue(plate, mask) is None else round(mean_hue(plate, mask), 1),
             'pose_error_m': round(float(side.get('pose_error_m', 99.0)), 4)})


def missing(d):
    """Every still, sidecar or manifest the measure needs and does not have, as the clean extract would find it."""
    out = []
    for stem in stems():
        for ext in ('.jpg', '.json'):
            if not os.path.exists(os.path.join(d, stem + ext)):
                out.append(stem + ext)
    for name in ('manifest.json', 'manifest-plate.json'):
        if not os.path.exists(os.path.join(d, name)):
            out.append(name)
    return out


def main():
    if '--selftest' in sys.argv:
        return selftest()

    d = HERE
    if '--dir' in sys.argv:
        d = os.path.join(HERE, sys.argv[sys.argv.index('--dir') + 1])
    out_name = sys.argv[sys.argv.index('--out') + 1] if '--out' in sys.argv else 'probe.json'
    if '--pack' in sys.argv:
        src = sys.argv[sys.argv.index('--pack') + 1]
        print('packed into %s: %s' % (os.path.basename(d), ', '.join(pack(src, d)) or 'nothing'))

    gone = missing(d)
    if gone:
        print('missing, so nothing can be measured: ' + ', '.join(gone))
        return 1

    bars = {k: [] for k in list(BARS) + ['t1_gap_frac', 'side_relief_ratio']}
    rows = {}
    for seed in range(SEEDS):
        b1, r1 = measure_t1(d, seed)
        b2, r2 = measure_o240(d, seed)
        b3, r3 = measure_side(d, seed)
        for k, v in list(b1.items()) + list(b2.items()) + list(b3.items()):
            bars[k].append(v)
        for k, v in list(r1.items()) + list(r2.items()) + list(r3.items()):
            rows.setdefault(k, []).append(v)

    result = dict(bars)
    result['bars'] = BARS
    result['owner_bars'] = OWNER_BARS
    result['gap_bar'] = GAP_BAR
    result['side_bar'] = SIDE_BAR
    result['drop_ceiling'] = DROP_CEILING
    result['rows'] = rows
    with open(os.path.join(d, out_name), 'w', encoding='utf-8') as f:
        json.dump(result, f, indent=1, sort_keys=True)

    ok = True
    print('seed  t1_changed_frac  texture_var_drop  o240_hue_dist_deg  t1_gap_frac  side_relief_ratio'
          '  side_bank_m  side_ground_m  height_m  pose_err_m')
    for seed in range(SEEDS):
        print('  s%d  %15.4f  %16.4f  %17.2f  %11.4f  %17.4f  %11.3f  %13.3f  %8.3f  %10.4f'
              % (seed, bars['t1_changed_frac'][seed],
                 bars['texture_var_drop'][seed], bars['o240_hue_dist_deg'][seed], bars['t1_gap_frac'][seed],
                 bars['side_relief_ratio'][seed], rows['side_bank_relief_m'][seed],
                 rows['side_ground_relief_m'][seed],
                 rows['gas_t1_height_m'][seed], max(rows['pose_error_m'][seed * 3:seed * 3 + 3])))
    for k in ('t1_changed_frac', 'texture_var_drop', 'o240_hue_dist_deg'):
        got = bars[k]
        met = len(got) >= SEEDS and min(got) >= BARS[k]
        ok = ok and met
        print('%-18s want >= %-7s worst %-9s %s  (the owner asked for %s)'
              % (k, BARS[k], round(min(got), 4) if got else 'none', 'OK' if met else 'MISSED', OWNER_BARS[k]))
    # the drop ceiling: the same reading as the 0.40 floor above, barred the other way round, so a bank that
    # simply paints the frame green (no ground texture left to read at all) fails instead of scoring well
    got = bars['texture_var_drop']
    if DROP_CEILING is None:
        print('texture_var_drop   want <= (unset)  worst %-9s MISSED  (no baseline yet)'
              % (round(max(got), 4) if got else 'none'))
        ok = False
    else:
        met = len(got) >= SEEDS and max(got) <= DROP_CEILING
        ok = ok and met
        print('texture_var_drop   want <= %-7s worst %-9s %s'
              % (DROP_CEILING, round(max(got), 4) if got else 'none', 'OK' if met else 'MISSED'))

    got = bars['side_relief_ratio']
    if SIDE_BAR is None:
        print('side_relief_ratio  want >= (unset)  worst %-9s MISSED  (no baseline yet)'
              % (round(min(got), 4) if got else 'none'))
        ok = False
    else:
        met = len(got) >= SEEDS and min(got) >= SIDE_BAR
        ok = ok and met
        print('side_relief_ratio  want >= %-7s worst %-9s %s  (at or under 1 the blocks are flatter than the ground)'
              % (SIDE_BAR, round(min(got), 4) if got else 'none', 'OK' if met else 'MISSED'))

    got = bars['t1_gap_frac']
    if GAP_BAR is None:
        print('t1_gap_frac        want <= (unset)  worst %-9s MISSED  (no baseline yet)'
              % (round(max(got), 4) if got else 'none'))
        ok = False
    else:
        met = len(got) >= SEEDS and max(got) <= GAP_BAR
        ok = ok and met
        print('t1_gap_frac        want <= %-7s worst %-9s %s'
              % (GAP_BAR, round(max(got), 4) if got else 'none', 'OK' if met else 'MISSED'))
    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
