#!/usr/bin/env python3
"""Measure the chlorine gas bank in the stills beside this script: three seeds, from the pixels, in metres.

r2-3-g2 ADDS a third band, "side": T1 (pitch 25, zoom 30) and O240 (pitch 25, zoom 240) both look down steeply
enough that ground lying further out than the bank's own base reads as height, not footprint. At T1's pose
(distance 78.1 m, camera 33.0 m up, 70.8 m out) 10 m of extra ground depth sits 2.8 deg higher in frame, which
`gas_t1_height_m` turns into 70.8*tan(2.8deg) = 3.5 m of "height" that is only footprint: half the 5.7-7.2 m T1
reports. The side band (zoom 16, pitch 8 = TacticalCamera.PitchMin, aimY raised to clear the horizon) grazes the
bank instead: at its pose (distance 41.7 m, camera 7.8 m up, 41.3 m out) the same 10 m of depth reads as only
41.3*tan(2.05deg) = 1.5 m. `gas_side_height_m` takes the SILHOUETTE TOP of each column (not the 45th percentile
T1 uses) and subtracts `footprint_bias_m`, the honest ground-depth error at that pose, so the number left is the
bank's own thickness, not the field sloping away under it.

r2-3-g2-g2-g2 MAKES THE THREE GAS SEEDS ONE HUE. `o240_bank_hue_deg` (the core median, r2-3-g2-g2's rung) still
reads 106.2 / 126.5 / 148.8 deg - the core restriction narrows a MIXED pixel's hue, but a mixed pixel's hue is
still a blend of the card's colour and the ground showing through it, and the fringe-to-core ratio still differs
per seed. `unmix_hue` reads the DIFFERENCE vector instead: a gas card is alpha-blended, so a masked pixel is
`P = a*C + (1-a)*B` for plate `B`, card colour `C` and per-pixel alpha `a`, and `d = P - B = a*(C - B)`. The
*direction* of `d` depends only on `C - B`, never on `a`: a fringe pixel (small `a`) and a core pixel (large `a`)
that come from the same card colour point the SAME way, they just have different lengths. So the hue of `d`
(after clamping negative channels to 0, since `hue` is undefined on a vector with a negative channel) is the
hue of the card's OWN colour against the plate, with no per-seed fringe ratio left to drag it around. Reading
`unmix_hue` over the WHOLE blob brings the three seeds to 85.7-100.0 deg (unchanged from the core median - the
core restriction and the unmix read the same quantity once alpha stops mattering) with no core restriction
needed; `o240_hue_off_deg`, the distance from `GasBankNight`'s own painted hue (67.5 deg, see `HUE_PAINT_DEG`),
replaces `o240_hue_dist_deg`'s "not grass" check with "one hue, and the hue the paint actually is". No drawn
look changes (`CombatFx.Banks.cs` is untouched): the whole fix is in the measure, again.

r2-3-g2-g2 CHANGES WHAT THE SIDE BAND BARS. `gas_side_height_m` stayed inside 2.798-2.888 m while the side
blob ran 83100, 210655 and 303462 pixels: a reading that barely moves while the bank it measures triples is
blind, because a median column's silhouette top and the 10 m footprint bias under it are both set by the pose.
The bar moves to `side_face_px`: per column, the pixels standing HIGHER than that column's own
`footprint_bias_m`. Ground sloping away beyond the base can never read above its own bias, so those pixels are
the bank's upright face on screen, measured in pixels and growing when the bank grows. `gas_side_height_m`
stays, as a reported row beside `side_top_m` and `side_bias_m` - the complaint is that it was barred and blind,
not that it is useless - and `side_face_frac` / `side_face_col_px` report the face free of the cloud's size.

WHY. The harsh critic scored r2-2 at 22/100. Three faults, and this probe answers all three:
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
  4. "at O240 it cannot be told from grass" — `o240_hue_dist_deg` is the circular distance between the hue of the
     pixels the bank changed and the hue of THE SAME PIXELS in the plate. The same pixels are the grass the bank
     covers, so the plate is the right control for "is it still grass". r2-3-g2-g2 reads that hue over the blob's
     CORE (`HUE_CORE_PCTL`, the top quartile of plate difference) with a saturation-weighted MEDIAN: the card is
     alpha-blended, so its fringe is mostly ground showing through and each seed holds a different fringe-to-core
     ratio, which moved a whole-blob mean 42 deg between seeds. Over the core, with a median, the three agree to
     14.3 deg (README.md, "The hue").

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
                o240_hue_dist_deg    degrees between the bank's CORE hue and the grass it covers         (bar)
                o240_hue_off_deg     degrees between the unmixed hue and GasBankNight's own paint        (bar)
                o240_unmix_hue_deg   the alpha-free hue of the shot-minus-plate difference vector          (row)
                o240_bank_hue_deg    the bank's core hue, and o240_grass_hue_deg the same pixels' plate    (row)
                o240_blob_hue_deg    the old whole-blob mean hue, kept so the two can be compared          (row)
                o240_core_px         how many of the blob's pixels the core restriction kept               (row)
                side_face_px         side-band pixels standing above their column's footprint bias         (bar)
                side_face_frac       that face as a share of the side blob (free of how big the cloud is)  (row)
                side_face_col_px     the median column's face, in pixels (free of the bank's width)        (row)
                gas_side_height_m    the median column's silhouette top less its footprint bias, in m      (row)
                t1_luma_delta        mean absolute luma change over the changed pixels                 (row)
                gas_t1_height_m      median column height of the bank, in metres                       (row)
                pose_error_m         the worst pose error of the seed's four sidecars                   (row)

USAGE
    python probe.py --pack <capture dir>              copy the stills here as JPGs + sidecars, then measure
    python probe.py                                   measure the JPGs already here
    python probe.py --dir baseline --out baseline.json   measure a subfolder, write its own json
    python probe.py --selftest                        the camera maths against a hand calculation, no images

Exit 0 only when all three lists hold three values and every bar is met by every seed.
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
RIPPLE_TILE = 4             # `ripple`'s coarse tile: 4x4 pixels, the scale a per-cell pad's rim steps at
RIPPLE_WIN = 9              # `ripple`'s smoothing window, in tiles: what it subtracts is the cloud's slow dome
RIPPLE_COVER = 0.5          # a tile under half inside the blob is rim, not bank, and is not measured
MIN_COLUMN_PX = 8           # fewer masked pixels in a column is stray haze or JPEG noise
HEIGHT_PCTL = 0.45          # a column's top, as a percentile of its masked pixels (r2-2's finding)
HUE_CORE_PCTL = 75          # the hue is read over the blob's CORE: the pixels in the top quartile of plate
                            # difference, where the card's own colour dominates instead of ground showing through
                            # the alpha-blended fringe (r2-3-g2-g2, step 7)
# GasBankNight = (0.78, 0.86, 0.22) (CombatFx.Banks.cs:29). By the HSV formula: max = g 0.86, min = b 0.22,
# c = 0.64, h = ((0.22 - 0.78)/0.64 + 2) * 60 = (2 - 0.875) * 60 = 67.5 deg. This is the paint's OWN hue, not a
# reading off any still, so it never drifts with the fringe-to-core ratio that moves the other two readings
# (r2-3-g2-g2-g2, step 3).
HUE_PAINT_DEG = 67.5

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
# r2-3-g2: the side band's own pose and the ground-depth it is shot against (Open() clears a 10 m radius, which is
# the bank's own spread, so that is the depth any single view can be fooled by).
SIDE_PITCH_DEG = 8.0
SIDE_FOOT_DEPTH_M = 10.0
# bar = min(seeds) - margin, margin = the floor below (0.5 m / 5.0 deg): the capture of commit 2ba27d4 spreads
# its three seeds 2.80-2.89 m (height) and 74.8-123.0 deg (hue) apart - a spread-sized margin on the hue reading
# would undo "it does not police the look" by dropping the bar BELOW the 38.0 r2-3 already set, so the floor
# governs and the bar is rounded DOWN from the arithmetic so rounding never moves it inside the spread. Never
# lowered below the owner's bar (`o240_hue_dist_deg` only; `gas_side_height_m` has none), never widened to make a
# run green. Set from the capture of commit 2ba27d4 (the build the owner accepted); see README.md, "The bars".
BARS = {'t1_changed_frac': 0.08, 'texture_var_drop': 0.40, 'o240_hue_dist_deg': 103.5}
# r2-3-g2-g2, the hue bar: the old 69.8 sat 5 deg under a scatter of 74.8-123.0 deg, so a seed could drift 45 deg
# and still pass. The reading is fixed first (the core, and a weighted median: HUE_CORE_PCTL), which brings the
# three seeds' bank hue to 85.7-100.0 deg - a 14.3 deg spread - and their distance from the grass to
# 120.80 / 134.53 / 138.04 deg. The bar is then
#   bar = min(seeds) - max(10.0, max(seeds) - min(seeds)) = 120.80 - 17.24 = 103.56 -> 103.5 (rounded DOWN)
# a margin at least one full spread wide, outside the spread of three captures of this build, and far above the
# owner's 25.0. Never lowered below the owner's bar, never widened to make a run green.
# r2-3-g2-g2: `gas_side_height_m` is no longer barred, it is reported. The owner: it sat inside 2.798-2.888 m
# while `side_blob_px` ran 83100, 210655 and 303462 - a number that barely moves while the thing it measures
# triples is blind, because it is one median column's silhouette top minus a 10 m footprint bias, and both of
# those are set by the pose, not by the bank. `side_face_px` counts instead THE PIXELS THAT CANNOT BE GROUND:
# per column, the pixels standing higher than that column's own `footprint_bias_m`. Ground sloping away can
# never reach above its own bias, so what is left is the bank's upright face, in pixels, and it grows when the
# bank grows. Its bar is `floor(0.8 * min(seeds) / 1000) * 1000`: a 20 % margin (a SHARE, because face area
# scales with how big a patch's cloud is) rounded down, so the bar sits outside the spread at the low end.
# `side_face_frac` (face / blob) and `side_face_col_px` (the median column) sit beside it as size-free rows.
#
# r2-3-g2-g2-g2, the hue bar: `o240_hue_dist_deg` (the core median against the grass it covers) is kept exactly
# as it was, unchanged, still comparable to r2-3-g2-g2 - it is a "not grass" check and stays one. The new bar is
# `o240_hue_off_deg`, an UPPER bound (MAX_BARS, checked with <=): the circular distance between the alpha-free
# `unmix_hue` (read over the WHOLE blob - the unmix already makes the core restriction unnecessary, see
# README.md) and HUE_PAINT_DEG (67.5 deg, GasBankNight's own hue). Against the stills of commit 2ba27d4 (the
# build the owner accepted) the three seeds' `o240_hue_off_deg` came back 12.50 / 13.43 / 12.50 deg, a 0.93 deg
# spread - the three seeds are one hue, and a tight one. The rule, never widened to make a run green:
#   bar = ceil(max(seeds) + max(5.0, max(seeds) - min(seeds))) rounded UP to the next 0.5
#       = ceil(13.43 + max(5.0, 0.93)) = ceil(18.43) -> 18.5
# rounding UP so rounding never moves the bar inside the spread (an upper bound must round AWAY from the
# passing side, which for a <= bar is up, the mirror of BARS' round-down).
#
# r2-3-g2-g2-g2-g1: `side_face_px` is NOT a bar any more, it is a row beside `side_face_frac`. A bar a carpet
# clears three times over is not a bar: the build the owner threw out as "a carpet of flat green pads" read
# 135367 / 52156 / 186565 against 41000 - cleared, and spread 12x across three seeds of ONE build, so it told
# nobody anything. It is `gas_side_height_m`'s fault one round later: the number grows with how much gas there
# is, not with whether the gas is one bank, and the owner never asked for more gas.
# Its replacements are `t1_ripple` and `side_ripple` (`ripple`), UPPER bounds in MAX_BARS: how hard the bank's
# own alpha steps at the 4 m-cell scale, as a share of its mean. Fewer pads is the passing side. Both are set
# BELOW the minimum of three seeds of the current build (`baseline.json`): bar = min(seeds) * 0.8, rounded DOWN
# to 3 decimals so rounding can only make the bar stricter and never pull it inside the spread. See README.md.
# The baseline (`baseline.json`, the current build reshot on this commit, three seeds): `t1_ripple`
# 0.3254 / 0.3800 / 0.3768, `side_ripple` 0.2583 / 0.3232 / 0.3003. So
#   t1_ripple   bar = 0.3254 * 0.8 = 0.26032 -> 0.260 (rounded DOWN)
#   side_ripple bar = 0.2583 * 0.8 = 0.20664 -> 0.206 (rounded DOWN)
MAX_BARS = {'o240_hue_off_deg': 18.5, 't1_ripple': 0.260, 'side_ripple': 0.206}


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


def footprint_bias_m(cam, pitch, yaw, ground_y, dist):
    """What `SIDE_FOOT_DEPTH_M` of extra ground depth beyond `dist` reads as, in metres, at `dist`.

    The honest error band of ANY single-view height claim: ground that lies further out than a bank's own base
    sits higher in frame purely by perspective, with nothing standing on it. The base's ray leaves the camera at
    some depression angle; a point `SIDE_FOOT_DEPTH_M` further out along the ground leaves at a shallower one, and
    the difference, carried back to the `dist` plane, is the bias. `gas_side_height_m` subtracts this so what is
    left is the bank's own thickness, not the field sloping away under it.
    """
    base_depression = math.degrees(math.atan2(cam[1] - ground_y, dist))
    far_depression = math.degrees(math.atan2(cam[1] - ground_y, dist + SIDE_FOOT_DEPTH_M))
    return dist * math.tan(math.radians(base_depression - far_depression))


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

    # camera 10 m up, pitch 45, base distance 10 m; ground 10 m further out (20 m) has depression atan(10/20) =
    # 26.565 deg against the base's 45 deg, a difference of 18.435 deg, so at the 10 m plane it reads
    # 10*tan(18.435) = 3.3333 m.
    bias = footprint_bias_m((0.0, 10.0, 0.0), 45.0, 0.0, 0.0, 10.0)
    assert abs(bias - 3.3333) < 1e-3, 'footprint bias came back %.4f m, hand calculation says 3.3333' % bias

    # The face measure, at the same pose and by plain trigonometry (NOT by height_at):
    # base distance 10 m, so the bias is the 3.3333 m above. A point that high, 10 m out, sits at depression
    # atan((10 - 3.3333)/10) = atan(0.66667) = 33.690 deg, which is 45 - 33.690 = 11.310 deg ABOVE the centre
    # ray, so ndc_y = tan(11.310)/tan(12.5) = 0.20000/0.22169 = 0.9022. On a 720-row frame that is row
    # ((1 - 0.9022)/2)*720 - 0.5 = 34.7, so rows 0..34 stand above the bias and row 35 does not: 35 pixels.
    # The synthetic column holds rows 0..100 plus the CENTRE row (360), which is the bottom pixel and the one
    # that fixes the base distance at exactly 10.000 m (the centre pixel of the selftest above).
    frame_h, frame_w = 720, 1280
    synth = np.zeros((frame_h, frame_w), dtype=bool)
    col = frame_w // 2 - 1                                  # nx = -0.00078: the aspect never enters
    synth[0:101, col] = True
    synth[frame_h // 2, col] = True
    sidecar = {'cam_x': 0.0, 'cam_y': 10.0, 'cam_z': 0.0, 'cam_pitch': 45.0, 'cam_yaw': 0.0}
    total_face, col_face, n_cols = face_px(synth, sidecar, 0.0)
    assert n_cols == 1, 'the synthetic blob has %d columns, wanted 1' % n_cols
    assert abs(total_face - 35) <= 1, 'the face came back %d pixels, hand calculation says 35' % total_face
    assert col_face == total_face, 'one column, so the median column is the total'

    # The core hue, worked from the HSV formula by hand (step 10). A 64x64 plate of grass (0.20, 0.25, 0.50),
    # a 32x32 blob on it whose central 16x16 is CombatFx.GasBankNight (0.78, 0.86, 0.22) and whose 768-pixel
    # ring is the 50/50 blend with the grass (0.49, 0.555, 0.36) - the alpha fringe.
    #   grass: max b 0.50, min 0.20, c 0.30 -> h = 60*((0.20-0.25)/0.30 + 4) = 230.0 deg
    #   core:  max g 0.86, min 0.22, c 0.64 -> h = 60*((0.22-0.78)/0.64 + 2) =  67.5 deg
    #   blend: max g 0.555, min 0.36, c 0.195 -> h = 60*((0.36-0.49)/0.195 + 2) = 80.0 deg
    # so the core reads 67.5 and sits 230.0 - 67.5 = 162.5 deg from the grass it covers. The core is a quarter
    # of the blob, which is exactly what HUE_CORE_PCTL = 75 keeps.
    def rgb8(c):
        return np.array([int(round(v * 255.0)) for v in c], dtype=np.uint8)
    grass, bank, blend = rgb8((0.20, 0.25, 0.50)), rgb8((0.78, 0.86, 0.22)), rgb8((0.49, 0.555, 0.36))
    plate = np.tile(grass, (64, 64, 1))
    img = plate.copy()
    img[16:48, 16:48] = blend
    img[24:40, 24:40] = bank
    blob = np.zeros((64, 64), dtype=bool)
    blob[16:48, 16:48] = True
    hue_diff = np.sqrt(((img.astype(np.float64) - plate.astype(np.float64)) ** 2).sum(axis=2))
    core = core_of(blob, hue_diff)
    assert int(core.sum()) == 256, 'the core came back %d pixels, the painted core is 256' % int(core.sum())
    core_hue = hue_of(img, core)
    core_dist = hue_distance(img, plate, core)
    blob_hue = mean_hue(img, blob)
    assert abs(core_hue - 67.5) < 1.0, 'the core hue read %.2f deg, the HSV formula says 67.5' % core_hue
    assert abs(core_dist - 162.5) < 1.0, 'the core distance read %.2f deg, by hand 162.5' % core_dist
    assert abs(blob_hue - 67.5) > 5.0, ('the whole blob read %.2f deg, the same as its core: then the core '
                                        'restriction does no work' % blob_hue)
    assert 67.5 < blob_hue < 81.0, ('the whole blob read %.2f deg, wanted it pulled toward the '
                                    "blend's 80.0" % blob_hue)

    # The unmixed hue (step 11), hand-worked outside the code under test: a 64x64 plate of grass
    # (0.20, 0.25, 0.50) = uint8 (51, 64, 128), a 32x32 blob whose central 16x16 is the full card
    # (0.78, 0.86, 0.22) = uint8 (199, 219, 56) and whose 768-pixel ring is the card at alpha 0.25,
    # blended against the grass = uint8 (88, 103, 110).
    #   core d = (199,219,56) - (51,64,128) = (148, 155, -72) -> clamped (148, 155, 0):
    #     c = 155, h = ((0 - 148)/155 + 2) * 60 = 62.71 deg
    #   ring d = (88,103,110) - (51,64,128) = (37, 39, -18) -> clamped (37, 39, 0):
    #     c = 39, h = ((0 - 37)/39 + 2) * 60 = 63.08 deg
    # only uint8 rounding separates the two, so the whole blob (core + ring together) must read near 62.9 deg -
    # unlike `mean_hue` (what `o240_blob_hue_deg` quotes), which reads the hue of the raw pixel `P` itself. The
    # ring's raw colour (88, 103, 110) has the GRASS's blue channel as its max, so its own hue is 199.1 deg, not
    # anywhere near the card: `mean_hue` mixes that straight in and is pulled far off (it reads 119.75 here).
    unmix_grass = np.tile(np.array([51, 64, 128], dtype=np.uint8), (64, 64, 1))
    unmix_img = unmix_grass.copy()
    unmix_img[16:48, 16:48] = np.array([88, 103, 110], dtype=np.uint8)
    unmix_img[24:40, 24:40] = np.array([199, 219, 56], dtype=np.uint8)
    unmix_blob = np.zeros((64, 64), dtype=bool)
    unmix_blob[16:48, 16:48] = True
    unmix_result = unmix_hue(unmix_img, unmix_grass, unmix_blob)
    old_result = mean_hue(unmix_img, unmix_blob)
    assert abs(unmix_result - 62.9) < 1.0, ('unmix_hue read %.2f deg, hand calculation says near 62.9'
                                             % unmix_result)
    assert abs(old_result - unmix_result) > 5.0, ('the old mean_hue read %.2f deg, within 5 deg of unmix_hue '
                                                    '%.2f: then unmix_hue does no work' % (old_result, unmix_result))
    assert abs(HUE_PAINT_DEG - unmix_result) < 6.0, ('the paint anchor %.1f sits %.2f deg from the unmixed hue, '
                                                       'wanted under 6' % (HUE_PAINT_DEG,
                                                                           abs(HUE_PAINT_DEG - unmix_result)))
    print('             unmix_hue of a card+fringe blob reads %.2f deg (hand: near 62.9); the old whole-blob '
          'mean_hue on the same image reads %.2f deg, pulled toward the raw blended colour' % (unmix_result,
                                                                                                 old_result))

    # `ripple`, worked from its definition and not from a run of it (step 3 of the plan).
    #   (a) a 64x64 delta of 0.40 everywhere under a full blob: every 4x4 tile reads A = 0.40, so the 9x9 tile
    #       box mean is 0.40 too, A - smooth = 0 at every tile, std = 0 and the ripple is exactly 0.000. A flat
    #       field has no ripple at any scale.
    #   (b) the same field with every other 4x4 tile raised to 0.60 - a checkerboard at exactly the pad scale.
    #       mean(A) = 0.50. The 9x9 window holds 81 tiles: 41 of the centre tile's own parity and 40 of the
    #       other, so smooth = (41 v + 40 v') / 81 and A - smooth = 40 (v - v') / 81 = +-8/81 = +-0.098765 at
    #       EVERY tile (the 16x16 tile grid is periodic and the box mean wraps, so there are no edge tiles).
    #       std = 8/81, and the ripple is (8/81) / 0.50 = 16/81 = 0.197531 -> 0.198.
    #       (The plan's 0.200 assumed smooth = 0.50 exactly; an odd window over a 2-periodic field cannot give
    #       that - 41 tiles against 40 - and 16/81 is the same arithmetic carried through. Not a fault of the
    #       measure: a real pad field is not a perfect checkerboard, and the reading is a ratio either way.)
    flat_delta = np.full((64, 64), 0.40)
    full_blob = np.ones((64, 64), dtype=bool)
    flat_ripple = ripple(flat_delta, full_blob)
    assert abs(flat_ripple - 0.000) < 5e-4, 'a flat delta field read ripple %.6f, by hand 0.000' % flat_ripple
    pads_delta = flat_delta.copy()
    ti = np.arange(64) // RIPPLE_TILE
    checker = ((ti[:, None] + ti[None, :]) % 2) == 0
    pads_delta[checker] = 0.60
    pads_ripple = ripple(pads_delta, full_blob)
    assert abs(pads_ripple - 0.198) < 5e-4, ('a 4x4 pad checkerboard read ripple %.6f, by hand 16/81 = 0.198'
                                             % pads_ripple)
    print('             a flat delta reads ripple %.3f and a 4x4 pad checkerboard %.3f (hand: 0.000, 0.198)'
          % (flat_ripple, pads_ripple))

    print('selftest ok: centre pixel 10.000 m out, one degree up %.5f m high (hand: 0.34311);' % h)
    print('             stripes -> flat grey is a texture drop of %.3f (hand: 1.000);' % drop)
    print('             red/green %.1f deg, red/magenta %.1f deg (hand: 120, 60);' % (rg, rm))
    print('             10 m of footprint at the 10 m plane: %.3f m (hand: 3.333);' % bias)
    print('             one synthetic column of rows 0-100 has a face of %d pixels (hand: 35, rows 0-34)'
          % total_face)
    print('             a 256-pixel core of GasBankNight on grass reads %.2f deg, %.2f deg from the grass'
          % (core_hue, core_dist))
    print('             (hand: 67.5 and 162.5); the whole blob, fringe and all, reads %.2f deg (blend: 80.0)'
          % blob_hue)
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


def median_hue(rgb, mask):
    """Saturation-weighted circular MEDIAN hue, in degrees, of the masked pixels (None if nothing is masked).

    Outlier-proof where `mean_hue` is not: a handful of fringe or JPEG-ringing pixels pulls a mean, never a
    median. Circular, so the hues are first folded to signed offsets around their own mean and the weighted
    median of those offsets is added back (r2-3-g2-g2, step 8's last rung).
    """
    ref = mean_hue(rgb, mask)
    if ref is None:
        return None
    h, s = hue_sat(rgb)
    w, ang = s[mask], h[mask]
    off = ((ang - ref + 180.0) % 360.0) - 180.0
    order = np.argsort(off)
    off, w = off[order], w[order]
    cum = np.cumsum(w)
    if cum[-1] <= 1e-9:
        return None
    med = float(off[int(np.searchsorted(cum, 0.5 * cum[-1]))])
    return (ref + med) % 360.0


def hue_of(rgb, mask):
    """The one hue reading the probe quotes: `median_hue` (step 8's rung, see HUE_CORE_PCTL)."""
    return median_hue(rgb, mask)


def unmix_hue(rgb, plate, mask):
    """The alpha-free hue of the masked pixels: the circular weighted median of `hue(d)`, `d = shot - plate`.

    A gas card is alpha-blended: a masked pixel is `P = a*C + (1-a)*B` for plate colour `B`, card colour `C` and
    per-pixel alpha `a`, so `d = P - B = a*(C - B)`. The DIRECTION of `d` depends only on `C - B`, never on `a`:
    a faint fringe pixel and a solid core pixel from the same card colour point the same way, only their length
    differs. So `hue(d)` reads the card's own colour against the plate with no per-seed fringe-to-core ratio left
    to drag it around (unlike `hue_of`, which reads the hue of `P` itself, a blend whose ratio varies).
    `hue` is undefined on a vector with a negative channel, so negative channels of `d` are clamped to 0 first;
    clamping commutes with the positive scaling by `a`, so a clamped fringe pixel and a clamped core pixel from
    the same `C` still read the same hue (r2-3-g2-g2-g2, step 2). Weighted by `|d|` (L2), so pixels `d` barely
    moved (near-zero alpha, mostly noise) carry little weight.
    """
    d = rgb.astype(np.float64) - plate.astype(np.float64)
    d = np.clip(d, 0.0, 255.0)
    length = np.sqrt((d ** 2).sum(axis=2))
    if not mask.any():
        return None
    scaled = d.astype(np.uint8)
    h, _ = hue_sat(scaled)
    w = length[mask]
    ang = np.radians(h[mask])
    if w.sum() <= 1e-9:
        return None
    x = float((w * np.cos(ang)).sum())
    y = float((w * np.sin(ang)).sum())
    if abs(x) < 1e-12 and abs(y) < 1e-12:
        return None
    ref = math.degrees(math.atan2(y, x)) % 360.0
    off = ((np.degrees(ang) - ref + 180.0) % 360.0) - 180.0
    order = np.argsort(off)
    off_s, w_s = off[order], w[order]
    cum = np.cumsum(w_s)
    med = float(off_s[int(np.searchsorted(cum, 0.5 * cum[-1]))])
    return (ref + med) % 360.0


def hue_distance(rgb_a, rgb_b, mask):
    """Circular distance in degrees (0-180) between the two images' mean hue over the SAME masked pixels."""
    ha, hb = hue_of(rgb_a, mask), hue_of(rgb_b, mask)
    if ha is None or hb is None:
        return 0.0
    d = abs(ha - hb) % 360.0
    return min(d, 360.0 - d)


def core_of(mask, diff):
    """The blob's core: the masked pixels in the top (100 - HUE_CORE_PCTL) percent of plate difference.

    The gas card is alpha-blended, so a fringe pixel is mostly ground showing through and its hue is a mix whose
    ratio changes with the seed. The core is where the card's own colour dominates, so the hue read there is the
    paint, not the blend (r2-3-g2-g2, step 7).
    """
    if not mask.any():
        return mask
    cut = np.percentile(diff[mask], HUE_CORE_PCTL)
    return mask & (diff >= cut)


def _box_mean_wrap(a, weight, win):
    """Box mean of `a` over a win x win window of tiles, weighted by `weight`, wrapped at the edges.

    Weighted, so a tile outside the blob (weight 0) never drags the mean of a tile inside it down and invents a
    step at the blob's rim. Wrapped, so the frame edge invents no step either (and so the selftest's periodic
    checkerboard is exact at every tile, not just the interior ones).
    """
    pad = win // 2
    aw = np.pad(a * weight, pad, mode='wrap')
    ww = np.pad(weight, pad, mode='wrap')

    def box(x):
        c = np.cumsum(np.cumsum(x, axis=0), axis=1)
        c = np.pad(c, ((1, 0), (1, 0)), mode='constant')
        return c[win:, win:] - c[:-win, win:] - c[win:, :-win] + c[:-win, :-win]

    num, den = box(aw), box(ww)
    out = np.zeros_like(a)
    np.divide(num, den, out=out, where=den > 1e-12)
    return out


def ripple(delta, blob, tile=RIPPLE_TILE, win=RIPPLE_WIN, cover=RIPPLE_COVER):
    """How much the bank's own alpha steps at the 4 m-cell scale: pads read high, one mass reads low.

    Inside the blob, the per-pixel |luma_gas - luma_plate| the T1 and side measures already build is a proxy for
    the bank's own per-pixel alpha, free of the ground's texture. Average it over `tile` x `tile` pixel tiles
    into a coarse field `A` (and the blob's coverage into `W`, keeping only tiles that are at least `cover`
    blob), subtract a `win` x `win` tile box mean of `A` - the cloud's own slow dome, which is not a fault - and
    return `std(A - smooth) / mean(A)` over the kept tiles.

    One mass is smooth at the pad scale and reads low. A pad's rim is a step at exactly that scale and reads
    high. The ratio is free of how big or how bright the cloud is, which is what `side_face_px` was not: a
    carpet cleared that bar three times over and still spread 12x across three seeds of one build.
    """
    h, w = delta.shape
    th, tw = h // tile, w // tile
    d = (delta * blob)[:th * tile, :tw * tile].reshape(th, tile, tw, tile).swapaxes(1, 2)
    b = blob[:th * tile, :tw * tile].reshape(th, tile, tw, tile).swapaxes(1, 2)
    a = d.reshape(th, tw, tile * tile).mean(axis=2)
    cov = b.reshape(th, tw, tile * tile).mean(axis=2)
    keep = cov >= cover
    if keep.sum() < 2:
        return 0.0
    smooth = _box_mean_wrap(a, keep.astype(np.float64), win)
    mean = float(a[keep].mean())
    if mean <= 1e-9:
        return 0.0
    return float(np.std(a[keep] - smooth[keep]) / mean)


def block_variance(lum, mask):
    """Mean per-BLOCKxBLOCK luma variance over the blocks whose centre pixel is masked (0.0 if there are none)."""
    h, w = lum.shape
    bh, bw = h // BLOCK, w // BLOCK
    tiles = lum[:bh * BLOCK, :bw * BLOCK].reshape(bh, BLOCK, bw, BLOCK).swapaxes(1, 2)
    var = tiles.reshape(bh, bw, BLOCK * BLOCK).var(axis=2)
    centres = mask[BLOCK // 2::BLOCK, BLOCK // 2::BLOCK][:bh, :bw]
    return float(var[centres].mean()) if centres.any() else 0.0


def largest_blob(mask, dilate=BLOB_DILATE):
    """Keep only the mask pixels inside the single biggest coherent patch (r2-2's cleanup, on numpy arrays).

    JPEG block noise and frame-to-frame animation (moving infantry, flickering fires) scatter thin, isolated diff
    threads across the WHOLE frame, not just where the gas sits. The bank is the one dense patch. Dilate to bridge
    the small gaps JPEG leaves inside it, flood-fill the dilated pixels into components, keep the biggest, then
    intersect back with the ORIGINAL mask so only pixels that really differed from the plate are ever counted.
    """
    from collections import deque
    h, w = mask.shape
    dil = mask.copy()
    for _ in range(dilate):
        grown = dil.copy()
        grown[1:, :] |= dil[:-1, :]
        grown[:-1, :] |= dil[1:, :]
        grown[:, 1:] |= dil[:, :-1]
        grown[:, :-1] |= dil[:, 1:]
        dil = grown

    flat = dil.reshape(-1)
    seen = np.zeros(flat.size, dtype=bool)
    best = []
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
        if len(members) > len(best):
            best = members
    keep = np.zeros(flat.size, dtype=bool)
    keep[best] = True
    return mask & keep.reshape(h, w)


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


def ground_of(d, stem):
    """The ground height under this still's gas, from the pass manifest; the sidecar does not carry it."""
    for name in ('manifest.json', 'manifest-plate.json'):
        path = os.path.join(d, name)
        if not os.path.exists(path):
            continue
        with open(path, encoding='utf-8') as f:
            for row in json.load(f).get('stills', []):
                if row.get('still', '') == stem + '.png':
                    return float(row.get('ground_y', 0.0))
    return None


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
    return ({'t1_changed_frac': round(changed_frac, 4),
             'texture_var_drop': round(drop, 4),
             't1_ripple': round(ripple(delta, blob), 4)},
            {'t1_luma_delta': round(luma_delta, 4),
             't1_blob_px': int(blob.sum()),
             't1_block_var_plate': round(var_p, 6),
             't1_block_var_gas': round(var_g, 6),
             'gas_t1_height_m': height_of(blob, side, ground),
             't1_footprint_bias_m': column_bias(blob, side, ground),
             'pose_error_m': round(float(side.get('pose_error_m', 99.0)), 4)})


def column_bias(blob, side, ground):
    """The median column's `footprint_bias_m`: how much of `height_of`'s claim is only ground sloping away."""
    if ground is None or not blob.any():
        return 0.0
    h, w = blob.shape
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ys, xs = np.nonzero(blob)
    dists = []
    for px in np.unique(xs):
        rows_ = ys[xs == px]
        if rows_.size < MIN_COLUMN_PX:
            continue
        nx = 2.0 * (px + 0.5) / w - 1.0
        ny = 1.0 - 2.0 * (rows_.max() + 0.5) / h
        dist = ground_distance(cam, pitch, yaw, ground, nx, ny)
        if dist is not None and 1.0 < dist < 2000.0:
            dists.append(dist)
    if not dists:
        return 0.0
    dists.sort()
    dist = dists[len(dists) // 2]
    return round(footprint_bias_m(cam, pitch, yaw, ground, dist), 3)


def face_px(blob, side, ground):
    """The upright face of the bank in pixels: per column, the pixels above that column's own footprint bias.

    A column's base distance is taken exactly as `measure_side` does (`ground_distance` from the BOTTOM pixel),
    and `footprint_bias_m` at that distance is the highest a patch of ground sloping away beyond the base can
    ever read. So every pixel of the column that sits ABOVE that bias is a pixel the perspective lie cannot
    explain: it is the bank's own upright body on screen. Returns (total pixels, the median column's count, how
    many columns were measured) - the total grows with the bank, the median column is free of its width.
    """
    if ground is None or not blob.any():
        return 0, 0, 0
    h, w = blob.shape
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ys, xs = np.nonzero(blob)
    counts = []
    for px in np.unique(xs):
        rows_ = ys[xs == px]
        if rows_.size < MIN_COLUMN_PX:
            continue
        nx = 2.0 * (px + 0.5) / w - 1.0
        base_ny = 1.0 - 2.0 * (rows_.max() + 0.5) / h
        dist = ground_distance(cam, pitch, yaw, ground, nx, base_ny)
        if dist is None or not (1.0 < dist < 2000.0):
            continue
        bias = footprint_bias_m(cam, pitch, yaw, ground, dist)
        n = 0
        for py in rows_:
            ny = 1.0 - 2.0 * (py + 0.5) / h
            y = height_at(cam, pitch, yaw, ground, nx, ny, dist)
            if y is not None and y > bias:
                n += 1
        counts.append(n)
    if not counts:
        return 0, 0, 0
    counts.sort()
    return int(sum(counts)), int(counts[len(counts) // 2]), len(counts)


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
    core = core_of(mask, diff)
    blob_hue = mean_hue(img, mask)
    unmix = unmix_hue(img, plate, mask)
    off = None
    if unmix is not None:
        off = abs(unmix - HUE_PAINT_DEG) % 360.0
        off = min(off, 360.0 - off)
    return ({'o240_hue_dist_deg': round(hue_distance(img, plate, core), 2),
             'o240_hue_off_deg': round(off, 2) if off is not None else 999.0},
            {'o240_changed_frac': round(float((diff >= DIFF_BAR).mean()), 4),
             'o240_blob_px': int(mask.sum()),
             'o240_core_px': int(core.sum()),
             'o240_bank_hue_deg': None if hue_of(img, core) is None else round(hue_of(img, core), 1),
             'o240_grass_hue_deg': None if hue_of(plate, core) is None else round(hue_of(plate, core), 1),
             'o240_blob_hue_deg': None if blob_hue is None else round(blob_hue, 1),
             'o240_unmix_hue_deg': None if unmix is None else round(unmix, 1),
             'pose_error_m': round(float(side.get('pose_error_m', 99.0)), 4)})


def measure_side(d, seed):
    """One seed's side (grazing) frame: the bank's silhouette top, bias-corrected for ground sloping away."""
    stem = 'gas-side-s%d' % seed
    img, side = load(d, stem)
    plate, _ = load(d, stem + '-plate')
    lum_g, lum_p = luma(img), luma(plate)
    delta = np.abs(lum_g - lum_p)
    blob = largest_blob(delta >= LUMA_BAR)

    ground = ground_of(d, stem)
    h, w = blob.shape
    cam = (side['cam_x'], side['cam_y'], side['cam_z'])
    pitch, yaw = side['cam_pitch'], side['cam_yaw']
    ys, xs = np.nonzero(blob)
    tops, biases = [], []
    if ground is not None:
        for px in np.unique(xs):
            rows_ = ys[xs == px]
            if rows_.size < MIN_COLUMN_PX:
                continue
            nx = 2.0 * (px + 0.5) / w - 1.0
            base_ny = 1.0 - 2.0 * (rows_.max() + 0.5) / h
            dist = ground_distance(cam, pitch, yaw, ground, nx, base_ny)
            if dist is None or not (1.0 < dist < 2000.0):
                continue
            top_ny = 1.0 - 2.0 * (rows_.min() + 0.5) / h
            top_h = height_at(cam, pitch, yaw, ground, nx, top_ny, dist)
            if top_h is None:
                continue
            tops.append(top_h)
            biases.append(footprint_bias_m(cam, pitch, yaw, ground, dist))
    tops.sort()
    side_top_m = tops[len(tops) // 2] if tops else 0.0
    bias_m = sorted(biases)[len(biases) // 2] if biases else 0.0

    total_face, col_face, n_cols = face_px(blob, side, ground)
    blob_n = int(blob.sum())

    return ({'side_ripple': round(ripple(delta, blob), 4)},
            # `side_face_px` is a ROW now, not a bar: see MAX_BARS. It grows with how much gas there is, which
            # is not what the owner asked for, and a carpet cleared its 41000 three times over.
            {'side_face_px': total_face,
             'gas_side_height_m': round(max(0.0, side_top_m - bias_m), 3),
             'side_top_m': round(side_top_m, 3),
             'side_bias_m': round(bias_m, 3),
             'side_face_frac': round(total_face / blob_n, 4) if blob_n else 0.0,
             'side_face_col_px': col_face,
             'side_face_cols': n_cols,
             'side_blob_px': blob_n,
             'pose_error_m': round(float(side.get('pose_error_m', 99.0)), 4),
             'side_cam_pitch': round(float(pitch), 3),
             'side_cam_height_m': round(float(cam[1] - (ground if ground is not None else 0.0)), 3)})


def side_pose_bad(d):
    """None if every side sidecar sits at the pose the bar assumes (pitch 8, camera 7.8 m above ground); else why."""
    for seed in range(SEEDS):
        for tail in ('', '-plate'):
            stem = 'gas-side-s%d%s' % (seed, tail)
            _, side = load(d, stem)
            ground = ground_of(d, stem)
            pitch = float(side.get('cam_pitch', -999.0))
            if abs(pitch - SIDE_PITCH_DEG) > 1.0:
                return '%s: cam_pitch %.2f is not within 1 deg of %.1f' % (stem, pitch, SIDE_PITCH_DEG)
            if ground is None:
                continue
            cam_h = float(side.get('cam_y', -999.0)) - ground
            if abs(cam_h - 7.8) > 1.5:
                return '%s: camera stands %.2f m above ground, not within 1.5 m of 7.8' % (stem, cam_h)
    return None


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

    bad = side_pose_bad(d)
    if bad:
        print('side band pose is wrong, so its bar means nothing: ' + bad)
        return 1

    bars, rows = {k: [] for k in list(BARS) + list(MAX_BARS)}, {}
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
    result['max_bars'] = MAX_BARS
    result['owner_bars'] = OWNER_BARS
    result['rows'] = rows
    with open(os.path.join(d, out_name), 'w', encoding='utf-8') as f:
        json.dump(result, f, indent=1, sort_keys=True)

    ok = True
    print('seed  t1_changed_frac  texture_var_drop  o240_hue_dist_deg  o240_hue_off_deg  t1_ripple  '
          'side_ripple  side_face_px  side_face_frac  side_blob_px  side_height_m  pose_err_m')
    for seed in range(SEEDS):
        print('  s%d  %15.4f  %16.4f  %17.2f  %16.2f  %9.4f  %11.4f  %12d  %14.4f  %12d  %13.3f  %10.4f'
              % (seed, bars['t1_changed_frac'][seed], bars['texture_var_drop'][seed],
                 bars['o240_hue_dist_deg'][seed], bars['o240_hue_off_deg'][seed], bars['t1_ripple'][seed],
                 bars['side_ripple'][seed], rows['side_face_px'][seed],
                 rows['side_face_frac'][seed], rows['side_blob_px'][seed],
                 rows['gas_side_height_m'][seed], max(rows['pose_error_m'][seed * 3:seed * 3 + 3])))
    for k in ('t1_changed_frac', 'texture_var_drop', 'o240_hue_dist_deg'):
        got = bars[k]
        met = len(got) >= SEEDS and min(got) >= BARS[k]
        ok = ok and met
        print('%-18s want >= %-7s worst %-9s %s  (the owner asked for %s)'
              % (k, BARS[k], round(min(got), 4) if got else 'none', 'OK' if met else 'MISSED',
                 OWNER_BARS.get(k, 'n/a')))
    for k in MAX_BARS:
        got = bars[k]
        met = len(got) >= SEEDS and max(got) <= MAX_BARS[k]
        ok = ok and met
        print('%-18s want <= %-7s worst %-9s %s'
              % (k, MAX_BARS[k], round(max(got), 4) if got else 'none', 'OK' if met else 'MISSED'))
    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
