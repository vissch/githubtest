#!/usr/bin/env python3
"""Measure the chlorine gas bank in metres, bar the PAINT by the DIRECTION it lifts, and reject a plain card.

THE ROW PROFILE (new here). The owner's complaint, carried over unanswered: at the game camera the three
gas-t1 stills still read as horizontal yellow-green bands with dark ground between the rows, even though
`bank_paint_share` already sits at 0.7488/0.7575/0.7872 on this build. `bank_paint_share` cannot see the gap: a
card is 13.5-19.3 m wide on a 4 m grid, so in X the cards lie three to four deep and paint is CONTINUOUS across
columns, but in Z there is exactly ONE card per 4 m row and the only depth overlap is the 1.72 m band leaning
towards the next row. The paint mask BRIDGES the rows through that lean (one coherent patch, share 0.75) while
the brightness still dips to bare mud at every row seam — a fact about HEIGHT down the frame that a PATCH-shape
or PAINT-share measure, blind to where in the patch the paint sits, cannot read. `row_profile_stats` reads the
mean brightness lift of each image row, across the bank's own column span, and asks how much of the bank's own
on-screen height sits at less than half the bank's typical brightness: that is the dark mud line between rows,
measured directly, not inferred from paint connectedness.

WHY. g2's probe read `gas_hue_window_px` 1 / 3 / 0 on this build's three runs while `mask_px_raw` sat at
46,702 / 50,755 / 47,337 — the old 45-95 PIL hue window saw almost none of the plate-difference pixels as gas. On
the isolation still (`cards-gas.jpg` against `cards-plate.jpg`, the only thing that differs from the plate) the
gas paints 35,016 px, its COMPOSITE hue median is 156 PIL (blue, ~220 deg) and its green lean
`dG - max(dR,dB)` runs p10/p50/p90 = -18/-8/+4. So at night the gas composites to the night field BRIGHTENED, not
to green: the hue of the pixel is the night's, and even the lift does not lean green. Hue cannot find this paint.
What can: the gas card's own LIFT VECTOR (how far each of R, G, B rises over the plate) points the same way across
most of its paint even though the composite colour does not, because the card's tint is applied to night-dark
pixels of many different hues. `gas_signature` (see below) measures that one direction from the isolation still
and `gas_paint_stats` keeps, out of the whole frame's plate-difference-and-brighter pixels, only the ones whose
own lift vector falls within that direction's cone. This is why `--signature` prints `GAS_DIR`/`GAS_COS` from
`cards-gas.jpg` once, by hand, into the module — reading the cards at bar time would let a look change in the
cards silently redefine what counts as gas.

WHY THE CONE IS NOT ENOUGH. The owner's complaint, kept here as the record: the cone keeps 0.800 of the gas card's
own paint but also 0.714 of the smoke card's and 0.742 of the puff card's, so `gas_paint_frac` passes on any
translucent night card. The cone is an AMOUNT FLOOR (is there enough paint of roughly this lift?), not a test of
what the paint is. What is gas-specific is chlorine's tint: G over R. `gas_green_share` is the share of the
frame's plate-difference-and-brighter paint whose OWN lift leans green over red by at least `GREEN_LEAN` of the
lift's length. On whole-frame paint the three runs read 0.2296 / 0.2131 / 0.2357, the smoke card 0.0908 and the
puff card 0.1076, and nothing else measured separates them (README.md, "Why no other colour test"). The g1 build's
higher alpha is what lifted this key from the old sheet's 0.1535 / 0.1426 / 0.1495: less pale reads as more green.

THE REJECTION. Barring the new key is not enough on its own either: a bar can be set and the cards never checked.
So `card_reject_check` reads `cards-smoke.jpg` and `cards-puff.jpg` against `cards-plate.jpg` from `CARDS_DIR`
through the same `green_share` the frame goes through, and this probe REFUSES to exit 0 unless both cards sit
below `BAR_VALUES['gas_green_share']` by at least the three runs' own spread (or if `CARDS_DIR` is gone). One
function measures the frame and the cards, so the two can never drift apart.

THE FRAME. A RESHOOT: `run1`/`run2`/`run3` are three fresh captures of the g1 build (`CombatFx.Banks.cs` at
`3d7aa478ea`: cards 12-19.3 m wide so one spans three field cells, alpha 0.19/0.62 instead of 0.14/0.5, roll
+-12 degrees, squash 0.27), t1 `sim_tick` 557 / 597 / 637 again, twelve distinct hashes. Every bar below is re-set
from them. `one_frame_fault` still refuses two runs sharing a t1 tick or any two byte-identical stills.

THE MASK. The metre mask (`bank_mask`, used by `bank_low_share`/`bank_height_frac`/`bank_rise_m`) is untouched:
plate difference >= `T1_DIFF_BAR`, inside the one largest coherent patch, and brighter than the plate. The PAINT
mask (`gas_paint_stats`, replacing g2's `paint_stats`) now also requires the pixel's own lift vector to sit within
`GAS_COS` of `GAS_DIR` — this is what cuts infantry motion and JPEG noise, which lift in scattered directions,
without depending on the gas reading as any particular colour. Keys: `gas_paint_px`, `gas_paint_frac` (how much of
the frame is gas-direction paint), with `bank_paint_share`/`bank_patch_count` recomputed on this mask.
`gas_hue_window_px` is dropped; `composite_hue_median` and `gas_lift_dir` are printed per run, unbarred, as the
row that explains the owner's 1/3/0.

THE SPECIFICITY CHECK. `--signature` also measures what share of `cards-smoke.jpg`'s and `cards-puff.jpg`'s own
paint (same diff-and-lift test, against `cards-plate.jpg`) falls inside the gas cone. See README.md: the cone is
not gas-exclusive, and the README says so plainly rather than claim a colour test this probe cannot make at night.

THE BAR. Unchanged rule from g2: three runs of the SAME build, a bar must clear the nearest run by at least the
three runs' own spread (max - min), and `--against` always exits 1 (the old build must fail the new bars). The new
`gas_green_share` bar, 0.125, is the one value that satisfies that rule in BOTH directions at once: it clears the
runs by 1.61x their spread and leaves the smoke card failing by 3.14x and the puff card by 1.60x. It does not
reject the zebra (0.2446 / 0.2446 / 0.2447: flat yellow-green paint leans green) and is not meant to —
`bank_paint_share`/`bank_patch_count` are the keys that reject the zebra. See README.md.

USAGE
    python probe.py                 bar run1/run2/run3 against each other, print the cards/ look report
    python probe.py --calibrate      print every run's numbers with no bar check (even with only one run loaded)
    python probe.py --selftest       the camera maths against a hand calculation, no images
    python probe.py --signature     print GAS_DIR/GAS_COS and the three isolation stills' cone and green shares
    python probe.py --against DIR     measure another evidence folder's runs with THIS probe (always exits 1)

Exit 0 only when three runs are loaded and every bar clears its run values by at least their spread.
Exit 1 with 'three runs needed' if fewer than three run*/ directories hold a full set of stills.
Exit 1 with 'three captures, one frame' if two runs stand on one frame, or two stills are the same file.
Exit 1 with FAILED if a bar does not clear the run values by at least their spread, or if the smoke or puff
    isolation card is not below the `gas_green_share` bar by at least that spread.
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
STILLS = ['gas-t1', 'gas-t1-plate', 'gas-o240', 'gas-o240-plate']
CARDS = ['cards-gas', 'cards-smoke', 'cards-puff', 'cards-plate']
BARS = {'bank_low_share': 'ge', 'bank_height_frac': 'le', 'o240_area_frac': 'ge', 'pose_error_max': 'lt'}
PATCH_MIN_PX = 200            # a paint patch this big or bigger counts as its own patch (a card)
BARS['gas_paint_frac'] = 'ge'       # the gas-direction paint in the frame: how much of 1280x720 it actually covers
BARS['gas_green_share'] = 'ge'      # the chlorine tint: how much of that paint leans GREEN over RED in its own lift
BARS['bank_paint_share'] = 'ge'     # one bank: most of the frame's PAINT sits in ONE patch
BARS['bank_patch_count'] = 'le'     # one bank: few paint patches at all (the zebra is many separate cards)
BARS['row_dark_frac'] = 'le'        # the dark mud lines, as a share of the bank's own on-screen height
# row_gap_deepest is NOT in BARS: no value clears this build's own spread and also leaves the previous round's
# captures failing by more than that spread (see BAR_VALUES comment below), so this round leaves it unbarred and
# reports it, rather than fake a bar the rule cannot actually support.

# From `python probe.py --signature`, run once against ../ability-fx-captures-r2-2-g2-g1/cards/cards-gas.jpg (the
# isolation still, the only thing in the frame that differs from its plate). GAS_DIR is the normalised mean lift
# vector (dR, dG, dB) of that still's own paint (diff >= 20 RGB and Rec.709 luma lift >= 2, 35,016 px); GAS_COS is
# the cosine that keeps 80% of that same paint inside the cone. Hard-coded so a look change in the cards cannot
# silently redefine what this probe counts as gas; re-run --signature by hand if the cards change.
GAS_DIR = (0.4874, 0.5865, 0.6469)
GAS_COS = 0.9455
CARDS_DIR = os.path.join(HERE, '..', 'ability-fx-captures-r2-2-g2-g1', 'cards')   # the isolation stills' pose
# How far a pixel's own lift must lean GREEN over RED, as a share of the lift's length, to count as chlorine tint:
# `(dG - dR) / |lift| >= GREEN_LEAN`. 0.16 is the threshold whose margins are widest measured in the runs' OWN
# spread on the g2-g2-g2 runs (README.md there, "The green lean"); carried over unchanged, because it is a
# property of the measure and not of the build, and the g1 build only moves the runs FARTHER above the cards.
# At 0.16 the three g1 runs read 0.2296 / 0.2131 / 0.2357, the smoke card 0.0908 and the puff card 0.1076.
GREEN_LEAN = 0.16


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


def lift_vectors(img, plate, mask):
    """The (dR, dG, dB) lift of every pixel where `mask` is set, as a flat list aligned with it."""
    a, b = list(img.getdata()), list(plate.getdata())
    return [(p[0] - q[0], p[1] - q[1], p[2] - q[2]) for p, q, m in zip(a, b, mask) if m]


def green_share(vecs):
    """Share of these lift vectors whose own lift leans green over red by at least `GREEN_LEAN` of its length.

    The ONE place the green lean is measured. `gas_paint_stats` calls it on a frame's paint and
    `card_reject_check` calls it on an isolation card's paint, so the build and the cards can never be measured
    two different ways (see module docstring, THE REJECTION).
    """
    if not vecs:
        return 0.0
    lean = 0
    for dr, dg, db in vecs:
        m = math.sqrt(dr * dr + dg * dg + db * db)
        if m > 1e-9 and (dg - dr) / m >= GREEN_LEAN:
            lean += 1
    return round(lean / float(len(vecs)), 4)


def card_paint_vectors(cards_dir, name):
    """An isolation card's own paint lift vectors: the same diff-and-brighter test the frame's paint uses."""
    img, _ = load(cards_dir, name)
    plate, _ = load(cards_dir, 'cards-plate')
    diff = changed(img, plate, T1_DIFF_BAR)
    lift = brighter(img, plate)
    return lift_vectors(img, plate, [d and l for d, l in zip(diff, lift)])


def card_reject_check(spread):
    """FAIL unless the smoke card and the puff card both sit BELOW the green-lean bar by at least `spread`.

    The cone (`gas_paint_frac`) is an amount floor and keeps 0.714 of the smoke card and 0.742 of the puff card:
    on its own this probe would pass any translucent night card. This check makes the rejection part of the
    verdict, so the probe cannot go green while a card it should refuse would also pass.
    """
    bar = BAR_VALUES['gas_green_share']
    if not os.path.isdir(CARDS_DIR) or not all(
            os.path.exists(os.path.join(CARDS_DIR, s + '.jpg')) for s in CARDS):
        print('card rejection: %s missing the isolation stills -> FAILED (the rejection cannot be shown)'
              % CARDS_DIR)
        return False
    ok = True
    for name in ('cards-smoke', 'cards-puff'):
        share = green_share(card_paint_vectors(CARDS_DIR, name))
        under = bar - share
        rejected = under >= spread
        print('  %-12s gas_green_share %.4f  bar %.4f  under by %.4f (%.2fx spread) -> %s'
              % (name, share, bar, under, under / spread if spread > 1e-9 else 0.0,
                 'REJECTED' if rejected else 'NOT REJECTED'))
        ok = rejected and ok
    return ok


def gas_signature(cards_dir):
    """GAS_DIR (normalised mean lift vector) and GAS_COS (cosine holding 80% of the paint) from cards-gas.jpg.

    Printed by `--signature` for hand-copying into the `GAS_DIR`/`GAS_COS` constants above; never read at bar
    time (see module docstring, WHY).
    """
    img, _ = load(cards_dir, 'cards-gas')
    plate, _ = load(cards_dir, 'cards-plate')
    diff = changed(img, plate, T1_DIFF_BAR)
    lift = brighter(img, plate)
    mask = [d and l for d, l in zip(diff, lift)]
    vecs = lift_vectors(img, plate, mask)
    sx, sy, sz = (sum(v[0] for v in vecs), sum(v[1] for v in vecs), sum(v[2] for v in vecs))
    n = math.sqrt(sx * sx + sy * sy + sz * sz)
    direction = (sx / n, sy / n, sz / n)
    coses = sorted((v[0] * direction[0] + v[1] * direction[1] + v[2] * direction[2])
                   / max(math.sqrt(v[0] ** 2 + v[1] ** 2 + v[2] ** 2), 1e-9) for v in vecs)
    cos_bar = coses[int(round(0.20 * (len(coses) - 1)))]
    return direction, cos_bar, len(vecs)


def cone_share(img, plate, direction, cos_bar):
    """Share of `img`'s own diff-and-lift paint (against `plate`) whose lift vector falls inside the cone."""
    diff = changed(img, plate, T1_DIFF_BAR)
    lift = brighter(img, plate)
    mask = [d and l for d, l in zip(diff, lift)]
    vecs = lift_vectors(img, plate, mask)
    if not vecs:
        return 0.0, 0
    inside = sum(1 for v in vecs
                 if (v[0] * direction[0] + v[1] * direction[1] + v[2] * direction[2])
                 / max(math.sqrt(v[0] ** 2 + v[1] ** 2 + v[2] ** 2), 1e-9) >= cos_bar)
    return round(inside / float(len(vecs)), 4), len(vecs)


def gas_paint_stats(img, plate, bar=T1_DIFF_BAR):
    """The PAINT, split two ways: by PATCH (unchanged from g2) and by DIRECTION (new, see module docstring, WHY).

    `bank_paint_share`/`bank_patch_count` are g2's own measure, untouched: the biggest coherent plate-difference-
    and-brighter patch's share of all such paint, and how many patches of `PATCH_MIN_PX`+ there are. That measure
    already separates this build from the zebra cleanly (see README.md, "The bars") because it reads PATCH
    COHERENCE, not colour — cutting it per pixel by `GAS_COS` was tried and FAILED: a translucent card's own
    interior shading lifts in scattered directions pixel to pixel even where the composite hue is uniform (the
    same fact `bank_mask`'s own comment records for the metre mask), so a per-pixel cone test fragments the one
    true patch into dozens under `PATCH_MIN_PX` and the zebra becomes indistinguishable from the build. `gas_paint_px`/
    `gas_paint_frac` are the new, separate count this probe adds: the SAME whole-frame paint, kept only where a
    pixel's own lift vector sits within `GAS_COS` of `GAS_DIR` — a floor under the build (like g2's old
    `paint_frac`), not a patch-shape test, so it is unaffected by that fragmentation.
    """
    diff = changed(img, plate, bar)
    lift = brighter(img, plate)
    paint = [d and l for d, l in zip(diff, lift)]
    vecs_all = lift_vectors(img, plate, paint)
    w, h = img.size
    sizes = [sum(1 for j in m if paint[j]) for m in blobs(paint, w, h)]
    paint_px = sum(1 for p in paint if p)
    biggest = max(sizes) if sizes else 0

    def in_cone(v):
        m = math.sqrt(v[0] ** 2 + v[1] ** 2 + v[2] ** 2)
        return m > 1e-9 and (v[0] * GAS_DIR[0] + v[1] * GAS_DIR[1] + v[2] * GAS_DIR[2]) / m >= GAS_COS

    gas_paint_px = sum(1 for v in vecs_all if in_cone(v))
    gas_green_lean = green_share(vecs_all)

    hsv = list(img.convert('HSV').getdata())
    hues = sorted(h for p, (h, s, v) in zip(paint, hsv) if p)
    composite_hue_median = hues[len(hues) // 2] if hues else -1

    mean_lift = (0.0, 0.0, 0.0)
    if vecs_all:
        n = max(len(vecs_all), 1)
        sx = sum(v[0] for v in vecs_all) / n
        sy = sum(v[1] for v in vecs_all) / n
        sz = sum(v[2] for v in vecs_all) / n
        mean_lift = (round(sx, 2), round(sy, 2), round(sz, 2))

    return {
        'gas_paint_px': gas_paint_px,
        'gas_paint_frac': round(gas_paint_px / float(w * h), 4),
        'gas_green_share': gas_green_lean,
        'bank_paint_share': round(biggest / float(max(paint_px, 1)), 4),
        'bank_patch_count': sum(1 for s in sizes if s >= PATCH_MIN_PX),
        'composite_hue_median': composite_hue_median,
        'gas_lift_dir': mean_lift,
    }


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


ROW_SMOOTH = 5           # box width in rows, against JPEG ringing
ROW_EXTENT_FLOOR = 0.25   # share of the peak lift a row must keep to count as part of the bank's own height
ROW_CORE_TRIM = 0.15      # share trimmed off EACH end of the extent before reading troughs, see WHY below
# Share of the core's median lift under which a row counts as a dark mud line. SHARPENED here, 0.75 -> 0.85: at
# 0.75 the previous round's striped build read row_dark_frac 0.0140 / 0.0140 / 0.0000 (../ability-fx-captures-r2-2
# -g2-g1-g2-g2-g2-g1-g1-g1/README.md), and no `le` bar can sit outside a spread that touches zero - a bar below 0
# does not exist - so the measure could pass a picture the owner still sees as stripes. That build's deepest core
# rows sit at 0.70-0.76 of its own median (row_gap_deepest 0.2438-0.3012), so 0.85 brackets them and the striped
# captures read a real share of dark rows again. Only the threshold moved; the profile, extent and trim are g1's.
ROW_TROUGH_FLOOR = 0.85


def row_profile_stats(img, plate):
    """How much of the bank's OWN on-screen height reads as a dark mud line between rows.

    `gas_paint_stats`/`bank_mask` read paint CONNECTEDNESS across the whole frame and miss the row gaps because
    the cards overlap in X (bridging the patch) but not in Z (see module docstring). This reads brightness lift
    PER IMAGE ROW, across the bank's own column span, so a row sitting on bare mud between two cards shows up as
    a trough regardless of whether some other column's paint keeps the frame-wide patch connected.

    WHY `ROW_CORE_TRIM`. The extent's own near/far edges (`ROW_EXTENT_FLOOR` of the peak, by definition) fall off
    towards the plate just because that is where the bank ENDS, not because a row there is a gap between cards.
    On this build's run1 the raw extent reads 5.5 at its near edge and 21.0 at its true peak, so a trough bar read
    off the WHOLE extent's median (14.2) never crosses its own edges without crossing them on every run — the
    edges, not the inter-row gaps, would set `row_dark_frac`. Trimming `ROW_CORE_TRIM` off each end before taking
    the median and hunting troughs keeps the measure reading the rows the gas painted over, not the bank's own
    silhouette boundary.
    """
    w, h = img.size
    paint = [c and b for c, b in zip(changed(img, plate, T1_DIFF_BAR), brighter(img, plate))]
    xs = [i % w for i, p in enumerate(paint) if p]
    if not xs:
        return {'row_dark_frac': 0.0, 'row_gap_deepest': 0.0, 'band_lift_med': 0.0,
                'row_dark_count': 0, 'row_dark_spacing_px': 0.0}
    xs.sort()
    x_lo = xs[int(round(0.05 * (len(xs) - 1)))]
    x_hi = xs[int(round(0.95 * (len(xs) - 1)))]

    pix = list(img.getdata())
    pix_p = list(plate.getdata())
    lift = []
    for y in range(h):
        row = y * w
        vals = [max(0.0, luma(*pix[row + x]) - luma(*pix_p[row + x])) for x in range(x_lo, x_hi + 1)]
        lift.append(sum(vals) / len(vals) if vals else 0.0)

    smooth = []
    half = ROW_SMOOTH // 2
    for y in range(h):
        lo, hi = max(0, y - half), min(h, y + half + 1)
        window = lift[lo:hi]
        smooth.append(sum(window) / len(window))

    peak = max(smooth)
    if peak <= 1e-9:
        return {'row_dark_frac': 0.0, 'row_gap_deepest': 0.0, 'band_lift_med': 0.0,
                'row_dark_count': 0, 'row_dark_spacing_px': 0.0}
    peak_y = smooth.index(peak)
    floor = ROW_EXTENT_FLOOR * peak
    lo = peak_y
    while lo > 0 and smooth[lo - 1] >= floor:
        lo -= 1
    hi = peak_y
    while hi < h - 1 and smooth[hi + 1] >= floor:
        hi += 1
    band = smooth[lo:hi + 1]
    trim = int(round(ROW_CORE_TRIM * len(band)))
    core = band[trim:len(band) - trim] if len(band) - 2 * trim >= 3 else band
    core_sorted = sorted(core)
    band_lift_med = core_sorted[len(core_sorted) // 2]

    trough_bar = ROW_TROUGH_FLOOR * band_lift_med
    dark_rows = [y for y, v in enumerate(core) if v < trough_bar]
    row_dark_frac = round(len(dark_rows) / float(len(core)), 4) if core else 0.0

    min_core = min(core) if core else band_lift_med
    row_gap_deepest = round(min(1.0, max(0.0, (band_lift_med - min_core) / band_lift_med)), 4) \
        if band_lift_med > 1e-9 else 0.0

    # dark-row runs, for the unbarred spacing report: how many separate troughs, and their median pitch in px
    runs = []
    start = None
    for i, y in enumerate(dark_rows):
        if start is None:
            start = y
        elif y != dark_rows[i - 1] + 1:
            runs.append(start)
            start = y
    if start is not None:
        runs.append(start)
    spacings = [b - a for a, b in zip(runs, runs[1:])]
    spacing_med = sorted(spacings)[len(spacings) // 2] if spacings else 0.0

    return {
        'row_dark_frac': row_dark_frac,
        'row_gap_deepest': row_gap_deepest,
        'band_lift_med': round(band_lift_med, 3),
        'row_dark_count': len(runs),
        'row_dark_spacing_px': round(spacing_med, 1),
    }


def measure_t1(run_dir):
    img, side = load(run_dir, 'gas-t1')
    plate, _ = load(run_dir, 'gas-t1-plate')
    w, h = img.size
    mask, raw_count, patch_count, _ = bank_mask(img, plate, bar=T1_DIFF_BAR, blob=True)
    paint = gas_paint_stats(img, plate)
    paint.update(row_profile_stats(img, plate))
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
               'bank_width_m': 0.0, 'bank_aspect': 0.0}
        out.update(paint)
        return out
    med_h = heights[len(heights) // 2]
    dvals = sorted(dists.values())
    med_d = dvals[len(dvals) // 2]
    tan_h = math.tan(math.radians(FOV_V_DEG * 0.5)) * ASPECT
    frame_h = 2.0 * med_d * math.tan(math.radians(FOV_V_DEG * 0.5))

    # THE SHEET, AS A NUMBER (unbarred): the one bank patch's own width, kept only where it also lifts towards
    # GAS_DIR, so a sheet (wide) and a puff (narrow) read differently here even while `bank_rise_m` agrees.
    a, b = list(img.getdata()), list(plate.getdata())
    gas_cols = [px for px in dists if (lambda v: math.sqrt(v[0] ** 2 + v[1] ** 2 + v[2] ** 2) > 1e-9
                                        and (v[0] * GAS_DIR[0] + v[1] * GAS_DIR[1] + v[2] * GAS_DIR[2])
                                        / math.sqrt(v[0] ** 2 + v[1] ** 2 + v[2] ** 2) >= GAS_COS)
                ((a[columns[px] * w + px][0] - b[columns[px] * w + px][0],
                  a[columns[px] * w + px][1] - b[columns[px] * w + px][1],
                  a[columns[px] * w + px][2] - b[columns[px] * w + px][2]))]
    frame_w = 2.0 * med_d * tan_h
    bank_width_m = round((max(gas_cols) - min(gas_cols) + 1) / float(w) * frame_w, 3) if gas_cols else 0.0

    row = {
        'bank_low_share': round(low / float(total), 4),
        'bank_height_frac': round(med_h / frame_h, 4),
        'bank_rise_m': round(med_h, 3),
        'bank_area_frac': round(mask_px / float(w * h), 4),
        'mask_px': mask_px,
        'mask_px_raw': raw_count,
        'mask_px_patch': patch_count,
        'bank_width_m': bank_width_m,
        'bank_aspect': round(bank_width_m / med_h, 3) if med_h > 1e-9 else 0.0,
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

    if '--signature' in sys.argv:
        cards_dir = CARDS_DIR
        direction, cos_bar, n = gas_signature(cards_dir)
        print('GAS_DIR = (%.4f, %.4f, %.4f)   GAS_COS = %.4f   (from cards-gas.jpg, %d paint px)'
              % (direction[0], direction[1], direction[2], cos_bar, n))
        for name in ('cards-gas', 'cards-smoke', 'cards-puff'):
            img, _ = load(cards_dir, name)
            plate, _ = load(cards_dir, 'cards-plate')
            share, total = cone_share(img, plate, direction, cos_bar)
            print('  %-12s cone share %.4f  green share %.4f  (%d paint px)'
                  % (name, share, green_share(card_paint_vectors(cards_dir, name)), total))
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
            print('  %-6s gas_paint_frac %.4f  gas_green_share %.4f  bank_paint_share %.4f  bank_patch_count %d'
                  '  (paint %d px)  bank_low_share %.4f  row_dark_frac %.4f  row_gap_deepest %.4f  t1 sim_tick %s'
                  % (os.path.basename(d), out['gas_paint_frac'], out['gas_green_share'], out['bank_paint_share'],
                     out['bank_patch_count'], out['gas_paint_px'], out['bank_low_share'],
                     out['row_dark_frac'], out['row_gap_deepest'],
                     frames_of(d).get('gas-t1')))
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
              '  o240_area_frac %.4f  pose_error_max %.4f  mask_px %d (raw %d, patch %d)'
              '  gas_paint_px %d  gas_paint_frac %.4f  gas_green_share %.4f'
              '  bank_paint_share %.4f  bank_patch_count %d'
              '  composite_hue_median %d  gas_lift_dir %s  bank_width_m %.3f  bank_aspect %.3f'
              '  row_dark_frac %.4f  row_gap_deepest %.4f  band_lift_med %.3f  row_dark_count %d  row_dark_spacing_px %.1f'
              % (name, out['bank_low_share'], out['bank_height_frac'], out['bank_rise_m'], out['bank_area_frac'],
                 out['o240_area_frac'], out['pose_error_max'], out['mask_px'], out['mask_px_raw'],
                 out['mask_px_patch'], out['gas_paint_px'], out['gas_paint_frac'], out['gas_green_share'],
                 out['bank_paint_share'], out['bank_patch_count'],
                 out['composite_hue_median'], out['gas_lift_dir'],
                 out['bank_width_m'], out['bank_aspect'],
                 out['row_dark_frac'], out['row_gap_deepest'], out['band_lift_med'],
                 out['row_dark_count'], out['row_dark_spacing_px']))

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

    green = [results[r]['gas_green_share'] for r in results]
    print('card rejection (the cone passes any translucent night card; the green lean must refuse these two):')
    ok = card_reject_check(max(green) - min(green)) and ok

    with open(os.path.join(HERE, 'probe.json'), 'w', encoding='utf-8') as f:
        json.dump({'runs': results, 'look': look, 'bars': BAR_VALUES, 'sim_ticks': ticks},
                  f, indent=1, sort_keys=True)
    print('OK' if ok else 'FAILED')
    return 0 if ok else 1


# Bar values. Re-set in THIS round from the jitter-widened build's own three runs (`probe.json`, t1 sim_tick
# 556 / 596 / 636, GasBankJitterM a full field cell, commit 50c8d44ea5). A bar must clear the nearest run by at
# least the three runs' own spread. See README.md, "The bars", for the table these came out of, and which keys
# moved the wrong way against the previous round's captures.
BAR_VALUES = {
    'bank_low_share': 0.08,      # runs 0.0896/0.0877/0.0912, spread 0.0035 -> 0.0877 - 0.0035 = 0.0842 is the roof
    'bank_height_frac': 0.35,    # runs 0.3286/0.3349/0.3214, spread 0.0135 -> 0.3349 + 0.0135 = 0.3484 is the floor
    'o240_area_frac': 0.013,     # runs 0.0167/0.0190/0.0196, spread 0.0029, margin 0.0037 (unchanged, still clears)
    'pose_error_max': 0.5,       # GasBankStills.PoseBar; runs 0.0000, spread 0.0000
    'gas_paint_frac': 0.015,     # runs 0.0198/0.0188/0.0178, spread 0.0020, margin 0.0028 (1.4x spread)
    'gas_green_share': 0.41,     # runs 0.4258/0.4229/0.4270, spread 0.0041, margin 0.0129 (3.1x spread); the two
    # isolation cards (0.0908/0.1076) still fail it by over 70x their own spread.
    # The jitter widening (50c8d44ea5) spreads the cards further apart in X, so the biggest patch now bridges LESS
    # of the frame's paint than before: runs 0.6161/0.6903/0.7272 against the previous round's 0.7488/0.7575/0.7872
    # -- this key moved the WRONG way (down). spread 0.1111, bar <= 0.6161 - 0.1111 = 0.5050.
    'bank_paint_share': 0.50,
    # Also moved the wrong way: runs 19/26/30 (spread 11, up from 11/15/18) -- the jitter fragments the row lean
    # into more separate patches. bar >= 30 + 11 = 41. At this spread the bar can no longer promise the zebra (40
    # patches) fails this key alone; bank_paint_share and gas_green_share are what still reject it.
    'bank_patch_count': 41,
    # THE OWNER'S GAP MEASURE (new this round). runs 0.0140/0.0140/0.0000, spread 0.0140 -> bar <= 0.0140 + 0.0140
    # = 0.0280 to clear this build, AND bar < 0.0455 - 0.0140 = 0.0315 to leave the previous round's striped
    # captures (0.0455/0.0455/0.0537) failing by more than that spread. 0.03 sits in that window (margin 1.14x
    # this build's spread, 1.11x over the previous round's floor).
    'row_dark_frac': 0.03,
}
# row_gap_deepest has NO bar (not in BARS): see README.md, "row_gap_deepest does not clear". No value satisfies
# both sides of the rule at once (clearing this build's own spread, runs 0.2980/0.3012/0.2438, needs bar >=
# 0.3012 + 0.0574 = 0.3586; leaving the previous round's captures, min 0.2997, failing by more than that same
# spread needs bar < 0.2997 - 0.0574 = 0.2423) -- the two requirements do not overlap, with or without
# GasBankThin raised to 0.19 -> 0.28 (tried and reverted: it made both row_dark_frac and row_gap_deepest WORSE,
# not better; see README.md).



if __name__ == '__main__':
    sys.exit(main())
