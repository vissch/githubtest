#!/usr/bin/env python3
"""Is the Death Battalion man a different man on screen than the rifleman standing next to him?

Round 1 answered "silhouette_diff 0.73" and the harsh critic scored it 28/100, rightly: the rifleman-vs-rifleman
noise of the same measure was 0.40, so 0.73 was under 2x the noise, and each man had been shot at his own spot on
his own frame. This probe measures three men standing SIDE BY SIDE in one frame (shoot.py): same clip, same tick,
same light, same weather, same ground. Two of them are riflemen, so every number comes with its own noise floor
measured the same way in the same pixels.

Per seed (1, 2, 3), from `seed<s>_men.png` against `seed<s>_bg.png`:

  mask        |men - bg| summed over RGB above TOL, opened 3x3, closed 5x5, labelled; the three largest blobs of
              MIN_BLOB px or more, sorted by image x, are the Death Battalion man, riflemanA, riflemanB. The camera
              looks back along x (shoot.py's yaw 90 against the scene's BaseYaw 270), so the man spawned LAST - the
              Death Battalion, at the greatest world x - stands LEFTMOST on screen. Round 2 measured that with a
              throwaway capture (kit (1,0,0), coat 0.25 m, helm 0.07 m): only the leftmost man changed, and his head
              IoU against the other two was 0.674 and 0.691 while theirs against each other was 0.957. Before that
              the probe read the rightmost blob as the Death Battalion and so scored one rifleman against another. The men
              stand still (Speed 0) and the clock is held, so the same three masks also locate them in the fire
              frame.
  db_diff     count(xor)/count(or) of the whole masks, each recentred on its own centroid in a BOX px box, the
              MIN over the two riflemen: the Death Battalion man has to beat BOTH of them.
  noise_diff  the same between the two riflemen: what pose, shadow and rain do between two men who are the same.
  head_iou    the same recentring on the top HEAD_FRAC of each mask's bounding-box height (head and helmet), as
              intersection over union, the MAX over the two riflemen; head_iou_noise is the two riflemen's.
  torso_luma  Rec.709 luma 0-1 (CaptureRig's scale), the median inside each mask between TORSO_LO and TORSO_HI of
              its bounding-box height. The entry is min(rifleA, rifleB) - db, once for the no-fire frame and once
              for the fire frame: six entries over three seeds, so the cloth has to read darker in fire light too.
              torso_luma_noise is |rifleA - rifleB| in the same frame.

The bars, all of which must hold for exit 0:

    min(db_diff) >= max(noise_diff) + 0.15   and   min(db_diff) >= 3 * max(noise_diff)
    min(torso_luma_step) >= 0.08
    max(head_iou) <= 0.80

    python docs/evidence/death-battalion-look-r2/probe.py before   measures before_*.png, writes before.json
    python docs/evidence/death-battalion-look-r2/probe.py          measures the after stills, writes probe.json

THE SPREAD OF THE UNCHANGED BUILD, measured on this build before anything was touched (before.json, the
before_*.png stills beside it, commit 5ba0fafb5a):

    db_diff          0.1053  0.1128  0.2719      noise_diff  0.0890  0.0940  0.1300
    head_iou         0.9568  0.9496  0.5702      head_iou_noise  0.9143 (all three)
    torso_luma_step  0.0045  -0.0020  0.0036  0.0031  0.0045  0.0062   (no-fire, fire, per seed)

Read that and the round-1 verdict is plain. The Death Battalion man differs from the rifleman beside him by about
as much as one rifleman differs from another (0.11 against 0.09), his head is the rifleman's head, and the 0.65
cloth multiply moves his torso by four thousandths of a luma - nothing. Every bar above sits outside this spread:
the diff must reach 0.28 (0.13 + 0.15) and 0.39 (3 x 0.13), the torso step 0.08 where the build manages 0.006, and
the head IoU must come down to 0.80 from 0.96. None of them can be reached by re-rolling the dice.
"""
import json
import sys
from pathlib import Path

import numpy as np
from PIL import Image
from scipy import ndimage

HERE = Path(__file__).resolve().parent
SEEDS = (1, 2, 3)
TOL = 20           # summed over RGB, 0-765: the men are a night silhouette against night mud, so the step is small
MIN_BLOB = 1200    # px: a man at this zoom and 2560x1440 comes to about 1,700 px; anything smaller is not a man
BOX = 600          # the box each mask is recentred into, px
HEAD_FRAC = 0.15   # the top of the bounding box that is head and helmet
TORSO_LO, TORSO_HI = 0.20, 0.55   # the band of the bounding box that is coat and webbing

DIFF_MARGIN = 0.15
DIFF_RATIO = 3.0
LUMA_STEP = 0.08
HEAD_IOU_MAX = 0.80


def rgb(path):
    return np.asarray(Image.open(path).convert('RGB'), dtype=float)


def luma(a):
    """Rec.709 luma on 0-1, the scale CaptureRig's sidecars use."""
    return (0.2126 * a[:, :, 0] + 0.7152 * a[:, :, 1] + 0.0722 * a[:, :, 2]) / 255.0


def men_masks(men_png, bg_png):
    """The three men's own pixels, left to right: what changed between the empty ground and the lineup."""
    a, b = rgb(men_png), rgb(bg_png)
    if a.shape != b.shape:
        raise SystemExit('%s and %s are different sizes' % (men_png.name, bg_png.name))
    moved = np.abs(a - b).sum(axis=2) > TOL
    moved = ndimage.binary_opening(moved, np.ones((3, 3)))     # single rain pixels are not a man
    moved = ndimage.binary_closing(moved, np.ones((5, 5)))     # but the holes inside him are
    labels, n = ndimage.label(moved)
    if n == 0:
        raise SystemExit('%s: nothing changed against the empty ground: nobody is in frame' % men_png.name)
    sizes = np.array([int(np.count_nonzero(labels == i)) for i in range(1, n + 1)])
    order = list(np.argsort(sizes)[::-1][:3])
    big = [int(sizes[i]) for i in order]
    if len(order) < 3 or min(big) < MIN_BLOB:
        raise SystemExit('%s: wanted three blobs of %d px or more, got %s' % (men_png.name, MIN_BLOB, sorted(sizes)[::-1][:5]))
    masks = [labels == int(i) + 1 for i in order]
    masks.sort(key=lambda m: float(ndimage.center_of_mass(m)[1]))      # left to right on screen
    return masks, sorted(big, reverse=True)


def centred(mask, box=BOX):
    m = np.pad(mask, box)
    cy, cx = (int(round(v)) for v in ndimage.center_of_mass(m))
    return m[cy - box // 2:cy + box // 2, cx - box // 2:cx + box // 2]


def diff(a, b):
    union = np.count_nonzero(a | b)
    if union == 0:
        raise SystemExit('both masks are empty')
    return np.count_nonzero(a ^ b) / union


def iou(a, b):
    union = np.count_nonzero(a | b)
    if union == 0:
        raise SystemExit('both head masks are empty')
    return np.count_nonzero(a & b) / union


def band(mask, lo, hi):
    """The rows of a mask between lo and hi of its own bounding-box height, top down."""
    rows = np.flatnonzero(mask.any(axis=1))
    y0, y1 = int(rows[0]), int(rows[-1]) + 1
    h = y1 - y0
    out = np.zeros_like(mask)
    out[y0 + int(round(lo * h)):y0 + int(round(hi * h))] = mask[y0 + int(round(lo * h)):y0 + int(round(hi * h))]
    return out


def seed(stem_men, stem_bg, stem_fire):
    masks, sizes = men_masks(stem_men, stem_bg)
    db, rifle_a, rifle_b = masks          # left to right on screen: see the module docstring

    whole = [centred(m) for m in (db, rifle_a, rifle_b)]
    db_diff = min(diff(whole[0], whole[1]), diff(whole[0], whole[2]))
    noise_diff = diff(whole[1], whole[2])

    heads = [centred(band(m, 0.0, HEAD_FRAC)) for m in (db, rifle_a, rifle_b)]
    head_iou = max(iou(heads[0], heads[1]), iou(heads[0], heads[2]))
    head_iou_noise = iou(heads[1], heads[2])

    torsos = [band(m, TORSO_LO, TORSO_HI) for m in (db, rifle_a, rifle_b)]
    steps, noises, absolute = [], [], []
    for png in (stem_men, stem_fire):
        y = luma(rgb(png))
        med = [float(np.median(y[t])) for t in torsos]
        steps.append(min(med[1], med[2]) - med[0])
        noises.append(abs(med[1] - med[2]))
        absolute.append([round(v, 4) for v in med])   # db, rifleA, rifleB: the headroom the 0.08 bar needs
    return dict(db_diff=db_diff, noise_diff=noise_diff, head_iou=head_iou, head_iou_noise=head_iou_noise,
                torso_luma_step=steps, torso_luma_noise=noises, torso_luma_abs=absolute, blob_px=sizes)


def main():
    before = len(sys.argv) > 1 and sys.argv[1] == 'before'
    prefix = 'before_' if before else ''
    out = {'which': 'before' if before else 'after', 'db_diff': [], 'noise_diff': [], 'head_iou': [],
           'head_iou_noise': [], 'torso_luma_step': [], 'torso_luma_noise': [], 'torso_luma_abs': [], 'blob_px': [],
           'files': [],
           'tolerance': TOL, 'min_blob_px': MIN_BLOB, 'box_px': BOX, 'head_frac': HEAD_FRAC,
           'torso_band': [TORSO_LO, TORSO_HI],
           'bars': {'diff_margin': DIFF_MARGIN, 'diff_ratio': DIFF_RATIO, 'torso_luma_step': LUMA_STEP,
                    'head_iou_max': HEAD_IOU_MAX}}
    for s in SEEDS:
        names = ['%sseed%d_%s.png' % (prefix, s, k) for k in ('men', 'bg', 'fire')]
        paths = [HERE / n for n in names]
        for p in paths:
            if not p.exists():
                raise SystemExit('%s is missing: run shoot.py%s first' % (p.name, ' --prefix before_' if before else ''))
        r = seed(*paths)
        out['db_diff'].append(round(r['db_diff'], 4))
        out['noise_diff'].append(round(r['noise_diff'], 4))
        out['head_iou'].append(round(r['head_iou'], 4))
        out['head_iou_noise'].append(round(r['head_iou_noise'], 4))
        out['torso_luma_step'] += [round(v, 4) for v in r['torso_luma_step']]
        out['torso_luma_noise'] += [round(v, 4) for v in r['torso_luma_noise']]
        out['torso_luma_abs'].append(r['torso_luma_abs'])    # [[db, rifleA, rifleB] no-fire, [..] fire] per seed
        out['blob_px'].append(r['blob_px'])
        out['files'].append(names)

    checks = {
        'diff_beats_noise_by_margin': bool(min(out['db_diff']) >= max(out['noise_diff']) + DIFF_MARGIN),
        'diff_beats_noise_by_ratio': bool(min(out['db_diff']) >= DIFF_RATIO * max(out['noise_diff'])),
        'torso_darker_everywhere': bool(min(out['torso_luma_step']) >= LUMA_STEP),
        'head_differs': bool(max(out['head_iou']) <= HEAD_IOU_MAX),
    }
    out['checks'] = checks
    out['pass'] = bool(all(checks.values()))
    (HERE / ('before.json' if before else 'probe.json')).write_text(json.dumps(out, indent=1), encoding='utf-8')
    print(json.dumps(out, indent=1))
    return 0 if out['pass'] else 1


sys.exit(main())
