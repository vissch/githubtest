#!/usr/bin/env python3
"""Tile one still per bank of the ability-fx-captures-r2-2 fix chain, oldest to newest, so the owner can see it.

WHY. The owner's answer on the Decide page to "chain of fix units 8 deep, 26 units" was "show me visual results".
Numbers he already has: every bank folder carries a probe.json. What he has never had is the eight builds SIDE BY
SIDE, which is the only way to see that the chain made the gas bank into one mass instead of a pale stripe. This
script does nothing but arrange stills already committed here; it measures nothing and sets no bar. The share
printed under each tile was read ONCE, by hand, from `probe.py --against <bank>` run out of the -g1-g1 folder
(the measures are logged in the leg's measures.log and quoted in README.md) -- never recomputed here, so a tile's
label cannot drift from the bar the fix unit was judged against.

WHICH RUN. Each bank holds run1/run2/run3 of the same build; the newest capture (`now/`) is a single run. All
tiles therefore show run1, which is sim_tick 557 in every bank, so the rows are the same moment of the same shot
and the only difference between tiles is the build.
"""
import os
import sys

from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
EVIDENCE = os.path.dirname(HERE)

# (folder, label, bank_paint_share of run1). The shares are run1 of each bank as this chain's own probe reads it:
# `python probe.py --against <folder>` from ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1 on 2026-10-25.
# `--against` always exits 1 by design (an old build must fail the new bars), so the numbers are read off its
# report, not off an exit code.
BANKS = [
    ('ability-fx-captures-r2',                        'r2 (first shoot)',   0.0770),
    ('ability-fx-captures-r2-2-g2',                   'g2',                 0.0782),
    ('ability-fx-captures-r2-2-g2-g1',                'g2-g1',              0.1855),
    ('ability-fx-captures-r2-2-g2-g1-g2',             'g2-g1-g2',           0.1855),
    ('ability-fx-captures-r2-2-g2-g1-g2-g2',          'g2-g1-g2-g2',        0.1855),
    ('ability-fx-captures-r2-2-g2-g1-g2-g2-g2',       'g2-g1-g2-g2-g2',     0.1855),
    ('ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1',    '...-g2-g1',          0.2037),
    ('ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1', '...-g1-g1',          0.7488),
    ('ability-fx-captures-r2-2-visual-review-5/now',  'now (rebased)',      0.7496),
]

COLS = 3
TILE_W = 420
LABEL_H = 34


def still_of(folder, stem):
    """The bank's run1 still, or its top-level one: r2 and `now/` were packed flat, the banks into run1..run3."""
    for rel in (os.path.join('run1', stem + '.jpg'), stem + '.jpg'):
        p = os.path.join(EVIDENCE, folder, rel)
        if os.path.exists(p):
            return p
    return None


def sheet(stem, out_name):
    found = [(f, lab, share, still_of(f, stem)) for f, lab, share in BANKS]
    missing = [f for f, _, _, p in found if p is None]
    if missing:
        raise SystemExit('no %s.jpg in: %s' % (stem, ', '.join(missing)))

    first = Image.open(found[0][3])
    tile_h = int(round(TILE_W * first.height / first.width))
    rows = (len(found) + COLS - 1) // COLS
    out = Image.new('RGB', (COLS * TILE_W, rows * (tile_h + LABEL_H)), (18, 18, 18))
    draw = ImageDraw.Draw(out)

    for i, (_, label, share, path) in enumerate(found):
        img = Image.open(path).convert('RGB').resize((TILE_W, tile_h), Image.LANCZOS)
        x = (i % COLS) * TILE_W
        y = (i // COLS) * (tile_h + LABEL_H)
        out.paste(img, (x, y))
        draw.text((x + 6, y + tile_h + 9), '%s   paint share %.4f' % (label, share), fill=(235, 235, 235))

    path = os.path.join(HERE, out_name)
    for q in range(92, 40, -6):
        out.save(path, 'JPEG', quality=q)
        if os.path.getsize(path) <= 900 * 1024:
            break
    return path, len(found)


def main():
    if '--check' in sys.argv:
        missing = [f for f, _, _ in BANKS if still_of(f, 'gas-t1') is None]
        print('tiles %d' % len(BANKS))
        if missing:
            print('missing gas-t1.jpg: %s' % ', '.join(missing))
            return 1
        return 0

    for stem, name in (('gas-t1', 'chain-sheet.jpg'), ('gas-o240', 'chain-sheet-o240.jpg')):
        path, n = sheet(stem, name)
        print('%s: %d tiles, %d KB' % (os.path.basename(path), n, os.path.getsize(path) // 1024))
    return 0


if __name__ == '__main__':
    sys.exit(main())
