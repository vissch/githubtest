#!/usr/bin/env python3
"""sheet.py - the visual result: a before/after contact sheet of the chlorine-bank captures.

The owner asked for pictures, not numbers ("show me visual results"). This builds `sheet.jpg` beside it:

  top row     docs/evidence/ability-fx-captures-r2/gas-t1.jpg and gas-o240.jpg - the look 26 units ago
  bottom row  this folder's run*/gas-t1.jpg and run1/gas-o240.jpg - the same two shots on the rebased tip

Every tile carries its folder, the frame its sidecar was photographed at, and the three numbers that moved:
`green_low_share`, `green_height_frac` and `t1_pad_aspect` on the T1 tile (`o240_green_area_frac`,
`o240_cloud_share` and `pose_error_max` on the O240 tile, which those three do not describe), measured by this folder's probe.py so both rows are
measured the same way. No bar is checked here and nothing is decided: it only draws.

    python sheet.py            write sheet.jpg (at least 1200 px wide, under 400 KB)

Exit 0 when the sheet is written, 1 when a tile's folder is missing.
"""
import glob
import json
import os
import sys

from PIL import Image, ImageDraw

import probe

HERE = os.path.dirname(os.path.abspath(__file__))
BEFORE = os.path.normpath(os.path.join(HERE, '..', 'ability-fx-captures-r2'))
TILE_W = 640                  # four tiles of this wide give a 1280 px sheet, over the 1200 px check
PAD = 8
LABEL_H = 54
# The three numbers that moved are T1 field measures, so the O240 tile shows its own three instead.
KEYS = {'gas-t1': ('green_low_share', 'green_height_frac', 't1_pad_aspect'),
        'gas-o240': ('o240_green_area_frac', 'o240_cloud_share', 'pose_error_max')}


def frame_of(run_dir, stem):
    path = os.path.join(run_dir, stem + '.json')
    with open(path, encoding='utf-8') as f:
        return json.load(f).get('frame')


def tiles():
    """(label lines, image path) for every tile, top row first: before, then this build's runs."""
    out = []
    for run_dir in [BEFORE] + sorted(glob.glob(os.path.join(HERE, 'run*'))):
        if not os.path.isdir(run_dir):
            continue
        name = os.path.basename(run_dir)
        nums = probe.measure_run(run_dir)
        for stem in ('gas-t1', 'gas-o240'):
            jpg = os.path.join(run_dir, stem + '.jpg')
            if not os.path.exists(jpg):
                continue
            lines = ['%s / %s   frame %s' % (name, stem, frame_of(run_dir, stem)),
                     '  '.join('%s %.4f' % (k, nums[k]) for k in KEYS[stem])]
            out.append((lines, jpg))
    return out


def build(cells, cols):
    rows = (len(cells) + cols - 1) // cols
    sample = Image.open(cells[0][1])
    tile_h = int(round(TILE_W * sample.size[1] / float(sample.size[0])))
    cell_h = tile_h + LABEL_H
    sheet = Image.new('RGB', (cols * TILE_W + (cols + 1) * PAD, rows * cell_h + (rows + 1) * PAD), (16, 16, 18))
    draw = ImageDraw.Draw(sheet)
    for i, (lines, jpg) in enumerate(cells):
        x = PAD + (i % cols) * (TILE_W + PAD)
        y = PAD + (i // cols) * (cell_h + PAD)
        sheet.paste(Image.open(jpg).convert('RGB').resize((TILE_W, tile_h), Image.LANCZOS), (x, y))
        for j, line in enumerate(lines):
            draw.text((x + 4, y + tile_h + 4 + j * 14), line, fill=(235, 235, 220))
    return sheet


def main():
    cells = tiles()
    if not cells:
        print('no tiles: neither %s nor any run* folder holds gas-t1.jpg' % BEFORE)
        return 1
    # The before row is two tiles wide, so two columns keep before above after.
    sheet = build(cells, 2)
    out = os.path.join(HERE, 'sheet.jpg')
    for q in range(92, 40, -6):          # pack()'s own quality ladder, so the sheet stays under 400 KB
        sheet.save(out, 'JPEG', quality=q)
        if os.path.getsize(out) <= 400 * 1024:
            break
    print('sheet.jpg %dx%d  %d KB  %d tiles' % (sheet.size[0], sheet.size[1], os.path.getsize(out) // 1024,
                                                len(cells)))
    return 0


if __name__ == '__main__':
    sys.exit(main())
