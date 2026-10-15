#!/usr/bin/env python3
"""Counts the separated weapons on each sheet in a folder: python sheetcheck.py FOLDER.

A sheet holds four weapons on a flat grey background (216, 216, 216), with typed words above and below each
one. For every *.png/*.jpg: mask pixels that differ from the background by more than 20 in any channel, dilate
by 16 px so a weapon's own near-touching parts (strap, bipod leg) count as one, label the components, and keep
those whose undilated ink area is at least 1 percent of the sheet's pixels -- that floor drops the typed words
and specks, which are much smaller than a weapon. Exits 1 if any sheet does not hold exactly four, or the folder
holds no sheet at all.
"""
import os
import sys

import numpy as np
from PIL import Image
from scipy import ndimage as ndi

BG = np.array([216, 216, 216])
TOL = 20
DILATE_PX = 16
MIN_AREA_FRAC = 0.01


def count_weapons(path):
    """Returns the list of kept components as (area, bbox) for one sheet."""
    im = np.asarray(Image.open(path).convert("RGB"), dtype=np.int16)
    diff = np.abs(im - BG).max(axis=2)
    fg = diff > TOL
    dilated = ndi.binary_dilation(fg, iterations=DILATE_PX)
    labels, n = ndi.label(dilated)
    floor = MIN_AREA_FRAC * fg.shape[0] * fg.shape[1]
    kept = []
    for i in range(1, n + 1):
        comp = labels == i
        area = int(fg[comp].sum())
        if area >= floor:
            ys, xs = np.where(comp)
            kept.append((area, (int(xs.min()), int(ys.min()), int(xs.max()), int(ys.max()))))
    return kept


def main(argv):
    if len(argv) != 2:
        print("usage: python sheetcheck.py FOLDER", file=sys.stderr)
        return 1
    folder = argv[1]
    sheets = sorted(
        f for f in os.listdir(folder) if f.lower().endswith((".png", ".jpg", ".jpeg"))
    )
    if not sheets:
        print(f"no sheet found in {folder}", file=sys.stderr)
        return 1
    bad = False
    for name in sheets:
        kept = count_weapons(os.path.join(folder, name))
        print(f"{name}  {len(kept)}")
        for area, bbox in kept:
            print(f"  area={area} bbox={bbox}")
        if len(kept) != 4:
            bad = True
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
