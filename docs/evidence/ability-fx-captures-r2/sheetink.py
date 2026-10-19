#!/usr/bin/env python3
"""sheetink.py - two offline pixel measures for the chlorine-bank hunt (ability-fx-captures-r2-2).

Mode one, the SHEET: cut a flipbook PNG into cols x rows cells and print the mean alpha and mean RGB of each.
CombatFx.Banks.cs draws every gas card between GasBankFirst 17 and GasBankLast 28 ("the bank at its full spread").
If those cells of GasBank.png are near-empty the book draws nothing and no capture fix could ever show it.

    python sheetink.py <sheet.png> <cols> <rows> [frames]

Mode two, the BLOBS: count the pixels that differ between a still and its plate, over the whole frame and per third
(top, middle, bottom and left, centre, right), with the same bar the probe uses (RGB distance >= 12). A drawn 12 m
card at the T1 scale (0.054 m/px) is about 220 px wide, so it changes well over 2000 pixels; under 2000 it drew
nothing at all.

    python sheetink.py --blobs <still.png> <plate.png>

Exit 0 when it measured something, 1 on a bad call or a missing file.
"""
import os
import sys

try:
    from PIL import Image
except ImportError:
    sys.stderr.write("PIL (pillow) is needed: python -m pip install pillow\n")
    sys.exit(1)

DIFF_BAR = 12               # the same bar probe.py uses, so the two agree on "changed"
ROWS = ("top", "middle", "bottom")
COLS = ("left", "centre", "right")


def cells(path, cols, rows, frames):
    """Mean alpha and mean RGB of every cell of a sheet, row-major, as the book numbers its frames."""
    img = Image.open(path).convert("RGBA")
    w, h = img.size
    if w % cols or h % rows:
        sys.stderr.write("%d x %d does not cut into %d x %d cells\n" % (w, h, cols, rows))
        return None
    cw, ch = w // cols, h // rows
    px = img.load()
    out = []
    for r in range(rows):
        for c in range(cols):
            n = r * cols + c
            if frames and n >= frames:
                continue
            sa = sr = sg = sb = 0
            for y in range(r * ch, (r + 1) * ch):
                for x in range(c * cw, (c + 1) * cw):
                    pr, pg, pb, pa = px[x, y]
                    sa += pa
                    sr += pr * pa
                    sg += pg * pa
                    sb += pb * pa
            area = cw * ch
            mean_a = sa / (area * 255.0)
            # RGB weighted by alpha: an unmultiplied sheet's transparent pixels carry junk colour
            d = float(sa) if sa else 1.0
            out.append((n, mean_a, sr / d, sg / d, sb / d))
    return (w, h, cw, ch, out)


def blobs(a_path, b_path):
    """Changed pixels per horizontal band and per vertical band, and the whole frame's count."""
    a = Image.open(a_path).convert("RGB")
    b = Image.open(b_path).convert("RGB")
    if a.size != b.size:
        sys.stderr.write("the two pictures are not the same size: %s vs %s\n" % (a.size, b.size))
        return None
    w, h = a.size
    pa, pb = a.load(), b.load()
    bar2 = DIFF_BAR * DIFF_BAR
    rows, cols = [0, 0, 0], [0, 0, 0]
    for y in range(h):
        band = min(2, y * 3 // h)
        for x in range(w):
            r1, g1, b1 = pa[x, y]
            r2, g2, b2 = pb[x, y]
            dr, dg, db = r1 - r2, g1 - g2, b1 - b2
            if dr * dr + dg * dg + db * db >= bar2:
                rows[band] += 1
                cols[min(2, x * 3 // w)] += 1
    return (w, h, rows, cols)


def main():
    argv = sys.argv[1:]
    if "--blobs" in argv:
        i = argv.index("--blobs")
        if len(argv) < i + 3:
            sys.stderr.write(__doc__)
            return 1
        for p in argv[i + 1:i + 3]:
            if not os.path.exists(p):
                sys.stderr.write("no such file: %s\n" % p)
                return 1
        got = blobs(argv[i + 1], argv[i + 2])
        if got is None:
            return 1
        w, h, rows, cols = got
        print("%s against %s, %d x %d, changed at RGB distance >= %d" % (
            os.path.basename(argv[i + 1]), os.path.basename(argv[i + 2]), w, h, DIFF_BAR))
        print("  whole  %8d px  %.4f of the frame" % (sum(rows), sum(rows) / float(w * h)))
        for names, counts in ((ROWS, rows), (COLS, cols)):
            for n, name in enumerate(names):
                print("  %-6s %8d px  %.4f of the third" % (name, counts[n], counts[n] / float(w * h / 3)))
        return 0

    if len(argv) < 3:
        sys.stderr.write(__doc__)
        return 1
    path = argv[0]
    if not os.path.exists(path):
        sys.stderr.write("no such file: %s\n" % path)
        return 1
    try:
        cols, rows = int(argv[1]), int(argv[2])
        frames = int(argv[3]) if len(argv) > 3 else 0
    except ValueError:
        sys.stderr.write(__doc__)
        return 1
    got = cells(path, cols, rows, frames)
    if got is None:
        return 1
    w, h, cw, ch, rowsout = got
    print("%s  %d x %d, cells %d x %d of %d x %d px" % (os.path.basename(path), w, h, cols, rows, cw, ch))
    print("frame  mean_alpha   R     G     B")
    for n, a, r, g, b in rowsout:
        print("%5d  %10.4f  %5.1f %5.1f %5.1f" % (n, a, r, g, b))
    return 0


if __name__ == "__main__":
    sys.exit(main())
