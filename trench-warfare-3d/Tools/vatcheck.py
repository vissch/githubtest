"""Checks on what the battle actually draws: every clip of a baked VAT figure (Resources/Units/Figure<Name>Atlas.bytes),
rifle included, which the skeleton checks (clipcheck.py) cannot see. Per clip:
  floor   the lowest the rifle's axis goes, cm (below -6 = through the ground: a dead man's rifle standing in the mud)
  body    the lowest the body goes, cm (a few cm under is the pack's corner, not a fault)
No rifle-through-head check: measured against the rifle the made whistle swung past the face (0.4 m out, level with
the head) and a sniper's cheek on the stock, a distance from the skull cannot tell them apart; judge that by eye.
Flags are printed with the frame; exit 1 when any clip is flagged, so it can gate a bake.
Usage: python Tools/vatcheck.py [Soldier Sniper ...] [--atlas <file>]
  --atlas checks one atlas file (e.g. one pulled from git history) as the first figure named.
"""
import sys, os, re, gzip, struct
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
PROJECT = os.path.dirname(HERE)
UNITS = os.path.join(PROJECT, 'Assets', '_Project', 'Resources', 'Units')
CONTROLLER = os.path.join(PROJECT, 'Assets', '_Project', 'Presentation', 'Core', 'AnimationController.cs')
RIFLE = 24               # the rifle box is the last 24 vertices of every figure (VATBaker: skin, brim, rifle)
FLOOR = -0.06            # metres: the rifle below this is through the ground


def clip_names():
    body = open(CONTROLLER, encoding='utf-8-sig').read()
    body = body[body.index('enum Clip'):]
    body = re.sub(r'//[^\n]*', '', body[body.index('{') + 1:body.index('}')])
    return [n.strip() for n in body.split(',') if n.strip()]


def read_atlas(path):
    b = gzip.open(path, 'rb').read()
    o, ln, shift = 0, 0, 0
    while True:   # BinaryWriter string: 7-bit length
        c = b[o]; o += 1; ln |= (c & 0x7F) << shift; shift += 7
        if c < 0x80: break
    o += ln
    vc, total, rows = struct.unpack_from('<iii', b, o); o += 12
    table = []
    for _ in range(rows):
        s, n, _sec = struct.unpack_from('<iif', b, o); o += 12
        table.append((s, abs(n)))   # the sign marks a one-shot clip
    mn = np.array(struct.unpack_from('<fff', b, o)); o += 12
    sz = np.array(struct.unpack_from('<fff', b, o)); o += 12
    pos = np.frombuffer(b, dtype='<u2', count=vc * total * 3, offset=o).reshape(total, vc, 3).astype(np.float32) / 65535.0 * sz + mn
    return pos, table


def rifle_axis(r, samples=9):
    """Points along the rifle box's long axis (its centre line, not its surface)."""
    c = r.mean(0)
    u = np.linalg.svd(r - c)[2][0]
    t = (r - c) @ u
    return c + np.outer(np.linspace(t.min(), t.max(), samples), u)


def check(fig, pos, table, names):
    flagged = []
    print('%-8s %-18s %6s %7s %7s' % ('figure', 'clip', 'frames', 'floor', 'body'))
    for k, (start, count) in enumerate(table):
        if k >= len(names) or count == 0: continue
        low, body, at = 9.0, 9.0, 0
        for f in range(count):
            p = pos[start + f]
            y = rifle_axis(p[-RIFLE:])[:, 1].min()
            if y < low: low, at = y, f
            body = min(body, p[:-RIFLE, 1].min())
        bad = ['rifle %d cm under at frame %d' % (round(-low * 100), at)] if low < FLOOR else []
        print('%-8s %-18s %6d %6.0f %6.0f  %s' % (fig, names[k], count, low * 100, body * 100, '; '.join(bad)))
        if bad: flagged.append((fig, names[k], bad))
    return flagged


def main(argv):
    names = clip_names()
    figs, atlas = [], None
    i = 0
    while i < len(argv):
        a = argv[i]
        if a == '--atlas': atlas = argv[i + 1]; i += 2; continue
        figs.append(a); i += 1
    figs = figs or ['Soldier', 'Sniper']
    flagged = []
    for n, fig in enumerate(figs):
        path = atlas if (atlas and n == 0) else os.path.join(UNITS, 'Figure%sAtlas.bytes' % fig)
        pos, table = read_atlas(path)
        if len(table) != len(names) - 1:
            print('%s: %d atlas rows, %d clips in the enum (rebake?)' % (fig, len(table), len(names) - 1))
        flagged += check(fig, pos, table, names)
    print('\n%d flagged' % len(flagged))
    for fig, name, bad in flagged: print('  %s %s: %s' % (fig, name, '; '.join(bad)))
    return 1 if flagged else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
