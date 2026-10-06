#!/usr/bin/env python3
"""The frogs of the house page (house.html), packed into sheets a page can draw.

    python Tools/assetboard/sprites.py                 from trench-warfare-3d/: pack the sprites into this station's cache
    python Tools/assetboard/sprites.py --src FOLDER    ... from that copy of the sprite project
    python Tools/assetboard/sprites.py --out FOLDER    ... into that folder

WHY. The frogs are the owner's art and a project of their own (pepe_frog_walk_h3), outside git: a PNG per frame, each
on a 258 px canvas that is mostly empty. A page cannot ask for 250 files and draw from them. So every animation
becomes one sheet, cut down to what is drawn on it, at full and at half size (the half one is drawn while a frog is
small on the screen), and frog.json says what is on each sheet and where the frog's feet are.

The sheets go to the station's board cache (the folder frog under LOCALAPPDATA, TrenchWarfare/assetboard, beside
crew). ops.py copies them into the site (img/frog/) and writes the manifest as data/frog.js. A station that never
ran this has no sheets, and the house draws every worker as the mark of its kind.

What it reads, in the sprite project: normalized/258/<DIR>/ (a walk loop per heading), actions/258/<name>/ (the
actions; never the _flip twins, the page mirrors by itself), actions/<name>.json (an action's speed, and whether it
loops), actions/258/office_desk.png (the empty desk). The project is the first that exists of --src, TW_FROG_SRC,
Documents/drive/pepe_frog_walk_h3 (the desktop) and Documents/claude/pepe_frog_walk_h3 (the laptop's copy).
Needs Pillow.
"""
import argparse
import json
import math
import os
import statistics
import sys
from pathlib import Path

HEADINGS = ('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW')       # round the compass
KEEP_FIRST = ('S', 'SW', 'W', 'NW', 'N', 'NE', 'E', 'SE')     # of two loops that mirror each other the earlier is packed
MIRROR = dict(N='N', NE='NW', E='W', SE='SW', S='S', SW='SE', W='E', NW='NE')
ACTIONS = dict(sleep_in=8, sleep_loop=6, think=8, wrench=8, magnifier=10, dance=12, office=8)     # frames a second, when its json holds none
ONCE = ('sleep_in',)        # played once; every other action is a loop
WALK_FPS = 8                # tools/normalize.py of the sprite project writes the walk previews at 125 ms a frame
# How far the frog travels in one frame of a walk loop, in sprite pixels at full size: the page moves it this far a
# frame, so its feet do not slide. Measured by following the planted foot on the 512 px masters, good to about a
# fifth. Typed, not measured here: MEASURE THEM AGAIN if the walk art changes. A mirror walks as its source does.
STRIDE = dict(S=6.5, SW=5.6, SE=5.6, W=7.9, E=7.9, NW=5.1, NE=5.1)
STRIDE_ELSE = 5.5
SHEET_WIDTH = 2048          # no sheet is wider
PAD = 2
HOMES = ('Documents/drive/pepe_frog_walk_h3', 'Documents/claude/pepe_frog_walk_h3')


def source(arg=''):
    """The sprite project on this machine, or None."""
    named = arg or os.environ.get('TW_FROG_SRC', '')
    for p in ([Path(named)] if named else [Path.home() / h for h in HOMES]):
        if (p / 'normalized').is_dir() or (p / 'actions').is_dir():
            return p
    return None


def frames_of(folder: Path):
    from PIL import Image
    return [Image.open(f).convert('RGBA') for f in sorted(folder.glob('*.png'))]


def solid(im, over):
    """Where a frame is drawn: its pixels with more alpha than `over`, as a mask."""
    return im.getchannel('A').point(lambda a: 255 if a > over else 0)


def crop_box(frames):
    """What is drawn on any frame of an animation, with PAD pixels round it, inside the canvas. A faint halo (alpha
    1 to 8) reaches three pixels beyond the solid edge of these sprites; it is not kept."""
    box = None
    for im in frames:
        b = solid(im, 8).getbbox()
        if b:
            box = b if box is None else (min(box[0], b[0]), min(box[1], b[1]), max(box[2], b[2]), max(box[3], b[3]))
    w, h = frames[0].size
    return (0, 0, w, h) if box is None else (max(0, box[0] - PAD), max(0, box[1] - PAD), min(w, box[2] + PAD), min(h, box[3] + PAD))


def half(im, size):
    """A frame at half size. The colour of the empty pixels round a cut-out must not bleed into its edge (a dark
    fringe): Pillow resizes an RGBA picture with its colour multiplied by its alpha, which is what keeps it out.
    The test looks at the edge, so another way of making it smaller that darkens it is seen."""
    from PIL import Image
    return im.resize(size, Image.LANCZOS)


def sheet(frames, out: Path, name):
    """One animation as a sheet: its frames cut to what is drawn, row by row, at full and at half size. Returns what
    the page needs to draw from it: the number of frames, how many stand in a row, a frame's size and where its
    corner was on the canvas."""
    from PIL import Image
    box = crop_box(frames)
    w, h = box[2] - box[0], box[3] - box[1]
    cols = max(1, min(len(frames), SHEET_WIDTH // w))
    rows = math.ceil(len(frames) / cols)
    sizes = {}
    for tag, (cw, ch) in (('', (w, h)), ('@0.5', (round(w / 2), round(h / 2)))):
        sh = Image.new('RGBA', (cols * cw, rows * ch), (0, 0, 0, 0))
        for i, im in enumerate(frames):
            c = im.crop(box)
            sh.paste(c if not tag else half(c, (cw, ch)), ((i % cols) * cw, (i // cols) * ch))
        tmp = out / f'{name}{tag}.png.tmp'
        sh.save(tmp, format='PNG', optimize=True)
        tmp.replace(out / f'{name}{tag}.png')
        sizes[tag] = (sh.size, (out / f'{name}{tag}.png').stat().st_size)
    return dict(n=len(frames), cols=cols, w=w, h=h, ox=box[0], oy=box[1]), sizes


def mirrors(a, b):
    """Whether one frame is the other seen in a mirror (no channel of any pixel more than 2 apart)."""
    from PIL import Image, ImageChops
    if a.size != b.size:
        return False
    return max(hi for _, hi in ImageChops.difference(a, b.transpose(Image.FLIP_LEFT_RIGHT)).getextrema()) <= 2


def foot_of(frames):
    """The point of the canvas that stands on the ground. A walking frog lifts its feet, so the floor is the lowest
    row any frame reaches, not the middle one; across the canvas it is the middle of the head (the top two fifths of
    what is drawn), which sways less than the feet."""
    low, mids = [], []
    for im in frames:
        m = solid(im, 127)
        b = m.getbbox()
        if not b:
            continue
        low.append(b[3])
        top = m.crop((0, b[1], im.size[0], b[1] + max(1, round((b[3] - b[1]) * 0.4)))).getbbox()
        mids.append((top[0] + top[2] - 1) / 2)
    return [math.floor(statistics.median(mids) + 0.5), max(low)] if low else None      # a half goes up: a mirrored pair of walks lands on one


def nearest(heading, have):
    """The heading with a loop that is closest round the compass; of two as close, the one clockwise (N takes NE)."""
    at = HEADINGS.index(heading)
    for step in range(1, 5):
        for way in (1, -1):
            h = HEADINGS[(at + way * step) % 8]
            if h in have:
                return h
    return None


def pack(src: Path, out: Path, say=lambda *a: None):
    """Pack the sprite project `src` into `out`: a sheet and a half sheet per animation, and frog.json, which is also
    returned. Sheets an earlier pack left and this one does not name are not removed; nothing reads them."""
    out.mkdir(parents=True, exist_ok=True)
    man = dict(cell=0, foot=None, scales=[1, 0.5], anims={}, walk={}, stills={})
    total = {'': 0, '@0.5': 0}

    def add(key, frames, **more):
        a, sizes = sheet(frames, out, key)
        man['cell'] = man['cell'] or frames[0].size[0]
        for tag, (size, n) in sizes.items():
            total[tag] += n
        say(f'  {key:11} {a["n"]:2} frames  {a["w"]:3} x {a["h"]:3}  sheet {sizes[""][0][0]:4} x {sizes[""][0][1]:3}  {sizes[""][1]:7,} + {sizes["@0.5"][1]:6,} bytes')
        return dict(a, **more)

    walks_at = src / 'normalized' / '258'
    loops = {d.name.upper(): frames_of(d) for d in sorted(walks_at.iterdir()) if d.is_dir() and d.name.upper() in HEADINGS} if walks_at.is_dir() else {}
    loops = {h: fr for h, fr in loops.items() if fr}
    for h in KEEP_FIRST:                             # a loop of its own, unless it is another's mirror
        if h not in loops:
            continue
        twin = MIRROR[h]
        if twin != h and twin in man['walk'] and not man['walk'][twin]['flip'] and mirrors(loops[h][0], loops[twin][0]):
            man['walk'][h] = dict(anim=f'walk_{twin}', flip=True)
            continue
        man['anims'][f'walk_{h}'] = add(f'walk_{h}', loops[h], fps=WALK_FPS, loop=True, stride=STRIDE.get(h, STRIDE_ELSE))
        man['walk'][h] = dict(anim=f'walk_{h}', flip=False)
    have = set(man['walk'])
    for h in HEADINGS:                               # no loop that way: the nearest one stands in
        if h not in have and have:
            man['walk'][h] = dict(man['walk'][nearest(h, have)], standin=True)
    man['walk'] = {h: man['walk'][h] for h in HEADINGS if h in man['walk']}

    acts_at = src / 'actions' / '258'
    for name, fps in ACTIONS.items():
        frames = frames_of(acts_at / name) if (acts_at / name).is_dir() else []
        if not frames:
            say(f'  {name}: not in {acts_at}, left out')
            continue
        try:
            said = json.loads((src / 'actions' / f'{name}.json').read_text(encoding='utf-8'))
        except (OSError, ValueError):
            said = {}
        speed = said.get('fps') or fps
        man['anims'][name] = add(name, frames, fps=int(speed) if float(speed).is_integer() else speed, loop=name not in ONCE)
    desk = next((p for p in (acts_at / 'office_desk.png', src / 'actions' / 'office_desk.png') if p.exists()), None)
    if desk:                                         # on the canvas of the office loop, so it stands where that desk does
        from PIL import Image
        a = add('desk', [Image.open(desk).convert('RGBA')])
        man['stills']['desk'] = {k: a[k] for k in ('w', 'h', 'ox', 'oy')}

    # the feet are measured on the walks (every heading on disk, so a mirrored pair balances); with no walk at all
    # they are where these sprites have them, in proportion
    man['foot'] = foot_of([fr for frames in loops.values() for fr in frames]) or [round(man['cell'] * 0.5), round(man['cell'] * 216 / 258)]
    say(f'  full size {total[""]:,} bytes, half size {total["@0.5"]:,} bytes; the feet stand on {man["foot"]}')
    tmp = out / 'frog.json.tmp'
    tmp.write_text(json.dumps(man, indent=1), encoding='utf-8')
    tmp.replace(out / 'frog.json')
    return man


def main(argv=None):
    ap = argparse.ArgumentParser(description="pack the frog sprites into the house page's sheets")
    ap.add_argument('--src', default='', help='the sprite project (pepe_frog_walk_h3)')
    ap.add_argument('--out', default='', help="where the sheets go; this station's board cache when not given")
    args = ap.parse_args(argv)
    try:
        import PIL  # noqa: F401
    except ImportError:
        print('sprites: this needs Pillow (pip install pillow)')
        return 1
    src = source(args.src)
    if src is None:
        where = args.src or os.environ.get('TW_FROG_SRC') or ' or '.join(str(Path.home() / h) for h in HOMES)
        print(f'sprites: no sprite project at {where} (name it with --src or TW_FROG_SRC)')
        return 1
    if args.out:
        out = Path(args.out)
    else:
        sys.path.insert(0, str(Path(__file__).resolve().parent))
        import ops
        out = ops.CREW / 'frog'
    print(f'sprites: {src} -> {out}')
    man = pack(src, out, print)
    if not man['anims']:
        print('sprites: no animation found there')
        return 1
    own = [h for h, w in man['walk'].items() if not w['flip'] and not w.get('standin')]
    print(f'  walks: {", ".join(own)} drawn; ' + ', '.join(f'{h} is {w["anim"][5:]}{" mirrored" if w["flip"] else ""}{" (a stand-in)" if w.get("standin") else ""}'
                                                    for h, w in man['walk'].items() if h not in own))
    return 0


if __name__ == '__main__':
    sys.exit(main())
