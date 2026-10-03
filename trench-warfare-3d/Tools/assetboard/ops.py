#!/usr/bin/env python3
"""The floor: every branch with its items and what is happening in it, and the skills and agents, on the bench or at
work on a branch. A page of the asset board (floor.html) that re-reads its data every 20 seconds.

    python Tools/assetboard/ops.py                 write the page and its data once
    python Tools/assetboard/ops.py --watch 20      and again every 20 seconds until stopped (keeps the page live)

The data is data/ops.js beside the page (a script, not JSON: a page opened from the Drive as a file may not fetch
JSON, it may load a script). It is written only when it changed. What it reads: src_ops.py.
"""
import argparse
import json
import os
import shutil
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import build      # noqa: E402
import src_ops    # noqa: E402


def write_if_changed(path: Path, text: str):
    if path.exists() and path.read_text(encoding='utf-8') == text:
        return False
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + '.tmp')
    tmp.write_text(text, encoding='utf-8')
    tmp.replace(path)
    return True


def page(out: Path, meta):
    from jinja2 import Environment, FileSystemLoader, select_autoescape
    env = Environment(loader=FileSystemLoader(str(HERE / 'templates')), autoescape=select_autoescape(['html']), trim_blocks=True, lstrip_blocks=True)
    env.globals.update(meta=meta)
    write_if_changed(out / 'floor.html', env.get_template('floor.html').render(root=''))
    for name in ('floor.js', 'crew.js', 'office.js', 'site.css', 'kinetic.css'):
        write_if_changed(out / name, (HERE / 'static' / name).read_text(encoding='utf-8'))
    crew(out)


CREW = Path(os.environ.get('LOCALAPPDATA', str(Path.home()))) / 'TrenchWarfare' / 'assetboard'


def crew(out: Path):
    """The crew's pictures are the owner's and live outside git, in this station's cache: per frog a poster
    (<key>.jpg, a Krea2 still) and two loops (<key>.busy.mp4 at work, <key>.doze.mp4 idle; Minimax H3). They are
    copied into the site's img/crew/ when the station has them, and data/crew.js lists what is there. Without
    them crew.js draws each worker as the mark of its trade."""
    media = {}
    src = CREW / 'crew'
    for f in sorted(src.glob('*.jpg')) if src.is_dir() else []:
        key = f.stem
        media[key] = {m: (src / f'{key}.{m}.mp4').exists() for m in ('busy', 'doze')}
        for g in [f] + [src / f'{key}.{m}.mp4' for m in ('busy', 'doze') if media[key][m]]:
            dst = out / 'img' / 'crew' / g.name
            if not dst.exists() or dst.stat().st_size != g.stat().st_size or dst.read_bytes() != g.read_bytes():
                dst.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(g, dst)
    write_if_changed(out / 'data' / 'crew.js', f'window.CREW_MEDIA = {json.dumps(media, sort_keys=True)};\n')
    return media


def once(out: Path):
    data = src_ops.collect(build.REPO, out)
    # the stamp changes every time; compare without it so an unchanged floor is not uploaded again
    body = json.dumps({k: v for k, v in data.items() if k != 'now'}, sort_keys=True, default=str)
    old = out / 'data' / 'ops.js'
    if old.exists() and body in old.read_text(encoding='utf-8'):
        return data, False
    return data, write_if_changed(old, f'window.OPS_NOW = {json.dumps(data["now"])};\nwindow.OPS = {body};\n')


def main(argv=None):
    ap = argparse.ArgumentParser(description='the floor page of the asset board')
    ap.add_argument('--watch', type=float, default=0, help='seconds between reads; 0 reads once')
    ap.add_argument('--out', default='')
    args = ap.parse_args(argv)
    out = build.out_dir(args.out)
    import src_git
    commit, branch, as_of = src_git.head(build.REPO)
    page(out, dict(built=time.strftime('%Y-%m-%d %H:%M'), station=__import__('socket').gethostname(), commit=commit, refs_as_of=as_of))
    while True:
        data, changed = once(out)
        c = data['counts']
        print(f'{data["now"]}  {c["sessions"]} sessions and {c["machines"]} machines at work, {c["ready"]} stages ready, '
              f'{c["idle"]} of {len(data["roster"])} skills and agents idle{"" if changed else " (no change)"}', flush=True)
        if not args.watch:
            return 0
        time.sleep(args.watch)


if __name__ == '__main__':
    sys.exit(main())
