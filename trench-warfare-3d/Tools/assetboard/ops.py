#!/usr/bin/env python3
"""The floor: every branch with its items and what is happening in it, and the skills and agents, on the bench or at
work on a branch. Two pages of the asset board that re-read their data every 20 seconds: floor.html, a room per
branch, and house.html, a room per kind of work, where every worker is a frog that walks to the room of what it is
doing (static/house.js; which room is src_acts.py's to say).

    python Tools/assetboard/ops.py                 write the page and its data once
    python Tools/assetboard/ops.py --watch 20      and again every 20 seconds until stopped (keeps the page live)
    python Tools/assetboard/ops.py --queue         read once and print what waits on the owner (the page's "Needs you")

The data is data/ops.js beside the page (a script, not JSON: a page opened from the Drive as a file may not fetch
JSON, it may load a script), and data/queue.js, the owner queue (src_queue.py). Each is written only when it changed.
data/beat.js is a few bytes written on EVERY read: the time of the read, so the page can say how old it is and turn
red when nobody has read for an hour. Without it an unchanged floor and a stopped watcher look the same.
data/crew.js and data/frog.js list the owner's pictures this station has: a portrait per worker (crew) and the
sheets of the walking frog (frog, packed by sprites.py). Both are written when the pages are, not on every read.
data/graphs.js is what graphs.html draws (src_graphs.py) and data/notes.js the owner's notes and their answers
(notes.py); both are made again on every read. The same read gives each session and agent its `visual`, the last
picture or film it had in its hands, copied small into img/last/ (src_visuals.py), and puts the open decision briefs
(briefs.py) in data/briefs.js with their evidence under img/brief/, for decide.html. While it watches, this also takes the notes the owner writes on the
pages (a listener on this machine only, notes.py) and data/notebox.js tells the pages where it is.
What it reads: src_ops.py, src_queue.py.
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
import briefs     # noqa: E402
import build      # noqa: E402
import notes      # noqa: E402
import src_graphs  # noqa: E402
import src_ops    # noqa: E402
import src_queue  # noqa: E402
import src_visuals  # noqa: E402


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
    for name in ('floor.html', 'house.html', 'graphs.html', 'decide.html'):
        write_if_changed(out / name, env.get_template(name).render(root=''))
    for name in ('floor.js', 'crew.js', 'queue.js', 'office.js', 'house.js', 'housedraw.js', 'house.css', 'board.js', 'board.css', 'charts.js', 'control.js', 'control.css', 'decide.js', 'decide.css', 'site.css', 'kinetic.css'):
        write_if_changed(out / name, (HERE / 'static' / name).read_text(encoding='utf-8'))
    crew(out)
    frog(out)
    # where the pages send a note, and the key they must show (a file beside the page: a page from elsewhere cannot read it)
    write_if_changed(out / 'data' / 'notebox.js', f'window.NOTEBOX = {json.dumps(dict(url=f"http://127.0.0.1:{notes.PORT}", key=notes.key_of(notes.folder())))};\n')


CREW = Path(os.environ.get('LOCALAPPDATA', str(Path.home()))) / 'TrenchWarfare' / 'assetboard'


def crew(out: Path):
    """The crew's pictures are the owner's and live outside git, in this station's cache: per frog a poster
    (<key>.jpg, a Krea2 still) and two loops (<key>.busy.mp4 at work, <key>.doze.mp4 idle; Minimax H3). They are
    copied into the site's img/crew/ when the station has them, and data/crew.js lists what is there. Without
    them crew.js draws each worker as the mark of its trade."""
    media = {}
    src = CREW / 'crew'
    for f in sorted(src.glob('*.jpg')) if src.is_dir() else []:
        if f.stem.endswith('.sleep'):
            continue
        key = f.stem
        media[key] = {m: (src / f'{key}.{m}.mp4').exists() for m in ('busy', 'doze')}
        media[key]['sleep'] = (src / f'{key}.sleep.jpg').exists()
        media[key]['sleepv'] = (src / f'{key}.sleep.mp4').exists()
        for g in [f] + [src / f'{key}.{m}.mp4' for m in ('busy', 'doze') if media[key][m]] + ([src / f'{key}.sleep.jpg'] if media[key]['sleep'] else []) + ([src / f'{key}.sleep.mp4'] if media[key]['sleepv'] else []):
            dst = out / 'img' / 'crew' / g.name
            if not dst.exists() or dst.stat().st_size != g.stat().st_size or dst.read_bytes() != g.read_bytes():
                dst.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(g, dst)
    write_if_changed(out / 'data' / 'crew.js', f'window.CREW_MEDIA = {json.dumps(media, sort_keys=True)};\n')
    return media


def frog(out: Path):
    """The frog that walks through the house is the owner's art too: sprites.py packs it into sheets in this
    station's cache (frog/, beside crew/), with frog.json saying what is on each. The sheets it names are copied
    into the site's img/frog/ and data/frog.js is that manifest. A station that never packed them, or whose pack
    lost a sheet, gets an empty one, and housedraw.js draws each worker as the mark of its kind."""
    src = CREW / 'frog'
    try:
        man = json.loads((src / 'frog.json').read_text(encoding='utf-8'))
        names = [f'{key}{tag}.png' for key in list(man.get('anims', {})) + list(man.get('stills', {})) for tag in ('', '@0.5')]
    except (OSError, ValueError, AttributeError):
        man, names = {}, []
    if not all((src / n).exists() for n in names):
        man, names = {}, []
    for n in names:
        dst = out / 'img' / 'frog' / n
        if not dst.exists() or dst.stat().st_size != (src / n).stat().st_size or dst.read_bytes() != (src / n).read_bytes():
            dst.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(src / n, dst)
    write_if_changed(out / 'data' / 'frog.js', f'window.FROG = {json.dumps(man, sort_keys=True)};\n')
    return man


def ready_since(data, store_path: Path):
    """Stamp each ready stage with the first reading that saw it ready; a stage no longer ready is forgotten."""
    try:
        store = json.loads(store_path.read_text(encoding='utf-8'))
    except (OSError, ValueError):
        store = {}
    seen = {}
    for lane in data['lanes']:
        for it in lane['items']:
            for st in it['stages']:
                if st.get('state') == 'READY':
                    key = f"{lane['branch']}|{it.get('id')}|{st.get('id')}"
                    st['since'] = seen[key] = store.get(key) or data['now']
    if seen != store:
        write_if_changed(store_path, json.dumps(seen, sort_keys=True, indent=0))
    return seen


def board_root():
    sys.path.insert(0, str(build.ROOT / 'Tools' / 'pipeline'))
    try:
        import pipeline
        return pipeline.board_dir()
    except (SystemExit, Exception):
        return None


def queue(data, out: Path, cache_path: Path = None, repo: Path = None, board=None):
    """The owner queue for this reading, and the beat. What a read need not ask again is kept in this station's cache."""
    cache_path = cache_path or build.LOCAL / 'queue-cache.json'
    try:
        cache = json.loads(cache_path.read_text(encoding='utf-8'))
    except (OSError, ValueError):
        cache = {}
    q = src_queue.collect(repo or build.REPO, data, board=board or board_root(), cache=cache)
    write_if_changed(cache_path, json.dumps(cache, sort_keys=True))
    write_if_changed(out / 'data' / 'queue.js', f'window.QUEUE = {json.dumps(q, sort_keys=True)};\n')
    beat = out / 'data' / 'beat.js'
    beat.parent.mkdir(parents=True, exist_ok=True)
    tmp = beat.with_suffix('.js.tmp')
    tmp.write_text(f'window.BEAT = {json.dumps(data["now"].replace(" ", "T"))};\n', encoding='utf-8')
    tmp.replace(beat)
    return q


def graphs(data, out: Path, now=None):
    """What graphs.html draws, for this reading (src_graphs.py). The transcripts are read through a cache and the
    floor's history is kept in this station's cache folder, so neither is in the site. The same pass over the
    transcripts says which picture or film each worker last had in its hands; that goes on the workers in `data`."""
    trees = {Path(l['path']): dict(branch=l['branch']) for l in data['lanes'] if l.get('path')}
    now, seen = now or time.time(), {}
    g = src_graphs.collect(build.REPO, out, data, trees, now, build.LOCAL / 'graphs-cache.json', build.LOCAL / 'history.jsonl', seen=seen)
    src_visuals.attach(data['lanes'], seen, out, now)
    write_if_changed(out / 'data' / 'graphs.js', f'window.GRAPHS = {json.dumps(g, sort_keys=True)};\n')
    return g


def owner_notes(out: Path):
    """The owner's notes the pages show: the open ones and the lately answered (notes.py)."""
    shown = notes.shown(notes.read_all(notes.folder()))
    write_if_changed(out / 'data' / 'notes.js', f'window.NOTES = {json.dumps(shown, sort_keys=True)};\n')
    return shown


def once(out: Path):
    data = src_ops.collect(build.REPO, out)
    ready_since(data, out / 'data' / 'ready-since.json')
    data['queue'] = queue(data, out)
    graphs(data, out)
    data['notes'] = sum(1 for n in owner_notes(out) if n['state'] != 'done')
    briefs.site(briefs.folder(), out)          # the decisions that wait on the owner, each as a brief with what it shows (decide.html)
    # the stamp changes every time; compare without it so an unchanged floor is not uploaded again
    body = json.dumps({k: v for k, v in data.items() if k not in ('now', 'queue', 'notes')}, sort_keys=True, default=str)
    old = out / 'data' / 'ops.js'
    if old.exists() and body in old.read_text(encoding='utf-8'):
        return data, False
    return data, write_if_changed(old, f'window.OPS_NOW = {json.dumps(data["now"])};\nwindow.OPS = {body};\n')


def main(argv=None):
    ap = argparse.ArgumentParser(description='the floor page of the asset board')
    ap.add_argument('--watch', type=float, default=0, help='seconds between reads; 0 reads once')
    ap.add_argument('--out', default='')
    ap.add_argument('--queue', action='store_true', help='read once and print what waits on the owner')
    args = ap.parse_args(argv)
    out = build.out_dir(args.out)
    if args.queue:
        print('\n'.join(src_queue.lines(once(out)[0]['queue'])))
        return 0
    import src_git
    commit, branch, as_of = src_git.head(build.REPO)
    page(out, dict(built=time.strftime('%Y-%m-%d %H:%M'), station=__import__('socket').gethostname(), commit=commit, refs_as_of=as_of))
    if args.watch:
        box, _ = notes.serve()
        print(f'notes: {"taking the pages notes on 127.0.0.1:" + str(notes.PORT) if box else "port " + str(notes.PORT) + " is taken, most likely by another watcher"}, into {notes.folder()}', flush=True)
    while True:
        data, changed = once(out)
        c = data['counts']
        print(f'{data["now"]}  {data["queue"]["count"]} wait on the owner, {data["notes"]} notes open, {c["sessions"]} sessions and {c["machines"]} machines at work, {c["ready"]} stages ready, '
              f'{c["idle"]} of {len(data["roster"])} skills and agents idle{"" if changed else " (no change)"}', flush=True)
        if not args.watch:
            return 0
        time.sleep(args.watch)


if __name__ == '__main__':
    sys.exit(main())
