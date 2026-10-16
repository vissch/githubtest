#!/usr/bin/env python3
"""The concept page's data: the pictures ComfyUI made for the project, which of them the owner approved, and the
things he asked the board to make.

    python Tools/assetboard/concepts.py                 what the page would show: sessions and counts, approved, requests
    python Tools/assetboard/concepts.py request "text" [--kind vehicle|character|item|other] [--takes 4] [--frog]
    python Tools/assetboard/concepts.py tick            take the open requests one step (ops.py --watch does this itself)

WHY. Hundreds of concept pictures sat in folders on the Drive (the vehicle and mech sets of 2026-10-10/11) and there
was no place to see them by the run that made them, to say "this one", or to ask for more. The page (concepts.html)
shows them, lets the owner approve, and has a box that makes new ones. Approving changes nothing else: the approved
section is an overview, no work follows from it (the owner, 2026-10-15).

A SESSION is one generation run: a set of pictures made together (the first vehicle set, the frog set, mech 60,
mech JP) and "prompted", every picture the page's own box made. The pictures are found on disk (that is the truth of
what exists) and joined to their prompt and seed (the run's manifest) and their idea name (the run's ideas index).
Those two files live in the exploration project outside git (TW_CONCEPT_PROJECT); a picture without them is still shown,
with the name its file gives it.

WHERE THINGS ARE. The pictures: TW_CONCEPT_IMAGES, else the desktop's Drive-synced output folder as the laptop sees it
(G:/Other computers/My Computer/drive). The owner's word: folder `concepts` of the Drive's TW3D-pipeline beside the notes
(TW_CONCEPTS names another): approved.json, and requests/<id>.json, one file per thing he asked for. Thumbnails: the
site's img/concept/. The page cannot write a file, so it sends an approval or a request through the notes listener
(notes.py, /concept/...), which holds the same key.

A REQUEST is made on the desktop: the watcher builds the manifest, finds the broker alive, copies the manifest over and
starts the desktop's gen.py (the Clash LoRA graph of the exploration) through ssh, detached, then counts the pictures
that land in the Drive. States: queued (waiting for the desktop, with the reason), rendering, done, failed (with why),
cancelled. Nothing is retried by itself after a failure: the owner presses Retry.
"""
import argparse
import datetime
import hashlib
import json
import os
import random
import re
import subprocess
import sys
import threading
import time
import urllib.parse
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import build  # noqa: E402

IMAGES = Path(os.environ.get('TW_CONCEPT_IMAGES') or 'G:/Other computers/My Computer/drive')
PROJECT = Path(os.environ.get('TW_CONCEPT_PROJECT') or Path.home() / 'Documents' / 'claude' / 'twf3d-vehicle-exploration')
PROMPTED = 'twf3d_concepts'                      # the folder of IMAGES the page's own requests render into
DESKTOP = 'emtd-desktop'                         # the ssh host of the GPU machine (the owner's ~/.ssh/config)
DESKTOP_DIR = 'C:/Users/PC/Documents/claude/twf3d-vehicle-exploration'      # gen.py and launch.ps1 are there
THUMB = 360                                      # the longest side of a thumbnail, in pixels
THUMB_SECONDS = 25                               # how long a read spends on new thumbnails at most: the first read of 1,600 takes a few of them
RESCAN_SECONDS = 60                              # how often the folders are listed again
RENDER_LIMIT = 60 * 60                           # a request still not done after this long has failed
LOOK_AFTER = 10 * 60                             # and after this long without all its pictures the desktop is asked how it went
IMG = re.compile(r'\.(png|jpe?g|webp)$', re.I)
NAME = re.compile(r'^(?P<n>\d+)-(?P<i>\d+)_(?P<slug>.+?)_s(?P<s>\d+)_(?P<k>\d+)_?$')
PROMPTED_NAME = re.compile(r'^(?P<slug>.+?)_s(?P<seed>\d+)_(?P<k>\d+)_?$')
LOCK = threading.Lock()

# (id, label, folder under IMAGES, manifests, ideas index, about): a session each. The last, "prompted", is not listed here.
SESSIONS = [
    ('first', '1 - First set (plain Clash)', 'twf3d_vehicle_exploration', ['manifest.json', 'manifest_a.json', 'manifest_b.json'], 'ideas_index.json',
     '2026-10-10: nine vehicle categories, Clash LoRA, not frog themed. Paused at about 140 of 960.'),
    ('frog', '2 - Frog set', 'twf3d_frog_vehicle_exploration', ['manifest_frog.json'], 'ideas_index_frog.json',
     '2026-10-11: the same nine categories, frog + dieselpunk/WW1, entirely new ideas.'),
    ('mech60', '3 - Mech 60', 'twf3d_mech60_exploration', ['manifest_mech60.json'], 'ideas_index_mech60.json',
     '2026-10-11: 60 mech types in six slices, dieselpunk and frog themed.'),
    ('mechjp', '4 - Mech JP', 'twf3d_mechjp_exploration', ['manifest_mechjp.json'], 'ideas_index_mechjp.json',
     '2026-10-11: 20 Japanese-style mecha types fused with frog + dieselpunk.'),
]
PROMPTED_SESSION = ('prompted', '5 - Prompted on the board', 'Made from the page\'s own box: what you described, with the Clash LoRA.')

# the words a request's prompt is made of: the LoRA's trigger, the description, then a closing for the kind of thing
TRIGGER = '@clashroyale, 3d render, '
KINDS = {
    'vehicle': ('Vehicle', ', three-quarter view, game unit concept, soft studio lighting, plain light background'),
    'character': ('Character', ', full body, standing pose, three-quarter view, game character concept, soft studio lighting, plain light background'),
    'item': ('Item', ', a single object centred, game item concept, soft studio lighting, plain light background'),
    'other': ('Other', ', game concept art, soft studio lighting, plain light background'),
}
THEMES = {'': '', 'frog': ', frog-themed, dieselpunk World War One style, riveted brass and khaki plate'}
LONGEST_TEXT = 1500
MOST_TAKES = 8


def folder():
    if os.environ.get('TW_CONCEPTS'):
        return Path(os.environ['TW_CONCEPTS'])
    return build.DRIVE / 'concepts' if build.DRIVE.is_dir() else build.LOCAL / 'concepts'


def now_text(now=None):
    return f'{now or datetime.datetime.now():%Y-%m-%d %H:%M:%S}'


def line(s, n=200):
    return re.sub(r'\s+', ' ', str(s or '')).strip()[:n]


def atomic(path: Path, text: str):
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(f'{path.name}.{threading.get_ident()}.tmp')
    tmp.write_text(text, encoding='utf-8')
    tmp.replace(path)


def load_json(path: Path, default):
    try:
        return json.loads(Path(path).read_text(encoding='utf-8'))
    except (OSError, ValueError):
        return default


def slug_of(text, n=40):
    return re.sub(r'[^a-z0-9]+', '-', str(text).lower()).strip('-')[:n] or 'concept'


def pretty(s):
    return re.sub(r'^\d+_', '', s).replace('-', ' ').replace('_', ' ')


# ---- what the owner approved ----------------------------------------------------------------------------------------

def safe_id(i):
    """An approval names a picture by its path under the images folder: nothing that leaves it, nothing that is not a picture."""
    i = str(i or '').replace('\\', '/')
    if not i or len(i) > 400 or i.startswith('/') or ':' in i or '..' in i.split('/') or not IMG.search(i):
        raise ValueError('that is not a picture of the concepts')
    return i


def approved(where: Path = None):
    d = load_json((where or folder()) / 'approved.json', {})
    return d if isinstance(d, dict) else {}


def approve(ids, on, where: Path = None, now=None):
    """Approve (or take the approval back from) one picture or several. Returns what is approved now."""
    where = where or folder()
    ids = [safe_id(i) for i in (ids if isinstance(ids, list) else [ids])]
    with LOCK:
        d = approved(where)
        for i in ids:
            if on:
                d.setdefault(i, now_text(now))
            else:
                d.pop(i, None)
        atomic(where / 'approved.json', json.dumps(d, indent=1, sort_keys=True))
    return d


# ---- what he asked for ----------------------------------------------------------------------------------------------

def build_prompt(kind, text, theme=''):
    """The prompt a description becomes: the LoRA's trigger, his words, the theme if asked, the closing for the kind."""
    body = re.sub(r'\s+', ' ', str(text or '')).strip().rstrip('.,;')
    body = re.sub(r'^@clashroyale,?\s*(3d render,?\s*)?', '', body, flags=re.I)
    return TRIGGER + body + THEMES.get(theme or '', '') + KINDS[kind][1]


def requests_dir(where: Path = None):
    return (where or folder()) / 'requests'


def read_requests(where: Path = None):
    out = []
    for f in sorted(requests_dir(where).glob('*.json')) if requests_dir(where).is_dir() else []:
        r = load_json(f, None)
        if isinstance(r, dict) and r.get('id') == f.stem:
            out.append(r)
    return sorted(out, key=lambda r: (r.get('when', ''), r['id']))


def save_request(r, where: Path = None):
    atomic(requests_dir(where) / f'{r["id"]}.json', json.dumps(r, indent=1, ensure_ascii=False))


def request(text, kind='vehicle', title='', takes=4, theme='', prompt='', lora=1.0, where: Path = None, now=None, rng=None):
    """Write a request and return it: queued, with its seeds and the prefix its pictures are saved under."""
    where = where or folder()
    text = str(text or '').strip()
    if len(text) < 8:
        raise ValueError('describe what you want in a few words at least (8 characters)')
    if len(text) > LONGEST_TEXT:
        raise ValueError(f'a description is at most {LONGEST_TEXT} characters')
    if kind not in KINDS:
        raise ValueError(f'the kind is one of {", ".join(KINDS)}')
    if theme not in THEMES:
        raise ValueError('that theme is not one of the page\'s')
    try:
        takes, lora = int(takes), float(lora)
    except (TypeError, ValueError):
        raise ValueError('takes is a whole number and the LoRA strength a number')
    if not 1 <= takes <= MOST_TAKES:
        raise ValueError(f'takes: 1 to {MOST_TAKES}')
    if not 0 <= lora <= 1.5:
        raise ValueError('the LoRA strength is between 0 (off) and 1.5')
    now = now or datetime.datetime.now()
    prompt = str(prompt or '').strip() or build_prompt(kind, text, theme)
    if len(prompt) > 3 * LONGEST_TEXT:
        raise ValueError('the full prompt is too long')
    title = line(title, 60) or line(text, 48)
    rng = rng or random.SystemRandom()
    base = f'c{now:%y%m%d-%H%M%S}'
    rid, n = base, 0
    while (requests_dir(where) / f'{rid}.json').exists():
        n += 1
        rid = f'{base}-{n}'
    seeds = [rng.randrange(100000, 2 ** 31 - 1) for _ in range(takes)]
    r = dict(id=rid, when=now_text(now), kind=kind, title=title, text=text, theme=theme, prompt=prompt, takes=takes, lora=lora, seeds=seeds,
             prefix=f'{PROMPTED}/{kind}/{rid}/{slug_of(title)}', state='queued', why='waiting for the desktop', log=[])
    save_request(r, where)
    return r


def set_state(r, state, why='', where: Path = None):
    r['state'] = state
    r['why'] = line(why, 300)
    r.setdefault('log', []).append(f'{now_text()} {state}' + (f': {line(why, 120)}' if why else ''))
    r['log'] = r['log'][-12:]
    save_request(r, where)
    return r


def change(rid, action, where: Path = None):
    """What the page may do to a request: cancel one that has not started, or retry one that failed or was cancelled."""
    where = where or folder()
    r = next((x for x in read_requests(where) if x['id'] == str(rid)), None)
    if not r:
        raise ValueError(f'{rid}: no such request')
    if action == 'cancel' and r['state'] == 'queued':
        return set_state(r, 'cancelled', 'cancelled by the owner', where)
    if action == 'retry' and r['state'] in ('failed', 'cancelled'):
        r.pop('launched', None)
        r.pop('checked', None)
        return set_state(r, 'queued', 'waiting for the desktop', where)
    raise ValueError(f'{rid} is {r["state"]}: it cannot be {"cancelled" if action == "cancel" else "retried"}')


ROUTES = ('/concept/approve', '/concept/request', '/concept/cancel', '/concept/retry')      # what the listener passes on (notes.py)


def post(path, d, where: Path = None):
    """The listener's /concept/... routes: what the page sends. Raises ValueError for what it refuses. What changed is in
    the site's state file before this returns."""
    res = route(path, d, where)
    if SITE['out']:
        state_file(SITE['out'], where)
    return res


def route(path, d, where: Path = None):
    if path == '/concept/approve':
        return dict(approved=approve(d.get('ids') or d.get('id'), bool(d.get('approved')), where))
    if path == '/concept/request':
        return dict(request=request(d.get('text'), kind=d.get('kind') or 'vehicle', title=d.get('title') or '', takes=d.get('takes') or 4,
                                    theme=d.get('theme') or '', prompt=d.get('prompt') or '', lora=d.get('lora') if d.get('lora') is not None else 1.0, where=where))
    if path in ('/concept/cancel', '/concept/retry'):
        return dict(request=change(d.get('id'), path.rsplit('/', 1)[1], where))
    raise ValueError('nothing here')


# ---- the pictures ---------------------------------------------------------------------------------------------------

def sessions_table():
    return [dict(id=s[0], label=s[1], about=s[5]) for s in SESSIONS] + [dict(id=PROMPTED_SESSION[0], label=PROMPTED_SESSION[1], about=PROMPTED_SESSION[2])]


def listing(root: Path):
    """[(category, type folder, file name)] of a session's folder, in order. A folder that is not there lists nothing."""
    out = []
    if not root.is_dir():
        return out
    for cat in sorted(os.listdir(root)):
        cdir = root / cat
        if cat.startswith('_') or not cdir.is_dir():
            continue
        for typ in sorted(os.listdir(cdir)):
            tdir = cdir / typ
            if not tdir.is_dir():
                continue
            out += [(cat, typ, f) for f in sorted(os.listdir(tdir)) if IMG.search(f)]
    return out


def scan(images: Path = None, project: Path = None, requests=None):
    """Every concept picture on disk with what is known of it: [item], items in the order of the sessions. An item:
    id (path under the images folder), s (session), c/cl (category and its words), t (type), n (idea name), i (idea
    number), k (take), seed, p (prompt)."""
    images, project = Path(images or IMAGES), Path(project or PROJECT)

    def load(name):
        return load_json(project / name, [])
    items = []
    for sid, _label, folder_name, manifests, index, _about in SESSIONS:
        by_prefix = {}
        for m in manifests:
            for j in load(m):
                by_prefix[str(j.get('prefix', '')).replace('\\', '/')] = j
        ideas = {(i['category'], i['n'], i['idea']): i for i in load(index)}
        for cat, typ, f in listing(images / folder_name):
            stem = os.path.splitext(f)[0]
            m = NAME.match(stem)
            if not m:
                continue
            n, i = int(m['n']), int(m['i'])
            job = by_prefix.get(f'{folder_name}/{cat}/{typ}/{stem.rsplit("_s", 1)[0]}_s{m["s"]}', {})
            info = ideas.get((cat, n, i), {})
            items.append(dict(id=f'{folder_name}/{cat}/{typ}/{f}', s=sid, c=cat, cl=pretty(cat), t=info.get('type') or pretty(typ), n=info.get('name') or pretty(m['slug']),
                              i=i, k=int(m['s']), seed=job.get('seed'), p=job.get('prompt') or info.get('subject') or ''))
    asked = {r['id']: r for r in (requests or [])}
    for cat, typ, f in listing(images / PROMPTED):          # <kind>/<request id>/<slug>_s<seed>_00001_.png
        m = PROMPTED_NAME.match(os.path.splitext(f)[0])
        r = asked.get(typ, {})
        kind = cat if cat in KINDS else 'other'
        seed = int(m['seed']) if m else None
        seeds = r.get('seeds') or []
        items.append(dict(id=f'{PROMPTED}/{cat}/{typ}/{f}', s=PROMPTED_SESSION[0], c=kind, cl=KINDS[kind][0], t=r.get('text') or pretty(typ), n=r.get('title') or pretty(m['slug'] if m else f),
                          i=0, k=seeds.index(seed) + 1 if seed in seeds else 1, seed=seed, p=r.get('prompt') or ''))
    return items


def thumb_name(item_id):
    return hashlib.sha1(item_id.encode('utf-8')).hexdigest()[:14] + '.jpg'


def make_thumb(src: Path, dst: Path):
    from PIL import Image
    im = Image.open(src).convert('RGB')
    im.thumbnail((THUMB, THUMB))
    dst.parent.mkdir(parents=True, exist_ok=True)
    tmp = dst.with_name(f'{dst.name}.{threading.get_ident()}.tmp')
    im.save(tmp, 'JPEG', quality=78)
    tmp.replace(dst)


def thumbs(items, out: Path, images: Path = None, seconds=THUMB_SECONDS):
    """Make the thumbnails the site does not have yet, for `seconds` at most a read. Returns how many are still missing."""
    images, missing, stop = Path(images or IMAGES), 0, time.time() + seconds
    for it in items:
        dst = out / 'img' / 'concept' / thumb_name(it['id'])
        if dst.exists():
            continue
        if time.time() > stop:
            missing += 1
            continue
        try:
            make_thumb(images / it['id'], dst)
        except Exception:     # noqa: BLE001  a picture still being written, or one that does not open: next read
            missing += 1
    return missing


# ---- the page's data ------------------------------------------------------------------------------------------------

SCAN = dict(at=0.0, items=[], key='')


def current_items(requests, force=False, now=None):
    """The scan, listed again at most every RESCAN_SECONDS (the folders are on the Drive and are not cheap to list)."""
    now = time.time() if now is None else now
    if force or now - SCAN['at'] >= RESCAN_SECONDS or not SCAN['items']:
        SCAN.update(at=now, items=scan(requests=requests))
    return SCAN['items']


SITE = dict(out=None, items='', missing=0, by_id={})     # where the watcher put the site, the stamp of its pictures and how many thumbnails it still owes


def state_file(out: Path, where: Path = None):
    """data/concepts-state.js: what the page reads again every few seconds. Written on every read and again at once when
    the page changes something, so a page does not take a state older than its own click."""
    where = where or folder()
    reqs = read_requests(where)
    ap = approved(where)
    by = SITE.get('by_id') or {}
    recent = [dict(id=i, n=by[i]['n'], s=by[i]['s'], th=thumb_name(i)) for i, _when in sorted(ap.items(), key=lambda kv: kv[1], reverse=True) if i in by][:8]
    top = dict(total=len(by), approved=len(ap), recent=recent)      # what the overview shows of the page: no pictures' list, a few approved ones
    state = dict(top=top, approved=ap, requests=[{k: r.get(k) for k in ('id', 'when', 'kind', 'title', 'text', 'prompt', 'takes', 'state', 'why', 'theme', 'lora', 'got')} for r in reqs[-30:]],
                 items=SITE['items'], thumbs_missing=SITE['missing'],
                 builder=dict(trigger=TRIGGER, order=list(KINDS), kinds={k: v[1] for k, v in KINDS.items()}, labels={k: v[0] for k, v in KINDS.items()}, themes=THEMES, most=MOST_TAKES))
    write(out / 'data' / 'concepts-state.js', f'window.CONCEPT_STATE = {json.dumps(state, ensure_ascii=False, sort_keys=True)};\n')


def site(out: Path, where: Path = None, force=False, now=None):
    """Put data/concepts.js (the pictures: one script, written when they change) and data/concepts-state.js (what the
    page reads again every few seconds: approved, requests, the stamp of the pictures) in the site. Returns the item count."""
    where = where or folder()
    reqs = read_requests(where)
    items = current_items(reqs, force=force, now=now)
    missing = thumbs(items, out)
    prompts, index = [], {}
    rows = []
    for it in items:
        p = it['p']
        if p not in index:
            index[p] = len(prompts)
            prompts.append(p)
        rows.append([it['id'], it['s'], it['c'], it['cl'], it['t'], it['n'], it['i'], it['k'], it['seed'], index[p], thumb_name(it['id'])])
    counts = {s['id']: sum(1 for it in items if it['s'] == s['id']) for s in sessions_table()}
    body = json.dumps(dict(sessions=[dict(s, count=counts[s['id']]) for s in sessions_table()], prompts=prompts, rows=rows,
                           fields='id s c cl t n i k seed p th'.split(), root=urllib.parse.quote(str(IMAGES).replace('\\', '/'), safe='/:')), ensure_ascii=False, separators=(',', ':'))
    stamp = hashlib.sha1(body.encode('utf-8')).hexdigest()[:10]
    write(out / 'data' / 'concepts.js', f'window.CONCEPTS = {body};\n')
    SITE.update(out=out, items=stamp, missing=missing, by_id={it['id']: it for it in items})
    state_file(out, where)
    return len(items)


def write(path: Path, text: str):
    if path.exists() and path.read_text(encoding='utf-8') == text:
        return False
    atomic(path, text)
    return True


# ---- making them: the desktop ---------------------------------------------------------------------------------------

def ssh(cmd, timeout=45):
    """One command on the desktop: (exit code, output). The remote shell is Windows PowerShell 5.1."""
    try:
        p = subprocess.run(['ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=8', DESKTOP, cmd], capture_output=True, text=True, timeout=timeout)
        return p.returncode, (p.stdout + p.stderr).strip()
    except (OSError, subprocess.SubprocessError) as e:
        return 255, f'{type(e).__name__}: {e}'


def scp(local: Path, remote_name: str, timeout=45):
    try:
        p = subprocess.run(['scp', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=8', str(local), f'{DESKTOP}:{DESKTOP_DIR}/{remote_name}'], capture_output=True, text=True, timeout=timeout)
        return p.returncode, (p.stdout + p.stderr).strip()
    except (OSError, subprocess.SubprocessError) as e:
        return 255, f'{type(e).__name__}: {e}'


def manifest_of(r):
    """gen.py's manifest of a request: a job a take. The prefix is relative to ComfyUI's output folder, which is the Drive."""
    return [dict(id=f'{r["id"]}_{n + 1}', prompt=r['prompt'], seed=seed, lora=r.get('lora', 1.0), prefix=f'{r["prefix"]}_s{seed}') for n, seed in enumerate(r['seeds'])]


def got(r, images: Path = None):
    """How many of a request's pictures are in the Drive now."""
    d = Path(images or IMAGES) / PROMPTED / r['kind'] / r['id']
    return sum(1 for f in d.iterdir() if IMG.search(f.name)) if d.is_dir() else 0


def wait(r, why, where: Path = None):
    """Stay queued for a reason. The log gets a line when the reason changes, not on every look."""
    if r.get('why') != line(why, 300):
        return set_state(r, 'queued', why, where)
    save_request(r, where)
    return r


def step(r, where: Path = None, run_ssh=ssh, run_scp=scp, images: Path = None, now=None, tmp: Path = None):
    """Take one request one step. queued: the broker answers? then send the manifest and start gen.py. rendering: are
    the pictures there? A request in any other state is left alone. The desktop is only reached by `run_ssh` and
    `run_scp`, so a test hands in its own."""
    where = where or folder()
    now = time.time() if now is None else now
    if r['state'] == 'queued':
        if now - r.get('checked', 0) < 45:
            return r
        r['checked'] = now
        code, said = run_ssh("curl.exe -s -m 5 -o NUL -w '%{http_code}' http://127.0.0.1:8188/system_stats")
        if not said.strip().endswith('200'):          # ssh itself failing is 255; any other code is curl's, through the remote shell
            return wait(r, f'the desktop is not reachable ({line(said, 80)}); waiting' if code == 255 else f'ComfyUI on the desktop is not answering ({line(said, 60) or "no answer"}); waiting', where)
        tmp = tmp or build.LOCAL / 'concepts'
        tmp.mkdir(parents=True, exist_ok=True)
        local, name = tmp / f'manifest_{r["id"]}.json', f'manifest_{r["id"]}.json'
        local.write_text(json.dumps(manifest_of(r), indent=1), encoding='utf-8')
        code, said = run_scp(local, name)
        if code != 0:
            return wait(r, f'the manifest did not reach the desktop ({line(said, 100)}); waiting', where)
        code, said = run_ssh(f'powershell -NoProfile -ExecutionPolicy Bypass -File {DESKTOP_DIR}/launch.ps1 -Manifest {name} -Tag {r["id"]}')
        if code != 0 or 'rc 0' not in said:
            return set_state(r, 'failed', f'gen.py did not start ({line(said, 120)})', where)
        r['launched'] = now
        return set_state(r, 'rendering', f'{r["takes"]} pictures on the desktop', where)
    if r['state'] == 'rendering':
        n = got(r, images)
        r['got'] = n
        if n >= r['takes']:
            return set_state(r, 'done', f'{n} pictures', where)
        waited = now - r.get('launched', now)
        if waited > RENDER_LIMIT:
            return set_state(r, 'failed', f'{n} of {r["takes"]} pictures after {int(waited // 60)} minutes', where)
        if waited > LOOK_AFTER and now - r.get('checked', 0) > 120:
            r['checked'] = now
            code, said = run_ssh(f'Get-Content {DESKTOP_DIR}/render_{r["id"]}.out -Tail 3')
            if code == 0 and 'finished' in said:
                return set_state(r, 'failed', f'the render ended with {n} of {r["takes"]} pictures: {line(said, 160)}', where)
        save_request(r, where)
    return r


WORKER = dict(thread=None)


def tick(where: Path = None, **kw):
    """Take every open request one step, on a thread of its own (an ssh call may take half a minute and a read of the
    board must not wait for it). One pass at a time: a read that finds the last still going does nothing."""
    t = WORKER['thread']
    if t and t.is_alive():
        return False
    where = where or folder()
    if not any(r['state'] in ('queued', 'rendering') for r in read_requests(where)):
        return False

    def work():
        for r in read_requests(where):
            if r['state'] in ('queued', 'rendering'):
                try:
                    step(r, where, **kw)
                except Exception as e:      # noqa: BLE001  one request's trouble must not stop the others
                    print(f'concepts: {r["id"]}: {type(e).__name__}: {e}', flush=True)
    WORKER['thread'] = threading.Thread(target=work, daemon=True)
    WORKER['thread'].start()
    return True


def main(argv=None):
    ap = argparse.ArgumentParser(description='the concept page\'s data')
    ap.add_argument('verb', nargs='?', choices=('list', 'request', 'tick'), default='list')
    ap.add_argument('text', nargs='?', default='')
    ap.add_argument('--kind', default='vehicle', choices=tuple(KINDS))
    ap.add_argument('--takes', type=int, default=4)
    ap.add_argument('--frog', action='store_true', help='frog-themed dieselpunk World War One flavour')
    ap.add_argument('--title', default='')
    a = ap.parse_args(argv)
    where = folder()
    try:
        if a.verb == 'request':
            r = request(a.text, kind=a.kind, title=a.title, takes=a.takes, theme='frog' if a.frog else '', where=where)
            print(f'concepts: {r["id"]} queued, {r["takes"]} takes: {r["prompt"]}')
            return 0
        if a.verb == 'tick':
            reqs = [r for r in read_requests(where) if r['state'] in ('queued', 'rendering')]
            for r in reqs:
                r = step(r, where)
                print(f'concepts: {r["id"]} {r["state"]}: {r.get("why", "")}')
            print(f'concepts: {len(reqs)} open requests')
            return 0
    except ValueError as e:
        print(f'concepts: {e}')
        return 1
    items = scan(requests=read_requests(where))
    print(f'concepts: {len(items)} pictures, {len(approved(where))} approved, in {where}')
    for s in sessions_table():
        print(f'  {s["label"]:34s} {sum(1 for it in items if it["s"] == s["id"])}')
    for r in read_requests(where):
        print(f'  request {r["id"]} {r["state"]:9s} {r["title"]}: {r.get("why", "")}')
    return 0


if __name__ == '__main__':
    sys.exit(main())
