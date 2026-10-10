#!/usr/bin/env python3
"""The owner's notes: what the owner writes on the board about an asset, a branch, a worker or a question, kept where
every session can read it, and answered where the owner sees the answer.

    python Tools/assetboard/notes.py                       the open notes, oldest first
    python Tools/assetboard/notes.py --for lane/show/x     only those for that branch, and those for everyone
    python Tools/assetboard/notes.py --all                 the answered ones too
    python Tools/assetboard/notes.py done ID "what I did"  answer a note and close it (the page shows the answer)
    python Tools/assetboard/notes.py add "the text" [--lane L] [--asset A]      write one by hand
    python Tools/assetboard/notes.py serve                 only take the page's notes (ops.py --watch does this itself)

WHY. The board showed the owner everything and took nothing back: a remark about a model or a branch had to be typed
into a session, and only that session heard it. Now every thing on the board that can be clicked has a note box, and
a note is a file every session finds.

A NOTE IS THE OWNER'S WORD about the thing it names. Read the open ones for your branch before you start (health.py
lists them), do what they ask or say why not, and answer with `done`. A note is not a decision row: when it decides
something, write that into decisions.md as well.

Where they are: a file a note, in TW_NOTES, else the folder notes of the Drive's TW3D-pipeline when that Drive is
mounted (so both stations read the same ones), else this station's board cache. The page cannot write a file, so
`ops.py --watch` keeps a listener on this machine only (127.0.0.1) that takes a note from the page and writes it
here. It takes nothing without the key in the notes folder (key.txt): the page gets the key from a file beside it,
which a web page from elsewhere cannot read, so nobody else's page can put words in the owner's mouth.
"""
import argparse
import datetime
import hashlib
import hmac
import json
import os
import re
import secrets
import sys
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import build  # noqa: E402

PORT = 8765
KINDS = ('asset', 'lane', 'worker', 'queue', 'graph', 'page')     # what a note can be about
LONGEST = 4000          # characters of a note
FIELDS = ('id', 'when', 'from', 'kind', 'about', 'title', 'lane', 'asset', 'page', 'then', 'sig', 'state')
SIGNED = ('id', 'when', 'from', 'kind', 'about', 'then')        # the head fields a signature covers, with the text
SHOWN_DAYS = 7          # an answered note stays on the page this long


def folder():
    if os.environ.get('TW_NOTES'):
        return Path(os.environ['TW_NOTES'])
    return build.DRIVE / 'notes' if build.DRIVE.is_dir() else build.LOCAL / 'notes'


def key_of(where: Path):
    """The key the page must show; made the first time it is asked for."""
    f = where / 'key.txt'
    try:
        k = f.read_text(encoding='utf-8').strip()
    except OSError:
        k = ''
    if not k:
        where.mkdir(parents=True, exist_ok=True)
        k = secrets.token_urlsafe(18)
        f.write_text(k + '\n', encoding='utf-8')
    return k


def sign_key(make=False):
    """The key this machine's listener signs a click with: a file beside the board's local folder, never on the
    Drive (TW_CLICK_KEY names another). Made by the listener the first time; '' when there is none."""
    f = Path(os.environ.get('TW_CLICK_KEY') or build.LOCAL.parent / 'click.key')
    try:
        k = f.read_text(encoding='utf-8').strip()
    except OSError:
        k = ''
    if not k and make:
        f.parent.mkdir(parents=True, exist_ok=True)
        k = secrets.token_urlsafe(32)
        f.write_text(k + '\n', encoding='utf-8')
    return k


def sign_keys():
    """Every key a signature may be made with, for the machine that checks one: its own listener's, and the keys
    of the other stations' listeners that were brought here (click-keys/*.key beside it)."""
    own = Path(os.environ.get('TW_CLICK_KEY') or build.LOCAL.parent / 'click.key')
    out = [sign_key()]
    for f in sorted((own.parent / 'click-keys').glob('*.key')) if (own.parent / 'click-keys').is_dir() else []:
        try:
            out.append(f.read_text(encoding='utf-8').strip())
        except OSError:
            pass
    return [k for k in out if k]


def signature(note, key):
    """What the listener writes in a note's head as `sig`: a keyed digest of who, when, about what, the stamp of
    the Then line and the words. A note written as a file, or by write() from a script, has none: only a click
    that came through this machine's listener carries one (the critique of 2026-10-10: a note is a plain file,
    so `from: owner, kind: page` was a click any session could make). It does not stop a program that sets out
    to post to the listener as the page does; it stops every note that was merely written."""
    body = '\n'.join(str(note.get(k) or '') for k in SIGNED) + '\n' + str(note.get('text') or '').strip()
    return hmac.new(key.encode('utf-8'), body.encode('utf-8'), hashlib.sha256).hexdigest()[:40]


def signed(note, keys=None):
    """True when the note carries a signature one of the keys here made."""
    sig = str(note.get('sig') or '')
    return bool(sig) and any(hmac.compare_digest(sig, signature(note, k)) for k in (sign_keys() if keys is None else keys))


def line(s, n=200):
    """A value for the head of a note: one line, no more than n characters."""
    return re.sub(r'\s+', ' ', str(s or '')).strip()[:n]


def write(where: Path, text, kind='page', about='', title='', lane='', asset='', page='', who='owner', now=None, then='', sign=''):
    """Write a note and return it. Its name is made here, from the time and what it is about: nothing the page sends
    is used as a path. `then` is the stamp of the "Then: ..." line the Decide page showed under the option he clicked
    (briefs.py): what he saw goes with his click."""
    text = str(text or '').strip()
    if not text:
        raise ValueError('a note with nothing in it')
    if len(text) > LONGEST:
        raise ValueError(f'a note is at most {LONGEST} characters')
    if kind not in KINDS:
        raise ValueError(f'a note is about one of {", ".join(KINDS)}')
    now = now or datetime.datetime.now()
    slug = re.sub(r'[^a-z0-9]+', '-', (line(asset) or line(lane).split('/')[-1] or line(about) or kind).lower()).strip('-')[:40] or kind
    where.mkdir(parents=True, exist_ok=True)
    n, nid = 0, ''
    while not nid or (where / f'{nid}.md').exists():       # two notes in one second about one thing
        nid = f'{now:%Y-%m-%d-%H%M%S}-{slug}' + (f'-{n}' if n else '')
        n += 1
    note = dict(id=nid, when=f'{now:%Y-%m-%d %H:%M:%S}', kind=kind, about=line(about), title=line(title), lane=line(lane), asset=line(asset),
                page=line(page), then=line(then, 16), state='open')
    note['from'] = line(who, 40) or 'owner'
    if sign:                                    # only the listener passes its key: see signature()
        note['sig'] = signature(dict(note, text=text), sign)
    head = ''.join(f'{k}: {note[k]}\n' for k in FIELDS if note.get(k))
    tmp = where / f'{nid}.md.tmp'
    tmp.write_text(f'---\n{head}---\n{text}\n', encoding='utf-8')
    tmp.replace(where / f'{nid}.md')
    return dict(note, text=text, answers=[])


def parse(raw, name=''):
    m = re.match(r'---\n(.*?)\n---\n?(.*)\Z', raw.replace('\r\n', '\n'), re.S)
    if not m:
        return None
    note = dict(id=name, state='open')
    for row in m.group(1).split('\n'):
        k, _, v = row.partition(': ')
        if k in FIELDS:
            note[k] = v.strip()
    body = re.split(r'^## Answer \((.*?)\)\n', m.group(2), flags=re.M)
    note['text'] = body[0].strip()
    note['answers'] = [dict(when=w.split(', ')[0], by=', '.join(w.split(', ')[1:]), text=t.strip()) for w, t in zip(body[1::2], body[2::2])]
    return note


def read_all(where: Path):
    """Every note, oldest first. A file that is not a note is passed over."""
    out = []
    for f in sorted(where.glob('*.md')) if where.is_dir() else []:
        try:
            note = parse(f.read_text(encoding='utf-8', errors='replace'), f.stem)
        except OSError:
            note = None
        if note:
            out.append(note)
    return sorted(out, key=lambda n: (n.get('when', ''), n['id']))


def answer(where: Path, nid, text, by='', now=None):
    """Answer a note and close it. `nid` is its name or the start of it, when that names one note only."""
    hits = [n for n in read_all(where) if n['id'] == nid] or [n for n in read_all(where) if n['id'].startswith(nid)]
    if len(hits) != 1:
        raise ValueError(f'{nid}: {"no such note" if not hits else "that is the start of " + str(len(hits)) + " notes"}')
    now = now or datetime.datetime.now()
    f = where / f'{hits[0]["id"]}.md'
    raw = f.read_text(encoding='utf-8').replace('\r\n', '\n')
    raw = re.sub(r'^state: .*$', 'state: done', raw, count=1, flags=re.M)
    f.write_text(raw.rstrip('\n') + f'\n\n## Answer ({now:%Y-%m-%d %H:%M}{", " + line(by, 80) if by else ""})\n{str(text).strip()}\n', encoding='utf-8')
    return parse(f.read_text(encoding='utf-8'), f.stem)


def for_lane(notes, lane):
    """The notes a session on this branch should read: those that name it, and those that name no branch."""
    return [n for n in notes if not n.get('lane') or n['lane'] == lane]


def shown(notes, now=None):
    """What the page lists: every open note, and the answered ones of the last SHOWN_DAYS days."""
    since = f'{(now or datetime.datetime.now()) - datetime.timedelta(days=SHOWN_DAYS):%Y-%m-%d %H:%M}'
    return [n for n in notes if n['state'] != 'done' or (n['answers'] and n['answers'][-1]['when'] >= since) or n.get('when', '') >= since]


def lines(notes):
    out = []
    for n in notes:
        about = ', '.join(x for x in (n.get('asset') and 'asset ' + n['asset'], n.get('lane'), n.get('title') if n.get('title') not in (n.get('asset'), n.get('lane')) else '') if x)
        out.append(f'{"open" if n["state"] != "done" else "done"}  {n["id"]}  ({n.get("when", "")[:16]}, {about or n.get("kind", "")})')
        out += ['      ' + row for row in n['text'].split('\n')]
        for a in n['answers']:
            out += [f'      -> {a["when"]}{", " + a["by"] if a["by"] else ""}: ' + a['text'].replace('\n', '\n         ')]
    return out


# ---- the listener the page writes through ----------------------------------------------------------------------------

def handler(where: Path, key: str, sign: str = ''):
    class Box(BaseHTTPRequestHandler):
        def log_message(self, *a):
            pass

        def reply(self, code, body):
            data = json.dumps(body).encode('utf-8')
            self.send_response(code)
            # a page opened as a file has no origin of its own; the key, not the origin, is what is asked for
            self.send_header('Access-Control-Allow-Origin', '*')
            self.send_header('Access-Control-Allow-Headers', 'Content-Type')
            self.send_header('Access-Control-Allow-Methods', 'POST, GET, OPTIONS')
            self.send_header('Content-Type', 'application/json; charset=utf-8')
            self.send_header('Content-Length', str(len(data)))
            self.end_headers()
            self.wfile.write(data)

        def do_OPTIONS(self):
            self.reply(200, {})

        def do_GET(self):
            self.reply(200, dict(ok=True, box='tw3d notes')) if self.path.split('?')[0] == '/ping' else self.reply(404, dict(ok=False, why='nothing here'))

        def do_POST(self):
            try:
                size = int(self.headers.get('Content-Length') or 0)
                if size > 4 * LONGEST + 2000:
                    return self.reply(413, dict(ok=False, why='too long'))
                d = json.loads(self.rfile.read(size).decode('utf-8'))
                if not isinstance(d, dict) or not secrets.compare_digest(str(d.get('key', '')), key):
                    return self.reply(403, dict(ok=False, why='the key is not the one in the notes folder'))
                if self.path == '/note':
                    note = write(where, d.get('text'), sign=sign, **{k: d.get(k, '') for k in ('kind', 'about', 'title', 'lane', 'asset', 'page', 'then') if d.get(k)})
                elif self.path == '/close':
                    note = answer(where, str(d.get('id', '')), d.get('text') or 'Closed by the owner.', by='owner')
                else:
                    return self.reply(404, dict(ok=False, why='nothing here'))
                self.reply(200, dict(ok=True, note=note))
            except (ValueError, TypeError) as e:
                self.reply(400, dict(ok=False, why=str(e)))
    return Box


def serve(where: Path = None, port=None, background=True):
    """Start the listener, on this machine only. Returns (server, key), or (None, key) when the port is taken: then a
    listener is most likely running already, and the page finds that one."""
    where = where or folder()
    key = key_of(where)
    port = PORT if port is None else port
    try:
        server = ThreadingHTTPServer(('127.0.0.1', port), handler(where, key, sign_key(make=True)))
    except OSError:
        return None, key
    if background:
        threading.Thread(target=server.serve_forever, daemon=True).start()
    return server, key


def main(argv=None):
    ap = argparse.ArgumentParser(description="the owner's notes from the board")
    ap.add_argument('verb', nargs='?', choices=('list', 'done', 'add', 'serve'), default='list')
    ap.add_argument('args', nargs='*')
    ap.add_argument('--for', dest='lane', default='', help='only the notes for this branch and for everyone')
    ap.add_argument('--all', action='store_true', help='the answered ones too')
    ap.add_argument('--lane', dest='about_lane', default='')
    ap.add_argument('--asset', default='')
    ap.add_argument('--by', default='', help='who answers (your branch)')
    a = ap.parse_args(argv)
    where = folder()
    try:
        if a.verb == 'done':
            if len(a.args) != 2:
                print('notes: done ID "what was done"')
                return 2
            n = answer(where, a.args[0], a.args[1], by=a.by)
            print(f'notes: {n["id"]} is answered and closed')
            return 0
        if a.verb == 'add':
            if len(a.args) != 1:
                print('notes: add "the text"')
                return 2
            n = write(where, a.args[0], kind='asset' if a.asset else 'lane' if a.about_lane else 'page', about=a.asset or a.about_lane, lane=a.about_lane, asset=a.asset, who=a.by or 'owner')
            print(f'notes: wrote {n["id"]} in {where}')
            return 0
    except ValueError as e:
        print(f'notes: {e}')
        return 1
    if a.verb == 'serve':
        server, _ = serve(where, background=False)
        if not server:
            print(f'notes: port {PORT} is taken (a listener is most likely running already)')
            return 1
        print(f'notes: taking the page\'s notes on 127.0.0.1:{PORT}, into {where}', flush=True)
        try:
            server.serve_forever()
        except KeyboardInterrupt:
            pass
        return 0
    notes = read_all(where)
    if a.lane:
        notes = for_lane(notes, a.lane)
    if not a.all:
        notes = [n for n in notes if n['state'] != 'done']
    print(f'notes: {len(notes)} {"" if a.all else "open "}in {where}' + (f' for {a.lane}' if a.lane else ''))
    print('\n'.join(lines(notes)))
    return 0


if __name__ == '__main__':
    sys.exit(main())
