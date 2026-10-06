"""The last picture or film a worker had in its hands, for its profile on the control screen (index.html).

Nothing records what a worker made. What its transcript does hold: every picture it looked at (a Read of an image)
and every picture or film one of its commands named (the film it cut, the screenshot it took). On the laptop's
transcripts of one week (2026-10-06) every image file call was a Read, never a Write, and films were named only in
commands. So a visual says which it is (`how`): 'read' looked at, 'shell' named in a command, 'capture' the newest
picture under the branch's Captures folder, for a worker whose transcript names none that is still there.

src_graphs.take() asks named() about each tool call while it reads the transcripts (one cached pass for both), and
keeps the last KEEP per transcript. ops.py calls attach(): the newest that still exists gets a small copy in the
site (img/last/) and its name on the worker in data/ops.js.
Not seen: a picture a script wrote without its name in the command, a picture a tool returned without a path, a
picture the owner pasted.
"""
import json
import os
import re
import shutil
from pathlib import Path

PICTURES = ('.png', '.jpg', '.jpeg', '.gif', '.webp')
FILMS = ('.mp4', '.webm')        # what a browser plays; a .mov or an .mkv is passed over
KEEP = 16                        # candidates kept per transcript: the newest may be gone, the one before it not
FILM_MAX = 30 * 2 ** 20          # a film larger than this is not copied into the site
MOVING_MAX = 8 * 2 ** 20         # ... and a gif larger than this is shown as its first frame
WIDE = 960                       # a picture is shown at most this wide
CAPTURE_DAYS = 7                 # an older capture is not this worker's

WORD = re.compile(r'"([^"\n]*)"|\'([^\'\n]*)\'|([^\s"\'|;&<>()=,]+)')
CD = re.compile(r'(?:^|&&|;|\n)\s*(?:cd|pushd|Set-Location)\s+(?:"([^"\n]+)"|\'([^\'\n]+)\'|([^\s;&|]+))', re.I)
LOOSE = set('*?$%{}`')           # a word with one of these is a pattern or a variable, not a file


def full(p, base):
    """A path as a command spelled it, made whole: Git Bash's /c/Users/x is C:/Users/x on Windows, ~ is the home
    folder, and a relative one hangs under `base`. None when it cannot be told where it is."""
    p = p.strip()
    if os.name == 'nt':
        p = re.sub(r'^/([a-zA-Z])/', lambda m: m.group(1).upper() + ':/', p)
    if p.startswith('~'):
        p = str(Path.home()) + p[1:]
    try:
        q = Path(p)
        if not q.is_absolute():
            if not base or p.startswith('/'):         # /tmp/x in Git Bash is somewhere this does not know
                return None
            q = Path(base) / q
        return os.path.normpath(str(q))
    except (OSError, ValueError):
        return None


def named(name, inp, cwd=None):
    """The pictures and films one tool call names, in the order it names them, each with how: [(path, 'read')] for a
    file tool on a picture, [(path, 'shell'), ...] for a command. A relative path in a command hangs under the
    folder the command itself changes to, else under the session's."""
    inp = inp if isinstance(inp, dict) else {}
    if name in ('Bash', 'PowerShell'):
        text = str(inp.get('command') or '')
        there = CD.search(text)
        base = full(next(g for g in there.groups() if g), cwd) if there else cwd
        out = []
        for m in WORD.finditer(text):
            word = next(g for g in m.groups() if g is not None)
            low = word.lower()
            if not low.endswith(PICTURES + FILMS) or LOOSE & set(word) or re.split(r'[\/]', low)[-1] in PICTURES + FILMS:
                continue                              # not a picture or film, a pattern or a variable, or an ending with no name
            p = full(word, base)
            if p and p not in [o[0] for o in out]:
                out.append((p, 'shell'))
        return out
    path = str(inp.get('file_path') or '')
    if path.lower().endswith(PICTURES + FILMS):
        p = full(path, cwd)
        return [(p, 'read')] if p else []
    return []


def keep(seen, when, found):
    """Add what one call named to a transcript's list [[when, path, how], ...], oldest first, a path once (at its
    newest mention) and no more than KEEP."""
    for p, how in found:
        seen[:] = [s for s in seen if s[1] != p] + [[int(when), p, how]]
    del seen[:-KEEP]
    return seen


def pick(seen):
    """The newest candidate that is still there and that a page can show: dict(path, when, how), or None."""
    for when, p, how in reversed(seen or []):
        try:
            f = Path(p)
            if not f.is_file():
                continue
            if f.suffix.lower() in FILMS and f.stat().st_size > FILM_MAX:
                continue
        except OSError:
            continue
        return dict(path=str(f), when=when, how=how)
    return None


def capture(checkout, now):
    """The newest picture under a checkout's Captures folder, when it is of the last CAPTURE_DAYS days."""
    root = Path(checkout) / 'trench-warfare-3d' / 'Captures' if checkout else None
    best = None
    try:
        for f in root.rglob('*') if root and root.is_dir() else []:
            if f.suffix.lower() in PICTURES and f.is_file():
                t = f.stat().st_mtime
                if best is None or t > best[0]:
                    best = (t, f)
    except OSError:
        return None
    if best is None or now - best[0] > CAPTURE_DAYS * 86400:
        return None
    return dict(path=str(best[1]), when=int(best[0]), how='capture')


def shrink(src: Path, dst: Path):
    """A picture at most WIDE across, written beside the page: a .jpg, or a .png when it has see-through parts.
    Returns the file written, or None when it cannot be read as a picture."""
    try:
        from PIL import Image
        with Image.open(src) as im:
            im.load()
            clear = im.mode in ('RGBA', 'LA') or 'transparency' in im.info
            im = im.convert('RGBA' if clear else 'RGB')
            if im.width > WIDE:
                im = im.resize((WIDE, max(1, round(im.height * WIDE / im.width))))
            dst = dst.with_suffix('.png' if clear else '.jpg')
            dst.parent.mkdir(parents=True, exist_ok=True)
            tmp = dst.with_name(dst.name + '.tmp')
            im.save(tmp, 'PNG' if clear else 'JPEG', **({} if clear else dict(quality=86)))
            tmp.replace(dst)
            return dst
    except Exception:                                 # no Pillow, or a file that only has a picture's name
        return None


def place(out: Path, key, v, index):
    """Put a worker's visual in the site as img/last/<key>.<ext> and return that name ('' when it cannot be shown).
    `index` {key: [source, mtime, size, name]} says what is there already, so a file is made once."""
    src = Path(v['path'])
    try:
        st = src.stat()
    except OSError:
        return ''
    mark = [str(src), int(st.st_mtime), st.st_size]
    had = index.get(key)
    if had and had[:3] == mark and (out / had[3]).exists():
        return had[3]
    ext = src.suffix.lower()
    dst = out / 'img' / 'last' / (key + ext)
    whole = ext in FILMS or (ext == '.gif' and st.st_size <= MOVING_MAX)
    if whole:
        dst.parent.mkdir(parents=True, exist_ok=True)
        tmp = dst.with_name(dst.name + '.tmp')
        shutil.copyfile(src, tmp)
        tmp.replace(dst)
    else:
        dst = shrink(src, dst)
        if dst is None:
            return ''
    name = dst.relative_to(out).as_posix()
    index[key] = mark + [name]
    return name


def by_worker(seen):
    """{a transcript's path: its candidates} as {a worker's key: candidates}: a session is 'session:' and the first
    eight of its transcript's name (its id on the floor), an agent '#' and the first six of its log's (the end of
    its uid)."""
    out = {}
    for path, cands in seen.items():
        f = Path(path)
        if f.parent.name == 'subagents':
            out['#' + (f.stem[len('agent-'):] if f.stem.startswith('agent-') else f.stem)[:6]] = cands
        else:
            out['session:' + f.stem[:8]] = cands
    return out


def attach(lanes, seen, out: Path, now):
    """Give every session and agent on the floor its `visual`: dict(src, kind 'picture' or 'film', how, name, at).
    Files of workers that left are removed from img/last/. Returns how many got one."""
    store = out / 'img' / 'last' / 'last.json'
    try:
        index = json.loads(store.read_text(encoding='utf-8'))
    except (OSError, ValueError):
        index = {}
    by, used, n = by_worker(seen), {}, 0
    for l in lanes:
        fallback = False
        for w in l.get('workers', []):
            if w.get('kind') == 'session':
                key, cands = 's-' + w['id'].split(':')[-1], by.get(w['id'])
            elif w.get('kind') == 'agent' and '#' in w.get('uid', ''):
                six = w['uid'].rsplit('#', 1)[-1]
                key, cands = 'a-' + six, by.get('#' + six)
            else:
                continue
            v = pick(cands)
            if v is None:
                fallback = capture(l.get('path'), now) if fallback is False else fallback
                v = fallback
            name = place(out, key, v, index) if v else ''
            if name:
                used[key] = index[key]
                w['visual'] = dict(src=name, kind='film' if name.lower().endswith(FILMS) else 'picture', how=v['how'],
                                   name=Path(v['path']).name, at=int(v['when']))
                n += 1
    folder = store.parent
    for f in folder.iterdir() if folder.is_dir() else []:
        if f != store and f.relative_to(out).as_posix() not in [u[3] for u in used.values()]:
            try:
                f.unlink()
            except OSError:
                pass
    if used or store.exists():
        text = json.dumps(used, sort_keys=True)
        if not store.exists() or store.read_text(encoding='utf-8') != text:
            folder.mkdir(parents=True, exist_ok=True)
            store.write_text(text, encoding='utf-8')
    return n
