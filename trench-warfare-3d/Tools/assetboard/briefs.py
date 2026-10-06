#!/usr/bin/env python3
"""Decision briefs: for every decision that waits on the owner, one short report he can decide from.

WHY. "Needs you" listed thirty decisions as thirty titles. The owner (2026-10-06): "i need you for every decision
that has to be made, also future ones write a really short to the point report on what the decision is for and any
visual evidence." A brief says what the decision is for, the options (the first is the one the writer would take, and
why), and shows the pictures or films that bear on it. It is short by rule: add() refuses one that is not.

    python Tools/assetboard/briefs.py                       the open briefs
    python Tools/assetboard/briefs.py missing               what waits on the owner with no brief yet (run from trench-warfare-3d/)
    python Tools/assetboard/briefs.py add --title "The house's look" --for "What the house is drawn like ..." \
        --option "Near-black where nobody works" --option "Every room light" --why "It is the site's own look" \
        --evidence shots/after.png="As built" --evidence shots/before.png="Before" [--about "A title under Open in decisions.md"] [--lane lane/show/x]
    python Tools/assetboard/briefs.py take ID --by lane/show/x    he answered on the page: close it with what his note says, and answer the note
    python Tools/assetboard/briefs.py answer ID B "his words"     he answered somewhere else: the brief is closed with it

A brief is a folder of its own beside the owner's notes (the folder decisions of the Drive's TW3D-pipeline; TW_BRIEFS
names another): brief.json and a copy of each piece of evidence, so it still shows when the original is gone. The
board shows them on decide.html (ops.py puts them in the site on every read, static/decide.js draws them); the owner
picks an option there, which leaves a note (notes.py) that is his word; the session that takes it up writes the row
in decisions.md and closes the brief with `answer`.
--no-evidence "why" is for a decision nothing can be shown of; a brief without either is refused.
"""
import argparse
import datetime
import json
import os
import re
import shutil
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import build  # noqa: E402

FOR_WORDS = 45          # "really short": what the decision is for, in at most this many words
OPTION_WORDS = 22       # ... an option
WHY_WORDS = 35          # ... why the first option
CAPTION_WORDS = 14
MOST_OPTIONS, MOST_EVIDENCE = 4, 6
PICTURES = ('.png', '.jpg', '.jpeg', '.gif', '.webp')
FILMS = ('.mp4', '.webm')
FILM_MAX = 30 * 2 ** 20
WIDE = 1600             # a picture of evidence is kept at most this wide
SHOWN_DAYS = 7          # an answered brief stays on the page this long


def folder():
    if os.environ.get('TW_BRIEFS'):
        return Path(os.environ['TW_BRIEFS'])
    return build.DRIVE / 'decisions' if build.DRIVE.is_dir() else build.LOCAL / 'decisions'


def words(s):
    return len(str(s or '').split())


def slug(s):
    return re.sub(r'[^a-z0-9]+', '-', str(s).lower()).strip('-')[:48] or 'decision'


def one(s):
    return re.sub(r'\s+', ' ', str(s or '')).strip()


def check(title, what_for, options, why, evidence, no_evidence):
    """Why a brief would be refused: every reason, in words the writer can act on. An empty list is a brief."""
    bad = []
    if not one(title):
        bad.append('it has no title')
    if not one(what_for):
        bad.append('it does not say what the decision is for')
    elif words(what_for) > FOR_WORDS:
        bad.append(f'what it is for takes {words(what_for)} words; {FOR_WORDS} at most')
    if not 2 <= len(options) <= MOST_OPTIONS:
        bad.append(f'it has {len(options)} options; 2 to {MOST_OPTIONS}, the one you would take first')
    for o in options:
        if not one(o):
            bad.append('an option is empty')
        elif words(o) > OPTION_WORDS:
            bad.append(f'an option takes {words(o)} words; {OPTION_WORDS} at most: "{one(o)[:40]}..."')
    if not one(why):
        bad.append('it does not say why the first option')
    elif words(why) > WHY_WORDS:
        bad.append(f'the why takes {words(why)} words; {WHY_WORDS} at most')
    if len(evidence) > MOST_EVIDENCE:
        bad.append(f'it has {len(evidence)} pieces of evidence; {MOST_EVIDENCE} at most: the ones that decide it')
    for path, caption in evidence:
        p = Path(path)
        if not p.is_file():
            bad.append(f'the evidence {path} is not a file')
        elif p.suffix.lower() not in PICTURES + FILMS:
            bad.append(f'the evidence {p.name} is not a picture or a film a page can show')
        elif p.suffix.lower() in FILMS and p.stat().st_size > FILM_MAX:
            bad.append(f'the film {p.name} is over {FILM_MAX // 2 ** 20} MB: cut the part that shows it')
        if not one(caption):
            bad.append(f'the evidence {p.name} has no caption: say what it shows')
        elif words(caption) > CAPTION_WORDS:
            bad.append(f'the caption of {p.name} takes {words(caption)} words; {CAPTION_WORDS} at most')
    if not evidence and not one(no_evidence):
        bad.append('it shows nothing: give the evidence, or say with --no-evidence why nothing can be shown')
    return bad


def keep(src: Path, dst: Path):
    """A copy of a piece of evidence in the brief's folder: a film as it is, a picture at most WIDE across."""
    dst.parent.mkdir(parents=True, exist_ok=True)
    if src.suffix.lower() in PICTURES and src.suffix.lower() != '.gif':
        try:
            from PIL import Image
            with Image.open(src) as im:
                im.load()
                if im.width > WIDE:
                    im = im.resize((WIDE, max(1, round(im.height * WIDE / im.width))))
                    im.save(dst)
                    return dst
        except Exception:               # no Pillow, or a file it cannot read: the copy below serves
            pass
    shutil.copyfile(src, dst)
    return dst


def add(where: Path, title, what_for, options, why, evidence=(), no_evidence='', about='', lane='', by='', now=None):
    """Write a brief and return it. `evidence` is [(path, caption)]; the first option is the one the writer would take.
    A brief that is not short, shows nothing without saying why, or names a file that is not there, is a ValueError
    that says every reason."""
    evidence = list(evidence)
    bad = check(title, what_for, options, why, evidence, no_evidence)
    if bad:
        raise ValueError('not a brief yet: ' + '; '.join(bad))
    now = now or datetime.datetime.now()
    bid, n = f'{now:%Y-%m-%d}-{slug(title)}', 1
    while (where / bid).exists():
        n += 1
        bid = f'{now:%Y-%m-%d}-{slug(title)}-{n}'
    home, shown = where / bid, []
    for i, (path, caption) in enumerate(evidence):
        src = Path(path)
        name = f'{i + 1}-{slug(src.stem)}{src.suffix.lower()}'
        keep(src, home / name)
        shown.append(dict(file=name, caption=one(caption), kind='film' if src.suffix.lower() in FILMS else 'picture', source=str(src)))
    b = dict(id=bid, title=one(title), asked=f'{now:%Y-%m-%d %H:%M}', by=one(by), lane=one(lane), about=one(about), state='open',
             what_for=one(what_for), options=[dict(key='ABCD'[i], text=one(o)) for i, o in enumerate(options)], pick='A', why=one(why),
             evidence=shown, no_evidence=one(no_evidence))
    home.mkdir(parents=True, exist_ok=True)
    (home / 'brief.json').write_text(json.dumps(b, indent=1, sort_keys=True) + '\n', encoding='utf-8')
    return b


def read_all(where: Path):
    """Every brief there is, the one asked first first. A folder without a brief that reads is passed over."""
    out = []
    for f in sorted(where.glob('*/brief.json')) if where.is_dir() else []:
        try:
            b = json.loads(f.read_text(encoding='utf-8'))
        except (OSError, ValueError):
            continue
        if isinstance(b, dict) and b.get('id') == f.parent.name:
            out.append(b)
    return sorted(out, key=lambda b: (b.get('asked', ''), b['id']))


def answer(where: Path, bid, option, said='', by='', now=None):
    """The owner decided: close the brief with the option he took and his words. Returns the brief."""
    hits = [b for b in read_all(where) if b['id'] == bid or b['id'].endswith(bid)]
    if len(hits) != 1:
        raise ValueError(f'{len(hits)} briefs are called {bid}')
    b = hits[0]
    if b.get('state') == 'answered':        # a brief is closed once: a second session finds it taken
        a = b.get('answer') or {}
        raise ValueError(f'{b["id"]} is already closed ({a.get("when", "")}{", by " + a["by"] if a.get("by") else ""}: {a.get("option", "")})')
    keys = [o['key'] for o in b['options']]
    if option not in keys and option != 'other':
        raise ValueError(f'{bid} has the options {", ".join(keys)} (or "other", with his words)')
    if option == 'other' and not one(said):
        raise ValueError('an answer that is none of the options needs his words')
    now = now or datetime.datetime.now()
    b.update(state='answered', answer=dict(option=option, said=one(said), by=one(by), when=f'{now:%Y-%m-%d %H:%M}'))
    (where / b['id'] / 'brief.json').write_text(json.dumps(b, indent=1, sort_keys=True) + '\n', encoding='utf-8')
    return b


def waiting(briefs, notes):
    """The open briefs the owner has answered on the page and no session has taken up yet: [(brief, his note)], with
    his last open note about each. `notes` is notes.read_all()."""
    out = []
    for b in briefs:
        his = [n for n in notes if n.get('about') == 'brief:' + b['id'] and n.get('state') != 'done']
        if b.get('state') != 'answered' and his:
            out.append((b, his[-1]))
    return out


def said(b, text):
    """What a note left on the page says: (the option, his words). A click writes "B: the option's text" and his own
    words under it; words alone are an answer that is none of the options."""
    first, _, rest = str(text or '').strip().partition('\n')
    m = re.match(r'([A-D]): ', first)
    if m and any(o['key'] == m.group(1) for o in b['options']):
        return m.group(1), one(rest)
    return 'other', one(text)


def take(where: Path, notes_where: Path, bid, by='', now=None):
    """Take the owner's answer up in one step: close the brief with what his note says and answer every open note of
    his about it. A brief somebody has already taken is refused, so two sessions do not both act on one answer.
    Write the row in decisions.md first. Returns (the brief, his note)."""
    import notes
    hits = [(b, n) for b, n in waiting(read_all(where), notes.read_all(notes_where)) if b['id'] == bid or b['id'].endswith(bid)]
    if len(hits) != 1:
        raise ValueError(f'{bid}: {len(hits)} open briefs of that name have an answer of the owner\'s waiting')
    b, n = hits[0]
    option, words = said(b, n['text'])
    b = answer(where, b['id'], option, words, by, now)
    for m in notes.read_all(notes_where):
        if m.get('about') == 'brief:' + b['id'] and m.get('state') != 'done':
            notes.answer(notes_where, m['id'], f'Taken up: the brief is closed with {option}{" (your own words)" if option == "other" else ""}.', by=by, now=now)
    return b, n


def shown(briefs, now=None):
    """What the page lists: every open brief, and the ones answered in the last SHOWN_DAYS days."""
    since = f'{(now or datetime.datetime.now()) - datetime.timedelta(days=SHOWN_DAYS):%Y-%m-%d %H:%M}'
    return [b for b in briefs if b.get('state') != 'answered' or (b.get('answer') or {}).get('when', '') >= since]


def same(a, b):
    """Whether a brief is about a question of the queue: the same title, however it is spelled."""
    return bool(slug(a)) and slug(a) == slug(b)


def missing(briefs, titles):
    """The questions that wait on the owner with no brief: what is still to be written."""
    have = [b.get('about') or b['title'] for b in briefs] + [b['title'] for b in briefs]
    return [t for t in titles if not any(same(t, h) for h in have)]


def site(where: Path, out: Path, now=None):
    """Put the briefs the page shows in the site: data/briefs.js, and their evidence under img/brief/<id>/ (a file
    is copied once). Folders of briefs no longer shown are removed. Returns what the page was given."""
    listed = shown(read_all(where), now)
    for b in listed:
        for e in b['evidence']:
            src, dst = where / b['id'] / e['file'], out / 'img' / 'brief' / b['id'] / e['file']
            try:
                if not dst.exists() or dst.stat().st_size != src.stat().st_size:
                    dst.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copyfile(src, dst)
                e['src'] = dst.relative_to(out).as_posix()
            except OSError:
                e['src'] = ''                       # the evidence is gone: the page says so
    root = out / 'img' / 'brief'
    for d in root.iterdir() if root.is_dir() else []:
        if d.is_dir() and d.name not in [b['id'] for b in listed]:
            shutil.rmtree(d, ignore_errors=True)
    text = f'window.BRIEFS = {json.dumps(listed, sort_keys=True)};\n'
    f = out / 'data' / 'briefs.js'
    if not f.exists() or f.read_text(encoding='utf-8') != text:
        f.parent.mkdir(parents=True, exist_ok=True)
        f.write_text(text, encoding='utf-8')
    return listed


def lines(briefs):
    out = []
    for b in briefs:
        a = b.get('answer')
        out.append(f'{"done" if a else "open"}  {b["id"]}  ({b["asked"]}{", " + b["lane"] if b.get("lane") else ""})')
        out.append(f'      {b["title"]}: {b["what_for"]}')
        for o in b['options']:
            out.append(f'      {o["key"]}{" (the writer\'s)" if o["key"] == b["pick"] else ""}  {o["text"]}')
        out.append(f'      evidence: {len(b["evidence"])}' + (f' ({b["no_evidence"]})' if b.get('no_evidence') else ''))
        if a:
            out.append(f'      answered {a["when"]}: {a["option"]}{" · " + a["said"] if a.get("said") else ""}')
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description='decision briefs for the owner')
    ap.add_argument('what', nargs='?', default='list', choices=('list', 'add', 'answer', 'take', 'missing'))
    ap.add_argument('args', nargs='*')
    ap.add_argument('--all', action='store_true', help='the answered ones too')
    ap.add_argument('--title', default='')
    ap.add_argument('--for', dest='what_for', default='', help=f'what the decision is for, {FOR_WORDS} words at most')
    ap.add_argument('--option', action='append', default=[], help='an option; the first is the one you would take')
    ap.add_argument('--why', default='', help='why the first option')
    ap.add_argument('--evidence', action='append', default=[], help='PATH=what it shows; a picture or a short film')
    ap.add_argument('--no-evidence', default='', help='why nothing can be shown')
    ap.add_argument('--about', default='', help='the title of the question in decisions.md this brief is for')
    ap.add_argument('--lane', default='')
    ap.add_argument('--by', default='')
    a = ap.parse_args(argv)
    where = folder()
    try:
        if a.what == 'add':
            ev = [(e.rsplit('=', 1) + [''])[:2] for e in a.evidence]
            b = add(where, a.title, a.what_for, a.option, a.why, ev, a.no_evidence, a.about, a.lane, a.by)
            print(f'briefs: {b["id"]} is written, in {where}')
        elif a.what == 'answer':
            if len(a.args) < 2:
                raise ValueError('answer ID OPTION ["his words"]')
            b = answer(where, a.args[0], a.args[1], ' '.join(a.args[2:]), a.by)
            print(f'briefs: {b["id"]} is closed with {b["answer"]["option"]}')
        elif a.what == 'take':
            if len(a.args) != 1:
                raise ValueError('take ID [--by your-branch]')
            import notes
            b, n = take(where, notes.folder(), a.args[0], a.by)
            print(f'briefs: {b["id"]} is closed with {b["answer"]["option"]}, and his note {n["id"]} is answered')
        elif a.what == 'missing':
            import src_queue
            import src_git
            rows = [r for r in src_queue.open_questions(build.REPO, src_git.INTEGRATION, {}) if not r['answered']]
            left = missing(read_all(where), [r.get('title') or r.get('top') or '' for r in rows])
            print(f'briefs: {len(left)} of {len(rows)} open questions have no brief')
            print('\n'.join('      ' + t for t in left))
        else:
            every = read_all(where)
            some = every if a.all else [b for b in every if b.get('state') != 'answered']
            print(f'briefs: {len(some)} {"in all" if a.all else "open"} in {where}')
            print('\n'.join(lines(some)))
    except ValueError as e:
        print(f'briefs: {e}')
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
