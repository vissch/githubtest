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
    python Tools/assetboard/briefs.py then ID B --says "Queues a sim lane: houses cut the damage" --unit unit.json
                                                                  what happens when he takes B: the unit that is queued (a file of the
                                                                  relay's queue), or with no --unit that nothing is built
    python Tools/assetboard/briefs.py waiting [--json]            what he has answered on the page and nobody has taken up, and what each leads to
    python Tools/assetboard/briefs.py unit ID --note NOTE --out unit.json     the unit his click queues, when his click is a yes to it
    python Tools/assetboard/briefs.py take ID --note NOTE --by lane/show/x [--queued UNIT | --outcome "words"]
                                                                  close it with what his notes say and what became of it, and answer the notes
    python Tools/assetboard/briefs.py answer ID B "his words"     he answered somewhere else: the brief is closed with it
    python Tools/assetboard/briefs.py concepts --title "The flame jet" --for "What the flamethrower's jet looks like ..." \
        --concept a.png="One long card, ragged edge" --concept b.html="Three puffs in a row" --why "It reads at 120 m" [--lane lane/show/x]
                                                                  concepts or references to pick from, before the work is built
    python Tools/assetboard/briefs.py steps [--board DIR] [--dry-run]     a brief for every step an asset has passed, from its captures;
                                                                  and the steps that owe a capture

A brief is a folder of its own beside the owner's notes (the folder decisions of the Drive's TW3D-pipeline; TW_BRIEFS
names another): brief.json and a copy of each piece of evidence, so it still shows when the original is gone. The
board shows them on decide.html (ops.py puts them in the site on every read, static/decide.js draws them); the owner
picks an option there, which leaves a note (notes.py) that is his word; the session that takes it up writes the row
in decisions.md and closes the brief with `take`.
EVERY STEP OF AN ASSET (the owner, 2026-10-06). `steps` writes a brief for each stage of an item on the pipeline's
board that has passed: approve it, send it back, or ask for better captures, with the step's own pictures. A step
with nothing a page can show gets no brief and is listed as owing a capture; so is a stage that names no band.
ops.py runs it on every read.
CONCEPTS FIRST (the owner, 2026-10-06). Work on an effect, an animation or a character starts with a brief of two to
four concepts or references he picks from: `concepts`. Each is a picture, a short film, or a page drawn in HTML or
SVG (photographed here). Nothing of that work is built before he has picked.
--no-evidence "why" is for a decision nothing can be shown of; a brief without either is refused.

WHEN A CLICK IS A YES TO WORK (the owner, 2026-10-06). An option may say what happens then: "Then: ..." under it on the
page, and the unit that is queued. A click on such an option is his yes for that unit, and only then: the page sends
the stamp of the Then line it showed with the click, and answers() says `queue` only when every note he left about
the brief is a click on that option with the stamp the option has now. Anything else (no Then line, one added or
changed after his click, clicks on two options, his own words) is `write`. This is the one place that rule is
written; the relay shows that answers wait and decides nothing.
HIS ANSWER IS THE DECISION (the owner, the same evening: "they are a decision to the question", and yes to "should
your answer on a brief also queue its work, with no second yes?"). So `write` is no question back to him: the session
that takes the answer up writes the unit his answer leads to (id, lane, goal, done_when), queues it, and closes the
brief with it; or closes it with the words why nothing is built. It asks him only when his words do not say what to
build. Until that evening the word was `ask`: the master asked him before any work was queued.
"""
import argparse
import datetime
import hashlib
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
UNIT_ID = re.compile(r'[\w.-]+\Z')
UNIT_LANES = ('lane/sim/', 'lane/show/')
UNIT_KEYS = ('id', 'lane', 'role', 'goal', 'done_when')     # a file of the relay's queue (Tools/relay/sources/lane.py)


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


def add(where: Path, title, what_for, options, why, evidence=(), no_evidence='', about='', lane='', by='', now=None, bid=''):
    """Write a brief and return it. `evidence` is [(path, caption)]; the first option is the one the writer would take.
    A brief that is not short, shows nothing without saying why, or names a file that is not there, is a ValueError
    that says every reason."""
    evidence = list(evidence)
    bad = check(title, what_for, options, why, evidence, no_evidence)
    if bad:
        raise ValueError('not a brief yet: ' + '; '.join(bad))
    now = now or datetime.datetime.now()
    if bid and (where / bid).exists():
        raise ValueError(f'the brief {bid} is there already')
    bid, n = bid or f'{now:%Y-%m-%d}-{slug(title)}', 1
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


def stamp(says, unit=None):
    """A Then line in eight characters. The page sends it with the click, so a click is a yes to the line it showed and
    to no other: a line added or changed afterwards has another stamp."""
    return hashlib.sha1(json.dumps([one(says), unit or None], sort_keys=True).encode('utf-8')).hexdigest()[:8]


def check_then(says, unit=None):
    """Why a Then line would be refused: every reason. `unit` is what gets queued, a file of the relay's queue as a
    dict (id, lane, goal, done_when, and a role when it is not "lane"), or None when nothing is built. Its rules are
    those of Tools/relay/sources/lane.py load(): a unit refused there would be a yes nobody can act on."""
    bad = []
    if not one(says):
        bad.append('it does not say what happens then')
    elif words(says) > CAPTION_WORDS:
        bad.append(f'what happens then takes {words(says)} words; {CAPTION_WORDS} at most')
    if unit is None:
        return bad
    if not isinstance(unit, dict):
        return bad + ['the unit is not a file of the queue: {"id", "lane", "goal", "done_when"}']
    gone = [k for k in ('id', 'lane', 'goal', 'done_when') if not unit.get(k)]
    if gone:
        bad.append(f'the unit has no {", ".join(gone)}')
    more = [k for k in unit if k not in UNIT_KEYS]
    if more:
        bad.append(f'the unit has {", ".join(more)}, which a file of the queue does not')
    if unit.get('id') and not (isinstance(unit['id'], str) and UNIT_ID.match(unit['id'])):
        bad.append('the unit\'s id is letters, digits, . _ - only')
    if unit.get('lane') and not str(unit['lane']).startswith(UNIT_LANES):
        bad.append(f'{unit["lane"]} is not a lane/sim or lane/show lane')
    if unit.get('done_when') and not (isinstance(unit['done_when'], list) and all(isinstance(w, str) and w for w in unit['done_when'])):
        bad.append('done_when is a command as a list of words, e.g. ["python", "Tools/x.py", "--check"]')
    return bad


def find(where: Path, bid):
    """The one brief called `bid`: its id, or the end of it."""
    hits = [b for b in read_all(where) if b['id'] == bid or b['id'].endswith(bid)]
    if len(hits) != 1:
        raise ValueError(f'{len(hits)} briefs are called {bid}')
    return hits[0]


def then(where: Path, notes, bid, option, says, unit=None):
    """Say on an option what happens when the owner takes it: `says` is the line the page shows under it, `unit` the
    unit that is queued (None: nothing is built). Returns the brief. Refused on a brief that is closed, and on one he
    has already answered: a line he did not see when he clicked is not one he said yes to. `notes` is
    notes.read_all()."""
    b = find(where, bid)
    if b.get('state') == 'answered':
        raise ValueError(f'{b["id"]} is closed')
    if any(n.get('about') == 'brief:' + b['id'] and n.get('state') != 'done' for n in notes):
        raise ValueError(f'{b["id"]}: he has answered it already, and a Then line added now is not one he saw')
    o = [x for x in b['options'] if x['key'] == option]
    if not o:
        raise ValueError(f'{b["id"]} has the options {", ".join(x["key"] for x in b["options"])}')
    bad = check_then(says, unit)
    if unit and not bad:
        unit = dict(id=unit['id'], lane=unit['lane'], role=unit.get('role') or 'lane', goal=unit['goal'], done_when=list(unit['done_when']))
        used = [x['id'] for x in read_all(where) for p in x['options'] if ((p.get('then') or {}).get('unit') or {}).get('id') == unit['id'] and (x['id'], p['key']) != (b['id'], option)]
        if used:
            bad.append(f'the unit {unit["id"]} is already what an option of {used[0]} queues: a unit\'s id is used once')
    if bad:
        raise ValueError('not a Then line yet: ' + '; '.join(bad))
    o[0]['then'] = dict(says=one(says), stamp=stamp(says, unit), **(dict(unit=unit) if unit else {}))
    (where / b['id'] / 'brief.json').write_text(json.dumps(b, indent=1, sort_keys=True) + '\n', encoding='utf-8')
    return b


def answer(where: Path, bid, option, said='', by='', now=None, queued='', outcome=''):
    """The owner decided: close the brief with the option he took and his words, and with what became of it when that
    is known (`queued`: the unit on the relay's queue; `outcome`: in words, when nothing was queued). Returns the brief."""
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
    b['answer'].update({k: one(v) for k, v in (('queued', queued), ('outcome', outcome)) if one(v)})
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


def answers(briefs, notes):
    """What the owner has answered on the page and no session has taken up: one record a brief, with every open note
    of his about it (not only the last: words he typed before a click are his too), and what it leads to:
      go 'queue'    his click is a yes to the unit the option names: queue it without asking
      go 'nothing'  his click is a yes to an option that says nothing is built
      go 'write'    anything else, and `why`: his answer is the decision all the same, but it names no unit, so the
                    session writes the unit it leads to and queues it (or says why nothing is built)
    A click is a yes only when every open note about the brief is a click on the page on that one option (from the
    owner, kind page, no words of his own) and carries the stamp the option's Then line has now."""
    out = []
    for b, last in waiting(briefs, notes):
        his = [n for n in notes if n.get('about') == 'brief:' + b['id'] and n.get('state') != 'done']
        picks = [said(b, n['text']) for n in his]
        option = picks[-1][0]
        o = ([x for x in b['options'] if x['key'] == option] or [{}])[0]
        t = o.get('then') or {}
        if not o:
            why = 'he answered in his own words'
        elif not t.get('stamp'):
            why = 'the option has no Then line'
        elif any(w for _, w in picks):
            why = 'he added words of his own'
        elif any(k != option for k, _ in picks):
            why = 'his notes do not all pick the same option'
        elif any(n.get('from') != 'owner' or n.get('kind') != 'page' for n in his):
            why = 'a note about it is not a click of his on the page'
        elif any(n.get('then') != t.get('stamp') for n in his):
            why = 'the Then line is not the one the page showed when he clicked'
        else:
            why = ''
        out.append(dict(id=b['id'], title=b['title'], about=b.get('about', ''), lane=b.get('lane', ''), option=option, text=o.get('text', ''), said=' / '.join(w for _, w in picks if w),
                        when=last['when'], note=last['id'], notes=[dict(id=n['id'], when=n['when'], text=n['text']) for n in his],
                        go='write' if why else 'queue' if t.get('unit') else 'nothing', why=why, says=t.get('says', ''), unit=t.get('unit')))
    return out


def one_answer(where: Path, notes, bid, note):
    """The record of answers() for one brief, for the session that read `note` as his last word on it. Refused when
    he has answered again since: what the session is about to act on is no longer what he said."""
    hits = [a for a in answers(read_all(where), notes) if a['id'] == bid or a['id'].endswith(bid)]
    if len(hits) != 1:
        raise ValueError(f'{bid}: {len(hits)} open briefs of that name have an answer of the owner\'s waiting')
    a = hits[0]
    if not one(note):
        raise ValueError(f'{a["id"]}: say which note of his you read (--note {a["note"]}), so an answer he changes meanwhile is not closed unread')
    if a['note'] != note:
        raise ValueError(f'{a["id"]}: his last note about it is {a["note"]} ({a["when"]}), not {note}: read it first')
    return a


def unit(where: Path, notes, bid, note):
    """The unit his click queues, as the file the relay's queue takes. Only when the click is a yes to it."""
    a = one_answer(where, notes, bid, note)
    if a['go'] != 'queue':
        raise ValueError(f'{a["id"]}: this answer names no unit ({a["why"] or "the option says nothing is built"}): write the unit it leads to yourself')
    return a['unit']


def take(where: Path, notes_where: Path, bid, by='', now=None, note='', queued='', outcome='', option=''):
    """Take the owner's answer up in one step: close the brief with what his notes say and with what became of it, and
    answer every open note of his about it. A brief somebody has already taken is refused, so two sessions do not both
    act on one answer; so is one he has answered again since `note`, the note the session read. What became of it:
    `queued` (the unit on the relay's queue) or `outcome` (in words, when nothing was queued). An answer that is a yes
    to a Then line carries it already; any other answer needs one of the two, so no brief closes without saying.
    `option` overrules the option read from his notes, for an answer whose option his notes do not settle.
    Write the row in decisions.md first. Returns (the brief, his note)."""
    import notes
    a = one_answer(where, notes.read_all(notes_where), bid, note)
    if one(queued) and one(outcome):
        raise ValueError('what became of it is a unit that was queued or words, not both')
    if not one(queued) and not one(outcome):
        if a['go'] == 'write':
            raise ValueError(f'{a["id"]}: say what became of it: --queued UNIT, or --outcome "words" when nothing was queued')
        queued, outcome = (a['unit']['id'], '') if a['go'] == 'queue' else ('', a['says'])
    if one(option) and a['go'] != 'write':
        raise ValueError(f'{a["id"]}: his click says {a["option"]}; another option is for an answer whose option his notes do not settle')
    took = one(option) or a['option']
    b = answer(where, a['id'], took, a['said'], by, now, queued=queued, outcome=outcome)
    became = f'Queued as {one(queued)}.' if one(queued) else one(outcome).rstrip('.') + '.'
    n = [m for m in notes.read_all(notes_where) if m['id'] == a['note']][0]
    for m in notes.read_all(notes_where):
        if m.get('about') == 'brief:' + b['id'] and m.get('state') != 'done':
            notes.answer(notes_where, m['id'], f'Taken up: the brief is closed with {took}{" (your own words)" if took == "other" else ""}. {became}', by=by, now=now)
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


BROWSERS = ('msedge', 'chrome', 'chromium', r'C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe', r'C:\Program Files\Microsoft\Edge\Application\msedge.exe',
            r'C:\Program Files\Google\Chrome\Application\chrome.exe')
PAGES = ('.html', '.htm', '.svg')       # a concept drawn as a page: photographed, so the brief shows a picture of it


def shoot(src: Path, dst: Path, size=(1280, 720)):
    """A picture of a page (a concept drawn in HTML or SVG), taken by a headless browser. A ValueError when no browser
    is found or nothing was written: a concept nobody can see is not put to the owner."""
    import subprocess
    import tempfile
    import time
    exe = next((shutil.which(b) or (b if Path(b).is_file() else '') for b in BROWSERS if shutil.which(b) or Path(b).is_file()), '')
    if not exe:
        raise ValueError(f'no browser to photograph {src.name} with: make a picture of it yourself and give that')
    dst.parent.mkdir(parents=True, exist_ok=True)
    dst = dst.resolve()
    with tempfile.TemporaryDirectory(prefix='tw-shoot-', ignore_cleanup_errors=True) as prof:
        subprocess.run([exe, '--headless', '--disable-gpu', '--hide-scrollbars', '--force-prefers-reduced-motion', f'--window-size={size[0]},{size[1]}',
                        '--virtual-time-budget=4000', f'--user-data-dir={Path(prof).resolve()}', f'--screenshot={dst}', src.resolve().as_uri()], capture_output=True, timeout=120)
        for _ in range(60):                 # the browser returns before the picture is on the disk (seen 2026-10-06): wait for it
            if dst.is_file() and dst.stat().st_size:
                break
            time.sleep(0.5)
    if not dst.is_file() or not dst.stat().st_size:
        raise ValueError(f'the browser made no picture of {src.name}')
    return dst


def concepts(where: Path, title, what_for, shown, why, about='', lane='', by='', now=None, shooter=None):
    """A brief whose options are concepts or references the owner picks from, before anything is built. The owner,
    2026-10-06: "whenever we do vfx, animations or characters, lets make concepts first or search references ... then
    concepts land in the decisions and the user will pick". `shown` is [(path, what it is)], two to four, the writer's
    own choice first: a picture or a short film (generated, or a reference that was found), or a page drawn in HTML or
    SVG, which is photographed. Each is an option, and its picture carries the option's letter. Returns the brief."""
    import tempfile
    shown = [(Path(p), one(c)) for p, c in shown]
    if not 2 <= len(shown) <= MOST_OPTIONS:
        raise ValueError(f'not a brief yet: {len(shown)} concepts; 2 to {MOST_OPTIONS} to pick from, the one you would take first')
    with tempfile.TemporaryDirectory(prefix='tw-concepts-') as tmp:
        ev = []
        for i, (p, c) in enumerate(shown):
            if p.suffix.lower() in PAGES and p.is_file():
                p = (shooter or shoot)(p, Path(tmp) / f'{i + 1}-{slug(p.stem)}.png')
            ev.append((str(p), f'{"ABCD"[i]}: {c}'))
        b = add(where, title, what_for, [c for _, c in shown], why, ev, about=about, lane=lane, by=by, now=now)
    b['kind'] = 'concepts'
    for i, e in enumerate(b['evidence']):
        e['option'] = 'ABCD'[i]
    (where / b['id'] / 'brief.json').write_text(json.dumps(b, indent=1, sort_keys=True) + '\n', encoding='utf-8')
    return b


STEP_OPTIONS = ('Approve this step', 'Send it back: I say below what is wrong with it', 'I cannot judge it from these pictures: capture it better')
STEP_THEN = 'Nothing is built: the step counts as approved'
STEP_SKIP = ('master',)         # a stage of this role is the gate or the landing: his word for those is "land", not a look at a picture


def board_items(board: Path):
    """The items on the pipeline's board (Tools/pipeline/pipeline.py), by the name of their file. One that does not
    read is passed over."""
    out = {}
    for f in sorted((board / 'items').glob('*.json')) if (board / 'items').is_dir() else []:
        try:
            item = json.loads(f.read_text(encoding='utf-8'))
        except (OSError, ValueError):
            continue
        if isinstance(item, dict) and isinstance(item.get('stages'), list):
            out[f.stem] = item
    return out


def newest_result(board: Path, item, stage):
    """The last result a stage has on the board, or None."""
    got = []
    for f in (board / 'results').glob(f'{item}--{stage}--*.json') if (board / 'results').is_dir() else []:
        try:
            r = json.loads(f.read_text(encoding='utf-8'))
        except (OSError, ValueError):
            continue
        if isinstance(r, dict) and r.get('item') == item and r.get('stage') == stage:
            got.append(r)
    return max(got, key=lambda r: (str(r.get('finished_at', '')), r.get('attempt', 0))) if got else None


def step_evidence(board: Path, item, stage, result):
    """What a step has to show: [(path, caption)], the captures its result names first (a band each), then the other
    pictures and films in the step's folder of evidence. Text and numbers are not something a page shows."""
    out, seen = [], set()

    def shows(p):
        return p.is_file() and p.suffix.lower() in PICTURES + FILMS and not (p.suffix.lower() in FILMS and p.stat().st_size > FILM_MAX)
    for band, rel in sorted((result.get('evidence') or {}).items()):
        p = board / str(rel)
        if shows(p) and p.resolve() not in seen:
            seen.add(p.resolve())
            out.append((str(p), f'{stage}: {re.sub(r"[-_]+", " ", str(band))}'))
    home = board / 'evidence' / item / stage
    for p in sorted(home.iterdir()) if home.is_dir() else []:
        if shows(p) and p.resolve() not in seen:
            seen.add(p.resolve())
            out.append((str(p), f'{stage}: {re.sub(r"[-_]+", " ", p.stem)}'))
    return out[:MOST_EVIDENCE]


def steps(where: Path, board: Path, states=None, landed=(), now=None, write=True):
    """Put every step an asset has passed to the owner as a brief he can approve from its captures. The owner,
    2026-10-06: "put in decisions each step a asset goes through, making sure we do captures that make it easy to
    approve". A step is a stage of an item on the pipeline's board whose last result is PASS. Its brief is named after
    the step's job, so a step is put to him once, whichever machine writes it, and again only when it is rebuilt.
    `states` is {(item, stage): state} from pipeline.evaluate when the caller has it: a step whose inputs changed since
    it passed is not put to him. `landed` names the items whose lane is on the integration branch already.
    Returns (briefs written, owed): owed is [(item, stage, why)], the steps with no capture a page can show. Such a step
    gets no brief: an approval of something he cannot see is no approval. `write` False writes nothing and returns
    what would be written as (item, stage, brief id, pieces of evidence)."""
    written, owed = [], []
    for iid, item in board_items(board).items():
        if iid in landed:
            continue
        stages = [s for s in item['stages'] if isinstance(s, dict) and s.get('id') and s.get('role') not in STEP_SKIP]
        for n, s in enumerate(stages):
            sid, r = s['id'], newest_result(board, iid, s['id'])
            passed = bool(r) and r.get('verdict') == 'PASS' and (states is None or states.get((iid, sid), 'DONE') == 'DONE')
            ev = step_evidence(board, iid, sid, r) if passed else []
            if passed and not ev:
                owed.append((iid, sid, 'it passed with nothing a page can show: no brief until it has a capture'))
            elif not s.get('bands'):
                owed.append((iid, sid, 'it names no capture (no band): the pipeline lets it pass with nothing to show'))
            if not ev:
                continue
            bid = 'step-' + slug(r.get('job') or f'{iid}--{sid}')
            if (where / bid).exists():
                continue                        # put to him already, open or decided
            if not write:
                written.append((iid, sid, bid, len(ev)))
                continue
            head = ' '.join(one(item.get('title') or iid).split()[:20]).rstrip('.,;:')
            after = f'Your yes lets the step {stages[n + 1]["id"]} build on it.' if n + 1 < len(stages) else 'It is the last step before the gate.'
            what_for = f'{head}. Its step {sid} passed on the {r.get("station") or "board"}, {str(r.get("finished_at", ""))[:10]}. {after}'
            why = ' '.join(f'It passed its own check, attempt {r.get("attempt", 1)}{": " + one(r["note"]) if one(r.get("note")) else ""}'.split()[:WHY_WORDS])
            b = add(where, f'{iid}: the {sid} step', what_for, STEP_OPTIONS, why, ev, lane=item.get('lane', ''), by='briefs.py steps', now=now, bid=bid)
            b['step'] = dict(item=iid, stage=sid, job=r.get('job', ''), attempt=r.get('attempt', 1))
            b['options'][0]['then'] = dict(says=STEP_THEN, stamp=stamp(STEP_THEN))
            (where / bid / 'brief.json').write_text(json.dumps(b, indent=1, sort_keys=True) + '\n', encoding='utf-8')
            written.append(b)
    return written, owed


def site(where: Path, out: Path, now=None, got=(), owed=()):
    """Put the briefs the page shows in the site: data/briefs.js, and their evidence under img/brief/<id>/ (a file
    is copied once). Folders of briefs no longer shown are removed. Returns what the page was given. `got` is
    answers(): a brief he has answered is given `waits`, what his answer leads to and the note that was read for, so
    the page tells him what his click did."""
    listed = shown(read_all(where), now)
    for a in got:
        for b in listed:
            if b['id'] == a['id']:
                b['waits'] = dict(go=a['go'], note=a['note'], unit=(a['unit'] or {}).get('id', ''))
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
    if owed:                                # the steps that passed with nothing to show him (steps()): the page lists them
        text += f'window.OWED = {json.dumps([dict(item=i, stage=s, why=w) for i, s, w in owed], sort_keys=True)};\n'
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
            if o.get('then'):
                out.append(f'         then: {o["then"]["says"]}' + (f' (queues {o["then"]["unit"]["id"]} on {o["then"]["unit"]["lane"]})' if o['then'].get('unit') else ' (nothing is built)'))
        out.append(f'      evidence: {len(b["evidence"])}' + (f' ({b["no_evidence"]})' if b.get('no_evidence') else ''))
        if a:
            out.append(f'      answered {a["when"]}: {a["option"]}{" · " + a["said"] if a.get("said") else ""}')
            if a.get('queued') or a.get('outcome'):
                out.append(f'      taken up{" by " + a["by"] if a.get("by") else ""}: {"queued as " + a["queued"] if a.get("queued") else a["outcome"]}')
    return out


def leads(a):
    """What an answer leads to, in a few words, for a session's lines."""
    return f'queues {a["unit"]["id"]} on {a["unit"]["lane"]}' if a['go'] == 'queue' else 'nothing to build' if a['go'] == 'nothing' else f'decided: write its unit and queue it, or say why nothing is built ({a["why"]})'


def waiting_lines(got):
    """The answers nobody has taken up, as text: every note of his about each, and what it leads to."""
    out = []
    for a in got:
        out.append(f'{a["go"]:8}{a["id"]}')
        out.append(f'        {a["title"]}')
        for n in a['notes']:
            out.append(f'        {n["when"][5:16]}  {" | ".join(n["text"].splitlines())}      (note {n["id"]})')
        out.append(f'        {leads(a)}')
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description='decision briefs for the owner')
    ap.add_argument('what', nargs='?', default='list', choices=('list', 'add', 'then', 'waiting', 'unit', 'answer', 'take', 'missing', 'steps', 'concepts'))
    ap.add_argument('args', nargs='*')
    ap.add_argument('--all', action='store_true', help='the answered ones too')
    ap.add_argument('--title', default='')
    ap.add_argument('--for', dest='what_for', default='', help=f'what the decision is for, {FOR_WORDS} words at most')
    ap.add_argument('--option', action='append', default=[], help='add: an option; the first is the one you would take. take: the option he chose, for an answer you had to ask him about')
    ap.add_argument('--says', default='', help=f'then: what happens when he takes the option, {CAPTION_WORDS} words at most')
    ap.add_argument('--unit', default='', help='then: the unit that is queued, a JSON file in the shape of the relay\'s queue (id, lane, goal, done_when)')
    ap.add_argument('--note', default='', help='unit, take: the note of his you read (waiting names it)')
    ap.add_argument('--queued', default='', help='take: the unit that was queued for it')
    ap.add_argument('--outcome', default='', help='take: what became of it in words, when nothing was queued')
    ap.add_argument('--out', default='', help='unit: the file the unit is written to')
    ap.add_argument('--json', action='store_true', help='waiting: as JSON')
    ap.add_argument('--board', default='', help='steps: the pipeline\'s board (default: TW_BOARD, or tw3d-board beside the checkout)')
    ap.add_argument('--dry-run', action='store_true', help='steps: write nothing, say what would be written')
    ap.add_argument('--why', default='', help='why the first option')
    ap.add_argument('--evidence', action='append', default=[], help='PATH=what it shows; a picture or a short film')
    ap.add_argument('--no-evidence', default='', help='why nothing can be shown')
    ap.add_argument('--concept', action='append', default=[], help='concepts: PATH=what it is; a picture, a short film, or a page in HTML or SVG. Yours first')
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
        elif a.what == 'then':
            if len(a.args) != 2:
                raise ValueError('then ID OPTION --says "what happens then" [--unit unit.json]')
            import notes
            u = None
            if a.unit:
                try:
                    u = json.loads(Path(a.unit).read_text(encoding='utf-8-sig'))
                except (OSError, ValueError) as e:
                    raise ValueError(f'the unit {a.unit} does not read: {e}')
            b = then(where, notes.read_all(notes.folder()), a.args[0], a.args[1], a.says, u)
            t = [o for o in b['options'] if o['key'] == a.args[1]][0]['then']
            print(f'briefs: {b["id"]} {a.args[1]} now says "Then: {t["says"]}", and {"queues " + t["unit"]["id"] if t.get("unit") else "builds nothing"}')
        elif a.what == 'waiting':
            import notes
            got = answers(read_all(where), notes.read_all(notes.folder()))
            if a.json:
                print(json.dumps(got, indent=1, sort_keys=True))
            else:
                print('\n'.join([f'briefs: {len(got)} answer{"" if len(got) == 1 else "s"} of the owner\'s nobody has taken up'] + waiting_lines(got)))
        elif a.what == 'unit':
            if len(a.args) != 1:
                raise ValueError('unit ID --note NOTE [--out unit.json]')
            import notes
            u = unit(where, notes.read_all(notes.folder()), a.args[0], a.note)
            text = json.dumps(u, indent=2, sort_keys=True) + '\n'
            if a.out:
                Path(a.out).write_text(text, encoding='utf-8', newline='\n')
                print(f'briefs: the unit {u["id"]} is in {a.out}')
            else:
                print(text, end='')
        elif a.what == 'take':
            if len(a.args) != 1:
                raise ValueError('take ID --note NOTE [--by your-branch] [--queued UNIT | --outcome "words"]')
            import notes
            b, n = take(where, notes.folder(), a.args[0], a.by, note=a.note, queued=a.queued, outcome=a.outcome, option=a.option[0] if len(a.option) == 1 else '')
            w = b['answer']
            print(f'briefs: {b["id"]} is closed with {w["option"]} ({"queued as " + w["queued"] if w.get("queued") else w.get("outcome", "")}), and his notes about it are answered')
        elif a.what == 'concepts':
            b = concepts(where, a.title, a.what_for, [(e.rsplit('=', 1) + [''])[:2] for e in a.concept], a.why, a.about, a.lane, a.by)
            print(f'briefs: {b["id"]} is written with {len(b["evidence"])} concepts to pick from, in {where}')
        elif a.what == 'steps':
            import ops
            board = Path(a.board) if a.board else ops.board_root()
            if not board or not (Path(board) / 'items').is_dir():
                raise ValueError(f'no board at {board}: name it with --board')
            new, owed = steps(where, Path(board), landed=ops.landed_items(Path(board)), write=not a.dry_run)
            print(f'briefs: {len(new)} step{"" if len(new) == 1 else "s"} {"would be" if a.dry_run else ""} put to the owner, {len(owed)} owe a capture'.replace('  ', ' '))
            for b in new:
                print(f'      {b[2]}  ({b[3]} to show)' if a.dry_run else f'      {b["id"]}  ({len(b["evidence"])} to show)')
            for i, s, w in owed:
                print(f'      owes a capture: {i} / {s}: {w}')
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
