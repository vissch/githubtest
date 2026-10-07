#!/usr/bin/env python3
"""Ideas: what could be done next, each put to the owner as a card with a picture; he says which are done.

WHY. The owner, 2026-10-07: "there should always be something to improve make better or new things to do. we have a
agent that comes up with specific tasks we can do. we dont do every tasks, the user chooses what tasks we can do,
every task should have some small visuals to already convince the user ... we need to make sure this agent is aligned
with the proper objective and doesn't suggest things already tried or rejected". And: "we usually want to have an
idea already ready for the user when he sees the screen. but also a button where multiple ideas will be generated. as
well as a typable box ... when asked for more ideas we usually present 3."

    python Tools/assetboard/ideas.py                        the open ideas
    python Tools/assetboard/ideas.py context [--json]       what the ideas agent reads first: the goals, what not to suggest, his taste, the routes
    python Tools/assetboard/ideas.py check --title "..." --pitch "..."      what an idea would be refused for, without writing it
    python Tools/assetboard/ideas.py add --title "Stretcher frogs" --pitch "..." --why-now "..." --kind unit --size M --score "R1 C3 MAJOR" \
        --sketch stretcher.svg="Two frogs, one stretcher" --capture shots/front.png="The front today" [--differs ENTRY="how it is another thing"]
    python Tools/assetboard/ideas.py fetch URL --out ref.jpg       a reference found online, as a file `add --reference` takes
    python Tools/assetboard/ideas.py take                   read what he said on the page about the ideas, and close those notes
    python Tools/assetboard/ideas.py wanted                 what he asked for that no run has answered
    python Tools/assetboard/ideas.py tick                   what the watcher does every read: take, and start a run when one is wanted

AN IDEA IS NOT A DECISION. It is a record of its own (a folder with idea.json and its pictures, in TW_IDEAS, else
the folder ideas of the Drive's TW3D-pipeline, else this station's cache), so an idea never counts in "Needs you".
It is short by rule and it shows something: add() refuses one that is not or does not.

WHAT IS NOT SUGGESTED. ledger() gathers what was decided, passed over, withdrawn, queued, built on a lane that has not
landed, or put to him as an idea before: the briefs with every option, the rows of decisions.md on integration and on
the lanes that have not landed, the relay's queue, the units handed to the master, the open lanes, the earlier ideas.
add() refuses an idea that matches one of them, unless the idea names that entry and says how it is another thing
(--differs). What he answered "Never" to is refused whatever is said. The agent reads the same ledger first
(`context`); the check is what holds when the agent did not.

HIS ANSWER. The card has three buttons. "Do it" is his yes to the route the card showed (the click carries the stamp
of that route, as a click on a brief carries the stamp of its Then line): the idea is accepted and waits for the
desktop to put it on the pipeline's board. "Not now" parks it; it may come back after PARKED_DAYS. "Never" closes it
for good, with his reason when he gave one. Words of his own about an idea are a request for a better version of it.

THE BOARD STARTS THE AGENT (the owner, 2026-10-07: "Board starts it"). The button "3 more ideas" and the box leave a
note (about ideas:more, ideas:request); `tick`, run by ops.py --watch on every read, starts one headless session with
the tw-ideas skill for the oldest request, or for one idea when none is open. One run at a time; ideas.json holds
how many a day and how much one may spend; every run is a line in spend.jsonl for the day's budget
(relay.py agents book). The session may read, search the web and run this tool; it writes nowhere else.
"""
import argparse
import datetime
import json
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import briefs     # noqa: E402
import build      # noqa: E402

TITLE_WORDS = 10
PITCH_WORDS = 35        # what it is, in at most this many words
WHY_WORDS = 30          # why now: the goal it serves
CAPTION_WORDS = 14
MOST_PICTURES = 3
SIZES = ('S', 'M', 'L')                 # a day, a few days, a week or more of the relay's work
PICTURE_KINDS = ('sketch', 'capture', 'reference', 'generated')
SCORE = re.compile(r'R([0-5]) C([0-5])((?: DANGEROUS)?(?: SYSTEMIC)?(?: MAJOR)?)\Z')
SCORED = re.compile(r'R[0-5] C[0-5]\b')
STATES = ('open', 'accepted', 'parked', 'rejected')
SHOWN_DAYS = 7          # an answered idea stays on the page this long
PARKED_DAYS = 14        # "Not now": the same idea is not brought back sooner
GOALS = 'docs/reference/goals.md'
FETCH_MAX = 8 * 2 ** 20

# The route of an accepted idea, by its kind: who works on it in which order. (role, what the card calls the step,
# whether the step is the owner's own: a decision put to him with pictures.) The roles are those of
# Tools/pipeline/roles.json; a stage template of the pipeline is made from the same table.
ROUTES = {
    'mechanic': (('game-designer', 'Game design', False), ('concept-artist', 'Concept', False), ('master', 'You pick', True), ('balance-simulator', 'Numbers', False),
                 ('ux', 'UX', False), ('ui-artist', 'UI art', False), ('master', 'You: build it?', True), ('sim', 'Sim build', False), ('lane', 'Show build', False),
                 ('hard-critic', 'Critic', False), ('master', 'Land', True)),
    'unit': (('game-designer', 'Game design', False), ('concept-artist', 'Concept', False), ('master', 'You pick', True), ('lowpoly', 'Model', False),
             ('character-simulator', 'Motion', False), ('destruction-vfx-simulator', 'Effects', False), ('hard-critic', 'Critic', False), ('master', 'Land', True)),
    'look': (('concept-artist', 'Concept', False), ('master', 'You pick', True), ('destruction-vfx-simulator', 'Build', False), ('hard-critic', 'Critic', False), ('master', 'Land', True)),
    'level': (('game-designer', 'Game design', False), ('env-simulator', 'Battlefield', False), ('master', 'You look', True), ('hard-critic', 'Critic', False), ('master', 'Land', True)),
    'interface': (('ux', 'UX', False), ('ui-artist', 'UI art', False), ('master', 'You pick', True), ('lane', 'Build', False), ('ux', 'UX review', False), ('master', 'Land', True)),
    'tool': (('lane', 'Build', False), ('master', 'Land', True)),
}
KIND_SAYS = dict(mechanic='a rule of the game', unit='a unit or a character', look='a look or an effect', level='a battlefield', interface='the HUD, a menu or the board', tool='a tool or the way we work')


def folder():
    if os.environ.get('TW_IDEAS'):
        return Path(os.environ['TW_IDEAS'])
    return build.DRIVE / 'ideas' if build.DRIVE.is_dir() else build.LOCAL / 'ideas'


words, slug, one = briefs.words, briefs.slug, briefs.one


def route_line(kind):
    return ' > '.join(says for _, says, _ in ROUTES.get(kind, ()))


def stamp(kind):
    """The route of a kind in eight characters. The page sends it with "Do it", so his yes is to the route he saw."""
    return briefs.stamp(route_line(kind))


# ---- what is not suggested -------------------------------------------------------------------------------------------

STOP = set('a an the and or of to in on for with by at from is are be it its this that as into than then when what how not no yes we you he his our their they them there here '
           'make makes made add adds new more less every each one two three all any can could should would will has have had do does done get gets so if but only also own '
           'keep keeps kept let lets use uses used still now too very about over under after before again same other first last'.split())


def keys(s):
    """The words of a line that say what it is about: lower case, without the small words, the endings cut."""
    out = set()
    for w in re.findall(r'[a-z0-9]+', str(s or '').lower()):
        if len(w) < 3 or w in STOP:
            continue
        for end in ('ing', 'ed', 'es', 's'):
            if w.endswith(end) and len(w) - len(end) >= 4:
                w = w[:-len(end)]
                break
        out.add(w)
    return out


def alike(title, pitch, e):
    """Whether an idea is the same thing as an entry of the ledger, and how much (0 when it is not). Two ways to be the
    same: the titles share most of their words; or the idea, title and pitch, has most of the words of the entry's
    title in it. One shared word is never enough: "trench" is in half the ledger. An option of a brief is held to
    the first way only: "Leave all nine as they are today" is in many a pitch and says nothing by itself."""
    a, b = keys(title), keys(e.get('title'))
    both = a & b
    if len(both) >= 2 and len(both) >= 0.6 * min(len(a), len(b)):
        return round(len(both) / min(len(a), len(b)), 2)
    wide = keys(f'{title} {pitch}') & b
    if e.get('src') != 'option' and len(b) >= 3 and len(wide) >= 3 and len(wide) >= 0.75 * len(b):
        return round(len(wide) / len(b), 2)
    return 0


def match(title, pitch, entries):
    """The entries of the ledger an idea is the same thing as, the most alike first: [(how much, entry)]."""
    hits = [(alike(title, pitch, e), e) for e in entries]
    return sorted([h for h in hits if h[0]], key=lambda h: (-h[0], h[1]['id']))


GONE = re.compile(r'\b(withdrawn|is dropped|was dropped|not chosen|parked|forget it)\b', re.I)        # how a no is worded in a row or an outcome
KEPT = re.compile(r'\b(stays? as (it is|built)|(is|are) left as (it is|they are)|nothing to build|nothing to do|nothing changes|kept, nothing)\b|^no\b', re.I)


def verdict(text):
    """What a row or an outcome says became of a thing, as the ledger calls it: he took it back, he left the game as
    it is, or it is decided. There is no field for this anywhere: it is read from the words."""
    return 'withdrawn' if GONE.search(text) else 'left as it is' if KEPT.search(text) else 'decided'


def ledger_briefs(all_briefs):
    """A brief is an entry, and so is every option of it: the one he took is decided, the others were passed over."""
    out = []
    for b in all_briefs:
        a = b.get('answer') or {}
        done = b.get('state') == 'answered'
        said = one(f'{a.get("said", "")} {a.get("outcome", "")}')
        state = 'waits on him' if not done else verdict(said)
        out.append(dict(src='brief', id=b['id'], title=b['title'], text=one(f'{b.get("what_for", "")} {said}'), state=state, when=str(a.get('when') or b.get('asked', ''))[:10]))
        for o in b.get('options', []):
            took = done and a.get('option') == o['key']
            out.append(dict(src='option', id=f'{b["id"]}#{o["key"]}', title=o['text'], text=b['title'],
                            state='waits on him' if not done else 'he took it' if took else 'not chosen', when=str(a.get('when') or b.get('asked', ''))[:10]))
    return out


def ledger_decisions(rows):
    """The rows of decisions.md (src_queue.parse): decided, or a no when the row says so in words; an open bullet waits on him."""
    out = []
    for e in rows:
        body = re.sub(r'[`*|]', '', e.get('text', ''))
        bold = [one(b) for b in re.findall(r'\*\*(.+?)\*\*', e.get('text', ''), re.S)]
        title = next((b for b in bold if not SCORED.match(b)), e['title'])          # a scored row opens with its score in bold: its headline is the next
        state = 'waits on him' if e['kind'] == 'open' else verdict(f'{title} {body[:400]}')
        out.append(dict(src='decision', id=f'{e.get("date") or "open"} {title[:60]}', title=title, text=one(body)[:300], state=state, when=e.get('date', '')))
    return out


def ledger_units(units, done=()):
    """A unit of the relay's queue, or one handed to the master: queued, or done when the queue says so."""
    return [dict(src='unit', id=u['id'], title=re.sub(r'[-_.]+', ' ', u['id']), text=one(u.get('goal', ''))[:300], state='done' if u['id'] in done else 'queued', when='') for u in units if u.get('id')]


def ledger_lanes(lanes):
    """A lane that has not landed: work in flight. `lanes` is [(branch, the subject of its last commit)]."""
    return [dict(src='lane', id=b, title=re.sub(r'[-_/]+', ' ', re.sub(r'^(origin/)?lane/[^/]+/', '', b)), text=one(s), state='in flight', when='') for b, s in lanes]


def ledger_ideas(ideas, now=None):
    """The ideas put to him before. One he parked comes off the ledger after PARKED_DAYS: "not now" is not "never"."""
    since = f'{(now or datetime.datetime.now()) - datetime.timedelta(days=PARKED_DAYS):%Y-%m-%d %H:%M}'
    out = []
    for i in ideas:
        a = i.get('answer') or {}
        if i['state'] == 'parked' and a.get('when', '') < since:
            continue
        state = dict(open='put to him, open', accepted='accepted', parked='not now', rejected='never')[i['state']]
        out.append(dict(src='idea', id=i['id'], title=i['title'], text=one(f'{i.get("pitch", "")} {a.get("said", "")}'), state=state, when=str(a.get('when') or i.get('made', ''))[:10]))
    return out


def read_units(folder_: Path):
    out = []
    for f in sorted(folder_.glob('*.json')) if folder_.is_dir() else []:
        try:
            u = json.loads(f.read_text(encoding='utf-8-sig'))
        except (OSError, ValueError):
            continue
        if isinstance(u, dict) and u.get('id'):
            out.append(u)
    return out


def board_units(board):
    """The relay's queue and what it has done, read from the board's origin/main as last fetched: this station's
    checkout of the board may be behind or another session's. Returns (units, the ids that are done)."""
    import src_git
    if not board or not Path(board).is_dir():
        return [], set()
    names = src_git.git(Path(board), 'ls-tree', '-r', '--name-only', 'origin/main', 'relay/queue', 'relay/done').split()
    units, done = [], set()
    for n in names:
        if n.startswith('relay/done/'):
            done.add(Path(n).stem)
        elif n.endswith('.json'):
            try:
                u = json.loads(src_git.git(Path(board), 'show', f'origin/main:{n}'))
            except ValueError:
                continue
            if isinstance(u, dict) and u.get('id'):
                units.append(u)
    return units, done


def ledger(where: Path = None, repo: Path = None, board=None, drive: Path = None, now=None, briefs_where: Path = None):
    """Everything an idea is held against, from where each is kept. A source that is not there on this station gives
    nothing and is named in `missing`, so a thin ledger is seen to be thin. Returns (entries, missing)."""
    import src_git
    import src_queue
    where, repo, drive = where or folder(), repo or build.REPO, drive or build.DRIVE
    out, missing = [], []
    got = briefs.read_all(briefs_where or briefs.folder())
    out += ledger_briefs(got)
    if not got:
        missing.append('the decision briefs')
    try:
        integ = src_git.INTEGRATION
        blob = src_git.git(repo, 'rev-parse', '--verify', '--quiet', f'{integ}:{src_queue.DECISIONS}').strip()
        rows = src_queue.parse(src_git.git(repo, 'show', blob)) if blob else []
        rows += src_queue.stranded(repo, integ)
        out += ledger_decisions(rows)
        if not rows:
            missing.append('decisions.md')
        lanes = [l.split('|', 1) for l in src_git.git(repo, 'branch', '-r', '--no-merged', integ, '--format=%(refname:short)|%(subject)').splitlines() if '/lane/' in l.split('|', 1)[0] and '|' in l]
        out += ledger_lanes(lanes)
    except Exception as e:      # noqa: BLE001  no git here, or a checkout without its history
        missing.append(f'decisions.md and the open lanes ({type(e).__name__})')
    try:
        if board is None:
            import ops
            board = ops.board_root()
        units, done = board_units(board)
    except Exception:           # noqa: BLE001
        units, done = [], set()
    if not units:
        missing.append('the relay\'s queue')
    out += ledger_units(units, done)
    out += ledger_units(read_units(drive / 'units-for-master'))
    out += ledger_ideas(read_all(where), now)
    return out, missing


# ---- the record ------------------------------------------------------------------------------------------------------

def check(title, pitch, why_now, kind, pictures, size, score):
    """Why an idea would be refused as a card: every reason, in words the writer can act on."""
    bad = []
    if not one(title):
        bad.append('it has no title')
    elif words(title) > TITLE_WORDS:
        bad.append(f'the title takes {words(title)} words; {TITLE_WORDS} at most')
    if not one(pitch):
        bad.append('it does not say what the idea is')
    elif words(pitch) > PITCH_WORDS:
        bad.append(f'the pitch takes {words(pitch)} words; {PITCH_WORDS} at most')
    if not one(why_now):
        bad.append('it does not say why now: the goal it serves')
    elif words(why_now) > WHY_WORDS:
        bad.append(f'why now takes {words(why_now)} words; {WHY_WORDS} at most')
    if kind not in ROUTES:
        bad.append(f'its kind is one of {", ".join(ROUTES)}: that says who works on it')
    if size not in SIZES:
        bad.append(f'its size is one of {", ".join(SIZES)}')
    m = SCORE.match(one(score))
    if not m:
        bad.append('its score reads "R2 C1", risk and change 0 to 5 as the master scores a decision, with DANGEROUS SYSTEMIC MAJOR after it when they hold')
    elif (int(m.group(1)) >= 3 or int(m.group(2)) >= 3 or 'DANGEROUS' in m.group(3) or 'SYSTEMIC' in m.group(3)) and 'MAJOR' not in m.group(3):
        bad.append('a score of R3, C3 or more, or DANGEROUS or SYSTEMIC, is MAJOR: say so')
    if not 1 <= len(pictures) <= MOST_PICTURES:
        bad.append(f'it shows {len(pictures)} pictures; 1 to {MOST_PICTURES}: an idea with nothing to look at is not put to him')
    for p in pictures:
        f = Path(p['path'])
        if p['kind'] not in PICTURE_KINDS:
            bad.append(f'the picture {f.name} is a {p["kind"]}; one of {", ".join(PICTURE_KINDS)}')
        if not f.is_file():
            bad.append(f'the picture {p["path"]} is not a file')
        elif f.suffix.lower() not in briefs.PICTURES + briefs.FILMS + briefs.PAGES:
            bad.append(f'the picture {f.name} is not a picture, a film or a page a browser can show')
        elif f.suffix.lower() in briefs.FILMS and f.stat().st_size > briefs.FILM_MAX:
            bad.append(f'the film {f.name} is over {briefs.FILM_MAX // 2 ** 20} MB')
        if not one(p.get('caption')):
            bad.append(f'the picture {f.name} has no caption: say what it shows')
        elif words(p['caption']) > CAPTION_WORDS:
            bad.append(f'the caption of {f.name} takes {words(p["caption"])} words; {CAPTION_WORDS} at most')
        if p['kind'] == 'reference' and not re.match(r'https?://', one(p.get('source'))):
            bad.append(f'the reference {f.name} does not say where it was found (its link)')
    return bad


def tried(title, pitch, entries, differs=None):
    """Why an idea would be refused as already there: every entry of the ledger it is the same thing as and does not
    answer. `differs` is {entry id: how the idea is another thing}. What he said "Never" to is not answered by words."""
    bad, differs = [], differs or {}
    for score, e in match(title, pitch, entries)[:5]:
        if e['state'] == 'never':
            bad.append(f'he said never to "{e["title"]}" ({e["id"]}): it is not put to him again')
        elif not one(differs.get(e['id'])):
            bad.append(f'it is the same thing as "{e["title"]}" ({e["state"]}; {e["src"]} {e["id"]}): drop it, or say with --differs "{e["id"]}=..." how it is another thing')
    return bad


def read_all(where: Path):
    """Every idea there is, the one made first first. A folder without an idea that reads is passed over."""
    out = []
    for f in sorted(where.glob('*/idea.json')) if where.is_dir() else []:
        try:
            i = json.loads(f.read_text(encoding='utf-8'))
        except (OSError, ValueError):
            continue
        if isinstance(i, dict) and i.get('id') == f.parent.name and i.get('state') in STATES:
            out.append(i)
    return sorted(out, key=lambda i: (i.get('made', ''), i['id']))


def save(where: Path, i):
    (where / i['id']).mkdir(parents=True, exist_ok=True)
    tmp = where / i['id'] / 'idea.json.tmp'
    tmp.write_text(json.dumps(i, indent=1, sort_keys=True) + '\n', encoding='utf-8')
    tmp.replace(where / i['id'] / 'idea.json')
    return i


def find(where: Path, iid):
    hits = [i for i in read_all(where) if i['id'] == iid or i['id'].endswith(iid)]
    if len(hits) != 1:
        raise ValueError(f'{len(hits)} ideas are called {iid}')
    return hits[0]


def add(where: Path, title, pitch, why_now, kind, pictures, size='M', score='', asked='', differs=None, entries=None, by='', run='', now=None, shooter=None):
    """Write an idea and return it. `pictures` is [dict(path, caption, kind, source)]: a sketch (a picture, or a page in
    HTML or SVG, which is photographed), a capture of the game, a reference found online (with its link), or a
    generated picture. `entries` is the ledger it is held against (None: read it here). An idea that is not short,
    shows nothing, or is already in the ledger is a ValueError that says every reason."""
    import tempfile
    pictures = [dict(p) for p in pictures]
    bad = check(title, pitch, why_now, kind, pictures, size, score)
    if entries is None:
        entries = ledger(where, now=now)[0]
    bad += tried(title, pitch, entries, differs)
    if bad:
        raise ValueError('not an idea yet: ' + '; '.join(bad))
    now = now or datetime.datetime.now()
    iid, n = f'{now:%Y-%m-%d}-{slug(title)}', 1
    while (where / iid).exists():
        n += 1
        iid = f'{now:%Y-%m-%d}-{slug(title)}-{n}'
    home, shown = where / iid, []
    with tempfile.TemporaryDirectory(prefix='tw-idea-') as tmp:
        for n, p in enumerate(pictures):
            src = Path(p['path'])
            stem = slug(src.stem)
            if src.suffix.lower() in briefs.PAGES:
                src = (shooter or briefs.shoot)(src, Path(tmp) / f'{stem}.png')
            name = f'{n + 1}-{stem}{src.suffix.lower()}'
            briefs.keep(src, home / name)
            shown.append(dict(file=name, caption=one(p['caption']), kind=p['kind'], film=src.suffix.lower() in briefs.FILMS, source=one(p.get('source')) or str(p['path'])))
    seen = [dict(id=e['id'], title=e['title'], state=e['state'], differs=one((differs or {}).get(e['id']))) for _, e in match(title, pitch, entries)[:5]]
    i = dict(id=iid, title=one(title), pitch=one(pitch), why_now=one(why_now), kind=kind, size=size, score=one(score), state='open', made=f'{now:%Y-%m-%d %H:%M}', by=one(by),
             asked=one(asked) or 'auto', run=one(run), route=[dict(role=r, says=s, own=o) for r, s, o in ROUTES[kind]], stamp=stamp(kind), pictures=shown, checked=dict(entries=len(entries), near=seen))
    return save(where, i)


def answer(where: Path, iid, state, said='', by='', now=None):
    """The owner answered: the idea is accepted, parked or rejected, with his words. An idea is answered once."""
    i = find(where, iid)
    if i['state'] != 'open':
        raise ValueError(f'{i["id"]} is {i["state"]} already ({(i.get("answer") or {}).get("when", "")})')
    if state not in STATES[1:]:
        raise ValueError(f'an answer is one of {", ".join(STATES[1:])}')
    now = now or datetime.datetime.now()
    i.update(state=state, answer=dict(said=one(said), by=one(by), when=f'{now:%Y-%m-%d %H:%M}'))
    return save(where, i)


SAYS = (('Do it', 'accepted'), ('Not now', 'parked'), ('Never', 'rejected'))      # the card's buttons, and what each makes of the idea


def said(text):
    """What a note about an idea says: (the state it asks for, his words). A button writes its own words first and
    his reason after a colon; anything else is words of his own, ('', the words)."""
    text = str(text or '').strip()
    for says, state in SAYS:
        if text == says or text.startswith(says + ':') or text.startswith(says + '\n'):
            return state, one(text[len(says):].lstrip(':'))
    return '', one(text)


def take(where: Path, notes_where: Path, now=None, by='ideas.py'):
    """Read what he said on the page about the ideas and act on it: a button's note changes the idea and is answered;
    "Do it" only with the stamp of the route the idea has now, else the note is answered with why and the idea stays
    open. Words of his own are left open: they are a request (wanted()). Returns [(idea id, what happened)]."""
    import notes
    out, ideas = [], {i['id']: i for i in read_all(where)}
    for n in notes.read_all(notes_where):
        if n.get('state') == 'done' or not str(n.get('about', '')).startswith('idea:') or n.get('from') != 'owner':
            continue
        i = ideas.get(n['about'][5:])
        state, his = said(n['text'])
        if not i:
            notes.answer(notes_where, n['id'], 'No idea of that name is there any more.', by=by, now=now)
        elif not state:
            continue
        elif i['state'] != 'open':
            notes.answer(notes_where, n['id'], f'The idea was {i["state"]} already.', by=by, now=now)
        elif state == 'accepted' and n.get('then') != i['stamp']:
            notes.answer(notes_where, n['id'], 'Its route changed after the page showed it to you. Look at it again and press once more.', by=by, now=now)
            out.append((i['id'], 'the route was not the one he saw'))
        else:
            ideas[i['id']] = answer(where, i['id'], state, his, by='owner', now=now)
            what = dict(accepted=f'Accepted. Its route: {route_line(i["kind"])}. The desktop puts it on the board next.', parked=f'Parked. It is not brought back for {PARKED_DAYS} days.',
                        rejected='Closed for good. It is not put to you again.')[state]
            notes.answer(notes_where, n['id'], what, by=by, now=now)
            out.append((i['id'], state))
    return out


def wanted(where: Path, all_notes):
    """What he asked for that no run has answered, oldest first: the button (three ideas), the box (three ideas on
    what he typed), words of his own about an idea (one better version of it). Each is {note, asked, n}."""
    out, ideas = [], {i['id']: i for i in read_all(where)}
    for n in all_notes:
        about = str(n.get('about', ''))
        if n.get('state') == 'done' or n.get('from') != 'owner':
            continue
        if about == 'ideas:more':
            out.append(dict(note=n['id'], asked='button', says='Three more ideas, each of another kind.', n=3))
        elif about == 'ideas:request':
            out.append(dict(note=n['id'], asked=one(n['text'])[:300], says=f'Three ideas on what he asked for: "{one(n["text"])[:300]}"', n=3))
        elif about.startswith('idea:') and not said(n['text'])[0] and about[5:] in ideas:
            i = ideas[about[5:]]
            out.append(dict(note=n['id'], asked=f'about "{i["title"]}": {one(n["text"])[:240]}', n=1,
                            says=f'One better version of the idea "{i["title"]}" ({i["id"]}), after what he said about it: "{one(n["text"])[:240]}". Give --differs for that idea.'))
    return out


def shown(ideas, now=None):
    """What the page lists: every open idea, and the ones answered in the last SHOWN_DAYS days."""
    since = f'{(now or datetime.datetime.now()) - datetime.timedelta(days=SHOWN_DAYS):%Y-%m-%d %H:%M}'
    return [i for i in ideas if i['state'] == 'open' or (i.get('answer') or {}).get('when', '') >= since]


def site(where: Path, out: Path, all_notes=(), status=None, now=None):
    """Put the ideas the page shows in the site: data/ideas.js, and their pictures under img/idea/<id>/. `status` is
    what tick() says: whether a run is going, and how many are left today. Returns what the page was given."""
    listed = shown(read_all(where), now)
    for i in listed:
        for p in i['pictures']:
            src, dst = where / i['id'] / p['file'], out / 'img' / 'idea' / i['id'] / p['file']
            try:
                if not dst.exists() or dst.stat().st_size != src.stat().st_size:
                    dst.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copyfile(src, dst)
                p['src'] = dst.relative_to(out).as_posix()
            except OSError:
                p['src'] = ''
    root = out / 'img' / 'idea'
    for d in root.iterdir() if root.is_dir() else []:
        if d.is_dir() and d.name not in [i['id'] for i in listed]:
            shutil.rmtree(d, ignore_errors=True)
    data = dict(ideas=listed, asked=[dict(note=w['note'], asked=w['asked'], n=w['n']) for w in wanted(where, all_notes)], says=[s for s, _ in SAYS], **(status or {}))
    text = f'window.IDEAS = {json.dumps(data, sort_keys=True)};\n'
    f = out / 'data' / 'ideas.js'
    if not f.exists() or f.read_text(encoding='utf-8') != text:
        f.parent.mkdir(parents=True, exist_ok=True)
        f.write_text(text, encoding='utf-8')
    return data


# ---- what the agent reads first --------------------------------------------------------------------------------------

def goals(repo: Path = None):
    """The goals page, or, while there is none, where the goals are written: the agent is told which."""
    repo = repo or build.REPO
    f = repo / GOALS
    if f.is_file():
        return f.read_text(encoding='utf-8')
    return ('There is no goals page yet. Read docs/00-overview.md (what the game is), docs/11-plan-review.md sections 1, 4 and 5, and the newest rows of '
            'docs/reference/decisions.md: where they disagree, the newest row wins.')


def taste(all_notes, most=40):
    """The owner's notes in his own words, newest first: what he likes and what he turned down, as he said it."""
    own = [n for n in all_notes if n.get('from') == 'owner' and not re.match(r'[A-D]: ', n.get('text', '')) and not said(n.get('text'))[0] and len(n.get('text', '')) > 12]
    return [dict(when=n.get('when', '')[:10], about=n.get('title') or n.get('about', ''), said=one(n['text'])[:400]) for n in sorted(own, key=lambda n: n.get('when', ''), reverse=True)[:most]]


def context(where: Path = None, now=None):
    """What the ideas agent reads before it proposes anything: the goals, what not to suggest, his taste, the routes."""
    import notes
    where = where or folder()
    entries, missing = ledger(where, now=now)
    return dict(goals=goals(), ledger=entries, missing=missing, taste=taste(notes.read_all(notes.folder())),
                routes={k: dict(for_=KIND_SAYS[k], route=route_line(k)) for k in ROUTES},
                limits=dict(title_words=TITLE_WORDS, pitch_words=PITCH_WORDS, why_words=WHY_WORDS, caption_words=CAPTION_WORDS, pictures=MOST_PICTURES, sizes=SIZES, kinds=PICTURE_KINDS),
                open=[dict(id=i['id'], title=i['title'], kind=i['kind']) for i in read_all(where) if i['state'] == 'open'])


def context_lines(c):
    out = ['# The goals', c['goals'].strip(), '', '# The routes: an idea\'s kind says who works on it']
    out += [f'{k:10} {v["for_"]}: {v["route"]}' for k, v in c['routes'].items()]
    out += ['', f'# Do not suggest ({len(c["ledger"])} entries; an idea like one of these is refused)']
    if c['missing']:
        out.append('NOT READ on this station, so the list is thin there: ' + '; '.join(c['missing']))
    for src in ('idea', 'brief', 'option', 'decision', 'unit', 'lane'):
        rows = [e for e in c['ledger'] if e['src'] == src]
        out += ['', f'## {dict(idea="Ideas put to him before", brief="Decision briefs", option="Options of those briefs", decision="Rows of decisions.md", unit="Units queued or done", lane="Lanes not landed")[src]} ({len(rows)})']
        out += [f'[{e["state"]}] {e["title"]}   ({e["id"]})' for e in rows]
    out += ['', f'# His taste, in his own words ({len(c["taste"])} notes, newest first)']
    out += [f'{t["when"]}  on "{t["about"]}": {t["said"]}' for t in c['taste']]
    return out


def fetch(url, dst: Path):
    """A reference found online, as a file: a picture, at most FETCH_MAX. A ValueError says why not."""
    import urllib.request
    if not re.match(r'https?://', str(url)):
        raise ValueError('a reference is found at an http or https link')
    if dst.suffix.lower() not in briefs.PICTURES:
        raise ValueError(f'a reference is kept as a picture ({", ".join(briefs.PICTURES)})')
    try:
        with urllib.request.urlopen(urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0 (tw3d ideas)'}), timeout=30) as r:
            kind, data = r.headers.get_content_type(), r.read(FETCH_MAX + 1)
    except Exception as e:      # noqa: BLE001
        raise ValueError(f'{url} did not answer: {e}')
    if not kind.startswith('image/'):
        raise ValueError(f'{url} is {kind}, not a picture: give the link of the picture itself')
    if len(data) > FETCH_MAX:
        raise ValueError(f'{url} is over {FETCH_MAX // 2 ** 20} MB')
    dst.parent.mkdir(parents=True, exist_ok=True)
    dst.write_bytes(data)
    return dst


# ---- the board starts the agent --------------------------------------------------------------------------------------

LIMITS = dict(runs_per_day=8, auto_per_day=3, auto_rest_minutes=60, usd_per_run=4.0, minutes=30, model='')       # ideas.json beside this file overrules them
RUNS = {}               # the run this process started: {'p': Popen, 'out': file}


def limits():
    try:
        got = json.loads((HERE / 'ideas.json').read_text(encoding='utf-8'))
    except (OSError, ValueError):
        got = {}
    return {k: type(v)(got.get(k, v)) for k, v in LIMITS.items()}


def spent(where: Path, day):
    """The runs of a day, from spend.jsonl: every run is a line, whatever came of it."""
    out = []
    try:
        for row in (where / 'spend.jsonl').read_text(encoding='utf-8').splitlines():
            r = json.loads(row)
            if str(r.get('when', '')).startswith(day):
                out.append(r)
    except (OSError, ValueError):
        pass
    return out


TOOL = Path(__file__).resolve().as_posix()
SKILL = (build.REPO / '.claude' / 'skills' / 'tw-ideas' / 'SKILL.md').as_posix()
IN_A_RUN = ('list', 'context', 'check', 'add', 'fetch')         # what a run may ask of this tool: never an answer of his, never another run


def prompt_for(want, run):
    return (f'You are the ideas agent of Trench Warfare 3D, started by the board. First read {SKILL} and follow it. {want["says"]} Make exactly {want["n"]}. '
            f'The tool is `python {TOOL}`: run it with that path, as written, from where you are (`python {TOOL} context` first, then `python {TOOL} add ...` for each idea). '
            'You are in your scratch folder: write sketches here and nowhere else. Stop when the ideas are written. '
            'Nobody answers a question in this session: where you are unsure, take the smaller idea.')


def command(want, run, lim, exe=None):
    """The headless session a request starts. It runs in its own scratch folder and may write there and nowhere else
    (the mode accepts edits in the folder it runs in; a write anywhere else would ask, and nobody is there to say
    yes: tried 2026-10-07, five ways, all refused). Beyond that it may read, search the web and run this tool. It
    starts no agent, and it is cut off at the run's money."""
    exe = exe or shutil.which('claude')
    if not exe:
        raise ValueError('no claude on this station\'s path: the ideas agent cannot be started here')
    allow = ['Read', 'Grep', 'Glob', 'WebSearch', 'WebFetch', f'Bash(python {TOOL} *)']
    cmd = [exe, '-p', prompt_for(want, run), '--output-format', 'json', '--permission-mode', 'acceptEdits', '--allowedTools', *allow, '--disallowedTools', 'AskUserQuestion', 'Agent',
           '--max-budget-usd', str(lim['usd_per_run'])]
    return cmd + (['--model', lim['model']] if lim['model'] else [])


def start(where: Path, want, now=None, launch=None, lim=None):
    """Start a run for one request and write down that it runs. `launch(cmd, cwd, env, out)` starts the process (the
    tests give their own). Returns the run's record."""
    now = now or datetime.datetime.now()
    lim, run = lim or limits(), f'{now:%Y%m%d-%H%M%S}'
    scratch = where / 'runs' / run
    scratch.mkdir(parents=True, exist_ok=True)
    cmd = command(want, run, lim) if launch is None else ['claude', '-p', prompt_for(want, run)]
    env = dict(os.environ, TW_IDEAS=str(where), TW_IDEAS_RUN=run, TW_IDEAS_ASKED=want['asked'], TW_IDEAS_SCRATCH=str(scratch))
    out = where / 'runs' / f'{run}.json'            # beside the scratch folder, not in it: what the session says of itself is not its to rewrite
    if launch is None:
        fh = open(out, 'wb')
        p = subprocess.Popen(cmd, cwd=str(scratch), env=env, stdin=subprocess.DEVNULL, stdout=fh, stderr=subprocess.STDOUT, creationflags=getattr(subprocess, 'CREATE_NO_WINDOW', 0))
    else:
        p = launch(cmd, str(scratch), env, out)
    RUNS.update(p=p, run=run)
    rec = dict(run=run, since=f'{now:%Y-%m-%d %H:%M:%S}', note=want.get('note', ''), asked=want['asked'], n=want['n'], pid=getattr(p, 'pid', 0))
    (where / 'running.json').write_text(json.dumps(rec, sort_keys=True) + '\n', encoding='utf-8')
    return rec


def finish(where: Path, notes_where: Path, rec, why='', now=None):
    """A run ended: write its line in spend.jsonl, answer the note that asked for it with what came of it, and forget
    the run. `why` says it did not end by itself."""
    import notes
    now = now or datetime.datetime.now()
    usd, result = 0.0, ''
    try:
        raw = (where / 'runs' / f'{rec["run"]}.json').read_text(encoding='utf-8', errors='replace')
        r = json.loads(raw[raw.index('{'):])
        usd, result = float(r.get('total_cost_usd') or 0), one(r.get('result'))[:300]
        if r.get('is_error'):
            why = why or f'the session ended on an error: {result[:160]}'
    except (OSError, ValueError):
        why = why or 'the session left no result'
    made = [i for i in read_all(where) if i.get('run') == rec['run']]
    line = dict(when=f'{now:%Y-%m-%d %H:%M}', run=rec['run'], asked=rec['asked'], usd=round(usd, 2), ideas=len(made), why=why)
    with open(where / 'spend.jsonl', 'a', encoding='utf-8') as f:
        f.write(json.dumps(line, sort_keys=True) + '\n')
    if rec.get('note'):
        titles = '; '.join(i['title'] for i in made)
        says = f'{len(made)} of the {rec["n"]} asked for: {titles}.' if made else 'No idea came of it.'
        try:
            notes.answer(notes_where, rec['note'], says + (f' ({why})' if why else ''), by='the ideas agent', now=now)
        except ValueError:
            pass                        # he closed the note himself meanwhile
    try:
        (where / 'running.json').unlink()
    except OSError:
        pass
    RUNS.clear()
    return line


def pid_alive(rec):
    """Whether the process a run was started as is still there: for a run this process did not start itself (the
    watcher was stopped and started, or `tick` was run by hand)."""
    pid = int(rec.get('pid') or 0)
    if not pid:
        return False
    if os.name == 'nt':
        import ctypes
        h = ctypes.windll.kernel32.OpenProcess(0x1000, False, pid)         # PROCESS_QUERY_LIMITED_INFORMATION
        if not h:
            return False
        code = ctypes.c_ulong()
        ok = ctypes.windll.kernel32.GetExitCodeProcess(h, ctypes.byref(code))
        ctypes.windll.kernel32.CloseHandle(h)
        return bool(ok) and code.value == 259                               # STILL_ACTIVE
    try:
        os.kill(pid, 0)
    except OSError:
        return False
    return True


def stop(rec, p=None):
    try:
        p.kill() if p is not None else os.kill(int(rec.get('pid') or 0), 9) if rec.get('pid') else None
    except OSError:
        pass


def tick(where: Path = None, notes_where: Path = None, now=None, launch=None, alive=None, lim=None):
    """What the watcher does on every read. Take what he said about the ideas; look whether the run that is going has
    ended (or has run past its minutes: then it is stopped); and when none runs and something is wanted, start one.
    Something is wanted when a request of his waits, or when no idea is open (one is made, so the screen has one
    ready), each within the day's numbers. Returns what the page is told: {running, left, last, off}."""
    import notes
    where, notes_where, now = where or folder(), notes_where or notes.folder(), now or datetime.datetime.now()
    lim, day = lim or limits(), f'{now:%Y-%m-%d}'
    took = take(where, notes_where, now=now)
    try:
        rec = json.loads((where / 'running.json').read_text(encoding='utf-8'))
    except (OSError, ValueError):
        rec = None
    last = None
    if rec:
        p = RUNS.get('p') if RUNS.get('run') == rec['run'] else None
        age = (now - datetime.datetime.strptime(rec['since'], '%Y-%m-%d %H:%M:%S')).total_seconds() / 60
        going = (p.poll() is None) if p is not None else (alive or pid_alive)(rec)      # a run this process did not start: asked of the system
        if going and age >= lim['minutes']:
            stop(rec, p) if (p is not None or alive is None) else None
            last, rec = finish(where, notes_where, rec, why=f'stopped after {lim["minutes"]} minutes', now=now), None
        elif not going:
            last, rec = finish(where, notes_where, rec, now=now), None
    today = spent(where, day)
    left = max(0, lim['runs_per_day'] - len(today))
    off = ''
    if not rec and left:
        asks = wanted(where, notes.read_all(notes_where))
        none_open = not any(i['state'] == 'open' for i in read_all(where))
        autos = [r for r in today if r.get('asked') == 'auto']
        # an unasked run that found nothing is not tried again at once: the ledger has not changed in a minute
        rest = autos and not autos[-1].get('ideas') and (now - datetime.datetime.strptime(autos[-1]['when'], '%Y-%m-%d %H:%M')).total_seconds() < 60 * lim.get('auto_rest_minutes', 0)
        want = asks[0] if asks else dict(asked='auto', says='One idea, the best you have, so the screen has one ready.', n=1) if none_open and len(autos) < lim['auto_per_day'] and not rest else None
        if want:
            try:
                rec = start(where, want, now=now, launch=launch, lim=lim)
            except (ValueError, OSError) as e:
                off = str(e)
    elif not rec:
        off = f'the day\'s {lim["runs_per_day"]} runs are used'
    left = max(0, lim['runs_per_day'] - len(spent(where, day)) - (1 if rec else 0))     # a run that is going is a line only when it ends
    return dict(running=dict(since=rec['since'], asked=rec['asked'], n=rec['n']) if rec else None, left=left, last=last, off=off, took=took)


# ---- the command -----------------------------------------------------------------------------------------------------

def lines(ideas):
    out = []
    for i in ideas:
        a = i.get('answer') or {}
        out.append(f'{i["state"]:9} {i["id"]}  ({i["made"]}, {i["kind"]}, {i["size"]}, {i["score"]}; asked: {i["asked"]})')
        out.append(f'      {i["title"]}: {i["pitch"]}')
        out.append(f'      why now: {i["why_now"]}')
        out.append(f'      route: {route_line(i["kind"])}')
        out.append(f'      shows: {", ".join(p["kind"] + " " + p["file"] for p in i["pictures"])}')
        if a:
            out.append(f'      he said {a["when"]}: {i["state"]}{" · " + a["said"] if a.get("said") else ""}')
    return out


def pictures_of(a):
    out = []
    links = list(a.source)
    for kind in PICTURE_KINDS:
        for e in getattr(a, kind):
            path, caption = (e.rsplit('=', 1) + [''])[:2]
            out.append(dict(path=path, caption=caption, kind=kind, source=links.pop(0) if kind == 'reference' and links else ''))
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description='ideas for the owner to pick from')
    ap.add_argument('what', nargs='?', default='list', choices=('list', 'add', 'check', 'context', 'answer', 'take', 'wanted', 'tick', 'fetch'))
    ap.add_argument('args', nargs='*')
    ap.add_argument('--all', action='store_true', help='the answered ones too')
    ap.add_argument('--json', action='store_true')
    ap.add_argument('--title', default='', help=f'{TITLE_WORDS} words at most')
    ap.add_argument('--pitch', default='', help=f'what the idea is, {PITCH_WORDS} words at most')
    ap.add_argument('--why-now', default='', help=f'the goal it serves, {WHY_WORDS} words at most')
    ap.add_argument('--kind', default='', help='who works on it: ' + ', '.join(ROUTES))
    ap.add_argument('--size', default='M', help='S a day, M a few days, L a week or more')
    ap.add_argument('--score', default='', help='"R1 C2", risk and change as the master scores a decision')
    ap.add_argument('--sketch', action='append', default=[], help='PATH=what it shows; a picture, or a page in HTML or SVG (photographed)')
    ap.add_argument('--capture', action='append', default=[], help='PATH=what it shows; a capture of the game as it is')
    ap.add_argument('--reference', action='append', default=[], help='PATH=what it shows; a picture found online (fetch), with --source')
    ap.add_argument('--generated', action='append', default=[], help='PATH=what it shows; a generated picture')
    ap.add_argument('--source', action='append', default=[], help='the link of a --reference, in their order')
    ap.add_argument('--differs', action='append', default=[], help='ENTRY=how the idea is another thing than that entry of the ledger')
    ap.add_argument('--asked', default=os.environ.get('TW_IDEAS_ASKED', ''), help='what was asked for (the run says it)')
    ap.add_argument('--run', default=os.environ.get('TW_IDEAS_RUN', ''))
    ap.add_argument('--by', default='')
    ap.add_argument('--out', default='', help='fetch: the file the picture is kept as')
    a = ap.parse_args(argv)
    where = folder()
    try:
        if os.environ.get('TW_IDEAS_RUN') and a.what not in IN_A_RUN:
            raise ValueError(f'{a.what} is not for a run the board started: a run reads the context and adds ideas ({", ".join(IN_A_RUN)})')
        if a.what in ('add', 'check'):
            differs = dict((d.split('=', 1) + [''])[:2] for d in a.differs)
            if a.what == 'check':
                bad = tried(a.title, a.pitch, ledger(where)[0], differs)
                print('ideas: nothing in the ledger is the same thing' if not bad else 'ideas: ' + '\n       '.join(bad))
                return 1 if bad else 0
            i = add(where, a.title, a.pitch, a.why_now, a.kind, pictures_of(a), a.size, a.score, a.asked, differs, by=a.by or 'the ideas agent', run=a.run)
            print(f'ideas: {i["id"]} is written, in {where}; its route: {route_line(i["kind"])}')
        elif a.what == 'context':
            c = context(where)
            print(json.dumps(c, indent=1, sort_keys=True) if a.json else '\n'.join(context_lines(c)))
        elif a.what == 'answer':
            if len(a.args) < 2:
                raise ValueError('answer ID accepted|parked|rejected ["his words"]')
            i = answer(where, a.args[0], a.args[1], ' '.join(a.args[2:]), a.by)
            print(f'ideas: {i["id"]} is {i["state"]}')
        elif a.what == 'take':
            import notes
            got = take(where, notes.folder())
            print('\n'.join([f'ideas: {len(got)} of his answers taken'] + [f'      {i}: {w}' for i, w in got]))
        elif a.what == 'wanted':
            import notes
            got = wanted(where, notes.read_all(notes.folder()))
            print('\n'.join([f'ideas: {len(got)} asked for'] + [f'      {w["n"]} · {w["asked"]}   (note {w["note"]})' for w in got]))
        elif a.what == 'tick':
            s = tick(where)
            print(f'ideas: {"a run is going since " + s["running"]["since"] + " (" + s["running"]["asked"] + ")" if s["running"] else "no run"}; {s["left"]} left today{"; " + s["off"] if s["off"] else ""}')
        elif a.what == 'fetch':
            if len(a.args) != 1 or not a.out:
                raise ValueError('fetch URL --out FILE.jpg')
            scratch = os.environ.get('TW_IDEAS_SCRATCH')
            if scratch and Path(scratch).resolve() not in Path(a.out).resolve().parents:
                raise ValueError(f'in a run a reference is kept in the run\'s own folder, {scratch}')
            print(f'ideas: the reference is in {fetch(a.args[0], Path(a.out))}; give it with --reference and its link with --source')
        else:
            every = read_all(where)
            some = every if a.all else [i for i in every if i['state'] == 'open']
            print(f'ideas: {len(some)} {"in all" if a.all else "open"} in {where}')
            print('\n'.join(lines(some)))
    except ValueError as e:
        print(f'ideas: {e}')
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
