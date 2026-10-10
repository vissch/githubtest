#!/usr/bin/env python3
"""Found tasks: old work is looked at in turn, and what a look finds becomes tasks. No model is in this file.

    python Tools/assetboard/found.py                    what would be looked at next, and what waits (changes nothing)
    python Tools/assetboard/found.py rota               queue the next look, when none is queued and the fixes have caught up
    python Tools/assetboard/found.py take               take up what finished looks wrote: queue the small tasks, card the major ones

WHY. The owner, 2026-10-10: the loop "should run by itslef, find tasks. and critique previous old work to get more
tasks". Until then the relay only worked what somebody had queued. His answers that day: 15 percent of the day's
tokens go to looking at old work, and of what a look finds "small ones yes, big ones are a card".

A LOOK is an ordinary unit on the relay's queue, of the kind `critique` (so it is paid from that share): one leg
reads one subject and writes docs/found/<unit>.json on the lane lane/show/found, and nothing else. This file decides
which subject is next (rota) and what becomes of what was written (take); the runner runs both between two units
(steward.py puts them in the card round).

Subjects, in turn, never the same one twice unless it changed since it was looked at:
  code      a folder of the game's code, as it stands on integration
  look      a view of the game, from fresh captures (LOOKS)
  branch    a branch on origin that is not in integration: finish it, close it, or what is missing
  tools     the board's tools and the skills, seldom
What a look may hand in is five tasks at most. For each one this file, not the look, decides (route):
  dropped   it names a file integration does not have, says nothing checkable, or was found before
  queued    it is small: it goes on the relay's queue as a fix, with a done-when this file writes itself (the
            lane holds a commit that names the unit): no command a model wrote is ever run to check it
  a card    it is major by the score of decisions.md (risk 3 or more, change 3 or more, hard to undo, can break the
            sim or the gate, changes a rule agents follow or a file format), or it touches the sim, the net code,
            the data, the gate, the landing tools, the relay or the pipeline whatever its score says
No new look is queued while FOUND_MOST found tasks wait unstarted: the queue cannot grow without bound.
"""
import argparse
import datetime
import hashlib
import json
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import build  # noqa: E402

GH = Path(os.environ.get('TW_STEWARD_GH') or 'C:/Users/PC/Documents/GitHub')
RELAY = GH / 'githubtest-relay-run' / 'trench-warfare-3d' / 'Tools' / 'relay' / 'relay.py'
BOARD = GH / 'tw3d-board'
INTEGRATION = 'claude/trench-warfare-2d-3d-plan-idt7lf'
LANE = 'lane/show/found'            # where a look writes; it is never landed
P = 'trench-warfare-3d/'
A = P + 'Assets/_Project/'
FOUND_MOST = 20                     # found tasks waiting unstarted: at this many no new look is queued
TASKS_MOST = 5                      # tasks one look may hand in
WEIGHTS = dict(code=4, look=3, branch=2, tools=1)       # how often each kind of subject has its turn
CODE = (A + 'Sim', A + 'Net', A + 'Data', A + 'Presentation', A + 'UI', A + 'Editor')
LOOKS = (('every-unit-close', 'every unit of the Proving Ground at close range: silhouette, colour, motion, how it reads against the ground'),
         ('first-assault', 'a match on the default field, the first assault across no man\'s land: can a player tell who is winning and why'),
         ('trench-fight', 'a trench being taken: melee, grenades, bodies, the moment it changes hands'),
         ('vehicles', 'tanks and walkers on the move and under fire: weight, tracks and legs, wrecks'),
         ('night', 'the night look: can units, lights and effects be told apart'),
         ('hud', 'the HUD in a match: what a player must read at a glance, and what gets in the way'))
TOOLS = ((P + 'Tools/assetboard', 'the board\'s tools'), (P + 'Tools/pipeline', 'the pipeline'), ('.claude/skills', 'the skills agents follow'))
NOT_BRANCHES = ('lane/show/relay', 'lane/show/pipe-idea-', 'lane/sim/pipe-idea-', 'lane/show/landing-', 'lane/sim/landing-', LANE, 'backup/', 'play/')
ALWAYS_HIS = (A + 'Sim/', A + 'Net/', A + 'Data/', P + 'Tools/land.py', P + 'gate.ps1', P + 'Tools/gate', P + 'Tools/relay/', P + 'Tools/pipeline/',
              P + 'Tools/assetboard/landq.py', P + 'Tools/assetboard/notes.py', P + 'Tools/assetboard/briefs.py', P + 'Tools/assetboard/steward.py',
              P + 'Tools/assetboard/found.py', P + 'Tools/toolcheck.py', P + 'validate.py')
ROLES = ('lane', 'review-fix', 'destruction-vfx-simulator', 'character-simulator', 'vehicle-simulator', 'optimizer', 'balance-simulator', 'env-simulator', 'bug-catcher')
ROLE_OF_LOOK = dict(code='bug-catcher', look='hard-critic', branch='lane', tools='lane')
RISK = ('It is hard to undo: landed, deleted, pushed over, sent out', 'It can break the sim, replays or the gate, or stop other agents\' work',
        'It reaches outside its lane: the board, the Drive, another checkout or machine', 'It costs over 2% of the week',
        'No script or test can check the result')
CHANGE = ('It touches more than one lane or unit', 'It changes a rule, tool, skill or prompt that agents follow from now on',
          'It changes what the player sees or how the game plays', 'It changes a file format, the replay version or a shared name',
          'It overturns an earlier row in decisions.md')
HOW = dict(
    code='Read the code under {path} as it stands on integration, and its tests. Look for what is wrong or will go wrong: a bug, a test that cannot fail, state that is not hashed, work done every frame that need not be, code two places copy. Reread a line before you report it.',
    look='Take fresh captures with the CaptureRig of: {path}. Judge them as the tw-critic skill does. Say what a player cannot read, what looks unfinished or wrong, what breaks the look the game has.',
    branch='Read the branch {path} against integration (git log and git diff origin/' + INTEGRATION + '...origin/{path}). Say which it is: finished and worth landing, dead and to be closed, or missing something named. One task at most: what would finish it, or "close it" with why.',
    tools='Read {path}. Look for what makes agents do the wrong thing or spend tokens for nothing: a rule two files state differently, a tool whose check cannot fail, a step done by hand every time.')
GOAL = ('LOOK, do not build. {how}\n'
        'Write ONE file, docs/found/{id}.json, and nothing else. Its shape:\n'
        '{{"subject": "{subject}", "looked_at": ["each file or capture you really read"], "tasks": [ ... ]}}\n'
        'At most {most} tasks, the worst first; an empty list when you found nothing worth a unit of work (that is a good answer). Each task:\n'
        '{{"title": "what to do, 12 words at most", "why": "what is wrong and what it costs, with the line or the frame", "files": ["repo paths it touches"], '
        '"lane": "sim" or "show", "role": one of {roles}, "done_when": "how a script or test proves it", "risk": [five true/false], "change": [five true/false]}}\n'
        'risk, in this order: {risk}.\nchange, in this order: {change}.\n'
        'Answer each of the ten honestly: a task that is scored too low is found out when it is built. Do not report what is already queued, decided or rejected '
        '(docs/reference/tasks.md and decisions.md). Commit the file with [{id}] in the message and push the lane.')


def slug(s, n=48):
    return re.sub(r'[^a-z0-9]+', '-', str(s).lower()).strip('-')[:n].strip('-')


def one(s):
    return ' '.join(str(s or '').split())


def load(f: Path, default):
    try:
        return json.loads(f.read_text(encoding='utf-8'))
    except (OSError, ValueError):
        return default


def put(f: Path, data):
    f.parent.mkdir(parents=True, exist_ok=True)
    tmp = f.with_name(f.name + '.tmp')
    tmp.write_text(json.dumps(data, indent=1), encoding='utf-8', newline='\n')
    os.replace(tmp, f)


# ---- whose turn it is --------------------------------------------------------------------------------------------------

def pick(subjects, state):
    """The subject to look at next, or None. `subjects` is [{id, kind, path, at}] (`at`: the commit it stands at now),
    `state` {id: {at, looked}} what was looked at before. Due is what was never looked at or has changed since. Of
    the kinds with something due, the one that has had the fewest turns for its weight; of that kind the one never
    looked at, else the one looked at longest ago."""
    due = [s for s in subjects if (state.get(s['id']) or {}).get('at') != s['at']]
    if not due:
        return None
    turns = {k: sum(1 for i, v in state.items() if i.split(':', 1)[0] == k and v.get('looked')) for k in WEIGHTS}
    kind = min({s['kind'] for s in due}, key=lambda k: (turns.get(k, 0) / WEIGHTS[k], -WEIGHTS[k]))
    return min((s for s in due if s['kind'] == kind), key=lambda s: ((state.get(s['id']) or {}).get('looked') or '', s['id']))


def unit_of(subject, day):
    """The queue file of a look at this subject."""
    uid = f'look-{subject["kind"]}-{slug(subject["id"].split(":", 1)[1], 40)}-{day}'
    goal = GOAL.format(how=HOW[subject['kind']].format(path=subject['path']), id=uid, subject=subject['id'], most=TASKS_MOST, roles=', '.join(ROLES),
                       risk='; '.join(f'{i + 1}. {r}' for i, r in enumerate(RISK)), change='; '.join(f'{i + 1}. {c}' for i, c in enumerate(CHANGE)))
    check = ("import json,subprocess,sys;d=json.load(open('docs/found/%s.json',encoding='utf-8'));"
             "o=subprocess.run(['git','log','--format=%%B','origin/%s..HEAD'],capture_output=True).stdout.decode('utf-8','replace');"
             "sys.exit(0 if isinstance(d.get('tasks'),list) and d.get('subject') and '[%s]' in o else 1)" % (uid, INTEGRATION, uid))
    return dict(id=uid, lane=LANE, role=ROLE_OF_LOOK[subject['kind']], kind='critique', goal=goal, done_when=['python', '-c', check])


# ---- what a look found ---------------------------------------------------------------------------------------------------

def check_task(t, exists):
    """Why a task a look handed in is not taken, or ''. `exists(path)` says whether integration has that path."""
    if not isinstance(t, dict):
        return 'it is not a task'
    if not one(t.get('title')) or len(one(t.get('title')).split()) > 16:
        return 'its title is missing or longer than 16 words'
    if len(one(t.get('why'))) < 30:
        return 'it does not say what is wrong'
    files = t.get('files')
    if not isinstance(files, list) or not files or not all(isinstance(f, str) and f for f in files):
        return 'it names no file'
    gone = [f for f in files if not exists(f)]
    if gone:
        return f'it names a file integration does not have ({gone[0]})'
    for k in ('risk', 'change'):
        if not (isinstance(t.get(k), list) and len(t[k]) == 5 and all(isinstance(x, bool) for x in t[k])):
            return f'its {k} is not five true/false answers'
    if not one(t.get('done_when')):
        return 'it does not say how it is proved'
    return ''


def score(t):
    """(R, C, marks, major) as decisions.md scores a decision, with this file's own rule on top: what touches the sim,
    the net code, the data, the gate, the landing tools, the relay or the pipeline is major whatever was answered."""
    r, c = sum(t['risk']), sum(t['change'])
    marks = (['DANGEROUS'] if t['risk'][0] or t['risk'][1] else []) + (['SYSTEMIC'] if t['change'][1] or t['change'][3] else [])
    his = [f for f in t['files'] if f.startswith(ALWAYS_HIS)]
    major = r >= 3 or c >= 3 or bool(marks) or bool(his)
    return r, c, marks, major, (his[0] if his else '')


def key_of(t):
    """What makes two findings the same finding: the first file and the words of the title, whatever their order."""
    words = sorted(set(w for w in re.findall(r'[a-z0-9]+', one(t.get('title')).lower()) if len(w) > 3))
    return hashlib.sha1((sorted(t.get('files') or [''])[0] + '|' + ' '.join(words)).encode('utf-8')).hexdigest()[:12]


def same(a, b):
    """True when two titles say the same thing: most of the shorter one's words are in the other."""
    wa, wb = (set(w for w in re.findall(r'[a-z0-9]+', one(x).lower()) if len(w) > 3) for x in (a, b))
    return bool(wa and wb) and len(wa & wb) / min(len(wa), len(wb)) >= 0.7


def fix_unit(t, look, n):
    """The queue file of a small found task. Its done-when is written here: the lane holds a commit that names it."""
    uid = f'found-{slug(t["title"], 44)}-{look[-8:]}-{n}'
    sim = t.get('lane') == 'sim' or any(f.startswith((A + 'Sim/', A + 'Net/', A + 'Data/')) for f in t['files'])     # whatever the look called it
    lane = f'lane/{"sim" if sim else "show"}/{uid}'
    goal = (f'{one(t["title"])}. {one(t["why"])} Files: {", ".join(t["files"][:6])}. Proved by: {one(t["done_when"])}. '
            f'(Found by the look {look}.) Commit with [{uid}] in the message.')
    check = ("import subprocess,sys;o=subprocess.run(['git','log','--format=%%B','origin/%s..HEAD'],capture_output=True).stdout.decode('utf-8','replace');"
             "sys.exit(0 if '[%s]' in o else 1)" % (INTEGRATION, uid))
    return dict(id=uid, lane=lane, role=t.get('role') if t.get('role') in ROLES else 'lane', kind='fix', goal=goal[:3000], done_when=['python', '-c', check])


def route(found, look, exists, known):
    """What becomes of each task of one look: [(task, 'queue' | 'card' | 'dropped', words, score)]. `known` is the
    titles already queued, carded or taken before: [(key, title)]."""
    out, seen = [], list(known)
    for t in (found.get('tasks') or [])[:TASKS_MOST]:
        bad = check_task(t, exists)
        if not bad and any(k == key_of(t) or same(title, t['title']) for k, title in seen):
            bad = 'it was found before, or is queued already'
        if bad:
            out.append((t, 'dropped', bad, None))
            continue
        seen.append((key_of(t), one(t['title'])))
        r, c, marks, major, his = score(t)
        out.append((t, 'card' if major else 'queue', (f'it touches {his}' if his and r < 3 and c < 3 and not marks else ''), (r, c, marks)))
    for t in (found.get('tasks') or [])[TASKS_MOST:]:
        out.append((t, 'dropped', f'a look hands in {TASKS_MOST} tasks at most', None))
    return out


# ---- the outside world -----------------------------------------------------------------------------------------------------

class World:
    def __init__(self, repo: Path, board: Path, home: Path):
        self.repo, self.board, self.home = Path(repo), Path(board), Path(home)

    def git(self, *a):
        r = subprocess.run(['git', '-C', str(self.repo), *a], capture_output=True, stdin=subprocess.DEVNULL)
        return r.returncode, (r.stdout + r.stderr).decode('utf-8', 'replace').strip()

    def subjects(self):
        self.git('fetch', '-q', '--prune', 'origin')
        tip, out = 'origin/' + INTEGRATION, []
        for top in CODE:
            for d in self.git('ls-tree', '-d', '--name-only', tip, top + '/')[1].splitlines():
                out.append(dict(id='code:' + d[len(A):], kind='code', path=d, at=self.git('log', '-1', '--format=%H', tip, '--', d)[1]))
        head = self.git('rev-parse', tip)[1]
        week = datetime.date.today().isocalendar()
        for name, what in LOOKS:                       # a view is due again once a week, when integration moved
            out.append(dict(id='look:' + name, kind='look', path=what, at=f'{week[0]}w{week[1]}' if head else ''))
        for line in self.git('for-each-ref', '--no-merged', tip, '--format=%(objectname) %(refname:short)', 'refs/remotes/origin/lane')[1].splitlines():
            sha, _, ref = line.partition(' ')
            name = ref[len('origin/'):]
            if not name.startswith(NOT_BRANCHES):
                out.append(dict(id='branch:' + name, kind='branch', path=name, at=sha))
        for path, what in TOOLS:
            out.append(dict(id='tools:' + path, kind='tools', path=f'{path} ({what})', at=self.git('log', '-1', '--format=%H', tip, '--', path)[1]))
        return [s for s in out if s['at']]

    def exists(self, path):
        return self.git('cat-file', '-e', f'origin/{INTEGRATION}:{path}')[0] == 0

    def read_found(self, uid):
        self.git('fetch', '-q', 'origin', LANE)
        code, out = self.git('show', f'origin/{LANE}:docs/found/{uid}.json')
        if code:
            return None
        try:
            d = json.loads(out)
        except ValueError:
            return None
        return d if isinstance(d, dict) else None

    def queued(self):
        """{unit id: the unit} of the relay's queue, and the ids that are done."""
        q = {f.stem: load(f, {}) for f in (self.board / 'relay' / 'queue').glob('*.json')} if (self.board / 'relay' / 'queue').is_dir() else {}
        done = {f.stem for f in (self.board / 'relay' / 'done').glob('*.json')} if (self.board / 'relay' / 'done').is_dir() else set()
        return q, done

    def add(self, unit):
        """Put a unit on the relay's queue the relay's own way: (ok, what it said)."""
        f = Path(tempfile.mkdtemp(prefix='tw-found-')) / (unit['id'] + '.json')
        f.write_text(json.dumps(unit, indent=1), encoding='utf-8')
        r = subprocess.run([sys.executable, str(RELAY), 'add', '--unit', str(f)], capture_output=True, text=True, stdin=subprocess.DEVNULL,
                           encoding='utf-8', errors='replace', env=dict(os.environ, TW_BOARD=str(self.board)))
        said = (r.stdout + r.stderr).strip()
        return r.returncode == 0, said.splitlines()[-1][:300] if said else ''

    def card(self, t, look, sc, why_his):
        """A major found task as one card for him, whose first option queues it."""
        import briefs
        import notes
        r, c, marks = sc
        unit = fix_unit(t, look, 0)
        b = briefs.add(briefs.folder(), title=one(t['title'])[:90], what_for=' '.join(one(t['why']).split()[:44]),
                       options=['Do it', 'Not this'], why=' '.join((f'R{r} C{c} {" ".join(marks)} MAJOR. A look at old work found it' + (f'; {why_his}' if why_his else '') + '.').split()[:34]),
                       no_evidence=f'the look {look} names the lines: {", ".join(t["files"][:2])}'[:200], lane=unit['lane'], by='found.py')
        briefs.then(briefs.folder(), notes.read_all(notes.folder()), b['id'], 'A', 'It goes on the relay\'s queue as a fix', {k: v for k, v in unit.items() if k != 'kind'})
        return b['id']


# ---- the two steps ---------------------------------------------------------------------------------------------------------

def waiting(w, taken):
    """How many found tasks are queued and not started or done yet."""
    q, done = w.queued()
    return sum(1 for t in taken if t.get('went') == 'queue' and t.get('unit') in q and t['unit'] not in done)


def rota(w, now=None):
    """Queue the next look, or say why not. Returns a line."""
    state, taken = load(w.home / 'state.json', {}), load(w.home / 'taken.json', [])
    q, done = w.queued()
    open_looks = [u for u, v in q.items() if v.get('kind') == 'critique' and u not in done]
    if open_looks:
        return f'found: the look {open_looks[0]} is queued and not done: no second one beside it'
    n = waiting(w, taken)
    if n >= FOUND_MOST:
        return f'found: {n} found tasks wait unstarted (the most is {FOUND_MOST}): no new look until the fixes catch up'
    s = pick(w.subjects(), state)
    if not s:
        return 'found: every subject was looked at as it stands now: nothing is due'
    unit = unit_of(s, f'{now or datetime.datetime.now():%Y%m%d}')
    ok, said = w.add(unit)
    if not ok:
        return f'found: the look at {s["id"]} was not queued: {said}'
    state[s['id']] = dict(state.get(s['id']) or {}, at=s['at'], unit=unit['id'], queued=f'{now or datetime.datetime.now():%Y-%m-%d %H:%M}')
    put(w.home / 'state.json', state)
    return f'found: queued the look {unit["id"]} ({s["id"]})'


def take(w, now=None):
    """Take up every look that is done and not taken yet. Returns lines."""
    state, taken, out = load(w.home / 'state.json', {}), load(w.home / 'taken.json', []), []
    q, done = w.queued()
    stamp = f'{now or datetime.datetime.now():%Y-%m-%d %H:%M}'
    for sid, st in sorted(state.items()):
        uid = st.get('unit')
        if not uid or st.get('looked') or uid not in done:
            continue
        found = w.read_found(uid)
        if found is None:
            st.update(looked=stamp, found=0, said='the look left no file that reads')
            out.append(f'found: {uid} is done and left no file that reads: nothing taken')
            continue
        known = [(t.get('key', ''), t.get('title', '')) for t in taken] + [('', one(v.get('goal'))[:120]) for v in q.values() if v.get('kind') != 'critique']
        counts = dict(queue=0, card=0, dropped=0)
        for n, (t, went, words, sc) in enumerate(route(found, uid, w.exists, known)):
            rec = dict(when=stamp, look=uid, title=one(t.get('title') if isinstance(t, dict) else '')[:160], key=key_of(t) if isinstance(t, dict) and sc is not None else '', went=went, words=words)
            if went == 'queue':
                unit = fix_unit(t, uid, n)
                ok, said = w.add(unit)
                rec.update(unit=unit['id'], score=f'R{sc[0]} C{sc[1]}') if ok else rec.update(went='dropped', words='the relay refused it: ' + said)
            elif went == 'card':
                try:
                    rec.update(brief=w.card(t, uid, sc, words), score=f'R{sc[0]} C{sc[1]} {" ".join(sc[2])} MAJOR'.replace('  ', ' '))
                except ValueError as e:
                    rec.update(went='dropped', words=f'its card was refused: {e}'[:300])
            counts[rec['went']] += 1
            taken.append(rec)
        st.update(looked=stamp, found=len(found.get('tasks') or []), **counts)
        out.append(f'found: {uid}: {len(found.get("tasks") or [])} tasks handed in, {counts["queue"]} queued, {counts["card"]} are cards for him, {counts["dropped"]} dropped')
    put(w.home / 'state.json', state)
    put(w.home / 'taken.json', taken)
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description='old work is looked at in turn, and what a look finds becomes tasks')
    ap.add_argument('what', nargs='?', default='look', choices=('look', 'rota', 'take'))
    ap.add_argument('--board', default='', help='the pipeline\'s board (default: tw3d-board beside the checkouts)')
    ap.add_argument('--home', default='', help='its own folder (what was looked at, what was taken)')
    a = ap.parse_args(argv)
    w = World(build.REPO, Path(a.board) if a.board else BOARD, Path(a.home) if a.home else build.LOCAL.parent / 'found')
    if a.what == 'rota':
        print(rota(w))
    elif a.what == 'take':
        print('\n'.join(take(w)) or 'found: no finished look waits to be taken up')
    else:
        state, taken = load(w.home / 'state.json', {}), load(w.home / 'taken.json', [])
        subjects = w.subjects()
        s = pick(subjects, state)
        kinds = {k: sum(1 for x in subjects if x['kind'] == k) for k in WEIGHTS}
        print(f'found: {len(subjects)} subjects ({", ".join(f"{n} {k}" for k, n in kinds.items())}), {sum(1 for v in state.values() if v.get("looked"))} looked at; '
              f'{waiting(w, taken)} found tasks wait unstarted (the most is {FOUND_MOST})')
        print(f'next: {s["id"]}' if s else 'next: nothing is due')
    return 0


if __name__ == '__main__':
    sys.exit(main())
