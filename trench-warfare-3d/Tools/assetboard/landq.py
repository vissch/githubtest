#!/usr/bin/env python3
"""The landing queue: one lane at a time is put on integration's tip, tested and landed. No model is in it.

    python Tools/assetboard/landq.py                       what waits, and what was said about each (changes nothing)
    python Tools/assetboard/landq.py add LANE --why "..."  put a lane in the queue: a session's own lane goes here too
    python Tools/assetboard/landq.py offer                 put in every lane a relay unit finished that is not landed
    python Tools/assetboard/landq.py work                  take the lanes one by one until none is left to do

WHY. On 2026-10-10 a branch was tested green five times in one night and never landed: a landing needed the owner's
command on integration's exact tip, a docs lane landed nine times in between, and every landing made the others'
half-hour test worthless. Nothing held the landings in a row. And "land it" from him became a new card each time,
because no session may land. His answers that day: "Desktop lands on my click", and "Fixes land alone, look and play
wait".

So there is one queue and one worker, on the desktop, in one checkout of its own:
- a lane is taken as it stands on origin, rebased on integration's tip, tested (the full gate when Tools/land.py
  would ask for it; land.py runs the tool check or the docs check itself), and landed with Tools/land.py, which is
  not changed and decides as before. If integration moved while it was tested, it is rebased and tested again.
- which lanes need no click is read from what the rebased lane changes (klass): a script's reading, never a
  session's say-so. Today that is docs, tools, skills and tests. Code of the sim does NOT land alone: no test pins
  the hash of a played battle (SimHashTests pins the list of systems and the replay version), so a green gate does
  not show that a battle still plays out the same. In doubt a lane waits for him.
- a lane that needs his click gets a card whose id names the lane AND the commit, and whose option lands exactly
  that commit. His click counts when briefs.answers() calls it `land`: signed by the page's listener, the only note
  about the card, on that Then line, not older than 48 hours. When the lane moves on, the card of the old commit is
  closed by the queue and a new one asks.
- a lane that was red or refused is not tried again until the lane or integration has moved.
- every landing is a line in landed.jsonl with what it was landed on (alone, or which click).

The faults of the first try (lander.py, stopped after a critique of 41 in 100) and what stands against each here:
a note any session could write counted as his click (the signature); his yes bound a folder and a branch, never a
commit, and never lapsed (the tip in the Then line and in the card's id, 48 hours); land.py's "nothing to land" was
taken for a landing (a lane that rebases to nothing is `in`, and integration's tip is read back and must be the
lane's head); a timeout stopped only the parent (the gate's own process tree, then every Unity on the worker's
project); a second card could be written for one lane (briefs.then refuses it); two could run at once (a lock);
nothing showed it was alive (a beat file, written while the gate runs too).
"""
import argparse
import datetime
import json
import os
import re
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import build  # noqa: E402

GH = Path(os.environ.get('TW_STEWARD_GH') or 'C:/Users/PC/Documents/GitHub')
INTEGRATION = 'claude/trench-warfare-2d-3d-plan-idt7lf'
P = 'trench-warfare-3d/'
A = P + 'Assets/_Project/'
B = P + 'Tools/assetboard/'
# What never lands without him, whatever else the lane is: the gate and what it runs, the landing tool, this queue,
# the click and what sends or checks it, the steward, the relay and the pipeline, and the tests that hold these.
RULES = ('gate.ps1', P + 'gate.ps1', P + 'Tools/land.py', P + 'Tools/gate', P + 'Tools/toolcheck.py', P + 'Tools/selftest.py', P + 'Tools/checks/',
         P + 'validate.py', P + 'Tools/relay/', P + 'Tools/pipeline/', B + 'landq.py', B + 'notes.py', B + 'briefs.py', B + 'steward.py', B + 'found.py',
         B + 'build.py', B + 'ops.py', B + 'ideas.py', B + 'idearoute.py', B + 'static/decide.js', B + 'static/board.js', B + 'test_landq.py',
         B + 'test_steward.py', B + 'test_found.py')
ALONE_CLAUDE = ('.claude/skills/', '.claude/agents/')      # the rest of .claude (settings, hooks) is his
GATE_SECONDS = 3600         # the full gate takes 25 minutes; past this it is stopped and the lane is said red
LAND_SECONDS = 1800         # land.py runs the tool check (5 minutes) and one push
ROUNDS = 3                  # how often one lane is rebased and tested again because integration moved meanwhile
ASK_MOST = 4                # landing cards open on his page at one time: the lanes behind them wait their turn
SILENT = 7200               # a worker whose beat is older than this is taken for gone
AGAIN = 6 * 3600            # a lane that was red or refused is tried again after this long though nothing moved: a
#                             test can be red for the hour of the day (two of the board's own were, 2026-10-10)


def klass(paths):
    """(True, '') when a lane that changes these paths may land with no click of his, else (False, why). The owner,
    2026-10-10: fixes, tests, tools and docs land alone; look and play wait. Read from the paths, most cautious first:
      never alone   RULES: the gate, the landing tools, this queue, the click, the steward, the relay, the pipeline
      alone         docs; tools; skills and agents; test code (C# under Tests/, nothing else that lies there)
      else his      the code of the sim, the net and the data (until a test pins a played battle's hash, a green gate
                    does not show a battle plays out the same); everything that draws or is drawn; every data file;
                    settings and hooks; and any path this list does not know."""
    for p in paths:
        if p.startswith(RULES):
            return False, f'it changes the gate, the landing tools, the relay or the click itself ({p})'
    for p in paths:
        if p.startswith('docs/') or (p.endswith('.md') and not p.startswith((P + 'Assets/', '.claude/'))):
            continue
        if p.startswith(P + 'Tools/') or p.startswith(ALONE_CLAUDE):
            continue
        if p.startswith(A + 'Tests/') and p.endswith(('.cs', '.cs.meta')):
            if 'SimHashTests' in p:
                return False, f'it changes the pinned sim chain ({p.rsplit("/", 1)[-1]}): how a battle plays out changed'
            continue
        if p.startswith((A + 'Sim/', A + 'Net/', A + 'Data/')):
            return False, f'it changes the sim, the net code or the data, and no test pins how a battle plays out ({p})'
        return False, f'it changes what a battle looks like or how it plays, or a path the queue does not know ({p})'
    return True, ''


def code_changed(paths):
    """True when the lane changes anything of the game itself, so the full gate is owed. World.owes_gate asks
    Tools/land.py itself (is_code); this is the same rule without the files a test quotes, for where land.py is not."""
    return any((p.startswith(P) and not p.startswith(P + 'Tools/') and not p.endswith('.md')) or p == 'gate.ps1' for p in paths)


def slug(lane):
    return re.sub(r'[^a-z0-9]+', '-', lane.lower()).strip('-')


def card_id(lane, tip):
    """The id of the card that asks him to land this lane at this commit: one card a commit, never one for ever."""
    return f'land-{slug(lane)}-{tip[:8]}'


def same_folder(a, b):
    """True when two paths are one folder, however each is spelt (git prints the long name, TEMP the short one)."""
    try:
        return os.path.samefile(str(a), str(b))
    except OSError:
        return os.path.normcase(os.path.normpath(str(a))) == os.path.normcase(os.path.normpath(str(b)))


# ---- the queue: a file in the worker's folder ------------------------------------------------------------------------

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


def queue(home: Path):
    q = load(home / 'queue.json', [])
    return q if isinstance(q, list) else []


def add(home: Path, lane, why='', by='', now=None):
    """Put a lane in the queue, once. Returns (the entry, True when it is new)."""
    if not lane.startswith(('lane/sim/', 'lane/show/')):
        raise ValueError(f'{lane} is not a lane/sim or lane/show lane')
    q = queue(home)
    for e in q:
        if e['lane'] == lane:
            return e, False
    e = dict(lane=lane, why=' '.join(' '.join(str(why).split()).split()[:24]), by=by, added=f'{now or datetime.datetime.now():%Y-%m-%d %H:%M}', state='waiting', said='', tries=0, reds=0)
    put(home / 'queue.json', q + [e])
    return e, True


def keep(home: Path, e):
    put(home / 'queue.json', [e if x['lane'] == e['lane'] else x for x in queue(home)])


def drop(home: Path, lane):
    put(home / 'queue.json', [x for x in queue(home) if x['lane'] != lane])


def beat(home: Path, on=''):
    put(home / 'beat.json', dict(at=time.time(), pid=os.getpid(), on=on))


def worker_alive(home: Path):
    """True when a worker holds the lock, its process is there and it has given a sign of life lately."""
    import ideas
    rec = load(home / 'lock.json', {})
    return bool(rec.get('pid')) and rec['pid'] != os.getpid() and ideas.pid_alive(dict(pid=rec['pid'])) and time.time() - load(home / 'beat.json', {}).get('at', 0) < SILENT


def take_lock(home: Path):
    """True when this is the one worker. A lock whose process is gone, or silent for two hours, is taken over."""
    if worker_alive(home):
        return False
    put(home / 'lock.json', dict(pid=os.getpid(), since=time.strftime('%Y-%m-%d %H:%M:%S')))
    beat(home)
    return True


# ---- the outside world, so a test can stand in for the gate and for land.py -----------------------------------------

class World:
    """git in the worker's checkout, the gate, land.py and the owner's clicks. `tree` is the worker's checkout: a full
    one, its own, with a warm Unity library."""

    def __init__(self, tree: Path, home: Path):
        self.tree, self.home = Path(os.path.realpath(str(tree))), Path(home)

    def git(self, *a, cwd=None):
        r = subprocess.run(['git', *a], cwd=str(cwd or self.tree), capture_output=True, stdin=subprocess.DEVNULL)
        return r.returncode, (r.stdout + r.stderr).decode('utf-8', 'replace').strip()

    def fetch(self):
        return self.git('fetch', '-q', '--prune', 'origin')[0] == 0

    def remote(self, branch):
        out = self.git('rev-parse', '-q', '--verify', f'refs/remotes/origin/{branch}^{{commit}}')
        return out[1] if out[0] == 0 else ''

    def is_ancestor(self, a, b):
        return self.git('merge-base', '--is-ancestor', a, b)[0] == 0

    def head(self):
        return self.git('rev-parse', 'HEAD')[1]

    def held_by(self, lane):
        """The other checkout that has this lane checked out, or ''. git moves no branch another checkout stands on."""
        tree = ''
        for line in self.git('worktree', 'list', '--porcelain')[1].splitlines():
            if line.startswith('worktree '):
                tree = line[9:].strip()
            elif line.strip() == 'branch refs/heads/' + lane and not same_folder(tree, self.tree):
                return tree
        return ''

    taken = None            # (the lane the worker stands on, where its local branch stood before: '' when there was none)

    def release(self):
        """Stand on no branch between two lanes, and leave no branch of the worker's own behind: the local branch it
        rebased is put back where it stood, or removed when the worker made it. (The worker's checkout is best a
        clone of its own, so that no session's local branch is ever one of these.)"""
        self.git('rebase', '--abort')
        self.git('checkout', '-q', '--detach')
        if self.taken:
            lane, prior = self.taken
            self.git('branch', '-f', lane, prior) if prior else self.git('branch', '-D', lane)
            self.taken = None

    def take(self, lane, tip):
        """Put the worker's checkout on the lane as origin has it. (True, '') or (False, why)."""
        if self.git('status', '--porcelain')[1].strip():
            return False, 'the worker\'s checkout has uncommitted files: nothing is landed from a checkout somebody works in'
        local = self.git('rev-parse', '-q', '--verify', f'refs/heads/{lane}^{{commit}}')
        if local[0] == 0 and not self.is_ancestor(local[1], tip):
            return False, f'this machine holds commits of {lane} that are not on origin ({local[1][:8]}): push them or drop them first'
        code, out = self.git('checkout', '-q', '-B', lane, tip)
        if code == 0:
            self.taken = (lane, local[1] if local[0] == 0 else '')
        return code == 0, out[-300:]

    def rebase(self, onto):
        code, out = self.git('rebase', '-q', onto)
        if code:
            self.git('rebase', '--abort')
            hit = re.findall(r'CONFLICT[^\n]*', out)
            return False, ('; '.join(hit)[:300] if hit else out[-300:])
        return True, ''

    def changed(self, base):
        out = self.git('diff', '-z', '--name-only', '--no-renames', base, 'HEAD')[1]
        return [p for p in out.split('\0') if p]

    def owes_gate(self, paths):
        """Whether Tools/land.py would ask for a green full gate on these paths: its own rule, read from it."""
        try:
            sys.path.insert(0, str(HERE.parent))
            import land
            names = land.names_tests_read(self.tree)
            return any(land.is_code(p, names) for p in paths)
        except Exception:       # noqa: BLE001  land.py that cannot be asked: the cautious answer
            return True
        finally:
            sys.path.remove(str(HERE.parent))

    def gate(self):
        """The full gate in the worker's checkout: (green, its last words). Stopped with its whole tree past its time."""
        proj = self.tree / 'trench-warfare-3d'
        start = subprocess.run([sys.executable, 'Tools/gate_bg.py'], cwd=str(proj), capture_output=True, text=True, stdin=subprocess.DEVNULL)
        if start.returncode:
            return False, 'the gate did not start: ' + (start.stdout + start.stderr).strip()[-300:]
        p = subprocess.Popen([sys.executable, 'Tools/gate_bg.py', '--wait'], cwd=str(proj), stdout=subprocess.PIPE, stderr=subprocess.STDOUT, stdin=subprocess.DEVNULL)
        until = time.time() + GATE_SECONDS
        while p.poll() is None and time.time() < until:
            beat(self.home, 'the gate')
            time.sleep(30)
        if p.poll() is None:
            p.kill()
            self.stop_gate()
            return False, f'the gate did not end within {GATE_SECONDS // 60} minutes: it was stopped, with what it started and every Unity on the worker\'s project'
        said = p.stdout.read().decode('utf-8', 'replace').strip()
        return p.returncode == 0, ' '.join(said.splitlines()[-8:])[-700:]

    def stop_gate(self):
        """Stop the gate's own process tree (gate.ps1 writes its number beside its log), then any Unity on the project."""
        pid_file = self.git('rev-parse', '--path-format=absolute', '--git-path', 'tw-gate.pid')[1]
        try:
            pid = int(Path(pid_file).read_text(encoding='utf-8').split()[0])
            subprocess.run(['taskkill', '/PID', str(pid), '/T', '/F'], capture_output=True)
        except (OSError, ValueError, IndexError):
            pass
        want = str(self.tree / 'trench-warfare-3d').replace('/', '\\').lower()
        ask = "Get-CimInstance Win32_Process | Where-Object { $_.Name -in 'Unity.exe','unity.exe' } | ForEach-Object { '{0}|{1}' -f $_.ProcessId, $_.CommandLine }"
        out = subprocess.run(['powershell', '-NoProfile', '-Command', ask], capture_output=True, text=True, stdin=subprocess.DEVNULL).stdout
        for line in out.splitlines():
            pid, _, cmd = line.partition('|')
            if want in cmd.replace('/', '\\').lower() and pid.isdigit():
                subprocess.run(['taskkill', '/PID', pid, '/T', '/F'], capture_output=True)

    def land(self):
        """Tools/land.py in the worker's checkout, unchanged: (its exit code, its last words). Its tree is stopped past its time."""
        proj = self.tree / 'trench-warfare-3d'
        p = subprocess.Popen([sys.executable, 'Tools/land.py'], cwd=str(proj), stdout=subprocess.PIPE, stderr=subprocess.STDOUT, stdin=subprocess.DEVNULL)
        try:
            out, _ = p.communicate(timeout=LAND_SECONDS)
        except subprocess.TimeoutExpired:
            subprocess.run(['taskkill', '/PID', str(p.pid), '/T', '/F'], capture_output=True) if os.name == 'nt' else p.kill()
            return 1, f'land.py did not end within {LAND_SECONDS // 60} minutes; it was stopped with what it started'
        text = out.decode('utf-8', 'replace').strip()
        return p.returncode, ' '.join(' '.join(text.splitlines()[-3:]).split())[-500:]

    def clicks(self):
        """{lane: {brief, note, tip}} for every landing he has clicked that counts (briefs.answers go 'land')."""
        import briefs
        import notes
        out = {}
        for a in briefs.answers(briefs.read_all(briefs.folder()), notes.read_all(notes.folder())):
            if a['go'] == 'land':
                out[a['land']['lane']] = dict(brief=a['id'], note=a['note'], tip=a['land']['tip'])
        return out

    def card(self, e, tip, why, paths):
        """The card that asks him to land this lane at this commit. The cards of the lane's earlier commits are closed
        first, in the queue's own name: the commit he would say yes to on them is not the lane any more."""
        import briefs
        import notes
        where, bid = briefs.folder(), card_id(e['lane'], tip)
        for old in briefs.read_all(where):
            lands = [((o.get('then') or {}).get('land') or {}) for o in old.get('options') or []]
            if old.get('state') != 'answered' and old['id'] != bid and any(l.get('lane') == e['lane'] for l in lands):
                briefs.answer(where, old['id'], 'other', said=f'(not his answer) the lane moved on to {tip[:8]}; the card {bid} asks for that commit', by='the landing queue')
                for n in notes.read_all(notes.folder()):
                    if n.get('about') == 'brief:' + old['id'] and n.get('state') != 'done':
                        notes.answer(notes.folder(), n['id'], f'Not landed: the lane moved on after this, to {tip[:8]}. The card {bid} asks for that commit.', by='the landing queue')
        try:
            briefs.find(where, bid)
            return bid
        except ValueError:
            pass
        words = ' '.join((e.get('why') or 'A finished lane waits to land.').split()[:20])
        briefs.add(where, title=f'Land {e["lane"].split("/", 2)[-1]}?'[:90], what_for=f'{words} It needs your click: {" ".join(why.split()[:18])}.',
                   options=['Land it', 'Not now: it stays a branch'], why=f'R1 C1 DANGEROUS MAJOR. It is finished; {len(paths)} files change. The queue tests it on integration before it lands.',
                   evidence=[], no_evidence='the lane\'s own cards and captures show it; this card is the landing', lane=e['lane'], bid=bid, by='the landing queue')
        briefs.then(where, notes.read_all(notes.folder()), bid, 'A', f'The desktop tests it on integration and lands it, at {tip[:8]}', land=dict(lane=e['lane'], tip=tip))
        return bid

    def close(self, click, words):
        import briefs
        import notes
        try:
            briefs.take(briefs.folder(), notes.folder(), click['brief'], by='the landing queue', note=click['note'], outcome=words)
        except ValueError as e:
            return str(e)
        return ''


# ---- one lane ----------------------------------------------------------------------------------------------------------

def work_one(w, e, may_ask=True):
    """Take one lane as far as it goes now. Returns (state, words):
      landed        on integration, read back; words is the commit
      in            it was in integration already (as these commits, or as others that say the same)
      asked         it needs his click and the card is on his page
      red           the gate was red; words are its last
      waiting       something outside holds it up for now (another checkout has the lane, the worker's is not clean)
      refused       it cannot land as it is: it does not rebase by itself, his click was changed, land.py refused
      gone          the lane is no longer on origin
      same          it was red or refused before, and neither the lane nor integration has moved since: not tried again"""
    w.fetch()
    tip, base = w.remote(e['lane']), w.remote(INTEGRATION)
    if not base:
        return 'waiting', 'integration could not be read from origin'
    if not tip:
        return 'gone', 'the lane is no longer on origin'
    if w.is_ancestor(tip, base):
        return 'in', 'it is in integration already'
    click = w.clicks().get(e['lane'])
    if click and click['tip'] != tip:
        click = None                                # his yes was to another commit: it is no yes to this one (card() closes that card)
    if e.get('state') in ('red', 'refused') and e.get('seen') == [tip, base, bool(click)] and time.time() - e.get('seen_at', 0) < AGAIN:
        return 'same', ''
    e.update(seen=[tip, base, bool(click)], seen_at=time.time())
    holder = w.held_by(e['lane'])
    if holder:
        return 'waiting', f'{holder} has the lane checked out'
    ok, said = w.take(e['lane'], tip)
    if not ok:
        return 'waiting', said
    for _ in range(ROUNDS):
        ok, said = w.rebase(base)
        if not ok:
            return 'refused', 'it does not go on integration\'s tip by itself: ' + said
        if w.head() == base:
            return 'in', 'everything it holds is in integration already, under other commits'
        paths = w.changed(base)
        alone, why = klass(paths)
        if not click and not alone:
            if not may_ask and e.get('state') != 'asked':
                return 'waiting', f'it needs your click ({why}); its card comes when the {ASK_MOST} in front of it are answered'
            return 'asked', why + '|' + w.card(e, tip, why, paths)
        if w.owes_gate(paths):
            green, said = w.gate()
            if not green:
                return 'red', said
        w.fetch()
        if w.remote(INTEGRATION) != base:           # somebody landed past the queue while this was tested: once more
            base = w.remote(INTEGRATION)
            continue
        if w.remote(e['lane']) != tip:
            return 'waiting', 'the lane moved on origin while it was tested: it is taken again as it now stands'
        if click and w.clicks().get(e['lane']) != click:        # he wrote on the card, or took his click back, while it was tested
            return 'refused', 'your click on its card changed while it was tested: nothing was landed'
        code, said = w.land()
        head = w.head()
        w.fetch()
        if code == 0 and w.remote(INTEGRATION) == head:
            e['on'] = dict(click=click['brief'], note=click['note']) if click else dict(alone=True)
            return 'landed', head
        return 'refused', said or f'land.py ended {code} and integration is not the lane\'s head'
    return 'waiting', f'integration moved {ROUNDS} times while this lane was tested: it is taken again'


def work(w, home: Path, now=None, say=print):
    """Take the queue's lanes in their order, each as far as it goes, once each. One landing at a time by
    construction: there is one worker. A lane that falls over is said and the next one is taken. Returns
    [(lane, state, words)] of this call."""
    did, seen = [], set()
    while True:
        q = [e for e in queue(home) if e['lane'] not in seen]
        if not q:
            return did
        e = q[0]
        seen.add(e['lane'])
        beat(home, e['lane'])
        asked = sum(1 for x in queue(home) if x.get('state') == 'asked' and x['lane'] != e['lane'])
        try:
            state, words = work_one(w, e, may_ask=asked < ASK_MOST)
        except Exception as x:      # noqa: BLE001  one lane's trouble must not end the queue for the lanes behind it
            state, words = 'refused', f'the queue fell over on it ({type(x).__name__}: {x})'[:400]
        finally:
            w.release()
        if state == 'same':
            did.append((e['lane'], 'same', e.get('said', '')))
            keep(home, e)
            continue
        stamp = f'{now or datetime.datetime.now():%Y-%m-%d %H:%M}'
        e.update(state=state, said=words.split('|')[0][:700], tries=e.get('tries', 0) + 1, at=stamp)
        if state == 'asked':
            e['card'] = words.split('|')[-1]
        if state == 'red':
            e['reds'] = e.get('reds', 0) + 1
        did.append((e['lane'], state, e['said']))
        say(f'{stamp}  {state:8} {e["lane"]}: {e["said"][:300]}')
        if state == 'landed':
            line = dict(when=stamp, lane=e['lane'], head=words, on=e.get('on'), why=e.get('why', ''), by=e.get('by', ''))
            with open(home / 'landed.jsonl', 'a', encoding='utf-8') as f:
                f.write(json.dumps(line) + '\n')
            if (e.get('on') or {}).get('click'):
                w.close(dict(brief=e['on']['click'], note=e['on']['note']), f'Landed on your click, as integration {words[:8]}')
        if state in ('landed', 'in', 'gone'):
            drop(home, e['lane'])
        else:
            keep(home, e)


def offer(home: Path, board: Path, skip=('lane/show/found',)):
    """Put in the queue every lane a relay unit finished (the board's relay/done/<unit>.json names its lane). What
    has landed since drops out at its turn; what needs his click becomes a card, ASK_MOST at a time. The lane the
    looks at old work write on (found.py) is never landed. Returns the lanes that are new in the queue."""
    new = []
    for f in sorted((Path(board) / 'relay' / 'done').glob('*.json')) if (Path(board) / 'relay' / 'done').is_dir() else []:
        rec = load(f, {})
        lane = str(rec.get('lane') or '')
        if lane.startswith(('lane/sim/', 'lane/show/')) and lane not in skip and add(home, lane, why=f'The relay finished {rec.get("id") or f.stem} on it.', by='the relay')[1]:
            new.append(lane)
    return new


def lines(home: Path):
    """The queue for a person: one line a lane."""
    out = []
    for e in queue(home):
        out.append(f'{e["lane"]}: {e.get("state", "waiting")}' + (f' ({e["said"]})' if e.get('said') else '') + (f', {e["reds"]} red gates' if e.get('reds') else ''))
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description='one lane at a time is put on integration\'s tip, tested and landed')
    ap.add_argument('what', nargs='?', default='look', choices=('look', 'add', 'offer', 'work'))
    ap.add_argument('lane', nargs='?', default='')
    ap.add_argument('--why', default='', help='add: what the lane is, in a line, for the card if it needs his click')
    ap.add_argument('--by', default='', help='add: who puts it in')
    ap.add_argument('--board', default='', help='offer: the pipeline\'s board (default: tw3d-board beside the checkouts)')
    ap.add_argument('--tree', default='', help='the worker\'s checkout (default: TW_LANDQ_TREE, or githubtest-landq beside the others)')
    ap.add_argument('--home', default='', help='the queue\'s folder')
    a = ap.parse_args(argv)
    home = Path(a.home) if a.home else build.LOCAL.parent / 'landq'
    tree = Path(a.tree or os.environ.get('TW_LANDQ_TREE') or GH / 'githubtest-landq')
    try:
        if a.what == 'add':
            e, new = add(home, a.lane, a.why, a.by)
            print(f'landq: {e["lane"]} {"is in the queue now" if new else "was in the queue already"} ({len(queue(home))} waiting)')
        elif a.what == 'offer':
            new = offer(home, Path(a.board) if a.board else GH / 'tw3d-board')
            print(f'landq: {len(new)} lanes the relay finished are new in the queue ({len(queue(home))} waiting)' + ''.join(f'\n  {l}' for l in new))
        elif a.what == 'work':
            if not take_lock(home):
                print('landq: a worker is at it already')
                return 0
            try:
                w = World(tree, home)
                for lane in w.clicks():              # a lane he clicked is in the queue whether or not somebody put it there
                    add(home, lane, by='his click')
                work(w, home)
            finally:
                if load(home / 'lock.json', {}).get('pid') == os.getpid():
                    os.remove(home / 'lock.json')
        else:
            rows = lines(home)
            print('\n'.join(rows) if rows else 'landq: nothing waits to land')
    except ValueError as e:
        print(f'landq: {e}', file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
