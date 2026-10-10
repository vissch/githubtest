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
wait": bug fixes, tests, tools and docs land by themselves once the full gate is green; what changes how a battle
looks or plays waits for his click.

So there is one queue and one worker, on the desktop, in one checkout of its own:
- a lane is taken as it stands on origin, rebased on integration's tip, tested (the full gate when it changes code;
  Tools/land.py runs the tool check or the docs check itself), and landed with Tools/land.py, which is not changed
  and decides as before. If integration moved while it was tested, it is rebased and tested again, by itself.
- which lanes need no click is read from what the rebased lane changes (klass): a script's reading, never a
  session's say-so. In doubt it waits for him.
- a lane that needs his click gets ONE card (its id is the lane's, so it is asked once) whose option names the lane
  and its commit. His click counts when briefs.answers() calls it `land`: signed by the page's listener, on that
  Then line, not older than 48 hours. A lane that moved on after his click is not landed on it.
- every landing is a line in landed.jsonl with what it was landed on (alone, or which click).

The faults of the first try (lander.py, stopped after a critique of 41 in 100) and what stands against each here:
a note any session could write counted as his click (the signature); his yes bound a folder and a branch, never a
commit, and never lapsed (the tip in the Then line, 48 hours); land.py's "nothing to land" was taken for a landing
(integration's tip is read back and must be the lane's head); a timeout stopped only the parent (the tree, and any
Unity on the worker's project); a second card could be written for one lane (briefs.then refuses it); two could run
at once (a lock); nothing showed it was alive (a beat file).
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
# What never lands without him, whatever else the lane is: the gate, the landing tool, this queue and the click.
RULES = (P + 'Tools/land.py', P + 'gate.ps1', P + 'Tools/gate', P + 'Tools/toolcheck.py', P + 'validate.py', P + 'Tools/relay/',
         P + 'Tools/pipeline/', P + 'Tools/assetboard/landq.py', P + 'Tools/assetboard/notes.py', P + 'Tools/assetboard/briefs.py',
         P + 'Tools/assetboard/steward.py')
SIM = (A + 'Sim/', A + 'Net/', A + 'Data/')
PINNED = 'SimHashTests'
GATE_SECONDS = 3600         # the full gate takes 25 minutes; past this it is stopped and the lane is said red
LAND_SECONDS = 1800         # land.py runs the tool check (5 minutes) and one push
ROUNDS = 3                  # how often one lane is rebased and tested again because integration moved meanwhile
ASK_MOST = 4                # landing cards open on his page at one time: the lanes behind them wait their turn


def klass(paths):
    """(True, '') when a lane that changes these paths may land with no click of his, else (False, why). The owner,
    2026-10-10: fixes, tests, tools and docs land alone; look and play wait. Read from the paths, most cautious first:
      never alone   the gate, the landing tool, this queue, the click, the relay and the pipeline (RULES)
      alone         docs; tools and skills; tests, but not the pinned sim hashes; code of the sim, the net and the data
                    (C# only) as long as the pinned hashes are not touched: the full gate then proves they still hold
      else his      everything that draws or is drawn (Presentation, Shaders, Art, UI, Scenes, Resources, Settings,
                    prefabs, materials), every data file, and any path this list does not know."""
    for p in paths:
        if p.startswith(RULES):
            return False, f'it changes the gate, the landing tools, the relay or the click itself ({p})'
    for p in paths:
        if p.startswith('docs/') or (p.endswith('.md') and not p.startswith(P + 'Assets/')):
            continue
        if p.startswith((P + 'Tools/', '.claude/')):
            continue
        if p.startswith(A + 'Tests/'):
            if PINNED in p:
                return False, f'it changes the pinned sim hashes ({p.rsplit("/", 1)[-1]}): how a battle plays out changed'
            continue
        if p.startswith(SIM) and p.endswith(('.cs', '.cs.meta', '.asmdef', '.asmdef.meta')):
            continue
        return False, f'it changes what a battle looks like or how it plays ({p})'
    return True, ''


def code_changed(paths):
    """True when the lane changes anything of the game itself, so the full gate is owed (land.py asks the same)."""
    return any(p.startswith(P) and not p.startswith(P + 'Tools/') and not p.endswith('.md') for p in paths)


def slug(lane):
    return re.sub(r'[^a-z0-9]+', '-', lane.lower()).strip('-')


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
    e = dict(lane=lane, why=' '.join(str(why).split())[:200], by=by, added=f'{now or datetime.datetime.now():%Y-%m-%d %H:%M}', state='waiting', said='', tries=0, reds=0)
    put(home / 'queue.json', q + [e])
    return e, True


def keep(home: Path, e):
    put(home / 'queue.json', [e if x['lane'] == e['lane'] else x for x in queue(home)])


def drop(home: Path, lane):
    put(home / 'queue.json', [x for x in queue(home) if x['lane'] != lane])


def same_folder(a, b):
    """True when two paths are one folder, however each is spelt (git prints the long name, TEMP the short one)."""
    try:
        return os.path.samefile(str(a), str(b))
    except OSError:
        return os.path.normcase(os.path.normpath(str(a))) == os.path.normcase(os.path.normpath(str(b)))


# ---- the outside world, so a test can stand in for the gate and for land.py -----------------------------------------

class World:
    """git in the worker's checkout, the gate, land.py and the owner's clicks. `tree` is the worker's checkout: a full
    one, its own, with a warm Unity library."""

    def __init__(self, tree: Path, home: Path):
        self.tree, self.home = Path(tree), Path(home)

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

    def release(self):
        """Stand on no branch between two lanes: the worker's checkout must never be what holds a lane up."""
        self.git('checkout', '-q', '--detach')

    def take(self, lane, tip):
        """Put the worker's checkout on the lane as origin has it. (True, '') or (False, why)."""
        if self.git('status', '--porcelain')[1].strip():
            return False, 'the worker\'s checkout has uncommitted files: nothing is landed from a checkout somebody works in'
        code, out = self.git('checkout', '-q', '-B', lane, tip)
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

    def gate(self):
        """The full gate in the worker's checkout: (green, its last words). Stopped with its whole tree past its time."""
        proj = self.tree / 'trench-warfare-3d'
        start = subprocess.run([sys.executable, 'Tools/gate_bg.py'], cwd=str(proj), capture_output=True, text=True, stdin=subprocess.DEVNULL)
        if start.returncode:
            return False, 'the gate did not start: ' + (start.stdout + start.stderr).strip()[-300:]
        try:
            r = subprocess.run([sys.executable, 'Tools/gate_bg.py', '--wait'], cwd=str(proj), capture_output=True, text=True, stdin=subprocess.DEVNULL, timeout=GATE_SECONDS)
        except subprocess.TimeoutExpired:
            self.stop_unity()
            return False, f'the gate did not end within {GATE_SECONDS // 60} minutes: it was stopped, with every Unity on the worker\'s project'
        said = (r.stdout + r.stderr).strip()
        return r.returncode == 0, ' '.join(said.splitlines()[-8:])[-700:]

    def stop_unity(self):
        want = str(self.tree / 'trench-warfare-3d').replace('/', '\\').lower()
        ask = "Get-CimInstance Win32_Process -Filter \"Name='Unity.exe'\" | ForEach-Object { '{0}|{1}' -f $_.ProcessId, $_.CommandLine }"
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
        """The one card that asks him to land this lane, with the commit his click lands. Its id is the lane's: asked once."""
        import briefs
        import notes
        where, bid = briefs.folder(), 'land-' + slug(e['lane'])
        said = f'The desktop tests it on integration and lands it, at {tip[:8]}'
        try:
            b = briefs.find(where, bid)
        except ValueError:
            b = None
        if b and b.get('state') == 'answered':
            return bid
        if not b:
            briefs.add(where, title=f'Land {e["lane"].split("/", 2)[-1]}?', what_for=(e.get('why') or 'A finished lane waits to land.') + f' It needs your click because {why}.',
                       options=['Land it', 'Not now: it stays a branch'], why=f'R1 C1 DANGEROUS MAJOR. It is finished and tested; {len(paths)} files change.',
                       evidence=[], no_evidence='the lane\'s own cards and captures show it; this card is the landing', lane=e['lane'], bid=bid)
        try:
            briefs.then(where, notes.read_all(notes.folder()), bid, 'A', said, land=dict(lane=e['lane'], tip=tip))
        except ValueError as x:
            if 'answered it already' not in str(x) and 'is closed' not in str(x):
                raise               # a card with no Then line would be a landing nobody can click
        return bid                  # (he answered while this ran: his answer is read on the next look)

    def close(self, click, words):
        import briefs
        import notes
        try:
            briefs.take(briefs.folder(), notes.folder(), click['brief'], by='the landing queue', note=click['note'], outcome=words)
        except ValueError as e:
            return str(e)
        return ''


# ---- one lane ----------------------------------------------------------------------------------------------------------

def offer(home: Path, board: Path):
    """Put in the queue every lane a relay unit finished (the board's relay/done/<unit>.json names its lane). What
    has landed since drops out at its turn; what needs his click becomes a card, ASK_MOST at a time. Returns the
    lanes that are new in the queue."""
    new = []
    for f in sorted((Path(board) / 'relay' / 'done').glob('*.json')) if (Path(board) / 'relay' / 'done').is_dir() else []:
        rec = load(f, {})
        lane = str(rec.get('lane') or '')
        if lane.startswith(('lane/sim/', 'lane/show/')) and add(home, lane, why=f'The relay finished {rec.get("id") or f.stem} on it.', by='the relay')[1]:
            new.append(lane)
    return new


def work_one(w, e, may_ask=True):
    """Take one lane as far as it goes now. Returns (state, words):
      landed        on integration, read back; words is the commit
      in            it was in integration already
      asked         it needs his click and the card is on his page
      red           the gate was red; words are its last
      waiting       something outside holds it up for now (another checkout has the lane, the worker's is not clean)
      refused       it cannot land as it is: it does not rebase by itself, the lane moved after his click, land.py refused
      gone          the lane is no longer on origin"""
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
        return 'refused', f'the lane moved on after your click: you said yes to {click["tip"][:8]}, origin has {tip[:8]}'
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
        paths = w.changed(base)
        alone, why = klass(paths)
        if not click and not alone:
            if not may_ask and e.get('state') != 'asked':
                return 'waiting', f'it needs your click ({why}); its card comes when the {ASK_MOST} in front of it are answered'
            return 'asked', why + '|' + w.card(e, tip, why, paths)
        if code_changed(paths):
            green, said = w.gate()
            if not green:
                return 'red', said
        w.fetch()
        if w.remote(INTEGRATION) != base:           # somebody landed past the queue while this was tested: once more
            base = w.remote(INTEGRATION)
            continue
        if w.remote(e['lane']) != tip:
            return 'refused', 'the lane moved on origin while it was tested: it is taken again as it now stands' if not click else 'the lane moved on after your click, while it was tested'
        code, said = w.land()
        head = w.head()
        w.fetch()
        if code == 0 and w.remote(INTEGRATION) == head:
            e['on'] = dict(click=click['brief'], note=click['note']) if click else dict(alone=True)
            return 'landed', head
        return 'refused', said or f'land.py ended {code} and integration is not the lane\'s head'
    return 'refused', f'integration moved {ROUNDS} times while this lane was tested'


def work(w, home: Path, now=None, say=print):
    """Take the queue's lanes in their order, each as far as it goes, until a round does nothing new. One landing at
    a time by construction: there is one worker. Returns [(lane, state, words)] of this call."""
    did, seen = [], set()
    while True:
        q = [e for e in queue(home) if e['lane'] not in seen]
        if not q:
            return did
        e = q[0]
        seen.add(e['lane'])
        put(home / 'beat.json', dict(at=time.time(), pid=os.getpid(), on=e['lane']))
        asked = sum(1 for x in queue(home) if x.get('state') == 'asked' and x['lane'] != e['lane'])
        state, words = work_one(w, e, may_ask=asked < ASK_MOST)
        w.release()
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


def lines(home: Path):
    """The queue for a person: one line a lane."""
    out = []
    for e in queue(home):
        out.append(f'{e["lane"]}: {e.get("state", "waiting")}' + (f' ({e["said"]})' if e.get('said') else '') + (f', {e["reds"]} red gates' if e.get('reds') else ''))
    return out


def take_lock(home: Path):
    """True when this is the one worker. A lock whose process is gone, or silent for two hours, is taken over."""
    import ideas
    rec = load(home / 'lock.json', {})
    if rec.get('pid') and rec['pid'] != os.getpid() and ideas.pid_alive(dict(pid=rec['pid'])) and time.time() - load(home / 'beat.json', {}).get('at', 0) < 7200:
        return False
    put(home / 'lock.json', dict(pid=os.getpid(), since=time.strftime('%Y-%m-%d %H:%M:%S')))
    return True


def main(argv=None):
    ap = argparse.ArgumentParser(description='one lane at a time is put on integration\'s tip, tested and landed')
    ap.add_argument('what', nargs='?', default='look', choices=('look', 'add', 'offer', 'work'))
    ap.add_argument('--board', default='', help='offer: the pipeline\'s board (default: tw3d-board beside the checkouts)')
    ap.add_argument('lane', nargs='?', default='')
    ap.add_argument('--why', default='', help='add: what the lane is, in a line, for the card if it needs his click')
    ap.add_argument('--by', default='', help='add: who puts it in')
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
            w = World(tree, home)
            for lane in w.clicks():                  # a lane he clicked is in the queue whether or not somebody put it there
                add(home, lane, by='his click')
            work(w, home)
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
