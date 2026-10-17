#!/usr/bin/env python3
"""Tests of the loop that works by itself (autoloop.py). Run from trench-warfare-3d/: python Tools/assetboard/test_autoloop.py

The boards are real folders in a temp directory; the relay, git and the briefs are a stand-in (Fake), fed the relay's
own status text. Each case is a thing that did not happen before 2026-10-15: done records stayed on the extra boards,
an extra relay that once read empty was never fed or started again, and his Decide-page answers waited for a session."""
import json
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import autoloop  # noqa: E402

results = []
# what relay.py status prints (githubtest-relay-run, 2026-10-15), a run and none
RUN = ('RUNNING: relay 20261015-005551-54116 relay-four-checks, lane lane/show/relay-four-checks\n'
       '  started by laptop-relay-test, relay code ba5f59e669; so far 2 legs\n'
       'Today: about 0.3% of the week used by the relay, 10.7% left of a day\'s cap of 11.0%.')
IDLE = 'NOT RUNNING. Last run 20261014-221502-4410: nothing left to do (3 legs).\nToday: about 0.3% of the week used.'


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:700]))


def put(board, kind, uid, **u):
    f = Path(board) / 'relay' / kind / f'{uid}.json'
    f.parent.mkdir(parents=True, exist_ok=True)
    f.write_text(json.dumps(dict(dict(id=uid, lane=f'lane/show/{uid}', role='lane', goal='g', done_when=['python', '-c', '1']), **u)), encoding='utf-8')


class Fake(autoloop.World):
    def __init__(self, tmp, extras=(2, 3)):
        self.main, self.home, self.acting = tmp / 'main', tmp / 'home', True
        self.home.mkdir(parents=True, exist_ok=True)
        self.extras = {n: tmp / f'b{n}' for n in extras}
        for b in [self.main, *self.extras.values()]:
            (b / 'relay' / 'queue').mkdir(parents=True, exist_ok=True)
            (b / 'relay' / 'done').mkdir(parents=True, exist_ok=True)
        self.text = {b: IDLE for b in [self.main, *self.extras.values()]}
        self.committed, self.started, self.pushed, self.prios, self.added, self.taken, self.rows, self.ans = set(), [], [], {}, [], [], [], []
        self.refuse_prio, self.clock = set(), 1000.0

    def relay(self, board, *a):
        if a[0] == 'status':
            return 0, self.text[board]
        if a[0] == 'hold':
            self.holds = getattr(self, 'holds', []) + [a[1:]]
            return 0, ''
        if a[0] == 'prio':
            if a[1] in self.refuse_prio:
                return 1, 'relay: the board push was refused'
            self.prios[a[1]] = int(a[2])
            f = self.main / 'relay' / 'queue' / f'{a[1]}.json'
            d = json.loads(f.read_text(encoding='utf-8'))
            d['priority'] = int(a[2])
            f.write_text(json.dumps(d), encoding='utf-8')
            return 0, ''
        raise AssertionError(a)

    def pull(self, board):
        return 0, ''

    def works(self):            # never the desktop's real work checkouts
        return {n: Path(p) for n, p in self.work_dirs.items()}

    def has(self, ref, path):
        return True

    def lane_ref(self, lane):
        return 'origin/' + lane

    def tracked(self, board, folder):
        return set(self.committed)

    def commit_push(self, board, paths, message, push=True):
        self.pushed.append((Path(board).name, list(paths), push))
        if Path(board) == self.main:
            self.committed |= {Path(p).name for p in paths}
        return 'pushed' if push else 'committed (kept local)'

    pipe = False

    def pipeline_work(self):
        return self.pipe

    def start(self, n, sources='lane'):
        self.started.append(n)
        self.sources = getattr(self, 'sources', []) + [(n, sources)]
        self.text[self.main if n == 1 else self.extras[n]] = RUN.replace('relay-four-checks', f'x{n}')
        return 'started'

    def answers(self):
        return list(self.ans)

    def add(self, unit):
        self.added.append(unit)
        put(self.main, 'queue', unit['id'], **{k: v for k, v in unit.items() if k != 'id'})
        return True, 'queued. Board: pushed.'

    def take(self, a, queued='', outcome='', option=''):
        if a['id'] == 'bad':
            raise ValueError('bad: the unit is not on the board\'s origin/main yet')
        self.taken.append((a['id'], queued, outcome) + ((option,) if option else ()))

    def log_decision(self, line):
        self.rows.append(line)

    def now(self):
        return self.clock

    # the steps of loopsteps.py: nothing outside the test is ever reached
    free, legs, booked, routed_ideas, stop_records, unqueued, briefs_written, answered_briefs = 0.0, [], [], None, [], [], [], set()
    shas, reply = {}, ''

    def pace_free(self):
        return self.free

    def leg(self, prompt, usd):
        self.legs = self.legs + [prompt]
        return self.reply, 0.02

    def book(self, rec):
        self.booked = self.booked + [rec]
        return 0, ''

    def lane_facts(self, lane):
        return f'{lane} is not on origin'

    def ideas(self, write):
        self.idea_calls = getattr(self, 'idea_calls', []) + [write]
        return self.routed_ideas or ([], dict(asked=[], done=[], owed=[]), '')

    def changed(self, board, folders):
        return []

    def stops(self):
        return list(self.stop_records)

    def lane_sha(self, lane, at=''):
        return self.shas.get(lane, 'L0')

    def int_sha(self, at=''):
        return self.shas.get('INT', 'I0')

    def lane_files(self, lane):
        return []

    def changed_files(self, a, b):
        return []

    def unqueue(self, board, uid, why=""):
        (Path(board) / 'relay' / 'queue' / f'{uid}.json').unlink(missing_ok=True)
        self.unqueued = self.unqueued + [(Path(board).name, uid)]
        return True

    def park_brief(self, uid, rec, nxt, unit):
        self.briefs_written = self.briefs_written + [(uid, nxt, unit)]
        return f'brief-{uid}'

    def brief_answered(self, bid):
        return bid in self.answered_briefs

    # the checkouts: what `git worktree list` says, which are clean, and what was detached
    cos, dirty, detached, on_main, work_dirs = {}, set(), [], ('', ''), {}

    def checkouts(self):
        return dict(self.cos)

    def clean(self, work):
        return work not in self.dirty

    def detach(self, work):
        self.detached = self.detached + [work]
        self.cos = {k: v for k, v in self.cos.items() if v != work}
        return 0, ''

    def main_on(self):
        return self.on_main

    ps, killed = [], []

    def procs(self):
        return list(self.ps)

    def kill(self, pid):            # as taskkill /T: a pid ended already, or whose parent was, is not found
        dead = {q['pid'] for q in self.ps if q['pid'] in self.killed or q.get('ppid') in self.killed}
        self.killed = self.killed + [pid]
        return (128, 'not found') if pid in dead else (0, '')


def main():
    tmp = Path(tempfile.mkdtemp(prefix='tw-autoloop-test-'))

    case('the relay\'s own status text is read: the lane of a run, none when idle, and "unknown" when it is neither',
         autoloop.parse_status(RUN) == ({'lane/show/relay-four-checks'}, '') and autoloop.parse_status(IDLE) == (set(), 'nothing left to do')
         and autoloop.parse_status('Traceback (most recent call last)')[0] is None, (autoloop.parse_status(RUN), autoloop.parse_status(IDLE)))

    # ---- back ----
    w = Fake(tmp / 'a')
    put(w.extras[2], 'done', 'banner-legs')
    put(w.extras[3], 'done', 'ridge-own-line')
    put(w.main, 'done', 'ridge-own-line')
    w.committed.add('ridge-own-line.json')
    put(w.main, 'done', 'left-uncommitted')
    put(w.extras[2], 'done', 'left-uncommitted')
    lines = autoloop.copy_back(w)
    again = autoloop.copy_back(w)
    case('the done records of the extra boards that the main board has not committed are copied to it and pushed, once; one copied before but never committed is committed now',
         (w.main / 'relay' / 'done' / 'banner-legs.json').exists() and '2 done records' in lines[0] and sorted(w.pushed[0][1]) == ['relay/done/banner-legs.json', 'relay/done/left-uncommitted.json']
         and w.pushed[0][2] and again == [], (lines, again, w.pushed))

    # ---- feed ----
    w = Fake(tmp / 'b')
    put(w.main, 'queue', 'held-1', lane='lane/show/held')
    put(w.extras[3], 'queue', 'held-1', lane='lane/show/held')
    put(w.main, 'queue', 'mainrun-1', lane='lane/show/relay-four-checks', priority=5)
    put(w.main, 'queue', 'parked-1', lane='lane/show/parked', priority=99)
    put(w.main, 'queue', 'free-2', lane='lane/show/free', priority=40)
    put(w.main, 'queue', 'free-1', lane='lane/show/free', priority=20)
    put(w.main, 'queue', 'other-1', lane='lane/show/other', priority=30)
    put(w.main, 'queue', 'gone-1', lane='lane/show/gone', priority=10)
    put(w.main, 'done', 'gone-1')
    w.text[w.main] = RUN
    lines = autoloop.feed(w)
    got = sorted(p.stem for p in (w.extras[2] / 'relay' / 'queue').glob('*.json'))
    case('an empty extra relay that is not running gets the open units of the free lane first in priority, never the lane the main relay runs (read from its status), '
         'one another relay holds, a parked one or a done one; they are parked at 99 on the main board and the relay is started; '
         'one with open units that reads "nothing left to do" is started too',
         got == ['free-1', 'free-2'] and w.prios == {'free-1': 99, 'free-2': 99} and sorted(w.started) == [2, 3]
         and 'priority' not in json.loads((w.extras[2] / 'relay' / 'queue' / 'free-1.json').read_text(encoding='utf-8')) and any(not p for _, _, p in w.pushed), (lines, got, w.prios, w.started))
    w.started.clear()
    w.text[w.extras[3]] = IDLE
    w.clock += 60
    soon = autoloop.feed(w)
    w.clock += autoloop.STARTS[0] + 1
    later = autoloop.feed(w)
    w.text[w.extras[3]] = IDLE
    w.clock += autoloop.STARTS[0] + 1
    doubled = autoloop.feed(w)
    w.clock += autoloop.STARTS[0] + 1
    after_double = autoloop.feed(w)
    case('a relay that stops at once is not started every round: not again inside the wait after a start; when it stopped again for the same reason the wait doubles; a running one is left alone',
         w.started == [3, 3] and not any('gets' in l for l in soon + later) and soon == [] and len(later) == 1 and doubled == [] and len(after_double) == 1,(soon, later, doubled, after_double, w.started))
    wm = Fake(tmp / 'm')
    idle_main = autoloop.feed(wm)
    put(wm.main, 'queue', 'm-1', lane='lane/show/m', priority=99)
    parked_only = autoloop.feed(wm)
    put(wm.main, 'queue', 'm-2', lane='lane/show/m2', priority=50)
    put(wm.extras[2], 'queue', 'm-2', lane='lane/show/m2')       # an extra holds that lane: the main relay keeps its own work
    wm.text[wm.extras[2]] = RUN
    wm.text[wm.extras[3]] = RUN
    held_only = autoloop.feed(wm)
    put(wm.main, 'queue', 'm-3', lane='lane/show/m3', priority=50)
    has_work = autoloop.feed(wm)
    case('the loop is the only keeper: the main relay is started when it is not running and has a unit to take, not when its queue is empty or parked',
         idle_main == [] and parked_only == [] and held_only == [] and wm.started == [1] and 'main relay' in has_work[-1]
         and ('autoloop', '--release') in wm.holds, (idle_main, parked_only, held_only, has_work, wm.started, wm.holds))
    wd = Fake(tmp / 'dirty')
    wd.works = lambda: {3: tmp / 'w3'}
    wd.asides = []
    wd.put_aside = lambda work, n: (wd.asides.append((work.name, n)) or True, 'put aside')
    put(wd.extras[3], 'queue', 'd-1', lane='lane/show/d')
    wd.text[wd.main] = RUN
    wd.text[wd.extras[2]] = RUN
    wd.text[wd.extras[3]] = ('NOT RUNNING. Last run 20261015-141327-10424: the work checkout cannot be used: it has uncommitted changes '
                             'that are not a saved leg snapshot (0 legs).')
    lines = autoloop.feed(wd)
    wd.clock += 60
    soon = autoloop.feed(wd)
    wd.clock += autoloop.QUIET
    later = autoloop.feed(wd)
    case('a stopped relay whose checkout holds a leg\'s leftover files has them put aside (patch and stash, as the old keeper did) once, '
         'and is started only after the relay\'s ten quiet minutes',
         wd.asides == [('w3', 3)] and any('leftover files' in l for l in lines) and soon == [] and wd.started == [3] and len(later) == 1, (lines, soon, later, wd.asides, wd.started))
    wf = Fake(tmp / 'dirty2')
    wf.works = lambda: {3: tmp / 'w3'}
    wf.put_aside = lambda work, n: (False, 'not put aside: git stash failed (index.lock)')
    put(wf.extras[3], 'queue', 'd-1', lane='lane/show/d')
    wf.text[wf.main] = RUN
    wf.text[wf.extras[2]] = RUN
    wf.text[wf.extras[3]] = ('NOT RUNNING. Last run 20261015-141327-10424: the work checkout cannot be used: '
                             'it has uncommitted changes that are not a saved leg snapshot (0 legs).')
    failed = autoloop.feed(wf)
    case('when the files cannot be put aside the relay is not started, and it says why', wf.started == [] and 'stash failed' in failed[0], failed)
    w3 = Fake(tmp / 'c')
    put(w3.main, 'queue', 'x-1', lane='lane/show/x', priority=99)
    case('with no free lane an empty relay is not started', autoloop.feed(w3) == [] and w3.started == [])
    w4 = Fake(tmp / 'd')
    put(w4.main, 'queue', 'two-1', lane='lane/show/two', priority=20)
    put(w4.main, 'queue', 'two-2', lane='lane/show/two', priority=30)
    w4.refuse_prio = {'two-2'}
    w4.text[w4.main] = RUN
    lines = autoloop.feed(w4)
    case('when the main board refuses to park a lane, what was parked is put back and the extra board gets nothing: two relays never work one lane',
         list((w4.extras[2] / 'relay' / 'queue').glob('*.json')) == [] and w4.prios == {'two-1': 20} and w4.started == [] and 'put back' in lines[0], (lines, w4.prios))
    w5 = Fake(tmp / 'e')
    put(w5.main, 'queue', 'y-1', lane='lane/show/y')
    w5.text[w5.extras[3]] = 'Traceback: the board is locked'
    case('when a relay does not say whether it runs, nothing is fed or started', 'nothing is fed' in autoloop.feed(w5)[0] and w5.started == [])

    # ---- answers ----
    w = Fake(tmp / 'f')
    unit = dict(id='banner-whole', lane='lane/show/banner-whole', goal='g', done_when=['python', '-c', '1'], priority=10)
    base = dict(option='A', text='Land it', when='2026-10-15 01:00', says='', unit=None)
    w.ans = [dict(base, id='bad', title='Pushed late', note='n0', go='queue', unit=dict(unit, id='late-unit')),
             dict(base, id='b1', title='Land the Banner?', note='n1', go='queue', unit=unit),
             dict(base, id='b2', title='Keep the old fire?', note='n2', go='nothing', says='nothing is built'),
             dict(base, id='b3', title='Ridge slope?', note='n3', go='write')]
    lines = autoloop.take_answers(w)
    case('his plain yes on the Decide page is carried out with no session: the unit is queued (with a role, never its own priority) and the brief closed; a yes to nothing closes it; '
         'an answer in his own words waits while the day\'s pace has nothing free; one that cannot be closed yet holds up no other and leaves no row',
         [u['id'] for u in w.added] == ['late-unit', 'banner-whole'] and w.added[1]['role'] == 'lane' and 'priority' not in w.added[1]
         and w.taken == [('b1', 'banner-whole', ''), ('b2', '', 'nothing is built')] and len(w.rows) == 2 and not any('late-unit' in r for r in w.rows)
         and 'not closed this round' in lines[0] and 'in his own words wait' in lines[-1], (lines, w.taken, w.rows))

    # ---- a rebased lane that cannot fast-forward ----
    class Rb(Fake):
        def works(self):
            return {1: tmp / 'w1', 2: tmp / 'w2'}

        def git(self, work, *a):
            self.gitcalls.append(a[0])
            return 0, ''

        def rebased(self, work):        # two worktrees of one repo see the same branches; a session's own lane among them
            return [('lane/show/heroes-off-bench-c', 'abc123def'), ('lane/show/relay-four-checks', 'fed456'), ('lane/show/a-sessions-own', 'aaa111')] if work.name in ('w1', 'w2') else []

        def free_name(self, work, lane):
            return lane + '-b'

        def push_as(self, work, sha, name):
            self.pushes.append((work.name, sha, name))
            return 0, ''
    w = Rb(tmp / 'h')
    w.gitcalls, w.pushes = [], []
    put(w.main, 'queue', 'heroes-c', lane='lane/show/heroes-off-bench-c')     # a unit names that lane; the other lane is nobody's
    w.text[w.extras[2]] = RUN
    lines = autoloop.rescue(w)
    again = autoloop.rescue(w)
    case('a lane a leg rebased and could not push is pushed as <lane>-b with no force, once, and recorded; a relay that runs is left alone; '
         'nothing but a fetch and a push touches its checkout (the index never moves)',
         w.pushes == [('w1', 'abc123def', 'lane/show/heroes-off-bench-c-b')] and again == [] and len(w.rows) == 1 and '-b' in lines[0]
         and set(w.gitcalls) <= {'fetch'}, (lines, w.pushes, w.gitcalls))

    # ---- depends-on: units about the relay wait for the relay line ----
    class Dep(Fake):
        landed = False

        def has(self, ref, path):
            return self.landed and ref.endswith(autoloop.INTEGRATION)

        def lane_ref(self, lane):
            return 'origin/' + lane
    w = Dep(tmp / 'i')
    put(w.main, 'queue', 'second-critic-on', goal='change Tools/relay/runner.py so ...', priority=20)
    put(w.main, 'queue', 'banner-legs', goal='the Banner walks', priority=30)
    put(w.main, 'queue', 'relay-moved', goal='Tools/relay/x.py', priority=40)
    first = autoloop.wait_for_relay(w)
    w.relay(w.main, 'prio', 'relay-moved', '3')            # a person moves one meanwhile
    w.landed = True
    back = autoloop.wait_for_relay(w)
    case('a unit about the relay whose lane has no Tools/relay waits parked at 99 (not a run that blocks); once the relay line has landed it goes back to its own priority; other units are left alone',
         len(first) == 2 and all('waits for relay-harness' in l for l in first) and w.prios == {'second-critic-on': 20, 'relay-moved': 3} and len(back) == 1 and 'goes back to priority 20' in back[0], (first, back, w.prios))

    # ---- a lane checked out in another checkout (the night of 2026-10-16) ----
    def wt(name, **kw):
        w = Fake(tmp / name, extras=(2, 3))
        w.work_dirs = {1: 'C:/gh/relay-work', 2: 'C:/gh/relay-work2', 3: 'C:/gh/relay-work3'}
        put(w.main, 'queue', 'first', lane='lane/show/first', priority=10)
        put(w.main, 'queue', 'second', lane='lane/show/second', priority=20)
        for k, v in kw.items():
            setattr(w, k, v)
        return w
    w = wt('wt1', cos={'lane/show/first': 'C:/gh/relay-work'}, dirty={'C:/gh/relay-work'})
    lines = autoloop.feed(w)
    got = sorted(p.stem for p in (w.extras[2] / 'relay' / 'queue').glob('*.json'))
    case("a lane checked out in another relay's work checkout whose tree has changes is never fed: the relay gets the next lane, and the log says the lane was skipped",
         got == ['second'] and w.detached == [] and any('lane/show/first skipped' in l for l in lines), (got, lines))
    w = wt('wt2', cos={'lane/show/first': 'C:/gh/relay-work'})
    lines = autoloop.feed(w)
    got = sorted(p.stem for p in (w.extras[2] / 'relay' / 'queue').glob('*.json'))
    case("the main relay's work checkout still holding its last lane after its run stopped (clean, not running) is detached to free it, and the lane is fed",
         got == ['first'] and w.detached == ['C:/gh/relay-work'] and any('lane/show/first freed' in l for l in lines) and 2 in w.started, (got, lines, w.detached))
    w = wt('wt3', cos={'lane/show/first': 'C:/gh/relay-work3', 'lane/show/second': 'C:/gh/githubtest-relay'})
    w.text[w.extras[3]] = RUN.replace('relay-four-checks', 'x3')
    lines = autoloop.feed(w)
    got = sorted(p.stem for p in (w.extras[2] / 'relay' / 'queue').glob('*.json'))
    case("a lane checked out by a running relay or by a checkout that is no relay's is not fed nor freed; with no lane left the relay gets nothing and is not started",
         got == [] and w.detached == [] and 2 not in w.started and sum('relay 2:' in l and 'skipped' in l for l in lines) == 2, (got, lines))
    w = wt('wt4', cos={'lane/show/mine': 'C:/gh/githubtest-relay'})
    put(w.extras[2], 'queue', 'mine', lane='lane/show/mine')
    put(w.main, 'queue', 'mine', lane='lane/show/mine', priority=99)
    lines = autoloop.feed(w)
    got = sorted(p.stem for p in (w.extras[2] / 'relay' / 'queue').glob('*.json'))
    case("an extra relay whose own queued unit's lane is held where the loop cannot free it is not left standing on it: the unit comes off its queue and it is fed the next free lane (relay 5, 11:42-12:16)",
         got == ['first', 'second'][:len(got)] and 'mine' not in got and got and 2 in w.started, (lines, got, w.started))
    w.cos = {}
    w.text = {b: IDLE for b in w.text}
    back = autoloop.feed(w)
    case('once its lane is free, the unit taken off for it goes back to priority 50 on the main board',
         w.prios.get('mine') == 50 and any('mine goes back to priority 50' in l for l in back), (back, w.prios))
    w = wt('wt11', cos={'lane/show/mine': 'C:/gh/githubtest-relay'})
    put(w.extras[2], 'queue', 'mine', lane='lane/show/mine')               # no copy on the main board
    lines = autoloop.feed(w)
    case('a unit with no copy on the main board is never taken off: it would be lost; its relay is not started on the held lane',
         (w.extras[2] / 'relay' / 'queue' / 'mine.json').exists() and 2 not in w.started, (lines, w.started))
    w = Fake(tmp / 'wt12', extras=())
    w.work_dirs = {1: 'C:/gh/relay-work'}
    w.asides = []
    w.put_aside = lambda work, n: (w.asides.append(n) or True, 'put aside')
    put(w.main, 'queue', 'mainwork', lane='lane/show/mainwork', priority=10)
    w.text[w.main] = ('NOT RUNNING. Last run 20261016-142533-81884: the work checkout cannot be used: it has uncommitted changes '
                      'that are not a saved leg snapshot (0 legs).')
    lines = autoloop.feed(w)
    again = autoloop.feed(w)
    case('the main relay\'s checkout left dirty by a leg is put aside (stash and patch) once its run has stopped, as the extras\' are, and it is not started into the dirty tree',
         w.asides == [1] and 1 not in w.started and any('main relay stopped on leftover files' in l for l in lines), (lines, again, w.asides, w.started))
    w2 = Fake(tmp / 'wt13', extras=())
    w2.work_dirs = {1: 'C:/gh/relay-work'}
    w2.asides = []
    w2.put_aside = lambda work, n: (w2.asides.append(n) or True, 'put aside')
    put(w2.main, 'queue', 'mainwork', lane='lane/show/mainwork', priority=10)
    w2.text[w2.main] = RUN
    autoloop.feed(w2)
    case('a running main relay\'s checkout is never put aside', w2.asides == [], w2.asides)
    w.clock += autoloop.QUIET + 1
    third = autoloop.feed(w)
    case('after the quiet wait the main relay is started, and the same stopped run is not put aside twice',
         1 in w.started and w.asides == [1], (third, w.asides, w.started))
    w3 = Fake(tmp / 'wt14', extras=())
    w3.work_dirs = {1: 'C:/gh/relay-work'}
    w3.asides = []
    w3.put_aside = lambda work, n: (w3.asides.append(n) or True, 'put aside')
    put(w3.main, 'queue', 'mainwork', lane='lane/show/mainwork', priority=10)
    w3.text[w3.main] = ('NOT RUNNING. Last run 20261016-142533-81884: the work checkout cannot be used: it has uncommitted changes '
                        'that are not a saved leg snapshot (0 legs).\nThe relay build is held by coordinator until 2026-10-16 16:00 (relay.py hold).')
    autoloop.feed(w3)
    case('while a session holds the relay, the main checkout\'s files are left alone and it is not started', w3.asides == [] and 1 not in w3.started, (w3.asides, w3.started))
    w = Fake(tmp / 'wt10', extras=(4,))
    w.work_dirs = {1: 'C:/gh/relay-work', 4: 'C:/gh/relay-work4'}
    w.cos = {'lane/show/heroes': 'C:/gh/relay-work4'}
    put(w.extras[4], 'queue', 'other', lane='lane/show/other')                 # relay 4 stopped, waiting out its restart wait
    (w.home / 'starts.json').write_text(json.dumps({'4': dict(at=w.clock, reason='', wait=1800)}), encoding='utf-8')
    put(w.main, 'queue', 'heroes', lane='lane/show/heroes', priority=10)
    lines = autoloop.feed(w)
    case('the main relay\'s lane held by a stopped extra relay\'s clean checkout is freed before the main relay starts (its 12:42 run: 4 FAILs that were only these refusals)',
         w.detached == ['C:/gh/relay-work4'] and 1 in w.started, (lines, w.detached, w.started))
    w = wt('wt5', on_main=('first', ''))
    w.text[w.main] = RUN.replace('lane lane/show/relay-four-checks', 'lane None')
    lines = autoloop.feed(w)
    got = sorted(p.stem for p in (w.extras[2] / 'relay' / 'queue').glob('*.json'))
    case('the unit the running main relay is on (its progress now_on) is never fed to an extra relay, also while its status names no lane yet',
         'first' not in got and got == ['second'], (got, lines))
    w = wt('wt6')
    (w.home / 'attempts.json').write_text(json.dumps({'first': dict(seen=['r1'], fails=['r1 FAIL'], main_try=True)}), encoding='utf-8')
    lines = autoloop.feed(w)
    got = sorted(p.stem for p in (w.extras[2] / 'relay' / 'queue').glob('*.json'))
    case('a unit taken off an extra relay for its second try on the main relay is not handed straight back to an extra relay',
         got == ['second'], (got, lines))

    w = wt('wt8', cos={'lane/show/first': 'C:/gh/relay-work2'})
    put(w.extras[2], 'queue', 'own', lane='lane/show/own')
    lines = autoloop.feed(w)
    case('a relay started this round keeps its checkout: another relay does not detach it in the same round',
         2 in w.started and w.detached == [] and not (w.extras[3] / 'relay' / 'queue' / 'first.json').exists(), (lines, w.detached))
    w = wt('wt7')
    put(w.extras[2], 'queue', 'first', lane='lane/show/first')
    w.stop_records = [('r1', '2026-10-16T05:00:00', 'first', 'FAIL', 'execute leg 1 of 1 reported blocked'),
                      ('r2', '2026-10-16T09:46:00', 'first', 'FAIL', "fatal: 'lane/show/first' is already used by worktree at 'C:/gh/relay-work'"),
                      ('r3', '2026-10-16T09:52:00', 'first', 'FAIL', "fatal: 'lane/show/first' is already used by worktree at 'C:/gh/relay-work'")]
    lines = autoloop.attempts(w)
    rec = json.loads((w.home / 'attempts.json').read_text(encoding='utf-8')).get('first') or {}
    case('a 0-leg FAIL that is only a lane-switch refusal ("already used by worktree") is the loop\'s mistake: it is no try of the unit, and the unit is not parked for it',
         rec.get('fails') == ['r1 FAIL'] and not rec.get('parked') and (w.extras[2] / 'relay' / 'queue' / 'first.json').exists() is False and rec.get('main_try'), (rec, lines))
    w.stop_records = w.stop_records + [('r4', '2026-10-16T11:00:00', 'first', 'BLOCKED', 'execute leg 2 of 2 reported blocked')]
    autoloop.attempts(w)
    rec = json.loads((w.home / 'attempts.json').read_text(encoding='utf-8')).get('first') or {}
    case('once the main relay has given it a real try, the unit may be fed to an extra relay again', not rec.get('main_try'), rec)

    w = wt('wt9')
    put(w.main, 'queue', 'first', lane='lane/show/first', priority=99)
    put(w.extras[2], 'queue', 'first', lane='lane/show/first')
    w.refuse_prio = {'first'}
    w.stop_records = [('r1', '2026-10-16T05:00:00', 'first', 'FAIL', 'execute leg 1 of 1 reported blocked')]
    autoloop.attempts(w)
    rec = json.loads((w.home / 'attempts.json').read_text(encoding='utf-8')).get('first') or {}
    case('when its main copy cannot come off 99, the unit is not marked as the main relay\'s, so it is never stranded', not rec.get('main_try'), rec)

    whys = []
    real = (autoloop.found.take, autoloop.found.rota)
    autoloop.found.take, autoloop.found.rota = (lambda fw: []), (lambda fw, why='': whys.append(why) or 'found: x')
    try:
        w = wt('wt15')
        for uid in ('first', 'second'):
            (w.main / 'relay' / 'queue' / f'{uid}.json').unlink()
        put(w.main, 'queue', 'ridge', lane='lane/show/ridge', priority=50)
        (w.home / 'attempts.json').write_text(json.dumps({'ridge': dict(seen=['r1'], fails=['r1 FAIL'], main_try=True)}), encoding='utf-8')
        w.text[w.main] = RUN.replace('relay-four-checks', 'other')
        w.found = lambda: None
        autoloop.round_(w)
    finally:
        autoloop.found.take, autoloop.found.rota = real
    case('"dry" means no unit an idle relay can take: with idle extra relays and only a unit kept for the main relay, a look is queued (15:41-16:25)',
         whys and 'idle' in whys[-1], whys)

    # ---- two clicks of his seconds apart: the newest note is the one acted on (34 briefs, 22:21-22:22, 10-17) ----
    class Twice(Fake):
        def answers(self):
            self.reads = getattr(self, 'reads', 0) + 1
            return list(self.ans if self.reads == 1 else self.later)

        def take(self, a, queued='', outcome='', option=''):
            if a['note'] != 'new-' + a['id']:
                raise ValueError(f'{a["id"]}: his last note about it is new-{a["id"]}, not {a["note"]}: read it first')
            Fake.take(self, a, queued, outcome, option)
    w = Twice(tmp / 'twice')
    u1, u2 = dict(id='u1', lane='lane/show/u1', goal='g', done_when=['python', '-c', '1']), dict(id='u2', lane='lane/show/u2', goal='g', done_when=['python', '-c', '1'])
    b = dict(text='Do it', when='2026-10-17 22:21', says='', title='t')
    w.ans = [dict(b, id='same', option='A', note='old-same', go='queue', unit=u1), dict(b, id='turn', option='A', note='old-turn', go='queue', unit=u2)]
    w.later = [dict(b, id='same', option='A', note='new-same', go='queue', unit=u1), dict(b, id='turn', option='B', note='new-turn', go='write', why='his notes do not all pick the same option')]
    lines = autoloop.take_answers(w)
    case('two notes of his on a brief: the newest is the note taken up, the unit is queued once, and when his notes pick two options nothing of the older is queued (a leg reads his words)',
         [u['id'] for u in w.added] == ['u1'] and w.taken == [('same', 'u1', '')], (lines, w.added, w.taken))

    # ---- the main relay with nothing on its queue it can take works the pipeline board (option A, 2026-10-17) ----
    w = Fake(tmp / 'pipe', extras=())
    w.pipe = True
    lines = autoloop.feed(w)
    w0 = Fake(tmp / 'pipe0', extras=())
    autoloop.feed(w0)
    w2 = Fake(tmp / 'pipe2', extras=())
    w2.pipe = True
    put(w2.main, 'queue', 'q1', priority=50)
    autoloop.feed(w2)
    case('the main relay with no unit it can take is started on the pipeline board when it has a job (pipeline only, not the held units); none, not started; with a unit, on its lane queue as before',
         getattr(w, 'sources', []) == [(1, 'pipeline')] and w0.started == [] and getattr(w2, 'sources', []) == [(1, 'lane')], (lines, getattr(w, 'sources', None), getattr(w2, 'sources', None)))

    # ---- editors a relay leg left open (relay 4, 10-16 17:59 to 10-17 22:20) ----
    w = Fake(tmp / 'ed')
    w.work_dirs = {n: tmp / 'ed' / f'work{n}' for n in (1, 2, 3)}
    w.text[w.extras[3]] = RUN
    other = tmp / 'ed' / 'empire-og-client'
    for d in (w.work_dirs[2] / 'trench-warfare-3d', w.work_dirs[3] / 'trench-warfare-3d', other):
        (d / 'Temp').mkdir(parents=True)
        (d / 'Temp' / 'UnityLockfile').write_text('', encoding='utf-8')
    ed = lambda pid, proj, age=60, parent='explorer.exe', pcmd='', ppid=1: dict(
        pid=pid, ppid=ppid, name='Unity.exe', cmd=f'"C:/Unity/Unity.exe" -projectPath "{proj}" -batchmode', age=age, parent=parent, parent_cmd=pcmd)
    w.ps = [ed(10, w.work_dirs[2] / 'trench-warfare-3d'),                       # relay 2 stopped: ended
            ed(11, w.work_dirs[3] / 'trench-warfare-3d'),                       # relay 3 runs: kept
            ed(12, other, age=99999, parent=''),                                # another project: never touched
            ed(13, w.work_dirs[3] / 'trench-warfare-3d', age=3 * 3600, parent='cmd.exe', pcmd='cmd /c unity.cmd -projectPath x', ppid=99),
            dict(pid=99, ppid=1, name='cmd.exe', cmd='cmd /c unity.cmd -projectPath x', age=3 * 3600, parent='', parent_cmd='')]
    lines = getattr(autoloop, 'end_editors', lambda w: [])(w)
    case("an editor on a stopped relay's work checkout is ended and its UnityLockfile removed; a unity.cmd Unity older than 2 h goes with its stub; a running relay's editor and another project's are left alone",
         sorted(w.killed) == [10, 13, 99] and not any('did not end' in l for l in lines) and not (w.work_dirs[2] / 'trench-warfare-3d' / 'Temp' / 'UnityLockfile').exists()
         and (other / 'Temp' / 'UnityLockfile').exists() and (w.work_dirs[3] / 'trench-warfare-3d' / 'Temp' / 'UnityLockfile').exists(), (w.killed, lines))

    # ---- one round, and the lock ----
    calls = []
    real = (autoloop.found.take, autoloop.found.rota)
    autoloop.found.take, autoloop.found.rota = (lambda fw: calls.append('take') or []), (lambda fw, why='': calls.append('rota') or 'found: queued the look x')
    try:
        w = Fake(tmp / 'g')
        w.found = lambda: None
        w.answers = lambda: (_ for _ in ()).throw(RuntimeError('the Drive is not there'))
        lines = autoloop.round_(w)
    finally:
        autoloop.found.take, autoloop.found.rota = real
    case('one round runs every step, and one that fails (the Drive gone) does not stop the others: found take and rota still run',
         calls == ['take', 'rota'] and any('take_answers: failed' in l for l in lines) and 'found: queued the look x' in lines, lines)
    home = tmp / 'lock'
    home.mkdir()
    first = autoloop.take_lock(home, now=5000)
    second = autoloop.take_lock(home, now=5000 + 60)
    stale = autoloop.take_lock(home, now=5000 + autoloop.STALE + 1)
    case('one loop a machine: a second is refused and says why; a lock whose loop stopped beating is taken over',
         first == '' and 'another loop holds' in second and stale == '', (first, second, stale))
    print(f'{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
