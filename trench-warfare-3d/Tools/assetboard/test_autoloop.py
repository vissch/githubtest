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
        return {}

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

    def start(self, n):
        self.started.append(n)
        self.text[self.main if n == 1 else self.extras[n]] = RUN.replace('relay-four-checks', f'x{n}')
        return 'started'

    def answers(self):
        return list(self.ans)

    def add(self, unit):
        self.added.append(unit)
        put(self.main, 'queue', unit['id'], **{k: v for k, v in unit.items() if k != 'id'})
        return True, 'queued. Board: pushed.'

    def take(self, a, queued='', outcome=''):
        if a['id'] == 'bad':
            raise ValueError('bad: the unit is not on the board\'s origin/main yet')
        self.taken.append((a['id'], queued, outcome))

    def log_decision(self, line):
        self.rows.append(line)

    def now(self):
        return self.clock


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
    put(wm.main, 'queue', 'm-3', lane='lane/show/m3', priority=50)
    wm.text[wm.extras[2]] = RUN
    wm.text[wm.extras[3]] = RUN
    has_work = autoloop.feed(wm)
    case('the loop is the only keeper: the main relay is started when it is not running and has a unit to take, not when its queue is empty or parked',
         idle_main == [] and parked_only == [] and wm.started == [1] and 'main relay' in has_work[-1], (idle_main, parked_only, has_work, wm.started))
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
         'an answer in his own words is left for a session; one that cannot be closed yet holds up no other and leaves no row',
         [u['id'] for u in w.added] == ['late-unit', 'banner-whole'] and w.added[1]['role'] == 'lane' and 'priority' not in w.added[1]
         and w.taken == [('b1', 'banner-whole', ''), ('b2', '', 'nothing is built')] and len(w.rows) == 2 and not any('late-unit' in r for r in w.rows)
         and 'not closed this round' in lines[0] and 'wait for a session' in lines[-1], (lines, w.taken, w.rows))

    # ---- a rebased lane that cannot fast-forward ----
    class Rb(Fake):
        def works(self):
            return {1: tmp / 'w1', 2: tmp / 'w2'}

        def git(self, work, *a):
            self.gitcalls.append(a[0])
            return 0, ''

        def rebased(self, work):        # two worktrees of one repo see the same branches
            return [('lane/show/heroes-off-bench-c', 'abc123def'), ('lane/show/relay-four-checks', 'fed456')] if work.name in ('w1', 'w2') else []

        def free_name(self, work, lane):
            return lane + '-b'

        def push_as(self, work, sha, name):
            self.pushes.append((work.name, sha, name))
            return 0, ''
    w = Rb(tmp / 'h')
    w.gitcalls, w.pushes = [], []
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

    # ---- one round, and the lock ----
    calls = []
    real = (autoloop.found.take, autoloop.found.rota)
    autoloop.found.take, autoloop.found.rota = (lambda fw: calls.append('take') or []), (lambda fw: calls.append('rota') or 'found: queued the look x')
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
