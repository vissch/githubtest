#!/usr/bin/env python3
"""Tests of the loop that works by itself (autoloop.py). Run from trench-warfare-3d/: python Tools/assetboard/test_autoloop.py

The boards are real folders in a temp directory; the relay, git and the briefs are a stand-in (Fake). Each case is a
thing that did not happen before 2026-10-15: done records stayed on the extra boards, an extra relay that once read
empty was never fed or started again, and his Decide-page answers waited for a session."""
import json
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import autoloop  # noqa: E402

results = []


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
        self.extras = {n: tmp / f'b{n}' for n in extras}
        for b in [self.main, *self.extras.values()]:
            (b / 'relay' / 'queue').mkdir(parents=True, exist_ok=True)
            (b / 'relay' / 'done').mkdir(parents=True, exist_ok=True)
        self.run, self.on, self.started, self.pushed, self.prios, self.added, self.taken, self.rows, self.ans = set(), '', [], [], {}, [], [], [], []

    def pull(self, board):
        return 0, ''

    def commit_push(self, board, paths, message, push=True):
        self.pushed.append((Path(board).name, list(paths), push))
        return 'pushed' if push else 'committed (kept local)'

    def running(self, n):
        return n in self.run

    def main_on(self):
        return self.on

    def start(self, n):
        self.started.append(n)
        return 'started'

    def prio(self, uid, n):
        self.prios[uid] = n
        f = self.main / 'relay' / 'queue' / f'{uid}.json'
        d = json.loads(f.read_text(encoding='utf-8'))
        d['priority'] = n
        f.write_text(json.dumps(d), encoding='utf-8')
        return 0, ''

    def answers(self):
        return list(self.ans)

    def add(self, unit):
        self.added.append(unit)
        put(self.main, 'queue', unit['id'], **{k: v for k, v in unit.items() if k != 'id'})
        return True, 'queued. Board: pushed.'

    def take(self, a, queued='', outcome=''):
        self.taken.append((a['id'], queued, outcome))

    def log_decision(self, line):
        self.rows.append(line)


def main():
    tmp = Path(tempfile.mkdtemp(prefix='tw-autoloop-test-'))

    # ---- back ----
    w = Fake(tmp / 'a')
    put(w.extras[2], 'done', 'banner-legs')
    put(w.extras[3], 'done', 'ridge-own-line')
    put(w.main, 'done', 'ridge-own-line')
    lines = autoloop.copy_back(w)
    again = autoloop.copy_back(w)
    case('the done records of the extra boards that the main board lacks are copied to it and pushed, once',
         (w.main / 'relay' / 'done' / 'banner-legs.json').exists() and '1 done records' in lines[0] and w.pushed[0][0] == 'main' and w.pushed[0][2] and again == [], (lines, again))

    # ---- feed ----
    w = Fake(tmp / 'b')
    put(w.main, 'queue', 'held-1', lane='lane/show/held')
    put(w.extras[3], 'queue', 'held-1', lane='lane/show/held')
    put(w.main, 'queue', 'mainrun-1', lane='lane/show/mainrun')
    put(w.main, 'queue', 'parked-1', lane='lane/show/parked', priority=99)
    put(w.main, 'queue', 'free-2', lane='lane/show/free', priority=40)
    put(w.main, 'queue', 'free-1', lane='lane/show/free', priority=20)
    put(w.main, 'queue', 'other-1', lane='lane/show/other', priority=30)
    put(w.main, 'queue', 'gone-1', lane='lane/show/gone', priority=10)
    put(w.main, 'done', 'gone-1')
    w.on = 'mainrun-1'
    lines = autoloop.feed(w)
    got = sorted(p.stem for p in (w.extras[2] / 'relay' / 'queue').glob('*.json'))
    case('an empty extra relay that is not running gets the open units of the free lane first in priority, never a lane another relay holds, a parked one, a done one or the main relay\'s own; '
         'they are parked at 99 on the main board and the relay is started; one with open units that is not running is started too, whatever its last stop said',
         got == ['free-1', 'free-2'] and w.prios == {'free-1': 99, 'free-2': 99} and sorted(w.started) == [2, 3]
         and 'priority' not in json.loads((w.extras[2] / 'relay' / 'queue' / 'free-1.json').read_text(encoding='utf-8')) and any(not p for _, _, p in w.pushed), (lines, got, w.prios, w.started))
    w.started.clear()
    w.run = {2}
    lines2 = autoloop.feed(w)
    case('a relay that runs is left alone; the next round gives the next free lane only to a relay that needs one', w.started == [3] and not any('gets' in l for l in lines2), lines2)
    w3 = Fake(tmp / 'c')
    put(w3.main, 'queue', 'x-1', lane='lane/show/x', priority=99)
    case('with no free lane an empty relay is not started', autoloop.feed(w3) == [] and w3.started == [])

    # ---- answers ----
    w = Fake(tmp / 'd')
    unit = dict(id='banner-whole', lane='lane/show/banner-whole', goal='g', done_when=['python', '-c', '1'], priority=10)
    w.ans = [dict(id='b1', title='Land the Banner?', option='A', text='Land it', note='n1', when='2026-10-15 01:00', go='queue', unit=unit, says=''),
             dict(id='b2', title='Keep the old fire?', option='B', text='Keep it', note='n2', when='2026-10-15 01:00', go='nothing', unit=None, says='nothing is built'),
             dict(id='b3', title='Ridge slope?', option='other', text='', note='n3', when='2026-10-15 01:00', go='write', unit=None, says='')]
    lines = autoloop.take_answers(w)
    case('his plain yes on the Decide page is carried out with no session: the unit is queued (with a role, never its own priority) and the brief closed; a yes to nothing closes it; '
         'an answer in his own words is left for a session; each taken answer leaves a row for decisions.md',
         [u['id'] for u in w.added] == ['banner-whole'] and w.added[0]['role'] == 'lane' and 'priority' not in w.added[0]
         and w.taken == [('b1', 'banner-whole', ''), ('b2', '', 'nothing is built')] and len(w.rows) == 2 and 'wait for a session' in lines[-1], (lines, w.taken))

    # ---- one round ----
    class Dry(Fake):
        def found(self):
            class F:
                pass
            return F()
    calls = []
    real = (autoloop.found.take, autoloop.found.rota)
    autoloop.found.take, autoloop.found.rota = (lambda fw: calls.append('take') or []), (lambda fw: calls.append('rota') or 'found: queued the look x')
    try:
        w = Dry(tmp / 'e')
        w.answers = lambda: (_ for _ in ()).throw(RuntimeError('the Drive is not there'))
        lines = autoloop.round_(w)
    finally:
        autoloop.found.take, autoloop.found.rota = real
    case('one round runs every step, and one that fails (the Drive gone) does not stop the others: found take and rota still run',
         calls == ['take', 'rota'] and any('take_answers: failed' in l for l in lines) and 'found: queued the look x' in lines, lines)
    print(f'{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
