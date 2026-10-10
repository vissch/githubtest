#!/usr/bin/env python3
"""Tests of found tasks (found.py): whose turn it is to be looked at, and what becomes of what a look hands in.
Run from trench-warfare-3d/: python Tools/assetboard/test_found.py

git and the relay's queue are a stand-in here (Fake); the cards are real briefs in a temp folder. No look is run:
what a look writes is given as it would be handed in, the good and the bad."""
import datetime
import json
import os
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import briefs   # noqa: E402
import found    # noqa: E402

results = []
A, T = found.A, found.P + 'Tools/'
NO, YES = [False] * 5, [True] * 5


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:700]))


def task(title, files, risk=NO, change=NO, **more):
    return dict(dict(title=title, why='The flag is drawn before the trench changes hands, two frames early, in FlagView.Update line 40.', files=files, lane='show',
                     role='destruction-vfx-simulator', done_when='a PlayMode test that takes a trench and reads the flag on the frame it changes', risk=list(risk), change=list(change)), **more)


class Fake(found.World):
    def __init__(self, home):
        super().__init__(Path('.'), Path('.'), home)
        self.subs, self.have, self.q, self.done, self.added, self.files, self.refuse = [], set(), {}, set(), [], {}, False

    def subjects(self):
        return list(self.subs)

    def exists(self, path):
        return path in self.have

    def read_found(self, uid):
        return self.files.get(uid)

    def queued(self):
        return dict(self.q), set(self.done)

    def add(self, unit):
        if self.refuse:
            return False, 'relay: the unit file names no role'
        self.added.append(unit)
        self.q[unit['id']] = unit
        return True, 'queued %s. Board: pushed.' % unit['id']


def main():
    tmp = Path(tempfile.mkdtemp(prefix='tw-found-test-'))
    S = [dict(id='code:Sim/Core', kind='code', path=A + 'Sim/Core', at='c1'), dict(id='code:Presentation/Units', kind='code', path=A + 'Presentation/Units', at='c2'),
         dict(id='look:night', kind='look', path='the night look', at='2026w41'), dict(id='branch:lane/show/gym', kind='branch', path='lane/show/gym', at='b1'),
         dict(id='tools:x', kind='tools', path='x', at='t1')]

    # ---- whose turn ----
    order, state = [], {}
    for i in range(6):
        s = found.pick(S, state)
        order.append(s['id'] if s else None)
        if s:
            state[s['id']] = dict(at=s['at'], looked=f'2026-10-11 0{i}:00')
    case('turns: every kind has its first turn in the order of its weight (code, a view, a branch, the tools), then the kind that has had the fewest turns for its weight; then nothing is due',
         order == ['code:Presentation/Units', 'look:night', 'branch:lane/show/gym', 'tools:x', 'code:Sim/Core', None], order)
    S[0] = dict(S[0], at='c9')
    case('a subject that changed since it was looked at is due again, and only that one', found.pick(S, state)['id'] == 'code:Sim/Core' and found.pick(S[1:], state) is None)
    u = found.unit_of(S[2], '20261011')
    case('a look is a unit of the kind critique on the lane that is never landed, told to write one file and build nothing, with the ten score rows and the most tasks it may hand in',
         u['kind'] == 'critique' and u['lane'] == found.LANE and u['id'] == 'look-look-night-20261011' and u['role'] == 'hard-critic' and 'docs/found/look-look-night-20261011.json' in u['goal']
         and 'LOOK, do not build' in u['goal'] and '5. No script or test can check the result' in u['goal'] and 'At most 5 tasks' in u['goal'] and u['done_when'][:2] == ['python', '-c']
         and '[look-look-night-20261011]' in u['done_when'][2] and len(u['goal']) < 6000, u)

    # ---- what a look hands in ----
    have = {A + 'Presentation/Flag/FlagView.cs', A + 'Sim/Core/Match.cs', T + 'sweep.py', T + 'land.py', 'docs/reference/tasks.md'}
    ex = have.__contains__
    small = task('Raise the flag on the frame the trench changes hands', [A + 'Presentation/Flag/FlagView.cs'])
    bad = [(task('Fix the flag', ['Assets/Nowhere.cs']), 'does not have'), (task('', [A + 'Presentation/Flag/FlagView.cs']), 'title'),
           (dict(small, why='It is wrong.'), 'what is wrong'), (dict(small, files=[]), 'no file'), (dict(small, risk=[True, False]), 'five true/false'),
           (dict(small, change=['yes'] * 5), 'five true/false'), (dict(small, done_when=''), 'proved'), ('nonsense', 'not a task')]
    said = [found.check_task(t, ex) for t, _ in bad]
    case('a task is not taken when it names a file integration does not have, has no title, does not say what is wrong, names no file, does not answer the ten rows, or does not say how it is proved',
         found.check_task(small, ex) == '' and all(w in s for s, (_, w) in zip(said, bad)), said)
    rows = [(NO, NO, False), ([False, False, True, False, True], [True, False, True, False, False], False), ([True] + [False] * 4, NO, True), ([False, True] + [False] * 3, NO, True),
            (NO, [False, True, False, False, False], True), (NO, [False, False, False, True, False], True), ([False, False, True, True, True], NO, True), (NO, [True, False, True, False, True], True)]
    got = [found.score(task('x', [T + 'sweep.py'], r, c))[3] for r, c, _ in rows]
    case('major is what decisions.md calls major: hard to undo, can break the sim or the gate, a rule agents follow, a file format, or three points of risk or of change; two and two is not',
         got == [m for _, _, m in rows] and found.score(task('x', [T + 'sweep.py'], [True, True, False, False, False], [False, True, False, True, False]))[:3] == (2, 2, ['DANGEROUS', 'SYSTEMIC']), got)
    his = [found.score(task('x', [f]))[3] for f in (A + 'Sim/Core/Match.cs', T + 'land.py', T + 'relay/runner.py', T + 'assetboard/landq.py', found.P + 'gate.ps1', A + 'Net/Lockstep.cs', A + 'Data/Units.cs')]
    case('whatever a look answered, a task that touches the sim, the net code, the data, the gate, the landing tools or the relay is major',
         all(his) and not found.score(task('x', [A + 'Presentation/Flag/FlagView.cs']))[3] and not found.score(task('x', [T + 'sweep.py', 'docs/reference/tasks.md']))[3], his)
    handed = dict(subject='code:Presentation/Flag', looked_at=['x'], tasks=[
        small, task('On the frame the trench changes hands, raise the flag', [A + 'Presentation/Flag/FlagView.cs']), task('Hash the wind seed', [A + 'Sim/Core/Match.cs']),
        task('Sweep the temp captures after a gate', [T + 'sweep.py'], change=[False, True, False, False, False]), task('Fix it', ['nowhere.cs']),
        task('Flag pole height from the trench depth', [A + 'Presentation/Flag/FlagView.cs']), task('One more', [T + 'sweep.py'])])
    went = [(g, w) for _, g, w, _ in found.route(handed, 'look-code-flag-20261011', ex, [('', 'Sweep the temp captures after every gate run')])]
    case('one look: the small task is queued; the same finding in other words is dropped; the sim task and the one found before are a card and dropped; a missing file is dropped; five are read and the rest are not',
         [g for g, _ in went] == ['queue', 'dropped', 'card', 'dropped', 'dropped', 'dropped', 'dropped'] and 'found before' in went[1][1] and 'Sim/Core/Match.cs' in went[2][1]
         and 'found before' in went[3][1] and 'does not have' in went[4][1] and '5 tasks at most' in went[5][1], went)
    f = found.fix_unit(small, 'look-code-flag-20261011', 0)
    case('a small found task is a fix on a lane of its own, with a done-when written here (the lane holds a commit that names it): no command a look wrote is run',
         f['kind'] == 'fix' and f['lane'] == 'lane/show/' + f['id'] and f['role'] == 'destruction-vfx-simulator' and f['done_when'][:2] == ['python', '-c'] and f'[{f["id"]}]' in f['done_when'][2]
         and 'PlayMode test' in f['goal'] and 'PlayMode test' not in f['done_when'][2] and found.fix_unit(dict(small, role='rm -rf', lane='sim'), 'l-20261011', 1)['role'] == 'lane'
         and found.fix_unit(dict(small, lane='sim'), 'l-20261011', 1)['lane'].startswith('lane/sim/'), f)

    # ---- the two steps, end to end on the stand-in ----
    os.environ.update(TW_BRIEFS=str(tmp / 'briefs'), TW_NOTES=str(tmp / 'notes'))
    w = Fake(tmp / 'home')
    w.subs, w.have = S, have
    now = datetime.datetime(2026, 10, 11, 9, 0)
    first = found.rota(w, now)
    second = found.rota(w, now)
    look = w.added[0]['id']
    case('rota: the next look is queued, as a critique; while it is queued and not done no second look is put beside it',
         'queued the look' in first and w.added[0]['kind'] == 'critique' and 'no second one beside it' in second and len(w.added) == 1, (first, second))
    not_yet = found.take(w, now)
    w.done.add(look)
    w.files[look] = dict(handed, tasks=handed['tasks'][:4])
    lines = found.take(w, now)
    taken = found.load(tmp / 'home' / 'taken.json', [])
    cards = briefs.read_all(tmp / 'briefs')
    sim_card = [c for c in cards if 'wind' in c['title']]
    case('take: nothing is taken from a look that is not done; a done one has its small task queued as a fix, and each major one (the sim task, and the one that changes a tool agents follow) '
         'put to him as a card whose first option queues it, on a sim lane when it touches the sim',
         not_yet == [] and '4 tasks handed in, 1 queued, 2 are cards for him, 1 dropped' in lines[0] and [u['kind'] for u in w.added] == ['critique', 'fix']
         and len(cards) == 2 and len(sim_card) == 1 and sim_card[0]['options'][0]['then']['unit']['id'].startswith('found-hash-the-wind-seed') and sim_card[0]['lane'].startswith('lane/sim/')
         and 'MAJOR' in sim_card[0]['why'] and 'Sim/Core/Match.cs' in sim_card[0]['why'] and all('SYSTEMIC MAJOR' in c['why'] for c in cards if c not in sim_card)
         and [t['went'] for t in taken] == ['queue', 'dropped', 'card', 'card'], (lines, [t['went'] for t in taken], [c['why'] for c in cards]))
    again = found.take(w, now)
    case('a look is taken up once', again == [] and len(found.load(tmp / 'home' / 'taken.json', [])) == 4 and len(briefs.read_all(tmp / 'briefs')) == 2, again)
    third = found.rota(w, now)
    w.files[w.added[-1]['id']] = dict(subject='x', looked_at=[], tasks=[small])
    w.done.add(w.added[-1]['id'])
    lines = found.take(w, now)
    case('what one look found is not queued again when a later look finds it', 'queued the look' in third and '1 tasks handed in, 0 queued, 0 are cards for him, 1 dropped' in lines[0], lines)
    w.q.update({f'found-x-{i}': dict(kind='fix') for i in range(found.FOUND_MOST)})
    found.put(tmp / 'home' / 'taken.json', found.load(tmp / 'home' / 'taken.json', []) + [dict(went='queue', unit=f'found-x-{i}', title=f't{i}', key=f'k{i}') for i in range(found.FOUND_MOST)])
    full = found.rota(w, now)
    w.done.update(f'found-x-{i}' for i in range(5))
    case(f'no new look is queued while {found.FOUND_MOST} found tasks wait unstarted; once fixes are done it goes on', 'no new look until the fixes catch up' in full and 'queued the look' in found.rota(w, now), full)
    w2 = Fake(tmp / 'home2')
    w2.subs, w2.refuse = S, True
    case('a look the relay refuses is not marked as queued: its subject is tried again', 'was not queued' in found.rota(w2, now) and found.load(tmp / 'home2' / 'state.json', {}) == {})
    w3 = Fake(tmp / 'home3')
    w3.subs = S
    found.rota(w3, now)
    w3.done.add(w3.added[0]['id'])
    case('a look that is done and left no file that reads is said, and its subject is not looked at again until it changes',
         'left no file that reads' in found.take(w3, now)[0] and found.load(tmp / 'home3' / 'state.json', {})[S[1]['id']]['looked'], found.load(tmp / 'home3' / 'state.json', {}))
    print(f'{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    os.environ['TW_NOTES'] = tempfile.mkdtemp(prefix='tw-notes-test-')
    os.environ['TW_BRIEFS'] = tempfile.mkdtemp(prefix='tw-briefs-test-')
    os.environ['TW_CLICK_KEY'] = str(Path(tempfile.mkdtemp(prefix='tw-click-test-')) / 'click.key')
    sys.exit(main())
