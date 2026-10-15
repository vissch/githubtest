#!/usr/bin/env python3
"""Tests of the four steps the owner asked for on 2026-10-15 (loopsteps.py, run by autoloop.py). Run from
trench-warfare-3d/: python Tools/assetboard/test_loopsteps.py

Every case goes through autoloop.py's own steps (take_answers, copy_back, round_), never loopsteps.py alone, so each
one fails on the loop as it was before (bef63329d): his own words waited for a session, no idea was routed, a review
waited for a dry queue, and a unit that went nowhere was retaken by every run. The history of the night of 2026-10-15
is the material: relay-harness-rebase-b BLOCKED three times on one push, heroes-off-bench restarted four times on
relay 6 on work done two days before, and the main run of 13:51 stopped when done records were copied under it."""
import json
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import autoloop  # noqa: E402
import found  # noqa: E402
from test_autoloop import Fake, put, RUN, IDLE  # noqa: E402

results = []


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:900]))


class FF:
    """found.World in a folder: the review index and integration's head are set by the test."""

    def __init__(self, board, home):
        self.board, self.home = Path(board), Path(home)
        self.home.mkdir(parents=True, exist_ok=True)
        self.tip_now, self.diff, self.subs, self.added = 'T0', [], [], []

    def queued(self):
        q = {f.stem: json.loads(f.read_text(encoding='utf-8')) for f in (self.board / 'relay' / 'queue').glob('*.json')}
        return q, {f.stem for f in (self.board / 'relay' / 'done').glob('*.json')}

    def tip(self):
        return self.tip_now

    def changed(self, a, b):
        return list(self.diff)

    def reviews(self, tip, state=None):
        return [s for s in self.subs if s['kind'] == 'review']

    def subjects(self):
        return list(self.subs)

    def add(self, unit):
        self.added.append(unit)
        f = self.board / 'relay' / 'queue' / f'{unit["id"]}.json'
        f.write_text(json.dumps(unit), encoding='utf-8')
        return True, 'queued'

    def read_found(self, uid):
        return None

    def exists(self, p):
        return True


def world(tmp, name, busy=False):
    w = Fake(tmp / name, extras=(2, 6))
    w.ff = FF(w.main, w.home / 'found')
    w.found = lambda: w.ff
    if busy:                    # every relay runs: the queue stays as the test put it
        for b in [w.main, *w.extras.values()]:
            w.text[b] = RUN.replace('relay-four-checks', 'busy-' + Path(b).name)
    return w


WRITE = dict(id='2026-10-15-the-ridge-s-new-slope-test-measure-its-two-limit', title='The Ridge\'s new slope test: measure its two limits from the parapet, or loosen them for the floor',
             option='A', text='Measure both bars from the parapet instead, keeping the 0.5 m and 4.0 m limits as they are', said='', go='write', why='the option has no Then line',
             when='2026-10-15 00:15', note='2026-10-15-001545-pipe-idea-the-ridge-one-side-holds-the-h', lane='lane/show/pipe-idea-the-ridge', unit=None,
             notes=[dict(id='n', when='2026-10-15 00:15', text='A: Measure both bars from the parapet instead, keeping the 0.5 m and 4.0 m limits as they are')])
BUILD = json.dumps(dict(decision='build', id='ridge-slope-from-parapet', lane='lane/show/pipe-idea-the-ridge', role='env-simulator',
                        goal='Measure the slope test\'s two bars from the parapet, keeping the 0.5 m and 4.0 m limits as they are.',
                        done_when='a test measures both bars from the parapet and passes'))
STOPS = [('20261015-061529-20328', '2026-10-15T04:15:29Z', 'relay-harness-rebase-b', 'BLOCKED', 'the push was refused: lane/show/relay-harness is not a fast-forward'),
         ('20261015-073321-54588', '2026-10-15T05:33:21Z', 'relay-harness-rebase-b', 'BLOCKED', 'the push was refused: lane/show/relay-harness is not a fast-forward'),
         ('20261015-085213-55384', '2026-10-15T06:52:13Z', 'relay-harness-rebase-b', 'BLOCKED', 'the push was refused: lane/show/relay-harness is not a fast-forward')]


def main():
    tmp = Path(tempfile.mkdtemp(prefix='tw-loopsteps-test-'))

    # ---- 1. his own words become a unit ----
    w = world(tmp, 'w1')
    w.free, w.reply, w.ans = 1.0, 'Here it is:\n' + BUILD, [dict(WRITE)]
    lines = autoloop.take_answers(w)
    u = (w.added or [{}])[0]
    case('an answer in his own words is read by one leg and the unit it leads to is queued (his words in the goal, a role, a done-when the loop writes) and the brief closed with --queued, booked on the day',
         u.get('id') == 'ridge-slope-from-parapet' and u.get('role') == 'env-simulator' and 'keeping the 0.5 m and 4.0 m limits' in u.get('goal', '')
         and '[ridge-slope-from-parapet]' in u.get('done_when', [''])[-1] and w.taken == [(WRITE['id'], 'ridge-slope-from-parapet', '')]
         and len(w.legs) == 1 and w.booked and w.booked[-1]['station'] == 'autoloop' and w.booked[-1]['usd'] == 0.02 and len(w.rows) == 1, (lines, w.added, w.taken, w.booked))
    w = world(tmp, 'w2')
    w.free, w.ans = 1.0, [dict(WRITE, id='night-tracers', note='n9', text='Keep today\'s beams (the default this lands with)')]
    w.reply = json.dumps(dict(decision='nothing', why='He keeps the beams as they are: nothing is to be built.'))
    lines = autoloop.take_answers(w)
    case('an answer that builds nothing closes the brief with what the leg read, and queues nothing',
         w.added == [] and len(w.taken) == 1 and w.taken[0][2].startswith('Read by the loop: He keeps the beams'), (lines, w.taken))
    w = world(tmp, 'w3')
    w.free, w.ans = 1.0, [dict(WRITE)]
    w.reply = BUILD.replace('env-simulator', 'wizard')
    first = autoloop.take_answers(w)
    second = autoloop.take_answers(w)
    third = autoloop.take_answers(w)
    case('a reply that is no unit (a role no table has) is refused, never queued: tried once more, then left and said once, with no third leg',
         w.added == [] and w.taken == [] and len(w.legs) == 2 and 'tried again' in first[-1] and 'said once' in second[-1] and third == [], (first, second, third))
    w = world(tmp, 'w4')
    w.free, w.reply, w.ans = 0.0, BUILD, [dict(WRITE)]
    lines = autoloop.take_answers(w)
    case('no leg runs while the day\'s pace has nothing free: the answer waits, and says so', w.legs == [] and w.added == [] and 'nothing free' in lines[-1], lines)
    w = world(tmp, 'w5')
    w.free, w.reply = 1.0, BUILD
    w.ans = [dict(WRITE, why='his notes do not all pick the same option', option='B')]
    autoloop.take_answers(w)
    case('when his notes pick two options the brief closes on his last one (--option)', w.taken and w.taken[0][-1] == 'B', w.taken)

    # ---- 2. the ideas he said yes to ----
    w = world(tmp, 'i1')
    unitfile = tmp / 'idea-unit.json'
    unitfile.write_text(json.dumps(dict(id='idea-the-game-s-time-lapse', lane='lane/show/idea-time-lapse', goal='g', done_when=['python', '-c', '1'])), encoding='utf-8')
    w.routed_ideas = ([('2026-10-08-the-game-s-time-lapse', 'unit', 'idea-the-game-s-time-lapse', unitfile),
                       ('2026-10-08-a-man-killed-in-the-wire', 'item', 'idea-a-man-killed-in-the-wire', tmp / 'x.json')], dict(asked=['gate-x'], done=[], owed=[]), '')
    w.text[w.extras[2]] = w.text[w.extras[6]] = RUN
    autoloop.round_(w)
    case('with no main run going, the ideas he said yes to are routed (idearoute route and gates, for real) and a tool idea\'s unit is queued with a role',
         True in getattr(w, 'idea_calls', []) and any(a['id'] == 'idea-the-game-s-time-lapse' and a.get('role') == 'lane' for a in w.added), (getattr(w, 'idea_calls', None), w.added))
    w = world(tmp, 'i2')
    w.routed_ideas = ([('a', 'unit', 'idea-u', unitfile)], dict(asked=[], done=[], owed=[]), '')
    w.text[w.main] = RUN
    lines = autoloop.round_(w)
    case('while the main relay runs nothing is written on its board: the ideas wait, and say so',
         True not in getattr(w, 'idea_calls', []) and not any(a['id'] == 'idea-u' for a in w.added) and any('ideas:' in l and 'wait' in l for l in lines), lines)

    # ---- 3. code reviews when it is time ----
    rv = dict(id='review:u19-sim-combat', kind='review', path='u19 (A.cs)', at='T1', base='T0', drift=1, files=['A.cs'])
    w = world(tmp, 'r1', busy=True)
    put(w.main, 'queue', 'busy-1', lane='lane/show/busy', priority=10)        # the queue is not dry
    autoloop.round_(w)                                                        # marks where integration stands
    w.ff.tip_now, w.ff.diff, w.ff.subs = 'T1', ['A.cs'], [rv]
    lines = autoloop.round_(w)
    case('after a landing that changed a review unit\'s files, that unit\'s review is queued though the queue still has work',
         [a['id'] for a in w.ff.added if a.get('kind') == 'critique'] and 'u19' in w.ff.added[0]['id'], lines)
    w = world(tmp, 'r2', busy=True)
    put(w.main, 'queue', 'busy-1', lane='lane/show/busy', priority=10)
    for i in range(found.FIX_MOST):
        put(w.main, 'queue', f'rv-{i}', lane=f'lane/show/rv-{i}', role='review-fix')
    w.text[w.main] = RUN
    autoloop.round_(w)
    w.ff.tip_now, w.ff.diff, w.ff.subs = 'T1', ['A.cs'], [rv]
    w.free = 5.0
    lines = autoloop.round_(w)
    case('with 20 fixes unstarted no review is queued, after a landing or for the day: the ceiling holds',
         not any(a.get('kind') == 'critique' for a in w.ff.added), (lines, w.ff.added))
    w = world(tmp, 'r3', busy=True)
    put(w.main, 'queue', 'busy-1', lane='lane/show/busy', priority=10)
    w.text[w.main] = RUN
    w.ff.subs = [dict(rv, at='T0')]
    w.free = 5.0
    first = autoloop.round_(w)
    w.ff.queued = lambda: (lambda q: (q[0], q[1] | {a['id'] for a in w.ff.added}))(FF.queued(w.ff))     # the look ran
    w.ff.subs = [dict(rv, id='review:u20', at='T0')]
    same_day = autoloop.round_(w)
    w.clock += 86400
    next_day = autoloop.round_(w)
    looks = [a['id'] for a in w.ff.added if a.get('kind') == 'critique']
    case('one review a day while the pace has room, though the queue is not dry; not a second one the same day',
         len(looks) == 2 and 'u19' in looks[0] and 'u20' in looks[1], (first, same_day, next_day, looks))

    # ---- 4. no going over the same unit without progress ----
    w = world(tmp, 'a1')
    put(w.main, 'queue', 'relay-harness-rebase-b', lane='lane/show/relay-harness', priority=99)
    put(w.extras[2], 'queue', 'relay-harness-rebase-b', lane='lane/show/relay-harness')
    w.stop_records = STOPS
    first = autoloop.round_(w)
    again = autoloop.round_(w)
    case('relay-harness-rebase-b, BLOCKED three times with its lane and its base unchanged, is taken off every queue a runner reads (99 only sorts it last) and put to him in ONE brief; '
         'the next round writes no second brief and starts no relay for it',
         sorted(w.unqueued) == [('b2', 'relay-harness-rebase-b'), ('main', 'relay-harness-rebase-b')] and len(w.briefs_written) == 1
         and (w.home / 'parked' / 'relay-harness-rebase-b.json').exists() and w.started == [], (first, again, w.unqueued, w.briefs_written, w.started))
    w.shas = {'lane/show/relay-harness': 'L9'}
    back = autoloop.round_(w)
    case('a parked unit goes back on the queue by itself once its lane has new commits',
         any(a['id'] == 'relay-harness-rebase-b' for a in w.added) and any('back on the queue' in l for l in back), back)
    w = world(tmp, 'a2')
    put(w.main, 'queue', 'moving-1', lane='lane/show/moving', priority=40)

    class Moving(type(w)):
        def lane_sha(self, lane, at=''):
            return {'': 'L2', '2026-10-15T04:15:29Z': 'L1', '2026-10-15T05:33:21Z': 'L2'}.get(at, 'L2')
    w.__class__ = Moving
    w.stop_records = [(r, at, 'moving-1', v, y) for r, at, _, v, y in STOPS[:2]]
    lines = autoloop.round_(w)
    case('a unit whose lane got new commits between its two tries (the second try pushed work) is not parked: only tries with nothing changed between count',
         w.unqueued == [] and w.briefs_written == [], lines)
    w = world(tmp, 'a2b')
    put(w.main, 'queue', 'clash-1', lane='lane/show/clash', priority=40)
    w.stop_records = [(r, at, 'clash-1', 'FAIL', 'its lane lane/show/clash cannot be switched to: already used by worktree at work5') for r, at, _, _, _ in STOPS]
    autoloop.round_(w)
    case('a FAIL that is a clash between relays (its lane is out in another checkout) is not the unit\'s try and parks nothing', w.unqueued == [], w.unqueued)
    w = world(tmp, 'a2c')
    put(w.main, 'queue', 'relay-harness-rebase-b', lane='lane/show/relay-harness', priority=99)
    w.stop_records = STOPS
    w.text[w.main] = RUN
    under_run = autoloop.round_(w)
    n_under = len(w.unqueued)
    w.text[w.main] = IDLE
    autoloop.round_(w)
    case('no queue is written on a board whose relay runs: the unit is parked the first round that board is idle',
         n_under == 0 and w.unqueued == [('main', 'relay-harness-rebase-b')], (under_run, w.unqueued))
    w = world(tmp, 'a3')
    put(w.main, 'queue', 'once-1', lane='lane/show/once', priority=40)
    w.stop_records = STOPS[:1]
    autoloop.round_(w)
    case('one try that went nowhere is not parked (at most two)', w.unqueued == [], w.unqueued)
    w = world(tmp, 'a4')
    put(w.main, 'queue', 'relay-harness-rebase-b', lane='lane/show/relay-harness', priority=99)
    w.stop_records = STOPS
    autoloop.round_(w)
    w.ans = [dict(WRITE, id='brief-relay-harness-rebase-b', go='queue', note='nA', unit=dict(id='relay-harness-rebase-b', lane='lane/show/relay-harness', goal='g', done_when=['python', '-c', '1']))]
    autoloop.round_(w)
    w.ans = []
    after = autoloop.round_(w)
    case('his A on that brief puts it back and its tries start over: it is not parked again by the same old tries',
         len(w.unqueued) == 1 and (w.main / 'relay' / 'queue' / 'relay-harness-rebase-b.json').exists(), (after, w.unqueued))
    w.stop_records = STOPS + [('20261016-0%d0000-1' % i, '2026-10-16T0%d:00:00Z' % i, 'relay-harness-rebase-b', 'BLOCKED', 'the push was refused') for i in (1, 2)]
    w.text = {b: IDLE for b in w.text}                                    # the runs that took it again have stopped
    again = autoloop.round_(w)
    case('if it then goes nowhere twice more it is parked again with a brief of its own (not the closed one)',
         len(w.unqueued) >= 2 and len(w.briefs_written) == 2 and not list(w.main.glob('relay/queue/*.json')), (again, w.unqueued, w.briefs_written))
    w = world(tmp, 'a5')
    put(w.main, 'queue', 'relay-harness-rebase-b', lane='lane/show/relay-harness', priority=99)
    w.stop_records = STOPS
    autoloop.round_(w)
    w.shas = {'lane/show/relay-harness': 'L9'}
    autoloop.round_(w)                                                    # back by itself
    w.ans = [dict(WRITE, id='brief-relay-harness-rebase-b', go='nothing', note='nB', says='Nothing more is built')]
    w.text = {b: IDLE for b in w.text}
    autoloop.round_(w)
    w.ans = []
    autoloop.round_(w)
    case('his B on its brief after it came back by itself takes it off the queue again, and it stays off',
         not list(w.main.glob('relay/queue/*.json')) + list(w.extras[2].glob('relay/queue/*.json')) and len(w.unqueued) >= 2, w.unqueued)

    # the real unqueue, on a real git board: the file off the queue, committed alone
    import subprocess
    g = tmp / 'gitboard'
    (g / 'relay' / 'queue').mkdir(parents=True)
    (g / 'relay' / 'done').mkdir(parents=True)
    git = lambda *a: subprocess.run(['git', '-C', str(g), *a], capture_output=True, text=True)    # noqa: E731
    git('init', '-q')
    git('config', 'user.email', 't@t')
    git('config', 'user.name', 't')
    (g / 'relay' / 'queue' / 'stuck-1.json').write_text('{}', encoding='utf-8')
    (g / 'other.txt').write_text('x', encoding='utf-8')
    git('add', '-A')
    git('commit', '-q', '-m', 'start')
    (g / 'staged.txt').write_text('y', encoding='utf-8')
    git('add', 'staged.txt')                                              # somebody else's staged file
    rw = autoloop.World(tmp / 'no-main', extras=(), home=tmp / 'rhome')
    ok = rw.unqueue(g, 'stuck-1')
    case('the real unqueue takes the file off the queue in a commit of its own and leaves another staged file alone',
         ok and not (g / 'relay' / 'queue' / 'stuck-1.json').exists() and 'parked by the loop' in git('log', '-1', '--format=%s').stdout
         and git('show', '--name-only', '--format=', 'HEAD').stdout.split() == ['relay/queue/stuck-1.json'] and 'A  staged.txt' in git('status', '--porcelain').stdout,
         (ok, git('log', '-2', '--stat').stdout, git('status', '--porcelain').stdout))

    # an id a board holds is never reused for his words
    w = world(tmp, 'w6')
    put(w.main, 'done', 'ridge-slope-from-parapet')
    w.free, w.reply, w.ans = 1.0, BUILD, [dict(WRITE)]
    autoloop.take_answers(w)
    case('when the leg names an id a board has done or queued already, his unit is queued under a free one (-2), not closed on the old unit',
         [a['id'] for a in w.added] == ['ridge-slope-from-parapet-2'] and w.taken[0][1] == 'ridge-slope-from-parapet-2', (w.added, w.taken))

    # heroes-off-bench: done on the main board since 2026-10-13, still open on relay 6's board
    w = world(tmp, 'h1')
    put(w.main, 'done', 'heroes-off-bench')
    put(w.extras[6], 'queue', 'heroes-off-bench', lane='lane/show/heroes-off-bench')
    w.text[w.main] = RUN
    lines = autoloop.round_(w)
    case('heroes-off-bench, done on the main board, is not restarted on relay 6: its done record is copied to that board and the relay is not started',
         (w.extras[6] / 'relay' / 'done' / 'heroes-off-bench.json').exists() and 6 not in w.started, (lines, w.started))

    # the main run of 13:51 stopped when done records were copied under it
    w = world(tmp, 'c1')
    put(w.extras[2], 'done', 'look-code-editor-ui-20261015')
    w.text[w.main] = RUN
    lines = autoloop.copy_back(w)
    case('no done record is copied to the main board while its relay runs (a leg that sees relay/done move ends the run)',
         not (w.main / 'relay' / 'done' / 'look-code-editor-ui-20261015.json').exists() and w.pushed == [], (lines, w.pushed))
    w.text[w.main] = IDLE
    lines = autoloop.copy_back(w)
    case('once it has stopped they are copied', (w.main / 'relay' / 'done' / 'look-code-editor-ui-20261015.json').exists(), lines)

    # a close refused after the unit was queued: the next round closes it with that unit, writes its row, runs no second leg
    w = world(tmp, 'w7')
    w.free, w.reply, w.ans = 1.0, BUILD, [dict(WRITE)]
    real_take = w.take
    w.take = lambda a, **k: (_ for _ in ()).throw(ValueError('the unit is not on the board\'s origin/main yet'))
    autoloop.take_answers(w)
    w.take = real_take
    autoloop.take_answers(w)
    case('a brief whose close was refused after its unit was queued is closed next round with that unit and its row, with no second leg',
         len(w.legs) == 1 and [a['id'] for a in w.added] == ['ridge-slope-from-parapet'] and w.taken == [(WRITE['id'], 'ridge-slope-from-parapet', '')] and len(w.rows) == 1,
         (w.legs, w.added, w.taken, w.rows))

    # an original and its -b copy are never both worked
    w = world(tmp, 's1')
    put(w.main, 'queue', 'relay-proof-to-drive', lane='lane/show/relay-proof-to-drive', priority=99)
    put(w.extras[2], 'queue', 'relay-proof-to-drive', lane='lane/show/relay-proof-to-drive')
    put(w.main, 'queue', 'relay-proof-to-drive-b', lane='lane/show/relay-proof-to-drive-b', priority=20)
    w.text[w.main] = RUN
    lines = autoloop.round_(w)
    extra_ids = sorted(p.stem for b in w.extras.values() for p in b.glob('relay/queue/*.json'))
    case('when relay-proof-to-drive-b is queued, relay-proof-to-drive goes off the extra relay (no brief) and only the -b is worked there, never both',
         ('b2', 'relay-proof-to-drive') in w.unqueued and w.briefs_written == [] and extra_ids == ['relay-proof-to-drive-b'], (lines, w.unqueued, extra_ids))
    w = world(tmp, 's2')
    put(w.extras[2], 'queue', 'proof-x', lane='lane/show/proof-x')
    put(w.main, 'queue', 'proof-x-b', lane='lane/show/proof-x-b', priority=20)
    w.text[w.main] = RUN
    w.text[w.extras[2]] = RUN
    lines = autoloop.feed(w)
    case('while a relay works x, its copy x-b is not fed to another relay', not list(w.extras[6].glob('relay/queue/*.json')), lines)
    # a relay holding only a unit that went nowhere is fed other work
    w = world(tmp, 's3')
    put(w.main, 'queue', 'relay-four-checks', lane='lane/show/relay-four-checks', priority=99)
    put(w.extras[2], 'queue', 'relay-four-checks', lane='lane/show/relay-four-checks')
    put(w.main, 'queue', 'fresh-1', lane='lane/show/fresh', priority=30)
    w.stop_records = STOPS[:1]
    w.stop_records = [(r, at, 'relay-four-checks', 'BLOCKED', 'no result') for r, at, _, _, _ in STOPS[:1]]
    w.text[w.main] = RUN
    lines = autoloop.round_(w)
    got = sorted(p.stem for p in w.extras[2].glob('relay/queue/*.json'))
    case('an idle extra relay whose only unit went nowhere (BLOCKED, nothing changed) does not count it as work: it goes off that board and the relay gets other work',
         got == ['fresh-1'] and ('b2', 'relay-four-checks') in w.unqueued and w.prios.get('relay-four-checks') == 50, (lines, got, w.prios))
    w = world(tmp, 's4')
    put(w.main, 'queue', 'cover-pass', lane='lane/show/cover-pass', priority=30)
    put(w.main, 'queue', 'cover-pass-c', lane='lane/show/cover-art', priority=30)
    autoloop.round_(w)
    case('two units that only share a name ending (cover-pass, cover-pass-c on another lane) are not copies: neither is parked',
         w.unqueued == [], w.unqueued)
    w = world(tmp, 's5')
    put(w.extras[2], 'queue', 'relay-four-checks', lane='lane/show/relay-four-checks')
    put(w.main, 'queue', 'relay-four-checks-b', lane='lane/show/relay-four-checks-b', priority=20)
    w.text[w.extras[2]] = RUN
    w.text[w.extras[6]] = RUN.replace('relay-four-checks', 'other')
    lines = autoloop.feed(w)
    case('while an extra relay works x, the main relay is not started on its copy x-b', 1 not in w.started, (lines, w.started))

    # restarts: a run that worked and then failed is one attempt; a start the hold refused is no start
    w = world(tmp, 't1')
    put(w.extras[2], 'queue', 'slow-1', lane='lane/show/slow')
    w.text[w.main] = w.text[w.extras[6]] = RUN
    stop = 'NOT RUNNING. Last run 20261015-19%02d00-1 (desktop): leg 03 ended TIMEOUT (3 legs, 1 unit: 1 FAIL).'
    w.text[w.extras[2]] = stop % 10
    autoloop.feed(w)
    w.text[w.extras[2]] = stop % 40
    w.clock += autoloop.STARTS[0] + 1
    autoloop.feed(w)
    w.text[w.extras[2]] = stop % 59
    w.clock += autoloop.STARTS[0] + 1
    autoloop.feed(w)
    case('a relay whose runs worked and then timed out is restarted after the usual wait each time (relay 5, 19:30): the wait doubles only for a relay that stops at once',
         w.started == [2, 2, 2], w.started)
    w = world(tmp, 't2')
    put(w.main, 'queue', 'main-1', lane='lane/show/main-1', priority=20)
    w.text[w.extras[2]] = w.text[w.extras[6]] = RUN
    w.text[w.main] = 'NOT RUNNING. Last run 20261015-180712-71488 (desktop): leg 02 broke a rule: the board changed (2 legs, 1 unit: 1 FAIL).'
    real_start = w.start
    w.start = lambda n: 'not started: HELD by master-2026-10-15 until 22:06'
    held = autoloop.feed(w)
    w.start = real_start
    w.clock += 300
    freed = autoloop.feed(w)
    case('a main relay start that its hold refused is not counted: once the hold is gone it is started the next round, not an hour later',
         w.started == [1] and 'HELD' in held[-1], (held, freed, w.started))

    # the day's pace as relay.py day prints it (ledger.pace_line), in percent and in dollars
    pp = autoloop.parse_pace
    case('the pace line is read in percent and dollars; a spent cap or a day ahead is nothing free; no line or a new wording is unknown (no leg runs)',
         pp('Pace: 7.0% of the week allowed by 15:11, about 4.6% of it free.') == 4.6 and pp('Pace: $40.00 allowed by 15:11, $4.50 of it free.') == 4.5
         and pp('Pace: 7.0% of the week allowed by 15:11. The day\'s cap is spent.') == 0.0 and pp('Pace: 7% allowed by 1:00, and the day is about 1.2% ahead of that.') == 0.0
         and pp('Today: about 2.3%') is None and pp('Pace: something new') is None)

    print(f'{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
