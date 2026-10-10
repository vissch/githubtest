#!/usr/bin/env python3
"""Tests of the Runs page's rules. Run from trench-warfare-3d/: python Tools/assetboard/test_runs.py

What a run is and what is worked out of its records (src_runs.py), which decision briefs are a run's, the run
reader's report and briefs and when a run is read (runreport.py), the page's own rules (runs.js, under node) and the
page as the watcher writes it (ops.py). Every case is built from records written here, the board among them (a git
repository made in a temp folder); nothing of the owner's is read and no session is started.
"""
import datetime
import io
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
TMP = Path(tempfile.mkdtemp(prefix='tw-runs-test-'))
# no case writes into the owner's own folders or starts a reading: set before anything reads them
os.environ.update(TW_TASKS=str(TMP / 'root'), TW_FEEDBACK=str(TMP / 'inbox'), TW_NOTES=str(TMP / 'notes'), TW_BRIEFS=str(TMP / 'briefs'), TW_TASKBRIEFS=str(TMP / 'taskbriefs'), TW_TASKBRIEF_OFF='1',
                  TW_RUNREPORTS=str(TMP / 'reports'), TW_RUNREPORT_OFF='1', TW_BOARD=str(TMP / 'no-board'))
for k in ('TW_RUNREPORT_READING', 'TW_RUNREPORT_GIVEN', 'TW_RUNREPORT_SCRATCH'):
    os.environ.pop(k, None)
import briefs     # noqa: E402
import notes      # noqa: E402
import ops        # noqa: E402
import runreport  # noqa: E402
import src_runs   # noqa: E402

results = []
NOW = int(time.time())
# a run is named by when it began (this station's clock) and the runner's pid: these began 9100, 20100, 30100 and 90100 seconds ago
A, B, C, D = (time.strftime('%Y%m%d-%H%M%S', time.localtime(NOW - ago)) + f'-{pid}' for ago, pid in ((9100, 11300), (20100, 23060), (30100, 40656), (90100, 46128)))
PNG = bytes.fromhex('89504e470d0a1a0a0000000d49484452000000010000000108060000001f15c4890000000d49444154789c6360000002000001e221bc330000000049454e44ae426082')


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:900]))


def utc(sec):
    return datetime.datetime.fromtimestamp(sec, datetime.timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ')


def leg(run, n, unit, phase='execute', report='RESULT: done - it is built.\n\nCHANGED:\n- one thing\n- another\n  on two lines\n\nNEXT: the next part.', ago=5000, **more):
    d = dict(run=run, leg=n, unit=unit, role='review-fix', source='lane', phase=phase, lane='lane/show/review-fixes', model='opus', ran_model='claude-opus-5', effort='low', state='DONE',
             started_at=utc(NOW - ago), finished_at=utc(NOW - ago + 600), seconds=600, cost_usd=1.5, report=report, resumed=0)
    d.update(more)
    return d


def records():
    """A board's records: run A (three units: passed, blocked with no NEEDS YOU line, cut off; a leg that asks, a leg
    that asks nothing in three ways), run B (one unit, an ask), a start that ran no leg (C), a proof, and an older
    run D whose lane a brief is about."""
    f = {}
    for n, d in enumerate([
            leg(A, 1, 'rv-one', 'plan', 'The plan is written.\n\n**RESULT: done** - the plan passes.\nNEEDS YOU: nothing. One unread message for this machine.', ago=9000, effort='high', cost_usd=2.0),
            leg(A, 2, 'rv-one', report='RESULT: done - fixed, pushed (7861fe1857).\nNEEDS YOU: the Banner has no shield: add a plate, or move another part?\nCHANGED:\n- `A.cs:3` now lifts\nNEXT: nothing', ago=8000),
            leg(A, 3, 'rv-two', 'plan', 'RESULT: blocked - the crest trench sits 6 m behind the crest.\nCHANGED:\n- nothing', ago=7000, lane='lane/sim/ridge'),
            leg(A, 4, 'rv-three', 'plan', 'RESULT: done - planned.\nNEEDS YOU: none', ago=6000, cost_usd=None)], 1):
        f[f'relay/desktop/legs/{A}-{n:02d}.json'] = d
    f[f'relay/desktop/stops/{A}.json'] = dict(run=A, started_by='laptop', stopped_at=utc(NOW - 5000), reason='stopped by the owner (relay.py stop)', detail='', legs=4, refusals=3,
                                               units={'rv-one': 'PASS', 'rv-two': 'BLOCKED'}, code='80a415e63ff4', day_pct=13.6, day_budget_pct=25.0)
    f[f'relay/desktop/legs/{B}-01.json'] = leg(B, 1, 'rv-b', report='RESULT: done - ok.\nNEEDS YOU: one call for later: keep the rule of 28 September, or drop it?', ago=20000, lane='lane/sim/review-fixes-2')
    f[f'relay/desktop/stops/{B}.json'] = dict(run=B, stopped_at=utc(NOW - 19000), reason="leg 01 the run's 8 hours are up (mid-leg)", legs=1, units={'rv-b': 'PASS'})
    f[f'relay/desktop/stops/{C}.json'] = dict(run=C, stopped_at=utc(NOW - 30000), reason='error: git switch -q lane/x failed', legs=0, units={})
    f['relay/desktop/stops/proof-wait-1.json'] = dict(run='proof-wait-1', stopped_at=utc(NOW - 100), reason='nothing left to do', legs=1)
    f['relay/desktop/legs/proof-wait-1-01.json'] = leg('proof-wait-1', 1, 'proof')
    f[f'relay/desktop/legs/{D}-01.json'] = leg(D, 1, 'idea-ridge--battlefield--5723c4f5', role='env-simulator', source='pipeline', lane='lane/show/pipe-ridge', ago=90000)
    f[f'relay/desktop/stops/{D}.json'] = dict(run=D, stopped_at=utc(NOW - 89000), reason='nothing left to do', legs=1, units={'idea-ridge--battlefield--5723c4f5': 'PASS'})
    f['relay/queue/rv-one.json'] = dict(id='rv-one', lane='lane/show/review-fixes', role='review-fix', goal='Mend what the second reader found.', done_when=['the test passes'])
    f['relay/done/rv-one.json'] = dict(id='rv-one', lane='lane/show/review-fixes', head='7861fe1857aa00', done_at=utc(NOW - 7000))
    f[f'relay/proposals/{A}-04.md'] = '# Proposals\n\n- mechanical: check the report shape sooner.\n'
    return f


def reading():
    """What is worked out of the records."""
    p = src_runs.parts('Leg complete.\n\n**RESULT: blocked** - the crest is wrong.\n\nNEEDS YOU: pick one of three.\nMore of it.\nCHANGED:\n- one\n  and its second line\n* two\nNEXT: wait')
    case('runs: a leg\'s report is read by its parts, wherever they stand, a bullet with its second line, the verdict from the first line that starts with RESULT',
         p == dict(result='the crest is wrong.', needs='pick one of three. More of it.', changed=['one and its second line', 'two'], next='wait') and src_runs.said('Done.\n\n- RESULT: Blocked - x') == 'blocked'
         and src_runs.said('all done, the result is fine') == '', p)
    q = src_runs.parts('RESULT: done - ok\nNEEDS YOU: pick the trench.\nNote: it is 6 m back.\nCHANGED:\n- Next to the trench the wire is cut\n- Changed the wire\n\u2022 2 files moved\n  0 tests fail\n\nNotes: the gate was flaky.\n\nNEXT: part two')
    case('runs: a part begins at its word in capitals with its colon, not at a bullet that starts "Next" or "Changed" or a line that starts "Note:"; a bullet may start with a digit or a dot; the paragraph after the bullets is not one',
         q == dict(result='ok', needs='pick the trench. Note: it is 6 m back.', changed=['Next to the trench the wire is cut', 'Changed the wire', '2 files moved 0 tests fail'], next='part two')
         and src_runs.leg_row(dict(leg=1, report='RESULT done, the plan passes.'))['result'] == 'the plan passes.', q)
    asks_nothing = ['nothing', 'Nothing.', 'none', 'No.', 'n/a', 'nothing needed', 'Nothing is needed.', 'nothing. One unread message for this machine', 'nothing (post office: 1 unread)']
    case('runs: a leg that asks nothing, however it says so, asks nothing; one that starts with "no shield" asks',
         all(src_runs.parts(f'RESULT: done\nNEEDS YOU: {t}\n')['needs'] == '' for t in asks_nothing) and src_runs.parts('RESULT: done\nNEEDS YOU: no shield on the model: your call')['needs'].startswith('no shield'),
         [t for t in asks_nothing if src_runs.parts(f'RESULT: done\nNEEDS YOU: {t}\n')['needs']])
    kinds = {'nothing left to do': 'done', "the run's 8 hours are up": 'hours', "leg 22 the run's 8 hours are up (mid-leg)": 'hours', 'leg 04 broke a rule: it pushed to integration': 'leg', 'the leg cap (40) is reached': 'legs', 'stopped by the owner (relay.py stop)': 'asked',
             "the day's budget is spent (13% of 11%)": 'budget', "the day's pace lets it start at 14:00": 'pace', 'the work checkout cannot be used: dirty': 'checkout', 'error: git switch failed': 'error',
             'unit x left uncommitted work in the checkout': 'uncommitted', '3 units in a row brought no result and no pushed code': 'no-result', '2 units in a row whose lane cannot be switched to': 'lane', 'something new': 'other', '': ''}
    case('runs: why a run ended is one word, from the relay\'s own sentences', all(src_runs.kind_of(r) == k for r, k in kinds.items()), {r: src_runs.kind_of(r) for r, k in kinds.items() if src_runs.kind_of(r) != k})
    runs = src_runs.collect(records(), NOW)
    a = runs[0]
    u = {x['id']: x for x in a['units']}
    case('runs: the newest first; a proof is no run; a start that ran no leg is listed between the runs, marked', [r['id'] for r in runs] == [A, B, C, D] and [r['empty'] for r in runs] == [False, False, True, False], [r['id'] for r in runs])
    case('runs: a run with no start in its stop record began at the time in its name, not at its first leg (which may have waited for the pace of the day); its cost is its legs\' (a leg with none is counted as unpriced); it ended when it was stopped',
         a['started'] == NOW - 9100 and a['ended'] == NOW - 5000 and a['usd'] == 5.0 and a['unpriced'] == 1 and a['legs'] == 4 and a['kind'] == 'asked' and a['state'] == 'ended' and a['station'] == 'desktop', {k: a[k] for k in ('started', 'ended', 'usd', 'unpriced', 'kind')})
    case('runs: a unit has its verdict from the stop record, its legs with who ran them, its goal from the queue and its lane\'s head once it passed; a unit the run ended on has none',
         [x['id'] for x in a['units']] == ['rv-one', 'rv-two', 'rv-three'] and u['rv-one']['verdict'] == 'PASS' and u['rv-one']['head'] == '7861fe1857' and u['rv-one']['goal'].startswith('Mend') and u['rv-one']['usd'] == 3.5
         and [(l['phase'], l['model'], l['effort'], l['said']) for l in u['rv-one']['legs']] == [('plan', 'claude-opus-5', 'high', 'done'), ('execute', 'claude-opus-5', 'low', 'done')]
         and u['rv-two']['verdict'] == 'BLOCKED' and u['rv-two']['lane'] == 'lane/sim/ridge' and u['rv-three']['verdict'] == '' and u['rv-three']['legs'][0]['usd'] is None, u)
    case('runs: what a leg asked of the owner is listed with its leg and unit; a leg that asked nothing is not; a unit that ended blocked without asking is, with what it said',
         [(x['key'], x['unit'], x['kind']) for x in a['asks']] == [('02', 'rv-one', 'needs'), ('03', 'rv-two', 'blocked')] and 'no shield' in a['asks'][0]['text'] and 'crest trench' in a['asks'][1]['text'], a['asks'])
    case('runs: a leg names the commits its report names until the relay records them; a retrospective\'s proposals are the run\'s',
         u['rv-one']['legs'][1]['shas'] == ['7861fe1857'] and u['rv-one']['legs'][1]['changed'] == ['`A.cs:3` now lifts'] and a['proposals'][0]['leg'] == 4 and 'report shape' in a['proposals'][0]['text'], u['rv-one']['legs'][1])
    case('runs: the last N are the last N that ran a leg (a start that failed between them is listed and not counted), and the list does not end on a start that failed',
         [r['id'] for r in src_runs.collect(records(), NOW, most=2)] == [A, B] and [r['id'] for r in src_runs.collect(records(), NOW, most=3)] == [A, B, C, D], [r['id'] for r in src_runs.collect(records(), NOW, most=3)])
    # what the relay will write itself (lane/show/relay-run-record) is taken as written
    f = records()
    f[f'relay/desktop/stops/{A}.json'].update(started_at=utc(NOW - 9500), reason_kind='hours', asked_why='the watcher\'s time was up', now_on='rv-three', problems={'rv-two': ['the proof found the trench behind the crest']}, hours=12)
    f[f'relay/desktop/legs/{A}-02.json'].update(needs_you='', said='failed', commits=[dict(sha='abcdef1234567', subject='show: wire stands')], commits_more=2)
    f['relay/desktop/live/20261010-010101-1.json'] = dict(run='20261010-010101-1', started_at=utc(NOW - 1200), started_by='watch', code='abc', hours=12, units={'u1': 'PASS'}, now_on=dict(unit='u2', phase='plan', leg=3), beat=utc(NOW - 60))
    f['relay/desktop/legs/20261010-010101-1-01.json'] = leg('20261010-010101-1', 1, 'u1', ago=1100)
    new = src_runs.collect(f, NOW)
    a2, live = new[1], new[0]
    case('runs: once the relay records them itself, a run\'s start, why it ended, what a leg asked, its verdict and its commits are the record\'s, and a unit that failed says why',
         a2['started'] == NOW - 9500 and a2['kind'] == 'hours' and a2['asked_why'].startswith('the watcher') and a2['now_on'] == 'rv-three' and a2['units'][1]['why'] == ['the proof found the trench behind the crest']
         and a2['units'][0]['legs'][1]['said'] == 'failed' and a2['units'][0]['legs'][1]['commits'] == [dict(sha='abcdef1234', subject='show: wire stands')] and a2['units'][0]['legs'][1]['more'] == 2
         and [x['key'] for x in a2['asks']] == ['03'], (a2['kind'], a2['asks']))
    case('runs: a run that is going is listed from its live record, with the unit it is on and the verdicts so far',
         live['state'] == 'going' and live['now_on'] == 'u2' and live['started'] == NOW - 1200 and live['heard'] == NOW - 60 and [x['verdict'] for x in live['units']] == ['PASS'], live)
    f[f'relay/desktop/legs/{B}-01.json'] = dict(f[f'relay/desktop/legs/{B}-01.json'], leg='one', seconds='long')
    f[f'relay/desktop/stops/{D}.json'] = dict(f[f'relay/desktop/stops/{D}.json'], units=['not', 'a', 'map'])
    case('runs: a record that is not one costs its own run and no other', [r['id'] for r in src_runs.collect(f, NOW)] == ['20261010-010101-1', A], [r['id'] for r in src_runs.collect(f, NOW)])
    return runs


def stamped(when, **more):
    b = dict(id=more.pop('id'), title=more.pop('title', 'A decision'), asked=time.strftime('%Y-%m-%d %H:%M', time.localtime(when)), state='open', what_for='For something.', options=[dict(key='A', text='Yes'), dict(key='B', text='No')], pick='A', why='w', evidence=[], lane='')
    b.update(more)
    return b


def decisions():
    """Which decision briefs are a run's."""
    every = [stamped(NOW - 4000, id='raised', raised_by=dict(run=A, unit='rv-two', leg=3)),
             stamped(NOW - 80000, id='step', step=dict(item='idea-ridge', stage='battlefield', job='idea-ridge--battlefield--5723c4f5')),
             stamped(NOW - 99000, id='from', state='answered', answer=dict(option='A', when='2026-10-07 10:00', queued='rv-b')),
             stamped(NOW - 18000, id='guess', lane='lane/sim/review-fixes-2'),
             stamped(NOW - 18000, id='early', lane='lane/show/review-fixes'),
             stamped(NOW - 4000, id='both', lane='lane/show/review-fixes', raised_by=dict(run=B, unit='rv-b', leg=1)),
             stamped(NOW - 400000, id='old', lane='lane/show/pipe-ridge'), stamped(NOW - 3000, id='nobody', lane='lane/show/elsewhere')]
    runs = src_runs.briefs_of(src_runs.collect(records(), NOW), every, {A: dict(sig='stale', asks=[dict(key='02', no='a chore')])}, shown=['raised'])
    got = {r['id']: [(b['id'], b['how']) for b in r['briefs']] for r in runs}
    case('runs: a brief is a run\'s when it says so, when it is the step of a job the run worked, when his answer queued a unit the run worked, and, as a guess, when it is about a lane the run worked and was asked soon after',
         got == {A: [('raised', 'raised')], B: [('both', 'raised'), ('guess', 'lane'), ('from', 'from')], C: [], D: [('step', 'step')]}, got)
    case('runs: a brief is listed on one run only, a guess never takes it from the run that raised it; one asked before a lane\'s first run, or long after its last, is nobody\'s',
         not any(b['id'] in ('early', 'old', 'nobody') for r in runs for b in r['briefs']), got)
    case('runs: what waits on him from a run is its own open briefs and what a leg asked that nothing holds; a guess and an answer of his are not counted; a report of another state of the run marks nothing',
         [r['waits'] for r in runs] == [3, 2, 0, 1] and runs[0]['briefs'][0]['shown'] is True and runs[0]['briefs'][0]['leg'] == 3 and not runs[0]['asks'][0]['no'], [r['waits'] for r in runs])
    # as it was on 2026-10-10: a brief about what a leg of an earlier run asked, written while a later run worked the same lane
    f = records()
    late = time.strftime('%Y%m%d-%H%M%S', time.localtime(NOW - 18500)) + '-777'
    f[f'relay/desktop/legs/{late}-01.json'] = leg(late, 1, 'rv-later', ago=18400, lane='lane/sim/review-fixes-2')
    f[f'relay/desktop/stops/{late}.json'] = dict(run=late, stopped_at=utc(NOW - 17000), reason='nothing left to do', legs=1, units={'rv-later': 'PASS'})
    two = src_runs.briefs_of(src_runs.collect(f, NOW), [stamped(NOW - 18000, id='guess', lane='lane/sim/review-fixes-2')], {})
    where = {r['id']: [b['id'] for b in r['briefs']] for r in two if r['briefs']}
    case('runs: a guess goes to the run where a unit on that lane asked something no brief holds, not to the later run that was going when the brief was written; that run counts it apart from what waits',
         where == {B: ['guess']} and [(r['waits'], r['maybe']) for r in two if r['id'] in (late, B)] == [(0, 0), (1, 1)], where)
    # a newer run's report names a brief an older run stamped: it stays the older run's
    named = src_runs.collect(records(), NOW)
    named = src_runs.briefs_of(named, every, {A: dict(sig=named[0]['sig'], asks=[dict(key='02', brief='both'), dict(key='03', brief='raised')])})
    case('runs: a brief a run stamped stays that run\'s when a newer run\'s report names it for an ask of its own', [b['id'] for b in named[1]['briefs'] if b['how'] == 'raised'] == ['both'] and [b['id'] for b in named[0]['briefs']] == ['raised'], [[b['id'] for b in r['briefs']] for r in named])
    rep = {A: dict(sig=runs[0]['sig'], asks=[dict(key='02', no='a chore for an agent'), dict(key='03', brief='raised')])}
    again = src_runs.briefs_of(src_runs.collect(records(), NOW), every, rep)
    case('runs: once a report says what became of each ask (a brief, or not his), the run\'s asks say so and only its open brief waits', again[0]['waits'] == 1 and [(a['brief'], a['no']) for a in again[0]['asks']] == [('', 'a chore for an agent'), ('raised', '')], again[0]['asks'])


def git(cwd, *args):
    return subprocess.run(['git', '-C', str(cwd), *args], capture_output=True, env=dict(os.environ, GIT_AUTHOR_NAME='t', GIT_AUTHOR_EMAIL='t@t', GIT_COMMITTER_NAME='t', GIT_COMMITTER_EMAIL='t@t'))


def a_board():
    """A pipeline board as a clone has it: the records on origin/main, the checkout behind and without them."""
    board = TMP / 'board'
    board.mkdir()
    git(board, 'init', '-q', '-b', 'main')
    (board / 'README.md').write_text('board\n', encoding='utf-8')
    git(board, 'add', '-A')
    git(board, 'commit', '-q', '-m', 'first')
    for n, d in records().items():
        (board / n).parent.mkdir(parents=True, exist_ok=True)
        (board / n).write_text(d if isinstance(d, str) else json.dumps(d), encoding='utf-8')
    (board / 'evidence' / 'idea-ridge' / 'battlefield').mkdir(parents=True)
    (board / 'evidence' / 'idea-ridge' / 'battlefield' / 'shown.png').write_bytes(PNG)
    (board / 'evidence' / 'idea-ridge' / 'battlefield' / 'critic-r1.md').write_text('score 80\n', encoding='utf-8')
    git(board, 'add', '-A')
    git(board, 'commit', '-q', '-m', 'relay: run')
    git(board, 'update-ref', 'refs/remotes/origin/main', 'HEAD')
    git(board, 'reset', '-q', '--hard', 'HEAD~1')           # the checkout is behind, as the laptop's is
    return board



def off_the_board(board):
    R = src_runs.read(board, [], {}, now=NOW)
    case('runs: the records are read off the board\'s origin/main with git, from a checkout that is behind and holds none of them; a board that is not there says so and gives no run',
         not (board / 'relay').exists() and [r['id'] for r in R['runs']] == [A, B, C, D] and R['board'] is True and len(R['commit']) == 8 and R['as_of'] > 0
         and src_runs.read(TMP / 'no-board', [], {}, now=NOW) == dict(runs=[], read_at=NOW, as_of=0, commit='', most=20, board=False), [r['id'] for r in R['runs']])
    keep = src_runs.tasks.git
    try:
        src_runs.CACHE.clear()
        src_runs.tasks.git = lambda b, *a, **k: b'' if a[0] == 'cat-file' else keep(b, *a, **k)
        try:
            src_runs.read(board, [], {}, now=NOW)
            short = 'read as no runs'
        except RuntimeError as e:
            short = str(e)
        src_runs.tasks.git = keep
        names = ['relay/desktop/stops/gone.json', f'relay/desktop/stops/{A}.json']
        skipped = list(src_runs.blobs(board, names))
    finally:
        src_runs.tasks.git = keep
    case('runs: a git call that fails mid-read is an error the page is told, not "no runs", and is not kept as that commit\'s records; a file that is gone is passed over and the rest still read',
         'did not all read' in short and not src_runs.CACHE and skipped == names[1:] and len(src_runs.read(board, [], {}, now=NOW)['runs']) == 4, (short, skipped))
    lines = src_runs.lines(R)
    case('runs: a session reads the same runs as text', lines[0].startswith(A) and '4 legs, $5.00, 3 units: 1 PASS, 1 BLOCKED' in lines[0] and any('asks (leg 02)' in l for l in lines) and any('no leg ran' in l for l in lines), lines[:6])
    return R


def good(r):
    return dict(title='Two review fixes; one is blocked on the ridge', did='Fixed the wire that stood too low, with a test that fails on the old code.', left='The ridge is blocked: its trench sits behind the crest.',
                units=[('rv-one', 'The wire stands at its height again.'), ('rv-two', 'Blocked: the trench is behind the crest.'), ('rv-three', 'Planned only.')])


def the_report(R):
    """The run reader's report and briefs, and what is refused."""
    where, r = runreport.folder(), R['runs'][0]
    g = good(r)
    bad, _, _ = runreport.check(r, 'x' * 80, '', 'word ' * 50, [('rv-one', 'ok'), ('rv-nine', 'x')], [('99', 'b')], [('02', '')], [(str(TMP / 'none.png'), 'c')], [])
    want = ('the title is 80 characters', '--did is empty', '--left takes 50 words', 'no unit of this run that ran a leg is called that', 'the unit rv-two has no sentence', 'the unit rv-three has no sentence', '--asked 99: this run has no ask',
            '--not-his 02 needs its reason', 'the ask 02', 'the ask 03', 'is not a picture that is there')
    case('report: one that is long, empty, leaves a unit without its sentence, leaves an ask unanswered or shows a picture that is not there is refused, with every reason', all(any(w in b for b in bad) for w in want), [w for w in want if not any(w in b for b in bad)] + bad)
    bad, _, _ = runreport.check(r, g['title'], 'Fixed `BattlefieldComposer.cs:326`.', 'Left WireBeltTests.cs red.', g['units'], [], [('02', 'a chore'), ('03', 'a chore')], [], [])
    case('report: words that name a file or quote code are refused: it says what a thing is for', sum('names a file or quotes code' in b for b in bad) == 2 and len(bad) == 2, bad)
    bad, _, _ = runreport.check(r, 'Two review fixes landed', 'Fixed the wire; it is merged.', g['left'], [(u, 'It landed.' if u == 'rv-one' else t) for u, t in g['units']], [], [('02', 'a chore'), ('03', 'a chore')], [], [])
    case('report: "landed" and "merged" are refused wherever they stand: a unit that passed is on its lane, and landing is his', sum('Landing is his own act' in b for b in bad) == 3 and len(bad) == 3, bad)
    bad, answers, _ = runreport.check(r, g['title'], g['did'], g['left'], g['units'], [], [('02', 'a chore')], [], [], still_open=[('03', 'His to decide; the run left three briefs already.'), ('09', 'x')])
    case('report: a decision of his past the run\'s three briefs is kept open, not filed as not his', answers == [dict(key='02', no='a chore'), dict(key='03', open='His to decide; the run left three briefs already.')] and len(bad) == 1 and '--still-open 09' in bad[0], (bad, answers))
    try:
        runreport.add(where, r, g['title'], g['did'], g['left'], g['units'], every=[])
        refused = ''
    except ValueError as e:
        refused = str(e)
    case('report: one that answers no ask is not written', 'the ask 02' in refused and 'the ask 03' in refused and not (where / r['id'] / 'report.json').exists(), refused)
    # a brief for what a leg asked: stamped with the run, the unit and the leg
    try:
        runreport.brief(r, '07', 'T', 'F', ['a', 'b'], 'w', no_evidence='nothing to show')
        no_ask = ''
    except ValueError as e:
        no_ask = str(e)
    b = runreport.brief(r, '03', 'The ridge\'s trench sits behind the crest: move it, or keep it', 'The ridge was to hide its trench behind the crest. The proof found it 6 m back.', ['Move the trench onto the crest', 'Keep it'], 'It is what the idea asked.', no_evidence='The unit left no picture.')
    try:
        runreport.brief(r, '03', 'Again', 'F', ['a', 'b'], 'w', no_evidence='n')
        twice = ''
    except ValueError as e:
        twice = str(e)
    case('report: a brief is written for what a leg asked and for nothing else, once, stamped with its run, unit and leg, and about the unit\'s lane',
         'has no ask 07' in no_ask and 'has its brief already' in twice and b['raised_by'] == dict(run=A, unit='rv-two', leg=3, key='03') and b['lane'] == 'lane/sim/ridge' and b['by'] == f'the run reader (run {A})' and b['state'] == 'open', (no_ask, twice, b))
    # a blocked unit that ran no leg is asked about by its own name: its brief is its ask's too, once
    bare = dict(r, id=r['id'], asks=[dict(key='rv-nine', leg=0, unit='rv-nine', kind='blocked', text='its lane could not be used')], units=r['units'] + [dict(id='rv-nine', lane='lane/show/x', legs=[])])
    nb = runreport.brief(bare, 'rv-nine', 'A lane nobody can use: free it, or drop the unit', 'F', ['a', 'b'], 'w', no_evidence='n')
    try:
        runreport.brief(bare, 'rv-nine', 'Again', 'F', ['a', 'b'], 'w', no_evidence='n')
        again = ''
    except ValueError as e:
        again = str(e)
    _, answers, _ = runreport.check(bare, g['title'], g['did'], g['left'], g['units'], [], [], [], briefs.read_all(briefs.folder()))
    case('report: an ask with no leg (a unit blocked before one ran) has its brief by its own key: written once, and counted as answered', 'has its brief already' in again and answers == [dict(key='rv-nine', brief=nb['id'])] and nb['raised_by']['key'] == 'rv-nine', (again, answers))
    shutil.rmtree(briefs.folder() / nb['id'])
    outside = TMP / 'elsewhere.png'
    outside.write_bytes(PNG)
    try:
        runreport.brief(r, '02', 'T', 'F', ['a', 'b'], 'w', [(str(outside), 'c')], scratch=TMP / 'ctx')
        far = ''
    except ValueError as e:
        far = str(e)
    for name in ('..', '../x', 'a/b', ''):
        try:
            runreport.add(runreport.folder(), dict(r, id=name), g['title'], g['did'], g['left'], g['units'], not_his=[('02', 'x'), ('03', 'y')], every=[])
            far += f' wrote for {name!r}'
        except ValueError:
            pass
    case('report: a brief shows no picture from outside the reading\'s folder, and a run\'s name that is not one is never a folder', 'is not in this reading\'s folder' in far and 'wrote for' not in far and (TMP / 'reports').parent == TMP, far)
    keep, runreport.MOST_BRIEFS = runreport.MOST_BRIEFS, 1
    try:
        runreport.brief(r, '02', 'Second', 'F', ['a', 'b'], 'w', no_evidence='n')
        capped = ''
    except ValueError as e:
        capped = str(e)
    runreport.MOST_BRIEFS = keep
    case('report: a run leaves him no more briefs than its cap', 'the most a run leaves' in capped, capped)
    every = briefs.read_all(briefs.folder())
    d = runreport.add(where, r, g['title'], g['did'], g['left'], g['units'], not_his=[('02', 'A later leg settled it.')], every=every, reading='r1', now=datetime.datetime(2026, 10, 10, 9, 30))
    case('report: with every unit said and every ask answered (the brief written for one counts by itself) it is written, for the run as it is',
         d['sig'] == r['sig'] and d['asks'] == [dict(key='02', no='A later leg settled it.'), dict(key='03', brief=b['id'])] and d['units']['rv-three'] == 'Planned only.' and d['when'] == '2026-10-10 09:30'
         and runreport.read_all(where)[A]['title'] == g['title'] and runreport.current(where, r)['reading'] == 'r1' and runreport.current(where, dict(r, sig='other')) is None, d)
    return b


def context(board, R):
    r = [x for x in R['runs'] if x['id'] == D][0]
    scratch = TMP / 'ctx'
    near = [stamped(NOW - 4000, id='on-its-lane', lane='lane/show/review-fixes', title='The wire: keep the lift'), stamped(NOW - 99000, id='before-it', lane='lane/show/review-fixes'),
            stamped(NOW - 4000, id='closed', lane='lane/show/review-fixes', state='answered'), stamped(NOW - 4000, id='other-lane', lane='lane/show/elsewhere')]
    lines = runreport.context(R['runs'][0], str(board), scratch, near) + runreport.context(r, str(board), scratch)
    listed = [l.split()[0] for l in lines[lines.index('OPEN BRIEFS ON THIS RUN\'S LANES, asked since it began (if one asks what a leg asked, answer that ask with --asked KEY=ID; never write it again):') + 1:][:1]]
    case('context: the open briefs about a lane of the run, asked since it began, are listed whichever run they are tied to, so no question is written twice; an older, a closed or another lane\'s is not', listed == ['on-its-lane'] and not any('before-it' in l or 'closed' in l or 'other-lane' in l for l in lines), listed)
    text = '\n'.join(lines)
    shots = [l for l in lines if 'picture (may be shown)' in l]
    case('context: the agent is given the run whole: why it ended and that a watcher\'s stop reads the same, each unit with what it was asked, every leg\'s own report, the asks to answer, and a pipeline unit\'s evidence copied where it may be shown',
         f'RUN {A} on desktop' in text and 'is also what a watcher\'s stop reads as' in text and 'what it was asked: Mend what the second reader found.' in text and 'done when: the test passes' in text
         and '| NEEDS YOU: the Banner has no shield' in text and re.search(r'^  02  \(NEEDS YOU, unit rv-one\)', text, re.M) and re.search(r'^  03  \(the unit ended BLOCKED, unit rv-two\)', text, re.M)
         and 'PROPOSALS of the retrospective, leg 04' in text and len(shots) == 1 and Path(shots[0].split(': ', 1)[1]).read_bytes() == PNG and any('paper (read it)' in l and 'critic-r1.md' in l for l in lines), text[:1500])
    # a picture is shown only from the reading's own folder
    outside = TMP / 'elsewhere.png'
    outside.write_bytes(PNG)
    bad, _, _ = runreport.check(r, 'The ridge is built', 'Built the ridge.', 'Nothing: it passed.', [(r['units'][0]['id'], 'Built.')], [], [], [(str(outside), 'A picture from elsewhere'), (shots[0].split(': ', 1)[1], 'The ridge from the attacker\'s side')], [], scratch=scratch)
    case('context: a report shows a picture the context put in its folder, and no picture from elsewhere', len(bad) == 1 and 'elsewhere.png is not in this reading\'s folder' in bad[0], bad)


def when_read(R):
    """Which runs are read, and the reading itself with a session that is not one."""
    where = TMP / 'reports2'
    host, now = 'here', datetime.datetime.combine(datetime.date.today(), datetime.time(12, 0))          # noon: four ticks a minute apart stay in one day
    ran = [r for r in R['runs'] if not r['empty']]
    keep, runreport.BACK = runreport.BACK, 1
    first = [r['id'] for r in runreport.wanted(R, where, host, NOW)]
    began = runreport.since(where)
    late = dict(ran[1], ended=began + 5)
    R2 = dict(R, runs=[ran[0], late, ran[2]])
    his = [dict(id='n1', state='open', about=f'run: {D}', text=runreport.READ_SAY, when='2026-10-10 09:00:00'), dict(id='n2', state='open', about=f'run: {B}', text='Looks odd.'), dict(id='n3', state='done', about=f'run: {B}', text=runreport.READ_SAY)]
    asked = [r['id'] for r in runreport.wanted(R2, where, host, NOW + 10, his)]
    going = dict(ran[0], state='going', ended=0)
    case('reading: read are the newest runs from before the reading was switched on (as many as he picked), every run that ended after it, and one he asks for, that one first; a start with no leg and a run that is going are not',
         first == [A] and began == NOW and asked == [D, A, B] and runreport.wanted(dict(R, runs=[going]), where, host, NOW) == [] and C not in asked, (first, asked))
    runreport.BACK = keep
    runreport.put(where / A / 'claim.json', dict(host='desktop', at=NOW - 60, sig=ran[0]['sig']))
    claimed = [r['id'] for r in runreport.wanted(R, where, host, NOW)]
    # old against both clocks: the ticks below run at noon, and after 12:30 a claim an hour before NOW read as fresh to them
    runreport.put(where / A / 'claim.json', dict(host='desktop', at=min(NOW, now.timestamp()) - 3600, sig=ran[0]['sig']))
    case('reading: a run another station claimed lately is left to it, an old claim is not', A not in claimed and A in [r['id'] for r in runreport.wanted(R, where, host, NOW)], claimed)
    started = []

    class P:
        pid = 4242

        def poll(self):
            return None if not started[-1].get('done') else 0

    def launch(cmd, cwd, env, out):
        started.append(dict(cmd=cmd, cwd=cwd, env=env, out=out))
        return P()
    notes_where = notes.folder()
    n = notes.write(notes_where, runreport.READ_SAY, kind='queue', about=f'run: {D}', title='the ridge run')
    his = notes.read_all(notes_where)
    lim = dict(runreport.LIMITS, runs_per_day=2)
    s = runreport.tick(R, where, board='B', now=now, launch=launch, lim=lim, host=host, every_note=his, notes_where=notes_where)
    handed = json.loads(Path(started[0]['env']['TW_RUNREPORT_GIVEN']).read_text(encoding='utf-8'))
    case('reading: the watcher starts one reading, for the run he asked for, in a folder of its own, given the run whole and the board, and says which runs still wait',
         len(started) == 1 and s['running']['run'] == D and D not in s['waiting'] and A in s['waiting'] and handed['run']['id'] == D and handed['board'] == 'B' and Path(started[0]['cwd']).is_dir()
         and started[0]['env']['TW_RUNREPORTS'] == str(where) and started[0]['env']['TW_BRIEFS'] == str(briefs.folder()) and D in started[0]['cmd'][2] and 'tw-run-report' in started[0]['cmd'][2] and s['left'] == 1, s)
    cmd = runreport.command(D, lim, exe='claude')
    case('reading: the session may read and run this tool (its path quoted or not: a session that quoted it was refused every call), starts no agent, searches no web, is cut off at its money and runs on the model the limits name',
         cmd[cmd.index('--allowedTools') + 1:cmd.index('--disallowedTools')] == ['Read', 'Grep', 'Glob', f'Bash(python {runreport.TOOL} *)', f'Bash(python "{runreport.TOOL}" *)'] and {'Agent', 'WebSearch', 'WebFetch', 'AskUserQuestion'} <= set(cmd)
         and cmd[cmd.index('--max-budget-usd') + 1] == '2.0' and cmd[-2:] == ['--model', 'sonnet'], cmd)
    again = runreport.tick(R, where, board='B', now=now + datetime.timedelta(minutes=1), launch=launch, lim=lim, host=host, every_note=his, notes_where=notes_where)
    case('reading: while one is going no second is started', len(started) == 1 and again['running']['run'] == D, again)
    # it ends having written nothing: a failed reading is written down, and the run is tried once more, then left
    started[-1]['done'] = True
    Path(started[-1]['out']).write_text(json.dumps(dict(total_cost_usd=0.41, is_error=False, result='done')), encoding='utf-8')
    s2 = runreport.tick(R, where, board='B', now=now + datetime.timedelta(minutes=2), launch=launch, lim=lim, host=host, every_note=his, notes_where=notes_where)
    spend = [json.loads(l) for l in (where / 'spend.jsonl').read_text(encoding='utf-8').splitlines()]
    told = [x for x in notes.read_all(notes_where) if x['id'] == n['id']][0]
    case('reading: one that ends with no report is a line in the spend with its dollars and that it made none, his note that asked is answered with why, and the run is read again',
         told['state'] == 'done' and 'Not read: the reading failed' in json.dumps(told) and s2['last']['made'] is False and s2['last']['usd'] == 0.41 and spend[0]['run'] == D and spend[0]['host'] and 'no report' in spend[0]['why'] and len(started) == 2 and s2['running']['run'] == D, (s2, spend))
    # the second writes the report: his note is answered, and the run is not read again
    r = [x for x in R['runs'] if x['id'] == D][0]
    runreport.add(where, r, 'The ridge is built', 'Built the ridge for one side.', 'Nothing: it passed.', [(r['units'][0]['id'], 'The ridge is built and passes.')], every=[])
    started[-1]['done'] = True
    Path(started[-1]['out']).write_text('warning\n' + json.dumps(dict(total_cost_usd=0.6, is_error=False, result='done')), encoding='utf-8')
    s3 = runreport.tick(R, where, board='B', now=now + datetime.timedelta(minutes=3), launch=launch, lim=lim, host=host, every_note=his, notes_where=notes_where)
    answered = [x for x in notes.read_all(notes_where) if x['id'] == n['id']][0]
    case('reading: one that wrote the report says so in the spend, his note that asked for it is answered, and the day\'s readings are a cap',
         s3['last']['made'] is True and s3['last']['usd'] == 0.6 and answered['state'] == 'done' and len(started) == 2 and s3['running'] is None and 'readings are used' in s3['off'] and A in s3['waiting'], (s3, answered))
    w = runreport.load(where / A / 'wait.json')
    runreport.put(where / A / 'wait.json', dict(sig=ran[0]['sig'], since=NOW, n=runreport.TRIES, why='the session left no result'))
    ask = [dict(id='n9', state='open', about=f'run: {A}', text=runreport.READ_SAY)]
    case('reading: a run two readings failed on is left with its records, until he asks: his ask is one more try',
         A not in [x['id'] for x in runreport.wanted(R, where, host, NOW)] and 'readings of it failed' in runreport.given_up(where, ran[0]) and [x['id'] for x in runreport.wanted(R, where, host, NOW, ask)][0] == A, w)
    # a reading found from before a restart: believed by its number while its minutes last, then written off, and never killed
    runreport.RUNS.clear()
    killed = []
    keep_stop, runreport.stop = runreport.stop, lambda rec, p=None: killed.append(p) or keep_stop(rec, p)
    runreport.put(runreport.state() / 'running.json', dict(reading='20260101-000000', since=f'{now - datetime.timedelta(minutes=5):%Y-%m-%d %H:%M:%S}', run=A, sig=ran[0]['sig'], pid=4))
    young = runreport.tick(R, where, board='B', now=now, launch=launch, lim=dict(lim, runs_per_day=0), host=host, alive=lambda rec: True)
    old = runreport.tick(R, where, board='B', now=now + datetime.timedelta(minutes=30), launch=launch, lim=dict(lim, runs_per_day=0), host=host, alive=lambda rec: True)
    runreport.stop = keep_stop
    case('reading: after a restart a reading is believed alive by its number only while its minutes last; past them it is written off with a line in the spend, and no process is killed by a number',
         young['running']['run'] == A and old['running'] is None and old['last']['made'] is False and killed == [], (young, old, killed))
    keep_which, keep_popen = runreport.shutil.which, runreport.subprocess.Popen
    runreport.shutil.which = runreport.subprocess.Popen = lambda *a, **k: (_ for _ in ()).throw(AssertionError('a test started a session'))
    try:
        runreport.put(runreport.state() / 'running.json', dict(reading='20260101-000001', since=f'{now:%Y-%m-%d %H:%M:%S}', run=A, sig=ran[0]['sig'], pid=4))
        lines_before = (where / 'spend.jsonl').read_text(encoding='utf-8')
        off = runreport.tick(R, where, board='B', now=now + datetime.timedelta(hours=2), lim=runreport.LIMITS, host=host)
    finally:
        runreport.shutil.which, runreport.subprocess.Popen = keep_which, keep_popen
    case('reading: a station where the reading is switched off starts none, ends none and writes nothing down, and says so',
         off == dict(running=None, waiting=[], left=0, last=None, off='the reading is switched off on this station (TW_RUNREPORT_OFF)') and (where / 'spend.jsonl').read_text(encoding='utf-8') == lines_before
         and (runreport.state() / 'running.json').exists() and os.environ.get('TW_RUNREPORT_OFF') == '1', off)
    (runreport.state() / 'running.json').unlink()
    (where / 'spend.jsonl').write_text(lines_before + 'not json at all\n' + json.dumps(dict(when=f'{now:%Y-%m-%d} 13:00', run='x')) + '\n', encoding='utf-8')
    case('reading: a line of the spend that does not read is counted as a reading of the day, and the lines after it still count', len(runreport.spent(where, f'{now:%Y-%m-%d}')) == len(lines_before.splitlines()) + 2, runreport.spent(where, f'{now:%Y-%m-%d}'))


def in_a_reading(R):
    """What a reading may ask of the tool."""
    handed = TMP / 'handed.json'
    handed.write_text(json.dumps(dict(run=R['runs'][1], board='')), encoding='utf-8')
    scratch = TMP / 'scratch'
    scratch.mkdir(exist_ok=True)
    keep = {k: os.environ.get(k) for k in ('TW_RUNREPORT_READING', 'TW_RUNREPORT_GIVEN', 'TW_RUNREPORT_SCRATCH')}
    os.environ.update(TW_RUNREPORT_READING='r7', TW_RUNREPORT_GIVEN=str(handed), TW_RUNREPORT_SCRATCH=str(scratch))
    out, real = sys.stdout, ''
    try:
        sys.stdout = io.StringIO()
        other = scratch / 'mine.json'
        other.write_text(json.dumps(dict(run=R['runs'][0], board='')), encoding='utf-8')
        codes = [runreport.main(['tick']), runreport.main(['context', A]), runreport.main(['context', A, '--given', str(other)]), runreport.main(['context', '..']), runreport.main(['context', B]),
                 runreport.main(['brief', B, '01', '--title', 'The rule of 28 September: keep it, or drop it', '--for', 'A gun with nobody to shoot at keeps a hidden garrison down. A review fix would end that.',
                                 '--option', 'Keep the rule', '--option', 'Drop it', '--why', 'A landed test holds it.', '--no-evidence', 'A rule, nothing to see.']),
                 runreport.main(['add', B, '--title', 'One review fix passed', '--did', 'Mended the tests that could not fail.', '--left', 'Nothing: it passed.', '--unit', 'rv-b=The tests can fail now.'])]
        real = sys.stdout.getvalue()
    finally:
        sys.stdout = out
        for k, v in keep.items():
            os.environ.pop(k, None) if v is None else os.environ.update({k: v})
    d = runreport.read_all(runreport.folder()).get(B) or {}
    case('a reading: it may read the context of the run it was given, write that run\'s briefs and its report; it starts no reading and reads no other run, not even one it describes in a file of its own',
         codes == [1, 1, 1, 1, 0, 0, 0] and 'tick is not for a reading' in real and 'is not the run this reading was given' in real and '--given is not for a reading' in real and 'is not the name of a run' in real and d.get('reading') == 'r7' and len(d.get('asks') or []) == 1 and d['asks'][0]['brief'].endswith('keep-it-or-drop-it'), (codes, real[-700:]))


def site(board):
    every = briefs.read_all(briefs.folder())
    out = TMP / 'site'
    keep = (ops.board_root, ops.REPO_PAGE.copy())
    ops.board_root = lambda: board
    ops.REPO_PAGE['url'] = 'https://github.com/x/y'
    try:
        counts = ops.the_runs(out, every, [], False)
        text = (out / 'data' / 'runs.js').read_text(encoding='utf-8')
        R = json.loads(text[len('window.RUNS = '):].rstrip().rstrip(';'))
        a = R['runs'][0]
        case('site: the watcher puts the runs in the site as a script, each with its report, its briefs and what still waits on him, and counts them; a session\'s single read starts no reading',
             text.startswith('window.RUNS = ') and counts == dict(runs=3, waits=2, going=0) and a['report']['title'].startswith('Two review fixes') and set(a['report']) == {'title', 'did', 'left', 'units', 'when', 'shots'} and a['gave_up'] == ''
             and [b['how'] for b in a['briefs']] == ['raised'] and a['waits'] == 1 and R['reading'] is None and R['repo'] == 'https://github.com/x/y' and str(TMP) not in text.replace('\\\\', '\\'), (counts, a.get('report')))
        ops.board_root = lambda: (_ for _ in ()).throw(RuntimeError('the board hung'))
        bad = ops.the_runs(out, every, [], False)
        after = json.loads((out / 'data' / 'runs.js').read_text(encoding='utf-8')[len('window.RUNS = '):].rstrip().rstrip(';'))
        case('site: a reading that failed leaves the runs of the one before and says on the page that they are old, and why', bad == dict(runs=0, waits=0, going=0) and after['failed']['why'] == 'RuntimeError: the board hung' and len(after['runs']) == 4, after.get('failed'))
    finally:
        ops.board_root = keep[0]
        ops.REPO_PAGE.clear()
        ops.REPO_PAGE.update(keep[1])
    try:
        import jinja2  # noqa: F401
    except ImportError:
        print('      (no jinja2 on this machine: the runs page was not written)')
        return
    keep_crew, ops.CREW = ops.CREW, TMP / 'nothing'
    ops.page(out, dict(built='then', station='here', commit='abc', refs_as_of='then'))
    ops.CREW = keep_crew
    html = (out / 'runs.html').read_text(encoding='utf-8') if (out / 'runs.html').exists() else ''
    loads = re.findall(r'(?:src|href)="([^"#:]+\.(?:js|css))"', html)
    readings = {'data/ops.js', 'data/queue.js', 'data/beat.js', 'data/notes.js', 'data/briefs.js', 'data/runs.js'}
    case('site: the runs page is written with every script and style it loads, has a place for the runs, their count, the strip and what is wrong with the reading, and every page\'s top bar leads to it',
         {'runs.js', 'runsboard.js', 'runs.css', 'decide.js', 'decide.css', 'board.js', 'crew.js', 'data/runs.js', 'data/briefs.js'} <= set(loads) and not [u for u in loads if u not in readings and not (out / u).exists()]
         and all(f'id="{i}"' in html for i in ('runs', 'r-board', 'r-count', 'r-foot', 'r-warn', 'r-strip', 'r-only')) and 'href="runs.html" aria-current="page"' in html
         and all('href="runs.html"' in (out / p).read_text(encoding='utf-8') and 'href="runs.html" aria-current' not in (out / p).read_text(encoding='utf-8') for p in ('decide.html', 'tasks.html', 'floor.html')), [u for u in loads if u not in readings and not (out / u).exists()])
    order = [loads.index(u) for u in ('crew.js', 'decide.js', 'runs.js', 'runsboard.js')]
    case('site: the page loads its scripts each after what it needs', order == sorted(order), loads)
    return R


def page_rules(R):
    node = shutil.which('node')
    if not node:
        print('      (no node on this machine: the runs page\'s own cases were not run)')
        return
    js = ('const T = require(process.argv[1]); const R = JSON.parse(require("fs").readFileSync(process.argv[2], "utf8")); const now = R.read_at; const a = R.runs[0], b = R.runs[1];'
          'const going = Object.assign({}, a, {state: "going", ended: 0, now_on: "rv-three", report: null}), d = R.runs[3];'
          'const mixed = Object.assign({}, b, {briefs: b.briefs.concat([{id: "g", state: "open", how: "lane"}, {id: "f", state: "answered", how: "from", options: [{key: "A", text: "Yes"}], answer: {option: "A", when: "2026-10-07 10:00", queued: "rv-b"}}, {id: "x", state: "answered", how: "raised"}]),'
          ' asks: [{key: "01", brief: "", no: ""}, {key: "02", brief: "", no: "a chore"}, {key: "03", brief: "k", no: ""}]});'
          'console.log(JSON.stringify({'
          ' rows: T.rows(R).map(x => x.run ? x.run.id : x.starts.length), two: T.rows({runs: [R.runs[2], R.runs[2], a]}).map(x => x.run ? 1 : x.starts.length),'
          ' title: [T.title(a), T.title(Object.assign({}, a, {report: null})), T.title(R.runs[2])],'
          ' verdicts: a.units.map(u => T.verdict(u, a).key), live: going.units.map(u => T.verdict(u, going).key), tally: T.tallyWords(Object.assign({}, a, {report: null})),'
          ' order: T.agents({role: "env-simulator", legs: [{phase: "plan", model: "claude-opus-5"}, {phase: "execute", model: "claude-opus-5"}, {phase: "execute", model: "claude-opus-5"}, {phase: "critic", model: "claude-opus-5"}, {phase: "execute", model: "claude-opus-5"}]}).phases,'
          ' line: [T.unitLine(a.units[0], a), T.unitLine(a.units[0], going), T.unitLine({id: "u", verdict: "PASS", legs: []}, going), T.unitLine({id: "u", verdict: "FAIL", legs: [], why: ["its lane could not be switched to"]}, going)].map(x => x.top),'
          ' whyNot: [T.whyNot(a.units[0]).length, T.whyNot({verdict: "FAIL", why: []})[0], T.whyNot({verdict: "BLOCKED", why: ["the proof failed"]})[0]], cost: [T.cost(a), T.cost({legs: 6, unpriced: 6, usd: 0}), T.cost({legs: 2, unpriced: 0, usd: 3})],'
          ' plain: [T.plain("error: git switch -q lane/sim/x failed in C:/w: fatal: \'lane/sim/x\' is already used by worktree at \'C:/Users/PC/GitHub/githubtest-modular\'"), T.plain("something else")],'
          ' none: [T.none(null), T.none({board: false}), T.none({failed: {}, runs: []}), T.none({runs: []}, true), T.none({runs: []})], maybe: [T.maybe(mixed), T.maybe(a)], stamp: T.stamp("2026-10-10 00:36"),'
          ' guessHead: T.head({runs: [Object.assign({}, d, {briefs: [{id: "g", state: "open", how: "lane"}]})]}), guessStrip: T.strip({runs: [Object.assign({}, d, {briefs: [{id: "g", state: "open", how: "lane"}]})]})[0].key,'
          ' gaveUp: T.reading(Object.assign({}, d, {gave_up: "2 readings of it failed (the session left no result)"}), R, []),'
          ' agents: T.agents(a.units[0]), crew: T.crew(a), model: [T.model("claude-opus-5"), T.model("claude-sonnet-5-5"), T.model("opus"), T.model("")], role: [T.role("lane"), T.role("env-simulator"), T.role("retro")],'
          ' ended: [T.ended(a), T.ended(Object.assign({}, a, {asked_why: "the watcher\'s time was up"})), T.ended(b), T.ended(R.runs[2]), T.ended(going), T.ended(Object.assign({}, going, {silent: true}))],'
          ' dec: Object.fromEntries(Object.entries(T.decisions(mixed)).map(([k, v]) => [k, v.length])), waits: [T.waits(a), T.waits(b), T.waits(mixed)], decided: T.decided(mixed.briefs.filter(x => x.how === "from")[0]),'
          ' reading: [T.reading(a, R, []).key, T.reading(d, R, []).key, T.reading(d, R, [T.READ]).key, T.reading(d, {reading: {running: {run: d.id}}}, []).key, T.reading(d, {reading: {waiting: [d.id], off: "the day\'s 8 readings are used"}}, []).label,'
          '  T.reading(going, R, []).key, T.reading(R.runs[2], R, []).key, T.reading(d, R, []).ask, T.reading(d, R, [T.READ]).ask],'
          ' head: [T.head(R), T.head(null), T.head({board: false, runs: []})], warn: [T.warnings(R, now + 30), T.warnings(R, now + 900), T.warnings(Object.assign({}, R, {failed: {at: now, why: "x"}}), now)].map(w => w.length),'
          ' span: [T.span(20), T.span(600), T.span(6360), T.span(28800)], money: [T.money(18.4), T.money(0), T.money(249.59), T.money(null)], strip: T.strip(R).map(s => [s.id, s.key, s.height]),'
          ' leg: T.leg(a.units[0].legs[0]), starts: T.starts([R.runs[2]]), subject: T.subject(a), when: T.when(a, now).split(" · ").length}))')
    data = TMP / 'page.json'
    data.write_text(json.dumps(R), encoding='utf-8')
    p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'runs.js'), str(data)], capture_output=True)
    try:
        g = json.loads(p.stdout.decode())
    except ValueError:
        case('page: runs.js runs under node', False, p.stderr.decode()[-900:])
        return
    case('page: the runs are rows, the newest first, and the starts that ran no leg one after another are one row', g['rows'] == [A, B, 1, D] and g['two'] == [2, 1], (g['rows'], g['two']))
    case('page: a run is called by its report\'s title; with none, by what its units came to; a start that failed says no leg ran',
         g['title'] == ['Two review fixes; one is blocked on the ridge', '3 units: 1 passed, 1 blocked, 1 cut off', 'No leg ran'] and g['tally'] == ['1 passed', '1 blocked', '1 cut off'], g['title'])
    case('page: a unit with no verdict was cut off by the run\'s end; in a run that is going it is being worked on when the run is on it',
         g['verdicts'] == ['pass', 'blocked', 'cut'] and g['live'] == ['pass', 'blocked', 'going'], (g['verdicts'], g['live']))
    case('page: the agent of a unit is its role and, phase by phase, how many legs on which model; a model is named as he names it',
         g['agents'] == dict(role='review fix agent', phases=['plan on Opus 5 high', 'execute on Opus 5 low']) and g['crew'] == [dict(role='review fix agent', legs=4)] and g['model'] == ['Opus 5', 'Sonnet 5.5', 'Opus', '']
         and g['role'] == ['lane agent (no role brief)', 'env simulator agent', 'retrospective'], (g['agents'], g['crew'], g['model']))
    e = g['ended']
    case('page: why a run ended is said in plain words: a stop on request does not say he stopped it unless the record says why; an error shows the relay\'s own sentence; a run that is going says so, and that it went silent',
         e[0] == dict(line='Stopped on request (the record does not say by whom)', told='') and e[1]['line'] == 'Stopped on request: the watcher\'s time was up' and e[2] == dict(line='Its hours were up, in the middle of a leg', told='')
         and e[3]['told'].startswith('error: git switch') and e[4]['line'] == 'Going now' and 'cut off' in e[5]['line'], e)
    case('page: what a run leaves him is sorted: its own open briefs and the asks nothing holds wait on him; a guess, his own answers and what is not his do not',
         g['dec'] == dict(open=1, likely=1, decided=1, **{'from': 1}, asks=1, notHis=1) and g['waits'] == [1, 1, 2] and g['decided'].startswith('You decided 2026-10-07: A, Yes') and 'queued as rv-b' in g['decided'], (g['dec'], g['waits'], g['decided']))
    case('page: phases are said in the order they ran; a unit is called by its report\'s sentence, else by what its last leg said, and one that ran no leg says why; a failure is never a bare word; a run nobody priced does not read as free',
         g['order'] == ['plan on Opus 5', '2 execute legs on Opus 5', 'critic on Opus 5', 'execute on Opus 5'] and g['line'][0] == 'The wire stands at its height again.' and g['line'][1] == 'fixed, pushed (7861fe1857).'
         and 'already on its lane' in g['line'][2] and g['line'][3] == 'No leg ran on it: its lane could not be switched to.' and g['whyNot'][0] == 0 and 'do not say why its check failed' in g['whyNot'][1] and g['whyNot'][2] == 'the proof failed'
         and g['cost'] == ['$5.00 (1 leg not priced)', 'cost not recorded', '$3.00'], (g['order'], g['line'], g['whyNot'], g['cost']))
    case('page: a git error with a machine\'s paths is said in plain words; with no runs to draw the page says which of four reasons it is; an open decision that is a run\'s only by a guess is counted and said apart, and marks its bar',
         g['plain'] == ['The lane x was open in another checkout on that machine (githubtest-modular), so the relay could not switch to it.', 'something else'] and 'not been read' in g['none'][0] and 'no pipeline board' in g['none'][1]
         and 'could not be read' in g['none'][2] and 'has an open decision' in g['none'][3] and 'No relay run' in g['none'][4] and g['maybe'] == [1, 0] and g['stamp'].endswith('10 Oct 00:36')
         and 'nothing is known to wait on you' in g['guessHead'] and '1 open decision may come from them' in g['guessHead'] and g['guessStrip'] == 'you', (g['plain'], g['none'], g['guessHead'], g['guessStrip']))
    case('page: a run two readings failed on says so with why, and offers one more try', g['gaveUp']['key'] == 'failed' and g['gaveUp']['ask'] is True and '2 readings of it failed (the session left no result)' in g['gaveUp']['label'], g['gaveUp'])
    case('page: a run says whether it is written up, being read, waiting (and why nothing reads), or can be asked for; his click shows at once; a run that is going or ran no leg is not asked about',
         g['reading'] == ['read', 'bare', 'next', 'now', 'Waits to be read, and nothing is reading: the day\'s 8 readings are used.', 'na', 'na', True, False], g['reading'])
    case('page: the line under the title counts the runs, what waits on him from them and what nobody wrote up; a reading that is old or failed is said, a fresh one is not',
         g['head'][0] == 'The last 3 runs · 2 things wait on you from them · 1 not written up' and 'not been read' in g['head'][1] and 'no pipeline board' in g['head'][2] and g['warn'] == [0, 1, 1], (g['head'], g['warn']))
    case('page: time and money read as he reads them', g['span'] == ['under a minute', '10 min', '1 h 46', '8 h'] and g['money'] == ['$18.40', '$0.00', '$250', ''], (g['span'], g['money']))
    case('page: the strip has a bar a run that ran, the oldest first, as tall as its cost, and says which wait on him', [s[:2] for s in g['strip']] == [[D, 'ok'], [B, 'you'], [A, 'you']] and g['strip'][2][2] == 100 and g['strip'][0][2] == 30, g['strip'])
    case('page: a leg says who ran it, for how long and for how much, and what it said; a note about a run is about that run',
         g['leg'] == dict(n='Leg 01', facts=['plan', 'review fix agent', 'Opus 5 high', '10 min', '$2.00'], said='done', key='done') and g['subject']['id'] == 'run: ' + A and g['subject']['kind'] == 'queue'
         and g['starts']['top'].startswith('A start that ran no leg') and g['when'] == 3, (g['leg'], g['subject'], g['starts']))
    return g


# a stand-in for the browser's document, enough for runsboard.js to draw into: elements keep their children and their
# words, and a click is its listener called
DOM = ('function E(tag) { this.tag = tag; this.children = []; this.dataset = {}; this.style = {}; this.on = {}; this.className = ""; this.own = ""; this.attrs = {}; this.hidden = false; this.classList = { add() {} }; }'
       'E.prototype = { appendChild(c) { this.children.push(c); return c; }, addEventListener(k, f) { this.on[k] = f; }, setAttribute(k, v) { this.attrs[k] = v; }, querySelectorAll() { return []; }, querySelector() { return null; },'
       ' contains() { return false; }, scrollIntoView() {}, get firstChild() { return this.children[0]; }, get childNodes() { return this.children; },'
       ' set textContent(v) { this.own = String(v); this.children = []; }, get textContent() { return [this.own].concat(this.children.map(c => c.textContent)).join(" "); }, set innerHTML(v) { this.children = []; this.own = ""; } };'
       'const ids = {}; ["r-board", "r-count", "r-foot", "r-warn", "r-strip", "r-only"].forEach(i => { ids[i] = new E("div"); });'
       'global.document = { getElementById: i => ids[i] || null, createElement: t => new E(t), createTextNode: t => { const e = new E("#"); e.own = String(t); return e; }, activeElement: null, body: new E("body") };'
       'global.location = { hash: "" }; global.setInterval = () => 0;')


def drawn(R):
    """What a card says, as it is drawn: the words and the count agree with the cards under them."""
    node = shutil.which('node')
    if not node:
        print('      (no node on this machine: the drawn page\'s cases were not run)')
        return
    js = (DOM + 'const R = JSON.parse(require("fs").readFileSync(process.argv[2], "utf8")); const guess = {id: "g", title: "A gun firing blind: keep it or drop it", state: "open", how: "lane", asked: "2026-10-10 00:00", what_for: "For a rule.", options: [], shown: true};'
          'R.runs[3] = Object.assign({}, R.runs[3], {briefs: [guess], asks: [], report: null, gave_up: "2 readings of it failed (the session left no result)"});'
          'R.runs[1] = Object.assign({}, R.runs[1], {briefs: [], asks: [], units: R.runs[1].units.concat([{id: "rv-f", verdict: "FAIL", legs: [], why: [], lane: "", goal: "", role: "lane", usd: 0, seconds: 0, head: ""}])});'
          'global.window = { Runs: require(process.argv[1]), RUNS: R, BRIEFS: [] }; require(process.argv[3]);'
          'const b = ids["r-board"], cards = () => b.children.map(c => c.textContent), all = cards(), bars = ids["r-strip"].children.length;'
          'ids["r-only"].on.click(); const only = cards();'
          'window.RUNS = { failed: { at: R.read_at, why: "x" }, runs: [] }; ids["r-only"].on.click(); ids["r-only"].on.click(); ids["r-only"].on.click(); const none = cards();'
          'console.log(JSON.stringify({ all, only, none, count: ids["r-count"].textContent, strip: bars }))')
    data = TMP / 'drawn.json'
    data.write_text(json.dumps(R), encoding='utf-8')
    p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'runs.js'), str(data), str(HERE / 'static' / 'runsboard.js')], capture_output=True)
    try:
        g = json.loads(p.stdout.decode())
    except ValueError:
        case('drawn: runsboard.js draws the page', False, p.stderr.decode()[-900:])
        return
    a, b, starts, d = g['all']
    case('drawn: a card is drawn a run, the starts that ran no leg as a line between them, and a bar a run that ran', len(g['all']) == 4 and 'A start that ran no leg' in starts and g['strip'] == 3, [x[:80] for x in g['all']])
    case('drawn: a read run shows its title, what was done and what is left over its units, each with its sentence and the agent that worked it; its open brief is under "Needs you"',
         'Two review fixes; one is blocked on the ridge' in a and 'Fixed the wire that stood too low' in a and 'Left:' in a and 'The wire stands at its height again.' in a and 'review fix agent: plan on Opus 5 high, then execute on Opus 5 low' in a
         and 'Needs you 1' in a and 'this run left it for you' in a and 'Nothing from this run waits on you' not in a, a[:900])
    case('drawn: a card never says nothing waits on him above an open decision: one that is the run\'s only by a guess has its own heading and count, and the run is marked',
         'Nothing from this run waits on you' not in d and 'An open decision that may come from this run 1' in d and '1 may be yours' in d and 'A gun firing blind' in d, d[:900])
    case('drawn: a run two readings failed on says so on its card, with why', 'An agent tried to write this run up and could not: 2 readings of it failed (the session left no result)' in d, d[:600])
    case('drawn: a run with nothing open says nothing waits; a unit that failed with no reason recorded says the records do not say why, and one that ran no leg says so',
         'Nothing from this run waits on you.' in b and 'The records do not say why its check failed' in b and 'No leg ran on it: the records do not say why.' in b, b[:900])
    case('drawn: "Only what needs you" keeps every run with an open card, the guessed one too, and drops the rest; a first reading that failed says so instead of "no runs yet"',
         len(g['only']) == 2 and 'Two review fixes' in g['only'][0] and 'A gun firing blind' in g['only'][1] and len(g['none']) == 1 and 'could not be read' in g['none'][0], (len(g['only']), g['none']))


if __name__ == '__main__':
    reading()
    decisions()
    BOARD = a_board()
    R0 = off_the_board(BOARD)
    the_report(R0)
    context(BOARD, R0)
    when_read(R0)
    in_a_reading(R0)
    PAGE = site(BOARD)
    if PAGE:
        G = page_rules(PAGE)
        drawn(PAGE)
    shutil.rmtree(TMP, ignore_errors=True)
    print(f'{sum(results)} of {len(results)} cases behaved')
    sys.exit(0 if all(results) else 1)
