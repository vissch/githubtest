#!/usr/bin/env python3
"""Tests of the priority list's rules. Run from trench-warfare-3d/: python Tools/assetboard/test_priorities.py

What is listed and in which order (src_priorities.py), what a row says of why it stands there, the page's own rules
(priorities.js, under node) and the page as the watcher writes it (ops.py). Every case is built from records written
here, the pipeline board among them (a git repository made in a temp folder); nothing of the owner's is read.
"""
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
TMP = Path(tempfile.mkdtemp(prefix='tw-priorities-test-'))
os.environ.update(TW_TASKS=str(TMP / 'root'), TW_FEEDBACK=str(TMP / 'inbox'), TW_NOTES=str(TMP / 'notes'), TW_BRIEFS=str(TMP / 'briefs'), TW_TASKBRIEFS=str(TMP / 'taskbriefs'), TW_TASKBRIEF_OFF='1',
                  TW_RUNREPORTS=str(TMP / 'reports'), TW_RUNREPORT_OFF='1', TW_BOARD=str(TMP / 'no-board'))
import src_priorities as sp  # noqa: E402

results = []


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:900]))


def git(cwd, *args):
    return subprocess.run(['git', '-C', str(cwd), *args], capture_output=True, env=dict(os.environ, GIT_AUTHOR_NAME='t', GIT_AUTHOR_EMAIL='t@t', GIT_COMMITTER_NAME='t', GIT_COMMITTER_EMAIL='t@t'))


UNITS = [
    dict(id='late-finish', lane='lane/show/late', role='lane', priority=60, goal='Carry the late thing to a landing. It is long.'),
    dict(id='landing-small-lanes', lane='lane/show/landing', role='lane', priority=5, goal='The owner, 2026-10-12, to the brief "Land it?": "continue and land". Land the small lanes.'),
    dict(id='no-number', lane='lane/sim/rules', role='sim', goal='A unit that names no priority.'),
    dict(id='rv3-07-lod-floors', lane='lane/show/review-fixes', role='review-fix', priority=48, goal='Fix 2 findings. U069.2, P1 bug: mechsplit.py:40. Also Gym.cs:12, P1 bug.'),
    dict(id='rv-02-done-fix', lane='lane/sim/review-fixes', role='review-fix', priority=1, goal='Fix one finding: S2, P2 bug, DirectFire.cs:137.'),
    dict(id='look-again', lane='lane/show/critique', role='lane', kind='critique', priority=70, goal='Look at the night field again.'),
    dict(id='in-hand', lane='lane/sim/hand', role='balance-simulator', priority=2, goal='The unit the relay holds.'),
]


def a_board():
    """A pipeline board as a clone has it: the queue on origin/main, the checkout behind and without it."""
    board = TMP / 'board'
    board.mkdir()
    git(board, 'init', '-q', '-b', 'main')
    (board / 'README.md').write_text('board\n', encoding='utf-8')
    git(board, 'add', '-A')
    git(board, 'commit', '-q', '-m', 'first')
    files = {f'relay/queue/{u["id"]}.json': u for u in UNITS}
    files['relay/done/rv-02-done-fix.json'] = dict(id='rv-02-done-fix', done_at='2026-10-12T10:00:00Z')
    files['relay/tried/late-finish.json'] = dict(id='late-finish', verdict='BLOCKED', tries=2)
    files['relay/desktop/live/run1.json'] = dict(run='run1', now_on=dict(unit='in-hand', lane='lane/sim/hand'))
    files['relay/desktop/legs/run1-01.json'] = dict(unit='in-hand', state='done')        # not the list's to read
    for n, d in files.items():
        (board / n).parent.mkdir(parents=True, exist_ok=True)
        (board / n).write_text(json.dumps(d), encoding='utf-8')
    git(board, 'add', '-A')
    git(board, 'commit', '-q', '-m', 'relay: queue')
    git(board, 'update-ref', 'refs/remotes/origin/main', 'HEAD')
    git(board, 'reset', '-q', '--hard', 'HEAD~1')
    return board


QUEUE = dict(briefs=[dict(brief='2026-10-09-a-gun', title='A gun firing blind: keep it?', days=3, options=3, lane='lane/sim/x', shot='img/brief/a.png'),
                     dict(brief='2026-10-12-new', title='Asked today', days=0, options=2, lane='')],
             decide=[dict(title='Infantry weapons: toy box', text='Infantry weapons: toy box, and brass', days=4)],
             said=[dict(lane='lane/show/proving-ground', ahead=9, behind=536, words='land it')],
             owed=[dict(lane='lane/show/faction-cards', ahead=1, behind=0)], land=[], approved=[],
             ready=[dict(item='lowpoly', stage='build', role='tw-lowpoly', title='Low-poly machines', lane='lane/show/lowpoly', days=7)],
             broken=[dict(kind='stranded', title='1 decision written down only on a lane', lane='backup/x', detail=['These decisions are on this lane only.'])])
TASKS = dict(rows=[dict(id='t-old', kind='handoff', state='left', idle=4000, title='An old handoff', stands='Nobody took it.'),
                   dict(id='t-new', kind='session', state='left', idle=100, title='A session stopped', lane='lane/show/a'),
                   dict(id='t-cap', kind='capture', state='left', idle=9000, title='Fire effects stacking'),
                   dict(id='t-q', kind='unit', state='queued', idle=10, title='Queued already', unit='late-finish')])
IDEAS = dict(ideas=[dict(id='i-routed', state='accepted', routed='idea-x', title='Routed'), dict(id='i-open', state='open', title='Open'),
                    dict(id='i-loose', state='accepted', title='A warm rim light', score='R1 C2', size='S', answer=dict(when='2026-10-08 12:34'))])
RUNS = dict(runs=[dict(id='run1', state='going', now_on='in-hand'), dict(id='run0', state='ended', now_on='late-finish')])
LANES = [dict(branch='lane/show/a', workers=[dict(id='session:aaaa', kind='session', state='working', title='Board revamp', since='22:07', what='execute'),
                                               dict(id='agent:Explore', uid='agent:Explore#abc', kind='agent', state='working', name='Explore', what='Map the page'),
                                               dict(id='session:bbbb', kind='session', state='resting', title='Asleep'),
                                               dict(id='pid:1', kind='machine', state='working', name='Unity editor'),
                                               dict(id='agent:relay', kind='agent', state='working', name='relay', what='leg 6')])]
REVIEW = dict(findings=[dict(id='U069.2', severity='P1', title='Named by id', path='Tools/mechsplit.py', size='S'),
                        dict(id='U002.3', severity='P1', title='Named by its file among P1s', path='Assets/Gym.cs', size='M'),
                        dict(id='U031.4', severity='P1', title='Its file is named, but as a P2', path='P/Sim/DirectFire.cs', size='L'),
                        dict(id='U024.6', severity='P1', title='Star shells by day', path='Assets/NightLights.cs', size='S', lane='show', kind='bug'),
                        dict(id='U001.1', severity='P2', title='Not a P1', path='x.cs', size='S')])


def data_dir():
    d = TMP / 'site' / 'data'
    d.mkdir(parents=True)
    for name, glob_, v in (('queue.js', 'QUEUE', QUEUE), ('tasks.js', 'TASKS', TASKS), ('ideas.js', 'IDEAS', IDEAS), ('runs.js', 'RUNS', RUNS)):
        (d / name).write_text(f'window.{glob_} = {json.dumps(v)};\nwindow.OTHER = 1;\n', encoding='utf-8')
    return d


def rules(board, d):
    case('words: a unit\'s id is a name he can read, and a review fix says it is one',
         [sp.named(x) for x in ('landing-small-lanes', 'rv3-07-lod-floors', 'rv-07-burst-replay', 'rv-c2-wide_look', '')] == ['Landing small lanes', 'Review fix: lod floors', 'Review fix: burst replay', 'Review fix: wide look', ''],
         [sp.named(x) for x in ('landing-small-lanes', 'rv3-07-lod-floors', 'rv-07-burst-replay', 'rv-c2-wide_look', '')])
    case('words: what a unit is, is its goal past the "The owner, <date>, to the brief" opening, up to its first full stop',
         sp.first_sentence(UNITS[1]['goal']) == 'continue and land' and sp.first_sentence(UNITS[0]['goal']) == 'Carry the late thing to a landing.' and len(sp.first_sentence('a ' * 400, 50)) == 50,
         (sp.first_sentence(UNITS[1]['goal']), sp.first_sentence(UNITS[0]['goal'])))
    case('relay: a unit\'s kind is the relay\'s own rule: a review fix is a fix, a unit that says critique is one, the rest is finishing',
         [sp.kind_of(u) for u in UNITS] == ['finish', 'finish', 'finish', 'fix', 'fix', 'critique', 'finish'] and sp.kind_of(dict(source='critique')) == 'critique', [sp.kind_of(u) for u in UNITS])
    case('relay: a unit\'s area is its role\'s, a sim lane with no such role is the rules, the rest is tools',
         [sp.area_of(u) for u in UNITS[:4]] == ['tools and other', 'tools and other', 'rules', 'review fixes'] and sp.area_of(UNITS[6]) == 'rules', [sp.area_of(u) for u in UNITS])
    cache = {}
    rq = sp.relay_queue(board, cache)
    case('relay: the queue is read off origin/main, not the checkout (which is behind): every unit with its priority, what is done, what was tried, what is in hand',
         rq['read'] and len(rq['units']) == 7 and rq['done'] == ['rv-02-done-fix'] and rq['tried']['late-finish']['tries'] == 2 and [l['now_on']['unit'] for l in rq['live']] == ['in-hand']
         and not (board / 'relay').exists() and {u['id']: u['priority'] for u in rq['units']}['no-number'] is None, rq)
    again = sp.relay_queue(board, cache)
    case('relay: a board that did not move is not read again', again is rq)
    case('relay: no board, or one with no origin/main, is an empty queue that says it was not read', not sp.relay_queue(TMP / 'nowhere', {})['read'] and not sp.relay_queue(None, {})['read'])
    rows = sp.relay_rows(rq, ['in-hand'])
    case('relay: the waiting units stand by their number, lower first; one that names none stands at 50; done units and the one in hand are not listed',
         [r['name'] for r in rows] == ['landing-small-lanes', 'rv3-07-lod-floors', 'no-number', 'late-finish', 'look-again'] and [r['n'] for r in rows] == [5, 48, 50, 60, 70], [(r['name'], r['n']) for r in rows])
    case('relay: a row says its kind of work and its place in that kind, and only the first of each kind may be next',
         [r.get('first') for r in rows] == [True, True, False, False, True] and 'number 2 of that kind' in rows[2]['why'] and 'none set' in rows[2]['why'] and 'none set' not in rows[0]['why']
         and 'the relay may take this next' in rows[1]['why'] and 'the relay may take this next' not in rows[3]['why'], [r['why'] for r in rows])
    case('relay: a unit the relay tried says how often and how it ended', 'tried 2 times, last result blocked' in rows[3]['why'] and 'tried 2×' in rows[3]['chips'] and 'tried' not in rows[0]['why'], rows[3])
    p = sp.collect(d, board, LANES, {}, review=REVIEW)
    by = {b['id']: [r for r in p['rows'] if r['band'] == b['id']] for b in p['bands']}
    case('list: the bands stand in one order (yours, now, next, landings, left, not yet) and the rows are numbered through, from 1, with no gap',
         [b['id'] for b in p['bands']] == ['yours', 'now', 'next', 'land', 'left', 'notyet'] and [r['rank'] for r in p['rows']] == list(range(1, len(p['rows']) + 1))
         and [r['band'] for r in p['rows']] == sorted((r['band'] for r in p['rows']), key=[b['id'] for b in p['bands']].index) and [b['n'] for b in p['bands']] == [len(by[b['id']]) for b in p['bands']], p['bands'])
    case('list: his are the open briefs and the open questions; a brief leads to itself on the Decide page and says how long it has waited',
         [r['id'] for r in by['yours']] == ['brief:2026-10-09-a-gun', 'brief:2026-10-12-new', 'ask:Infantry weapons: toy box'] and by['yours'][0]['link'] == 'decide.html#2026-10-09-a-gun'
         and 'asked 3 days ago' in by['yours'][0]['why'] and 'asked today' in by['yours'][1]['why'], by['yours'])
    case('list: worked now is the unit of a run that is going, and the sessions and agents at work; not a resting one, a machine, the relay itself or the unit of a run that ended',
         [r['id'] for r in by['now']] == ['unit:in-hand', 'worker:session:aaaa', 'worker:agent:Explore#abc'] and by['now'][1]['link'] == 'house.html?pin=session:aaaa' and by['now'][2]['title'] == 'Map the page', by['now'])
    case('list: landings say whose turn it is and whether the branch is behind the game', [r['lane'] for r in by['land']] == ['lane/show/proving-ground', 'lane/show/faction-cards']
         and 'you said to land it' in by['land'][0]['why'] and '536 behind' in by['land'][0]['why'] and 'up to date with the game' in by['land'][1]['why'], by['land'])
    case('list: left unfinished is the tasks nobody is on (his capture first, then the one left alone longest last), the ready steps and the loose ends; a task that is queued is the relay\'s row, not a second one',
         [r['id'] for r in by['left']][:3] == ['task:t-cap', 'task:t-new', 'task:t-old'] and 'task:t-q' not in [r['id'] for r in by['left']] and by['left'][3]['kind'] == 'step' and by['left'][4]['kind'] == 'loose end', [r['id'] for r in by['left']])
    case('list: a P1 of the review is nobody\'s work only when no unit, waiting or done, names it or names its file among P1s; an accepted idea only when it has no route',
         [r['id'] for r in by['notyet']] == ['finding:U024.6', 'finding:U031.4', 'idea:i-loose'], [r['id'] for r in by['notyet']])
    q = sp.collect(d, None, [], {}, review=None)
    case('list: what could not be read is said, and the rest still stands', len(q['wrong']) == 2 and 'relay' in q['wrong'][0] and 'review' in q['wrong'][1] and not [r for r in q['rows'] if r['band'] == 'next'] and len([r for r in q['rows'] if r['band'] == 'yours']) == 3, q['wrong'])
    case('list: it says how the relay splits its day, so the page need not guess', p['relay']['shares'] == dict(finish=60, fix=25, critique=15) and p['relay']['kinds'] == dict(finish=3, fix=1, critique=1) and p['relay']['waiting'] == 5, p['relay'])
    return p


def page_rules(p):
    node = shutil.which('node')
    if not node:
        print('      (no node on this machine: the page\'s own cases were not run)')
        return
    js = ('const T = require(process.argv[1]), P = JSON.parse(require("fs").readFileSync(process.argv[2], "utf8"));'
          'console.log(JSON.stringify({all: T.rows(P).length, band: T.rows(P, "land").map(r => r.id), find: T.rows(P, "all", "PROVING").map(r => r.id), none: T.rows(P, "all", "zzz").length,'
          ' top: T.top(P).map(r => r.id), top3: T.top(P, 3).map(r => r.id), count: [T.count(P), T.count(null), T.count({bands: [], rows: []}), T.count({bands: [{id: "next", n: 1}], rows: [{}]})],'
          ' relay: [T.relay(P), T.relay({relay: {waiting: 0}})]}))')
    data = TMP / 'page.json'
    data.write_text(json.dumps(p), encoding='utf-8')
    r = subprocess.run([node, '-e', js, str(HERE / 'static' / 'priorities.js'), str(data)], capture_output=True)
    try:
        g = json.loads(r.stdout.decode())
    except ValueError:
        case('page: priorities.js runs under node', False, r.stderr.decode()[-900:])
        return
    case('page: one band, or a word he typed (in any case), narrows the rows', g['all'] == len(p['rows']) and g['band'] == ['land:lane/show/proving-ground', 'land:lane/show/faction-cards'] and g['find'] == ['land:lane/show/proving-ground'] and g['none'] == 0, g)
    case('page: under the house stand his rows first, then the relay\'s unit in hand, the first of each kind of work and the landings, in the list\'s own order, and never more than the place holds',
         g['top'] == ['brief:2026-10-09-a-gun', 'brief:2026-10-12-new', 'ask:Infantry weapons: toy box', 'unit:in-hand', 'unit:landing-small-lanes', 'unit:rv3-07-lod-floors', 'unit:look-again', 'land:lane/show/proving-ground']
         and g['top3'] == g['top'][:3], g['top'])
    case('page: the line over the list says how much there is and how much of it is his', g['count'][0] == f'{len(p["rows"])} things to do, 3 of them yours' and g['count'][1] == 'reading the list…' and g['count'][2] == 'Nothing is listed.' and g['count'][3] == '1 thing to do, none of them yours', g['count'])
    case('page: it says the relay does not go straight down the list, with the shares and what waits of each kind; nothing waiting says nothing',
         'does not go straight down' in g['relay'][0] and 'finishing 60 %' in g['relay'][0] and 'every third turn' in g['relay'][0] and '5 units wait: 3 finish, 1 fix, 1 critique' in g['relay'][0] and g['relay'][1] == '', g['relay'])


def site(board, d):
    import ops
    out = d.parent
    n = ops.the_priorities(out, dict(lanes=LANES), board=board, review=TMP / 'review.json', cache_path=TMP / 'cache.json')
    text = (d / 'priorities.js').read_text(encoding='utf-8')
    case('site: the list is in the site as a script the page loads, and the review that is not there is said, not an error', n > 0 and text.startswith('window.PRIORITIES = {') and 'window.PRIORITIES.at = "' in text and 'could not be read' in text, text[:200])
    at = (d / 'priorities.js').stat().st_mtime_ns
    (TMP / 'review.json').write_text(json.dumps(REVIEW), encoding='utf-8')
    ops.the_priorities(out, dict(lanes=LANES), board=board, review=TMP / 'review.json', cache_path=TMP / 'cache.json')
    changed = (d / 'priorities.js').stat().st_mtime_ns
    ops.the_priorities(out, dict(lanes=LANES), board=board, review=TMP / 'review.json', cache_path=TMP / 'cache.json')
    case('site: a list that changed is written again, one that did not is left as it is', changed != at and (d / 'priorities.js').stat().st_mtime_ns == changed and 'finding:U024.6' in (d / 'priorities.js').read_text(encoding='utf-8'))
    keep = sp.collect
    sp.collect = lambda *a, **k: (_ for _ in ()).throw(RuntimeError('it hung'))
    try:
        case('site: a list that fails leaves the site as it was and stops nothing', ops.the_priorities(out, dict(lanes=LANES), board=board, review=TMP / 'review.json', cache_path=TMP / 'cache.json') == 0 and (d / 'priorities.js').stat().st_mtime_ns == changed)
    finally:
        sp.collect = keep
    try:
        import jinja2  # noqa: F401
    except ImportError:
        print('      (no jinja2 on this machine: the priorities page was not written)')
        return
    keep_crew, ops.CREW = ops.CREW, TMP / 'nothing'
    ops.page(out, dict(built='then', station='here', commit='abc', refs_as_of='then'))
    ops.CREW = keep_crew
    html = (out / 'priorities.html').read_text(encoding='utf-8') if (out / 'priorities.html').exists() else ''
    loads = re.findall(r'(?:src|href)="([^"#:]+\.(?:js|css))"', html)
    readings = {'data/ops.js', 'data/queue.js', 'data/beat.js', 'data/notes.js', 'data/priorities.js'}
    case('site: the priorities page is written with every script and style it loads, has a place for the rows, their count, the bands, the box to find a word and what is wrong with the reading, and every page\'s top bar leads to it',
         {'priorities.js', 'priorities.css', 'crew.js', 'data/priorities.js'} <= set(loads) and not [u for u in loads if u not in readings and not (out / u).exists()]
         and all(f'id="{i}"' in html for i in ('pr-board', 'pr-count', 'pr-bands', 'pr-find', 'pr-warn', 'pr-foot')) and 'href="priorities.html" aria-current="page"' in html
         and all('href="priorities.html"' in (out / p).read_text(encoding='utf-8') and 'href="priorities.html" aria-current' not in (out / p).read_text(encoding='utf-8') for p in ('decide.html', 'tasks.html', 'floor.html', 'runs.html')),
         [u for u in loads if u not in readings and not (out / u).exists()])
    order = [loads.index(u) for u in ('data/priorities.js', 'crew.js', 'priorities.js')]
    case('site: the page loads its scripts each after what it needs, and has no button or form that writes (the list is read-only)', order == sorted(order) and '<form' not in html and '<button' not in html, loads)
    src = (HERE / 'static' / 'priorities.js').read_text(encoding='utf-8')
    case('site: the page\'s script sends nothing: no note, no fetch (the owner, 2026-10-12: read-only)', not re.search(r'\bfetch\(|Board\.note|XMLHttpRequest|\.submit\(', src))
    crew = (HERE / 'static' / 'crew.js').read_text(encoding='utf-8')
    case('site: the list is read again with the floor, so a page left open stays true', "'data/priorities.js'" in crew)


if __name__ == '__main__':
    BOARD = a_board()
    D = data_dir()
    P = rules(BOARD, D)
    page_rules(P)
    site(BOARD, D)
    shutil.rmtree(TMP, ignore_errors=True)
    print(f'{sum(results)} of {len(results)} cases behaved')
    sys.exit(0 if all(results) else 1)
