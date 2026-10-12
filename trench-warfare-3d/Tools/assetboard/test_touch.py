#!/usr/bin/env python3
"""Tests of the graph a click on a frog opens: what a worker is doing and what it touches.
Run from trench-warfare-3d/: python Tools/assetboard/test_touch.py

Two halves. src_touch.py reads the transcripts and the relay's records into data/touch.js: here it reads made-up
transcripts in a temp folder (every kind of tool call lands as the right kind of node, the files a command names are
found, a full graph folds its oldest files into their folders, a transcript is never read twice, an agent hangs under
the one that sent it, a relay leg is its run's records). static/touch.js lays the graph out with no page in it, so
node runs it here: the same data gives the same picture, a thing that is added moves nothing else, nothing lies on
the worker. The picture itself (touchdraw.js) is looked at, not tested.
"""
import datetime
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import src_ops     # noqa: E402
import src_touch   # noqa: E402

results = []
T0 = 1791830000      # a moment the made-up calls count from
REPO = 'C:/work/repo'
TREES = {Path(REPO): dict(branch='lane/show/demo')}


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:700]))


class Lax(dict):
    """A dict that answers a missing key with an empty one: a case that looks for a node that is not there fails, and
    the cases after it still run."""
    def __missing__(self, key):
        return Lax()

    def __hash__(self):
        return 0


def iso(t):
    return datetime.datetime.fromtimestamp(t, datetime.timezone.utc).strftime('%Y-%m-%dT%H:%M:%S.000Z')


def call(name, inp, t, tid=None, side=False, cwd=REPO):
    """One line of a transcript: an assistant turn that makes one tool call."""
    return json.dumps(dict(type='assistant', timestamp=iso(t), sessionId='s', isSidechain=side, cwd=cwd,
                           message=dict(role='assistant', content=[dict(type='text', text='so'), dict(type='tool_use', id=tid or f'toolu_{t}', name=name, input=inp)])))


def started(tid, agent, t):
    """The line that answers an Agent call: which agent it started."""
    return json.dumps(dict(type='user', timestamp=iso(t), toolUseResult=dict(agentId=agent, status='async_launched'),
                           message=dict(role='user', content=[dict(type='tool_result', tool_use_id=tid, content='started')])))


def write(path, lines, end='\n'):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(('\n'.join(lines) + end).encode('utf-8'))
    return path


def read(path, side=False, **more):
    e = {}
    src_touch.take(path, e, src_ops.spellings(list(TREES)), side=side, **more)
    return e


def graph(path, cap=src_touch.NODES, side=False):
    return src_touch.graph(read(path, side), {str(Path(REPO)): 'lane/show/demo'}, cap=cap)


def kinds(g):
    return {n['label']: n['kind'] for n in g['nodes']}


def python_cases(tmp: Path):
    # ---- every kind of call is the right kind of node
    every = [
        call('Read', dict(file_path=REPO + '/Tools/a.py'), T0 + 1),
        call('Read', dict(file_path='C:\\work\\repo\\docs\\shot.png'), T0 + 2),
        call('Edit', dict(file_path=REPO + '/Tools/b.py', old_string='x', new_string='y'), T0 + 3),
        call('Write', dict(file_path='C:/else/notes/new.md', content='hi'), T0 + 4),
        call('Bash', dict(command='cd "C:/work/repo" && python Tools/run_it.py --fast | tail -3', description='Run it'), T0 + 5),
        call('Grep', dict(pattern='def take', path=REPO + '/Tools'), T0 + 6),
        call('Glob', dict(pattern='**/*.cs'), T0 + 7),
        call('Agent', dict(subagent_type='tw-critic', description='Score the captures', prompt='Look'), T0 + 8, tid='toolu_agent1'),
        started('toolu_agent1', 'abc123def456', T0 + 8),
        call('Skill', dict(skill='unity:tw-gym', args='run'), T0 + 9),
        call('WebFetch', dict(url='https://docs.unity3d.com/Manual/x.html', prompt='p'), T0 + 10),
        call('WebSearch', dict(query='flow field tie break'), T0 + 11),
        call('SendMessage', dict(to='critic', message='again'), T0 + 12),
        call('mcp__station__station_send', dict(text='x'), T0 + 13),
        call('MultiEdit', dict(file_path=REPO + '/Tools/a.py', edits=[]), T0 + 14),
        call('PowerShell', dict(command='Get-Content C:\\work\\repo\\Tools\\c.ps1'), T0 + 15),
    ]
    g = graph(write(tmp / 'every.jsonl', every))
    k = kinds(g)
    want = {'a.py': 'changed', 'shot.png': 'picture', 'b.py': 'changed', 'new.md': 'changed', 'python run_it.py': 'command', 'run_it.py': 'named', 'def take': 'search', '**/*.cs': 'search',
            'Score the captures': 'subagent', 'tw-gym': 'skill', 'docs.unity3d.com': 'web', 'flow field tie break': 'web', 'critic': 'message', 'station: station_send': 'tool',
            'demo': 'checkout', 'C:/else/notes': 'folder', 'c.ps1': 'named', 'get-content': 'command'}
    case('kinds: every kind of tool call lands as its kind of node: a file read, changed or named in a command, a picture, a command, a search, an agent sent, a skill, the web, a message, a tool, '
         'the checkout they are in and the folder outside every checkout', all(k.get(a) == b for a, b in want.items()), {a: (k.get(a), b) for a, b in want.items() if k.get(a) != b})
    by = Lax((n['label'], n) for n in g['nodes'])
    case('kinds: a file in a checkout hangs under that checkout, which names its branch; a file outside hangs under its folder; a command hangs under the worker',
         by['a.py'].get('p') == by['demo']['id'] and by['demo'].get('ref') == 'lane/show/demo' and by['a.py']['full'] == 'Tools/a.py' and by['new.md'].get('p') == by['C:/else/notes']['id']
         and 'p' not in by['python run_it.py'] and by['def take'].get('p') == by['demo']['id'], (by['a.py'], by['demo'], by['new.md']))
    c = g['counts']
    case('counts: the counters are the calls: three read (two files, one page), three changed (Edit, Write, MultiEdit), two ran, three searched, two sent out, one skill',
         (c['read'], c['changed'], c['ran'], c['searched'], c['sent'], c['skills']) == (3, 3, 2, 3, 2, 1) and g['counters'] == [['read', 3], ['changed', 3], ['ran', 2], ['sent out', 2]] and g['calls'] == 15, (c, g['counters'], g['calls']))
    case('kinds: a file that was read and then edited is a changed file with both touches on it, the newest last; one that was only named in a command stays named',
         by['a.py']['kind'] == 'changed' and by['a.py']['n'] == 2 and [t[1] for t in by['a.py']['t']] == ['read', 'edited'] and by['a.py']['last'] == T0 + 14 and by['run_it.py'].get('t') == [[T0 + 5, 'named in a command']] and by['run_it.py']['kind'] == 'named', by['a.py'])
    case('log: the running log is the calls in order, each with its verb, the thing it names and the node that stands for it',
         [(l['verb'], l['thing']) for l in g['log'][:5]] == [('read', 'a.py'), ('looked at', 'shot.png'), ('edited', 'b.py'), ('wrote', 'new.md'), ('ran', 'python run_it.py')]
         and g['log'][0]['id'] == by['a.py']['id'] and g['log'][-1]['at'] == T0 + 15 and g['first'] == T0 + 1 and g['last'] == T0 + 15, g['log'][:5])
    links = {(l['a'], l['b']) for l in g['links']}
    case('links: every node has one link, from the node it hangs under or from the worker itself',
         len(g['links']) == len(g['nodes']) and ('core', by['demo']['id']) in links and (by['demo']['id'], by['a.py']['id']) in links and ('core', by['python run_it.py']['id']) in links, sorted(links)[:6])
    sub = by['Score the captures']
    case('agents: an Agent call is a node that names the agent it started, by the id the floor gives that agent', sub.get('to') == 'agent:tw-critic#abc123' and sub['full'].startswith('tw-critic'), sub)
    heat = g['heat']
    case('heat: the strip is five rows of thirty two-minute buckets ending on the last call, and holds every counted call', len(heat) == 5 and all(len(r) == 30 for r in heat) and sum(map(sum, heat)) == 3 + 3 + 2 + 3 + 2 and heat[2][-1] == 2, heat)

    # ---- the files a command names
    spell = src_ops.spellings(list(TREES))
    def named(cmd, cwd=''):
        found, where = src_touch.in_command(cmd, cwd, spell)
        return [(Path(t).as_posix() if t else '', r) for t, r in found], Path(where).as_posix() if where else ''
    a = named('cd "C:/work/repo" && git status --short && cd trench/Tools && wc -l src_ops.py static/board.js ../docs/x.md')
    case('commands: paths relative to the folder a command changes into are found, the second cd after the first', a == ([(REPO, 'trench/Tools/static/board.js'), (REPO, 'trench/docs/x.md')], REPO), a)
    b = named('python /c/work/repo/Tools/build.py --out "C:/else/out dir/site" > C:\\else\\log\\run.txt; curl https://example.com/a/b.js')
    case('commands: a path written whole is found however it is spelled (Git Bash, backslashes, in quotes with a space), and a web address is no path',
         b == ([('', 'C:/else/log/run.txt'), (REPO, 'Tools/build.py')], REPO) or b == ([(REPO, 'Tools/build.py'), ('', 'C:/else/log/run.txt')], REPO), b)
    c2 = named('cd C:/work/repo && python - <<EOF\nopen("Tools/never.py")\nEOF')
    d = named("cd C:/work/repo && python -c \"import x; x.go('Tools/also_never.py')\"")
    case('commands: code written into a command names files it does not touch: they are left out, the checkout it runs in is kept', c2 == ([], REPO) and d == ([], REPO), (c2, d))
    e = named('python Tools/assetboard/test_house.py', cwd='C:/work/repo/trench')
    case('commands: with no cd, a relative path is from the folder the call was made in', e == ([(REPO, 'trench/Tools/assetboard/test_house.py')], REPO), e)
    keys = [src_touch.command_key(x) for x in ('cd "C:/x y" && git status --short | head', 'python Tools/assetboard/test_house.py', 'python - <<EOF', 'FOO=1 node -e "x"', 'ls -la',
                                               'grep -n "a\\|b" f.py | head', 'time python run.py', '& "C:/Program Files/App/App.exe" a', 'S="C:/t"; mkdir -p "$S/x" && cp -r a b', 'for i in $(seq 1 40); do if grep -q x f.txt; then break; fi; sleep 3; done', '$rows = Get-Content C:/x/y.txt | Select-Object -First 3')]
    case('commands: a command is named by what it runs, so forty runs of one script are one node',
         keys == ['git status', 'python test_house.py', 'python inline', 'node inline', 'ls', 'grep', 'python run.py', 'app', 'cp', 'grep', 'get-content'], keys)

    # ---- the cap: the oldest files fold into their folders
    many = [call('Read', dict(file_path=f'{REPO}/dir{i % 4}/sub/f{i:03d}.cs'), T0 + i) for i in range(200)] + [call('Bash', dict(command=f'tool{i} --x'), T0 + 300 + i) for i in range(10)]
    g = graph(write(tmp / 'many.jsonl', many), cap=40)
    folders = [n for n in g['nodes'] if n['kind'] == 'folder']
    files = [n for n in g['nodes'] if n['kind'] == 'read']
    case('cap: a graph over its cap folds the files touched longest ago into the folder they are in; no file is lost, the newest stay themselves',
         len(g['nodes']) <= 40 and len(folders) == 4 and sum(n['m'] for n in folders) + len(files) == 200 and min(n['last'] for n in files) > max(T0 + n['m'] for n in folders) - 60
         and {n['label'] for n in files} >= {'f199.cs', 'f198.cs'} and g['files'] == 200 and all(n['label'].endswith('sub/') and n['full'].endswith('/sub/') for n in folders),
         (len(g['nodes']), [(n['label'], n['m']) for n in folders], len(files)))
    ids = {n['id'] for n in g['nodes']}
    case('cap: a folder that holds folded files hangs under their checkout, counts their touches, and the log names the folder for a file that was folded',
         all(n.get('p') in ids for n in folders) and sum(n['n'] for n in folders) + sum(n['n'] for n in files) == 200 and all(l['id'] in ids for l in g['log']), [l for l in g['log'] if l['id'] not in ids][:3])
    g2 = graph(write(tmp / 'cmds.jsonl', [call('Bash', dict(command=f'tool{i} --x'), T0 + i) for i in range(60)]), cap=20)
    older = [n for n in g2['nodes'] if n['label'] == 'older commands']
    case('cap: when there are no files to fold, the oldest of a kind fold into one node of that kind', len(g2['nodes']) <= 20 and len(older) == 1 and older[0]['m'] == 42 and older[0]['n'] == 42, [(n['label'], n.get('m')) for n in g2['nodes']][:4])

    # ---- a transcript is read once
    f = write(tmp / 'grow.jsonl', [call('Read', dict(file_path=f'{REPO}/a{i}.py'), T0 + i) for i in range(5)])
    e = {}
    first = src_touch.take(f, e, spell)
    again = src_touch.take(f, e, spell)
    size = f.stat().st_size
    junk = b'#' * (size - 1) + b'\n'          # what was read is spoiled where it lies: a second reading of it would find no call
    f.write_bytes(junk + (call('Edit', dict(file_path=f'{REPO}/a9.py'), T0 + 9) + '\n').encode('utf-8'))
    third = src_touch.take(f, e, spell)
    case('cache: a transcript is read from where the last reading stopped: nothing new, nothing read; one new line, one new call, and what was read before is not read again',
         first and not again and third and e['all'] == 6 and [x['x'][0][2] for x in e['ev']] == ['a0.py', 'a1.py', 'a2.py', 'a3.py', 'a4.py', 'a9.py'] and e['off'] == f.stat().st_size, (first, again, third, e.get('all'), e.get('off')))
    with open(f, 'ab') as h:
        h.write(call('Read', dict(file_path=f'{REPO}/half.py'), T0 + 10).encode('utf-8')[:-20])
    src_touch.take(f, e, spell)
    case('cache: a last line still being written waits for the next reading', e['all'] == 6 and e['off'] < f.stat().st_size, (e['all'], e['off'], f.stat().st_size))
    big = write(tmp / 'big.jsonl', [call('Read', dict(file_path=f'{REPO}/b{i}.py'), T0 + i) for i in range(400)])
    eb = read(big, first=6000, more=6000)
    case('cache: a transcript seen for the first time is read from its tail only, says that older calls exist, and is then read from its end',
         0 < eb['all'] < 40 and eb['more'] and eb['off'] == big.stat().st_size and eb['ev'][-1]['x'][0][2] == 'b399.py', (eb['all'], eb['more']))
    ef = read(big, first=10 ** 9)
    case('cache: no more than the last three hundred calls of a worker are kept', len(ef['ev']) == src_touch.CALLS and ef['all'] == 400 and ef['ev'][0]['x'][0][2] == 'b100.py' and ef['more'], (len(ef['ev']), ef['all']))
    write(big, [call('Read', dict(file_path=f'{REPO}/new.py'), T0 + 999)])
    src_touch.take(big, ef, spell)
    case('cache: a transcript that got shorter was written again: it is read from its start', [x['x'][0][2] for x in ef['ev']] == ['new.py'] and ef['all'] == 1, ef.get('all'))
    few = write(tmp / 'few.jsonl', [call('Read', dict(file_path=f'{REPO}/old{i}.py'), T0 + i) for i in range(70)] + ['{"type":"user","message":{"content":"' + 'x' * 9000 + '"}}'] + [call('Read', dict(file_path=f'{REPO}/late.py'), T0 + 80)])
    ew = read(few, first=3000, more=10 ** 9)
    case('cache: when the tail holds hardly a call (a picture fills it), a longer tail is read', ew['all'] == 71, ew['all'])
    side = write(tmp / 'side.jsonl', [call('Read', dict(file_path=f'{REPO}/mine.py'), T0 + 1), call('Read', dict(file_path=f'{REPO}/agents.py'), T0 + 2, side=True)])
    case('cache: an agent\'s calls written into a session\'s transcript are not the session\'s; in the agent\'s own log they are its own',
         [x['x'][0][2] for x in read(side)['ev']] == ['mine.py'] and len(read(side, side=True)['ev']) == 2, read(side)['ev'])

    # ---- everyone on the floor: a session, the agent it sent, an agent that is over, someone who left
    projects = tmp / 'projects'
    sid, aid, gone = '11112222-aaaa-bbbb-cccc-000000000001', 'a1b2c3d4e5f6a7b8c', 'f0f0f0e1e1e1d2d2d'
    ses = write(projects / 'proj' / f'{sid}.jsonl', [
        call('Agent', dict(subagent_type='tw-scout', description='Map the board', prompt='p'), T0 + 1, tid='toolu_s1'), started('toolu_s1', aid, T0 + 1),
        call('Agent', dict(description='An earlier errand', prompt='p'), T0 + 2, tid='toolu_s2'), started('toolu_s2', gone, T0 + 2),
        call('Edit', dict(file_path=REPO + '/Tools/shared.py'), T0 + 3)])
    write(projects / 'proj' / sid / 'subagents' / f'agent-{aid}.jsonl', [call('Read', dict(file_path=REPO + '/Tools/shared.py'), T0 + 4, side=True), call('Grep', dict(pattern='x', path=REPO), T0 + 5, side=True)])
    write(projects / 'proj' / sid / 'subagents' / f'agent-{gone}.jsonl', [call('Read', dict(file_path=REPO + '/Tools/errand.py'), T0 + 2, side=True)])
    me, kid = dict(kind='session', id='session:' + sid[:8], name='Claude', title='The board', state='working'), dict(kind='agent', id='agent:tw-scout', uid='agent:tw-scout#' + aid[:6], name='tw-scout', state='working', parent='session:' + sid[:8])
    desk = dict(kind='agent', id='agent:relay-leg', uid='leg:20261012-204937-19740-6', name='relay leg 6', state='working', host='DESKTOP', parent='agent:relay', what='rv-07-burst: fix (2 min)')
    bench = dict(kind='agent', id='agent:tw-critic', name='tw-critic', state='idle')
    floor = [('lane/show/demo', me), ('lane/show/demo', kid), ('lane/sim/fixes', desk), ('other work', bench), ('other work', dict(kind='machine', id='pid:1', name='Unity', state='working'))]
    cache, now = {}, time.time()
    src_touch.BUILT.clear(); src_touch.MISSED.clear()
    by = src_touch.collect(floor, TREES, now, cache, projects=projects, runs=[])
    mine, its = by.get('session:' + sid[:8]) or {}, by.get(kid['uid']) or {}
    sent = Lax((n['label'], n) for n in mine.get('nodes', []) if n['kind'] == 'subagent')
    case('floor: a session and the agent it sent each have a graph, under the name the floor gives them; a machine, a frog on the bench and the other station\'s leg with no run on record have none',
         set(by) == {'session:' + sid[:8], kid['uid'], 'agent:general-purpose#' + gone[:6], desk['uid']} and mine['kind'] == 'session' and its['kind'] == 'agent' and mine['calls'] == 3 and its['calls'] == 2, sorted(by))
    case('floor: an agent links to its parent, and the parent\'s node for it names it: a click on the node opens the agent',
         its.get('parent') == 'session:' + sid[:8] and sent['Map the board'].get('to') == kid['uid'], (its.get('parent'), sent.get('Map the board')))
    over = by.get('agent:general-purpose#' + gone[:6]) or {}
    case('floor: an agent that is over and no longer on the floor still opens from its node: its graph is there, marked as over, with the one that sent it',
         over.get('gone') is True and over.get('parent') == 'session:' + sid[:8] and over.get('title') == 'An earlier errand' and sent['An earlier errand'].get('to') == 'agent:general-purpose#' + gone[:6]
         and [n['label'] for n in over['nodes'] if n['kind'] == 'read'] == ['errand.py'], over)
    a_ids, b_ids = {n['id'] for n in mine['nodes']}, {n['id'] for n in its['nodes']}
    case('floor: one file is one node for everyone: what the session changed and its agent read has the same id in both graphs', len([n for n in its['nodes'] if n['label'] == 'shared.py' and n['id'] in a_ids]) == 1 and len(a_ids & b_ids) >= 2, a_ids & b_ids)
    offs = {p: e['off'] for p, e in cache['files'].items()}
    seen = []
    real_open = open
    def spy(path, *a, **k):
        seen.append(str(path))
        return real_open(path, *a, **k)
    src_touch.open = spy
    try:
        by2 = src_touch.collect(floor, TREES, now + 20, cache, projects=projects, runs=[])
    finally:
        del src_touch.open
    case('floor: a second reading of a floor where nothing was written opens no transcript and gives the same graphs', not seen and by2 == by and {p: e['off'] for p, e in cache['files'].items()} == offs, seen)
    rest = [f for f in floor if f[1] is not kid and f[1] is not me]
    left = src_touch.collect(rest, TREES, now + 60, cache, projects=projects, runs=[])
    held = sorted(cache['who'])
    late = src_touch.collect(rest, TREES, now + 60 + src_touch.GONE + 200, cache, projects=projects, runs=[])
    case('floor: a worker that left the floor is kept for half an hour with what it touched, marked as over, and is then gone',
         left.get(me['id'], {}).get('gone') is True and left[me['id']]['calls'] == 3 and left[me['id']].get('title') == 'The board' and left.get(kid['uid'], {}).get('parent') == me['id'] and held == sorted([me['id'], kid['uid']])
         and me['id'] not in late and kid['uid'] not in late and not cache['who'], (sorted(left), held, sorted(late), sorted(cache['who'])))

    # ---- the relay: a run's own records
    run = dict(id='20261012-204937-19740', state='going', station='desktop', started=T0, heard=T0 + 900, legs=3, now_on='rv-07-burst', empty=False, units=[
        dict(id='rv-06-state', lane='lane/sim/fixes', verdict='PASS', goal='Fix two findings', legs=[
            dict(n=1, phase='plan', said='done', started=T0 + 10, seconds=100, commits=[], changed=[]),
            dict(n=2, phase='execute', said='done', started=T0 + 200, seconds=300, commits=[dict(sha='cb353da95d', subject='sim: hash the state')],
                 changed=['- `Assets/_Project/Sim/Core/SimWorld.cs`: the buffer is hashed', '- Tests/Sim/SimHashTests.cs and docs/03-determinism-rules.md'])]),
        dict(id='rv-07-burst', lane='lane/sim/fixes', verdict='', goal='Fix the burst', legs=[dict(n=3, phase='plan', said='', started=T0 + 600, seconds=60, commits=[], changed=[])]),
        dict(id='rv-08-queued', lane='', verdict='', goal='', legs=[])])
    floor2 = [('lane/sim/fixes', desk), ('lane/sim/fixes', dict(kind='agent', id='agent:relay', name='relay', state='working', host='DESKTOP'))]
    by = src_touch.collect(floor2, TREES, now, {}, projects=projects, runs=[run])
    g = by.get(desk['uid']) or {}
    k = Lax((n['label'], n) for n in g.get('nodes', []))
    case('relay: a leg that runs is its run\'s records: the run, its units, each unit\'s lane, the commits and the files the reports name; and the relay itself has the same graph',
         {n['kind'] for n in g.get('nodes', [])} == {'run', 'unit', 'lane', 'commit', 'reported'} and g['kind'] == 'run' and g['leg'] == 6 and g['run'] == run['id'] and 'agent:relay' in by and by['agent:relay']['run'] == run['id']
         and {'SimWorld.cs', 'SimHashTests.cs', '03-determinism-rules.md', 'cb353da', 'rv-06-state', 'rv-07-burst', 'rv-08-queued', 'fixes'} <= set(k), sorted(k))
    chain = {(l['a'], l['b']) for l in g.get('links', [])}
    lane = next((n for n in g.get('nodes', []) if n['kind'] == 'lane' and n.get('p') == k['rv-06-state'].get('id')), Lax())
    case('relay: the links are the chain: the run to its units, a unit to its lane, the lane to the commit, the commit to the files; the unit it is on now is marked, one with no leg yet is queued',
         ('core', k['run 1012-204937']['id']) in chain and (k['run 1012-204937']['id'], k['rv-06-state']['id']) in chain and (k['rv-06-state']['id'], lane['id']) in chain and (lane['id'], k['cb353da']['id']) in chain
         and (k['cb353da']['id'], k['SimWorld.cs']['id']) in chain and k['rv-07-burst'].get('on') == 1 and k['rv-08-queued'].get('q') == 1 and 'q' not in k['rv-06-state'] and k['rv-06-state'].get('verdict') == 'PASS', sorted(chain)[:5])
    case('relay: its counters are the run\'s: units, legs, commits, files named; its log is the legs and the commits in order',
         g['counters'] == [['units', 3], ['legs', 3], ['commits', 1], ['files', 3]] and [l['verb'] for l in g['log']][:4] == ['plan', 'execute', 'commit', 'plan'] and g['log'][2]['thing'] == 'sim: hash the state', (g['counters'], g['log']))
    case('relay: the leg that runs now has no record yet: the graph is the run so far and says so, in its note and as the last line of its log, on the unit it is on',
         g['log'][-1]['verb'] == 'running' and g['log'][-1].get('now') == 1 and g['log'][-1]['thing'].startswith('leg 6 \u00b7 fix') and g['log'][-1]['id'] == k['rv-07-burst']['id'] and 'leg 6 runs now and has no record yet' in g['note']
         and 'no record yet' not in src_touch.relay(run, 3)['note'] and [l['verb'] for l in src_touch.relay(run, 3)['log']] == ['plan', 'execute', 'commit', 'plan'], (g['log'][-1], g['note']))
    new = dict(desk, uid='leg:20261012-224121-21536-1', what='rv-06-state: review (lane, 1 min)')
    g = src_touch.collect([('lane/sim/fixes', new)], TREES, now, {}, projects=projects, runs=[dict(run, state='ended')]).get(new['uid']) or {}
    k = Lax((n['label'], n) for n in g.get('nodes', []))
    case('relay: a run whose records have not reached this station is what the floor says of it: its unit and lane, with that unit\'s legs from the run before, and it says so',
         g.get('run') == '20261012-224121-21536' and k.get('rv-06-state', {}).get('on') == 1 and 'cb353da' in k and 'only just started' in g.get('note', ''), (sorted(k), g.get('note')))

    # ---- the site: one small file, written only when a graph changed
    out = tmp / 'site'
    data = dict(lanes=[dict(branch='lane/show/demo', path=REPO, workers=[me, kid]), dict(branch='other work', path=None, workers=[bench])])
    src_touch.BUILT.clear()
    Tt = src_touch.site(data, out, now=now, cache_path=tmp / 'touch-cache.json', projects=projects)
    text = (out / 'data' / 'touch.js').read_text(encoding='utf-8')
    page = json.loads(text[len('window.TOUCH = '):].rstrip().rstrip(';'))
    stamp = (out / 'data' / 'touch.js').stat().st_mtime_ns
    time.sleep(0.05)
    src_touch.site(data, out, now=now + 20, cache_path=tmp / 'touch-cache.json', projects=projects)
    case('site: the page\'s file is data/touch.js, a script that sets window.TOUCH = {by, at}; its time is the last touch, so a reading where nothing was touched leaves the file alone',
         text.startswith('window.TOUCH = {') and set(page) == {'by', 'at'} and page['at'] == T0 + 5 and set(page['by']) == set(Tt['by']) and (out / 'data' / 'touch.js').stat().st_mtime_ns == stamp
         and set(page['by']['session:' + sid[:8]]) >= {'nodes', 'links', 'log', 'counts'}, (page.get('at'), sorted(page.get('by', {}))))
    fat = {f'session:{i:08d}': graph(tmp / 'many.jsonl') for i in range(6)}
    fat['agent:x#gone01'] = dict(graph(tmp / 'many.jsonl'), gone=True)
    state = {k: 'working' if k.endswith('0') else 'resting' for k in fat}
    whole = src_touch.size(fat)
    lean = src_touch.fit(fat, state, budget=whole * 2 // 3)
    case('site: over its budget, the graphs of who is over and who rests are made smaller first, and the one at work keeps the most; a graph that was cut says how much',
         src_touch.size(lean) <= whole * 2 // 3 and len(lean['session:00000000']['nodes']) == len(fat['session:00000000']['nodes']) and len(lean['agent:x#gone01']['nodes']) <= 48
         and lean['agent:x#gone01']['cut'] > 0 and all(l['b'] in {n['id'] for n in lean['agent:x#gone01']['nodes']} for l in lean['agent:x#gone01']['links']) and src_touch.fit(fat, state, budget=whole * 2) is fat,
         (whole, src_touch.size(lean), {k: len(v['nodes']) for k, v in lean.items()}))
    bad = src_touch.site(dict(lanes=[dict(branch='x', path=REPO)]), out, now=now, cache_path=tmp / 'touch-cache.json', projects=projects)
    case('site: a reading that fails says so and leaves the file of the reading before: the floor is not lost over a graph', bad is None and (out / 'data' / 'touch.js').read_text(encoding='utf-8') == text, bad)


HARNESS = r'''
const T = require(process.argv[2]);
const by = JSON.parse(require('fs').readFileSync(process.argv[3], 'utf8'));
const out = {};
const e = by.busy, m = T.model(e, by, 'busy');
const L1 = T.layout(m, null, 1.7), L2 = T.layout(T.model(e, by, 'busy'), null, 1.7);
out.same = JSON.stringify(L1) === JSON.stringify(L2);
out.nodes = m.nodes.length; out.placed = Object.keys(L1.pos).length;
const all = Object.keys(L1.pos).map(k => L1.pos[k]).concat(Object.keys(L1.at).map(k => L1.at[k]));
out.nan = all.filter(p => !isFinite(p[0]) || !isFinite(p[1])).length;
out.onCore = Object.keys(L1.pos).filter(k => Math.hypot(L1.pos[k][0], L1.pos[k][1]) < T.CORE).length;
out.groupOnCore = m.groups.filter(g => Math.hypot(L1.at[g.id][0], L1.at[g.id][1]) < T.CORE + L1.r[g.id]).length;
const near = m.groups.filter(g => !g.far);
out.overlap = []; for (let i = 0; i < near.length; i++) for (let j = i + 1; j < near.length; j++) { const a = L1.at[near[i].id], b = L1.at[near[j].id]; if (Math.hypot(a[0] - b[0], a[1] - b[1]) < L1.r[near[i].id] + L1.r[near[j].id] - 1e-6) out.overlap.push(near[i].label + '/' + near[j].label); }
const seats = {}; out.shared = 0; for (const k of Object.keys(L1.pos)) { const s = L1.pos[k].map(v => v.toFixed(4)).join(','); if (seats[s]) out.shared++; seats[s] = 1; }
out.inBox = Object.keys(L1.pos).every(k => L1.pos[k][0] >= L1.box[0] && L1.pos[k][0] <= L1.box[2] && L1.pos[k][1] >= L1.box[1] && L1.pos[k][1] <= L1.box[3]);
// a thing is added: a file in a cluster that is there, then a cluster that is new
const grown = JSON.parse(JSON.stringify(e)), head = e.nodes.find(n => n.kind === 'checkout');
grown.nodes.unshift({ id: 'fnew0001', kind: 'read', label: 'new.cs', full: 'x/new.cs', n: 1, last: e.last + 5, p: head.id, t: [[e.last + 5, 'read']] });
const L3 = T.layout(T.model(grown, by, 'busy'), L1, 1.7);
out.moved = Math.max(...Object.keys(L1.pos).map(k => Math.hypot(L1.pos[k][0] - L3.pos[k][0], L1.pos[k][1] - L3.pos[k][1])));
out.newPlaced = !!L3.pos.fnew0001 && Object.keys(L1.pos).every(k => Math.hypot(L3.pos[k][0] - L3.pos.fnew0001[0], L3.pos[k][1] - L3.pos.fnew0001[1]) > 1e-4);
grown.nodes.push({ id: 'pnew0002', kind: 'folder', label: 'C:/far', full: 'C:/far', n: 1, last: e.last + 6 }, { id: 'fnew0003', kind: 'changed', label: 'far.md', full: 'C:/far/far.md', n: 1, last: e.last + 6, p: 'pnew0002' });
const L4 = T.layout(T.model(grown, by, 'busy'), L3, 1.7);
out.movedByCluster = Math.max(...Object.keys(L3.pos).map(k => Math.hypot(L3.pos[k][0] - L4.pos[k][0], L3.pos[k][1] - L4.pos[k][1])));
// the oldest thing goes (it was folded): everyone else keeps its seat
const less = JSON.parse(JSON.stringify(e)); const goneId = less.nodes.find(n => n.p === head.id).id; less.nodes = less.nodes.filter(n => n.id !== goneId);
const L5 = T.layout(T.model(less, by, 'busy'), L1, 1.7);
out.movedByLoss = Math.max(...Object.keys(L5.pos).map(k => Math.hypot(L1.pos[k][0] - L5.pos[k][0], L1.pos[k][1] - L5.pos[k][1])));
// the model
const g = id => m.groups.find(x => x.id === id), nodeBy = l => m.nodes.find(n => n.label === l);
out.groups = m.groups.filter(x => !x.far).map(x => [x.label, x.members.length, x.head ? 1 : 0]);
out.fileGroup = nodeBy('f199.cs').g === 'g:' + head.id && g('g:' + head.id).head === head.id && nodeBy('demo').head === true;
out.loose = nodeBy('tool3').g === 'k:command' && g('k:command').label === 'commands' && g('k:command').head === '';
out.also = [nodeBy('f199.cs').also, nodeBy('f198.cs').also || null];
out.far = m.groups.filter(x => x.far).map(x => [x.label, x.n, x.by]);
out.folded = g('g:' + head.id).files;
// what a click does
out.actions = [T.action({ kind: 'subagent', to: 'agent:x#abc123' }), T.action({ kind: 'subagent' }), T.action({ kind: 'checkout', ref: 'lane/show/board-revamp' }), T.action({ kind: 'checkout', ref: 'detached: x' }),
  T.action({ kind: 'run', ref: '2026-1' }), T.action({ kind: 'read' }), T.action({ kind: 'lane', ref: 'lane/sim/fixes' })];
// the tentacles, the touches, the pick, the names
const ref = e.last;
out.reach = [T.reach(m, ref, 30, 28, 0).length, T.reach(m, ref + 99999, 30, 28, 5).length, T.reach(m, ref + 99999, 30, 28, 0).length, T.reach(m, ref, 1e9, 7, 0).length];
out.reachNewest = m.byId[T.reach(m, ref, 1e9, 1, 0)[0]].last === Math.max(...m.nodes.filter(n => !n.head).map(n => n.last));
out.touches = T.touches({ log: [{ at: 5, verb: 'read', id: 'a' }, { at: 9, verb: 'edited', id: 'a' }, { at: 7, verb: 'read', id: 'b' }] }, { id: 'a', t: [[9, 'edited'], [2, 'read']] });
out.pick = [T.pick({ a: [10, 10, 3], b: [30, 10, 3] }, 12, 11, 6), T.pick({ a: [10, 10, 3], b: [30, 10, 3] }, 100, 100, 6), T.pick({ a: [10, 10, 3], b: [14, 10, 3] }, 13.5, 10, 6)];
out.keys = [T.keys({ uid: 'agent:x#1', id: 'agent:x' }), T.keyOf({ by: { 'agent:x': {} } }, { uid: 'agent:x#1', id: 'agent:x' }), T.keyOf({ by: {} }, { uid: 'leg:1-2', id: 'agent:relay-leg' }), T.entry({ by: { 'session:1': { calls: 3 } } }, { id: 'session:1' }).calls, T.entry({ by: {} }, { id: 'session:1' })];
out.has = [T.has({ kind: 'session', state: 'resting' }, null), T.has({ kind: 'agent', state: 'working' }, null), T.has({ kind: 'agent', state: 'idle' }, null), T.has({ kind: 'agent', state: 'idle' }, {}), T.has({ kind: 'skill', state: 'working' }, null), T.has({ kind: 'machine', state: 'working' }, {})];
out.words = [T.ago(5), T.ago(600), T.ago(7200), T.fresh(100, 100, 600), T.fresh(100, 700, 600), T.rate({ heat: [[0, 0, 0, 0, 0, 5, 5, 0, 0, 10]] }), T.sig(null), T.sig(e).length];
out.path = [T.path(m, nodeBy('f199.cs')), T.path(m, nodeBy('tool3'))];
// how it stands, what it does now, the hour before now, names that lie on nothing
out.state = [T.state({ state: 'working' }, { last: 1000 }, 1060).id, T.state({ state: 'working' }, { last: 1000 }, 1720), T.state({ state: 'resting' }, { last: 1000 }, 1060).id,
  T.state({ state: 'working' }, { last: 1000, gone: true }, 1060).id, T.state({ state: 'working' }, { kind: 'run', last: 1 }, 99999).words, T.state({ state: 'working' }, null, 5).id];
out.doing = [T.doing({ log: [{ at: 5, verb: 'read', thing: 'a.py', id: 'x' }, { at: 9, verb: 'ran', thing: 'git status', id: 'y' }] }, 69), T.doing({ log: [] }, 5)];
out.since = [T.since({ age: 20 }, { id: 'live', words: 'live' }), T.since({ age: 200 }, { id: 'live', words: 'live' }), T.since({ age: 900 }, { id: 'quiet', words: 'quiet for 15 min' }), T.since({ age: 7200 }, { id: 'resting', words: 'resting' }),
  T.doing({ log: [{ at: 5, verb: 'running', thing: 'leg 2', id: 'u', now: 1 }] }, 9999).age];
out.hour = [T.hour([[1, 2, 3, 4]], 1000, 1000, 120), T.hour([[1, 2, 3, 4]], 1000, 1250, 120), T.hour([[1, 2, 3, 4]], 1000, 99999, 120)];
out.overlaps = [T.overlaps([[0, 0, 10, 10, 'a'], [5, 5, 20, 20, 'b']], {}), T.overlaps([[0, 0, 10, 10, 'a'], [11, 0, 20, 10, 'b']], { a: [5, 5, 2], c: [15, 5, 2], core: [5, 5, 3] }), T.overlaps([[0, 0, 10, 10, 'a']], { a: [1, 1, 1], z: [50, 50, 2] })];
// an empty graph, and a run
const none = T.model({ nodes: [], links: [], log: [] }, {}, 'x'), LN = T.layout(none, null, 1);
out.none = [none.nodes.length, none.groups.length, LN.box.every(isFinite)];
const run = T.model(by.leg, by, 'leg'), LR = T.layout(run, null, 1.7);
out.run = [run.groups.filter(x => !x.far).map(x => [x.label, x.kind, x.members.length]), Object.keys(LR.pos).length === by.leg.nodes.length, Object.keys(LR.pos).filter(k => Math.hypot(LR.pos[k][0], LR.pos[k][1]) < T.CORE).length];
console.log(JSON.stringify(out));
'''


def node_cases(tmp: Path):
    node = shutil.which('node')
    if not node:
        print('      (no node on this machine: the layout\'s cases were not run)')
        return
    many = [call('Read', dict(file_path=f'{REPO}/dir{i % 4}/sub/f{i:03d}.cs'), T0 + i) for i in range(200)] + [call('Bash', dict(command=f'tool{i} --x'), T0 + 300 + i) for i in range(10)] \
        + [call('Edit', dict(file_path=f'C:/else/notes/n{i}.md'), T0 + 320 + i) for i in range(6)] + [call('Skill', dict(skill='tw-gym'), T0 + 330), call('WebFetch', dict(url='https://a.example/x'), T0 + 331)]
    busy = graph(write(tmp / 'node-busy.jsonl', many), cap=90)
    other = graph(write(tmp / 'node-other.jsonl', [call('Read', dict(file_path=f'{REPO}/dir3/sub/f199.cs'), T0 + 400), call('Read', dict(file_path='C:/third/place/a.md'), T0 + 401), call('Read', dict(file_path='C:/third/place/b.md'), T0 + 402)]))
    run = dict(id='20261012-204937-19740', state='going', station='desktop', started=T0, heard=T0 + 900, legs=2, now_on='rv-07', units=[
        dict(id='rv-06', lane='lane/sim/fixes', verdict='PASS', goal='', legs=[dict(n=1, phase='execute', said='done', started=T0 + 10, seconds=100, commits=[dict(sha='cb353da95d', subject='s')], changed=['- Sim/Core/SimWorld.cs'])]),
        dict(id='rv-07', lane='lane/sim/fixes', verdict='', goal='', legs=[dict(n=2, phase='plan', said='', started=T0 + 600, seconds=60, commits=[], changed=[])])])
    (tmp / 'by.json').write_text(json.dumps(dict(busy=busy, other=other, leg=src_touch.relay(run, 2))), encoding='utf-8')
    (tmp / 'harness.js').write_text(HARNESS, encoding='utf-8')
    p = subprocess.run([node, str(tmp / 'harness.js'), str(HERE / 'static' / 'touch.js'), str(tmp / 'by.json')], capture_output=True, text=True, encoding='utf-8', timeout=120)
    if p.returncode != 0:
        case('layout: touch.js runs under node', False, p.stderr[-900:])
        return
    o = json.loads(p.stdout)
    case('layout: the same data gives the same picture, and every node has a place', o['same'] and o['placed'] == o['nodes'] and o['nodes'] > 80, (o['same'], o['placed'], o['nodes']))
    case('layout: no place is not a number, nothing lies on the worker (no thing, no cluster), no two things share a place and every place is inside the picture\'s box',
         o['nan'] == 0 and o['onCore'] == 0 and o['groupOnCore'] == 0 and o['shared'] == 0 and o['inBox'], (o['nan'], o['onCore'], o['groupOnCore'], o['shared'], o['inBox']))
    case('layout: the clusters a worker reached do not lie over each other', o['overlap'] == [], o['overlap'])
    case('layout: a thing added to a cluster takes a free seat and moves nothing else by more than a nudge', o['moved'] < 0.03 and o['newPlaced'], (o['moved'], o['newPlaced']))
    case('layout: a new cluster finds a place of its own and moves the others by no more than a nudge', o['movedByCluster'] < 0.08, o['movedByCluster'])
    case('layout: a thing that goes (folded into its folder) leaves its seat empty: the others stay where they were', o['movedByLoss'] < 1e-9, o['movedByLoss'])
    groups = Lax((g[0], g) for g in o['groups'])
    case('model: the files of a checkout are one cluster with the checkout as its head, the files outside one with their folder, and what hangs under nothing is a cluster of its kind',
         groups['demo'][2] == 1 and groups['C:/else/notes'][1:] == [6, 1] and groups['commands'][1:] == [10, 0] and groups['skills'][1:] == [1, 0] and groups['the web'][1:] == [1, 0] and o['fileGroup'] and o['loose'], o['groups'])
    case('model: a cluster counts the files folded into its folders too', o['folded'] == 200, o['folded'])
    case('model: a file another worker touched too says who; what the others reached and this one did not is a far cluster that says how much and by whom',
         o['also'] == [['other'], None] and o['far'] == [['C:/third/place', 2, ['other']]], (o['also'], o['far']))
    case('click: an agent it sent opens that agent, a branch goes to its room on the branches page, a run to the runs page, anything else is shown where it is',
         o['actions'] == [dict(go='worker', key='agent:x#abc123'), dict(go='show'), dict(go='page', href='floor.html#room-lane-show-board-revamp'), dict(go='show'), dict(go='page', href='runs.html#run-2026-1'), dict(go='show'),
                          dict(go='page', href='floor.html#room-lane-sim-fixes')], o['actions'])
    case('tentacles: they reach for what was touched lately, at most so many, the newest first; a worker whose last call is long ago still holds the last few',
         o['reach'][0] >= 2 and o['reach'][1] == 5 and o['reach'][2] == 0 and o['reach'][3] == 7 and o['reachNewest'], o['reach'])
    case('words: every touch of a thing the data has, its own marks and the lines of the log that name it, each once, the newest first',
         o['touches'] == [dict(at=9, verb='edited'), dict(at=5, verb='read'), dict(at=2, verb='read')], o['touches'])
    case('words: the thing under the pointer is the nearest one within reach, and nothing when none is', o['pick'] == ['a', '', 'b'], o['pick'])
    case('words: a worker\'s graph is found under its own name first, then its kind\'s; a frog on the bench, a skill and a machine have none to show',
         o['keys'] == [['agent:x#1', 'agent:x'], 'agent:x', 'leg:1-2', 3, None] and o['has'] == [True, True, False, True, False, False], (o['keys'], o['has']))
    case('words: ages, freshness (a half every ten minutes), calls a minute over the last ten, a file\'s whole path',
         o['words'] == ['5 s', '10 min', '2 h', 1, 0.5, 2, None, 5] and o['path'] == ['repo/dir3/sub/f199.cs', 'tool3 --x'], (o['words'], o['path']))
    case('now: a worker at work with a call in the last three minutes is live; quiet for longer it says for how long; else it rests, is over, or (a run) runs',
         o['state'] == ['live', dict(id='quiet', words='quiet for 12 min'), 'resting', 'over', 'running', 'live'], o['state'])
    case('now: what it is doing is its last call, with how long ago and the node it touched; the heat strip is the hour before now, moved left by the time since its last call',
         o['doing'] == [dict(text='ran git status', age=60, id='y'), None] and o['since'] == ['now', '3 min ago', 'quiet for 15 min', '2 h ago \u00b7 resting', 0] and o['hour'] == [[[1, 2, 3, 4]], [[3, 4, 0, 0]], [[0, 0, 0, 0]]], (o['doing'], o['hour']))
    case('names: a name on another name counts, a name on a dot that is not its own counts, a name on its own dot or on the worker does not',
         o['overlaps'] == [1, 1, 0], o['overlaps'])
    case('layout: a worker that touched nothing is a picture of the worker alone, and a relay run is its units as clusters with their lane, commits and files in them',
         o['none'] == [0, 0, True] and [g[:2] for g in o['run'][0]] == [['the run', 'run'], ['rv-06', 'unit'], ['rv-07', 'unit']] and o['run'][0][1][2] == 3 and o['run'][1] and o['run'][2] == 0, (o['none'], o['run']))


def main():
    tmp = Path(tempfile.mkdtemp(prefix='tw-touch-'))
    try:
        python_cases(tmp)
        node_cases(tmp)
    finally:
        shutil.rmtree(tmp, ignore_errors=True)
    print(f'{sum(results)} of {len(results)} cases behaved')
    return 0 if all(results) else 1


if __name__ == '__main__':
    sys.exit(main())
