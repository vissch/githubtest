#!/usr/bin/env python3
"""Tests of the asset board's rules. Run from trench-warfare-3d/: python Tools/assetboard/test_assetboard.py

Two kinds. Against the REAL tree: the counts and a handful of assets whose status is known, so a rule change that
moves them is seen. Against FIXTURES: a code table that changed shape must stop the build, the notes file must
refuse an unknown id, lane families must collapse, and a status must follow its ladder. The house page's Python half
is here too (the cases named house: and frog:): which room a piece of work is in, which checkout and which state a
session's transcript says, and the frog's sheets. The page's own rules are test_house.py's.
"""
import datetime
import json
import os
import re
import shutil
import socket
import subprocess
import sys
import tempfile
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import briefs     # noqa: E402
import build      # noqa: E402
import films      # noqa: E402
import ideas      # noqa: E402
import ops        # noqa: E402
import looks      # noqa: E402
import render     # noqa: E402
import sprites    # noqa: E402
import src_acts   # noqa: E402
import src_ops    # noqa: E402
import model      # noqa: E402
import notes      # noqa: E402
import src_graphs  # noqa: E402
import src_code   # noqa: E402
import src_git    # noqa: E402
import src_queue  # noqa: E402
import src_visuals  # noqa: E402

results = []


def case(name, ok, detail=''):
    results.append(ok)
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:500]))


def real_tree():
    code = src_code.read_all(build.P)
    notes = model.load_notes(build.NOTES)
    assets, extra = model.build(build.P, code, notes)
    for a in assets.values():
        model.decide(a)
    by = {}
    for a in assets.values():
        if a['kind'] != 'idea':
            by.setdefault(a['category'], []).append(a['id'])
    case('real tree: 16 characters', len(by['character']) == 16, by['character'])
    # 22 since the Bullfrog (archetype 37) landed with the sim on 2026-10-06
    case('real tree: 22 vehicle archetypes and the three machines that are not units', len(by['vehicle']) == 25, by['vehicle'])
    case('real tree: 14 chunked buildings and the five Siege structures', len(by['building']) == 19, by['building'])
    want = {'Sniper': 'FINAL', 'Rifle': 'FINAL', 'Maw': 'FINAL', 'Pincer': 'FINAL', 'Officer': 'NEEDS_VISUAL', 'Breaker': 'NEEDS_VISUAL',
            'MarkIV': 'NEEDS_VISUAL', 'Brute': 'READY_UNUSED', 'Frog': 'READY_UNUSED', 'Skimmer': 'READY_UNUSED', 'Salvo': 'READY_UNUSED',
            'Boilerhouse': 'READY_UNUSED', 'Gasholder': 'READY_UNUSED', 'House0': 'FINAL', 'Watchtower': 'FINAL', 'Cutter': 'FINAL'}
    got = {k: assets[k]['status'] for k in want}
    case('real tree: the known assets are in their buckets', got == want, {k: v for k, v in got.items() if want[k] != v})
    case('real tree: the Breaker and the Officer say whose model they borrow',
         assets['Breaker']['drawn_as'] == 'Maw' and assets['Officer']['drawn_as'] == 'Soldier', (assets['Breaker']['drawn_as'], assets['Officer']['drawn_as']))
    tags = assets['House0']['level_tags']
    case('real tree: the village houses are placed only where there is a river', 'ShelledForest' in tags and 'WinterLine' not in tags, tags)
    case('real tree: the Salvo has its sixteen rocket tubes', sum(s.startswith('Socket_Tube') for m in assets['Salvo']['models'] for s in m['sockets']) == 16)
    case('real tree: the jetpack leap is an event nothing draws', 'LeapStarted' in extra['orphan_events']
         and any(not r['ok'] for r in assets['Jetpack']['vfx']['rows']), extra['orphan_events'])
    case('real tree: an idea from the notes file is a bucket of its own', assets['Arditi']['status'] == 'IDEA')
    clips = code['clips']
    groups = []
    for _, _, g in clips[1:]:
        if g not in groups:
            groups.append(g)
    case('real tree: the men\'s clips are read in the order of the enum, under their seven headings',
         clips[0] == ('None', 0, '') and clips[1][:2] == ('Idle', 1) and groups == ['idle', 'locomotion', 'fire', 'actions', 'reactions', 'trench and stance', 'deaths'], groups)
    rows = films.atlas_rows(build.P / 'Resources/Units/FigureSoldierAtlas.bytes')
    case('real tree: the baked soldier has a row for every clip', rows == len(clips), f'{rows} rows, {len(clips)} clips')
    jobs = films.jobs_for(build.P, assets, code)
    by_id = {j['id']: j for j in jobs}
    case('real tree: three units drawn with the Soldier share one set of films',
         sorted(a['id'] for a, _ in by_id['FigureSoldier.fire']['models']) == ['Assault', 'Machinegunner', 'Rifle'], [a['id'] for a, _ in by_id['FigureSoldier.fire']['models']])
    case('real tree: the shooting film holds the fire clips, by their rows', [r['name'] for r in by_id['FigureSoldier.fire']['rows']][:2] == ['Fire stand', 'Fire snap']
         and by_id['FigureSoldier.fire']['rows'][0]['row'] == dict((n, r) for n, r, _ in clips)['FireStand'], by_id['FigureSoldier.fire']['rows'][:2])
    vs = looks.versions(build.REPO, [build.P / 'Playground/Art/Tanks/Brute/Brute_LOD0.fbx'])
    case('real tree: the versions of a model come out of git oldest first, each a different file',
         len(vs) >= 2 and [v[1] for v in vs] == sorted(v[1] for v in vs) and len({v[3][0] for v in vs}) == len(vs), [(v[0][:8], v[1]) for v in vs])
    case('real tree: every model is filmed on a turntable, a chunked building also drawn apart',
         all(f'{n}.turn' in by_id for n in ('Pincer.battle', 'Brute.trial', 'House0', 'Pillbox', 'Frog.trial')) and 'House0.apart' in by_id and 'Pillbox.apart' not in by_id,
         sorted(by_id))


def fixtures():
    tmp = Path(tempfile.mkdtemp(prefix='tw-assetboard-test-'))
    src = 'public static class InfantryArchetype\n{\n    public const byte Rifle = 0, Sniper = 3;\n    public const byte Frog = 25;   // a frog\n}\n'
    got = src_code.consts(src, 'InfantryArchetype', 'x.cs', [('Rifle', 0), ('Sniper', 3)])
    case('probe: constants are read with their ids and comments', got == {'Rifle': (0, ''), 'Sniper': (3, ''), 'Frog': (25, 'a frog')}, got)
    for name, text, sentinels in [('a renamed class', src.replace('InfantryArchetype', 'FootArchetype'), [('Rifle', 0)]),
                                  ('a constant that is gone', src.replace('Sniper = 3', 'Marksman = 3'), [('Sniper', 3)]),
                                  ('an id that moved', src.replace('Rifle = 0', 'Rifle = 7'), [('Rifle', 0)])]:
        try:
            src_code.consts(text, 'InfantryArchetype', 'x.cs', sentinels)
            case(f'probe: {name} stops the build', False, 'no ProbeError')
        except src_code.ProbeError as e:
            case(f'probe: {name} stops the build', 'x.cs' in str(e), e)

    enum = 'public enum Clip : byte\n{\n    None,\n    // idle (standing)\n    Idle, AimedIdle,\n    // fire\n    FireStand,\n    Count\n}\n'
    filler = ', '.join(f'C{k}' for k in range(20))
    got = src_code.clips(enum.replace('FireStand,', f'Walk, FireStand, {filler},'), 'x.cs')
    case('probe: clips carry their row and the heading above them', got[1] == ('Idle', 1, 'idle') and got[4] == ('FireStand', 4, 'fire') and got[-1][0] == 'C19', got[:5])
    try:
        src_code.clips(enum.replace('FireStand,', f'Walk, FireStand, {filler},').replace('None,', 'Unset,'), 'x.cs')
        case('probe: a Clip enum that no longer starts at None stops the build', False, 'no ProbeError')
    except src_code.ProbeError as e:
        case('probe: a Clip enum that no longer starts at None stops the build', 'x.cs' in str(e), e)
    case('films: a clip name is written in words', (films.words('FireStand'), films.words('Turn90L'), films.words('FireMG')) == ('Fire stand', 'Turn 90 l', 'Fire mg'),
         (films.words('FireStand'), films.words('Turn90L'), films.words('FireMG')))

    skills = ['tw-balance-sim', 'tw-critic', 'tw-master', 'tw-character-sim', 'tw-destruction-vfx', 'pipeline']
    got = {r: src_ops.skill_for(r, skills) for r in ('balance-simulator', 'critic', 'master', 'character', 'destruction-vfx-simulator', 'lowpoly', '')}
    case('floor: a board role finds the skill that plays it, and no skill is made up',
         got == {'balance-simulator': 'tw-balance-sim', 'critic': 'tw-critic', 'master': 'tw-master', 'character': 'tw-character-sim',
                 'destruction-vfx-simulator': 'tw-destruction-vfx', 'lowpoly': None, '': None}, got)
    case('floor: text a transcript holds as cp1252-read UTF-8 is read back', src_ops.fix_text('work\u00e2\u20ac\u00a6') == 'work\u2026',
         src_ops.fix_text('work\u00e2\u20ac\u00a6'))
    trees = [tmp / 'a', tmp / 'a-b', tmp / 'a' / 'inner']
    for t in trees:
        t.mkdir(parents=True, exist_ok=True)
    case('floor: a session belongs to the deepest checkout that holds its folder',
         src_ops.owner_of(tmp / 'a' / 'inner' / 'x', trees) == trees[2] and src_ops.owner_of(tmp / 'a-b', trees) == trees[1]
         and src_ops.owner_of(tmp / 'elsewhere', trees) is None)
    people = src_ops.roster(build.REPO)
    case('real tree: every project skill is on the roster, with what it is for',
         {'tw-critic', 'tw-master', 'pipeline'} <= {r['id'] for r in people} and all(r['does'] for r in people if r['kind'] == 'skill'),
         [(r['id'], r['does'][:30]) for r in people])

    case('films: an earlier film is named by its day and what its folder says',
         films.earlier_title('2026-09-28-drive-feel/after', 'Maw') == 'Earlier · 28 Sep · drive feel, after'
         and films.earlier_title('misc', 'Maw_a') == 'Earlier · Maw a', films.earlier_title('2026-09-28-drive-feel/after', 'Maw'))
    shared = dict(id='Rifle', models=[dict(form='battle', films=[dict(file='film/FigureSoldier.turn.mp4', what='render'),
                                                              dict(file='film/Rifle.battle.game-battle.mp4', what='game')])])
    own = dict(id='Maw', models=[dict(form='battle', films=[dict(file='film/Maw.battle.game-moves.mp4', what='game'),
                                                           dict(file='film/Maw.battle.game-turn.mp4', what='game')])])
    case('reel: a unit drawn with a shared figure opens on its own game film; a model of its own on its turntable',
         render.reel(shared)[0]['file'] == 'film/Rifle.battle.game-battle.mp4' and render.reel(own)[0]['file'] == 'film/Maw.battle.game-turn.mp4',
         [render.reel(shared)[0]['file'], render.reel(own)[0]['file']])
    lg = tmp / 'agent.jsonl'
    lg.write_text('\n'.join(json.dumps(x) for x in [
        {'type': 'user', 'message': {'role': 'user', 'content': 'Score round 1'}},
        {'type': 'assistant', 'message': {'content': [{'type': 'text', 'text': 'ok'}]}},
        {'type': 'user', 'message': {'content': [{'type': 'tool_result', 'content': 'x'}]}},
        {'type': 'user', 'message': {'content': [{'type': 'text', 'text': 'Round 21 is ready in rounds/r21\nmore'}]}}]), encoding='utf-8')
    case('floor: an agent is doing what it was last asked, not what it was first spawned for',
         src_ops.last_ask(lg) == 'Round 21 is ready in rounds/r21', src_ops.last_ask(lg))
    case('floor: a task line has no paths, no brackets and ends on a word',
         src_ops.tidy('Round 21 in C:\\Users\\PC\\AppData\\rounds\\r21\\ (index-fold and the rest)') == 'Round 21'
         and src_ops.tidy('Rebuild the site, run the tests on /c/Users/PC/x.py now') == 'Rebuild the site, run the tests on now'
         and len(src_ops.tidy('word ' * 40)) <= 61,
         [src_ops.tidy('Round 21 in C:\\Users\\PC\\AppData\\rounds\\r21\\ (index-fold and the rest)'), src_ops.tidy('Rebuild the site, run the tests on /c/Users/PC/x.py now')])
    case('floor: a bare "Round 23" keeps the agent\'s description with the number brought up to date',
         src_ops.agent_task('Round 23 in C:\\x\\y (a, b)', 'Score board UI/UX round 1') == 'Score board UI/UX round 23'
         and src_ops.agent_task('Rebuild the board and shoot it', 'Score round 1') == 'Rebuild the board and shoot it',
         src_ops.agent_task('Round 23 in C:\\x\\y (a, b)', 'Score board UI/UX round 1'))
    import ops
    rs = tmp / 'rs.json'
    lanes = lambda st: [dict(branch='b', items=[dict(id='i', stages=[dict(id='s', state=st)])])]
    first = ops.ready_since(dict(now='2026-10-03 01:00:00', lanes=lanes('READY')), rs)
    later = dict(now='2026-10-03 05:00:00', lanes=lanes('READY')); ops.ready_since(later, rs)
    gone = ops.ready_since(dict(now='2026-10-03 06:00:00', lanes=lanes('DONE')), rs)
    case('floor: a ready stage keeps the time it first turned ready, and is forgotten once it is not',
         first == {'b|i|s': '2026-10-03 01:00:00'} and later['lanes'][0]['items'][0]['stages'][0]['since'] == '2026-10-03 01:00:00' and gone == {},
         [first, gone])
    keep = ops.CREW
    ops.CREW = tmp / 'cache'
    site = tmp / 'site'
    nothing = ops.crew(site) == {} and 'CREW_MEDIA = {}' in (site / 'data' / 'crew.js').read_text(encoding='utf-8')
    (tmp / 'cache' / 'crew').mkdir(parents=True, exist_ok=True)
    (tmp / 'cache' / 'crew' / 'tw-critic.jpg').write_bytes(b'frog')
    (tmp / 'cache' / 'crew' / 'tw-critic.busy.mp4').write_bytes(b'busy')
    got = ops.crew(site)
    ops.CREW = keep
    case("crew: each frog's poster and loops are copied in and listed, and none at all is no error",
         nothing and got == {'tw-critic': {'busy': True, 'doze': False, 'sleep': False, 'sleepv': False}} and (site / 'img' / 'crew' / 'tw-critic.busy.mp4').read_bytes() == b'busy', got)

    notes = tmp / 'notes.json'
    notes.write_text('{"assets": {"Maw": {"colour": "red"}}}')
    try:
        model.load_notes(notes)
        case('notes: an unknown key is refused', False)
    except model.NotesError as e:
        case('notes: an unknown key is refused', 'colour' in str(e), e)
    code = src_code.read_all(build.P)
    try:
        model.build(build.P, code, dict(assets={'Nobody': {'priority': 1}}, planned=[], aliases={}, ignore=[]))
        case('notes: an asset id nothing knows is refused', False)
    except model.NotesError as e:
        case('notes: an asset id nothing knows is refused', 'Nobody' in str(e), e)

    fam = {n: src_git.family(n) for n in ('lane/show/mid-figure', 'lane/show/mid-figure-v2', 'lane/show/mid-figure-v3', 'lane/show/aosa-land3',
                                          'lane/show/night-look-2', 'lane/show/look-deep-2026-10-01')}
    case('lanes: -v2, -v3 and -land copies are one family, and a date is not a version',
         len({fam['lane/show/mid-figure'], fam['lane/show/mid-figure-v2'], fam['lane/show/mid-figure-v3']}) == 1
         and fam['lane/show/aosa-land3'] == 'lane/show/aosa' and fam['lane/show/night-look-2'] == 'lane/show/night-look'
         and fam['lane/show/look-deep-2026-10-01'] == 'lane/show/look-deep-2026-10-01', fam)
    case('lanes: the later copy wins', src_git.version('lane/show/mid-figure-v3') > src_git.version('lane/show/mid-figure-v2') > 0)

    def unit(model_ok, drawn_ok, fielded, trial=False, lanes=(), board=(), parked=False):
        a = model.new_asset('X', 'vehicle')
        a['drawn_as'] = None if model_ok else 'Maw'
        a['stages'] = [model.stage('trial', 't', trial), model.stage('model', 'm', model_ok), model.stage('drawn', 'd', drawn_ok),
                       model.stage('fielded', 'f', fielded, 'Iron')]
        a['lanes'] = [dict(branch=b, live=live, touches_art=art) for b, live, art in lanes]
        a['board'] = [dict(item='i', stage='s', state=s) for s in board]
        a['notes'] = dict(parked=True) if parked else {}
        return model.decide(a)['status']
    rules = [('a fielded model of its own is final', unit(True, True, True), 'FINAL'),
             ('a model of its own that nobody fields is ready, not used', unit(True, True, False), 'READY_UNUSED'),
             ('a model the game does not draw is not its own yet', unit(True, False, True), 'NEEDS_VISUAL'),
             ('trial art makes it in progress', unit(False, False, True, trial=True), 'IN_PROGRESS'),
             ('a live lane touching its art makes it in progress', unit(False, False, True, lanes=[('l', True, True)]), 'IN_PROGRESS'),
             ('a lane that only names it does not', unit(False, False, True, lanes=[('l', True, False)]), 'NEEDS_VISUAL'),
             ('a parked lane does not', unit(False, False, True, lanes=[('l', False, True)]), 'NEEDS_VISUAL'),
             ('an open board stage makes it in progress', unit(False, False, True, board=['READY']), 'IN_PROGRESS'),
             ('a finished board stage does not', unit(False, False, True, board=['DONE']), 'NEEDS_VISUAL'),
             ('the notes file can park it', unit(False, False, True, trial=True, parked=True), 'NEEDS_VISUAL'),
             ('a live lane on a final asset leaves it final', unit(True, True, True, lanes=[('l', True, True)]), 'FINAL')]
    for name, got, want in rules:
        case('status: ' + name, got == want, f'{got}, wanted {want}')


DECISIONS = """# Owner decisions

## Process
| Date | Decision |
|---|---|
| 2026-09-01 | **An old row.** It stays. |

## Open: waiting on the owner
- **A question (2026-09-02):** its first wording.
Do not build any of these without asking.
- **Another question** that runs
  over two lines.

## Plans that live outside the repo
| Plan file | Topic |
|---|---|
| a-plan.md | not a decision |
"""


def queue_fixtures():
    entries = src_queue.parse(DECISIONS)
    case('queue: a decisions page is its dated rows and its open bullets, and nothing else',
         [(e['kind'], e['date'], e['title']) for e in entries] == [('row', '2026-09-01', 'An old row.'),
             ('open', '2026-09-02', 'A question (2026-09-02):'), ('open', '', 'Another question')], entries)
    case('queue: a bullet over two lines is one entry, whole', entries[2]['text'].endswith('  over two lines.'), entries[2]['text'])

    # a repo whose integration moved on (a bullet reworded, a row landed) while two lanes each wrote decisions down:
    # lane a only in this clone, lane b only on origin and touched later
    with tempfile.TemporaryDirectory() as tmp:
        repo, page, clock = Path(tmp), Path(tmp) / src_queue.DECISIONS, [0]

        def git(*args):
            clock[0] += 60
            when = f'{1790000000 + clock[0]} +0000'
            env = dict(os.environ, GIT_AUTHOR_DATE=when, GIT_COMMITTER_DATE=when)
            p = subprocess.run(['git', '-c', 'user.name=t', '-c', 'user.email=t@t', '-c', 'core.autocrlf=false', *args],
                               cwd=repo, capture_output=True, env=env)
            assert p.returncode == 0, (args, p.stderr)
            return p.stdout.decode().strip()

        def commit(text, branch=None, off=None):
            if branch:
                git('checkout', '-q', '-B', branch, off or base)
            page.parent.mkdir(parents=True, exist_ok=True)
            page.write_bytes(text.encode('utf-8'))
            git('add', '-A')
            git('commit', '-q', '-m', branch or 'integration')
            return git('rev-parse', 'HEAD')

        landed = '| 2026-09-03 | **A landed row.** |\n'
        row = '| 2026-09-01 | **An old row.** It stays. |\n'
        git('init', '-q', '-b', 'trunk')
        base = commit(DECISIONS)
        integ = commit(DECISIONS.replace('A question (2026-09-02):** its first', 'A question, narrowed (2026-09-02):** its second')
                       .replace(row, row + landed))
        git('update-ref', 'refs/remotes/' + src_git.INTEGRATION, integ)
        on_a = (DECISIONS.replace(row, row + '| 2026-09-04 | **A lane row.** The first wording. |\n')
                .replace('- **Another', '- **A lane question (2026-09-05):** asked on lane a.\n- **Another'))
        asked = '- **An answered question (2026-09-06):** asked on lane a too.\n'
        a = commit(on_a.replace('- **Another', asked + '- **Another'), 'lane/show/a')
        commit(on_a.replace(row, row + '| 2026-09-07 | **The answer.** |\n'), 'lane/show/c', off=a)      # c answers it
        b = commit(DECISIONS.replace(row, row + landed + '| 2026-09-04 | **A lane row.** The newer wording. |\n'), 'lane/show/b')
        git('update-ref', 'refs/remotes/origin/lane/show/b', b)
        git('checkout', '-q', '--detach', base)
        git('branch', '-q', '-D', 'lane/show/b', 'trunk')

        got = {e['title']: e for e in src_queue.stranded(repo)}
        case('queue: what a lane wrote down and integration lacks is stranded, a row and an open bullet',
             sorted(got) == ['A lane question (2026-09-05):', 'A lane row.', 'The answer.'], sorted(got))
        case('queue: an open bullet another lane has since answered and taken out is not', 'An answered question (2026-09-06):' not in got, sorted(got))
        case('queue: an older wording of a bullet integration has since changed is not', not any('A question' in t for t in got), sorted(got))
        case('queue: a row that also landed is not', 'A landed row.' not in got, sorted(got))
        case('queue: a branch that was never pushed is read', got.get('A lane question (2026-09-05):', {}).get('lanes') == ['lane/show/a', 'lane/show/c'], got)
        r = got.get('A lane row.', {})
        case('queue: one row on two lanes is reported once, in the wording touched last, with both lanes',
             r.get('lane') == 'lane/show/b' and 'newer' in r.get('text', '') and r.get('lanes') == ['lane/show/a', 'lane/show/b', 'lane/show/c'], r)

        # the queue: lane a is checked out here, its gate went green on its tip, the owner said land, a stage is ready
        git('checkout', '-q', 'lane/show/a')
        git('update-ref', 'refs/heads/lane/show/landed', integ)
        green, took = repo / '.git' / 'tw-gate-green', repo / '.git' / 'tw-gate-edit-seconds'
        green.write_text(git('rev-parse', 'HEAD^{tree}') + ' 2026-09-08T10:00:00\n')
        took.write_text('400 2026-09-08T09:00:00\n')
        board = repo / 'board'
        (board / 'approvals').mkdir(parents=True)
        for name, lane in (('show-a', 'lane/show/a'), ('show-landed', 'lane/show/landed'), ('show-gone', 'lane/show/gone')):
            (board / 'approvals' / f'{name}.json').write_text(json.dumps(dict(lane=lane, date='2026-09-07', words='yes, land it')))
        stage = dict(id='gate', state='READY', role='qa', since='2026-09-08 08:00:00')
        floor = dict(now='2026-09-09 12:00:00', lanes=[dict(branch='lane/show/a', path=str(repo), checkout='repo', dirty=0,
                                                             items=[dict(id='item', title='An item', stages=[stage, dict(id='land', state='BLOCKED')])])])
        red = dict(status='completed', conclusion='failure', url='https://ci/1', createdAt='2026-09-08T11:00:00Z')
        when = time.mktime((2026, 9, 9, 12, 0, 0, 0, 0, -1))
        q = src_queue.collect(repo, floor, board=board, cache=dict(ci=dict(at=when, run=red)), now=when)
        case('queue: the open questions on integration, the oldest first, an undated one dated by the commit that wrote it',
             [d['title'] for d in q['decide']] == ['A question', 'Another question'] and q['decide'][0]['date'] == '2026-09-02'
             and len(q['decide'][1]['date']) == 10 and q['decide'][0]['days'] == 7, q['decide'])
        case('queue: a checkout whose green gate tested its tip is not put to him told in commits: it owes him a brief, and says how far behind it is',
             [(l['lane'], l['ahead'], l['behind']) for l in q['owed']] == [('lane/show/a', 1, 1)] and q['land'] == [] and q['said'] == [], (q['owed'], q['land'], q['said']))
        case('queue: an approved lane is listed until it has landed, with the owner\'s words and what holds it up',
             [(a['lane'], a['words'], a['why']) for a in q['approved']] == [('lane/show/a', 'yes, land it', ['1 behind'])], q['approved'])
        case('queue: a ready stage of the board', [(r['item'], r['stage'], r['days']) for r in q['ready']] == [('item', 'gate', 1)], q['ready'])
        kinds = sorted(b['kind'] for b in q['broken'])
        case('queue: broken is a red checks run, a run before a commit over its budget, and each lane with stranded decisions',
             kinds == ['ci', 'gate', 'stranded', 'stranded'] and q['ci'] == 'failure', q['broken'])
        case('queue: its count is what waits on him and nothing else: the two open questions. What is broken, approved, owed a brief or ready is an agent\'s, counted apart',
             q['count'] == 2 == sum(len(q[g]) for g in src_queue.YOURS) and q['agents'] == 4 + 1 + 1 + 1 == sum(len(q[g]) for g in src_queue.AGENTS)
             and not set(src_queue.YOURS) & set(src_queue.AGENTS), (q['count'], q['agents']))
        green.write_text('0' * 40 + ' 2026-09-08T10:00:00\n')
        took.write_text('250 2026-09-08T09:00:00\n')
        q2 = src_queue.collect(repo, floor, board=board, cache=dict(ci=dict(at=when, run=dict(red, conclusion='success'))), now=when)
        case('queue: a tip no green gate tested is not listed to land, and the approval says why it has not',
             not q2['land'] and not q2['owed'] and q2['approved'][0]['why'] == ['1 behind', 'no green gate on its tip'], (q2['land'], q2['owed'], q2['approved']))
        case('queue: a green checks run and a run inside its budget are not broken', sorted(b['kind'] for b in q2['broken']) == ['stranded'] * 2, q2['broken'])
        # the owner answered a question and this checkout took its bullet out (not committed): it no longer waits on him
        kept = page.read_bytes()
        page.write_bytes(kept.replace(b'- **Another question** that runs\n  over two lines.\n', b''))
        q3 = src_queue.collect(repo, floor, board=board, cache=dict(ci=dict(at=when, run=dict(red, conclusion='success'))), now=when)
        page.write_bytes(kept)
        case('queue: a question this checkout has answered and taken out is not the owner\'s to decide any more, is not counted, and is said to be on its lane; '
             'one integration reworded since the lane left it still is',
             [d['title'] for d in q3['decide']] == ['A question'] and [(d['title'], d['lane']) for d in q3['answered']] == [('Another question', 'lane/show/a')]
             and q3['count'] == q2['count'] - 1 and q2['answered'] == [], (q3['decide'], q3['answered'], q3['count'], q2['count']))
        # a question he has answered on its brief is decided: his answer is the decision, so it no longer waits on him
        q6 = src_queue.collect(repo, floor, board=board, cache=dict(ci=dict(at=when, run=dict(red, conclusion='success'))), now=when,
                               answers=[dict(id='b9', title='Its brief has another title', about='another QUESTION!', when='')])
        case('queue: a question he has answered on its brief is decided: not his to decide any more and not counted, by the title the brief is about however it is spelled; the others still are',
             [d['title'] for d in q6['decide']] == ['A question'] and [(d['title'], d['brief']) for d in q6['decided']] == [('Another question', 'b9')]
             and q6['count'] == q2['count'] - 1 and q2['decided'] == [], (q6['decide'], q6['decided'], q6['count'], q2['count']))
        # what he answered on the Decide page and no session took up: nothing wakes one, so past two hours the board says it
        ago = lambda sec: time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(when - sec))
        calm = dict(ci=dict(at=when, run=dict(red, conclusion='success')))
        q4 = src_queue.collect(repo, floor, board=board, cache=dict(calm), now=when, answers=[dict(id='b2', title='The second', when=ago(600)), dict(id='b1', title='The first', when=ago(2 * 3600 + 60))])
        q5 = src_queue.collect(repo, floor, board=board, cache=dict(calm), now=when, answers=[dict(id='b1', title='The first', when=ago(2 * 3600 - 60))])
        late = [b for b in q4['broken'] if b['kind'] == 'untaken']
        case('queue: answers of his on the Decide page that nobody has taken up for over two hours are one broken row for all that wait, counted once, that leads to the page; '
             'under two hours there is none, nor when no answers are handed in',
             len(late) == 1 and late[0]['title'] == '2 answers of yours nobody has taken up, the oldest 2 h ago' and late[0]['url'] == 'decide.html' and late[0]['text'] == 'The first; The second'
             and late[0]['days'] == 0 and q4['agents'] == q2['agents'] + 1 and q4['count'] == q2['count'] and not [b for b in q5['broken'] if b['kind'] == 'untaken'] and q5['agents'] == q2['agents']
             and not [b for b in q2['broken'] if b['kind'] == 'untaken'], (late, q4['count'], q5['count'], q2['count']))

        # WHOSE IT IS (the owner, 2026-10-06: "i have no clue what to pick or what needs me"). On the real board that day not
        # one of thirteen rows was an open decision. Each rule below is one of the ways a row that was not his got there.
        green.write_text(git('rev-parse', 'HEAD^{tree}') + ' 2026-09-08T10:00:00\n')          # the gate is green on its tip again
        calm2 = lambda: dict(ci=dict(at=when, run=dict(red, conclusion='success')))
        base_q = src_queue.collect(repo, floor, board=board, cache=calm2(), now=when)
        brief = lambda bid, title, state='open', **kw: dict(dict(id=bid, title=title, state=state, asked='2026-09-08 09:00', pick='A', what_for='What it is for.',
            options=[dict(key='A', text='Do it'), dict(key='B', text='Leave it')], evidence=[dict(file='1-shot.png', kind='picture', caption='c'), dict(file='2-run.mp4', kind='film', caption='c')]), **kw)
        qa = src_queue.collect(repo, floor, board=board, cache=calm2(), now=when, briefs=[brief('b-closed', 'Its brief', 'answered', about='Another question')])
        case('queue: a question whose brief a session has closed is decided for good, though its bullet stays under Open until that lane lands: not his, not counted',
             [d['title'] for d in qa['decide']] == ['A question'] and qa['count'] == base_q['count'] - 1 and qa['briefs'] == [], (qa['decide'], qa['count'], base_q['count']))
        qb = src_queue.collect(repo, floor, board=board, cache=calm2(), now=when, briefs=[brief('b-open', 'Its brief', about='Another question', lane='lane/show/a'), brief('b-step', 'A step to approve', asked='2026-09-08 09:30')])
        rb = {r['brief']: r for r in qb['briefs']}
        case('queue: an open brief he has not answered is his, whatever it is about (a question, a step, concepts), and is listed with what the page needs to show it; '
             'a question with a brief is listed once, as its brief',
             [r['brief'] for r in qb['briefs']] == ['b-step', 'b-open'] and [d['title'] for d in qb['decide']] == ['A question'] and qb['count'] == 3
             and rb['b-step'] == dict(brief='b-step', title='A step to approve', date='2026-09-08', lane='', about='', kind='', what_for='What it is for.', options=2, pick='Do it',
                                      stills=1, films=1, shot='img/brief/b-step/1-shot.png', days=1), (qb['briefs'], qb['decide'], qb['count']))
        qc = src_queue.collect(repo, floor, board=board, cache=calm2(), now=when, briefs=[brief('b-open', 'Its brief', about='Another question')],
                               answers=[dict(id='b-open', title='Its brief', about='Another question', when='')])
        case('queue: a brief he has answered is not his any more: neither the brief nor its question is listed or counted',
             qc['briefs'] == [] and [d['title'] for d in qc['decide']] == ['A question'] and qc['count'] == base_q['count'] - 1, (qc['briefs'], qc['decide'], qc['count']))
        note = lambda about, text, **kw: dict(dict(id='n1', when='2026-09-09 09:40:39', state='open', kind='queue', about=about, text=text, lane='lane/show/a'), **{'from': 'owner'}, **kw)
        qn = src_queue.collect(repo, floor, board=board, cache=calm2(), now=when, notes=[note('land: a', 'land it')])
        case('queue: a lane he has said land on (a note of his on its row) is an agent\'s to rebase, gate and land: listed with his words, and neither owed a brief nor put to him again',
             [(l['lane'], l['words'], l['date'], l['days']) for l in qn['said']] == [('lane/show/a', 'land it', '2026-09-09', 0)] and qn['owed'] == [] and qn['land'] == []
             and qn['count'] == base_q['count'] and qn['agents'] == base_q['agents'], (qn['said'], qn['owed'], qn['count']))
        qo = src_queue.collect(repo, floor, board=board, cache=calm2(), now=when, notes=[note('land: a', 'land it', state='done'), note('land: other', 'land it'), dict(note('land: a', 'x'), **{'from': 'agent'})])
        case('queue: a note that is answered, about another lane, or not his, is not his word on this one', qo['said'] == [] and len(qo['owed']) == 1, (qo['said'], qo['owed']))
        ql = src_queue.collect(repo, floor, board=board, cache=calm2(), now=when, briefs=[brief('b-land', 'The lane a adds a house', about='land: a', lane='lane/show/a')])
        case('queue: a lane to land is put to him as a brief somebody wrote for it (about "land: <lane>"), and counted once, as that brief',
             [l['lane'] for l in ql['land']] == ['lane/show/a'] and ql['owed'] == [] and [r['brief'] for r in ql['briefs']] == ['b-land'] and ql['count'] == base_q['count'] + 1
             and ql['agents'] == base_q['agents'] - 1, (ql['land'], ql['owed'], ql['count']))
        green.write_text('0' * 40 + ' 2026-09-08T10:00:00\n')

        # the page: its number is the rows of his it lists, each leads to its brief, and it knows how old it is
        node = shutil.which('node')
        if node:
            js = ('const Q = require(process.argv[1]); const q = JSON.parse(process.argv[2]); const at = new Date("2026-09-09T12:00:00").getTime();'
                  'console.log(JSON.stringify([Q.count(q), Q.groups(q).map(g => g.rows.length), Q.fresh("2026-09-09T10:59:00", at).stale,'
                  ' Q.fresh("2026-09-09T11:01:00", at).stale, Q.fresh("2026-09-09T11:01:00", at).text]))')
            p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'queue.js'), json.dumps(q)], capture_output=True)
            got = json.loads(p.stdout.decode() or 'null')
            case('page: "Needs you" is the number of rows of his the queue lists', got and got[0] == q['count'] == sum(got[1]), (got, p.stderr))
            case('page: a floor nobody has read for over an hour is stale, one read 59 minutes ago is not',
                 got and got[2] is True and got[3] is False and got[4] == 'read 59 min ago', got)
            js = ('const Q = require(process.argv[1]); const q = JSON.parse(process.argv[2]); const mine = Q.groups(q), theirs = Q.agents(q);'
                  'const rows = g => g.reduce((a, x) => a.concat(x.rows), []);'
                  'console.log(JSON.stringify([rows(mine).map(Q.leads), mine.map(g => g.key), theirs.map(g => g.key), Q.total(theirs), rows(mine).map(r => [r.shot || "", r.pick || ""]),'
                  ' rows(theirs).filter(r => r.brief).length, JSON.stringify([mine, theirs]).indexOf("floor.html")]))')
            both = dict(qb, said=qn['said'], owed=[])
            p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'queue.js'), json.dumps(both)], capture_output=True)
            got = json.loads(p.stdout.decode() or 'null')
            case('page: every row of his leads to its brief on the Decide page, or to its place among the questions without one, and to nothing else; a brief shows its first picture and the '
                 'pick of who wrote it; what is the agents\' is a list apart that is not his number, and no row of either names the branches page',
                 got and got[0] == ['decide.html#b-step', 'decide.html#b-open', 'decide.html#q-a-question'] and got[1] == ['briefs', 'decide'] and got[2] == ['broken', 'said', 'approved', 'ready']
                 and got[3] == both['agents'] and got[4][1] == ['img/brief/b-open/1-shot.png', 'Do it'] and got[5] == 0 and got[6] == -1, (got, p.stderr[-300:]))
        else:
            print('      (no node on this machine: the page\'s own cases were not run)')
        # the owner, the same evening: "the screen to the right of the house should be exclusively for agents. Under the house
        # and the agents we can put the decisions. We need those a bit bigger since they hold visual data"
        panel, deciding = (HERE / 'static' / 'board.js').read_text(encoding='utf-8'), (HERE / 'static' / 'decide.js').read_text(encoding='utf-8')
        case('page: the place beside the house takes a worker and nothing else: any other thing opens in the drawer, and at rest it says nobody is selected, not the notes',
             'function forDock(s) { return !!s && (!!s.worker || s === NOBODY); }' in panel and 'if (dockEl && docked && !forDock(s)) {' in panel and 'show(NOBODY); resting = true;' in panel
             and 'show(ALL); resting = true;' not in panel, '')
        case('page: the overview draws a decision with the Decide page\'s own card, so it reads the same in both places', 'pure.card = card; pure.said = said;' in deciding
             and deciding.index('pure.card = card') < deciding.index('if (!page) return;'), '')
        src = (HERE / 'static' / 'office.js').read_text(encoding='utf-8')
        block = src[src.index('// the pulse: "Needs you"'):src.index("set('p-at', working.length)")]
        case('page: the part of the overview that draws "Needs you" builds no address of a branch page: a row of his is a link to Q.leads, a row of the agents\' is no link',
             'floor.html' not in block and 'slug(r.lane)' not in block and "el('a', 'k-qrow k-mine')" in block.replace(" + (r.shot ? ' k-shown' : '')", '') and "el('div', 'k-qrow k-theirs')" in block
             and 'big.appendChild(Bf.card(byId[sel.brief]))' in block and block.count('.href = ') == block.count('.href = Q.leads(r)') + block.count(".href = 'decide.html'") + block.count(".href = '#queue'"), block[:200])

        # what a click on a row opens: a few lines that say what it is, and the pictures the board holds for it
        green.write_text(git('rev-parse', 'HEAD^{tree}') + ' 2026-09-08T10:00:00\n')          # the gate is green on its tip again
        (board / 'items').mkdir()
        (board / 'items' / 'item.json').write_text(json.dumps(dict(id='item', title='An item about a house', lane='lane/show/a', stages=[
            dict(id='gate', station='desktop', role='qa', notes='Run the gate on the exact tree.'), dict(id='land', after=['gate'])])))
        for n in ('one', 'two', 'three', 'four'):
            png(board / 'evidence' / 'item' / 'look' / f'{n}.png', 40, 30)
        (board / 'evidence' / 'item' / 'look' / 'notes.md').write_text('words')
        qd = src_queue.details(src_queue.collect(repo, floor, board=board, cache=dict(ci=dict(at=when, run=red)), now=when), repo, floor, board=board)
        land_d, ready_d = qd['owed'][0], qd['ready'][0]
        said_d = src_queue.details(src_queue.collect(repo, floor, board=board, cache=dict(ci=dict(at=when, run=red)), now=when, notes=[note('land: a', 'land it')]), repo, floor, board=board)['said'][0]
        case('queue: a click on a row of the agents\' has what it is in at most five short lines: a lane with a green gate says nothing is his yet and what it owes him, a lane he said land on '
             'says his words and what an agent does next, a ready step what it is and what waits behind it',
             all(isinstance(e.get('detail'), list) and 0 < len(e['detail']) <= 5 and all(isinstance(t, str) and t for t in e['detail']) for g in src_queue.AGENTS + ('decide',) for e in qd[g])
             and land_d['detail'] == ['Nothing for you yet. Before it is put to you an agent writes its brief: what it adds to the game, with a capture from the game.',
                                      'The full gate went green on its tip, 2026-09-08. It holds 1 commit the game does not have yet.']
             and said_d['detail'] == ['You said "land it" on 2026-09-09. The rest is an agent\'s.', 'It is 1 commit behind the game as it is now: an agent rebases it, runs the full gate again, then lands it.']
             and ready_d['detail'] == ['An item about a house.', 'Its step gate is ready to be taken on the desktop by the qa role. Nobody has taken it.', 'The step: Run the gate on the exact tree.',
                                       'Waiting behind it: land.'], (land_d['detail'], said_d['detail'], ready_d['detail'], [(g, e.get('detail')) for g in src_queue.GROUPS for e in qd[g]]))
        case('queue: a row shows at most three pictures, the newest the board holds for its item or for the items of its lane; what is no picture is not one',
             len(ready_d['pictures']) == 3 and all(p.endswith('.png') for p in ready_d['pictures']) and land_d['pictures'] == ready_d['pictures']
             and [e for g in src_queue.GROUPS for e in qd[g] if len(e.get('pictures', [])) > 3] == [], (ready_d['pictures'], land_d['pictures']))
        if node:
            js = ('const Q = require(process.argv[1]); const q = JSON.parse(process.argv[2]); const g = Q.agents(q);'
                  'console.log(JSON.stringify([g.map(x => x.more.length === x.rows.length), Q.more("owed", q.owed[0]).actions.map(a => a.label), Q.more("ready", q.ready[0]).actions.map(a => a.say),'
                  ' Q.more("broken", {kind: "stranded"}).actions.length, Q.more("broken", {kind: "ci"}).actions.length, Q.more("decide", {}).actions.length, Q.more("owed", q.owed[0]).detail.length,'
                  ' Q.more("ready", {detail: ["1", "2", "3", "4", "5", "6"], shots: [1, 2, 3, 4]}).detail.length, Q.more("ready", {shots: [1, 2, 3, 4]}).shots.length]))')
            p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'queue.js'), json.dumps(qd)], capture_output=True)
            got = json.loads(p.stdout.decode() or 'null')
            case('page: every row of the agents\' has what a click opens; a ready step offers what he can say should happen; a lane that owes him a brief, a red checks run and a question offer none',
                 got and all(got[0]) and got[1] == [] and got[2] == ['Take this step next.', 'Leave this step for now.'] and got[3:] == [1, 0, 0, len(land_d['detail']), 5, 3], (got, p.stderr[-300:]))

        # ops.py writes the queue beside the page, and the beat on every read, changed or not
        out = repo / 'site'
        cache_file = repo / 'cache.json'
        cache_file.write_text(json.dumps(dict(ci=dict(at=time.time(), run=red))))
        ops.queue(floor, out, cache_file, repo=repo, board=board)
        wrote = (out / 'data' / 'queue.js').read_text(encoding='utf-8')
        case('ops: the queue it writes has each row\'s lines and its pictures in the site, and no path of this machine',
             '"detail": ["Nothing for you yet.' in wrote and '"shots": [{"name": ' in wrote and '"pictures"' not in wrote and str(board) not in wrote
             and len(list((out / 'img' / 'queue').glob('*'))) == 3, wrote[:300])
        first = (out / 'data' / 'queue.js').stat().st_mtime_ns, (out / 'data' / 'beat.js').read_text()
        ops.queue(dict(floor, now='2026-09-09 12:00:20'), out, cache_file, repo=repo, board=board)
        case('ops: an unchanged queue is not written again, and the beat is, so the page can tell stale from unchanged',
             (out / 'data' / 'queue.js').stat().st_mtime_ns == first[0] and first[1] == 'window.BEAT = "2026-09-09T12:00:00";\n'
             and (out / 'data' / 'beat.js').read_text() == 'window.BEAT = "2026-09-09T12:00:20";\n', first)
        # an open tab reads its data every 20 seconds and its scripts never: the beat carries the stamp of the scripts, and
        # loads a page that runs other ones again (the owner clicked on a page from before the change, 2026-10-06)
        was = ops.SITE['v']
        ops.SITE['v'] = 'stamp-2'
        ops.queue(dict(floor, now='2026-09-09 12:00:30'), out, cache_file, repo=repo, board=board)
        ops.SITE['v'] = was
        stamped = (out / 'data' / 'beat.js').read_text()
        case('ops: once the pages are in the site the beat carries the stamp of their scripts; the stamp is the scripts and templates, whatever their line ends',
             stamped.startswith('window.BEAT = "2026-09-09T12:00:30";\n(function (v) {') and stamped.rstrip().endswith('})("stamp-2");') and re.fullmatch(r'[0-9a-f]{10}', ops.site_version())
             and ops.site_version() == ops.site_version(), stamped[:120])
        if node:
            # the beat in a page: `src` is how it was loaded (with the page, or again by the page's clock), `at` the stamp the page loaded with
            js = ('const text = require("fs").readFileSync(process.argv[1], "utf8"); const out = [];'
                  'function run(src, at, typed, seen) { let n = 0; const store = seen ? {"tw-site": seen} : {};'
                  ' const window = at === null ? {} : {SITE_AT: at}; const document = {currentScript: {src}, querySelectorAll: () => typed ? [{value: typed}] : [{value: ""}]};'
                  ' const sessionStorage = {getItem: k => store[k] || null, setItem: (k, v) => { store[k] = v; }}; const location = {reload: () => { n++; }};'
                  ' new Function("window", "document", "sessionStorage", "location", text)(window, document, sessionStorage, location); return [n, window.SITE_AT || ""]; }'
                  'out.push(run("file:///s/data/beat.js", null), run("file:///s/data/beat.js?t=1", "stamp-2"), run("file:///s/data/beat.js?t=1", "stamp-1"), run("file:///s/data/beat.js?t=1", null),'
                  ' run("file:///s/data/beat.js?t=1", "stamp-1", "half a note"), run("file:///s/data/beat.js?t=1", "stamp-1", "", "stamp-2"));'
                  'console.log(JSON.stringify(out))')
            p = subprocess.run([node, '-e', js, str(out / 'data' / 'beat.js')], capture_output=True)
            got = json.loads(p.stdout.decode() or 'null')
            case('page: a page that loads notes the stamp it runs and is left alone, and so is one whose stamp is still the site\'s; a page with another stamp, or from before pages had '
                 'one, is loaded again, once; never while he has words in a box',
                 got == [[0, 'stamp-2'], [0, 'stamp-2'], [1, 'stamp-1'], [1, ''], [0, 'stamp-1'], [0, 'stamp-1']], (got, p.stderr[-300:]))
        ops.queue(dict(floor, now='2026-09-09 12:00:40'), out, cache_file, repo=repo, board=board, answers=[dict(id='b1', title='The first', when='2026-01-01 10:00:00')])
        case('ops: the queue it writes lists the answers nobody has taken up that it was handed', '"kind": "untaken"' in (out / 'data' / 'queue.js').read_text(encoding='utf-8')
             and '"kind": "untaken"' not in json.dumps(q2), (out / 'data' / 'queue.js').read_text(encoding='utf-8')[:200])


# A shell command and the room it is work in (src_acts.of_command), a group per rule. Most are shapes the transcripts
# of this project are full of; the ones in pairs are a rule and the thing that looks like it and is not.
COMMANDS = [
    ('only looking is the lab', 'lab', [
        'ls', 'ls -la | head', 'git log', 'grep -n "a|b" f | cut -c1-80', 'cd x && S="a b"; for f in a b; do cat $f | head -3; done',
        'echo "n: $(ls | wc -l)"', 'echo x 2>&1 | head', 'sleep 5; tail -3 log', 'git branch -a', 'git branch --show-current',
        'git branch --list "lane/*"', 'find . -name "*.py"', 'sed -n 1,5p f', 'cat <<EOF\nhello; rm x\nEOF', 'diff <(git show a:f) <(git show b:f)',
        'git stash list', 'git worktree list', 'git fetch origin && git log origin/main -3', 'Get-ChildItem x | Select-String foo',
        'Get-Content x.txt | Select-Object -First 5', 'git diff --stat > /dev/null', 'GIT_PAGER=cat git log -3', 'if [ -f x ]; then echo yes; fi',
        'while read l; do echo $l; done < f', 'f=$(grep -l x *.py | head -1); cat "$f"', 'git log --oneline | head -5; git status --short',
        'git tag -l', 'git config --get user.name', 'git config user.name', 'ls | xargs grep foo', "sed 's/a/b/' f | head", 'cd comfy && ls',
        '$p = Get-Process Unity -ErrorAction SilentlyContinue; $p', "'=== a ==='; Get-ChildItem x", 'Start-Sleep -Seconds 5; Get-Content log.txt -Tail 5',
        '(cd x && git log -3)', 'SP="$(cygpath -w "$TEMP")"; ls "$SP"', 'ls > /dev/null 2>&1; echo $?', 'git branch -a --list "a" "b"']),
    ('a test or a gate is the lab', 'lab', [
        'powershell -NoProfile -ExecutionPolicy Bypass -File gate.ps1 -EditOnly', 'git status && python Tools/assetboard/test_assetboard.py', 'npm test',
        'unity test --mode EditMode', '"C:/x/Unity.exe" -batchmode -runTests -projectPath .', 'python Tools/otr.py', 'python validate.py', 'pytest -q',
        'dotnet test', 'timeout 600 python Tools/toolcheck.py']),
    ('a film, a render or a sheet is the studio', 'studio', [
        'python - <<EOF\nimport subprocess; subprocess.run(["ffmpeg"])\nEOF\nls', 'ffmpeg -i a b', '"C:/Program Files/Blender Foundation/Blender 5.0/blender.exe" -b -P x.py',
        'python Tools/assetboard/films.py', 'python Tools/assetboard/gamefilm.py film Maw', 'python Tools/assetboard/sprites.py', 'npx remotion render',
        'G=x/ffmpeg.exe; $G -i a b', 'cd ComfyUI && python main.py', 'find . -name "*.mp4" -exec ffmpeg -i {} {}.gif \\;']),
    ('a build, or anything else that does something, is the workshop', 'shop', [
        'python Tools/assetboard/build.py --no-films', 'echo "n: $(rm -rf x)"', 'echo x > f', "python - <<'EOF'\nprint(1)\nEOF", 'cd x', '',
        'find . -name x -delete', 'sed -i s/a/b/ f', 'npm run build', 'rm -rf x', 'python make_film.py',
        '"C:/x/Unity.exe" -batchmode -executeMethod TW.Editor.Build.BuildWindows', 'Tools/tw eval "return 1;"', 'dotnet build', 'pip install pillow',
        'Remove-Item x', 'grep ffmpeg x.py && python y.py', 'cat x | xargs rm', 'find . -name "*.cs" | xargs -n 1 rm', 'python a.py && \\\nbash b.sh 2>&1 | head',
        'nohup python server.py &', 'echo "a" >> notes.md', 'echo "Blender moved it" >> notes.md', "cat > x.py <<'EOF'\nimport ffmpeg\nEOF",
        'grep -c unity x.log > n.txt && python y.py']),
    ('git that changes something, and a landing, are the workroom', 'work', [
        'git -C x commit -m "run gate.ps1"', 'git add Tools/assetboard/build.py && git commit -m x', 'git branch foo', 'python Tools/land.py', 'git stash',
        'git worktree add ../x', 'git commit -m "$(cat <<\'EOF\'\nFilms: ffmpeg and blender\n\nCo-Authored-By: x\nEOF\n)"', 'git diff > patch.diff',
        'git push origin HEAD', 'git checkout -- gate.ps1', 'ls; git tag v1', 'git config user.name x', 'cd comfy && git commit -m x']),
]


def house():
    A = src_acts
    for what, room, cmds in COMMANDS:
        got = {c: A.of_command(c) for c in cmds}
        case(f'house: a command: {what}', all(r == room for r in got.values()), {c: r for c, r in got.items() if r != room})
    tools = [('Read', dict(file_path='a.cs'), 'lab'), ('Grep', dict(pattern='x'), 'lab'), ('Edit', dict(file_path='Assets/a.cs'), 'work'),
             ('Write', dict(file_path='C:\\Users\\x\\.claude\\plans\\a-plan.md'), 'plan'), ('Write', dict(file_path='art/Frog.PNG'), 'studio'),
             ('Bash', dict(command='ls'), 'lab'), ('PowerShell', dict(command='git -C x commit -m y'), 'work'), ('Agent', dict(prompt='go'), 'plan'),
             ('AskUserQuestion', {}, 'plan'), ('Skill', dict(skill='trench:tw-critic'), 'lab'), ('ScheduleWakeup', dict(delaySeconds=60), None),
             ('mcp__AfterEffectsMCP__run-script', {}, 'studio'), ('mcp__unity__eval', {}, 'shop'), ('mcp__station__station_send', {}, 'work'),
             ('SomethingNew', None, 'work'), (None, 'not a dict', 'work')]
    got = [(n, A.of_tool(n, i)) for n, i, _ in tools]
    case('house: a tool call says which room its work is in, and a call that only waits says none', got == [(n, r) for n, _, r in tools], [g for g, t in zip(got, tools) if g[1] != t[2]])

    case('house: the room is where most of the last calls point, a later call counting for more, the newest breaking a tie',
         A.pick([(100, 'lab'), (110, 'lab'), (120, 'work')]) == 'lab' and A.pick([(100, 'lab'), (110, 'work')]) == 'work'
         and A.pick([(t, 'lab') for t in range(4)] + [(4 + t, 'work') for t in range(3)]) == 'work' and A.pick([(1, 'lab'), (2, 'work'), (3, 'work'), (4, 'lab')]) == 'lab' and A.pick([(None, 'shop'), (100, None)]) == 'shop',
         [A.pick([(100, 'lab'), (110, 'lab'), (120, 'work')]), A.pick([(100, 'lab'), (110, 'work')]), A.pick([(t, 'lab') for t in range(4)] + [(4 + t, 'work') for t in range(3)]), A.pick([(1, 'lab'), (2, 'work'), (3, 'work'), (4, 'lab')])])
    old = [(t, 'studio') for t in range(0, 50, 10)]
    case('house: a call more than three minutes before the newest says nothing about now, nor does one before the last twelve, and no call is no room',
         A.pick(old + [(400, 'lab')]) == 'lab' and A.pick(old + [(170, 'lab')]) == 'studio' and A.pick([(t, 'work') for t in range(20)] + [(20 + t, 'lab') for t in range(12)]) == 'lab'
         and A.pick([]) is None and A.pick([(5, None)]) is None, [A.pick(old + [(400, 'lab')]), A.pick(old + [(170, 'lab')])])

    want = {'tw-critic': 'lab', 'tw-bug-catcher': 'lab', 'tw-balance-sim': 'lab', 'tw-master': 'plan', 'pipeline': 'plan', 'tw-vfx-sheets': 'studio',
            'tw-destruction-vfx': 'studio', 'tw-character-sim': 'studio', 'tw-env-sim': 'studio', 'tw-vehicle-sim': 'shop', 'tw-optimizer': 'shop', 'unity-pipeline': 'shop'}
    homes = {r['id']: r['home'] for r in src_ops.roster(build.REPO)}
    case('house: every project skill has the room of its trade, on the roster too', {k: A.of_skill(k) for k in want} == want and {k: homes.get(k) for k in want} == want,
         {k: (A.of_skill(k), homes.get(k)) for k in want if A.of_skill(k) != want[k] or homes.get(k) != want[k]})
    got = [A.of_agent('Explore'), A.of_agent('general-purpose', 'Review the diff for bugs'), A.of_skill('emtd-icon-agent', 'Generate new game icons'),
           A.of_agent('helper', 'Start the next part of the artifact right away'), A.of_skill('logon-helper'), A.of_agent('gamedesign'), A.of_skill('docx', 'make a document'),
           A.of_machine('the gate'), A.of_machine('Blender, rendering films'), A.of_machine('Unity editor'), A.of_machine('a machine nobody listed')]
    case('house: an agent, a skill nobody listed and a machine go by the words of what they do, and no word is found inside another (start is not art)',
         got == ['lab', 'lab', 'studio', 'work', 'work', 'plan', 'work', 'lab', 'studio', 'shop', 'shop'], got)
    case('house: every machine the floor knows has a room, and the rule book names no machine the floor does not',
         set(A.MACHINES) <= {m[2] for m in src_ops.MACHINES} and all(A.of_machine(m[2]) in A.ROOMS for m in src_ops.MACHINES), set(A.MACHINES) - {m[2] for m in src_ops.MACHINES})

    # which checkout: two checkouts of which one's name begins the other's, and a session started in neither
    tmp = Path(tempfile.mkdtemp(prefix='tw-house-test-'))
    one, pipe, away, inner = tmp / 'githubtest', tmp / 'githubtest-pipe', tmp / 'claude', tmp / 'githubtest' / '.claude' / 'worktrees' / 'a'
    for d in (one, pipe, away, inner):
        d.mkdir(parents=True)
    trees, spell = [one, pipe, inner], src_ops.spellings([one, pipe, inner])
    fwd = str(pipe).replace('\\', '/')
    bash = '/' + fwd[0].lower() + fwd[2:]
    got = [src_ops.pointed(i, spell) for i in (dict(file_path=str(pipe) + '\\Tools\\a.py'), dict(command=f'cd "{fwd}" && git status'), dict(command=f'git -C {bash.upper()} log -3'),
                                              dict(path=str(one)), dict(command=f'ls {fwd}x'), dict(command='ls'), dict(file_path=str(away / 'notes.md')), dict(file_path=str(inner / 'a.cs')))]
    case('house: a call points into the checkout its path or its command names, spelled any of three ways; githubtest is not found in githubtest-pipe, and a worktree inside a checkout is itself',
         got == [pipe, pipe, pipe, one, None, None, None, inner], got)
    case('house: a session started outside every checkout belongs to the one most of its last calls point into, the one named last when level, and to none below three',
         src_ops.home_of(str(away), [pipe, one, None, pipe, pipe, one], trees) == pipe and src_ops.home_of(str(away), [one, pipe, one, pipe, one, pipe], trees) == pipe
         and src_ops.home_of(str(away), [pipe, pipe, None, one], trees) is None and src_ops.home_of(None, [], trees) is None
         and src_ops.home_of(str(one / 'Tools'), [pipe] * 9, trees) == one,
         [src_ops.home_of(str(away), [pipe, one, None, pipe, pipe, one], trees), src_ops.home_of(str(away), [pipe, pipe, None, one], trees), src_ops.home_of(str(one / 'Tools'), [pipe] * 9, trees)])

    # the sessions, from transcripts written here: when, in which folder, with which calls
    now = time.time()
    keep, src_ops.PROJECTS = src_ops.PROJECTS, tmp / 'projects'
    stamp = lambda ago: datetime.datetime.fromtimestamp(now - ago, datetime.timezone.utc).isoformat().replace('+00:00', 'Z')
    call = lambda n, name, inp, ago=30: dict(type='assistant', timestamp=stamp(ago), message=dict(content=[dict(type='tool_use', id=f't{n}', name=name, input=inp)]))
    said = lambda cwd, **more: dict(type='user', cwd=str(cwd), timestamp=stamp(60), message=dict(content='go on'), **more)
    answer = lambda n: dict(type='user', message=dict(content=[dict(type='tool_result', tool_use_id=f't{n}', content='yes')]))

    def write(name, entries, age, folder='p'):
        f = src_ops.PROJECTS / folder / f'{name}.jsonl'
        f.parent.mkdir(parents=True, exist_ok=True)
        f.write_text('\n'.join(json.dumps(e) for e in entries), encoding='utf-8')
        os.utime(f, (now - age, now - age))
        return f

    def read():
        return {s['id']: s for s in src_ops.sessions(trees, now)}

    into = [call(k, 'Read', dict(file_path=str(pipe / 'a.py'))) for k in range(3)]
    write('outside0', [said(away)] + into + [call(9, 'Bash', dict(command='ls'))], 30)
    write('outside1', [said(away)] + into[:2], 30)
    write('inside00', [said(one), call(1, 'Edit', dict(file_path='a.cs')), call(2, 'Edit', dict(file_path='b.cs'))], 30)
    got = read()
    case('house: a session belongs to the checkout it was started in, or, started outside every one, to the one its calls point into; one that points nowhere is not on the floor',
         {k: s['tree'] for k, s in got.items()} == {'outside0': pipe, 'inside00': one}, {k: s['tree'] for k, s in got.items()})
    rooms = {k: src_ops.session_worker(s) for k, s in got.items()}
    case('house: a session at work is in the room of its last calls', [(rooms[k]['act'], rooms[k]['state']) for k in ('outside0', 'inside00') if k in rooms] == [('lab', 'working'), ('work', 'working')],
         {k: (w['act'], w['state']) for k, w in rooms.items()})

    quiet = src_ops.WORKING + 600
    parent = write('waiting0', [said(one), call(1, 'Agent', dict(prompt='look into it'))], quiet)
    sub = parent.with_suffix('') / 'subagents'
    sub.mkdir(parents=True)
    (sub / 'agent-abc123def456.meta.json').write_text(json.dumps(dict(agentType='Explore', description='Find the callers')), encoding='utf-8')
    log = sub / 'agent-abc123def456.jsonl'
    log.write_text('\n'.join(json.dumps(e) for e in [dict(type='user', message=dict(content='Find every caller of SimHost')), call(1, 'Grep', dict(pattern='SimHost')), call(2, 'Read', dict(file_path='a.cs'))]), encoding='utf-8')
    s = read()['waiting0']
    w = src_ops.session_worker(s)
    case('house: a session that waits on an agent still running is at work, though its own transcript went quiet, and the agent has a name of its own and the room of its calls',
         s['working'] and w['state'] == 'working' and w['act'] == 'plan' and [(a['type'], a['uid'], a['act'], a['what']) for a in s['agents']] == [('Explore', 'agent:Explore#abc123', 'lab', 'Find every caller of SimHost')],
         (s['working'], w, s['agents']))
    os.utime(log, (now - src_ops.AGENT_FRESH - 60,) * 2)
    s = read()['waiting0']
    case('house: once its agents are done too it rests, in the bunkhouse', not s['working'] and s['agents'] == [] and src_ops.session_worker(s)['act'] == 'bunk' and 'wait' not in src_ops.session_worker(s),
         (s['working'], s['agents'], src_ops.session_worker(s)))
    write('longgone', [said(one), call(1, 'Edit', dict(file_path='a.cs'))], src_ops.RECENT + 60)
    case('house: a session nobody wrote to for hours is not on the floor', 'longgone' not in read(), sorted(read()))

    asked = [said(one), call(1, 'Edit', dict(file_path='a.cs')), call(2, 'AskUserQuestion', dict(questions=[]))]
    write('asking00', asked, 30)
    write('asking01', asked, quiet)
    write('asked000', asked + [answer(2)], 30)
    write('asked001', asked + [answer(2), call(3, 'Edit', dict(file_path='a.cs'))], 30)
    write('planned0', [said(one), call(1, 'ExitPlanMode', dict(plan='the plan'))], 30)
    got = {k: src_ops.session_worker(s) for k, s in read().items()}
    case('house: a session whose last call asks the owner waits on the owner, in the war room, and still says so when its transcript has gone quiet',
         [(got[k]['act'], got[k].get('wait'), got[k]['state']) for k in ('asking00', 'asking01', 'planned0')] == [('plan', 'owner', 'working'), ('plan', 'owner', 'resting'), ('plan', 'owner', 'working')],
         [(got[k]['act'], got[k].get('wait'), got[k]['state']) for k in ('asking00', 'asking01', 'planned0')])
    case('house: once the answer is in it waits no longer', [(got[k]['act'], got[k].get('wait')) for k in ('asked000', 'asked001')] == [('plan', None), ('work', None)],
         [(got[k]['act'], got[k].get('wait')) for k in ('asked000', 'asked001')])
    mode = lambda kind: dict(type='attachment', attachment=dict(type=kind))
    edits = [call(k, 'Edit', dict(file_path='a.cs')) for k in range(3)]
    write('planmode', [said(one, permissionMode='plan')] + edits, 30)
    write('planover', [said(one, permissionMode='plan')] + edits[:1] + [mode('plan_mode_exit')] + edits[1:], 30)
    write('planback', [said(one, permissionMode='auto')] + edits[:1] + [mode('plan_mode_reentry')] + edits[1:], 30)
    write('planrest', [said(one, permissionMode='plan')] + edits, quiet)
    got = {k: src_ops.session_worker(s)['act'] for k, s in read().items() if k.startswith('plan') and k != 'planned0'}
    case('house: a session in plan mode is in the war room whatever its calls, until it leaves the mode or rests',
         got == dict(planmode='plan', planover='work', planback='plan', planrest='bunk'), got)
    src_ops.PROJECTS = keep

    # the frog's sheets, from frames drawn here: a green block that moves a pixel a frame on a 64 px canvas
    try:
        from PIL import Image
    except ImportError:
        print('      (no Pillow on this machine: the packing cases were not run)')
        Image = None
    if Image:
        src, cache = tmp / 'frogs', tmp / 'cache' / 'frog'

        def frame(x0, flip=False, top=10, low=50):
            im = Image.new('RGBA', (64, 64), (0, 0, 0, 0))
            im.paste((40, 160, 60, 255), (x0, top, x0 + 20, low))
            im.paste((20, 60, 20, 255), (x0, top, x0 + 4, top + 4))            # an eye, so its mirror is not itself
            return im.transpose(Image.FLIP_LEFT_RIGHT) if flip else im

        def save(folder, name, frames):
            (src / folder).mkdir(parents=True, exist_ok=True)
            for k, im in enumerate(frames):
                im.save(src / folder / f'{name}_{k + 1:02}.png')

        save('normalized/258/SW', 'SW', [frame(20 + k, low=46 if k else 50) for k in range(3)])      # its feet are off the ground on most frames
        save('normalized/258/SE', 'SE', [frame(20 + k, flip=True, low=46 if k else 50) for k in range(3)])
        save('normalized/258/S', 'S', [frame(22, low=46) for k in range(3)])
        save('normalized/258/thumbs', 'x', [frame(0)])
        save('actions/258/think', 'think', [frame(22, top=6), frame(22, top=8)])
        save('actions/258/think_flip', 'think_flip', [frame(22, flip=True)])
        save('actions/258/office', 'office', [frame(10)])
        frame(8).save(src / 'actions' / '258' / 'office_desk.png')
        (src / 'actions' / 'think.json').write_text('{"fps": 4.0, "loop": true}')
        packed = []
        man = sprites.pack(src, cache, packed.append)
        a, w = man['anims'], man['walk']
        sw = a.get('walk_SW', {})
        case('frog: a walk is one sheet of its frames, cut to what is drawn on any of them with two pixels round it, and the manifest says where the cut was',
             {k: sw.get(k) for k in ('n', 'cols', 'w', 'h', 'ox', 'oy', 'fps', 'loop', 'stride')} == dict(n=3, cols=3, w=26, h=44, ox=18, oy=8, fps=8, loop=True, stride=5.6)
             and Image.open(cache / 'walk_SW.png').size == (78, 44) and Image.open(cache / 'walk_SW.png').getpixel((26 + 2 + 1, 2)) == (20, 60, 20, 255), sw)
        small = Image.open(cache / 'walk_SW@0.5.png')
        edge = [px for px in (small.getpixel((x, y)) for x in range(13) for y in range(11, 22)) if px[3] > 24]
        case('frog: every sheet has a half-size twin, a frame of it half as wide and as high, and the edge of the frog is not darkened on it',
             small.size == (39, 22) and all((cache / f'{k}@0.5.png').exists() for k in list(a) + ['desk']) and len({px[3] for px in edge}) > 1
             and all(abs(px[1] - 160) <= 8 for px in edge), (small.size, sorted({(px[1], px[3]) for px in edge})))
        case('frog: a walk that is another seen in a mirror is not packed: its heading is the other, flipped',
             sorted(k for k in a if k.startswith('walk_')) == ['walk_S', 'walk_SW'] and w['SE'] == dict(anim='walk_SW', flip=True) and w['SW'] == dict(anim='walk_SW', flip=False)
             and not (cache / 'walk_SE.png').exists(), (sorted(a), w.get('SE')))
        case('frog: all eight headings are there; one with no walk of its own is drawn with the nearest, as a stand-in',
             list(w) == list(sprites.HEADINGS) and w['N'] == dict(anim='walk_SW', flip=True, standin=True) and w['W'] == dict(anim='walk_SW', flip=False, standin=True)
             and w['E'] == dict(anim='walk_SW', flip=True, standin=True) and 'standin' not in w['S'], w)
        case('frog: the feet are measured: the lowest row any walk frame reaches, under the middle of the head', man['foot'] == [32, 50] and man['cell'] == 64, (man['foot'], man['cell']))
        case('frog: an action plays at the speed its json gives, else at the table\'s; its mirrored twin is not packed, and one that is missing is said and left out',
             (a['think']['fps'], a['think']['n'], a['think']['oy'], a['office']['fps'], a['office']['loop']) == (4, 2, 4, 8, True) and 'think_flip' not in a and 'sleep_in' not in a
             and any('sleep_in' in n for n in packed), (a.get('think'), packed))
        case('frog: the empty desk is a still on the canvas of the office loop', man['stills'] == dict(desk=dict(w=24, h=44, ox=6, oy=8)) and Image.open(cache / 'desk.png').size == (24, 44), man['stills'])

        keep = ops.CREW
        ops.CREW = tmp / 'nothing'
        site = tmp / 'frogsite'
        none = ops.frog(site) == {} and (site / 'data' / 'frog.js').read_text(encoding='utf-8') == 'window.FROG = {};\n' and not (site / 'img').exists()
        ops.CREW = tmp / 'cache'
        got = ops.frog(site)
        copied = sorted(f.name for f in (site / 'img' / 'frog').glob('*.png'))
        case('frog: the sheets the manifest names are copied into the site and it is the page\'s data; a station with none gets an empty one and no error',
             none and got == man and copied == sorted(f.name for f in cache.glob('*.png')) and (site / 'img' / 'frog' / 'think.png').read_bytes() == (cache / 'think.png').read_bytes()
             and (site / 'data' / 'frog.js').read_text(encoding='utf-8') == f'window.FROG = {json.dumps(man, sort_keys=True)};\n', (none, copied))
        (cache / 'think@0.5.png').unlink()
        case('frog: a pack that lost a sheet is no pack: the page is told there are no frogs, not sent to draw a hole', ops.frog(site) == {} and 'FROG = {}' in (site / 'data' / 'frog.js').read_text(encoding='utf-8'))
        ops.CREW = keep

    # the page itself: everything house.html loads from the site is put there by ops.page (the readings are ops.once's)
    try:
        import jinja2  # noqa: F401
    except ImportError:
        print('      (no jinja2 on this machine: the page was not written)')
        return
    keep = ops.CREW
    ops.CREW = tmp / 'nothing'
    out = tmp / 'pagesite'
    ops.page(out, dict(built='then', station='here', commit='abc', refs_as_of='then'))
    ops.CREW = keep
    html = (out / 'house.html').read_text(encoding='utf-8') if (out / 'house.html').exists() else ''
    loads = re.findall(r'(?:src|href)="([^"#:]+\.(?:js|css))"', html)
    readings = {'data/ops.js', 'data/queue.js', 'data/beat.js', 'data/notes.js', 'data/graphs.js'}
    case('house: the page is written with every script and style it loads, with everyone listed under the picture, and it leads back to the control screen',
         {'house.js', 'housedraw.js', 'house.css', 'data/frog.js', 'data/crew.js', 'crew.js'} <= set(loads) and not [u for u in loads if u not in readings and not (out / u).exists()]
         and 'id="h-list"' in html and 'id="h-canvas"' in html and 'href="index.html#deck"' in html, [u for u in loads if u not in readings and not (out / u).exists()])
    gl = re.findall(r'(?:src|href)="([^"#:]+\.(?:js|css))"', (out / 'graphs.html').read_text(encoding='utf-8')) if (out / 'graphs.html').exists() else []
    box = (out / 'data' / 'notebox.js').read_text(encoding='utf-8') if (out / 'data' / 'notebox.js').exists() else ''
    gh = (out / 'graphs.html').read_text(encoding='utf-8') if (out / 'graphs.html').exists() else ''
    case('board: the graphs page and the note panel are written with what they load, the page has its five graphs and leads back to the control screen, and the pages are told where notes go and with which key',
         {'charts.js', 'board.js', 'board.css', 'data/notebox.js', 'data/graphs.js'} <= set(gl) and not [u for u in gl if u not in readings and not (out / u).exists()]
         and all(f'id="g-{g}"' in gh for g in ('rooms', 'lanes', 'needs', 'commits', 'models')) and 'href="index.html#graphs"' in gh
         and notes.key_of(notes.folder()) in box and '127.0.0.1' in box, ([u for u in gl if u not in readings and not (out / u).exists()], box))
    case('control: the files the control screen loads beyond the other pages\' are put in the site by ops.page too', (out / 'control.js').exists() and (out / 'control.css').exists())


def board():
    """The owner's notes (notes.py), the numbers behind the graphs (src_graphs.py), and the page's own rules for both."""
    import urllib.error
    import urllib.request
    tmp = Path(tempfile.mkdtemp(prefix='tw-board-test-'))
    where = tmp / 'notes'
    day = datetime.datetime(2026, 10, 5, 12, 0, 0)
    a = notes.write(where, 'Make the tracks muddier.\nAnd the hull darker.', kind='asset', about='Maw', title='Maw', asset='Maw', now=day)
    b = notes.write(where, 'Rebase this before you go on.', kind='lane', about='lane/show/x', lane='lane/show/x', now=day)
    c = notes.write(where, 'A second one in the same second.', kind='asset', about='Maw', asset='Maw', now=day)
    got = notes.read_all(where)
    case('notes: a note is a file of its own, read back as it was written, and two in one second about one thing are two files',
         [n['id'] for n in got] == [a['id'], c['id'], b['id']] and len({a['id'], c['id']}) == 2
         and next(n for n in got if n['id'] == a['id'])['text'] == 'Make the tracks muddier.\nAnd the hull darker.' and all(n['state'] == 'open' for n in got), [n['id'] for n in got])
    case('notes: a session reads the notes for its branch and those for no branch, not another branch\'s',
         sorted(n['id'] for n in notes.for_lane(got, 'lane/show/x')) == sorted([a['id'], b['id'], c['id']])
         and sorted(n['id'] for n in notes.for_lane(got, 'lane/show/y')) == sorted([a['id'], c['id']]), [n['id'] for n in notes.for_lane(got, 'lane/show/y')])
    done = notes.answer(where, b['id'], 'Rebased onto integration.', by='lane/show/x', now=day)
    again = {n['id']: n for n in notes.read_all(where)}
    case('notes: an answer closes a note and stays with it, who answered and when',
         done['state'] == 'done' and again[b['id']]['answers'] == [dict(when='2026-10-05 12:00', by='lane/show/x', text='Rebased onto integration.')]
         and again[b['id']]['text'] == 'Rebase this before you go on.' and again[a['id']]['state'] == 'open', again[b['id']])
    refused = []
    for bad in (dict(text=''), dict(text='x' * (notes.LONGEST + 1)), dict(text='x', kind='elsewhere')):
        try:
            notes.write(where, **bad)
        except ValueError as e:
            refused.append(str(e))
    try:
        notes.answer(where, '2026', 'which one?')
    except ValueError as e:
        refused.append(str(e))
    case('notes: an empty note, one too long, one about nothing the board has and an answer to no one note are refused', len(refused) == 4, refused)
    old = notes.write(where, 'Long ago.', now=day - datetime.timedelta(days=30))
    notes.answer(where, old['id'], 'Done long ago.', now=day - datetime.timedelta(days=29))
    shown = [n['id'] for n in notes.shown(notes.read_all(where), now=day)]
    case('notes: the page lists every open note and the lately answered, not one answered weeks ago', sorted(shown) == sorted([a['id'], b['id'], c['id']]), shown)

    # the listener the page writes through: only with the key, and never a path of the sender's
    box, key = notes.serve(where, port=0)
    url = f'http://127.0.0.1:{box.server_address[1]}'

    def send(path, body):
        req = urllib.request.Request(url + path, data=json.dumps(body).encode('utf-8'), headers={'Content-Type': 'text/plain'})
        try:
            with urllib.request.urlopen(req, timeout=10) as r:
                return r.status, json.loads(r.read().decode('utf-8'))
        except urllib.error.HTTPError as e:
            return e.code, json.loads(e.read().decode('utf-8'))
    before = len(list(where.glob('*.md')))
    no_key = send('/note', dict(text='I am the owner, honest.', kind='lane'))
    wrong = send('/note', dict(key=key + 'x', text='I am the owner, honest.'))
    ok = send('/note', dict(key=key, text='From the page.', kind='asset', about='Maw', asset='..\\..\\outside', lane='../up', title='Maw'))
    files = sorted(f.name for f in tmp.rglob('*') if f.is_file())
    case('notes: the listener takes a note only with the key from the notes folder',
         no_key[0] == 403 and wrong[0] == 403 and ok[0] == 200 and ok[1]['note']['text'] == 'From the page.' and len(list(where.glob('*.md'))) == before + 1, (no_key, wrong, ok[0]))
    case('notes: whatever the page sends as its asset or branch, the file is written in the notes folder and nowhere else',
         all(f.parent == where for f in tmp.rglob('*') if f.is_file()) and re.fullmatch(r'[0-9a-z-]+', ok[1]['note']['id']) is not None, files)
    closed = send('/close', dict(key=key, id=ok[1]['note']['id']))
    case('notes: the owner can close a note from the page', closed[0] == 200 and closed[1]['note']['state'] == 'done' and send('/close', dict(key=key, id='nothing-like-it'))[0] == 400, closed)
    seen = send('/note', dict(key=key, text='A: Fix it', kind='page', about='brief:b1', then='ab12cd34'))
    head = (where / (seen[1]['note']['id'] + '.md')).read_text(encoding='utf-8').replace('\r\n', '\n') if seen[0] == 200 else ''
    case('notes: a click on the Decide page carries the stamp of the Then line the page showed: it is in the head of the note and read back, and a note without one has no such line',
         seen[0] == 200 and '\nthen: ab12cd34\n' in head and {n['id']: n for n in notes.read_all(where)}[seen[1]['note']['id']].get('then') == 'ab12cd34'
         and '\nthen:' not in (where / (ok[1]['note']['id'] + '.md')).read_text(encoding='utf-8'), head[:300])
    box.shutdown()
    box.server_close()

    # the graphs: the work by hour and room and by branch, from transcripts written here
    now = time.mktime((2026, 10, 5, 15, 30, 0, 0, 0, -1))
    proj, one, away = tmp / 'projects', tmp / 'checkout', tmp / 'claude'
    for d in (one, away):
        d.mkdir()
    trees = {one: dict(branch='lane/show/x')}
    stamp = lambda t: datetime.datetime.fromtimestamp(t, datetime.timezone.utc).isoformat().replace('+00:00', 'Z')
    call = lambda t, name, inp, **more: dict(type='assistant', cwd=str(away), timestamp=stamp(t), message=dict(content=[dict(type='tool_use', id='t', name=name, input=inp)]), **more)
    main_log = proj / 'p' / 'session1.jsonl'
    main_log.parent.mkdir(parents=True)
    rows = [call(now - 7200, 'Read', dict(file_path=str(one / 'a.cs'))), call(now - 7100, 'Edit', dict(file_path=str(one / 'a.cs'))), call(now - 60, 'Edit', dict(file_path=str(one / 'b.cs'))),
            call(now - 50, 'Read', dict(file_path='x'), isSidechain=True), call(now - 40, 'ScheduleWakeup', dict(delaySeconds=60))]
    main_log.write_text('\n'.join(json.dumps(r) for r in rows) + '\n', encoding='utf-8')
    side = proj / 'p' / 'session1' / 'subagents' / 'agent-1.jsonl'
    side.parent.mkdir(parents=True)
    side.write_text(json.dumps(call(now - 50, 'Read', dict(file_path='x'), isSidechain=True)) + '\n', encoding='utf-8')
    for f in (main_log, side):
        os.utime(f, (now, now))
    cache = {}
    act = src_graphs.activity(trees, now, cache, projects=proj)
    by = {h['h'][-2:]: {r: h[r] for r in src_graphs.ROOMS if h[r]} for h in act['hours'] if any(h[r] for r in src_graphs.ROOMS)}
    case('graphs: every tool call is counted once, in its hour and its room; an agent\'s call in its own log only; a call that only waits not at all',
         by == {'13': dict(work=1, lab=1), '15': dict(work=1, lab=1)} and act['today'] == 4 and len(act['hours']) == src_graphs.HOURS, (by, act['today']))
    case('graphs: the work is the branch\'s its calls point into, when the session was started outside every checkout',
         act['lanes'] == [dict(branch='lane/show/x', calls=3)], act['lanes'])
    with open(main_log, 'a', encoding='utf-8') as h:
        h.write(json.dumps(call(now - 10, 'Bash', dict(command='ls'))) + '\n' + json.dumps(call(now - 5, 'Edit', dict(file_path='y')))[:40])     # and a line still being written
    os.utime(main_log, (now, now))
    off = cache['files'][str(main_log)]['off']
    act2 = src_graphs.activity(trees, now, cache, projects=proj)
    case('graphs: a second read takes only what was written since, and leaves a line still being written for the next',
         act2['today'] == 5 and cache['files'][str(main_log)]['off'] > off and cache['files'][str(main_log)]['off'] < main_log.stat().st_size
         and src_graphs.activity(trees, now, cache, projects=proj)['today'] == 5, (act2['today'], off, cache['files'][str(main_log)]['off']))
    store = tmp / 'history.jsonl'
    floor = lambda n, q: dict(lanes=[dict(workers=[dict(kind='session', state='working')] * n, items=[], dirty=0)], queue=dict(count=q))
    steps = [(0, 1, 3), (20, 1, 3), (40, 2, 3), (60, 2, 3), (60 + src_graphs.SAMPLE_EVERY, 2, 3), (src_graphs.KEEP_DAYS * 86400 + 1000, 0, 4)]
    kept = [len(src_graphs.history(store, src_graphs.sample(floor(n, q), now + t), now + t)) for t, n, q in steps]
    case('graphs: the floor\'s history gets a sample when a number moved or five minutes passed, and loses what is older than two weeks',
         kept == [1, 1, 2, 2, 3, 1] and len(store.read_text(encoding='utf-8').strip().split('\n')) == 1, kept)
    many = [dict(t=k) for k in range(5000)]
    case('graphs: the page gets a thinned history that still ends on the newest sample', len(src_graphs.thin(many)) <= 420 and src_graphs.thin(many)[-1] == many[-1] and src_graphs.thin(many[:7]) == many[:7])
    days = src_graphs.commits(build.REPO, time.time())
    case('graphs: the commits are a number for every one of the last thirty days', len(days) == src_graphs.COMMIT_DAYS and days[-1]['day'] == str(datetime.date.today())
         and all(isinstance(d['n'], int) for d in days), days[-3:])

    node = shutil.which('node')
    if not node:
        print('      (no node on this machine: the page\'s cases for notes and graphs were not run)')
        return
    js = ('const B = require(process.argv[1]), G = require(process.argv[2]);'
          'const N = [{id: "1", kind: "asset", about: "Maw", asset: "Maw", state: "open", when: "2026-10-01"}, {id: "2", kind: "lane", about: "lane/show/x", lane: "lane/show/x", state: "done", when: "2026-10-03"},'
          ' {id: "3", kind: "worker", about: "session:ab", lane: "lane/show/x", state: "open", when: "2026-10-02"}, {id: "4", kind: "queue", about: "decide: A question", state: "open", when: "2026-10-04"}];'
          'console.log(JSON.stringify([B.pick(N, {asset: "Maw"}).map(n => n.id), B.pick(N, {kind: "lane", id: "lane/show/x", lane: "lane/show/x"}).map(n => n.id), B.open(N, {kind: "lane", id: "lane/show/x", lane: "lane/show/x"}),'
          ' B.pick(N, {kind: "queue", id: "decide: A question"}).map(n => n.id), B.pick(N, {kind: "worker", id: "skill"}).length, B.open(N, {kind: "page", id: "all"}),'
          ' B.merged(N, [{id: "1"}, {id: "9"}], [{id: "u"}]).map(n => n.id), B.slug("lane/show/frog-house"),'
          ' G.ticks(1510, 4), G.ticks(3, 4), G.ticks(0, 4), G.fmt(4028), G.fmt(12900), G.hourLabel("2026-10-05 13"), G.dayLabel("2026-10-05"),'
          ' G.hourPlot(354), G.hourPlot(1180), G.hourPlot(0)]))')
    p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'board.js'), str(HERE / 'static' / 'charts.js')], capture_output=True)
    got = json.loads(p.stdout.decode() or 'null')
    case('page: a thing shows the notes about it (its model, its branch, or the very thing), the open ones first, and counts the open ones',
         got and got[:6] == [['1'], ['3', '2'], 1, ['4'], 0, 3], (got, p.stderr[-300:]))
    case('page: a note just sent shows until a reading lists it, once; one not sent shows too', got and got[6] == ['1', '2', '3', '4', '9', 'u'] and got[7] == 'room-lane-show-frog-house', got and got[6:8])
    case('page: a graph\'s axis has clean steps that reach its largest number, and its numbers are written short',
         got and got[8] == [0, 500, 1000, 1500, 2000] and got[9] == [0, 1, 2, 3] and got[10] == [0, 1] and got[11:15] == ['4,028', '12.9K', '13:00', 'Mon 5'], got and got[8:15])
    phone, wide, unknown = (got or [None] * 18)[15:18]
    case('page: in a plot as narrow as a phone the hour graph is drawn at the plot\'s own size (a unit is a px, so its words keep their size) with a name under every twelfth hour; '
         'on a wide page, and before the plot has a width, it is the wide drawing',
         phone and phone['narrow'] and phone['W'] == 354 and phone['every'] == 12 and (phone['W'] - phone['L'] - phone['R']) / 48 >= 4
         and wide == unknown and not wide['narrow'] and wide['W'] == 1000 and wide['every'] == 6, (phone, wide, unknown))


def png(path: Path, w, h):
    """A real picture of one grey, w by h, written without any library."""
    import struct
    import zlib
    chunk = lambda kind, data: struct.pack('>I', len(data)) + kind + data + struct.pack('>I', zlib.crc32(kind + data) & 0xffffffff)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', struct.pack('>IIBBBBB', w, h, 8, 0, 0, 0, 0)) + chunk(b'IDAT', zlib.compress((b'\x00' + b'\x80' * w) * h)) + chunk(b'IEND', b''))
    return path


def control():
    """The control screen (index.html): the last picture or film a worker had in its hands (src_visuals.py), the page
    with the house, the profile and the graphs on it, and the page's own rules for the profile and its moves."""
    tmp = Path(tempfile.mkdtemp(prefix='tw-control-test-'))
    work, art = tmp / 'work', tmp / 'art'
    work.mkdir()
    a, b, film = png(art / 'a.png', 8, 8), png(art / 'wide shot.png', 1400, 20), art / 'clip.mp4'
    film.write_bytes(b'not a film, but a file of that name ' * 40)
    V = src_visuals

    # ---- which files a call names
    got = [V.named('Read', dict(file_path=str(a))), V.named('Read', dict(file_path=str(work / 'a.cs'))), V.named('Edit', dict(file_path=str(work / 'x.PNG')), str(work)),
           V.named('Read', dict(file_path='shots/b.jpg'), str(work)), V.named('Read', dict(file_path='shots/b.jpg'))]
    case('visual: a file call names a picture when its file is one, by any spelling of the ending; a path that is not whole hangs under the session\'s folder, and without one says nothing',
         got == [[(str(a), 'read')], [], [(str(work / 'x.PNG'), 'read')], [(str(work / 'shots' / 'b.jpg'), 'read')], []], got)
    cmd = f'cd "{art}" && ffmpeg -i "wide shot.png" -vf scale=640:-1 out/clip.mp4 && ls *.png $f.png frame_%04d.png {{a,b}}.jpg && magick {a} --out=thumbs/t.webp; echo .png done.mkv'
    got = V.named('Bash', dict(command=cmd), str(work))
    case('visual: a command names the pictures and films written out in it, in its order, each once, under the folder it changes to; a pattern, a variable, a bare ending and a film no browser plays are not files',
         got == [(str(art / 'wide shot.png'), 'shell'), (str(art / 'out' / 'clip.mp4'), 'shell'), (str(a), 'shell'), (str(art / 'thumbs' / 't.webp'), 'shell')], got)
    got = V.named('PowerShell', dict(command='ffmpeg -i in.mov shots\\new.png'), str(work))
    case('visual: without a change of folder a command\'s files hang under the session\'s', got == [(str(work / 'shots' / 'new.png'), 'shell')], got)
    if os.name == 'nt':
        got = V.named('Bash', dict(command='cp /c/Users/x/shot.png /tmp/y.png'))
        case('visual: Git Bash\'s /c/Users/x is the C: drive, and a path under /tmp is nowhere this knows', got == [(os.path.normpath('C:/Users/x/shot.png'), 'shell')], got)

    # ---- which of them is the last one
    seen = []
    V.keep(seen, 100, [(str(a), 'read')])
    V.keep(seen, 200, [(str(b), 'read'), (str(film), 'shell')])
    V.keep(seen, 300, [(str(a), 'read')])
    case('visual: a transcript keeps each file once, at its newest mention, the newest last', seen == [[200, str(b), 'read'], [200, str(film), 'shell'], [300, str(a), 'read']], seen)
    many = []
    for k in range(V.KEEP + 9):
        V.keep(many, k, [(str(art / f'n{k}.png'), 'read')])
    case('visual: no more than KEEP are kept, the newest ones', len(many) == V.KEEP and many[-1][0] == V.KEEP + 8 and many[0][0] == 9, (len(many), many[0], many[-1]))
    gone = seen + [[400, str(art / 'gone.png'), 'read']]
    case('visual: the last visual is the newest one named that is still there', V.pick(gone) == dict(path=str(a), when=300, how='read') and V.pick([[1, str(art / 'gone.png'), 'read']]) is None and V.pick(None) is None, V.pick(gone))
    newer = [[100, str(a), 'read'], [500, str(film), 'shell']]
    keep_max = V.FILM_MAX
    first = V.pick(newer)
    V.FILM_MAX = 100
    second = V.pick(newer)
    V.FILM_MAX = keep_max
    case('visual: a film a command named after the last picture is the last visual; one too large to put in the site is passed over for what came before',
         first == dict(path=str(film), when=500, how='shell') and second == dict(path=str(a), when=100, how='read'), (first, second))

    # ---- on the floor: the workers get theirs, as files in the site
    now = time.time()
    proj = tmp / 'projects'
    stamp = lambda t: datetime.datetime.fromtimestamp(t, datetime.timezone.utc).isoformat().replace('+00:00', 'Z')
    call = lambda t, name, inp, **more: dict(type='assistant', cwd=str(work), timestamp=stamp(t), message=dict(content=[dict(type='tool_use', id='t', name=name, input=inp)]), **more)
    s1, s2 = proj / 'p' / 'aaaaaaaa-1111.jsonl', proj / 'p' / 'bbbbbbbb-2222.jsonl'
    ag = proj / 'p' / 'aaaaaaaa-1111' / 'subagents' / 'agent-abc123def456.jsonl'
    ag.parent.mkdir(parents=True)
    s1.write_text('\n'.join(json.dumps(r) for r in [call(now - 900, 'Read', dict(file_path=str(a))), call(now - 600, 'Read', dict(file_path=str(b))),
                                                    call(now - 500, 'Read', dict(file_path=str(a)), isSidechain=True), call(now - 300, 'Edit', dict(file_path=str(work / 'a.cs')))]) + '\n', encoding='utf-8')
    s2.write_text(json.dumps(call(now - 100, 'Read', dict(file_path=str(work / 'a.cs')))) + '\n', encoding='utf-8')
    ag.write_text(json.dumps(call(now - 50, 'Bash', dict(command=f'ffmpeg -i x.mkv "{film}"'), isSidechain=True)) + '\n', encoding='utf-8')
    cache = {}
    src_graphs.activity({}, now, cache, projects=proj)
    seen = {k: e.get('seen', []) for k, e in cache['files'].items()}
    case('visual: the pass that counts the calls keeps what each transcript named: a session its own calls, not its agents\'; an agent\'s from its own log',
         [x[1:] for x in seen[str(s1)]] == [[str(a), 'read'], [str(b), 'read']] and seen[str(s2)] == [] and [x[1:] for x in seen[str(ag)]] == [[str(film), 'shell']], seen)
    checkout = tmp / 'checkout'
    old = png(checkout / 'trench-warfare-3d' / 'Captures' / 'run' / 'old.png', 6, 6)
    fresh = png(checkout / 'trench-warfare-3d' / 'Captures' / 'new.png', 6, 6)
    os.utime(old, (now - 86400, now - 86400))
    os.utime(fresh, (now - 3600, now - 3600))
    floor = lambda: [dict(branch='lane/show/x', path=str(tmp / 'nowhere'), workers=[dict(kind='session', id='session:aaaaaaaa'), dict(kind='agent', id='agent:Explore', uid='agent:Explore#abc123'),
                                                                                  dict(kind='session', id='session:bbbbbbbb'), dict(kind='skill', id='tw-critic')]),
                     dict(branch='lane/show/y', path=str(checkout), workers=[dict(kind='session', id='session:cccccccc')])]
    site, lanes = tmp / 'site', floor()
    n = V.attach(lanes, seen, site, now)
    w = {x.get('uid') or x['id']: x for l in lanes for x in l['workers']}
    pic, clip, cap = w['session:aaaaaaaa'].get('visual') or {}, w['agent:Explore#abc123'].get('visual') or {}, w['session:cccccccc'].get('visual') or {}
    case('visual: a session has the last picture it looked at and an agent the film its command named, each by its own name; a worker whose transcript names none, and a skill, have none',
         n == 3 and pic.get('how') == 'read' and pic.get('name') == 'wide shot.png' and pic.get('kind') == 'picture' and abs(pic.get('at', 0) - (now - 600)) < 2
         and clip == dict(src='img/last/a-abc123.mp4', kind='film', how='shell', name='clip.mp4', at=int(now - 50))
         and 'visual' not in w['session:bbbbbbbb'] and 'visual' not in w['tw-critic'], (n, pic, clip))
    case('visual: a film is put in the site as it is', (site / 'img' / 'last' / 'a-abc123.mp4').read_bytes() == film.read_bytes())
    try:
        from PIL import Image
        with Image.open(site / pic['src']) as im:
            size = im.size
        case('visual: a picture is put in the site no wider than the page shows it, its shape kept', size[0] == V.WIDE and size[1] == round(20 * V.WIDE / 1400) and pic['src'] == 'img/last/s-aaaaaaaa.jpg', (size, pic))
    except ImportError:
        print('      (no Pillow on this machine: a picture was not made smaller, so its size was not looked at)')
    case('visual: a worker whose transcript names none has the newest picture of its branch\'s Captures folder, said to be that',
         cap.get('how') == 'capture' and cap.get('name') == 'new.png' and abs(cap.get('at', 0) - (now - 3600)) < 2, cap)
    os.utime(fresh, (now - (V.CAPTURE_DAYS + 1) * 86400,) * 2)
    os.utime(old, (now - (V.CAPTURE_DAYS + 2) * 86400,) * 2)
    lanes = floor()
    V.attach(lanes, seen, site, now)
    case('visual: a capture older than a week is nobody\'s last visual, and its copy leaves the site', 'visual' not in lanes[1]['workers'][0] and not list((site / 'img' / 'last').glob('s-cccccccc.*')),
         sorted(f.name for f in (site / 'img' / 'last').iterdir()))
    made = {f.name: f.stat().st_mtime_ns for f in (site / 'img' / 'last').iterdir()}
    time.sleep(0.05)
    V.attach(floor(), seen, site, now)
    case('visual: a reading that changes nothing writes nothing', made == {f.name: f.stat().st_mtime_ns for f in (site / 'img' / 'last').iterdir()}, made)
    lanes = floor()
    lanes[0]['workers'] = lanes[0]['workers'][:1]
    V.attach(lanes, seen, site, now)
    case('visual: when a worker leaves the floor its file leaves the site', sorted(f.name for f in (site / 'img' / 'last').iterdir()) == ['last.json', 's-aaaaaaaa.jpg'],
         sorted(f.name for f in (site / 'img' / 'last').iterdir()))

    # ---- a cache written before the transcripts were asked for their pictures is read again
    cache_path, keep_projects = tmp / 'graphs-cache.json', src_ops.PROJECTS
    cache_path.write_text(json.dumps(dict(files={str(s1): dict(off=s1.stat().st_size, hours={}, named={}, cwd=str(work))})), encoding='utf-8')
    src_ops.PROJECTS = proj
    try:
        seen2 = {}
        src_graphs.collect(build.REPO, tmp / 'site', dict(lanes=[], queue=dict(count=0)), {}, now, cache_path, tmp / 'history.jsonl', seen=seen2)
    finally:
        src_ops.PROJECTS = keep_projects
    case('visual: a cache of the version before is not trusted: its transcripts are read again, and the new one says which version it is',
         [x[1] for x in seen2.get(str(s1), [])] == [str(a), str(b)] and json.loads(cache_path.read_text(encoding='utf-8')).get('version') == src_graphs.VERSION, seen2.get(str(s1)))

    # ---- the page: one screen with the house, the profile and the graphs, each id once
    try:
        from jinja2 import Environment, FileSystemLoader, select_autoescape
    except ImportError:
        print('      (no jinja2 on this machine: the control screen was not written)')
    else:
        env = Environment(loader=FileSystemLoader(str(HERE / 'templates')), autoescape=select_autoescape(['html']), trim_blocks=True, lstrip_blocks=True)
        env.globals.update(STATUS_LABEL=model.STATUS_LABEL, CATEGORY=render.CATEGORY, BLURB=render.BLURB, KIND=render.KIND, meta=dict(built='then', station='here', commit='abc', refs_as_of='then'))
        html = env.get_template('index.html').render(sections=[], levels=[], total=0, root='')
        ids = re.findall(r'\bid="([^"]+)"', html)
        loads = re.findall(r'(?:src|href)="([^"#:?]+\.(?:js|css))"', html)
        need = {'house', 'h-canvas', 'h-tags', 'h-card', 'h-rooms', 'h-list', 'c-h-at', 'c-h-needs', 'c-age', 'profile', 'deck', 'graphs', 'g-rooms', 'g-lanes', 'g-needs', 'g-commits', 'g-models', 'stamp', 'p-ready', 'p-at', 't-calls', 't-calls-s', 't-notes-b', 'ideas', 'ideas-cards', 'queue', 'office', 'assets'}
        case('control: the overview is the control screen: the tiles, the house with the profile beside it and the five graphs are on it, and no id is there twice',
             need <= set(ids) and len(ids) == len(set(ids)), (sorted(need - set(ids)), sorted(i for i in set(ids) if ids.count(i) > 1)))
        order = [loads.index(u) for u in ('crew.js', 'office.js', 'house.js', 'housedraw.js', 'charts.js', 'control.js')] if {'crew.js', 'office.js', 'house.js', 'housedraw.js', 'charts.js', 'control.js'} <= set(loads) else []
        case('control: it loads the house\'s, the graphs\' and its own files, the frog and the graphs\' data, each after what it needs, and every one is a file of the board',
             order and order == sorted(order) and {'house.css', 'control.css', 'data/frog.js', 'data/graphs.js', 'data/ops.js'} <= set(loads)
             and not [u for u in loads if not u.startswith('data/') and not (HERE / 'static' / u).exists()], loads)
        case('control: the house and the graphs keep a page of their own, one click from the screen, and the top bar no longer lists them',
             'href="house.html"' in html and 'href="graphs.html"' in html and html.count('href="house.html"') == 1 and html.count('href="graphs.html"') == 1, html.count('href="house.html"'))
        at = {k: html.find(f'id="{k}"') for k in ('top', 'deck', 'ideas', 'queue', 'graphs', 'office', 'assets')}
        case('control: down the page: the head and the tiles, the house, the ideas, what waits on the owner, the graphs, the branch rooms, the models',
             -1 not in at.values() and list(at.values()) == sorted(at.values()) and html.find('c-pulse') < at['deck'], at)

    node = shutil.which('node')
    if not node:
        print('      (no node on this machine: the control screen\'s own cases were not run)')
        return
    js = ('const B = require(process.argv[1]).pure || require(process.argv[1]), K = require(process.argv[2]);'
          'const steps = []; for (let k = 0; k <= 10; k++) steps.push(K.step(0, 4028, k / 10));'
          'console.log(JSON.stringify([B.when(30), B.when(600), B.when(7200), B.when(4 * 86400),'
          ' B.caption({how: "read", at: 1000, name: "a.png"}, 1600), B.caption({how: "shell", at: 1000, name: "clip.mp4"}, 1030), B.caption({how: "capture", at: 0, name: "c.png"}, 3 * 86400), B.caption({how: "other", at: 9, name: "x"}, 5),'
          ' B.closable({}, false, false), B.closable({follow: true}, true, false), B.closable({}, true, true), B.closable({}, true, false),'
          ' steps, K.step(37, 12, 1), K.step(37, 12, 0), K.step(5, 9, 7), K.plain("4,028"), K.plain("37"), K.plain("12.9K"), K.plain("–"), K.grouped(4028), K.grouped(12),'
          ' K.headline(2, 37), K.headline(0, 0), K.headline(1, 1), K.headline(1200, 2), K.age(8), K.age(60), K.age(61), K.age(400), K.age(7300), K.age(-3)]))')
    p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'board.js'), str(HERE / 'static' / 'control.js')], capture_output=True)
    got = json.loads(p.stdout.decode() or 'null')
    case('control: the profile says what a visual is to the worker and how long ago, in words',
         got and got[:8] == ['just now', '10 min ago', '2 h ago', '4 days ago', 'Looked at 10 min ago · a.png', 'Named in a command just now · clip.mp4', "The branch's newest capture, 3 days ago · c.png", 'Last seen just now · x'],
         (got and got[:8], p.stderr[-300:]))
    case('control: a drawer can always be closed; docked, the worker the house follows and the panel at rest cannot, what the owner opened can', got and got[8:12] == [True, False, False, True], got and got[8:12])
    steps = got[12] if got else []
    case('control: a number counts up without stepping back, starts where it was and ends on its value exactly; a number written short is left as it is',
         got and steps[0] == 0 and steps[-1] == 4028 and steps == sorted(steps) and steps[5] > 2014 and got[13:22] == [12, 37, 9, 4028, 37, None, None, '4,028', '12'], got and got[12:22])
    case('control: the headline is the answer: how many are at work and how much waits, in words that fit one and none',
         got and got[22:26] == [['2 at work,', '37 wait on you.'], ['Nobody at work,', 'nothing waits on you.'], ['1 at work,', '1 waits on you.'], ['1,200 at work,', '2 wait on you.']], got and got[22:26])
    case('control: the age of the last reading is said in words, and a reading three readings old is called late, long before the hour that turns the page red',
         got and got[26:] == [dict(words='8 s ago', late=False), dict(words='1 min ago', late=False), dict(words='1 min ago, late', late=True), dict(words='7 min ago, late', late=True),
                              dict(words='2 h ago, late', late=True), dict(words='0 s ago', late=False)], got and got[26:])
    js = ('let ticks = [], n = {a: 0, b: 0}, asked = [];'
          'global.window = {}; global.setInterval = (f, ms) => { ticks.push([f, ms]); };'
          'global.document = { createElement: () => ({ remove() {} }), body: { appendChild: s => { asked.push(s.src.split("?")[0]); s.onload(); } } };'
          'require(process.argv[1]); const C = window.Crew; C.live(() => n.a++); C.live(() => n.b++); const first = [n.a, n.b, ticks.length]; ticks[0][0]();'
          'console.log(JSON.stringify([first, [n.a, n.b, ticks.length, ticks[0][1]], asked]))')
    p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'crew.js')], capture_output=True)
    got = json.loads(p.stdout.decode() or 'null')
    case('control: a page with several live parts reads the floor once every 20 seconds for all of them, each file once, and then every part draws',
         got == [[1, 1, 1], [2, 2, 1, 20000], ['data/beat.js', 'data/queue.js', 'data/graphs.js', 'data/briefs.js', 'data/ideas.js', 'data/ops.js']], (got, p.stderr[-300:]))


def decisions():
    """The decision briefs (briefs.py) and their page (decide.html): a decision that waits on the owner is a short
    report he can decide from, with what there is to see."""
    tmp = Path(tempfile.mkdtemp(prefix='tw-brief-test-'))
    where, day = tmp / 'decisions', datetime.datetime(2026, 10, 6, 10, 0, 0)
    wide, small, film = png(tmp / 'art' / 'Wide Shot.png', 2400, 30), png(tmp / 'art' / 'small.png', 40, 30), tmp / 'art' / 'clip.mp4'
    film.write_bytes(b'a file with a film\'s name ' * 30)
    good = dict(title='The house\'s look', what_for='What a room nobody works in looks like.', options=['Near-black', 'Light'], why='It is the site\'s own pair of panels.',
                evidence=[(str(wide), 'As built'), (str(film), 'The walk')])
    b = briefs.add(where, about='The house look', lane='lane/show/x', by='a session', now=day, **good)
    back = briefs.read_all(where)
    case('brief: a brief is a folder of its own with what it is for, the options (the first the writer\'s) and a copy of each piece of evidence',
         len(back) == 1 and back[0] == b and b['id'] == '2026-10-06-the-house-s-look' and [o['key'] for o in b['options']] == ['A', 'B'] and b['pick'] == 'A' and b['state'] == 'open'
         and [e['file'] for e in b['evidence']] == ['1-wide-shot.png', '2-clip.mp4'] and [e['kind'] for e in b['evidence']] == ['picture', 'film']
         and (where / b['id'] / '2-clip.mp4').read_bytes() == film.read_bytes(), b)
    try:
        from PIL import Image
        with Image.open(where / b['id'] / '1-wide-shot.png') as im:
            case('brief: a picture of evidence is kept no wider than a page shows it', im.size == (briefs.WIDE, round(30 * briefs.WIDE / 2400)), im.size)
    except ImportError:
        print('      (no Pillow on this machine: the evidence was copied as it is)')
    again = briefs.add(where, now=day, **good)
    case('brief: two briefs of one title on one day are two briefs', again['id'] == b['id'] + '-2' and len(briefs.read_all(where)) == 2, again['id'])
    long = ' '.join(['word'] * (briefs.FOR_WORDS + 1))
    wrong = [dict(good, what_for=long), dict(good, options=['Only one']), dict(good, options=['a', 'b', 'c', 'd', 'e']), dict(good, options=['a', ' '.join(['w'] * (briefs.OPTION_WORDS + 1))]),
             dict(good, why=''), dict(good, why=' '.join(['w'] * (briefs.WHY_WORDS + 1))), dict(good, evidence=[]), dict(good, evidence=[(str(tmp / 'art' / 'none.png'), 'Gone')]),
             dict(good, evidence=[(str(small), '')]), dict(good, evidence=[(str(small), 'x')] * (briefs.MOST_EVIDENCE + 1)), dict(good, title=''), dict(good, evidence=[(str(tmp / 'art' / 'small.png').replace('.png', '.txt'), 'A text')])]
    refused = []
    for w in wrong:
        try:
            briefs.add(where, now=day, **w)
        except ValueError as e:
            refused.append(str(e))
    case('brief: a brief that is not short, has one option or five, no reason, nothing to show and no word why, a file that is not there or not a picture, or evidence without a caption, is refused, and nothing is written',
         len(refused) == len(wrong) and len(briefs.read_all(where)) == 2 and 'words' in refused[0] and 'options' in refused[1], (len(refused), len(wrong)))
    none = briefs.add(where, now=day, **dict(good, title='A rule of the sim', evidence=[], no_evidence='it is a rule of the sim, nothing is drawn'))
    case('brief: a decision nothing can be shown of says so in place of evidence', none['evidence'] == [] and none['no_evidence'].startswith('it is a rule'), none)
    titles = ['The house look', 'A rule of the sim', 'The frog\'s far LOD size', 'the HOUSE\'s look!']
    case('brief: what waits on the owner with no brief is found by title, however it is spelled; a brief is found by the question it is about or by its own title',
         briefs.missing(briefs.read_all(where), titles) == ['The frog\'s far LOD size'], briefs.missing(briefs.read_all(where), titles))
    bad = []
    for args in ((b['id'], 'Z'), ('no-such', 'A'), (b['id'], 'other')):
        try:
            briefs.answer(where, *args)
        except ValueError as e:
            bad.append(str(e))
    done = briefs.answer(where, b['id'], 'B', 'the light one', by='lane/show/x', now=day)
    case('brief: the owner\'s answer closes a brief with the option he took and his words; an option it does not have, a brief there is not, and other words without the words are refused',
         len(bad) == 3 and done['state'] == 'answered' and done['answer'] == dict(option='B', said='the light one', by='lane/show/x', when='2026-10-06 10:00')
         and {x['id']: x for x in briefs.read_all(where)}[b['id']]['answer']['option'] == 'B', (bad, done.get('answer')))
    site = tmp / 'site'
    listed = briefs.site(where, site, now=day)
    js = (site / 'data' / 'briefs.js').read_text(encoding='utf-8')
    case('brief: the page is given the open briefs and the lately answered, with their evidence put in the site',
         sorted(x['id'] for x in listed) == sorted([b['id'], again['id'], none['id']]) and json.loads(js[len('window.BRIEFS = '):-2]) == listed
         and {x['id']: x for x in listed}[b['id']]['evidence'][1]['src'] == f'img/brief/{b["id"]}/2-clip.mp4' and (site / 'img' / 'brief' / b['id'] / '2-clip.mp4').read_bytes() == film.read_bytes(), [x['id'] for x in listed])
    later = briefs.site(where, site, now=day + datetime.timedelta(days=briefs.SHOWN_DAYS + 1))
    case('brief: a brief answered over a week ago leaves the page, and its evidence leaves the site',
         sorted(x['id'] for x in later) == sorted([again['id'], none['id']]) and not (site / 'img' / 'brief' / b['id']).exists() and (site / 'img' / 'brief' / again['id']).is_dir(), [x['id'] for x in later])
    # he answers on the page (a note about the brief); a session takes it up in one step, and only one session can
    third, box = briefs.add(where, now=day, **dict(good, title='A third call')), tmp / 'notes'
    his = notes.write(box, 'B: Light\nbut keep the rug', kind='page', about='brief:' + third['id'], now=day)
    notes.write(box, 'a late word', kind='page', about='brief:' + b['id'], now=day)             # about a brief already closed: waits on nobody
    wait = [(x['id'], n['id']) for x, n in briefs.waiting(briefs.read_all(where), notes.read_all(box))]
    took, was = briefs.take(where, box, third['id'], by='lane/show/x', now=day, note=his['id'], outcome='Nothing to build: the rug stays')
    twice = []
    for again_by in (lambda: briefs.take(where, box, third['id'], by='lane/show/y', now=day, note=his['id'], outcome='Again'), lambda: briefs.answer(where, third['id'], 'A', by='lane/show/y', now=day)):
        try:
            again_by()
        except ValueError as e:
            twice.append(str(e))
    after = {x['id']: x for x in briefs.read_all(where)}[third['id']]['answer']
    case('brief: an answer he left on the page is taken up in one step (the brief closes with his option and his words, his note is answered), a second take-up or answer is refused and changes nothing, and words alone are an answer of his own',
         wait == [(third['id'], his['id'])] and was['id'] == his['id'] and (took['answer']['option'], took['answer']['said'], took['answer']['by']) == ('B', 'but keep the rug', 'lane/show/x')
         and [n['state'] for n in notes.read_all(box) if n['about'] == 'brief:' + third['id']] == ['done'] and len(twice) == 2 and 'already closed' in twice[1] and after['by'] == 'lane/show/x'
         and not briefs.waiting(briefs.read_all(where), notes.read_all(box)) and briefs.said(third, 'neither, do it later') == ('other', 'neither, do it later') and briefs.said(third, 'D: no such option') [0] == 'other',
         (wait, took.get('answer'), twice))
    keep = os.environ.get('TW_BRIEFS')
    os.environ['TW_BRIEFS'] = str(where)
    try:
        case('brief: TW_BRIEFS names the folder the briefs are kept in', briefs.folder() == where)
    finally:
        os.environ.pop('TW_BRIEFS') if keep is None else os.environ.__setitem__('TW_BRIEFS', keep)

    # an option can say what happens then. A click on it is his yes to that work, and only to the line the page showed
    def unit_of(n):
        return dict(id=f'unit-{n}', lane=f'lane/sim/unit-{n}', goal='Men behind a building take less damage.', done_when=['python', 'Tools/otr.py', 'CoverTests'])

    def ask(title):
        return briefs.add(where, now=day, **dict(good, title=title))

    def says(q, key, line, u=None):
        return briefs.then(where, notes.read_all(box), q['id'], key, line, u)

    def then_of(q):
        return {o['key']: o.get('then') for o in {x['id']: x for x in briefs.read_all(where)}[q['id']]['options']}

    def click(q, key, sec, more='', **kw):      # what the page leaves on a click: the option's text, and the stamp of the Then line it showed
        o = [x for x in {x['id']: x for x in briefs.read_all(where)}[q['id']]['options'] if x['key'] == key][0]
        return notes.write(box, f'{key}: {o["text"]}' + (f'\n{more}' if more else ''), about='brief:' + q['id'], now=day + datetime.timedelta(seconds=sec),
                           **dict(dict(kind='page', then=(then_of(q)[key] or {}).get('stamp', '')), **kw))

    def took(q, **kw):                          # the answer a brief is closed with, or why it was not closed
        try:
            return briefs.take(where, box, q['id'], by='master', now=day, **kw)[0]['answer']
        except ValueError as e:
            return dict(refused=str(e))

    def got():
        return {a['id']: a for a in briefs.answers(briefs.read_all(where), notes.read_all(box))}

    def no(f):
        try:
            f()
        except ValueError as e:
            return str(e)
        return ''
    cover, other, u1, line = ask('Do houses give cover'), ask('Another question'), unit_of(1), 'Queues a sim lane: men behind a building take less damage'
    says(cover, 'A', line, u1)
    says(cover, 'B', 'Nothing to build')
    opts = then_of(cover)
    # `third` is closed and its notes are answered: nothing but its being closed refuses a Then line on it
    wrong = [lambda: says(third, 'A', 'Too late'), lambda: says(cover, 'Z', 'No such option'), lambda: says(cover, 'A', ' '.join(['w'] * (briefs.CAPTION_WORDS + 1))), lambda: says(cover, 'A', '', u1),
             lambda: says(cover, 'A', 'Queues it', dict(u1, lane='feature/x')), lambda: says(cover, 'A', 'Queues it', dict(u1, done_when='python Tools/otr.py CoverTests')),
             lambda: says(cover, 'A', 'Queues it', dict(u1, goal='')), lambda: says(cover, 'A', 'Queues it', dict(u1, id='has a space')), lambda: says(cover, 'A', 'Queues it', dict(u1, priority=1)),
             lambda: says(other, 'A', 'Queues it', u1)]
    refused = [no(f) for f in wrong]
    case('brief: an option can say what happens then: the line the page shows and the unit that is queued, or that nothing is built. A line that is empty or long, a unit the relay would refuse, '
         'a unit another option already queues, an option there is not and a closed brief are refused, and nothing is written',
         opts['A'] == dict(says=line, stamp=briefs.stamp(line, dict(u1, role='lane')), unit=dict(u1, role='lane')) and opts['B'] == dict(says='Nothing to build', stamp=briefs.stamp('Nothing to build'))
         and all(refused) and len(refused) == 10 and then_of(cover) == opts and not any(then_of(other).values()) and not any(then_of(third).values()), (opts, refused))
    case('brief: the stamp of a Then line is eight characters that change with the line and with the unit, so a click is a yes to one line only',
         len({briefs.stamp(line, u1), briefs.stamp(line + '!', u1), briefs.stamp(line, dict(u1, goal='Something else.')), briefs.stamp(line)}) == 4
         and briefs.stamp(' ' + line + '  ', u1) == briefs.stamp(line, u1) and len(opts['A']['stamp']) == 8, opts['A']['stamp'])
    plain, quiet, stale, bare, worded, both, forged, by_hand, remark = (ask(t) for t in ('No Then on this one', 'Nothing follows', 'The line changed', 'Clicked with no stamp', 'Words before the click',
                                                                                       'First A then B', 'Not his click', 'Written by hand', 'A click with a remark'))
    for q, key, n in ((stale, 'A', 2), (bare, 'A', 3), (worded, 'A', 4), (both, 'A', 5), (both, 'B', 6), (forged, 'A', 7), (by_hand, 'A', 8), (remark, 'A', 10)):
        says(q, key, f'Queues unit {n}', unit_of(n))
    says(quiet, 'A', 'Nothing to build: it stays as it is')
    c_plain, c1, c2, c_quiet = click(plain, 'A', 1), click(cover, 'A', 2), click(cover, 'A', 3), click(quiet, 'A', 4)
    click(stale, 'A', 5, then=briefs.stamp('An older line', unit_of(2)))
    click(bare, 'A', 6, then='')                                   # an old watcher drops the stamp
    notes.write(box, 'only if it costs no frames', kind='page', about='brief:' + worded['id'], now=day + datetime.timedelta(seconds=7))
    c_worded, _, c_both = click(worded, 'A', 8), click(both, 'A', 9), click(both, 'B', 10)
    click(forged, 'A', 11, who='an agent')
    click(by_hand, 'A', 12, kind='queue')
    click(remark, 'A', 13, more='but keep the rug')
    g = got()
    go = {q['title']: g.get(q['id'], {}).get('go') for q in (cover, quiet, plain, stale, bare, worded, both, forged, by_hand, remark, other)}
    case('brief: a click that carries the stamp its option has now is his yes: the answer says queue and gives the unit, or nothing when the option builds nothing; two clicks on the one option are one yes, '
         'and a brief he has not answered is not listed',
         (go[cover['title']], go[quiet['title']], go[other['title']]) == ('queue', 'nothing', None) and g[cover['id']]['unit'] == dict(u1, role='lane') and g[cover['id']]['note'] == c2['id']
         and [n['id'] for n in g[cover['id']]['notes']] == [c1['id'], c2['id']] and g[quiet['id']]['says'] == 'Nothing to build: it stays as it is' and g[quiet['id']]['unit'] is None, go)
    case('brief: a click on an option with no Then line names no unit: his answer is the decision, and the session writes the unit it leads to', go[plain['title']] == 'write' and 'no Then line' in g[plain['id']]['why'], g[plain['id']])
    case('brief: a click that carries the stamp of an older Then line, or none, is no yes to the line the option has now',
         (go[stale['title']], go[bare['title']]) == ('write', 'write') and 'showed' in g[stale['id']]['why'] and 'showed' in g[bare['id']]['why'], (g[stale['id']]['why'], g[bare['id']]['why']))
    case('brief: words of his own beside a click make it no yes, in a note of their own or under the click, and every note of his is given, not the last only',
         go[worded['title']] == 'write' and g[worded['id']]['option'] == 'A' and g[worded['id']]['said'] == 'only if it costs no frames' and len(g[worded['id']]['notes']) == 2
         and 'words of his own' in g[worded['id']]['why'] and go[remark['title']] == 'write' and g[remark['id']]['said'] == 'but keep the rug', (g[worded['id']], g[remark['id']]))
    case('brief: clicks on two options are no yes to either; the last is the option the answer names',
         go[both['title']] == 'write' and g[both['id']]['option'] == 'B' and g[both['id']]['note'] == c_both['id'] and 'same option' in g[both['id']]['why'], g[both['id']])
    case('brief: a note that is not the owner\'s click on a page is no yes, whatever stamp it carries',
         (go[forged['title']], go[by_hand['title']]) == ('write', 'write') and 'not a click of his' in g[forged['id']]['why'], (g[forged['id']]['why'], g[by_hand['id']]['why']))
    every = notes.read_all(box)
    case('brief: the unit a click queues is given only when the click is a yes to it, and only for the note that is his last word',
         briefs.unit(where, every, cover['id'], c2['id']) == dict(u1, role='lane') and all(no(lambda q=q, n=n: briefs.unit(where, every, q['id'], n)) for q, n in
                                                                                         ((cover, c1['id']), (cover, ''), (plain, c_plain['id']), (quiet, c_quiet['id']))),
         no(lambda: briefs.unit(where, every, cover['id'], c1['id'])))
    case('brief: a Then line cannot be put on a brief he has already answered: it would not be one he saw', 'already' in no(lambda: says(plain, 'A', 'Queues unit 9', unit_of(9))) and not any(then_of(plain).values()), then_of(plain))
    site2 = tmp / 'site2'
    told = {x['id']: x.get('waits') for x in briefs.site(where, site2, now=day, got=list(g.values()))}
    case('brief: the page is told what each answer of his leads to and the note that was read for, and nothing of a brief he has not answered',
         told[cover['id']] == dict(go='queue', note=c2['id'], unit='unit-1') and told[plain['id']] == dict(go='write', note=c_plain['id'], unit='') and told[other['id']] is None, told)
    t_bad = [took(cover, note=c1['id']).get('refused'), took(cover).get('refused'), took(plain, note=c_plain['id']).get('refused'),
             took(plain, note=c_plain['id'], queued='unit-x', outcome='and words').get('refused'), took(cover, note=c2['id'], option='B').get('refused')]
    still = got()
    t_cover, t_quiet, t_plain = took(cover, note=c2['id']), took(quiet, note=c_quiet['id']), took(plain, note=c_plain['id'], outcome='Nothing to build: it stays as built')
    t_worded, t_both = took(worded, note=c_worded['id'], queued='unit-4'), took(both, note=c_both['id'], queued='unit-5', option='A')
    his_two = [n for n in notes.read_all(box) if n['about'] == 'brief:' + cover['id']]
    case('brief: taking an answer up says what became of it: a yes to a Then line carries its unit or its "nothing to build", any other answer needs the unit that was queued or the words why none was. '
         'A note older than his last, no note, a unit and words together, and another option than his click are refused and close nothing',
         all(t_bad) and len(t_bad) == 5 and sorted(still) == sorted(g) and t_cover.get('queued') == 'unit-1' and 'outcome' not in t_cover and t_cover.get('option') == 'A'
         and t_quiet.get('outcome') == 'Nothing to build: it stays as it is' and 'queued' not in t_quiet and t_plain.get('outcome') == 'Nothing to build: it stays as built'
         and (t_both.get('option'), t_both.get('queued')) == ('A', 'unit-5') and [n['state'] for n in his_two] == ['done', 'done'] and 'Queued as unit-1.' in his_two[0]['answers'][-1]['text']
         and sorted(got()) == sorted(x['id'] for x in (stale, bare, forged, by_hand, remark)), (t_bad, t_cover, t_quiet, t_plain, t_both))
    case('brief: words he typed before a click are in the answer the brief is closed with',
         (t_worded.get('option'), t_worded.get('said'), t_worded.get('queued')) == ('A', 'only if it costs no frames', 'unit-4'), t_worded)

    # concepts first: a brief whose options are concepts or references to pick from (briefs.py concepts)
    cw, page_c = tmp / 'concept-briefs', tmp / 'art' / 'three puffs.html'
    page_c.write_text('<html><body>three puffs</body></html>', encoding='utf-8')
    (tmp / 'art' / 'notes.txt').write_text('words', encoding='utf-8')
    shot = lambda src, dst: png(dst, 80, 45)
    cb = briefs.concepts(cw, 'The flame jet', 'What the flamethrower\'s jet looks like from the standard view.', [(str(small), 'One long card, ragged edge'), (str(page_c), 'Three puffs in a row'),
                                                                                                                    (str(film), 'A reference: a film of one')], 'It reads at 120 m.', lane='lane/show/x', now=day, shooter=shot)

    def refused(*a, **k):
        try:
            briefs.concepts(*a, **k)
        except ValueError as e:
            return str(e)
    case('concepts: each concept is an option he picks, with its picture under the option\'s letter; a page drawn in HTML is photographed; the writer\'s own is first',
         cb['kind'] == 'concepts' and [o['text'] for o in cb['options']] == ['One long card, ragged edge', 'Three puffs in a row', 'A reference: a film of one'] and cb['pick'] == 'A'
         and [(e['option'], e['caption'], e['kind'], Path(e['file']).suffix) for e in cb['evidence']] == [('A', 'A: One long card, ragged edge', 'picture', '.png'), ('B', 'B: Three puffs in a row', 'picture', '.png'),
                                                                                                         ('C', 'C: A reference: a film of one', 'film', '.mp4')]
         and all((cw / cb['id'] / e['file']).is_file() for e in cb['evidence']) and briefs.read_all(cw) == [cb], cb)
    case('concepts: one concept is no choice, a concept that is no picture, film or page is refused, and so is a page no browser photographed',
         '1 concepts' in (refused(cw, 'T', 'For.', [(str(small), 'Only one')], 'Why.') or '') and 'not a picture or a film' in (refused(cw, 'T', 'For.', [(str(small), 'One'), (str(tmp / 'art' / 'notes.txt'), 'Words')], 'Why.') or '')
         and 'no picture' in (refused(cw, 'T', 'For.', [(str(small), 'One'), (str(page_c), 'A page')], 'Why.', shooter=lambda s, d: (_ for _ in ()).throw(ValueError('the browser made no picture of it'))) or '')
         and len(briefs.read_all(cw)) == 1, None)

    # every step an asset passes is put to him as a brief with the step's captures (briefs.py steps)
    bd, sw = tmp / 'board', tmp / 'step-briefs'
    (bd / 'items').mkdir(parents=True)
    (bd / 'results').mkdir()
    (bd / 'items' / 'house.json').write_text(json.dumps(dict(id='house', title='The village house, nine chunks.', lane='lane/show/house', stages=[
        dict(id='numbers', station='laptop', role='balance'), dict(id='look', station='desktop', role='vfx', bands=['intact', 'down'], after=['numbers']),
        dict(id='far', station='desktop', role='vfx', bands=['t1'], after=['look']), dict(id='gate', station='desktop', role='master', after=['far'])])), encoding='utf-8')
    (bd / 'items' / 'shed.json').write_text(json.dumps(dict(id='shed', title='A shed', lane='lane/show/shed', stages=[dict(id='look', role='vfx', bands=['intact'])])), encoding='utf-8')
    png(bd / 'evidence' / 'house' / 'look' / 'intact.png', 60, 40), png(bd / 'evidence' / 'house' / 'look' / 'down.png', 60, 40), png(bd / 'evidence' / 'house' / 'look' / 'side_sheet.png', 60, 40)
    png(bd / 'evidence' / 'shed' / 'look' / 'intact.png', 60, 40), png(bd / 'evidence' / 'house' / 'far' / 't1.png', 60, 40)
    (bd / 'evidence' / 'house' / 'numbers').mkdir(parents=True)
    (bd / 'evidence' / 'house' / 'numbers' / 'table.md').write_text('numbers', encoding='utf-8')

    def result(item, stage, n, verdict, ev, job=None, at='2026-10-04T20:00:00Z'):
        job = job or f'{item}--{stage}--abcd1234'
        (bd / 'results' / f'{job}--{n}.json').write_text(json.dumps(dict(item=item, stage=stage, attempt=n, verdict=verdict, evidence=ev, job=job, station='desktop',
                                                                         finished_at=at, note='checked by relay')), encoding='utf-8')
    result('house', 'numbers', 1, 'PASS', {'table': 'evidence/house/numbers/table.md'})
    result('house', 'look', 1, 'FAIL', {}, at='2026-10-04T19:00:00Z')
    result('house', 'look', 2, 'PASS', {'intact': 'evidence/house/look/intact.png', 'down': 'evidence/house/look/down.png'})
    result('house', 'far', 1, 'PASS', {'t1': 'evidence/house/far/t1.png'})
    result('house', 'gate', 1, 'PASS', {})
    result('shed', 'look', 1, 'PASS', {'intact': 'evidence/shed/look/intact.png'})
    dry = briefs.steps(sw, bd, states={('house', 'far'): 'STALE'}, landed={'shed'}, write=False)
    wrote, owed = briefs.steps(sw, bd, states={('house', 'far'): 'STALE'}, landed={'shed'}, now=day)
    sb = wrote[0] if wrote else {}
    case('steps: a step an asset has passed is a brief he can approve from its captures: the ones its result names first, then the other pictures of the step; '
         'a step whose inputs changed since, the gate, and an item that has landed get none; a dry run writes nothing and says the same',
         [b['id'] for b in wrote] == ['step-house-look-abcd1234'] and dry[0] == [('house', 'look', 'step-house-look-abcd1234', 3)] and len(briefs.read_all(sw)) == 1
         and [e['caption'] for e in sb['evidence']] == ['look: down', 'look: intact', 'look: side sheet'] and sb['lane'] == 'lane/show/house'
         and sb['step'] == dict(item='house', stage='look', job='house--look--abcd1234', attempt=2) and 'Its step look passed on the desktop, 2026-10-04' in sb['what_for']
         and 'lets the step far build on it' in sb['what_for'] and [o['text'] for o in sb['options']] == list(briefs.STEP_OPTIONS), (wrote, dry))
    case('steps: a step that passed with nothing a page can show gets no brief and is owed a capture, and so is a stage that names no band; the gate owes none',
         owed == dry[1] and [(i, st) for i, st, _ in owed] == [('house', 'numbers')] and 'nothing a page can show' in owed[0][2]
         and [w for _, _, w in briefs.steps(tmp / 'none', bd, landed={'shed'}, write=False)[1] if 'no band' in w] == []
         and [(i, st) for i, st, w in briefs.steps(tmp / 'none', tmp / 'board2', write=False)[1]] == [], owed)
    (bd / 'results' / 'house--numbers--abcd1234--1.json').unlink()
    case('steps: a stage that names no band is owed a capture before it has run: the pipeline would let it pass with nothing to show',
         [(i, st, 'no band' in w) for i, st, w in briefs.steps(tmp / 'none', bd, landed={'shed'}, write=False)[1]] == [('house', 'numbers', True)], None)
    again_w, _ = briefs.steps(sw, bd, landed={'shed'}, now=day)
    result('house', 'look', 1, 'PASS', {'intact': 'evidence/house/look/intact.png'}, job='house--look--ffff0000', at='2026-10-05T09:00:00Z')
    rebuilt, _ = briefs.steps(sw, bd, states={('house', 'far'): 'STALE'}, landed={'shed'}, now=day)
    case('steps: a step is put to him once, whichever machine reads the board, and again when it is rebuilt; a step whose state is not known is put to him',
         [b['id'] for b in again_w] == ['step-house-far-abcd1234'] and [b['id'] for b in rebuilt] == ['step-house-look-ffff0000'] and len(briefs.read_all(sw)) == 3, (again_w, rebuilt))
    sn = [notes.write(tmp / 'step-notes', 'A: ' + briefs.STEP_OPTIONS[0], kind='page', about='brief:' + sb['id'], then=sb['options'][0]['then']['stamp'], now=day),
          notes.write(tmp / 'step-notes', 'B: ' + briefs.STEP_OPTIONS[1] + '\nthe roof is too dark', kind='page', about='brief:step-house-far-abcd1234', now=day)]
    sgo = {a['id']: a['go'] for a in briefs.answers(briefs.read_all(sw), notes.read_all(tmp / 'step-notes'))}
    case('steps: his click on "approve" is a yes that builds nothing, so no session asks him again; sending a step back with his words is for the session to turn into feedback',
         sgo == {sb['id']: 'nothing', 'step-house-far-abcd1234': 'write'} and len(sn) == 2, sgo)
    out_s = tmp / 'step-site'
    briefs.site(sw, out_s, now=day, owed=owed)
    case('steps: the page is told which steps owe a capture; with none owed the briefs file says nothing of it',
         'window.OWED = [{"item": "house", "stage": "numbers"' in (out_s / 'data' / 'briefs.js').read_text(encoding='utf-8')
         and 'OWED' not in (briefs.site(sw, tmp / 'step-site2', now=day) and (tmp / 'step-site2' / 'data' / 'briefs.js').read_text(encoding='utf-8')), None)

    node = shutil.which('node')
    if not node:
        print('      (no node on this machine: the decisions page\'s own cases were not run)')
        return
    js = ('const D = require(process.argv[1]);'
          'const T = {says: "Queues a sim lane", stamp: "ab12cd34", unit: {id: "unit-1", lane: "lane/sim/house-cover", goal: "g"}}, W = {waits: {go: "queue", note: "n1", unit: "unit-1"}};'
          'const B = [{id: "b1", title: "The house\'s look", about: "The house look", state: "open", asked: "2026-10-06 10:00", options: [{key: "A", text: "Near-black", then: T}, {key: "B", text: "Light", then: {says: "Nothing to build", stamp: "ee00ee00"}}, {key: "C", text: "Later"}]},'
          ' {id: "b0", title: "Older", about: "", state: "open", asked: "2026-10-01 09:00"}, {id: "b2", title: "Closed", about: "Forward+", state: "answered", asked: "2026-10-02 09:00", answer: {when: "2026-10-05 10:00"}},'
          ' {id: "b3", title: "Closed later", state: "answered", asked: "2026-10-02 09:00", answer: {when: "2026-10-06 10:00"}}];'
          'const Q = [{title: "The house look"}, {title: "Forward+"}, {title: "the HOUSE\'S look"}, {title: "Repo hygiene"}];'
          'console.log(JSON.stringify([D.match(B, "the house look!").id, D.match(B, "The house\'s look").id, D.match(B, "Forward+"), D.match(B, ""), D.order(B).open.map(b => b.id).concat(D.order(B, x => x.id === "b1").open.map(b => b.id), D.order([B[1], Object.assign({}, B[0], W)]).open.map(b => b.id)), D.order(B).done.map(b => b.id),'
          ' D.bare(B, Q).map(q => q.title), D.word(B[0], "B", ""), D.word(B[0], "A", "but keep the rug"), D.word(B[0], "Z", "my own words"), D.slug("  The Frog\'s far LOD size "),'
          ' [D.then(B[0].options[0]), D.then(B[0].options[1]), D.then(B[0].options[2])], [D.stamp(B[0], "A"), D.stamp(B[0], "B"), D.stamp(B[0], "C"), D.stamp(B[1], "A")],'
          ' [D.became({by: "lane/show/x", queued: "unit-1"}), D.became({outcome: "Nothing to build"}), D.became({option: "A"})],'
          ' [D.waits(W, {id: "n1"}), D.waits(W, {id: "n2"}), D.waits({waits: {go: "write", note: "n1"}}, {id: "n1"}), D.waits({waits: {go: "nothing", note: "n1"}}, {id: "n1"}), D.waits({}, {id: "n1"})],'
          ' require(process.argv[2]).sends({text: "A: Near-black", kind: "page", about: "brief:b1", title: "T", lane: "", asset: "", page: "decide.html", then: "ab12cd34", id: "unsent-1", state: "unsent"}),'
          ' require(process.argv[2]).sends({text: "x"}).then]))')
    p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'decide.js'), str(HERE / 'static' / 'board.js')], capture_output=True)
    got = json.loads(p.stdout.decode() or 'null')
    case('decide: a question of the queue leads to its open brief, by the title it is about or its own; one whose brief is answered, and one with none, are listed without',
         got and got[:4] == ['b1', 'b1', None, None] and got[6] == ['Forward+', 'Repo hygiene'], (got and got[:7], p.stderr[-300:]))
    case('decide: the open briefs come newest first, one he has answered under the ones he has not, the closed after them, the last closed first', got and got[4:6] == [['b1', 'b0', 'b0', 'b1', 'b0', 'b1'], ['b3', 'b2']], got and got[4:6])
    case('decide: picking an option says the option in the note that is left, and his own words go as they are',
         got and got[7:11] == ['B: Light', 'A: Near-black\nbut keep the rug', 'my own words', 'the-frog-s-far-lod-size'], got and got[7:11])
    case('decide: an option that says what happens then shows it under its text with the lane of the unit it queues, and one that says nothing shows nothing',
         got and got[11] == ['Then: Queues a sim lane · sim lane house-cover', 'Then: Nothing to build', ''], got and got[11])
    case('decide: a click carries the stamp of the Then line its own option shows, and none when the option has none',
         got and got[12] == ['ab12cd34', 'ee00ee00', '', ''] and got[15] == dict(text='A: Near-black', kind='page', about='brief:b1', title='T', lane='', asset='', page='decide.html', then='ab12cd34') and got[16] == '',
         got and (got[12], got[15:]))
    case('decide: a closed brief says who took it up and what was queued, or why nothing was; one closed without either says no more than before',
         got and got[13] == ['Taken up by x: queued as unit-1', 'Taken up: Nothing to build', ''], got and got[13])
    case('decide: an answer of his is told what it waits for only for the note that was read for: the master queues the unit, writes down that nothing is built, or writes the unit his answer leads to',
         got and got[14] == ['the master queues unit-1 when you next talk to him', 'a session takes it up next', 'what it leads to is queued next, without asking you again',
                             'nothing to build; the master writes it down when you next talk to him', 'a session takes it up next'], got and got[14])
    try:
        import jinja2  # noqa: F401
    except ImportError:
        return
    keep_crew, out = ops.CREW, tmp / 'pagesite'
    ops.CREW = tmp / 'nothing'
    ops.page(out, dict(built='then', station='here', commit='abc', refs_as_of='then'))
    ops.CREW = keep_crew
    html = (out / 'decide.html').read_text(encoding='utf-8') if (out / 'decide.html').exists() else ''
    loads = re.findall(r'(?:src|href)="([^"#:]+\.(?:js|css))"', html)
    readings = {'data/ops.js', 'data/queue.js', 'data/beat.js', 'data/notes.js', 'data/graphs.js', 'data/briefs.js'}
    case('decide: the decisions page is written with every script and style it loads, has a place for the open briefs, the questions without one and the decided, and every page\'s top bar leads to it',
         {'decide.js', 'decide.css', 'board.js', 'data/briefs.js', 'crew.js'} <= set(loads) and not [u for u in loads if u not in readings and not (out / u).exists()]
         and all(f'id="{i}"' in html for i in ('decide', 'd-open', 'd-bare', 'd-done', 'd-count')) and 'href="decide.html"' in (out / 'house.html').read_text(encoding='utf-8'),
         [u for u in loads if u not in readings and not (out / u).exists()])


def ideas_cases():
    """The ideas (ideas.py) and their part of the control screen (ideas.js): an idea is a short card that shows
    something; one that is already decided, passed over, queued or on a lane is refused; his three buttons change it;
    and the watcher starts the agent when he asked or none is open, one run at a time, so many a day."""
    tmp = Path(tempfile.mkdtemp(prefix='tw-ideas-test-'))
    where, nwhere, bwhere = tmp / 'ideas', tmp / 'notes', tmp / 'briefs'
    day = datetime.datetime(2026, 10, 7, 12, 0, 0)
    pic, svg = tmp / 'front.png', tmp / 'sketch.svg'
    pic.write_bytes(b'\x89PNG\r\n\x1a\n' + b'0' * 64)
    svg.write_text('<svg xmlns="http://www.w3.org/2000/svg" width="64" height="36"><rect width="64" height="36" fill="#223"/></svg>', encoding='utf-8')
    shots = []

    def shooter(src, dst):
        shots.append(src.name)
        dst.parent.mkdir(parents=True, exist_ok=True)
        dst.write_bytes(pic.read_bytes())
        return dst
    good = dict(title='Stretcher frogs carry the wounded', pitch='Two frogs with a stretcher fetch a badly hurt man from the front and walk him back to the rear.', why_now='The frog faction is the main faction and has no medic.',
                kind='unit', size='M', score='R1 C3 MAJOR')
    P = [dict(path=str(svg), caption='Two frogs, one stretcher', kind='sketch'), dict(path=str(pic), caption='The front today', kind='capture')]

    def refused(**over):
        try:
            ideas.add(where, **{**good, 'pictures': P, 'entries': [], 'now': day, 'shooter': shooter, **over})
        except ValueError as e:
            return str(e)
        return ''
    case('ideas: an idea with nothing to look at is refused, and so is one that is not short, of no kind, or scored major without saying so',
         'shows 0 pictures' in refused(pictures=[]) and '35 at most' in refused(pitch='word ' * 36) and 'its kind is one of' in refused(kind='game') and 'is MAJOR' in refused(score='R1 C3')
         and 'score reads' in refused(score='low') and 'why now' in refused(why_now='') and not list(where.glob('*')), (refused(pictures=[]), refused(score='R1 C3')))
    case('ideas: a reference found online says where it was found, or the idea is refused',
         'where it was found' in refused(pictures=[dict(path=str(pic), caption='A stretcher party, 1917', kind='reference')])
         and not refused(pictures=[dict(path=str(pic), caption='A stretcher party, 1917', kind='reference', source='https://example.org/a.jpg')], title='A reference idea of its own'), '')
    i = ideas.add(where, pictures=P, entries=[], now=day, shooter=shooter, asked='the frog faction', run='r1', **good)
    home = where / i['id']
    case('ideas: an idea is a folder of its own with its pictures in it; a sketch drawn as a page is photographed; it carries the route of its kind and that route\'s stamp',
         (home / 'idea.json').is_file() and [p['file'] for p in i['pictures']] == ['1-sketch.png', '2-front.png'] and all((home / p['file']).is_file() for p in i['pictures']) and shots == ['sketch.svg']
         and [s['says'] for s in i['route']][:3] == ['Game design', 'Concept', 'You pick'] and i['route'][2]['own'] and i['stamp'] == ideas.stamp('unit') != ideas.stamp('look') and i['state'] == 'open' and i['asked'] == 'the frog faction',
         (i['pictures'], shots, i['route'][:3]))
    case('ideas: every kind has a route that ends with the owner\'s word to land, and a step of his own is a master stage',
         all(r[-1][0] == 'master' and r[-1][2] and all((role == 'master') == own for role, _, own in r) for r in ideas.ROUTES.values()) and set(ideas.ROUTES) == set(ideas.KIND_SAYS), '')

    # ---- what is not suggested
    b1 = briefs.add(bwhere, 'The enemy waits for the odds before it attacks', 'When the enemy AI starts an attack.', ['Keep it as built: it attacks only once it has the odds', 'Make it attack sooner and smaller, so the front is busier early'],
                    'It is built and played.', no_evidence='Nothing to show.', now=day)
    briefs.answer(bwhere, b1['id'], 'A', outcome='nothing changes: asked how much sooner, you said to ignore this and forget it', now=day)
    b2 = briefs.add(bwhere, 'Houses give cover: by how much?', 'How much a house cuts the damage.', ['Half', 'A quarter'], 'Half reads.', no_evidence='Nothing to show.', now=day)
    L = ideas.ledger_briefs(briefs.read_all(bwhere))
    by = {e['id']: e['state'] for e in L}
    case('ledger: a brief he answered is an entry and so is each option: the one he took, the ones he passed over; an open brief waits on him; "forget it" is a withdrawal',
         by[b1['id']] == 'withdrawn' and by[b1['id'] + '#A'] == 'he took it' and by[b1['id'] + '#B'] == 'not chosen' and by[b2['id']] == 'waits on him' and by[b2['id'] + '#A'] == 'waits on him', by)
    md = ('## Look\n| Date | Decision |\n|---|---|\n| 2026-09-28 | **No constant bombardment on the field** (the owner: "too much chaos"). A match gets no ambient shelling. |\n'
          '| 2026-10-07 | **R1 C2 DANGEROUS SYSTEMIC MAJOR.** **A side wins its own trenches back nearest first.** Not built. |\n| 2026-10-06 | **The enemy attacks sooner and smaller.** Withdrawn the same evening. |\n'
          '## Open: waiting on the owner\n- **Shelters that protect** from shells: which.\n')
    D = ideas.ledger_decisions(src_queue.parse(md))
    case('ledger: a row of decisions.md is read by its headline, not its score; a no, a withdrawal and an open question are told from the words',
         [(e['title'], e['state']) for e in D] == [('No constant bombardment on the field', 'left as it is'), ('A side wins its own trenches back nearest first.', 'decided'),
                                                    ('The enemy attacks sooner and smaller.', 'withdrawn'), ('Shelters that protect', 'waits on him')], [(e['title'], e['state']) for e in D])
    U = ideas.ledger_units([dict(id='kettle-turns', goal='A stopped Kettle turns on the spot.'), dict(id='record-match', goal='Record a match.'), dict(goal='no id')], done={'record-match'})
    N = ideas.ledger_lanes([('origin/lane/show/night-look-3', 'night: lamps'), ('origin/lane/rig/gait-attitude', 'gait')])
    case('ledger: a unit of the queue is queued or done, a lane that has not landed is in flight, each under the words of its name',
         [(e['title'], e['state']) for e in U] == [('kettle turns', 'queued'), ('record match', 'done')] and [(e['title'], e['state']) for e in N] == [('night look 3', 'in flight'), ('gait attitude', 'in flight')], (U, N))
    led = L + D + U + N
    hit = lambda t, p='': [e['id'] for _, e in ideas.match(t, p, led)]      # noqa: E731
    case('ledger: an idea he withdrew is found again, as it was worded and in other words; so is one that is queued; a fresh one is not, and one shared word is never enough',
         hit('The enemy attacks sooner and smaller') and hit('Enemy AI should attack sooner in smaller waves', 'The enemy attacks sooner and with smaller groups.') and 'kettle-turns' in hit('Kettle turns to face its target')
         and not hit('Carrier pigeons bring the match report', 'A pigeon lands with the casualty list.') and not hit('A trench periscope', 'Men look over the parapet of the trench.') and not hit('Enemy supply mules'),
         (hit('The enemy attacks sooner and smaller'), hit('A trench periscope', 'Men look over the parapet of the trench.'), hit('Enemy supply mules')))
    same = dict(good, title='The enemy attacks sooner and smaller', pitch='Make the enemy attack earlier with smaller groups so the front is busy early.', kind='mechanic')
    try:
        ideas.add(where, pictures=P, entries=led, now=day, shooter=shooter, **same)
        why = ''
    except ValueError as e:
        why = str(e)
    near = ideas.match(same['title'], same['pitch'], led)[0][1]['id']
    again = ideas.add(where, pictures=P, entries=led, now=day, shooter=shooter, differs={e['id']: 'Only on Hard, as a setting' for _, e in ideas.match(same['title'], same['pitch'], led)}, **same)
    case('ideas: an idea that is in the ledger is refused with the entry it is the same thing as; it is written only when it says of each such entry how it is another thing, and the card keeps what it was held against',
         'the same thing as' in why and '--differs' in why and again['checked']['near'] and all(n['differs'] for n in again['checked']['near']) and again['checked']['entries'] == len(led), (why[:200], again['checked']))
    ideas.answer(where, again['id'], 'rejected', 'No.', now=day)
    led2 = led + ideas.ledger_ideas(ideas.read_all(where), now=day)
    try:
        ideas.add(where, pictures=P, entries=led2, now=day, shooter=shooter, differs={e['id']: 'Different' for _, e in ideas.match(same['title'], same['pitch'], led2)}, **same)
        never = ''
    except ValueError as e:
        never = str(e)
    case('ideas: what he said never to is not put to him again, whatever the idea says of itself', 'he said never' in never, never[:200])
    parked = ideas.add(where, pictures=P, entries=[], now=day, shooter=shooter, **dict(good, title='Medals on veteran squads', pitch='A squad that survives three assaults wears a ribbon.'))
    ideas.answer(where, parked['id'], 'parked', 'Later.', now=day)
    soon, late = ideas.ledger_ideas(ideas.read_all(where), now=day + datetime.timedelta(days=3)), ideas.ledger_ideas(ideas.read_all(where), now=day + datetime.timedelta(days=ideas.PARKED_DAYS + 1))
    case('ledger: an idea he parked is held back for two weeks and may then come back; one he said never to stays for good',
         {e['id']: e['state'] for e in soon}.get(parked['id']) == 'not now' and parked['id'] not in [e['id'] for e in late] and {e['id']: e['state'] for e in late}.get(again['id']) == 'never', (soon, late))

    # ---- his answer on the card
    j = ideas.add(where, pictures=P, entries=[], now=day, shooter=shooter, **dict(good, title='A whistle before the assault', pitch='An officer blows a whistle and the trench goes over the top together.', kind='mechanic'))
    k = ideas.add(where, pictures=P, entries=[], now=day, shooter=shooter, **dict(good, title='Photo mode with a period frame', pitch='Pause, fly the camera, save a sepia plate.', kind='interface'))
    tock = [0]

    def note(about, text, **kw):            # each a second after the last: his notes are read in the order he left them
        tock[0] += 1
        return notes.write(nwhere, text, kind='page', about=about, now=day - datetime.timedelta(minutes=5) + datetime.timedelta(seconds=tock[0]), **kw)
    stale = note('idea:' + i['id'], 'Do it', then='0000dead')
    took1 = ideas.take(where, nwhere, now=day)
    case('take: "Do it" counts only with the stamp of the route the card showed; otherwise the idea stays open and his note says why',
         took1 == [(i['id'], 'the route was not the one he saw')] and ideas.find(where, i['id'])['state'] == 'open' and [n for n in notes.read_all(nwhere) if n['id'] == stale['id']][0]['state'] == 'done', took1)
    note('idea:' + i['id'], 'Do it', then=i['stamp'])
    note('idea:' + j['id'], 'Not now: after the frog faction')
    note('idea:' + k['id'], 'Never')
    words_ = note('idea:' + i['id'] + 'x', 'make it bigger')
    forged = notes.write(nwhere, 'Do it', kind='page', about='idea:' + j['id'], who='an agent', then=j['stamp'], now=day)
    took2 = ideas.take(where, nwhere, now=day)
    st = {x['id']: x for x in ideas.read_all(where)}
    case('take: his three buttons make the idea accepted, parked with his reason, or closed for good, and each note is answered with what happened; a note that is not his changes nothing',
         sorted(took2) == sorted([(i['id'], 'accepted'), (j['id'], 'parked'), (k['id'], 'rejected')]) and st[i['id']]['state'] == 'accepted' and st[j['id']]['answer']['said'] == 'after the frog faction' and st[k['id']]['state'] == 'rejected'
         and [n for n in notes.read_all(nwhere) if n['id'] == forged['id']][0]['state'] == 'open' and 'Its route: Game design > Concept' in [n for n in notes.read_all(nwhere) if n['about'] == 'idea:' + i['id']][-1]['answers'][0]['text'], (took2, st[j['id']].get('answer')))
    try:
        ideas.answer(where, i['id'], 'rejected', now=day)
        twice = ''
    except ValueError as e:
        twice = str(e)
    case('ideas: an idea is answered once', 'accepted already' in twice and [n for n in notes.read_all(nwhere) if n['id'] == words_['id']][0]['state'] == 'done', twice)

    # ---- what he asked for, and the run the watcher starts for it
    m = ideas.add(where, pictures=P, entries=[], now=day, shooter=shooter, **dict(good, title='Rain fills the shell holes', pitch='Craters fill with water after ten minutes of rain.', kind='level'))
    n_more, n_ask, n_fix = note('ideas:more', 'Three more ideas.'), note('ideas:request', 'something for the frog faction\'s artillery'), note('idea:' + m['id'], 'make the ponds deeper, men should swim')
    w = ideas.wanted(where, notes.read_all(nwhere))
    case('wanted: the button asks for three, the box for three on what he typed, and his own words about an idea for one better version of it',
         [(x['note'], x['n']) for x in w] == [(n_more['id'], 3), (n_ask['id'], 3), (n_fix['id'], 1)] and w[0]['asked'] == 'button' and 'artillery' in w[1]['asked'] and m['id'] in w[2]['says'], w)
    lim = dict(runs_per_day=3, auto_per_day=1, auto_rest_minutes=60, usd_per_run=2.0, minutes=30, model='')
    started = []

    class Proc:
        def __init__(self):
            self.code, self.killed, self.pid = None, False, 4242

        def poll(self):
            return self.code

        def kill(self):
            self.killed, self.code = True, -9

    def launch(cmd, cwd, env, out):
        started.append(dict(cmd=cmd, cwd=cwd, env=env, out=out, p=Proc()))
        return started[-1]['p']
    s1 = ideas.tick(where, nwhere, now=day, launch=launch, lim=lim)
    s2 = ideas.tick(where, nwhere, now=day + datetime.timedelta(seconds=20), launch=launch, lim=lim)
    case('tick: a request of his starts one run, for the oldest; while it runs nothing else is started and the page is told it runs',
         len(started) == 1 and s1['running'] and s1['running']['asked'] == 'button' and s2['running'] and s1['left'] == 2 and (where / 'running.json').is_file() and started[0]['env']['TW_IDEAS'] == str(where)
         and 'tw-ideas' in started[0]['cmd'][-1] and 'exactly 3' in started[0]['cmd'][-1], (s1, len(started)))
    run = started[0]['env']['TW_IDEAS_RUN']
    for t in ('A field telephone the enemy can cut', 'Observation balloon spots for the guns'):
        ideas.add(where, pictures=P, entries=[], now=day, shooter=shooter, run=run, **dict(good, title=t, pitch='A new thing on the field.', kind='mechanic'))
    started[0]['out'].write_text('warming up\n' + json.dumps(dict(total_cost_usd=1.234, result='Two ideas written.', is_error=False)), encoding='utf-8')
    started[0]['p'].code = 0
    s3 = ideas.tick(where, nwhere, now=day + datetime.timedelta(minutes=4), launch=launch, lim=lim)
    spend = [json.loads(r) for r in (where / 'spend.jsonl').read_text(encoding='utf-8').splitlines()]
    ans = [n for n in notes.read_all(nwhere) if n['id'] == n_more['id']][0]
    case('tick: a run that ended is a line of what it cost and made and which machine ran it, his note is answered with the ideas by name, and the next request starts at once',
         spend == [dict(when='2026-10-07 12:04', run=run, asked='button', usd=1.23, ideas=2, why='', host=socket.gethostname())] and ans['state'] == 'done' and '2 of the 3 asked for: A field telephone' in ans['answers'][0]['text']
         and len(started) == 2 and s3['running']['asked'].startswith('something for the frog') and s3['last']['ideas'] == 2 and s3['left'] == 1, (spend, ans['answers'], s3))
    s4 = ideas.tick(where, nwhere, now=day + datetime.timedelta(minutes=40), launch=launch, lim=lim)
    case('tick: a run past its minutes is stopped and written down as stopped, with no idea and its note answered; it still counts as a run of the day',
         started[1]['p'].killed and 'stopped after 30 minutes' in s4['last']['why'] and [n for n in notes.read_all(nwhere) if n['id'] == n_ask['id']][0]['answers'][0]['text'].startswith('No idea came of it')
         and len(started) == 3 and s4['running'] and s4['left'] == 0, (s4, len(started)))
    started[2]['out'].write_text(json.dumps(dict(total_cost_usd=0.5, result='x', is_error=True)), encoding='utf-8')
    started[2]['p'].code = 1
    n_late = note('ideas:more', 'Three more ideas.')
    s5 = ideas.tick(where, nwhere, now=day + datetime.timedelta(minutes=50), launch=launch, lim=lim)
    case('tick: the day\'s runs are a number: when they are used a request waits, nothing is started, and the page says why',
         len(started) == 3 and not s5['running'] and s5['left'] == 0 and 'runs are used' in s5['off'] and 'error' in s5['last']['why'] and [n for n in notes.read_all(nwhere) if n['id'] == n_late['id']][0]['state'] == 'open', (s5, len(started)))
    e_where, e_notes, e_started = tmp / 'empty', tmp / 'empty-notes', []

    def launch2(cmd, cwd, env, out):
        e_started.append(dict(cmd=cmd, out=out, p=Proc()))
        return e_started[-1]['p']
    a1 = ideas.tick(e_where, e_notes, now=day, launch=launch2, lim=lim)
    e_started[0]['out'].write_text(json.dumps(dict(total_cost_usd=0.2, result='none', is_error=False)), encoding='utf-8')
    e_started[0]['p'].code = 0
    a2 = ideas.tick(e_where, e_notes, now=day + datetime.timedelta(minutes=5), launch=launch2, lim=lim)
    case('tick: with no idea open one is made unasked, so the screen has one ready; that happens so often a day and no more, so an agent that finds nothing does not spend the day',
         a1['running']['asked'] == 'auto' and a1['running']['n'] == 1 and len(e_started) == 1 and not a2['running'] and a2['left'] == 2, (a1, a2, len(e_started)))
    r_where, r_started = tmp / 'rest', []

    def launch3(cmd, cwd, env, out):
        r_started.append(dict(out=out, p=Proc()))
        return r_started[-1]['p']
    lim3 = dict(lim, auto_per_day=3, runs_per_day=8)
    ideas.tick(r_where, tmp / 'rest-notes', now=day, launch=launch3, lim=lim3)
    r_started[0]['out'].write_text(json.dumps(dict(total_cost_usd=0.2, result='none', is_error=False)), encoding='utf-8')
    r_started[0]['p'].code = 0
    r2 = ideas.tick(r_where, tmp / 'rest-notes', now=day + datetime.timedelta(minutes=5), launch=launch3, lim=lim3)
    r3 = ideas.tick(r_where, tmp / 'rest-notes', now=day + datetime.timedelta(minutes=30), launch=launch3, lim=lim3)
    r4 = ideas.tick(r_where, tmp / 'rest-notes', now=day + datetime.timedelta(minutes=66), launch=launch3, lim=lim3)
    case('tick: an unasked run that found nothing is not tried again for an hour; then it is',
         not r2['running'] and not r3['running'] and len(r_started) == 2 and r4['running'] and r4['running']['asked'] == 'auto', (r2, r3, r4, len(r_started)))
    gone = ideas.tick(tmp / 'crashed', tmp / 'crashed-notes', now=day, launch=launch2, lim=lim)       # started by a watcher that is gone: nobody holds its process
    ideas.RUNS.clear()
    still = ideas.tick(tmp / 'crashed', tmp / 'crashed-notes', now=day + datetime.timedelta(minutes=10), launch=launch2, lim=lim, alive=lambda r: r['pid'] == 4242)
    dead = ideas.tick(tmp / 'crashed', tmp / 'crashed-notes', now=day + datetime.timedelta(minutes=11), launch=launch2, lim=dict(lim, auto_per_day=0), alive=lambda r: False)
    case('tick: a run whose watcher was stopped is asked of the system by its process: while that lives the run goes on, and once it is gone the run is written off with what it left',
         gone['running'] and still['running'] and not dead['running'] and dead['last'] and 'left no result' in dead['last']['why'] and not ideas.pid_alive(dict(pid=0)) and ideas.pid_alive(dict(pid=os.getpid())), (still, dead))
    cmd = ideas.command(dict(says='One idea.', n=1, asked='auto'), 'r9', lim, exe='claude')
    allow = cmd[cmd.index('--allowedTools') + 1:cmd.index('--disallowedTools')]
    case('tick: the session it starts may read, search and run the ideas tool and nothing else; it writes only in the folder it runs in, asks nobody, starts no agent, and is cut off at the money of the run',
         cmd[cmd.index('--permission-mode') + 1] == 'acceptEdits' and cmd[cmd.index('--max-budget-usd') + 1] == '2.0' and {'AskUserQuestion', 'Agent'} <= set(cmd[cmd.index('--disallowedTools') + 1:cmd.index('--max-budget-usd')])
         and sorted(allow) == sorted(['Read', 'Grep', 'Glob', 'WebSearch', 'WebFetch', f'Bash(python {ideas.TOOL} *)']) and '--add-dir' not in cmd and '--dangerously-skip-permissions' not in cmd
         and started[0]['cwd'] == str(where / 'runs' / run) and started[0]['out'] == where / 'runs' / f'{run}.json', (allow, started[0]['cwd']))
    keep_env = {k_: os.environ.get(k_) for k_ in ('TW_IDEAS', 'TW_IDEAS_RUN', 'TW_IDEAS_SCRATCH')}
    os.environ.update(TW_IDEAS=str(where), TW_IDEAS_RUN='r9', TW_IDEAS_SCRATCH=str(tmp / 'scratch'))
    import contextlib
    import io
    said_ = io.StringIO()
    try:
        with contextlib.redirect_stdout(said_):
            codes = [ideas.main(['answer', m['id'], 'accepted']), ideas.main(['take']), ideas.main(['tick']), ideas.main(['fetch', 'https://example.org/a.jpg', '--out', str(tmp / 'elsewhere' / 'a.jpg')]), ideas.main(['list'])]
    finally:
        for k_, v in keep_env.items():
            os.environ.pop(k_, None) if v is None else os.environ.__setitem__(k_, v)
    case('tick: inside a run the tool reads the context and adds ideas; it does not answer for the owner, start another run, or keep a reference outside its own folder',
         codes == [1, 1, 1, 1, 0] and ideas.find(where, m['id'])['state'] == 'open' and not (tmp / 'elsewhere').exists() and said_.getvalue().count('not for a run the board started') == 3 and 'own folder' in said_.getvalue(), (codes, said_.getvalue()[:300]))

    # ---- the site and the page
    out = tmp / 'site'
    data = ideas.site(where, out, notes.read_all(nwhere), dict(running=None, left=0, last=None, off='x'), now=day)
    js = (out / 'data' / 'ideas.js').read_text(encoding='utf-8')
    case('site: the page gets the ideas with their pictures beside it, what he asked for that waits, and whether a run is going',
         js.startswith('window.IDEAS = ') and {x['id'] for x in data['ideas']} >= {i['id'], m['id']} and all((out / p['src']).is_file() for x in data['ideas'] for p in x['pictures'])
         and [a['note'] for a in data['asked']] == [n_late['id']] and data['left'] == 0 and data['says'] == ['Do it', 'Not now', 'Never'], data['asked'])
    old = ideas.site(where, out, [], None, now=day + datetime.timedelta(days=ideas.SHOWN_DAYS + 1))
    case('site: an answered idea leaves the page after a week, with its pictures; an open one stays',
         i['id'] not in [x['id'] for x in old['ideas']] and not (out / 'img' / 'idea' / i['id']).exists() and m['id'] in [x['id'] for x in old['ideas']], [x['id'] for x in old['ideas']])

    # ---- where an accepted idea stands on its route: from what is committed on the board's origin, nothing else
    where2, board = tmp / 'ideas2', tmp / 'board'

    def git(*a):
        return subprocess.run(['git', '-C', str(board), '-c', 'user.name=t', '-c', 'user.email=t@t', '-c', 'core.autocrlf=false', *a], capture_output=True)

    def pushed(files):
        """Write files on the board and commit them; origin/main is that commit, as after a fetch."""
        for name, body in files.items():
            (board / name).parent.mkdir(parents=True, exist_ok=True)
            (board / name).write_text(json.dumps(body), encoding='utf-8')
        git('add', '-A'), git('commit', '-q', '-m', 'x'), git('update-ref', 'refs/remotes/origin/main', 'HEAD')

    def accepted(title, kind, rid):
        x = ideas.add(where2, pictures=P, entries=[], now=day, shooter=shooter, **{**good, 'title': title, 'kind': kind})
        x = ideas.answer(where2, x['id'], 'accepted', now=day)
        x['routed'] = rid
        ideas.save(where2, x)
        return x

    def result(rid, stage, verdict, n, when):
        return {f'results/{rid}--{stage}--abc123--{n}.json': dict(item=rid, stage=stage, attempt=n, verdict=verdict, finished_at=when)}
    board.mkdir()
    subprocess.run(['git', 'init', '-q', str(board)], capture_output=True)
    pushed({'readme.json': {}})
    look = accepted('Tracer glow on wet mud', 'look', 'idea-tracer-glow')                 # concept > you pick > build > critic > land
    tool = accepted('A weekly digest page', 'tool', 'unit:idea-weekly-digest')
    odd = accepted('Rain fills the shell holes', 'level', 'idea-rain')
    stages = [dict(id='concept', role='concept-artist'), dict(id='you-pick', role='master', after=['concept']), dict(id='build', role='destruction-vfx-simulator', after=['concept', 'you-pick']),
              dict(id='critic', role='hard-critic', after=['build']), dict(id='land', role='master', after=['critic'])]
    listed = ideas.read_all(where2)
    s0 = ideas.stands(listed, board)
    pushed({'items/idea-tracer-glow.json': dict(id='idea-tracer-glow', stages=stages), 'items/idea-rain.json': dict(id='idea-rain', stages=stages[:2]), 'relay/queue/idea-weekly-digest.json': dict(id='idea-weekly-digest')})
    s1 = ideas.stands(listed, board)
    (board / 'results').mkdir()
    (board / 'results' / 'idea-tracer-glow--concept--abc123--1.json').write_text(json.dumps(dict(item='idea-tracer-glow', stage='concept', attempt=1, verdict='PASS', finished_at='2026-10-07 13:00')), encoding='utf-8')
    s_tree = ideas.stands(listed, board)                   # written on this station and not committed: another session's work, or a leg's
    pushed(result('idea-tracer-glow', 'concept', 'PASS', 1, '2026-10-07 13:00'))
    s_pick = ideas.stands(listed, board)
    pushed({**result('idea-tracer-glow', 'you-pick', 'PASS', 1, '2026-10-07 13:30'),
            **result('idea-tracer-glow', 'build', 'PASS', 1, '2026-10-07 14:00'), **result('idea-tracer-glow', 'build', 'FAIL', 2, '2026-10-07 15:00'), **result('idea-tracer-glow-2', 'critic', 'PASS', 1, '2026-10-07 15:30')})
    s2 = ideas.stands(listed, board)
    pushed({'relay/done/idea-weekly-digest.json': {}})
    s3 = ideas.stands(listed, board)
    case('stands: an idea no session has put on the board yet says so; once its item is pushed, its first step is next and the rest wait; a tool idea is next while its unit is queued',
         s0[look['id']] == dict(on=False, steps=[]) and s0[tool['id']] == dict(on=False, steps=[]) and s1[look['id']] == dict(on=True, steps=['next', '', '', '', '']) and s1[tool['id']] == dict(on=True, steps=['next', '']), (s0, s1))
    case('stands: a result that is on this station and not on the board\'s origin moves nothing', s_tree == s1, s_tree)
    case('stands: a step is done when its newest result passed, a step of his own waits on him once all before it is done, and a later FAIL takes a pass back; another item\'s result is not this one\'s',
         s_pick[look['id']]['steps'] == ['done', 'you', '', '', ''] and s2[look['id']]['steps'] == ['done', 'done', 'next', '', ''] and s3[tool['id']] == dict(on=True, steps=['done', 'you']), (s_pick, s2, s3))
    case('stands: an item whose stages are not the idea\'s route gets no words, not wrong ones; with no board there is nothing to say',
         s2[odd['id']] == dict(on=True, steps=[]) and ideas.stands(listed, tmp / 'no-board') == {} and ideas.stands([m], board) == {}, s2[odd['id']])
    out2 = tmp / 'site-stands'
    d2 = ideas.site(where2, out2, [], None, now=day, board=board)
    case('site: the page is given where each routed idea stands, and the idea\'s own file is not written for it',
         {x['id']: x.get('stands') for x in d2['ideas']} == s3 and '"stands"' in (out2 / 'data' / 'ideas.js').read_text(encoding='utf-8') and 'stands' not in ideas.find(where2, look['id']), d2['ideas'][0].get('stands'))
    keep_tick, calls = ideas.tick, []
    ideas.tick = lambda *a, **k: calls.append(1) or dict(running=None, left=1, last=None, off='', took=[])
    keep_env = {k: os.environ.get(k) for k in ('TW_IDEAS',)}
    os.environ['TW_IDEAS'] = str(where)
    try:
        ops.the_ideas(tmp / 'site2', False)
        reads = len(calls)
        ops.the_ideas(tmp / 'site2', True)
    finally:
        ideas.tick = keep_tick
        for k_, v in keep_env.items():
            os.environ.pop(k_, None) if v is None else os.environ.__setitem__(k_, v)
    case('ops: a read that only reads starts no agent; the watcher does, and either puts the ideas in the site', reads == 0 and len(calls) == 1 and (tmp / 'site2' / 'data' / 'ideas.js').is_file(), (reads, len(calls)))
    c = ideas.taste([dict(text='A: Keep it', when='2026-10-06 10:00', **{'from': 'owner'}), dict(text='Do it', when='2026-10-06 11:00', **{'from': 'owner'}),
                     dict(text='lets use less lights and instead try to simulate them or bake them', when='2026-10-07 09:00', title='Forward+', **{'from': 'owner'}), dict(text='I rebased the lane for you.', when='2026-10-07 10:00', **{'from': 'an agent'})])
    case('context: his taste is what he wrote in his own words, not his clicks and not what agents wrote; with no goals page the agent is told where the goals are',
         [t['said'] for t in c] == ['lets use less lights and instead try to simulate them or bake them'] and ('docs/00-overview.md' in ideas.goals(tmp) or (tmp / ideas.GOALS).exists()), c)

    try:
        from jinja2 import Environment, FileSystemLoader, select_autoescape
    except ImportError:
        print('      (no jinja2 on this machine: the ideas\' place on the control screen was not checked)')
    else:
        env = Environment(loader=FileSystemLoader(str(HERE / 'templates')), autoescape=select_autoescape(['html']), trim_blocks=True, lstrip_blocks=True)
        env.globals.update(STATUS_LABEL=model.STATUS_LABEL, CATEGORY=render.CATEGORY, BLURB=render.BLURB, KIND=render.KIND, meta=dict(built='then', station='here', commit='abc', refs_as_of='then'))
        html = env.get_template('index.html').render(sections=[], levels=[], total=0, root='')
        loads = re.findall(r'(?:src|href)="([^"#:?]+\.(?:js|css))"', html)
        at = [html.find(f'id="{x}"') for x in ('deck', 'ideas', 'ideas-cards', 'queue')]
        case('page: the ideas are on the overview under the house and over what waits on him, hidden until their script draws them, and the script comes after the notes\' and the briefs\'',
             -1 not in at and at == sorted(at) and re.search(r'<section[^>]*id="ideas"[^>]*hidden', html) and {'ideas.js', 'ideas.css', 'data/ideas.js'} <= set(loads)
             and loads.index('crew.js') < loads.index('decide.js') < loads.index('ideas.js') and (HERE / 'static' / 'ideas.js').exists() and (HERE / 'static' / 'ideas.css').exists(), (at, loads))
        keep_crew = ops.CREW
        ops.CREW = tmp / 'nothing'
        ops.page(tmp / 'pagesite', dict(built='then', station='here', commit='abc', refs_as_of='then'))
        ops.CREW = keep_crew
        case('page: the watcher puts the ideas\' script and style in the site with the others', (tmp / 'pagesite' / 'ideas.js').is_file() and (tmp / 'pagesite' / 'ideas.css').is_file(), '')
    css = (HERE / 'static' / 'ideas.css').read_text(encoding='utf-8')
    greys = [c_ for c_ in re.findall(r'#[0-9a-fA-F]{3,6}\b', css) if len(set(c_[1:].lower())) == 1 and c_.lower() not in ('#fff', '#ffffff')]
    case('page: the ideas\' greys come from the board\'s tokens, none is written out', not greys and 'var(--k-fire)' in css, greys)

    node = shutil.which('node')
    if not node:
        print('      (no node on this machine: the ideas\' own page rules were not run)')
        return
    src = ('const I = require(process.argv[1]);'
           'const ideas = [{id: "a", state: "open", made: "2026-10-07 10:00"}, {id: "b", state: "open", made: "2026-10-07 12:00"}, {id: "c", state: "open", made: "2026-10-07 11:00"},'
           ' {id: "d", state: "accepted", answer: {when: "2026-10-07 09:00"}}, {id: "e", state: "rejected", answer: {when: "2026-10-07 09:30"}}, {id: "f", state: "accepted", answer: {when: "2026-10-07 13:00"}}];'
           'const o = I.order(ideas, i => i.id === "b"); const S = ["Do it", "Not now", "Never"];'
           'console.log(JSON.stringify([o.open.map(i => i.id), o.done.map(i => i.id), I.featured(o.open, "a").id, I.featured(o.open, "gone").id, I.featured([], "a"),'
           ' I.word("Never", "  too silly "), I.word("Do it", ""), I.asked(S, "Never: too silly"), I.asked(S, "Do it"), I.asked(S, "Do it properly this time"), I.asked(S, "Not now\\nlater"),'
           ' I.status({running: {since: "2026-10-07 14:02:10", asked: "button", n: 3}, asked: [{}], left: 4}, false), I.status({asked: [{}], left: 3}, false), I.status({asked: [{}], left: 0}, false),'
           ' I.status({asked: [{}], left: 2, off: "no claude on this station\'s path"}, false), I.status({asked: [], left: 3, last: {ideas: 0, why: "stopped after 30 minutes"}}, false), I.status({asked: [], left: 5}, false), I.status({}, true),'
           ' I.site("https://www.iwm.org.uk/collections/item/1"), I.site("a file"), I.meta({kind: "unit", size: "M", score: "R1 C3 MAJOR", asked: "the frog faction"}), I.meta({kind: "look", size: "S", score: "R0 C1", asked: "auto"})]))')
    p = subprocess.run([node, '-e', src, str(HERE / 'static' / 'ideas.js')], capture_output=True)
    got = json.loads(p.stdout.decode() or 'null')
    case('page: the open ideas he has not answered come first, newest first, then the ones his answer is on its way for; the accepted follow, and what he turned down is not listed',
         got and got[:5] == [['c', 'a', 'b'], ['f', 'd'], 'a', 'c', None], (got and got[:5], p.stderr[-300:]))
    case('page: a button leaves its own words and his reason after a colon, and a note is read back the same way; words that only begin like a button are his own',
         got and got[5:11] == ['Never: too silly', 'Do it', 'Never', 'Do it', '', 'Not now'], got and got[5:11])
    case('page: the line under the box says what the agent is doing about what he asked: at work since when, about to start, out of runs, could not start, or made nothing',
         got and got[11].startswith('The ideas agent is at work since 14:02, on three more.') and '1 more request waits behind it.' in got[11] and got[12] == 'Asked. The agent starts within a minute.' and 'starts tomorrow' in got[13]
         and 'could not start: no claude' in got[14] and got[15] == 'The last run made no idea (stopped after 30 minutes).' and got[16] == '' and got[17].startswith('Not connected'), got and got[11:18])
    case('page: a reference says the site it was found on, and the card\'s small line says the kind, the size and the score in words',
         got and got[18:] == ['iwm.org.uk', '', 'unit · a few days · risk 1 · change 3 · major', 'look · about a day · risk 0 · change 1'], got and got[18:])
    src = ('const I = require(process.argv[1]); const R = [{says: "Concept"}, {says: "You pick", own: true}, {says: "Numbers"}, {says: "Build"}];'
           'const at = (steps, on) => I.stands({route: R, routed: "idea-x", stands: {on: on !== false, steps: steps}});'
           'console.log(JSON.stringify([I.stands({route: R}), I.stands({route: R, routed: "idea-x"}), at([], false), at(["next", "", "", ""]), at(["done", "you", "next", ""]), at(["done", "done", "done", "done"]), at([])]))')
    p = subprocess.run([node, '-e', src, str(HERE / 'static' / 'ideas.js')], capture_output=True)
    got = json.loads(p.stdout.decode() or 'null')
    case('page: an accepted idea\'s row says where it stands: not on the board yet, what is next, what waits on him (said first), or that every step is done; with nothing known it says only where it is',
         got == ['queued for the pipeline', 'on the board as idea-x', 'not on the board yet: the master puts it there at its next turn', 'next: Concept', 'waits on you: You pick · next: Numbers', 'every step is done',
                 'on the board as idea-x'], (got, p.stderr[-300:]))
    page_js = (HERE / 'static' / 'ideas.js').read_text(encoding='utf-8')
    case('page: each step of the route is drawn with what the board says of it, and every such word has its look: done, next, waits on him',
         "'i-s-' + at[n]" in page_js and all(f'.i-route li.i-s-{w}' in css for w in ('done', 'next', 'you')) and 'pure.stands(i)' in page_js, '')


def critiques_cases():
    """The critique loops (critiques.py): a paper in the critic's shape is read for its score and its fixes; a loop is
    a folder with its papers and the pictures the critic judged; the relay's rounds are taken from the board's origin
    and nowhere else; and the ideas agent's context lists the loops, so an idea can answer a finding and say which."""
    import critiques
    tmp = Path(tempfile.mkdtemp(prefix='tw-critiques-test-'))
    where = tmp / 'critiques'
    paper = ('VERDICT: shots ROUND 1: 62/100 - the far band is empty\nCAPTURES: valid\n'
             'FINDINGS: | MAJOR | no smoke at far | MEASURED | far.json | smoke at 240 m |\nTOP-3 MANDATED FIXES:\n'
             '1. Barrage smoke, far band; now: no column; want: a column readable at 240 m; proof: far.jpg again\n'
             '2. Crater rim; now: rim luma equals mud; want: rim 0.05 lighter; proof: Diff of the pair\n'
             '3. Shot wanted: T3 at tick 140\nOVERDONE: nothing\n4. not a fix, it stands under another heading\n')
    got = critiques.read_paper(paper)
    case('critiques: a paper in the critic\'s shape gives its score, its sentence and its three fixes; a numbered line under the next heading is no fix',
         got['score'] == 62 and got['verdict'] == 'the far band is empty' and len(got['fixes']) == 3 and got['fixes'][0].startswith('Barrage smoke') and got['fixes'][2] == 'Shot wanted: T3 at tick 140', got)
    free = critiques.read_paper('It looks fine to me.\n\nVERDICT: thing: 90/100 - fine\n')
    high = critiques.read_paper('VERDICT: thing ROUND 1: 250/100 - too good\n')
    case('critiques: a paper in another shape is kept with no score and no fixes: a VERDICT that is not the first line, or a number over 100, is not a score',
         free == dict(score=None, verdict='', fixes=[]) and high['score'] is None, (free, high))

    pic = tmp / 'far.jpg'
    pic.write_bytes(b'\xff\xd8' + b'0' * 64)

    def refused(*a, **k):
        try:
            critiques.save(where, *a, **k)
        except ValueError as e:
            return str(e)
        return ''
    case('critiques: a loop is kept with a title, what was judged and a paper, or not at all',
         'title' in refused([(paper, 'round 1')], '', 'the barrage') and 'title' in refused([(paper, 'round 1')], 'Barrage', '') and 'at least one paper' in refused([], 'Barrage', 'the barrage')
         and not list(where.glob('*')), list(where.glob('*')) if where.is_dir() else '')
    c = critiques.save(where, [(paper, 'round 1')], 'Barrage at night: the evidence stage', 'the stills of a night barrage', by='a trial', role='destruction-vfx-simulator',
                       pictures=[(pic, 'The far band')], note='the far band was shot too late', day='2026-10-06')
    home = where / c['id']
    case('critiques: a loop is a folder of its own named by its day and title, with the paper as it was written and the picture the critic judged',
         c['id'] == '2026-10-06-' + briefs.slug('Barrage at night: the evidence stage') and (home / 'critique.json').is_file() and (home / 'paper-1.md').read_text(encoding='utf-8') == paper
         and (home / 'far.jpg').read_bytes() == pic.read_bytes() and c['papers'][0]['score'] == 62 and c['pictures'] == [dict(file='far.jpg', caption='The far band')], c)
    round2 = paper.replace('62/100', '88/100').replace('ROUND 1', 'ROUND 2')
    c = critiques.save(where, [(paper, 'round 1'), (round2, 'round 2')], 'Barrage at night: the evidence stage', 'the stills of a night barrage', day='2026-10-06', pictures=[(pic, 'The far band')])
    case('critiques: kept again under the same day and title, a loop gains only the paper and the picture it does not hold yet',
         [(p['label'], p['score']) for p in c['papers']] == [('round 1', 62), ('round 2', 88)] and len(list(home.glob('paper-*.md'))) == 2 and len(c['pictures']) == 1, c['papers'])

    # ---- the relay's rounds, from the board's origin
    def g(repo, *a):
        return subprocess.run(['git', '-C', str(repo), '-c', 'user.name=t', '-c', 'user.email=t@t', '-c', 'core.autocrlf=false', *a], capture_output=True, text=True)
    far, board = tmp / 'board-origin', tmp / 'board'
    (far / 'evidence' / 'thing' / 'shots' / 'older').mkdir(parents=True)
    (far / 'items').mkdir()
    subprocess.run(['git', 'init', '-q', '-b', 'main', str(far)], capture_output=True)
    (far / 'items' / 'thing.json').write_text(json.dumps(dict(id='thing', title='A thing', lane='lane/show/pipe-thing', stages=[dict(id='shots', role='env-simulator', bands=['near'])])), encoding='utf-8')
    (far / 'evidence' / 'thing' / 'shots' / 'critic-r1.md').write_text(paper, encoding='utf-8', newline='\n')
    (far / 'evidence' / 'thing' / 'shots' / 'near.jpg').write_bytes(b'\xff\xd8near')
    (far / 'evidence' / 'thing' / 'shots' / 'notes.md').write_text('not a critic round', encoding='utf-8')
    (far / 'evidence' / 'thing' / 'shots' / 'older' / 'deep.jpg').write_bytes(b'\xff\xd8deep')
    g(far, 'add', '-A'), g(far, 'commit', '-q', '-m', 'round 1')
    subprocess.run(['git', 'clone', '-q', str(far), str(board)], capture_output=True)
    (board / 'evidence' / 'thing' / 'shots' / 'critic-r7.md').write_text(round2, encoding='utf-8')        # in the checkout only: nobody committed it
    took = critiques.collect(where, board)
    b = critiques.find(where, 'board-thing-shots') if took else {}
    case('collect: a critic round on the board\'s origin becomes a loop named by its item and stage, with the item\'s title, the stage\'s role and its evidence pictures; a file in this station\'s checkout is not read',
         took == ['board-thing-shots'] and b['title'] == 'A thing: the stage shots' and b['role'] == 'env-simulator' and [p['label'] for p in b['papers']] == ['round 1']
         and [p['file'] for p in b['pictures']] == ['near.jpg'] and (where / 'board-thing-shots' / 'near.jpg').read_bytes() == b'\xff\xd8near', (took, b))
    case('collect: asked again with nothing new on the board, it takes nothing', critiques.collect(where, board) == [], '')
    (far / 'evidence' / 'thing' / 'shots' / 'critic-r2.md').write_text(round2, encoding='utf-8', newline='\n')
    g(far, 'add', '-A'), g(far, 'commit', '-q', '-m', 'round 2')
    before = critiques.collect(where, board)
    g(board, 'fetch', '-q', 'origin')
    took = critiques.collect(where, board)
    b = critiques.find(where, 'board-thing-shots')
    case('collect: a second round is added once it is fetched, and not before', before == [] and took == ['board-thing-shots'] and [(p['label'], p['score']) for p in b['papers']] == [('round 1', 62), ('round 2', 88)], (before, took, b['papers']))
    case('collect: with no board on this station it takes nothing and says nothing went wrong', critiques.collect(where, tmp / 'no-board') == [] and critiques.collect(where, None) == [], '')

    # ---- what the ideas agent is shown
    long_fix = paper.replace('proof: far.jpg again', 'proof: ' + 'word ' * 90).replace(
        'the far band is empty', 'standing 9\u21920, fidelity T3\u2265T2 \u2014 and a sign \u2603 of no known kind')
    critiques.save(where, [(long_fix, 'run 1')], 'A newer loop with a long fix', 'something else', day='2099-01-01')
    kept = critiques.read_all(where)
    rows = critiques.for_context(kept, where, most=2)
    text = '\n'.join(ideas.context_lines(dict(goals='G', routes={}, ledger=[], missing=[], taste=[], critiques=rows, critiques_kept=len(kept))))
    case('context: the ideas agent is shown the newest loops first, each with every paper\'s score, the fixes of its newest paper cut to a line, its pictures and the folder they are in; the rest are counted',
         [r['id'] for r in rows] == [kept[0]['id'], kept[1]['id']] and kept[0]['when'] == '2099-01-01' and '3 loops kept, the newest 2 here' in text and 'scores of 100: 62, 88' in text
         and max(len(f) for f in rows[0]['fixes']) == critiques.FIX_CHARS and str(where / kept[1]['id']) in text and '- Barrage smoke, far band' in text, text[:900])
    case('context: a critic\'s arrows and dashes reach the agent as ASCII, so the list prints through any pipe; the paper keeps them',
         rows[0]['verdict'] == 'standing 9->0, fidelity T3>=T2 - and a sign ? of no known kind' and text[text.index('# What the critics found'):].isascii()
         and '\u2192' in (where / kept[0]['id'] / 'paper-1.md').read_text(encoding='utf-8'), rows[0]['verdict'])
    plain = '\n'.join(ideas.context_lines(dict(goals='G', routes={}, ledger=[], missing=[], taste=[])))
    case('context: with no loop kept the agent is told nothing about critics', 'critics' not in plain, plain)
    iwhere, svg = tmp / 'ideas', tmp / 'sketch.svg'
    svg.write_bytes(b'\x89PNG\r\n\x1a\n' + b'0' * 64)
    i = ideas.add(iwhere, 'Smoke that reads at the far band', 'A barrage column keeps one tall dark card at distance so it reads from 240 m.', 'The critic found the far band empty.', 'look',
                  [dict(path=str(pic), caption='The far band today', kind='capture')], size='S', score='R1 C1', entries=[], critique='board-thing-shots')
    j = ideas.add(iwhere, 'A second idea of its own', 'Something that no critic asked for and that stands by itself.', 'It serves the look of the front.', 'look',
                  [dict(path=str(pic), caption='The far band today', kind='capture')], size='S', score='R1 C1', entries=[])
    case('ideas: an idea made from a finding names its critique loop, and an idea of its own names none', i.get('critique') == 'board-thing-shots' and 'critique' not in j, (i.get('critique'), j.get('critique')))
    keep = os.environ.get('TW_CRITIQUES')
    os.environ['TW_CRITIQUES'] = str(where)
    try:
        import contextlib
        import io
        said_ = io.StringIO()
        with contextlib.redirect_stdout(said_):
            listed = critiques.main([])
            unknown = critiques.main(['show', 'no-such-loop'])
            shown = critiques.main(['show', 'board-thing-shots'])
    finally:
        os.environ.pop('TW_CRITIQUES', None) if keep is None else os.environ.__setitem__('TW_CRITIQUES', keep)
    case('critiques: the command lists the kept loops with their scores, shows one with its fixes, and says so when asked for one that is not kept',
         (listed, unknown, shown) == (0, 1, 0) and '3 loops kept' in said_.getvalue() and 'no critique no-such-loop' in said_.getvalue() and 'round 2): 88/100' in said_.getvalue(), said_.getvalue()[:600])


if __name__ == '__main__':
    os.environ['TW_NOTES'] = tempfile.mkdtemp(prefix='tw-notes-test-')      # no case writes into the owner's own notes
    real_tree()
    fixtures()
    queue_fixtures()
    house()
    board()
    control()
    decisions()
    ideas_cases()
    critiques_cases()
    print(f'{sum(results)} of {len(results)} cases behaved')
    sys.exit(0 if all(results) else 1)
