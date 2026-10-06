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
    case('real tree: 21 vehicle archetypes and the three machines that are not units', len(by['vehicle']) == 24, by['vehicle'])
    case('real tree: 14 chunked buildings and the five Siege structures', len(by['building']) == 19, by['building'])
    want = {'Sniper': 'FINAL', 'Rifle': 'FINAL', 'Maw': 'FINAL', 'Pincer': 'FINAL', 'Officer': 'NEEDS_VISUAL', 'Breaker': 'NEEDS_VISUAL',
            'MarkIV': 'NEEDS_VISUAL', 'Brute': 'IN_PROGRESS', 'Frog': 'IN_PROGRESS', 'Skimmer': 'READY_UNUSED', 'Salvo': 'READY_UNUSED',
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
        case('queue: a checkout whose green gate tested its tip waits for the word, with how far behind it is',
             [(l['lane'], l['ahead'], l['behind']) for l in q['land']] == [('lane/show/a', 1, 1)], q['land'])
        case('queue: an approved lane is listed until it has landed, with the owner\'s words and what holds it up',
             [(a['lane'], a['words'], a['why']) for a in q['approved']] == [('lane/show/a', 'yes, land it', ['1 behind'])], q['approved'])
        case('queue: a ready stage of the board', [(r['item'], r['stage'], r['days']) for r in q['ready']] == [('item', 'gate', 1)], q['ready'])
        kinds = sorted(b['kind'] for b in q['broken'])
        case('queue: broken is a red checks run, a run before a commit over its budget, and each lane with stranded decisions',
             kinds == ['ci', 'gate', 'stranded', 'stranded'] and q['ci'] == 'failure', q['broken'])
        case('queue: its count is the number of entries', q['count'] == 2 + 1 + 1 + 1 + 4 and q['count'] == sum(len(q[g]) for g in src_queue.GROUPS), q['count'])
        green.write_text('0' * 40 + ' 2026-09-08T10:00:00\n')
        took.write_text('250 2026-09-08T09:00:00\n')
        q2 = src_queue.collect(repo, floor, board=board, cache=dict(ci=dict(at=when, run=dict(red, conclusion='success'))), now=when)
        case('queue: a tip no green gate tested is not listed to land, and the approval says why it has not',
             not q2['land'] and q2['approved'][0]['why'] == ['1 behind', 'no green gate on its tip'], (q2['land'], q2['approved']))
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

        # the page: its number is the rows it lists, and it knows how old it is
        node = shutil.which('node')
        if node:
            js = ('const Q = require(process.argv[1]); const q = JSON.parse(process.argv[2]); const at = new Date("2026-09-09T12:00:00").getTime();'
                  'console.log(JSON.stringify([Q.count(q), Q.groups(q).map(g => g.rows.length), Q.fresh("2026-09-09T10:59:00", at).stale,'
                  ' Q.fresh("2026-09-09T11:01:00", at).stale, Q.fresh("2026-09-09T11:01:00", at).text]))')
            p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'queue.js'), json.dumps(q)], capture_output=True)
            got = json.loads(p.stdout.decode() or 'null')
            case('page: "Needs you" is the number of rows the queue lists', got and got[0] == q['count'] == sum(got[1]), (got, p.stderr))
            case('page: a floor nobody has read for over an hour is stale, one read 59 minutes ago is not',
                 got and got[2] is True and got[3] is False and got[4] == 'read 59 min ago', got)
        else:
            print('      (no node on this machine: the page\'s own two cases were not run)')

        # ops.py writes the queue beside the page, and the beat on every read, changed or not
        out = repo / 'site'
        cache_file = repo / 'cache.json'
        cache_file.write_text(json.dumps(dict(ci=dict(at=time.time(), run=red))))
        ops.queue(floor, out, cache_file, repo=repo, board=board)
        first = (out / 'data' / 'queue.js').stat().st_mtime_ns, (out / 'data' / 'beat.js').read_text()
        ops.queue(dict(floor, now='2026-09-09 12:00:20'), out, cache_file, repo=repo, board=board)
        case('ops: an unchanged queue is not written again, and the beat is, so the page can tell stale from unchanged',
             (out / 'data' / 'queue.js').stat().st_mtime_ns == first[0] and first[1] == 'window.BEAT = "2026-09-09T12:00:00";\n'
             and (out / 'data' / 'beat.js').read_text() == 'window.BEAT = "2026-09-09T12:00:20";\n', first)


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
          ' G.ticks(1510, 4), G.ticks(3, 4), G.ticks(0, 4), G.fmt(4028), G.fmt(12900), G.hourLabel("2026-10-05 13"), G.dayLabel("2026-10-05")]))')
    p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'board.js'), str(HERE / 'static' / 'charts.js')], capture_output=True)
    got = json.loads(p.stdout.decode() or 'null')
    case('page: a thing shows the notes about it (its model, its branch, or the very thing), the open ones first, and counts the open ones',
         got and got[:6] == [['1'], ['3', '2'], 1, ['4'], 0, 3], (got, p.stderr[-300:]))
    case('page: a note just sent shows until a reading lists it, once; one not sent shows too', got and got[6] == ['1', '2', '3', '4', '9', 'u'] and got[7] == 'room-lane-show-frog-house', got and got[6:8])
    case('page: a graph\'s axis has clean steps that reach its largest number, and its numbers are written short',
         got and got[8] == [0, 500, 1000, 1500, 2000] and got[9] == [0, 1, 2, 3] and got[10] == [0, 1] and got[11:] == ['4,028', '12.9K', '13:00', 'Mon 5'], got and got[8:])


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
        need = {'house', 'h-canvas', 'h-tags', 'h-card', 'h-rooms', 'h-list', 'c-h-at', 'c-h-needs', 'c-age', 'profile', 'deck', 'graphs', 'g-rooms', 'g-lanes', 'g-needs', 'g-commits', 'g-models', 'stamp', 'p-ready', 'p-at', 't-calls', 't-calls-s', 't-notes-b', 'queue', 'office', 'assets'}
        case('control: the overview is the control screen: the tiles, the house with the profile beside it and the five graphs are on it, and no id is there twice',
             need <= set(ids) and len(ids) == len(set(ids)), (sorted(need - set(ids)), sorted(i for i in set(ids) if ids.count(i) > 1)))
        order = [loads.index(u) for u in ('crew.js', 'office.js', 'house.js', 'housedraw.js', 'charts.js', 'control.js')] if {'crew.js', 'office.js', 'house.js', 'housedraw.js', 'charts.js', 'control.js'} <= set(loads) else []
        case('control: it loads the house\'s, the graphs\' and its own files, the frog and the graphs\' data, each after what it needs, and every one is a file of the board',
             order and order == sorted(order) and {'house.css', 'control.css', 'data/frog.js', 'data/graphs.js', 'data/ops.js'} <= set(loads)
             and not [u for u in loads if not u.startswith('data/') and not (HERE / 'static' / u).exists()], loads)
        case('control: the house and the graphs keep a page of their own, one click from the screen, and the top bar no longer lists them',
             'href="house.html"' in html and 'href="graphs.html"' in html and html.count('href="house.html"') == 1 and html.count('href="graphs.html"') == 1, html.count('href="house.html"'))
        at = {k: html.find(f'id="{k}"') for k in ('top', 'deck', 'queue', 'graphs', 'office', 'assets')}
        case('control: down the page: the head and the tiles, the house, what waits on the owner, the graphs, the branch rooms, the models',
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
         got == [[1, 1, 1], [2, 2, 1, 20000], ['data/beat.js', 'data/queue.js', 'data/graphs.js', 'data/briefs.js', 'data/ops.js']], (got, p.stderr[-300:]))


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
    took, was = briefs.take(where, box, third['id'], by='lane/show/x', now=day)
    twice = []
    for again_by in (lambda: briefs.take(where, box, third['id'], by='lane/show/y', now=day), lambda: briefs.answer(where, third['id'], 'A', by='lane/show/y', now=day)):
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

    node = shutil.which('node')
    if not node:
        print('      (no node on this machine: the decisions page\'s own cases were not run)')
        return
    js = ('const D = require(process.argv[1]);'
          'const B = [{id: "b1", title: "The house\'s look", about: "The house look", state: "open", asked: "2026-10-06 10:00", options: [{key: "A", text: "Near-black"}, {key: "B", text: "Light"}]},'
          ' {id: "b0", title: "Older", about: "", state: "open", asked: "2026-10-01 09:00"}, {id: "b2", title: "Closed", about: "Forward+", state: "answered", asked: "2026-10-02 09:00", answer: {when: "2026-10-05 10:00"}},'
          ' {id: "b3", title: "Closed later", state: "answered", asked: "2026-10-02 09:00", answer: {when: "2026-10-06 10:00"}}];'
          'const Q = [{title: "The house look"}, {title: "Forward+"}, {title: "the HOUSE\'S look"}, {title: "Repo hygiene"}];'
          'console.log(JSON.stringify([D.match(B, "the house look!").id, D.match(B, "The house\'s look").id, D.match(B, "Forward+"), D.match(B, ""), D.order(B).open.map(b => b.id), D.order(B).done.map(b => b.id),'
          ' D.bare(B, Q).map(q => q.title), D.word(B[0], "B", ""), D.word(B[0], "A", "but keep the rug"), D.word(B[0], "Z", "my own words"), D.slug("  The Frog\'s far LOD size ")]))')
    p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'decide.js')], capture_output=True)
    got = json.loads(p.stdout.decode() or 'null')
    case('decide: a question of the queue leads to its open brief, by the title it is about or its own; one whose brief is answered, and one with none, are listed without',
         got and got[:4] == ['b1', 'b1', None, None] and got[6] == ['Forward+', 'Repo hygiene'], (got and got[:7], p.stderr[-300:]))
    case('decide: the open briefs come oldest first, the answered after them, the last answered first', got and got[4:6] == [['b0', 'b1'], ['b3', 'b2']], got and got[4:6])
    case('decide: picking an option says the option in the note that is left, and his own words go as they are',
         got and got[7:] == ['B: Light', 'A: Near-black\nbut keep the rug', 'my own words', 'the-frog-s-far-lod-size'], got and got[7:])
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


if __name__ == '__main__':
    os.environ['TW_NOTES'] = tempfile.mkdtemp(prefix='tw-notes-test-')      # no case writes into the owner's own notes
    real_tree()
    fixtures()
    queue_fixtures()
    house()
    board()
    control()
    decisions()
    print(f'{sum(results)} of {len(results)} cases behaved')
    sys.exit(0 if all(results) else 1)
