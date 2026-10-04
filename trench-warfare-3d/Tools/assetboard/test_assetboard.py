#!/usr/bin/env python3
"""Tests of the asset board's rules. Run from trench-warfare-3d/: python Tools/assetboard/test_assetboard.py

Two kinds. Against the REAL tree: the counts and a handful of assets whose status is known, so a rule change that
moves them is seen. Against FIXTURES: a code table that changed shape must stop the build, the notes file must
refuse an unknown id, lane families must collapse, and a status must follow its ladder.
"""
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import build      # noqa: E402
import films      # noqa: E402
import looks      # noqa: E402
import render     # noqa: E402
import src_ops    # noqa: E402
import model      # noqa: E402
import src_code   # noqa: E402
import src_git    # noqa: E402
import src_queue  # noqa: E402

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


if __name__ == '__main__':
    real_tree()
    fixtures()
    queue_fixtures()
    print(f'{sum(results)} of {len(results)} cases behaved')
    sys.exit(0 if all(results) else 1)
