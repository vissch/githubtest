#!/usr/bin/env python3
"""Tests of the briefs of the task board. Run from trench-warfare-3d/: python Tools/assetboard/test_taskbrief.py

What the records say about a task (digest), what is refused as a brief (check), the gate (a task with no brief is
not listed, and which are listed without one), the run the watcher starts (tick, with a launcher of the test's own:
no session is ever started from here), the brief on the page (tasks.site, tasks.js under node) and in the unit a
queued task becomes. Every case is built from files written here; nothing of the owner's is read or written.
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
TMP = Path(tempfile.mkdtemp(prefix='tw-taskbrief-test-'))
os.environ.update(TW_TASKS=str(TMP / 'root'), TW_FEEDBACK=str(TMP / 'inbox'), TW_NOTES=str(TMP / 'notes'), TW_BRIEFS=str(TMP / 'briefs'), TW_TASKBRIEFS=str(TMP / 'taskbriefs'), TW_TASKBRIEF_OFF='1')
for k in ('TW_TASKBRIEF_RUN', 'TW_TASKBRIEF_ROWS', 'TW_TASKBRIEF_SCRATCH'):
    os.environ.pop(k, None)
import ops          # noqa: E402
import src_tasks    # noqa: E402
import taskbrief    # noqa: E402
import tasks        # noqa: E402

results = []
NOW = time.time()
DAY = datetime.datetime.fromtimestamp(NOW)


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:900]))


def at(ago):
    return datetime.datetime.fromtimestamp(NOW - ago, datetime.timezone.utc).isoformat().replace('+00:00', 'Z')


def png(path: Path):
    path.parent.mkdir(parents=True, exist_ok=True)
    try:
        from PIL import Image
        Image.new('RGB', (320, 180), (40, 60, 90)).save(path, 'PNG')
    except Exception:       # noqa: BLE001  no Pillow here: the bytes still copy
        path.write_bytes(b'\x89PNG\r\n\x1a\n' + b'0' * 64)
    return path


def line(kind, content, ago, side, **more):
    return dict(type=kind, isSidechain=side, timestamp=at(ago), cwd=str(TMP / 'checkout'), message=dict(content=content), **more)


def log(path: Path, entries):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text('\n'.join(json.dumps(e) for e in entries) + '\n', encoding='utf-8')
    return path


STILL, NEXT, STRAY = png(TMP / 'shots' / 'night1.png'), png(TMP / 'shots' / 'night2.png'), png(TMP / 'elsewhere' / 'other.png')
DOC = TMP / 'shots' / 'CRITIC_BRIEF.md'
DOC.write_text('# The critic\'s brief\nScore the night battle <out of 100>.\n', encoding='utf-8')
ASK = f'You are a harsh art director reviewing stills. Read {STILL} and score the night battle out of 100. Do not modify any files.'


def rows():
    """An agent that was cut off (its log and its session's are written here), and the other kinds as rows."""
    parent = log(TMP / 'projects' / 'p' / 'sess1.jsonl', [
        line('user', 'make the night battle look finished, loop with a critic', 9900, False),
        line('assistant', [dict(type='text', text='Round two scored 56. Starting the third critic on the new stills.')], 9800, False),
        line('user', 'This session is being continued from a previous conversation that ran out of context.', 9700, False),
        line('user', 'and this was typed after the agent had started', 100, False)])
    agent = log(TMP / 'projects' / 'p' / 'sess1' / 'subagents' / 'agent-b1.jsonl', [
        line('user', ASK, 9600, True),
        line('assistant', [dict(type='tool_use', id='t1', name='Read', input=dict(file_path=str(STILL)))], 9500, True),
        line('assistant', [dict(type='tool_use', id='t2', name='Bash', input=dict(command='git log lane/show/night-look-2..HEAD && git diff lane/sim/no-such.thing'))], 9400, True),
        line('user', [dict(type='tool_result', tool_use_id='t2', content='[lane/show/night-look-2 abc1234] look: the flare\nlane/show/listed-only 9 commits')], 9300, True),
        line('assistant', [dict(type='tool_use', id='t3', name='Write', input=dict(file_path=str(TMP / 'checkout' / 'notes.md')))], 9200, True)])
    base = dict(state='left', lane='', by='', idle=150, stopped='2026-10-07 20:00', words=[])
    a = dict(base, id='agent-b1', kind='agent', title='Art critic round three', what=ASK[:200], ask=ASK, where='HERE', by='Night look', agent='general-purpose', touched=int(NOW - 9000), changed=int(NOW - 9000),
             why='a call to the service failed', log=str(agent), parent='sess1', parent_log=str(parent), cwd=str(TMP / 'checkout'), turns=['it: called Read'])
    theirs = dict(a, id='agent-far', where='OTHER', log='', parent_log='')
    hand = dict(base, id='handoff-night', kind='handoff', title='night look', what='for the next agent', ask='Read the handoff', where='', file='HANDOFF_AGENT_night.md', touched=int(NOW - 8000), changed=int(NOW - 8000))
    cap = dict(base, id='capture-1', kind='capture', title='The wire', what='The wire', ask='', where='HERE', by='you', touched=int(NOW - 60), idle=1, now=True, picture=str(STILL), lines=['In the forest.'])
    queued = dict(base, id='unit-q', kind='unit', title='rv-1', what='Fix it', ask='Fix it', where='', state='queued', touched=int(NOW - 7000))
    return a, theirs, hand, cap, queued


def good(**change):
    """A brief that is one, with what a case changes."""
    return dict(dict(title='Critic\'s third look at the night battle', about='Score twelve stills of the night battle and say which of six earlier faults are still visible.',
                     part_of='The owner\'s loop to make the night battle look finished.', stands='It never ran: the service failed on its first call. A retry was started after it.',
                     links=[('doc', 'The brief it was given', str(DOC))], pictures=[(str(STILL), 'A still it was to judge')], may_show=[str(STILL)]), **change)


def reading():
    a = rows()[0]
    (src_tasks.root()).mkdir(parents=True, exist_ok=True)
    d = taskbrief.digest(a)
    text = '\n'.join(taskbrief.digest_lines(d))
    case('digest: an agent\'s records give the owner\'s last request before it was started (not a summary the session was continued from, not what he typed later), its session\'s last line, '
         'what it wrote and the picture it opened',
         d['owner_asked_before'] == 'make the night battle look finished, loop with a critic' and d['session_said_before'] == ['Round two scored 56. Starting the third critic on the new stills.']
         and [Path(p['path']) for p in d['pictures']] == [STILL] and d['wrote'] == [str(TMP / 'checkout' / 'notes.md')] and 'make the night battle look finished' in text and str(STILL) in text,
         {k: d[k] for k in ('owner_asked_before', 'session_said_before', 'pictures', 'wrote')})
    case('digest: a lane is one it named in its own commands or one it committed on, never one a listing only showed, and the dots of a range are not part of its name',
         [l['lane'] for l in d['lanes']] == ['lane/show/night-look-2', 'lane/sim/no-such.thing'] and not d['lanes'][1]['on_origin'] and 'NOT on origin' in text, d['lanes'])
    asked_only = dict(a, log='', parent_log='')
    case('digest: a still its prompt names is the task\'s to show even when the agent never opened it', [Path(p['path']) for p in taskbrief.digest(asked_only)['pictures']] == [STILL], None)
    case('sig: a task that changed is another task to read; its parent session moving is not a change',
         taskbrief.sig(a) == taskbrief.sig(dict(a, touched=a['touched'] + 500)) and taskbrief.sig(a) != taskbrief.sig(dict(a, changed=a['changed'] + 1)) and taskbrief.sig(a) != taskbrief.sig(dict(a, id='agent-b2')), None)


def refused():
    a = rows()[0]

    def bad(**change):
        g = good(**change)
        return taskbrief.check(a, g['title'], g['about'], g['part_of'], g['stands'], g['links'], g['pictures'], g.get('no_picture', ''), g.get('no_link', ''), g['may_show'], g.get('scratch'))[0]
    case('check: a brief that is short, in its own words, links a doc that is there and shows a picture the task\'s records name is a brief', bad() == [], bad())
    case('check: words over the limit are refused, each field by its own limit, and so is an empty one',
         any('--about takes 46 words' in b for b in bad(about=' '.join(['word'] * 46))) and bad(about=' '.join(['word'] * 45)) == [] and any('--part-of takes 26' in b for b in bad(part_of=' '.join(['w'] * 26)))
         and any('--stands takes 46' in b for b in bad(stands=' '.join(['w'] * 46))) and any('--stands is empty' in b for b in bad(stands='')) and any('title is 71' in b for b in bad(title='x' * 71)), bad(about=' '.join(['word'] * 46)))
    case('check: the prompt the agent was given is not a brief: its wording and its own first words are refused',
         any('repeats the prompt' in b for b in bad(about='You are a harsh art director who reviews stills.')) and any('repeats the prompt' in b for b in bad(about='It says: ' + ASK[:70]))
         and any('the title repeats' in b for b in bad(title='You are a harsh art director')), bad(about='You are a harsh art director who reviews stills.'))
    case('check: a link is refused when its target is not there: a doc that is no file, a handoff the folder does not hold, a lane and a commit origin does not have, a page that is no link, a kind it does not know',
         any('the doc' in b for b in bad(links=[('doc', 'A plan', str(TMP / 'no-such.md'))])) and any('the handoff' in b for b in bad(links=[('handoff', 'Its handoff', 'HANDOFF_AGENT_no_such.md')]))
         and any('the lane lane/show/no-such-lane-ever' in b for b in bad(links=[('lane', 'Its lane', 'lane/show/no-such-lane-ever')])) and any('not a commit' in b for b in bad(links=[('commit', 'The fix', '0123456789ab')]))
         and any('is not a link' in b for b in bad(links=[('page', 'A page', 'ftp://x')])) and any('of kind' in b for b in bad(links=[('wiki', 'A page', 'x')]))
         and any('board has no page' in b for b in bad(links=[('board', 'The decisions', 'secret.html')])) and bad(links=[('board', 'The decisions', 'decide.html')]) == [], bad(links=[('doc', 'A plan', str(TMP / 'no-such.md'))]))
    case('check: a link needs a label of six words at most', any('label' in b for b in bad(links=[('doc', '', str(DOC))])) and any('label' in b for b in bad(links=[('doc', 'one two three four five six seven', str(DOC))])), None)
    case('check: a picture is the task\'s own or it is refused: one its records name, one beside such a one, one the run made in its folder; never one from elsewhere, never a file that is not there',
         bad(pictures=[(str(NEXT), 'The next still')]) == [] and any('other.png is not one of the pictures' in b for b in bad(pictures=[(str(STRAY), 'Another')]))
         and bad(pictures=[(str(STRAY), 'A page it shot')], scratch=str(STRAY.parent)) == [] and any('is not a picture or a film that is there' in b for b in bad(pictures=[(str(TMP / 'shots' / 'gone.png'), 'Gone')]))
         and any('caption' in b for b in bad(pictures=[(str(STILL), '')])), bad(pictures=[(str(STRAY), 'Another')]))
    case('check: nothing shown, or nothing linked, is said with its reason or refused; more than the page takes is refused',
         any('it shows nothing' in b for b in bad(pictures=[])) and bad(pictures=[], no_picture='It stopped before it opened a still, and the stills are gone.') == [] and any('it links nothing' in b for b in bad(links=[]))
         and bad(links=[], no_link='Nothing was written down for it.') == [] and any('5 pictures' in b for b in bad(pictures=[(str(STILL), 'A still')] * 5)) and any('7 links' in b for b in bad(links=[('board', 'The tasks', 'tasks.html')] * 7)), bad(pictures=[]))


def written():
    a = rows()[0]
    where = taskbrief.folder()
    g = good()
    try:
        taskbrief.add(where, a, g['title'], ASK[:70] + ' and so on', g['part_of'], g['stands'], g['links'], g['pictures'], may_show=g['may_show'])
        no = ''
    except ValueError as e:
        no = str(e)
    case('add: what is not a brief is not written, and the error says why', 'repeats the prompt' in no and not (where / 'agent-b1' / 'brief.json').exists(), no)
    keep_facts, keep_origin = taskbrief.lane_facts, taskbrief.origin
    taskbrief.lane_facts, taskbrief.origin = (lambda lane: (True, 3)), (lambda: 'https://github.com/x/y')
    try:
        b = taskbrief.add(where, a, g['title'], g['about'], g['part_of'], g['stands'], g['links'] + [('lane', 'The lane it judged', 'lane/show/night-look-2')], g['pictures'], may_show=g['may_show'], now=DAY)
    finally:
        taskbrief.lane_facts, taskbrief.origin = keep_facts, keep_origin
    home = where / 'agent-b1'
    case('add: a brief is a folder with brief.json, a copy of each picture and of each doc it links; a lane\'s link leads to origin; it is the brief of the task as it is now',
         b['sig'] == taskbrief.sig(a) and (home / b['pictures'][0]['file']).is_file() and (home / b['links'][0]['file']).read_text(encoding='utf-8').startswith('# The critic')
         and b['links'][1] == dict(kind='lane', label='The lane it judged', target='lane/show/night-look-2', href='https://github.com/x/y/tree/lane/show/night-look-2')
         and taskbrief.current(where, a)['title'] == g['title'] and taskbrief.current(where, dict(a, changed=a['changed'] + 60)) is None and 'source' not in json.dumps(b), b)
    return b


def gate():
    a, theirs, hand, cap, queued = rows()
    where = taskbrief.folder()
    failed = dict(hand, id='handoff-failed')
    w = taskbrief.waits(where, failed, NOW)
    taskbrief.put(where / 'handoff-failed' / 'wait.json', dict(w, n=taskbrief.TRIES, why='the session left no result'))
    once = dict(hand, id='handoff-once')
    taskbrief.put(where / 'handoff-once' / 'wait.json', dict(taskbrief.waits(where, once, NOW), n=taskbrief.TRIES - 1))
    old = dict(hand, id='handoff-old')
    taskbrief.put(where / 'handoff-old' / 'wait.json', dict(taskbrief.waits(where, old, NOW), since=int(NOW - taskbrief.HOLD_MAX - 60)))
    young = dict(hand, id='handoff-young')
    taskbrief.put(where / 'handoff-young' / 'wait.json', dict(taskbrief.waits(where, young, NOW), since=int(NOW - taskbrief.HOLD_MAX + 600)))
    T = dict(rows=[dict(r) for r in (cap, a, theirs, hand, failed, once, old, young, queued)])
    n = taskbrief.attach(T, where)
    held = taskbrief.hold(T, where, NOW)
    by = {r['id']: r for r in T['rows']}
    case('gate: a task with no brief is not listed, it is counted; one with its brief is listed and carries it',
         n == 1 and by['agent-b1']['brief']['about'] and {r['id'] for r in held} == {'agent-far', 'handoff-night', 'handoff-once', 'handoff-young'} and T['reading'] == 4 and not {r['id'] for r in held} & set(by), (sorted(by), T['reading']))
    case('gate: listed without a brief are only a capture from the game, a task he already said something about, and a task that could not be read: readings that failed, or a day\'s wait; that one says so',
         'capture-1' in by and 'nocontext' not in by['capture-1'] and 'unit-q' in by and 'nocontext' not in by['unit-q'] and 'failed' in by['handoff-failed']['nocontext'] and '24 hours' in by['handoff-old']['nocontext'],
         {i: r.get('nocontext') for i, r in by.items()})
    changed = dict(a, changed=a['changed'] + 60)
    T2 = dict(rows=[changed])
    taskbrief.attach(T2, where)
    case('gate: a task that changed after it was read is held again until it is read as it is now', [r['id'] for r in taskbrief.hold(T2, where, NOW)] == ['agent-b1'] and T2['rows'] == [], T2)


class Proc:
    def __init__(self):
        self.code, self.killed, self.pid = None, False, 4242

    def poll(self):
        return self.code

    def kill(self):
        self.killed, self.code = True, -9


def runs():
    """The watcher's side, on a folder of its own: nothing here starts a session."""
    keep = os.environ['TW_TASKBRIEFS']
    os.environ['TW_TASKBRIEFS'] = str(TMP / 'runs')
    where, started = taskbrief.folder(), []
    a, theirs, hand, cap, queued = rows()
    more = [dict(hand, id=f'handoff-{i}', idle=200 + i) for i in range(5)]
    T = dict(rows=[a, theirs, queued] + more + [cap])
    lim = dict(runs_per_day=3, usd_per_run=2.0, minutes=20, model='sonnet')

    def launch(cmd, cwd, env, out):
        started.append(dict(cmd=cmd, cwd=cwd, env=env, out=out, p=Proc()))
        return started[-1]['p']

    def boom(*a_, **k_):
        raise AssertionError('a test started a real session')
    keep_popen, taskbrief.subprocess.Popen = taskbrief.subprocess.Popen, boom
    try:
        s0 = taskbrief.tick(T, where, now=DAY, lim=lim, host='HERE')
        case('tick: switched off on a station (the tests of every tool), nothing is started and the page is told why', s0['running'] is None and 'switched off' in s0['off'] and s0['waiting'] == 8, s0)
        s1 = taskbrief.tick(T, where, now=DAY, launch=launch, lim=lim, host='HERE')
        s2 = taskbrief.tick(T, where, now=DAY + datetime.timedelta(seconds=30), launch=launch, lim=lim, host='HERE')
        ids = started[0]['env'] and json.loads(Path(started[0]['env']['TW_TASKBRIEF_ROWS']).read_text(encoding='utf-8'))
        case('tick: one run is started for the next four that wait, the capture first, then what waits off the page (what stopped last first), a task he already queued after those; another station\'s agent is not this one\'s to read; '
             'while it runs nothing else is started',
             len(started) == 1 and s1['running']['ids'] == ['capture-1', 'agent-b1', 'handoff-0', 'handoff-1'] and s2['running'] and sorted(ids) == sorted(s1['running']['ids']) and ids['agent-b1']['log'] == a['log']
             and s1['waiting'] == 8 and s1['left'] == 2 and taskbrief.wanted(T, where, 'HERE', NOW)[-1]['id'] == 'unit-q', (s1, len(started)))
        cmd = taskbrief.command(['agent-b1'], lim, exe='claude')
        case('tick: the session it starts may read and run this tool, and nothing else: no agent, no web, no question; it is held to the run\'s money and the cheaper model, and told the skill and the tasks',
             cmd[cmd.index('--allowedTools') + 1:cmd.index('--disallowedTools')] == ['Read', 'Grep', 'Glob', f'Bash(python {taskbrief.TOOL} *)']
             and cmd[cmd.index('--disallowedTools') + 1:cmd.index('--max-budget-usd')] == ['AskUserQuestion', 'Agent', 'WebSearch', 'WebFetch'] and cmd[cmd.index('--max-budget-usd') + 1] == '2.0'
             and cmd[-2:] == ['--model', 'sonnet'] and cmd[cmd.index('--permission-mode') + 1] == 'acceptEdits' and 'tw-task-context' in cmd[2] and 'agent-b1' in cmd[2]
             and Path(started[0]['cwd']).is_dir() and Path(started[0]['cwd']).parent == taskbrief.state() / 'runs' and Path(started[0]['env']['TW_TASKBRIEF_ROWS']).parent == Path(started[0]['cwd']).parent, cmd)
        # the run writes one brief of its four, and ends
        g = good()
        taskbrief.add(where, a, g['title'], g['about'], g['part_of'], g['stands'], g['links'], g['pictures'], may_show=g['may_show'], run=started[0]['env']['TW_TASKBRIEF_RUN'])
        Path(started[0]['out']).write_text('warming up\n' + json.dumps(dict(total_cost_usd=0.4321, result='done', is_error=False)), encoding='utf-8')
        started[0]['p'].code = 0
        s3 = taskbrief.tick(T, where, now=DAY + datetime.timedelta(minutes=5), launch=launch, lim=lim, host='HERE')
        spend = taskbrief.spent(where, f'{DAY:%Y-%m-%d}')
        case('tick: a run that ended is a line with its dollars, its host and how many briefs it made; a task it left without one has one failed reading written down; the next four are started',
             s3['last']['made'] == 1 and s3['last']['usd'] == 0.43 and spend[0]['host'] and spend[0]['ids'] == s1['running']['ids'] and taskbrief.waits(where, cap)['n'] == 1 and taskbrief.waits(where, more[0])['n'] == 1
             and 'wait.json' in os.listdir(where / 'handoff-0') and len(started) == 2 and s3['running']['ids'] == ['capture-1', 'handoff-0', 'handoff-1', 'handoff-2'], (s3, spend))
        s4 = taskbrief.tick(T, where, now=DAY + datetime.timedelta(minutes=26), launch=launch, lim=lim, host='HERE')
        case('tick: a run past its minutes is stopped and said so; a task two readings failed on is given up and no longer asked for',
             started[1]['p'].killed and 'stopped after 20 minutes' in s4['last']['why'] and 'failed' in taskbrief.given_up(where, more[0], NOW) and len(started) == 3
             and s4['running']['ids'] == ['handoff-2', 'handoff-3', 'handoff-4', 'unit-q'], (s4, started[1]['p'].killed))
        started[2]['p'].code = 1
        s5 = taskbrief.tick(T, where, now=DAY + datetime.timedelta(minutes=30), launch=launch, lim=lim, host='HERE')
        case('tick: the day\'s runs are a number: once they are used nothing is started and the page is told', len(started) == 3 and s5['running'] is None and 'the day\'s 3 readings are used' in s5['off'] and s5['waiting'] == 3
             and s5['last']['why'] == 'the session left no result', s5)
        other = dict(hand, id='handoff-claimed')
        taskbrief.put(where / 'handoff-claimed' / 'claim.json', dict(host='OTHER', at=int(NOW - 60), sig=taskbrief.sig(other)))
        stale = dict(hand, id='handoff-claim-old')
        taskbrief.put(where / 'handoff-claim-old' / 'claim.json', dict(host='OTHER', at=int(NOW - taskbrief.CLAIM - 60), sig=taskbrief.sig(stale)))
        case('tick: a shared task another station claimed lately is left to it; a claim that is old holds nothing', [r['id'] for r in taskbrief.wanted(dict(rows=[other, stale]), where, 'HERE', NOW)] == ['handoff-claim-old'], None)
    finally:
        taskbrief.subprocess.Popen = keep_popen
        os.environ['TW_TASKBRIEFS'] = keep


def in_a_run():
    """What a run may ask of the tool."""
    a = rows()[0]
    given = TMP / 'given.json'
    given.write_text(json.dumps({a['id']: a}), encoding='utf-8')
    scratch = TMP / 'scratch'
    scratch.mkdir(exist_ok=True)
    keep = {k: os.environ.get(k) for k in ('TW_TASKBRIEF_RUN', 'TW_TASKBRIEF_ROWS', 'TW_TASKBRIEF_SCRATCH')}
    os.environ.update(TW_TASKBRIEF_RUN='r1', TW_TASKBRIEF_ROWS=str(given), TW_TASKBRIEF_SCRATCH=str(scratch))
    out, real = sys.stdout, None
    try:
        import io
        sys.stdout = io.StringIO()
        codes = [taskbrief.main(['tick']), taskbrief.main(['context', 'handoff-night']), taskbrief.main(['shoot', str(DOC), str(TMP / 'elsewhere' / 'x.png')]), taskbrief.main(['context', 'agent-b1']),
                 taskbrief.main(['add', 'agent-b1', '--title', 'Critic\'s third look at the night battle', '--about', 'Score the stills of the night battle.', '--part-of', 'The night look loop.', '--stands', 'It never ran.',
                                 '--link', f'doc=The brief it was given={DOC}', '--picture', f'{NEXT}=The second still'])]
        real = sys.stdout.getvalue()
    finally:
        sys.stdout = out
        for k, v in keep.items():
            os.environ.pop(k, None) if v is None else os.environ.update({k: v})
    b = taskbrief.read(taskbrief.folder(), 'agent-b1')
    case('a run: it may read the context of the tasks it was given and add their briefs; it starts no run, reads no other task, and makes no picture outside its own folder',
         codes == [1, 1, 1, 0, 0] and 'tick is not for a run' in real and 'is not a task this run was given' in real and 'in the run\'s own folder' in real and b['run'] == 'r1' and b['about'] == 'Score the stills of the night battle.'
         and b['pictures'][0]['caption'] == 'The second still', (codes, real[-600:]))


def on_the_page(b):
    a, theirs, hand, cap, queued = rows()
    where, out = taskbrief.folder(), TMP / 'site'
    g = good()
    b = taskbrief.add(where, a, g['title'], g['about'], g['part_of'], g['stands'], [('doc', 'The <brief> it was given', str(DOC)), ('board', 'The decisions that wait', 'decide.html')], g['pictures'], may_show=g['may_show'], now=DAY)
    T = dict(rows=[dict(a), dict(queued)], relay=dict(waiting=0, as_of='', taken=[]), stale_minutes=90)
    taskbrief.attach(T, where)
    unit = src_tasks.unit_for(T['rows'][0])
    case('unit: a queued task\'s unit carries what the owner read on the page, with its links, before what the agent was asked',
         g['about'] in unit['goal'] and 'Part of: ' + g['part_of'] in unit['goal'] and 'The decisions that wait: decide.html' in unit['goal'] and unit['goal'].index(g['about']) < unit['goal'].index('What it was asked:\n' + ASK[:40])
         and 'as it was read for' not in src_tasks.unit_for(dict(queued, state='left'))['goal'], unit['goal'][:600])
    (out / 'img' / 'task' / 'agent-gone').mkdir(parents=True)
    (out / 'task' / 'agent-gone').mkdir(parents=True)
    tasks.site(T, out)
    text = (out / 'data' / 'tasks.js').read_text(encoding='utf-8')
    by = {r['id']: r for r in json.loads(text[len('window.TASKS = '):].rstrip().rstrip(';'))['rows']}
    r = by['agent-b1']
    page = out / r['links'][0]['href']
    case('site: a task with its brief is shown by it: the brief\'s title (what it was called is kept), what it is about, what it was part of, where it stands, its picture beside the page',
         r['title'] == g['title'] and r['was'] == 'Art critic round three' and r['about'] == g['about'] and r['part_of'] == g['part_of'] and r['stands'] == g['stands']
         and r['shots'] == [dict(src=f'img/task/agent-b1/{b["pictures"][0]["file"]}', name='A still it was to judge', caption='A still it was to judge', film=False)] and (out / r['shots'][0]['src']).is_file()
         and 'brief' not in r and 'ask' not in r and r['told'].startswith('You are a harsh'), r)
    case('site: a doc it links is a page of its own beside the board, with the doc\'s text as text; no row carries a path of this machine; a task with no brief is as it was',
         r['links'][0]['href'].startswith('task/agent-b1/') and page.is_file() and 'Score the night battle &lt;out of 100&gt;.' in page.read_text(encoding='utf-8') and '<h1>The &lt;brief&gt; it was given</h1>' in page.read_text(encoding='utf-8') and r['links'][1] == dict(label='The decisions that wait', href='decide.html', kind='board', title='decide.html')
         and str(TMP) not in text.replace('\\\\', '\\').replace(r['told'], '') and by['unit-q']['title'] == 'rv-1' and 'about' not in by['unit-q'] and by['unit-q']['shots'] == [], (r['links'], by['unit-q']))
    case('site: what the brief of a task no longer listed showed is removed from the site', not (out / 'img' / 'task' / 'agent-gone').exists() and not (out / 'task' / 'agent-gone').exists() and (out / 'img' / 'task' / 'agent-b1').is_dir(), None)
    # the watcher's own order: read, the briefs put on, the gate, the site
    keep_read, tasks.read = tasks.read, lambda **k: dict(rows=[dict(a), dict(hand), dict(cap)], relay=dict(waiting=0, as_of='', taken=[]), left=2, queued=0, captures=1, stale_minutes=90)
    try:
        counts = ops.the_tasks(out, {}, [], False)
    finally:
        tasks.read = keep_read
    D = json.loads((out / 'data' / 'tasks.js').read_text(encoding='utf-8')[len('window.TASKS = '):].rstrip().rstrip(';'))
    case('ops: every reading of the board puts the briefs on and holds what has none: the page gets the read task and the capture, and the count of what is being read first',
         [x['id'] for x in D['rows']] == ['agent-b1', 'capture-1'] and D['reading'] == 1 and D['rows'][0]['about'] == g['about'] and counts['left'] == 2 and D['reading_off'] == '' and D['reading_now'] is False, (D.get('reading'), [x['id'] for x in D['rows']]))
    node = shutil.which('node')
    if not node:
        print('      (no node on this machine: the page\'s own cases were not run)')
        return
    js = ('const T = require(process.argv[1]);'
          'const L = [1, 2, 3, 4].map(i => ({label: "L" + i, href: "task/a/" + i + ".html", kind: "doc", title: "t" + i}));'
          'const A = {id: "a", kind: "agent", title: "Critic\'s third look", was: "Art critic round three", what: "You are a harsh", about: "Score the stills.", part_of: "The night loop.", stands: "It never ran.", state: "left", idle: 91,'
          ' links: L, shots: [{src: "img/task/a/1.mp4", caption: "The film", film: true}, {src: "img/task/a/2.jpg", caption: "A still"}], detail: ["An agent on MSI.", "It stopped."]};'
          'const B = {id: "b", kind: "agent", title: "Review", what: "Review the sim", state: "left", idle: 91, detail: ["An agent."], nocontext: "2 readings of it failed"};'
          'const D = {rows: [A, B], reading: 3, relay: {waiting: 0}, stations: [], stale_minutes: 90, read_at: 1000};'
          'const G = T.groups(D), S = T.subject(A, "Agents", []), SB = T.subject(B, "Agents", []);'
          'console.log(JSON.stringify([G[0].rows[0], G[0].rows[1], S.detail, S.links.length, S.shots.length, SB.detail[0], SB.links, T.head(D, G), T.head(Object.assign({}, D, {reading: 1}), G),'
          ' T.warnings(Object.assign({}, D, {reading_off: "the day\'s 12 readings are used"}), 1000), T.warnings(D, 1000), T.foot(D, 1000)]))')
    p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'tasks.js')], capture_output=True)
    got = json.loads(p.stdout.decode() or 'null')
    case('page: a row that was read says what the task is about in place of the prompt, what it was part of, and carries its first three links and its film; one that could not be read says so on a chip',
         got and got[0]['top'] == 'Critic\'s third look' and got[0]['sub'] == 'Score the stills.' and got[0]['part'] == 'The night loop.' and got[0]['stands'] == 'It never ran.' and got[0]['read'] is True and got[1]['read'] is False and [l['label'] for l in got[0]['links']] == ['L1', 'L2', 'L3'] and got[0]['film'] is True
         and got[0]['shot'] == 'img/task/a/1.mp4' and got[0]['tip'] == 'Score the stills.' and got[1]['sub'] == 'Review the sim' and got[1]['links'] == [] and got[1]['film'] is False and 'no context found' in got[1]['chips']
         and 'no context found' not in got[0]['chips'], (got and got[:2], p.stderr[-400:]))
    case('page: the panel opens on what it is, what it was part of and where it stands, then the record\'s own lines, with every link and picture; one that could not be read says why first',
         got and got[2] == ['Score the stills.', 'Part of: The night loop.', 'Where it stands: It never ran.', 'An agent on MSI.', 'It stopped.'] and got[3] == 4 and got[4] == 2
         and got[5] == 'Nothing could say what this is about: 2 readings of it failed. Below is what its records hold.' and got[6] == [], got and got[2:7])
    case('page: the line under the title counts what is being read first, and when nothing is reading the page says so and why above the rows',
         got and got[7].endswith('3 more are being read first') and got[8].endswith('1 more is being read first')
         and got[9] == ['3 tasks wait to be read before they are listed, and nothing is reading: the day\'s 12 readings are used.'] and got[10] == [] and 'an agent has read what it is about' in got[11], got and got[7:])


if __name__ == '__main__':
    reading()
    refused()
    b = written()
    gate()
    runs()
    in_a_run()
    on_the_page(b)
    shutil.rmtree(TMP, ignore_errors=True)
    print(f'{sum(results)} of {len(results)} cases behaved')
    sys.exit(0 if all(results) else 1)
