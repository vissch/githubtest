#!/usr/bin/env python3
"""Tests of the task board's rules. Run from trench-warfare-3d/: python Tools/assetboard/test_tasks.py

What a task is and when it is listed (src_tasks.py), the game's captures taken in (feedback.py), the owner's word
taken up (tasks.py), the page's own rules (tasks.js, under node) and the watcher going on after a read that failed
(ops.py). Every case is built from files written here, aged with os.utime; nothing of the owner's is read except by
the one case that counts this station's own logs, which changes nothing.
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
TMP = Path(tempfile.mkdtemp(prefix='tw-tasks-test-'))
# no case writes into the owner's own folders: set before anything reads them
os.environ.update(TW_TASKS=str(TMP / 'root'), TW_FEEDBACK=str(TMP / 'inbox'), TW_NOTES=str(TMP / 'notes'), TW_BRIEFS=str(TMP / 'briefs'), TW_TASKBRIEFS=str(TMP / 'taskbriefs'), TW_TASKBRIEF_OFF='1')
import briefs     # noqa: E402
import feedback   # noqa: E402
import notes      # noqa: E402
import ops        # noqa: E402
import src_ops    # noqa: E402
import src_tasks  # noqa: E402
import tasks      # noqa: E402

results = []
NOW = time.time()
MIN = 60


def case(name, ok, detail=''):
    results.append(bool(ok))
    print(('ok    ' if ok else 'FAIL  ') + name + ('' if ok else '\n      ' + str(detail)[:700]))


def at(ago):
    return datetime.datetime.fromtimestamp(NOW - ago, datetime.timezone.utc).isoformat().replace('+00:00', 'Z')


# the lines a log ends on, as this station's own logs have them
def asked(text, cwd='C:\\Users\\x\\Documents\\GitHub\\githubtest-pipe', ago=9000, side=True):
    return dict(type='user', isSidechain=side, cwd=cwd, timestamp=at(ago), message=dict(content=text))


def ended(ago=6000, side=True):
    return dict(type='assistant', isSidechain=side, timestamp=at(ago), message=dict(stop_reason='end_turn', content=[dict(type='text', text='Here is the report.')]))


def calling(name='Bash', inp=None, ago=6000, side=True):
    return dict(type='assistant', isSidechain=side, timestamp=at(ago), message=dict(stop_reason='tool_use', content=[dict(type='tool_use', id='t1', name=name, input=inp or dict(command='ls'))]))


def result(text='ok', ago=6000, side=True, ends=False):
    d = dict(type='user', isSidechain=side, timestamp=at(ago), message=dict(content=[dict(type='tool_result', tool_use_id='t1', content=text)]))
    return dict(d, toolEndsTurn=True) if ends else d


def failed(ago=6000, side=True):
    return dict(type='assistant', isSidechain=side, isApiErrorMessage=True, timestamp=at(ago), message=dict(stop_reason='stop_sequence', content=[dict(type='text', text="API Error: Can't reach the API server")]))


def interrupted(ago=6000, side=True):
    return dict(type='user', isSidechain=side, timestamp=at(ago), message=dict(content=[dict(type='text', text='[Request interrupted by user]')]))


def log(path: Path, entries, age_min):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text('\n'.join(json.dumps(e) for e in entries) + '\n', encoding='utf-8')
    os.utime(path, (NOW - age_min * MIN, NOW - age_min * MIN))
    return path


def agent(projects, session, name, entries, age_min, desc='Review the sim', kind='general-purpose'):
    f = log(projects / 'p' / session / 'subagents' / f'agent-{name}.jsonl', entries, age_min)
    f.with_name(f.stem + '.meta.json').write_text(json.dumps(dict(agentType=kind, description=desc)), encoding='utf-8')
    return f


def endings():
    E = src_tasks.ending
    got = [E([asked('do it'), ended()], side=True)[0], E([asked('do it'), calling('SubagentHandback'), result(ends=True)], side=True)[0],
           E([asked('do it'), calling()], side=True)[0], E([asked('do it'), calling(), result()], side=True)[0], E([asked('do it'), failed()], side=True)[0],
           E([asked('do it')], side=True)[0], E([asked('do it'), interrupted()], side=True)[0],
           E([asked('go', side=False), calling('AskUserQuestion', dict(questions=[]), side=False)])[0],
           E([asked('go', side=False), ended(side=False), calling(side=True)])[0],          # an agent's line in a session's log is not the session's
           E([])[0],
           E([asked('do it'), dict(type='assistant', isSidechain=True, message=dict(stop_reason=None, content=[dict(type='text', text='I will now')]))], side=True)[0]]
    case('tasks: a log that ends on the model\'s last word or its report handed back is done; on a tool call, a result or a request nobody answered, a failed call or a sentence cut short, '
         'it was cut off; the owner\'s interrupt is his doing, and a question to him nobody answered is told apart',
         got == ['done', 'done', 'cut', 'cut', 'cut', 'cut', 'stopped', 'asks', 'done', 'done', 'cut'], got)
    why = [E([asked('x'), calling()], side=True)[1], E([asked('x'), failed()], side=True)[1], E([asked('x'), calling(), result()], side=True)[1]]
    case('tasks: a cut-off log says how it stopped, in words for the page', why == ['in the middle of a tool call', 'a call to the service failed', 'before it answered'], why)


def listed():
    """The one rule: unfinished, and nothing touched it for STALE."""
    P = TMP / 'projects'
    ours = [asked('carry on with the lane', side=False), ended(side=False)]
    log(P / 'p' / 'quiet001.jsonl', ours, 300)
    agent(P, 'quiet001', 'cutlong', [asked('Review the sim'), calling()], 91, desc='Cut 91 minutes ago')
    agent(P, 'quiet001', 'cutshort', [asked('Review the sim'), calling()], 89, desc='Cut 89 minutes ago')
    agent(P, 'quiet001', 'finished', [asked('Review the sim'), ended()], 300, desc='Finished')
    agent(P, 'quiet001', 'stoppedx', [asked('Review the sim'), interrupted()], 300, desc='Stopped by him')
    agent(P, 'quiet001', 'redone01', [asked('Review the sim'), failed()], 300, desc='Run twice')
    agent(P, 'quiet001', 'redone02', [asked('Review the sim'), ended()], 280, desc='Run twice')
    agent(P, 'quiet001', 'ancient1', [asked('Review the sim'), calling()], (src_tasks.DAYS + 1) * 24 * 60, desc='From before the window')
    log(P / 'p' / 'busy0001.jsonl', ours, 10)
    agent(P, 'busy0001', 'parentup', [asked('Review the sim'), calling()], 300, desc='Its session is still at work')
    log(P / 'p' / 'other001.jsonl', [asked('cut the trailer', cwd='C:\\films', side=False), ended(side=False)], 300)
    agent(P, 'other001', 'notours1', [asked('Grade the balcony shot', cwd='C:\\films'), calling()], 300, desc='Another project')
    log(P / 'p' / 'cutsess1.jsonl', [asked('fix the wire', side=False), calling(side=False)], 100)
    log(P / 'p' / 'asksess1.jsonl', [asked('plan the lane', side=False), calling('AskUserQuestion', dict(questions=[]), side=False)], 100)
    log(P / 'p' / 'freshcut.jsonl', [asked('fix the wire', side=False), calling(side=False)], 20)
    log(P / 'p' / 'stopsess.jsonl', [asked('try the other shader', side=False), interrupted(side=False)], 100)
    cache = {}
    mine = src_tasks.local(P, NOW, cache, 'HERE')
    T = src_tasks.collect(mine, now=NOW)
    ids = sorted(r['id'] for r in T['rows'])
    case('tasks: a cut-off agent is listed at 91 minutes and not at 89; never one that finished, one he stopped, one its session ran again to the end, one whose session is still at work, '
         'one of another project or one from before the window; a session cut off or left on a question is listed the same way, and one he stopped himself apart from them',
         ids == ['agent-cutlong', 'session-asksess1', 'session-cutsess1', 'session-stopsess'] and {r['id']: r['kind'] for r in T['rows']}['session-stopsess'] == 'paused'
         and T['paused'] == 1 and T['left'] == 3, (ids, T['paused'], T['left']))
    cut = next(r for r in mine if r['id'] == 'session-cutsess1')
    u = src_tasks.unit_for(dict(cut, words=[]))
    case('tasks: the unit of a cut-off session tells the leg where the work is: the station, the folder, the transcript and its session, what it was asked and its last lines, '
         'and that those files are on that station only',
         all(x in u['goal'] for x in ('station HERE', 'githubtest-pipe', str(P / 'p' / 'cutsess1.jsonl'), 'session cutsess1', 'fix the wire', 'Its last lines', 'on HERE only', 'in the middle of a tool call'))
         and u['id'] == 'task-session-cutsess1', u['goal'])
    ag = next(r for r in mine if r['id'] == 'agent-cutlong')
    u = src_tasks.unit_for(dict(ag, words=[]))
    case('tasks: the unit of a cut-off agent names its log and the session that started it',
         str(P / 'p' / 'quiet001' / 'subagents' / 'agent-cutlong.jsonl') in u['goal'] and 'quiet001' in u['goal'] and 'Review the sim' in u['goal'], u['goal'])
    row = next((r for r in T['rows'] if r['id'] == 'agent-cutlong'), {})
    case('tasks: a row says what the task was, who started it, on which station, how long nobody has been on it and how it stopped',
         row.get('title') == 'Cut 91 minutes ago' and row.get('where') == 'HERE' and row.get('by') == 'carry on with the lane' and row.get('idle') == 91
         and 'in the middle of a tool call' in ' '.join(row.get('detail', [])) and row.get('state') == 'left', row)
    case('tasks: a session left on a question to him says so', next(r for r in T['rows'] if r['id'] == 'session-asksess1').get('asks') is True, None)
    seen = len(cache['files'])
    os.utime(P / 'p' / 'busy0001.jsonl', (NOW - 200 * MIN, NOW - 200 * MIN))
    again = src_tasks.collect(src_tasks.local(P, NOW, cache, 'HERE'), now=NOW)
    case('tasks: when its session goes quiet too, the agent it left cut off is listed; a log is read again only when it changed',
         'agent-parentup' in [r['id'] for r in again['rows']] and len(cache['files']) == seen and seen == 15, (sorted(r['id'] for r in again['rows']), seen))
    return P


def shared(P):
    """What both stations read the same: the handoffs, the unit files, the relay's queue, the ready steps."""
    root = src_tasks.root()
    root.mkdir(parents=True, exist_ok=True)
    (root / 'handoffs.json').write_text(json.dumps(dict(handoffs={
        'HANDOFF_AGENT_waiting.md': dict(topic='the waiting one', state='current', **{'for': 'Finish the critique.'}),
        'HANDOFF_AGENT_in_hand.md': dict(topic='the one in hand', state='current', **{'for': 'Land the lanes.'}),
        'HANDOFF_AGENT_listed_only.md': dict(topic='the one only listed', state='current', **{'for': 'Rebase.'}),
        'HANDOFF_AGENT_parked.md': dict(topic='parked', state='parked', **{'for': 'Later.'}),
        'HANDOFF_AGENT_gone.md': dict(topic='its file is gone', state='current', **{'for': 'Nothing.'})})), encoding='utf-8')
    for n in ('waiting', 'in_hand', 'listed_only', 'parked'):
        f = root / f'HANDOFF_AGENT_{n}.md'
        f.write_text('# handoff\n', encoding='utf-8')
        os.utime(f, (NOW - 200 * MIN, NOW - 200 * MIN))
    log(P / 'p' / 'reader01.jsonl', [asked('pick it up', side=False), calling('Read', dict(file_path=str(root / 'HANDOFF_AGENT_in_hand.md')), ago=300, side=False),
                                     result('HANDOFF_AGENT_listed_only.md\nHANDOFF_AGENT_waiting.md', ago=290, side=False), ended(ago=280, side=False)], 5)
    got = sorted(r['id'] for r in src_tasks.collect([], shared=src_tasks.handoffs(root, P, NOW), now=NOW)['rows'])
    case('tasks: a handoff the index calls current is listed when nobody opened it for an hour and a half: not one a live session reads, not a parked one, not one whose file is gone; '
         'a folder listing that shows its name is not reading it',
         got == ['handoff-listed_only', 'handoff-waiting'], got)

    units = root / 'units-for-master'
    units.mkdir(exist_ok=True)
    for uid in ('never-queued', 'in-the-queue', 'task-agent-x'):
        (units / f'{uid}.json').write_text(json.dumps(dict(id=uid, lane='lane/show/x', role='lane', goal='Do it.', done_when=['true'])), encoding='utf-8')
        os.utime(units / f'{uid}.json', (NOW - 200 * MIN, NOW - 200 * MIN))
    (units / 'agents-laptop.json').write_text(json.dumps(dict(day='2026-10-07', usd=3)), encoding='utf-8')
    got = [r['id'] for r in src_tasks.collect([], shared=src_tasks.unit_files(units, queued={'in-the-queue': {}}), now=NOW)['rows']]
    case('tasks: a unit file written for the relay that never reached its queue is listed: not one that is queued, not one the board wrote from a task, not a file that is no unit',
         got == ['unit-never-queued'], got)

    # the relay's queue, read off a board's origin/main with git: nothing checked out
    board = TMP / 'board'
    (board / 'relay' / 'queue').mkdir(parents=True)
    (board / 'relay' / 'done').mkdir()
    (board / 'relay' / 'desktop' / 'legs').mkdir(parents=True)
    (board / 'relay' / 'desktop' / 'stops').mkdir()
    for uid in ('tried-and-failed', 'finished', 'never-run'):
        (board / 'relay' / 'queue' / f'{uid}.json').write_text(json.dumps(dict(id=uid, lane='lane/sim/x', role='lane', goal=f'The goal of {uid}.', done_when=['true'])), encoding='utf-8')
    (board / 'relay' / 'done' / 'finished.json').write_text(json.dumps(dict(id='finished', done_at=at(9000))), encoding='utf-8')
    for k, (uid, ago) in enumerate((('tried-and-failed', 4 * 3600), ('tried-and-failed', 3 * 3600), ('finished', 5 * 3600))):
        (board / 'relay' / 'desktop' / 'legs' / f'run1-0{k}.json').write_text(json.dumps(dict(run='run1', leg=k, unit=uid, state='DONE', finished_at=at(ago), report=f'RESULT: leg {k}')), encoding='utf-8')
    (board / 'relay' / 'desktop' / 'stops' / 'run1.json').write_text(json.dumps(dict(run='run1', stopped_at=at(3 * 3600), units={'tried-and-failed': 'FAIL'})), encoding='utf-8')
    env = dict(os.environ, GIT_AUTHOR_NAME='t', GIT_AUTHOR_EMAIL='t@t', GIT_COMMITTER_NAME='t', GIT_COMMITTER_EMAIL='t@t')
    for a in (['init', '-q'], ['add', '-A'], ['commit', '-q', '-m', 'the relay'], ['update-ref', 'refs/remotes/origin/main', 'HEAD']):
        subprocess.run(['git', '-C', str(board), *a], capture_output=True, env=env)
    shutil.rmtree(board / 'relay')                  # the checkout has none of it: it is read from the commit
    cache = {}
    rel = tasks.relay(board, cache, NOW, fetch=False)
    rows = src_tasks.collect([], shared=src_tasks.relay_rows(rel, NOW), now=NOW)['rows']
    case('tasks: the relay\'s queue is read from the board\'s origin/main without a checkout; a unit it ran legs of and did not finish is listed with its last leg and its verdict, '
         'not one that is done and not one still waiting its turn',
         sorted(rel['queue']) == ['finished', 'never-run', 'tried-and-failed'] and rel['done'] == ['finished'] and len(rel['legs']) == 3
         and [(r['id'], r['legs'], r['verdict'], r['report']) for r in rows] == [('relay-tried-and-failed', 2, 'FAIL', 'RESULT: leg 1')], (rel, rows))
    case('tasks: the board is read again only when its origin/main moved', tasks.relay(board, cache, NOW, fetch=False) is cache['relay']['data'] and tasks.relay(TMP / 'no-board', {}, NOW)['queue'] == {}, None)

    when = lambda ago: time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(NOW - ago))
    got = [r['id'] for r in src_tasks.collect([], shared=src_tasks.ready_rows([dict(item='house5', stage='clips', title='The house', since=when(91 * MIN)), dict(item='house5', stage='sim', since=when(60 * MIN)),
                                                                                dict(item='house5', stage='look', since='')], NOW), now=NOW)['rows']]
    case('tasks: a pipeline step is listed once it has been ready for an hour and a half', got == ['ready-house5-clips'], got)
    return board


def png(path: Path):
    path.parent.mkdir(parents=True, exist_ok=True)
    try:
        from PIL import Image
        Image.new('RGB', (320, 180), (40, 60, 90)).save(path, 'PNG')
        return True
    except Exception:       # noqa: BLE001  no Pillow here: the bytes still copy and compare
        path.write_bytes(b'\x89PNG\r\n\x1a\n' + b'0' * 64)
        return False


def capture(name, note='The tank drove through the wire.', age=60, shot=True, **change):
    folder = feedback.inbox() / name
    folder.mkdir(parents=True, exist_ok=True)
    c = json.loads((HERE / 'feedback.example.json').read_text(encoding='utf-8'))
    c.update(id=name, note=note, when_local=time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(NOW - age)), **change)
    (folder / 'capture.json').write_text(json.dumps(c), encoding='utf-8')
    os.utime(folder / 'capture.json', (NOW - age, NOW - age))
    if shot:
        png(folder / 'shot.png')
        os.utime(folder / 'shot.png', (NOW - age, NOW - age))
    return folder


def captures():
    ex = json.loads((HERE / 'feedback.example.json').read_text(encoding='utf-8'))
    case('capture: the example the game\'s tests and these read is a capture: its schema, an id, and every part a row is written from',
         ex.get('schema') == feedback.SCHEMA and all(k in ex for k in ('id', 'when_local', 'note', 'shot', 'scene', 'in_match', 'build', 'match', 'view', 'perf', 'machine'))
         and all(k in ex['match'] for k in ('tick', 'silver', 'units', 'request', 'report', 'holds_before')) and 'project' in ex['build'] and 'editor' in ex['build'], sorted(ex))
    done = capture('2026-10-07-200000')
    capture('2026-10-07-200100', age=2)                                  # the game is still writing it
    capture('2026-10-07-200200', age=20, shot=False)                     # its picture may still come
    capture('2026-10-07-200300', age=40, shot=False, note='')            # a game with no screen: none will
    capture('2026-10-07-200400', schema='something-else/9')
    (feedback.inbox() / 'not-a-capture').mkdir()
    before = {f.name: f.read_bytes() for f in done.iterdir()}
    took = feedback.take_in(host='HERE', now=NOW, checkout=lambda p: dict(branch='lane/show/x', commit='abc12345', dirty=2))
    kept = feedback.store() / '2026-10-07-200000-HERE'
    case('capture: a finished capture is moved to the tasks\' folder under its id and the station\'s name, every file as the game wrote it, and the game\'s copy goes only then; '
         'one still being written, one whose picture may yet come, one with another schema and a folder that is no capture stay where they are',
         took == ['2026-10-07-200000-HERE', '2026-10-07-200300-HERE'] and all((kept / n).read_bytes() == b for n, b in before.items()) and not done.exists()
         and sorted(p.name for p in feedback.inbox().iterdir()) == ['2026-10-07-200100', '2026-10-07-200200', '2026-10-07-200400', 'not-a-capture'], (took, sorted(p.name for p in feedback.inbox().iterdir())))
    meta = json.loads((kept / 'board.json').read_text(encoding='utf-8'))
    case('capture: taken in, it says which station it came from and which branch and commit the game ran from',
         meta.get('host') == 'HERE' and meta.get('branch') == 'lane/show/x' and meta.get('commit') == 'abc12345' and meta.get('dirty') == 2, meta)
    bad = capture('2026-10-07-200500')
    keep_copy, feedback.shutil.copyfile = feedback.shutil.copyfile, lambda a, b: Path(b).write_bytes(b'not what the game wrote')
    try:
        took = feedback.take_in(host='HERE', now=NOW, checkout=lambda p: {})
    finally:
        feedback.shutil.copyfile = keep_copy
    case('capture: a copy that does not compare with what the game wrote is not a capture taken in: the game\'s copy stays, to be tried again',
         took == [] and (bad / 'capture.json').exists() and (bad / 'shot.png').exists(), took)
    feedback.take_in(host='HERE', now=NOW, checkout=lambda p: {})
    every = feedback.read_all()
    one = next(c for c in every if c['id'] == '2026-10-07-200000-HERE')
    case('capture: read back, a capture has his words, its picture and five lines at most that say where in the match it was and which game it was',
         [c['id'] for c in every] == ['2026-10-07-200500-HERE', '2026-10-07-200300-HERE', '2026-10-07-200000-HERE'] and one['note'] == 'The tank drove through the wire.'
         and one['shot'].endswith('shot.png') and len(one['lines']) <= 5 and 'THE SHELLED FOREST, 03:00 into the match (tick 5400), match seed 12648430, ground seed 1917' in one['lines'][0]
         and '98 of ours and 114 of theirs' in one['lines'][1] and 'the cursor on the ground at 46, 68; 2 selected; 1 error in the console' in one['lines'][2], one['lines'])
    case('capture: the commit a row names is the one the game wrote at the press; when the checkout had moved on by the time the board took the capture in, the row says both',
         'lane/show/feedback-capture at 55b5d13b with 2 files changed (the checkout had moved to abc12345 when the board took the capture in)' in one['lines'][-1]
         and 'lane/show/x at abc12345.' in feedback.lines(dict(ex, build=dict(ex['build'], branch='', commit='')), dict(host='H', branch='lane/show/x', commit='abc12345'))[-1]
         and 'moved' not in feedback.lines(ex, dict(host='H', branch='lane/show/feedback-capture', commit='55b5d13b'))[-1], one['lines'][-1])
    built = feedback.lines(dict(ex, build=dict(version='0.9', editor=False, build_info=json.dumps(dict(commit='0123456789abcdef', branch='lane/show/b', dirty=False)))), dict(host='H'))[-1]
    case('capture: one from a build says which commit the build was made from, as the build\'s own file has it', 'a build, version 0.9, built from lane/show/b at 01234567.' in built
         and feedback.lines(dict(ex, build=dict(version='0.9', editor=False, build_info='not json')), dict(host='H'))[-1].endswith('a build, version 0.9.'), built)
    rows = src_tasks.collect([], shared=src_tasks.capture_rows(every), now=NOW)['rows']
    case('capture: a capture is a task from its first minute, the newest first, his words its title, and one with no words says so',
         [r['id'] for r in rows] == ['capture-2026-10-07-200300-HERE', 'capture-2026-10-07-200500-HERE', 'capture-2026-10-07-200000-HERE'] and rows[1]['title'] == 'The tank drove through the wire.'
         and rows[0]['title'] == 'No words: THE SHELLED FOREST, 03:00 in' and all(r['idle'] <= 1 for r in rows), [(r['id'], r['title'], r['idle']) for r in rows])
    menu = feedback.lines(dict(ex, in_match=False, scene='MainMenu'), dict(host='HERE'))
    case('capture: one taken on a menu says no match was running', menu[0] == 'On a menu (MainMenu): no match was running.' and len(menu) == 2, menu)
    # his box for words is open: the capture waits for them (folders of its own, so the cases after this count what they did)
    src, dst = TMP / 'inbox-open', TMP / 'store-open'
    for name, age_min in (('open-now', 2), ('open-forgotten', 45)):
        f = src / name
        f.mkdir(parents=True)
        (f / 'capture.json').write_text(json.dumps(dict(ex, id=name)), encoding='utf-8')
        png(f / 'shot.png')
        (f / feedback.OPEN).write_text('', encoding='utf-8')
        for g in f.iterdir():
            os.utime(g, (NOW - age_min * MIN, NOW - age_min * MIN))
    took = feedback.take_in(src, dst, host='HERE', now=NOW, checkout=lambda p: {})
    case('capture: while his box for words is open the capture waits for them; one whose game ended with the box open is taken in after half an hour, and the marker is not part of it',
         took == ['open-forgotten-HERE'] and (src / 'open-now' / 'capture.json').exists() and not (src / 'open-forgotten').exists()
         and sorted(p.name for p in (dst / 'open-forgotten-HERE').iterdir()) == ['board.json', 'capture.json', 'shot.png'], (took, sorted(p.name for p in src.iterdir())))


def his_word(P, board):
    """What he says about a task: queued becomes a unit file, once; dropped leaves the list; his own words go with it."""
    where, units = notes.folder(), src_tasks.root() / 'units-for-master'
    day = datetime.datetime.fromtimestamp(NOW)

    def read(live=True, host='HERE'):
        return tasks.read(every_note=notes.read_all(where), now=NOW, cache_path=TMP / f'cache-{host}.json', projects=P, board=board, host=host, live=live, fetch=False)
    T = read()
    ids = [r['id'] for r in T['rows']]
    case('tasks: one reading lists the captures first, then the agents, the sessions, the handoffs, the unit files and the relay\'s; it counts what is left and how long the relay\'s queue is',
         [r['kind'] for r in T['rows']] == sorted((r['kind'] for r in T['rows']), key=src_tasks.KINDS.index) and {'capture', 'agent', 'session', 'handoff', 'unit', 'relay'} <= {r['kind'] for r in T['rows']}
         and T['left'] + T['captures'] + T['paused'] == len(ids) and T['queued'] == 0 and T['relay']['waiting'] == 2 and T['captures'] == 3 and T['paused'] == 1, (ids, T['relay'], T['left']))
    say = lambda tid, text, **k: notes.write(where, text, kind='queue', about='task: ' + tid, title=tid, now=k.pop('now', day), **k)
    q = say('agent-cutlong', src_tasks.QUEUE_SAY)
    say('agent-cutlong', 'Mind the wire test while you are at it.', now=day - datetime.timedelta(minutes=5))
    say('session-cutsess1', src_tasks.DROP_SAY)
    say('capture-2026-10-07-200000-HERE', src_tasks.QUEUE_SAY)
    say('unit-never-queued', src_tasks.QUEUE_SAY)
    back = say('session-asksess1', src_tasks.QUEUE_SAY)
    notes.answer(where, back['id'], 'Closed by the owner.', by='owner', now=day)
    notes.write(where, 'Done on the lane: the wire breaks now.', kind='queue', about='task: handoff-waiting', who='lane/show/wire', now=day)
    T = read()
    by = {r['id']: r for r in T['rows']}
    case('tasks: what he says about a task is a note of his about it: queued shows as queued with his own words kept, dropped leaves the list, one an agent closed leaves it too, '
         'and a click he took back before it was taken up leaves the task as it was',
         by['agent-cutlong']['state'] == 'queued' and by['agent-cutlong']['words'] == ['Mind the wire test while you are at it.'] and 'session-cutsess1' not in by and 'handoff-waiting' not in by
         and by['session-asksess1']['state'] == 'left' and T['dropped'] == 1 and T['done'] == 1 and T['queued'] == 3, {k: v['state'] for k, v in by.items()})
    before = sorted(f.name for f in units.glob('*.json'))
    did = tasks.act(T, where, units, now=day)
    made = sorted(set(f.name for f in units.glob('*.json')) - set(before))
    u = json.loads((units / 'task-agent-cutlong.json').read_text(encoding='utf-8')) if (units / 'task-agent-cutlong.json').exists() else {}
    case('tasks: a task he queued is written as a unit of the relay\'s queue, named after it: the five keys and no other, a lane the relay takes, a goal that holds what was asked and what he '
         'said, and the relay\'s own check of done; a task that is a unit already is not written twice',
         made == ['task-agent-cutlong.json', 'task-capture-2026-10-07-200000-HERE.json'] and sorted(u) == ['done_when', 'goal', 'id', 'lane', 'role'] and u['id'] == 'task-agent-cutlong'
         and u['lane'] == 'lane/show/task-agent-cutlong' and briefs.check_then('queued', u) == [] and 'Review the sim' in u['goal'] and 'Mind the wire test' in u['goal']
         and '[task-agent-cutlong]' in u['done_when'][2] and u['done_when'][:2] == ['python', '-c'], (made, u, did))
    cap = json.loads((units / 'task-capture-2026-10-07-200000-HERE.json').read_text(encoding='utf-8'))
    case('tasks: the unit of a capture tells the leg where the picture and the state file are, on the folder both stations read, and what he wrote',
         str(feedback.store() / '2026-10-07-200000-HERE') in cap['goal'] and 'shot.png' in cap['goal'] and 'capture.json' in cap['goal'] and 'The tank drove through the wire.' in cap['goal'], cap['goal'])
    said = {n['id']: n for n in notes.read_all(where)}
    case('tasks: his note is answered with the unit\'s name, and the note that dropped a task is answered too, so neither stays open',
         said[q['id']]['state'] == 'done' and said[q['id']]['answers'][-1]['text'].startswith('Unit task-agent-cutlong: written') and said[q['id']]['answers'][-1]['by'] == tasks.BY
         and all(n['state'] == 'done' for n in said.values() if n['text'] in (src_tasks.QUEUE_SAY, src_tasks.DROP_SAY))
         and next(n for n in said.values() if n['about'] == 'task: unit-never-queued' and n['text'] == src_tasks.QUEUE_SAY)['answers'][-1]['text'].startswith('Unit never-queued: it is a unit file already'), did)
    stamp = (units / 'task-agent-cutlong.json').stat().st_mtime_ns
    T = read()
    again = tasks.act(T, where, units, now=day)
    by = {r['id']: r for r in T['rows']}
    case('tasks: taken up once: the next reading knows the unit by name, says so on the row, and writes and answers nothing again',
         again == [] and (units / 'task-agent-cutlong.json').stat().st_mtime_ns == stamp and by['agent-cutlong']['unit'] == 'task-agent-cutlong' and by['agent-cutlong']['state'] == 'queued'
         and any('unit task-agent-cutlong' in l for l in by['agent-cutlong']['detail']) and 'unit-task-agent-cutlong' not in by, (again, by['agent-cutlong']))

    # two stations: each writes its own rows where the other reads them
    theirs = json.loads((src_tasks.root() / 'tasks' / 'HERE.json').read_text(encoding='utf-8'))
    other = tasks.read(every_note=[], now=NOW, cache_path=TMP / 'cache-THERE.json', projects=TMP / 'no-projects', board=None, host='THERE', live=True, fetch=False)
    from_here = [r for r in other['rows'] if r.get('where') == 'HERE']
    case('tasks: a station writes its own agents and sessions where the other reads them, so both boards list both, each row with the station it is on',
         theirs['host'] == 'HERE' and {r['kind'] for r in theirs['rows']} == {'agent', 'session', 'paused'} and 'agent-cutlong' in [r['id'] for r in from_here]
         and sorted(s['host'] for s in other['stations']) == ['HERE', 'THERE'] and (src_tasks.root() / 'tasks' / 'THERE.json').exists(), [r['id'] for r in from_here])

    # a session that only reads changes nothing
    waiting = capture('2026-10-07-210000')
    (src_tasks.root() / 'tasks' / 'HERE.json').unlink()
    read(live=False)
    case('tasks: a session that reads the list takes no capture in and writes no station file: only the watcher does',
         waiting.exists() and not (src_tasks.root() / 'tasks' / 'HERE.json').exists() and not (TMP / 'cache-HERE.json.tmp').exists(), None)
    return read()


def site(T):
    out = TMP / 'site'
    tasks.site(T, out)
    text = (out / 'data' / 'tasks.js').read_text(encoding='utf-8')
    data = json.loads(text[len('window.TASKS = '):].rstrip().rstrip(';'))
    cap = next(r for r in data['rows'] if r['id'] == 'capture-2026-10-07-200000-HERE')
    try:
        import PIL  # noqa: F401
        shown = bool(cap['shots']) and (out / cap['shots'][0]['src']).is_file() and cap['shots'][0]['src'].startswith('img/task/')
    except ImportError:
        shown = True
        print('      (no Pillow on this machine: the capture\'s picture was not put in the site)')
    case('tasks: the site gets the tasks as a script, a capture with its picture beside the page, and no row carries the long text a unit is written from or a path of this machine',
         text.startswith('window.TASKS = ') and shown and all(not {'ask', 'picture', 'log', 'parent_log', 'cwd', 'turns', 'first'} & set(r) for r in data['rows']) and data['stale_minutes'] == 90
         and 'taken' not in data['relay'] and data['read_at'] == int(NOW) and str(TMP) not in text.replace('\\\\', '\\'), cap)
    tasks.failed(out, 'RuntimeError: the Drive hung', now=NOW + 60)
    after = json.loads((out / 'data' / 'tasks.js').read_text(encoding='utf-8')[len('window.TASKS = '):].rstrip().rstrip(';'))
    case('tasks: a reading that failed leaves the rows of the one before and says on the page that they are old, when it failed and why',
         after['failed'] == dict(at=int(NOW + 60), why='RuntimeError: the Drive hung') and [r['id'] for r in after['rows']] == [r['id'] for r in data['rows']], after.get('failed'))
    tasks.site(T, out)
    case('tasks: the next reading that works takes the mark away', '"failed": {' not in (out / 'data' / 'tasks.js').read_text(encoding='utf-8'), None)
    case('tasks: a session reads the same list as text', tasks.lines(T)[0].startswith(f'{T["left"]} left unfinished') and any('QUEUED' in l for l in tasks.lines(T)), tasks.lines(T)[:3])
    try:
        import jinja2  # noqa: F401
    except ImportError:
        print('      (no jinja2 on this machine: the tasks page was not written)')
        return
    keep_crew, ops.CREW = ops.CREW, TMP / 'nothing'
    ops.page(out, dict(built='then', station='here', commit='abc', refs_as_of='then'))
    ops.CREW = keep_crew
    html = (out / 'tasks.html').read_text(encoding='utf-8') if (out / 'tasks.html').exists() else ''
    loads = re.findall(r'(?:src|href)="([^"#:]+\.(?:js|css))"', html)
    readings = {'data/ops.js', 'data/queue.js', 'data/beat.js', 'data/notes.js', 'data/tasks.js'}
    case('tasks: the tasks page is written with every script and style it loads, has a place for the rows and their count, and every page\'s top bar leads to it',
         {'tasks.js', 'taskboard.js', 'tasks.css', 'board.js', 'crew.js', 'data/tasks.js'} <= set(loads) and not [u for u in loads if u not in readings and not (out / u).exists()]
         and all(f'id="{i}"' in html for i in ('taskboard', 't-board', 't-count', 't-foot')) and 'href="tasks.html" aria-current="page"' in html
         and 'href="tasks.html"' in (out / 'decide.html').read_text(encoding='utf-8'), [u for u in loads if u not in readings and not (out / u).exists()])
    order = [loads.index(u) for u in ('crew.js', 'tasks.js', 'taskboard.js')]
    case('tasks: the page loads its scripts each after what it needs', order == sorted(order), loads)


def page_rules():
    node = shutil.which('node')
    if not node:
        print('      (no node on this machine: the tasks page\'s own cases were not run)')
        return
    js = ('const T = require(process.argv[1]);'
          'const R = [{id: "a", kind: "agent", title: "Review", what: "Review the sim", state: "left", idle: 91, where: "MSI", agent: "general-purpose", stopped: "2026-10-07 01:42", detail: ["An agent."], words: ["x"]},'
          ' {id: "c", kind: "capture", title: "The wire", state: "left", idle: 3, stopped: "2026-10-07 21:14", shots: [{src: "img/task/c.jpg"}], detail: ["In the forest."]},'
          ' {id: "u", kind: "unit", title: "rv-1", what: "Fix it", state: "queued", idle: 4000, lane: "lane/sim/x"}, {id: "h", kind: "handoff", title: "Landing", state: "left", idle: 130},'
          ' {id: "r", kind: "session", title: "The wire", state: "relay", idle: 200, legs: 2, verdict: "FAIL", unit: "task-r"}, {id: "p", kind: "paused", title: "Shader", state: "left", idle: 300}];'
          'const D = {rows: R, relay: {waiting: 96, as_of: "2026-10-07 01:26"}, stations: [{host: "MSI", at: 1000}, {host: "DESK", at: 1000 - 3 * 3600}], stale_minutes: 90, read_at: 1000, blind: "It cannot see X."};'
          'const said = r => r.id === "a" ? [T.QUEUE] : r.id === "h" ? [T.DROP] : r.id === "u" ? [T.DROP] : [];'
          'const G = T.groups(D), S = T.groups(D, said);'
          'console.log(JSON.stringify([G.map(g => [g.key, g.rows.map(r => r.id)]), S.map(g => [g.key, g.rows.map(r => r.id + ":" + r.state)]), T.head(D, G), T.head(D, S), T.head(null, []), T.head({rows: []}, []),'
          ' G[0].rows[0], G[1].rows[0].chips, G.map(g => g.rows.map(r => r.acts.map(a => a.label))), T.subject(R[0], "x", [T.QUEUE]).actions.map(a => a.label), T.subject(R[4], "x", []).actions,'
          ' [T.subject(R[0], "Agents", []).id, T.subject(R[0], "Agents", []).kind, T.subject(R[0], "x", []).lane],'
          ' [T.span(0), T.span(45), T.span(119), T.span(120), T.span(2879), T.span(2880)], T.heard(D, 1060), [T.state(R[2], [T.DROP]), T.state(R[0], [T.DROP, T.QUEUE]), T.state(R[0], ["other words"]), T.state(R[4], [T.DROP])],'
          ' G.filter(g => g.key === "session")[0].rows[0].chips, G.filter(g => g.folded).map(g => g.key), G[0].note.length > 0,'
          ' T.warnings(D, 1060), T.warnings(Object.assign({}, D, {failed: {at: 1000, why: "boom"}, drive_away: true, waiting_here: 2, stations: []}), 1060).map(w => w.replace(/at \\d\\d:\\d\\d/, "at HH:MM")),'
          ' T.warnings(Object.assign({}, D, {stations: []}), 1000 + 6 * 60), T.warnings(null, 5),'
          ' T.firstOf([{key: "capture", rows: [1, 2, 3, 4, 5, 6, 7]}, {key: "agent", rows: [1, 2]}, {key: "handoff", rows: [1]}, {key: "paused", folded: true, rows: [1]}], 6).map(x => x[0].key + x[1]),'
          ' T.foot(D, 1060)]))')
    p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'tasks.js')], capture_output=True)
    got = json.loads(p.stdout.decode() or 'null')
    case('tasks page: the rows are grouped by what they are, the captures first and his own stops last; a task he drops is gone from his click on, one he queues says so, and a queued one he '
         'drops is taken back, before the next reading does',
         got and got[0] == [['capture', ['c']], ['agent', ['a']], ['session', ['r']], ['handoff', ['h']], ['unit', ['u']], ['paused', ['p']]]
         and got[1] == [['capture', ['c:left']], ['agent', ['a:queued']], ['session', ['r:relay']], ['paused', ['p:left']]], (got and got[:2], p.stderr[-400:]))
    case('tasks page: the line under the title counts his captures apart from what waits for an agent, then what he queued, what the relay has, and how long the relay\'s queue is and how old '
         'that figure is; his own stops are counted in neither',
         got and got[2:6] == ['1 capture of yours waits for your word · 2 tasks wait for an agent · 1 queued for the relay · 1 with the relay · the relay\'s queue holds 96 units (its board last changed 2026-10-07 01:26)',
                              '1 capture of yours waits for your word · nothing waits for an agent · 1 queued for the relay · 1 with the relay · the relay\'s queue holds 96 units (its board last changed 2026-10-07 01:26)',
                              'The tasks have not been read yet.', 'Nothing is left unfinished'], got and got[2:6])
    case('tasks page: a capture\'s row has its picture, when it was taken and where in the match; another\'s says how long nobody has been on it, when it stopped, on which station, and what kind of agent it was',
         got and got[6] == dict(id='c', top='The wire', sub='In the forest.', part='', stands='', read=False, shot='img/task/c.jpg', film=False, links=[], chips=['captured 3 min ago', '2026-10-07 21:14'], state='left', tip='In the forest.',
                                acts=[dict(label='Queue it for the relay', say='Queue this for the relay.'), dict(label='Not needed', say='Not needed: drop this task.')])
         and got[7] == ['nobody on it for 91 min', 'stopped 2026-10-07 01:42', 'on MSI', 'general agent', '1 note of yours'], got and got[6:8])
    case('tasks page: every row carries what he can say about it: both while nothing was said, the way back while it is only queued, nothing once the relay has it; the panel offers the same',
         got and got[8] == [[['Queue it for the relay', 'Not needed']], [['Queue it for the relay', 'Not needed']], [[]], [['Queue it for the relay', 'Not needed']], [['Take it back']], [['Queue it for the relay', 'Not needed']]]
         and got[9] == ['Take it back'] and got[10] == [] and got[11] == ['task: a', 'queue', None], got and got[8:12])
    case('tasks page: how long is said in minutes, then hours, then days; each station says when it was last heard from; of his two set phrases the drop wins, other words change nothing, '
         'and a drop does not take back what the relay already has',
         got and got[12] == ['under a minute', '45 min', '119 min', '2 h', '48 h', '2 days'] and got[13] == 'MSI just now, DESK 3 h ago' and got[14] == ['dropped', 'dropped', 'left', 'relay'], got and got[12:15])
    case('tasks page: a task the relay has says so first, with its legs and what the relay\'s check said, in words; his own stops are folded; the captures\' group says they wait for him',
         got and got[15] == ['with the relay, 2 legs run', 'nobody on it for 3 h', 'its check failed'] and got[16] == ['paused'] and got[17] is True, got and got[15:18])
    case('tasks page: what is wrong with the reading is said above the rows: a station not heard from, a reading that failed, the Drive away with the captures that wait, a list nobody has read '
         'for five minutes; and nothing when all is well',
         got and got[18] == ['DESK was last read 3 h ago: its agents and sessions may be missing here.']
         and got[19] == ['The tasks could not be read at HH:MM (boom). What is below is from the reading before.',
                         'The shared Drive is away on this station. 2 captures wait in the game\'s folder. Nothing is taken in, and nothing you say here is taken up while it is away.']
         and got[20] == ['This list was read 6 min ago and not since: the board\'s watcher may have stopped.'] and got[21] == [], got and got[18:22])
    case('tasks page: the control screen\'s first rows are one of each group in turn, so the captures do not crowd the rest out, and never his own stops; the foot says what the board cannot see',
         got and got[22] == ['capture0', 'agent0', 'handoff0', 'capture1', 'agent1', 'capture2'] and got[23].endswith('Read on MSI just now, DESK 3 h ago. It cannot see X.'), got and got[22:])


def back(aid, ago, status=None):
    """What a session's transcript keeps when an agent comes back: its report handed back, or a notification."""
    if status is None:
        text = f'<agent-message from="{aid}">\n[Subagent hand-back] The text below is the final report of a subagent this session delegated to.\n  Here is the report.\n</agent-message>'
    else:
        text = f'<task-notification>\n<task-id>{aid}</task-id>\n<tool-use-id>toolu_1</tool-use-id>\n<output-file>C:\\x\\{aid}.output</output-file>\n<status>{status}</status>\n<summary>Agent stopped</summary>\n</task-notification>'
    return dict(type='queue-operation', operation='enqueue', timestamp=at(ago), content=text)


def delivered():
    """A1: an agent whose log ends on a failed call is lost only if its report never reached its session."""
    P = TMP / 'projects-back'
    parent = [asked('review the relay', side=False), back('handed01', 5990), back('handed01', 5989, 'failed'), back('failedto', 5990, 'failed'), back('complete', 5990, 'completed'),
              dict(type='user', isSidechain=False, timestamp=at(5990), toolUseResult=dict(agentId='waitedfor', status='completed', content='the report'), message=dict(content=[dict(type='tool_result', tool_use_id='t9', content='the report')])),
              back('backthen', 20000), back('stillout', 5990, 'failed'), ended(ago=5000, side=False)]
    log(P / 'p' / 'parent01.jsonl', parent, 300)
    for name in ('handed01', 'failedto', 'complete', 'waitedfor', 'silent01', 'backthen'):
        agent(P, 'parent01', name, [asked('Review the sim'), failed(ago=6000)], 300, desc='Review ' + name)
    # handed its report back, was asked for another round five seconds later and died on it at once: the round is lost
    parent.insert(1, back('nextround', 6010))
    log(P / 'p' / 'parent01.jsonl', parent, 300)
    agent(P, 'parent01', 'nextround', [asked('Review the sim', ago=9000), ended(ago=6011), asked('Now the second round', ago=6005), failed(ago=6004)], 300, desc='Review nextround')
    # the same failure, told to a session that never spoke again: nobody was there to go on without it
    log(P / 'p' / 'parent02.jsonl', [asked('review the relay', side=False), ended(ago=7000, side=False), back('stillout', 5990, 'failed')], 300)
    agent(P, 'parent02', 'stillout', [asked('Review the sim'), failed(ago=6000)], 300, desc='Review stillout')
    cache = {}
    mine = src_tasks.local(P, NOW, cache, 'HERE')
    rows = {r['id']: r for r in src_tasks.collect(mine, now=NOW)['rows']}
    case('tasks: an agent whose log ends on a failed call is not a task when its report had reached its session: handed back, told as completed, or the result of an agent the session waited for. '
         'It is one when nothing came back, when only the failure did, and when the report that came back was an earlier run\'s',
         sorted(rows) == ['agent-backthen', 'agent-failedto', 'agent-nextround', 'agent-silent01', 'agent-stillout'], sorted(rows))
    case('tasks: a report counts only when it came after the agent\'s last request: one that handed back, was asked again seconds later and failed at once is a lost round',
         'agent-nextround' in rows and src_tasks.read_agent(P / 'p' / 'parent01' / 'subagents' / 'agent-nextround.jsonl')['asked'] == src_tasks.stamp(at(6005)), sorted(rows))
    log(P / 'p' / 'parent03.jsonl', [asked('review the relay', side=False), back('freshone', 590), ended(ago=500, side=False)], 10)
    agent(P, 'parent03', 'freshone', [asked('Review the sim', ago=900), failed(ago=600)], 10, desc='Review freshone')
    young = [r['id'] for r in src_tasks.local(P, NOW, {}, 'HERE')]
    case('tasks: the check is made before a row is written at all, not when it has aged: the other station lists what this one writes, and a finished agent must not reach it',
         'agent-freshone' not in young and 'agent-silent01' in young, young)
    told = rows.get('agent-failedto', {})
    case('tasks: an agent that failed, whose session was told and went on, says so, and its unit tells the leg to check first that the task was not done another way',
         src_tasks.TOLD in told.get('why', '') and src_tasks.TOLD in ' '.join(told.get('detail', [])) and 'Check that first' in src_tasks.unit_for(told)['goal']
         and src_tasks.TOLD not in rows['agent-silent01']['why'] and src_tasks.TOLD not in rows['agent-stillout']['why'] and 'Do the task.' in src_tasks.unit_for(rows['agent-silent01'])['goal'], told)
    again = src_tasks.local(P, NOW, cache, 'HERE')
    f = P / 'p' / 'parent01.jsonl'
    case('tasks: a session\'s transcript is read for what came back once, and after that only what it gained', len(cache['heard']) == 3 and len(again) == len(mine)
         and cache['heard'][str(f)]['read'] == f.stat().st_size and src_tasks.heard_of(f)['handed01'].keys() == {'back', 'told'}, cache.get('heard'))
    with open(f, 'ab') as h:
        h.write((json.dumps(back('silent01', 5000)) + '\n').encode())
    os.utime(f, (NOW - 300 * MIN, NOW - 300 * MIN))
    later = [r['id'] for r in src_tasks.local(P, NOW, cache, 'HERE')]
    case('tasks: a report that reaches the session later takes the agent off the list', 'agent-silent01' not in later and 'agent-failedto' in later, later)


def long_lines():
    """A6: a last line longer than the tail is still the last line."""
    P = TMP / 'projects-long'
    big = 'x' * (src_tasks.TAIL + 50000)
    f = log(P / 'p' / 'longsess.jsonl', [asked('fix the wire', side=False), ended(side=False), asked('and the gas', ago=5000, side=False), calling('Write', dict(file_path='a.txt', content=big), ago=4000, side=False)], 100)
    g = agent(P, 'longsess', 'longtail', [asked('Review the sim'), ended(), asked('again'), calling('Write', dict(file_path='a.txt', content=big))], 100)
    case('tasks: a log whose last line is longer than the part read from its end is read further back: its open tool call is a cut, not a quiet end',
         src_tasks.read_session(f)['end'] == 'cut' and src_tasks.read_agent(g)['end'] == 'cut' and src_tasks.ending(src_tasks.entries(src_tasks.tail(f)))[0] == 'done',
         (src_tasks.read_session(f)['end'], src_tasks.read_agent(g)['end']))


def in_hand():
    """A7: a handoff a session opened hours ago and is still working from is in its hands. A6: a file the index does not know."""
    P, where = TMP / 'projects-hand', TMP / 'root-hand'
    where.mkdir(parents=True)
    (where / 'handoffs.json').write_text(json.dumps(dict(handoffs={'HANDOFF_AGENT_long_haul.md': dict(topic='the long haul', state='current', **{'for': 'Fix the board.'}),
                                                                   'HANDOFF_AGENT_old.md': dict(topic='replaced', state='replaced', **{'for': 'Nothing.'})})), encoding='utf-8')
    for n in ('long_haul', 'old', 'stray'):
        (where / f'HANDOFF_AGENT_{n}.md').write_text('# handoff\n', encoding='utf-8')
        os.utime(where / f'HANDOFF_AGENT_{n}.md', (NOW - 600 * MIN, NOW - 600 * MIN))
    filler = [result('y' * 4000, ago=20000 - k, side=False) for k in range(90)]           # hours of work after the handoff was opened: 360 KB
    f = log(P / 'p' / 'worker01.jsonl', [asked('continue from the handoff', side=False), calling('Read', dict(file_path=str(where / 'HANDOFF_AGENT_long_haul.md')), ago=30000, side=False)] + filler, 5)
    # the session that wrote a handoff, listed the folder or told an agent about it has not opened it
    (where / 'handoffs.json').write_text(json.dumps(dict(handoffs={'HANDOFF_AGENT_long_haul.md': dict(topic='the long haul', state='current', **{'for': 'Fix the board.'}),
                                                                   'HANDOFF_AGENT_written.md': dict(topic='just written', state='current', **{'for': 'Carry on.'}),
                                                                   'HANDOFF_AGENT_old.md': dict(topic='replaced', state='replaced', **{'for': 'Nothing.'})})), encoding='utf-8')
    (where / 'HANDOFF_AGENT_written.md').write_text('# handoff\n', encoding='utf-8')
    os.utime(where / 'HANDOFF_AGENT_written.md', (NOW - 600 * MIN, NOW - 600 * MIN))
    log(P / 'p' / 'writer01.jsonl', [asked('write the handoff', side=False), calling('Write', dict(file_path=str(where / 'HANDOFF_AGENT_written.md'), content='# handoff'), side=False),
                                     calling('Bash', dict(command='ls HANDOFF_AGENT_written.md'), side=False), calling('Agent', dict(prompt='Read HANDOFF_AGENT_written.md and carry on'), side=False)], 5)
    cache = {}
    got = [(r['id'], bool(r.get('fault'))) for r in src_tasks.collect([], shared=src_tasks.handoffs(where, P, NOW, cache), now=NOW)['rows']]
    case('tasks: a handoff a session opened at its start and is still working from, hours and many lines later, is in that session\'s hands and is not listed; '
         'a handoff file the index does not know is listed as a fault, and one the index knows as replaced is not',
         got == [('handoff-stray', True), ('handoff-written', False)] and f.stat().st_size > src_tasks.TAIL and 'HANDOFF_AGENT_long_haul.md' not in json.dumps(src_tasks.entries(src_tasks.tail(f))), got)
    read = cache['named'][str(f)]['read']
    with open(f, 'ab') as h:
        h.write((json.dumps(calling('Read', dict(file_path='HANDOFF_AGENT_stray.md'), side=False)) + '\n').encode())
        h.write(b'{"type": "assistant", "half a line')
    os.utime(f, (NOW - 3 * MIN, NOW - 3 * MIN))
    got = [r['id'] for r in src_tasks.collect([], shared=src_tasks.handoffs(where, P, NOW, cache), now=NOW)['rows']]
    case('tasks: only what a log gained since the last reading is read, and a line still being written waits for the next',
         got == ['handoff-written'] and read == f.stat().st_size - len(json.dumps(calling('Read', dict(file_path='HANDOFF_AGENT_stray.md'), side=False))) - 1 - 34
         and cache['named'][str(f)]['read'] == f.stat().st_size - 34, (got, read, cache['named'][str(f)]['read'], f.stat().st_size))
    os.utime(f, (NOW - 200 * MIN, NOW - 200 * MIN))
    got = [r['id'] for r in src_tasks.collect([], shared=src_tasks.handoffs(where, P, NOW, cache), now=NOW)['rows']]
    case('tasks: when that session goes quiet for an hour and a half the handoff it held is listed; a handoff is held by the session that opened it, not by the one that wrote it, listed it '
         'or named it to an agent', got == ['handoff-long_haul', 'handoff-stray', 'handoff-written'] and list(cache['named']) == [str(P / 'p' / 'writer01.jsonl')], (got, list(cache['named'])))


def one_life():
    """A3, A4: a task is left, queued, with the relay, done, and shows once; a note is about the task as it was."""
    ts = lambda ago: time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(NOW - ago))
    task = lambda **k: dict(dict(id='agent-x', kind='agent', title='Review', ask='Review the sim', what='Review the sim', lane='', where='HERE', by='', agent='Explore', why='in the middle of a tool call',
                                 stopped='then', touched=int(NOW - 200 * MIN)), **k)
    note = lambda text, ago, who='owner', answers=(), state='open', tid='agent-x': {'id': f'n-{text[:6]}-{ago}', 'when': ts(ago), 'about': 'task: ' + tid, 'text': text, 'from': who, 'state': state,
                                                                                   'answers': [dict(text=a, when=ts(ago), by='the task board') for a in answers]}
    queued = note(src_tasks.QUEUE_SAY, 60 * MIN, answers=['Unit task-agent-x: written to units-for-master for the relay.'], state='done')
    legs = [dict(unit='task-agent-x', state='DONE', finished_at=at(4 * 3600), report='RESULT: half of it'), dict(unit='task-agent-x', state='DONE', finished_at=at(3 * 3600), report='RESULT: the check failed')]
    rel = lambda **k: dict(dict(queue={}, done=[], legs=[], stops=[]), **k)
    got = lambda relay, have, notes=(queued,), rows=None: src_tasks.collect(rows if rows is not None else [task()], shared=src_tasks.relay_rows(relay, NOW), notes=list(notes), now=NOW, relay=relay, have=have)
    a = got(rel(), {'task-agent-x'})
    with_it = rel(queue={'task-agent-x': dict(id='task-agent-x', lane='lane/show/task-agent-x', goal='Do the task.')}, legs=legs, stops=[dict(stopped_at=at(3 * 3600), units={'task-agent-x': 'FAIL'})])
    b = got(with_it, set())
    c = got(dict(with_it, done=['task-agent-x']), set())
    d = got(with_it, set(), rows=[])
    case('tasks: a queued task has one life and shows once: queued while its unit file waits for the master; with the relay once the unit is in its queue, with the legs run and the last verdict, '
         'and not a second time as the relay\'s own row; gone when the relay\'s record says done; and the relay\'s row alone when the task it came from is no longer there',
         [(r['id'], r['state']) for r in a['rows']] == [('agent-x', 'queued')] and a['queued'] == 1 and 'take it back' in a['rows'][0]['detail'][-1]
         and [(r['id'], r['state'], r.get('legs'), r.get('verdict')) for r in b['rows']] == [('agent-x', 'relay', 2, 'FAIL')] and b['with_relay'] == 1 and b['queued'] == 0 and '2 legs run' in b['rows'][0]['detail'][-1]
         and c['rows'] == [] and c['done'] == 1 and [r['id'] for r in d['rows']] == ['relay-task-agent-x'],
         [[(r['id'], r['state'], r.get('legs'), r.get('verdict'), r['detail'][-1]) for r in x['rows']] for x in (a, b, c, d)])
    gone = got(rel(), set())['rows'][0]
    case('tasks: a queued task whose unit file is gone, and which the relay never had, is queued with no unit, so the unit is written again', gone['state'] == 'queued' and gone['unit'] == '' and a['rows'][0]['unit'] == 'task-agent-x', gone)
    moved = [task(touched=int(NOW - 100 * MIN), changed=int(NOW - 200 * MIN))]         # its parent session moved after he spoke; the agent's own log did not
    stays = [got(rel(), set(), notes=[n], rows=moved) for n in (note(src_tasks.DROP_SAY, 150 * MIN), note('Done on the lane.', 150 * MIN, who='lane/show/x'))]
    case('tasks: a word about a task is judged against the task\'s own change, not against its parent session moving or somebody opening it: what he dropped stays dropped, what was closed stays closed',
         [x['rows'] for x in stays] == [[], []] and [x['dropped'] + x['done'] for x in stays] == [1, 1], [[r['state'] for r in x['rows']] for x in stays])
    fin = rel(done=['task-agent-x'], done_at={'task-agent-x': at(120 * MIN)})       # done and out of the queue: nothing keeps the unit alive but the record
    edited = got(fin, set(), rows=[task(touched=int(NOW - 125 * MIN), changed=int(NOW - 125 * MIN))], notes=[note(src_tasks.QUEUE_SAY, 300 * MIN, answers=['Unit task-agent-x: written'], state='done')])
    again = got(fin, set(), rows=[task(touched=int(NOW - 95 * MIN), changed=int(NOW - 95 * MIN))], notes=[note(src_tasks.QUEUE_SAY, 300 * MIN, answers=['Unit task-agent-x: written'], state='done')])
    case('tasks: a task the relay finished is done though the leg that did it changed it on the way; changed again after the relay was done, it is a new task, with no unit and both buttons',
         edited['rows'] == [] and edited['done'] == 1 and [(r['state'], r['unit'], r['note']) for r in again['rows']] == [('left', '', '')], ([r['state'] for r in edited['rows']], again['rows']))
    stale = got(rel(), set(), notes=[note(src_tasks.QUEUE_SAY, 300 * MIN), note(src_tasks.DROP_SAY, 300 * MIN), note(src_tasks.QUEUE_SAY, 300 * MIN, state='done')])['rows'][0]
    case('tasks: his open clicks that the task has outlived are named on the row, for the board to answer: the page would go on showing them as said',
         stale['state'] == 'left' and len(stale['stale']) == 2, stale.get('stale'))
    running = dict(queue={'u1': dict(id='u1', lane='lane/sim/x', goal='g'), 'u2': dict(id='u2', lane='lane/sim/x', goal='g'), 'u3': dict(id='u3', lane='lane/sim/x', goal='g')}, done=[], stops=[],
                   legs=[dict(unit='u1', state='RUNNING', started_at=at(100 * MIN)), dict(unit='u2', state='DONE', started_at=at(200 * MIN), finished_at=at(100 * MIN)), dict(unit='u3', state='RUNNING', started_at=at(7 * 3600))])
    case('tasks: a unit whose newest leg has not ended is the relay at work and is not listed as given up on; one that ended is, and so is a leg that never ended and started six hours ago',
         [r['id'] for r in src_tasks.collect([], shared=src_tasks.relay_rows(running, NOW), now=NOW)['rows']] == ['relay-u2', 'relay-u3'], [r['id'] for r in src_tasks.relay_rows(running, NOW)])
    old = 300 * MIN
    states = [got(rel(), set(), notes=[n])['rows'] for n in (note(src_tasks.DROP_SAY, old), note('Done on the lane.', old, who='lane/show/x'), note(src_tasks.QUEUE_SAY, old, answers=['Unit task-agent-x: written'], state='done'))]
    fresh = [got(rel(), set(), notes=[n]) for n in (note(src_tasks.DROP_SAY, 60 * MIN), note('Done on the lane.', 60 * MIN, who='lane/show/x'))]
    kept = got(rel(), {'task-agent-x'}, notes=[note(src_tasks.QUEUE_SAY, old, answers=['Unit task-agent-x: written'], state='done'), note('Mind the wire.', old)])['rows'][0]
    case('tasks: what was said about a task holds for the task as it was: a task touched after the note (a handoff written again, a session cut again) is a new task and is listed again, '
         'whether the note dropped it, closed it or queued it; a note newer than the touch holds, a unit that is still alive holds, and his own words stay with the task',
         all(len(r) == 1 and r[0]['state'] == 'left' for r in states) and [f['rows'] for f in fresh] == [[], []] and [f['dropped'] + f['done'] for f in fresh] == [1, 1]
         and kept['state'] == 'queued' and kept['words'] == ['Mind the wire.'], ([[x['state'] for x in r] for r in states], kept['state']))


def taken_back(P, board):
    """A5, B4, B1: a drop after a queue takes the unit back; a unit that cannot be written is said; the Drive away."""
    where, units = notes.folder(), src_tasks.root() / 'units-for-master'
    day = datetime.datetime.fromtimestamp(NOW)
    read = lambda live=True: tasks.read(every_note=notes.read_all(where), now=NOW, cache_path=TMP / 'cache-HERE.json', projects=P, board=board, host='HERE', live=live, fetch=False)
    say = lambda tid, text: notes.write(where, text, kind='queue', about='task: ' + tid, title=tid, now=day + datetime.timedelta(seconds=30))
    had = (units / 'task-agent-cutlong.json').exists()
    d = say('agent-cutlong', src_tasks.DROP_SAY)
    T = read()
    did = tasks.act(T, where, units, now=day)
    ans = {n['id']: n for n in notes.read_all(where)}[d['id']]['answers'][-1]['text']
    case('tasks: "not needed" after "queue it" takes the unit file back while the master has not taken it, says so on his note, and the task leaves the list',
         had and not (units / 'task-agent-cutlong.json').exists() and 'task-agent-cutlong.json was taken back' in ans and 'agent-cutlong' not in [r['id'] for r in T['rows']]
         and (units / 'never-queued.json').exists(), (had, ans, did))
    # the relay already has the unit: the file is not the board's to pull, and he is told the relay will still run it
    (units / 'task-agent-y.json').write_text('{}', encoding='utf-8')
    d = say('agent-y', src_tasks.DROP_SAY)
    tasks.act(dict(rows=[], relay=dict(taken=['task-agent-y'])), where, units, now=day)
    ans = {n['id']: n for n in notes.read_all(where)}[d['id']]['answers'][-1]['text']
    case('tasks: a drop of a task the relay already has says the relay will still run it, and removes nothing', (units / 'task-agent-y.json').exists() and 'The relay already has it as unit task-agent-y' in ans, ans)
    (units / 'task-agent-y.json').unlink()
    d = say('unit-never-queued', src_tasks.DROP_SAY)
    r = say('relay-tried-and-failed', src_tasks.DROP_SAY)
    tasks.act(dict(rows=[], relay=dict(taken=['tried-and-failed'])), where, units, now=day)
    ans = {n['id']: n['answers'][-1]['text'] for n in notes.read_all(where) if n['answers']}
    case('tasks: a drop of a unit somebody else wrote, or of a unit in the relay\'s queue, says it only leaves the list: the file stays for the master, the relay may run it again',
         (units / 'never-queued.json').exists() and 'never-queued.json is somebody else\'s and stays' in ans[d['id']] and 'The relay still has unit tried-and-failed' in ans[r['id']], (ans[d['id']], ans[r['id']]))
    old = notes.write(where, src_tasks.QUEUE_SAY, kind='queue', about='task: session-cutsess9', title='x', now=day)
    did = tasks.act(dict(rows=[dict(id='session-cutsess9', state='left', stale=[old['id']])], relay={}), where, units, now=day)
    ans = {n['id']: n for n in notes.read_all(where)}[old['id']]
    case('tasks: a click of his that the task has outlived is answered and closed, so the page stops showing it as said and he knows to say it again',
         ans['state'] == 'done' and ans['answers'][-1]['text'].startswith('Not taken up: the task changed after you said this') and not (units / 'task-session-cutsess9.json').exists(), (ans['answers'], did))
    # a unit the relay's rules refuse
    q = say('session-asksess1', src_tasks.QUEUE_SAY)
    keep, briefs.check_then = briefs.check_then, lambda says, unit=None: ['the lane is not one the relay takes']
    try:
        T = read()
        did = tasks.act(T, where, units, now=day)
    finally:
        briefs.check_then = keep
    row = next(r for r in T['rows'] if r['id'] == 'session-asksess1')
    after = next(r for r in read()['rows'] if r['id'] == 'session-asksess1')
    ans = {n['id']: n for n in notes.read_all(where)}[q['id']]
    case('tasks: a task that cannot be written as a unit is not left saying "queued": his note is answered with the reason, the task is back on the list with it, and it is not tried again every reading',
         ans['state'] == 'done' and ans['answers'][-1]['text'].startswith('Not queued: ') and 'the lane is not one the relay takes' in ans['answers'][-1]['text'] and row['state'] == 'left'
         and after['state'] == 'left' and 'could not be written as a unit' in after['detail'][-1] and not (units / 'task-session-asksess1.json').exists() and tasks.act(read(), where, units, now=day) == [],
         (ans['answers'], after['state'], after['detail']))
    # the Drive both stations read is away
    wait = capture('2026-10-07-220000')
    q = say('session-asksess1', src_tasks.QUEUE_SAY)
    (src_tasks.root() / 'tasks' / 'HERE.json').unlink(missing_ok=True)
    keep, src_tasks.away = src_tasks.away, lambda: True
    try:
        T = read()
        did = tasks.act(T, where, units, now=day)
    finally:
        src_tasks.away = keep
    case('tasks: with the Drive away nothing is taken in or written where only this station would see it: the capture stays in the game\'s folder and is counted, his click is not taken up, '
         'no station file is written, and the reading says the Drive is away',
         T['drive_away'] is True and T['waiting_here'] == 1 and wait.exists() and not (units / 'task-session-asksess1.json').exists() and not (src_tasks.root() / 'tasks' / 'HERE.json').exists()
         and did == ['the Drive is away: nothing he said was taken up, it is when the Drive is back'] and {n['id']: n for n in notes.read_all(where)}[q['id']]['state'] != 'done', (T['drive_away'], T['waiting_here'], did))
    T = read()
    tasks.act(T, where, units, now=day)
    case('tasks: when the Drive is back the capture is taken in and his click is taken up', T['drive_away'] is False and not wait.exists() and (units / 'task-session-asksess1.json').exists(), None)
    # an F10 that failed: a folder the game made and wrote no state file into
    dead = feedback.inbox() / '2026-10-07-230000'
    dead.mkdir()
    (dead / feedback.OPEN).write_text('', encoding='utf-8')
    for g in (dead / feedback.OPEN, dead):
        os.utime(g, (NOW - 45 * MIN, NOW - 45 * MIN))
    young = feedback.inbox() / '2026-10-07-230100'
    young.mkdir()
    (young / feedback.OPEN).write_text('', encoding='utf-8')
    T = read()
    hit = [r for r in T['rows'] if r['id'].startswith('capture-broken-')]
    case('tasks: a folder the game made for a capture and wrote no state file into is said on the board as an F10 that failed, half an hour on; one still being written is not, and neither is taken in',
         [r['id'] for r in hit] == ['capture-broken-2026-10-07-230000-HERE'] and hit[0].get('fault') is True and 'wrote no state file' in hit[0]['title'] + ' '.join(hit[0]['detail']) and dead.exists() and young.exists(),
         [r['id'] for r in hit])
    shutil.rmtree(dead)
    shutil.rmtree(young)


def watcher():
    """One read that fails must not end the watcher."""
    calls, slept, said = [], [], []
    keep = (ops.once, ops.page, ops.notes.serve, ops.time.sleep, ops.site_version, ops.traceback.print_exc)

    def once(out, watching=False):
        calls.append(watching)
        if len(calls) == 1:
            raise RuntimeError("can't start new thread")
        if len(calls) == 3:
            raise KeyboardInterrupt
        return dict(now='then', counts=dict(sessions=0, machines=0, ready=0, idle=0), queue=dict(count=0, agents=0), notes=0, tasks=dict(left=4, queued=0), runs=dict(runs=3, waits=1, going=0), roster=[]), False
    ops.once, ops.page, ops.notes.serve, ops.time.sleep, ops.site_version, ops.traceback.print_exc = once, lambda *a: None, lambda *a, **k: (None, 'k'), slept.append, lambda: ops.SITE['v'], lambda: said.append('trace')
    try:
        ops.main(['--watch', '20', '--out', str(TMP / 'watch-site')])
    except KeyboardInterrupt:
        pass
    finally:
        ops.once, ops.page, ops.notes.serve, ops.time.sleep, ops.site_version, ops.traceback.print_exc = keep
    case('watcher: a read that fails is said, with why, and the next one runs; every read of the watcher is told it is the watcher\'s', calls == [True, True, True] and slept == [20.0, 20.0] and said == ['trace'], (calls, slept, said))
    try:
        ops.once = lambda out, watching=False: (_ for _ in ()).throw(RuntimeError('broken'))
        ops.page = lambda *a: None
        try:
            ops.main(['--out', str(TMP / 'watch-site')])
            raised = False
        except RuntimeError:
            raised = True
    finally:
        ops.once, ops.page = keep[0], keep[1]
    case('watcher: read once (no watch), a failure is the command\'s failure and is not swallowed', raised, None)


def this_station():
    """This station's own logs, read and not changed: the rule runs on real lines, and reads more than nothing."""
    if not src_ops.PROJECTS.is_dir():
        print('      (no Claude logs on this machine: the count of its own was not run)')
        return
    cache, t = {}, time.time()
    rows = src_tasks.local(src_ops.PROJECTS, t, cache, 'here')
    ends = {}
    for e in cache['files'].values():
        ends[e['info']['end']] = ends.get(e['info']['end'], 0) + 1
    print(f'      this station: {len(cache["files"])} logs of the last {src_tasks.DAYS} days read in {time.time() - t:.1f} s: {ends}; {len(rows)} cut off or left on a question')
    case('tasks: on this station\'s own logs every log is told done, cut, stopped or asking, and every row has what the page shows of it',
         set(ends) <= {'done', 'cut', 'stopped', 'asks'} and all(r['id'] and r['title'] and r['touched'] for r in rows) and (not cache['files'] or ends.get('done', 0) > 0), ends)


if __name__ == '__main__':
    endings()
    P = listed()
    board = shared(P)
    captures()
    T = his_word(P, board)
    site(T)
    delivered()
    long_lines()
    in_hand()
    one_life()
    taken_back(P, board)
    page_rules()
    watcher()
    this_station()
    shutil.rmtree(TMP, ignore_errors=True)
    print(f'{sum(results)} of {len(results)} cases behaved')
    sys.exit(0 if all(results) else 1)
