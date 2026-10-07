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
os.environ.update(TW_TASKS=str(TMP / 'root'), TW_FEEDBACK=str(TMP / 'inbox'), TW_NOTES=str(TMP / 'notes'), TW_BRIEFS=str(TMP / 'briefs'))
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
    cache = {}
    mine = src_tasks.local(P, NOW, cache, 'HERE')
    T = src_tasks.collect(mine, now=NOW)
    ids = sorted(r['id'] for r in T['rows'])
    case('tasks: a cut-off agent is listed at 91 minutes and not at 89; never one that finished, one he stopped, one its session ran again to the end, one whose session is still at work, '
         'one of another project or one from before the window; a session cut off or left on a question is listed the same way',
         ids == ['agent-cutlong', 'session-asksess1', 'session-cutsess1'], ids)
    row = next((r for r in T['rows'] if r['id'] == 'agent-cutlong'), {})
    case('tasks: a row says what the task was, who started it, on which station, how long nobody has been on it and how it stopped',
         row.get('title') == 'Cut 91 minutes ago' and row.get('where') == 'HERE' and row.get('by') == 'carry on with the lane' and row.get('idle') == 91
         and 'in the middle of a tool call' in ' '.join(row.get('detail', [])) and row.get('state') == 'left', row)
    case('tasks: a session left on a question to him says so', next(r for r in T['rows'] if r['id'] == 'session-asksess1').get('asks') is True, None)
    seen = len(cache['files'])
    os.utime(P / 'p' / 'busy0001.jsonl', (NOW - 200 * MIN, NOW - 200 * MIN))
    again = src_tasks.collect(src_tasks.local(P, NOW, cache, 'HERE'), now=NOW)
    case('tasks: when its session goes quiet too, the agent it left cut off is listed; a log is read again only when it changed',
         'agent-parentup' in [r['id'] for r in again['rows']] and len(cache['files']) == seen and seen == 14, (sorted(r['id'] for r in again['rows']), seen))
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
         and one['shot'].endswith('shot.png') and len(one['lines']) <= 5 and 'THE SHELLED FOREST, 03:00 into the match (tick 5400), seed 1917' in one['lines'][0]
         and '98 of ours and 114 of theirs' in one['lines'][1] and 'lane/show/x at abc12345 with 2 files changed' in one['lines'][-1], one['lines'])
    rows = src_tasks.collect([], shared=src_tasks.capture_rows(every), now=NOW)['rows']
    case('capture: a capture is a task from its first minute, the newest first, his words its title, and one with no words says so',
         [r['id'] for r in rows] == ['capture-2026-10-07-200300-HERE', 'capture-2026-10-07-200500-HERE', 'capture-2026-10-07-200000-HERE'] and rows[1]['title'] == 'The tank drove through the wire.'
         and rows[0]['title'] == 'A capture with no words' and all(r['idle'] <= 1 for r in rows), [(r['id'], r['title'], r['idle']) for r in rows])
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
         and T['left'] == len(ids) and T['queued'] == 0 and T['relay']['waiting'] == 2 and T['captures'] == 3, (ids, T['relay'], T['left']))
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
         theirs['host'] == 'HERE' and {r['kind'] for r in theirs['rows']} == {'agent', 'session'} and 'agent-cutlong' in [r['id'] for r in from_here]
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
         text.startswith('window.TASKS = ') and shown and all('ask' not in r and 'picture' not in r for r in data['rows']) and data['stale_minutes'] == 90, cap)
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
          'const R = [{id: "a", kind: "agent", title: "Review", what: "Review the sim", state: "left", idle: 91, where: "MSI", agent: "Explore", detail: ["An agent."], words: ["x"]},'
          ' {id: "c", kind: "capture", title: "The wire", state: "left", idle: 3, shots: [{src: "img/task/c.jpg"}], detail: ["In the forest."]},'
          ' {id: "u", kind: "unit", title: "rv-1", what: "Fix it", state: "queued", idle: 4000, lane: "lane/sim/x"}, {id: "h", kind: "handoff", title: "Landing", state: "left", idle: 130}];'
          'const D = {rows: R, relay: {waiting: 96, as_of: "2026-10-07 01:26"}, stations: [{host: "MSI", at: 1000}, {host: "DESK", at: 1000 - 3 * 3600}], stale_minutes: 90};'
          'const said = r => r.id === "a" ? [T.QUEUE] : r.id === "h" ? [T.DROP] : [];'
          'const G = T.groups(D), S = T.groups(D, said);'
          'console.log(JSON.stringify([G.map(g => [g.key, g.rows.map(r => r.id)]), S.map(g => [g.key, g.rows.map(r => r.id + ":" + r.state)]), T.head(D, G), T.head(D, S), T.head(null, []), T.head({rows: []}, []),'
          ' G[0].rows[0], G[1].rows[0].chips, T.subject(R[0], "Agents that were cut off", []).actions.map(a => a.say), T.subject(R[0], "x", [T.QUEUE]).actions, T.subject(R[2], "x", []).actions,'
          ' [T.subject(R[0], "Agents that were cut off", []).id, T.subject(R[0], "Agents that were cut off", []).kind, T.subject(R[0], "x", []).lane],'
          ' [T.span(0), T.span(45), T.span(119), T.span(120), T.span(2879), T.span(2880)], T.heard(D, 1060), [T.state(R[2], [T.DROP]), T.state(R[0], [T.DROP, T.QUEUE]), T.state(R[0], ["other words"])]]))')
    p = subprocess.run([node, '-e', js, str(HERE / 'static' / 'tasks.js')], capture_output=True)
    got = json.loads(p.stdout.decode() or 'null')
    case('tasks page: the rows are grouped by what they are, the captures first; a task he drops is gone from his click on and one he queues says so, before the next reading does',
         got and got[0] == [['capture', ['c']], ['agent', ['a']], ['handoff', ['h']], ['unit', ['u']]] and got[1] == [['capture', ['c:left']], ['agent', ['a:queued']], ['unit', ['u:queued']]],
         (got and got[:2], p.stderr[-400:]))
    case('tasks page: the line under the title counts what waits for an agent, what he queued and how long the relay\'s queue is, in words that fit one and none',
         got and got[2:6] == ['3 tasks wait for an agent · 1 queued for the relay · 96 units in the relay\'s queue as of 2026-10-07 01:26',
                              '1 task waits for an agent · 2 queued for the relay · 96 units in the relay\'s queue as of 2026-10-07 01:26', 'The tasks have not been read yet.', 'Nothing is left unfinished'], got and got[2:6])
    case('tasks page: a capture\'s row has its picture, when it was taken and where in the match; another\'s says how long nobody has been on it, on which station, and what kind of agent it was',
         got and got[6] == dict(id='c', top='The wire', sub='In the forest.', shot='img/task/c.jpg', chips=['captured 3 min ago'], state='left', tip='In the forest.')
         and got[7] == ['nobody on it for 91 min', 'on MSI', 'Explore agent', '1 note of yours'], got and got[6:8])
    case('tasks page: a click on a task offers the two things he can say while nothing has been said, and no button once it is queued; the panel is about the task and nothing wider, so the '
         'notes under it are the notes about it',
         got and got[8] == ['Queue this for the relay.', 'Not needed: drop this task.'] and got[9] == [] and got[10] == [] and got[11] == ['task: a', 'queue', None], got and got[8:12])
    case('tasks page: how long is said in minutes, then hours, then days; each station says when it was last heard from; of his two set phrases the drop wins, and other words change nothing',
         got and got[12] == ['under a minute', '45 min', '119 min', '2 h', '48 h', '2 days'] and got[13] == 'MSI just now, DESK 3 h ago' and got[14] == ['queued', 'dropped', 'left'], got and got[12:])


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
        return dict(now='then', counts=dict(sessions=0, machines=0, ready=0, idle=0), queue=dict(count=0, agents=0), notes=0, tasks=dict(left=4, queued=0), roster=[]), False
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
    page_rules()
    watcher()
    this_station()
    shutil.rmtree(TMP, ignore_errors=True)
    print(f'{sum(results)} of {len(results)} cases behaved')
    sys.exit(0 if all(results) else 1)
