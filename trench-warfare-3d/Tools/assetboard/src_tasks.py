#!/usr/bin/env python3
"""The tasks agents left unfinished: what the board's Tasks page lists (tasks.py reads them, tasks.html shows them).

WHY (the owner, 2026-10-07: "a task board where the user can see all the tasks that are spawned by any agent ... we
only start putting them in after no agent has worked on them for longer then 1.5 hours ... so we dont lose track of
unfinished tasks"). Work an agent starts is an agent's to finish, so a task is not listed while anyone is on it. It
is listed once it is unfinished and nothing has touched it for STALE seconds: then nobody is coming back to it.

Nothing here is stored. No session on these stations has a todo tool, so there is no list to read; a task is derived
from the record its work already leaves:
- agent:    a subagent whose log stops on an open tool call, a line nobody answered or a failed call to the service
            (ending()), and whose report never reached the session that started it (heard_of(): a log that ends on
            a failed call after its report was handed back is a finished agent). It is touched when its log or its
            parent session's transcript moves: a parent that is still at work may yet resume it;
- session:  a session's transcript that stops the same way, or on a question to the owner nobody answered;
- paused:   a session the owner interrupted himself. His doing, so it waits for no agent and is counted apart;
- handoff:  a handoff the index (handoffs.json, Tools/handoffs.py) calls current, touched when its file changes or a
            session that ever opened it moves (named()); and a handoff file the index does not know, as a fault;
- unit:     a unit file written for the relay (units-for-master/) whose id never reached the relay's queue;
- relay:    a unit of the relay's queue the relay has run legs of and not finished (tasks.py reads the board's
            origin/main with git, nothing is checked out);
- ready:    a pipeline step ready to take (the queue's `ready` rows, src_queue.py);
- capture:  a feedback capture from the game (F10, feedback.py). It is the owner's and no agent has it, so it is
            listed at once.
Only work on this project is read (NAMES in the folder or the request), and nothing older than DAYS.

What the owner says about a task is a note of his about "task: <id>" (notes.py), like every other word of his on the
board: QUEUE_SAY queues it for the relay (tasks.py writes the unit), DROP_SAY takes it off the list (and takes the
unit back, when the relay has not taken it), anything else is shown with it. A note about it from anyone else says an
agent closed it. A note is about the task as it was when it was written: a task touched after the note (a handoff
written again, a session resumed and cut again) is a new task, and the old word does not hold it.

ONE LIFE (said()): left, queued (its unit file waits for the master), with the relay (the unit is in the relay's
queue: the row says how the relay stands with it, and the relay's own row for it is not shown beside it), done (the
relay finished the unit, or an agent closed it).

WHAT THE BOARD CANNOT SEE is said on the page (BLIND): a session that ended its turn with work left reads as done.
"""
import datetime
import json
import os
import re
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import build      # noqa: E402

STALE = 90 * 60         # seconds nothing may have touched a task before it is listed
DAYS = 14               # a log older than this is not read
TAIL = 262144           # bytes of a log's end that say how it ended
ENOUGH = 3              # tool calls of a session that must name the project when its folder does not
ASK = 2500              # characters of what a task was asked that are kept
TURNS = 6               # the last lines of talk of a log that go into its unit
DONE_SLACK = 300        # seconds after the relay finished a unit in which a touch is still the leg's own
RUNNING = 6 * 3600      # a relay leg with no end, started this recently, is still running
NAMES = ('trench-warfare-3d', 'trench warfare', 'githubtest', 'tw3d')
ASKS = ('AskUserQuestion', 'ExitPlanMode')
QUEUE_SAY = 'Queue this for the relay.'
DROP_SAY = 'Not needed: drop this task.'
KINDS = ('capture', 'agent', 'session', 'handoff', 'unit', 'relay', 'ready', 'paused')      # the order the page lists them in
SHOWN = ('left', 'queued', 'relay')     # the states a row is listed in
INTEGRATION = 'origin/claude/trench-warfare-2d-3d-plan-idt7lf'
UNIT_ID = re.compile(r'[^\w.-]+')
HANDOFF = re.compile(r'HANDOFF_AGENT_[\w.-]+?\.md')
BACK = re.compile(r'<agent-message from="([\w-]+)">')
NOTICE = re.compile(r'<task-id>([\w-]+)</task-id>.*?<status>(\w+)</status>', re.S)
MARKS = (b'<task-notification>', b'<agent-message from=', b'"agentId"')
TOLD = 'its session was told and went on without it'
BLIND = ('The board cannot see a session that ended its turn with work left: it reads as finished. '
         'A station\'s agents and sessions are read only while its board watcher runs.')


def away():
    """The Drive both stations read is not there (and no folder was named instead): what is written now would be
    written where only this station looks, and gone from the board when the Drive is back."""
    return not os.environ.get('TW_TASKS') and not build.DRIVE.is_dir()


def root():
    """Where the tasks' files are: TW_TASKS, else the Drive both stations see, else this station's board cache.
    Under it: feedback/ (the captures), tasks/<host>.json (each station's own rows), units-for-master/, handoffs.json."""
    if os.environ.get('TW_TASKS'):
        return Path(os.environ['TW_TASKS'])
    return build.DRIVE if build.DRIVE.is_dir() else build.LOCAL


def clip(s, n=110):
    s = re.sub(r'\s+', ' ', str(s or '')).strip()
    return s if len(s) <= n else s[:n - 1].rstrip() + '…'


def stamp(s):
    """A transcript's time as seconds, or None."""
    try:
        return datetime.datetime.fromisoformat(str(s).replace('Z', '+00:00')).timestamp()
    except (ValueError, AttributeError):
        return None


def said_at(n):
    """A note's time as seconds (notes.py writes local time), or 0."""
    try:
        return time.mktime(time.strptime(str(n.get('when', ''))[:19], '%Y-%m-%d %H:%M:%S'))
    except ValueError:
        return 0


def entries(data: bytes):
    out = []
    for line in data.split(b'\n'):
        try:
            d = json.loads(line)
        except ValueError:
            continue
        if isinstance(d, dict):
            out.append(d)
    return out


def tail(f: Path, n=TAIL):
    with open(f, 'rb') as h:
        h.seek(max(0, f.stat().st_size - n))
        return h.read()


def talks(d, side):
    return d.get('type') in ('user', 'assistant') and (side or not d.get('isSidechain'))


def tail_rows(f: Path, side=False, n=TAIL):
    """A log's last entries: its last TAIL bytes, and four times further back, again and again, while those hold no
    line of talk. A last line longer than the tail is cut at its head and is no line at all: read as it was, such a
    log had no talk and so had ended well."""
    size = f.stat().st_size
    while True:
        rows = entries(tail(f, n))
        if n >= size or any(talks(d, side) for d in rows):
            return rows
        n *= 4


def text_of(d):
    c = (d.get('message') or {}).get('content')
    if isinstance(c, str):
        return c
    return ' '.join(str(x.get('text') or '') for x in c or [] if isinstance(x, dict) and x.get('type') == 'text')


def ending(rows, side=False):
    """How a log ended, from its last line of talk: (state, why).
    'done'     the model ended its turn, or handed its report back;
    'stopped'  the owner interrupted it: his doing, not a task anyone lost;
    'asks'     it stopped on a question to the owner nobody answered;
    'cut'      anything else: a tool call with no result, a result or a message the model never answered, a call to
               the service that failed. `rows` are a log's last entries; `side` reads a subagent's log (its lines
               are all marked as a side chain; a session's own log skips those)."""
    talk = [d for d in rows if talks(d, side)]
    if not talk:
        return 'done', ''
    d = talk[-1]
    m = d.get('message') or {}
    content = m.get('content') if isinstance(m.get('content'), list) else []
    if d.get('isApiErrorMessage'):
        return 'cut', 'a call to the service failed'
    if d['type'] == 'assistant':
        calls = [c for c in content if isinstance(c, dict) and c.get('type') == 'tool_use']
        if calls:
            if any(c.get('name') in ASKS for c in calls):
                return 'asks', 'it asked you something and got no answer'
            return 'cut', 'in the middle of a tool call'
        return ('done', '') if m.get('stop_reason') == 'end_turn' else ('cut', 'in the middle of a sentence')
    if d.get('toolEndsTurn'):
        return 'done', ''
    if text_of(d).lstrip().startswith('[Request interrupted'):
        return 'stopped', ''
    return 'cut', 'before it answered'


def last_turns(rows, side=False):
    """The last few lines of talk of a log, each short: what a leg that picks the work up reads first."""
    out = []
    for d in rows:
        if not talks(d, side):
            continue
        t = text_of(d).strip()
        if not t and d['type'] == 'assistant':
            calls = [str(c.get('name')) for c in (d.get('message') or {}).get('content') or [] if isinstance(c, dict) and c.get('type') == 'tool_use']
            t = 'called ' + ', '.join(calls) if calls else ''
        if not t or t.startswith('<'):
            continue                                # a tool's result, a reminder, a notification: not talk
        out.append(('asked: ' if d['type'] == 'user' else 'it: ') + clip(t, 300))
    return out[-TURNS:]


def names_project(*texts):
    low = ' '.join(str(t or '') for t in texts).lower().replace('\\', '/')
    return any(n in low for n in NAMES)


def read_agent(f: Path):
    """What a subagent's log says of itself: how it ended, what it was asked, where it ran, its last line's time."""
    with open(f, 'rb') as h:
        head = h.read(65536)
    first = (entries(head.split(b'\n', 1)[0]) or [{}])[0]
    rows = tail_rows(f, side=True)
    state, why = ending(rows, side=True)
    last = next((stamp(d.get('timestamp')) for d in reversed(rows) if d.get('timestamp')), None)
    try:
        meta = json.loads(f.with_name(f.stem + '.meta.json').read_text(encoding='utf-8'))
    except (OSError, ValueError):
        meta = {}
    ask = text_of(first)
    # when it was last asked something (a request, not a tool's result): a report that reached its session before
    # that is an earlier round's. With no request in the part read, the part's first line stands in: later than the
    # request, so an old report is never taken for the last one's.
    requests = [stamp(d.get('timestamp')) for d in rows if d.get('type') == 'user' and d.get('timestamp') and not any(isinstance(c, dict) and c.get('type') == 'tool_result' for c in ((d.get('message') or {}).get('content') if isinstance((d.get('message') or {}).get('content'), list) else []))]
    asked = next((t for t in reversed(requests) if t), None) or next((stamp(d.get('timestamp')) for d in rows if d.get('timestamp')), None)
    return dict(end=state, why=why, ask=ask[:ASK], cwd=first.get('cwd') or '', last=last, asked=asked, type=meta.get('agentType') or 'agent',
                desc=meta.get('description') or '', ours=names_project(first.get('cwd'), ask), turns=last_turns(rows, side=True))


def read_session(f: Path):
    """The same of a session's transcript, from its end: its title and last request too, and whether it is work on
    this project (its folder names it, or ENOUGH of its last tool calls do). From its start: what it was first asked."""
    rows = tail_rows(f)
    state, why = ending(rows)
    s = dict(end=state, why=why, cwd='', branch='', title='', ask='', last=None, spoke=0, named=0)
    typed = ''

    def words(d):
        t = text_of(d).strip()
        return t if d.get('type') == 'user' and not d.get('isSidechain') and t and not t.startswith(('<', '[')) else ''
    for d in rows:
        s['cwd'] = d.get('cwd') or s['cwd']
        s['branch'] = d.get('gitBranch') or s['branch']
        if d.get('type') == 'ai-title':
            s['title'] = d.get('aiTitle', '')
        elif d.get('type') == 'last-prompt':
            s['ask'] = str(d.get('lastPrompt', ''))[:ASK]
        elif words(d):
            typed = words(d)[:ASK]                  # what he typed last: the request, when the log names none
        if d.get('timestamp'):
            s['last'] = stamp(d['timestamp']) or s['last']
        if d.get('type') == 'assistant' and not d.get('isSidechain'):
            s['spoke'] = stamp(d.get('timestamp')) or s['spoke']
            for c in (d.get('message') or {}).get('content') or []:
                if isinstance(c, dict) and c.get('type') == 'tool_use' and names_project(json.dumps(c.get('input') or {})):
                    s['named'] += 1
    with open(f, 'rb') as h:
        head = entries(h.read(65536))
    s['first'] = next((words(d)[:ASK] for d in head if words(d)), '')
    s['ask'] = s['ask'] or typed
    s['ours'] = names_project(s['cwd']) or s['named'] >= ENOUGH
    s['turns'] = last_turns(rows)
    del s['named']
    return s


def heard_of(f: Path, e=None):
    """What a session's transcript holds of its agents coming back: {agent id: {back: when, told: when}}.
    `back` is the agent's report reaching the session (a hand-back, a notification that it completed, or the result
    of an agent the session waited for); `told` is a notification that it stopped any other way. Only the lines that
    can be one are parsed (MARKS). `e` is what an earlier reading kept ({read: bytes, got: {...}}): only what the
    transcript gained since is read, so a session still at work costs its new lines and no more."""
    e = e if e is not None else {}
    size = f.stat().st_size
    if e.get('read', 0) > size:
        e.clear()
    out = e.setdefault('got', {})

    def mark(aid, key, at):
        x = out.setdefault(str(aid), {})
        x[key] = max(x.get(key, 0), at)
    with open(f, 'rb') as h:
        h.seek(e.get('read', 0))
        data = h.read()
        whole = data.rfind(b'\n') + 1
        e['read'] = e.get('read', 0) + whole
        for line in data[:whole].split(b'\n'):
            if not any(m in line for m in MARKS):
                continue
            try:
                d = json.loads(line)
            except ValueError:
                continue
            if not isinstance(d, dict) or d.get('isSidechain'):
                continue
            at = stamp(d.get('timestamp')) or 0
            if d.get('type') == 'queue-operation':
                c = d.get('content') if d.get('operation') == 'enqueue' else ''
            else:
                c = (d.get('message') or {}).get('content') if d.get('type') == 'user' else ''
            if isinstance(c, str) and c:
                head = c.lstrip()
                m = BACK.match(head)
                if m and '[Subagent hand-back]' in head[:400]:
                    mark(m.group(1), 'back', at)
                m = NOTICE.search(head[:4000]) if head.startswith('<task-notification>') else None
                if m:
                    mark(m.group(1), 'back' if m.group(2) == 'completed' else 'told', at)
            r = d.get('toolUseResult')
            if isinstance(r, dict) and r.get('agentId') and r.get('status') == 'completed':
                mark(r['agentId'], 'back', at)
    return out


def cached(cache, f: Path, read):
    """What `read` says of a log, read again only when the log changed (its size and its time)."""
    st = f.stat()
    e = cache.get(str(f))
    if not e or e.get('size') != st.st_size or e.get('mtime') != int(st.st_mtime):
        e = cache[str(f)] = dict(size=st.st_size, mtime=int(st.st_mtime), info=read(f))
    return e['info'], st.st_mtime


def when(t):
    return time.strftime('%Y-%m-%d %H:%M', time.localtime(t)) if t else ''


def local(projects: Path, now, cache, host=''):
    """This station's own tasks: the subagents and the sessions that were cut off, each with when it was last touched.
    Not yet filtered by STALE (the caller does that, so another station's rows age on this one's clock)."""
    files, heard = cache.setdefault('files', {}), cache.setdefault('heard', {})
    since, seen, rows = now - DAYS * 86400, set(), []
    for f in sorted(projects.glob('*/*.jsonl')) if projects.is_dir() else []:
        try:
            if f.stat().st_mtime < since:
                continue
            s, at = cached(files, f, read_session)
            seen.add(str(f))
            sub = f.with_suffix('') / 'subagents'
            logs = sorted(sub.glob('agent-*.jsonl')) if sub.is_dir() else []
            agents = []
            for g in logs:
                if g.stat().st_mtime < since:       # its session went on, the agent is from before the window
                    continue
                a, moved = cached(files, g, read_agent)
                seen.add(str(g))
                agents.append((g, a, moved))
            newest = max([at] + [m for _, _, m in agents])
            title = clip(s['title'] or s['ask'], 70)
            if s['ours'] and s['end'] in ('cut', 'asks', 'stopped'):
                rows.append(dict(id='session-' + f.stem[:8], kind='paused' if s['end'] == 'stopped' else 'session', title=title or 'A session', what=clip(s['ask'], 200), ask=s['ask'], lane='', where=host,
                                 by='', touched=int(newest), changed=int(at), why=s['why'], asks=s['end'] == 'asks', stopped=when(s['last'] or at),
                                 sid=f.stem, log=str(f), cwd=s['cwd'], branch=s['branch'], first=s['first'], turns=s['turns']))
            # an agent the same session ran again to its end is not lost: the later run is the task, done
            redone = {a['desc'] for _, a, m in agents if a['end'] == 'done' and a['desc']}
            for g, a, moved in agents:
                if a['end'] != 'cut' or not (a['ours'] or s['ours']) or a['desc'] in redone:
                    continue
                aid, why = g.stem[len('agent-'):], a['why']
                # before the row is written anywhere (the other station lists what this one writes): did its report reach its session?
                got = heard_of(f, heard.setdefault(str(f), {})).get(aid) or {}
                seen.add('heard:' + str(f))
                since_asked = a.get('asked') or a['last'] or moved
                if got.get('back', 0) >= since_asked:
                    continue                        # its report reached the session after its last request: what failed after that lost nothing
                if got.get('told', 0) >= since_asked and s['spoke'] > got['told']:
                    why = f'{why}; {TOLD}'
                rows.append(dict(id='agent-' + aid[:10], kind='agent', title=clip(a['desc'] or a['ask'], 70), what=clip(a['ask'], 200), ask=a['ask'], lane='', where=host,
                                 by=title, agent=a['type'], touched=int(max(moved, at)), changed=int(moved), why=why, stopped=when(a['last'] or moved),
                                 log=str(g), parent=f.stem, parent_log=str(f), cwd=a['cwd'], turns=a['turns']))
        except OSError:
            continue
    for key in [k for k in files if k not in seen]:
        del files[key]
    for key in [k for k in heard if 'heard:' + k not in seen]:
        del heard[key]
    return rows


def named(projects: Path, now, cache):
    """{handoff file name: when a session that ever opened it last moved}, over the logs still being written (moved
    in the last STALE seconds). A session that opened a handoff at its start and has worked for hours no longer
    names it in its last lines: what a log has opened is kept (cache['named']) and only what was added since is read.
    Only a Read of the file counts: the session that wrote it, a folder listing that shows its name, a command or a
    prompt that mentions it have not opened it, and a handoff its own writer still "holds" would never be listed."""
    store, out, live = cache.setdefault('named', {}), {}, set()
    logs = (list(projects.glob('*/*.jsonl')) + list(projects.glob('*/*/subagents/*.jsonl'))) if projects.is_dir() else []
    for f in logs:
        try:
            st = f.stat()
            if now - st.st_mtime > STALE:
                continue
            e = store.get(str(f))
            if not e or e['read'] > st.st_size:
                e = store[str(f)] = dict(read=0, names=[])
            if e['read'] < st.st_size:
                with open(f, 'rb') as h:
                    h.seek(e['read'])
                    data = h.read()
                whole = data.rfind(b'\n') + 1       # a line still being written is read next time
                for line in data[:whole].split(b'\n'):
                    if b'HANDOFF_AGENT_' not in line:
                        continue
                    for d in entries(line):
                        if d.get('type') != 'assistant':
                            continue
                        for c in (d.get('message') or {}).get('content') or []:
                            if isinstance(c, dict) and c.get('type') == 'tool_use' and c.get('name') == 'Read':
                                given = json.dumps(c.get('input') or {})
                                e['names'] = sorted(set(e['names']) | set(HANDOFF.findall(given)))
                e['read'] += whole
        except OSError:
            continue
        live.add(str(f))
        for n in e['names']:
            out[n] = max(out.get(n, 0), st.st_mtime)
    for key in [k for k in store if k not in live]:
        del store[key]
    return out


def handoffs(where: Path, projects: Path, now, cache=None):
    """The handoffs the index calls current, each touched when its file changed or a session that opened it moved.
    And every handoff file the index does not know: nobody will be pointed to it, which is a fault to list."""
    try:
        index = json.loads((where / 'handoffs.json').read_text(encoding='utf-8')).get('handoffs') or {}
    except (OSError, ValueError, AttributeError):
        return []
    current = {n: h for n, h in index.items() if isinstance(h, dict) and h.get('state') == 'current'}
    seen, rows = named(projects, now, cache if cache is not None else {}), []
    stray = {f.name: None for f in where.glob('HANDOFF_AGENT_*.md') if f.name not in index}
    for name, h in sorted({**current, **stray}.items()):
        try:
            changed = (where / name).stat().st_mtime
        except OSError:
            continue                                # the index names a file that is gone: nothing to pick up
        slug = UNIT_ID.sub('-', Path(name).stem.replace('HANDOFF_AGENT_', ''))[:60]
        if h is None:
            rows.append(dict(id='handoff-' + slug, kind='handoff', title=clip(f'{name} is not in the handoff index', 70), fault=True, lane='', where='', by='', file=name,
                             what='A handoff file the index does not know: no agent is pointed to it.',
                             ask=f'The handoff {name} in {where} is not in the handoff index (handoffs.json). Read it, then register it with Tools/handoffs.py or say why it should go.',
                             touched=int(max(changed, seen.get(name, 0))), changed=int(changed), stopped=when(changed)))
            continue
        rows.append(dict(id='handoff-' + slug, kind='handoff', title=clip(h.get('topic') or name, 70), what=clip(h.get('for'), 200),
                         ask=f'Read the handoff {name} in {where} and carry on from it. It is for: {h.get("for", "")}', lane='', where='', by='', file=name,
                         touched=int(max(changed, seen.get(name, 0))), changed=int(changed), stopped=when(changed)))
    return rows


def unit_files(where: Path, queued=(), done=()):
    """The unit files written for the relay that never reached its queue. A file the board wrote itself from a task
    (its name starts task-) is that task, queued, and not a new one."""
    rows = []
    for f in sorted(where.glob('*.json')) if where.is_dir() else []:
        try:
            u = json.loads(f.read_text(encoding='utf-8'))
            at = f.stat().st_mtime
        except (OSError, ValueError):
            continue
        if not isinstance(u, dict) or not all(u.get(k) for k in ('id', 'lane', 'goal')) or u['id'] != f.stem:
            continue                                # a booking, a note: not a unit
        if u['id'] in queued or u['id'] in done or u['id'].startswith('task-'):
            continue
        rows.append(dict(id='unit-' + u['id'], kind='unit', title=u['id'], what=clip(u['goal'], 200), ask=str(u['goal'])[:ASK], lane=u['lane'], where='', by='', file=f.name,
                         touched=int(at), stopped=when(at)))
    return rows


def unit_names(where: Path):
    """The ids of the unit files that are there now (what said() asks to know a queued task's unit still exists)."""
    return {f.stem for f in where.glob('*.json')} if where.is_dir() else set()


def legs_of(relay):
    """{unit id: (its legs, the newest one, the verdict of the last stop that named it)} of the relay's record."""
    verdict = {}
    for s in sorted(relay.get('stops') or [], key=lambda s: str(s.get('stopped_at', ''))):
        verdict.update({u: v for u, v in (s.get('units') or {}).items()})
    by = {}
    for leg in relay.get('legs') or []:
        if leg.get('unit'):
            by.setdefault(leg['unit'], []).append(leg)
    return {uid: (legs, max(legs, key=lambda l: str(l.get('finished_at') or l.get('started_at') or '')), verdict.get(uid, '')) for uid, legs in by.items()}


def relay_rows(relay, now):
    """The units the relay ran legs of and did not finish. `relay` is what tasks.py read off the board: queue {id:
    unit}, done [ids], legs [leg records], stops [stop records]."""
    rows = []
    for uid, (legs, last, verdict) in sorted(legs_of(relay).items()):
        u = (relay.get('queue') or {}).get(uid)
        if not u or uid in (relay.get('done') or []):
            continue
        at = stamp(last.get('finished_at') or last.get('started_at')) or 0
        if not last.get('finished_at') and now - at < RUNNING:
            continue                                # its newest leg has not ended: the relay is on it now
        rows.append(dict(id='relay-' + uid, kind='relay', title=uid, what=clip(u.get('goal'), 200), ask=str(u.get('goal', ''))[:ASK], lane=u.get('lane', ''), where='', by='', legs=len(legs),
                         verdict=verdict, report=clip(last.get('report'), 300), touched=int(at), stopped=when(at)))
    return rows


def ready_rows(ready, now):
    """The pipeline steps that are ready to take, from the queue's rows (src_queue.py): touched when they became ready."""
    rows = []
    for r in ready or []:
        try:
            at = time.mktime(time.strptime(str(r.get('since', ''))[:19], '%Y-%m-%d %H:%M:%S'))
        except ValueError:
            continue                                # no time it became ready: it cannot be said to have waited
        rows.append(dict(id='ready-' + UNIT_ID.sub('-', f'{r.get("item")}-{r.get("stage")}'), kind='ready', title=clip(r.get('title') or r.get('item'), 70), what=f'Its step {r.get("stage")} is ready and nobody has taken it.',
                         ask=f'Take the pipeline step {r.get("stage")} of the item {r.get("item")}: python Tools/pipeline/pipeline.py next', lane=r.get('lane', ''), where='', by='', role=r.get('role', ''),
                         touched=int(at), stopped=when(at)))
    return rows


def capture_rows(captures, broken=()):
    """A feedback capture as a task (feedback.py read_all): the owner's own, listed from the moment it is taken in.
    One with no words is told from the next by where and when in the match it was. `broken` are the folders the
    game left that are no capture (feedback.py broken()): an F10 that failed is said, not skipped."""
    rows = []
    for c in captures or []:
        rows.append(dict(id='capture-' + c['id'], kind='capture', title=clip(c.get('note'), 70) or clip('No words: ' + (c.get('place') or 'a capture'), 70), what=clip(c.get('note'), 200), ask=c.get('ask', ''), lane='',
                         where=c.get('host', ''), by='you', touched=int(c.get('at') or 0), stopped=c.get('when', ''), picture=c.get('shot') or '', lines=c.get('lines') or [], now=True))
    for b in broken or []:
        rows.append(dict(id='capture-broken-' + UNIT_ID.sub('-', f'{b["name"]}-{b.get("host", "")}').strip('-'), kind='capture', title=clip(f'A capture that failed: {b["why"]}', 70), what=clip(b['why'], 200), fault=True,
                         ask=f'A feedback capture on {b.get("host", "")} was not taken in: {b["why"]}. Its folder: {b["folder"]}', lane='', where=b.get('host', ''), by='you', touched=int(b.get('at') or 0),
                         stopped=when(b.get('at')), picture='', lines=[f'F10 was pressed on {b.get("host", "")} and the capture is not whole: {b["why"]}.', f'Its folder: {b["folder"]}'], now=True))
    return rows


def unit_id(r):
    """The id of the relay unit a task is, or becomes: a unit file's and a relay unit's own, else task-<the task's id>."""
    if r['kind'] in ('unit', 'relay'):
        return r['id'].split('-', 1)[1]
    return ('task-' + UNIT_ID.sub('-', r['id']))[:80].strip('-.')


def said(rows, notes, relay=None, have=None):
    """Put on each row what was said about it and where it stands: `state` (left, queued, relay, dropped, done), his
    words, the note that queued it and its unit. The last of his two set phrases wins; a note from anyone but him
    closes the task. A note older than the task's own last change (`changed`: the agent's log, the session's
    transcript, the handoff's file; not its parent session moving or somebody opening it) is about what the task was,
    and holds nothing now, unless its unit is still alive (a file for the master, or in the relay's queue): then the
    unit is the task. A unit the relay finished closes the task, unless the task changed after the relay was done.
    His open notes that no longer hold are listed on the row (`stale`): tasks.py answers them, so the page does not go
    on showing a click that nothing will take up.
    `relay` is the relay's record (queue, done, legs, stops), `have` the ids of the unit files that are there; with
    `have` None nothing is known of the files and a queued task is taken at its note's word."""
    relay = relay or {}
    queue, done, legs, done_at = relay.get('queue') or {}, set(relay.get('done') or []), legs_of(relay), relay.get('done_at') or {}
    by = {}
    for n in sorted(notes or [], key=lambda n: str(n.get('when', ''))):
        if str(n.get('about', '')).startswith('task: '):
            by.setdefault(n['about'][len('task: '):], []).append(n)
    for r in rows:
        r['state'], r['words'], r['note'], r['unit'], r['stale'] = 'left', [], '', '', []
        uid = unit_id(r)
        alive = uid in queue or (have is not None and uid in have)
        mine = r.get('changed', r.get('touched', 0))
        finished = uid in done and mine <= (stamp(done_at.get(uid)) or float('inf')) + DONE_SLACK       # done, and not changed since
        for n in by.get(r['id'], []):
            text, old = str(n.get('text', '')).strip(), said_at(n) < mine
            if old and n.get('state') != 'done' and n.get('from', 'owner') == 'owner' and text in (QUEUE_SAY, DROP_SAY):
                r['stale'].append(n['id'])
            if n.get('from', 'owner') != 'owner':
                if not old:
                    r['state'], r['closed'] = 'done', f'{n.get("from")}: {clip(text, 160)}'
            elif text == QUEUE_SAY:
                took = [a for a in n.get('answers') or [] if str(a.get('text', '')).startswith('Unit ')]
                if n.get('state') == 'done' and not took:
                    no = [a for a in n.get('answers') or [] if str(a.get('text', '')).startswith('Not queued')]
                    if no and not old:
                        r['refused'] = clip(no[-1]['text'], 300)
                    continue                        # he closed the note before it was taken up, or it could not be queued
                if old and not alive and not finished:
                    continue
                r['state'], r['note'], r['answered'] = 'queued', n['id'], bool(took)
                r['unit'] = took[-1]['text'].split()[1].rstrip(':.') if took else ''
                r['said'] = str(n.get('when', ''))[:16]
                r.pop('refused', None)
            elif text == DROP_SAY:
                if not old:
                    r['state'], r['note'] = 'dropped', n['id']
            else:
                r['words'].append(clip(text, 400))
        if r['state'] != 'queued':
            continue
        if finished:
            r['state'], r['closed'] = 'done', f'the relay finished unit {uid}'
        elif uid in done:
            r['state'], r['unit'], r['note'] = 'left', '', ''       # finished once, and changed since: a new task
        elif uid in queue:
            r['state'], r['unit'] = 'relay', uid
            mine = legs.get(uid)
            if mine and r['kind'] != 'relay':
                r['legs'], r['verdict'], r['report'] = len(mine[0]), mine[2], clip(mine[1].get('report'), 300)
        elif r['unit'] and have is not None and uid not in have and r['kind'] not in ('unit', 'relay'):
            r['unit'] = ''                          # its unit file is gone and the relay never had it: it is written again
    return rows


def detail(r):
    """At most five short lines that say what a task is, for the panel a click on its row opens."""
    on = f' on {r["where"]}' if r.get('where') else ''
    k = r['kind']
    if k == 'agent':
        kind = 'general' if r.get('agent') == 'general-purpose' else r.get('agent', '')
        out = [f'A {kind} agent{on}, started by the session “{r["by"]}”.' if r.get('by') else f'A {kind} agent{on}.',
               f'It stopped {r["stopped"]}, {r.get("why", "")}.' + ('' if TOLD in r.get('why', '') else ' Its session has not moved since.'), 'It was asked: ' + clip(r['ask'], 320)]
    elif k == 'session':
        out = [f'A session{on}. It stopped {r["stopped"]}: {r.get("why", "")}.'] + (['Last asked of it: ' + clip(r['ask'], 320)] if r.get('ask') else [])
    elif k == 'paused':
        out = [f'A session{on} that you stopped yourself, {r["stopped"]}. It waits for no agent unless you queue it.'] + (['Last asked of it: ' + clip(r['ask'], 320)] if r.get('ask') else [])
    elif k == 'handoff' and r.get('fault'):
        out = [f'{r.get("file", "")} is a handoff file the index does not know, last changed {r["stopped"]}.', 'No agent is pointed to it. Queued, a leg reads it and registers it or says why it should go.']
    elif k == 'handoff':
        out = [f'A handoff written for the next agent: {r.get("file", "")}.', clip(r['what'], 320), f'Nobody has opened or changed it since {r["stopped"]}.']
    elif k == 'unit':
        out = [f'A unit written for the relay on {r["stopped"]} that never reached its queue ({r.get("file", "")}).', 'It asks: ' + clip(r['ask'], 320)]
    elif k == 'relay':
        out = [f'The relay ran {r.get("legs", 0)} leg{"" if r.get("legs") == 1 else "s"} of this unit, the last on {r["stopped"]}. It is not done{": " + str(r["verdict"]).lower() if r.get("verdict") else ""}.',
               'It asks: ' + clip(r['ask'], 240)] + (['The last leg said: ' + r['report']] if r.get('report') else [])
    elif k == 'ready':
        out = [r['what'], f'Ready since {r["stopped"]}{", for the " + r["role"] + " role" if r.get("role") else ""}.']
    else:
        out = list(r.get('lines') or [])
    out = [l for l in out if l]
    last = ''
    if r.get('state') == 'queued':
        last = (f'You queued it for the relay{" on " + r["said"] if r.get("said") else ""}{": unit " + r["unit"] if r.get("unit") else ""}. It waits for the master to add it to the relay\'s queue. '
                'Until then you can take it back.')
    elif r.get('state') == 'relay' and k != 'relay':
        legs = r.get('legs', 0)
        last = (f'The relay has it as unit {r["unit"]}: ' + (f'{legs} leg{"" if legs == 1 else "s"} run so far{", the last stop said " + str(r["verdict"]).lower() if r.get("verdict") else ""}.' if legs else 'it waits its turn in the queue.')
                + ' It leaves this list when the relay finishes it.')
    elif r.get('refused'):
        last = f'You queued it and it could not be written as a unit. {r["refused"]}'
    return out[:4] + [last] if last else out[:5]


def where_it_was(r):
    """What a leg needs to find the work a cut-off session or agent left: the station, the folder and branch, the
    transcript and how to read on from it. A transcript is on one machine: the lines say which, and carry its last
    turns for a leg that runs on the other."""
    k, out = r['kind'], []
    if k not in ('agent', 'session', 'paused'):
        return ''
    out.append(f'Where it was: station {r.get("where") or "unknown"}' + (f', folder {r["cwd"]}' if r.get('cwd') else '') + (f', branch {r["branch"]}' if r.get('branch') and r['branch'] != 'HEAD' else '') + '.')
    if k == 'agent':
        out.append(f'Its log: {r.get("log", "")}. The session that started it: {r.get("parent", "")} ({r.get("parent_log", "")}).')
    else:
        out.append(f'Its transcript: {r.get("log", "")} (session {r.get("sid", "")}).')
    out.append(f'Those files are on {r.get("where") or "that station"} only. On another station work from what is written here, and look for what it left in the checkouts it names: uncommitted work is on {r.get("where") or "that station"} too.')
    if r.get('first') and r['first'] != r.get('ask'):
        out.append('It was first asked: ' + clip(r['first'], 600))
    if r.get('turns'):
        out.append('Its last lines:\n' + '\n'.join('  ' + t for t in r['turns']))
    return '\n'.join(out)


def unit_for(r):
    """The relay unit a task becomes when he queues it: a file of the relay's queue (id, lane, role, goal, done_when;
    Tools/relay/sources/lane.py load()). Its goal is all a leg on the other station gets: what was asked, where the
    work was and how it ended, his words, and where the picture is. done_when is the relay's own: the unit's id in a
    commit message on the lane. The task leaves the board when the relay's record says the unit is done."""
    uid = unit_id(r)
    lane = r['lane'] if str(r.get('lane', '')).startswith(('lane/sim/', 'lane/show/')) else 'lane/show/' + uid
    says = ''.join(f'\nThe owner said about it: {w}' for w in r.get('words') or [])
    told = TOLD in str(r.get('why', ''))
    head = dict(capture='The owner pressed F10 in the game and left this feedback. Look at the picture and the state file first, then make the change it asks for.',
                agent=('An agent failed before it finished this task. The session that started it was told and went on; nothing says the task was done another way. Check that first, then do what is left of it.' if told else
                       'An agent was cut off before it finished this task, and nobody came back to it. Do the task.'),
                session='A session was cut off in the middle of this work, and nobody came back to it. Find where it stopped and finish it.',
                paused='The owner stopped this session himself and has now queued what it was doing. Find where it stopped and finish it.',
                handoff='This handoff waits for the next agent. Read it whole, then carry on from where it says.',
                ready='This pipeline step is ready and nobody took it.').get(r['kind'], 'This task was left unfinished.')
    how = f'\nIt stopped {r.get("stopped", "")}: {r["why"]}.' if r.get('why') else ''
    was = where_it_was(r)
    goal = (f'{head}{how}\n\n{r.get("ask") or r.get("what") or r["title"]}{says}\n\n' + (was + '\n\n' if was else '')
            + f'If the task only reads (a review, a survey), write what you found as docs/inbox/<today>-all-{uid}.md and commit that. '
            f'Put [{uid}] in the message of the commit that finishes it.')
    check = ("import subprocess,sys;o=subprocess.run(['git','log','--format=%B','" + INTEGRATION + "..HEAD'],capture_output=True).stdout.decode('utf-8','replace');"
             "sys.exit(0 if '[" + uid + "]' in o else 1)")
    return dict(id=uid, lane=lane, role='lane', goal=goal, done_when=['python', '-c', check])


def collect(mine, others=(), shared=(), notes=(), now=None, relay=None, have=None):
    """The board's tasks. `mine` are this station's own rows (local()), `others` the rows the other stations wrote,
    `shared` what both stations read the same (handoffs, units, the relay, ready steps, captures). A row is listed
    when nothing touched it for STALE seconds, or at once when it is marked `now` (a capture). `relay` and `have`
    are what said() reads a queued task's life from. Returns the rows, newest idle first within their kind, and the
    counts the page's head shows: `left` waits for an agent, `captures` wait for him, `paused` are his own stops."""
    now = now or time.time()
    rows, seen = [], set()
    for r in list(shared) + list(mine) + list(others):
        if r['id'] in seen or (not r.get('now') and now - r.get('touched', 0) <= STALE):
            continue
        seen.add(r['id'])
        r = dict(r, idle=max(0, int((now - r.get('touched', now)) // 60)))
        rows.append(r)
    said(rows, notes, relay, have)
    # a task the relay has is shown once, as the task: the relay's own row for its unit stands down
    carried = {'relay-' + r['unit'] for r in rows if r['kind'] != 'relay' and r['state'] in ('queued', 'relay') and r['unit']}
    rows = [r for r in rows if r['id'] not in carried]
    for r in rows:
        r['detail'] = detail(r)
    order = {k: i for i, k in enumerate(KINDS)}
    rows.sort(key=lambda r: (order.get(r['kind'], 99), r['idle']))
    shown = [r for r in rows if r['state'] in SHOWN]
    left = [r for r in shown if r['state'] == 'left']
    return dict(rows=shown, left=sum(1 for r in left if r['kind'] not in ('capture', 'paused')), queued=sum(1 for r in shown if r['state'] == 'queued'),
                with_relay=sum(1 for r in shown if r['state'] == 'relay'), paused=sum(1 for r in left if r['kind'] == 'paused'),
                dropped=sum(1 for r in rows if r['state'] == 'dropped'), done=sum(1 for r in rows if r['state'] == 'done'),
                captures=sum(1 for r in left if r['kind'] == 'capture'), stale_minutes=STALE // 60, blind=BLIND)
