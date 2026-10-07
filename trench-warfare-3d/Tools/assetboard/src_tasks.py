#!/usr/bin/env python3
"""The tasks agents left unfinished: what the board's Tasks page lists (tasks.py reads them, tasks.html shows them).

WHY (the owner, 2026-10-07: "a task board where the user can see all the tasks that are spawned by any agent ... we
only start putting them in after no agent has worked on them for longer then 1.5 hours ... so we dont lose track of
unfinished tasks"). Work an agent starts is an agent's to finish, so a task is not listed while anyone is on it. It
is listed once it is unfinished and nothing has touched it for STALE seconds: then nobody is coming back to it.

Nothing here is stored. No session on these stations has a todo tool, so there is no list to read; a task is derived
from the record its work already leaves:
- agent:    a subagent whose log stops on an open tool call, a line nobody answered or a failed call to the service
            (ending()). It is touched when its log or its parent session's transcript moves: a parent that is still
            at work may yet resume it;
- session:  a session's transcript that stops the same way, or on a question to the owner nobody answered;
- handoff:  a handoff the index (handoffs.json, Tools/handoffs.py) calls current, touched when its file changes or a
            transcript that is being written names it;
- unit:     a unit file written for the relay (units-for-master/) whose id never reached the relay's queue;
- relay:    a unit of the relay's queue the relay has run legs of and not finished (tasks.py reads the board's
            origin/main with git, nothing is checked out);
- ready:    a pipeline step ready to take (the queue's `ready` rows, src_queue.py);
- capture:  a feedback capture from the game (F10, feedback.py). It is the owner's and no agent has it, so it is
            listed at once.
Only work on this project is read (NAMES in the folder or the request), and nothing older than DAYS.

What the owner says about a task is a note of his about "task: <id>" (notes.py), like every other word of his on the
board: QUEUE_SAY queues it for the relay (tasks.py writes the unit), DROP_SAY takes it off the list, anything else is
shown with it. A note about it from anyone else says an agent closed it.
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
NAMES = ('trench-warfare-3d', 'trench warfare', 'githubtest', 'tw3d')
ASKS = ('AskUserQuestion', 'ExitPlanMode')
QUEUE_SAY = 'Queue this for the relay.'
DROP_SAY = 'Not needed: drop this task.'
KINDS = ('capture', 'agent', 'session', 'handoff', 'unit', 'relay', 'ready')      # the order the page lists them in
INTEGRATION = 'origin/claude/trench-warfare-2d-3d-plan-idt7lf'
UNIT_ID = re.compile(r'[^\w.-]+')


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
    talk = [d for d in rows if d.get('type') in ('user', 'assistant') and (side or not d.get('isSidechain'))]
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


def names_project(*texts):
    low = ' '.join(str(t or '') for t in texts).lower().replace('\\', '/')
    return any(n in low for n in NAMES)


def read_agent(f: Path):
    """What a subagent's log says of itself: how it ended, what it was asked, where it ran, its last line's time."""
    with open(f, 'rb') as h:
        head = h.read(65536)
    first = (entries(head.split(b'\n', 1)[0]) or [{}])[0]
    rows = entries(tail(f))
    state, why = ending(rows, side=True)
    last = next((stamp(d.get('timestamp')) for d in reversed(rows) if d.get('timestamp')), None)
    try:
        meta = json.loads(f.with_name(f.stem + '.meta.json').read_text(encoding='utf-8'))
    except (OSError, ValueError):
        meta = {}
    ask = text_of(first)
    return dict(end=state, why=why, ask=ask[:ASK], cwd=first.get('cwd') or '', last=last, type=meta.get('agentType') or 'agent',
                desc=meta.get('description') or '', ours=names_project(first.get('cwd'), ask))


def read_session(f: Path):
    """The same of a session's transcript, from its end: its title and last request too, and whether it is work on
    this project (its folder names it, or ENOUGH of its last tool calls do)."""
    rows = entries(tail(f))
    state, why = ending(rows)
    s = dict(end=state, why=why, cwd='', title='', ask='', last=None, named=0)
    typed = ''
    for d in rows:
        s['cwd'] = d.get('cwd') or s['cwd']
        if d.get('type') == 'ai-title':
            s['title'] = d.get('aiTitle', '')
        elif d.get('type') == 'last-prompt':
            s['ask'] = str(d.get('lastPrompt', ''))[:ASK]
        elif d.get('type') == 'user' and not d.get('isSidechain') and text_of(d).strip() and not text_of(d).lstrip().startswith(('<', '[')):
            typed = text_of(d).strip()[:ASK]        # what he typed last: the request, when the log names none
        if d.get('timestamp'):
            s['last'] = stamp(d['timestamp']) or s['last']
        if d.get('type') == 'assistant' and not d.get('isSidechain'):
            for c in (d.get('message') or {}).get('content') or []:
                if isinstance(c, dict) and c.get('type') == 'tool_use' and names_project(json.dumps(c.get('input') or {})):
                    s['named'] += 1
    s['ask'] = s['ask'] or typed
    s['ours'] = names_project(s['cwd']) or s['named'] >= ENOUGH
    del s['named']
    return s


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
    files = cache.setdefault('files', {})
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
        except OSError:
            continue
        title = clip(s['title'] or s['ask'], 70)
        if s['ours'] and s['end'] in ('cut', 'asks'):
            rows.append(dict(id='session-' + f.stem[:8], kind='session', title=title or 'A session', what=clip(s['ask'], 200), ask=s['ask'], lane='', where=host,
                             by='', touched=int(newest), why=s['why'], asks=s['end'] == 'asks', stopped=when(s['last'] or at)))
        # an agent the same session ran again to its end is not lost: the later run is the task, done
        redone = {a['desc'] for _, a, m in agents if a['end'] == 'done' and a['desc']}
        for g, a, moved in agents:
            if a['end'] != 'cut' or not (a['ours'] or s['ours']) or a['desc'] in redone:
                continue
            rows.append(dict(id='agent-' + g.stem[len('agent-'):][:10], kind='agent', title=clip(a['desc'] or a['ask'], 70), what=clip(a['ask'], 200), ask=a['ask'], lane='', where=host,
                             by=title, agent=a['type'], touched=int(max(moved, at)), why=a['why'], stopped=when(a['last'] or moved)))
    for key in [k for k in files if k not in seen]:
        del files[key]
    return rows


def mentions(projects: Path, names, now):
    """When each of these file names was last in an agent's hands: the time of the newest tool call that names it, in
    the logs still being written (moved in the last STALE seconds). Only what a call was given counts: a folder
    listing that happens to show the name is a result, and nobody opened the file."""
    out = {}
    logs = (list(projects.glob('*/*.jsonl')) + list(projects.glob('*/*/subagents/*.jsonl'))) if projects.is_dir() and names else []
    for f in logs:
        try:
            if now - f.stat().st_mtime > STALE:
                continue
            rows = entries(tail(f))
        except OSError:
            continue
        for d in rows:
            if d.get('type') != 'assistant':
                continue
            at = stamp(d.get('timestamp')) or 0
            for c in (d.get('message') or {}).get('content') or []:
                if isinstance(c, dict) and c.get('type') == 'tool_use':
                    given = json.dumps(c.get('input') or {})
                    for n in names:
                        if n in given:
                            out[n] = max(out.get(n, 0), at)
    return out


def handoffs(where: Path, projects: Path, now):
    """The handoffs the index calls current, each touched when its file changed or a live log names it."""
    try:
        index = json.loads((where / 'handoffs.json').read_text(encoding='utf-8')).get('handoffs') or {}
    except (OSError, ValueError, AttributeError):
        return []
    current = {n: h for n, h in index.items() if isinstance(h, dict) and h.get('state') == 'current'}
    seen, rows = mentions(projects, list(current), now), []
    for name, h in sorted(current.items()):
        try:
            changed = (where / name).stat().st_mtime
        except OSError:
            continue                                # the index names a file that is gone: nothing to pick up
        rows.append(dict(id='handoff-' + UNIT_ID.sub('-', Path(name).stem.replace('HANDOFF_AGENT_', ''))[:60], kind='handoff', title=clip(h.get('topic') or name, 70), what=clip(h.get('for'), 200),
                         ask=f'Read the handoff {name} in {where} and carry on from it. It is for: {h.get("for", "")}', lane='', where='', by='', file=name,
                         touched=int(max(changed, seen.get(name, 0))), stopped=when(changed)))
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


def relay_rows(relay, now):
    """The units the relay ran legs of and did not finish. `relay` is what tasks.py read off the board: queue {id:
    unit}, done [ids], legs [leg records], stops [stop records]."""
    rows = []
    verdict = {}
    for s in sorted(relay.get('stops') or [], key=lambda s: str(s.get('stopped_at', ''))):
        verdict.update({u: v for u, v in (s.get('units') or {}).items()})
    by = {}
    for leg in relay.get('legs') or []:
        if leg.get('unit'):
            by.setdefault(leg['unit'], []).append(leg)
    for uid, legs in sorted(by.items()):
        u = (relay.get('queue') or {}).get(uid)
        if not u or uid in (relay.get('done') or []):
            continue
        last = max(legs, key=lambda l: str(l.get('finished_at') or l.get('started_at') or ''))
        at = stamp(last.get('finished_at') or last.get('started_at')) or 0
        rows.append(dict(id='relay-' + uid, kind='relay', title=uid, what=clip(u.get('goal'), 200), ask=str(u.get('goal', ''))[:ASK], lane=u.get('lane', ''), where='', by='', legs=len(legs),
                         verdict=verdict.get(uid, ''), report=clip(last.get('report'), 300), touched=int(at), stopped=when(at)))
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


def capture_rows(captures):
    """A feedback capture as a task (feedback.py read_all): the owner's own, listed from the moment it is taken in."""
    rows = []
    for c in captures or []:
        rows.append(dict(id='capture-' + c['id'], kind='capture', title=clip(c.get('note'), 70) or 'A capture with no words', what=clip(c.get('note'), 200), ask=c.get('ask', ''), lane='', where=c.get('host', ''),
                         by='you', touched=int(c.get('at') or 0), stopped=c.get('when', ''), picture=c.get('shot') or '', lines=c.get('lines') or [], now=True))
    return rows


def said(rows, notes):
    """Put on each row what was said about it: `state` (left, queued, dropped, done), his words, and the note that
    queued it. The last of his two set phrases wins; a note from anyone but him closes the task."""
    by = {}
    for n in sorted(notes or [], key=lambda n: str(n.get('when', ''))):
        if str(n.get('about', '')).startswith('task: '):
            by.setdefault(n['about'][len('task: '):], []).append(n)
    for r in rows:
        r['state'], r['words'], r['note'], r['unit'] = 'left', [], '', ''
        for n in by.get(r['id'], []):
            text = str(n.get('text', '')).strip()
            if n.get('from', 'owner') != 'owner':
                r['state'], r['closed'] = 'done', f'{n.get("from")}: {clip(text, 160)}'
            elif text == QUEUE_SAY:
                took = [a for a in n.get('answers') or [] if str(a.get('text', '')).startswith('Unit ')]
                if n.get('state') == 'done' and not took:
                    continue                        # he closed the note before it was taken up: he took it back
                r['state'], r['note'] = 'queued', n['id']
                r['unit'] = took[-1]['text'].split()[1].rstrip(':.') if took else ''
                r['said'] = str(n.get('when', ''))[:16]
            elif text == DROP_SAY:
                r['state'], r['note'] = 'dropped', n['id']
            else:
                r['words'].append(clip(text, 400))
    return rows


def detail(r):
    """At most five short lines that say what a task is, for the panel a click on its row opens."""
    on = f' on {r["where"]}' if r.get('where') else ''
    k = r['kind']
    if k == 'agent':
        out = [f'A {r.get("agent", "")} agent{on}, started by the session “{r["by"]}”.' if r.get('by') else f'A {r.get("agent", "")} agent{on}.',
               f'It stopped {r["stopped"]}, {r.get("why", "")}. Its session has not moved since.', 'It was asked: ' + clip(r['ask'], 320)]
    elif k == 'session':
        out = [f'A session{on}. It stopped {r["stopped"]}: {r.get("why", "")}.'] + (['Last asked of it: ' + clip(r['ask'], 320)] if r.get('ask') else [])
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
    if r.get('state') == 'queued':
        out.append(f'You queued it for the relay{" on " + r["said"] if r.get("said") else ""}{": unit " + r["unit"] if r.get("unit") else ""}. It runs when the master adds it to the queue.')
    return [l for l in out if l][:5]


def unit_for(r):
    """The relay unit a task becomes when he queues it: a file of the relay's queue (id, lane, role, goal, done_when;
    Tools/relay/sources/lane.py load()). Its goal is all a leg on the other station gets: what was asked, his words,
    and where the picture is. done_when is the relay's own: the unit's id in a commit message on the lane."""
    uid = ('task-' + UNIT_ID.sub('-', r['id']))[:80].strip('-.')
    lane = r['lane'] if str(r.get('lane', '')).startswith(('lane/sim/', 'lane/show/')) else 'lane/show/' + uid
    says = ''.join(f'\nThe owner said about it: {w}' for w in r.get('words') or [])
    head = dict(capture='The owner pressed F10 in the game and left this feedback. Look at the picture and the state file first, then make the change it asks for.',
                agent='An agent was cut off before it finished this task, and nobody came back to it. Do the task.',
                session='A session was cut off in the middle of this work, and nobody came back to it. Find where it stopped and finish it.',
                handoff='This handoff waits for the next agent. Read it whole, then carry on from where it says.',
                ready='This pipeline step is ready and nobody took it.').get(r['kind'], 'This task was left unfinished.')
    goal = (f'{head}\n\n{r.get("ask") or r.get("what") or r["title"]}{says}\n\n'
            f'If the task only reads (a review, a survey), write what you found as docs/inbox/<today>-all-{uid}.md and commit that. '
            f'Put [{uid}] in the message of the commit that finishes it.')
    check = ("import subprocess,sys;o=subprocess.run(['git','log','--format=%B','" + INTEGRATION + "..HEAD'],capture_output=True).stdout.decode('utf-8','replace');"
             "sys.exit(0 if '[" + uid + "]' in o else 1)")
    return dict(id=uid, lane=lane, role='lane', goal=goal, done_when=['python', '-c', check])


def collect(mine, others=(), shared=(), notes=(), now=None):
    """The board's tasks. `mine` are this station's own rows (local()), `others` the rows the other stations wrote,
    `shared` what both stations read the same (handoffs, units, the relay, ready steps, captures). A row is listed
    when nothing touched it for STALE seconds, or at once when it is marked `now` (a capture). Returns the rows,
    newest idle first within their kind, and the counts the page's head shows."""
    now = now or time.time()
    rows, seen = [], set()
    for r in list(shared) + list(mine) + list(others):
        if r['id'] in seen or (not r.get('now') and now - r.get('touched', 0) <= STALE):
            continue
        seen.add(r['id'])
        r = dict(r, idle=max(0, int((now - r.get('touched', now)) // 60)))
        rows.append(r)
    said(rows, notes)
    for r in rows:
        r['detail'] = detail(r)
    order = {k: i for i, k in enumerate(KINDS)}
    rows.sort(key=lambda r: (order.get(r['kind'], 99), r['idle']))
    shown = [r for r in rows if r['state'] in ('left', 'queued')]
    return dict(rows=shown, left=sum(1 for r in shown if r['state'] == 'left'), queued=sum(1 for r in shown if r['state'] == 'queued'),
                dropped=sum(1 for r in rows if r['state'] == 'dropped'), done=sum(1 for r in rows if r['state'] == 'done'),
                captures=sum(1 for r in shown if r['kind'] == 'capture' and r['state'] == 'left'), stale_minutes=STALE // 60)
