"""Every agent that is not a Claude session of the owner's, read for the floor, and the floor shared between stations.

src_ops.py reads the Claude sessions and their subagents. Here is the rest of who works, each read from what its
tool leaves on disk, so this needs none of their code:
- Claude's own list of the sessions that run now: ~/.claude/sessions/<pid>.json (pid, sessionId, status busy or idle,
  name). src_ops asks it whether a quiet session is still at work, and whether the parent of a quiet subagent lives.
- Codex: ~/.codex/state_5.sqlite, table `threads` (title, model, cwd, tokens, when it last moved; a thread another
  thread spawned says so in `source`). The rollout files are no sign of life: one stood still for three hours while
  its thread worked (2026-10-09).
- Grok: ~/.grok/sessions/<folder>/<session>/summary.json and events.jsonl beside it (turn_started, turn_ended).
- the relay's legs: src_relay.runs() has them; a leg that runs is a worker of its own, sent by its run.
- a second opinion (Tools/relay/second.py: Codex or Grok as critic or reader): <relay home>/second/<stamp>-<vendor>-
  <kind>-<pid>/ and <relay home>/runs/<run>/second/<nn>-r<round>-<vendor>/. It writes record.json only when it
  ends, so a folder whose out.jsonl moves and that has no record yet is a run that is going.

Every worker says who made it (`vendor`: claude, codex, grok) and may say who sent it (`parent`, another worker's
`uid` or `id`), its `model`, and since when (`since`, HH:MM).

The floor of one station is a file both stations see: <Drive>/TW3D-pipeline/floor/<host>.json (TW_FLOOR names another
folder), written by `publish` on every read and by floor.py on a station that runs no board. `others` reads the files
of the other stations: the house shows both. A file that stopped moving is said so (`stations`), and its workers
rest: a station that went silent must not look like a station with nobody at work.

Reads only, but for `publish`.
"""
import datetime
import json
import os
import re
import socket
import sqlite3
import sys
import time
import urllib.parse
from pathlib import Path

import src_acts

HOME = Path(os.path.expanduser('~'))
CODEX_FRESH = 5 * 60        # a Codex thread that moved this recently is at work, unless its rollout says its task is over ...
CODEX_TASK = 30 * 60        # ... and one whose rollout says a task is open, for this long (a long command moves nothing)
GROK_FRESH = 5 * 60         # a Grok session whose events moved this recently is at work ...
GROK_TURN = 30 * 60         # ... or one in a turn that has not ended, for this long (a long tool call writes nothing)
SECOND_FRESH = 30 * 60      # a second opinion with no record yet whose folder moved this recently is going (Grok: 6 to 13 minutes a run)
BEAT = 60                   # a floor nobody moved on is still written this often: the file's time is the station's sign of life
STALE = 5 * 60              # a station's floor older than this: the station went silent, its workers rest (the Drive may take a minute or two, and the two clocks differ)
GONE = 30 * 60              # ... older than this: they are not shown
FORGET = 24 * 3600          # ... older than this: the station itself is not named any more
OTHER = 'other work'        # the lane of work that is in no checkout of this repo
VENDORS = ('claude', 'codex', 'grok')


def claude_home():
    return Path(os.environ.get('TW_CLAUDE_HOME') or HOME / '.claude')


def codex_home():
    return Path(os.environ.get('TW_CODEX_HOME') or HOME / '.codex')


def grok_home():
    return Path(os.environ.get('TW_GROK_HOME') or HOME / '.grok')


def host():
    return os.environ.get('TW_FLOOR_HOST') or socket.gethostname()


def read(p):
    try:
        return json.loads(Path(p).read_text(encoding='utf-8'))
    except (OSError, ValueError):
        return None


def clock(t):
    """A time in seconds as HH:MM, or '' for anything that is not one."""
    try:
        return datetime.datetime.fromtimestamp(t).strftime('%H:%M') if t else ''
    except (TypeError, ValueError, OverflowError, OSError):
        return ''


def stamp(s):
    """A time the tools write (2026-10-09T22:01:51.931520300Z) as seconds, or None."""
    m = re.match(r'(\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d)', str(s or ''))
    if not m:
        return None
    return datetime.datetime.strptime(m.group(1), '%Y-%m-%dT%H:%M:%S').replace(tzinfo=datetime.timezone.utc).timestamp()


def short(s, n=110):
    s = re.sub(r'\s+', ' ', str(s or '')).strip()
    return s if len(s) <= n else s[:n - 1].rstrip() + '…'


def alive(pid, born=None):
    """Whether a process with this id runs. (os.kill(pid, 0) ends the process on Windows: never that.) `born`: the
    time it started as Claude writes it (procStart, Windows' own count); a process with this id that started at
    another time is another process (the id was given out again), so the answer is no."""
    try:
        pid = int(pid)
    except (TypeError, ValueError):
        return False
    if pid <= 0:
        return False
    if sys.platform != 'win32':
        try:
            os.kill(pid, 0)
            return True
        except PermissionError:
            return True
        except OSError:
            return False
    import ctypes
    k = ctypes.WinDLL('kernel32')                   # a handle of our own: the shared one's settings are not ours to change
    k.OpenProcess.restype = ctypes.c_void_p
    h = k.OpenProcess(0x1000, False, pid)           # PROCESS_QUERY_LIMITED_INFORMATION
    if not h:
        return False
    code, times = ctypes.c_ulong(), [ctypes.c_ulonglong() for _ in range(4)]
    ok = k.GetExitCodeProcess(ctypes.c_void_p(h), ctypes.byref(code))
    timed = k.GetProcessTimes(ctypes.c_void_p(h), *[ctypes.byref(t) for t in times])
    k.CloseHandle(ctypes.c_void_p(h))
    if not ok or code.value != 259:                 # STILL_ACTIVE
        return False
    try:
        return not born or not timed or abs(times[0].value - int(born)) < 50_000_000      # within five seconds of each other
    except (TypeError, ValueError):
        return True


# ---- Claude's own list of running sessions ------------------------------------------------------------------------

def registry(is_alive=alive):
    """sessionId -> dict(pid, status, name, entrypoint, since) for every Claude session whose process runs."""
    out = {}
    folder = claude_home() / 'sessions'
    for f in sorted(folder.glob('*.json')) if folder.is_dir() else []:
        d = read(f)
        if not isinstance(d, dict) or not d.get('sessionId') or not is_alive(d.get('pid'), d.get('procStart')):
            continue
        began = d.get('startedAt')
        out[str(d['sessionId'])] = dict(pid=d.get('pid'), status=str(d.get('status') or ''), entrypoint=str(d.get('entrypoint') or ''),
                                        since=began / 1000 if isinstance(began, (int, float)) else None)
    return out


# ---- Codex --------------------------------------------------------------------------------------------------------

def codex_rows(db: Path, since):
    """The threads that moved after `since`, as dicts. The file is opened to read only; a database another process
    holds in a way that refuses that is read as it lies on disk."""
    if not db.is_file():
        return []
    # with no write-ahead file beside it nothing is pending and Codex is shut: read as it lies, which makes no file
    # there; with one, read through it (the threads that just moved are in it)
    for mode in (('mode=ro', 'immutable=1') if db.with_name(db.name + '-wal').exists() else ('immutable=1',)):
        try:
            con = sqlite3.connect(f'file:{urllib.parse.quote(db.as_posix())}?{mode}', uri=True, timeout=2)
            try:
                con.row_factory = sqlite3.Row
                return [dict(r) for r in con.execute('select * from threads where updated_at >= ? and coalesce(archived, 0) = 0 order by updated_at', (int(since),))]
            finally:
                con.close()
        except sqlite3.Error:
            continue
    return []


def codex_task(rollout):
    """What the end of a thread's rollout says of its task: 'open' (started, not ended), 'over', or '' when it says
    nothing (a thread that only waits on the threads it spawned writes none of this for a long time)."""
    last = ''
    for line in last_lines(Path(rollout), 256 * 1024) if rollout else []:
        m = re.search(r'"type"\s*:\s*"(task_started|task_complete|turn_aborted)"', line[:400])
        if m:
            last = 'open' if m.group(1) == 'task_started' else 'over'
    return last


def codex(now, fresh=CODEX_FRESH, task=CODEX_TASK):
    """Every Codex thread at work: (where its folder is, worker). A thread another one spawned names it (`parent`).
    The thread's own time moves after its task is over as well (2026-10-10: five threads that had ended stood on the
    floor for five minutes each), so the rollout's last word on the task decides when it has one."""
    out = []
    for r in codex_rows(codex_home() / 'state_5.sqlite', now - task):
        said = codex_task(r.get('rollout_path'))
        if said == 'over' or (said != 'open' and now - (r.get('updated_at') or 0) > fresh):
            continue
        tid = str(r.get('id') or '')
        parent = ''
        try:
            src = json.loads(r.get('source') or '')
            sub = src.get('subagent') if isinstance(src, dict) else None
            spawn = sub.get('thread_spawn') if isinstance(sub, dict) else None
            parent = str(spawn.get('parent_thread_id') or '') if isinstance(spawn, dict) else ''
        except ValueError:
            pass
        nick = r.get('agent_nickname') or ''
        role = (r.get('agent_path') or '').rsplit('/', 1)[-1] or r.get('agent_role') or ''
        what = short(r.get('title') or r.get('name') or r.get('first_user_message') or r.get('preview') or role.replace('_', ' '))
        w = dict(kind='agent', vendor='codex', id='agent:codex', uid='codex:' + tid[-8:], name=f'Codex {nick}'.strip() if parent else 'Codex',
                 what=what, state='working', act=src_acts.of_agent(role, what) if parent or what else 'work', model=r.get('model') or '',
                 since=clock(r.get('created_at')), tokens=r.get('tokens_used') or 0)
        if parent:
            w['parent'] = 'codex:' + parent[-8:]
        out.append((re.sub(r'^\\\\\?\\', '', r.get('cwd') or ''), w))
    return out


# ---- Grok ---------------------------------------------------------------------------------------------------------

def last_lines(f: Path, tail=64 * 1024):
    """The lines in the last `tail` bytes of a file; none when it cannot be read."""
    try:
        with open(f, 'rb') as h:
            h.seek(max(0, f.stat().st_size - tail))
            return h.read().decode('utf-8', 'replace').splitlines()
    except OSError:
        return []


def grok_turn(events: Path):
    """Whether the last turn of a Grok session is still open: a turn_started with no turn_ended after it."""
    for line in reversed(last_lines(events)):
        m = re.search(r'"type"\s*:\s*"(turn_started|turn_ended)"', line)
        if m:
            return m.group(1) == 'turn_started'
    return False


def grok(now, fresh=GROK_FRESH, turn=GROK_TURN):
    """Every Grok session at work: (where its folder is, worker). A session folder with no events is a sign-in probe."""
    out = []
    root = grok_home() / 'sessions'
    for ev in sorted(root.glob('*/*/events.jsonl')) if root.is_dir() else []:
        try:
            age = now - ev.stat().st_mtime
        except OSError:
            continue
        if age > fresh and not (age <= turn and grok_turn(ev)):
            continue
        s = read(ev.parent / 'summary.json')
        s = s if isinstance(s, dict) else {}
        info = s.get('info') if isinstance(s.get('info'), dict) else {}
        what = short(s.get('last_turn_summary') or s.get('generated_title') or s.get('session_summary') or '')
        title = short(s.get('generated_title') or s.get('session_summary') or '', 60)
        w = dict(kind='agent', vendor='grok', id='agent:grok', uid='grok:' + ev.parent.name[-8:], name='Grok', title=title, what=what, state='working',
                 act=src_acts.trade(f'{s.get("agent_name", "")} {title}') or 'work', model=s.get('current_model_id') or '', since=clock(stamp(s.get('created_at'))))
        out.append((info.get('cwd') or '', w))
    return out


# ---- the relay's legs and the second opinions ---------------------------------------------------------------------

PHASES = {'plan': 'plan', 'execute': 'work', 'evidence': 'lab', 'critic': 'lab', 'review': 'lab'}


def legs(runs):
    """The legs that run now, each a worker its run sent: (lane, worker). `runs` is src_relay.runs()."""
    out = []
    for run in runs:
        for l in run.get('legs') or []:
            if l.get('state') != 'RUNNING':
                continue
            role = l.get('role') or ''
            bits = [b for b in (role, f'{l["minutes"]} min' if l.get('minutes') is not None else '',
                                f'{round(l["tokens"] / 1000)}k tokens' if l.get('tokens') else '') if b]
            unit = ' '.join(p for p in (l.get('unit') or run.get('unit') or '').split('--')[:2] if p)
            w = dict(kind='agent', vendor='claude', id='agent:relay-leg', uid=f'leg:{run["run"]}-{l.get("leg")}', name=f'relay leg {l.get("leg")}',
                     what=short(f'{unit}: {l.get("phase") or "leg"}' + (f' ({", ".join(bits)})' if bits else '')), state='working',
                     act=PHASES.get(l.get('phase') or '') or src_acts.of_agent(role, unit), model=l.get('model') or '', parent='agent:relay',
                     tokens=l.get('tokens') or 0)
            if l.get('session'):
                w['session'] = l['session']
            out.append((run.get('lane') or OTHER, w))
    return out


SECOND = re.compile(r'(?:^|-)(codex|grok)(?:-([a-z]+))?(?:-\d+)?$')


def seconds(homes, now, fresh=SECOND_FRESH, lanes=None):
    """The second opinions that are going in these relay homes: (lane, worker). Their folders name the vendor. One
    asked for inside a relay run is on that run's lane (`lanes`: run -> lane), any other in OTHER."""
    out = []
    for home in homes:
        home = Path(home)
        folders = list(home.glob('second/*')) + list(home.glob('runs/*/second/*'))
        for d in sorted(p for p in folders if p.is_dir()):
            m = SECOND.search(d.name)
            if not m or (d / 'record.json').exists():
                continue
            marks = [f for f in (d / 'out.jsonl', d / 'prompt.txt') if f.exists()]
            try:
                newest = max(f.stat().st_mtime for f in marks) if marks else None
                began = min(f.stat().st_mtime for f in marks) if marks else None
            except OSError:
                continue
            if newest is None or now - newest > fresh:
                continue
            vendor, kind = m.group(1), m.group(2) or 'critic'
            in_run = d.parent.parent.name if d.parent.parent.parent.name == 'runs' else ''
            w = dict(kind='agent', vendor=vendor, id='agent:second', uid=f'second:{(in_run + "-" if in_run else "") + d.name}'[-48:], name=f'{vendor.capitalize()} second {kind}',
                     what=f'a second {kind}' + (f' for relay run {in_run}' if in_run else ''), state='working', act='lab', since=clock(began))
            if in_run:
                w['parent'] = 'agent:relay'
            out.append(((lanes or {}).get(in_run) or OTHER, w))
    return out


def second_homes(relay_homes):
    """The relay homes to look in for second opinions: this station's, and the ones TW_RELAY_HOMES_ALSO names (a
    measurement run keeps a home of its own)."""
    out = [Path(h) for h in relay_homes]
    for p in os.environ.get('TW_RELAY_HOMES_ALSO', '').split(os.pathsep):
        if p.strip() and Path(p.strip()) not in out:
            out.append(Path(p.strip()))
    return out


# ---- the floor, shared between stations -----------------------------------------------------------------------------

def folder():
    """Where the stations' floor files are: TW_FLOOR, else the Drive both see, else none (a station with no Drive
    shows its own floor only)."""
    if os.environ.get('TW_FLOOR'):
        return Path(os.environ['TW_FLOOR'])
    import build
    return build.DRIVE / 'floor' if build.DRIVE.is_dir() else None


KEEP = ('kind', 'vendor', 'id', 'uid', 'name', 'title', 'what', 'doing', 'state', 'act', 'wait', 'model', 'parent', 'since', 'tokens', 'where', 'age', 'going')


def same(a, b, loose=('age', 'tokens', 'doing')):
    """Whether two floors hold the same workers at the same work (the figures that move on every read left out)."""
    def firm(ws):
        return [{k: v for k, v in w.items() if k not in loose} for w in ws or []]
    return firm(a) == firm(json.loads(json.dumps(b)))


def publish(rows, now=None, where=None, me=None):
    """Write this station's floor: `rows` is [(lane, worker)]. Written beside and moved into place, so the other
    station never reads half a file. Returns the file, or None when there is nowhere to write."""
    where = where or folder()
    if where is None:
        return None
    me, now = me or host(), now or time.time()
    had = read(where / f'{me}.json')
    had = had if isinstance(had, dict) and isinstance(had.get('at'), (int, float)) else {}
    data = dict(host=me, at=int(now), workers=[dict({k: w[k] for k in KEEP if w.get(k) not in (None, '')}, lane=lane) for lane, w in rows])
    if same(had.get('workers'), data['workers']) and now - (had.get('at') or 0) < BEAT:
        return where / f'{me}.json'             # nothing moved: the Drive is not made to carry the same file again
    try:
        where.mkdir(parents=True, exist_ok=True)
        tmp = where / f'{me}.json.tmp'
        tmp.write_text(json.dumps(data, sort_keys=True), encoding='utf-8')
        os.replace(tmp, where / f'{me}.json')
    except OSError:
        return None
    return where / f'{me}.json'


def others(now, where=None, me=None):
    """What the other stations wrote: (stations, rows). stations = [dict(host, age, stale, workers)], rows = [(lane,
    worker)] with `host` on each. The workers of a station gone silent rest; after GONE seconds they are not shown."""
    where = where or folder()
    me = me or host()
    stations, rows, newest = [], [], {}
    for f in sorted(where.glob('*.json')) if where and where.is_dir() else []:
        d = read(f)
        if not isinstance(d, dict) or not d.get('host') or str(d['host']).lower() == me.lower() or not isinstance(d.get('at'), (int, float)):
            continue
        if now - d['at'] > FORGET:
            continue                                    # a station not heard of for a day is not a station of this floor (a file left behind)
        had = newest.get(str(d['host']).lower())       # two files of one station (the Drive keeps a copy when two writes cross): the newer one is its floor
        if had is None or (d.get('at') or 0) > (had.get('at') or 0):
            newest[str(d['host']).lower()] = d
    for d in newest.values():
        age = max(0, int(now - (d.get('at') or 0)))
        stale = age > STALE
        said = d.get('workers') if isinstance(d.get('workers'), list) else []
        workers = [w for w in said if isinstance(w, dict) and all(isinstance(w.get(k), str) and w[k] for k in ('id', 'kind', 'state'))] if age <= GONE else []
        stations.append(dict(host=d['host'], age=age, stale=stale, workers=len(workers)))
        for w in workers:
            w = dict(w, host=d['host'])
            lane = w.pop('lane', '') or OTHER
            if stale:
                w.update(state='resting', act='bunk')
                w.pop('wait', None)
            rows.append((lane, w))
    return stations, rows


def count(workers):
    """The floor in numbers, for the line above the house: those at work, by what they are. Each once: a skill two
    sessions took up is one, and a relay run is counted by its running leg (`going`), or as one leg when it is between two."""
    at = list({w.get('uid') or w.get('id'): w for w in workers if w.get('state') == 'working' and not (w.get('id') == 'agent:relay' and w.get('going'))}.values())

    def n(test):
        return sum(1 for w in at if test(w))
    legs_n = n(lambda w: w.get('id') in ('agent:relay-leg', 'agent:relay'))       # a run between two legs counts as the leg it is about to start
    return dict(working=len(at), sessions=n(lambda w: w['kind'] == 'session'),
                subagents=n(lambda w: w['kind'] == 'agent' and w.get('vendor', 'claude') == 'claude' and w.get('id') not in ('agent:relay-leg', 'agent:relay')),
                relay=legs_n, skills=n(lambda w: w['kind'] in ('skill', 'role')), machines=n(lambda w: w['kind'] == 'machine'), codex=n(lambda w: w.get('vendor') == 'codex'), grok=n(lambda w: w.get('vendor') == 'grok'))
