#!/usr/bin/env python3
"""The task board: what agents left unfinished, read for the site and for a session, and the owner's word on it.

    python Tools/assetboard/tasks.py                    from trench-warfare-3d/: the tasks, as the site lists them
    python Tools/assetboard/tasks.py unit ID            the relay unit a task becomes when he queues it
    python Tools/assetboard/tasks.py done ID "what you did" --by <your lane>     close a task you finished

src_tasks.py says what a task is and when it is listed. This file reads the records it derives them from (read()),
puts the result in the site (site(): data/tasks.js, drawn by tasks.js and taskboard.js on tasks.html and on the
overview) and takes up what the owner said about a task (act()).

HIS WORD. He chose (2026-10-07) that the button on a task queues it for the relay; the board starts no agent. A click
is a note of his about the task, as every click on the board is. On the next read act() writes the task as a unit
file into units-for-master/, where the master takes the units it adds to the relay's queue, and answers his note
with the unit's name. A task that is a unit already is not written twice; the answer says where it stands. A unit
that cannot be written is said to him on his note, and the task goes back to the list. "Not needed" after "Queue it"
takes the unit file back while the master has not taken it, and says so when the relay already has it.

WHEN SOMETHING IS WRONG the page says so in plain sight: the Drive both stations read is away (nothing is taken in or
written until it is back, so nothing is stranded on one machine: `drive_away`), a reading failed (failed(): the rows
of the reading before stay, marked as old), the watcher stopped (`read_at`, which the page ages).

TWO STATIONS. A subagent's log is on the machine it ran on. Each station writes its own rows to tasks/<host>.json in
the tasks' folder, and reads the other's, so both boards list both. The relay's queue is read off the pipeline
board's origin/main with git (fetched at most every FETCH_EVERY seconds; nothing is checked out or changed).
"""
import argparse
import json
import socket
import subprocess
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import briefs     # noqa: E402
import build      # noqa: E402
import feedback   # noqa: E402
import notes      # noqa: E402
import src_ops    # noqa: E402
import src_tasks  # noqa: E402
import src_visuals  # noqa: E402

FETCH_EVERY = 900       # seconds between two fetches of the pipeline board
BY = 'the task board'
REF = 'origin/main'


def put(path: Path, text):
    """Write a file whole or not at all; an unchanged one is left alone (the Drive uploads what changes)."""
    try:
        if path.read_text(encoding='utf-8') == text:
            return False
    except OSError:
        pass
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name + '.tmp')
    tmp.write_text(text, encoding='utf-8')
    tmp.replace(path)
    return True


def git(board, *args, send=None, timeout=60):
    try:
        p = subprocess.run(['git', '-C', str(board), *args], capture_output=True, input=send, timeout=timeout)
    except (OSError, subprocess.TimeoutExpired):
        return b''
    return p.stdout if p.returncode == 0 else b''


def relay(board, cache, now, fetch=True):
    """The relay's queue as the pipeline board's origin/main has it: {queue {id: unit}, done [ids], legs, stops, as_of}.
    Read with git from the commit, so a board whose checkout is behind or in another session's hands is neither
    needed nor touched. Read again only when origin/main moved."""
    empty = dict(queue={}, done=[], legs=[], stops=[], as_of='')
    if not board or not Path(board).is_dir():
        return empty
    seen = cache.setdefault('relay', {})
    if fetch and now - seen.get('fetched', 0) >= FETCH_EVERY:
        git(board, 'fetch', '--quiet', '--no-tags', 'origin', 'main')
        seen['fetched'] = now
    sha = git(board, 'rev-parse', '--verify', '--quiet', REF).decode().strip()
    if not sha:
        return empty
    if seen.get('sha') == sha and seen.get('data'):
        return seen['data']
    names = [n for n in git(board, 'ls-tree', '-r', '--name-only', REF, 'relay').decode('utf-8', 'replace').splitlines() if n.endswith('.json')]
    out = git(board, 'cat-file', '--batch', send=''.join(f'{REF}:{n}\n' for n in names).encode('utf-8'))
    blobs, at = {}, 0
    for n in names:                                 # each answer: "<sha> blob <size>\n", the bytes, "\n"
        end = out.find(b'\n', at)
        head = out[at:end].split()
        if end < 0 or len(head) != 3 or head[1] != b'blob':
            break
        size = int(head[2])
        try:
            blobs[n] = json.loads(out[end + 1:end + 1 + size].decode('utf-8', 'replace'))
        except ValueError:
            pass
        at = end + 1 + size + 1
    data = dict(empty, as_of=git(board, 'log', '-1', '--format=%ci', REF).decode().strip()[:16])
    for n, d in blobs.items():
        part = n.split('/')
        if not isinstance(d, dict):
            continue
        if part[1] == 'queue' and d.get('id'):
            data['queue'][d['id']] = dict(id=d['id'], lane=d.get('lane', ''), goal=str(d.get('goal', ''))[:600])
        elif part[1] == 'done':
            data['done'].append(d.get('id') or Path(n).stem)
        elif len(part) == 4 and part[2] == 'legs':
            data['legs'].append({k: d.get(k) for k in ('unit', 'state', 'started_at', 'finished_at')} | dict(report=str(d.get('report') or '')[:400]))
        elif len(part) == 4 and part[2] == 'stops':
            data['stops'].append(dict(stopped_at=d.get('stopped_at'), units=d.get('units') or {}))
    seen.update(sha=sha, data=data)
    return data


def stations(where: Path, host, mine, now, write=True):
    """Write this station's own rows where the other station reads them, and read the others'. Returns (their rows,
    [{host, at}] of every station heard from)."""
    folder = where / 'tasks'
    if write:
        body = json.dumps(dict(host=host, rows=mine), sort_keys=True)
        try:
            old = json.loads((folder / f'{host}.json').read_text(encoding='utf-8'))
        except (OSError, ValueError):
            old = {}
        if json.dumps(dict(host=old.get('host'), rows=old.get('rows')), sort_keys=True) != body:      # the time alone is no change
            put(folder / f'{host}.json', json.dumps(dict(host=host, at=int(now), rows=mine), sort_keys=True))
    theirs, heard = [], [dict(host=host, at=int(now))]
    for f in sorted(folder.glob('*.json')) if folder.is_dir() else []:
        if f.stem == host:
            continue
        try:
            d = json.loads(f.read_text(encoding='utf-8'))
        except (OSError, ValueError):
            continue
        if isinstance(d, dict) and isinstance(d.get('rows'), list):
            theirs += [r for r in d['rows'] if isinstance(r, dict) and r.get('id') and r.get('kind') in ('agent', 'session', 'paused', 'capture')]
            heard.append(dict(host=str(d.get('host') or f.stem), at=int(f.stat().st_mtime)))
    return theirs, heard


def read(ready=None, every_note=None, now=None, cache_path=None, projects=None, board=None, host=None, live=True, fetch=True):
    """The tasks, for this reading. `ready` are the queue's ready rows (src_queue.py), `every_note` the notes
    (notes.read_all). `live` is the watcher's reading: it takes the game's captures in and writes this station's rows
    for the other; a session that only wants the list (the command line) reads without either."""
    now, where, host = now or time.time(), src_tasks.root(), host or socket.gethostname()
    cache_path = cache_path or build.LOCAL / 'tasks-cache.json'
    try:
        cache = json.loads(cache_path.read_text(encoding='utf-8'))
    except (OSError, ValueError):
        cache = {}
    if cache.get('v') != 2:
        cache = dict(v=2)
    projects = projects or src_ops.PROJECTS
    gone = src_tasks.away()                         # the Drive is away: nothing is taken in or written where only this station looks
    mine = src_tasks.local(projects, now, cache, host)
    mine += src_tasks.capture_rows([], [dict(b, host=host) for b in feedback.broken(now=now)])     # an F10 that failed, on this station
    theirs, heard = stations(where, host, mine, now, write=live and not gone)
    if live and not gone:
        feedback.take_in(host=host, now=now)
    rel = relay(board, cache, now, fetch=fetch and live)
    units = where / 'units-for-master'
    shared = (src_tasks.capture_rows(feedback.read_all()) + src_tasks.handoffs(where, projects, now, cache)
              + src_tasks.unit_files(units, rel['queue'], rel['done']) + src_tasks.relay_rows(rel, now) + src_tasks.ready_rows(ready, now))
    T = src_tasks.collect(mine, theirs, shared, every_note if every_note is not None else notes.read_all(notes.folder()), now, rel, src_tasks.unit_names(units))
    T['relay'] = dict(waiting=len([u for u in rel['queue'] if u not in rel['done']]), as_of=rel['as_of'], taken=sorted(rel['queue']))
    T['stations'] = heard
    T['read_at'] = int(now)
    T['drive_away'] = gone
    T['waiting_here'] = feedback.waiting(now=now) if gone else 0
    if live:
        put(cache_path, json.dumps(cache, sort_keys=True))
    return T


def act(T, notes_where: Path = None, units: Path = None, now=None):
    """Take up what he said: a task he queued becomes a unit file for the relay, once, and his note is answered with
    its name; a task he dropped has its note answered, so it does not stay open. Returns what was done, as lines."""
    notes_where, units, did = notes_where or notes.folder(), units or src_tasks.root() / 'units-for-master', []
    if T.get('drive_away'):
        return ['the Drive is away: nothing he said was taken up, it is when the Drive is back']
    for r in T['rows']:
        if r['state'] not in ('queued', 'relay') or not r.get('note') or (r.get('answered') and r.get('unit')):
            continue
        if r['kind'] in ('unit', 'relay') or r['state'] == 'relay':
            uid = src_tasks.unit_id(r)
            text = f'Unit {uid}: ' + ('it is a unit file already, in units-for-master. The master adds it to the relay\'s queue.' if r['kind'] == 'unit' else 'it is in the relay\'s queue already and runs again in its turn.')
        else:
            u = src_tasks.unit_for(r)
            bad = briefs.check_then('queued', u)
            if bad:
                # said to him where he said it, and the task goes back to the list: a row that says "queued" with no unit is a lie
                why = 'Not queued: the task could not be written as a unit of the relay (' + '; '.join(bad) + ').'
                try:
                    notes.answer(notes_where, r['note'], why, by=BY, now=now)
                    r['state'], r['refused'] = 'left', why
                    r['detail'] = src_tasks.detail(r)
                except (OSError, ValueError) as e:
                    why += f' Its note was not answered ({e})'
                did.append(f'{r["id"]}: not written as a unit ({"; ".join(bad)})')
                continue
            uid = u['id']
            if not (units / f'{uid}.json').exists():
                put(units / f'{uid}.json', json.dumps(u, indent=1, sort_keys=True) + '\n')
            text = f'Unit {uid}: written to units-for-master for the relay. It runs once the master has added it to the queue.'
        try:
            notes.answer(notes_where, r['note'], text, by=BY, now=now)
        except (OSError, ValueError) as e:
            did.append(f'{r["id"]}: its note was not answered ({e})')
            continue
        r['unit'] = uid
        r['detail'] = src_tasks.detail(r)
        did.append(f'{r["id"]}: queued as {uid}')
    taken = set((T.get('relay') or {}).get('taken') or [])
    for n in notes.read_all(notes_where):
        if n['state'] != 'done' and n.get('from', 'owner') == 'owner' and str(n.get('about', '')).startswith('task: ') and n['text'].strip() == src_tasks.DROP_SAY:
            # the unit he queued it as goes with it, while it is only a file for the master: left there, the master
            # still queues what he said is not needed. A unit file somebody else wrote (a unit- row) is not the board's to remove.
            tid = n['about'][len('task: '):]
            uid = src_tasks.unit_id(dict(id=tid, kind=tid.split('-', 1)[0]))
            text = 'Taken off the task board.'
            if uid.startswith('task-') and uid in taken:
                text += f' The relay already has it as unit {uid} and will still run it: tell the master to take it out of the queue.'
            elif uid.startswith('task-') and (units / f'{uid}.json').exists():
                (units / f'{uid}.json').unlink()
                text += f' Its unit file {uid}.json was taken back before the relay had it.'
            notes.answer(notes_where, n['id'], text, by=BY, now=now)
            did.append(f'{tid}: dropped')
    return did


def failed(out: Path, why, now=None):
    """A reading of the tasks failed. The page keeps the rows of the reading before, and says in plain sight that
    they are old and why: left as it was, an old page looks like a current one."""
    path = out / 'data' / 'tasks.js'
    try:
        text = path.read_text(encoding='utf-8')
        T = json.loads(text[len('window.TASKS = '):].rstrip().rstrip(';'))
    except (OSError, ValueError):
        T = dict(rows=[])
    T['failed'] = dict(at=int(now or time.time()), why=str(why)[:300])
    return put(path, f'window.TASKS = {json.dumps(T, sort_keys=True)};\n')


def site(T, out: Path):
    """Put the tasks in the site: data/tasks.js, and a capture's picture as img/task/<id>.jpg (made once)."""
    rows = []
    for r in T['rows']:
        r = dict(r)
        src, shots = r.pop('picture', ''), []
        if src:
            had = [f for f in (out / 'img' / 'task').glob(r['id'] + '.*')] if (out / 'img' / 'task').is_dir() else []
            dst = had[0] if had else src_visuals.shrink(Path(src), out / 'img' / 'task' / (r['id'] + '.jpg'))
            if dst:
                shots = [dict(src=dst.relative_to(out).as_posix(), name='the screen when you pressed F10')]
        r['shots'] = shots
        r.pop('ask', None)                          # what a unit is written from: a session's to read, too long for a page
        for k in ('lines', 'log', 'parent_log', 'parent', 'sid', 'cwd', 'branch', 'first', 'turns'):      # a unit's too, and paths of one machine
            r.pop(k, None)
        rows.append(r)
    return put(out / 'data' / 'tasks.js', f'window.TASKS = {json.dumps(dict(T, rows=rows, relay={k: v for k, v in T["relay"].items() if k != "taken"}), sort_keys=True)};\n')


def idle(minutes):
    return f'{minutes} min' if minutes < 120 else f'{minutes // 60} h' if minutes < 2880 else f'{minutes // 1440} days'


def lines(T):
    """The tasks as text, for a session."""
    out = [f'{T["left"]} left unfinished wait for an agent, {T["captures"]} captures from the game wait for the owner, {T["queued"]} queued for the relay, {T.get("with_relay", 0)} with the relay; '
           f'{T["relay"]["waiting"]} units wait in the relay\'s queue' + ('; THE DRIVE IS AWAY: nothing is taken in or written' if T.get('drive_away') else '')]
    kind = None
    for r in T['rows']:
        if r['kind'] != kind:
            kind = r['kind']
            out.append(f'\n{kind}:')
        out.append(f'  {r["id"]:44} {"QUEUED " if r["state"] == "queued" else "WITH THE RELAY " if r["state"] == "relay" else ""}idle {idle(r["idle"])}{" on " + r["where"] if r.get("where") else ""}  {r["title"]}')
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description='the tasks agents left unfinished')
    ap.add_argument('verb', nargs='?', default='list', choices=('list', 'unit', 'done'))
    ap.add_argument('id', nargs='?', default='')
    ap.add_argument('text', nargs='?', default='')
    ap.add_argument('--by', default='')
    args = ap.parse_args(argv)
    if args.verb == 'done':
        if not args.id or not args.text or not args.by:
            print('done ID "what you did" --by <your lane>')
            return 1
        n = notes.write(notes.folder(), args.text, kind='queue', about='task: ' + args.id, title=args.id, who=args.by)
        notes.answer(notes.folder(), n['id'], 'Closed with the task.', by=args.by)
        print(f'{args.id}: closed by {args.by}')
        return 0
    import ops
    T = read(board=ops.board_root(), live=False)
    if args.verb == 'unit':
        hits = [r for r in T['rows'] if r['id'] == args.id]
        if not hits:
            print(f'{args.id}: no such task on the board')
            return 1
        print(json.dumps(src_tasks.unit_for(hits[0]), indent=1, sort_keys=True))
        return 0
    print('\n'.join(lines(T)))
    return 0


if __name__ == '__main__':
    sys.exit(main())
