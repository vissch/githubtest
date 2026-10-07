"""The numbers behind the office, counted (graphs.html and the control screen: static/counted.js draws them as
things on a floor and as a plain list), made again on every read of ops.py, so the page follows the work without
anyone redrawing it.

Derived where it can be, kept where it cannot:
- the work of the week (today and the six days before it, each from its midnight), hour by hour and room by room,
  and which branch got it, day by day: from the Claude transcripts on this station (every tool call has a time;
  src_acts.py says which room it is work in). A transcript is read once: a cache keeps, per file, how far it was
  read and what was counted, and the next read takes only what was written since;
- the commits a day on integration: from git;
- the owner's decisions (waiting on him, answered and waiting on the crew, asked and closed in the week): from the
  briefs and the owner queue of the same read (briefs.py, src_queue.py);
- what the relay's legs cost: from the legs' own records (src_relay.py), passed in by ops.py;
- how many were at work and how much waited on the owner over time: nothing holds yesterday's answer, so each read
  leaves a sample in this station's cache (history.jsonl), at most one a SAMPLE_EVERY seconds unless a number moved.

Everything here is this station's view: its transcripts, its history. A branch gets the work of the checkout that is
on it now: a checkout that changed branch in the week takes its week with it.
"""
import collections
import datetime
import json
import subprocess
from pathlib import Path

import src_acts
import src_git
import src_ops
import src_visuals

VERSION = 2              # of the cache: a new number has every transcript read again from its start
DAYS = 7                 # the work of this many days is counted: today and the six before it
COMMIT_DAYS = 30
SAMPLE_EVERY = 300       # seconds between two samples of the floor when nothing moved
KEEP_DAYS = 14           # of history
ROOMS = ('work', 'lab', 'shop', 'studio', 'plan')      # the rooms a call can be work in (nobody calls from the bunkhouse)


def hour_of(t):
    return datetime.datetime.fromtimestamp(t).strftime('%Y-%m-%d %H')


def week_days(now):
    """The days the page counts, oldest first, today the last."""
    day0 = datetime.datetime.fromtimestamp(now).date()
    return [str(day0 - datetime.timedelta(days=k)) for k in range(DAYS - 1, -1, -1)]


def take(f: Path, entry, spell, side=False):
    """Read what was written to a transcript since the last time, and add its tool calls to the file's entry:
    `hours` {hour: {room: calls}}, `named` {checkout: calls that point into it} and `seen`, the last pictures and
    films its calls named (src_visuals.py). A file that got shorter (it was rewritten) is read again from its start."""
    size = f.stat().st_size
    if size < entry.get('off', 0):
        entry.update(off=0, hours={}, named={}, seen=[], cwd=None)
    if size == entry.get('off', 0):
        return entry
    with open(f, 'rb') as h:
        h.seek(entry.get('off', 0))
        data = h.read()
    end = data.rfind(b'\n') + 1                      # a last line still being written waits for the next read
    hours, named, seen = entry.setdefault('hours', {}), entry.setdefault('named', {}), entry.setdefault('seen', [])
    for row in data[:end].split(b'\n'):
        if b'"cwd"' in row[:2000] and not entry.get('cwd'):
            try:
                entry['cwd'] = json.loads(row).get('cwd')
            except ValueError:
                pass
        if b'"tool_use"' not in row:
            continue
        try:
            d = json.loads(row)
        except ValueError:
            continue
        when = src_ops.ts(d.get('timestamp')) if isinstance(d, dict) else None
        if when is None or (d.get('isSidechain') and not side):      # an agent's calls are counted in the agent's own log
            continue
        for c in src_ops.uses(d):
            inp = c.get('input') if isinstance(c.get('input'), dict) else {}
            room = src_acts.of_tool(c.get('name'), inp)
            if room in ROOMS:
                at = hours.setdefault(hour_of(when), {})
                at[room] = at.get(room, 0) + 1
            tree = src_ops.pointed(inp, spell)
            if tree is not None:
                named[str(tree)] = named.get(str(tree), 0) + 1
            src_visuals.keep(seen, when, src_visuals.named(c.get('name'), inp, d.get('cwd') or entry.get('cwd')))
    entry['off'] = entry.get('off', 0) + end
    return entry


def activity(trees, now, cache, projects=None):
    """The tool calls of the week on this station (week_days): per hour and room, per day, and per branch with its
    days and the hours it was busy in. `trees` is {checkout path: {'branch': ...}} (src_ops.checkouts); `cache` is
    kept between reads by the caller."""
    projects = projects or src_ops.PROJECTS
    spell = src_ops.spellings(list(trees))
    files = cache.setdefault('files', {})
    since, seen = now - DAYS * 86400, set()
    logs = list(projects.glob('*/*.jsonl')) + list(projects.glob('*/*/subagents/*.jsonl'))
    for f in logs:
        try:
            if f.stat().st_mtime < since:
                continue
            key = str(f)
            seen.add(key)
            take(f, files.setdefault(key, {}), spell, side=f.parent.name == 'subagents')
        except OSError:
            continue
    for key in [k for k in files if k not in seen]:      # a transcript nobody wrote to for a week, or one that is gone
        del files[key]
    days = week_days(now)
    first = days[0] + ' 00'                             # the week starts at a midnight: a day is whole, or it is today
    hours, lanes = collections.defaultdict(lambda: dict.fromkeys(ROOMS, 0)), {}
    by_path = {str(p): w['branch'] for p, w in trees.items()}
    for key, e in files.items():
        mine = collections.Counter()
        for h, rooms in e.get('hours', {}).items():
            if h < first:
                continue
            for room, k in rooms.items():
                hours[h][room] += k
                mine[h] += k
        # the branch a transcript worked on: where it was started, or, started outside every checkout (an agent's
        # log has its session's folder), the checkout most of its calls point into
        own = e.get('cwd') and src_ops.owner_of(e['cwd'], list(trees))
        tree = str(own) if own else max(e.get('named', {}), key=lambda t: e['named'][t], default=None)
        if mine and tree in by_path and (own or e['named'][tree] >= src_ops.ENOUGH):
            lanes.setdefault(by_path[tree], collections.Counter()).update(mine)
    last = datetime.datetime.fromtimestamp(now).replace(minute=0, second=0, microsecond=0)
    start = datetime.datetime.strptime(first, '%Y-%m-%d %H')
    strip = [(start + datetime.timedelta(hours=k)).strftime('%Y-%m-%d %H') for k in range(int((last - start).total_seconds() // 3600) + 1)]

    def of_day(at, d):
        return sum(k for h, k in at.items() if h.startswith(d))
    rows = [dict(branch=b, calls=sum(at.values()), days=[of_day(at, d) for d in days], busy=sum(1 for k in at.values() if k)) for b, at in lanes.items()]
    return dict(hours=[dict(h=h, **hours[h]) if h in hours else dict(h=h, **dict.fromkeys(ROOMS, 0)) for h in strip],
                days=[dict(day=d, calls=sum(sum(r.values()) for h, r in hours.items() if h.startswith(d)),
                           **{room: sum(r[room] for h, r in hours.items() if h.startswith(d)) for room in ROOMS}) for d in days],
                lanes=sorted(rows, key=lambda r: (-r['calls'], r['branch'])),
                today=sum(sum(r.values()) for h, r in hours.items() if h.startswith(days[-1])),
                week=sum(sum(r.values()) for r in hours.values()))


def commits(repo: Path, now):
    """The commits a day on integration, the last COMMIT_DAYS days, each day there (a day with none is a nought)."""
    try:
        out = src_git.git(repo, 'log', f'--since={COMMIT_DAYS}.days.ago', '--format=%cs', src_git.INTEGRATION)
    except (subprocess.SubprocessError, OSError):
        out = ''
    n = collections.Counter(l.strip() for l in out.split('\n') if l.strip())
    day0 = datetime.datetime.fromtimestamp(now).date()
    return [dict(day=str(day0 - datetime.timedelta(days=k)), n=n.get(str(day0 - datetime.timedelta(days=k)), 0)) for k in range(COMMIT_DAYS - 1, -1, -1)]


def decisions(briefs, answers, waiting, now):
    """The owner's decisions as the page counts them. `waiting`: on him now, the number "Needs you" shows (the owner
    queue's count, src_queue.py). `crew`: he has answered and no session has taken it up (briefs.answers). And of the
    week: `asked`, the briefs put to him, and `closed`, the briefs a session closed with his answer."""
    first = week_days(now)[0]
    return dict(waiting=int(waiting or 0), crew=len(answers or []),
                asked=sum(1 for b in briefs or [] if str(b.get('asked', ''))[:10] >= first),
                closed=sum(1 for b in briefs or [] if b.get('state') == 'answered' and str((b.get('answer') or {}).get('when', ''))[:10] >= first))


def sample(data, now):
    """What is kept of one reading of the floor."""
    workers = [w for l in data['lanes'] for w in l['workers'] if w.get('state') == 'working']
    return dict(t=int(now), at=len(workers), sessions=sum(w['kind'] == 'session' for w in workers),
                needs=(data.get('queue') or {}).get('count', 0), waiting=sum(1 for w in workers if w.get('wait')),
                open=sum(1 for l in data['lanes'] if l['workers'] or l.get('items') or l.get('dirty')))


def history(store: Path, s, now):
    """Add a sample to this station's history when a number moved or SAMPLE_EVERY seconds passed, drop what is older
    than KEEP_DAYS days, and return all of it, oldest first."""
    rows = []
    try:
        for row in store.read_text(encoding='utf-8').split('\n'):
            if row.strip():
                rows.append(json.loads(row))
    except (OSError, ValueError):
        rows = rows or []
    kept = [r for r in rows if r.get('t', 0) >= now - KEEP_DAYS * 86400]
    last = kept[-1] if kept else None
    moved = last is None or any(last.get(k) != s[k] for k in s if k != 't')
    if moved or s['t'] - last['t'] >= SAMPLE_EVERY:
        kept.append(s)
        if len(kept) == len(rows) + 1:               # nothing dropped: only the new line is written
            store.parent.mkdir(parents=True, exist_ok=True)
            with open(store, 'a', encoding='utf-8') as h:
                h.write(json.dumps(s, sort_keys=True) + '\n')
        else:
            tmp = store.with_suffix('.tmp')
            tmp.write_text(''.join(json.dumps(r, sort_keys=True) + '\n' for r in kept), encoding='utf-8')
            tmp.replace(store)
    return kept


def thin(rows, n=400):
    """History for the page: at most about n samples, the newest always among them."""
    step = max(1, len(rows) // n)
    return [r for i, r in enumerate(rows) if (len(rows) - 1 - i) % step == 0]


def collect(repo: Path, data, trees, now, cache_path: Path, store: Path, seen=None, briefs=None, answers=None, relay=None):
    """Everything the office draws, for this reading. `seen`, when given, is filled with what the same pass over the
    transcripts found for src_visuals.py: {transcript: the pictures and films it named}. `briefs` and `answers` are
    briefs.read_all() and briefs.answers(); `relay` is src_relay.week(), or None when the station has no leg records:
    the page then draws no shells and says why."""
    try:
        cache = json.loads(cache_path.read_text(encoding='utf-8'))
    except (OSError, ValueError):
        cache = {}
    if cache.get('version') != VERSION:
        cache = dict(version=VERSION)
    act = activity(trees, now, cache)
    if seen is not None:
        seen.update({k: e.get('seen', []) for k, e in cache['files'].items()})
    tmp = cache_path.with_suffix('.tmp')
    cache_path.parent.mkdir(parents=True, exist_ok=True)
    tmp.write_text(json.dumps(cache), encoding='utf-8')
    tmp.replace(cache_path)
    rows = history(store, sample(data, now), now)
    return dict(rooms=list(ROOMS), hours=act['hours'], days=act['days'], lanes=act['lanes'],
                today=act['today'], week=act['week'], commits=commits(repo, now), relay=relay,
                decisions=decisions(briefs, answers, (data.get('queue') or {}).get('count', 0), now),
                history=thin(rows), since=rows[0]['t'] if rows else int(now))
