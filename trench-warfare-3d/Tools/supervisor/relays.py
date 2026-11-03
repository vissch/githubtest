"""Read the relays' own records on this machine, read-only, in the shape of the replay fixture (replay/records.py).

    import relays
    relays.recent_legs(since_s)       # legs that finished after since_s, or are still running: dicts like the fixture's
    relays.recent_gates(since_s)      # gate jobs that finished after since_s, with the lines of out.log that name a fault
    relays.running_worktrees()        # the work checkouts a leg runs in right now (normalised, lower case)

The homes are %LOCALAPPDATA%/TrenchWarfare/relay, relay2..relay6 (TW_RELAY_HOMES, ';'-separated, in tests). A record
here has the same keys as one in the fixture, so a module's decision is tested on the fixture and fed live from here
with no second reader. The fault lines come from replay/build_fixtures.py's excerpt(): one rule for both.
"""
import calendar
import glob
import json
import os
import time
from pathlib import Path

from replay.build_fixtures import LEG_KEYS, excerpt

TW = Path(os.environ.get('LOCALAPPDATA') or Path.home()) / 'TrenchWarfare'
NAMES = ['relay'] + [f'relay{n}' for n in range(2, 7)]
RUN_DAYS = 2            # a run folder older than this many days (by its name) is not read


def homes():
    """[(relay number as a string, its home)] of the homes that exist."""
    got = os.environ.get('TW_RELAY_HOMES')
    if got:
        return [(str(i + 1), Path(p)) for i, p in enumerate(got.split(';')) if p]
    return [(n[5:] or '1', TW / n) for n in NAMES if (TW / n).is_dir()]


def epoch(iso):
    """'2026-10-31T13:00:46Z' (UTC; '2026-10-31T13:00' and '2026-10-31' too) as seconds since 1970, or 0."""
    s = str(iso or '').rstrip('Z')[:19]
    for fmt in ('%Y-%m-%dT%H:%M:%S', '%Y-%m-%dT%H:%M', '%Y-%m-%d'):
        try:
            return calendar.timegm(time.strptime(s, fmt))
        except ValueError:
            pass
    return 0


def _rj(p):
    try:
        with open(p, encoding='utf-8', errors='replace') as f:
            return json.load(f)
    except (OSError, ValueError):
        return None


def _runs(home, now=None):
    floor = time.strftime('%Y%m%d', time.localtime((now or time.time()) - RUN_DAYS * 86400))
    return [p for p in glob.glob(str(Path(home) / 'runs' / '2026*')) if Path(p).name[:8] >= floor]


def recent_legs(since_s, now=None):
    out = []
    for rn, home in homes():
        for run in _runs(home, now):
            for lj in glob.glob(str(Path(run) / 'legs' / '*' / 'leg.json')):
                d = _rj(lj) or {}
                if d.get('state') != 'RUNNING' and epoch(d.get('finished_at')) < since_s:
                    continue
                rec = dict({k: d.get(k) for k in LEG_KEYS}, t='leg', relay=rn, run=Path(run).name,
                           report=(d.get('report') or '')[:600], worktree=d.get('worktree') or '')
                out.append(rec)
    return out


def recent_gates(since_s, now=None):
    out = []
    for rn, home in homes():
        for run in _runs(home, now):
            for gj in glob.glob(str(Path(run) / 'desk' / '*' / 'jobs' / 'gate-*' / 'run.json')):
                d = _rj(gj) or {}
                if epoch(d.get('finished_at')) < since_s:
                    continue
                parts = Path(gj).parts
                out.append({'t': 'gate', 'relay': rn, 'run': Path(run).name, 'leg': parts[-4], 'job': parts[-2],
                            'started_at': d.get('started_at'), 'finished_at': d.get('finished_at'),
                            'exit_code': d.get('exit_code'), 'state': d.get('state'),
                            'lines': excerpt(Path(gj).with_name('out.log'))})
    return out


def norm(path):
    return str(path or '').replace('\\', '/').rstrip('/').lower()


def running_worktrees(now=None):
    """The checkouts of every leg whose leg.json says RUNNING. A leg that died without a record keeps its checkout
    on this list: the census then leaves its Unity alone, which is the safe side."""
    out = set()
    for rn, home in homes():
        for run in _runs(home, now):
            for lj in glob.glob(str(Path(run) / 'legs' / '*' / 'leg.json')):
                d = _rj(lj) or {}
                if d.get('state') == 'RUNNING' and d.get('worktree'):
                    out.add(norm(d['worktree']))
    return out
