"""Read the replay fixture (build_fixtures.py) for a module's tests.

    from replay import records
    rs = records.load()                       # the committed fixture of 2026-10-19..11-02
    rs.legs, rs.runs, rs.gates                # lists of dicts, oldest first
    rs.between('2026-10-31T11:00', '2026-10-31T14:00')   # a Records of only those times
    rs.timeline()                             # every record in time order, for a module that steps through time

Times are UTC ISO strings ('2026-10-31T13:00:46Z'); the desktop is UTC+1 (UTC+2 before 2026-10-25).
"""
import gzip
import json
from pathlib import Path

FIXTURES = Path(__file__).resolve().parent / 'fixtures'
DEFAULT = FIXTURES / 'relays_20261019_20261103.jsonl.gz'


def when(r):
    """The record's time: a leg or gate by its start, a run by its stop (when its outcome is known)."""
    if r.get('t') == 'run':
        return r.get('stopped_at') or r.get('started_at') or ''
    return r.get('finished_at') or r.get('started_at') or ''


class Records:
    def __init__(self, recs):
        self.all = sorted(recs, key=when)
        self.runs = [r for r in self.all if r['t'] == 'run']
        self.legs = [r for r in self.all if r['t'] == 'leg']
        self.gates = [r for r in self.all if r['t'] == 'gate']

    def between(self, a, b):
        return Records([r for r in self.all if a <= when(r) < b])

    def timeline(self):
        return list(self.all)


def load(path=None):
    with gzip.open(path or DEFAULT, 'rt', encoding='utf-8') as f:
        return Records([json.loads(l) for l in f if l.strip()])
