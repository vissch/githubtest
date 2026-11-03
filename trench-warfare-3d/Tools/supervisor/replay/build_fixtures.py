#!/usr/bin/env python3
"""Build the replay fixture from the relays' real records: what every supervisor module is tested against.

    python Tools/supervisor/replay/build_fixtures.py --since 20261019 --until 20261103 [--out FILE]

Reads, read-only, on the desktop: each relay's stop records (<board>/relay/desktop/stops/*.json), its legs
(<home>/runs/<run>/legs/NN/leg.json) and its gate jobs (<home>/runs/<run>/desk/NN/jobs/gate-*/run.json plus the
lines of out.log that name a fault). Writes one gzip JSON-lines file: one record a line, {"t": "run"|"leg"|"gate",
...}, times in UTC ISO. Default out: Tools/supervisor/replay/fixtures/relays_<since>_<until>.jsonl.gz.

WHY. The review of 2026-11-02 measured the harness on these records (1,034 runs, 1,418 legs, 937 gates). A module
proves itself by deciding over them: the breaker must trip in the first hour of the licence fault of 10-31, the
capacity rule must start fewer relays on the 5% days, and nothing may trip on the good night of 10-26/27.
"""
import argparse
import glob
import gzip
import json
import os
import re
from pathlib import Path

GH = Path(r'C:\Users\PC\Documents\GitHub')
TW = Path(os.environ.get('LOCALAPPDATA', '')) / 'TrenchWarfare'
RELAYS = [('1', 'relay', 'tw3d-board')] + [(str(n), f'relay{n}', f'tw3d-board-{n}') for n in range(2, 7)]
FAULT_LINE = re.compile(r'Licens|entitlement|error CS|GATE|Timed-out|Gate (green|red)|toolcheck (OK|FAILED)|'
                        r'FAIL |validation|Aborting|another Unity|already open|exit', re.I)
LEG_KEYS = ('leg', 'unit', 'role', 'kind', 'phase', 'state', 'said', 'exit_code', 'is_error', 'seconds',
            'cost_usd', 'started_at', 'finished_at', 'tool_uses', 'lane', 'subtype')
RUN_KEYS = ('started_at', 'stopped_at', 'legs', 'units', 'reason_kind', 'reason', 'run_usd', 'day_pct',
            'day_budget_pct', 'started_by', 'code')


def rj(p):
    try:
        with open(p, encoding='utf-8', errors='replace') as f:
            return json.load(f)
    except (OSError, ValueError):
        return None


def excerpt(log, keep=8):
    try:
        lines = Path(log).read_text(encoding='utf-8', errors='replace').splitlines()
    except OSError:
        return []
    hits, seen = [], set()
    for l in lines:
        s = l.strip()[:240]
        key = re.sub(r'\d+', 'N', s)
        if FAULT_LINE.search(s) and key not in seen:
            seen.add(key)
            hits.append(s)
        if len(hits) >= keep:
            break
    return hits + [l.strip()[:240] for l in lines[-3:]]


def build(since, until, gates=True):
    """The records of runs whose id day is in [since, until); gates=False leaves out the gate jobs (the retry module
    reads only runs and legs, every five minutes)."""
    out = []
    for rn, hname, bname in RELAYS:
        rdir = TW / hname / 'runs'
        for s in glob.glob(str(GH / bname / 'relay' / 'desktop' / 'stops' / '*.json')):
            run = Path(s).stem
            if since <= run[:8] < until:
                d = rj(s) or {}
                out.append(dict({k: d.get(k) for k in RUN_KEYS}, t='run', relay=rn, run=run))
        for lj in glob.glob(str(rdir / '2026*' / 'legs' / '*' / 'leg.json')):
            run = Path(lj).parts[-4]
            if not (since <= run[:8] < until):
                continue
            d = rj(lj) or {}
            rec = dict({k: d.get(k) for k in LEG_KEYS}, t='leg', relay=rn, run=run,
                       model=d.get('ran_model') or d.get('model'),
                       report=(d.get('report') or '')[:600])
            out.append(rec)
        for gj in glob.glob(str(rdir / '2026*' / 'desk' / '*' / 'jobs' / 'gate-*' / 'run.json')) if gates else []:
            parts = Path(gj).parts
            run = parts[-6]
            if not (since <= run[:8] < until):
                continue
            d = rj(gj) or {}
            out.append({'t': 'gate', 'relay': rn, 'run': run, 'leg': parts[-4], 'job': parts[-2],
                        'started_at': d.get('started_at'), 'finished_at': d.get('finished_at'),
                        'exit_code': d.get('exit_code'), 'state': d.get('state'),
                        'lines': excerpt(Path(gj).with_name('out.log'))})
    out.sort(key=lambda r: (r.get('started_at') or r.get('run') or '', r['t']))
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description='build the replay fixture from the relays records')
    ap.add_argument('--since', default='20261019')
    ap.add_argument('--until', default='20261103')
    ap.add_argument('--out')
    a = ap.parse_args(argv)
    recs = build(a.since, a.until)
    out = Path(a.out or Path(__file__).resolve().parent / 'fixtures' / f'relays_{a.since}_{a.until}.jsonl.gz')
    out.parent.mkdir(parents=True, exist_ok=True)
    with gzip.open(out, 'wt', encoding='utf-8') as f:
        for r in recs:
            f.write(json.dumps(r, sort_keys=True) + '\n')
    n = {k: sum(1 for r in recs if r['t'] == k) for k in ('run', 'leg', 'gate')}
    print(f'{out}: {n}')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
