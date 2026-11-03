#!/usr/bin/env python3
"""The Health page's data: how the relay harness stands, from the supervisor home's state files (Tools/supervisor).

    python Tools/assetboard/src_health.py              from trench-warfare-3d/: the health as text, as a session reads it
    python Tools/assetboard/src_health.py --site OUT   write OUT/health.html, OUT/health.js and OUT/data/health.js

WHY. Only a long-running watching session knew the whole state of the relays, so the owner asked for a handoff every
day (plan PLAN_relay_harness_v2_2026-11-02.md, Phase 6). This page shows it from the files the modules write: each
module's switch and last turn, the breaker and why, the day's budget window and how many relays it carries, families
over their cap, units the critic holds, the landing stage and what landed this week, the last canary gate, and
whether the loop is paused. It reads only; a file that is missing or does not read is shown as "no record".
"""
import json
import os
import re
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
SUP = HERE.parent / 'supervisor'
LOOP_HOME = Path(os.environ.get('LOCALAPPDATA') or Path.home()) / 'TrenchWarfare' / 'autoloop'


def sup_home():
    h = os.environ.get('TW_SUP_HOME')
    return Path(h) if h else Path(os.environ.get('LOCALAPPDATA') or Path.home()) / 'TrenchWarfare' / 'supervisor'


def _json(p, default=None):
    try:
        return json.loads(Path(p).read_text(encoding='utf-8'))
    except (OSError, ValueError):
        return default


def _iso(t):
    try:
        return time.strftime('%Y-%m-%d %H:%M', time.localtime(float(t))) if t else ''
    except (TypeError, ValueError, OverflowError, OSError):
        return str(t)[:16].replace('T', ' ')


def modules(h, folder=None):
    """[{name, mode, every, last}] of every module file, read as text (no module is imported, none runs)."""
    over = _json(h / 'modules.json', {}) or {}
    turns = _json(h / 'state' / '_turns.json', {}) or {}
    out = []
    for p in sorted(Path(folder or SUP / 'modules').glob('*.py')):
        if p.name.startswith(('_', 'test_')):
            continue
        t = p.read_text(encoding='utf-8', errors='replace')
        name = re.search(r"^NAME\s*=\s*['\"]([\w-]+)['\"]", t, re.M)
        if not name:
            continue
        default = re.search(r"^DEFAULT_MODE\s*=\s*['\"](\w+)['\"]", t, re.M)
        every = re.search(r'^EVERY_S\s*=\s*(\d+)', t, re.M)
        n = name.group(1)
        mode = over.get(n, default.group(1) if default else 'off')
        out.append(dict(name=n, mode=mode if mode in ('off', 'dry', 'on') else 'off', set=n in over,
                        every=int(every.group(1)) if every else 300, last=_iso(turns.get(n))))
    return out


def read(h=None, now=None, loop_home=None, folder=None):
    h, now = Path(h or sup_home()), now or time.time()
    st = lambda n: _json(h / 'state' / f'{n}.json', None)        # noqa: E731
    br, cap, bud, fam, crit, land, can = (st(n) for n in ('breaker', 'capacity', 'budget', 'families', 'critic_gate',
                                                           'landing', 'canary'))
    loop_home = Path(loop_home or LOOP_HOME)
    H = dict(read_at=int(now), home=str(h), modules=modules(h, folder))
    H['paused'] = dict(loop=(loop_home / 'autoloop.stop').exists(), supervisor=(h / 'supervisor.stop').exists())
    H['breaker'] = None if br is None else dict(open=bool(br.get('open')), kind=br.get('kind') or '',
                                                reason=br.get('reason') or '', since=_iso(br.get('since')),
                                                evidence=[str(e) for e in (br.get('evidence') or [])][:6])
    H['capacity'] = None if cap is None else dict(allowed=cap.get('allowed'), why=cap.get('why') or '',
                                                  cap=cap.get('cap'), spent=cap.get('spent'), hours=cap.get('hours'),
                                                  at=_iso(cap.get('at')))
    stamp = time.strftime('%Y-%m-%dT%H:%M:%S', time.localtime(now))     # budget.py's FMT, local time
    wins = [w for w in ((bud or {}).get('windows') or []) if isinstance(w, dict)
            and str(w.get('start') or '') <= stamp < str(w.get('end') or '')]
    w = wins[-1] if wins else None          # budget.window(): the newest window that holds now
    H['budget'] = dict(pct=w.get('pct') if w else (bud or {}).get('standing', 11.0), kind=(w or {}).get('kind') or 'standing',
                       set_at=str((w or {}).get('start') or '')[:16].replace('T', ' '),
                       end=str((w or {}).get('end') or '')[:16].replace('T', ' '))
    fams = (fam or {}).get('families') or {}
    H['families'] = None if fam is None else dict(
        cap_usd=fam.get('cap_usd'), cap_generations=fam.get('cap_generations'), at=_iso(fam.get('at')),
        over=sorted((dict(family=k, over=v.get('over'), usd=v.get('usd'), depth=v.get('depth'), brief=v.get('brief') or '')
                     for k, v in fams.items() if isinstance(v, dict) and v.get('over')), key=lambda x: -(x['usd'] or 0)))
    held = (crit or {}).get('held') or {}
    H['critic'] = None if crit is None else dict(
        held=sorted((dict(unit=k, score=v.get('score'), at=str(v.get('at') or '')[:16].replace('T', ' '))
                     for k, v in held.items() if isinstance(v, dict)), key=lambda x: x['unit']),
        passed=len(crit.get('passed') or []))
    sys.path.insert(0, str(HERE))
    try:
        import src_runs
        landed = src_runs.landed(now, str(h))
    except Exception as e:      # noqa: BLE001  the page still shows the rest
        landed = dict(week=0, last='', line=f'Landed this week: not read ({type(e).__name__})')
    H['landing'] = dict(phase=(land or {}).get('phase') or ('no record' if land is None else 'idle'),
                        gate=_iso(((land or {}).get('gate') or {}).get('started')), landed=landed)
    H['canary'] = None if can is None else dict(verdict=can.get('verdict') or '?', kind=can.get('kind') or '',
                                                at=_iso(can.get('at')), line=str(can.get('line') or '')[:200])
    return H


def text(H):
    out = [f'Health, read {_iso(H["read_at"])} from {H["home"]}']
    p = H['paused']
    out.append('Paused: ' + (', '.join(k for k in ('loop', 'supervisor') if p[k]) or 'no'))
    b = H['breaker']
    out.append('Breaker: no record' if b is None else f'Breaker: {"OPEN" if b["open"] else "closed"}'
               + (f' ({b["kind"]}) since {b["since"]}: {b["reason"]}' if b['open'] else ''))
    c, d = H['capacity'], H['budget']
    out.append(f'Budget: {d["pct"]}% ({d["kind"]})' + (f' from {d["set_at"]} until {d["end"]}' if d['end'] else ''))
    out.append('Capacity: no record' if c is None else f'Capacity: {c["allowed"]} relays allowed: {c["why"]}')
    f = H['families']
    out.append('Families: no record' if f is None else f'Families over cap: {len(f["over"])}'
               + ''.join(f'\n  {x["family"]}: {x["over"]}' for x in f['over']))
    k = H['critic']
    out.append('Critic: no record' if k is None else f'Critic holds: {len(k["held"])}'
               + ''.join(f'\n  {x["unit"]}: {x["score"]}' for x in k['held']))
    out.append(f'Landing: {H["landing"]["phase"]}; {H["landing"]["landed"]["line"]}')
    v = H['canary']
    out.append('Canary: no record' if v is None else f'Canary: {v["verdict"]} {v["at"]} {v["line"]}')
    out.append('Modules:')
    out += [f'  {m["name"]:18} {m["mode"]:4} last {m["last"] or "never"}' for m in H['modules']]
    return '\n'.join(out)


def site(H, out: Path):
    """data/health.js for the page."""
    p = Path(out) / 'data' / 'health.js'
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(f'window.HEALTH = {json.dumps(H, sort_keys=True)};\n', encoding='utf-8', newline='\n')
    return p


def page(out: Path, meta=None):
    """health.html (templates/health.html on base.html) and health.js into `out`, as ops.page() does for the others."""
    from jinja2 import Environment, FileSystemLoader, select_autoescape
    env = Environment(loader=FileSystemLoader(str(HERE / 'templates')), autoescape=select_autoescape(['html']),
                      trim_blocks=True, lstrip_blocks=True)
    env.globals.update(meta=meta or dict(built=time.strftime('%Y-%m-%d %H:%M'), station='', commit='', refs_as_of=''))
    out = Path(out)
    out.mkdir(parents=True, exist_ok=True)
    (out / 'health.html').write_text(env.get_template('health.html').render(root=''), encoding='utf-8', newline='\n')
    (out / 'health.js').write_text((HERE / 'static' / 'health.js').read_text(encoding='utf-8'), encoding='utf-8', newline='\n')


def main(argv=None):
    a = list(sys.argv[1:] if argv is None else argv)
    H = read()
    if a[:1] == ['--site'] and len(a) > 1:
        page(Path(a[1]))
        print(site(H, Path(a[1])))
        return 0
    print(text(H))
    return 0


if __name__ == '__main__':
    sys.exit(main())
