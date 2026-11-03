"""Decide each unit's park by fault kind (Tools/supervisor/retry.py says the rule, and which rule wins over
loopsteps.attempts()). Reads the relays' stop records and legs of the last WINDOW_DAYS days, writes only its own
state {units: {uid: {tries: {kind: n}, last_relay, last, park, kind, why, since}}}. 'dry' logs what would be parked;
'on' lets loopsteps.attempts() park it through its own park path (supervisor hooks.retry_park), and lets the relay's
picker keep a unit off the relay whose newest run failed it (hooks.skip, the cool-down)."""
import os
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import registry  # noqa: E402
import retry as rule  # noqa: E402
from replay import build_fixtures  # noqa: E402

NAME = 'retry'
EVERY_S = 300
DEFAULT_MODE = 'dry'
WINDOW_DAYS = 7


def queued_ids(bds=None):
    out = set()
    for b in (registry.boards() if bds is None else bds).values():
        out |= {p.stem for p in (Path(b) / 'relay' / 'queue').glob('*.json')}
    return out


def parked_at(home=None):
    """{uid: UTC ISO time} of the loop's last park of each unit (the mtime of autoloop's parked/<uid>.json, which
    loopsteps.park writes on every park, whichever rule parked it). Read-only."""
    d = Path(home or (Path(os.environ.get('LOCALAPPDATA') or Path.home()) / 'TrenchWarfare' / 'autoloop')) / 'parked'
    out = {}
    for p in (d.glob('*.json') if d.is_dir() else []):
        try:
            out[p.stem] = time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime(p.stat().st_mtime))
        except OSError:
            pass
    return out


def step(units, tries, queued, parks, dry):
    """Pure: the new {uid: rec} and the log lines, from the old state, every unit's tries, the queued ids and the
    loop's park times. A unit's tries count from its last park on: one that comes back starts with none, so the
    loop does not park it again on the tries that parked it."""
    units, lines = dict(units or {}), []
    for uid, ts in sorted(tries.items()):
        u = dict(units.get(uid) or {})
        u['since'] = max(u.get('since', ''), parks.get(uid, ''))
        d = rule.decide(ts, since=u['since'])
        was = bool(u.get('park'))
        last = ts[-1]
        u.update(tries=d['counts'], last_relay=last['relay'], last=f'{last["run"]} {last["verdict"]} {last["kind"]}'.strip(),
                 park=d['park'], kind=d.get('kind', ''))
        if d['park']:
            runs = [t['run'] for t in ts if t.get('kind') == d['kind'] and (t.get('at') or '') > u.get('since', '')]
            u['why'] = f'{len(runs)} tries ended {d["kind"]} ({", ".join(runs[-3:])})'
            if not was and uid in queued:
                lines.append(f'{uid} would be parked: {u["why"]}' if dry else f'{uid}: the loop parks it: {u["why"]}')
        units[uid] = u
    for uid in set(units) - set(tries):         # no try left in the window: nothing to park it on
        units[uid] = dict(units[uid], park=False)
    return units, lines


def run(ctx):
    since = time.strftime('%Y%m%d', time.localtime(ctx.now - WINDOW_DAYS * 86400))
    until = time.strftime('%Y%m%d', time.localtime(ctx.now + 86400))
    recs = build_fixtures.build(since, until, gates=False)
    tries = rule.tries_of([r for r in recs if r['t'] == 'run'], [r for r in recs if r['t'] == 'leg'])
    st, queued = ctx.state(), queued_ids()
    units, lines = step(st.get('units'), tries, queued, parked_at(), ctx.dry)
    ctx.save(dict(units=units, at=ctx.now))
    parked = sum(1 for uid, u in units.items() if u.get('park') and uid in queued)
    return lines + [f'{len(tries)} units tried in {WINDOW_DAYS} days; {parked} due a park and still queued']
