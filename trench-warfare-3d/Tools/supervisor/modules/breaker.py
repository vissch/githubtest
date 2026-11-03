"""The breaker: the machine is broken, not the unit. While it is open no new leg and no new relay starts.

It OPENS when two legs on different units end not-done with the same machine fault kind (signatures.py: licence,
broken-base, unity-wont-start, gate-stall, login-expired, disk) within 60 minutes, or when the canary (canary.py: a
gate on a clean checkout at integration's tip) is red with a machine fault (a test that fails on integration
itself does not open it: the relays are what fixes that). It CLOSES only on a green canary started after it opened.
Its switch only matters through breaker_gate.allow(): with the switch at dry (the default), or the canary's
not on (then nothing could close it), it decides, logs and keeps its state, and nothing anywhere waits on it.

WHY. From 2026-10-31 ~11:00 UTC a stray batch-mode Unity (githubtest-footage, session 0, since 10-27) held the
machine's licence pipe; about 70 legs wrote "machine-wide licence fault, not my edit", each ended BLOCKED, and the loop
fed the next unit for 2.5 days (~$337). On 10-19 a hung tool test timed out every relay's legs together. Nothing read
those as ONE fault. On replay (test_breaker.py) this opens 43 min after the first licence-red gate of 10-31.

A leg's fault kind is read from its report. A leg that TIMED OUT with no machine words in its report counts as
gate-stall when its gate job never finished (killed with the leg, no exit code): that is how the hang of 10-19 looks.

    decide(events, canary, now, prev) -> state       pure; the replay test drives it
    events_of(legs, gates) -> [event]                 records (fixture shape) to machine-fault events
"""
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
import core  # noqa: E402
import relays  # noqa: E402
from signatures import classify, is_machine  # noqa: E402

NAME = 'breaker'
EVERY_S = 120
DEFAULT_MODE = 'dry'
WINDOW_S = 3600          # two units with one machine kind inside this many seconds open it
UNITS = 2
LOOK_BACK_S = 2 * 3600   # how far back run() reads the legs


def _first_line(text):
    for l in (text or '').splitlines():
        l = l.strip()
        if l:
            return l[:200]
    return ''


def events_of(legs, gates=()):
    """[{at, unit, kind, relay, line}] of the not-done legs whose fault is the machine's, oldest first."""
    unfinished = set()
    for g in gates:
        if g.get('exit_code') is None and g.get('state') in ('STOPPED', 'TIMEOUT'):
            try:
                unfinished.add((str(g.get('relay')), g.get('run'), int(g.get('leg'))))
            except (TypeError, ValueError):
                pass
    out = []
    for leg in legs:
        state, said = leg.get('state'), (leg.get('said') or '').lower()
        if state == 'RUNNING' or said == 'done':
            continue
        report = leg.get('report') or ''
        kind = classify(report)
        line = _first_line(report)
        if kind == 'unit' and state == 'TIMEOUT':
            try:
                key = (str(leg.get('relay')), leg.get('run'), int(leg.get('leg')))
            except (TypeError, ValueError):
                key = None
            if key in unfinished:
                kind, line = 'gate-stall', f'the leg timed out with its gate unfinished ({state})'
        if not is_machine(kind):
            continue
        at = relays.epoch(leg.get('finished_at') or leg.get('started_at'))
        if at:
            out.append({'at': at, 'unit': leg.get('unit') or '?', 'kind': kind, 'relay': str(leg.get('relay')),
                        'line': line})
    return sorted(out, key=lambda e: e['at'])


def _iso(t):
    return time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime(t)) if t else ''


def decide(events, canary, now, prev):
    """The breaker's state after `now`. Pure.

    events: events_of(...), any span (only those in the last WINDOW_S and after the last close count).
    canary: canary.py's state ({verdict, started, kind, ...}) or {}.
    prev: this function's last answer or {}."""
    prev = dict(prev or {})
    canary = canary or {}
    if prev.get('open'):
        started = canary.get('started') or 0
        if canary.get('verdict') == 'green' and started >= prev.get('since', 0):
            return {'open': False, 'closed_at': now, 'reason': f'closed: the canary started {_iso(started)} is green',
                    'last': {k: prev.get(k) for k in ('kind', 'since', 'reason')}}
        return prev
    floor = max(now - WINDOW_S, prev.get('closed_at') or 0)
    recent = [e for e in events if floor < e['at'] <= now and is_machine(e['kind'])]
    by_kind = {}
    for e in recent:
        by_kind.setdefault(e['kind'], {}).setdefault(e['unit'], e)
    hit = [(k, us) for k, us in by_kind.items() if len(us) >= UNITS]
    if hit:
        kind, units = max(hit, key=lambda h: (len(h[1]), -min(e['at'] for e in h[1].values())))
        ev = sorted(units.values(), key=lambda e: e['at'])
        return {'open': True, 'kind': kind, 'since': now, 'closed_at': prev.get('closed_at'),
                'evidence': [f"{e['unit']} (relay {e['relay']}, {_iso(e['at'])}): {e['line']}" for e in ev],
                'reason': f'{len(ev)} different units ended on the same machine fault ({kind}) within '
                          f'{WINDOW_S // 60} min'}
    if canary.get('verdict') == 'red' and is_machine(canary.get('kind')):   # a test red on integration: not ours
        kind = canary['kind']
        return {'open': True, 'kind': kind, 'since': now, 'closed_at': prev.get('closed_at'),
                'evidence': [f"canary gate on tree {canary.get('tree', '?')}: {canary.get('line', '')}"],
                'reason': f'the canary gate on a clean checkout of integration is red ({kind})'}
    return {'open': False, 'closed_at': prev.get('closed_at'), 'reason': 'closed'}


def message(st):
    return ('The relays are on hold: the machine is broken, not the work.\n'
            f"Why: {st.get('reason')}.\nSince: {_iso(st.get('since'))}.\n"
            + ''.join(f'  - {e}\n' for e in st.get('evidence') or [])
            + 'No new leg or relay starts until a fresh canary gate (a clean checkout of integration) is green.\n'
              'Look at: Tools/supervisor (state/breaker.json, state/unity_census.json) in the supervisor home.\n')


def run(ctx):
    since = ctx.now - LOOK_BACK_S
    evs = events_of(relays.recent_legs(since, ctx.now), relays.recent_gates(since, ctx.now))
    prev = ctx.state()
    st = decide(evs, ctx.other('canary'), ctx.now, prev)
    ctx.save(st)
    if st.get('open') and not prev.get('open'):
        lines = [f"OPEN ({st['kind']}): {st['reason']}"] + [f'  {e}' for e in st.get('evidence', [])]
        if ctx.dry:
            lines.append('would hold every new leg and relay start, and write the owner a message')
        else:
            msg = core.state_path(NAME, ctx.home).with_name('breaker_message.txt')
            msg.write_text(message(st), encoding='utf-8')
            lines.append(f'held every new leg and relay start; the owner\'s message is in {msg}')
        return lines
    if prev.get('open') and not st.get('open'):
        return [st['reason']]
    return [f"{'open' if st.get('open') else 'closed'}; {len(evs)} machine-fault legs in the last "
            f'{LOOK_BACK_S // 3600} h']
