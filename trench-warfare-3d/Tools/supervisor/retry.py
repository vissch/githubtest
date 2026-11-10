#!/usr/bin/env python3
"""Retried by reason: a unit is parked after TRIES fails with the SAME fault kind; a machine fault is no try; a
relay never takes the unit its last run failed again (the cool-down). The core is pure (records in, decision out).

    from retry import tries_of, decide, cooling
    tries = tries_of(runs, legs)          # {uid: [{run, relay, at, verdict, kind}]}, oldest first, from the replay
                                          #   kit's shape (replay/build_fixtures.py reads the same live records)
    decide(tries['x'])                    # {'park': True, 'kind': 'push-refused', 'run': ..., 'counts': {...}}
    cooling(newest_stop_units, 'x')       # True: this relay's last run failed x, so it takes another unit first

WHY. fix-job-ordering-tests was tried 20 times with no pass (10-19..10-22); 15 times a relay ran one unit 3+ times
in a row. loopsteps.attempts() parks a unit after TRIES FAIL/BLOCKED "with nothing changed between them", and its
own legs changed things (a commit on its lane, a landing) so the count kept starting over; meanwhile a machine
fault (the licence of 10-31..11-02) counted as a try like any other. Here a try's kind is signatures.classify of
its last execute leg's report (the run's stop reason when it has none): two of one kind is the unit's own pattern,
whatever moved between; a machine kind (signatures.MACHINE) is not counted at all.

WHICH RULE WINS. With the switch of modules/retry.py 'off' or 'dry', loopsteps.attempts() parks by today's rule,
unchanged. With it 'on', retry's verdict REPLACES attempts' TRIES count and its "something changed, one more try"
excuse: attempts parks a unit when retry says so (through its own park() and park_brief(), so his A/B on the brief
and the way back are as before) and no longer parks on its own fail count. attempts' other rules stay its own: the
API safety refusals (REFUSALS) and the fix-chain depth (CHAIN_DEPTH). A unit that comes back on a queue after a park
starts with no tries (retry's `since`).
"""
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
if str(HERE) not in sys.path:
    sys.path.append(str(HERE))
from signatures import classify, is_machine  # noqa: E402

TRIES = 2


def kind_of(run, uid, legs):
    """The fault kind of one unit's try in one run: its last execute leg's report, else the run's stop reason."""
    mine = [l for l in legs if l.get('unit') == uid and l.get('phase') != 'plan']
    text = next((l.get('report') or '' for l in reversed(mine) if (l.get('report') or '').strip()), '')
    return classify(text or run.get('reason') or '')


def tries_of(runs, legs):
    """{uid: [try]} from stop records (runs) and legs, oldest first; a PASS is a try with verdict PASS."""
    by_run = {}
    for l in legs:
        by_run.setdefault(l.get('run'), []).append(l)
    out = {}
    for r in sorted(runs, key=lambda r: r.get('stopped_at') or r.get('started_at') or ''):
        for uid, verdict in sorted((r.get('units') or {}).items()):
            out.setdefault(uid, []).append(dict(run=r.get('run'), relay=str(r.get('relay') or ''),
                                                at=r.get('stopped_at') or r.get('started_at') or '', verdict=verdict,
                                                kind='' if verdict == 'PASS' else kind_of(r, uid, by_run.get(r.get('run'), []))))
    return out


def decide(tries, since='', machine_counts=False, by_kind=True):
    """Park or not, from one unit's tries (oldest first) after `since`. A PASS starts the count over. The two flags
    are today's rule for the replay's proofs: machine_counts=True, by_kind=False counts every FAIL/BLOCKED alike.
    The whole list is read before deciding: a PASS after the second fail clears the park (FAIL, FAIL, PASS = no park;
    on 11-10 three units whose last try passed were marked due a park)."""
    counts, hit = {}, None
    for i, t in enumerate(tries or []):
        if since and (t.get('at') or '') <= since:
            continue
        if t.get('verdict') == 'PASS':
            counts, hit = {}, None
            continue
        k = t.get('kind') or 'unit'
        if is_machine(k) and not machine_counts:
            continue
        key = k if by_kind else 'any'
        counts[key] = counts.get(key, 0) + 1
        if hit is None and counts[key] >= TRIES:
            hit = dict(park=True, kind=k, run=t.get('run'), at=t.get('at'), index=i)
    if hit:
        return dict(hit, counts=counts)
    return dict(park=False, counts=counts)


def cooling(last_units, uid):
    """True when the relay's newest run had `uid` and it did not pass: the relay takes another unit first."""
    v = (last_units or {}).get(uid)
    return v is not None and v != 'PASS'


def streaks(runs, least=3, verdict_ok=('PASS',)):
    """[(relay, uid, length)] of one unit failing in `least`+ runs in a row on one relay (runs with no unit skipped)."""
    by = {}
    for r in sorted(runs, key=lambda r: r.get('stopped_at') or ''):
        if r.get('units') or r.get('cooled'):
            by.setdefault(str(r.get('relay')), []).append(r)
    out = []
    for rel, rs in sorted(by.items()):
        cur = {}
        for r in rs:
            nxt = {u: cur.get(u, 0) + 1 for u, v in r['units'].items() if v not in verdict_ok}
            out += [(rel, u, n) for u, n in cur.items() if u not in nxt and n >= least]
            cur = nxt
        out += [(rel, u, n) for u, n in cur.items() if n >= least]
    return out


def with_cooldown(runs):
    """The runs as they would have gone with the cool-down: a unit a relay's previous run failed is not in its next
    run (the relay takes other work first). Returns the runs with those units taken out."""
    last, out = {}, []
    for r in sorted(runs, key=lambda r: r.get('stopped_at') or ''):
        rel = str(r.get('relay'))
        if not r.get('units'):
            out.append(r)
            continue
        keep = {u: v for u, v in r['units'].items() if not cooling(last.get(rel), u)}
        out.append(dict(r, units=keep, cooled=sorted(set(r['units']) - set(keep))))    # a run all cooled took other work
        last[rel] = keep                # what it ran: the unit it was kept off may come back the run after
    return out
