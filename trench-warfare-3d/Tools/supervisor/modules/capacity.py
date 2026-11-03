"""How many relays the day's budget carries now (supervisor module, Tools/supervisor/core.py).

    allowed = floor(budget left today / (one relay-hour's cost * hours left in the window)), 1..MOST
              (1 while anything is left: relay 1 first; 0 when the day's cap is spent)

Budget left = the day's cap (Tools/supervisor/budget.py, else TW_DAY_PCT, else 11) minus what the day used (the
relay's ledger), in percent of the week. One relay-hour's cost is measured from the last LOOKBACK_H hours of legs:
their dollars over the hours the runs that did work were going ($28.40 at list price = 1% of the week); RATE_PCT
until there are enough. The window ends at the budget window's end when one ends today, else at midnight. Relay 1
(the main relay) holds the first slot: autoloop's feed counts it as taken, running or not, so with allowed 1 no
extra relay starts and the main relay still takes the best work.

State: {allowed, why, cap, spent, rate, hours, at}. Hook: Tools/assetboard/autoloop.py feed asks
spend.capacity_room() before an extra relay starts; under 'dry' (the default) that answers yes and the loop log
only gets this module's line.

WHY. Six relays ran on 5-6% caps on 2026-10-20 and 10-24: a cap that small carries one or two. The others started,
waited for the day's pace and stopped with 0 legs: 357 such runs in two weeks (review of 2026-11-02).
"""
import datetime
import math
import os
import sys
from pathlib import Path

NAME = 'capacity'
EVERY_S = 300
DEFAULT_MODE = 'dry'

MOST = 6                # relays there are (1 main + 5 extras)
RATE_PCT = 0.22         # % of the week one relay-hour costs, measured 2026-10-13 (0.2-0.25): until legs say better
USD_PER_PCT = 28.40     # list dollars that are 1% of the plan's week
LOOKBACK_H = 72         # legs this recent measure the rate
MIN_RUN_H = 3.0         # fewer working relay-hours than this measure nothing: RATE_PCT holds
MIN_HOURS = 0.25        # the window never counts as shorter than this: no division by a minute

SUP = Path(__file__).resolve().parent.parent


def _iso(s):
    return datetime.datetime.strptime(s[:19], '%Y-%m-%dT%H:%M:%S')


def rate(runs, legs, now_utc, lookback_h=LOOKBACK_H):
    """(% of the week a relay-hour costs, how it was measured). runs/legs are replay-kit records (UTC ISO times)."""
    t0 = now_utc - datetime.timedelta(hours=lookback_h)
    usd = sum(float(l.get('cost_usd') or 0) for l in legs
              if l.get('started_at') and t0 <= _iso(l['started_at']) < now_utc)
    hrs = 0.0
    for r in runs:
        if not (r.get('legs') and r.get('started_at') and r.get('stopped_at')):
            continue
        a, z = max(_iso(r['started_at']), t0), min(_iso(r['stopped_at']), now_utc)
        hrs += max((z - a).total_seconds(), 0) / 3600.0
    if hrs < MIN_RUN_H or usd <= 0:
        return RATE_PCT, f'{RATE_PCT}% a relay-hour (the 10-13 measure: {hrs:.1f} working hours in {lookback_h} h)'
    r = usd / hrs / USD_PER_PCT
    return r, f'{r:.3f}% a relay-hour (${usd:.0f} over {hrs:.0f} working relay-hours in {lookback_h} h)'


def decide(s):
    """Pure. s: cap, spent (% of the week), hours (left in the window), rate (% a relay-hour). -> {allowed, why}"""
    left = float(s['cap']) - float(s['spent'])
    hours = max(float(s['hours']), MIN_HOURS)
    if left <= 0:
        return dict(allowed=0, why=f'the day\'s {s["cap"]:g}% is spent')
    carried = left / (float(s['rate']) * hours)
    n = max(1, min(MOST, int(math.floor(carried))))        # relay 1 first: while anything is left it may work
    return dict(allowed=n, why=f'{left:.2f}% left of {s["cap"]:g}% for {hours:.1f} h at {s["rate"]:.3f}% a relay-hour '
                               f'carries {carried:.2f} relays' + (' (relay 1 holds the first slot)' if carried < 1 else ''))


def window_end(now, h=None):
    """Local time the budget window ends today: the owner's window when it ends before midnight, else midnight."""
    nxt = now.replace(hour=0, minute=0, second=0, microsecond=0) + datetime.timedelta(days=1)
    try:
        sys.path.insert(0, str(SUP))
        import budget
        w = budget.window(now, h)
        if w and w['kind'] == 'more':
            return min(budget._t(w['end']), nxt)
    except Exception:  # noqa: BLE001
        pass
    return nxt


def run(ctx):
    sys.path.insert(0, str(SUP))
    import budget
    import spend
    from replay import build_fixtures
    now = datetime.datetime.fromtimestamp(ctx.now)
    now_utc = datetime.datetime.fromtimestamp(ctx.now, datetime.timezone.utc).replace(tzinfo=None)
    cap = float(spend.day_figure(os.environ.get('TW_DAY_PCT') or '11', now, ctx.home))
    spent = budget.used_now()
    if spent is None:
        ctx.save(dict(allowed=None, why='the day used could not be read', at=ctx.now))
        return ['the day used could not be read: no figure (feed is not held back)']
    old = ctx.state() or {}
    if old.get('rate') and ctx.now - float(old.get('rate_at') or 0) < 3600:
        r, how, r_at = old['rate'], old.get('rate_how', ''), old['rate_at']
    else:
        since = (now_utc - datetime.timedelta(hours=LOOKBACK_H + 24)).strftime('%Y%m%d')
        recs = build_fixtures.build(since, (now_utc + datetime.timedelta(days=1)).strftime('%Y%m%d'))
        r, how = rate([x for x in recs if x['t'] == 'run'], [x for x in recs if x['t'] == 'leg'], now_utc)
        r_at = ctx.now
    hours = (window_end(now, ctx.home) - now).total_seconds() / 3600.0
    d = decide(dict(cap=cap, spent=spent, hours=hours, rate=r))
    ctx.save(dict(d, cap=cap, spent=round(spent, 2), rate=round(r, 4), rate_how=how, rate_at=r_at,
                  hours=round(hours, 2), at=ctx.now))
    word = 'allows' if not ctx.dry else 'would allow'
    return [f'{word} {d["allowed"]} relays: {d["why"]}']
