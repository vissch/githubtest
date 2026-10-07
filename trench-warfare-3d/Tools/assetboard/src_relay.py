"""What the relay's legs cost, for the shells in the office on the graphs page (static/counted.js).

The relay (Tools/relay on its own lane) runs short headless legs and writes one record a leg, in two places:
- its leg folder on the station that ran it: <home>/runs/<run>/legs/<nn>/leg.json, where <home> is TW_RELAY_HOME, else
  the folder relay under LOCALAPPDATA/TrenchWarfare (a second relay beside it has a home of its own there: relay2);
- the board repo, for every station to read: relay/<station>/legs/<run>-<nn>.json.
A record holds `cost_usd`, the figure Claude prints for the leg (total_cost_usd). On a plan login that is not money
paid: it is the yardstick the relay's own day budget is counted in (its ledger.py). The page says so.

The rule here is the ledger's, without its estimates: a leg is counted once, by its run and its number, wherever its
record lies; its cost is the one a record of it holds; a leg no record gives a cost for is counted as a leg and
named as having none (`unpriced`), never guessed. A proof run (the relay testing its own meter, source "proof") is
not work and is left out, as the ledger leaves it out. Checked on the desktop on 2026-10-07 against the ledger's own
history: wherever the ledger had every leg's cost (4 Oct, 5 Oct, the second relay's 6 Oct) the day's sum was the
same to the cent; where it had not, it estimated 19 to 34 legs, which this does not.

Reads only. Nothing here knows the relay's code: the record's few fields are all it asks for.
"""
import datetime
import json
import os
from pathlib import Path

HOMES = Path(os.environ.get('LOCALAPPDATA', str(Path.home()))) / 'TrenchWarfare'
DAYS = 7


def homes(root: Path = None):
    """Every relay home on this station: the folders named relay* that hold runs, and the one TW_RELAY_HOME names."""
    root = root or HOMES
    out = [d for d in sorted(root.glob('relay*')) if (d / 'runs').is_dir()] if root.is_dir() else []
    named = os.environ.get('TW_RELAY_HOME')
    if named and Path(named).resolve() not in [d.resolve() for d in out]:
        out.append(Path(named))
    return out


def boards(board=None):
    """The board repo, and the boards TW_BOARD_ALSO names (a second relay's own clone), each once."""
    out = [Path(board)] if board else []
    for p in os.environ.get('TW_BOARD_ALSO', '').split(os.pathsep):
        if p.strip() and Path(p.strip()).resolve() not in [b.resolve() for b in out]:
            out.append(Path(p.strip()))
    return out


def day_of(stamp):
    """The local day of a stamp the relay wrote in UTC (2026-10-05T14:25:45Z), or None."""
    try:
        t = datetime.datetime.strptime(stamp, '%Y-%m-%dT%H:%M:%SZ').replace(tzinfo=datetime.timezone.utc)
    except (TypeError, ValueError):
        return None
    return t.astimezone().strftime('%Y-%m-%d')


def cost_of(rec):
    c = rec.get('cost_usd')
    return float(c) if isinstance(c, (int, float)) and not isinstance(c, bool) and c >= 0 else None


def legs(board_list=(), home_list=()):
    """Every leg that started, once: {(run, leg): dict(day, cost)}. `cost` is None when no record of it holds one."""
    files = [f for h in home_list for f in sorted(Path(h).glob('runs/*/legs/*/leg.json'))]
    files += [f for b in board_list for f in sorted(Path(b).glob('relay/*/legs/*.json'))]
    out = {}
    for f in files:
        try:
            rec = json.loads(f.read_text(encoding='utf-8'))
        except (OSError, ValueError):
            continue
        if not isinstance(rec, dict) or rec.get('source') == 'proof':
            continue
        day = day_of(rec.get('started_at'))
        if not day or rec.get('run') is None or rec.get('leg') is None:
            continue
        had = out.setdefault((str(rec['run']), str(rec['leg'])), dict(day=day, cost=None))
        if had['cost'] is None:
            had['cost'] = cost_of(rec)
    return out


def week(board_list, home_list, now, days=DAYS):
    """The legs of the last `days` days, today the last: dict(usd, legs, unpriced, days=[dict(day, usd, legs, unpriced)]).
    `usd` is the sum of the costs on record, `legs` every leg that started, `unpriced` the legs among them with no
    cost on record. None when this station has no record of any leg at all: then the page draws no shells and says
    why, instead of a nought it could not stand behind."""
    every = legs(board_list, home_list)
    if not every:
        return None
    day0 = datetime.datetime.fromtimestamp(now).date()
    rows = {str(day0 - datetime.timedelta(days=k)): dict(usd=0.0, legs=0, unpriced=0) for k in range(days - 1, -1, -1)}
    for leg in every.values():
        r = rows.get(leg['day'])
        if r is None:
            continue
        r['legs'] += 1
        if leg['cost'] is None:
            r['unpriced'] += 1
        else:
            r['usd'] += leg['cost']
    out = [dict(day=d, usd=round(r['usd'], 2), legs=r['legs'], unpriced=r['unpriced']) for d, r in rows.items()]
    return dict(usd=round(sum(r['usd'] for r in rows.values()), 2), legs=sum(r['legs'] for r in out), unpriced=sum(r['unpriced'] for r in out), days=out)
