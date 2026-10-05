#!/usr/bin/env python3
"""The day's spend, added up from the leg records on the board (relay/<station>/legs/), every station.

  spent(board, day)         what the legs started that local day cost: {day, usd, legs, estimated, units}
  usual(board, phase, ..)   the usual cost of one leg of a phase: the median of the newest legs with a known cost
  history(board, days)      [{day, usd, legs, estimated}] for the last days, oldest first
Cost is the figure Claude prints per leg (total_cost_usd): it weighs the model, and cached reads count little. On a
plan login it is not money, it is the yardstick the day's budget (limits.json day_budget_usd) is counted in.
A leg whose record holds no cost (it was killed, or an older relay wrote the record) takes it from its own leg
folder on this machine when that is there, else it counts at the usual cost of its phase and is marked estimated.
Only relay legs are counted: a session the owner talks to is not.
Stdlib only. ASCII only.
"""
import datetime, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
from pipeline import read_json          # noqa: E402
import config, legdir                   # noqa: E402

DAY = "%Y-%m-%d"
PHASES = ("plan", "execute", "critic", "retro")     # the order a report shows them in


def today():
    return datetime.datetime.now().strftime(DAY)


def day_of(stamp):
    """The local day of a UTC stamp the relay wrote (2026-10-05T14:25:45Z), or None."""
    try:
        t = datetime.datetime.strptime(stamp, "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=datetime.timezone.utc)
    except (TypeError, ValueError):
        return None
    return t.astimezone().strftime(DAY)


def _files(board, since=None):
    """The leg records, oldest first. A run is named by its local start (20261005-161419-<pid>) and lasts 12 hours
    at most, so with since (a day) the runs that started before the day ahead of it are not even opened."""
    files = sorted(Path(board).glob("relay/*/legs/*.json"), key=lambda p: p.name)
    if since:
        cut = (datetime.datetime.strptime(since, DAY) - datetime.timedelta(days=1)).strftime("%Y%m%d")
        files = [f for f in files if not (f.name[:8].isdigit() and f.name[:8] < cut)]
    return files


def _read(f):
    try:
        rec = read_json(f)
        return rec if isinstance(rec, dict) else None
    except (OSError, ValueError):
        return None


def known_cost(rec, home=None):
    """The cost of one leg from its record, else from its leg folder here, else None."""
    c = rec.get("cost_usd")
    if not isinstance(c, (int, float)) or isinstance(c, bool):
        try:
            own = Path(home or legdir.home()) / "runs" / str(rec["run"]) / "legs" / ("%02d" % rec["leg"]) / legdir.LEG
            c = read_json(own).get("cost_usd") if own.exists() else None
        except (OSError, ValueError, KeyError, TypeError):
            c = None
    return float(c) if isinstance(c, (int, float)) and not isinstance(c, bool) else None


def _median(xs):
    xs = sorted(xs)
    return (xs[(len(xs) - 1) // 2] + xs[len(xs) // 2]) / 2.0


def usuals(board, home=None, lim=None):
    """A function (phase, model, effort) -> the usual cost of such a leg. From the newest limits.json price_legs
    records with a known cost: three or more legs of the same phase, model and effort, else the legs of the phase,
    else every leg, else limits.json usual_leg_usd."""
    lim = lim or config.limits()
    known = []
    for f in _files(board)[-int(lim["price_legs"]):]:
        rec = _read(f)
        c = known_cost(rec, home) if rec else None
        if c is not None:
            known.append((rec.get("phase"), rec.get("model"), rec.get("effort"), c))

    def usual(phase, model=None, effort=None):
        same = [c for p, m, e, c in known if (p, m, e) == (phase, model, effort)]
        of_phase = [c for p, _, _, c in known if p == phase]
        pick = same if len(same) >= 3 else of_phase or [c for _, _, _, c in known]
        return _median(pick) if pick else float(lim["usual_leg_usd"])
    return usual


def usual(board, phase, model=None, effort=None, home=None, lim=None):
    return usuals(board, home, lim)(phase, model, effort)


def _days(board, since, home, lim):
    """{day: {day, usd, legs, estimated, units: {unit: {phase: usd}}}} for the legs started on since or later."""
    out, price = {}, None
    for f in _files(board, since):
        rec = _read(f)
        day = day_of(rec.get("started_at") or rec.get("finished_at")) if rec else None
        if not day or day < since:
            continue
        d = out.setdefault(day, {"day": day, "usd": 0.0, "legs": 0, "estimated": 0, "units": {}})
        c = known_cost(rec, home)
        if c is None:
            price = price or usuals(board, home, lim)
            c = price(rec.get("phase"), rec.get("model"), rec.get("effort"))
            d["estimated"] += 1
        d["usd"] += c
        d["legs"] += 1
        unit = d["units"].setdefault(str(rec.get("unit") or "?"), {})
        unit[str(rec.get("phase") or "?")] = unit.get(str(rec.get("phase") or "?"), 0.0) + c
    return out


def spent(board, day=None, home=None, lim=None):
    day = day or today()
    return _days(board, day, home, lim).get(day) or {"day": day, "usd": 0.0, "legs": 0, "estimated": 0, "units": {}}


def history(board, days=7, home=None, lim=None):
    first = datetime.datetime.now() - datetime.timedelta(days=days - 1)
    got = _days(board, first.strftime(DAY), home, lim)
    names = [(first + datetime.timedelta(days=i)).strftime(DAY) for i in range(days)]
    return [got.get(n) or {"day": n, "usd": 0.0, "legs": 0, "estimated": 0, "units": {}} for n in names]


def lines(board, budget, days=7, home=None, lim=None):
    """What `relay.py budget` prints: today against the budget, per unit and phase, then the days before."""
    hist = history(board, days, home, lim)
    now = hist[-1]
    est = ", %d estimated" % now["estimated"] if now["estimated"] else ""
    head = "Today %s: $%.2f spent" % (now["day"], now["usd"])
    head += (" of $%.2f, $%.2f left." % (budget, max(0.0, budget - now["usd"])) if budget else ". No day budget is set.")
    out = [head + " %d leg%s%s." % (now["legs"], "" if now["legs"] == 1 else "s", est)]
    for unit, phases in sorted(now["units"].items()):
        order = sorted(phases, key=lambda p: (PHASES.index(p) if p in PHASES else len(PHASES), p))
        out.append("  %-34s %s" % (unit, "  ".join("%s $%.2f" % (p, phases[p]) for p in order)))
    before = [d for d in hist[:-1] if d["legs"]]
    if before:
        out.append("Before, in the last %d days:" % (len(hist) - 1))
        out += ["  %s  $%7.2f  %d leg%s" % (d["day"], d["usd"], d["legs"], "" if d["legs"] == 1 else "s")
                for d in before]
    return out


def one_line(board, budget, home=None, lim=None):
    """One line for `relay.py status` and the stop line."""
    now = spent(board, None, home, lim)
    if not budget:
        return "Today: $%.2f spent (no day budget)." % now["usd"]
    return "Today: $%.2f of $%.2f spent, $%.2f left." % (now["usd"], budget, max(0.0, budget - now["usd"]))
