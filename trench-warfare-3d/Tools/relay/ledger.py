#!/usr/bin/env python3
"""The day's spend, added up from the leg records on the board (relay/<station>/legs/), every station.

  spent(board, day)         what the legs started that local day cost: {day, usd, legs, estimated, units}
  usual(board, phase, ..)   the usual cost of one leg of a phase: the median of the newest legs with a known cost
  need(price, ph, phases)   the usual cost of legs of these phases together (a unit: plan and execute)
  history(board, days)      [{day, usd, legs, estimated}] for the last days, oldest first
Cost is the figure Claude prints per leg (total_cost_usd): it weighs the model, and cached reads count little. On a
plan login it is not money, it is the yardstick the day's budget (limits.json day_budget_usd) is counted in.
A leg whose record holds no cost (it was killed, or an older relay wrote the record) takes it from its own leg
folder on this machine when that is there, else it counts at the usual cost of its phase and is marked estimated.
Only relay legs are counted: a session the owner talks to is not.
The owner reads the day in percent of the plan's weekly limit, not in dollars. A leg record holds what the leg used
of the week (week_used, percent points) when a reading came in before and after it (usage.py). A leg without one
is counted from its cost at the rate the measured legs show (rate()), and said to be estimated. While no leg at all
is measured the rate is a guess: a full week is taken as limits.json week_usd dollars of leg cost (what people who
ran into the weekly limit report for the plan), and every line says it is guessed. With week_usd 0 there is no
guess, and the lines stay in dollars until a leg is measured.
  rate(board)               percent points of the week per dollar of cost: measured, else guessed, else None
  guessed(board)            True when that rate is the guess, not a measurement
  week(day, rate)           (percent points the day's legs used, how many of them are estimated) or None
  standing(board)           the newest reading a leg left on the board: where the week stood, and when
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


def need(price, ph, phases):
    """The usual cost of one leg of each of these phases together, each at the model and effort phases.json (ph)
    gives it. price is what usuals() returned."""
    return sum(price(p, ph.get(p, {}).get("model"), ph.get(p, {}).get("effort")) for p in phases)


def known_week(rec):
    """What one leg used of the week, in percent points, from its record, else None (not measured)."""
    w = rec.get("week_used")
    return float(w) if isinstance(w, (int, float)) and not isinstance(w, bool) and w >= 0 else None


def rate(board, home=None, lim=None):
    """Percent points of the week one dollar of leg cost stands for: what the newest limits.json price_legs legs
    with a measured week used, over what they cost. While no leg is measured it is the guess, 100 over
    limits.json week_usd, and None when that is 0."""
    return _rate(board, home, lim)[0]


def guessed(board, home=None, lim=None):
    return _rate(board, home, lim)[1]


def _rate(board, home, lim):
    """(the rate or None, True when it is the guess)."""
    lim = lim or config.limits()
    used = cost = 0.0
    price = None
    for f in _files(board)[-int(lim["price_legs"]):]:
        rec = _read(f)
        w = known_week(rec) if rec else None
        if w is None:
            continue
        c = known_cost(rec, home)
        if c is None:
            price = price or usuals(board, home, lim)
            c = price(rec.get("phase"), rec.get("model"), rec.get("effort"))
        used, cost = used + w, cost + c
    if cost > 0 and used > 0:
        return used / cost, False
    return (100.0 / lim["week_usd"], True) if lim.get("week_usd", 0) > 0 else (None, False)


def week(day, per_usd):
    """(percent points of the week the day's legs used, how many legs of it are estimated from their cost), or
    None when it cannot be said: a leg is not measured and there is no rate to count it by."""
    if day["unmeasured"] and per_usd is None:
        return None
    return day["week"] + day["usd_unmeasured"] * (per_usd or 0.0), day["unmeasured"]


def standing(board):
    """The newest reading a leg left on the board ({at, week, resets}), or None."""
    for f in reversed(_files(board)):
        rec = _read(f) or {}
        for key in ("week_end", "week_start"):
            r = rec.get(key)
            if isinstance(r, dict) and isinstance(r.get("week"), (int, float)):
                return r
    return None


def _empty(day):
    return {"day": day, "usd": 0.0, "legs": 0, "estimated": 0, "units": {}, "week": 0.0, "unmeasured": 0,
            "usd_unmeasured": 0.0, "week_units": {}}


def _days(board, since, home, lim):
    """{day: {day, usd, legs, estimated, units: {unit: {phase: usd}}, week, unmeasured, usd_unmeasured,
    week_units: {unit: {phase: [percent points measured, dollars of the legs not measured]}}}} for the legs started
    on since or later."""
    out, price = {}, None
    for f in _files(board, since):
        rec = _read(f)
        day = day_of(rec.get("started_at") or rec.get("finished_at")) if rec else None
        if not day or day < since:
            continue
        d = out.setdefault(day, _empty(day))
        c = known_cost(rec, home)
        if c is None:
            price = price or usuals(board, home, lim)
            c = price(rec.get("phase"), rec.get("model"), rec.get("effort"))
            d["estimated"] += 1
        d["usd"] += c
        d["legs"] += 1
        unit = d["units"].setdefault(str(rec.get("unit") or "?"), {})
        unit[str(rec.get("phase") or "?")] = unit.get(str(rec.get("phase") or "?"), 0.0) + c
        w = known_week(rec)
        cell = d["week_units"].setdefault(str(rec.get("unit") or "?"), {})
        cell = cell.setdefault(str(rec.get("phase") or "?"), [0.0, 0.0])
        if w is None:
            d["unmeasured"] += 1
            d["usd_unmeasured"] += c
            cell[1] += c
        else:
            d["week"] += w
            cell[0] += w
    return out


def spent(board, day=None, home=None, lim=None):
    day = day or today()
    return _days(board, day, home, lim).get(day) or _empty(day)


def history(board, days=7, home=None, lim=None):
    first = datetime.datetime.now() - datetime.timedelta(days=days - 1)
    got = _days(board, first.strftime(DAY), home, lim)
    names = [(first + datetime.timedelta(days=i)).strftime(DAY) for i in range(days)]
    return [got.get(n) or _empty(n) for n in names]


def lines(board, budget, days=7, home=None, lim=None):
    """What `relay.py budget` prints: today against the budget, per unit and phase, then the days before."""
    hist = history(board, days, home, lim)
    now = hist[-1]
    per_usd, guess = _rate(board, home, lim)
    if per_usd is not None:
        return _week_lines(hist, budget, per_usd, GUESS % (lim or config.limits())["week_usd"] if guess else "")
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


def _pct(x):
    return "%.1f%%" % x


def _legs(n):
    return "%d leg%s" % (n, "" if n == 1 else "s")


GUESS = " A guess: no leg is measured yet, so a full week is taken as $%d of leg cost."


def _week_lines(hist, budget, per_usd, guess=""):
    """The budget report in percent of the week. A figure counted from cost, not measured, is marked "about"."""
    now = hist[-1]
    used, est = week(now, per_usd)
    head = "Today %s: %s%s of the week used by the relay" % (now["day"], "about " if guess else "", _pct(used))
    head += ", the day's cap is about %s." % _pct(budget * per_usd) if budget else ". No day budget is set."
    out = [head + " %s%s.%s" % (_legs(now["legs"]), ", %d estimated from cost" % est if est and not guess else "",
                                guess)]
    for unit, phases in sorted(now["week_units"].items()):
        order = sorted(phases, key=lambda p: (PHASES.index(p) if p in PHASES else len(PHASES), p))
        out.append("  %-34s %s" % (unit, "  ".join(
            "%s %s%s" % (p, "about " if phases[p][1] else "", _pct(phases[p][0] + phases[p][1] * per_usd))
            for p in order)))
    before = [d for d in hist[:-1] if d["legs"]]
    if before:
        out.append("Before, in the last %d days:" % (len(hist) - 1))
        out += ["  %s  %s%6s  %s" % (d["day"], "about " if d["unmeasured"] else "      ", _pct(week(d, per_usd)[0]),
                                    _legs(d["legs"])) for d in before]
    return out


def one_line(board, budget, home=None, lim=None):
    """One line for `relay.py status`, `relay.py day` and the stop line: the day in percent of the week once a
    leg is measured, in dollars until then. It says so when legs are counted at the usual cost, not their own, or
    from their cost, not a reading."""
    now = spent(board, None, home, lim)
    per_usd, guess = _rate(board, home, lim)
    if per_usd is not None:
        used, est = week(now, per_usd)
        cap = budget * per_usd if budget else None
        return "Today: %s%s of the week used by the relay%s.%s" % (
            "about " if guess else "", _pct(used),
            (", %s left of a day's cap of about %s" % (_pct(max(0.0, cap - used)), _pct(cap))) if cap else
            " (no day budget)",
            GUESS % (lim or config.limits())["week_usd"] if guess else
            " %d of %d legs estimated from their cost." % (est, now["legs"]) if est else "")
    est = " %d of %d legs counted at the usual cost." % (now["estimated"], now["legs"]) if now["estimated"] else ""
    if not budget:
        return "Today: $%.2f spent (no day budget).%s" % (now["usd"], est)
    return "Today: $%.2f of $%.2f spent, $%.2f left.%s" % (now["usd"], budget, max(0.0, budget - now["usd"]), est)
