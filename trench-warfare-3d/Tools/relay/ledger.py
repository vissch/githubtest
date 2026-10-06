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
The day's cap is limits.json day_budget_pct percent points of the week, for every agent spawned on the project (the
owner, 2026-10-07). It is counted in percent whenever there is a rate, measured or guessed; with none, the cap is
day_budget_usd dollars. The cap is spent slowly: it is spread evenly from pace_from_hour to pace_to_hour, local
time, and what the pace allows by now is the cap times the share of that window gone. What earlier hours left
unused stays allowed later the same day. The same hour for both ends switches the pace off.
  day_budget(board, lim)    the day as the runner and every screen count it: cap, spent, left, allowed by the pace
  pace_at(b, need, lim)     the time today from which the pace covers that much more cost
  pace_line(b, lim)         one line for a person: what the pace allows by now and what of it is free
Every relay counts: the boards TW_BOARD_ALSO names (a second relay's own clone of the board) are read beside the
board, each leg file once. So do the agents a session spawned outside the relay, once agents.py has booked them
(relay/<station>/agents/<day>.json): their cost is added to the day, and it is never a measured figure.
Stdlib only. ASCII only.
"""
import datetime, os, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
from pipeline import read_json          # noqa: E402
import config, legdir                   # noqa: E402

DAY = "%Y-%m-%d"
ALSO = "TW_BOARD_ALSO"
PHASES =("plan", "execute", "critic", "retro")     # the order a report shows them in


def today():
    return datetime.datetime.now().strftime(DAY)


def day_of(stamp):
    """The local day of a UTC stamp the relay wrote (2026-10-05T14:25:45Z), or None."""
    try:
        t = datetime.datetime.strptime(stamp, "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=datetime.timezone.utc)
    except (TypeError, ValueError):
        return None
    return t.astimezone().strftime(DAY)


def boards(board):
    """The board, then the boards TW_BOARD_ALSO names (paths, apart by the system's path separator)."""
    out = [Path(board)]
    for p in os.environ.get(ALSO, "").split(os.pathsep):
        if p.strip() and Path(p.strip()).resolve() not in [b.resolve() for b in out]:
            out.append(Path(p.strip()))
    return out


def _once(board, pattern):
    """The files of every board that match, each station's file name once: a record a second relay wrote and
    somebody copied to the shared board is one record."""
    seen, files = set(), []
    for b in boards(board):
        for f in b.glob(pattern):
            if (f.parent.parent.name, f.name) not in seen:
                seen.add((f.parent.parent.name, f.name))
                files.append(f)
    return files


def _files(board, since=None):
    """The leg records, oldest first. A run is named by its local start (20261005-161419-<pid>) and lasts 12 hours
    at most, so with since (a day) the runs that started before the day ahead of it are not even opened."""
    files = sorted(_once(board, "relay/*/legs/*.json"), key=lambda p: p.name)
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
    None when it cannot be said: a leg is not measured and there is no rate to count it by. Booked agents are
    counted from their cost too."""
    if (day["unmeasured"] or day["agents_usd"]) and per_usd is None:
        return None
    return day["week"] + (day["usd_unmeasured"] + day["agents_usd"]) * (per_usd or 0.0), day["unmeasured"]


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
            "usd_unmeasured": 0.0, "week_units": {}, "agents": 0, "agents_usd": 0.0}


def booked(board, since=None):
    """What agents.py booked for the agents sessions spawned outside the relay: [{day, station, usd, agents}], one
    per station and day, for since (a day) or later. A station books its day's total, so a newer booking of the
    same day replaces the older one; a file that does not read counts as nothing."""
    out = []
    for f in sorted(_once(board, "relay/*/agents/*.json"), key=lambda p: (p.name, p.parent.parent.name)):
        rec = _read(f) or {}
        usd, n = rec.get("usd"), rec.get("agents")
        if f.stem < (since or "") or not isinstance(usd, (int, float)) or isinstance(usd, bool) or usd < 0:
            continue                                            # the file is named by its day
        out.append({"day": f.stem, "station": f.parent.parent.name, "usd": float(usd),
                    "agents": n if isinstance(n, int) and not isinstance(n, bool) and n > 0 else 0})
    return out


def _days(board, since, home, lim):
    """{day: {day, usd, legs, estimated, units: {unit: {phase: usd}}, week, unmeasured, usd_unmeasured,
    week_units: {unit: {phase: [percent points measured, dollars of the legs not measured]}}, agents, agents_usd}}
    for the legs started on since or later, and the agents booked for those days (in usd, and apart in agents_usd)."""
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
    for rec in booked(board, since):
        d = out.setdefault(rec["day"], _empty(rec["day"]))
        d["usd"] += rec["usd"]
        d["agents_usd"] += rec["usd"]
        d["agents"] += rec["agents"]
    return out


def pace_share(now, lim):
    """The share of the day's cap the pace allows by now, 0 to 1, or None when there is no pace (limits.json
    pace_to_hour is not after pace_from_hour)."""
    a, z = float(lim.get("pace_from_hour", 0)), float(lim.get("pace_to_hour", 0))
    if z <= a:
        return None
    hour = now.hour + now.minute / 60.0 + now.second / 3600.0
    return min(1.0, max(0.0, (hour - a) / (z - a)))


def day_budget(board, lim=None, home=None, now=None):
    """The day's budget as the runner and every screen count it, or None when none is set.
      unit              "pct": percent points of the week (limits.json day_budget_pct), whenever there is a rate
                        "usd": dollars of cost (day_budget_usd), while nothing can be said in percent
      cap, spent, left  in that unit; left goes under 0 when the day is overspent
      allowed, free     what the pace allows by now, and what of that is not spent yet; None with no pace
      left_usd, free_usd    left and free in dollars of cost: what a leg's own cap and its usual cost are in
      about             True when spent is not all measured
      day               the day's sums (spent())"""
    lim = lim or config.limits()
    d = spent(board, None, home, lim)
    per_usd, guess = _rate(board, home, lim)
    pct = float(lim.get("day_budget_pct") or 0)
    if pct > 0 and per_usd is not None:
        used, est = week(d, per_usd)
        b = {"unit": "pct", "cap": pct, "spent": used, "usd_per": 1.0 / per_usd,
             "about": bool(guess or est or d["agents_usd"])}
    elif lim.get("day_budget_usd"):
        b = {"unit": "usd", "cap": float(lim["day_budget_usd"]), "spent": d["usd"], "usd_per": 1.0,
             "about": bool(d["estimated"] or d["agents_usd"])}
    else:
        return None
    share = pace_share(now or datetime.datetime.now(), lim)
    b["left"] = b["cap"] - b["spent"]
    b["allowed"] = None if share is None else b["cap"] * share
    b["free"] = None if share is None else b["allowed"] - b["spent"]
    b["left_usd"] = b["left"] * b["usd_per"]
    b["free_usd"] = None if share is None else b["free"] * b["usd_per"]
    b["day"] = d
    return b


def amount(b, x, of=False, usd=False):
    """A figure of the day's budget in its own unit: "$4.00", or "0.22%" ("0.22% of the week" with of). usd: x is
    dollars of cost, to be said in the budget's unit."""
    if b["unit"] == "usd":
        return "$%.2f" % x
    return "%.2f%%%s" % (x / b["usd_per"] if usd else x, " of the week" if of else "")


def pace_at(b, need_usd, lim, now=None):
    """The time today from which the pace covers need_usd more dollars of cost (a whole minute, not before now), or
    None when there is no pace or the day's cap does not cover it at all."""
    now = now or datetime.datetime.now()
    a, z = float(lim.get("pace_from_hour", 0)), float(lim.get("pace_to_hour", 0))
    share = (b["spent"] + need_usd / b["usd_per"]) / b["cap"]
    if b["allowed"] is None or share > 1.0:
        return None
    at = now.replace(hour=0, minute=0, second=0, microsecond=0) + datetime.timedelta(hours=a + share * (z - a))
    if at.second or at.microsecond:
        at = at.replace(second=0, microsecond=0) + datetime.timedelta(minutes=1)
    return max(at, now)


def pace_line(b, lim, now=None, need_usd=None):
    """One line for `relay.py day` and `relay.py budget`: what the pace allows by now and what of it is free. With
    need_usd (the usual cost of a unit) it also says when the next unit may start, when the pace does not cover
    one now. None when there is no budget or no pace."""
    if not b or b["allowed"] is None:
        return None
    now = now or datetime.datetime.now()
    say = (lambda x: "$%.2f" % x) if b["unit"] == "usd" else _pct
    head = "Pace: %s%s allowed by %s" % (say(b["allowed"]), " of the week" if b["unit"] == "pct" else "",
                                        now.strftime("%H:%M"))
    if b["left"] <= 0:
        return head + ". The day's cap is spent."
    about = "about " if b["about"] else ""
    tail = (", %s%s of it free." % (about, say(b["free"])) if b["free"] >= 0 else
            ", and the day is %s%s ahead of that." % (about, say(-b["free"])))
    if need_usd is not None and b["free_usd"] < need_usd:
        at = pace_at(b, need_usd, lim, now)
        tail += (" The next unit may start at %s." % at.strftime("%H:%M") if at else
                 " The day does not cover another unit.")
    return head + tail


def spent(board, day=None, home=None, lim=None):
    day = day or today()
    return _days(board, day, home, lim).get(day) or _empty(day)


def history(board, days=7, home=None, lim=None):
    first = datetime.datetime.now() - datetime.timedelta(days=days - 1)
    got = _days(board, first.strftime(DAY), home, lim)
    names = [(first + datetime.timedelta(days=i)).strftime(DAY) for i in range(days)]
    return [got.get(n) or _empty(n) for n in names]


def lines(board, budget, days=7, home=None, lim=None, now=None):
    """What `relay.py budget` prints: today against the budget, the pace, per unit and phase, then the days
    before. budget is the cap in dollars, which counts while nothing can be said in percent."""
    lim = dict(lim or config.limits(), day_budget_usd=budget or 0)
    hist = history(board, days, home, lim)
    today_ = hist[-1]
    b = day_budget(board, lim, home, now)
    pace = [pace_line(b, lim, now)] if pace_line(b, lim, now) else []
    per_usd, guess = _rate(board, home, lim)
    if per_usd is not None:
        cap = b["cap"] if b and b["unit"] == "pct" else None
        out = _week_lines(hist, budget, per_usd, GUESS % lim["week_usd"] if guess else "", cap)
        return out[:1] + pace + out[1:]
    est = ", %d estimated" % today_["estimated"] if today_["estimated"] else ""
    head = "Today %s: $%.2f spent" % (today_["day"], today_["usd"])
    head += (" of $%.2f, $%.2f left." % (budget, max(0.0, budget - today_["usd"])) if budget else ". No day budget is set.")
    out = [head + " %d leg%s%s." % (today_["legs"], "" if today_["legs"] == 1 else "s", est)] + pace
    for unit, phases in sorted(today_["units"].items()):
        order = sorted(phases, key=lambda p: (PHASES.index(p) if p in PHASES else len(PHASES), p))
        out.append("  %-34s %s" % (unit, "  ".join("%s $%.2f" % (p, phases[p]) for p in order)))
    if today_["agents_usd"]:
        out.append("  %-34s about $%.2f (%s)" % (OTHERS, today_["agents_usd"], _agents(today_["agents"])))
    before = [d for d in hist[:-1] if d["legs"] or d["agents_usd"]]
    if before:
        out.append("Before, in the last %d days:" % (len(hist) - 1))
        out += ["  %s  $%7.2f  %d leg%s" % (d["day"], d["usd"], d["legs"], "" if d["legs"] == 1 else "s")
                for d in before]
    return out


def _pct(x):
    return "%.1f%%" % x


def _legs(n):
    return "%d leg%s" % (n, "" if n == 1 else "s")


def _agents(n):
    return "%d agent%s" % (n, "" if n == 1 else "s")


GUESS = " A guess: no leg is measured yet, so a full week is taken as $%d of leg cost."
OTHERS = "agents outside the relay"


def _used_by(day):
    """Who used the day: the relay, and the agents booked from outside it when there are any."""
    return "the relay" + (" and %s outside it" % _agents(day["agents"]) if day["agents_usd"] else "")


def _week_lines(hist, budget, per_usd, guess="", cap=None):
    """The budget report in percent of the week. A figure counted from cost, not measured, is marked "about".
    cap: the day's cap in percent points (limits.json day_budget_pct); with None it is counted from budget, the
    cap in dollars, and so it is "about" too."""
    now = hist[-1]
    used, est = week(now, per_usd)
    head = "Today %s: %s%s of the week used by %s" % (now["day"], "about " if guess or now["agents_usd"] else "",
                                                      _pct(used), _used_by(now))
    head += (", the day's cap is %s." % _pct(cap) if cap else
             ", the day's cap is about %s." % _pct(budget * per_usd) if budget else ". No day budget is set.")
    out = [head + " %s%s.%s" % (_legs(now["legs"]), ", %d estimated from cost" % est if est and not guess else "",
                                guess)]
    for unit, phases in sorted(now["week_units"].items()):
        order = sorted(phases, key=lambda p: (PHASES.index(p) if p in PHASES else len(PHASES), p))
        out.append("  %-34s %s" % (unit, "  ".join(
            "%s %s%s" % (p, "about " if phases[p][1] else "", _pct(phases[p][0] + phases[p][1] * per_usd))
            for p in order)))
    if now["agents_usd"]:
        out.append("  %-34s about %s (%s)" % (OTHERS, _pct(now["agents_usd"] * per_usd), _agents(now["agents"])))
    before = [d for d in hist[:-1] if d["legs"] or d["agents_usd"]]
    if before:
        out.append("Before, in the last %d days:" % (len(hist) - 1))
        out += ["  %s  %s%6s  %s" % (d["day"], "about " if d["unmeasured"] or d["agents_usd"] else "      ",
                                    _pct(week(d, per_usd)[0]), _legs(d["legs"])) for d in before]
    return out


def one_line(board, budget, home=None, lim=None):
    """One line for `relay.py status`, `relay.py day` and the stop line: the day in percent of the week whenever
    there is a rate, in dollars until then. It says so when legs are counted at the usual cost, not their own, or
    from their cost, not a reading. budget is the cap in dollars, which counts while limits.json day_budget_pct is
    0 or nothing can be said in percent."""
    lim = dict(lim or config.limits(), day_budget_usd=budget or 0)
    now = spent(board, None, home, lim)
    b = day_budget(board, lim, home)
    per_usd, guess = _rate(board, home, lim)
    if per_usd is not None:
        used, est = week(now, per_usd)
        if b and b["unit"] == "pct":
            cap = ", %s left of a day's cap of %s" % (_pct(max(0.0, b["left"])), _pct(b["cap"]))
        elif budget:
            cap = ", %s left of a day's cap of about %s" % (_pct(max(0.0, budget * per_usd - used)),
                                                           _pct(budget * per_usd))
        else:
            cap = " (no day budget)"
        return "Today: %s%s of the week used by %s%s.%s" % (
            "about " if guess or now["agents_usd"] else "", _pct(used), _used_by(now), cap,
            GUESS % lim["week_usd"] if guess else
            " %d of %d legs estimated from their cost." % (est, now["legs"]) if est else "")
    est = " %d of %d legs counted at the usual cost." % (now["estimated"], now["legs"]) if now["estimated"] else ""
    if now["agents_usd"]:
        est += " $%.2f of it by %s outside the relay." % (now["agents_usd"], _agents(now["agents"]))
    if not budget:
        return "Today: $%.2f spent (no day budget).%s" % (now["usd"], est)
    return "Today: $%.2f of $%.2f spent, $%.2f left.%s" % (now["usd"], budget, max(0.0, budget - now["usd"]), est)
