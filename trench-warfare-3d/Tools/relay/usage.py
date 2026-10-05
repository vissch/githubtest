#!/usr/bin/env python3
"""How much of the plan's weekly limit is used: the newest reading on this machine, and what a leg used of it.

  put(home, raw)           keep a reading that a source hands in; returns it as the relay keeps it, or None
  read(home, lim)          the newest reading {at, at_s, week, five_hour, resets, windows}, or None when there is
                           none or it is older than limits.json usage_max_age_seconds
  delta(start, end)        what was used of the week between two readings, in percent points, or None
  line(reading)            one line for a person: where the week stands
Nothing here calls anybody or reads a login: a source outside this file hands the numbers in (relay.py usage put).
Two shapes are taken: Claude Code's status line input (rate_limits.seven_day.used_percentage, resets_at in epoch
seconds: `relay.py usage put --statusline` is a status line command) and the usage answer Claude Code's own /usage
screen gets (seven_day.utilization, resets_at as a date). Both count 0 to 100.
A reading that is not there is None, never a guess: the caller says "not measured".
Stdlib only. ASCII only.
"""
import sys, time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
from pipeline import read_json, write_json   # noqa: E402
import legdir                           # noqa: E402

FILE = "usage.json"
WEEK, SHORT = "seven_day", "five_hour"


def _number(x):
    return float(x) if isinstance(x, (int, float)) and not isinstance(x, bool) else None


def _when(x):
    """A reset time as text: epoch seconds become a UTC stamp, a date stays as it came (cut to the minute)."""
    if _number(x) is not None:
        return time.strftime("%Y-%m-%d %H:%M", time.gmtime(x))
    return str(x)[:16].replace("T", " ") if x else None


def shape(raw, at_s):
    """A source's answer as the relay keeps it, or None when it holds no weekly figure. windows holds every window
    that came with a number (the week, the five hours, a week per model); week is the one the day is counted in."""
    raw = raw.get("rate_limits", raw) if isinstance(raw, dict) else {}
    windows, resets = {}, {}
    for name, w in raw.items() if isinstance(raw, dict) else ():
        if not isinstance(w, dict) or not (name == SHORT or name.startswith(WEEK)):
            continue
        u = _number(w.get("utilization"))
        u = _number(w.get("used_percentage")) if u is None else u
        if u is not None:
            windows[name], resets[name] = u, _when(w.get("resets_at"))
    if WEEK not in windows:
        return None
    return {"at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(at_s)), "at_s": int(at_s), "week": windows[WEEK],
            "five_hour": windows.get(SHORT), "resets": resets.get(WEEK), "windows": windows}


def state(home=None):
    p = Path(home or legdir.home()) / FILE
    try:
        rec = read_json(p) if p.exists() else {}
    except (OSError, ValueError):
        rec = {}
    return rec if isinstance(rec, dict) else {}


def put(home, raw, clock=time.time):
    """Keep what a source read just now. An answer with no weekly figure changes nothing and gives None."""
    reading = shape(raw, clock())
    if reading:
        home = Path(home or legdir.home())
        home.mkdir(parents=True, exist_ok=True)
        write_json(home / FILE, {"reading": reading})
    return reading


def read(home=None, lim=None, clock=time.time):
    """The newest reading, or None when there is none or it is too old to speak for now."""
    reading = state(home).get("reading")
    age = float((lim or {}).get("usage_max_age_seconds", 600))
    if not isinstance(reading, dict) or _number(reading.get("week")) is None:
        return None
    return reading if 0 <= clock() - reading.get("at_s", 0) <= age else None


def brief(reading):
    """A reading as a leg record keeps it: when, the week, when the week starts over."""
    return {k: reading.get(k) for k in ("at", "at_s", "week", "resets")} if reading else None


def delta(start, end):
    """What was used of the week between two readings, in percent points, or None when it was not measured: a
    reading is missing, or both are the same reading (nothing new came in while the leg ran). After the week
    turned over in between, it is what the new week holds."""
    if not start or not end or end.get("at_s", 0) <= start.get("at_s", 0):
        return None
    if end.get("resets") != start.get("resets") and end["week"] < start["week"]:
        return round(end["week"], 2)
    return round(max(0.0, end["week"] - start["week"]), 2)


def line(reading, old=False):
    """Where the plan's week stands, or None. old: the reading is a leg's, not one from just now, so it says when."""
    if not reading:
        return None
    short = "" if reading.get("five_hour") is None else ", 5-hour window %.0f%%" % reading["five_hour"]
    until = ", starts over %s UTC" % reading["resets"] if reading.get("resets") else ""
    when = str(reading.get("at") or "")
    if old:
        return "Week: %.0f%% used at a leg's last reading (%s %s UTC)%s." % (reading["week"], when[:10], when[11:16],
                                                                          until)
    return "Week: %.0f%% used%s (read %s UTC)%s." % (reading["week"], short, when[11:16], until)
