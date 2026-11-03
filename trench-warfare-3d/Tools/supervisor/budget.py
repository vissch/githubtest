#!/usr/bin/env python3
"""The day's budget in the owner's words: one command instead of an env var, a restart and a stop for every relay.

    python Tools/supervisor/budget.py set 7% --until 18:00     7% of the week MORE between now and 18:00
    python Tools/supervisor/budget.py set 15% --for 10h        15% more over the next 10 hours (crosses midnight fine)
    python Tools/supervisor/budget.py set 26%                  26% for today: the cap until midnight
    python Tools/supervisor/budget.py set 11                   the standing rule, held until midnight over any other figure
    python Tools/supervisor/budget.py show                     the figure now, the window, until when
    python Tools/supervisor/budget.py clear                    the open window ends now: the old figures stand again
  set takes --used P: what the day has used so far, in percent of the week, when the board's ledger cannot be read
  here (it is read through Tools/relay/ledger.py otherwise; unread counts as 0, which spends less, never more).

The cap is what the relay's pace reads: limits.json day_budget_pct, percent points of the week spent evenly from
00:00 to 24:00 (ledger.pace_share). "X% more until T" is the cap whose pace allows what is used now plus X by T:
    cap = (used + X) * 24 / T_hours                    (T today)
The pace runs from 00:00, so part of X may go at once (10:00, 0 used, "7% until 18:00": cap 9.33, 3.9% free at
once); by T it is exactly used + X. A window that crosses midnight spreads X over its hours: the part before midnight
is counted on today's used, the part after on a new day that starts at 0. At its end the window is gone and the
old figures hold again (TW_DAY_PCT, a run's -DayPct, day.json; 11% when none is set). "set 11" is a window too: 11
until midnight over any of them.

Library: day_pct(now) -> float, the cap now; path() -> state/budget.json in the supervisor's home (core.home()).
Readers: Tools/supervisor/spend.py day_figure() (autoloop's -DayPct for a start) and day_cap() (the runner, between
units, Run.allot). With no budget.json both keep the old figure (TW_DAY_PCT, limits.json, day.json).

WHY. The owner changed the day about ten times by chat in two weeks ("26% today", "another 4% until 9am", "only 2%
before 18:00", "15% for 10 hours"); each time a session did this sum by hand, set TW_DAY_PCT, restarted the loop and
stopped every relay (review of 2026-11-02, PLAN_relay_harness_v2_2026-11-02.md Phase 2).
"""
import argparse
import datetime
import os
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import spend  # noqa: E402

core = spend.core
STANDING = 11.0         # the owner's standing rule, 2026-10-07: 11% of the week a day for every relay together
KEEP = 20               # past windows kept in the file, for show
FMT = '%Y-%m-%dT%H:%M:%S'


def path(h=None):
    return core.state_path('budget', h)


def _whole(w):
    try:
        return w['kind'] in ('day', 'more') and float(w['pct']) >= 0 and _t(w['start']) < _t(w['end'])
    except (KeyError, TypeError, ValueError):
        return False


def load(h=None):
    """The windows in budget.json; a window that does not read whole is left out (never a crash)."""
    d = core.read_json(path(h), {})
    d = d if isinstance(d, dict) else {}
    ws = d.get('windows') if isinstance(d.get('windows'), list) else []
    return dict(d, windows=[w for w in ws if isinstance(w, dict) and _whole(w)])


def _t(s):
    return datetime.datetime.strptime(s, FMT)


def midnight(t):
    return t.replace(hour=0, minute=0, second=0, microsecond=0)


def cap_of(w, now):
    """The day's cap a window gives at `now` (inside it)."""
    if w['kind'] == 'day':
        return float(w['pct'])
    start, end = _t(w['start']), _t(w['end'])
    day0 = midnight(now)
    a, z = max(start, day0), min(end, day0 + datetime.timedelta(days=1))
    span = (end - start).total_seconds()
    part = float(w['pct']) * max((z - a).total_seconds(), 0) / span if span > 0 else 0.0
    base = float(w.get('used') or 0) if midnight(start) == day0 else 0.0
    hours = (z - day0).total_seconds() / 3600.0
    return (base + part) * 24.0 / hours if hours > 0 else STANDING


def window(now, h=None):
    """The newest window that holds `now`, or None."""
    for w in reversed(load(h)['windows']):
        try:
            if _t(w['start']) <= now < _t(w['end']):
                return w
        except (KeyError, ValueError):
            continue
    return None


def day_pct(now=None, h=None):
    """The day's cap now, percent points of the week, 0..100: the window's, else the standing rule."""
    now = now or datetime.datetime.now()
    w = window(now, h)
    return round(min(100.0, max(0.0, cap_of(w, now) if w else STANDING)), 2)


def pct(text):
    m = re.fullmatch(r'\s*(\d+(?:\.\d+)?)\s*%?\s*', str(text))
    if not m:
        raise ValueError(f'{text!r} is not a percent of the week (7%, 11, 2.5%)')
    return float(m.group(1))


def until(text, now):
    """The next time of day `text` (18:00, 9am, 9) after now: today, else tomorrow."""
    m = re.fullmatch(r'\s*(\d{1,2})(?::(\d{2}))?\s*(am|pm)?\s*', str(text).lower())
    if not m:
        raise ValueError(f'{text!r} is not a time of day (18:00, 9am)')
    hh, mm = int(m.group(1)), int(m.group(2) or 0)
    if m.group(3) == 'pm' and hh < 12:
        hh += 12
    if m.group(3) == 'am' and hh == 12:
        hh = 0
    if hh > 23 or mm > 59:
        raise ValueError(f'{text!r} is not a time of day')
    t = now.replace(hour=hh, minute=mm, second=0, microsecond=0)
    return t if t > now else t + datetime.timedelta(days=1)


def hours_of(text):
    m = re.fullmatch(r'\s*(\d+(?:\.\d+)?)\s*(h|m|min)?\s*', str(text).lower())
    if not m:
        raise ValueError(f'{text!r} is not a length of time (10h, 90m)')
    x = float(m.group(1))
    return x / 60.0 if m.group(2) in ('m', 'min') else x


def used_now():
    """What the day has used so far, percent of the week, by the relay's own ledger; None when it cannot be read."""
    try:
        sys.path.insert(0, str(HERE.parent / 'relay'))
        sys.path.insert(0, str(HERE.parent / 'pipeline'))
        import config, ledger, pipeline  # noqa: E401
        board = Path(pipeline.board_dir())
        also = [p for p in os.environ.get(ledger.ALSO, '').split(os.pathsep) if p.strip()]
        also += [str(b) for b in sorted(board.parent.glob(board.name + '-[2-9]')) if b.is_dir()]   # every relay's board, as the start script counts them
        os.environ[ledger.ALSO] = os.pathsep.join(dict.fromkeys(also))
        b = ledger.day_budget(board, config.limits(overrides={'day_budget_pct': STANDING}))
        return float(b['spent']) if b and b.get('unit') == 'pct' else None
    except (SystemExit, Exception):  # noqa: BLE001
        return None


def set_(x, at=None, hours=None, used=None, now=None, h=None, said='', clear=False):
    """Store the owner's words. Returns the window written, or None for clear (an open window ends now)."""
    now = (now or datetime.datetime.now()).replace(microsecond=0)
    d = load(h)
    ws = [w for w in d['windows'] if _t(w['end']) <= now]          # the past stays for show; an open one ends here
    for w in d['windows']:
        if _t(w['start']) <= now < _t(w['end']):
            ws.append(dict(w, end=now.strftime(FMT), ended='replaced'))
    if clear:
        w = None
    elif at is None and hours is None:          # "26% today"; and "11" holds 11 until midnight over any other figure
        w = dict(kind='day', pct=x, start=now.strftime(FMT), end=(midnight(now) + datetime.timedelta(days=1)).strftime(FMT))
    else:
        end = at if at is not None else now + datetime.timedelta(hours=hours)
        if end <= now:
            raise ValueError('the window ends before it starts')
        if used is None:
            used = used_now()
        w = dict(kind='more', pct=x, start=now.strftime(FMT), end=end.strftime(FMT), used=round(float(used or 0), 2),
                 used_read=used is not None)
    if w:
        w.update(said=said, set_at=now.strftime(FMT))
        ws.append(w)
    ws.sort(key=lambda w: w['start'])
    core.write_json(path(h), dict(windows=ws[-KEEP:], standing=STANDING))
    return w


def show(now=None, h=None):
    now = now or datetime.datetime.now()
    w = window(now, h)
    out = [f'The day\'s cap now: {day_pct(now, h):g}% of the week (spent evenly over the day).']
    if not w:
        out.append(f'No window: the old figures stand (TW_DAY_PCT, a run\'s -DayPct, day.json; {STANDING:g}% when '
                   f'none is set).')
        return out
    if w['kind'] == 'day':
        out.append(f'Set {w["set_at"]}: {w["pct"]:g}% for today, until {w["end"][11:16]}; then {STANDING:g}% again.')
    else:
        out.append(f'Set {w["set_at"]}: {w["pct"]:g}% more until {w["end"].replace("T", " ")[:16]} on top of '
                   f'{w.get("used", 0):g}% used then{"" if w.get("used_read", True) else " (not read: counted 0)"}; '
                   f'then {STANDING:g}% again.')
        nxt = midnight(now) + datetime.timedelta(days=1)
        if _t(w['end']) > nxt:
            out.append(f'After midnight the cap is {day_pct(nxt, h):g}% until the window ends.')
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description="the day's budget in the owner's words")
    sub = ap.add_subparsers(dest='cmd', required=True)
    s = sub.add_parser('set')
    s.add_argument('pct')
    g = s.add_mutually_exclusive_group()
    g.add_argument('--until')
    g.add_argument('--for', dest='for_')
    s.add_argument('--used', type=float)
    sub.add_parser('show')
    sub.add_parser('clear')
    a = ap.parse_args(argv)
    now = datetime.datetime.now()
    try:
        if a.cmd == 'set':
            x = pct(a.pct)
            at = until(a.until, now) if a.until else None
            hrs = hours_of(a.for_) if a.for_ else None
            said = ' '.join(['set', a.pct] + (['--until', a.until] if a.until else []) + (['--for', a.for_] if a.for_ else []))
            set_(x, at, hrs, a.used, now, said=said)
        elif a.cmd == 'clear':
            set_(STANDING, now=now, said='clear', clear=True)
    except ValueError as e:
        print(f'budget: {e}', file=sys.stderr)
        return 2
    for line in show(now):
        print(line)
    return 0


if __name__ == '__main__':
    sys.exit(main())
