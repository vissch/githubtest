#!/usr/bin/env python3
"""One screen for the master and the owner (relay.py day): what the day has left, what the day's pace allows by
now, where the plan's week stands when something read it, is a run going or how the last one stopped, what is queued in the order the runner takes it
with the usual cost of each unit, what needs the owner, what he answered on the asset board's Decide page that no
session has taken up, and who holds the relay build.

  lines(board, home, lim, ph, holder=None, answered=None)   the screen, as plain lines
  queue(board)                               (the units not done yet in the runner's order, what is wrong with the rest)
  live(home)                                 the lock records of the runs going on this machine
Only what has a source today is shown. Learnings, tools waiting to land, proposed work and the asset queue are not
built on this lane yet: they get a line when they exist, never a guess.
Reads only: nothing here writes, commits or starts anything. It reads this machine's copy of the board, and a run
going on another machine shows up as its stop record once the board is pulled.
Stdlib only. ASCII only.
"""
import sys, textwrap
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
import pipeline as P                    # noqa: E402
import ledger, runner, usage            # noqa: E402
from sources import lane                # noqa: E402

FINE = ("PASS",)                         # how a unit ends when it needs nobody


def live(home):
    """The lock records of the runs that hold a checkout on this machine now."""
    locks = Path(home) / "locks"
    out = []
    for f in sorted(locks.glob("*.json")) if locks.is_dir() else []:
        try:
            rec = P.read_json(f)
        except (OSError, ValueError):
            continue
        if P.proc_start(rec["pid"]) == rec["pid_start"]:
            out.append(rec)
    return out


def last_stop(board):
    """(station, record) of the newest stop record on the board, any station, or None. A run is named by its local
    start, so the newest name is the newest run."""
    stops = sorted(Path(board).glob("relay/*/stops/*.json"), key=lambda p: p.name)
    for f in reversed(stops):
        try:
            rec = P.read_json(f)
        except (OSError, ValueError):
            continue
        if isinstance(rec, dict):
            return f.parent.parent.name, rec
    return None


def queue(board):
    """(units, problems): the queued units with no done file, in the order the runner takes them, and one line per
    queue file it will not take as it is (not committed on the board, or not a valid unit)."""
    ok, skipped = lane.files(board)
    units, problems = [], ["%s is not committed on the board, so the runner skips it" % p.name for p in skipped]
    for p in ok:
        try:
            u = lane.load(p)
        except SystemExit as e:
            problems.append(str(e).replace("relay: ", "", 1))
            continue
        if not (Path(board) / "relay" / "done" / (u["id"] + ".json")).exists():
            units.append(u)
    return units, problems


def cut(text, width):
    """A row of a list stays one line: a name too long for the screen is cut."""
    return text if len(text) <= width else text[:width - 3] + "..."


def fit(text, width):
    """A sentence too long for the screen goes on over the next line, indented: a stop reason is never cut."""
    return textwrap.wrap(text, width, subsequent_indent="    ") or [text]


def run_lines(board, home):
    """Is a run going here, else how the newest run on the board stopped. Second value: what needs the owner."""
    out, needs = [], []
    for rec in live(home):
        who = rec["who"].split()
        prog = Path(home) / "runs" / (who[1] if len(who) > 1 else "") / "progress.json"
        pr = P.read_json(prog) if len(who) > 1 and prog.exists() else {}
        units = pr.get("units") or {}
        out.append("Run going: %s on %s, started by %s."
                   % (who[1] if len(who) > 1 else rec["who"], rec.get("lane"), pr.get("started_by") or "?"))
        out.append("  %d leg%s so far%s." % (pr.get("legs", 0), "" if pr.get("legs", 0) == 1 else "s",
                                           (", now on %s" % pr["now_on"]) if pr.get("now_on") else ""))
        needs += ["%s ended %s in the run going now" % (u, v) for u, v in sorted(units.items()) if v not in FINE]
    if out:
        return out, needs
    last = last_stop(board)
    if not last:
        return ["No run going. No run yet."], needs
    station, rec = last
    units = rec.get("units") or {}
    out.append("No run going. Last run %s (%s): %d leg%s%s."
               % (rec.get("run"), station, rec.get("legs", 0), "" if rec.get("legs", 0) == 1 else "s",
                  runner.tally(units)))
    out.append("  It stopped: %s." % str(rec.get("reason", "?")).rstrip("."))
    needs += ["%s ended %s in the last run" % (u, v) for u, v in sorted(units.items()) if v not in FINE]
    return out, needs


def week_line(board, home, lim):
    """Where the plan's week stands: from a reading on this machine that is new enough, else from the newest one a
    leg left on the board, else None: a line with no source is not shown."""
    return usage.line(usage.read(home, lim)) or usage.line(ledger.standing(board), old=True)


def answer_lines(answered, width, rows):
    """The block "Your answers": what the owner answered on the Decide page that no session has taken up, as
    answers.read() gives it: (rows, why). The one block that also speaks when its source cannot be read: left out, a
    Drive that is not mounted would read as "nothing waits". A row starts with what he picked, so a long title is
    what gets cut."""
    got, why = answered
    if why:
        return fit("Your answers: not read (%s)." % why, width)
    if not got:
        return ["Your answers: nothing waits."]
    out = fit("Your answers: %d not taken up. What each leads to: briefs.py waiting." % len(got), width)
    out += [cut("  - %s: %s" % (r["picked"], r["title"]), width) for r in got[:rows]]
    if len(got) > rows:
        out.append("  ... and %d more" % (len(got) - rows))
    return out


def lines(board, home, lim, ph, holder=None, answered=None, now=None):
    """What `relay.py day` prints. At most limits.json day_queue_rows rows in each list, no line over
    day_line_chars: a row is cut there, a sentence goes on over the next line. `answered` is answers.read(): with
    None the block "Your answers" is left out (a caller that did not look says nothing about it). now: the time
    the pace is read at (a test's own clock)."""
    width, rows = int(lim["day_line_chars"]), int(lim["day_queue_rows"])
    units, problems = queue(board)
    price = ledger.need(ledger.usuals(board, home, lim), ph, ("plan", "execute"))
    out = fit(ledger.one_line(board, lim["day_budget_usd"], home, lim), width)
    pace = ledger.pace_line(ledger.day_budget(board, lim, home, now), lim, now, price if units else None)
    out += fit(pace, width) if pace else []
    out += fit(week_line(board, home, lim), width) if week_line(board, home, lim) else []
    run, needs = run_lines(board, home)
    for line in run:
        out += fit(line, width)
    if units:
        per_usd = ledger.rate(board, home, lim)
        if per_usd is None:
            out.append("Queue: %d unit%s, about $%.2f at the usual cost of $%.2f a unit (plan and execute)."
                       % (len(units), "" if len(units) == 1 else "s", price * len(units), price))
        else:
            out.append("Queue: %d unit%s, about %.1f%% of the week at the usual %.1f%% a unit (plan and execute)."
                       % (len(units), "" if len(units) == 1 else "s", price * len(units) * per_usd, price * per_usd))
        for n, u in enumerate(units[:rows], 1):
            out.append(cut("  %2d. %-34s priority %-3d %s" % (n, u["id"], lane.priority_of(u, lim), u["lane"]), width))
        if len(units) > rows:
            out.append("  ... and %d more" % (len(units) - rows))
    else:
        out.append("Queue: nothing is queued.")
    needs += problems
    out.append("Needs you: nothing." if not needs else "Needs you:")
    out += [cut("  - " + n, width) for n in needs[:rows]]
    if len(needs) > rows:
        out.append("  ... and %d more" % (len(needs) - rows))
    if answered is not None:
        out += answer_lines(answered, width, rows)
    if holder:
        out += fit("The relay build is held by %s until %s." % (holder["who"], holder["until"]), width)
    return out
