#!/usr/bin/env python3
"""One screen for the master and the owner (relay.py day): what the day has left, what the day's pace allows by
now, where the plan's week stands when something read it, is a run going or how the last one stopped, what is queued in the order the runner takes it
with the usual cost of each unit, what needs the owner (also a unit that failed in an older run and is still
queued), what he answered on the asset board's Decide page that no session has taken up, and who holds the relay
build.

  lines(board, home, lim, ph, holder=None, answered=None)   the screen, as plain lines
  queue(board)                               (the units not done yet in the runner's order, what is wrong with the rest)
  live(home)                                 the lock records of the runs going on this machine
Only what has a source today is shown. Learnings, tools waiting to land, proposed work and the asset queue are not
built on this lane yet: they get a line when they exist, never a guess.
Reads only: nothing here writes, commits or starts anything. It reads this machine's copy of the board, and a run
going on another machine shows up as its stop record once the board is pulled.
Stdlib only. ASCII only.
"""
import glob, sys, textwrap
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
import pipeline as P                    # noqa: E402
import config, ledger, runner, sources, usage    # noqa: E402
from sources import lane                # noqa: E402

FINE = ("PASS",)                         # how a unit ends when it needs nobody
STOPS = 50                               # how many stop records back "Needs you" looks for a unit's newest outcome


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


def outcomes(board):
    """{unit: (how it ended, the run)}: each unit's newest outcome over the newest STOPS stop records on the board,
    any station. A run is named by its local start, so the newest name is the newest run."""
    out = {}
    for f in sorted(Path(board).glob("relay/*/stops/*.json"), key=lambda p: p.name)[-STOPS:]:
        try:
            rec = P.read_json(f)
        except (OSError, ValueError):
            continue
        units = rec.get("units") if isinstance(rec, dict) else None
        for u, v in (units if isinstance(units, dict) else {}).items():
            out[u] = (v, rec.get("run") or f.stem)
    return out


def unresolved(board, uid, verdict):
    """True while a unit that did not pass in an older run still needs somebody: queued lane work while its queue
    file is there and it has no done file; a board job while its newest result is still that verdict and nothing
    newer was recorded for its stage. A unit nobody queues any more needs nobody."""
    root = Path(board)
    if (root / "relay" / "done" / (uid + ".json")).exists():
        return False
    if (root / "relay" / "queue" / (uid + ".json")).exists():
        return True
    try:
        mine = [P.read_json(f) for f in root.glob("results/%s--*.json" % glob.escape(uid))]
        last = max(mine, key=lambda r: r["attempt"]) if mine else None
        if not last or last.get("verdict") != verdict:
            return False
        stage = [P.read_json(f) for f in root.glob("results/%s--%s--*.json" % (glob.escape(last["item"]),
                                                                              glob.escape(last["stage"])))]
        return not any(str(r.get("finished_at") or "") > str(last.get("finished_at") or "") for r in stage)
    except (OSError, ValueError, KeyError, TypeError):
        return False


def stands(board, uid, lim=None, turns=()):
    """(how, record) of a queued unit that did not pass: how the runner stands with it as sources/lane.py standing
    says ('' it is taken as any other, 'aside', 'parked'), and what the board keeps of its tries. ('', {}) for a
    unit that is not queued lane work or has no such record."""
    p = Path(board) / "relay" / "queue" / (uid + ".json")
    try:
        rec = lane.tried(board, uid)
        return (lane.standing(rec, lane.load(p), lim, turns) if p.exists() else ""), (rec if p.exists() else {})
    except (SystemExit, OSError, ValueError, KeyError):
        return "", {}


def asks(board, uid, lim=None, turns=()):
    """What a unit that did not pass asks of the owner, as the end of its line under "Needs you"; None when it
    asks nothing: a queued unit that only failed steps aside and comes back by itself, and the screen lists it
    under "Stepping aside". A unit that is parked asks (rewrite, split or drop), and so does one whose leg said
    BLOCKED, which is tried once more first."""
    how, rec = stands(board, uid, lim, turns)
    if how == "parked":
        return "; parked after %d %s: rewrite, split or drop" % (
            lane.tries_of(rec), "try" if lane.tries_of(rec) == 1 else "tries")
    if rec.get("verdict") == "FAIL":
        return None
    if rec.get("verdict") == "BLOCKED":
        left = lane.turns_left(rec, uid, lim or config.limits(), turns)
        return "; one more try %s" % ("after %d more turn%s of other work" % (left, "" if left == 1 else "s")
                                      if left else "at its next turn")
    return ""


def older(board, shown, lim=None, turns=()):
    """What needs the owner from runs before the one on the screen: every unit not in `shown` whose newest outcome
    is not a pass, that is still unresolved() and that asks() something. The newest stop record alone lost them:
    an empty run, or another station's, and the screen said "Needs you: nothing." (the review, R001.6)."""
    out = []
    for u, (v, run) in sorted(outcomes(board).items()):
        if u in shown or v in FINE or not unresolved(board, u, v) or asks(board, u, lim, turns) is None:
            continue
        out.append("%s ended %s in run %s%s" % (u, v, run, asks(board, u, lim, turns)))
    return out


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


def run_lines(board, home, lim=None, turns=()):
    """Is a run going here, else how the newest run on the board stopped. Second value: what needs the owner: the
    units of that run that did not pass and ask something (asks()), then those of older runs that are still
    unresolved (older())."""
    out, needs, shown = [], [], set()

    def ended(units, where):
        return ["%s ended %s %s%s" % (u, v, where, asks(board, u, lim, turns)) for u, v in sorted(units.items())
                if v not in FINE and asks(board, u, lim, turns) is not None]
    for rec in live(home):
        who = rec["who"].split()
        prog = Path(home) / "runs" / (who[1] if len(who) > 1 else "") / "progress.json"
        pr = P.read_json(prog) if len(who) > 1 and prog.exists() else {}
        units = pr.get("units") or {}
        out.append("Run going: %s on %s, started by %s."
                   % (who[1] if len(who) > 1 else rec["who"], rec.get("lane"), pr.get("started_by") or "?"))
        out.append("  %d leg%s so far%s." % (pr.get("legs", 0), "" if pr.get("legs", 0) == 1 else "s",
                                           (", now on %s" % pr["now_on"]) if pr.get("now_on") else ""))
        needs += ended(units, "in the run going now")
        shown |= set(units)
    if out:
        return out, needs + older(board, shown, lim, turns)
    last = last_stop(board)
    if not last:
        return ["No run going. No run yet."], needs
    station, rec = last
    units = rec.get("units") or {}
    out.append("No run going. Last run %s (%s): %d leg%s%s."
               % (rec.get("run"), station, rec.get("legs", 0), "" if rec.get("legs", 0) == 1 else "s",
                  runner.tally(units)))
    out.append("  It stopped: %s." % str(rec.get("reason", "?")).rstrip("."))
    needs += ended(units, "in the last run")
    return out, needs + older(board, set(units), lim, turns)


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


def next_turns(board, home, lim, names, n, usual, state=None):
    """(the next n turns as the runner would take them now, a note or ''): sources.forecast over what the runner's
    own pick reads (runner.pick_state). With no sources named: the runner's default; when the board's jobs cannot
    be read here (a machine that is no station, a board that is only a folder) the lane queue alone, and the note
    says so."""
    ctx = dict(state or runner.pick_state(board, home, lim), board=Path(board), skip=set(), work=None)
    if not names:                                           # a board with no items has no jobs to read
        names = [s for s in sources.ORDER if s != "pipeline" or any((Path(board) / "items").glob("*.json"))]
    try:
        ctx["station"] = P.station()
        return sources.forecast(list(names), ctx, n, usual), ""
    except (SystemExit, Exception) as e:                    # noqa: BLE001 - the screen never stops on a source
        if list(names) == ["lane"]:
            raise
        return sources.forecast(["lane"], ctx, n, usual), "the board's jobs were not read (%s): lane work only" % (
            str(e) or type(e).__name__)


def area_line(units, turns):
    """Where the queued lane work is, by the area of the game a wide turn goes round: how many units wait in each
    and how many turns ago it had its last wide one."""
    count, last = {}, {}
    for u in units:
        a = sources.area_of("lane", u["role"], u["lane"])
        count[a] = count.get(a, 0) + 1
    for i, t in enumerate(turns):
        if t.get("turn") == "wide":
            last[sources.area_of(t.get("source"), t.get("role"), t.get("lane"))] = len(turns) - i
    return "Areas: " + "; ".join("%s %d waiting (%s)" % (a, count[a], "last wide turn %d ago" % last[a] if a in last
                                                         else "no wide turn yet")
                                 for a in sources.GAME + sources.OTHER if a in count) + "."


def lines(board, home, lim, ph, holder=None, answered=None, now=None, names=None, turns=None):
    """What `relay.py day` prints. At most limits.json day_queue_rows rows in each list, no line over
    day_line_chars: a row is cut there, a sentence goes on over the next line. `answered` is answers.read(): with
    None the block "Your answers" is left out (a caller that did not look says nothing about it). now: the time
    the pace is read at (a test's own clock). names: the sources the run is started with (--sources), turns: how
    many of the next turns to show."""
    width, rows = int(lim["day_line_chars"]), int(lim["day_queue_rows"])
    units, problems = queue(board)
    price = ledger.need(ledger.usuals(board, home, lim), ph, ("plan", "execute"))
    out = fit(ledger.one_line(board, lim["day_budget_usd"], home, lim), width)
    pace = ledger.pace_line(ledger.day_budget(board, lim, home, now), lim, now, price if units else None)
    out += fit(pace, width) if pace else []
    out += fit(week_line(board, home, lim), width) if week_line(board, home, lim) else []
    state = runner.pick_state(board, home, lim)
    run, needs = run_lines(board, home, lim, state["turns"])
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
        coming, note = next_turns(board, home, lim, names, int(turns or rows), price, state)
        out += runner.turn_lines(coming, width)
        out += fit("  Note: %s." % note, width) if note else []
        aside, parked = [], 0
        for u in units:
            how, rec = stands(board, u["id"], lim, state["turns"])
            parked += how == "parked"
            if how == "parked" and not any(n.startswith(u["id"] + " ") for n in needs):
                needs.append("%s did not pass%s" % (u["id"], asks(board, u["id"], lim, state["turns"])))
            if how == "aside":
                left = lane.turns_left(rec, u["id"], lim, state["turns"])
                aside.append("  - %s: %s, try %d, back after %d more turn%s of other work"
                             % (u["id"], rec["verdict"], lane.tries_of(rec), left, "" if left == 1 else "s"))
        waiting = len(units) - len([u for u in coming if u["source"] == "lane"]) - len(aside) - parked
        if waiting > 0:
            out.append("  ... and %d more behind them" % waiting)
        if aside:
            out.append("Stepping aside (each comes back by itself): %d." % len(aside))
            out += [cut(a, width) for a in aside[:rows]]
            if len(aside) > rows:
                out.append("  ... and %d more" % (len(aside) - rows))
        if int(lim.get("wide_every") or 0):
            out += fit(area_line(units, state["turns"]), width)
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
