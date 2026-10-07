#!/usr/bin/env python3
"""The relay: runs work as a chain of short headless Claude sessions (legs), so no session fills its context.
Contract: docs/reference/relay.md. Settings: limits.json, phases.json, style.json and roles/ next to this file.

  python Tools/relay/relay.py run --work <checkout> [--dry-run] [--hours 3] [--max-legs N] [--sources pipeline,lane]
                                  [--view]  (a Windows Terminal tab per leg, showing its output)
                                  [--day-pct N]  (the day's cap in percent of the week, when the owner named one)
  python Tools/relay/relay.py status           is a run going, on what, and how the last one stopped
  python Tools/relay/relay.py stop [--now]     end the run before its next leg (--now: end the leg too)
  python Tools/relay/relay.py refusals [--runs N]   what the guard refused in the last N runs (default 3)
  python Tools/relay/relay.py budget [--days N]     what today's legs cost against the day's budget, and the days before
  python Tools/relay/relay.py day              one screen: the budget, the pace, the run, the queue in its order, what
                                               needs the owner
  python Tools/relay/relay.py agents [--day D]      what the agents cost that sessions here spawned outside the relay
  python Tools/relay/relay.py agents book [--out F | --file F] [--who NAME]   add that to the day's budget (agents.py)
  python Tools/relay/relay.py usage            where the plan's week stands, from the newest reading on this machine
  python Tools/relay/relay.py usage put [--statusline]   keep a reading given on stdin as JSON (usage.py)
  python Tools/relay/relay.py prio <id> <n>    move a queued unit: 0 to 99, the lower runs first (50 when none is set)
  python Tools/relay/relay.py role <role> <id> [<id> ..]   give queued units a role from Tools/pipeline/roles.json:
                                               their legs get that role's brief
  python Tools/relay/relay.py hold <who> [--hours 4] [--release]   one session at a time builds the relay or runs it
  python Tools/relay/relay.py update [<commit>]     move the frozen copy (githubtest-relay-run) to a commit
  python Tools/relay/relay.py add <id> --lane lane/show/x --role <role> --goal ".." --done-when <program> <arg> ..   queue lane work
  python Tools/relay/relay.py add --unit <file>     the same, the unit from a file in the shape of the queue's
  python Tools/relay/relay.py leg gate start|status|wait, leg finish, leg done      a leg's close-out (legcmd.py)
  python Tools/relay/relay.py proof meter      a small real leg with low thresholds: amber, red, a refused edit
  python Tools/relay/relay.py proof timeout    a leg that is killed at its time limit
  python Tools/relay/relay.py view <leg folder> [--follow]   the leg's output as readable lines
Stdlib only. ASCII only.
"""
import argparse, json, sys, tempfile, time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
import pipeline as P                    # noqa: E402
import agents, answers, boardio, config, day, gitio, launch, ledger, legcmd, legdir, runner, usage   # noqa: E402
from sources import lane as lane_source  # noqa: E402


# ---------- view ----------

def readable(line):
    """One output record as a short line, or None for the records nobody needs to see."""
    try:
        e = json.loads(line)
    except ValueError:
        return line.strip() or None
    if e.get("type") == "assistant":
        out = []
        for c in (e.get("message") or {}).get("content") or []:
            if c.get("type") == "text" and c.get("text", "").strip():
                out.append(c["text"].strip())
            elif c.get("type") == "tool_use":
                i = c.get("input") or {}
                out.append("> %s: %s" % (c.get("name"), str(i.get("command") or i.get("file_path") or
                                                             i.get("pattern") or i.get("description") or "")[:140]))
        return "\n".join(out) or None
    if e.get("type") == "result":
        return "== %s, %s turns, $%.2f" % (e.get("subtype"), e.get("num_turns"), e.get("total_cost_usd") or 0)
    return None


def view(d, follow):
    path, pos = Path(d) / legdir.OUT, 0
    while True:
        if path.exists():
            with open(path, "rb") as f:
                f.seek(pos)
                data = f.read()
            end = data.rfind(b"\n") + 1
            pos += end
            for line in data[:end].decode("utf-8", "replace").splitlines():
                text = readable(line)
                if text:
                    print(text, flush=True)
        if not follow or legdir.read(d).get("state") not in ("NEW", "RUNNING"):
            return
        time.sleep(1)


# ---------- proofs ----------

def proof(name):
    tmp = Path(tempfile.mkdtemp(prefix="relay-proof-"))
    work = tmp / "work"
    work.mkdir()
    lim = config.limits()
    unit = {"id": "proof-" + name, "source": "proof", "role": "proof"}
    run = "proof-%s-%d" % (name, time.time())
    if name == "meter":
        for n in range(1, 5):
            (work / ("big%d.txt" % n)).write_text("".join("%d-%04d trench wire mud shell lamp ridge crater\n" % (n, i)
                                                          for i in range(1900)), encoding="utf-8")
        def trips(d):
            p = d / "trips.jsonl"
            return [json.loads(x)["level"] for x in p.read_text(encoding="utf-8").splitlines()] if p.exists() else []
        # leg 1, amber: told to stop starting things, the leg leaves the later files unread
        lim.update(amber_tokens=70000, red_tokens=200000)
        body = ("Read big1.txt, big2.txt, big3.txt and big4.txt in full, one Read call each, one after another. "
                "Then end with your report.")
        d1 = launch.make_leg(run, 1, unit, "execute", work, "lane/show/proof", "", body, lim, model="sonnet")
        a = launch.run_leg(d1, lim, 300)
        amber = a["state"] == "DONE" and trips(d1) == ["amber"] and a["final_tokens"] < 160000
        print("amber leg: state %s, trips %s, final %s tokens, $%s" % (a["state"], trips(d1), a["final_tokens"],
                                                                      a["cost_usd"]))
        # leg 2, red: the guard itself, so the leg is asked to try one edit after the red message
        lim.update(amber_tokens=44000, red_tokens=45000)
        body = ("This leg tests the red guard. Read big1.txt in full. You will then get a red message. After it, try "
                "ONCE to create done.txt containing ok with the Write tool, then end with your report.")
        d = launch.make_leg(run, 2, unit, "execute", work, "lane/show/proof", "", body, lim, model="sonnet")
        b = launch.run_leg(d, lim, 300)
        denied = (d / "denials.jsonl").exists()
        red = b["state"] == "DONE" and "red" in trips(d) and denied and not (work / "done.txt").exists()
        print("red leg: state %s, trips %s, edit refused: %s, $%s" % (b["state"], trips(d), denied, b["cost_usd"]))
        ok = amber and red
    elif name == "timeout":
        body = "Run this shell command and wait for it to finish: python -c \"import time; time.sleep(600)\""
        d = launch.make_leg(run, 1, unit, "execute", work, "lane/show/proof", "", body, lim, model="sonnet")
        leg = launch.run_leg(d, lim, 25)
        gone = launch.proc_start(leg["child_pid"]) is None
        ok = leg["state"] == "TIMEOUT" and gone
        print("state %s after %s s, process gone: %s, mode %s" % (leg["state"], leg["seconds"], gone, leg["ran_mode"]))
    else:
        raise SystemExit("relay: no proof named %s" % name)
    print("leg folder: %s" % d)
    print("PROOF OK" if ok else "PROOF FAILED")
    return 0 if ok else 1


# ---------- status, stop, add ----------

def status():
    """Plain lines: the run that holds a checkout now (unit, leg, minutes), else how the last run stopped."""
    home, live = legdir.home(), 0
    for rec in day.live(home):
        live += 1
        run = rec["who"].split()[1] if len(rec["who"].split()) > 1 else ""
        legs = sorted((home / "runs" / run / "legs").glob("*/leg.json")) if run else []
        leg = P.read_json(legs[-1]) if legs else {}
        print("RUNNING: %s, lane %s" % (rec["who"], rec.get("lane")))
        prog = home / "runs" / run / "progress.json"
        if run and prog.exists():
            pr = P.read_json(prog)
            print("  started by %s, relay code %s; so far %d legs%s%s" % (
                pr.get("started_by"), (pr.get("code") or "")[:10] or "not in git", pr.get("legs", 0),
                runner.tally(pr.get("units") or {}),
                "".join("\n    %s: %s" % kv for kv in sorted((pr.get("units") or {}).items()))))
        if leg:
            print("  leg %02d (%s) is %s since %s; watch it: python Tools/relay/relay.py view \"%s\" --follow"
                  % (leg["leg"], leg["phase"], leg["state"], leg.get("started_at", "-"), legs[-1].parent))
        if runner.stop_path(home).exists():
            print("  a stop is asked: it ends before its next leg")
    if not live:
        stops = sorted((boardio.folder(P.board_dir(), P.station()) / "stops").glob("*.json"))
        last = P.read_json(stops[-1]) if stops else None
        print("NOT RUNNING." + (" Last run %s: %s (%d legs%s)." % (last["run"], last["reason"], last["legs"],
                                                                 runner.tally(last.get("units") or {}))
                                if last else " No run yet."))
    print(ledger.one_line(P.board_dir(), config.limits()["day_budget_usd"]))
    h = holder()
    if h:
        print("The relay build is held by %s until %s (relay.py hold)." % (h["who"], h["until"]))
    return 0


def budget(days):
    """Today's legs against the day's budget (limits.json day_budget_pct, or day_budget_usd while nothing can be
    said in percent), the pace, per unit and phase, then the days before."""
    for line in ledger.lines(P.board_dir(), config.limits()["day_budget_usd"], max(1, days)):
        print(line)
    return 0


def day_screen():
    """One screen for the master and the owner (day.py): reads only."""
    for line in day.lines(P.board_dir(), legdir.home(), config.limits(), config.phases(), holder(),
                          answers.read(*answers.folders())):
        print(line)
    return 0


def agents_cmd(a):
    """What the agents cost that this machine's sessions spawned outside the relay (agents.py), and `book`: add it
    to the day's budget. --out writes the booking to a file in place of the board (a machine that does not hold
    the board hands the file over); --file books such a file. While a run is going here the booking waits for it:
    the run writes it on the board before its next unit."""
    board, home, station = P.board_dir(), legdir.home(), P.station()
    if a.file and a.out:
        raise SystemExit("relay: agents book takes --out or --file, not both")
    if a.what != "book":
        counted = agents.count(a.day)
        there = agents.path(board, {"station": station, "day": counted["day"]})
        try:
            booked = P.read_json(there) if there.exists() else None
        except (OSError, ValueError):
            booked = None
        for line in agents.lines(counted, ledger.rate(board), booked):
            print(line)
        return 0
    if a.file:
        try:
            rec = P.read_json(Path(a.file))
        except (OSError, ValueError) as e:
            raise SystemExit("relay: the booking %s does not read: %s" % (a.file, e))
        if agents.check(rec):
            raise SystemExit("relay: %s is no booking of agents: %s" % (a.file, agents.check(rec)))
    else:
        rec = agents.record(agents.count(a.day), station, a.who or "")
    said = "%s %s: about $%.2f, %d agent%s" % (rec["station"], rec["day"], rec["usd"], rec["agents"],
                                             "" if rec["agents"] == 1 else "s")
    if a.out:
        P.write_json(Path(a.out), rec)
        print("written to %s (%s). Book it where the board is: relay.py agents book --file <that file>." % (a.out, said))
        return 0
    if day.live(home):
        agents.hand_in(home, rec)
        print("%s. A run is going here: it adds this to the day before its next unit." % said)
        return 0
    if not rec["agents"] and not agents.path(board, rec).exists():
        print("nothing to book: no session here spawned an agent for the project on %s." % rec["day"])
        return 0
    agents.keep(board, rec)
    print("booked %s. Board: %s." % (said, boardio.push(board, "relay: agents outside the relay, %s %s"
                                                         % (rec["station"], rec["day"]))))
    return 0


def usage_cmd(put, statusline):
    """Where the plan's week stands. `put` keeps a reading given on stdin as JSON, in either shape usage.py takes;
    with --statusline it is a Claude Code status line command: it keeps the reading and prints the short line the
    status line shows. Exit 1: no reading (none came in, or the newest is too old)."""
    home, lim = legdir.home(), config.limits()
    if put:
        try:
            raw = json.loads(sys.stdin.read() or "{}")
        except ValueError:
            raw = {}
        reading = usage.put(home, raw)
        if statusline:
            reading = reading or usage.read(home, lim)      # early in a session nothing comes in: show the last one
            print("week %.0f%%" % reading["week"] if reading else "week ?")
            return 0
        print(usage.line(reading) or "no weekly figure in what came in: nothing kept")
        return 0 if reading else 1
    reading = usage.read(home, lim)
    print(usage.line(reading) or day.week_line(P.board_dir(), home, lim)
          or "Week: not measured. Nothing has read the plan's weekly limit here or in a leg yet.")
    return 0 if reading else 1


def stop(now):
    P.write_json(runner.stop_path(legdir.home()), {"now": bool(now), "asked_at": P.now()})
    print("stop asked: the run ends %s." % ("now, mid-leg" if now else "before its next leg"))
    return 0


def refusals(runs):
    """Every command the guard refused in the newest runs, with the reason. A refusal of normal work is a bug in
    the rules: it becomes a line in the ordinary-work tests and a fix in cmdrules.py."""
    root = legdir.home() / "runs"
    names = sorted((p.name for p in root.iterdir() if p.is_dir() and not p.name.startswith("proof-")),
                   reverse=True)[:runs] if root.is_dir() else []
    total = 0
    for run in reversed(names):
        for f in sorted((root / run / "legs").glob("*/denials.jsonl")):
            leg = legdir.read(f.parent) or {}
            for line in f.read_text(encoding="utf-8").splitlines():
                try:
                    e = json.loads(line)
                    tin = json.loads(e.get("input") or "{}")
                except ValueError:
                    e, tin = e if isinstance(e, dict) else {}, {}
                what = tin.get("command") or tin.get("file_path") or (e.get("input") or "")
                total += 1
                print("%s leg %s (%s) %s: %s\n    %s" % (run, f.parent.name, leg.get("phase"), e.get("tool"),
                                                        e.get("why"), " ".join(str(what).split())[:220]))
    print("%d refused in %d run%s." % (total, len(names), "" if len(names) == 1 else "s"))
    return 0


HOLD = "build-hold.json"


def holder():
    """Who holds the relay build now ({who, since, until}), or None. A hold ends by itself: a session that went
    away must not block the next one for ever."""
    p = legdir.home() / HOLD
    try:
        rec = P.read_json(p) if p.exists() else None
    except (OSError, ValueError):
        return None
    return rec if rec and rec.get("until_s", 0) > time.time() else None


def hold(who, hours, release):
    """One session at a time changes the relay or starts its runs. `hold <who>` takes or renews the hold and exits
    1 when another name has it: then stop and tell the owner. It is a promise between sessions, not a lock on
    files."""
    p, cur = legdir.home() / HOLD, holder()
    if cur and cur["who"] != who:
        print("HELD by %s since %s, until %s. Do not edit the relay or start a run: stop and tell the owner."
              % (cur["who"], cur["since"], cur["until"]))
        return 1
    if release:
        if p.exists():
            p.unlink()
        print("released: nobody holds the relay build")
        return 0
    until = time.time() + max(0.25, min(hours, 12)) * 3600
    P.write_json(p, {"who": who, "since": cur["since"] if cur else P.now(), "until_s": until,
                     "until": time.strftime("%Y-%m-%d %H:%M", time.localtime(until))})
    print("held by %s until %s. Renew with the same command; give it back with --release."
          % (who, time.strftime("%H:%M", time.localtime(until))))
    return 0


def update(ref):
    """Move the frozen copy of the relay (a detached checkout that only runs it) to a commit. Refused while a run
    is going, in a checkout that is on a branch (that one is for building), and over uncommitted changes."""
    home = legdir.home()
    for rec in day.live(home):
        raise SystemExit("relay: a run is going (%s): update after it stops" % rec["who"])
    if gitio.git(["rev-parse", "--abbrev-ref", "HEAD"], HERE) != "HEAD":
        raise SystemExit("relay: this checkout is on a branch (%s): it is for building. Update the frozen copy, "
                         "githubtest-relay-run" % gitio.git(["rev-parse", "--abbrev-ref", "HEAD"], HERE))
    if gitio.code_state(HERE)[1]:
        raise SystemExit("relay: the frozen copy has uncommitted changes: %s" % ", ".join(gitio.code_state(HERE)[1][:3]))
    gitio.git(["fetch", "-q", "origin"], HERE, check=False)
    gitio.git(["checkout", "-q", "--detach", ref], HERE)
    print("the relay now runs %s" % gitio.git(["log", "-1", "--format=%h %s"], HERE))
    return 0


def unit_from(path):
    """The unit a file names, for `add --unit`: the queue file's own shape (id, lane, goal, done_when, and a role
    when it is not "lane"). What is wrong with the unit itself is lane.load's to say, once it is a queue file."""
    try:
        u = P.read_json(Path(path))
    except (OSError, ValueError) as e:
        raise SystemExit("relay: the unit file %s does not read: %s" % (path, e))
    if not isinstance(u, dict) or not isinstance(u.get("id"), str) or not lane_source.ID.match(u["id"]):
        raise SystemExit("relay: the unit file %s names no id (letters, digits, . _ - only)" % path)
    more = sorted(k for k in u if k not in lane_source.NEED)
    if more:
        raise SystemExit("relay: the unit file %s has %s, which a unit to queue does not" % (path, ", ".join(more)))
    return dict(u, role=u.get("role") or "lane")


def add(a):
    """Queue one unit of lane work: a committed file on the board, which is the only kind the runner trusts.
    The unit comes in words (an id, --lane, --goal, --done-when) or as a file (--unit): an answer of the owner's on
    the Decide page names its unit as a file, and a file crosses ssh where quoted words do not.
    The same unit from a file, queued a second time, is queued once: the call pushes the board again and ends 0, so
    a take-up that stopped halfway can be repeated. Another unit under an id that is taken is refused."""
    board = P.board_dir()
    if a.unit:
        if a.id or a.lane or a.goal or a.done_when:
            raise SystemExit("relay: add --unit takes the whole unit from the file: no id, --lane, --goal or "
                             "--done-when beside it")
        unit = unit_from(a.unit)
    else:
        missing = [n for n, v in (("an id", a.id), ("--lane", a.lane), ("--goal", a.goal),
                                  ("--done-when", a.done_when)) if not v]
        if missing:
            raise SystemExit("relay: add needs %s (or --unit FILE)" % ", ".join(missing))
        if not a.role:                               # 105 of 112 legs ran as `lane`, which has no brief
            raise SystemExit("relay: add needs --role: the kind of work, so its legs get that role's brief (%s). "
                             "`lane` is plain lane work with no brief." % ", ".join(sorted(P.roles())))
        unit = {"id": a.id, "lane": a.lane, "role": a.role, "goal": a.goal, "done_when": a.done_when}
    known_role(unit["role"])
    p = board / "relay" / "queue" / (unit["id"] + ".json")
    if p.exists():
        try:
            there = P.read_json(p)
        except (OSError, ValueError):
            there = None
        same = [k for k in lane_source.NEED if k != "role"]     # its role, like its priority, may have moved since
        if a.unit and isinstance(there, dict) and all(there.get(k) == unit[k] for k in same):
            said = boardio.push(board, "relay: queue %s" % unit["id"])
            if said == "nothing to push":            # committed the first time; that push may have failed
                said = boardio.push_kept(board)
            print("%s is queued already, the same unit. Board: %s." % (unit["id"], said))
            return 0
        raise SystemExit("relay: %s is queued already" % unit["id"])
    P.write_json(p, unit)
    try:
        lane_source.load(p)
    except SystemExit:
        p.unlink()
        raise
    print("queued %s on %s. Board: %s." % (unit["id"], unit["lane"], boardio.push(board, "relay: queue %s" % unit["id"])))
    return 0


def prio(uid, n):
    """Move one queued unit: write its priority (the lower runs first) and commit the file on the board, which is
    the only kind the runner trusts. Refused for a file nobody committed: committing it here would let a leg's
    own file into the queue."""
    board = P.board_dir()
    p = board / "relay" / "queue" / (uid + ".json")
    if not p.exists():
        raise SystemExit("relay: nothing is queued as %s" % uid)
    if not lane_source.trusted(board, p):
        raise SystemExit("relay: %s is not committed on the board: nobody but a leg may have written it" % p.name)
    unit = lane_source.load(p)
    if (board / "relay" / "done" / (uid + ".json")).exists():
        raise SystemExit("relay: %s is done already" % uid)
    before = p.read_bytes()
    P.write_json(p, dict(unit, priority=n))
    try:
        lane_source.load(p)
    except SystemExit:
        p.write_bytes(before)
        raise
    print("%s now has priority %d (the lower runs first). Board: %s."
          % (uid, n, boardio.push(board, "relay: priority %s %d" % (uid, n))))
    return 0


def known_role(name):
    """Refuse a role the table does not have: its legs would run without a brief and nothing would say so."""
    have = P.roles()
    if name not in have:
        raise SystemExit("relay: no role named %s in Tools/pipeline/roles.json (have: %s)" % (name, ", ".join(sorted(have))))


def role(name, uids):
    """Give queued units a role, so their legs get that role's brief: one commit on the board for all of them.
    All of them or none: one unit that cannot be changed stops the call before a file is written. Refused, as
    prio is, for a unit that is done and for a file nobody committed."""
    known_role(name)
    board = P.board_dir()
    units = []
    for uid in uids:
        p = board / "relay" / "queue" / (uid + ".json")
        if not p.exists():
            raise SystemExit("relay: nothing is queued as %s" % uid)
        if not lane_source.trusted(board, p):
            raise SystemExit("relay: %s is not committed on the board: nobody but a leg may have written it" % p.name)
        if (board / "relay" / "done" / (uid + ".json")).exists():
            raise SystemExit("relay: %s is done already" % uid)
        units.append((p, lane_source.load(p)))
    for p, unit in units:
        P.write_json(p, dict(unit, role=name))
    said = ", ".join(uids)
    print("%s now %s the role %s. Board: %s." % (said, "has" if len(uids) == 1 else "have", name,
                                                 boardio.push(board, "relay: role %s %s" % (name, said))))
    return 0


def main(argv=None):
    ap = argparse.ArgumentParser(prog="relay.py")
    sub = ap.add_subparsers(dest="cmd", required=True)
    runner.add_args(sub.add_parser("run"))
    legcmd.add_args(sub.add_parser("leg"))
    sub.add_parser("status")
    sub.add_parser("stop").add_argument("--now", action="store_true")
    sub.add_parser("refusals").add_argument("--runs", type=int, default=3)
    sub.add_parser("budget").add_argument("--days", type=int, default=8)
    sub.add_parser("day")
    p = sub.add_parser("agents")
    p.add_argument("what", nargs="?", choices=("book",))
    p.add_argument("--day", help="a local day, 2026-10-07 (default: today)")
    p.add_argument("--out", help="book: write the booking to this file, not to the board")
    p.add_argument("--file", help="book: a booking another machine wrote with --out")
    p.add_argument("--who", help="book: who books (a session name)")
    p = sub.add_parser("usage")
    p.add_argument("put", nargs="?", choices=("put",))
    p.add_argument("--statusline", action="store_true")
    p = sub.add_parser("prio")
    p.add_argument("id")
    p.add_argument("n", type=int)
    p = sub.add_parser("role")
    p.add_argument("role")
    p.add_argument("ids", nargs="+")
    sub.add_parser("update").add_argument("ref", nargs="?", default="origin/lane/show/relay")
    p = sub.add_parser("hold")
    p.add_argument("who")
    p.add_argument("--hours", type=float, default=4)
    p.add_argument("--release", action="store_true")
    p = sub.add_parser("add")
    p.add_argument("id", nargs="?")
    p.add_argument("--unit", help="the unit as a file in the shape of the queue's, in place of the words")
    p.add_argument("--lane")
    p.add_argument("--role", help="a role of Tools/pipeline/roles.json; `lane` for work no role fits")
    p.add_argument("--goal")
    p.add_argument("--done-when", dest="done_when", nargs=argparse.REMAINDER)
    p = sub.add_parser("proof")
    p.add_argument("name", choices=("meter", "timeout"))
    p = sub.add_parser("view")
    p.add_argument("leg")
    p.add_argument("--follow", action="store_true")
    a = ap.parse_args(argv)
    if a.cmd == "run":
        return runner.Run(a).loop()
    if a.cmd == "leg":
        return legcmd.main(a)
    if a.cmd == "status":
        return status()
    if a.cmd == "stop":
        return stop(a.now)
    if a.cmd == "update":
        return update(a.ref)
    if a.cmd == "hold":
        return hold(a.who, a.hours, a.release)
    if a.cmd == "refusals":
        return refusals(a.runs)
    if a.cmd == "budget":
        return budget(a.days)
    if a.cmd == "day":
        return day_screen()
    if a.cmd == "agents":
        return agents_cmd(a)
    if a.cmd == "usage":
        return usage_cmd(bool(a.put), a.statusline)
    if a.cmd == "prio":
        return prio(a.id, a.n)
    if a.cmd == "role":
        return role(a.role, a.ids)
    if a.cmd == "add":
        return add(a)
    if a.cmd == "proof":
        return proof(a.name)
    if a.cmd == "view":
        return view(a.leg, a.follow)


if __name__ == "__main__":
    sys.exit(main())
