#!/usr/bin/env python3
"""The relay: runs work as a chain of short headless Claude sessions (legs), so no session fills its context.
Contract: docs/reference/relay.md. Settings: limits.json, phases.json, style.json and roles/ next to this file.

  python Tools/relay/relay.py run --work <checkout> [--dry-run] [--hours 3] [--max-legs N] [--sources pipeline,lane]
  python Tools/relay/relay.py status           is a run going, on what, and how the last one stopped
  python Tools/relay/relay.py stop [--now]     end the run before its next leg (--now: end the leg too)
  python Tools/relay/relay.py add <id> --lane lane/show/x --goal ".." --done-when <program> <arg> ..   queue lane work
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
import boardio, config, gitio, launch, legcmd, legdir, runner   # noqa: E402
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
    for f in sorted((home / "locks").glob("*.json")) if (home / "locks").is_dir() else []:
        rec = P.read_json(f)
        if P.proc_start(rec["pid"]) != rec["pid_start"]:
            continue
        live += 1
        run = rec["who"].split()[1] if len(rec["who"].split()) > 1 else ""
        legs = sorted((home / "runs" / run / "legs").glob("*/leg.json")) if run else []
        leg = P.read_json(legs[-1]) if legs else {}
        print("RUNNING: %s, lane %s" % (rec["who"], rec.get("lane")))
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
    return 0


def stop(now):
    P.write_json(runner.stop_path(legdir.home()), {"now": bool(now), "asked_at": P.now()})
    print("stop asked: the run ends %s." % ("now, mid-leg" if now else "before its next leg"))
    return 0


def add(a):
    """Queue one unit of lane work: a committed file on the board, which is the only kind the runner trusts."""
    board = P.board_dir()
    p = board / "relay" / "queue" / (a.id + ".json")
    if p.exists():
        raise SystemExit("relay: %s is queued already" % a.id)
    P.write_json(p, {"id": a.id, "lane": a.lane, "role": a.role, "goal": a.goal, "done_when": a.done_when})
    try:
        lane_source.load(p)
    except SystemExit:
        p.unlink()
        raise
    print("queued %s on %s. Board: %s." % (a.id, a.lane, boardio.push(board, "relay: queue %s" % a.id)))
    return 0


def main(argv=None):
    ap = argparse.ArgumentParser(prog="relay.py")
    sub = ap.add_subparsers(dest="cmd", required=True)
    runner.add_args(sub.add_parser("run"))
    legcmd.add_args(sub.add_parser("leg"))
    sub.add_parser("status")
    sub.add_parser("stop").add_argument("--now", action="store_true")
    p = sub.add_parser("add")
    p.add_argument("id")
    p.add_argument("--lane", required=True)
    p.add_argument("--role", default="lane")
    p.add_argument("--goal", required=True)
    p.add_argument("--done-when", dest="done_when", nargs=argparse.REMAINDER, required=True)
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
    if a.cmd == "add":
        return add(a)
    if a.cmd == "proof":
        return proof(a.name)
    if a.cmd == "view":
        return view(a.leg, a.follow)


if __name__ == "__main__":
    sys.exit(main())
