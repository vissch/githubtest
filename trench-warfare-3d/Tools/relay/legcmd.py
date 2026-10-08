#!/usr/bin/env python3
"""What a leg runs to close out, by script: the edit gate as a detached job, then the commit and the push.

  relay.py leg gate start         start the edit gate (gate.ps1 -EditOnly) detached, for the files as they are now
  relay.py leg gate status        RUNNING | GREEN | RED | STALE (green, but the files changed since) | NONE
  relay.py leg gate wait [--max S]   wait for the gate, at most S seconds (default 540: one tool call)
  relay.py leg play <filter> [--max S]   run the PlayMode tests the filter names (a class or a full test name) in
                                  this checkout and wait for them: GREEN | RED with total, passed, failed and each
                                  failed test, or RUNNING after S seconds (the same command again waits on)
  relay.py leg finish [-m MSG]    commit and push when the gate is green for exactly these files; if it is not,
                                  save the uncommitted work as a patch beside the leg and say so
  relay.py leg done               exit 0 when nothing is left: no uncommitted file, the lane pushed. In a plan leg:
                                  exit 0 when plan.md passes the check the runner will run on it

gate.ps1 -EditOnly records no tree, so the relay keeps its own record: gate.json in the leg's desk folder holds the
tree the gate started on. The leg is found through TW_RUNS, which the runner sets to <desk>/jobs.
A PlayMode run is a detached job too, so the runner stops it with the leg and the leg never looks for "its" Unity
in the process list (rv-12-hud-f9 waited on another checkout's editor until its turn ended). Its report goes in
the desk folder, never in the checkout.
TW_RELAY_GATE (a JSON list) replaces the gate command, TW_RELAY_PLAY (a JSON list, the filter and the report's path
are added to it) the PlayMode command: the tests use stubs. Stdlib only. ASCII only.
"""
import contextlib, io, json, os, subprocess, sys, tempfile, time
import xml.etree.ElementTree as ET
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent / "pipeline"))
from pipeline import now, read_json, write_json   # noqa: E402
import run_detached                               # noqa: E402
import config, gitio, papers                      # noqa: E402

GATE, PLAY = "gate.json", "play.json"
EXIT = {"GREEN": 0, "RED": 1, "RUNNING": 2, "STALE": 3, "NONE": 4}


def find():
    """(desk folder, guard folder, leg record) of the leg this command runs in."""
    runs = os.environ.get("TW_RUNS")
    desk = Path(runs).parent if runs else None
    guard = desk.parents[1] / "legs" / desk.name if desk else None
    if not guard or not (guard / "leg.json").exists():
        raise SystemExit("relay: this is not a relay leg (no leg folder found): the leg commands work only inside one")
    return desk, guard, read_json(guard / "leg.json")


def working_tree(wt):
    """The tree a commit of the working tree would record (untracked files too), the way gate.ps1 reads it."""
    with tempfile.TemporaryDirectory() as tmp:
        env = dict(os.environ, GIT_INDEX_FILE=str(Path(tmp) / "index"))
        gitio.git_raw(["read-tree", "HEAD"], wt, env)
        gitio.git_raw(["add", "-A"], wt, env)
        return gitio.git_raw(["write-tree"], wt, env).stdout.decode().strip()


def gate_command(root):
    stub = os.environ.get("TW_RELAY_GATE")
    return json.loads(stub) if stub else ["powershell", "-NoProfile", "-ExecutionPolicy", "Bypass", "-File",
                                          str(Path(root) / "gate.ps1"), "-EditOnly"]


def job_state(job):
    """(state, detail, record) of a detached job. The record is read once per look, and a job that looks crashed is
    looked at again: its wrapper writes the record and exits, and a read can fall between the two."""
    for _ in range(4):
        run = read_json(job)
        if run.get("state") == "STARTING":
            st, detail = "RUNNING", "starting"
        else:
            st, detail = run_detached.state(run)
        if st != "CRASHED" and run.get("state") != "STARTING":
            break
        time.sleep(0.5)
    return st, detail, run


def verdict(desk, wt):
    """(GREEN | RED | RUNNING | STALE | NONE, a few words)."""
    rec = read_json(desk / GATE) if (desk / GATE).exists() else None
    job = desk / "jobs" / rec["job"] / "run.json" if rec else None
    if not rec or not job.exists():
        return "NONE", "no gate was started in this leg"
    st, detail, run = job_state(job)
    if st in ("RUNNING", "STALLED"):
        return "RUNNING", "%s, started %s" % (detail, rec["started_at"])
    if st != "DONE" or run.get("exit_code") != 0:
        return "RED", "%s %s; the log: %s" % (st, detail, run.get("log"))
    if working_tree(wt) != rec["tree"]:
        return "STALE", "the gate was green, but the files changed since: gate again"
    return "GREEN", "green for exactly these files"


def gate_start(desk, leg):
    wt = leg["worktree"]
    if verdict(desk, wt)[0] == "RUNNING":
        raise SystemExit("relay: a gate is already running: python relay.py leg gate wait")
    if play_verdict(desk)[0] == "RUNNING":
        raise SystemExit("relay: a PlayMode run holds the project: wait for it first (python relay.py leg play <filter>)")
    root = gitio.git(["rev-parse", "--show-toplevel"], wt)
    job = "gate-%d" % (len(list((desk / "jobs").glob("gate-*"))) + 1 if (desk / "jobs").is_dir() else 1)
    write_json(desk / GATE, {"job": job, "tree": working_tree(wt), "started_at": now()})
    run_detached.main(["start", job, "--timeout", str(config.limits()["gate_seconds"]), "--cwd", root, "--"]
                      + gate_command(root))
    print("gate started for the files as they are now. Wait for it: python relay.py leg gate wait")
    return 0


def gate_wait(desk, leg, max_s):
    end = time.time() + max_s
    while True:
        st, why = verdict(desk, leg["worktree"])
        if st != "RUNNING" or time.time() >= end:
            print("%s: %s" % (st, why))
            return EXIT[st]
        time.sleep(5)


# ---------- a PlayMode run, started and waited for ----------

def play_command(pattern, results):
    stub = os.environ.get("TW_RELAY_PLAY")
    if stub:
        return json.loads(stub) + [pattern, str(results)]
    cli = Path(os.environ.get("LOCALAPPDATA", "")) / "unity" / "bin" / "unity.exe"     # the CLI gate.ps1 runs
    return [str(cli), "test", ".", "--mode", "PlayMode", "--filter", pattern, "--output", str(results),
            "--timeout", str(config.limits()["play_seconds"]), "--", "-nographics"]


def play_counts(results):
    """(total, passed, failed, the run's own result word, a line per failed test) from the NUnit report; None when
    there is no report to read."""
    try:
        run = ET.parse(str(results)).getroot()
    except (OSError, ET.ParseError):
        return None
    if run.tag != "test-run":
        return None
    num = lambda k: int(run.get(k) or 0)
    lines = []
    for c in run.iter("test-case"):
        if (c.get("result") or "").startswith("Failed"):
            msg = " | ".join(x.strip() for x in (c.findtext("failure/message") or "").strip().splitlines()[:3])
            lines.append("  FAILED %s: %s" % (c.get("fullname"), msg))
    return num("total"), num("passed"), num("failed"), run.get("result") or "", lines


def play_verdict(desk):
    """(GREEN | RED | RUNNING | NONE, a few words, a line per failed test). The report is the verdict, not the exit
    code, as in gate.ps1: a failure in it, or a run in which nothing passed, is red."""
    rec = read_json(desk / PLAY) if (desk / PLAY).exists() else None
    job = desk / "jobs" / rec["job"] / "run.json" if rec else None
    if not rec or not job.exists():
        return "NONE", "no PlayMode run was started in this leg", []
    st, detail, run = job_state(job)
    if st in ("RUNNING", "STALLED"):
        return "RUNNING", "%s, started %s; wait on with the same command" % (detail, rec["started_at"]), []
    c = play_counts(rec["results"])
    where = "the report: %s; the log: %s" % (rec["results"], run.get("log"))
    if c is None or c[0] == 0:
        return "RED", ("no verdict: %s %s and no test ran (a compile error, a filter that names no test, or the "
                       "project is held); %s" % (st, detail, where)), []
    total, passed, failed, word, lines = c
    counts = "total %d, passed %d, failed %d (%s); %s" % (total, passed, failed, rec["filter"], where)
    if failed or word != "Passed" or not passed:
        return "RED", counts, lines
    return "GREEN", counts, []


def play(desk, leg, pattern, max_s):
    """Start the run unless one is going, then wait at most max_s seconds. Called again while it is RUNNING, it only
    waits; called after a verdict, it runs the tests again (the leg fixed something)."""
    wt = leg["worktree"]
    if leg.get("mode") != "work":
        raise SystemExit("relay: a %s leg runs no tests" % leg.get("phase"))
    if play_verdict(desk)[0] != "RUNNING":
        if verdict(desk, wt)[0] == "RUNNING":
            raise SystemExit("relay: the edit gate holds the project: wait for it first (python relay.py leg gate wait)")
        proj = Path(gitio.git(["rev-parse", "--show-toplevel"], wt)) / "trench-warfare-3d"
        if not os.environ.get("TW_RELAY_PLAY"):
            held = subprocess.run([sys.executable, "Tools/editor_lock.py", "guard"], cwd=str(proj),
                                  stdin=subprocess.DEVNULL, capture_output=True)
            if held.returncode:
                print("HELD: an editor or another batch run holds this checkout's project. Stop the editor you "
                      "started, then run this again. Ask the lock (python Tools/editor_lock.py status), never the "
                      "process list: another checkout's Unity is not yours.")
                return EXIT["STALE"]
        n = len(list((desk / "jobs").glob("play-*"))) + 1 if (desk / "jobs").is_dir() else 1
        results = desk / ("playmode-%d.xml" % n)
        write_json(desk / PLAY, {"job": "play-%d" % n, "filter": pattern, "results": str(results), "started_at": now()})
        with contextlib.redirect_stdout(io.StringIO()):       # its "started" line: the verdict below names the log
            run_detached.main(["start", "play-%d" % n, "--timeout", str(config.limits()["play_seconds"] + 60),
                               "--cwd", str(proj if proj.is_dir() else wt), "--"] + play_command(pattern, results))
    end = time.time() + max_s
    while True:
        st, why, lines = play_verdict(desk)
        if st != "RUNNING" or time.time() >= end:
            print("\n".join(["%s: %s" % (st, why)] + lines))
            return EXIT[st]
        time.sleep(5)


def finish(desk, guard, leg, message):
    wt, lane = leg["worktree"], leg["lane"]
    if gitio.branch(wt) != lane:
        raise SystemExit("relay: the checkout is on %s, not your lane %s: nothing done" % (gitio.branch(wt), lane))
    if gitio.dirty(wt):
        st, why = verdict(desk, wt)
        if st != "GREEN":
            gitio.snapshot_dirty(wt, guard)
            print("NOT COMMITTED: the gate is %s (%s). The uncommitted work is saved as a patch beside the leg. "
                  "Say so in your report and end the leg." % (st, why))
            return 1
        gitio.git(["add", "-A"], wt)
        gitio.git(["commit", "-q", "-m", message or "relay: %s, leg %02d close-out" % (leg["unit"], leg["leg"])], wt)
        print("committed %s" % gitio.head(wt)[:10])
    if not gitio.pushed(wt, lane):
        r = gitio.git_raw(["push", "-q", "origin", lane], wt)
        if r.returncode:
            print("PUSH FAILED: %s" % r.stderr.decode("utf-8", "replace").strip()[-300:])
            return 1
        print("pushed %s" % lane)
    print("closed: nothing uncommitted, %s is on origin" % lane)
    return 0


def done(desk, leg):
    wt, lane = leg["worktree"], leg["lane"]
    if leg.get("mode") == "read_only":           # a leg that only reads leaves one file: check it as the runner will
        f, lim = desk / (leg.get("output") or "plan.md"), config.limits()
        if not f.exists():
            print("NOT DONE: %s is not in your leg folder (%s)" % (f.name, desk))
            return 1
        text, bad = f.read_text(encoding="utf-8"), []
        if leg.get("phase") == "plan":
            bad = papers.check_plan(text, lim["plan_max_bytes"], wt, lim["max_plan_parts"])
        elif leg.get("phase") == "critic":
            bad = papers.check_critic(text, lim["critic_max_bytes"])
        elif leg.get("phase") == "retro":
            bad = papers.check_retro(text, lim["retro_max_bytes"])
        for b in bad:
            print("NOT DONE: " + b)
        if not bad:
            print("done: %s passes the runner's check" % f.name)
        return 1 if bad else 0
    left = gitio.dirty(wt)
    if left:
        print("NOT DONE: %d uncommitted file(s): %s" % (len(left), ", ".join(left[:5])))
    elif not gitio.pushed(wt, lane):
        print("NOT DONE: %s is not pushed" % lane)
    else:
        print("done: nothing uncommitted, %s is on origin" % lane)
        return 0
    return 1


def main(a):
    desk, guard, leg = find()
    if a.what == "gate":
        if a.op == "start":
            return gate_start(desk, leg)
        if a.op == "wait":
            return gate_wait(desk, leg, a.max)
        st, why = verdict(desk, leg["worktree"])
        print("%s: %s" % (st, why))
        return EXIT[st]
    if a.what == "play":
        return play(desk, leg, a.filter, a.max)
    if a.what == "finish":
        return finish(desk, guard, leg, a.message)
    return done(desk, leg)


def add_args(p):
    sub = p.add_subparsers(dest="what", required=True)
    g = sub.add_parser("gate")
    g.add_argument("op", choices=("start", "status", "wait"))
    g.add_argument("--max", type=int, default=540)
    pl = sub.add_parser("play")
    pl.add_argument("filter")
    pl.add_argument("--max", type=int, default=540)
    f = sub.add_parser("finish")
    f.add_argument("-m", "--message", default="")
    sub.add_parser("done")
