#!/usr/bin/env python3
"""What a leg runs to close out, by script: the edit gate as a detached job, then the commit and the push.

  relay.py leg gate start         start the edit gate (gate.ps1 -EditOnly) detached, for the files as they are now
  relay.py leg gate status        RUNNING | GREEN | RED | STALE (green, but the files changed since) | NONE
  relay.py leg gate wait [--max S]   wait for the gate, at most S seconds (default 540: one tool call); a gate
                                  whose log stopped growing GATE_QUIET_S ago is stopped there: RED, a GATE FAULT
  relay.py leg play <filter> [--max S]   run the PlayMode tests the filter names (a class or a full test name) in
                                  this checkout and wait for them: GREEN | RED with total, passed, failed and each
                                  failed test, or RUNNING after S seconds (the same command again waits on)
  relay.py leg finish [-m MSG]    commit and push only when the gate is green for exactly this tree, HEAD's tree
                                  included; otherwise it pushes nothing and saves any uncommitted work as a patch
                                  beside the leg
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
import calendar, contextlib, io, json, os, subprocess, sys, tempfile, time
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
# A gate whose log has not grown for this long is stopped by `leg gate wait` and called a gate fault. The limit
# depends on the gate's phase, read from its last "== <part> ==" header:
#  - the tool check (and validate): 900 s. toolcheck prints a dot whenever a running part's output grew (30 s
#    looks) and test_relay.py ends a test that runs 600 s (its watchdog), so a healthy part is never quiet that
#    long; before the dots test_relay.py was 1583 s without a line (relay 4, 2026-10-19).
#  - a Unity run (== EditMode == / == PlayMode ==): the unity CLI prints nothing until it is done and gate.ps1
#    lets one run take $TestTimeout = 1800 s, so 1800 + 300.
# TW_RELAY_GATE_QUIET overrides both (the tests).
GATE_QUIET_S, UNITY_QUIET_S = 900, 2100
# What a leg keeps after its gate for the finish, the commit and the report; a gate is capped at the leg's time
# left minus this, and is not started at all when less than GATE_MIN_S would remain.
CLOSE_S, GATE_MIN_S = 300, 300


def quiet_limit(log=None):
    if os.environ.get("TW_RELAY_GATE_QUIET"):
        return int(os.environ["TW_RELAY_GATE_QUIET"])
    try:
        heads = [l for l in Path(log).read_text(encoding="utf-8", errors="replace").splitlines() if l.startswith("== ")]
    except (OSError, TypeError):
        heads = []
    return UNITY_QUIET_S if heads and ("EditMode" in heads[-1] or "PlayMode" in heads[-1]) else GATE_QUIET_S


def gate_seconds(leg, now_s=None):
    """The gate's timeout: limits.json gate_seconds, but never past the leg's own end (leg.json ends_at, written by
    launch.run_leg) minus CLOSE_S. None when that leaves less than GATE_MIN_S."""
    full = int(config.limits()["gate_seconds"])
    ends = leg.get("ends_at")
    if not isinstance(ends, (int, float)):
        return full
    left = int(ends - (time.time() if now_s is None else now_s) - CLOSE_S)
    return min(full, left) if left >= GATE_MIN_S else None


def find():
    """(desk folder, guard folder, leg record) of the leg this command runs in."""
    runs = os.environ.get("TW_RUNS")
    desk = Path(runs).parent if runs else None
    guard = desk.parents[1] / "legs" / desk.name if desk else None
    if not guard or not (guard / "leg.json").exists():
        raise SystemExit("relay: this is not a relay leg (no leg folder found): the leg commands work only inside one")
    return desk, guard, read_json(guard / "leg.json")


def working_tree(wt):
    """The tree a commit of the working tree would record (untracked files too), the way gate.ps1 reads it.

    A failed git call, or write-tree returning no tree id, raises gitio.GitError. An empty string used to
    compare equal to itself and read as a green gate for files the tree never held.
    """
    with tempfile.TemporaryDirectory() as tmp:
        env = dict(os.environ, GIT_INDEX_FILE=str(Path(tmp) / "index"))
        for args in (["read-tree", "HEAD"], ["add", "-A"]):
            r = gitio.git_raw(args, wt, env)
            if r.returncode:
                raise gitio.GitError("git %s failed in %s: %s"
                                     % (" ".join(args), wt, r.stderr.decode("utf-8", "replace").strip()))
        r = gitio.git_raw(["write-tree"], wt, env)
        if r.returncode:
            raise gitio.GitError("git write-tree failed in %s: %s"
                                 % (wt, r.stderr.decode("utf-8", "replace").strip()))
        tree = r.stdout.decode().strip()
        if not tree:
            raise gitio.GitError("git write-tree gave no tree id")
        return tree


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
    if rec.get("fault"):
        return "RED", "GATE FAULT (not your edit): %s; the log: %s" % (rec["fault"], run.get("log"))
    if st == "TIMEOUT":
        return "RED", "GATE FAULT (not your edit): the gate ran out its %s s; the log: %s" % (run.get("timeout_s"),
                                                                                           run.get("log"))
    if st != "DONE" or run.get("exit_code") != 0:
        return "RED", "%s %s; the log: %s" % (st, detail, run.get("log"))
    try:
        if working_tree(wt) != rec["tree"]:
            return "STALE", "the gate was green, but the files changed since: gate again"
    except gitio.GitError as e:
        return "RED", "the working tree could not be read: %s; gate again" % e
    return "GREEN", "green for exactly these files"


def gate_start(desk, leg):
    wt = leg["worktree"]
    if verdict(desk, wt)[0] == "RUNNING":
        raise SystemExit("relay: a gate is already running: python relay.py leg gate wait")
    if play_verdict(desk)[0] == "RUNNING":
        raise SystemExit("relay: a PlayMode run holds the project: wait for it first (python relay.py leg play <filter>)")
    secs = gate_seconds(leg)
    if secs is None:
        raise SystemExit("relay: too little of the leg is left for a gate (it needs %d s and %d s to close out): run "
                         "python relay.py leg finish now; it saves your work as a patch" % (GATE_MIN_S, CLOSE_S))
    root = gitio.git(["rev-parse", "--show-toplevel"], wt)
    job = "gate-%d" % (len(list((desk / "jobs").glob("gate-*"))) + 1 if (desk / "jobs").is_dir() else 1)
    try:
        tree = working_tree(wt)
    except gitio.GitError as e:
        print("RED: the working tree could not be read: %s" % e)
        return EXIT["RED"]
    write_json(desk / GATE, {"job": job, "tree": tree, "started_at": now(), "timeout_s": secs})
    run_detached.main(["start", job, "--timeout", str(secs), "--cwd", root, "--"] + gate_command(root))
    print("gate started for the files as they are now (at most %d s: the leg keeps %d s to close out). Wait for it: "
          "python relay.py leg gate wait" % (secs, CLOSE_S))
    return 0


def stop_if_stalled(desk):
    """Stop the gate when its log has not grown for quiet_limit() seconds, and record it as a gate fault: a hung
    test must not hold the leg until the leg's own limit (relay 4, 2026-10-19: 30 minutes on one tool test).
    Returns the fault's words, or None."""
    rec = read_json(desk / GATE) if (desk / GATE).exists() else None
    job = desk / "jobs" / rec["job"] / "run.json" if rec else None
    if not rec or not job.exists():
        return None
    run = read_json(job)
    if run.get("state") != "RUNNING" or not run.get("heartbeat"):
        return None
    beat = calendar.timegm(time.strptime(run["heartbeat"], "%Y-%m-%dT%H:%M:%SZ"))
    quiet, limit = time.time() - beat, quiet_limit(run.get("log"))
    if quiet < limit:
        return None
    if read_json(job).get("state") != "RUNNING":      # it finished while we looked: its own verdict stands
        return None
    with contextlib.redirect_stdout(io.StringIO()):
        old = os.environ.get("TW_RUNS")
        os.environ["TW_RUNS"] = str(desk / "jobs")
        try:
            run_detached.main(["stop", rec["job"]])
        finally:
            os.environ.pop("TW_RUNS") if old is None else os.environ.__setitem__("TW_RUNS", old)
    rec["fault"] = "no log growth for %d s (the limit is %d s), so the relay stopped it" % (quiet, limit)
    write_json(desk / GATE, rec)
    return rec["fault"]


def gate_wait(desk, leg, max_s):
    end = time.time() + max_s
    while True:
        stop_if_stalled(desk)
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
        st, why = verdict(desk, wt)
        if st != "GREEN":
            print("NOT PUSHED: no gate covers this commit (the gate is %s: %s). Nothing left this checkout. "
                  "Run python relay.py leg gate start, wait for it, then leg finish again." % (st, why))
            return 1
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
            cut = papers.plan_cut(text, lim["plan_max_bytes"])
            if cut:
                bad.append(cut)
        elif leg.get("phase") == "critic":
            bad = papers.check_critic(text, lim["critic_max_bytes"])
        elif leg.get("phase") == "retro":
            bad = papers.check_retro(text, lim["retro_max_bytes"])
        for b in bad:
            print("NOT DONE: " + b)
        if not bad:
            print("done: %s passes the runner's check (%d bytes). Do not edit it again: write your report."
                  % (f.name, len(text.encode("utf-8"))))
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
