#!/usr/bin/env python3
"""Run a long job (gate, bench, sweep, Blender, ComfyUI batch) detached from the session that started it.

A tool call ends after ten minutes and a session can close; the job must not. This starts a wrapper outside the
caller's job object; the wrapper starts the command, records both process ids with their start times, beats a
heartbeat while the log grows, enforces the timeout by stopping only the process tree it started, and writes the
exit code. Runs live in %LOCALAPPDATA%/TrenchWarfare/runs/<name>/ (TW_RUNS overrides), never in a checkout, because
an untracked file in the gate tree changes the tree land.py checks.

  python Tools/pipeline/run_detached.py start <name> --timeout 3600 [--min-headroom-gb 10] [--cwd DIR] -- <cmd...>
  python Tools/pipeline/run_detached.py status <name>     RUNNING / DONE exit=N / TIMEOUT / STOPPED / CRASHED / STALLED
  python Tools/pipeline/run_detached.py stop <name>       stop the recorded tree (only that tree)
Commit headroom, not free RAM, decides whether a heavy job may start (workflow.md: under 10 GB, batch gate only).
--min-headroom-gb defaults to 10. Pass 0 to start anyway.
Stdlib only. ASCII only.
"""
import argparse, ctypes, datetime, json, os, re, subprocess, sys, time
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
from pipeline import proc_start, write_json, read_json   # noqa: E402

STALL_S = 600          # no log growth for this long while running = STALLED (reported, not killed)
BEAT_S = 10
CLOCK = time.monotonic   # the timeout is measured on a clock no NTP step or DST change can move
NAME_RE = re.compile(r"^[A-Za-z0-9._-]{1,64}$")
JOB_PREFIX = "tw-run-"
JOB_LIMIT_KILL_ON_JOB_CLOSE = 0x2000
JOB_ALL_ACCESS = 0x1F001F


def run_dir(name):
    """The folder of one run. A name is a file name, never a path: ".." or a slash would escape TW_RUNS."""
    if not NAME_RE.match(name or ""):
        raise SystemExit("bad run name %r: 1-64 characters of letters, digits, dot, dash or underscore" % (name,))
    return runs_root() / name


def runs_root():
    return Path(os.environ.get("TW_RUNS") or Path(os.environ.get("LOCALAPPDATA", Path.home())) / "TrenchWarfare" / "runs")


def now():
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def headroom_gb():
    if os.name != "nt":
        return None
    class MS(ctypes.Structure):
        _fields_ = [("dwLength", ctypes.c_ulong), ("dwMemoryLoad", ctypes.c_ulong)] + \
                   [(n, ctypes.c_ulonglong) for n in ("tp", "ap", "tpf", "apf", "tv", "av", "aev")]
    m = MS(); m.dwLength = ctypes.sizeof(MS)
    ctypes.windll.kernel32.GlobalMemoryStatusEx(ctypes.byref(m))
    return m.apf / 2 ** 30          # available page file = commit limit minus commit charge


def job_name(name):
    return JOB_PREFIX + name


def make_job(name):
    """A Windows Job object holding the child and everything it starts, so no grandchild can outlive a stop.
    Returns the handle, or None when Jobs are unavailable: that must never stop a run from starting."""
    if os.name != "nt":
        return None
    try:
        k = ctypes.windll.kernel32
        h = k.CreateJobObjectW(None, job_name(name))
        if not h:
            return None
        class LIMIT(ctypes.Structure):
            _fields_ = [("PerProcessUserTimeLimit", ctypes.c_longlong), ("PerJobUserTimeLimit", ctypes.c_longlong),
                        ("LimitFlags", ctypes.c_ulong), ("MinimumWorkingSetSize", ctypes.c_size_t),
                        ("MaximumWorkingSetSize", ctypes.c_size_t), ("ActiveProcessLimit", ctypes.c_ulong),
                        ("Affinity", ctypes.c_size_t), ("PriorityClass", ctypes.c_ulong),
                        ("SchedulingClass", ctypes.c_ulong)]
        class IO(ctypes.Structure):
            _fields_ = [("ReadOperationCount", ctypes.c_ulonglong), ("WriteOperationCount", ctypes.c_ulonglong),
                        ("OtherOperationCount", ctypes.c_ulonglong), ("ReadTransferCount", ctypes.c_ulonglong),
                        ("WriteTransferCount", ctypes.c_ulonglong), ("OtherTransferCount", ctypes.c_ulonglong)]
        class EXT(ctypes.Structure):
            _fields_ = [("BasicLimitInformation", LIMIT), ("IoInfo", IO), ("ProcessMemoryLimit", ctypes.c_size_t),
                        ("JobMemoryLimit", ctypes.c_size_t), ("PeakProcessMemoryUsed", ctypes.c_size_t),
                        ("PeakJobMemoryUsed", ctypes.c_size_t)]
        info = EXT()
        info.BasicLimitInformation.LimitFlags = JOB_LIMIT_KILL_ON_JOB_CLOSE
        k.SetInformationJobObject(h, 9, ctypes.byref(info), ctypes.sizeof(info))   # 9 = ExtendedLimitInformation
        return h
    except Exception:
        return None


def assign_job(h, pid):
    if not h or os.name != "nt":
        return False
    try:
        k = ctypes.windll.kernel32
        ph = k.OpenProcess(0x1F0FFF, False, int(pid))
        if not ph:
            return False
        try:
            return bool(k.AssignProcessToJobObject(h, ph))
        finally:
            k.CloseHandle(ph)
    except Exception:
        return False


def kill_job(name):
    """Kill a run's whole Job by name, from any process. True when a Job was found and terminated."""
    if os.name != "nt" or not name:
        return False
    try:
        k = ctypes.windll.kernel32
        h = k.OpenJobObjectW(JOB_ALL_ACCESS, False, job_name(name))
        if not h:
            return False
        try:
            return bool(k.TerminateJobObject(h, 1))
        finally:
            k.CloseHandle(h)
    except Exception:
        return False


def kill_tree(pid, name=None):
    """Stop the tree we started. The Job first (it holds grandchildren too), then taskkill /T as the fallback."""
    if os.name == "nt":
        kill_job(name)
        subprocess.run(["taskkill", "/T", "/F", "/PID", str(pid)], capture_output=True)
    else:
        try:
            os.killpg(pid, 9)
        except OSError:
            pass


def cmd_start(a):
    d = run_dir(a.name)
    rj = d / "run.json"
    if rj.exists():
        rec = read_json(rj)
        st = state(rec)[0]
        if st in ("RUNNING", "STALLED"):
            raise SystemExit("run %s is still going" % a.name)
        # A wrapper that died leaves CRASHED while its child still runs. Starting again stacks two jobs.
        child_alive = (rec.get("child_pid") and rec.get("child_start") is not None
                       and proc_start(rec["child_pid"]) == rec["child_start"])
        if st == "CRASHED" and child_alive:
            raise SystemExit("refused: %s child still running (stop it or wait)" % a.name)
    if a.min_headroom_gb:
        h = headroom_gb()
        if h is not None and h < a.min_headroom_gb:
            raise SystemExit("refused: %.1f GB commit headroom, this job needs %.1f" % (h, a.min_headroom_gb))
    d.mkdir(parents=True, exist_ok=True)
    rec = dict(name=a.name, cmd=a.cmd, cwd=a.cwd or os.getcwd(), timeout_s=a.timeout, log=str(d / "out.log"),
               started_at=now(), state="STARTING")
    write_json(rj, rec)
    flags = 0
    if os.name == "nt":   # DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP | CREATE_BREAKAWAY_FROM_JOB
        flags = 0x00000008 | 0x00000200 | 0x01000000
    kw = dict(creationflags=flags) if os.name == "nt" else dict(start_new_session=True)
    try:
        w = subprocess.Popen([sys.executable, __file__, "_wrap", a.name], stdin=subprocess.DEVNULL,
                             stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, close_fds=True, **kw)
    except OSError:   # the caller's job object forbids breakaway: still detach, just without it
        kw["creationflags"] = flags & ~0x01000000 if os.name == "nt" else 0
        w = subprocess.Popen([sys.executable, __file__, "_wrap", a.name], stdin=subprocess.DEVNULL,
                             stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, close_fds=True, **kw)
    for _ in range(50):
        if read_json(rj).get("child_pid"):
            break
        time.sleep(0.1)
    else:
        wp = w.poll()
        raise SystemExit("run %s did not start: wrapper %s" % (a.name, "exited %s" % wp if wp is not None else "still starting"))
    print("started", a.name, "wrapper", w.pid, "log", rec["log"])


def cmd_wrap(a):
    d = run_dir(a.name)
    rj = d / "run.json"
    rec = read_json(rj)
    me = os.getpid()
    job = make_job(a.name)
    with open(rec["log"], "wb") as log:   # one run, one log: appending let a retry read the old run's output as new
        kw = dict(creationflags=0x00000200) if os.name == "nt" else dict(start_new_session=True)
        child = subprocess.Popen(rec["cmd"], cwd=rec["cwd"], stdout=log, stderr=subprocess.STDOUT,
                                 stdin=subprocess.DEVNULL, **kw)
        in_job = assign_job(job, child.pid)
        rec.update(state="RUNNING", wrapper_pid=me, wrapper_start=proc_start(me), child_pid=child.pid,
                   child_start=proc_start(child.pid), heartbeat=now(), log_size=0,
                   job=job_name(a.name) if in_job else None)
        write_json(rj, rec)
        t0, last_size = CLOCK(), -1
        while True:
            try:
                code = child.wait(timeout=BEAT_S)
                break
            except subprocess.TimeoutExpired:
                pass
            size = os.path.getsize(rec["log"])
            if size != last_size:
                rec.update(heartbeat=now(), log_size=size)
                last_size = size
            if CLOCK() - t0 > rec["timeout_s"]:
                kill_tree(child.pid, a.name)
                child.wait()
                rec.update(state="TIMEOUT", exit_code=None, finished_at=now())
                write_json(rj, rec)
                return
            try:
                write_json(rj, rec)
            except OSError:
                pass
    rec.update(state="DONE", exit_code=code, finished_at=now())
    write_json(rj, rec)


def state(rec):
    """(STATE, detail). A recorded pid only counts when its start time matches: pids are reused."""
    if rec.get("state") in ("DONE", "TIMEOUT", "STOPPED"):
        return rec["state"], "exit=%s" % rec.get("exit_code")
    wrapper_alive = rec.get("wrapper_pid") and proc_start(rec["wrapper_pid"]) == rec.get("wrapper_start")
    child_alive = (rec.get("child_pid") and rec.get("child_start") is not None
                   and proc_start(rec["child_pid"]) == rec["child_start"])
    if not wrapper_alive:
        return "CRASHED", "wrapper gone" + (", child still running (stop it or wait)" if child_alive else "")
    beat = datetime.datetime.strptime(rec["heartbeat"], "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=datetime.timezone.utc)
    quiet = (datetime.datetime.now(datetime.timezone.utc) - beat).total_seconds()
    if quiet > STALL_S:
        return "STALLED", "no log growth for %d s" % quiet
    return "RUNNING", "log %d bytes" % rec.get("log_size", 0)


def cmd_status(a):
    rj = run_dir(a.name) / "run.json"
    if not rj.exists():
        raise SystemExit("no run %s" % a.name)
    st, detail = state(read_json(rj))
    print(st, detail)


def cmd_stop(a):
    rj = run_dir(a.name) / "run.json"
    if not rj.exists():
        raise SystemExit("no run %s" % a.name)
    rec = read_json(rj)
    if rec.get("state") in ("DONE", "TIMEOUT", "STOPPED"):   # over already: never overwrite its verdict
        print("already", rec["state"], a.name)
        return
    for p, s in (("wrapper_pid", "wrapper_start"), ("child_pid", "child_start")):
        if rec.get(p) and proc_start(rec[p]) == rec.get(s):   # only the process we started, never a reused pid
            kill_tree(rec[p], a.name)
    rec.update(state="STOPPED", exit_code=None, finished_at=now())
    write_json(rj, rec)
    print("stopped", a.name)


def main(argv=None):
    argv = list(sys.argv[1:] if argv is None else argv)
    cmd = argv[argv.index("--") + 1:] if "--" in argv else []
    argv = argv[:argv.index("--")] if "--" in argv else argv
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    sub = ap.add_subparsers(dest="op", required=True)
    s = sub.add_parser("start")
    s.add_argument("name"); s.add_argument("--timeout", type=int, required=True)
    s.add_argument("--min-headroom-gb", type=float, default=10); s.add_argument("--cwd")  # 0 turns the guard off
    for n in ("status", "stop", "_wrap"):
        sub.add_parser(n).add_argument("name")
    a = ap.parse_args(argv)
    a.cmd = cmd
    if a.op == "start" and not cmd:
        raise SystemExit("nothing to run: put the command after --")
    {"start": cmd_start, "status": cmd_status, "stop": cmd_stop, "_wrap": cmd_wrap}[a.op](a)


if __name__ == "__main__":
    main()
