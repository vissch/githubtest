#!/usr/bin/env python3
"""Two-station job board and stage tracker for the agent pipeline (docs/reference/stations.md).

Items live on the board repo (TW_BOARD, default ../../tw3d-board next to this checkout). Each item lists stages;
each stage declares its inputs (repo paths, or path#/json/pointer for one field), the lane ref it reads them from,
the stages it comes after, the station that runs it and the zoom bands its evidence must cover. Nothing here stores
a status: every state is derived from the repo and the board's results.

  consumed_hash  declared inputs + feedback request ids + generator version   -> a change means REGENERATE
  upstream_rev   ids of the upstream stages' valid results                    -> a change means RECHECK
  DONE           a PASS result whose consumed_hash and upstream_rev both match now

Usage (from anywhere in the checkout):
  python Tools/pipeline/pipeline.py status [item]        state of every stage, and what to do next
  python Tools/pipeline/pipeline.py why <item>           the reason chain behind each stage's state
  python Tools/pipeline/pipeline.py next                 the first job this station may take (READY or STALE)
  python Tools/pipeline/pipeline.py claim <job>          take a job (one worker per station)
  python Tools/pipeline/pipeline.py complete <job> --verdict PASS|FAIL|BLOCKED [--evidence band=path ...] [--note ..]
  python Tools/pipeline/pipeline.py release              drop this station's claim without a result
  python Tools/pipeline/pipeline.py feedback <item> <stage> "<the user's words>" --check "<measurable check>"
  python Tools/pipeline/pipeline.py close-feedback <FR-id>
Environment: TW_BOARD (board repo path), TW_STATION (desktop|laptop; default from stations.json by host name).
Stdlib only. ASCII only. Git output is read as bytes and decoded as UTF-8 (workflow.md: never text=True).
"""
import argparse, ctypes, datetime, hashlib, json, os, random, socket, subprocess, sys, time
from pathlib import Path

HERE = Path(__file__).resolve()
PROJ = HERE.parents[2]                      # trench-warfare-3d/
REPO = HERE.parents[3]
GENERATOR = "pipeline/1"
STATES = ("DONE", "RECHECK", "STALE", "IN_PROGRESS", "READY", "BLOCKED")


# ---------- small helpers ----------

def git(args, cwd=None, check=True):
    r = subprocess.run(["git"] + args, cwd=str(cwd or REPO), capture_output=True)
    if check and r.returncode:
        raise SystemExit("git %s failed: %s" % (" ".join(args), r.stderr.decode("utf-8", "replace").strip()))
    return r.stdout.decode("utf-8", "replace")


def sha(*parts):
    h = hashlib.sha256()
    for p in parts:
        h.update(p.encode("utf-8") if isinstance(p, str) else p)
        h.update(b"\0")
    return h.hexdigest()


def canon(obj):
    return json.dumps(obj, sort_keys=True, separators=(",", ":"))


def now():
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def board_dir():
    return Path(os.environ.get("TW_BOARD") or REPO.parent / "tw3d-board").resolve()


def read_json(p):
    for i in range(20):                          # on Windows a file being replaced cannot be opened for a moment
        try:
            return json.loads(Path(p).read_text(encoding="utf-8"))
        except PermissionError:
            time.sleep(0.05 * (i + 1))
    return json.loads(Path(p).read_text(encoding="utf-8"))


def write_json(p, obj):
    p = Path(p)
    p.parent.mkdir(parents=True, exist_ok=True)
    tmp = p.with_suffix(p.suffix + ".tmp")
    tmp.write_text(json.dumps(obj, indent=2, sort_keys=True) + "\n", encoding="utf-8", newline="\n")
    for i in range(20):                          # on Windows a reader holds the target for a moment: a writer that
        try:                                     # gave up here left a detached run looking crashed
            os.replace(tmp, p)
            return
        except PermissionError:
            time.sleep(0.05 * (i + 1))
    os.replace(tmp, p)


def station():
    s = os.environ.get("TW_STATION")
    if s:
        return s
    table = read_json(HERE.parent / "stations.json")
    host = socket.gethostname().upper()
    for name, info in table.items():
        if host in [h.upper() for h in info.get("hosts", [])]:
            return name
    raise SystemExit("unknown host %s: add it to Tools/pipeline/stations.json or set TW_STATION" % host)


# ---------- process liveness (Windows: pid plus creation time, so a reused pid is not mistaken for ours) ----------

def proc_start(pid):
    """Creation time of pid as an int, or None when the process is gone."""
    if os.name != "nt":
        try:
            os.kill(pid, 0)
            return 0
        except OSError:
            return None
    k = ctypes.windll.kernel32
    h = k.OpenProcess(0x1000, False, pid)   # PROCESS_QUERY_LIMITED_INFORMATION
    if not h:
        return None
    try:
        code = ctypes.c_ulong()
        if k.GetExitCodeProcess(h, ctypes.byref(code)) and code.value != 259:   # 259 = STILL_ACTIVE
            return None
        t = [ctypes.c_ulonglong() for _ in range(4)]
        if not k.GetProcessTimes(h, *[ctypes.byref(x) for x in t]):
            return None
        return t[0].value
    finally:
        k.CloseHandle(h)


# ---------- the board ----------

def lint_item(item, name):
    """Refuse definitions that would make the state machine lie."""
    seen = set()
    for s in item["stages"]:
        for a in s.get("after", []):
            if a not in seen:
                raise SystemExit("%s: stage %s comes after %s, which is not an earlier stage" % (name, s["id"], a))
        own = {o.rstrip("/") for o in s.get("outputs", [])}
        for inp in s.get("inputs", []):
            path = inp.partition("#")[0].rstrip("/")
            if any(path == o or path.startswith(o + "/") or o.startswith(path + "/") for o in own):
                raise SystemExit("%s: stage %s reads its own output %s, so it would stale itself" % (name, s["id"], path))
        if s.get("station") not in ("desktop", "laptop"):
            raise SystemExit("%s: stage %s has no station (desktop or laptop)" % (name, s["id"]))
        seen.add(s["id"])


class Board:
    def __init__(self, root=None):
        self.root = Path(root) if root else board_dir()
        if not (self.root / "items").is_dir():
            raise SystemExit("no board at %s (set TW_BOARD, or create it: see docs/reference/stations.md)" % self.root)

    def items(self):
        out = {}
        for p in sorted((self.root / "items").glob("*.json")):
            item = read_json(p)
            lint_item(item, p.name)
            out[p.stem] = item
        return out

    def results(self, item, stage):
        out = []
        for p in sorted((self.root / "results").glob("%s--%s--*.json" % (item, stage))):
            r = read_json(p)
            r["_file"] = p.name
            out.append(r)
        return out

    def feedback(self, item=None, stage=None):
        out = []
        for p in sorted((self.root / "feedback").glob("FR-*.json")):
            f = read_json(p)
            if (item is None or f["item"] == item) and (stage is None or f["stage"] == stage):
                out.append(f)
        return out

    def claim_path(self, st):
        return self.root / "claims" / ("%s.json" % st)

    def claim(self, st):
        p = self.claim_path(st)
        return read_json(p) if p.exists() else None


# ---------- hashing ----------

def blob_ids(ref, path):
    """Sorted 'path oid' lines for a file or folder at ref; '' when missing (a missing input is an input too)."""
    out = git(["ls-tree", "-r", ref, "--", path], check=False)
    return "\n".join(sorted(l.split("\t", 1)[1] + " " + l.split()[2] for l in out.splitlines() if "\t" in l))


def field_value(ref, path, pointer):
    raw = git(["show", "%s:%s" % (ref, path)], check=False)
    try:
        v = json.loads(raw)
    except ValueError:
        return "<missing or not json>"
    for part in [p for p in pointer.split("/") if p]:
        if isinstance(v, list) and part.isdigit() and int(part) < len(v):
            v = v[int(part)]
        elif isinstance(v, dict) and part in v:
            v = v[part]
        else:
            return "<no %s>" % pointer
    return canon(v)


def resolve_ref(lane):
    for ref in ("origin/" + lane, lane):
        if git(["rev-parse", "--verify", "-q", ref + "^{commit}"], check=False).strip():
            return ref
    return None


def consumed_hash(item, stage, board):
    ref = resolve_ref(stage.get("lane") or item["lane"])
    parts = [GENERATOR, canon({k: v for k, v in stage.items() if k not in ("notes",)}), "ref-found=%s" % bool(ref)]
    for inp in stage.get("inputs", []):
        path, _, pointer = inp.partition("#")
        parts.append(inp + "=" + ((field_value(ref, path, pointer) if pointer else blob_ids(ref, path)) if ref else ""))
    parts += sorted(f["id"] for f in board.feedback(item["id"], stage["id"]))   # open or closed: closing never re-stales
    return sha(*parts)


# ---------- state ----------

def job_id(item, stage, ch):
    return "%s--%s--%s" % (item["id"], stage["id"], ch[:8])


def valid_result(results, jid):
    """Highest-attempt finished result for this job id (a later FAIL outranks an earlier PASS)."""
    mine = [r for r in results if r["job"] == jid]
    return max(mine, key=lambda r: r["attempt"]) if mine else None


def evaluate(item, board):
    """{stage id: dict(state, job, reason, consumed, upstream_rev, result)} in stage order."""
    out = {}
    claims = {st: c for st, c in ((st, board.claim(st)) for st in ("desktop", "laptop")) if c and claim_alive(c)}
    for stage in item["stages"]:
        sid = stage["id"]
        ch = consumed_hash(item, stage, board)
        jid = job_id(item, stage, ch)
        ups = [out[a] for a in stage.get("after", [])]
        up_rev = sha(*[u["result"]["_file"] if u["result"] else "" for u in ups])
        results = board.results(item["id"], sid)
        res = valid_result(results, jid)
        claimed = any(c and c.get("job") == jid for c in claims.values())
        info = dict(job=jid, consumed=ch, upstream_rev=up_rev, result=None, station=stage["station"])
        waiting = [a for a in stage.get("after", []) if out[a]["state"] != "DONE"]
        if waiting:
            info.update(state="BLOCKED", reason="waits for " + ", ".join(waiting))
        elif claimed:
            info.update(state="IN_PROGRESS", reason="claimed")
        elif res and res["verdict"] == "PASS" and res["upstream_rev"] == up_rev:
            info.update(state="DONE", reason="PASS " + res["_file"], result=res)
        elif res and res["verdict"] == "PASS":
            info.update(state="RECHECK", reason="upstream results changed; this stage's own inputs did not")
        elif res:
            info.update(state="READY", reason="last attempt %s: %s" % (res["verdict"], res.get("note", "")))
        elif results:
            info.update(state="STALE", reason="inputs changed since %s" % results[-1]["_file"])
        else:
            info.update(state="READY", reason="never run")
        out[sid] = info
    return out


# ---------- station lock (one worker per station) ----------

def claim_alive(c):
    """A claim only counts while its worker runs: a crashed session must not hold a job forever."""
    return proc_start(c["pid"]) == c["pid_start"]


def refuse_if_busy(board, st):
    cur = board.claim(st)
    if cur and claim_alive(cur):
        raise SystemExit("station %s is busy: %s holds %s (pid %d)" % (st, cur["worker"], cur["job"], cur["pid"]))
    return cur


def take_lock(board, st, jid):
    p = board.claim_path(st)
    cur = refuse_if_busy(board, st)
    if cur:
        print("taking over from a dead worker: %s (%s)" % (cur["worker"], cur["job"]))
    pid = int(os.environ.get("TW_WORKER_PID") or os.getppid())   # the long-lived shell, not this short python
    rec = dict(job=jid, worker="%s-%s" % (st, pid), pid=pid, pid_start=proc_start(pid), token="%016x" % random.getrandbits(64),
               claimed_at=now(), heartbeat=now())
    p.parent.mkdir(parents=True, exist_ok=True)
    try:
        fd = os.open(str(p) + ".lock", os.O_CREAT | os.O_EXCL | os.O_WRONLY)
    except FileExistsError:
        raise SystemExit("another session is claiming on %s right now; retry" % st)
    try:
        write_json(p, rec)
    finally:
        os.close(fd)
        os.remove(str(p) + ".lock")
    return rec


# ---------- commands ----------

def find_job(board, jid):
    for item in board.items().values():
        for sid, info in evaluate(item, board).items():
            if info["job"] == jid:
                return item, next(s for s in item["stages"] if s["id"] == sid), info
    raise SystemExit("no such job now: %s (inputs may have changed; run status)" % jid)


def cmd_status(board, a):
    items = board.items()
    for iid in ([a.item] if a.item else items):
        if iid not in items:
            raise SystemExit("no item %s" % iid)
        print(iid)
        for sid, info in evaluate(items[iid], board).items():
            print("  %-12s %-11s %-8s %s" % (sid, info["state"], info["station"], info["job"]))


def cmd_why(board, a):
    item = board.items().get(a.item) or sys.exit("no item %s" % a.item)
    for sid, info in evaluate(item, board).items():
        print("%-12s %-11s %s" % (sid, info["state"], info["reason"]))


def cmd_next(board, a):
    st = station()
    for item in board.items().values():
        for sid, info in evaluate(item, board).items():
            if info["station"] == st and info["state"] in ("READY", "STALE", "RECHECK"):
                print(info["job"], "RECHECK" if info["state"] == "RECHECK" else "REGENERATE")
                return
    print("nothing for", st)


def refuse_in_leg():
    """A relay leg (Tools/relay) never claims, completes or releases: its runner does, after checking the result."""
    held = False
    if not os.environ.get("TW_RELAY"):           # the marker the runner leaves in the checkout a leg works in
        r = subprocess.run(["git", "rev-parse", "--absolute-git-dir"], capture_output=True, text=True)
        marker = Path(r.stdout.strip()) / "relay-leg.json" if r.returncode == 0 else None
        try:
            held = bool(marker) and marker.exists() and json.loads(marker.read_text(encoding="utf-8")).get("pid") != os.getpid()
        except (OSError, ValueError, AttributeError):
            held = True
    if os.environ.get("TW_RELAY") or held:
        raise SystemExit("a relay leg does not claim, complete or release a job: the runner does")


def cmd_claim(board, a):
    refuse_in_leg()
    st = station()
    refuse_if_busy(board, st)
    item, stage, info = find_job(board, a.job)
    if stage["station"] != st:
        raise SystemExit("%s runs on %s, this is %s" % (a.job, stage["station"], st))
    if info["state"] not in ("READY", "STALE", "RECHECK"):
        raise SystemExit("%s is %s: %s" % (a.job, info["state"], info["reason"]))
    rec = take_lock(board, st, a.job)
    print("claimed", a.job, "token", rec["token"])


def cmd_complete(board, a):
    refuse_in_leg()
    st = station()
    cur = board.claim(st)
    if not cur or cur["job"] != a.job:
        raise SystemExit("this station does not hold %s" % a.job)
    item, stage, info = find_job(board, a.job)
    ev = dict(e.split("=", 1) for e in a.evidence)
    missing = [b for b in stage.get("bands", []) if b not in ev]
    if a.verdict == "PASS" and missing:
        raise SystemExit("refused: PASS needs evidence for bands %s" % ", ".join(missing))
    for band, path in ev.items():
        if not (board.root / path).exists():
            raise SystemExit("refused: evidence %s=%s is not on the board" % (band, path))
    attempt = 1 + max([r["attempt"] for r in board.results(item["id"], stage["id"]) if r["job"] == a.job] or [0])
    res = dict(job=a.job, item=item["id"], stage=stage["id"], attempt=attempt, verdict=a.verdict, station=st,
               token=cur["token"], consumed=info["consumed"], upstream_rev=info["upstream_rev"], evidence=ev,
               note=a.note or "", feedback=[f["id"] for f in board.feedback(item["id"], stage["id"])],
               finished_at=now())
    write_json(board.root / "results" / ("%s--%d.json" % (a.job, attempt)), res)
    board.claim_path(st).unlink()
    print("recorded", res["verdict"], "attempt", attempt)


def cmd_release(board, a):
    refuse_in_leg()
    p = board.claim_path(station())
    if p.exists():
        p.unlink()
        print("released")


def cmd_feedback(board, a):
    item = board.items().get(a.item) or sys.exit("no item %s" % a.item)
    if a.stage not in [s["id"] for s in item["stages"]]:
        raise SystemExit("no stage %s in %s" % (a.stage, a.item))
    fid = "FR-%s-%s-%04x" % (station(), datetime.datetime.now().strftime("%Y%m%d%H%M%S"), random.getrandbits(16))
    write_json(board.root / "feedback" / (fid + ".json"),
               dict(id=fid, item=a.item, stage=a.stage, words=a.words, check=a.check, status="open", created_at=now()))
    print(fid)


def cmd_close_feedback(board, a):
    p = board.root / "feedback" / (a.id + ".json")
    f = read_json(p)
    f["status"], f["closed_at"] = "closed", now()
    write_json(p, f)
    print("closed", a.id)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("status").add_argument("item", nargs="?")
    sub.add_parser("why").add_argument("item")
    sub.add_parser("next")
    sub.add_parser("claim").add_argument("job")
    c = sub.add_parser("complete")
    c.add_argument("job")
    c.add_argument("--verdict", required=True, choices=("PASS", "FAIL", "BLOCKED"))
    c.add_argument("--evidence", nargs="*", default=[], help="band=path relative to the board")
    c.add_argument("--note")
    sub.add_parser("release")
    f = sub.add_parser("feedback")
    f.add_argument("item"); f.add_argument("stage"); f.add_argument("words")
    f.add_argument("--check", required=True)
    sub.add_parser("close-feedback").add_argument("id")
    a = ap.parse_args(argv)
    board = Board()
    {"status": cmd_status, "why": cmd_why, "next": cmd_next, "claim": cmd_claim, "complete": cmd_complete,
     "release": cmd_release, "feedback": cmd_feedback, "close-feedback": cmd_close_feedback}[a.cmd](board, a)


if __name__ == "__main__":
    main()
