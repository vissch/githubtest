#!/usr/bin/env python3
"""Git facts about a checkout, and the one lock that says "a leg is working here". Every check a leg's result is
judged by is a script here: no model is asked. Stdlib only. ASCII only.
"""
import hashlib, os, subprocess, sys, tempfile, time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "pipeline"))
from pipeline import now, proc_start, read_json, write_json   # noqa: E402

INTEGRATION = "claude/trench-warfare-2d-3d-plan-idt7lf"       # the same name Tools/health.py and Tools/land.py use
STALE_LOCK_S = 60


class GitError(Exception):
    pass


def git_raw(args, cwd, env=None):
    return subprocess.run(["git"] + args, cwd=str(cwd), capture_output=True, env=env)


def git(args, cwd, check=True):
    r = git_raw(args, cwd)
    if check and r.returncode:
        raise GitError("git %s failed in %s: %s" % (" ".join(args), cwd, r.stderr.decode("utf-8", "replace").strip()))
    return r.stdout.decode("utf-8", "replace").strip()


def head(wt):
    return git(["rev-parse", "HEAD"], wt)


def branch(wt):
    return git(["rev-parse", "--abbrev-ref", "HEAD"], wt)


def dirty(wt):
    """Changed and untracked paths, exactly as they are on disk (NUL records: no quoting, a rename gives both)."""
    raw = git_raw(["status", "--porcelain", "-z", "--untracked-files=all"], wt).stdout.decode("utf-8", "replace")
    out, recs, i = [], raw.split("\0"), 0
    while i < len(recs):
        r = recs[i]
        if len(r) > 3:
            out.append(r[3:])
            if r[0] in "RC" and i + 1 < len(recs):          # the old name follows as its own record
                out.append(recs[i + 1])
                i += 1
        i += 1
    return sorted(set(out))


def remote_head(wt, lane):
    git(["fetch", "-q", "origin"], wt, check=False)
    return git(["rev-parse", "-q", "--verify", "origin/" + lane], wt, check=False)


def changed_files(wt, before, after):
    if not before or not after or before == after:
        return []
    return [l for l in git(["diff", "--name-only", before, after], wt).splitlines() if l]


def code_changed(wt, before, after):
    """A change that touches something outside docs/: a docs-only commit does not count as progress."""
    return [f for f in changed_files(wt, before, after) if not f.startswith("docs/")]


def pushed(wt, lane):
    """True when origin's lane is exactly this checkout's HEAD."""
    return remote_head(wt, lane) == head(wt)


def switch_lane(wt, lane):
    """Put the work checkout on the unit's lane; a new lane starts at origin's integration branch."""
    git(["fetch", "-q", "origin"], wt, check=False)
    if branch(wt) == lane:
        return "on"
    if git(["rev-parse", "-q", "--verify", lane], wt, check=False):
        git(["switch", "-q", lane], wt)
        return "switched"
    start = "origin/" + lane if git(["rev-parse", "-q", "--verify", "origin/" + lane], wt, check=False) \
        else "origin/" + INTEGRATION
    git(["switch", "-q", "-c", lane, start], wt)
    return "created from " + start


# ---------- a dirty tree handed to the next leg ----------

def file_hashes(wt, paths):
    out = {}
    for p in paths:
        f = Path(wt) / p
        out[p] = hashlib.sha256(f.read_bytes()).hexdigest() if f.is_file() else "gone"
    return out


def snapshot_dirty(wt, folder):
    """Save uncommitted work beside the leg (never in the checkout): a patch that restores it (untracked and binary
    files too, through a throwaway index) and hashes to resume by."""
    paths = dirty(wt)
    if not paths:
        return None
    with tempfile.TemporaryDirectory() as tmp:
        env = dict(os.environ, GIT_INDEX_FILE=str(Path(tmp) / "index"))
        git_raw(["read-tree", "HEAD"], wt, env)
        git_raw(["add", "-A"], wt, env)
        patch = git_raw(["diff", "--cached", "--binary", "HEAD"], wt, env).stdout
    Path(folder).mkdir(parents=True, exist_ok=True)
    (Path(folder) / "red.patch").write_bytes(patch)
    rec = {"head": head(wt), "branch": branch(wt), "files": file_hashes(wt, paths), "saved_at": now()}
    write_json(Path(folder) / "red-hashes.json", rec)
    return rec


def matches_snapshot(wt, rec):
    """The next leg may resume a dirty tree only if it is byte for byte the one that was saved."""
    return bool(rec) and head(wt) == rec["head"] and file_hashes(wt, dirty(wt)) == rec["files"]


# ---------- one leg per checkout ----------

def lock_path(wt, home):
    return Path(home) / "locks" / (hashlib.sha256(os.path.normcase(str(Path(wt).resolve())).encode()).hexdigest()[:16]
                                   + ".json")


def lock_holder(wt, home):
    rec = read_json(lock_path(wt, home)) if lock_path(wt, home).exists() else None
    return rec if rec and proc_start(rec["pid"]) == rec["pid_start"] else None


def take_lock(wt, home, who, lane=None):
    """Check and write under one exclusive file, so two runners cannot both take it; a guard file a crash left
    behind is dropped after a minute."""
    p = lock_path(wt, home)
    p.parent.mkdir(parents=True, exist_ok=True)
    guard = str(p) + ".lock"
    try:
        if time.time() - os.path.getmtime(guard) > STALE_LOCK_S:
            os.remove(guard)
    except OSError:
        pass
    try:
        fd = os.open(guard, os.O_CREAT | os.O_EXCL | os.O_WRONLY)
    except FileExistsError:
        raise GitError("another runner is taking %s right now; retry" % wt)
    try:
        cur = lock_holder(wt, home)
        if cur and cur["pid"] != os.getpid():
            raise GitError("%s is held by %s (pid %d)" % (wt, cur["who"], cur["pid"]))
        me = {"who": who, "pid": os.getpid(), "pid_start": proc_start(os.getpid()), "worktree": str(wt),
              "lane": lane, "taken_at": now()}
        write_json(p, me)
        write_json(marker_path(wt), me)             # prepush.py and land.py read this: a leg holds the checkout
    finally:
        os.close(fd)
        os.remove(guard)


def release_lock(wt, home):
    cur = lock_holder(wt, home)
    if cur and cur["pid"] == os.getpid():
        lock_path(wt, home).unlink()
        try:
            marker_path(wt).unlink()
        except OSError:
            pass


def marker_path(wt):
    return Path(git(["rev-parse", "--absolute-git-dir"], wt)) / "relay-leg.json"


def hooks_dir(wt):
    custom = git(["config", "--get", "core.hooksPath"], wt, check=False)
    folder = Path(custom) if custom else Path(git(["rev-parse", "--git-common-dir"], wt)) / "hooks"
    return folder if folder.is_absolute() else (Path(wt) / folder).resolve()


def install_prepush(wt):
    """Put the relay's pre-push hook in the repo (shared by its checkouts; it acts only where a marker is)."""
    folder = hooks_dir(wt)
    fwd = lambda p: str(p).replace("\\", "/")
    body = ('#!/bin/sh\n# relay pre-push (Tools/relay/prepush.py): acts only in a checkout a relay leg holds\n'
            'exec "%s" "%s" "$@"\n' % (fwd(sys.executable), fwd(HERE / "prepush.py")))
    f = folder / "pre-push"
    if f.exists() and "relay pre-push" not in f.read_text(encoding="utf-8", errors="replace"):
        raise GitError("%s is not the relay's hook: the relay cannot guard pushes from this repo" % f)
    if not f.exists() or f.read_text(encoding="utf-8") != body:
        folder.mkdir(parents=True, exist_ok=True)
        f.write_text(body, encoding="utf-8", newline="\n")
    return f


DOOR_CONFIG = r"^(core\.(hookspath|sshcommand|fsmonitor|editor|pager)|alias\.|remote\.|url\.|credential\.|push\.|branch\..*\.pushremote)"


def door_state(wt):
    """What git's push guard rests on, as text: every file in the hooks folder, this checkout's marker (without the
    lane, which the runner itself changes per unit) and the git settings that decide where and how a push goes."""
    out = {"hook " + k: v for k, v in tree_hashes(hooks_dir(wt), ()).items()}
    out.setdefault("hook pre-push", "missing")
    rec = read_json(marker_path(wt)) if marker_path(wt).exists() else None
    out["leg marker"] = "%s %s" % (rec.get("pid"), rec.get("pid_start")) if isinstance(rec, dict) else "missing"
    out["settings"] = git(["config", "--get-regexp", DOOR_CONFIG], wt, check=False)
    return out


def code_state(folder):
    """(commit, [changed or new files]) of the checkout the relay's own code runs from, looking only at the
    folders the runner hashes (Tools/relay, Tools/pipeline). ("", []) when it is not in a git checkout."""
    folder = Path(folder)
    r = git_raw(["rev-parse", "HEAD"], folder)
    if r.returncode:
        return "", []
    out = git_raw(["status", "--porcelain", "--untracked-files=all", "--", str(folder), str(folder.parent / "pipeline")],
                  folder).stdout.decode("utf-8", "replace")
    return r.stdout.decode().strip(), [l[3:] for l in out.splitlines() if len(l) > 3 and "__pycache__" not in l]


def remote_heads(wt):
    """{ref: sha} for every branch on origin, asked of origin itself."""
    out = git(["ls-remote", "--heads", "origin"], wt, check=False)
    return {l.split("\t")[1]: l.split("\t")[0] for l in out.splitlines() if "\t" in l}


def tree_hashes(root, skip=(".git", "__pycache__")):
    """{relative path: sha} of every file under root, leaving out the skip folders (top-level names or any depth)."""
    out, root = {}, Path(root)
    for dirpath, dirs, files in os.walk(root):
        dirs[:] = [x for x in dirs if x not in skip]
        for name in files:
            f = Path(dirpath) / name
            try:
                out[f.relative_to(root).as_posix()] = hashlib.sha256(f.read_bytes()).hexdigest()
            except OSError:
                out[f.relative_to(root).as_posix()] = "unreadable"
    return out


def busy_reason(wt, home, snapshot=None, quiet=600):
    """Why no leg may start in this checkout now, or None. One session per checkout is the repo's rule."""
    wt = Path(wt)
    if not wt.exists():
        return "%s does not exist. Make it once: git worktree add --detach \"%s\" origin/%s" % (wt, wt, INTEGRATION)
    if not (wt / ".git").exists():
        return "%s is not a git checkout" % wt
    gitdir = Path(git(["rev-parse", "--absolute-git-dir"], wt))      # read the clock first: git status below
    newest = max((f.stat().st_mtime for f in (gitdir / "index", gitdir / "HEAD") if f.exists()), default=0)
    age = time.time() - newest                                       # may refresh the index itself
    cur = lock_holder(wt, home)
    if cur and cur["pid"] != os.getpid():
        return "a relay leg holds it (%s)" % cur["who"]
    if (wt / "trench-warfare-3d" / "Temp" / "UnityLockfile").exists():
        try:
            open(wt / "trench-warfare-3d" / "Temp" / "UnityLockfile", "rb").close()
        except OSError:
            return "a Unity editor has this project open"
    if dirty(wt) and not matches_snapshot(wt, snapshot):
        return "it has uncommitted changes that are not a saved leg snapshot"
    if quiet and age < quiet and not snapshot:   # quiet: seconds without git activity, 0 = skip
        return "its git index moved %d s ago: somebody may be working in it" % age
    return None
