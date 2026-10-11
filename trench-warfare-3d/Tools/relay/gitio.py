#!/usr/bin/env python3
"""Git facts about a checkout, and the one lock that says "a leg is working here". Every check a leg's result is
judged by is a script here: no model is asked. Stdlib only. ASCII only.
"""
import hashlib, os, re, subprocess, sys, tempfile, time
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


def commits(wt, before, after, most=20):
    """(the commits from before to after, oldest first and `most` at the most, each {sha, subject}; how many more
    there were). For a leg's record: what it added to its lane. Nothing when either end is unknown, or when the
    lane did not only grow (a rebase: the commits in between are then mostly other people's)."""
    if not before or not after or before == after:
        return [], 0
    if git_raw(["merge-base", "--is-ancestor", before, after], wt).returncode:
        return [], 0
    out = git(["log", "--reverse", "--format=%H%x09%s", "%s..%s" % (before, after)], wt, check=False)
    rows = [l.split("\t", 1) for l in out.splitlines() if "\t" in l]
    return [{"sha": sha[:10], "subject": subject[:120]} for sha, subject in rows[:most]], max(0, len(rows) - most)


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


def saved_ref(folder):
    """The ref a leg's uncommitted work is kept under: refs/relay/saved/<run>-<nn>, from the leg's guard folder
    (<home>/runs/<run>/legs/<nn>)."""
    f = Path(folder)
    return "refs/relay/saved/" + re.sub(r"[^A-Za-z0-9_.-]+", "-", "%s-%s" % (f.parent.parent.name, f.name)).strip("-.")


def _text(r):
    return r.stdout.decode("utf-8", "replace").strip()


def patch_tree(wt, patch_file):
    """The id of the tree that HEAD plus this patch gives, worked out in a throwaway index (the checkout and its
    own index are not touched); '' when the patch does not apply."""
    with tempfile.TemporaryDirectory() as tmp:
        env = dict(os.environ, GIT_INDEX_FILE=str(Path(tmp) / "index"))
        if git_raw(["read-tree", "HEAD"], wt, env).returncode:
            return ""
        if git_raw(["apply", "--cached", "--whitespace=nowarn", str(patch_file)], wt, env).returncode:
            return ""
        r = git_raw(["write-tree"], wt, env)
        return _text(r) if r.returncode == 0 else ""


def snapshot_dirty(wt, folder, clean=False):
    """Save uncommitted work beside the leg (never in the checkout): a patch that restores it (untracked and binary
    files too, through a throwaway index) and hashes to resume by.

    clean=True, the runner's (a leg it cannot trust, a unit that left work behind): the checkout is then put back to
    its HEAD, so the next run finds it usable; a saved tree that stayed in the checkout made every later run refuse
    it (the review, R002.3). That step destroys files, so it runs only after all of this held, and each is a
    comparison of ids, never of file lists:
      1. the throwaway index holds the whole dirty tree: git status against it shows nothing left over;
      2. HEAD plus red.patch, as the patch reads back from the leg's folder, gives the very tree of that index;
      3. the same tree is kept a second time as a commit on HEAD under saved_ref(folder), and the ref reads back;
      4. asked again as the last thing before the clean: HEAD has not moved and the checkout is still that tree.
    Then `git reset --hard HEAD` and `git clean -fd`: never -x, so an ignored folder (Library) is not touched, and a
    checkout nested in it is left alone. The record says how it went: "cleaned" True, or False with "kept_because"
    and the checkout exactly as it was. Put the work back with `git cherry-pick -n <ref>` or `git apply red.patch`."""
    paths = dirty(wt)
    if not paths:
        return None
    with tempfile.TemporaryDirectory() as tmp:
        env = dict(os.environ, GIT_INDEX_FILE=str(Path(tmp) / "index"))
        took = [git_raw(["read-tree", "HEAD"], wt, env), git_raw(["add", "-A"], wt, env)]
        diff = git_raw(["diff", "--cached", "--binary", "HEAD"], wt, env)
        Path(folder).mkdir(parents=True, exist_ok=True)
        (Path(folder) / "red.patch").write_bytes(diff.stdout)
        rec = {"head": head(wt), "branch": branch(wt), "files": file_hashes(wt, paths), "saved_at": now()}
        if clean:
            rec.update(_put_back(wt, folder, rec, took + [diff], env))
    write_json(Path(folder) / "red-hashes.json", rec)
    return rec


def _left_over(wt, env):
    """The paths of the checkout that the index of `env` does not hold as they are on disk (changed since, or never
    added); None when git cannot say."""
    r = git_raw(["status", "--porcelain", "-z", "--untracked-files=all"], wt, env)
    if r.returncode:
        return None
    recs, i, out = r.stdout.decode("utf-8", "replace").split("\0"), 0, []
    while i < len(recs):                                 # XY path: Y is the checkout against that index
        rec = recs[i]
        if len(rec) > 3:
            if rec[:2] == "??" or rec[1] != " ":
                out.append(rec[3:])
            if rec[0] in "RC":                           # the old name follows as its own record
                i += 1
        i += 1
    return out


def _put_back(wt, folder, rec, calls, env):
    """The checks of snapshot_dirty(clean=True), then the clean. {"tree", "ref", "cleaned", "kept_because"}.
    env names the throwaway index that holds the dirty tree."""
    out = {"tree": "", "ref": "", "cleaned": False, "kept_because": ""}

    def kept(why):
        out["kept_because"] = why
        return out
    made = git_raw(["write-tree"], wt, env)
    tree = out["tree"] = _text(made) if made.returncode == 0 else ""
    if any(c.returncode for c in calls) or not tree:
        return kept("git could not take the whole dirty tree into an index")
    left = _left_over(wt, env)
    if left is None or left:
        return kept("not all of it can be saved as a tree: %s" % ", ".join((left or ["git status failed"])[:3]))
    if patch_tree(wt, Path(folder) / "red.patch") != tree:
        return kept("HEAD plus the saved patch does not give the tree that was in the checkout")
    who = dict(os.environ)
    for k, v in (("GIT_AUTHOR_NAME", "relay"), ("GIT_AUTHOR_EMAIL", "relay@localhost"),
                 ("GIT_COMMITTER_NAME", "relay"), ("GIT_COMMITTER_EMAIL", "relay@localhost")):
        who.setdefault(k, v)
    ref = saved_ref(folder)
    made = git_raw(["commit-tree", tree, "-p", rec["head"], "-m",
                    "relay: uncommitted work put aside (%s)" % ref.rsplit("/", 1)[-1]], wt, who)
    if made.returncode or git_raw(["update-ref", ref, _text(made)], wt).returncode \
            or git(["rev-parse", "-q", "--verify", ref + "^{tree}"], wt, check=False) != tree:
        return kept("the saved tree could not be kept as a commit under %s" % ref)
    out["ref"] = ref
    if head(wt) != rec["head"] or _left_over(wt, env) != []:
        return kept("the checkout changed while it was being saved")
    if git_raw(["reset", "-q", "--hard", "HEAD"], wt).returncode or git_raw(["clean", "-q", "-fd"], wt).returncode:
        return kept("git could not put the checkout back to HEAD; what is saved is complete")
    still = dirty(wt)
    if still:
        return kept("git left these in the checkout: %s; what is saved is complete" % ", ".join(still[:3]))
    out["cleaned"] = True
    return out


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
