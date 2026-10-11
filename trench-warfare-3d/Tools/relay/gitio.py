#!/usr/bin/env python3
"""Git facts about a checkout, and the one lock that says "a leg is working here". Every check a leg's result is
judged by is a script here: no model is asked. Stdlib only. ASCII only.
"""
import hashlib, os, re, stat, subprocess, sys, tempfile, time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "pipeline"))
from pipeline import now, proc_start, read_json, write_json   # noqa: E402

INTEGRATION = "claude/trench-warfare-2d-3d-plan-idt7lf"       # the same name Tools/health.py and Tools/land.py use
STALE_LOCK_S = 60
ORIGIN_S, FETCH_S = 60, 300             # how long origin gets for a question, and for a fetch; then the call is given up


class GitError(Exception):
    pass


def git_raw(args, cwd, env=None):
    return subprocess.run(["git"] + args, cwd=str(cwd), capture_output=True, env=env)


def git(args, cwd, check=True):
    r = git_raw(args, cwd)
    if check and r.returncode:
        raise GitError("git %s failed in %s: %s" % (" ".join(args), cwd, r.stderr.decode("utf-8", "replace").strip()))
    return r.stdout.decode("utf-8", "replace").strip()


def origin(args, cwd, wait=ORIGIN_S):
    """(exit code, output) of a git call that talks to origin: timed, and it may not ask for a password. One that
    is given up, or cannot start, counts as failed: (1, ""). The answer goes to a file, not a pipe, so a helper
    process git left behind cannot hold the reader (as boardio._origin, which reads nothing). An origin that hangs
    must not hold a run."""
    try:
        with tempfile.TemporaryFile() as out:
            r = subprocess.run(["git"] + args, cwd=str(cwd), stdin=subprocess.DEVNULL, stdout=out,
                               stderr=subprocess.DEVNULL, timeout=wait, env=dict(os.environ, GIT_TERMINAL_PROMPT="0"))
            out.seek(0)
            return r.returncode, out.read().decode("utf-8", "replace").strip()
    except (OSError, subprocess.TimeoutExpired):
        return 1, ""


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
    origin(["fetch", "-q", "origin"], wt, FETCH_S)
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
    origin(["fetch", "-q", "origin"], wt, FETCH_S)
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
    it (the review, R002.3).

    The saved tree (_fill) holds everything either index knows: a fresh index read from HEAD, so no entry carries a
    bit that hides an edit (assume-unchanged, skip-worktree) and a file HEAD tracks is compared by content whatever
    the checkout's own index says of it; then every changed and new file git does not ignore; then every file the
    checkout's own index holds beyond that (staged by force under an ignored pattern).

    The clean (_put_back) cannot touch what is not in that tree. It runs no `git reset --hard` and no `git clean`.
    It walks the paths in which the saved tree differs from HEAD, and nothing else: a file is first moved out of the
    checkout, then proved to be byte for byte the blob the saved tree holds for that path, and only then removed;
    one that is not goes back where it was and is named. HEAD's version of a path is written only where no file is.
    A file written after the save, a file git ignores, a file in neither tree: never touched. Before it starts:
    HEAD plus red.patch, as the patch reads back from the leg's folder, gives the very tree that was saved; the same
    tree is kept a second time as a commit on HEAD under saved_ref(folder); HEAD has not moved.
    The record says how it went: "cleaned" True; or False with "kept_because", and "touched" False when the
    checkout is exactly as it was, True when the clean began and did not leave it at HEAD (what is gone from it is
    then in the patch and the ref). Put the work back with `git cherry-pick -n <ref>` or `git apply red.patch`."""
    paths = dirty(wt)
    if not paths:
        return None
    with tempfile.TemporaryDirectory() as tmp:
        env = dict(os.environ, GIT_INDEX_FILE=str(Path(tmp) / "index"))
        took = _fill(wt, env)
        diff = git_raw(["diff", "--cached", "--binary", "HEAD"], wt, env)
        Path(folder).mkdir(parents=True, exist_ok=True)
        (Path(folder) / "red.patch").write_bytes(diff.stdout)
        rec = {"head": head(wt), "branch": branch(wt), "files": file_hashes(wt, paths), "saved_at": now()}
        if clean:
            rec.update(_put_back(wt, folder, rec, took + [diff.returncode], env))
    write_json(Path(folder) / "red-hashes.json", rec)
    return rec


def _z(r):
    """The NUL-separated records of a git call's output."""
    return [x for x in r.stdout.decode("utf-8", "surrogateescape").split("\0") if x]


def _feed(args, wt, paths, env=None):
    """A git call that takes its paths on stdin, NUL-separated and never as patterns. Its exit code."""
    return subprocess.run(["git", "--literal-pathspecs"] + args, cwd=str(wt), env=env, capture_output=True,
                          input="".join(p + "\0" for p in paths).encode("utf-8", "surrogateescape")).returncode


def _fill(wt, env):
    """Fill the throwaway index of `env` with the dirty tree; the exit codes of its steps (all 0 = it worked).
      1. read HEAD: fresh entries, with none of the bits and none of the file times of the checkout's own index;
      2. a path the checkout's index marks skip-worktree and that is not on disk is not part of this checkout (a
         sparse one): it gets the same mark here and keeps HEAD's entry, where it would else read as deleted;
      3. `git add -A`: every changed, deleted and new file git does not ignore, compared by content;
      4. `git add -f` for what the checkout's own index holds and this one still lacks, where the file is on disk.
    Seeded from a copy of the checkout's index (13f8e532) an entry marked assume-unchanged hid its edit from every
    check, and a file HEAD tracks that was taken out of the index under an ignored pattern was in no tree: both
    went with the reset (the second reader, RV.1b and RV.1c)."""
    codes = [git_raw(["read-tree", "HEAD"], wt, env).returncode]
    own = git_raw(["ls-files", "-v", "-z"], wt)
    codes.append(own.returncode)
    tagged = [(x[0], x[2:]) for x in _z(own) if len(x) > 2]
    away = [p for tag, p in tagged if tag in "Ss" and not os.path.lexists(os.path.join(str(wt), p))]
    known = set(_z(git_raw(["ls-files", "-z"], wt, env)))
    away = [p for p in away if p in known]
    if away:
        codes.append(_feed(["update-index", "--skip-worktree", "-z", "--stdin"], wt, away, env))
    codes.append(git_raw(["add", "-A"], wt, env).returncode)
    known = set(_z(git_raw(["ls-files", "-z"], wt, env)))
    more = [p for _, p in tagged if p not in known and os.path.isfile(os.path.join(str(wt), p))]
    if more:
        codes.append(_feed(["add", "-f", "--pathspec-from-file=-", "--pathspec-file-nul"], wt, more, env))
    return codes


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
    """The checks of snapshot_dirty(clean=True), then the clean. {"tree", "ref", "cleaned", "touched",
    "kept_because"}. calls: the exit codes of the steps that filled the throwaway index of `env`."""
    out = {"tree": "", "ref": "", "staged_ref": "", "cleaned": False, "touched": False, "kept_because": ""}

    def kept(why):
        out["kept_because"] = why
        return out
    made = git_raw(["write-tree"], wt, env)
    tree = out["tree"] = _text(made) if made.returncode == 0 else ""
    if any(calls) or not tree:
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
    if head(wt) != rec["head"]:
        return kept("the checkout's HEAD moved while it was being saved")
    names = git_raw(["diff", "--name-status", "--no-renames", "-z", rec["head"], tree], wt)
    blobs = git_raw(["ls-tree", "-r", "-z", tree], wt)
    if names.returncode or blobs.returncode:
        return kept("git could not list what the saved tree changes")
    parts = _z(names)
    changes = list(zip(parts[0::2], parts[1::2]))        # (A | M | D | T, path), the saved tree against HEAD
    saved = {}
    for row in _z(blobs):                                # "<mode> <type> <id>\t<path>"
        meta, _, path = row.partition("\t")
        saved[path] = meta.split()
    under_way = in_progress(wt)
    if under_way:                                        # setting index entries back would leave that state behind,
        return kept("git is in the middle of %s (%s in its folder): nothing is put back, for the next commit here "
                    "would carry it on" % under_way)     # and the next leg's first commit would be a merge commit
    own = git_raw(["write-tree"], wt)                    # what is staged, as the checkout's own index holds it
    if own.returncode:                                   # unmerged paths, or a lock a killed git left: git says which
        return kept("the checkout's index cannot be written as a tree (git: %s): nothing is put back"
                    % " ".join(own.stderr.decode("utf-8", "replace").split())[:240])
    if _text(own) not in (tree, git(["rev-parse", "-q", "--verify", rec["head"] + "^{tree}"], wt, check=False)):
        made = git_raw(["commit-tree", _text(own), "-p", rec["head"], "-m",
                        "relay: what was staged when work was put aside (%s)" % ref.rsplit("/", 1)[-1]], wt, who)
        if made.returncode or git_raw(["update-ref", ref + "-staged", _text(made)], wt).returncode \
                or git(["rev-parse", "-q", "--verify", ref + "-staged^{tree}"], wt, check=False) != _text(own):
            return kept("what was staged could not be kept as a commit under %s-staged" % ref)
        out["staged_ref"] = ref + "-staged"
    hold = put_back_dir(wt) / ref.rsplit("/", 1)[-1]
    try:
        hold.mkdir(parents=True, exist_ok=False)
    except OSError as e:                                 # this leg's folder was cleaned before and something is held
        return kept("there is no folder to move the files to (%s: %s): nothing is put back" % (hold, e.strerror or e))
    out["touched"] = True                                # from here on the checkout is no longer as it was
    stays = _restore(wt, changes, saved, hold)
    still = dirty(wt)
    if stays or still:
        return kept("the rest is at HEAD and saved; these stay as they are: %s"
                    % "; ".join((stays + [p for p in still if not any(s.startswith(p + " ") for s in stays)])[:4]))
    out["cleaned"] = True
    return out


STATES = (("MERGE_HEAD", "a merge"), ("CHERRY_PICK_HEAD", "a cherry-pick"), ("REVERT_HEAD", "a revert"),
          ("rebase-merge", "a rebase"), ("rebase-apply", "a rebase or an am"),
          ("sequencer", "a row of cherry-picks or reverts"), ("BISECT_LOG", "a bisect"))


def in_progress(wt):
    """(what, the name in the git folder) of an operation git is in the middle of in this checkout, or None. Asked
    of git for each name, so a worktree's own folder is the one looked in."""
    for name, what in STATES:
        p = Path(git(["rev-parse", "--git-path", name], wt, check=False) or name)
        if (p if p.is_absolute() else Path(wt) / p).exists():
            return what, name
    return None


def put_back_dir(wt):
    """Where the clean holds a file between taking it out of the checkout and proving what it is."""
    return Path(git(["rev-parse", "--absolute-git-dir"], wt)) / "relay-put-back"


def held_files(wt):
    """What a clean that did not finish left in put_back_dir: [(the held file, the path it was moved from)]. Each
    held file has its path beside it as <name>.path, written before the move."""
    root, out = put_back_dir(wt), []
    for f in sorted(root.rglob("*")) if root.is_dir() else []:
        if f.is_file() and f.suffix != ".path":
            note = f.with_name(f.name + ".path")
            out.append((f, note.read_text(encoding="utf-8", errors="replace") if note.is_file() else "?"))
    return out


def _restore(wt, changes, saved, hold):
    """Put the paths of `changes` back to HEAD, and no other path. Returns what stays, each as "<path> (<why>)".
    hold: a new, empty folder under the checkout's git folder (put_back_dir), made by the caller.
    What can remove or overwrite a file here, and what bounds each:
      os.replace    moves one file of `changes` into a new, empty folder under the checkout's git folder: nothing is
                    overwritten, nothing is gone;
      os.remove     only of a file in that folder, after `git hash-object` of it gave the very blob id the saved
                    tree holds for its path, with the path's filters or, failing that, of its bytes as they are (a
                    file HEAD holds with CRLF is saved with CRLF, and the filters alone would never match it). So
                    what is removed is in the patch and in the ref as git stores it, which is not always byte for
                    byte: where git rewrites line ends, the LF and the CRLF form of one text are one blob, and a
                    file in which only the line ends changed after the save is removed as equal. A file that
                    hashes to anything else is moved back to its place (or kept in the folder when that place is
                    taken). A file that cannot be removed stays in the folder and is named. Beside every held file
                    its path is written first (<n>.path), so a clean that was killed half way can be read
                    (held_files), and busy_reason refuses the checkout while anything is held;
      os.rmdir      a folder a removed file was in, when empty; and that folder under the git folder, when empty;
      git reset -q HEAD -- <paths>   the index entries of the paths of `changes` and of what the index had staged;
                    no file of the checkout. What was staged is kept first, as a commit under <ref>-staged
                    (_put_back), whenever it is neither HEAD's tree nor the saved one: staged bytes that differ
                    from the file on disk, a file staged and then deleted. The base of this lane never cleaned,
                    so its index kept them;
      git checkout-index -u (without -f)   writes HEAD's version of a path where no file is; git skips a path that
                    has one."""
    wt, stays, gone = Path(wt), [], []
    last = lambda c: (c[1].rsplit("/", 1)[-1] in (".gitattributes", ".gitignore"), c[1])    # the rules go last
    for n, (kind, path) in enumerate(sorted(changes, key=last)):
        f = wt / path
        if kind == "D" or not os.path.lexists(f):        # nothing on disk to take away
            if kind != "D" or not os.path.lexists(f):
                gone.append((kind, path))
            else:
                stays.append("%s (a file is there that the saved tree does not have)" % path)
            continue
        meta = saved.get(path) or []
        if len(meta) != 3 or meta[0] not in ("100644", "100755") or meta[1] != "blob" or not f.is_file() or f.is_symlink():
            stays.append("%s (not a plain file)" % path)
            continue
        held = hold / ("%06d" % n)
        note = hold / ("%06d.path" % n)
        try:
            note.write_text(path, encoding="utf-8", errors="surrogateescape")    # first: a kill must leave a name
            os.replace(f, held)
        except OSError as e:
            stays.append("%s (it could not be moved: %s)" % (path, e.strerror or e))
            _unlink(note)
            continue
        if _same_blob(wt, path, held, meta[2]):
            gone.append((kind, path))                    # proved: this is the saved blob. Its place is free
            try:
                os.remove(held)
                _unlink(note)
            except OSError:                              # a read-only file, on Windows: it is proved, so it is
                try:                                     # made writable and removed; left there, it would stop
                    os.chmod(held, stat.S_IWRITE)        # every later run
                    os.remove(held)
                    _unlink(note)
                except OSError:
                    stays.append("%s (saved; its copy is now %s)" % (path, held))
            continue
        try:                                             # changed since it was saved: back where it was
            if os.path.lexists(f):
                raise OSError("its place is taken")
            os.replace(held, f)
            _unlink(note)
            stays.append("%s (changed after it was saved)" % path)
        except OSError:
            stays.append("%s (changed after it was saved; it is now %s)" % (path, held))
    for kind, path in gone:                              # folders the work made, when nothing is left in them
        d = (wt / path).parent
        while kind != "D" and d != wt and wt in d.parents:
            try:
                d.rmdir()
            except OSError:
                break
            d = d.parent
    staged = _z(git_raw(["diff", "--cached", "--name-only", "--no-renames", "-z", "HEAD"], wt))
    staged = sorted(set(staged) | {p for _, p in changes})       # also an entry only meant to be added (add -N),
    if staged and _feed(["reset", "-q", "--pathspec-from-file=-", "--pathspec-file-nul", "HEAD"], wt, staged):
        stays.append("the index (git reset of %d paths failed)" % len(staged))   # which that list does not name
    write = [p for kind, p in gone if kind != "A"]       # paths HEAD has: their entries are HEAD's again by now
    if write:
        for bit in ("--no-assume-unchanged", "--no-skip-worktree"):     # one mark a call: git takes the last one
            _feed(["update-index", "-z", bit, "--stdin"], wt, write)
        if _feed(["checkout-index", "-u", "-z", "--stdin"], wt, write):      # -u: the entry takes the new file's times
            stays.append("HEAD's files (git checkout-index failed for some of %d paths)" % len(write))
    try:
        hold.rmdir()
        hold.parent.rmdir()
    except OSError:
        pass
    return stays


def _same_blob(wt, path, held, blob):
    """True when the held file is the blob the saved tree has for its path: as git would add it under the path's
    filters, or else as its bytes stand. `git hash-object --path` reads no index, and `git add` does: a file HEAD
    holds with CRLF stays CRLF in the saved tree, while the filters alone would hash it as LF, so it never matched,
    was moved back as "changed" and kept the checkout dirty for every run (the second reader, RV3.5)."""
    if git(["hash-object", "--path=" + path, str(held)], wt, check=False) == blob:
        return True
    return git(["hash-object", "--no-filters", str(held)], wt, check=False) == blob


def _unlink(f):
    try:
        os.remove(f)
    except OSError:
        pass


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
    out = origin(["ls-remote", "--heads", "origin"], wt)[1]
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
    under_way = in_progress(wt)
    if under_way:                                    # also with nothing changed: the next commit here would carry
        return "git is in the middle of %s there (%s in its folder): end or abort it first" % under_way   # it on
    held = held_files(wt)
    if held:                                         # a clean that was killed, or a copy it could not remove
        return ("the relay moved %d file%s out of it and did not finish, in %s: %s. Put each back or delete it, "
                "then remove that folder" % (len(held), "" if len(held) == 1 else "s", put_back_dir(wt),
                                             "; ".join("%s (was %s)" % (f, was) for f, was in held)))
    if dirty(wt) and not matches_snapshot(wt, snapshot):
        return "it has uncommitted changes that are not a saved leg snapshot"
    if quiet and age < quiet and not snapshot:   # quiet: seconds without git activity, 0 = skip
        return "its git index moved %d s ago: somebody may be working in it" % age
    return None
