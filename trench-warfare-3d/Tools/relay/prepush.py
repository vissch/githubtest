#!/usr/bin/env python3
"""The git pre-push hook the relay installs: while a relay leg holds a checkout, git itself refuses every push from
it except a plain, forward push of the leg's own lane. It does not matter how the push command was written or
wrapped: git runs this before anything leaves the machine.

The hook file is shared by every checkout of the repo, but it acts only where the runner left a marker
(<that checkout's git dir>/relay-leg.json, with a live runner pid). Everywhere else it does nothing, except for a
process that carries the leg's TW_RELAY variable: that one may not push from any other checkout at all.
Stdin, from git: one line per ref, "<local ref> <local sha> <remote ref> <remote sha>". Stdlib only. ASCII only.
"""
import json, os, re, subprocess, sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "pipeline"))
from pipeline import proc_start   # noqa: E402

MARKER = "relay-leg.json"
ZERO = "0" * 40


def git(args):
    r = subprocess.run(["git"] + args, capture_output=True)
    return r.returncode, r.stdout.decode("utf-8", "replace").strip()


def marker():
    """The live leg's record for the checkout git is pushing from, or None."""
    code, gitdir = git(["rev-parse", "--absolute-git-dir"])
    p = Path(gitdir) / MARKER
    if code or not p.exists():
        return None
    try:
        rec = json.loads(p.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {"lane": None}                    # a marker nobody can read: refuse everything
    return rec if proc_start(rec.get("pid", -1)) == rec.get("pid_start") else None


def rebased_name(lane, remote_ref, remote_sha):
    """True for a NEW branch <lane>-b .. <lane>-j: where a leg that rebased its lane pushes it, since it may not
    force-push the lane itself (2026-10-15: two legs ended BLOCKED on that, and a session pushed them by hand)."""
    return bool(lane) and remote_sha == ZERO and re.fullmatch(re.escape("refs/heads/" + lane) + "-[b-j]", remote_ref) is not None


def refusal(rec, line):
    local_ref, local_sha, remote_ref, remote_sha = line.split()[:4]
    lane = rec.get("lane")
    if local_sha != ZERO and rebased_name(lane, remote_ref, remote_sha):
        return None
    if not lane or remote_ref != "refs/heads/" + lane:
        return "a relay leg pushes only its own lane (%s), not %s" % (lane, remote_ref)
    if local_sha == ZERO:
        return "a relay leg does not delete a branch"
    if remote_sha != ZERO and git(["merge-base", "--is-ancestor", remote_sha, local_sha])[0] != 0:
        return ("a relay leg does not force-push: origin's %s is not an ancestor of what you push. A rebased lane is "
                "pushed as a new branch instead: git push origin HEAD:refs/heads/%s-b (or -c .. -j when taken)" % (lane, lane))
    return None


def main():
    rec = marker()
    if rec is None:
        if os.environ.get("TW_RELAY"):           # a leg (or a job it started), pushing from some other checkout
            print("relay: push refused: a relay leg pushes only from the checkout it holds", file=sys.stderr)
            return 1
        return 0
    for line in sys.stdin.read().splitlines():
        if len(line.split()) >= 4:
            why = refusal(rec, line)
            if why:
                print("relay: push refused: " + why, file=sys.stderr)
                return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
