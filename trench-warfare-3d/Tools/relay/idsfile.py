#!/usr/bin/env python3
"""Finds which file on the board holds the bracketed ids (N5.3, T29b, U1, rv2-02, ...) a unit's goal names, so the
leg card can point straight at it instead of a leg searching the whole repo.

  card_line(body, work, board=None)   one card line naming the file, or None when no id is found or no file holds one

Candidates, in order: every .md path the body itself names, then work/docs/reference/decisions.md, then (when
`board` is given) board/relay/proposals/*.md. The one holding the most of the body's ids wins, a tie goes to the
first. When none holds any id but decisions.md exists, git blame finds the last commit that removed the row.
Stdlib only. ASCII only.
"""
import re
import subprocess
from pathlib import Path

ID = re.compile(r"\[([A-Za-z]{1,4}[\w.-]{0,8})\]")
MD_PATH = re.compile(r"\S*\.md")
MAX_BYTES = 400 * 1024
STRIP = "`\"'"


def ids(text):
    seen, out = set(), []
    for m in ID.findall(text):
        if m not in seen:
            seen.add(m)
            out.append(m)
        if len(out) == 8:
            break
    return out


def _read(p):
    try:
        if not p.is_file():
            return ""
        with p.open("r", encoding="utf-8", errors="replace") as f:
            return f.read(MAX_BYTES)
    except OSError:
        return ""


def _body_paths(body, work):
    out = []
    for m in MD_PATH.findall(body):
        p = Path(m.strip(STRIP))
        out.append(p if p.is_absolute() else Path(work) / p)
    return out


def _score(text, want):
    return sum(1 for w in want if "[%s]" % w in text)


def _blame_sha(work, rel, want):
    """The most recent commit whose version of `rel` still held one of `want` (not the commit that removed it)."""
    try:
        r = subprocess.run(["git", "log", "--format=%h", "--", rel], cwd=str(work),
                           capture_output=True, timeout=10)
    except (OSError, subprocess.TimeoutExpired):
        return None
    for sha in r.stdout.decode("utf-8", "replace").split():
        try:
            show = subprocess.run(["git", "show", "%s:%s" % (sha, rel)], cwd=str(work),
                                  capture_output=True, timeout=10)
        except (OSError, subprocess.TimeoutExpired):
            continue
        text = show.stdout.decode("utf-8", "replace")
        if _score(text, want):
            return sha
    return None


def card_line(body, work, board=None):
    want = ids(body)
    if not want:
        return None
    work = Path(work)
    dec = work / "docs" / "reference" / "decisions.md"
    candidates = _body_paths(body, work) + [dec]
    if board:
        candidates += sorted(Path(board).glob("relay/proposals/*.md"))

    best, best_score = None, 0
    for p in candidates:
        score = _score(_read(p), want)
        if score > best_score:
            best, best_score = p, score
    if best is not None:
        return "- The ids this unit names (%s) stand in %s" % (", ".join(want), best)

    if dec.exists():
        sha = _blame_sha(work, "docs/reference/decisions.md", want)
        if sha:
            return ("- The ids this unit names (%s) stand in %s, not at HEAD: read it with "
                    "git show %s:docs/reference/decisions.md" % (", ".join(want), dec, sha))
    return None
