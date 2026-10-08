#!/usr/bin/env python3
"""What the relay writes on the board (tw3d-board/relay/<station>/), so the other station can see it. One writer per
file: the station is in the path and every leg gets its own file. Nothing here stores a status to edit later.

  relay/<station>/legs/<run>-<nn>.json    one finished leg: unit, phase, how it ended, tokens, cost, week used, report
  relay/<station>/stops/<run>.json        why a run stopped
  relay/<station>/lessons.md              one row per critic round: the score and the first mandated fix
  relay/<station>/tuning.json             the limits the last retrospective set (inside the bounds of limits.json)
  relay/proposals/<run>-<nn>.md           a retrospective's proposals for role texts and rules: the owner reads them
Stdlib only. ASCII only.
"""
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "pipeline"))
from pipeline import now, read_json, write_json   # noqa: E402
import gitio                           # noqa: E402

KEEP = ("run", "leg", "unit", "source", "role", "phase", "model", "effort", "lane", "state", "exit_code", "seconds",
        "turns", "final_tokens", "level", "denials", "guard_refusals", "ran_model", "ran_mode", "report", "started_at",
        "finished_at", "cost_usd", "tokens_in", "tokens_out", "cache_read", "cache_write", "week_start", "week_end",
        "week_used", "resumed")


def folder(board, station):
    return Path(board) / "relay" / station


def leg_record(board, station, leg, **extra):
    rec = {k: leg.get(k) for k in KEEP}
    rec.update(extra)
    p = folder(board, station) / "legs" / ("%s-%02d.json" % (leg["run"], leg["leg"]))
    write_json(p, rec)
    return p


def stop_record(board, station, run, reason, legs, detail="", **extra):
    p = folder(board, station) / "stops" / (run + ".json")
    write_json(p, dict(extra, run=run, reason=reason, detail=detail, legs=legs, stopped_at=now()))
    return p


LESSONS_HEAD = "| When | Unit | Role | Round | Score | Target | First mandated fix |\n|---|---|---|---|---|---|---|\n"


def lesson(board, station, unit, round_no, score, target, fix):
    """Append one critic round to the station's lessons table (the retrospective reads it)."""
    p = folder(board, station) / "lessons.md"
    p.parent.mkdir(parents=True, exist_ok=True)
    row = "| %s | %s | %s | %d | %d | %d | %s |\n" % (now(), unit["id"], unit["role"], round_no, score, target,
                                                    " ".join(str(fix).replace("|", "/").split())[:160])
    with open(p, "a", encoding="utf-8", newline="\n") as f:
        f.write(("" if p.stat().st_size else LESSONS_HEAD) + row)
    return p


def tuning(board, station):
    """{key: number}: what the last retrospective on this station set, or {} when none did."""
    p = folder(board, station) / "tuning.json"
    try:
        return {k: v for k, v in read_json(p)["limits"].items() if isinstance(v, (int, float))} if p.exists() else {}
    except (OSError, ValueError, KeyError, AttributeError):
        return {}


def keep_tuning(board, station, run, limits):
    p = folder(board, station) / "tuning.json"
    write_json(p, {"limits": limits, "run": run, "set_at": now()})
    return p


def proposals(board, run, nn, text):
    p = Path(board) / "relay" / "proposals" / ("%s-%02d.md" % (run, nn))
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(text.rstrip() + "\n", encoding="utf-8", newline="\n")
    return p


def push(board, message):
    """Commit and push only what the relay wrote (relay/ and evidence/); a failed push keeps the local commit and
    is reported, never raised. A rebase that fails is aborted, so the board is never left half-rebased."""
    if not (Path(board) / ".git").exists():
        return "no board repo"
    try:
        have = [n for n in ("relay", "evidence", "results") if (Path(board) / n).exists()]   # a missing one fails all
        gitio.git(["add", "--"] + have, board, check=False)
        if not gitio.git(["diff", "--cached", "--name-only"], board):
            return "nothing to push"
        gitio.git(["commit", "-q", "-m", message], board)
        for _ in range(3):
            if gitio.git_raw(["pull", "--rebase", "-q"], board).returncode:
                gitio.git(["rebase", "--abort"], board, check=False)
                continue
            if gitio.git_raw(["push", "-q"], board).returncode == 0:
                return "pushed"
    except gitio.GitError as e:
        return "board push failed: %s" % e
    return "push failed: the commit is kept locally"


def push_kept(board):
    """Push what an earlier push() left on this machine's board: it keeps the commit when the push fails, and with
    nothing new to commit the next push() ends before it pushes. For a caller that finds its own work committed
    already (`relay.py add --unit`, called again). Reported like push(), never raised."""
    if not (Path(board) / ".git").exists():
        return "no board repo"
    ahead = gitio.git_raw(["rev-list", "--count", "@{u}..HEAD"], board)
    if ahead.returncode or ahead.stdout.decode("utf-8", "replace").strip() in ("", "0"):
        return "nothing to push"                 # also a board with no origin to compare with
    try:
        for _ in range(3):
            if gitio.git_raw(["pull", "--rebase", "-q"], board).returncode:
                gitio.git(["rebase", "--abort"], board, check=False)
                continue
            if gitio.git_raw(["push", "-q"], board).returncode == 0:
                return "pushed"
    except gitio.GitError as e:
        return "board push failed: %s" % e
    return "push failed: the commit is kept locally"
