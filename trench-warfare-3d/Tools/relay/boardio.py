#!/usr/bin/env python3
"""What the relay writes on the board (tw3d-board/relay/<station>/), so the other station can see it. One writer per
file: the station is in the path and every leg gets its own file. Nothing here stores a status to edit later.

  relay/<station>/legs/<run>-<nn>.json    one finished leg: unit, phase, how it ended, tokens, the report
  relay/<station>/stops/<run>.json        why a run stopped
Stdlib only. ASCII only.
"""
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "pipeline"))
from pipeline import now, write_json   # noqa: E402
import gitio                           # noqa: E402

KEEP = ("run", "leg", "unit", "source", "role", "phase", "model", "effort", "lane", "state", "exit_code", "seconds",
        "turns", "final_tokens", "level", "denials", "ran_model", "ran_mode", "report", "started_at", "finished_at")


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
