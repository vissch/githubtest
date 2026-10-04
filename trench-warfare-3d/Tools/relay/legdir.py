#!/usr/bin/env python3
"""Two folders per leg, the same shape for every kind of leg, outside any checkout (an untracked file in a checkout
changes the tree the gate records).

  <home>/runs/<run>/legs/<nn>/   the GUARD folder, the runner's: leg.json  system.md  card.md  hooks.json  prompt.txt
                                 out.jsonl  session.json  meter.json  meter.jsonl  trips.jsonl  denials.jsonl
                                 compact.json  red.patch  red-hashes.json  jobs/ (the leg's detached runs)
  <home>/runs/<run>/desk/<nn>/   the DESK folder, the leg's: plan.md or the phase's output, note.md
The leg is given the desk only; the hooks refuse every write to the guard folder, so a leg cannot edit its rules.
<home> is TW_RELAY_HOME, else %LOCALAPPDATA%/TrenchWarfare/relay. Stdlib only. ASCII only.
"""
import json, os, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "pipeline"))
from pipeline import now, read_json, write_json   # noqa: E402

LEG, RESULT, METER, COMPACT, CARD = "leg.json", "result.json", "meter.json", "compact.json", "card.md"
SYSTEM, HOOKS, PROMPT, OUT, NOTE, SESSION = "system.md", "hooks.json", "prompt.txt", "out.jsonl", "note.md", "session.json"


def home():
    return Path(os.environ.get("TW_RELAY_HOME")
                or Path(os.environ.get("LOCALAPPDATA", str(Path.home()))) / "TrenchWarfare" / "relay").resolve()


def leg_path(run, nn):
    return home() / "runs" / run / "legs" / ("%02d" % nn)


def desk_path(run, nn):
    return home() / "runs" / run / "desk" / ("%02d" % nn)


def new_leg(run, nn, unit, phase, phase_cfg, lim, worktree, lane, board):
    """Create both folders and leg.json: everything the hooks and the checks need, so nothing rides on env or argv."""
    d, desk = leg_path(run, nn), desk_path(run, nn)
    d.mkdir(parents=True, exist_ok=False)
    desk.mkdir(parents=True, exist_ok=False)
    rec = {"run": run, "leg": nn, "unit": unit["id"], "source": unit["source"], "role": unit["role"],
           "phase": phase, "mode": phase_cfg["mode"], "model": phase_cfg["model"], "effort": phase_cfg["effort"],
           "output": phase_cfg.get("output"), "worktree": str(worktree), "lane": lane, "board": str(board or ""),
           "desk": str(desk), "amber_tokens": lim["amber_tokens"], "red_tokens": lim["red_tokens"],
           "state": "NEW", "created_at": now()}
    write_json(d / LEG, rec)
    return d


def read(d):
    return read_json(Path(d) / LEG)


def desk(d):
    return Path(read(d)["desk"])


def update(d, **kw):
    rec = read(d)
    rec.update(kw)
    write_json(Path(d) / LEG, rec)
    return rec


def append(d, name, obj):
    with open(Path(d) / name, "a", encoding="utf-8", newline="\n") as f:
        f.write(json.dumps(obj, sort_keys=True) + "\n")
