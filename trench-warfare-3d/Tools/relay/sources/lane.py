"""Source: free lane work the owner queued on the board, one file per unit:

  <board>/relay/queue/<id>.json   {"id" (= the file name), "lane", "role", "goal" (the owner's words),
                                   "done_when": ["program", "arg", ...]  (run in the checkout; exit 0 = done)}
  <board>/relay/done/<id>.json    written by the runner when done_when passes; a unit with one is never picked again
  <board>/relay/notes/<id>.md     the last leg's handoff note, when the unit is not done yet
This is the only source with a handoff note: its state is not on the board or in a ledger. The note reaches the next
run's legs on the card, with each of its predictions scored hit or miss by script.
"""
import re, subprocess, sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "pipeline"))
from pipeline import now, read_json, write_json   # noqa: E402

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import config, gitio, papers                      # noqa: E402
from sources import verdict                       # noqa: E402

NAME = "lane"
NEED = ("id", "lane", "role", "goal", "done_when")
ID = re.compile(r"^[\w.-]+$")


def load(p):
    try:
        u = read_json(p)
    except ValueError as e:
        raise SystemExit("relay: %s is not valid JSON: %s" % (p.name, e))
    missing = [k for k in NEED if not u.get(k)]
    if missing:
        raise SystemExit("relay: %s has no %s" % (p.name, ", ".join(missing)))
    if not ID.match(u["id"]) or u["id"] != p.stem:
        raise SystemExit("relay: %s: id must be the file name, letters, digits, . _ - only" % p.name)
    if not u["lane"].startswith(("lane/sim/", "lane/show/")):
        raise SystemExit("relay: %s: %s is not a lane/sim or lane/show lane" % (p.name, u["lane"]))
    if not isinstance(u["done_when"], list) or not all(isinstance(w, str) for w in u["done_when"]):
        raise SystemExit('relay: %s: done_when must be a list of words, e.g. ["python", "Tools/x.py", "--check"]' % p.name)
    return u


def trusted(board, p):
    """A queue file counts only when it is committed on the board and unchanged: a leg cannot queue work."""
    if not (Path(board) / ".git").exists():
        return True                                  # a plain folder (a test board): nothing to check against
    rel = p.relative_to(board).as_posix()
    tracked = gitio.git_raw(["ls-files", "--error-unmatch", rel], board).returncode == 0
    return tracked and not gitio.git(["status", "--porcelain", "--", rel], board)


def next(ctx):
    root = Path(ctx["board"]) / "relay"
    for p in sorted((root / "queue").glob("*.json")) if (root / "queue").is_dir() else []:
        u = load(p)
        if u["id"] in ctx["skip"] or (root / "done" / (u["id"] + ".json")).exists():
            continue
        if not trusted(ctx["board"], p):
            print("skipping %s: it is not committed on the board, so nobody but a leg may have written it" % p.name)
            continue
        return dict(u, source=NAME)
    return None


def refresh(unit, ctx):
    """Once the checkout is on the lane: read the last leg's note and score its predictions there."""
    note = Path(ctx["board"]) / "relay" / "notes" / (unit["id"] + ".md")
    text = note.read_text(encoding="utf-8") if note.exists() else ""
    return dict(unit, note=text, predictions=papers.run_predictions(text, ctx["work"]) if text else [])


def done_when(unit, ctx):
    try:
        r = subprocess.run(unit["done_when"], cwd=str(ctx["work"]), capture_output=True,
                           timeout=config.limits()["done_when_seconds"])
        return None if r.returncode == 0 else "done_when exited %d: %s" % (
            r.returncode, (r.stdout + r.stderr).decode("utf-8", "replace").strip()[-160:])
    except (OSError, subprocess.TimeoutExpired) as e:
        return "done_when could not run: %s" % e


def already_done(unit, ctx):
    """Asked before any leg: work that is already there and pushed costs no session."""
    return done_when(unit, ctx) is None and not gitio.dirty(ctx["work"]) and gitio.pushed(ctx["work"], unit["lane"])


def body(unit):
    lines = ["# Lane work: %s" % unit["id"],
             "Goal, in the owner's words: %s" % unit["goal"],
             "Done when this exits 0 in the checkout: `%s`" % " ".join(unit["done_when"]),
             "- Commit with the edit gate green for that exact tree, and push the lane.",
             "- If you cannot finish, write note.md in your leg folder for the next leg (sections: Goal, Done, "
             "In flight, Next, Predictions, Dead ends). A prediction is `command` -> exit N, with a look-only command."]
    if unit.get("note"):
        lines += ["", "# Note from the last leg on this unit", unit["note"].strip()]
        lines += ["", "Its predictions, checked just now by script:"]
        lines += ["- %s: %s" % ("held" if hit else "DID NOT HOLD", cmd) for cmd, hit in unit.get("predictions", [])]
    return "\n".join(lines)


def verify(unit, ctx):
    out = [p for p in [done_when(unit, ctx)] if p]
    if not gitio.dirty(ctx["work"]) and not gitio.pushed(ctx["work"], unit["lane"]):
        out.append("the lane is not pushed")
    return out


def claim(unit):
    pass


def release():
    pass


def keep_note(unit, ctx, desk, lim):
    """A good note goes to the board for the next run; a bad one is dropped, so a false note is never handed on."""
    f = Path(desk) / "note.md"
    dst = Path(ctx["board"]) / "relay" / "notes" / (unit["id"] + ".md")
    if f.exists() and not papers.check_note(f.read_text(encoding="utf-8"), lim["note_max_bytes"]):
        dst.parent.mkdir(parents=True, exist_ok=True)
        dst.write_text(f.read_text(encoding="utf-8"), encoding="utf-8", newline="\n")
    elif dst.exists():
        dst.unlink()                                 # the old note describes a state that is gone


def finish(unit, ctx, problems, outcome):
    v = verdict(problems, outcome)
    if v == "PASS":
        root = Path(ctx["board"]) / "relay"
        write_json(root / "done" / (unit["id"] + ".json"),
                   {"id": unit["id"], "lane": unit["lane"], "head": gitio.head(ctx["work"]), "done_at": now()})
        note = root / "notes" / (unit["id"] + ".md")
        if note.exists():
            note.unlink()
    return v
