"""Source: free lane work the owner queued on the board, one file per unit:

  <board>/relay/queue/<id>.json   {"id" (= the file name), "lane", "role", "goal" (the owner's words),
                                   "done_when": ["program", "arg", ...]  (run in the checkout; exit 0 = done),
                                   "priority": 0 to 99, optional (relay.py prio): the lower runs first,
                                   "kind": finish | fix | critique, optional: the share of the day it is paid from
                                   (sources.kind_of; left out, its role says: the review's fixes are fix)}
  <board>/relay/done/<id>.json    written by the runner when done_when passes; a unit with one is never picked again
  <board>/relay/notes/<id>.md     the last leg's handoff note, when the unit is not done yet
  <board>/relay/tried/<id>.json   written by the runner for a unit that did not pass: {"verdict" (FAIL | BLOCKED),
                                   "run", "at", "problems", "tries" (in a row with no new code on origin's lane),
                                   "origin_head", "unit_hash" (of lane, role, goal and done_when: never the priority)},
                                   and for a leg the runner did not trust "rejected": {"run", "leg", "why", "head"},
                                   and for uncommitted work the runner put aside "saved": {"ref", "patch", "head",
                                   "run", "cleaned"}: a commit in the work checkout's repo, and a patch on that machine.
                                   A unit recorded BLOCKED, or FAIL limits.json no_progress_units times, is passed
                                   over while nothing has moved: taken again when origin's lane has a new head, the
                                   queue file changes, or `relay.py retry <id>` removes the record. Gone at a PASS
The queue runs by priority, then by name. A unit that names no priority has limits.json queue_priority.
The role is a name in Tools/pipeline/roles.json (relay.py role): the unit's legs get that role's brief, as a board
job's do. "lane" is the role with none.
This is the only source with a handoff note: its state is not on the board or in a ledger. The note reaches the next
run's legs on the card, with each of its predictions scored hit or miss by script.
"""
import hashlib, json, re, subprocess, sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "pipeline"))
from pipeline import now, read_json, write_json   # noqa: E402

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import config, gitio, papers                      # noqa: E402
from sources import briefs, verdict               # noqa: E402
from sources.briefs import place_brief            # noqa: E402,F401  (the runner asks a source for it)

NAME = "lane"
NEED = ("id", "lane", "role", "goal", "done_when")
MAY = ("kind",)                     # what a unit file may also say: the kind of work it is paid as. Never its
#                                     priority: a unit's place in the queue is the owner's to give (relay.py prio)
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
    if u.get("kind") is not None and u["kind"] not in ("finish", "fix", "critique"):
        raise SystemExit("relay: %s: kind is finish, fix or critique: the share of the day the unit is paid from" % p.name)
    pr, top = u.get("priority"), config.limits()["queue_priority_max"]
    if pr is not None and (not isinstance(pr, int) or isinstance(pr, bool) or not 0 <= pr <= top):
        raise SystemExit("relay: %s: priority must be a whole number from 0 to %d (the lower runs first)" % (p.name, top))
    return u


def priority_of(u, lim=None):
    """A unit's place in the queue: the lower runs first. A unit that names none has limits.json queue_priority."""
    pr = u.get("priority") if isinstance(u, dict) else None
    return pr if isinstance(pr, int) and not isinstance(pr, bool) else (lim or config.limits())["queue_priority"]


def trusted(board, p):
    """A queue file counts only when it is committed on the board and unchanged: a leg cannot queue work."""
    if not (Path(board) / ".git").exists():
        return True                                  # a plain folder (a test board): nothing to check against
    rel = p.relative_to(board).as_posix()
    tracked = gitio.git_raw(["ls-files", "--error-unmatch", rel], board).returncode == 0
    return tracked and not gitio.git(["status", "--porcelain", "--", rel], board)


def files(board):
    """(the queue files the runner may take, in its order: priority, then name; the files it skips because nobody
    committed them). A file that cannot be read sorts as one with no priority: load() says what is wrong with it
    when its turn comes, as before."""
    root, ok, skipped, lim = Path(board) / "relay" / "queue", [], [], config.limits()
    for p in sorted(root.glob("*.json")) if root.is_dir() else []:
        (ok if trusted(board, p) else skipped).append(p)    # asked first: a file nobody committed is not even read

    def place(p):
        try:
            u = read_json(p)
        except (OSError, ValueError):
            u = None
        return priority_of(u, lim), p.name
    return sorted(ok, key=place), skipped


def unit_hash(u):
    """What a unit asks for, as a short hash: its lane, role, goal and done_when. Not its priority or its kind: a
    move in the queue is no new unit."""
    return hashlib.sha256(json.dumps([u.get(k) for k in ("lane", "role", "goal", "done_when")]).encode("utf-8")
                          ).hexdigest()[:16]


def tried_path(board, uid):
    return Path(board) / "relay" / "tried" / (uid + ".json")


def tried(board, uid):
    """What the board keeps of a unit that did not pass, or {}."""
    p = tried_path(board, uid)
    try:
        rec = read_json(p) if p.exists() else None
    except (OSError, ValueError):
        rec = None
    return rec if isinstance(rec, dict) else {}


def origin_heads(work):
    """{lane: sha} of origin's branches, asked of origin itself; None when it cannot be asked (no checkout, no
    network): a caller then holds nothing back on a head it could not read."""
    if not work:
        return None
    try:
        r = gitio.git_raw(["ls-remote", "--heads", "origin"], work)
    except OSError:
        return None
    if r.returncode:
        return None
    rows = [l.split("\t") for l in r.stdout.decode("utf-8", "replace").splitlines() if "\t" in l]
    return {ref.replace("refs/heads/", "", 1): sha for sha, ref in rows}


def waits(rec, unit, lim=None, head=None):
    """True for a unit the runner passes over: its record says BLOCKED, or FAIL limits.json no_progress_units times
    in a row with nothing new pushed, and nothing has moved since: the unit asks for the same thing and, when `head`
    is given (the head of its lane on origin now, '' for none), that is the head the record was written at.
    Every run took the same failed units at the front of the queue again, a plan leg each, and the rule that stops
    a run after two empty units then fired before any later work (the review, R002.2)."""
    if not rec or rec.get("verdict") not in ("FAIL", "BLOCKED") or rec.get("unit_hash") != unit_hash(unit):
        return False
    if head is not None and rec.get("origin_head") != head:
        return False
    tries = rec.get("tries") if isinstance(rec.get("tries"), int) else 0
    return rec["verdict"] == "BLOCKED" or tries >= (lim or config.limits())["no_progress_units"]


def next(ctx, kind=None):
    """The first queued unit, or with `kind` the first of that kind of work (sources.kind_of). A unit that waits()
    is passed over, and said once a run."""
    from sources import kind_of
    root = Path(ctx["board"]) / "relay"
    ok, skipped = files(ctx["board"])
    for p in skipped:
        print("skipping %s: it is not committed on the board, so nobody but a leg may have written it" % p.name)
    heads, lim = False, None                         # origin is asked once, and only when a record is there
    for p in ok:
        u = load(p)
        if u["id"] in ctx["skip"] or (root / "done" / (u["id"] + ".json")).exists():
            continue
        if kind and kind_of(NAME, u["role"], u.get("kind")) != kind:
            continue
        rec = tried(ctx["board"], u["id"])
        if rec.get("verdict"):
            if heads is False:
                heads, lim = origin_heads(ctx.get("work")), config.limits()
            if heads is not None and waits(rec, u, lim, heads.get(u["lane"], "")):
                if u["id"] not in ctx.setdefault("passed_over", set()):
                    ctx["passed_over"].add(u["id"])
                    print("passing over %s: it ended %s in run %s%s and nothing has moved since. It is taken again "
                          "when its lane has new code on origin, its queue file changes, or after `relay.py retry %s`"
                          % (u["id"], rec["verdict"], rec.get("run") or "?",
                             "" if rec["verdict"] == "BLOCKED" else ", %d tries with nothing pushed" % rec["tries"],
                             u["id"]), flush=True)
                continue
        return dict(u, source=NAME)
    return None


def refresh(unit, ctx):
    """Once the checkout is on the lane: read the last leg's note and score its predictions there."""
    note = Path(ctx["board"]) / "relay" / "notes" / (unit["id"] + ".md")
    text = note.read_text(encoding="utf-8") if note.exists() else ""
    rej = tried(ctx["board"], unit["id"]).get("rejected") or {}
    if not (rej.get("head") and rej["head"] == gitio.head(ctx["work"])):
        rej = {}                                     # the lane has moved since: that leg's work is not what is here
    kept = tried(ctx["board"], unit["id"]).get("saved") or {}
    if not (kept.get("ref") and gitio.git(["rev-parse", "-q", "--verify", kept["ref"] + "^{commit}"], ctx["work"],
                                          check=False)):
        kept = {}                                    # saved on another machine, or the ref is gone: nothing to name
    return dict(unit, note=text, predictions=papers.run_predictions(text, ctx["work"]) if text else [], rejected=rej,
                saved=kept)


def done_when(unit, ctx):
    try:
        r = subprocess.run(unit["done_when"], cwd=str(ctx["work"]), capture_output=True,
                           timeout=config.limits()["done_when_seconds"])
        return None if r.returncode == 0 else "done_when exited %d: %s" % (
            r.returncode, (r.stdout + r.stderr).decode("utf-8", "replace").strip()[-160:])
    except (OSError, subprocess.TimeoutExpired) as e:
        return "done_when could not run: %s" % e


def already_done(unit, ctx):
    """Asked before any leg: work that is already there and pushed costs no session. Never for a head that a leg
    the runner did not trust left behind: that work gets a guarded plan and execute first (the review, R002.1)."""
    rej = tried(ctx["board"], unit["id"]).get("rejected") or {}
    if rej.get("head") and rej["head"] == gitio.head(ctx["work"]):
        return False
    return done_when(unit, ctx) is None and not gitio.dirty(ctx["work"]) and gitio.pushed(ctx["work"], unit["lane"])


def saved(unit, ctx, rec, patch):
    """The runner put a leg's uncommitted work aside (gitio.snapshot_dirty): keep where it is, for the unit's next
    card. rec is the snapshot's record."""
    old = tried(ctx["board"], unit["id"])
    old.update(id=unit["id"], lane=unit["lane"],
               saved={"ref": rec.get("ref") or "", "patch": str(patch), "head": rec.get("head") or "",
                      "run": ctx.get("run", ""), "at": now(), "cleaned": bool(rec.get("cleaned"))})
    write_json(tried_path(ctx["board"], unit["id"]), old)


def reject(unit, ctx, why, leg=0):
    """A leg of this unit could not be trusted and the run stops with no verdict: keep the head it left, so that
    the next run does not take what it pushed for work that is done. Written by the runner, after the leg."""
    rec = tried(ctx["board"], unit["id"])
    rec.update(id=unit["id"], lane=unit["lane"],
               rejected={"run": ctx.get("run", ""), "leg": leg, "why": " ".join(str(why).split())[:300],
                         "head": gitio.head(ctx["work"]), "at": now()})
    write_json(tried_path(ctx["board"], unit["id"]), rec)


def body(unit):
    lines = ["# Lane work: %s" % unit["id"],
             "Goal, in the owner's words: %s" % unit["goal"],
             "Done when this exits 0 in the checkout: `%s`" % " ".join(unit["done_when"]),
             "- Commit with the edit gate green for that exact tree, and push the lane.",
             "- If you cannot finish, write note.md in your leg folder for the next leg (sections: Goal, Done, "
             "In flight, Next, Predictions, Dead ends). A prediction is `command` -> exit N, with a look-only command."]
    if briefs.card_line(unit["role"]):
        lines.append(briefs.card_line(unit["role"]))
    elif unit["role"] not in briefs.P.roles():       # a queue file from before the table, or written by hand
        lines.append("- There is no brief for the role `%s`: it is not in Tools/pipeline/roles.json." % unit["role"])
    rej = unit.get("rejected") or {}
    if rej:
        lines.append("- The last leg on this unit (run %s, leg %02d) was not trusted by the runner: %s. The lane is as "
                     "it left it, at %s. Check what it did before you build on it or call it done."
                     % (rej.get("run") or "?", int(rej.get("leg") or 0), rej.get("why") or "?", str(rej["head"])[:10]))
    kept = unit.get("saved") or {}
    if kept:
        lines.append("- An earlier leg on this unit left uncommitted work (run %s). The runner put it aside as the "
                     "commit `%s`, made on %s, and the checkout was put back to its HEAD. Look at it with `git show "
                     "--stat %s`; an execute leg puts it back uncommitted with `git cherry-pick -n %s`. Take it up or "
                     "leave it, and say which in your report."
                     % (kept.get("run") or "?", kept["ref"], str(kept.get("head"))[:10], kept["ref"], kept["ref"]))
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


def critic(unit, ctx, round_no):
    return None                                      # lane work leaves no evidence bundle to score


def keep_critic(unit, ctx, round_no, text):
    pass


def finish(unit, ctx, problems, outcome):
    """PASS: the done file, and the note and the record of earlier tries go. Any other verdict: the record of the
    try (see the top of this file), which next() and the day screen read. The only thing the board kept of a unit
    was PASS, so a failed or blocked unit looked untouched to the next run."""
    v = verdict(problems, outcome)
    root, rec_file = Path(ctx["board"]) / "relay", tried_path(ctx["board"], unit["id"])
    if v == "PASS":
        write_json(root / "done" / (unit["id"] + ".json"),
                   {"id": unit["id"], "lane": unit["lane"], "head": gitio.head(ctx["work"]), "done_at": now()})
        for f in (root / "notes" / (unit["id"] + ".md"), rec_file):
            if f.exists():
                f.unlink()
        return v
    old, heads, same = tried(ctx["board"], unit["id"]), origin_heads(ctx.get("work")), unit_hash(unit)
    head = None if heads is None else heads.get(unit["lane"], "")
    again = old.get("verdict") and old.get("unit_hash") == same and head is not None and old.get("origin_head") == head
    rec = {"id": unit["id"], "lane": unit["lane"], "verdict": v, "run": ctx.get("run", ""), "at": now(),
           "problems": [str(p)[:300] for p in problems[:8]], "origin_head": head, "unit_hash": same,
           "tries": (old["tries"] if again and isinstance(old.get("tries"), int) else 0) + 1}
    if old.get("saved"):
        rec["saved"] = old["saved"]                  # work the runner put aside stays named until the unit passes
    write_json(rec_file, rec)
    return v
