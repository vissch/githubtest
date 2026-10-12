"""Source: free lane work the owner queued on the board, one file per unit:

  <board>/relay/queue/<id>.json   {"id" (= the file name), "lane", "role", "goal" (the owner's words),
                                   "done_when": ["program", "arg", ...]  (run in the checkout; exit 0 = done),
                                   "priority": 0 to 99, optional (relay.py prio): the lower runs first,
                                   "kind": finish | fix | critique, optional: the share of the day it is paid from
                                   (sources.kind_of; left out, its role says: the review's fixes are fix)}
  <board>/relay/done/<id>.json    written by the runner when done_when passes; a unit with one is never picked again
  <board>/relay/notes/<id>.md     the last leg's handoff note, when the unit is not done yet
  <board>/relay/tried/<id>.json    written by the runner for a unit that did not pass: {"verdict" (FAIL | BLOCKED),
                                   "run", "at", "problems", "tries" (in a row, for the same unit),
                                   "unit_hash" (of lane, role, goal and done_when: never the priority)},
                                   and for a leg the runner did not trust "rejected": {"run", "leg", "why", "head"},
                                   and for uncommitted work the runner put aside "saved": {"ref", "patch", "head",
                                   "run", "cleaned"}: a commit in the work checkout's repo, and a patch on that machine.
                                   A unit with such a record steps aside for turns of other work and then comes
                                   back by itself, until it is parked (standing): it is taken as new again when
                                   its queue file changes or `relay.py retry <id>` removes the record. Gone at a PASS
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
STEPS_ASIDE = True                  # this source keeps a record of a try, so the runner need not skip the unit


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


def queued_at(board):
    """{unit id: when its queue file came onto the board, in seconds}: from the board's history, one call (the
    newest commit that added the file), and from the file's own time on a plain folder or for a file the history
    does not name. A wide turn takes the unit that has waited longest by it."""
    root, out = Path(board) / "relay" / "queue", {}
    if (Path(board) / ".git").exists():
        stamp = 0
        for line in gitio.git(["log", "--diff-filter=A", "--name-only", "--format=@%ct", "--", "relay/queue"],
                              board, check=False).splitlines():
            if line.startswith("@"):
                stamp = int(line[1:]) if line[1:].isdigit() else 0
            elif line.endswith(".json"):
                out.setdefault(Path(line).stem, stamp)       # newest first: the first seen is the last time added
    for p in root.glob("*.json") if root.is_dir() else []:
        if p.stem not in out:
            out[p.stem] = p.stat().st_mtime
    return out


def tries_of(rec):
    return rec["tries"] if isinstance(rec.get("tries"), int) and not isinstance(rec.get("tries"), bool) else 1


def turns_left(rec, uid, lim, turns):
    """How many turns of other work a unit that did not pass still stands aside for: limits.json aside_turns after
    its first try, twice that after each further one in a row (3, 6, 12), less the turns taken since its own last
    one. Counted on the turns (ledger.turns); a record with no turn of its own behind it counts those begun after
    it was written."""
    need = int(lim["aside_turns"]) * 2 ** (tries_of(rec) - 1)
    mine = [i for i, t in enumerate(turns) if t.get("unit") == uid]
    since = len(turns) - 1 - mine[-1] if mine else \
        sum(1 for t in turns if str(t.get("started_at") or "") > str(rec.get("at") or ""))
    return max(0, need - since)


def standing(rec, unit, lim=None, turns=()):
    """How a unit that did not pass stands with the runner: '' it is taken as any other; 'aside' it is stepping
    aside for turns of other work (turns_left) and then comes back by itself; 'parked' it is out of the queue until
    a person looks: limits.json park_after tries in a row, or park_after_blocked of them when the last one ended
    BLOCKED (a leg that says it cannot go on is believed sooner than one that only failed). A record counts while
    the unit asks for the same thing (unit_hash); a changed unit file, `relay.py retry` or a PASS ends it. Not a
    push to its lane: forty units share one lane, and each push of one reopened them all.
    Before this a unit that did not pass was first again in every run, or was hidden for good."""
    if not rec or rec.get("verdict") not in ("FAIL", "BLOCKED") or rec.get("unit_hash") != unit_hash(unit):
        return ""
    lim = lim or config.limits()
    tries = tries_of(rec)
    if tries >= lim["park_after"] or (rec["verdict"] == "BLOCKED" and tries >= lim["park_after_blocked"]):
        return "parked"
    return "aside" if turns_left(rec, unit["id"], lim, turns) else ""


def read_queue(ctx):
    """files() of the board, read once for a pick and kept in ctx["queue"] with the units as they are loaded: a
    pick walks the queue once for every kind of work and again for a wide turn, and asking git about each of a
    hundred files every time took a minute a pick. The runner empties it before each pick (runner.pick_state); a
    list of the next turns (sources.forecast) reads the queue once for all of them."""
    kept = ctx.get("queue")
    if not isinstance(kept, dict):
        kept = ctx["queue"] = {}
    if "files" not in kept:
        kept["files"], kept["units"] = files(ctx["board"]), {}
    return kept["files"]


def walk(ctx, kind=None, aside=False, whole=False):
    """The queued units the runner may take now, in the queue's order, one at a time (a file that cannot be read
    stops it only when its turn comes; with `whole` it is left out, for a caller that looks at the whole queue).
    With `kind`, of that kind of work (sources.kind_of). With aside: only the units that are stepping aside and
    that this run did not try itself, as (when the record was written, unit)."""
    from sources import kind_of, limits_of, turns_of
    root = Path(ctx["board"]) / "relay"
    lim, turns, queued = limits_of(ctx), turns_of(ctx), ctx.get("queued") or {}
    for p in read_queue(ctx)[0]:
        u = ctx["queue"]["units"].get(p)
        if u is None:
            try:
                u = load(p)
            except SystemExit as e:
                u = e
            ctx["queue"]["units"][p] = u
        if isinstance(u, SystemExit):
            if whole:
                continue
            raise u
        if u["id"] in ctx["skip"] or (root / "done" / (u["id"] + ".json")).exists():
            continue
        if kind and kind_of(NAME, u["role"], u.get("kind")) != kind:
            continue
        rec = tried(ctx["board"], u["id"])
        how = standing(rec, u, lim, turns)
        if aside != (how == "aside") or how == "parked" or (aside and rec.get("run") == ctx.get("run")):
            continue
        unit = dict(u, source=NAME, why="stood aside" if aside else "priority %d" % priority_of(u, lim),
                    queued=queued.get(u["id"]) or 0)
        yield (str(rec.get("at") or ""), unit) if aside else unit


def waiting(ctx, kind=None, aside=False):
    """Every unit walk() gives, as a list: for a wide turn, which looks at the whole queue. A file that cannot be
    read is left out here: it stops the queue when its own turn comes, not somebody else's. With aside: the units
    stepping aside, the one that has stood longest first."""
    out = list(walk(ctx, kind, aside, whole=True))
    return [u for _, u in sorted(out, key=lambda x: x[0])] if aside else out


def next(ctx, kind=None):
    """The first queued unit the runner may take, or with `kind` the first of that kind of work."""
    for p in read_queue(ctx)[1]:
        print("skipping %s: it is not committed on the board, so nobody but a leg may have written it" % p.name)
    for unit in walk(ctx, kind):
        return unit
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
                      "run": ctx.get("run", ""), "at": now(), "cleaned": bool(rec.get("cleaned")),
                      "touched": bool(rec.get("touched")), "staged_ref": rec.get("staged_ref") or ""})
    write_json(tried_path(ctx["board"], unit["id"]), old)


def reject(unit, ctx, why, leg=0, counts=False):
    """A leg of this unit could not be trusted and the run stops with no verdict: keep the head it left, so that
    the next run does not take what it pushed for work that is done. Written by the runner, after the leg.
    counts: the leg itself went wrong (it timed out, broke a rule, ran unguarded), so this is a try of the unit
    that did not pass and the unit steps aside for it; not so when the owner, the budget or the run's hours cut
    the leg. One in five turns ended this way with no verdict, and its unit was first again in the next run."""
    rec = tried(ctx["board"], unit["id"])
    if counts:
        rec.update(try_record(unit, ctx, rec, "FAIL", ["its leg %02d was not trusted: %s" % (leg, why)]))
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
        how = ("the checkout was put back to its HEAD" if kept.get("cleaned") else
               "the checkout was put back only in part then: read `git status` before anything else"
               if kept.get("touched") else "the checkout was left as it was then")
        lines.append("- An earlier leg on this unit left uncommitted work (run %s). The runner put it aside as the "
                     "commit `%s`, made on %s, and %s. Look at it with `git show "
                     "--stat %s`; an execute leg puts it back uncommitted with `git cherry-pick -n %s`. Take it up or "
                     "leave it, and say which in your report."
                     % (kept.get("run") or "?", kept["ref"], str(kept.get("head"))[:10], how, kept["ref"], kept["ref"])
                     + (" What was staged then differed from the files, and is the commit `%s`." % kept["staged_ref"]
                        if kept.get("staged_ref") else ""))
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
    old = tried(ctx["board"], unit["id"])
    rec = dict(try_record(unit, ctx, old, v, problems), id=unit["id"], lane=unit["lane"])
    if old.get("saved"):
        rec["saved"] = old["saved"]                  # work the runner put aside stays named until the unit passes
    write_json(rec_file, rec)
    return v


def try_record(unit, ctx, old, v, problems):
    """What a try that did not pass leaves on the unit's record. tries counts the tries in a row for the same unit
    (unit_hash), whatever was pushed in between: a leg cannot buy its unit turns by pushing something."""
    same = unit_hash(unit)
    again = old.get("verdict") and old.get("unit_hash") == same
    return {"verdict": v, "run": ctx.get("run", ""), "at": now(), "problems": [str(p)[:300] for p in problems[:8]],
            "unit_hash": same, "tries": (tries_of(old) if again else 0) + 1}
