"""Source: free lane work the owner queued on the board, one file per unit:

  <board>/relay/queue/<id>.json   {"id" (= the file name), "lane", "role", "goal" (the owner's words),
                                   "done_when": ["program", "arg", ...]  (run in the checkout; exit 0 = done),
                                   "priority": 0 to 99, optional (relay.py prio): the lower runs first,
                                   "kind": finish | fix | critique, optional: the share of the day it is paid from
                                   (sources.kind_of; left out, its role says: the review's fixes are fix)}
  <board>/relay/done/<id>.json    written by the runner when done_when passes; a unit with one is never picked again
  <board>/relay/notes/<id>.md     the last leg's handoff note, when the unit is not done yet
The queue runs by priority, then by name. A unit that names no priority has limits.json queue_priority.
The role is a name in Tools/pipeline/roles.json (relay.py role): the unit's legs get that role's brief, as a board
job's do. "lane" is the role with none.
A unit that is not done leaves a handoff note (sources/notes.py): its state is not on the board or in a ledger. The
note reaches the next run's legs on the card, with each of its predictions scored hit or miss by script.
"""
import datetime, re, subprocess, sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "pipeline"))
from pipeline import now, read_json, write_json   # noqa: E402

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import config, gitio                              # noqa: E402
from sources import briefs, notes, verdict        # noqa: E402
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


def held_elsewhere(ctx):
    """{unit id: run} from the board's live records (boardio.live_record): what another run says it is on right
    now. A unit another run has a leg open on gets no second leg (the owner, 2026-10-10). A record whose beat is
    older than two legs' turns is stale: a run that was killed leaves its file behind, and it must not stall the
    queue."""
    stale = datetime.datetime.now(datetime.timezone.utc) - datetime.timedelta(
        minutes=2 * config.limits()["leg_minutes"])
    out = {}
    for f in sorted(Path(ctx["board"]).glob("relay/*/live/*.json")):
        try:
            rec = read_json(f)
            beat = datetime.datetime.strptime(rec["beat"], "%Y-%m-%dT%H:%M:%SZ").replace(
                tzinfo=datetime.timezone.utc)
        except (OSError, ValueError, KeyError, TypeError):
            continue
        if beat < stale or rec.get("run") == ctx.get("run"):
            continue
        on = rec.get("now_on") or {}
        if isinstance(on, dict) and on.get("unit"):
            out[on["unit"]] = rec.get("run") or f.stem
    return out


def next(ctx, kind=None):
    """The first queued unit, or with `kind` the first of that kind of work (sources.kind_of)."""
    from sources import kind_of
    root = Path(ctx["board"]) / "relay"
    held = held_elsewhere(ctx)
    ok, skipped = files(ctx["board"])
    for p in skipped:
        print("skipping %s: it is not committed on the board, so nobody but a leg may have written it" % p.name)
    for p in ok:
        u = load(p)
        if u["id"] in ctx["skip"] or (root / "done" / (u["id"] + ".json")).exists():
            continue
        if kind and kind_of(NAME, u["role"], u.get("kind")) != kind:
            continue
        if u["id"] in held:
            print("skipping %s: run %s has a leg open on it" % (u["id"], held[u["id"]]))
            continue
        return dict(u, source=NAME)
    return None


def refresh(unit, ctx):
    """Once the checkout is on the lane: read the last leg's note and score its predictions there."""
    return dict(unit, **notes.read(ctx, unit["id"]))


def done_when(unit, ctx):
    try:
        r = subprocess.run(unit["done_when"], cwd=str(ctx["work"]), capture_output=True,
                           timeout=config.limits()["done_when_seconds"])
        return None if r.returncode == 0 else "done_when exited %d: %s" % (
            r.returncode, (r.stdout + r.stderr).decode("utf-8", "replace").strip()[-160:])
    except (OSError, subprocess.TimeoutExpired) as e:
        return "done_when could not run: %s" % e


def on_origin(unit, ctx):
    """Put the work checkout on origin's lane before done_when is measured, so what is measured is what is pushed
    (the owner, 2026-10-10). None when it is there; else why it is not, in one line for the log. A dirty tree is
    never touched: its done_when runs on the checkout as it stands."""
    work, lane_name = ctx["work"], unit["lane"]
    try:
        sha = gitio.remote_head(work, lane_name)
        if not sha:
            return "origin has no %s yet: done_when runs on the checkout as it is" % lane_name
        if gitio.head(work) == sha:
            return None
        if gitio.dirty(work):
            return "the checkout is dirty: done_when runs on it as it is, not on origin/%s" % lane_name
        gitio.git(["reset", "-q", "--hard", sha], work)
    except gitio.GitError as e:
        return "the checkout could not be put on origin/%s (%s): done_when runs on it as it is" % (lane_name, e)
    return None


def already_done(unit, ctx):
    """Asked before any leg: work that is already there and pushed costs no session. Measured on origin/<lane>."""
    say = on_origin(unit, ctx)
    if say:
        print("unit %s: %s" % (unit["id"], say))
    return done_when(unit, ctx) is None and not gitio.dirty(ctx["work"]) and gitio.pushed(ctx["work"], unit["lane"])


def body(unit):
    lines = ["# Lane work: %s" % unit["id"],
             "Goal, in the owner's words: %s" % unit["goal"],
             "Done when this exits 0 in the checkout: `%s`" % " ".join(unit["done_when"]),
             "- Commit with the edit gate green for that exact tree, and push the lane.", notes.ASK]
    if briefs.card_line(unit["role"]):
        lines.append(briefs.card_line(unit["role"]))
    elif unit["role"] not in briefs.P.roles():       # a queue file from before the table, or written by hand
        lines.append("- There is no brief for the role `%s`: it is not in Tools/pipeline/roles.json." % unit["role"])
    return "\n".join(lines + notes.card(unit))


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
    notes.keep(ctx, unit["id"], desk, lim)


def critic(unit, ctx, round_no):
    return None                                      # lane work leaves no evidence bundle to score


def keep_critic(unit, ctx, round_no, text):
    pass


def finish(unit, ctx, problems, outcome):
    v = verdict(problems, outcome)
    if v == "PASS":
        root = Path(ctx["board"]) / "relay"
        write_json(root / "done" / (unit["id"] + ".json"),
                   {"id": unit["id"], "lane": unit["lane"], "head": gitio.head(ctx["work"]), "done_at": now()})
        notes.drop(ctx, unit["id"])
    return v
