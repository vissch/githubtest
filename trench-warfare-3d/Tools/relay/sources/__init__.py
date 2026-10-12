"""Work sources. Each module is one source with the same functions, so the runner never special-cases one:

  next(ctx)                         the next unit as a dict (id, source, role, lane, ...), or None
  refresh(unit, ctx)                the unit again once the checkout is on its lane (its id may depend on that)
  already_done(unit, ctx)           True when a script can see the unit needs no leg at all
  claim(unit) / release()           take the unit before the first leg; give it back when a run stops mid-unit
  body(unit)                        the text for the leg card: what this unit is
  verify(unit, ctx)                 problems as short strings, checked by script after the execute legs (empty = PASS)
  keep_note(unit, ctx, desk, lim)   keep a leg's handoff note for the next run, if this source uses notes
  critic(unit, ctx, round_no)       None when the source has nothing a critic can score; else {"body": the card
                                    text, "fill": a function that copies the evidence bundle into a folder}
  keep_critic(unit, ctx, round_no, text)   keep a critic round where this source keeps its evidence
  finish(unit, ctx, problems, outcome)   record PASS | FAIL | BLOCKED where this source keeps its state; outcome is
                                    what the last leg reported (done | blocked | failed)
A source may also have these; the runner and the pick ask for them by name and go on without:
  reject(unit, ctx, why, leg, counts)   a leg of the unit could not be trusted and the run stops with no verdict;
                                    counts: it is a try of the unit that did not pass (lane)
  saved(unit, ctx, rec, patch)      the runner put the unit's uncommitted work aside (gitio.snapshot_dirty's record)
  waiting(ctx, kind, aside=False)   every unit the source would give now, in its order: what a wide turn chooses
                                    from; with aside, the units that are stepping aside (lane, pipeline)
  STEPS_ASIDE                       True when the source keeps a record of a try that did not pass, so that the
                                    unit steps aside and comes back by itself; without it the runner skips such
                                    a unit for the rest of the run (lane)

ctx: board (Path), work (Path, the work checkout), station, skip (unit ids this run will not take again), since (the
time the unit started: evidence older than that is not this unit's), run (the run's name), and what the runner
reads before every pick (runner.pick_state): shares, spent_kinds, turns (ledger.turns), queued (when each queue
file came onto the board), lim (the limits). A pick with no turns or lim in ctx reads them itself.
Adding a source is adding a file here and its name to ORDER.
"""
import calendar, importlib, time

ORDER = ("pipeline", "lane")


def load(name):
    if name not in ORDER:
        raise SystemExit("relay: no source named %s (have: %s)" % (name, ", ".join(ORDER)))
    return importlib.import_module("sources." + name)


KINDS = ("finish", "fix", "critique")      # the three kinds of work the day's tokens are split over


def kind_of(source, role, named=None):
    """Which share of the day a unit, or a leg record, counts under (the owner, 2026-10-10: "Finish first"):
      critique   looking at old work to find tasks (the critique source)
      fix        mending what a look found: the review's units, and a found task that says so in its file
      finish     everything else: carrying started work to a landing"""
    if named in KINDS:
        return named
    if source == "critique":
        return "critique"
    return "fix" if role == "review-fix" else "finish"


def order(shares, spent):
    """The kinds with a share, the one furthest under its share of what the day has spent first; a tie goes to the
    larger share. A kind with no share is not in it: nothing of it runs."""
    have = {k: float(shares.get(k) or 0) for k in KINDS}
    total = sum(float(spent.get(k) or 0) for k in KINDS)
    owed = {k: (float(spent.get(k) or 0) / total if total else 0.0) / (have[k] / 100.0) for k in KINDS if have[k] > 0}
    return sorted(owed, key=lambda k: (owed[k], -have[k]))


# The areas of the game a wide turn goes round (the owner, 2026-10-12: "a system that covers the overall state of
# the game", "even the lower priority items should come into light at some point"). A unit's area is read from its
# role; the game's own areas go first, the review's fixes and tool work only when no game area has work: those two
# get the turns by priority as it is.
GAME = ("units", "vehicles", "effects", "fields", "rules", "HUD", "speed", "ideas")
OTHER = ("review fixes", "tools and other")
AREAS = {"character-simulator": "units", "character": "units", "vehicle-simulator": "vehicles",
         "destruction-vfx-simulator": "effects", "env-simulator": "fields", "balance-simulator": "rules",
         "game-designer": "rules", "sim": "rules", "ux": "HUD", "ui-artist": "HUD", "optimizer": "speed",
         "review-fix": "review fixes"}


def area_of(source, role, lane=""):
    """The area a unit, or a leg record, counts under. A board job is an idea on its way; a role the table names
    is that area; work with no such role on a lane/sim lane is the game's rules; the rest is tools and other."""
    if source == "pipeline":
        return "ideas"
    if role in AREAS:
        return AREAS[role]
    return "rules" if str(lane or "").startswith("lane/sim/") else "tools and other"


def turns_of(ctx):
    """The turns the board knows of, oldest first (ledger.turns): the runner reads them once a pick into ctx."""
    if ctx.get("turns") is None:
        import ledger
        return ledger.turns(ctx["board"])
    return ctx["turns"]


def limits_of(ctx):
    if ctx.get("lim") is None:
        import config
        return config.limits()
    return ctx["lim"]


def seconds(stamp):
    """A stamp the relay wrote (2026-10-05T14:25:45Z) as seconds, 0 for anything else."""
    try:
        return calendar.timegm(time.strptime(stamp, "%Y-%m-%dT%H:%M:%SZ"))
    except (TypeError, ValueError):
        return 0


def wide_due(turns, lim):
    """True when the next turn is a wide one: limits.json wide_every says every which turn (0: never), and the
    turns before it, one short of that many, hold no wide one. Counted on the turns themselves, so a turn that cost
    no leg is no turn and the wide one stays owed, and two runs side by side cannot both skip it."""
    n = int(lim.get("wide_every") or 0)
    return n >= 2 and len(turns) >= n - 1 and not any(t.get("turn") == "wide" for t in turns[-(n - 1):])


def wide_pick(names, ctx):
    """The unit of a wide turn, or None when nothing is eligible. Of every eligible unit of every source and of
    every kind that has a share: the area whose last WIDE turn is longest ago (never: first; a game area before the
    others), and in it the unit that has waited longest, counted from when it was queued or from its last turn,
    whichever is later. Not its priority: a unit queued today never passes one that has waited."""
    shares = ctx.get("shares") or {}
    split = any(shares.get(k) for k in KINDS)
    turns, found = turns_of(ctx), []
    for name in names:
        waiting = getattr(load(name), "waiting", None)
        if not waiting:
            continue
        for kind in ([k for k in KINDS if float(shares.get(k) or 0) > 0] if split else [None]):
            found += [dict(u, kind=kind) if kind else u for u in waiting(ctx, kind)]
    if not found:
        return None
    last_wide, last_turn = {}, {}
    for i, t in enumerate(turns):
        last_turn[str(t.get("unit"))] = seconds(t.get("started_at"))
        if t.get("turn") == "wide":
            last_wide[area_of(t.get("source"), t.get("role"), t.get("lane"))] = i
    by_area = {}
    for u in found:
        by_area.setdefault(area_of(u["source"], u["role"], u.get("lane")), []).append(u)
    pool = [a for a in GAME if a in by_area] or [a for a in OTHER if a in by_area]

    def since(u):
        """When the unit's wait began, in seconds: its last turn (a board job: any turn of its item), or the
        moment it was queued, whichever is later. The smaller, the longer it has waited."""
        mine = [s for k, s in last_turn.items() if k == u["id"] or (u.get("item") and k.startswith(u["item"] + "--"))]
        return max(mine + [float(u.get("queued") or 0)])

    # among areas that never had a wide turn: the one whose unit has waited longest; then the order they are named in
    area = min(pool, key=lambda a: (last_wide.get(a, -1), min(since(u) for u in by_area[a])))
    unit = min(by_area[area], key=since)                             # a tie: the source's own order
    ago = "no wide turn yet" if area not in last_wide else "its last wide turn was %d turns ago" % (len(turns) - last_wide[area])
    return dict(unit, turn="wide", why="wide: %s" % area, why_long="a wide turn for %s (%s)" % (area, ago))


def priority_pick(names, ctx):
    """The unit of a turn by priority. With shares in ctx (limits.json share_*): of the kind of work that is
    furthest under its share today; a kind with nothing to do lends its room to the next, so the day's tokens are
    never left idle for want of one kind. Without shares: the first source that has a unit, as before."""
    shares = ctx.get("shares") or {}
    if not any(shares.get(k) for k in KINDS):
        for name in names:
            unit = load(name).next(ctx)
            if unit:
                return unit
        return None
    for kind in order(shares, ctx.get("spent_kinds") or {}):
        for name in names:
            unit = load(name).next(ctx, kind=kind)
            if unit:
                return dict(unit, kind=kind)
    return None


def aside_pick(names, ctx):
    """When nothing else is left: a unit that is stepping aside, the one that has stood longest, and never one this
    run has tried itself. The turns a unit stands aside for are turns of OTHER work; with no other work there are
    none, and the unit would never come back."""
    shares = ctx.get("shares") or {}
    split = any(shares.get(k) for k in KINDS)
    for kind in (order(shares, ctx.get("spent_kinds") or {}) if split else [None]):
        for name in names:
            waiting = getattr(load(name), "waiting", None)
            got = waiting(ctx, kind, aside=True) if waiting else []
            if got:
                return dict(got[0], kind=kind) if kind else got[0]
    return None


def next_unit(names, ctx):
    """The next unit: a wide turn when one is due and has a unit (wide_due, wide_pick), else the turn by priority
    (priority_pick), else a unit that is stepping aside (aside_pick). The unit says which it is: "turn" is "wide"
    on a wide turn, "why" is the few words the screen shows."""
    if wide_due(turns_of(ctx), limits_of(ctx)):
        unit = wide_pick(names, ctx)
        if unit:
            return unit
    return priority_pick(names, ctx) or aside_pick(names, ctx)


def forecast(names, ctx, n, usual=1.0):
    """The next n turns as the runner would take them now. The first is the real pick; each one after it is taken
    as if the turns before it passed, at the usual cost: which kind is owed then hangs on what the legs really
    cost, so the rest is a forecast. Reads only; ctx is not changed."""
    sim = dict(ctx, skip=set(ctx.get("skip") or ()), turns=list(turns_of(ctx)), lim=limits_of(ctx),
               spent_kinds=dict(ctx.get("spent_kinds") or {}))
    out = []
    while len(out) < n:
        unit = next_unit(names, sim)
        if not unit:
            break
        out.append(unit)
        sim["skip"].add(unit["id"])
        sim["turns"].append({"unit": unit["id"], "source": unit["source"], "role": unit["role"],
                             "lane": unit.get("lane"), "turn": unit.get("turn") or "", "started_at": "forecast"})
        if unit.get("kind") and unit.get("turn") != "wide":          # a wide turn is no kind's share (ledger._days)
            sim["spent_kinds"][unit["kind"]] = float(sim["spent_kinds"].get(unit["kind"]) or 0) + usual
    return out


def verdict(problems, outcome):
    return "BLOCKED" if outcome == "blocked" else "FAIL" if problems or outcome != "done" else "PASS"
