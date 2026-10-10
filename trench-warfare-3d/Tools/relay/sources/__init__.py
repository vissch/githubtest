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

ctx: board (Path), work (Path, the work checkout), station, skip (unit ids this run already failed), since (the
time the unit started: evidence older than that is not this unit's).
Adding a source is adding a file here and its name to ORDER.
"""
import importlib

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


def next_unit(names, ctx):
    """The next unit. With shares in ctx (limits.json share_*): of the kind of work that is furthest under its
    share today; a kind with nothing to do lends its room to the next, so the day's tokens are never left idle for
    want of one kind. Without shares: the first source that has a unit, as before."""
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


def verdict(problems, outcome):
    return "BLOCKED" if outcome == "blocked" else "FAIL" if problems or outcome != "done" else "PASS"
