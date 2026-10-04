"""Work sources. Each module is one source with the same functions, so the runner never special-cases one:

  next(ctx)                         the next unit as a dict (id, source, role, lane, ...), or None
  refresh(unit, ctx)                the unit again once the checkout is on its lane (its id may depend on that)
  already_done(unit, ctx)           True when a script can see the unit needs no leg at all
  claim(unit) / release()           take the unit before the first leg; give it back when a run stops mid-unit
  body(unit)                        the text for the leg card: what this unit is
  verify(unit, ctx)                 problems as short strings, checked by script after the execute legs (empty = PASS)
  keep_note(unit, ctx, desk, lim)   keep a leg's handoff note for the next run, if this source uses notes
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


def next_unit(names, ctx):
    for name in names:
        unit = load(name).next(ctx)
        if unit:
            return unit
    return None


def verdict(problems, outcome):
    return "BLOCKED" if outcome == "blocked" else "FAIL" if problems or outcome != "done" else "PASS"
