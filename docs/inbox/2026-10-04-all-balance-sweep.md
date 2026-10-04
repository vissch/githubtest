# The balance sweep, and two harness signatures that grew (2026-10-04)

From `lane/show/balance-sweep` (the owner, 2026-10-04: build balance sweeps on what the repo has, no new framework).

**It sits on `lane/show/nightly-v2`** (it needs `Tools/abtest.py` and the behaviour bench, which are not on the
integration branch yet) and lands after it. Once nightly-v2 has landed it is rebased onto the integration branch.

What to know if you touch the same files:

- `MatchLoopTests.Play` has two more optional parameters at the end, `config` and `built`, and `AssaultLadderTests.Run`
  one, `built`. Unused, both play exactly what they played (the behaviour bench's report is byte-identical with and
  without the change, three seeds). A lane that adds a parameter to either will conflict on that one line: keep both.
- `BalanceSweepTests` is a new EditMode class beside them. `Report_TheSweep` is Explicit; its two plain tests climb
  three rungs of the ladder.

What it is for: `python Tools/sweep.py run Tools/sweeps/factions.json` (from `trench-warfare-3d/`, this checkout's
editor closed) plays variants of a number over the same seeds and reports attrition, time to breach, trench retention
and who wins from which seat. A variant is data, so a sweep commits nothing. `workflow.md`, section 5.

Delete this note when you have read it and it no longer concerns your lane.
