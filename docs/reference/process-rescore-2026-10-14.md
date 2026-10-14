# The way of working: the re-score of 2026-10-14 — not scored

What was asked, and by whom. The owner, 2026-10-06, to the brief "A critic that re-scores how we work, on a
schedule?": "One re-score once the two fixes are in; schedule it only if that helps"
(`docs/reference/decisions.md`, the row of 2026-10-06). On 2026-10-04 a one-off critique scored the project's way
of working 31 of 100 and was not saved; two of its three fixes were taken (`docs/reference/decisions.md`, the row
of 2026-10-04).

**The re-score itself was not run.** It had to be made by a fresh agent that has not seen what was expected of it,
and the session that came to do it had no way to start one: the subagent tool was disabled in it. Scoring it by
hand from this session would have been the one thing the ask rules out — the whole point was a judgement made
blind. So this page holds everything around the score and no score: the check that both fixes are in, the rubric as
it was to be given, and what the two fixes changed, measured. The score is still owed.

Base commit: `b7d4d0ede`, which was the tip of the integration branch
`claude/trench-warfare-2d-3d-plan-idt7lf` on 2026-10-14. Every number below is the printed output of the command
named beside it, run on that tree.

## Both fixes are in, and where

**Fix A — one generated owner queue.** First added 2026-10-04:

```
$ git log --diff-filter=A --format='%h %ad %s' --date=short -- trench-warfare-3d/Tools/assetboard/src_queue.py
75d91a127 2026-10-04 tools: the asset board finds the decisions stranded on lanes
```

It lives in `trench-warfare-3d/Tools/assetboard/src_queue.py`, whose own WHY names the critique that asked for it:
"Work was made faster than it landed and the owner could not see what waited on them: 'Needs you' counted the
board's ready stages and nothing else (the process critique of 2026-10-04)." It is read with
`python Tools/assetboard/ops.py --queue`, which is what the site's "Needs you" shows.

**Fix B — verification that enforces itself.** Landed 2026-10-04:

```
$ git log --format='%h %ad %s' --date=short -1 fbd873eff
fbd873eff 2026-10-04 tools: one docs-and-tools check, land.py runs it, and the run before a commit skips the Long tests
```

It lives in `trench-warfare-3d/Tools/toolcheck.py`, run by `trench-warfare-3d/Tools/land.py` itself rather than by
anyone remembering to, plus the Long tier that the run before a commit skips. The enforcement is in land.py's own
docstring, which lists what it refuses:

- the branch is not a lane, or the tree has uncommitted changes;
- HEAD does not contain origin's integration branch;
- origin's copy of the lane holds a commit HEAD does not;
- the lane changes code and the last green full gate did not test exactly this tree (gate.ps1 records the tree it
  tested; a rebase, an amend or one more commit makes a new tree, so the gate runs again). A lane that changes only
  docs is checked with validate.py instead;
- the lane changes a tool and `Tools/toolcheck.py` is not green — "it runs whether or not the lane also changes
  code: a green gate says nothing about the tools, and until 2026-10-04 a lane that changed both landed with no
  tool check at all";
- a SHOW lane's commits change Sim/, Net/ or Data/ without the owner decision that allows it;
- HEAD moved, or the tree went dirty, while the checks ran.

Both files are on the integration branch:

```
$ git ls-tree -r --name-only origin/claude/trench-warfare-2d-3d-plan-idt7lf -- trench-warfare-3d/Tools/assetboard/src_queue.py trench-warfare-3d/Tools/toolcheck.py trench-warfare-3d/Tools/land.py
trench-warfare-3d/Tools/assetboard/src_queue.py
trench-warfare-3d/Tools/land.py
trench-warfare-3d/Tools/toolcheck.py
```

## The rubric, as it was to be given

Five headings of 20 points each, 100 in total, judged by the fresh agent's own standards with no target score
named:

- **A.** Does finished work reach the owner?
- **B.** Can he see in one place what waits on him?
- **C.** Do the checks catch what they claim, with nobody remembering to run them?
- **D.** Is the written record true to the code?
- **E.** Waste: rework, stranded work, time burned.

**The 2026-10-04 rubric is lost.** That critique's own headings and weights were never written down, so when a
score does arrive, only the existence of two totals is comparable — not the areas, not the weights, and not the
difference between the numbers.

## What the two fixes changed, measured

**The owner queue the site shows him** (`python Tools/assetboard/ops.py --queue`), counted by list:

| List | Count |
|---|---|
| open questions with no brief | 2 |
| broken | 21 |
| approved, not landed | 1 |
| gate green, no brief | 3 |
| ready to take | 4 |

Before fix A, "Needs you" counted the board's ready stages and nothing else — that is the 4 in the last row. The
other 27 rows are what the fix added to his one view.

**Decisions stranded on lanes** (`python Tools/assetboard/src_queue.py --stranded`, last line):

```
62 stranded on 16 lanes
```

That is 62 decisions written down only on a lane and not on integration; `--stranded --text` prints each with its
full line, ready to copy. This number is a measure of the problem the fix made visible, not of the fix: nothing
counted it before 2026-10-04, so there is no earlier figure to set beside it.

**The self check** (`python Tools/toolcheck.py`), green, with its run time:

```
toolcheck OK: 20 parts, 270 s
```

Its 20 parts include `python validate.py` and `Tools/selftest.py`; run from this session it took 4 m 30 s of wall
clock.

**What runs on every gate** (`python validate.py`):

```
29 assemblies, 546 C# files
validation OK
```

**The Long tier** that the run before a commit skips:

```
$ grep -rho 'Category("Long")' Assets/_Project | wc -l
47
$ grep -rl 'Category("Long")' Assets/_Project | wc -l
20
```

47 marks in 20 files.

**What this lane's gate will run** (`python Tools/gate_scope.py`): `scoped: no`, "every module: nothing differs
from the merge-base b7d4d0edeb" — this lane changes only docs, so the scope narrowing had nothing to narrow.

**Numbers that cannot be had.** The board's own read of the queue needs the Drive, which this session has no access
to, so only the locally derived lists above are counted. And the 2026-10-04 critique left no measurements at all,
so no before-number exists for anything on this page.

**One honest line about the other 2026-10-04 decision.** The CI witness it called for is not in place:
`.github/workflows/` holds only `unity.yml`, and `git branch -r` has no `ci-checks` lane on origin.

## No schedule follows

The owner said "schedule it only if that helps". Nothing on this page is scheduled and no standing critic was
built: no new tool, no cron, no recurring job. Whether to schedule anything is a question for after a score exists,
and there is no score yet.
