# The way of working: the re-score of 2026-10-14 — scored 48 of 100, on a partial copy

What was asked, and by whom. The owner, 2026-10-06, to the brief "A critic that re-scores how we work, on a
schedule?": "One re-score once the two fixes are in; schedule it only if that helps"
(`docs/reference/decisions.md`, the row of 2026-10-06). On 2026-10-04 a one-off critique scored the project's way
of working 31 of 100 and was not saved; two of its three fixes were taken (`docs/reference/decisions.md`, the row
of 2026-10-04).

**The re-score was run on 2026-10-14 by a fresh agent, and it scored 48 of 100.** The agent was Claude Code
(`C:/Users/PC/.local/bin/claude.exe`, model as its default), started detached from a folder outside this checkout
with `python Tools/pipeline/run_detached.py start rescore --timeout 2400 --cwd <leg folder>/blind --
claude.exe -p "<the rubric prompt>" --allowed-tools "Read,Grep,Glob,Bash"`. It was given the five headings below
and nothing else: not the owner's words, not the 31 of 100 of 2026-10-04, not the two fixes, not this page, not
that a re-score was wanted. It returned `DONE exit=0`. Its answer is quoted below word for word and is not
rewritten.

**What it read, and why that matters.** It could not be given a git worktree of the integration branch: adding a
checkout is refused in this kind of session. So it read a file copy of the integration tip `b7d4d0ede`, holding
`docs/`, `CLAUDE.md`, `gate.ps1`, `.github/`, and the project's `Tools/`, `ProjectSettings/`, `Packages/`,
`validate.py` and all 559 C# and asmdef files under `Assets/_Project` — but **no `.git` and no art files**, and
the three lane-edited docs restored to their integration content. Two of its three worst findings rest on that
copy rather than on the project, and are marked as such below. A second run against the real checkout was
attempted and refused by this session's permissions, so the defect could not be removed. The total therefore
stands as the score this run produced, not as a sound measure of the project: areas C and D were marked down
mostly on the copy's own breakage.

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

**The 2026-10-04 rubric is lost.** That critique's own headings and weights were never written down, so of the two
scores only the existence of two totals is comparable — not the areas, not the weights, and not the
difference between the numbers.

## The score

Total, and the five sub-scores, as the agent gave them:

| Heading | Score |
|---|---|
| A. Does finished work reach the owner? | 13 / 20 |
| B. Can he see in one place what waits on him? | 14 / 20 |
| C. Do the checks catch what they claim, with nobody remembering to run them? | 4 / 20 |
| D. Is the written record true to the code? | 7 / 20 |
| E. Waste: rework, stranded work, time burned | 10 / 20 |
| **Total** | **48 / 100** |

Its three worst findings, quoted, each with the file, the command and the output it pasted.

**Finding 1 — "The project's own consistency checker is completely broken on this checkout — it doesn't flag real
drift, it floods the operator with false positives."** File: `docs/reference/agent-memory.md:133-135`. Command and
output as it printed them:

```
cd trench-warfare-3d && python Tools/health.py
validate FAILED
29 assemblies, 546 C# files
codemap: docs/reference/code-map.md: generated block "folders" is stale. Run: python Tools/codemap.py
codemap: CLAUDE.md:6: cites `docs/reference/workflow.md`, which does not exist
codemap: docs/README.md:3: cites `CLAUDE.md`, which does not exist
```

*This finding is an artifact of the copy it read.* Those files do exist; the copy did not keep the layout
`codemap.py` resolves from. On this checkout the same check is green, which is the proof the owner asked for:

```
$ cd trench-warfare-3d && python validate.py
29 assemblies, 546 C# files
validation OK
```

**Finding 2 — "Core workflow tooling hard-depends on git, and this checkout has none — landing,
handoff-mending, and lane-conflict detection are all non-functional."** Command and output as it printed them:

```
cd trench-warfare-3d && python Tools/health.py --lanes
1 checkouts; conflicts are against your branch fatal: not a git repository (or any of the parent directories): .git

?  [(detached)]  folder is gone: `git worktree prune` forgets it
```

*This finding is also an artifact:* the copy had no `.git` because none could be given to it. What it does show is
that the tools print a bare `fatal:` into the middle of a sentence meant for a person instead of failing cleanly
— that part holds, and is about `Tools/health.py`, not about the project's git.

**Finding 3 — "No CI enforcement of the gate — `validate.py` and `gate.ps1` only run if a human remembers to
run them locally."** File: `.github/workflows/unity.yml`. Command and output as it printed them:

```
grep -n "run:\|unity \|validate\|gate" .github/workflows/unity.yml
89:        run: echo "key=$(unity cache key --target StandaloneLinux64)" >> "$GITHUB_OUTPUT"
122:        run: unity install 6000.0.50f1 --yes --accept-eula
144:          unity license activate --file unity.ulf
158:        run: unity doctor --ci
161:        run: unity test --report-format junit --output test-results.xml
175:        run: unity build --target StandaloneLinux64 --output-path build/
201:        run: unity license return --yes
```

Its point: no step calls `validate.py`, `gate.ps1` or `Tools/codemap.py`, and CI runs on `ubuntu-latest` for a
project `CLAUDE.md` calls Windows x64 only. **This one holds against the real tree** and agrees with the line
further down this page: the CI witness the 2026-10-04 critique asked for is still not in place.

The two totals, 31 of 2026-10-04 and 48 of today, are not comparable beyond both being totals out of 100: the
older rubric is lost (above), and this run read the integration tree, so none of the lane's own measurements
below were in front of it.

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
built: no new tool, no cron, no recurring job. Whether to schedule anything is the owner’s to say; one defect of this run — that
the critic could not be given a real checkout — would have to be solved first for a score to be worth repeating.
