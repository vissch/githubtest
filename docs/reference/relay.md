# The relay: work as a chain of short sessions

A long session fills its context and compacts, and compaction loses detail. The relay runs the same work as a chain
of short headless Claude sessions ("legs"). A script, not a model, picks the work, starts one leg at a time, checks
the result and decides to go on. The code is `trench-warfare-3d/Tools/relay/`; the `/relay` skill wraps the commands.

## One unit, two legs

| Phase | Model, effort | May do | Leaves |
|---|---|---|---|
| plan | Opus, high | read anything; write only `plan.md` in its leg folder | a plan the runner checks by script |
| execute | Opus, low | the work, following the plan | commits on the lane, pushed |
| critic | Opus, high | read its evidence bundle; write only `critic.md`; no subagent | a score out of 100 and three mandated fixes |

A plan may cut the work at `--- leg break ---`: one execute leg per part, four at most. The phases, limits, the way a
leg talks and the role texts are files (`phases.json`, `limits.json`, `style.json`, `roles/`), not code.
A plan leg and a critic leg end with `leg done`, which runs the runner's own check on their paper, so they can mend
it before the runner reads it.

## The critic

A pipeline job whose script checks pass gets a critic leg. It is blind: it works in a folder of its own that holds
a copy of the stage's evidence and the stage's definition, it is not pointed at the board, and its card carries the `tw-critic`
rubric and nothing about how the work was made. Under the target (85, `critic_target`) one execute leg does the
three mandated fixes and makes the evidence again, then the critic scores once more (`critic_rounds`, 2 in all).

The verdict stays the script's: a PASS is not turned into a FAIL by a score. The scores go into the result's note
("critic 60/100, then 90/100 (target 85)"), each round is kept beside the evidence as `critic-r<n>.md`, and each
round adds a row to `relay/<station>/lessons.md` on the board. A fix round that scores lower is named in the note;
undoing it is the owner's call. With no room left (the leg cap, the time, the owner's stop) the critic is skipped
and the note says so. Queued lane work has no evidence bundle and gets no critic.

## Where the work comes from

| Source | A unit is | Checked by script | Result goes to |
|---|---|---|---|
| `pipeline` | a READY, STALE or RECHECK job on the board for this station (`docs/reference/stations.md`); never a master stage | one fresh JPG per band, the lane pushed | the board, written by the runner |
| `lane` | a committed file `relay/queue/<id>.json` on the board: `id`, `lane`, `role`, `goal`, `done_when` (a command as a list of words) | `done_when` exits 0, the lane pushed | `relay/done/<id>.json` |

The runner claims and completes pipeline jobs. A leg never does, and never lands.

## Commands

```bash
R="python trench-warfare-3d/Tools/relay/relay.py"
$R run --work <work checkout> --dry-run           # say what it would take; start nothing
$R run --work <work checkout> [--hours 3] [--max-legs N] [--sources pipeline,lane] [--leg-minutes 90]
$R status                                         # is a run going, on what; else how the last one stopped
$R stop [--now]                                   # end before the next leg (--now: end the leg too)
$R add <id> --lane lane/show/<x> --goal "<words>" --done-when <program> <arg> ...
$R view <leg folder> [--follow]                   # a leg's output as readable lines
python trench-warfare-3d/Tools/relay/test_relay.py   # the tests; they use a stand-in for Claude
```

The work checkout is the relay's own worktree (`githubtest-relay-work` on the desktop), switched to each unit's lane
by the runner. Never give it a checkout a session or an editor is using.

Inside a leg, the leg card names these:

| Command | Does |
|---|---|
| `$R leg gate start` | starts `gate.ps1 -EditOnly` detached and records the tree it started on |
| `$R leg gate wait` | waits up to 9 minutes; prints GREEN, RED, RUNNING, STALE (the files changed since) or NONE |
| `$R leg finish -m "<message>"` | commits and pushes when the gate is green for exactly these files; otherwise saves the work as a patch beside the leg |
| `$R leg done` | exit 0 when nothing is uncommitted and the lane is pushed |

## Context limits

A hook measures the context after every tool batch. At amber (240k tokens) the leg is told to finish its step and
start the gate. At red (300k) only the close-out works: `git status/diff/log`, the handoff note, and the `leg` commands.
Automatic compaction is blocked; a leg that reaches it ends the run.

## What stops a run

The time is up (`--hours`, 0.25 to 12), the leg cap, nothing left to do, the owner's `stop`, a work checkout that is
missing, dirty, open in Unity or just used, two units in a row with no result and no pushed code, uncommitted work
left behind, and any leg that cannot be trusted: a timeout, a compaction trip, not auto mode, no hooks, no result
record, a leg over its spend cap (`leg_budget_usd`, 30 notional dollars), or a change to the board outside
`evidence/`, to the relay's own code, or to git's push guard. Every stop writes `relay/<station>/stops/<run>.json`
on the board, with how each unit ended (`units`), so a failed unit is not hidden behind "nothing left to do".
A run started where no window can open (Windows session 0: every Claude session on the desktop) says so at its
start and on every leg card; work that needs a windowed Unity editor then ends BLOCKED. Start such a run from a
normal terminal. Leg folders and logs stay under
`%LOCALAPPDATA%\TrenchWarfare\relay\runs\`.

## What holds a leg

The stop is outside the model. While a run holds the checkout, a git pre-push hook
(`trench-warfare-3d/Tools/relay/prepush.py`, installed by the runner in the repo's shared hooks folder) refuses every
push from it except a forward push of the leg's own lane, and refuses a leg any push from another checkout.
`Tools/land.py` and the pipeline's claim, complete and release refuse inside a leg. After each leg the runner checks
the board, its own code and the push guard for changes. The rules that read a leg's commands
(`trench-warfare-3d/Tools/relay/cmdrules.py`) are a second layer: they cannot see every way to write a command.
Another branch moving on origin during a leg is recorded (`moved_on_origin`), not a stop: other sessions push all day.
