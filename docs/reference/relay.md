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
| retro | Opus, high | read the run's records; write only its retrospective paper | tuning inside the bounds, proposals for the owner |

A plan may cut the work at `--- leg break ---`: one execute leg per part, four at most. The phases, limits, the way a
leg talks and the role texts are files (`phases.json`, `limits.json`, `style.json`, `roles/`), not code.
A plan leg, a critic leg and a retrospective end with `leg done`, which runs the runner's own check on their paper, so they can mend
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

## The retrospective

Between units, every ten legs (`retro_every_legs`), one leg looks back: Opus high, read-only, in a folder that holds
this run's leg records, what the guard refused, the critic rounds and the limits with their bounds. It writes a
short paper with three sections, checked by script.

| Section | What the runner does with it |
|---|---|
| What happened | nothing; it is for the reader |
| Tuning | moves amber, red and the leg time, clamped to the bounds in `limits.json`, for the rest of the run and for later runs on this station (`relay/<station>/tuning.json` on the board). A `--leg-minutes` flag outranks it |
| Proposals | copies them to `relay/proposals/<run>-<leg>.md` on the board for the owner. Nothing is applied |

A paper that fails its check changes nothing. The run's hours and the spend cap are the owner's: a retrospective
cannot move them.

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
$R run --work <work checkout> --day-budget 20     # today's legs may cost this much (default: limits.json, 50)
$R run --work <work checkout> --view              # also open a Windows Terminal tab per leg that shows its output
$R run --work <work checkout> --who <name>        # who starts it: shown by status and kept in the stop record
$R status                                         # is a run going, on what; else how the last one stopped
$R stop [--now]                                   # end before the next leg (--now: end the leg too)
$R add <id> --lane lane/show/<x> --goal "<words>" --done-when <program> <arg> ...
$R view <leg folder> [--follow]                   # a leg's output as readable lines
$R refusals [--runs 3]                            # what the guard refused in the newest runs, with the reason
$R budget [--days 8]                              # what today's legs cost against the day's budget, and the days before
$R hold <who> [--hours 4] [--release]             # one session at a time builds the relay or starts its runs
$R update [<commit>]                              # move the frozen copy to a commit (default: origin's relay lane)
python trench-warfare-3d/Tools/relay/test_relay.py   # the tests; they use a stand-in for Claude
python trench-warfare-3d/Tools/relay/test_ledger.py  # the tests of the day's spend
```

The work checkout is the relay's own worktree (`githubtest-relay-work` on the desktop), switched to each unit's lane
by the runner. Never give it a checkout a session or an editor is using.

## Three checkouts, one job each

| Checkout | Job |
|---|---|
| `githubtest-relay-run` | the frozen copy: a detached checkout that only runs the relay. Start every run from here |
| `githubtest-relay-dev` | where the relay is built and tested. Editing here never touches a run |
| `githubtest-relay-work` | where the legs work |

A run uses committed code only: the runner refuses to start when its own code has uncommitted changes
(`--allow-dirty` is for developing the relay), prints the commit it runs, and keeps it in the stop record. The
runner still stops when its own code changes under it, so nobody edits the frozen copy; `update` moves it to a
commit when no run is going.

One session at a time builds the relay or starts its runs. A session takes the hold first (`hold <its name>`); when
another name has it, the command exits 1 and the session stops and tells the owner. The hold ends by itself after
its hours. `status` shows the holder, who started the run, and how each unit has ended so far.

Work may be queued while a run is going (`add`): a new file under the board's queue does not stop the run. When a
run stops, Windows shows one notification (not where no window can open). A refused command that was normal work
is a bug in the rules: `refusals` lists them, and each becomes a line in the ordinary-work tests and a rule fix.

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

## The day's budget

No leg starts once today's legs cost the day's budget: `day_budget_usd` in `limits.json`, 50, and 0 switches it off.
`--day-budget` on `run` outranks the file, inside the bounds 0 to 500. A retrospective cannot move it.

- What is counted: the cost Claude prints per leg (`total_cost_usd`), kept in the leg record on the board as
  `cost_usd`, for every leg started that local day on every station. On a plan login it is not money: it is the
  yardstick. The owner's own sessions are not counted.
- A leg whose record holds no cost (it was killed, or an older relay wrote it) takes it from its leg folder on this
  machine, else it counts at the usual cost of its phase and is marked estimated.
- The usual cost of a leg is the median of the newest `price_legs` (200) records with a known cost: legs of the
  same phase, model and effort when there are three or more, else legs of the phase, else every leg, else
  `usual_leg_usd` (3).
- No unit starts unless what is left covers a usual plan plus a usual execute. No leg starts unless it covers that
  leg's usual cost. A leg may spend the smaller of `leg_budget_usd` and what is left; a leg that ends on that cap
  stops the run.
- `ledger.py` does the sum from this machine's copy of the board, so another station's legs count once the board
  is pulled. `$R budget` shows today per unit and phase and the days before; `$R status` and the `STOP:` line show
  one line of it.

## What stops a run

The time is up (`--hours`, 0.25 to 12), the leg cap, the day's budget (above), nothing left to do, the owner's `stop`, a work checkout that is
missing, dirty, open in Unity or just used, two units in a row with no result and no pushed code, uncommitted work
left behind, and any leg that cannot be trusted: a timeout, a compaction trip, not auto mode, no hooks, no result
record, a leg over its spend cap (`leg_budget_usd`, 30 notional dollars), or a change to the board outside
`evidence/`, to the relay's own code, or to git's push guard. Every stop writes `relay/<station>/stops/<run>.json`
on the board, with how each unit ended (`units`), so a failed unit is not hidden behind "nothing left to do".
A run started where no window can open (Windows session 0: every Claude session on the desktop) says so at its
start and on every leg card; work that needs a windowed Unity editor then ends BLOCKED. Batch mode still works.
The card tells that leg to drive the editor with the Unity CLI at %LOCALAPPDATA%/unity/bin/unity.exe, the one
`trench-warfare-3d/Tools/tw` is written for. A different unity earlier on PATH refuses the project lock and starts
a second editor. Start such a run from a normal terminal. A pipeline leg's work checkout is another lane, so it
does not have the role's skill. The runner copies that skill into the leg folder and the card points at the copy.
Leg folders and logs stay under
`%LOCALAPPDATA%\TrenchWarfare\relay\runs\`.

## What holds a leg

The stop is outside the model. While a run holds the checkout, a git pre-push hook
(`trench-warfare-3d/Tools/relay/prepush.py`, installed by the runner in the repo's shared hooks folder) refuses every
push from it except a forward push of the leg's own lane, and refuses a leg any push from another checkout.
`Tools/land.py` and the pipeline's claim, complete and release refuse inside a leg. After each leg the runner checks
the board, its own code and the push guard for changes. The rules that read a leg's commands
(`trench-warfare-3d/Tools/relay/cmdrules.py`) are a second layer: they cannot see every way to write a command.
Another branch moving on origin during a leg is recorded (`moved_on_origin`), not a stop: other sessions push all day.
