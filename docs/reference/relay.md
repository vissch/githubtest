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

A role's legs of one phase run on another model or effort when `routes.json` says so (`config.route`; Opus or
Sonnet only), for every unit of the role or only for the units the route lists. The budget prices such a leg by
its route (`config.routed`). One route is in it, a trial: the execute legs of ten listed `review-fix` units on
Sonnet, nothing else changed, with ten more named in its `why` as the control on Opus. It is measured per unit by
the fix check of the review lane (fixcheck, not on this line yet). A route stays only on the owner's word.

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

## A second opinion from another vendor

`Tools/relay/second.py` asks another vendor's model for a critic's score or a second reader's report, on the same
files a Claude critic or reader gets: Grok Build (`grok`) or Codex (`codex`), under the owner's own sign-in on the
desktop. It records and decides nothing. The owner asked for it on 2026-10-09 (`decisions.md`).

**What holds a run to reading.** Nothing here goes through the relay's guard hooks; a second opinion has rails of
its own, and `providers/<vendor>.py` says how each vendor does its part.

| Rail | How |
|---|---|
| It works on a copy | a folder of its own under `<home>/second/` (in a run: `<home>/runs/<run>/second/`), outside every checkout; a review gets the code and docs at the head commit from git's objects, with no `.git` |
| The vendor's read-only mode | Grok: the three reading tools and nothing else, in `dontAsk` mode, MCP calls denied (its OS sandbox does not exist on Windows). Codex: `-s read-only`, the user's config left out. A command line that does not ask for this is not started |
| The last message is the paper | the script writes the file; text files ride in the prompt, pictures are attached (Codex) or opened with the reading tool (Grok) |
| The run is read afterwards | the copy is hashed before and after; a changed file, a tool beyond reading that ran, or a run that says it had another mode or more built-in tools than it was given makes the paper untrusted and unused |

One thing the proof does not try: Grok lists the tools of the owner's plugin MCP servers (flights, hotels) in a run
whose servers are up in time. The deny rule and the mode refuse such a call, and one that ran would show in the
run's output and make it untrusted; no run has shown one.

`$R proof second <vendor>` asks the model to write a file in and outside its folder and passes when nothing
changed; `--open` runs the same with the rail off, where the file must appear, or the proof shows nothing. On the
desktop on 2026-10-09: Grok passed both. Codex wrote nothing either way: over ssh on that machine its sandbox starts
no command at all, so the proof cannot yet tell the rail from that. Run both again after a Codex update, and before
trusting Codex with anything it could harm.

**In a run.** `limits.json` `second_critic` names the vendor ("" as shipped: nobody), and `run --second
grok|codex|off` outranks it. After each critic round the vendor scores a fresh copy of the same bundle with the same
card. Its paper is kept beside the critic's as `critic-r<n>-<vendor>.md`, a row goes to
`relay/<station>/second/` on the board, and the result's note says "grok on round 1: 62/100". A later round's
bundle holds neither critic's earlier paper. The Claude critic's score alone decides a fix round. A second opinion
that fails, runs out of its time (`second_minutes`) or cannot be trusted is words in the note: it never stops a run
and never changes a verdict. It is no leg: the leg cap does not count it and the day's spend does not hold it. Its
own cap is a count, `second_per_day` over every station, because it draws on the other vendor's plan.

**Cost.** The owner's plans with the two vendors cover these runs. `vendors.json` holds the model each is asked for
and list prices, for a "what this would have cost" figure on the row (`list_usd`): Grok prints its own, Codex prints
tokens only. Codex's row names the best model the installed `codex.exe` may use; move it when Codex is updated.

Not built: plan and execute legs by another vendor. Those need the guard, the context meter and the day's budget
for that vendor.

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
| `lane` | a committed file `relay/queue/<id>.json` on the board: `id`, `lane`, `role`, `goal`, `done_when` (a command as a list of words), and optionally `priority` | `done_when` exits 0, the lane pushed | `relay/done/<id>.json` |

The runner claims and completes pipeline jobs. A leg never does, and never lands.

**A unit's role.** A board stage and a queue file each name a `role`. `Tools/pipeline/roles.json` is the one table
of roles: each maps to the skill that is its brief, or to nothing for a role that is known and has none. For a role
with a brief the runner copies that skill into the leg's folder before a plan or an execute leg, and the card says
to read it; both sources do this. A role may also have a file `Tools/relay/roles/<role>.md`, which is added to the
leg's system prompt: `review-fix` has one, the standing rules of a unit that fixes review findings, so a goal need
not repeat them. `pipeline.py` refuses a board stage whose role is not in the table and `$R add` refuses such a
unit. A queue file that already holds an unknown role still runs, without a brief, and its card says so.
`$R role <role> <id> [<id> ...]` changes the role of queued units in one board commit. It refuses a unit that is
done, a queue file nobody committed and a role the table does not have.

## Commands

```bash
R="python trench-warfare-3d/Tools/relay/relay.py"
$R run --work <work checkout> --dry-run           # say what it would take; start nothing
$R run --work <work checkout> [--hours 3] [--max-legs N] [--sources pipeline,lane] [--leg-minutes 90]
$R run --work <work checkout> --day-pct 15        # the day's cap in percent of the week, when the owner named one (default 11)
$R run --work <work checkout> --day-budget 20     # the day's cap in dollars of cost for this run; the percent cap is then not used
$R run --work <work checkout> --view              # also open a Windows Terminal tab per leg that shows its output
$R run --work <work checkout> --who <name>        # who starts it: shown by status and kept in the stop record
$R status                                         # is a run going, on what; else how the last one stopped
$R stop [--now]                                   # end before the next leg (--now: end the leg too)
$R add <id> --lane lane/show/<x> --role <role> --goal "<words>" --done-when <program> <arg> ...
$R add --unit <file> [--role <role>]              # the same from a file in the queue file's shape; --role when it names none
$R role <role> <id> [<id> ...]                    # give queued units a role from roles.json ("A unit's role")
$R view <leg folder> [--follow]                   # a leg's output as readable lines
$R run --work <work checkout> --second grok       # another vendor scores each critic round too ("A second opinion")
$R proof second grok|codex [--open]               # the vendor's read-only run is asked to write: nothing may change
S="python trench-warfare-3d/Tools/relay/second.py"
$S critic --vendor grok|codex --bundle <folder> --role <role> --stage <id> --out critic.md
$S review --vendor grok|codex --checkout <repo> --commits <base>..<head> --about "<unit>" --out report.md
$S bundle --checkout <repo> --commits <base>..<head> --out <folder>   # where the repo is; then, where the vendor is:
$S review --vendor grok|codex --bundle <folder> --out report.md
python trench-warfare-3d/Tools/relay/test_second.py  # the tests of second opinions; they use a stand-in for both vendors
$R refusals [--runs 3]                            # what the guard refused in the newest runs, with the reason
$R budget [--days 8]                              # what today's legs cost against the day's budget, and the days before
$R day                                            # one screen: budget, pace, run, queue in its order, what needs the owner, his answers
$R agents [--day <day>]                           # what the agents cost that sessions here spawned outside the relay
$R agents book [--out <file> | --file <file>]     # add that to the day's budget ("Agents outside the relay")
$R usage                                          # where the plan's week stands, from the newest reading on this machine
$R usage put [--statusline]                       # keep a reading given on stdin as JSON (see "The day in percent")
$R prio <id> <n>                                  # move a queued unit: 0 to 99, the lower runs first (50 when none is set)
$R hold <who> [--hours 4] [--release]             # one session at a time builds the relay or starts its runs
$R update [<commit>]                              # move the frozen copy to a commit (default: origin's relay lane)
python trench-warfare-3d/Tools/relay/test_relay.py   # the tests; they use a stand-in for Claude
python trench-warfare-3d/Tools/relay/test_ledger.py  # the tests of the day's spend
python trench-warfare-3d/Tools/relay/test_day.py     # the tests of the day screen, the queue's order, his answers, add --unit
python trench-warfare-3d/Tools/relay/test_usage.py   # the tests of the weekly-limit readings
python trench-warfare-3d/Tools/relay/test_agents.py  # the tests of the agents outside the relay
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
| `$R leg play <filter>` | runs the PlayMode tests the filter names (a class or a full test name) for this checkout and waits up to 9 minutes; prints GREEN or RED with total, passed, failed and each failed test, or RUNNING (the same command again waits on). The report is `playmode-<n>.xml` in the leg's folder |
| `$R leg finish -m "<message>"` | commits and pushes when the gate is green for exactly these files; otherwise saves the work as a patch beside the leg |
| `$R leg done` | exit 0 when nothing is uncommitted and the lane is pushed |

**Nobody wakes a leg.** The end of a leg's turn is the end of the leg, so nothing in it may wait for a notice. A leg
runs with Claude Code's background tasks switched off (`NO_WAKING` in `launch.py`): a command that outlasts its
time stops with an error and is not "moved to the background", and one call may last ten minutes, so a wait of
9 minutes fits in one. The guard refuses `run_in_background` on any tool and the tools that promise a later turn
(`ScheduleWakeup`, `CronCreate`, `Monitor`). A PlayMode run is a detached job like the gate, so the runner stops it
with the leg and a leg never looks for "its" Unity in the process list. The cause: `rv-12-hud-f9`, 2026-10-08,
whose execute leg waited on another checkout's editor and ended its turn "to wait for the notification".
`$R proof wait` shows it on a real leg: a command of two and a half minutes, and the report has its result.

**One more turn for a leg that owes its report.** A leg's verdict is the first line of its report that starts with
`RESULT` (words above it do not hide it). A working leg whose session ends cleanly with no such line, most often
because it stopped to wait, is given one more turn in the same session (`claude -p --resume`), told that the turn's
end was the leg's end and to finish and report; the file resume.txt in the leg's folder holds those words. One extra
turn, never two; not at red, not for a leg that only reads, not with under two minutes left. Both turns are paid
for: the leg's cost and turns are the sum of its result records, and its record says `resumed: 1`.
`$R proof report` shows it on a real leg.

## Context limits

A hook measures the context after every tool batch. At amber (240k tokens) the leg is told to finish its step and
start the gate. At red (300k) only the close-out works: `git status/diff/log`, the handoff note, and the `leg` commands.
Automatic compaction is blocked; a leg that reaches it ends the run.

## The day's budget

All agents spawned to work on the project share one day: **11% of the plan's weekly limit** (`day_budget_pct` in
`limits.json`; the owner, `decisions.md` 2026-10-07). No leg starts once the day has used that much.
`--day-pct <N>` on `run` sets another figure for that run, inside 0 to 100: only when the owner named one for the
day. A retrospective cannot move it.

- **What is counted.** Every leg started that local day, on every station, and the agents sessions spawned
  outside the relay once they are booked ("Agents outside the relay" below). The owner's own talk with a session
  is not counted.
- **In percent.** A measured leg counts what it used of the week; any other leg is counted from its cost ("The day
  in percent of the week" below). So the cap holds whenever there is a rate, measured or guessed.
- **In dollars, as the fallback.** While nothing can be said in percent (`week_usd` 0 and no leg measured), the cap
  is `day_budget_usd` (50). `--day-budget <n>` gives one run a cap in dollars, inside 0 to 500, and the percent cap
  is then not used; 0 means no cap at all.
- **A leg's cost** is what Claude prints for it (`total_cost_usd`), kept in the leg record on the board as
  `cost_usd`. On a plan login it is not money: it is the yardstick. A record with no cost (the leg was killed, or
  an older relay wrote it) takes it from its leg folder on this machine, else it counts at the usual cost of its
  phase and is marked estimated.
- The usual cost of a leg is the median of the newest `price_legs` (200) records with a known cost: legs of the
  same phase, model and effort when there are three or more, else legs of the phase, else every leg, else
  `usual_leg_usd` (3).
- No unit starts unless what the day has left covers a usual plan plus a usual execute. No leg starts unless it
  covers that leg's usual cost. A leg may spend the smaller of `leg_budget_usd` and what the day has left; a leg
  that ends on that cap stops the run.
- **Every relay counts.** `ledger.py` does the sum from this machine's copy of the board, so another station's legs
  count once the board is pulled. A second relay with a board clone of its own is read too: `TW_BOARD_ALSO` names
  the other clone (several paths: apart by `;` on Windows), for both relays, and a leg record that is in both
  counts once.
- `$R budget` shows today per unit and phase and the days before; `$R status` and the `STOP:` line show one line
  of it. The stop record holds `day_pct` and `day_budget_pct` beside `day_usd`.

## The day's pace

The cap is spent slowly, not at once (the owner, 2026-10-07). It is spread evenly from `pace_from_hour` to
`pace_to_hour` (`limits.json`: 0 and 24, local time), and **what the pace allows by now is the cap times the share
of that window that has passed**: 5.5% of the week by noon, about 0.46% an hour. What earlier hours left unused
stays allowed later the same day. The same hour for both ends switches the pace off.

- The pace decides when a unit or a leg may **start**: only when what it allows, less what the day has used,
  covers the usual cost (a usual plan plus a usual execute for a unit).
- **The run waits.** When the day covers the work and the pace does not yet, the run prints
  `paced: a unit may start at HH:MM`, waits, and goes on; the owner's `stop` ends the wait. It keeps the work
  checkout while it waits. `$R status` shows "waiting for the day's pace until HH:MM".
- After a wait the run asks the day again (another relay may have spent it meanwhile) and reads the queue
  again, so a unit the owner moved to the front during the wait is the one that starts.
- When the wait would end after the run's own time (`--hours`), the run stops instead, with
  `the day's pace lets a unit start at HH:MM, after this run's N hours are up`. A dry run never waits: it says
  the time.
- A leg that has started may spend what the **day** has left, not only what the pace allows: a leg cut off half way
  leaves work nobody can use. So the day can run ahead of the pace by one leg.
- `$R day` and `$R budget` print the line `Pace: 5.5% of the week allowed by 12:00, about 2.4% of it free.`, and
  when it does not cover the next unit, the time it will.

## Agents outside the relay

A session can spawn agents of its own (subagents, a workflow's agents). They work on the project and cost the same
week, so the day counts them (`agents.py`).

- **Where the figure comes from.** Claude Code logs every answer of such an agent with its tokens, on the machine
  that ran it. `$R agents` adds up the agents of the day whose folder or first prompt names the project
  (`agents.json`, `names`), and prices the tokens at the list prices in the same file. It is never a measurement:
  every line says `about`. A model the file does not name is priced as its dearest one, and the lines say so.
- **A leg's own agents are not counted again**: the cost Claude prints for a leg covers the agents it spawned. A
  leg's session is known by its leg folder and by the folder it runs in (`agents.json`, `leg_folders`).
- **Booking.** `$R agents book` writes the day's total of this station to `relay/<station>/agents/<day>.json` on
  the board and pushes it; a later booking the same day replaces it. A run books its own machine before every
  unit, so on the station that runs the relay nobody has to.
- **A machine without the board** (the laptop) writes a file, `$R agents book --out <file>`, and the station that
  holds the board books it: `$R agents book --file <file>`.
- **While a run is going** a booking waits in the relay's home and the run writes it on the board before its next
  unit. Nothing but the run writes the board while a leg works, or the leg would be blamed for the change.

## The day in percent of the week

The owner reads the day in percent of the plan's weekly limit, not in dollars, and the day's cap is set in it. The
dollar figure stays underneath: it is known for every leg, so a leg that is not measured is counted from it.

- **A reading** is how much of the week is used, 0 to 100, with the time the week starts over. `usage.py` keeps the
  newest one in a file in the relay's home on that machine (not in the repo). It calls nobody and reads no login: a source
  hands the reading in through `$R usage put`.
- **Sources.** `$R usage put --statusline` is a Claude Code status line command: it takes the status line's input,
  keeps `rate_limits.seven_day`, and prints `week 41%`. A status line only runs in a terminal session, so it feeds
  readings only while one is open on that machine. `$R usage put` also takes the answer of the usage call behind
  Claude Code's own `/usage` screen (`seven_day.utilization`). **Nothing in the repo makes that call yet.** When a
  caller is added it calls at most once in five minutes (the owner's rule, `decisions.md` 2026-10-05).
- **A leg is measured** when a reading no older than `usage_max_age_seconds` (600) is there as it starts and a
  newer one as it ends. Its record then holds `week_start`, `week_end` and `week_used` (percent points). The
  figure is the whole account's: a session of the owner's that works while the leg runs is counted into the leg.
- **A leg that is not measured** is counted from its cost, at the rate the measured legs show: what the newest
  `price_legs` measured legs used of the week over what they cost. Such a figure is marked `about`, and the lines
  say how many legs are estimated.
- **Until one leg is measured** the rate is a guess: a full week is taken as `week_usd` dollars of leg cost
  (`limits.json`, 1830), so one dollar is about 0.055 points and the day's 11% about $200 of cost. Every line then
  says `about` and ends on "A guess: ...". The first measured leg replaces the guess. `week_usd` 0 switches the
  guess off, and the lines stay in dollars until a leg is measured.
- **Where 1830 comes from.** Anthropic publishes no figure. People who ran into the weekly limit and priced their
  own usage at API list prices report, for Max 20x, about $1,830 (one user, late September 2026) and $1,740 to
  $2,200 (another, August 2026), and for Max 5x about $1,170 (October 2026). The owner's plan is Max 20x
  (`decisions.md` 2026-10-06), so 1830 it is; on Max 5x the figures would be about 1.6 times higher (1170).
  The limit is also metered in Anthropic's own units and has been moved by promotions, so read a guessed figure as
  right to within a factor of two, not to the decimal.
- `$R day`, `$R budget`, `$R status` and the `STOP:` line say the day, each unit, the queue and the day's cap in
  percent whenever there is a rate, measured or guessed. The cap itself is exact (11.0%); what is used of it is
  `about` while a leg or an agent in it is counted from cost.
- `$R day` also says where the week stands: from a reading on this machine that is new enough, else from the
  newest one a leg left on the board, with its time. With neither, the line is left out.

## The master's screen and the queue's order

`$R day` prints one screen, and reads only: what the day has left, what the pace allows by now, is a run going on this machine or how the newest
run on the board stopped, the queue in the order the runner takes it with the usual cost of a unit (a usual plan
plus a usual execute), what needs the owner (a unit that did not pass, a queue file the runner will not take), what
he answered on the Decide page that no session has taken up, and who holds the relay build. It shows at most `day_queue_rows` (12) rows per list and no line over `day_line_chars`
(100). It reads this machine's copy of the board. The `/master` skill starts every turn from it.

The queue runs by priority, then by name: a queue file may hold `"priority"`, a whole number from 0 to
`queue_priority_max` (99), and the lower runs first. A unit that names none has `queue_priority` (50).
`$R prio <id> <n>` writes it and commits the file on the board, mid-run too. It refuses a unit that is done and a
queue file nobody committed.

**The owner's answers.** A decision that waits on the owner is put to him on the asset board's Decide page as a
short brief with options, and his click or his own words there is a note in a folder on the Drive. The screen says
whether such answers wait: `Your answers: nothing waits.`, or `Your answers: N not taken up.` with a row each (what
he picked, then the brief's title), or `Your answers: not read (...)` when the two folders are not there. That last
form is the one place the screen speaks of a source it could not read: left out, a Drive that is not mounted would
read as "nothing waits". The reader is `answers.py`; it reads the folders `decisions` and `notes` of the Drive's
TW3D-pipeline as the asset board writes them (`TW_BRIEFS` and `TW_NOTES` name others), and it decides nothing.

Whether an answer is his yes to queue work is the asset board's rule, written in its briefs tool (briefs.py, on the
asset board's lane, which this lane does not hold) and nowhere else:
a click on an option that showed what would be queued is a yes to that unit (decisions.md, 2026-10-06, on the asset
board's lane), anything else is asked first. The station he clicked on works that out and writes the unit as a
file; `$R add --unit <file>` queues it. A file crosses ssh where quoted words do not, which is how the laptop
queues on the desktop's board. The same unit added again is queued once: the call ends 0 and pushes the board
again, so a take-up that stopped halfway can be repeated; another unit under a taken id is refused. The steps, in
their order, are in the `/master` skill, "Decisions".

## What stops a run

The time is up (`--hours`, 0.25 to 12), the leg cap, the day's budget (above), the day's pace when waiting for it would outlast the run, nothing left to do, the owner's `stop`, a work checkout that is
missing, dirty, open in Unity or just used, two units in a row with no result and no pushed code, five units in a
row whose lane cannot be switched to (`stuck_lanes`), uncommitted work
left behind, and any leg that cannot be trusted: a timeout, a compaction trip, not auto mode, no hooks, no result
record, a leg over its spend cap (`leg_budget_usd`, 30 notional dollars), or a change to the board outside
`evidence/`, to the relay's own code, or to git's push guard. Every stop writes `relay/<station>/stops/<run>.json`
on the board, with how each unit ended (`units`), so a failed unit is not hidden behind "nothing left to do".
A unit whose lane the work checkout cannot switch to (another checkout has that branch out) is recorded FAIL with no
leg, and the next unit is taken; until 2026-10-09 it ended the whole run.
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
