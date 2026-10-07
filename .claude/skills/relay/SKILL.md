---
name: relay
description: Run Trench Warfare 3D work as a relay - a chain of short headless Claude sessions (legs) that plan, then execute, so no session fills its context. Start a run, see what it is doing, stop it, queue lane work for it, or dry-run to see what it would pick. Use for "/relay start", "/relay status", "/relay stop", "/relay add", "/relay dry", "run the relay for N hours", "why did the relay stop". NOT for taking one pipeline job by hand (pipeline skill), NOT for landing (tw-master) and NOT for "what is going on, what do you need from me" (master).
---

# Relay

The runner is a script. It picks the work, starts one leg at a time, checks the result and decides to go on. You only
start it, read its lines and tell the owner in short plain words (he skims: answer first, a few bullets).

```bash
R="python C:/Users/PC/Documents/GitHub/githubtest-relay-run/trench-warfare-3d/Tools/relay/relay.py"
WORK="C:/Users/PC/Documents/GitHub/githubtest-relay-work"
```
The work checkout is the relay's own. Never point `--work` at a checkout a session or an editor is using.
`githubtest-relay-run` is the frozen copy: it only runs the relay. The relay is built in `githubtest-relay-dev`.

**First, every time:** `$R hold <your session name>`. Exit 1 means another session holds the relay: stop and tell
the owner. Give it back with `$R hold <your session name> --release` when you are done.

| The owner says | Run | Then |
|---|---|---|
| `/relay dry` | `$R run --work $WORK --dry-run` | say the one unit it would take |
| `/relay start` | `$R run --work $WORK --who <your session name> --hours <H> [--max-legs N] [--sources pipeline,lane]` in the background | say it started and what it took; report the `STOP:` line when it ends |
| `/relay status` | `$R status` | one or two lines: who started it, the units so far |
| `/relay refusals` | `$R refusals` | name any refusal that was normal work: it needs a rule fix and a test |
| `/relay budget` | `$R budget` | one line: what today's legs cost, of how much, what is left, and what the pace allows by now |
| `/relay agents` | `$R agents`, `$R agents book` | what the agents cost that sessions here spawned outside the relay; `book` adds it to the day |
| `/relay day` | `$R day` | the whole picture in one screen; the `/master` skill starts every turn from it |
| `/relay prio` | `$R prio <id> <n>` (0 to 99, the lower runs first, 50 when none is set) | say the new order |
| `/relay update` | `$R update [<commit>]`, only when no run is going | say the commit it now runs |
| `/relay stop` | `$R stop` (ends before the next leg) or `$R stop --now` (ends the leg too) | confirm with `$R status` |
| `/relay add` | `$R add <id> --lane lane/show/<x> --goal "<the owner's words>" [--role <role>] --done-when <program> <arg> ...` | say it is queued |
| `/relay role` | `$R role <role> <id> [<id> ...]` (a role from `Tools/pipeline/roles.json`; between legs) | say which units now get which brief |
| watch a leg | `$R view "<leg folder>" --follow` (status prints the folder) | |

## Rules
- A unit's role decides the brief its legs get (the table in the `pipeline` skill; `Tools/pipeline/roles.json`).
  Queue a unit that fixes review findings with `--role review-fix`, or `"role"` in its unit file: the standing rules
  ride in the role file, so the goal names only the findings and what is special to the unit. A role the table does
  not have is refused. `lane`, the default, has no brief.
- Ask for the hours if the owner gave none. `--hours` is clamped to 0.25-12 (default 3). `--max-legs` caps the legs;
  two legs is one unit (plan, execute), so leave it off for a long run.
- `--view` opens a Windows Terminal tab per leg with its output. It needs a desktop: from a Claude session (no
  window) it does nothing, so give the owner the command to run in a normal terminal.
- The day has a budget for all agents on the project: 11% of the plan's weekly limit (`limits.json`
  `day_budget_pct`; `docs/reference/relay.md`, "The day's budget"). No unit starts once the day has used that
  much, and the run stops saying so. `--day-pct <n>` sets another figure for one run; use it only when the owner
  names the figure.
- The budget is spent evenly over the day ("The day's pace"): a run waits when it is ahead of the pace and prints
  `paced: a unit may start at HH:MM`. That is not a hang. Give one run long hours; do not start more runs to get
  around the wait.
- A leg with an edit gate takes 30 minutes or more. Do not poll: the runner ends with one `STOP:` line.
- `done_when` for `/relay add` is a command that exits 0 when the work is there, as words, not one string. No shell.
  Run it once in the work checkout before you queue: it must fail now. A check that is already green proves
  nothing, and on a pushed lane the runner, which asks it before any leg, marks the unit done with no work.
- A unit is one thin slice that can be shown working on its own. The goal says what should happen and what is out
  of scope, in the owner's words; it names behaviour, not line numbers, which move before the leg reads them.
- The runner stops on anything it cannot trust. Read the `STOP:` line and the last file under
  `tw3d-board/relay/<station>/stops/`; say the reason as it is, do not guess.
- "the work checkout cannot be used": it names why (missing, uncommitted files, a Unity editor open, git activity in
  the last 10 minutes: `--no-quiet` skips that last check when you know nobody is in it).
- Do not edit `Tools/relay/` or `Tools/pipeline/` while a run is going: the runner sees its own code change and stops.
- Legs never land. Landing stays the owner's word (tw-master).
- Leg folders and logs: `%LOCALAPPDATA%\TrenchWarfare\relay\runs\<run>\`. Tests: `python Tools/relay/test_relay.py`.

## Sources
The thin-slice, out-of-scope, independent-expected-value, written-guesses and mechanical-or-judgement rules, here
and in `Tools/relay/roles/` (`_phase_plan.md`, `review-fix.md`, `_phase_retro.md`), are adapted, in our own words,
from `to-tickets`, `triage`, `tdd`, `diagnosing-bugs` and `retro` in github.com/mattpocock/skills at f3fc5632f401
(MIT). Ideas only; no text or script copied.
