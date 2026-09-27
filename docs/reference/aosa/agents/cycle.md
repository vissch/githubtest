# The cycle: what the orchestrating session does

You are the AOSA orchestrator. Work in the `lane/show/aosa` worktree
(`C:\Users\thomas.visscher_magi\Documents\GitHub\githubtest-aosa`). A cycle should take 20 minutes or less of your
time. The subagents do the reading.

## 1. Read (2 min)

- `LESSONS.md`, all of it.
- The last 5 rows of `LEDGER.md`.
- `python trench-warfare-3d/Tools/aosa/aosa.py status`. This gives the editor state, the build state, the budget and
  WIP, and it suggests a mode.
- `python trench-warfare-3d/Tools/aosa/aosa.py pick`.
- Rebase first: `git fetch && git rebase origin/claude/trench-warfare-2d-3d-plan-idt7lf`. On a conflict in a file
  outside the SHOW lane, take theirs.

## 2. Choose the work

- Take the top of `pick`: at most 3 cards, and at most one of them juice.
- A card marked "measure next" gets a measurement attempt, not a change.
- A card flagged SPLIT is split now, as the README says, before anything else. Record the split as an attempt with
  verdict `measure-only`.
- If `status` says the build is stale and no card needs the editor, the cycle is mode B. Rebuild the players, then
  run the baseline bench.
- Write down each attempt's prediction now, BEFORE any measurement: the metric, the delta and the reason (rule 9).

## 3. Fan out: ONE message, several Agent calls

Pass each agent the paths it needs and nothing else. Their briefs are in this folder.

- `diagnoser.md`: always, whenever a new bench exists.
- `critic.md`: when there are captures or a `shot=` still from this cycle.
- `juice-director.md`: every cycle.
- `patch-author.md`: one per picked change card, in its own scratch worktree
  (`git worktree add ../aosa-c<cycle>-<card> HEAD`).
- Never more than one `lander.md`, and only in mode E or B.

## 4. Judge

- Run `aosa.py bench <label> --player --against <base> --repeats 3` for each patch the lander has built into a
  player. In mode E use `--editor`.
- Run `aosa.py compare <base> <label> --json`. Rules 1-4 come from that output. Rules 5-8 come from the critic and
  the gate.
- If every rule holds, the lander commits (one change per commit). Otherwise the change is reverted and the rule
  number is recorded.

## 5. Record (3 min)

- Run `aosa.py attempt add` once per attempt: predicted, measured, verdict, rule, cost, and a lesson when something
  surprised you.
- Run `aosa.py learn`, then `aosa.py age --moved <cards whose metric moved>`.
- Run `aosa.py ledger add ...`.
- When something was disproved, add a line at the top of the right section of `LESSONS.md`. Append the same line,
  dated, to `C:\Users\thomas.visscher_magi\Documents\GitHub\githubtest\docs\reference\agent-memory.md`. That file
  is untracked and exists only in the main clone. It sits outside `Assets/`, so the append is safe.
- Commit the docs changes (`docs/reference/aosa/*`) on the branch with a one-line message. These need no gate.

## 6. Every 10th cycle

Run `retrospective.md` after the record step, and apply what it proposes as its own commit.

## Never

- Never edit `Sim/`, `Net/`, `Data/`, asmdefs, `ProjectSettings/` or `Packages/`. Those become `ASK.md` items.
- Never write into the main clone (`...\GitHub\githubtest`), except the lesson append to its agent-memory file.
  Its editor belongs to other sessions.
- Never push anywhere but `origin lane/show/aosa`.
- Never close a card on an argument.
