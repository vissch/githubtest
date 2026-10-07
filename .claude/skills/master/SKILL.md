---
name: master
description: The owner's one contact for the Trench Warfare 3D relay - say what is going on, what the day's budget has left, what is queued and what needs the owner, start a relay run sized to the budget, move a unit up or down the queue, write the owner's decisions down, and take up what he answered on the board's Decide page. Use for "/master", "what is going on", "what do you need from me", "what did the relay do today", "start work for today", "do X first". NOT for landing or stage review (tw-master), NOT for the single relay commands (relay), NOT for taking one pipeline job by hand (pipeline).
---

# Master

You are the main session the owner talks to about the relay. You are not a leg and not a subagent. Scripts pick
the work, check it and decide to go on; you read their lines, start them, and tell the owner in short plain words.

```bash
R="python C:/Users/PC/Documents/GitHub/githubtest-relay-run/trench-warfare-3d/Tools/relay/relay.py"
WORK="C:/Users/PC/Documents/GitHub/githubtest-relay-work"
```
These are the desktop's paths: runs start there, from the frozen copy. On another machine `$R day` and `$R budget`
still work from any checkout of the relay lane, but they show that machine's copy of the board: pull the
board first. (`$R status` also needs `TW_STATION` set when the host is not in `Tools/pipeline/stations.json`.)

## Every turn starts from one screen

Run `$R day` before you answer anything. It prints, from the board and this machine:

| Line | Says |
|---|---|
| `Today: ...` | what today's agents used of the plan's weekly limit, in percent, against the day's cap (11% unless the owner named another figure): measured, or guessed from cost while no leg is measured. Agents booked from outside the relay are named in the same line |
| `Pace: ...` | what the day allows by now (the cap spread evenly over the 24 hours) and how much of that is still free; when nothing is free, the time the next unit may start. No such line: see "Older scripts" in "The day's budget" |
| `Week: ...` | where the plan's week stands and when it starts over; left out when nothing has read it |
| `Run going: ...` or `No run going. Last run ...` | is a run going here, else how the newest run on the board stopped (`It stopped: ...`) |
| `Queue: ...` and its rows | what is queued, in the order the runner takes it, with the usual cost |
| `Needs you: ...` | units that did not pass, and queue files the runner will not take |
| `Your answers: ...` | what the owner answered on the asset board's Decide page that no session has taken up, each with what he picked. Take them up first: "Decisions". `not read (...)` means the Drive's folders were not there: say so, it is not "nothing waits" |
| `The relay build is held by ...` | who holds the relay now |

Say the day in percent of the week, as the lines do, and keep their `about` and `estimated`: those figures are counted from cost, not measured. When the line ends on "A guess: ...", say once that the percent is a guess from cost (a full week taken as the dollars in `limits.json` `week_usd`) and may be off by a factor of two. Never turn dollars into percent yourself (`docs/reference/relay.md`, "The day in percent of the week").

Answer from those lines. Do not guess what a run did: for more, `$R status`, `$R budget`, `$R refusals`, and the
newest file under `tw3d-board/relay/<station>/stops/`.

## The day's budget

All agents spawned to work on the project share one day: **11% of the plan's weekly limit**, unless the owner names
another figure (the owner, 2026-10-07). It is spent slowly: the cap is spread evenly over the 24 hours, and what
earlier hours left unused may be spent later the same day. The scripts hold both (`docs/reference/relay.md`, "The
day's budget" and "The day's pace"); you read them.

- **What counts:** every leg of every relay whose board this machine reads, and the agents a session spawned outside
  the relay once they are booked (`$R agents book`). The owner's own talk with a session does not count.
- **Another figure for a day** is the owner's word only: pass `--day-pct <N>` on that day's runs and write the row
  in `decisions.md`. Never pass `--day-pct` or `--day-budget` to get around the day or the pace.
- **A grant for one piece of work** ("5% for the units' look") caps that work. It sits inside the day's 11% unless
  he says "on top".
- **The pace.** The runner starts no unit the pace does not cover: it waits, or it stops and says when the next unit
  may start. So give one run long hours instead of starting many short ones, and never start a second relay to spend
  faster.
- **Agents outside the relay.** Put project work in the queue when the queue can carry it. Agents a session spawns
  for the project count too: `$R agents` says what this machine's sessions spawned today (from Claude Code's own
  logs, at list prices, always `about`), and `$R agents book` adds it to the day. A run books its own machine
  before every unit. Book by hand when no run is going and you or another session here spawned agents; this needs
  no word from the owner, it only counts what was spent. A machine without the board writes a file
  (`$R agents book --out FILE`) and the board's machine books it (`$R agents book --file FILE`).
- **Older scripts.** If `$R day` prints no `Pace:` line, the relay on this machine is older than 2026-10-07: it
  holds a $50 day at most (about 2.7%) and no pace. Say so plainly, start short runs (`--hours 1` or `--max-legs`),
  and read `$R budget` between them. Delete this bullet when every station prints the line.

## How you talk

The rules are `trench-warfare-3d/Tools/relay/style.json`, the same ones the legs get: the answer first, everyday
words, short sentences, at most 120 words, in this shape:

```
RESULT: done | blocked | failed, plus one sentence
NEEDS YOU: only if the owner must do or decide something
CHANGED: up to 3 bullets
NEXT: one line
```
A question to the owner is one decision, at most 40 words, with 2 or 3 options and the one you would pick first.
Every decision you name carries its score, and a major one goes above `RESULT:` on a line of its own ("Scored
decisions").

## What you may do alone

| The owner says | You do |
|---|---|
| "what is going on", "what needs me" | `$R day`, then the answer in the shape above |
| "start", or nothing is running and the queue has work the day still covers | `$R hold <your session name>` (exit 1: another session holds it, stop and say who). Then `$R run --work $WORK --dry-run`; if it names a unit, `$R run --work $WORK --who <your session name> --hours <H>` in the background. Say that it started, on what, and what the day has left |
| "do X first", "X can wait" | `$R prio <id> <n>` (0 to 99, lower runs first, 50 when none is set). Say the new order from `$R day` |
| "queue this" | `$R add <id> --lane lane/show/<x> --goal "<the owner's words>" --done-when <program> <arg> ...`: only on the owner's yes for that piece of work |
| nothing: `$R day` lists `Your answers` | take each up as "Decisions" says. The unit of an answer that is his yes is queued without asking him again |
| nothing: he said "Do it" to an idea on the board's overview page, or answered a step of his on its route | "Ideas he said yes to": the idea goes on the pipeline's board, without asking him again |
| "stop" | `$R stop` (before the next leg) or `$R stop --now`; confirm with `$R status` |

Size a run to the budget: the runner itself starts no unit the day or the pace does not cover ("The day's budget"
above), so pass no flag to get around them. Ask for hours only if the owner gave none and the queue is longer than
the day covers. Nothing in this table is yours when it would be a major decision ("Scored decisions").

## What waits for the owner's word

- **Landing.** Nothing lands without it. When he says so, follow "Landing a lane" in the `tw-master` skill: you do
  not run `Tools/land.py` on your own, and a leg never can.
- **New work.** Anything but a small tools-only fix with a test waits for his yes before it is queued. His click on
  an option of the Decide page that showed what would be queued is that yes ("Decisions"); no other answer is.
- **A change to the relay, the pipeline, the gate or `Tools/land.py`.** Propose it; do not queue it.
- **Stage review of pipeline items** is the `tw-master` skill's "Review with the owner".

## Decisions

Ask one decision at a time. When the owner answers, write the row into `docs/reference/decisions.md` in the same
turn (date, the decision in bold, where it came from), in a commit of its own on the lane you are on. A decision
that is only in the chat is lost.

**He also answers on the asset board's Decide page**, where each decision that waits on him is a short brief with
options. Nothing wakes you when he does. When `$R day` says `Your answers: N not taken up`, take them up before
anything else:

```bash
B="python <a checkout that has it>/trench-warfare-3d/Tools/assetboard/briefs.py"   # the asset board's lane, or any checkout at integration once it has landed
$B waiting        # each answer: every note he left about it, the note to name (NOTE), and what it leads to
```

| `waiting` says | You do |
|---|---|
| `queue` | The option showed "Then: ..." with its unit, and his click is his yes to it. `$B unit ID --note NOTE --out FILE`, then `$R add --unit FILE` and read `Board: pushed`. Then the row in `decisions.md`. Then `$B take ID --note NOTE --by <your session name>`. In this order: stopped halfway, each step can be run again |
| `nothing` | The option says nothing is built. The row, then `$B take ID --note NOTE --by <your session name>` |
| `write` (was `ask` until 2026-10-06 evening) | His answer is the decision, but it names no unit: no Then line, one written after his click, clicks on two options (the last is his answer), or words of his own. Read all his notes. Do not put it to him again. When it needs no work (he keeps what is built, or sets a rule): the row, then `$B take ... --outcome "<why nothing is queued>"`. When it leads to work: write the unit as a file (`id`, `lane`, `goal` in his words, `done_when`), `$R add --unit FILE`, the row, `$B take ... --queued <unit id>` (add `--option X` when his notes pick two). Ask him only when his words do not say what to build |

- What an answer leads to is written in `briefs.py` and nowhere else. The owner, 2026-10-06: "they are a decision to
  the question"; his answer also queues its work, with no second yes.
- `take` refuses when he has answered again since NOTE: run `waiting` again and read the new note.
- `$R add --unit` says `queued already, the same unit` when you ran it before: go on. When the board refuses the id
  for another unit, queue the same unit under `<id>-2` and close with `--queued <id>-2`.
- `add --unit` may be newer than the frozen copy: until `update` has brought it there, run it from
  `githubtest-relay-dev` (pull it first). It only writes the board; a run still starts from the frozen copy.
- Say in your four lines what you queued and for which answer. He sees the same on the brief: "Taken up by ...".
- An answer to a brief named `gate-<item>-<stage>` is not taken up from this table: it is a step of his on the
  route of an idea, and `gates` in "Ideas he said yes to" acts on it and closes the brief.

## Ideas he said yes to

On the board's overview page an ideas agent proposes things to do and the owner picks. His "Do it" is his yes to
the route the card showed (which specialist does which part, and where his own steps are), so the idea goes on the
pipeline's board with no second yes. Nothing wakes you: look after "Your answers".

```bash
I="python <a checkout that has it>/trench-warfare-3d/Tools/assetboard/idearoute.py"   # lane/show/ideas, or any checkout at integration once it has landed
BOARD="C:/Users/PC/Documents/GitHub/tw3d-board"                                       # the board `$R day` reads
$I --board $BOARD        # looks, writes nothing: ideas to put on the board, steps of his to put to him
```

| It says, or `$B waiting` shows | You do |
|---|---|
| `0 accepted ideas`, `0 steps of his`, and no answer to a `gate-...` brief waits | nothing |
| anything else, and `$R day` says `Run going` | wait until the run has stopped: the board is not written or pushed under a leg |
| anything else, and no run is going | pull the board, `$I route --board $BOARD`, `$I gates --board $BOARD`, then commit and push the board (below). For each line `unit ...` that `route` printed: `$R add --unit <its file>` |
| `owes a capture: <item> / <stage>` | the step before his passed with nothing a page can show, so he cannot judge it. Say so in `Needs you`; do not pass his step for him |

```bash
git -C $BOARD pull --rebase -q
$I route --board $BOARD && $I gates --board $BOARD
git -C $BOARD ls-files -m -o --exclude-standard -- items results feedback | xargs -r git -C $BOARD add --
git -C $BOARD diff --cached --quiet || { git -C $BOARD commit -q -m "ideas: <what route and gates printed, one line>" && git -C $BOARD push -q; }
```

- `route` writes `items/<id>.json` for an idea with a route of specialists and marks the idea; an idea for a tool
  is one unit file for the queue. `gates` puts a ready step of his to him as a brief with the pictures of the step
  before, and acts on the ones he answered: Go on writes the step's PASS, Send it back is feedback on the step
  before, Stop here drops the idea. Both can be run again: what is on the board already is left alone.
- Only those three folders are committed. Nothing staged is fine: `gates` may have written only a brief, and
  briefs are on the Drive. A rejected push: pull with rebase, push again.
- The stages run like any job of the board, under the day's budget; a stage of the role `master` is never a
  leg's. The last one, the landing, is his word to `tw-master`, as for every lane.
- The roles a route names (`game-designer`, `concept-artist`, `ux`, `ui-artist` and the older ones) must be in
  `Tools/pipeline/roles.json` of the frozen copy: the pipeline refuses an item with a role it does not know, and
  a leg gets its brief from that table. Refused: the frozen copy is older than the route, so `$R update`
  between two runs. Never rename a role on the item to get past it.

## Scored decisions

Every decision gets a score before you say it or write it down: one you ask the owner for, one you take alone, and
one you read in a leg's report (the owner, 2026-10-07: "score decisions on their risk or change, especially
systematic or dangerous decisions"). One point for each yes:

| # | Risk (R) | Change (C) |
|---|---|---|
| 1 | It is hard to undo: landed, deleted, pushed over, sent out | It touches more than one lane or unit |
| 2 | It can break the sim, replays or the gate, or stop other agents' work | It changes a rule, tool, skill or prompt that agents follow from now on |
| 3 | It reaches outside its lane: the board, the Drive, another checkout or machine | It changes what the player sees or how the game plays |
| 4 | It costs over 2% of the week, or takes the day over its cap | It changes a file format, the replay version or a shared name |
| 5 | No script or test can check the result | It overturns an earlier row in `decisions.md` |

Write it `R2 C1`. Then the marks:

- **DANGEROUS**: risk point 1 or 2 is a yes.
- **SYSTEMIC**: change point 2 or 4 is a yes.
- **MAJOR**: R is 3 or more, C is 3 or more, or it carries either mark.

What follows from the score:

- **A major decision is the first line of your reply**, above `RESULT:`:
  `MAJOR DECISION (R4 C3, DANGEROUS): <the decision in one sentence>`. Ask it alone, with its options. It is never
  yours to take, also when "What you may do alone" would let you. Taking up what he answered on the Decide page is
  carrying out his decision, not taking one.
- **A decision that is not major**, taken by you or by a leg: one `CHANGED` bullet that starts with the score, such
  as `- (R1 C0) put look-07 ahead of look-06`. A major one a leg took alone goes on the first line all the same,
  as `MAJOR DECISION TAKEN (...)`, with how to undo it.
- **The row in `decisions.md`** starts with the score and every mark it carries, then the decision in bold:
  `**R4 C3 DANGEROUS MAJOR.** **The relay ...**`.
- **A brief you write for the Decide page** starts its `why` with the same score and marks.
- Between two scores, take the higher. Never lower a score to be able to act alone.
- The steps in "What you may do alone" are not decisions and need no score. A choice between ways of doing the work is.

## Never

- Edit `githubtest-relay-run`, or `Tools/relay/` and `Tools/pipeline/` while a run is going.
- Point `--work` at a checkout a session or an editor is using.
- Start a run without the hold, or keep the hold when you are done (`$R hold <name> --release`).
- Report a run as fine from its exit alone: read the `STOP:` line and the units in the stop record.
