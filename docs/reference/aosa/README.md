# AOSA: the optimisation and juice loop

AOSA (automated optimisation and simulation agent) is a Claude session that runs on a fixed cadence on the SHOW lane
(`lane/show/aosa`). Each cycle it measures the game, picks the card worth most, builds a change, judges the change
against measurements, and either commits it to its own branch or reverts it. It also learns from its own record. The
owner merges. Nothing under `Assets/_Project/Sim`, `Net`, or `Data` is ever edited here. Those become proposals in
`ASK.md`.

This file is the contract. A fresh session with no memory runs a cycle from this file, `LESSONS.md`, `BACKLOG.md`,
`JUICE.md` and the tail of `LEDGER.md`. It needs nothing else.

## Files

| File | Written by | What it holds |
|---|---|---|
| `README.md` | owner, retrospective | this contract; rule changes land here with the attempt ids that justify them |
| `LESSONS.md` | every cycle | what a measurement disproved; read FIRST every cycle |
| `BACKLOG.md` | every cycle | the cards (format below) |
| `JUICE.md` | juice director | the board of moments (format below) |
| `ASK.md` | every cycle | owner decisions and SIM-lane proposals, each with the number behind it |
| `LEDGER.md` | every cycle | one row per cycle, derived from that cycle's attempts |
| `attempts.jsonl` | every cycle | one JSON line per attempt; the learning input |
| `priors.json` | `aosa.py learn` | derived; never hand-edited |
| `knobs.json` | `aosa.py learn` | derived knob sensitivities; never hand-edited |
| `budget.json` | `aosa.py budget/refimg` | image spend |
| `agents/*.md` | owner, retrospective | the subagent briefs |
| `runs/<cycle>/` | every cycle | bench json, captures, sheets, reference images |

## One cycle

1. **Read.** `LESSONS.md`, the last 5 `LEDGER.md` rows, `python Tools/aosa/aosa.py status`.
2. **Mode.** `status` says which mode is possible:
   - **P, player only.** This is the default while the loop's own editor is busy. It runs player benches and sweeps,
     writes patches in a scratch worktree, compiles them with `Tools/aosa/occ.py`, and grooms the backlog. The critic
     scores the bench's `shot=` still, and image references can be requested.
   - **E, editor.** The loop has its own editor on THIS worktree's project (`githubtest-aosa/trench-warfare-3d`, with
     its own `Library/`). Enter this mode when `status` says that project is FREE. Do NOT claim the shared slot in
     `%LOCALAPPDATA%\TrenchWarfare\editor-slot.json`: that slot is the main clone's queue, and claiming it would make
     other sessions wait for nothing. Mode E lands staged patches, runs the gate, takes three-tier captures with
     `CaptureRig`, runs `Diff`, and reads `FrameBudget`. Close the editor after the commit, so a player bench is not
     measured next to a busy editor.
   - **B, rebuild.** Use it when `status` says the loop's own player (`Builds/` in this tree) is missing or older
     than HEAD. It rebuilds the release and development players. The first build on this tree imports `Library/`
     from nothing, which takes a long time.

   **Machine noise.** Other sessions' editors on the main clone share this machine's CPU and GPU. A player bench
   taken while they compile or play is noisy. `aosa.py status` lists the Unity processes. When one is busy, bench
   anyway and rely on the interleaving and the band (rule 2), and note the load in the ledger row.
3. **Pick.** `aosa.py pick` ranks the cards. Take at most 3 (the WIP cap) and at most one of them `class: juice`.
4. **Fan out in ONE message** (Agent tool, parallel). The agents are the diagnoser, the critic, the juice director and
   one patch author per picked card. Only the lander writes to `Assets/` or talks to the editor, and there is only
   ever one lander.
5. **Judge** each attempt with the acceptance rules below. Commit it or revert it.
6. **Record.** Write one `attempts.jsonl` line per attempt, run `aosa.py learn` and `aosa.py age`, add the ledger row,
   and add to `LESSONS.md` if anything was disproved.
7. **Every 10th cycle** run the retrospective agent (`agents/retrospective.md`).

## Tiers (never change these)

| Tier | Camera | Notes |
|---|---|---|
| T1 STANDARD | zoom 30, fov 25, pitch 25, yaw +21 | the owner's law; the budget is judged here |
| T2 MID | zoom 14-18 | a platoon and its bay |
| T3 CLOSE | zoom 6-9, 42 deg lens, ~1.1 m up | only `_TWClose` detail lives here |

Close-tier detail must sit behind `_TWClose` / `SceneHooks.CloseUp` and cost nothing at T1.

## Acceptance rules

A change lands only if every rule holds. The rule number goes into the revert reason.

1. **Same battle.** `window.hash_start` is equal in A and B. For a presentation-only change, `window.hash_end` must
   also be equal. The SHOW lane cannot change the sim, and `hash_end` proves it did not.
2. **Noise band.** Run at least 3 repeats, interleaved A B A B A B. A delta counts only when it exceeds the band for
   that metric in `priors.json` (per metric, per build type). With fewer than 3 prior samples, use the spread of this
   run's repeats.
3. **Perf.** The target metric improves by more than the band. No other of `main_ms`, `gpu_ms`, `draw_calls`,
   `setpass`, `gc_bytes`, `frame_budget_draws`, `frame_budget_vertices` at p95 rises by more than its band.
4. **Fidelity floor.** `alive_end`, `vat_vertices` and `vat_shadows_on` do not fall. A perf change may not win by
   drawing less.
5. **Image class.** A "same image" change needs `CaptureRig.Diff` `changed_frac` < 0.001 on the held T1, T2 and T3
   stills. An "indistinguishable" change needs the critic to score the after sheet no lower on any line it can see. A
   "juice" change needs the critic's moment score to rise and the moment's readability check to pass.
6. **Readability never drops.** If the critic says the men, trench lines or orders read worse, the change is reverted
   whatever else it did.
7. **T1 budget never rises.** `FrameBudget` draws and vertices at T1 must not rise unless the same commit pays for it.
8. **Gate.** Presentation-only changes need `gate.ps1 -EditOnly`, or `run_tests` EditMode after
   `RequestScriptReload` in a live editor. Anything under `Perf/`, `Presentation/Core/`, or touching the lockstep
   loop needs the full gate with the canary on. Exit 6 is not a pass.
9. **Predicted first.** An attempt without a predicted metric and delta written BEFORE the measurement is void.

Commit one change per commit on `lane/show/aosa`. The message carries the A/B table and the run labels, and ends with
the attribution line. Never push to the plan branch, and never merge lane to lane.

## Cards (`BACKLOG.md`)

Each card is one line in the table:

`| id | class | tier | metric: now -> target | predicted delta | evidence | size | owner | age | idle | status |`

- **class** is one of `cull cap cache batch shader budget-sweep instrument juice art sim-proposal`.
- **owner** is `loop`, `sim`, `owner` or `art`. Cards not owned by `loop` go to `ASK.md` and leave the backlog.
- **age** counts cycles since the card was created. **idle** counts cycles since the card last moved a measured
  metric. `aosa.py age` updates both.
- **status** is `ready`, `wip`, `blocked:editor`, `parked:<reason>` or `done:<attempt>`.

Rules that stop cards standing still:
- WIP is capped at 3. A card with status `blocked:editor` does not count against the cap.
- At **idle 2** the next attempt on the card must be a measurement, not a change.
- At **idle 3** the card is split into (a) the one measurement that decides it, (b) the smallest change that could
  move its metric, and (c) the rest, parked. The children start at idle 0. A card may be split only once. A child
  that reaches idle 3 is parked with a named blocker.
- A card closes only on a ledger row with an A/B or critic delta. An argument never closes a card.

## Self-learning

- **`attempts.jsonl`, one line per attempt:**
  ```json
  {"id":"a0007","cycle":7,"card":"C12","class":"cull","files":["Presentation/Camera/TankRenderer.cs"],
   "knob":null,"predicted":{"metric":"gpu_ms.p95","delta":-0.4},
   "measured":{"metric":"gpu_ms.p95","delta":-0.31,"band":0.12,"runs":["c7-a1","c7-b1","c7-a2","c7-b2","c7-a3","c7-b3"]},
   "verdict":"landed","rule":null,"commit":"abc1234",
   "cost":{"cycles":1,"editor_min":14,"usd":0.0},"lesson":null,"owner_verdict":null}
  ```
  `verdict` is `landed`, `reverted`, `void` or `measure-only`. `rule` holds the acceptance-rule number that caused a
  revert.
- **`aosa.py learn`** derives `priors.json` from the attempts:
  - per class and per file: `n`, `hit_rate`, `median_gain`, `median_cost_cycles` and `calibration` (the median of
    measured over predicted)
  - per metric and build type: `band`, the median absolute deviation across interleaved repeats times 3
  - per knob: `sensitivity`, the slope of metric against value from sweeps, which is written to `knobs.json`
- **`aosa.py pick`** ranks cards by `hit_rate x |predicted| / cost`. Unknown classes get the global prior. A class
  whose calibration falls outside [0.5, 2] gets its change cards demoted below an `instrument` card for that class,
  because a model that mispredicts needs measuring, not more changes.
- **Lessons.** When a measurement disproves an assumption, or a revert happens for a reason no rule predicted, add
  one line to `LESSONS.md` that names the attempt id. Also append it, dated, to the main clone's
  `docs/reference/agent-memory.md`, which is untracked and exists only there.
- **Rules evolve.** When the same revert reason appears twice, the retrospective proposes a rule or an instrument
  card. The rule change is a commit to this file that cites the two attempt ids.
- **Owner calibration.** Owner reactions go into `owner_verdict` (`merged`, `reverted`, or a quote). The critic's
  brief includes the last ten, and the "indistinguishable" class is trusted only while critic and owner agreed on the
  last five.

## Knobs

`TW.Presentation.Knobs` (in `Presentation/Core/Knobs.cs`) makes a presentation constant settable at run time without
a rebuild. The inputs are `-twknob "a=1,b=2"` on the player command line, the `TW_KNOBS` env var, or `knobs=a=1|b=2`
inside a `-twbench` string. Knobs are read once, in `Awake`/`Start`, never per frame. When nothing is set, every value
equals the old constant and the build behaves identically (`KnobsTests`). The bench writes the knobs that were read
and the ones that were set into `config.knobs`, so a report always says what it measured.

A sweep is `aosa.py bench <label> --player --knobs vat.lodDistance=90` at three values. The slope goes to
`knobs.json`, and the patch that changes the default is a normal card, judged normally.

## Scenarios

`scenario=` in the bench string picks what happens in the measured window. The default is `none`: the stress battle
alone. The other values are `barrage` (HE barrages on both front lines), `armour` (each side's vehicles fielded into
the fight) and `vfx` (gas, barrage and a star shell stacked in view). Every scenario is issued as sim commands after
`hash_start` is recorded, so the battle before the window is untouched, and `hash_end` then fixes what the scenario
did.

## Image references and the budget

`budget.json` holds `daily_cap_usd` (default 3.00), split `juice` 2/3 and `tier` 1/3. `aosa.py refimg <capture>
--for juice|tier --prompt-file <txt>` refuses when that share is spent for the day. On success it charges the actual
price, falling back to 0.35 per image if pricing cannot be read. It calls `fal-ai/nano-banana-pro/edit` with the
capture as `image_urls` and `num_images` 2, writes `runs/<cycle>/refs/`, and logs a `class: juice` or `class:
art` attempt.

A reference image goes to the critic beside the capture. It is never a pixel target: `CaptureRig.Diff` is the only
pixel metric, and only between two of our own captures. A moment gets at most two reference rounds without a critic
delta, and after that it is split or parked.

## Juice director

The brief is in `agents/juice-director.md`. The juice director owns `JUICE.md`. Each cycle it names the one moment on
screen a player would screenshot and the cheapest change that would make it land harder. It spends the juice share
of the image budget. It never lands anything itself: its cards go through the same patch authors, lander and rules as
any other card, with rule 6 (readability) as a hard condition on every moment.

## Tools

| Command | Does |
|---|---|
| `python Tools/aosa/aosa.py status` | editor lock, build freshness, budget left, WIP; exit 0 always |
| `aosa.py bench <label> [--player/--editor] [--scenario S] [--knobs k=v,...] [--repeats N] [--against <label>]` | runs benches, interleaving A/B when `--against` is given; writes `runs/` |
| `aosa.py compare <A> <B> [--metric M]` | the cmp.py table plus the verdict for rules 1-4 |
| `aosa.py attempt add <json>` / `aosa.py learn` / `aosa.py pick` / `aosa.py age` | the learning record |
| `aosa.py budget` / `aosa.py refimg ...` | image spend |
| `aosa.py retro` | the 10-cycle summary the retrospective starts from |
| `python Tools/aosa/occ.py <assemblies...>` / `--changed` | offline compile of whole assemblies from this tree |
| `Tools/aosa/bench.sh`, `player_bench.sh`, `cmp.py` | the perf pass's drivers, kept here so the branch carries them |
| `AgentScripts/aosa_*.cs` | eval snippets for the editor (`unity command eval_file <abs path>`) |

## Standing constraints

- The loop opens only its own editor, on this tree. It never opens the main clone's project and never claims the
  shared slot. Never refresh or recompile while
  `EditorApplication.isPlaying` is true under someone else's session.
- Never `EditorApplication.update = null`.
- A value read back from an editor eval is not evidence about the shipped path. When a number and a screenshot
  disagree, the screenshot is right. Look at the bench's `shot=` before trusting player numbers.
- Release and development numbers are never compared with each other.
