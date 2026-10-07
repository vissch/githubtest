---
name: pipeline
description: Run the two-station agent pipeline for Trench Warfare 3D — see what waits on which station, take and finish stage jobs, explain why a stage is stale or blocked, record the owner's feedback as a request, and start long jobs detached. Use for "/pipeline status", "/pipeline work", "why is X stale", "give feedback on X", "what is the desktop doing". NOT for ordinary lane work outside the pipeline (follow CLAUDE.md) and NOT for driving the editor directly (unity-pipeline skill).
---

# Pipeline (two stations)

Read `docs/reference/stations.md` once: the board, the six states, the commands. `CLAUDE.md` rules win over
anything here. The board repo is `tw3d-board` next to this checkout (`TW_BOARD` overrides).

```bash
P="python trench-warfare-3d/Tools/pipeline/pipeline.py"
R="python trench-warfare-3d/Tools/pipeline/run_detached.py"
```

## Before anything: which checkout am I in

The pipeline runs git only in its own worktrees: `githubtest-desk-show`, `githubtest-desk-sim`, `githubtest-desk-gate`
on the desktop, `githubtest-pipe` on the laptop, and `githubtest-relay-work` for the relay's legs (the `relay` skill). In any other checkout (the owner's lanes, the main clone), refuse
git work and say where to run it. Then `git -C ../tw3d-board pull --rebase -q` so the board is current.

## Commands

| The owner says | Do |
|---|---|
| `status [item]` | `$P status [item]`; add one line per station saying what it can take next (`$P next` with `TW_STATION` set) and, from `$R status`, what is running |
| `why <item>` | `$P why <item>`; for BLOCKED name the stage to run first, for STALE name the input that changed (`git log -1 -- <input>` on the stage's lane) |
| `work` | the loop below, once |
| `add <type> <name>` | write `items/<name>.json` on the board from `trench-warfare-3d/Tools/pipeline/stage_templates/<type>.json` if it exists (otherwise ask the owner for the stages), show it, commit it to the board after the owner agrees |
| `retry <job>` / `cancel <job>` | retry: claim it again (a new attempt). cancel: `$P release` if this station holds it, and `$R stop <run>` for its run |
| feedback on a stage | restate the remark in one sentence, name the earliest stage it affects, then `$P feedback <item> <stage> "<their words, verbatim>" --check "<a measurable check>"` after they confirm the wording |

## Role skills (load the one a stage's `role` names)

| Role | Skill | Station |
|---|---|---|
| balance-simulator | `tw-balance-sim` | laptop (data, ideas), desktop (sweeps) |
| env-simulator | `tw-env-sim` | desktop |
| character-simulator, character | `tw-character-sim` | desktop |
| vehicle-simulator | `tw-vehicle-sim` | desktop |
| destruction-vfx-simulator | `tw-destruction-vfx`, and `tw-vfx-sheets` for new sheets | desktop |
| optimizer | `tw-optimizer` | desktop (counters), laptop (low-end bench) |
| bug-catcher | `tw-bug-catcher` | desktop |
| hard-critic, critic, `/improve-loop` | `tw-critic` | either |
| master | `tw-master` | review either; gate and land on the desktop |
| review-fix | none: its rules are `Tools/relay/roles/review-fix.md`, which the relay adds to the leg's prompt | desktop |
| sim, lowpoly, lane, pipeline | none: follow the stage notes or the unit's goal | as the stage says |

The scripts read the same table from `Tools/pipeline/roles.json`: a stage or a queued unit with a role that is not
in it is refused. A new role goes into both, in one commit; a test holds them to each other. Use the long names for
new stages. Do not rename a role on an item that has results: the role is part of the job id, so the stage would
run again.

Driving the editor and capturing evidence at every zoom band, for all of them: `references/driving-and-evidence.md`.

## The work loop (one job)

1. `$P next` on this station. Nothing: say so and stop.
2. `$P claim <job>`. Refused as busy: another worker on this station has it; stop.
3. Do the stage. RECHECK means run only its checks on the outputs it already has; REGENERATE means the full stage.
   The skill of the stage's role says how (the table above). A job over a few minutes (gate, bench, sweep,
   Blender, ComfyUI) goes through `$R start <job> --timeout <s> --min-headroom-gb 10 -- <cmd>`; poll `$R status`.
4. Commit outputs on the stage's lane (`lane/sim/pipe-<item>` or `lane/show/pipe-<item>`) with
   `gate.ps1 -EditOnly` green first. A path on the seam list (`CLAUDE.md`) stops the job: BLOCKED, and ask.
5. Put evidence on the board under `evidence/<item>/<stage>/`, JPG, at most 400 KB each, one per required band.
6. `$P complete <job> --verdict PASS|FAIL|BLOCKED --evidence <band>=<path> ... --note "<one line>"`.
7. Commit and push the board: `git -C ../tw3d-board add -A && git -C ../tw3d-board commit -m "<job> <verdict>" && git -C ../tw3d-board push`.
   A rejected push: pull with rebase and push again (every file on the board is written by one station only).

## Rules that bite

- Nobody to ask (a watch loop, a headless job): write the question into `decisions.md` "Open" with the options and
  the default you took, finish as BLOCKED, never land on it (`CLAUDE.md`, Asking and remembering).
- Verdicts come from result files and numbers, never from an exit code alone or from looking at a picture.
- Never write evidence or run folders into a checkout: an untracked file changes the tree `land.py` checks.
- Landing is the owner's call. Say what is ready and what the full gate showed; do not run `land.py` unasked.
