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
On the laptop, `stations.json` lists no host for it: set `TW_STATION=laptop` once per session, or every `$P`
command that needs a station exits "unknown host".

## Commands

| The owner says | Do |
|---|---|
| `status [item]` | `$P status [item]`; add one line per station saying what it can take next (`$P next` with `TW_STATION` set) and, from `$R status <job>`, whether that job is still running. `run_detached.py` has no list command: name the job from your own `$R start <job> ...` calls, or list `%LOCALAPPDATA%/TrenchWarfare/runs/` (`TW_RUNS` override) to see what is on disk |
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
| game-designer | `tw-game-design` | either |
| concept-artist | `tw-concept-art` | desktop (generated pictures), either (drawn pages, references) |
| ux | `tw-ux` | either (the flow), desktop (the review of what was built) |
| ui-artist | `tw-ui-art` | either |
| master | `tw-master` | review either; gate and land on the desktop |
| gym | `tw-gym`, the proving ground for every role | desktop (runs); either (reading) |
| lowpoly | `tw-lowpoly` | desktop (Blender) |
| housekeeping | `tw-housekeeping`: after every long job | both |
| review-fix | none: its rules are `Tools/relay/roles/review-fix.md`, which the relay adds to the leg's prompt | desktop |
| sim, lowpoly, lane, pipeline | none: follow the stage notes or the unit's goal | as the stage says |

The scripts read the same table from `Tools/pipeline/roles.json`: a stage or a queued unit with a role that is not
in it is refused. A new role goes into both, in one commit; a test holds them to each other. Use the long names for
new stages. Renaming a role, or giving a finished stage a band, does not change its job id: the id hashes the
inputs and the lane, not the role. An input or a lane change does, and the stage runs again.

Driving the editor and capturing evidence at every zoom band, for all of them: `references/driving-and-evidence.md`.

Code review and the second read of a fix have a skill and no role yet: `tw-review` (the relay's role table gets
its row when that line is on integration).

Three of these jobs are also agents a session starts by name (`.claude/agents/`), each with its model and its
skill fixed: `tw-critic` (a critic round, Opus), `tw-reviewer` (a review unit or a second reader, Opus), `tw-scout`
(search and mapping, Sonnet). Start those, not a general agent with a prompt written fresh. Opus and Sonnet are
Claude Code's models, and starting by name is its way: a tool with no named agents reads the file in
`.claude/agents/` and takes that role in a fresh session, on its nearest model (`AGENTS.md` at the repo root).

## The learning loop (Brief 2 §B5; every role runs it)
"Allways make loops of learning and improving" (the owner). Lessons live in **one file**, `tw3d-board/lessons.md`: one
line per lesson, `| date | role | lesson | source |`. Each job:
0. **Read before producing:** `grep -i "<role>" ../tw3d-board/lessons.md | tail -20`. Quote every line that applies in
   your first message. The critic checks it: a recorded lesson repeated is a MAJOR finding.
1. Produce.
2. Evidence from the gym (`tw-gym`) or a bench, noise floor first.
3. A `tw-critic` round **per milestone** (not per small job), rotating its angles.
4. Fix, or rebut in writing. Keep the **best** round, not the last.
5. **Write:** the critic's top finding goes in verbatim, and so does anything that surprised you. A gym run appends its
   own recurring flags (a flag signature seen in two runs in a row) without anyone choosing to.

A lesson in the file three times becomes a proposed checklist line in that role's skill. Put it to the owner as one
question.

## The work loop (one job)

1. `$P next` on this station. Nothing: say so and stop.
2. `$P claim <job>`. It needs `TW_WORKER_PID`, set once per session to a pid that outlives a single command (the
   wrapper pid `$R start` prints, or a long-lived shell's pid); without it the claim is refused. Refused as busy:
   another worker on this station has it; stop.
3. Do the stage. RECHECK means run only its checks on the outputs it already has; REGENERATE means the full stage.
<<<<<<< HEAD
<<<<<<< HEAD
   The stage's role skill (the table above) says how. A job over a few minutes (gate, bench, sweep,
   Blender, ComfyUI) goes through `$R start <job> --timeout <s> --min-headroom-gb 10 -- <cmd>`; poll `$R status`.
=======
   The stage's role agent (`.claude/agents/<role>.md`) says how. A job over a few minutes (gate, bench, sweep,
=======
   The stage's role skill (`.claude/skills/<skill>/SKILL.md`, from the "Role skills" table above) says how. A job
   over a few minutes (gate, bench, sweep,
>>>>>>> c5d8e1eb7 ([K5s] test: SkillDocsTest.test_k5s_docs_say_to_set_tw_station_on_a_hostless_station - pipeline/SKILL.md and tw-master/SKILL.md called pipeline.py/feedback without ever setting TW_STATION, and stations.json lists no host for laptop, so station() exits "unknown host" there; added a setup line to each.)
   Blender, ComfyUI) goes through `$R start <job> --timeout <s> --min-headroom-gb 10 -- <cmd>`; poll
   `$R status <job>` using the same name given to `$R start`.
>>>>>>> e7afff6d8 ([K1s] test: SkillDocsTest.test_k1s_unity_pipeline_warns_against_bare_unity_commands - unity-pipeline/SKILL.md had no warning against bare unity calls/package_add; added a header pointing at Tools/tw and the package-management skill.)
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
  When the owner says land, write `approvals/<lane>.json` on the board in the same turn (`lane`, `date`, `words`: theirs,
  verbatim): the status site lists the lane under "Approved, not landed" until it is in.
