# Two stations: the job board

The laptop cannot run every simulation, so pipeline work is split: the desktop (`DESKTOP-NLJRH7R`, RTX 5090) runs
every Unity, Blender and ComfyUI job, the gate and landings; the laptop takes ideas, review and feedback. Which host
is which station is in `trench-warfare-3d/Tools/pipeline/stations.json` (or set `TW_STATION`).

## The board

A separate private repo, `tw3d-board`, cloned next to each checkout (`TW_BOARD` overrides the path). It holds what
changes too often to land: `items/` (one definition per item), `results/`, `feedback/`, `claims/`, `evidence/`.
Nothing on it stores a status. `Tools/pipeline/pipeline.py` works every state out from the repo and the results.

An item lists stages. A stage names its `station`, the lane ref it reads (`lane`), its `inputs` (a repo path, or
`path#/json/pointer` for one field), its `outputs`, the stages it comes `after`, and the zoom `bands` a PASS must
bring evidence for. A stage may not read its own outputs.

| State | Meaning | Next |
|---|---|---|
| DONE | a PASS for the current inputs and the current upstream results | nothing |
| RECHECK | its own inputs are unchanged, an upstream result is new | rerun its checks on the outputs it has |
| STALE | an input changed since its last result (a feedback request counts as an input) | regenerate |
| READY | never run, or the latest attempt failed | run it |
| IN_PROGRESS | a live worker holds it | wait |
| BLOCKED | a stage it comes after is not DONE | `pipeline.py why <item>` |

One worker per station: a claim names the worker's process and its start time, and is taken over only when that
process is gone. The latest attempt of a job decides, so a failed rerun outranks an earlier PASS. Closing a feedback
request does not make its stage stale again.

## Commands

```bash
python Tools/pipeline/pipeline.py status [item]
python Tools/pipeline/pipeline.py why <item>
python Tools/pipeline/pipeline.py next
python Tools/pipeline/pipeline.py claim <job>
python Tools/pipeline/pipeline.py complete <job> --verdict PASS --evidence t1=evidence/<item>/t1.jpg
python Tools/pipeline/pipeline.py feedback <item> <stage> "<the owner's words>" --check "<a measurable check>"
```

The pipeline follows `CLAUDE.md`: stages commit on `lane/sim/pipe-<item>` or `lane/show/pipe-<item>`, seam changes are
seam commits, and nothing lands without the owner. A job with nobody to ask records an open question in
`decisions.md` and does not land on it.

The relay (`docs/reference/relay.md`) takes this station's jobs unattended: its runner claims a job, runs a plan leg
and execute legs in its own worktree (`githubtest-relay-work` on the desktop), checks the evidence by script and
writes the result. It never takes a master stage, and its leg records and stop reasons go to `relay/<station>/` on
the board.
