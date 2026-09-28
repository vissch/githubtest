---
name: tw-housekeeping
description: Keep disk and memory tight on both Trench Warfare 3D stations — know every place the pipeline, gate, benches, gym, captures, Blender and ComfyUI jobs leave files, report their sizes, and prune to "newest two of each kind" (dry run first), without ever touching a Library, a checkout's tracked files or anything unrecognised. Also guards commit-memory headroom before heavy jobs. Use for "clean up", "how much disk are we using", "we can't keep hundreds of builds", "prune old runs", "is there room for a gate", after every long job. NOT for deleting branches or worktrees with work in them (ask the owner).
---

# Housekeeping: disk and memory

**The owner's words:** "Make sure memory management is tight. We can't keep hundreds of builds and temp items on the
drive." The rule: **keep the newest 2 of each kind**, dry run by default, and delete only on `--apply`.

## What piles up, and where
| Kind | Where | Written by | Pruned by |
|---|---|---|---|
| AOSA build snapshots (`WinBench@<sha>`) and cycle frames | `trench-warfare-3d/Builds/`, `docs/reference/aosa/runs/` | `aosa.py`, `BuildWindows` | `python Tools/aosa/aosa.py prune [--apply]`; `land.ps1` runs it |
| Live player builds | `Builds/WinBench`, `Builds/WinBenchDev` (gitignored) | `BuildWindows.CommandLine` | never; they are live. The shareable copy is **one** zip on the Drive, replaced, never piled up |
| Detached runs | `%LOCALAPPDATA%\TrenchWarfare\runs\<name>` | `run_detached.py` | `prune.py` |
| Gym runs | `%LOCALAPPDATA%\TrenchWarfare\gym\<date>-<sha>` | `Gym.Run` | the gym itself keeps 2; `prune.py` as a backstop |
| Captures | `trench-warfare-3d/Captures/`, `Captures/playground/` (gitignored) | CaptureRig, `pg.sh` | `prune.py` (newest 2 **sets**, by run prefix) |
| Gate results | `test-results-<mode>.xml` in the project, `%TEMP%` gate index, `land.ps1` logs | `gate.ps1`, `land.ps1` | `prune.py` (`%TEMP%` items older than 2 days) |
| Blender and LOD candidates | the job's run folder | `tw-lowpoly` | with the run folder |
| ComfyUI outputs | the generator's staging folders | `tw-vfx-sheets` | only a sheet the owner rejected, **with the owner's word**, because outputs may be the only copy |
| Board evidence | `tw3d-board/evidence/<item>/<stage>/` | the pipeline | fixed paths, overwritten; JPG ≤ 400 KB |
| Leftover worktree folders | `Documents/GitHub/githubtest-desk-*` with no `git worktree list` entry (e.g. an empty `githubtest-desk-decision`) | a removed worktree | `prune.py` lists them; delete one only if it is empty or its contents are in git |

`prune.py` (`trench-warfare-3d/Tools/pipeline/prune.py`) is to be built, in a Tools-only commit named in
`pipelines.md`, following `aosa.py prune`'s shape. Until it exists, do the same steps by hand with `du`, and say what
you would delete before deleting anything.

## Never delete
- A `Library/` folder. Rebuilding it costs about 9 minutes and a fresh import, and each worktree has its own.
- Anything tracked by git, or untracked inside a checkout that you didn't make yourself. List it and ask.
- A worktree or branch with commits that are not pushed.
- The newest 2 of any kind, a run that is still RUNNING (`run_detached.py status`), or anything you can't classify.

## Memory (commit headroom, not free RAM)
- `python Tools/health.py` reports headroom. **Under 6 GB, run nothing. Under 10 GB, only the batch gate**
  (`workflow.md`).
- Heavy jobs go through `run_detached.py --min-headroom-gb 10`, which refuses to start below that.
- **One heavy job at a time on the desktop:** never an H3 batch next to a full gate, a gym run or a Blender job. The GPU
  broker trips above about 108 % commit.
- One Unity editor per checkout. Close editors you are done with: each holds several GB.

## Routine
1. After every long job: `prune.py` dry run, then `--apply` for its own kind.
2. `/pipeline status` shows each kind's size and the free disk.
3. Weekly, or on "clean up": a dry run of every kind. Show the table (kind, count, size, what would go), then apply
   with the owner's word.

**Desktop, 2026-09-28:** C: had 780 GB free. The main clone, gate, show and vfx worktrees were 2.3-2.8 GB each (the
Library dominates). sim and skills were about 350 MB, and runs were 1.1 MB.
