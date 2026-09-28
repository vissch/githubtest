---
name: tw-housekeeping
description: Keep disk and memory tight on both Trench Warfare 3D stations — know every place the pipeline, gate, benches, gym, captures, Blender and ComfyUI jobs leave files, report their sizes, and prune only the recognised kinds to "newest two of each" (dry run first), never touching a Library, tracked files, a clone, or anything unrecognised. Also guards commit-memory headroom and git-history growth before heavy jobs. Use for "clean up", "how much disk are we using", "we can't keep hundreds of builds", "prune old runs", "is there room for a gate", after every long job. NOT for deleting branches, worktrees or clones (list them and ask the owner).
---

# Housekeeping: disk, memory and repo size

**The owner's words, verbatim:** "Make sure memory managemrnt is tight. We cant keep 100s of builds and temp items on
the drive."

**The rule:** keep the **newest 2 of each kind** below, dry run by default, and delete only on `--apply`. A kind not in
the table is not ours to delete.

## The kinds (and who prunes them)
| Kind | Where | One item is | Pruned by |
|---|---|---|---|
| Gym runs | `%LOCALAPPDATA%\TrenchWarfare\gym\<run>` | a folder holding `gym-run.txt` | the gym itself (keeps its run + 1); `prune.py` as backstop |
| Detached runs | `%LOCALAPPDATA%\TrenchWarfare\runs\<name>` | a folder whose `run.json` state is not RUNNING | `prune.py` |
| CaptureRig sets | `trench-warfare-3d/Captures/`, `Captures/playground/` (gitignored) | the files sharing one stem before `_NN` / the extension | `prune.py`. **Never** `lod*.json`, and never a file a board item cites |
| Gate copies | `trench-warfare-3d/test-results-*.xml` (written in the project folder by `gate.ps1`) | one per mode | `prune.py` keeps the newest per mode |
| AOSA temp files | `%TEMP%\aosa-*.log`, `%TEMP%\aosa-*.xml`, `%TEMP%\aosa-validate.txt` | a file | `prune.py`, **these exact globs only**. The session scratchpad, ComfyUI and the GPU broker also live under `%TEMP%` |
| AOSA builds and cycles | `trench-warfare-3d/Builds/WinBench@<sha>`, `docs/reference/aosa/runs/` | a snapshot | **`python Tools/aosa/aosa.py prune [--apply]` only**. It keeps any baseline a recent run points at. `land.ps1` runs it |
| Live player builds | `Builds/WinBench`, `Builds/WinBenchDev` | the live build | never. The shared copy is **one** zip on the Drive, replaced each time |
| Blender / LOD candidates | the job's run folder | with the run | `prune.py` (detached runs) |
| ComfyUI outputs | the generator's staging folders | a sheet | only a sheet the owner rejected, **with the owner's word**: outputs may be the only copy |
| Board evidence | `tw3d-board/evidence/<item>/<stage>/` | fixed paths, overwritten | nothing to prune; JPG ≤ 400 KB |

**Leftover folders.** Under `Documents/GitHub/githubtest-*`, a folder with **no `.git` at all and nothing inside** (e.g.
an empty `githubtest-desk-decision`) is listed, and the owner asked. Anything with a `.git` (a worktree or a
standalone clone) is never deleted by this skill.

## prune.py (to build)
`trench-warfare-3d/Tools/pipeline/prune.py` is **to build**. It goes in a Tools-only commit, named in `pipelines.md`,
shaped like `aosa.py prune`: `prune.py [--kind K] [--apply]`, a dry-run table first. `run_detached.py` will call it
after each finished job.

**Until it exists, do it by hand:**
```bash
L="$LOCALAPPDATA/TrenchWarfare"; du -sh "$L"/gym/* "$L"/runs/* 2>/dev/null | sort -h
ls -1dt "$L"/gym/*/ | tail -n +3        # gym runs past the newest 2 (only if each holds gym-run.txt)
ls -1dt "$L"/runs/*/ | tail -n +3       # detached runs past the newest 2 (check run.json state first)
python trench-warfare-3d/Tools/aosa/aosa.py prune          # AOSA's own (dry run)
```
Say what you would delete, with sizes, before deleting anything.

## Never delete
- A `Library/`. Rebuilding one costs a full import, and every worktree has its own (2.3-2.8 GB on the desktop).
- Anything tracked by git, or untracked inside a checkout that you didn't make.
- A worktree, clone or branch.
- The newest 2 of a kind, a RUNNING run (`run_detached.py status`), or anything you can't classify.

## Repo and worktree size (the biggest items on disk)
- **Git history grows forever**, with no LFS:
  - a VAT bake adds about **38 MB** (two ~19 MB atlases);
  - each committed FBX or sheet adds its own size.

  Batch bakes, commit only the LODs and sheets the owner approved, and report `git count-objects -vH` (size-pack) in
  status.
- **Worktrees:** at most **six** on the desktop (main, gate, show, sim, skills, vfx). A new one needs a reason. Retire
  one only after its branch is pushed and the owner agrees.

## Memory (commit headroom, not free RAM)
- `python Tools/health.py` reports headroom. **Under 6 GB, run nothing. Under 10 GB, only the batch gate**
  (`workflow.md` §2).
- Heavy jobs go through `run_detached.py --min-headroom-gb 10`.
- **One heavy job at a time on the desktop:** an H3 batch, a full gate, a gym run and a Blender job never overlap.
  Sessions tell each other before starting one (pc-e5 runs the VFX/H3 loop).
- One Unity editor per checkout. Close the ones you are done with: each holds several GB.

## Routine and loop
1. After every long job: the dry run for its kind, then `--apply`.
2. **`/pipeline status` sizes:** `pipeline.py` doesn't report them yet (to build). Use the `du` lines above.
3. Weekly, or on "clean up": a dry run of every kind, a table (kind, count, size, what would go), then apply with the
   owner's word.
4. **Learn** (Brief 2 §B5): a kind that keeps growing, or a file found that no kind covers, goes to the board's
   `lessons/housekeeping.md`. A new kind is added only when the owner approves.

**Desktop, 2026-09-28:**
- C: 780 GB free.
- Main clone and the gate, show and vfx worktrees: 2.3-2.8 GB each.
- The sim and skills worktrees: ~350 MB each.
- Runs: 1.1 MB.
