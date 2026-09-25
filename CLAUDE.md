# Trench Warfare 3D — working agreement

Unity 6000.0.50f1, Windows x64 only. The Unity project is `trench-warfare-3d/`, not the repo root.
Plan of record: `docs/11-plan-review.md`. It wins over `docs/PLAN.md` and docs 00–10 where they conflict.

Before you touch an area for the first time, skim `docs/reference/agent-memory.md`. It is where the sessions
record what cost them time — gotchas, measurements and dead ends, most of them things that silently did the
wrong thing rather than failing. Append to it, dated, when you learn something that would have saved an hour.

## Two agents, two lanes

Two Claude agents work this repo in tandem, one per machine. **Work out your lane from the current branch
before you edit anything**; if the branch is not `lane/sim/*` or `lane/show/*`, stop and ask which lane you are.

The lane boundary is the assembly graph, and it is one-way: `TW.Sim.*` references nothing outside `TW.Sim.*`,
while Presentation, UI, Data, Net and Perf all reference Sim. So SHOW can never desync the lockstep sim.

| | **SIM lane** (`lane/sim/*`) | **SHOW lane** (`lane/show/*`) |
|---|---|---|
| Owns | `Assets/_Project/Sim/**`, `Net/**`, `Data/**`, `Tests/**` sim cases | `Presentation/**`, `UI/**`, `Editor/**`, `Perf/**`, `Resources/**`, `Art/**` |
| Docs | 01–11, `02-contracts.md`, `03-determinism-rules.md` | 12–20 (art direction, environment, biomes, rig) |
| Gate | `EditMode` + `PlayMode` + determinism/replay/hash tests | `EditMode` + a Play-in-editor look at the thing changed |
| Must not touch | anything in the SHOW column | anything in the SIM column, ever |

`TW.Data` is SIM's (it bakes tables into sim arrays). SHOW consumes it read-only.
`TW.Editor` is SHOW's (importers, bakers, scene builders) — but it references every assembly, so a SIM rename
can break it. That is the SIM lane's problem to fix in the same commit.

## The seam

Three things are shared and are the only real collision risk:

1. **`SimWorld` fields and `SimWorld.Hash()`.** Only the SIM lane edits either. `Hash()` is an ordered chain —
   two branches appending a line each merge textually clean and then produce different hashes, silently
   invalidating every stored replay. **Append at the end of the existing block, never insert**, and re-run the
   determinism and replay tests after *every* merge or rebase, not just before the commit.
2. **`SimConfig`, asmdef reference lists, `ProjectSettings/`, `Packages/manifest.json` + `packages-lock.json`.**
3. **`docs/02-contracts.md`.**

When a SIM change alters a surface SHOW reads — a `SimWorld` array, an enum, a table layout, an archetype id —
push that as **its own commit, alone, first**, say so, and let the SHOW agent rebase before it continues.
Never bundle a seam change into a feature commit.

## Integration

- Both lanes branch off `claude/trench-warfare-2d-3d-plan-idt7lf` and rebase onto it. Never merge lane to lane.
- Push small and often — a few commits, not a session's worth. The other lane is rebasing onto you.
- `git pull --rebase` before every push. On a conflict in a file outside your lane, you rebased over someone
  else's work: take theirs, do not resolve by hand.
- Never edit a file the other lane owns "just to unblock yourself". Ask for a seam commit instead.

## Gate before every commit

```powershell
./gate.ps1              # validate.py + EditMode + PlayMode
./gate.ps1 -EditOnly    # SHOW lane, iterating
```
Exit 8 = a test failed, fix it. Exit 6 = no verdict at all (compile error or licence), which is not a pass.
`validate.py` alone is **not** the gate — see `docs/11-plan-review.md` §2.7.

## Unity specifics

- Two clones on two machines, each with its own `Library/`. Never put the project on a synced folder, never
  open two editors on one path.
- Commit `.meta` files with their assets, and `packages-lock.json` with `manifest.json`.
- Package changes go through the `unity-package-management` skill's Client API script, never by hand-editing
  `manifest.json`.
- Fresh clone setup: `unity run . -- -nographics -executeMethod TW.Editor.BootstrapSceneBuilder.SetupAll`
- `unity.exe` is at `%LOCALAPPDATA%\unity\bin\unity.exe` and is not on PATH in existing shells.
