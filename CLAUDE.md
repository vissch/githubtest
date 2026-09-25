# Trench Warfare 3D — working agreement

Unity 6000.0.50f1, Windows x64 only. The Unity project is `trench-warfare-3d/`, not the repo root.

## Start here
Read three files, in order: this one, `docs/reference/tasks.md` (task → files → tests → how to see it), then
`docs/reference/workflow.md` (run, test, see, commit). Open anything else only when the list below sends you there.

| You need | Open |
|---|---|
| which file to change, which test guards it | `docs/reference/tasks.md` |
| a command: open the editor, eval, compile, one test, the gate, a screenshot, a build | `docs/reference/workflow.md` |
| what the owner decided, and what is still waiting on them | `docs/reference/decisions.md` |
| a switch, arg or prefs key | `docs/reference/feature-flags.md` |
| importing, splitting, baking art; any `Tools/` script | `docs/reference/pipelines.md` |
| assemblies, folders, sim system order | `docs/reference/code-map.md` |
| notes other sessions left for you | `docs/reference/inbox.md` |
| design of a system (why it is built this way) | `docs/README.md` (index of docs 00-20) |
| known risks and the refactor backlog | `docs/reference/maintainability-audit-2026-09.md` |

## Land checks
```bash
cd trench-warfare-3d && python Tools/health.py
```
It reports the editor lock, free memory, your editor, `validate.py` and your lane. Then read `inbox.md`.
`python Tools/codemap.py` regenerates the doc tables; `validate.py` fails when a doc no longer matches the code.

## In flight (as of 2026-09-25)
Other checkouts on this machine and what they hold. Update this block when you start or finish something shared.
- `githubtest` (main clone, integration branch): the owner's editor; another session's uncommitted flamethrower work.
- `githubtest-sim` (`lane/show/units-meta`): unit meta work; uncommitted `SlotCount` 8 → 10 (see `inbox.md`).
- `githubtest-aosa` (`lane/show/aosa`) and `aosa-c1-*`: the AOSA optimisation loop, with its own contract in
  docs/reference/aosa/ on that branch.
- `githubtest-maint` (`lane/show/maint-2026-09`): these navigation docs and the maintainability pass.

## Lanes
Work out your lane from the current branch before you edit anything. If the branch is not `lane/sim/*` or
`lane/show/*`, stop and ask which lane you are. The boundary is the assembly graph and it is one-way: `TW.Sim.*`
references nothing outside `TW.Sim.*`; everything else references Sim. So SHOW can never desync the lockstep sim.

| | **SIM lane** (`lane/sim/*`) | **SHOW lane** (`lane/show/*`) |
|---|---|---|
| Owns | `Assets/_Project/Sim/**`, `Net/**`, `Data/**`, the sim tests | `Presentation/**`, `UI/**`, `Editor/**`, `Perf/**`, `Resources/**`, `Art/**`, `Shaders/**`, `Settings/**`, `Scenes/**`, their tests |
| Gate | EditMode + PlayMode + determinism/replay/hash tests | EditMode + a Play-in-editor look at the thing changed |
| Never touches | anything SHOW owns | anything SIM owns |

`TW.Editor` is SHOW's but references every assembly, so a SIM rename that breaks it is fixed in the SIM commit.
A test belongs to the lane of the code it tests (`tasks.md` rows say which). `Tools/**` and `gate.ps1` are shared:
change them in a commit of their own. A task that spans lanes is split: the SIM part lands first.
Docs in `docs/reference/` belong to whoever changes the code they describe; `validate.py` keeps them honest.

## The seam: do not touch without a seam commit
1. **`SimWorld` fields and `SimWorld.Hash()`.** SIM lane only. `Hash()` is an ordered chain: append at the end,
   never insert, bump `ReplayRecorder.FormatVersion` and log it in `docs/02-contracts.md`. Two branches that each
   append a line merge cleanly and hash differently, so rerun determinism and replay tests after every rebase.
2. **`RosterEntry.SlotCount`**, archetype ids, `SimConfig`, asmdef reference lists, `ProjectSettings/`,
   `Packages/manifest.json` + `packages-lock.json`.
3. **`docs/02-contracts.md`**, and the determinism rules in `docs/03-determinism-rules.md`.
4. **Shared presentation contracts:** the float4 house chunk mask in `Toon_URP.shader`, the env atlas grid
   (`BattlefieldKit` ↔ `Tools/envatlas.py`), `VehicleSize` (baked into meshes).

A change to a surface the other lane reads goes in **its own commit, alone, first**. Say so, and let the other lane
rebase before it continues. Never bundle a seam change into a feature commit.

## Integration
- Lanes branch off `claude/trench-warfare-2d-3d-plan-idt7lf` and rebase onto it. Never merge lane to lane.
- A branch without this file was cut before 2026-09-24: `git pull --rebase` onto the integration branch first.
- Push small and often; `git pull --rebase` before every push. A conflict in a file outside your lane means you
  rebased over someone else's work: take theirs.
- Never edit a file the other lane owns "just to unblock yourself". Ask for a seam commit.

## Gate before every commit
```bash
# from the repo root
powershell -NoProfile -ExecutionPolicy Bypass -File gate.ps1            # validate + EditMode + PlayMode
powershell -NoProfile -ExecutionPolicy Bypass -File gate.ps1 -EditOnly  # SHOW lane, iterating
```
Needs this checkout's editor closed. Exit 0 green, 8 a test failed (printed), 6 no verdict (compile error, not a
pass), 3 project held. `validate.py` alone is not the gate. Details and false reds: `workflow.md`, section 5.

## Asking and remembering
- **A decision only the owner can make:** AskUserQuestion, one decision per question. Write the answer into
  `decisions.md` in the same turn.
- **A note for another session:** `inbox.md`. Cross-session messages expire unread.
- **`docs/reference/agent-memory.md`** is a short dated log of incidents that cost time, capped at 150 lines. A
  fact, procedure or decision goes to its reference page instead.

## Unity specifics
- One editor per checkout; never put the project on a synced folder. Commit `.meta` files with their assets.
- Packages change only through the `unity-package-management` skill's Client API script, never by hand.
- Fresh clone: `unity run . -- -nographics -executeMethod TW.Editor.BootstrapSceneBuilder.SetupAll` (in `trench-warfare-3d/`).
- `unity.exe` is at `%LOCALAPPDATA%\unity\bin\unity.exe`. `Tools/tw` finds it and pins every call to this checkout.
