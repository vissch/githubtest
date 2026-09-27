# Trench Warfare 3D — working agreement

Unity 6000.0.50f1, Windows x64 only. The Unity project is `trench-warfare-3d/`, not the repo root.

## Start here
Read this file, then sections 1-5 and 8 of `docs/reference/workflow.md` (land, the shared machine, the editor,
compile, test, commit). `docs/reference/tasks.md` is a lookup: search it for your area's heading (task → files →
tests → how to see it); do not read it through. Open anything else only when the list below sends you there.

| You need | Open |
|---|---|
| which file to change, which test guards it | `docs/reference/tasks.md` |
| a command: open the editor, eval, compile, one test, the gate, a screenshot, a build; every `tw eval` helper | `docs/reference/workflow.md` (section 6 lists the helpers) |
| what the owner decided, and what is still waiting on them | `docs/reference/decisions.md` |
| a switch, arg or prefs key | `docs/reference/feature-flags.md` |
| importing, splitting, baking art; any `Tools/` script | `docs/reference/pipelines.md` |
| assemblies, folders, sim system order | `docs/reference/code-map.md` |
| notes other sessions left for you | `docs/inbox/` (one file per note; `health.py` lists them) |
| design of a system (why it is built this way) | `docs/README.md` (index of docs 00-20) |
| known risks and the refactor backlog | `docs/reference/maintainability-audit-2026-09.md` |

## Land checks
```bash
cd trench-warfare-3d && python Tools/health.py
```
It reports the editor lock, memory, your editor, `validate.py`, your lane, and the notes in `docs/inbox/` for you.
`validate.py` fails when a generated table (`python Tools/codemap.py` redoes them), cited path, tool, flag, folder,
test, hook or production file is out of step with the docs. It cannot read prose: when a sentence and the code
disagree, trust the code and fix the sentence.

## Who else is working
```bash
cd trench-warfare-3d && python Tools/health.py --lanes
```
Every checkout on this machine: branch, last commit, drift from integration, uncommitted files, and the files it
would conflict on with yours (a trial merge). Raise a conflict early: a note in `docs/inbox/`, or ask the owner
which lane lands first.

## Lanes
Work out your lane from the current branch before you edit anything: `lane/sim/*` or `lane/show/*`. Any other
name (`lane/rig/...`, `main`, the integration branch): stop and ask which lane you are.

| | **SIM lane** (`lane/sim/*`) | **SHOW lane** (`lane/show/*`) |
|---|---|---|
| Owns | `Assets/_Project/Sim/**`, `Net/**`, `Data/**`, the sim tests | `Presentation/**`, `UI/**`, `Editor/**`, `Perf/**`, `Resources/**`, `Art/**`, `Shaders/**`, `Settings/**`, `Scenes/**`, their tests |
| Never touches | anything SHOW owns | anything SIM owns |

The folder decides the lane, not the topic: `Presentation/Core/SimHost.cs` drives the sim and is SHOW's. The assembly
graph is one-way (`TW.Sim.*` references nothing else), but SHOW code holds the worlds, so it can still desync them.
**SHOW changes the sim only through commands (`SimCommand`) or `SimHost.WriteWorlds(...)`**, never by writing world
arrays. A SHOW task that needs a sim change is split: the SIM part lands first, on a SIM branch.
`TW.Editor` is SHOW's but references every assembly, so a SIM rename that breaks it is fixed in the SIM commit.
A test belongs to the lane of the code it tests. `Tools/**` and `gate.ps1` are shared: change them in a commit of
their own. Docs in `docs/reference/` belong to whoever changes the code they describe.

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
- Lanes branch off `claude/trench-warfare-2d-3d-plan-idt7lf` (the integration branch) and **rebase** onto it. Never
  merge lane to lane, never a merge commit on integration. (`main` is older; do not branch from it.)
- **A conflict during a rebase:** `HEAD` is upstream and "ours"; your commit being replayed is "theirs". In a file
  outside your lane, keep upstream (`git checkout --ours -- <file>`) and redo your change on top only if it is yours
  to make. Shared files (`Tools/**`, `gate.ps1`, `CLAUDE.md`, `docs/`): merge both sides by hand, never `--ours`; in
  `decisions.md` keep every row. In a generated block (`<!-- gen:NAME -->`): keep either side, then
  `python Tools/codemap.py`. A file split into partials: `python Tools/port_split.py <old file> --rebase`.
- **Landing a lane**, only when the owner says so: rebase onto origin's integration branch, run the full gate on
  that exact commit, then `python Tools/land.py`. It refuses a tree no green gate tested, a SHOW lane carrying SIM
  files, and a lane someone landed ahead of, and pushes both branches atomically. Refused: rebase, gate, land.
- **An owner decision** does not wait for its lane: commit it alone (only `decisions.md`), cherry-pick it onto a
  new `lane/show/decision-<date>` cut from origin's integration branch, `python Tools/land.py` there, delete it.
- Push your lane small and often.

## Gate
```bash
# from the repo root, this checkout's editor closed
powershell -NoProfile -ExecutionPolicy Bypass -File gate.ps1 -EditOnly  # before every commit: validate + EditMode
powershell -NoProfile -ExecutionPolicy Bypass -File gate.ps1            # before landing, and after any sim change
```
Exit 0 green, 8 a test failed (printed), 6 no verdict (compile error, or no tests ran), 5 validate.py failed
(printed), 3 project held, 1 unity.exe missing. SIM changes also need the determinism, replay and hash tests green
(EditMode). SHOW changes also need a look in Play at what changed. False reds: `workflow.md`, section 5.

## Asking and remembering
- **A decision only the owner can make:** AskUserQuestion, one decision per question. Write the answer into
  `decisions.md` in the same turn. **Nobody to ask** (a loop, a cron, no reply): add it to the open questions with
  the options and the default you take, go on only with work it does not decide, and do not land on it.
- **A note for another session:** a new file `docs/inbox/<date>-<to>-<topic>.md`, one note per file and one file
  per recipient lane (`<to>` is the lane without `lane/`, `-` for `/`: `show-aosa`; or `all`), so notes never conflict. The receiver
  deletes it when done. Cross-session messages expire unread; the inbox does not.
- **`docs/reference/agent-memory.md`** is a short dated log of incidents that cost time, capped at 150 lines. A
  fact, procedure or decision goes to its reference page instead.

## Unity specifics
- One editor per checkout; never put the project on a synced folder. Commit `.meta` files with their assets.
- Packages change only through the `unity-package-management` skill's Client API script, never by hand.
- Fresh clone: `unity run . -- -nographics -executeMethod TW.Editor.BootstrapSceneBuilder.SetupAll` (in `trench-warfare-3d/`).
- `unity.exe` is at `%LOCALAPPDATA%\unity\bin\unity.exe`. `Tools/tw` finds it and pins every call to this checkout.
- `github-test1/` at the repo root is an unrelated Unity project: leave it out of searches (`--glob "!github-test1"`).
