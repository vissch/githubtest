# Maintainability audit, September 2026

What will make this project hard to change or debug, ranked, with the evidence and the fix. Written 2026-09-25 on
`lane/show/maint-2026-09`, after five days of multi-session work took it to 259 C# files in 19 assemblies. Paths are
under `trench-warfare-3d/Assets/_Project/` unless they say otherwise. Re-take the counts with
`python Tools/codemap.py` and the greps named in each row.

**The short version.** The sim is in good shape: one-way assembly graph, no statics, every system hashed. The risk
is in presentation, in the process around shared constants, and in how knowledge was stored. The docs half is fixed
by this pass (see `CLAUDE.md`); the code half is a backlog, in order, at the bottom.

## Ranked risks

| # | Risk | Evidence | Remedy | Lane | Status |
|---|---|---|---|---|---|
| 1 | A seam constant is being changed on the wrong lane | Worktree `githubtest-sim`, branch `lane/show/units-meta`: uncommitted `RosterEntry.SlotCount` 8 → 10; the branch carries 41 `Sim/` files and is stacked on `lane/sim/units-meta`. `SlotCount` has 56 references in 19 files, including the IMGUI `BattleHud`, which no test runs | Land the `Sim/Core` change alone as a seam commit on the SIM lane; rebase the SHOW branch; check `BattleHud` in Play with 10 slots or retire it first (R4) | SIM + SHOW | open, note in `inbox.md` |
| 2 | Knowledge lived in a chronological log | `agent-memory.md` was 356 lines (~70k tokens), the only record of every workflow, with three contradictory test procedures; `docs/04` named four systems that do not exist; README called built systems stubs | Reference pages with one job each, generated tables, `Tools/codemap.py --check` in `validate.py`, memory capped at 150 lines | docs | **fixed this pass** |
| 3 | Statics survive Play and pollute tests | ~85 mutable statics in 27 presentation/UI types; the project reloads the domain on entering Play only; `CameraShake` state made `BlastReactionTests` fail in a live editor | `SceneStatics.ResetSession` on EnteredEditMode; `StaticLifecycleTests` fails on any new unexplained static | SHOW | **fixed this pass** (ratchet in place; per-type resets are R6) |
| 4 | `SceneHooks` is an untyped global bus | 14 static delegates and flags (`Presentation/Core/RenderGround.cs`), 108 references in 17 files; `CloseUp` has three writers (TacticalCamera, CaptureRig, PerfBench); dispatch order depends on script execution order | Wiring table generated into `tasks.md` (done). Replace with per-feature interfaces on one services object, one hook per commit, `CloseUp` first (R2) | SHOW | documented; refactor open |
| 5 | Very large presentation files | CombatFx 1,615 lines, TankRenderer 1,274, Flamethrower 1,141, BattlefieldComposer 1,033, GreyboxTerrainView 1,018, AnimationController 942 | CombatFx split by ownership (done). TankRenderer next (R3). Trigger: any file past 1,000 lines | SHOW | CombatFx **fixed**; rest open |
| 6 | Duplicated constants drift silently | `UnitPicker.FigureScale` 1.5 vs `VATRenderer.UnitScale` 1.125: every pick radius a third too big. `envatlas.py` `SETS` vs `BattlefieldKit.EnvSets` was checked for grid shape, not order | `FigureMetrics` is the one definition; SelectionTests pins picker to renderer. EnvAtlasTests now compares the set lists name by name | SHOW | **fixed this pass** |
| 7 | Determinism is only checked against itself | No stored reference hash: `DeterminismReplayTests` and `SimHashTests` compare two runs of the same build, so inserting a line into `SimWorld.Hash()` passes every test and breaks every stored replay. `FormatVersion` is bumped by hand | A golden-hash test per canary map; a reflection test that snapshots `SimWorld` fields and fails until `FormatVersion` moves (R5) | SIM | open |
| 8 | The gate goes red on noise, and people learn to ignore reds | Two baseline runs on 2026-09-25 failed a different test each ("Sharing violation ... .unity-pipeline-port", "Failed to handle /api/exec request"): the batch run listens on the pipeline port, and unpinned CLI calls reach it | `gate.ps1` reruns failures once only when every failure is that noise, and prints each failure; `tw` pins its project | tooling | **fixed this pass** |
| 9 | Memory exhaustion on the shared workstation | Three editors and a player benchmark left 0.4 GB free of 16; two editors vanished without a crash dump; the owner's editor was paged out and unreachable | `health.py` prints free memory; `workflow.md` says not to open an editor under ~4 GB | process | mitigated |
| 10 | Asset pipelines cannot be reproduced | Blender split flags (`TW_CUT`, `TW_SCALE`, `TW_FLOORS`...) are not recorded in their outputs; sources are in the owner's Downloads; `EnvKitImport` sets Read/Write by folder name | `pipelines.md` records what is known and marks the rest UNRECORDED. Next: write the flags into `houses.json` and each output folder | SHOW | documented; recording open |
| 11 | Per-frame work scales with the battle | ~20 loops over every unit slot per frame (CombatFx, TestPanel, UnitPicker, HudMinimap, QuietFog, SmallLife...); `PropDestruction.Crush` is vehicles × crushable modules × pages; `Strike` queries ~270 rules per explosion; every `CraterStamp` triggers a full prop recomposition that also throws away PropWear's caches | One alive-slot list from `SimPresenter` shared by every consumer (R3); dirty-region recomposition and a rule index (R7). Measure with PerfBench before and after | SHOW | open |
| 12 | Tooling with machine paths and silent side effects | Tools/aosa/ (on its branch) hard-codes this machine's paths; its land.ps1 runs `git checkout --` on the URP settings; the offline compiler resolves unchanged assemblies from the main clone's `Library`, so it can pass against yesterday's dlls | Parameterise paths; make the settings revert loud; name every changed assembly | AOSA | open, note in `inbox.md` |
| 13 | Repository weight and hygiene | HEAD `afc6fe8` is one 531-file commit; 369 MiB of loose objects; no LFS, with a 34 MB FBX and ~19 MB VAT atlases (five versions each in history); `github-test1/` is an unrelated project at the root; CI builds Linux for a Windows-only game | Owner's call (R8): LFS for `*.fbx` / `*.bytes` from now on, `git gc`, remove `github-test1/`, point CI at Windows or remove the build step | owner | open |

## Fixed in this pass
- **Navigation docs:** `CLAUDE.md` rewritten around a three-file reading path; `docs/reference/` gains `tasks.md`,
  `workflow.md`, `code-map.md`, `pipelines.md`, `feature-flags.md`, `decisions.md`, `inbox.md`; `docs/README.md`
  indexes the design docs; HANDOFF.md archived to `docs/archive/handoff-2026-09-22.md`; `agent-memory.md` cut to a capped incident log.
- **Docs cannot silently rot:** `Tools/codemap.py` generates the assembly, folder, system-order, hook-wiring, test and
  flag tables, and `--check` (run by `validate.py`) fails on a stale table, a cited file that does not exist, a test,
  hook, tool or flag no doc routes, or a memory log over its cap.
- **Tools:** `Tools/health.py` (land check), `Tools/shotstats.py` (numbers for a screenshot), `Tools/tw` pinned to its
  own checkout and waiting until an eval answers, `gate.ps1` printing failures and retrying only known noise.
- **Pick radius drift** fixed (`Presentation/Core/FigureMetrics.cs`).
- **`TankCapture.Spawn`** looks the archetype up in the live roster; Banner and Redoubt used to spawn with rifleman stats.
- **Dead stubs removed:** `RagdollPool`/`CorpseBaker`, `TerrainChunkRenderer`/`TrenchKitPlacer`, `UiStubs`
  (`UnitStateIcons`), `MapAuthoringWindow`. Nothing referenced them.
- **Env atlas order** tested; **statics** reset when Play ends and ratcheted by a test; **CombatFx** in five files.
- Commits on `lane/show/maint-2026-09`: pick radius `e693c02`, dead stubs `553566f`, TankCapture `91026d6`, env atlas
  test `55aba9c`, statics and CombatFx split `2c08268`; docs and shared tools follow.
- **Verified so far (2026-09-25):** all 19 assemblies compile offline (Roslyn, with the offline compiler from lane/show/aosa);
  a metadata scan of the built dlls finds exactly the 26 static-holding types `StaticLifecycleTests` accounts for;
  `validate.py` passes. **Not yet run:** the EditMode/PlayMode gate and the Play look, because the workstation had
  under 1 GB free for a second editor. Run `gate.ps1` before this lane merges.

## Checked and found not to be problems
- **`Roster` is not in `SimWorld.Hash()`.** Deliberate: it is written once from the same table on every machine and
  never mutated (comment on `SimWorld.Roster`). It must join the hash the day upgrades or missions change it.
- **`HouseKit.MaxChunks` vs the shader's float4 mask.** Already derived from `BitsPerWord * Words` and tested
  (HouseKitTests).
- **`SeaMargin` 36 m is less than the landing craft's 96 m stand-off.** Documented on the constant: 36 m is the
  scenery coast behind ShelledForest; the coastal level sets 300 m.
- **`TankCapture` writing both worlds by hand.** Null-guarded for single player; the real defect beside it was the
  archetype list (fixed).

## Left alone on purpose
- **`Presentation/VFX/EventVfxRouter.cs` and `Presentation/Audio/EventAudioRouter.cs`** are dead stubs, but each is
  its assembly's only file. Removing them means removing assemblies and editing asmdef reference lists: a seam
  commit of its own. `Net/HashExchange.cs` is dead too and belongs to the SIM lane.
- **Eight unregistered sim systems** (MissionRunner, WaveAi, Logistics, Grenades, IndirectFire, Burning, Stance,
  SpecialAbilities) and four unused order constants are placeholders for planned phases; the generated table in
  `code-map.md` marks each one.

## Backlog, in order
1. **R4 Retire the IMGUI `BattleHud`** after `lane/show/units-meta` merges (it rewrites `BattleHud.cs` and
   HudTextTests). Default the toolkit HUD, drop the PlayerPrefs flag, keep `TestPanel`.
2. **R2 `SceneHooks` → explicit services.** One interface per feature (water, vehicle view, lights, ground marks) on a
   services object cleared by `SceneStatics.ResetSession`. One hook per commit; make `TacticalCamera` the only
   writer of `CloseUp` first. Tests: BlastReactionTests, DebrisTests, TankTests, PropWearTests, before/after stills.
3. **R1 Split `Presentation/Camera` by content** into `Fx`, `Vehicles` and `Debug` assemblies, with `git mv` so GUIDs
   survive. Asmdef lists change, so it is a seam commit. Wait for the flamethrower session to commit.
4. **R3 Split `TankRenderer`**, and share one alive-slot list from `SimPresenter` to collapse the per-frame full-slot
   loops. Measure with PerfBench before and after.
5. **R5 (SIM) Golden hash, and a test that forces `FormatVersion` bumps.**
6. **R6 Give the explained statics real resets** where the reason is weak (Atmosphere's storm flash, DebrisRenderer's
   biome values, RenderGround's map), and make `MatchClock` the only writer of `Time.timeScale`.
7. **R7 Hot paths:** `Crush` prefilter, dirty-region recomposition that keeps PropWear's caches, a rule index for `Strike`.
8. **R8 Owner decisions:** LFS, `git gc`, `github-test1/`, CI target.

## When to audit again
- A file passes 1,000 lines.
- `StaticLifecycleTests` needs a new entry in its explained list.
- A new `SceneHooks` member appears (the generated wiring table in `tasks.md` shows it).
- `validate.py` is edited to skip a `codemap` rule rather than fix what it found.
