# Navigation eval: can a fresh agent find the right files?

A check on these docs, not on the game. Fresh agents get a task as a user would phrase it and only this repo, and we
score whether they name the files the real change touched. Run it after a large docs change, or when agents start
asking where things are. The answers below come from real commits, so the task set measures real work.

## How to run it
1. Check out the commit whose docs you are testing into a clean worktree:
   `git worktree add --detach ../githubtest-eval <commit>`.
2. Start one read-only agent per four tasks with the prompt below. Tell it not to run git: other branches hold the
   answers.
3. Score each task: **hit** when the first file it would edit is one the commit changed, and note any file the
   answer needed that no doc named (the agent reports "found via"). Fix the docs for every miss and every grep,
   then try tasks it has not seen: a doc fixed for a task will pass that task.

Prompt (fill in the checkout path and four tasks):
```
You are a fresh coding agent dropped into an unfamiliar Unity game repository, asked to plan four changes. Only
navigate: for each task find the file(s) you would edit, the test(s) you would run, and how you would verify. Do not
edit. Checkout: <path> (the Unity project is trench-warfare-3d/). Start the way the repo tells a new agent to start
(CLAUDE.md) and use its docs as intended; search code when you naturally would. No git commands, no other folders.
Tasks: <four tasks>
For EACH task: edit (paths, most important first) / tests / see it / found via (doc + section, or search) /
steps (tool calls) / confidence / friction. End with DOC PROBLEMS and your total tool calls.
```

## Task set
Paths are under `trench-warfare-3d/Assets/_Project/`. "Set" says whether the docs were already fixed for it.

| # | Task (as asked) | Files the commit changed | Commit | Set |
|---|---|---|---|---|
| 1 | Make a walker's body sit on the ground it stands on, and each step look where it is going | `Presentation/Camera/WalkerGait.cs` | cb68b59 | fixed |
| 2 | Burning fuel pools and pyres should burn out on the sim's clock, like the torch | `Presentation/Camera/Flamethrower.cs` | 1cf6e56 | fixed |
| 3 | The HUD bar overflows with many support cards: count every card in the width | `UI/HudLayout.cs` | 482de9f | fixed |
| 4 | The aiming circle and the in-range readout use different radii: make them one | `UI/Selection/AimReadout.cs` | 34c72d1 | fixed |
| 5 | The debug panel arms abilities the match never fielded: stop it | `Presentation/Camera/TestPanel.cs` | cfdc534 | fixed |
| 6 | Make the render shadow distance a tunable setting | `Presentation/Terrain/Atmosphere.cs` | f8dd628 | fixed |
| 7 | The crater hollow rescan is too slow: less work, same output | `Presentation/Terrain/BattlefieldSurface.cs` | 55d15e0 | fixed |
| 8 | Hide combat-effects overlays (strike discs, aiming circle, banner) in bench image runs | `Perf/PerfBench.cs`, `Presentation/Camera/CombatFx.cs` | 7193323 | fixed |
| 9 | Let the benchmark choose its battlefield from the command line | `Perf/BenchOptions.cs`, `Perf/PerfBench.cs` | ed26a80 | fixed |
| 10 | A launch request names the faction and the ten units it brings | `Presentation/Core/MatchLaunch.cs`, `Presentation/Core/SimHost.cs` | 7847b5f | fixed |
| 11 | Upload the crater colour texture at most every 100 ms | `Presentation/Terrain/GreyboxTerrainView.cs` | aec190d | fixed |
| 12 | A man set on fire before the burning system steps gets no fire: fix it in the sim | `Sim/Combat/Burning.cs`, `Sim/Core/SimEvents.cs` | 0a9af6a | fixed |
| 13 | The picker projects units one by one: project them all with one matrix, behind a setting | `UI/Selection/UnitPicker.cs`, `UI/Selection/SelectionController.cs`, `UI/HudController.cs` | 5378697 | fixed |
| 14 | Night burst smoke looks lit tan by the burst: make it charcoal with a hard toon edge | `Presentation/Camera/FlipbookFx.cs`, `Shaders/Flipbook_URP.shader`, `Presentation/Camera/CombatFx.cs` | dd6bef2 | fixed |
| 15 | Make shell smoke thinner at soldier height at the standard view | `Presentation/Camera/FlipbookFx.cs`, `Presentation/Camera/CombatFx.cs`, `Shaders/Flipbook_URP.shader` | 763664a | fixed |
| 16 | The stress preset piles the army in one spot: spread it over its trenches | `Perf/PerfBench.cs`, `Presentation/Core/ScriptedEnemy.cs`, `Presentation/Core/SimHost.cs` | e6bd10c | fixed |
| 17 | Every gameplay draw call should go through the frame budget | `Presentation/Units/VATRenderer.cs`, `Presentation/Camera/TankRenderer.cs`, `Presentation/Terrain/BattlefieldProps.cs`, `Presentation/Terrain/PropDestruction.cs`, `UI/Selection/SelectionMarkers.cs` | 5932df5 | fixed |
| 18 | The salvage report needs to know which vehicle each wreck was and where it died | `Sim/Units/VehicleModules.cs`, `Sim/Match/Deformation.cs`, `Sim/Match/SectorControl.cs` | a176a01 | fixed |
| 19 | Hide the Debug panel button in bench image runs | `Presentation/Camera/TestPanel.cs` | dd9e3b9 | fixed |
| 20 | Repeatable player-bench stills: hold the clock, choose the tick, hide the HUD | `Perf/BenchOptions.cs`, `Perf/PerfBench.cs`, `Presentation/Terrain/GreyboxTerrainView.cs` | 2c4f3f3 | fixed |

When the docs are fixed for a held-out task, mark it "fixed" and add new held-out tasks from recent commits
(`git log` on any lane; pick commits touching one to five existing code files).

## Results
| Date | Docs at | Tasks | First file right | Right file in first three | Tool calls a task | What it found |
|---|---|---|---|---|---|---|
| 2026-09-27 | e7de2bf | 1-12 | 11 of 12 | 11 of 12 | about 7 | no row for support-fire aiming, sim fire, render settings (the miss), factions; the flamethrower row said the sim has no fire; bench options that exist only on lane/show/aosa were documented as real |
| 2026-09-27 | e7292bd | 13-20, held out | 6 of 8 | 8 of 8 | about 7 | misses: night smoke (went to `BiomeProfile.cs`, which does set the runtime tint; the commit changed the sheet and shader) and wrecks (went to `PropDef.cs` first); a code comment claimed every draw goes through `FrameBudget` and four do not; three SHOW files sat under the SIM heading; no row for the stress preset; capture metrics unexplained |

Every finding in both rounds was fixed in the commit after it. Next run: new held-out tasks, and 13-20 as "fixed".
