> Snapshot read at integration 558c667 (2026-09-28). file:line citations drift: re-grep the symbol before editing. The code wins over this page.

Path keys: R = repo root, P = trench-warfare-3d/Assets/_Project, T = trench-warfare-3d/Tools, D = docs.

## B. OPTIMIZER (the AOSA loop)

### Contract (`D\reference\aosa\README.md`)
- **Mandate** (:3-7): a Claude session on `lane/show/aosa`. It never edits `Sim/`, `Net/` or `Data/`; those become proposals in `ASK.md`.
- **Files** (:12-27): README, LESSONS, BACKLOG, JUICE, ASK, LEDGER, `attempts.jsonl`, `priors.json` and `knobs.json` (derived, never hand-edited), `budget.json`, `agents/*.md`, `runs/<cycle>/`.

**One cycle** (:29-70)
1. **Read** LESSONS, the last 5 LEDGER rows and `aosa.py status`.
2. **Mode** (:32-44):
   - **P** (player only): benches, sweeps, offline compile.
   - **E** (the loop's own editor, only when `status` says this project is FREE). Never claim the shared slot `%LOCALAPPDATA%\TrenchWarfare\editor-slot.json`.
   - **B** (rebuild the players when stale).
3. **Pick** with `aosa.py pick`: at most 3 cards (WIP cap), at most one of them `juice`.
4. **Fan out in ONE message**: diagnoser, critic, juice director, one patch author per card. Only one lander ever. "Each agent closes its own card" (:53-63), and `git worktree list` shows no `aosa-*` afterwards.
5. **Judge** against the acceptance rules.
6. **Record:** `attempt add`, `learn`, `age`, the ledger row, LESSONS. Lessons are also appended to the main clone's `agent-memory.md`.
7. **Every 10th cycle**, the retrospective.

**Camera tiers** (:72-80, "never change these")
- **T1:** zoom 30, fov 25, pitch 25, yaw +21. The budget is judged here.
- **T2:** zoom 14-18.
- **T3:** zoom 6-9, a 42° lens, ~1.1 m up. Close detail sits behind `_TWClose` / `SceneHooks.CloseUp` and costs nothing at T1.

**Acceptance rules 1-10** (:82-113)
1. **Same battle:** `window.hash_start` equal. For a presentation-only change `hash_end` must be equal too.
2. **Noise band:** at least 3 repeats interleaved A B A B A B. A delta counts only past the band in `priors.json`, per metric and build type. With fewer than 3 priors, use this run's spread. A band is never tighter than the widest spread either side shows.
3. **Perf:** the target improves past its band. No other `main_ms`, `gpu_ms`, `draw_calls`, `setpass`, `gc_bytes`, `frame_budget_draws` or `frame_budget_vertices` p95 rises past its band.
4. **Fidelity floor:** `alive_end`, `vat_vertices` and `vat_shadows_on` do not fall.
5. **Image class:**
   - "same image" needs `CaptureRig.Diff` `changed_frac` < 0.001 on held T1, T2 and T3;
   - "indistinguishable" needs the critic to score no line lower;
   - "juice" needs the moment score to rise and its readability check to pass.
6. **Readability never drops.** Judged blind against the *current default* before the commit (C98, a0032).
7. **T1 budget never rises** unless the same commit pays for it.
8. **Gate:** presentation-only changes need `gate.ps1 -EditOnly`. `Perf/`, `Presentation/Core/` or the lockstep loop need the full gate with the canary. Exit 6 is not a pass.
9. **Predicted first.** Without a prediction written before the measurement, the attempt is void.
10. **Instrument cards:** rule 3 is replaced by "`main_ms`, `gpu_ms`, `draw_calls` and `setpass` stay within band, and the card's own metric lands within 2x of its prediction" (a0007/C31).

Commit one change per commit on `lane/show/aosa`, with the A/B table (:115-116).

**Cards** (:118-136)
- Row format: `| id | class | tier | metric: now -> target | predicted delta | evidence | size | owner | age | idle | status |`.
- Classes: `cull cap cache batch shader budget-sweep instrument juice art sim-proposal`. Owner is `loop`, `sim`, `owner` or `art`.
- Status: `ready`, `wip`, `blocked:editor`, `parked:<reason>`, `done:<attempt>`.
- **WIP cap 3** (blocked:editor is exempt).
- **idle 2:** the next attempt must be a measurement.
- **idle 3:** split into (a) the one deciding measurement, (b) the smallest change, (c) the rest, parked. Split only once. A child that reaches idle 3 is parked with a named blocker.
- A card closes only on a ledger row with an A/B or critic delta.

**Self-learning** (:138-165)
- attempts JSON schema (:141-147).
- `learn` derives per class and per file: n, hit_rate, median_gain, median_cost_cycles, calibration. Bands are 3·MAD. Knob sensitivities are slopes.
- `pick` ranks by `hit_rate × |predicted| / cost`. A class whose calibration is outside [0.5, 2] is demoted below the instrument cards.
- The "indistinguishable" class is trusted only while the critic agreed with the owner on the last 5 verdicts.

**Knobs** (:167-176)
- `TW.Presentation.Knobs` (`P\Presentation\Core\Knobs.cs`).
- Inputs: `-twknob "a=1,b=2"`, the env var `TW_KNOBS`, or `knobs=a=1|b=2` inside `-twbench`.
- Read once in `Awake`/`Start`. With nothing set, every value equals the old constant (`KnobsTests`).
- A sweep is `aosa.py bench <label> --player --knobs vat.lodDistance=90` at 3 values.

**Scenarios** (:178-184): `scenario=` none | barrage | armour | vfx. They are issued after `hash_start`.

**Image budget** (:186-196)
- `daily_cap_usd` 3.00, split juice 2/3 and tier 1/3. Fallback price $0.35 per image.
- The endpoint is `fal-ai/nano-banana-pro/edit`, `num_images` 2.
- A reference is never a pixel target. At most two reference rounds per moment without a critic delta.

**Standing constraints / what the loop never does** (:230-247, and `agents\cycle.md:62-68`)
- Everything runs in the background: the player is `SW_SHOWNOACTIVATE`, below-normal priority, pushed to the bottom of the z-order. Builds and tests are `-batchmode` only. No visible editor. Captures come from the player's `shot=`.
- It never opens the main clone's project and never claims the shared slot.
- It never sets `EditorApplication.update = null`.
- It never edits `Sim/`, `Net/`, `Data/`, asmdefs, `ProjectSettings/` or `Packages/`.
- It never pushes anywhere but `origin lane/show/aosa`.
- It never closes a card on an argument.
- It never compares release with development numbers. When a number and a screenshot disagree, the screenshot wins.

### The seven agent briefs (`D\reference\aosa\agents\`)
- **`cycle.md`** (the orchestrator, ≤20 min per cycle):
  - Read, `status`, `pick`, then `git fetch && git rebase origin/claude/trench-warfare-2d-3d-plan-idt7lf`.
  - Write each prediction before measuring. Fan out.
  - Judge with `aosa.py bench <label> --player --against <base> --repeats 3` and `aosa.py compare <label> <base> --json` (rules 1-4). Rules 5-8 come from the critic and the gate.
  - Record: `attempt add`, `learn`, `age --moved`, `ledger add`, LESSONS. Then commit the docs.
- **`diagnoser.md`** reads `tw-perf/1` reports. It:
  - voids a run whose `shot=` is not the standard view;
  - reports `warnings`;
  - checks `hash_start`;
  - ranks main, gpu, draws, setpass, gc and `per_tick_ms` against budget;
  - names the carrying marker;
  - turns a `TW.Sim.Sys.*` cost into an ASK item.

  Output: `VALID / TOP / NEW CARDS / ASK / STALE CARDS`.
- **`critic.md`** scores captures harshly (full detail in D).
- **`juice-director.md`** owns JUICE.md and two thirds of the image budget.
  - It picks at most one moment and writes three lines of look (value and hue, motion and timing, size).
  - It asks for a reference image only when there is a real capture this cycle, money is left, and the moment has had fewer than 2 rounds.
  - Its card is class `juice`, predicted +1/+2, touching only CombatFx, FlipbookFx, NightLights, Atmosphere, SceneMood, TankRenderer, DebrisRenderer, Storm or the shaders.

  Output: `MOMENT / LOOK / READABILITY CHECK / REFIMG / CARD / ASK`.
- **`lander.md`** is the only agent that touches the editor or the branch. It:
  - runs `status` first; an editor not FREE means `NO SLOT`;
  - runs `git apply`, then `occ.py --changed`;
  - gates, then builds with `Unity.exe -batchmode -quit -projectPath <wt> -executeMethod TW.Editor.BuildWindows.CommandLine [-twdev]`;
  - runs the A/B;
  - takes captures with `CaptureRig.Hold(120)` after stepping, then `Shot` via `AgentScripts/aosa_tiers.cs`, then `Diff`;
  - commits or reverts, one change per commit.

  Output: `SLOT / <card>: LANDED|REVERTED rule n|NOT RUN / GATE / BENCH / CAPTURES / RELEASED`.
- **`patch-author.md`** turns one card into one patch in its own worktree. It:
  - greps the declaration first; `PREMISE FALSE` is a valid result;
  - prefers `SWEEP <knob> <v1,v2,v3>` when the constant is a knob;
  - keeps to the SHOW lane, with a `// Phase:` header, ASCII only, original line endings, and a `.meta` with a fresh GUID for any new file;
  - compiles with `occ.py --changed` and runs `validate.py`;
  - chooses the image class honestly and names or writes a regression test.

  Output: `CARD/RESULT/PREDICTION/IMAGE CLASS/FILES/DIFF/OCC/TEST/RISK`.
- **`retrospective.md`** (every 10 cycles) runs `aosa.py retro` and answers: what paid, what stalled, repeated revert reasons, calibration outliers, image-budget split, critic vs owner agreement, cadence and WIP. It outputs patches to README, BACKLOG and budget.json plus a 10-line summary.

### ASK / LESSONS / JUICE
**`ASK.md` open items**
- A01 merge policy
- A02 metric ranking; default main p95, then gpu p95, then T1 critic
- A03 explosions batches B and C
- A04 coast obstacles
- A05 ruins
- A06 Redoubt legs
- A07 men's shadows (`vat.vertexBudget` 1.5 M → 3.4 M doubles VAT vertices, +0.3 ms GPU p95; answered "no at night")
- A08 (answered: option b, below)
- A09 tracer depth vs weight
- SIM proposals: S01 TargetAcquisition 0.80 ms/tick; S02 FlowField 3.07 ms peak; S03 the 10.5 s stress test; S04 (answered); S05 fill `Explosion.Dir`

**`LESSONS.md` top lessons**
- A blind critic's count was a quarter low. Count events from a log (a0041, :9).
- Say what an instrument cannot see (:10-11).
- `GC.GetAllocatedBytesForCurrentThread` reads 0; use `AllocProbe` (:12-13).
- Grep every draw API before trusting `FrameBudget` (:14-17).
- Only the player saw the missing shaders; look at `shot=` (:18-19).
- Drift of 1.3 ms GPU: interleave (:20-21).
- Render-to-texture captures bypass post-processing (:22-23).
- Hold the clock with `CaptureRig.Hold`, never pause (:24-26).
- An eval value is not evidence about the shipped path (:27-29).
- One loaded run can veto a clear win; predict on sums or p99, never `max` (:33-38).
- Held-clock draw counts vary about 0.25%; use a 0.3% band (:39-43).
- Release main p95 is noise-bound: judge CPU on dev `per_tick_ms` and p50, GPU on release p95 (:46-49).
- A player build churns three URP assets; `git checkout --` them (:77-79).
- `occ.py` single-assembly builds compile against old dlls; use `--changed` (:88-90).
- Critics must run in the foreground: wait for the critic's report before going on (in Claude Code: `run_in_background: false`) (:110-114).
- Split whatever `pick` marks SPLIT first (:116-119).
- Parts that pass alone fail together; test the exact set that ships (:123-127).
- Measure figure/ground before the critic on a shared-look change (:129-135).
- A sweep winner is not a pass (:137-141).
- An image card names its event's tick range from the code (J01 shells land in ticks ~82-200) (:143-148).
- Show an effect at an extreme knob value before judging (:149-159).
- A marker name is not a diagnosis; time the parts (:180-183).
- Mono runs float maths at double precision (:173-178).

**`JUICE.md`**
- Moments J01-J09, each with trigger and camera, look, readability check, cost class and score. A moment closes at 8.
- J01 recipe: `--scenario barrage --shot-tick 140 --shot-frames 16 --no-hud`.
- J03 is scored 5.

### `aosa.py` (`T\aosa\aosa.py`)
**Environment** (:20-23)
- `AOSA_DOCS`, `TW_BUILDS` (default `trench-warfare-3d/Builds`), `TW_PROJECT`, `AOSA_FALKIT` (default `~/Documents/claude/emtd-dragon-circle/falkit.py`), `AOSA_OFFLINE=1`.
- Standard bench: `stress=1500 settle_ticks=1800 ticks=400 warm=120 ff=8 vsync=0`, plus `quality=5` for the player and `canary=0` (:65, :672).

**Subcommands** (argparse :1486-1558)
- **`status`** (:371-410), always exits 0. Prints:
  - the editor lock of this project;
  - storage;
  - the Unity processes on the machine (≥300 MB means noisy benches);
  - the freshness of WinBench and WinBenchDev from `build-info.json` `git_sha`;
  - budget left;
  - WIP;
  - mode: B if the editor is free and a build is stale, E if the editor is free, otherwise P.
- **`bench LABEL [--player|--editor] [--dev] [--scenario S] [--shot-tick N] [--shot-frames N] [--no-hud] [--knobs k=v,...] [--repeats N] [--against LABEL] [--extra "k=v"] [--cycle N] [--build-dir D] [--dry-run]`** (:761-800)
  - Default repeats: 3 with `--against`, otherwise 1 (:783).
  - Interleaves A1 B1 A2 B2 using the recorded `<label>.args.json`.
  - Player timeout 1200 s; runs at 1920x1080 windowed (:714-715).
  - An editor bench refuses when no editor is open on this tree (exit 3, :781-782).
- **`compare A B [--metric main_ms.p95] [--higher-better] [--json]`** (:642-666). Delta is A − B; B is the baseline.
  - Refuses image runs, mismatched build types, and mismatched `machine.gpu` (:563-570).
  - Rule 1: hash_start, hash_end and hash_end_tick (:575-588). Rule 2: n ≥ 3 each (:604-605). Rule 3: RULE3 p95s (:607-627). Rule 4: FLOOR (:628-635).
  - Exit 0 pass, 1 fail, 2 refused.
  - Band logic (`band_for`, :538-555): priors with n ≥ 3, but never below this run's spread; otherwise `max(3*MAD, spread)`.
- **`attempt add JSON|@file|@-`** (:805-865). Requires cycle, card, class and verdict; a revert needs its rule; `predicted{metric,delta}` is required unless seed or void. It assigns `aNNNN`.
- **`learn`** (:909-980) writes `priors.json` and `knobs.json`. Groups are `runs/<cycle>/<stem>-N.json` of the same build and hash. `runs/seed` and held-clock runs are excluded. The band is 3·MAD, and below 10 samples never tighter than the widest group spread (:939-942).
- **`pick [--n 3] [--mode P|E|B]`** (:992-1038). Score = `hit_rate × |pred|/scale(metric) / max(cost,1)`. SIZE_COST XS1 S1 M2 L3 XL5 (:43). Notes "SPLIT (idle≥3)" and "measure next (idle≥2)" (:1013-1014). The metric scales are at :47-51.
- **`age [--moved C1,C2]`** (:1041-1061).
- **`budget`** (:242-249).
- **`refimg PNG --for juice|tier --prompt-file P [--n 2] [--card] [--cycle] [--timeout 600] [--dry-run]`** (:1081-1144). Exit 4 when refused over budget. Logs a juice or art `measure-only` attempt.
- **`retro`** (:1149-1178).
- **`ledger add --mode P|E|B --summary "..." [--cards] [--gate] [--commit] [--usd] [--result] [--cycle]`** (:1181-1209).
- **`snapshot [--label]`** (:1242-1260) copies the players to `Builds/<kind>@<sha>` and verifies them.
- **`prune [--apply]`** (:1314-1340) keeps the newest 2 snapshots per kind and the last 2 cycles' `.fN.png` frames (:1219-1220).
- **`shots LABEL [--all] [--min 4] [--section N] [--table]`** (:1385-1483): the shot_log analysis.
- **`stilldiff PNGs|LABEL [--threshold 8]`** (:345-368): pairwise per-channel threshold. The worst pair must be < 0.001; exits 1 otherwise.

### Other AOSA tools
**`land.ps1`** (`T\aosa\land.ps1`): `powershell -NoProfile -ExecutionPolicy Bypass -File Tools/aosa/land.ps1 [-EditOnly] [-NoBuild] [-Log <path>]` (:3-5)
- Runs at below-normal priority.
- Refuses with **exit 9** when `Assets/_AosaLocal/PipelineServerOff.asset` is missing (:20; A08 answered at `ASK.md:115-117`: an untracked asset with AutoStart off, never committed).
- Runs `validate.py`, then EditMode, then PlayMode with `-nographics`. Each has its own `-logFile` and never reads a stale xml (:47).
- Reruns once only when every failure matches `Failed to handle /api/exec request|No graphic device is available` (:38). Twice in a row is accepted with a note.
- Snapshot, then release and dev builds via `-executeMethod TW.Editor.BuildWindows.CommandLine [-twdev]` (:61-69).
- `Unchurn` reverts the three URP assets (:15-16, :71). `prune --apply` (:72).

**PerfBench**
- **`BenchOptions`** keys (`P\Perf\BenchOptions.cs:92-123`, defaults :13-79):

  | Key | Default |
  |---|---|
  | `stress` | 1500 |
  | `settle_ticks`/`settle` | 1800 |
  | `ticks` | 400 |
  | `warm` | 120 |
  | `ff` | 8 |
  | `quality` | -1 (pass it always) |
  | `vsync` | – |
  | `w`/`width`, `h`/`height` | 1920×1080 |
  | `weather` | 120 |
  | `zoom` / `yaw` / `pitch` | 30 / 21 / 25 |
  | `fx` / `fz` | NaN (armies' centre) |
  | `subs` | – |
  | `canary` | -1 |
  | `label` | – |
  | `out` | – |
  | `shot` | – |
  | `shot_tick` | -1 |
  | `shot_hud` | 1 |
  | `shot_frames` | 1 |
  | `quit` | 1 |
  | `scenario` | – |
  | `knobs` | – (repeated tokens add up) |
  | `ground` | – (an unknown value means exit 2) |

  Unknown keys go to `warnings` (:124).
- **`tw-perf/1` fields** (`P\Perf\PerfBench.cs`):
  - `run{label,utc,context,build(editor|development|release),args,exit,why,ended_in}` (:819-823)
  - `build_info` (a copy of `build-info.json`, player only) (:824-831)
  - `machine{cpu,cores,ram_mb,gpu,gpu_api,vram_mb,os,refresh_hz,battery}` (:832-836)
  - `config{quality,vsync,target_fps,screen,fullscreen,backend,gc_incremental,frame_timing_stats,knobs_arg,knobs,…,ground,ground_arg,biome,night,map_m}` (:843-856)
  - `scenario{scene,stress_per_side,seed,battlefield_seed,generated,bombardment_per_min,canary,view,weather_clock,name,requested}` (:857-865)
  - `still{path,at,tick,held_frame,time,…,window_clock(held|real),frames,hud}` (:871-878)
  - `window{tick_start,tick_end,hash_start,hash_end,hash_end_tick,alive_peak_before,alive_start,alive_end,frames,seconds,fps_mean,desync,focused,gc_collections,mono_used_mb,hitches_over_33ms,hitch_records,hitch_carriers}` (:880-904)
  - `series{dt_ms,cpu_frame_ms,main_ms,render_ms,gpu_ms,setpass,draw_calls,gc_bytes,vat_*,frame_draw_calls,frame_vertices,frame_indirect_draws,frame_budget_draws/vertices/indirect,script:*,TW.Hud.*}` with p50/p95/p99/mean/max/sum/n (:529-560, :696-697)
  - `per_tick_ms`, `script_markers`, `unavailable`, `warnings` (:912-930); `shot_log*` (:769-799)
- **Commands** (`D\reference\workflow.md:345-351`):
  - Player: `Builds/WinBench/TrenchWarfare.exe -twbench "stress=1500 settle_ticks=1800 ticks=400 quality=5 canary=0 shot=<png> out=<json>"`
  - Editor: `TW.Editor.CaptureRig.Bench("... out=<abs>.json")`
  - The env var `TW_BENCH` does the same as `-twbench` (`D\reference\feature-flags.md:34`).

**`perfcmp.py`** (`T\perfcmp.py`): `python Tools/perfcmp.py before.json after.json [...] [--force]`
- Refuses (exit 1) when `hash_start` or `run.build` differ, unless `--force` (:13-21). It **does not check the commit** (`build_info.git_sha`); check that by hand (workflow.md:359).
- Noise (:5-9; workflow.md:352-354): p50 about 1%, p95 up to about 12%, p99 and per-system up to about 25%, hitches 2 vs 5. Trust p50; believe a per-system change only past about 30% and in both runs.

**Instruments**
- **`FrameBudget`** (`P\Presentation\Core\RenderGround.cs:174-202`): `DrawCalls`, `Vertices` and `IndirectDraws` of the last complete frame. **`Vertices` excludes indirect draws, which means it excludes the men** (:193-197).
- **`HitchAttribution`** (`P\Perf\HitchAttribution.cs:1-7`) names the outermost TW marker that carried each hitch over 33 ms.
- **`AllocProbe`** (`P\Perf\AllocProbe.cs`): `Count(Action)` :38 and `PerCall(step, warm, reps)` :58 count GC.Alloc samples. Returns -1 in a release player.

**Budgets** (`D\05-performance-budgets.md`)
- Target 60 fps at 1080p on a GTX 1050 with 2,000 infantry (:5-6).
- Sim ≤ 3.6 ms per tick, hard 4 (:8-21).
- Main thread ≤ 3 ms (:23-32).
- GPU ≤ 13 ms (:34-44).
- VAT ≤ 256 MB; managed allocation 0 B per frame (:46-54).
- Fewer than 300 draw calls (:60-63).
- Banned patterns (:65-76). Profiling procedure (:78-93).

**Builds** (`P\Editor\BuildWindows.cs`)
- `ReleaseDir="Builds/WinBench"`, `DevelopmentDir="Builds/WinBenchDev"`, `Exe="TrenchWarfare.exe"` (:25).
- `Build(bool)` :41. `Queue` (:32-38) relies on `delayCall`, which never fires in a background editor (workflow.md:339).
- `CommandLine()` (:73-78) reads `-twdev` and exits 0 or 1.
- `build-info.json` (:80-100): `git_sha, dirty_files, unity, backend, development, frame_timing_stats, result, size_mb, build_seconds, built_utc, scenes`. `build-status.txt` sits beside it.
- Batch: `Unity.exe -batchmode -quit -projectPath <p> -executeMethod TW.Editor.BuildWindows.CommandLine [-twdev] -logFile <f>` (workflow.md:358).
- Release builds have no markers; never compare release with dev (:342-344).

**Fidelity bar** (`D\reference\decisions.md:64`): "a change that keeps the image lands freely; one that should be 'indistinguishable' needs before/after captures and a critic. Measure in the editor **and** a Windows player build."

**LODs and vertex budgets**
- `TW_DERIVE` (default `"12"`; `1` derives LOD1 only, `0` keeps Tripo's own) in `T\frogrig.py:242-251` and `T\tank3split.py:513-530`. Decided in `decisions.md:29` and `D\22-asset-playground.md:94`. Frog `TW_DERIVE_SYM` at :263.
- Unit art budget (decisions.md:26): 1,200-1,500 verts (max 2,000), far model 250-400 beyond 170 m, vehicles 3,000-5,000.
- `LodTiers.VertexBudget = 1500000`, knob `vat.vertexBudget` (`P\Presentation\Units\VATRenderer.cs:56`, :62). Shadows are on only while `nearVerts*2 + farVerts <= vertexBudget` (:272).
- `occ.py`: `python Tools/aosa/occ.py <assemblies in dependency order>` or `--changed`. `TW_LIB` overrides the Library (workflow.md:120-126).
