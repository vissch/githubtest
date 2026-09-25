# Agent memory: incidents that cost time

A dated log of mistakes and surprises, each with the lesson that generalises. **Capped at 150 lines** (`validate.py`
fails above it). Only incidents go here:
- a fact about the code goes in the code, or in a Trap line in `tasks.md`;
- a procedure goes in `workflow.md` or `pipelines.md`;
- a decision goes in `decisions.md`;
- a note for another session goes in `inbox.md`.

When the log is full, fold an old entry into its page or delete it. The full log as it stood on 2026-09-25 (356
lines, with every measurement) is in git: `git show afc6fe8:docs/reference/agent-memory.md`.

## Looking for things
- **2026-09-22. Grep the declaration, not a guessed usage.** "Stance is never written" came from grepping `Stance[`;
  the array is `StanceOf` and `MovementSystem` writes it every tick. `TrenchGarrisonSystem` lives in
  `TrenchGarrison.cs`. Search for `class X` / the field's declaration, then its uses.
- **2026-09-23. Narrowing a search and reporting it as general.** `git log -- Sim/Combat/` "proved" nothing changed;
  the change was in `Sim/Units/`. When a cause is disputed, **change the input instead of searching**: setting
  `VehicleSize` to 1 made the suspect values identical to HEAD, the failure stayed byte-identical, so the suspect was
  cleared. A red that survives a fresh domain with identical numbers is deterministic and committed.
- **2026-09-24. A number that coincides with a constant is not that constant.** The cook-off's `2.5` looked like a
  stale `VehicleSize.Walker` and was reported as a bug; it was a reference hull length. Read what a number divides.
- **2026-09-24. Search before building.** Tank ruts and ground marks already had a system (`CombatFx` marks,
  `GroundMark_URP.shader`) that handled snow properly; a second one was nearly written.

## Believing evidence
- **2026-09-22. Captures lied three ways at once.** Stills rendered inline from eval used the last frame's cull set
  and the tactical camera's zoom; the OnGUI HUD never appeared; rain on a 50 s cycle made two stills different
  weather. Result: `CaptureRig`. Every "too bright" fault of that day was visible in a histogram before a PNG was opened.
- **2026-09-23. A mesh without Read/Write scales silently.** Giant walkers drew with legs at the new spacing and
  bodies at the old size. An eval in the live editor looked fine, because the editor refetches vertex data on demand
  and the shipped load path does not. **A value read back from a live editor is not evidence about the shipped
  path, and when numbers and a screenshot disagree, the screenshot is right.**
- **2026-09-23. `GC.GetAllocatedBytesForCurrentThread` reads 0 under Unity's GC**, so every "measured 0 B" result
  before that date was void. Validate an instrument on a known allocation first (AllocProbeSanityTests does).
- **2026-09-24. Benchmark the code that shipped.** A note said the one-pass marks sweep was slower; that measurement
  was of a draft still calling `Mathf.Sqrt` per mark. The shipped code was 1.37x faster.
- **2026-09-25. Timed-effect captures landed on the wrong moment.** `EditorApplication.Step()` is one real frame, and
  under load a frame is several times longer, so a frame count lands anywhere; an effect created before pausing ran
  out in the gaps between CLI calls. `tw stepto` steps game time; pause before creating the effect.
- **2026-09-25. Post-processing had never rendered a pixel.** `Settings/TW-Renderer.asset` had a null
  `postProcessData`, so URP skipped the whole pass: the grade, bloom, tonemapping and grain in `Atmosphere.cs` did
  nothing, while `VolumeManager.instance.stack` reported the authored values. Proved by an A/B that should have
  moved everything (bloom intensity 6, +2.4 stops) and moved the mean by 0.04. **Test that a setting is consumed,
  not that it is set.** And a paused game view does not repaint: step one frame, or before and after are one buffer.
- **2026-09-25. The capture noise floor was bigger than what was being tuned.** Two runs of an identical script on
  identical code gave the pyre's hot core 12.18% and 1.43%, and a critique round called that a regression. Causes:
  unseeded `Random` (79 draws) and animation sampled at absolute `Time.time`. Seed, and anchor both birth and sample
  to absolute game time (`tw stepabs`). **Measure the noise floor with two runs before believing any difference.**
- **2026-09-25. The wall shot had no wall.** GreyboxCorridor's heightfield has no parapet (everything is instanced
  cards, one Renderer), so a site search settled on a gentle slope and the capture held no fire but measured cleanly.
  `Tools/flamecheck.py` now refuses a fire shot without fire. **Check a capture contains its subject.**
- **2026-09-25. Choosing art by one metric got cheated.** "Fewest holes" picked a thin arc (an arc encloses nothing),
  and cutting a book shorter to dodge a bad phase, four times, made a tank's cook-off smaller than a campfire.
  Score several measures at once; replace the wrong drawing instead of trimming it.
- **2026-09-25. Mono computes floats at double precision unless each step is cast.** A port of `(int)(v*255+.5)`
  differed on 515 texels until narrowed; only a test against the old loop caught it. Per-frame percentiles of count
  metrics shift with frame times, so compare them on held-clock runs.
- **2026-09-25. Fire could never be brighter than a lit white wall.** The flipbook fire curve `(1 - exp(-peak)) / peak`
  asymptotes at 1.0; four books from four sheets all capped at the same luminance. Found by measuring, not looking.

## Tests that passed while the game was broken
- **2026-09-23. No test runs OnGUI.** The IMGUI `BattleHud` threw IndexOutOfRange every frame (fixed 5-slot arrays)
  in a tree that passed 85/85. `validate.py` also said OK on code that did not compile. Green gate ≠ the game runs.
- **2026-09-23. The Windows build was broken and no editor check could see it.** Six `Shader.Find` shaders were not in
  builds, and popped menu screens stayed drawn over every match. Build the player and look at it.
- **2026-09-23. Eight gait tests and a screenshot missed walkers splaying flat a second after setting off.** The test
  that caught it asserted the thing that matters ("it does not sink while walking"). Tolerances on a rig are a share
  of the part: 2 cm is a hovering foot on a 70 cm leg.
- **2026-09-24. A derived array's default must be the inert value.** `CellTrenchDist` starts at 0, which means
  "touching a trench", so the generator refused every crater and the battlefield came out flat. All ten new tests
  passed because they shared one map where the array was built. **A feature whose tests share one map is untested.**
- **2026-09-24. A bound expressed as a total is not a bound per place.** A growing crater buried every rim ring it laid;
  20 shells gave a rim of +0.00 m while every test fired two. **Run a system to its limit, not once or twice.**
- **2026-09-24. `MaxRemembered` 2000 made destroyed props come back** about eight minutes into a match. A cap on
  remembered state needs a test at the rate the game actually produces it.

## Rendering traps found the slow way
- **2026-09-23. `GetIndirectInstanceID` restarts at 0 per draw on D3D.** Nine of ten debris pools, then the sniper
  and far tiers, read the first pool's data. Any RenderMeshIndirect shader indexes with `GetIndirectInstanceID_Base`.
- **2026-09-24. An unfired `clip()` cost 1.5 ms of a 3.5 ms opaque pass** by disabling early-Z on every living man.
  Clips in `VAT_URP` sit behind `_TW_LIMBCUT`; VatEarlyZTests enforces it.
- **2026-09-24. A single-instance `RenderMeshInstanced` ignores per-instance properties.** A mask test with count 1
  "proved" the mask broken. Submit several instances when testing one by hand.
- **2026-09-24. A mask word past 24 bits rounds away silently** inside a float, so distant house chunks came back.
  `HouseKitTests` guards each word.
- **2026-09-24. The env atlas was full although its script said it had spare cells.** Check the thing, not the comment.
- **2026-09-23. An explicit Euler spring blew a tank 3 km** on one long frame (omega·dt ≈ 2 during an eval stall).
  Springs here are exact or clamp dt.

## Sharing the machine
- **2026-09-23. Saving into `Assets/` under a peer's open editor recompiled it and killed their Play session**, twice
  in one day. Then a landing script ran although its lock claim had failed: `claim ... | tail` swallowed the exit
  code. The claim is a gate: `|| exit 1` inside the script.
- **2026-09-24. `EditorApplication.update = null` in an eval removed the pipeline's own pump**, and every session's
  evals timed out until a script change forced a domain reload.
- **2026-09-24. The in-editor test runner hung the pipeline after about four full runs** (850 s timeout, editor still
  "Responding"). And `test_status` returned another session's run. One class at a time in the editor; the gate for all.
- **2026-09-25. "The MCP bridge is down" was usually a misdiagnosis.** The MCP server wraps the `unity` CLI, which
  still worked; the `unity.exe` processes counted as editors were orphaned MCP servers. `Tools/tw` came from this.
- **2026-09-25. Two editors vanished and the owner's was paged out** (8.6 GB private, 13 MB resident, unreachable)
  with three editors and a player benchmark on a 16 GB machine: 0.4 GB free. No crash dump. `health.py` now prints
  available memory; do not open another editor under ~4 GB.
- **2026-09-25. Batch gate runs failed a random test each time** with "Sharing violation ... .unity-pipeline-port" or
  "Failed to handle /api/exec request". The batch run listens on the pipeline port like an editor; unpinned CLI calls
  and the MCP server reach it. `tw` now pins its project; `gate.ps1` reruns failures that are only this noise.

## Process
- **2026-09-22. After editing a Burst job the first run used managed code** and broke determinism tests. Every sim
  job has `CompileSynchronously = true`.
- **2026-09-22. Long inline Python through the Bash tool loses its backslashes.** Write patch scripts to a file.
- **2026-09-23. Three armchair diagnoses of the walker gait were wrong.** Porting the maths to Python with the real rig
  numbers reproduced Unity to three decimals and found the cause in two iterations. Offline reproduction beats
  90-second editor runs. The gait's hard-won rules are in the header comments of `WalkerGait.cs`; the Tripo
  sheet rules (limb symmetry, welded parts) in `Tools/crabsplit.py`.
- **2026-09-23. Tuning sweeps compared noise** until the scene was frozen (`SimHost.TimeScale = 0`, `Time.timeScale = 0`).
- **2026-09-24. `subprocess(text=True)` decoded a UTF-8 doc as cp1252** and mangled ten lines into a commit.
- **2026-09-25. Every entry-point doc had rotted within five days** (docs/04 named four systems that never existed;
  README called built systems stubs). Tables that can be derived are now generated, and the rest is checked, by
  `Tools/codemap.py` inside `validate.py`.
