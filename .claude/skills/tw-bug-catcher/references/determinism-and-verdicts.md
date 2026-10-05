> Snapshot read at integration 558c667 (2026-09-28). file:line citations drift: re-grep the symbol before editing. The code wins over this page.

Path keys: R = repo root, P = trench-warfare-3d/Assets/_Project, T = trench-warfare-3d/Tools, D = docs.

## C. BUG CATCHER

### Determinism tests
- **`SimHashTests`** (`P\Tests\EditMode\SimHashTests.cs`): `const string ChainV9` (:48-55), the order-numbered system list from `40 CombatCatalogueSystem` to `1200 SectorControlSystem`.
  - `TheHashChainIsTheOneItsFormatVersionNames` asserts `ReplayRecorder.FormatVersion == 9` and the chain (:57-65).
  - A system joining or moving fails here: bump FormatVersion and pin the new chain in the same commit (`D\inbox\2026-09-27-all-lanes-landed.md:29-31`).
- **`DeterminismReplayTests`** (`P\Tests\EditMode\DeterminismReplayTests.cs`):
  - `SameSeedAndCommands_ProduceIdenticalHashes` (:75): 300 greybox ticks with a scripted ability script, plus the `Seams()` system calls for mines, tripwire and burning (:44-55).
  - `Replay_SerializesAndVerifies` (:87): `ReplayRecorder` → `Serialize` → `ReplayPlayer.Parse` → `Verify` == -1.
  - `DifferentSeeds_DivergeOnDeployment` (:100).
- **`HashIntervalTests`**: skipping the hash leaves the state identical; the recorder refuses `HashInterval != 1` (:1-4, :19, :29). The guard itself is `P\Net\LockstepDriver.cs:34-35`.
- **`SinglePlayerEquivalenceTests`**: one world vs the canary vs the canary under latency 2 / jitter 1 / loss 0.05. Hash-identical every tick for 1000 ticks (see A).
- **`BattlefieldLockstepTests`** (`P\Tests\EditMode\BattlefieldLockstepTests.cs`):
  - `FirstDifference(a,b)` (:52-97) names the first differing array (Position, Velocity, Yaw, Hp, MaxHp, Speed, Team, Archetype, SourceTrench, Generation, Suppression, StanceOf, Layer, TrenchId, PostCell, PostKind, TargetSlot, GoalId, Flags, Cooldown, FireCooldown, Knock, Silver, SilverFraction, Rally, SlotCooldown, SlotUnlocked). Failing that, it names the system whose `Hash()` differs.
  - `SameGround` compares `Map.Hash` because the heightfield is **not** in `SimWorld.Hash()` (:99-104).
  - Tests at :130, :143, :200, :280, :317, :386 (seed 1917 plus a second seed).

### Canary and probes
- **Switches:** `SimHost.DeterminismCanary` (:19), `-twCanary`, and `SimHost.CanaryOverride` (`P\Presentation\Core\SimHost.cs:133`; feature-flags.md:26, :43, :51).
- **`CanaryFixture`** (`P\Tests\PlayMode\CanaryFixture.cs:10-15`) turns the canary on for the whole PlayMode gate.
- **Detection:** `LockstepSession.Desync` / `DesyncTick` and the log line `DESYNC at tick {tick}: local … peer …` (`LockstepSession.cs:103-115`).
- **`SimProbe`** (`P\Editor\SimProbe.cs`):
  - `Unit(int slot)` :17: sim state vs drawn state.
  - `Match()` :53: seed, battlefield seed, ground, bombardment, stress, tick and mission, everything needed to rebuild the match in a test.
  - `Arrays(bool peer=false)` :64-75 hashes **only 8 arrays**: position, hp, stance, layer, trench, post, target, flags. Pause at the desync tick and diff `Arrays()` with `Arrays(true)` (workflow.md:423-427). The test's `FirstDifference` is broader.
- **`DeterminismPlatformReport.Write`** (`P\Editor\DeterminismPlatformReport.cs:16-40`): menu `TW/Determinism/Write Platform Report`. 2,000 greybox ticks, seed `0xC0FFEE`, StartingSilver 5000, deploy commands only (every 7th and 11th tick). Writes `DeterminismReport-<platform>-<arch>.txt`.
  - Batch: `Unity.exe -batchmode -quit -projectPath … -executeMethod TW.Editor.DeterminismPlatformReport.Write -logFile …`.
  - Desktop vs laptop at 2ad4973 were byte-identical, final hash `10D3E099A988AC12` (`G:\My Drive\TW3D-pipeline\determinism\RESULT.md`). That covers deploys only.

### Other guards
- **`TickAllocationTests`**: ticks with an army out, counted with AllocProbe (:98 `ATickWithAnArmyOutDoesNotAllocateHundredsOfKilobytes`, :150 lockstep driver).
- **`StaticLifecycleTests`**: a new mutable static in Presentation or UI fails until it calls `SceneStatics.Register` or is added to `Explained` (:1-9, :25, :87).
- **`FrameBudgetCoverageTests`**: no `Graphics.*` draw or CommandBuffer draw outside `RenderGround.cs` and `DebugOverlay.cs` (:1-7, :43).
- **Determinism rules** (`D\03-determinism-rules.md`):
  - Rules (:9-19). The review checklist (:21-30): no UnityEngine, Random, DateTime or managed collections in a tick; Strict Burst on every job; slot order; new arrays in `Hash()`; `SimMath` for transcendentals.
  - Every job needs `CompileSynchronously = true` (:45-51).
  - Never write one world from an eval; use `h.WriteWorlds(...)` (:52-107).
  - The same-build table is still "(pending)" (:39).

### Gate verdicts
**`gate.ps1`** (`R\gate.ps1`)
- Exit codes: 0 green, 8 a test failed, 6 no verdict, 5 validate, 3 held, 1 no unity (:5-7).
- "The xml is the verdict, not unity's exit code" (:22-23, :63-64). Zero tests run, or nothing passed, is exit 6 (:62, :65).
- **Stale-xml fix:** it deletes `test-results.xml` and `test-results-<mode>.xml` before each run, so "a run that writes nothing must not leave the last green copy" (:70-71; commits `cfc2045` and `34de04b`).
- Noise reruns happen only when every failure matches `unity-pipeline-port|Failed to handle /api/exec request` and none carries `Expected|But was|Assert` (:27-28, :39-52, :75-83).
- The laptop's memory "Unity batch gate verdicts — never trust the exit code or an unwatched results file; four ways `unity test` has misreported, and the stale-xml one that failed open" (`G:\My Drive\TW3D-pipeline\LAPTOP_INVENTORY.md:245`) is implemented here. The memory file itself is not readable from this machine.
- **Latest results** in the main clone (2026-09-28 00:21): EditMode 736 run, 733 passed, 3 skipped; PlayMode 16/16. workflow.md:170 still quotes 343.

**False reds** (`D\reference\workflow.md:174-185`)
1. After Play in the same editor, statics not reset by `SceneStatics` survive. Run `RequestScriptReload` and rerun.
2. External pipeline noise: `WriteToProjectRoot failed: Sharing violation on path ...\.unity-pipeline-port` or `Failed to handle /api/exec request: Main thread operation timed out`. The gate reruns once.
3. The first run after a Burst job edit can run managed code; rerun before trusting a single determinism failure.
4. A stale `Library/BurstCache` gives NullReference or IndexOutOfRange inside Burst jobs; delete it with the editor closed.

Also seen: `No graphic device is available` under `-nographics` (`land.ps1:62`), and the `LockstepLoopbackTests.Stress_ThreeThousandUnits…` 10.5 s timeout (`ASK.md:78-83`; `LESSONS.md:93-100`: "Rerun once. Only a second red is a finding.").

### Pixel and offline tools
- **`CaptureRig.Diff(pathA, pathB, outPath=null)`** (`P\Editor\CaptureRig.cs:490-530+`): a pixel counts as changed above a luma difference of 0.02. It returns `changed_frac`, `luma_mean_abs`, `luma_p99_abs`, `red/green/blue_delta`, `box_x/y/w/h/frac`, and the before/after of `luma_mean, blown_frac, black_frac, men_in_frame, contrast_median, contrast_p10, vertices, rain, pose_error_m`. `aosa.py stilldiff` uses a different measure (a per-channel threshold of 8).
- **`otr.py`** (`T\otr.py:1-26`): `python Tools/otr.py [Filter…] [-v]`, after `occ.py`.
  - Unity's Mono with stand-in engine calls. No Burst codegen, no job threads, no GPU or pixels. UI Toolkit, GameObjects, physics and audio report ENGINE.
  - Env: `TW_OTR_ENGINE=0`, `TW_OTR_LOG=1`, `TW_OTR_TRACE=1`, `TW_OTR_STACK=1`, `TW_OTR_BUILD=<dir>`.
  - Verdicts: PASS, FAIL, ERROR, LOGERR, TIMEOUT, IGNORE, ENGINE, RUNNER, CRASH, KNOWN (from `T\otr\known.txt`), SKIP-*. It is a first filter, never the verdict (workflow.md:128-132).
  - Its hashes are not comparable with Burst ones: managed code evaluates float maths differently (`D\03-determinism-rules.md:47-50`; LESSONS.md:173-178 on Mono double precision).

### Reproducing a bug (workflow.md:389-439)
- Live matches are not recorded, so reproduce in an EditMode test: `MatchSim.CreateBattlefield(cfg, MatchLaunch.Field(Ground, BattlefieldSeed))`, driven by `LockstepSession` and `StepOnce(ai)`.
- Order timing depends on frame rate, so sweep the issue tick a few ticks either side (:395-406).
- Sim or drawing? Use `SimProbe.Unit`. If the sim has him in a trench but he is drawn on the parapet, the fault is SHOW. If the sim has him on the surface, it is SIM: write a test and a note in `docs/inbox/` (:407-417).
- A real red in the other lane's code is theirs: an inbox note with the test name and message, and no patching of their files (`D\reference\tasks.md:26-29`).

### What a good bug card contains (synthesised from the above)
1. **Symptom and expected vs actual.** Name the metric or assertion message verbatim, from the xml.
2. **Verdict provenance.** Say which runner (gate xml, in-editor run after `RequestScriptReload`, or otr). Name which false-red signature it is not. Say whether it reproduced on a rerun.
3. **Lane and owner file.** Use the SimProbe.Unit split, the tasks.md row, and the guarding test.
4. **Repro:** the `SimProbe.Match()` line (seed, battlefield seed, ground, bombardment, stress, tick, mission), the command script with ticks, a tick sweep of ±N, and whether the canary or latency is on.
5. **For a desync:** `DesyncTick`, `FirstDifference` (array or system), the `SimProbe.Arrays` diff, and the FormatVersion or chain impact.
6. **A discriminating test** that fails on the old code (LESSONS.md:166-171).
7. **Routing:** the fix, or an inbox note `docs/inbox/<date>-<lane>-<topic>.md` for the other lane.
