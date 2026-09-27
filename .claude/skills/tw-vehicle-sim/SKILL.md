---
name: tw-vehicle-sim
description: Vehicle simulator for Trench Warfare 3D — drive tanks (Maw, Tusk, Breaker) and walkers (Pincer, Kettle, Censer, Pavise, Banner, Redoubt) through every scenario (terrain, slopes, trenches, craters, turning, damage, lost legs and tracks, burning, wrecks), measure how they move, and repair their motion with the rig scoreboard as the judge. Use for "test the walkers", "the Pincer's feet slide", "does the Maw cross trenches", "score the rig board", "vehicle stage of item X". NOT for fracture/destruction looks (tw-destruction-vfx), unit numbers (tw-balance-sim) or infantry (tw-character-sim).
---

# Vehicle simulator

**Judge:** `docs/20-rig-scoreboard.md`. Every line is scored 0-10 by a harsh critic from rendered evidence, and a line
nobody has evidence for scores 0. **Guard:** `GaitTests` (14 tests). Deep reference: `references/vehicles.md`.
Driving and capture: `../pipeline/references/driving-and-evidence.md`.

## The rules the board sets (do not relax them)
1. A fix is applied only if the gait tests stay green. If they go red, revert it and log the attempt as a failure with its reason.
2. The camera starts far out (level 1, Tactical, 14× machine height) and comes closer only when **every** line is 8 or better at the current level. A score is valid only at the zoom it was taken at.
3. Write the threshold before the change and before measuring. Measure the noise floor first: a comparison of two samples of a noisy quantity was retracted once.
4. Size is fixed by `VehicleSize` (walkers ×2.5, tanks ×1.7, a seam item baked into meshes). The board is about how they **move**, not their size.
5. Never write into `Assets/` while another session holds this checkout's editor.

## The scenario matrix (a cell per machine × scenario × band)
Terrain types · slope (lean to it, T4) · parapet step (T1) · straddle a trench (T2) · reach down a revetment (T3,
capped: see owner questions) · find shell holes (T5) · turn on the spot · reverse · collision · turret traverse and
recoil (G1-G5) · lost leg (D1: stops, leans into the hole) · thrown track · burning (`TankCapture.Ignite`) · killed
(goes down on its legs) · wreck. Board lines: Walk W1-W6, Terrain T1-T5, Damage D1-D6, Guns G1-G5.
Metrics: foot slide (W1), ride steadiness, pitch/roll, stretched leg (W6), sink and wander (under 0.34 m / 0.38 m
on the level), parapet penetration, `pose_error_m` in every sidecar.

## How to run a cell
| Want | Use |
|---|---|
| Spawn and stage in battle (GreyboxCorridor, Play) | `Tools/tw eval 'return TW.Editor.TankCapture.Spawn(0, 6, 100f, 120f);'` (team, archetype, x, z) returns `slot N`, then read `World.Position[N]` and `CaptureRig.Shot(path, X, Z, zoom, yaw, pitch)` |
| Drive, stop, kill, enemies, film | `TW.Editor.RiderLab.Setup(archetype, riders, x, z, ...)`, `Drive(slot, dz)`, `Stop(slot)`, `Kill(slot)` (retry until dead), `Enemies(slot, count, ahead, bearing)`, `Freeze(bool)`, `Camera(slot, zoom, yaw, pitch)`, `Film(dir, seconds, fps, w, h)`, `Size(archetype, factor)` |
| Burn it | `TW.Editor.TankCapture.Ignite(slot, 0.62f)`; `Status()` (labels every non-Tusk as "Maw") |
| Locked-off stills of all six walkers (batch, needs a graphics device, **not** `-nographics`) | `Unity.exe -batchmode -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode -testFilter EveryWalkerIsPhotographedSideOnAgainstFixedGround -testResults <xml> -logFile <log>` (or `EveryWalkerIsPhotographedWalkingOnRealGround`); output `Captures/rigloop/<TW_STILLS_TAG>` |
| New 3-LOD machine, destructible parts, a walk cycle | Playground: menu **TW/Playground/Open and Play**, then `bash Tools/playground/pg.sh do "vehicle"`, `lod N`, `ap`, `he`, `ko`, `cook`, `walk speed`, `shot NAME`; `round2.sh TAG` + `python Tools/playground/score.py TAG PREV` (flags REGRESSED past measured noise floors) |
| Guard | `Tools/tw run run_tests -- --mode EditMode --filter TW.Tests.GaitTests --timeout 300`; also TankTests, TankMobilityTests, CrabTests, ChassisTests, WalkerArmamentTests, BreakerTests, WreckRecordTests, PlaygroundAssetTests |

Capture traps: sample frames 7 apart (22 aliased with the gait); retry `Kill` until `IsAlive` is false; machines cast
no shadow in stills; `CombatFx` moves `Camera.main` after the rig poses it (up to 0.088 m); read `pose_error_m`
before believing any displacement.

## Where motion lives
- `Presentation/Camera/WalkerGait.cs`: legs are solved, not animated. `Step(model, pos, yaw, vel, yawRate, lost, dead, dt, ground)`. Its constants are the repair knobs: `TriggerShare`, `SwingSlow/Fast`, `Clearance`, `StretchMin/Max`, `StepSafety`, `Lean`, `TurnLean`, `MaxSink`. `Carry` and `Step` already exist: check before adding.
- `Presentation/Camera/TankRenderer.cs`: `CrabNames` / `CrabArchetypes` (a new walker needs both), tank tilt spring (omega 7), `LodDistance` 170, footfalls to `SceneHooks.FootFall`.
- Sim side (SIM lane, not yours): `Sim/Nav/VehicleKinematics.cs` profiles (turn, `TrenchCrossWidth`, `SlopeLimit`, legs) and `Sim/Units/VehicleModules.cs` (tracks degrade by degrees, walkers limp per lost leg). A motion fix that needs a sim change is split: the SIM part lands first.

## Repair loop (at most 3 rounds per failing cell)
1. The threshold goes in the item's `thresholds.json` before measuring.
2. Change one constant or one rule. `GaitTests` green, or revert.
3. Re-shoot the same cell with the same pose, compare numbers, then look.
4. Three rounds without passing: BLOCKED, with a report of what source animation or model change is needed.

## Owner questions — never build around them
From docs/20's closing list: Banner's rear hips sit behind the leg's reach; Redoubt declares 6 legs but its model has 4, so leg-loss bits map to the wrong legs; parapet, revetment and duckboards have no height in the gait's ground, so T3 cannot be earned; feet are plain cones; the capture camera is nudged by CombatFx.
Also: "Not scaled with the giant machines: trench cross width, slope limit, turn rates, speeds, `MaxGrow`" (decisions.md, Open). No flying unit in the sim. Breaker has no model of its own (drawn as Maw).
