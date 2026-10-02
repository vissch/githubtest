# Vehicles: reference (read at integration 558c667; the code wins where this differs)

## Roster, chassis, size
- Archetype ids (`Sim/Core/RosterEntry.cs`): Maw 4 (heavy tank), Tusk 5 (light tank), Pincer 6, Kettle 7, Censer 8, Pavise 9, Banner 10, Redoubt 11 (walkers), Breaker 18 (assault tank, `Sim/Units/Breaker.cs`).
- `ChassisKind`: Foot 0, Tracked 1, Legged 2, Wheeled 3. Wheeled is declared, but no sim unit uses it; Mercy exists only in the Playground.
- `VehicleSize.Walker = 2.5`, `VehicleSize.Tank = 1.7` (`Sim/Nav/VehicleKinematics.cs`). This is a seam item, baked into mesh vertices at load. A rebake must not fold it into crabsplit's own scale as well.
- Walkers need about 10 m gaps between placed structures (tasks.md trap).

## Kinematic profiles (`VehicleProfile.*`, VehicleKinematics.cs; SIM lane)
| Machine | Turn rad/s | Trench cross m | Slope limit | Legs |
|---|---|---|---|---|
| Pincer | 1.15 | 3.6 | 0.95 | 6 |
| Kettle | 1.35 | 3.0 | 0.90 | 4 |
| Censer | 1.45 | 3.1 | | 4 |
| Pavise | 0.95 | 3.4 | | 4 |
| Banner | 1.05 | 3.5 | | 4 |
| Redoubt | 0.80 | 3.3 | | 6 (the model has 4) |
| Maw | 0.42 | tracked default | 0.55 | |
| Tusk | 0.75 | 2.4 | 0.6 | |
| Breaker | 0.6 | | | footprint not scaled by VehicleSize |

Speed factors: `PickSpeed` 0.88 in a crater; `StepOverSpeed` 0.72 in a trench.

## Damage in the sim (`Sim/Units/VehicleModules.cs`)
- **Tracks:** `TrackFullAbove` 0.8, `TrackThrownBelow` 0.2, `TrackWorstFactor` 0.35, `TrackMismatch` 0.15.
- **Legs:** `LegLoss` 0.16, `LegFloor` 0.25. `LegsLost` is a byte per slot. A lost leg's bit is `onRight ? perSide + k : k`, and it emits `VehicleLegLost`.
- **Stuck:** a walker is stuck when one side's share of legs reaches 0; a tank when a track falls below 0.2.
- **Mobility** = `max(LegFloor, 1 − 0.16 · lostLegs)`.
- **Modules:** TrackLeft 1, Engine 2, Crew 3, GunA 4, TrackRight 5, GunB 6, Fuel 7, Ammo 8.
- Owner decision: walkers limp as they lose legs; tank tracks degrade by degrees.

## The rig scoreboard (docs/20-rig-scoreboard.md) at the last read
- **Zoom ladder:** L1 Tactical 14× machine height at 35°; L2 Engagement 7× at 22°; L3 Close 3.5× at 14°; L4 Macro 1.8× at 9°. The lens puts the machine at 72 % of the frame. **Current level: 1.**
- **Scores:**

  | Line | Score |
  |---|---|
  | W1 foot slide | 4 |
  | W2 steady ride | 5 |
  | W3 pitch/roll | 5 |
  | W4 rhythm | 5 |
  | W5 leans into turn | 6 |
  | W6 no stretched leg | 2 |
  | T1 parapet step | 6 |
  | T2 straddle | 6 |
  | T3 revetment | 2 (capped) |
  | T4 level to slope | 4 |
  | T5 shell holes | 5 |
  | D1 lost leg stops | 2 |
  | D2-D6 damage | never scored in 103 cycles |
  | G1-G5 guns | 8 / 7 / 5 / 7 / 7 |

- **Cheapest next wins** (the board's own advice): subtract `pose_error_m` and score W1; score D2-D6; fix the `det > 1e-4f` check.
- **Last fixes, both in WalkerGait:** the ground has the last word on a swinging foot (parapet penetration 1.164 m → 0), and `Urgency` no longer starves long legs.
- The ladder itself is not in any tool. WalkerStills uses fixed zooms: 26 side-on and 15 walking.

## GaitTests (Tests/Show/GaitTests.cs)
Every leg rigged with a toe under it · a planted foot stays put · always feet on the ground · feet find the ground · leans to a slope · a foot does not pass through a parapet · a step goes over a parapet · the modelled leg reaches the chosen foot · a lost leg stops and leans into the hole · no sinking on the level (sink < 0.34 m, wander < 0.38 m) · built at the shipped size · a killed walker goes down on its legs · every footfall reported · steps when turning on the spot.

## WalkerStills (Tests/Stills/WalkerStills.cs; asmdef TW.Tests.Stills; `[UnityTest, Explicit]`, so the gate skips them)
- `EveryWalkerIsPhotographedSideOnAgainstFixedGround`: output `<tag>-side`. Lanes at z = 60 + 35·n; `Drive(slot, 30)`; camera zoom 26, yaw 0, pitch 16, 2560×1440, aimY 2.0; 12 frames 6 apart; strips via `Sheet`.
- `EveryWalkerIsPhotographedWalkingOnRealGround`: lanes at z = 30 + 45·n; follow camera; zoom 15, yaw 20, pitch 22, 1600×900; 6 frames 7 apart.
- Weather pinned with `Atmosphere.Rain = Squalls = 0` and `PinnedClock = 30`. `TW_STILLS_TAG` names the output folder; it is not listed in feature-flags.md.

## Playground (docs/22-asset-playground.md; Playground/)
- **New asset workflow:**
  1. Unzip the Tripo LODs to short paths.
  2. Split with `tank3split.py` or `mechsplit.py`.
  3. Output goes to `Playground/Art/Tanks/<Name>`.
  4. Run **TW/Playground/Build**, then the `TW.Tests.Playground` tests.
  5. Play, `bash Tools/playground/round.sh r1`, then critic → fix → `round.sh r2`.
- **pg.sh vehicle commands:**
  - Scenes: `vehicle`, `vehicle.compare`, `mixed`.
  - Actions: `lod N|-1`, `seq`, `ap`, `he`, `ko`, `cook`, `fire`, `repair`, `traverse`, `cookdelay s`, `size f`, `fly speed [alt]`, `walk speed [inPlace]`.
  - Camera: `cam close|standard|far|top`, `cam follow yaw pitch dist fov`, `freeze`, `timescale`, `biome`, `ground grid|mud`.
  - Measures: `lodfit path`, `lodpop path`, `sidehue path`, `shot path w h`.
- **`VehicleRig`:** Intact → Damaged → Immobilised → KnockedOut → CookedOff, with parts coming off by tier. `WalkerDrive` runs the battle's `WalkerGait` without `Solve`; `FlyerDrive` hovers at 14 m.
- **`round2.sh TAG`:** per machine it shoots compare, hits, burning, cookoff, after, a motion shot and standard, then `lodpop`. Vehicle index by library order: Brute 0, Croaker 1, Hopper 2, Mercy 3, Skimmer 4.
- **`score.py TAG [PREV|-] [DIR]`** noise floors: pop IoU 0.006; block colour 0.6 (tank, frog) or 1.0 (others); m3 contrast 0.08; side cross-talk 0.002; hue gap 5. fps is never flagged.
- **LOD switch points:** LOD0 above 0.70 of screen height (under ~45 m), LOD1 above 0.22 (the 78 m standard view), LOD2 beyond ~145 m. LODs are derived from LOD0 (`TW_DERIVE` 12, owner decision).
- **Budget:** vehicles 3,000-5,000 (decisions.md; the unit is written as vertices, docs/22 compares triangles).
- Nothing in the Playground is in the build. The battle's `TankModel` loads only 2 LODs, so 3-LOD Playground assets are not battle-ready.

## Blender splits (Blender 5.0: `"C:\Program Files\Blender Foundation\Blender 5.0\blender.exe" -b --factory-startup -P <script> -- <args>`)
| Script | Arguments | Flags |
|---|---|---|
| `tank3split.py` | `<Name> <lod0> <lod1> <lod2> <outdir> <renderdir>` | `TW_SCALE` 6.6, `TW_DERIVE` 12 |
| `mechsplit.py` | `<Name> <lod0> [<lod1> [<lod2>]] <outdir> <renderdir>` | `TW_KIND=walker\|hover\|flyer`, `TW_LOD2`, `TW_TURN` |
| `crabsplit.py` | `<sheet...> <outdir> <renderdir>` | Code order: Pincer, Kettle, Censer, Pavise, Banner, Redoubt, Cutter |
| `jeepsplit.py` | `<Name> <lod0> [<lower>] <outdir> <renderdir>` | `TW_SCALE` 4.6, `TW_LOD2`, `TW_SYM` |
| `tanksplit.py` | `<full> <far> <outdir> <renderdir>` | |

**Traps:**
- Parts come out turned 180°: check facing by the barrel vertices.
- Copy sheets to short paths first (MAX_PATH).
- Every part must be parented to the Body.
- Source files are the owner's and are not in the repo. Ask; do not substitute.
- Parameters used for past splits are UNRECORDED: re-derive them before regenerating.
